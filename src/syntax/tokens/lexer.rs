use std::{borrow::Cow, sync::Arc};

use logos::{Lexer, Logos};

use crate::syntax::{CompilerError, WithSpan};

use super::{FileLocation, FileRef, Span};

#[derive(Logos, Debug, PartialEq)]
#[logos(subpattern decimal = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(subpattern octal = r"[0-7][_0-7]*")]
#[logos(subpattern binary = r"[0-1][_0-1]*")]
#[logos(subpattern exp = r"[eE][+-]?[0-9][_0-9]*")]
enum StringToken {
    #[regex(r"[^\S\r\n\f]+")]
    Whitespace,

    #[regex(r"[\r\n\f]+")]
    Newline,

    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*")]
    Ident,

    #[regex("\"(?:[^\"]|\\\\\")*\"")]
    String,

    #[regex("(?&decimal)")]
    Integer,

    #[regex("0[xX](?&hex)")]
    HexInteger,

    #[regex("0[oO](?&octal)")]
    OctalInteger,

    #[regex("0[bB](?&binary)")]
    BinaryInteger,

    #[regex(r#"[+-]?(((?&decimal)\.(?&decimal)?(?&exp)?[fFdD]?)|(\.(?&decimal)(?&exp)?[fFdD]?)|((?&decimal)(?&exp)[fFdD]?)|((?&decimal)(?&exp)?[fFdD]))"#)]
    Float,

    #[regex(r"//[^\r\n]*")]
    LineComment,

    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    BlockComment,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(";")]
    Semi,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("^")]
    Caret,

    #[token("!")]
    Exclamation,

    #[token("~")]
    Tilde,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("=")]
    Eq,

    #[token("?")]
    Question,

    #[token("&")]
    And,

    #[token("|")]
    Or,
}

pub type TokenList = WithSpan<Arc<[WithSpan<TokenValue>]>>;

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedTokenList {
    pub tokens: TokenList,
    pub left_cap: Span,
    pub right_cap: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Ident(Arc<str>),
    String(Arc<str>),
    Integer(i64),
    Float(f64),
    Parens(GroupedTokenList),
    Braces(GroupedTokenList),
    Brackets(GroupedTokenList),
    Whitespace,
    Newline,
    Comment,
    Semi,
    Colon,
    Comma,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Exclamation,
    Tilde,
    Lt,
    Gt,
    Eq,
    Question,
    And,
    Or,
}

impl std::fmt::Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::Ident(s) => write!(f, "{}", s),
            TokenValue::String(s) => write!(f, "\"{}\"", s),
            TokenValue::Integer(i) => write!(f, "{}", i),
            TokenValue::Float(fl) => write!(f, "{}", fl),
            TokenValue::Parens(_) => write!(f, "(...)"),
            TokenValue::Braces(_) => write!(f, "{{...}}"),
            TokenValue::Brackets(_) => write!(f, "[...]"),
            TokenValue::Whitespace => write!(f, "{{whitespace}}"),
            TokenValue::Newline => write!(f, "{{newline}}"),
            TokenValue::Comment => write!(f, "{{comment}}"),
            TokenValue::Semi => write!(f, ";"),
            TokenValue::Colon => write!(f, ":"),
            TokenValue::Comma => write!(f, ","),
            TokenValue::Dot => write!(f, "."),
            TokenValue::Plus => write!(f, "+"),
            TokenValue::Minus => write!(f, "-"),
            TokenValue::Star => write!(f, "*"),
            TokenValue::Slash => write!(f, "/"),
            TokenValue::Percent => write!(f, "%"),
            TokenValue::Caret => write!(f, "^"),
            TokenValue::Exclamation => write!(f, "!"),
            TokenValue::Tilde => write!(f, "~"),
            TokenValue::Lt => write!(f, "<"),
            TokenValue::Gt => write!(f, ">"),
            TokenValue::Eq => write!(f, "="),
            TokenValue::Question => write!(f, "?"),
            TokenValue::And => write!(f, "&"),
            TokenValue::Or => write!(f, "|"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTokenTree {
    pub tokens: TokenList,
    pub errors: Vec<CompilerError>,
}

struct MetaLexer<'a, L: Logos<'a, Source = str>> {
    lexer: Lexer<'a, L>,

    file: Arc<FileRef>,

    // Tracking the current location
    line: u32,
    column: u32,
    index: u32,

    span: Span,
}

impl<'a, L: Logos<'a, Source = str>> MetaLexer<'a, L> {
    fn new(lexer: Lexer<'a, L>, file: Arc<FileRef>) -> Self {
        Self {
            lexer,
            file: file.clone(),
            line: 1,
            column: 1,
            index: 0,
            span: Span::new(file, FileLocation::new(0, 0, 0)..FileLocation::new(0, 0, 0)),
        }
    }

    /// Updates the location information based on the given index in the source code.
    ///
    /// # Arguments
    ///
    /// * `index` - The index in the source code to update the location to.
    ///
    /// # Panics
    ///
    /// Panics if the given index is less than the current index.
    ///
    /// # Returns
    ///
    /// The updated `FileLocation` struct containing the line number, column number, and index.
    fn update_location(&mut self, index: u32) -> FileLocation {
        assert!(index >= self.index);
        let source = self.lexer.source();

        for char in source[self.index as usize..index as usize].chars() {
            if char == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        self.index = index;

        FileLocation::new(self.line, self.column, self.index)
    }

    fn next(&mut self) -> Option<Result<L, Cow<'static, str>>> {
        let token = self.lexer.next()?;

        let lexer_span = self.lexer.span();

        let start = self.update_location(lexer_span.start as u32);
        let end = self.update_location(lexer_span.end as u32);

        self.span = Span::new(self.file.clone(), start..end);

        match token {
            Ok(token) => Some(Ok(token)),
            Err(_) => Some(Err("Failed to parse".into())),
        }
    }

    fn span(&self) -> Span {
        self.span.clone()
    }

    fn slice(&self) -> &str {
        self.lexer.slice()
    }

    fn make_error(&self, message: impl Into<Cow<'static, str>>) -> CompilerError {
        CompilerError::new(message, self.span())
    }
}

pub fn parse_file(file: FileRef) -> ParsedTokenTree {
    let file = Arc::new(file);

    let lexer = StringToken::lexer(&file.contents);
    let mut meta_lexer = MetaLexer::new(lexer, file.clone());

    parse_group(&mut meta_lexer, None)
}

fn parse_group(lexer: &mut MetaLexer<StringToken>, end: Option<StringToken>) -> ParsedTokenTree {
    let span_if_no_tokens = lexer.span();

    let mut span_start = None;
    let mut span_end = None;

    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    loop {
        let Some(token) = lexer.next() else {
            if end.is_some() {
                errors.push(lexer.make_error("Unexpected end of file"));
            }
            break;
        };

        let token = match token {
            Ok(token) => token,
            Err(err) => {
                errors.push(lexer.make_error(err));
                continue;
            }
        };

        if Some(&token) == end.as_ref() {
            break;
        }

        let span = lexer.span();
        if span_start.is_none() {
            span_start = Some(span.clone());
        }
        span_end = Some(span.clone());

        let make_token = move |token: TokenValue| WithSpan { value: token, span };

        let token = match token {
            StringToken::LParen => {
                let start = lexer.span();
                let group = parse_group(lexer, Some(StringToken::RParen));
                errors.extend(group.errors);
                let end = lexer.span();

                span_end = Some(end.clone());

                WithSpan {
                    span: start.join(&end),
                    value: TokenValue::Parens(GroupedTokenList {
                        tokens: group.tokens,
                        left_cap: start,
                        right_cap: end,
                    }),
                }
            }
            StringToken::LBrace => {
                let start = lexer.span();
                let group = parse_group(lexer, Some(StringToken::RBrace));
                errors.extend(group.errors);
                let end = lexer.span();

                span_end = Some(end.clone());

                WithSpan {
                    span: start.join(&end),
                    value: TokenValue::Braces(GroupedTokenList {
                        tokens: group.tokens,
                        left_cap: start,
                        right_cap: end,
                    }),
                }
            }
            StringToken::LBracket => {
                let start = lexer.span();
                let group = parse_group(lexer, Some(StringToken::RBracket));
                errors.extend(group.errors);
                let end = lexer.span();

                span_end = Some(end.clone());

                WithSpan {
                    span: start.join(&end),
                    value: TokenValue::Brackets(GroupedTokenList {
                        tokens: group.tokens,
                        left_cap: start,
                        right_cap: end,
                    }),
                }
            }
            StringToken::RParen | StringToken::RBrace | StringToken::RBracket => {
                errors.push(lexer.make_error("Unexpected closing bracket"));
                break;
            }
            StringToken::Ident => make_token(TokenValue::Ident(lexer.slice().into())),
            StringToken::String => {
                let mut string = lexer.slice();
                string = &string[1..string.len() - 1];

                let string = string.replace("\\n", "\n");
                let string = string.replace("\\r", "\r");
                let string = string.replace("\\t", "\t");
                let string = string.replace("\\\\", "\\");
                let string = string.replace("\\\"", "\"");
                // TODO: Unicode escapes

                make_token(TokenValue::String(string.into()))
            }
            StringToken::Integer => {
                let string = lexer.slice();
                let integer = string.replace("_", "").parse().unwrap();

                make_token(TokenValue::Integer(integer))
            }
            StringToken::HexInteger => {
                let string = &lexer.slice()[2..];
                let integer = i64::from_str_radix(&string.replace("_", ""), 16).unwrap();

                make_token(TokenValue::Integer(integer))
            }
            StringToken::OctalInteger => {
                let string = &lexer.slice()[2..];
                let integer = i64::from_str_radix(&string.replace("_", ""), 8).unwrap();

                make_token(TokenValue::Integer(integer))
            }
            StringToken::BinaryInteger => {
                let string = &lexer.slice()[2..];
                let integer = i64::from_str_radix(&string.replace("_", ""), 2).unwrap();

                make_token(TokenValue::Integer(integer))
            }
            StringToken::Float => {
                let string = lexer.slice();
                let float = string.replace("_", "").parse().unwrap();

                make_token(TokenValue::Float(float))
            }

            StringToken::Whitespace => make_token(TokenValue::Whitespace),
            StringToken::Newline => make_token(TokenValue::Newline),
            StringToken::LineComment => make_token(TokenValue::Comment),
            StringToken::BlockComment => make_token(TokenValue::Comment),
            StringToken::Plus => make_token(TokenValue::Plus),
            StringToken::Minus => make_token(TokenValue::Minus),
            StringToken::Star => make_token(TokenValue::Star),
            StringToken::Slash => make_token(TokenValue::Slash),
            StringToken::Percent => make_token(TokenValue::Percent),
            StringToken::Caret => make_token(TokenValue::Caret),
            StringToken::Exclamation => make_token(TokenValue::Exclamation),
            StringToken::Tilde => make_token(TokenValue::Tilde),
            StringToken::Lt => make_token(TokenValue::Lt),
            StringToken::Gt => make_token(TokenValue::Gt),
            StringToken::Eq => make_token(TokenValue::Eq),
            StringToken::Question => make_token(TokenValue::Question),
            StringToken::And => make_token(TokenValue::And),
            StringToken::Or => make_token(TokenValue::Or),
            StringToken::Colon => make_token(TokenValue::Colon),
            StringToken::Comma => make_token(TokenValue::Comma),
            StringToken::Dot => make_token(TokenValue::Dot),
            StringToken::Semi => make_token(TokenValue::Semi),
        };

        tokens.push(token);
    }

    let span = match (span_start, span_end) {
        (Some(start), Some(end)) => start.join(&end),
        (Some(span), None) | (None, Some(span)) => span,
        (None, None) => span_if_no_tokens,
    };

    ParsedTokenTree {
        tokens: WithSpan {
            value: Arc::from(tokens),
            span,
        },
        errors,
    }
}
