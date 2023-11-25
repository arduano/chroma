use std::{borrow::Cow, ops::Range, path::Path, sync::Arc};

use logos::{Lexer, Logos};

use super::{FileLocation, Span};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")] // new way to annotate whitespace
#[logos(subpattern decimal = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(subpattern octal = r"[0-7][_0-7]*")]
#[logos(subpattern binary = r"[0-1][_0-1]*")]
#[logos(subpattern exp = r"[eE][+-]?[0-9][_0-9]*")]
enum StringToken {
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

    // #[regex(r"0[xX](((?&hex))|((?&hex)\.)|((?&hex)?\.(?&hex)))[pP][+-]?(?&decimal)[fFdD]?")]
    // HexFloat,
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
    Not,

    #[token("~")]
    Tilde,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("=")]
    Assign,

    #[token("?")]
    Question,

    #[token("&")]
    And,

    #[token("|")]
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

pub type TokenList = WithSpan<Arc<[WithSpan<TokenValue>]>>;
pub type LexerError = WithSpan<Cow<'static, str>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Ident(Arc<str>),
    String(Arc<str>),
    Integer(i64),
    Float(f64),
    // LineComment,
    // BlockComment,
    Parens(TokenList),
    Braces(TokenList),
    Brackets(TokenList),
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
    Not,
    Tilde,
    Lt,
    Gt,
    Assign,
    Question,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTokenTree {
    pub tokens: TokenList,
    pub errors: Vec<LexerError>,
}

struct MetaLexer<'a, L: Logos<'a, Source = str>> {
    lexer: Lexer<'a, L>,

    file: Arc<Path>,

    // Tracking the current location
    line: u32,
    column: u32,
    index: u32,

    span: Span,
}

impl<'a, L: Logos<'a, Source = str>> MetaLexer<'a, L> {
    fn new(lexer: Lexer<'a, L>, file: Arc<Path>) -> Self {
        Self {
            lexer,
            file: file.clone(),
            line: 1,
            column: 1,
            index: 0,
            span: Span {
                file,
                range: FileLocation::new(0, 0, 0)..FileLocation::new(0, 0, 0),
            },
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

    fn make_error(&self, message: impl Into<Cow<'static, str>>) -> LexerError {
        LexerError {
            value: message.into(),
            span: self.span(),
        }
    }
}

pub fn parse_file(data: &str, path: Arc<Path>) -> ParsedTokenTree {
    let lexer = StringToken::lexer(data);
    let mut meta_lexer = MetaLexer::new(lexer, path);

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

        let span = lexer.span();
        if span_start.is_none() {
            span_start = Some(span.clone());
        }
        span_end = Some(span.clone());

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

        let make_token = move |token: TokenValue| WithSpan { value: token, span };

        let token = match token {
            StringToken::LParen => {
                let group = parse_group(lexer, Some(StringToken::RParen));
                WithSpan {
                    span: group.tokens.span.clone(),
                    value: TokenValue::Parens(group.tokens),
                }
            }
            StringToken::LBrace => {
                let group = parse_group(lexer, Some(StringToken::RBrace));
                WithSpan {
                    span: group.tokens.span.clone(),
                    value: TokenValue::Braces(group.tokens),
                }
            }
            StringToken::LBracket => {
                let group = parse_group(lexer, Some(StringToken::RBracket));
                WithSpan {
                    span: group.tokens.span.clone(),
                    value: TokenValue::Brackets(group.tokens),
                }
            }
            StringToken::RParen | StringToken::RBrace | StringToken::RBracket => {
                errors.push(lexer.make_error("Unexpected closing bracket"));
                break;
            }
            StringToken::LineComment | StringToken::BlockComment => {
                continue;
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
            StringToken::Plus => make_token(TokenValue::Plus),
            StringToken::Minus => make_token(TokenValue::Minus),
            StringToken::Star => make_token(TokenValue::Star),
            StringToken::Slash => make_token(TokenValue::Slash),
            StringToken::Percent => make_token(TokenValue::Percent),
            StringToken::Caret => make_token(TokenValue::Caret),
            StringToken::Not => make_token(TokenValue::Not),
            StringToken::Tilde => make_token(TokenValue::Tilde),
            StringToken::Lt => make_token(TokenValue::Lt),
            StringToken::Gt => make_token(TokenValue::Gt),
            StringToken::Assign => make_token(TokenValue::Assign),
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
