use std::sync::Arc;

use super::{Span, TokenReader, TokenValue};

// ==========
// = Traits =
// ==========

pub trait TokenItem {
    fn span(&self) -> Span;
}

pub trait DisplayStatic {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn displayed() -> StaticDisplay<Self> {
        StaticDisplay(std::marker::PhantomData::<Self>)
    }
}

pub struct StaticDisplay<T: DisplayStatic + ?Sized>(std::marker::PhantomData<T>);
impl<T: DisplayStatic> std::fmt::Display for StaticDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        T::display(f)
    }
}
impl<T: DisplayStatic> std::fmt::Debug for StaticDisplay<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        T::display(f)
    }
}

pub trait TestTokenValue: TokenItem {
    fn test(reader: &TokenReader) -> bool;
}

impl<T: ParseSimpleToken> TestTokenValue for T {
    fn test(reader: &TokenReader) -> bool {
        let mut reader = reader.clone();
        Self::parse(&mut reader).is_some()
    }
}

pub trait ParseSimpleToken: TokenItem + DisplayStatic
where
    Self: Sized,
{
    fn parse(reader: &mut TokenReader) -> Option<Self>;
}

pub trait ParseGroupToken: TokenItem + DisplayStatic
where
    Self: Sized,
{
    fn parse<'a>(reader: &mut TokenReader<'a>) -> Option<(Self, TokenReader<'a>)>;
}

// ================
// = Special/Meta =
// ================

#[derive(Debug, Clone, PartialEq)]
pub struct TkBlockLineEndSearch {
    span: Span,
}

impl TokenItem for TkBlockLineEndSearch {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkBlockLineEndSearch {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        // Don't trim, as trimming will skip newlines.
        let next_token = reader.next_token()?;

        if next_token.value == TokenValue::Newline || next_token.value == TokenValue::Semi {
            reader.skip(1);
            return Some(Self {
                span: next_token.span.clone(),
            });
        };

        None
    }
}

impl DisplayStatic for TkBlockLineEndSearch {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ";")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TkDataLineEndSearch {
    span: Span,
}

impl TokenItem for TkDataLineEndSearch {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkDataLineEndSearch {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        // Don't trim, as trimming will skip newlines.
        let next_token = reader.next_token()?;

        if next_token.value == TokenValue::Newline || next_token.value == TokenValue::Comma {
            reader.skip(1);
            return Some(Self {
                span: next_token.span.clone(),
            });
        };

        None
    }
}

impl DisplayStatic for TkDataLineEndSearch {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ",")
    }
}

// ========================
// = Advanced Token Types =
// ========================

#[derive(Debug, Clone, PartialEq)]
pub struct TkIdent {
    pub span: Span,
    pub ident: Arc<str>,
}

impl TkIdent {
    pub fn ident(&self) -> &Arc<str> {
        &self.ident
    }

    pub fn new_from_str(ident: &str) -> Self {
        Self {
            span: Span::new_empty(),
            ident: ident.into(),
        }
    }
}

impl TokenItem for TkIdent {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkIdent {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        reader.trim_empty();
        let next_token = reader.next_token()?;

        let TokenValue::Ident(s) = &next_token.value else {
            return None;
        };

        reader.skip(1);
        Some(Self {
            span: next_token.span.clone(),
            ident: s.clone(),
        })
    }
}

impl DisplayStatic for TkIdent {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ident}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TkInteger {
    span: Span,
    value: i64,
}

impl TkInteger {
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl TokenItem for TkInteger {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkInteger {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        reader.trim_empty();
        let next_token = reader.next_token()?;

        let TokenValue::Integer(i) = &next_token.value else {
            return None;
        };

        reader.skip(1);
        Some(Self {
            span: next_token.span.clone(),
            value: *i,
        })
    }
}

impl DisplayStatic for TkInteger {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{integer}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TkFloat {
    span: Span,
    value: f64,
}

impl TkFloat {
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl TokenItem for TkFloat {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkFloat {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        reader.trim_empty();
        let next_token = reader.next_token()?;

        let TokenValue::Float(i) = &next_token.value else {
            return None;
        };

        reader.skip(1);
        Some(Self {
            span: next_token.span.clone(),
            value: *i,
        })
    }
}

impl DisplayStatic for TkFloat {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{float}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TkString {
    span: Span,
    value: Arc<str>,
}

impl TkString {
    pub fn value(&self) -> &Arc<str> {
        &self.value
    }
}

impl TokenItem for TkString {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TkString {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
        reader.trim_empty();
        let next_token = reader.next_token()?;

        let TokenValue::String(s) = &next_token.value else {
            return None;
        };

        reader.skip(1);
        Some(Self {
            span: next_token.span.clone(),
            value: s.clone(),
        })
    }
}

impl DisplayStatic for TkString {
    fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{string}}")
    }
}

// ==========
// = Macros =
// ==========

macro_rules! simple_token {
    ($name:ident, $($token:ident),*) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $name(Span);

        impl $name {
            const MATCHES: &'static [TokenValue] = &[$(TokenValue::$token),*];
        }

        impl TokenItem for $name {
            fn span(&self) -> Span {
                self.0.clone()
            }
        }

        impl ParseSimpleToken for $name {
            fn parse(reader: &mut TokenReader) -> Option<Self> {
        reader.trim_empty();
                let matches = Self::MATCHES;

                let next_tokens = reader.remaining_tokens_slice();
                if next_tokens.len() < matches.len() {
                    return None;
                }

                let span = if matches.len() == 1 {
                    next_tokens[0].span.clone()
                } else {
                    next_tokens[0]
                        .span
                        .join(&next_tokens[matches.len() - 1].span)
                };

                let next_tokens = &next_tokens[..matches.len()];

                for (token, matches) in next_tokens.iter().zip(matches) {
                    if token.value != *matches {
                        return None;
                    }
                }

                reader.skip(matches.len());
                Some(Self(span))
            }
        }

        impl DisplayStatic for $name {
            fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let matches = Self::MATCHES;
                for token in matches.iter() {
                    write!(f, "{}", token)?;
                }
                Ok(())
            }
        }
    }
}

macro_rules! simple_ident_token {
    ($name:ident, $matches:expr) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $name(Span);

        impl $name {
            const MATCHES: &'static str = $matches;
        }

        impl TokenItem for $name {
            fn span(&self) -> Span {
                self.0.clone()
            }
        }

        impl ParseSimpleToken for $name {
            fn parse(reader: &mut TokenReader) -> Option<Self> {
                reader.trim_empty();
                let next_token = reader.next_token()?;

                let TokenValue::Ident(s) = &next_token.value else {
                    return None;
                };

                if &**s != Self::MATCHES {
                    return None;
                }

                reader.skip(1);
                Some(Self(next_token.span.clone()))
            }
        }

        impl DisplayStatic for $name {
            fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", Self::MATCHES)
            }
        }
    };
}

macro_rules! group_token {
    ($name:ident, $token:ident, $display: expr) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $name {
            span: Span,
        }

        impl TokenItem for $name {
            fn span(&self) -> Span {
                self.span.clone()
            }
        }

        impl ParseGroupToken for $name {
            fn parse<'a>(reader: &mut TokenReader<'a>) -> Option<(Self, TokenReader<'a>)> {
                reader.trim_empty();
                let next_token = reader.next_token()?;

                let TokenValue::$token(g) = &next_token.value else {
                    return None;
                };

                let token = Self {
                    span: next_token.span.clone(),
                };
                let inner_reader = TokenReader::new_grpuped(&g);

                reader.skip(1);
                Some((token, inner_reader))
            }
        }

        impl DisplayStatic for $name {
            fn display(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $display)
            }
        }
    };
}

// ================
// = Macro Tokens =
// ================

// Groups
group_token!(TkParens, Parens, "parentheses");
group_token!(TkBraces, Braces, "braces");
group_token!(TkBrackets, Brackets, "brackets");

// Arithmetic
simple_token!(TkPlus, Plus);
simple_token!(TkPlusAssign, Plus, Eq);
simple_token!(TkMinus, Minus);
simple_token!(TkMinusAssign, Minus, Eq);
simple_token!(TkMult, Star);
simple_token!(TkMultAssign, Star, Eq);
simple_token!(TkDiv, Slash);
simple_token!(TkDivAssign, Slash, Eq);
simple_token!(TkModulo, Percent);
simple_token!(TkModuloAssign, Percent, Eq);

// Logic
simple_token!(TkAnd, And, And);
simple_token!(TkBitAnd, And);
simple_token!(TkOr, Or, Or);
simple_token!(TkBitOr, Or);
simple_token!(TkBitXor, Caret);
simple_token!(TkNot, Exclamation);

// Comparison
simple_token!(TkLessThan, Lt);
simple_token!(TkLessThanEq, Lt, Eq);
simple_token!(TkGreaterThan, Gt);
simple_token!(TkGreaterThanEq, Gt, Eq);
simple_token!(TkEquals, Eq, Eq);
simple_token!(TkNotEquals, Exclamation, Eq);

// Other
simple_token!(TkAssign, Eq);
simple_token!(TkColon, Colon);
simple_token!(TkSemicolon, Semi);
simple_token!(TkComma, Comma);
simple_token!(TkDot, Dot);
simple_token!(TkEpsilon, Dot, Dot, Dot);
simple_token!(TkAt, At);
simple_token!(TkHash, Hash);

// Named tokens
simple_ident_token!(TkFn, "fn");
simple_ident_token!(TkType, "type");
simple_ident_token!(TkConst, "const");
simple_ident_token!(TkLet, "let");
simple_ident_token!(TkIf, "if");
simple_ident_token!(TkElse, "else");
simple_ident_token!(TkWhile, "while");
simple_ident_token!(TkFor, "for");
simple_ident_token!(TkIn, "in");
simple_ident_token!(TkBreak, "break");
simple_ident_token!(TkContinue, "continue");
simple_ident_token!(TkReturn, "return");
simple_ident_token!(TkTrue, "true");
simple_ident_token!(TkFalse, "false");
simple_ident_token!(TkExtends, "extends");
simple_ident_token!(TkTypeof, "typeof");
simple_ident_token!(TkMod, "mod");
simple_ident_token!(TkUse, "use");
simple_ident_token!(TkAs, "as");
simple_ident_token!(TkPub, "pub");
