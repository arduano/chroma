use std::sync::Arc;

use super::{Span, TokenReader, TokenValue};

// ==========
// = Traits =
// ==========

pub trait TokenItem {
    const TOKEN_LEN: usize;
    fn span(&self) -> Span;
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

pub trait ParseSimpleToken: TokenItem
where
    Self: Sized,
{
    fn parse(reader: &mut TokenReader) -> Option<Self>;
}

pub trait ParseGroupToken: TokenItem
where
    Self: Sized,
{
    fn parse<'a>(reader: &mut TokenReader<'a>) -> Option<(Self, TokenReader<'a>)>;
}

// ========================
// = Advanced Token Types =
// ========================

pub struct TIdent {
    span: Span,
    ident: Arc<str>,
}

impl TIdent {
    pub fn ident(&self) -> &Arc<str> {
        &self.ident
    }
}

impl TokenItem for TIdent {
    const TOKEN_LEN: usize = 1;
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TIdent {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
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

pub struct TInteger {
    span: Span,
    value: i64,
}

impl TInteger {
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl TokenItem for TInteger {
    const TOKEN_LEN: usize = 1;
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TInteger {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
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

pub struct TFloat {
    span: Span,
    value: f64,
}

impl TFloat {
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl TokenItem for TFloat {
    const TOKEN_LEN: usize = 1;
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TFloat {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
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

pub struct TString {
    span: Span,
    value: Arc<str>,
}

impl TString {
    pub fn value(&self) -> &Arc<str> {
        &self.value
    }
}

impl TokenItem for TString {
    const TOKEN_LEN: usize = 1;
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ParseSimpleToken for TString {
    fn parse(reader: &mut TokenReader) -> Option<Self> {
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

// ==========
// = Macros =
// ==========

macro_rules! simple_token {
    ($name:ident, $($token:ident),*) => {
        pub struct $name(Span);

        impl $name {
            const MATCHES: &'static [TokenValue] = &[$(TokenValue::$token),*];
        }

        impl TokenItem for $name {
            const TOKEN_LEN: usize = Self::MATCHES.len();
            fn span(&self) -> Span {
                self.0.clone()
            }
        }

        impl ParseSimpleToken for $name {
            fn parse(reader: &mut TokenReader) -> Option<Self> {
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
    }
}

macro_rules! simple_ident_token {
    ($name:ident, $matches:expr) => {
        pub struct $name(Span);

        impl $name {
            const MATCHES: &'static str = $matches;
        }

        impl TokenItem for $name {
            const TOKEN_LEN: usize = 1;
            fn span(&self) -> Span {
                self.0.clone()
            }
        }

        impl ParseSimpleToken for $name {
            fn parse(reader: &mut TokenReader) -> Option<Self> {
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
    };
}

macro_rules! group_token {
    ($name:ident, $token:ident) => {
        pub struct $name {
            span: Span,
        }

        impl TokenItem for $name {
            const TOKEN_LEN: usize = 1;
            fn span(&self) -> Span {
                self.span.clone()
            }
        }

        impl ParseGroupToken for $name {
            fn parse<'a>(reader: &mut TokenReader<'a>) -> Option<(Self, TokenReader<'a>)> {
                let next_token = reader.next_token()?;

                let TokenValue::$token(g) = &next_token.value else {
                    return None;
                };

                let token = Self {
                    span: next_token.span.clone(),
                };
                let inner_reader = TokenReader::new(&g);

                reader.skip(1);
                Some((token, inner_reader))
            }
        }
    };
}

// ================
// = Macro Tokens =
// ================

// Groups
group_token!(TParens, Parens);
group_token!(TBraces, Braces);
group_token!(TBrackets, Brackets);

// Arithmetic
simple_token!(TPlus, Plus);
simple_token!(TPlusAssign, Plus, Eq);
simple_token!(TMinus, Minus);
simple_token!(TMinusAssign, Minus, Eq);
simple_token!(TMult, Star);
simple_token!(TMultAssign, Star, Eq);
simple_token!(TDiv, Slash);
simple_token!(TDivAssign, Slash, Eq);
simple_token!(TModulo, Percent);
simple_token!(TModuloAssign, Percent, Eq);

// Logic
simple_token!(TAnd, And, And);
simple_token!(TBitAnd, And);
simple_token!(TOr, Or, Or);
simple_token!(TBitOr, Or);
simple_token!(TBitXor, Caret);
simple_token!(TNot, Exclamation);

// Comparison
simple_token!(TLessThan, Lt);
simple_token!(TLessThanEq, Lt, Eq);
simple_token!(TGreaterThan, Gt);
simple_token!(TGreaterThanEq, Gt, Eq);
simple_token!(TEquals, Eq, Eq);
simple_token!(TNotEquals, Exclamation, Eq);

// Named tokens
simple_ident_token!(TFn, "fn");
simple_ident_token!(TLet, "let");
simple_ident_token!(TIf, "if");
simple_ident_token!(TElse, "else");
simple_ident_token!(TWhile, "while");
simple_ident_token!(TFor, "for");
simple_ident_token!(TIn, "in");
simple_ident_token!(TBreak, "break");
simple_ident_token!(TContinue, "continue");
simple_ident_token!(TReturn, "return");
simple_ident_token!(TTrue, "true");
simple_ident_token!(TFalse, "false");
