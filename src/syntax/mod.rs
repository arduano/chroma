use std::borrow::Cow;

use self::tokens::Span;

pub mod ast;
pub mod tokens;

#[derive(Debug, Clone, PartialEq)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub fn new(value: impl Into<T>, span: Span) -> Self {
        Self {
            value: value.into(),
            span,
        }
    }
}

pub type CompilerError = WithSpan<Cow<'static, str>>;
