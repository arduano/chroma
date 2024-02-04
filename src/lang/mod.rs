use std::{backtrace::Backtrace, borrow::Cow, fmt::Debug, sync::Arc};

use self::tokens::Span;

pub mod ast;
pub mod solver;
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

#[derive(Debug, Clone)]
pub struct CompilerError {
    message: Cow<'static, str>,
    span: Span,
    backtrace: Arc<Backtrace>,
}

impl CompilerError {
    pub fn new(message: impl Into<Cow<'static, str>>, span: Span) -> Self {
        Self {
            message: message.into(),
            backtrace: Arc::new(Backtrace::capture()),
            span,
        }
    }
}

#[derive(Clone)]
pub struct ErrorCollector {
    errors: Arc<boxcar::Vec<CompilerError>>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self {
            errors: Arc::new(boxcar::Vec::new()),
        }
    }

    pub fn push(&self, error: CompilerError) {
        self.errors.push(error);
    }

    pub fn errors(&self) -> Vec<CompilerError> {
        self.errors
            .iter()
            .map(|(_, e)| e.clone())
            .collect::<Vec<_>>()
    }
}
