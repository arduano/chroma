use std::{backtrace::Backtrace, borrow::Cow, collections::BTreeMap, path::PathBuf, sync::Arc};

use self::{ast::items::STypeDefine, tokens::Span};

pub mod ast;
pub mod entity_ids;
pub mod ident_matcher;
pub mod modules;
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
