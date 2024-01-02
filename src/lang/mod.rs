use std::{
    backtrace::Backtrace,
    borrow::Cow,
    fmt::{Debug, Formatter},
    future::Future,
    mem::MaybeUninit,
    pin::Pin,
    sync::{atomic::AtomicBool, Arc, Mutex},
    task::{Context, Poll},
};

use self::tokens::Span;

pub mod ast;
pub mod entity_ids;
mod ident_matcher;
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

struct Shared<T: Sync + Send> {
    value: Arc<T>,
}

impl<T: Sync + Send> Shared<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    /// Gets the pointer as usize
    pub fn id(&self) -> usize {
        Arc::as_ptr(&self.value) as usize
    }
}

impl<T: Sync + Send> std::ops::Deref for Shared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Debug + Sync + Send> Debug for Shared<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Shared").field(&self.value).finish()
    }
}

impl<T: Clone + Send + Sync> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}
