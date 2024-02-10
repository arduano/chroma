use std::{
    ops::{Deref, Range},
    sync::Arc,
};

use crate::lang::solver::CodeFileRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileLocation {
    pub line: u32,
    pub column: u32,
    pub index: u32,
}

impl FileLocation {
    pub fn new(line: u32, column: u32, index: u32) -> Self {
        Self {
            line,
            column,
            index,
        }
    }
}

impl std::fmt::Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Ord for FileLocation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl PartialOrd for FileLocation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone)]
pub struct SpanInner {
    pub file: Arc<CodeFileRef>,
    pub range: Range<FileLocation>,
}

#[derive(Clone)]
pub struct Span {
    inner: Option<SpanInner>,
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        match (self.inner, other.inner) {
            (Some(self_inner), Some(other_inner)) => {
                self_inner.file.id == other_inner.file.id && self_inner.range == other_inner.range
            }
            _ => false,
        }
    }
}

impl Span {
    pub fn new(file: Arc<CodeFileRef>, range: Range<FileLocation>) -> Self {
        Self {
            inner: Some(SpanInner { file, range }),
        }
    }

    pub fn new_empty() -> Self {
        Self { inner: None }
    }

    pub fn join(&self, other: &Self) -> Self {
        let (self_inner, other_inner) = match (&self.inner, &other.inner) {
            (Some(self_inner), Some(other_inner)) => (self_inner, other_inner),
            (Some(inner), None) | (None, Some(inner)) => {
                return Self {
                    inner: Some(inner.clone()),
                }
            }
            (None, None) => return Self::new_empty(),
        };

        assert_eq!(self_inner.file.id, other_inner.file.id);

        let smaller_start = std::cmp::min(self_inner.range.start, other_inner.range.start);
        let larger_end = std::cmp::max(self_inner.range.end, other_inner.range.end);

        Self {
            inner: Some(SpanInner {
                file: self_inner.file.clone(),
                range: smaller_start..larger_end,
            }),
        }
    }
}

impl std::fmt::Debug for SpanInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.range.start;
        let end = self.range.end;

        write!(
            f,
            "{:?} {}:{}-{}:{}",
            self.file.path.path, start.line, start.column, end.line, end.column
        )
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            Some(inner) => write!(f, "{:?}", inner),
            None => write!(f, "empty span"),
        }
    }
}

pub trait ItemWithSpan {
    fn span(&self) -> Span;
}

impl<E, A: ItemWithSpan> ItemWithSpan for Result<A, E> {
    fn span(&self) -> Span {
        match self {
            Ok(item) => item.span(),
            Err(_) => Span::new_empty(),
        }
    }
}

impl<A: ItemWithSpan> ItemWithSpan for Option<A> {
    fn span(&self) -> Span {
        match self {
            Some(item) => item.span(),
            None => Span::new_empty(),
        }
    }
}

impl<A: ItemWithSpan> ItemWithSpan for Vec<A> {
    fn span(&self) -> Span {
        let mut iter = self.into_iter();
        let mut span = Span::new_empty();

        for item in iter {
            span = span.join(&item.span());
        }

        span
    }
}

impl<A: ?Sized + ItemWithSpan> ItemWithSpan for Arc<A> {
    fn span(&self) -> Span {
        self.deref().span()
    }
}
