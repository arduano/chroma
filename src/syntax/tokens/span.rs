use std::{
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

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

pub struct FileRef {
    pub path: PathBuf,
    pub contents: String,
}

impl std::fmt::Debug for FileRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FileRef")
            .field("path", &self.path)
            .field("contents", &"[omitted]")
            .finish()
    }
}

#[derive(Clone)]
pub struct Span {
    pub file: Arc<FileRef>,
    pub range: Range<FileLocation>,
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.file, &other.file) && self.range == other.range
    }
}

impl Span {
    pub fn new(file: Arc<FileRef>, range: Range<FileLocation>) -> Self {
        Self { file, range }
    }

    pub fn join(&self, other: &Self) -> Self {
        assert!(Arc::ptr_eq(&self.file, &other.file));

        let smaller_start = std::cmp::min(self.range.start, other.range.start);
        let larger_end = std::cmp::max(self.range.end, other.range.end);

        Self {
            file: self.file.clone(),
            range: smaller_start..larger_end,
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.range.start;
        let end = self.range.end;

        let str = &self.file.contents[start.index as usize..end.index as usize];

        write!(
            f,
            "{:?} {:?} {}:{}-{}:{}",
            str, self.file.path, start.line, start.column, end.line, end.column
        )
    }
}
