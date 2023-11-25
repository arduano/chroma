use std::{ops::Range, path::Path, sync::Arc};

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

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub file: Arc<Path>,
    pub range: Range<FileLocation>,
}

impl Span {
    pub fn new(file: Arc<Path>, range: Range<FileLocation>) -> Self {
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
