use std::collections::HashMap;

use crate::lang::solver_old::Id;

use super::CodeFile;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct KnownFileGroup {
    /// A *sorted* vec of file IDs.
    files: Vec<Id<CodeFile>>,
}

impl KnownFileGroup {
    pub fn new(mut files: Vec<Id<CodeFile>>) -> Self {
        files.sort();
        Self { files }
    }

    pub fn files(&self) -> &[Id<CodeFile>] {
        &self.files
    }
}

pub struct KnownFileGroups {
    group_map: HashMap<KnownFileGroup, Id<KnownFileGroup>>,
    reverse_group_map: HashMap<Id<KnownFileGroup>, KnownFileGroup>,
    dependencies: HashMap<Id<KnownFileGroup>, Vec<Id<KnownFileGroup>>>,
    reverse_dependencies: HashMap<Id<KnownFileGroup>, Vec<Id<KnownFileGroup>>>,
}
