use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroU32,
    path::PathBuf,
    sync::Arc,
};

use crate::lang::solver_old::{Id, IdCounter};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CodeFilePath {
    pub path: PathBuf,
}

impl CodeFilePath {
    pub fn from_path(path: PathBuf) -> Self {
        Self { path }
    }

    pub fn from_str(path: &str) -> Self {
        Self {
            path: PathBuf::from(path),
        }
    }

    pub fn new_empty_internal() -> Self {
        Self {
            path: PathBuf::from("internal"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CodeFileRef {
    // TODO: In the future, don't store path here, especially when serializing/deserializing.
    pub path: CodeFilePath,
    pub id: Id<CodeFile>,
}

impl CodeFileRef {
    pub fn new(path: CodeFilePath, id: Id<CodeFile>) -> Self {
        Self { path, id }
    }

    pub fn new_empty_internal() -> Self {
        Self {
            path: CodeFilePath::new_empty_internal(),
            id: Id::new(NonZeroU32::MAX),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CodeFile {
    id: Id<CodeFile>,
    path: CodeFilePath,
    text: Arc<str>,
}

pub struct KnownFiles {
    counter: IdCounter<CodeFile>,
    files: HashMap<Id<CodeFile>, CodeFile>,
    paths: BTreeMap<CodeFilePath, Id<CodeFile>>,
    std_file_id: Id<CodeFile>,
}

impl KnownFiles {
    pub fn new() -> Self {
        let mut counter = IdCounter::new();
        let std_file_id = counter.next();

        Self {
            counter,
            files: HashMap::new(),
            paths: BTreeMap::new(),
            std_file_id,
        }
    }

    pub fn add_file(&mut self, path: CodeFilePath, text: Arc<str>) -> Id<CodeFile> {
        let file_id = self.counter.next();

        let file = CodeFile {
            id: file_id,
            path,
            text,
        };

        self.paths.insert(file.path.clone(), file_id);
        self.files.insert(file_id, file);

        file_id
    }

    pub fn get_ref_for_file_id(&self, file_id: Id<CodeFile>) -> CodeFileRef {
        let file = self.files.get(&file_id).unwrap();
        CodeFileRef::new(file.path.clone(), file_id)
    }

    pub fn update_file(&mut self, file_id: Id<CodeFile>, new_text: Arc<str>) -> Id<CodeFile> {
        let current_file = self.files.remove(&file_id).unwrap();

        let new_file_id = self.counter.next();

        let new_file = CodeFile {
            id: new_file_id,
            path: current_file.path.clone(),
            text: new_text,
        };

        self.paths.insert(new_file.path.clone(), new_file_id);
        self.files.insert(new_file_id, new_file);

        new_file_id
    }

    pub fn get_file_by_id(&self, file_id: Id<CodeFile>) -> &CodeFile {
        self.files.get(&file_id).unwrap()
    }

    pub fn get_file_by_path(&self, path: &CodeFilePath) -> &CodeFile {
        self.files.get(&self.paths[path]).unwrap()
    }

    pub fn make_or_get_std_file(&mut self) -> Id<CodeFile> {
        let path = CodeFilePath::from_str("$$std");
        if let Some(std_file_id) = self.paths.get(&path) {
            return *std_file_id;
        }

        let std_file_id = self.add_file(path, Arc::from(""));

        std_file_id
    }
}
