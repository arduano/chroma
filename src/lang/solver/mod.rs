use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroU32,
    path::PathBuf,
    sync::Arc,
};

use self::{linked_ast::LiType, type_system::TyType};

use super::{ast::items::SyDeclarationBody, tokens::TkIdent, ErrorCollector};

mod entity_ids;
pub use entity_ids::*;
mod linked_ast;
mod parser;
mod type_assignability;
use type_assignability::*;
mod type_system;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CodeFilePath {
    path: PathBuf,
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
    id: Id<CodeFile>,
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

/// A "Module-associated ID", an ID that is associated with a module.
/// This allows any module to refer to another module's items.
pub type MId<T> = Id2<ModuleGroupCompilation, T>;

/// A module group ID
pub type ModId = Id<ModuleGroupCompilation>;

/// A set of items addressed by both their ID, as well as their module group ID.
pub type ModItemSet<T> = GroupItemSet<ModuleGroupCompilation, T>;

pub struct ModuleGroupCompilation {
    pub current_module_id: ModId,
    pub files: Vec<Id<CodeFile>>,
    pub modules: HashMap<Id<CodeFile>, ModuleNamespace>,
    pub linked_type_definitions: ModItemSet<LiType>,
    pub linked_type_to_type_mapping: HashMap<MId<LiType>, MId<TyType>>,
    pub types: ModItemSet<TyType>,
    pub type_assignability: TypeAssignabilityCache,
    pub errors: ErrorCollector,
}

impl ModuleGroupCompilation {
    pub fn new_without_deps(id: ModId) -> Self {
        Self {
            current_module_id: id,
            files: Vec::new(),
            modules: HashMap::new(),
            linked_type_definitions: ModItemSet::new(HashMap::new(), id),
            linked_type_to_type_mapping: HashMap::new(),
            types: ModItemSet::new(HashMap::new(), id),
            type_assignability: TypeAssignabilityCache::new(),
            errors: ErrorCollector::new(),
        }
    }

    pub fn new(id: ModId, past_compilations: HashMap<ModId, ModuleGroupResult>) -> Self {
        fn get_module_item_set<T>(
            past_compilations: &HashMap<ModId, ModuleGroupResult>,
            query: impl Fn(&ModuleGroupResult) -> &Arc<ItemSet<T>>,
        ) -> HashMap<ModId, Arc<ItemSet<T>>> {
            let mut result = HashMap::new();

            for (id, compilation) in past_compilations {
                result.insert(*id, query(compilation).clone());
            }

            result
        }

        Self {
            current_module_id: id,
            files: Vec::new(),
            modules: HashMap::new(),
            linked_type_definitions: ModItemSet::new(
                get_module_item_set(&past_compilations, |comp| &comp.linked_type_definitions),
                id,
            ),
            linked_type_to_type_mapping: HashMap::new(),
            types: ModItemSet::new(
                get_module_item_set(&past_compilations, |comp| &comp.types),
                id,
            ),
            type_assignability: TypeAssignabilityCache::new(),
            errors: ErrorCollector::new(),
        }
    }

    pub fn compile_in_ast(
        &mut self,
        file: Option<Id<CodeFile>>,
        ast: &SyDeclarationBody,
    ) -> Vec<MId<TyType>> {
        if let Some(file) = file {
            self.files.push(file);
        }

        let mod_results = parser::parse_module_decls(ast, self);

        let namespace_types = mod_results
            .namespace
            .items
            .values()
            .filter_map(|item| match item.kind {
                ModuleNamespaceItemKind::Type(id) => Some(id),
                _ => None,
            })
            .collect::<Vec<_>>();

        parser::parse_module_data_linking(ast, self, mod_results);

        let mut types = Vec::new();

        for li_type_id in namespace_types {
            let id = parser::get_type_id_for_linked_type_id(self, li_type_id);
            types.push(id);
        }

        types
    }
}

pub struct ModuleGroupResult {
    pub dependencies: Vec<Id<ModuleGroupResult>>,
    pub files: Vec<Id<CodeFile>>,
    pub modules: HashMap<Id<CodeFile>, ModuleNamespace>,
    pub linked_type_definitions: Arc<ItemSet<LiType>>,
    pub linked_type_to_type_mapping: HashMap<Id<LiType>, Id<TyType>>,
    pub types: Arc<ItemSet<TyType>>,
    pub type_assignability: TypeAssignabilityCache,
    pub errors: ErrorCollector,
}

pub struct ModuleNamespace {
    items: HashMap<Arc<str>, ModuleNamespaceItem>,
}

impl ModuleNamespace {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn add_item(&mut self, item: ModuleNamespaceItem) {
        let existing = self.items.insert(item.ident.ident.clone(), item);
        assert!(existing.is_none());
    }

    pub fn get_ident_kind(&self, ident: &TkIdent) -> Option<ModuleNamespaceItemKind> {
        Some(self.items.get(&ident.ident)?.kind)
    }

    pub fn get_item(&self, ident: &Arc<str>) -> Option<&ModuleNamespaceItem> {
        self.items.get(ident)
    }
}

pub struct ModuleNamespaceItem {
    ident: TkIdent,
    kind: ModuleNamespaceItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleNamespaceItemKind {
    Type(MId<LiType>),
    Unknown,
}
