use std::{
    collections::{BTreeMap, HashMap},
    path::PathBuf,
    sync::Arc,
};

use self::{linked_ast::LiType, type_system::TyType};

use super::{
    ast::items::SyDeclarationBody,
    entity_ids::{Id, IdCounter},
    tokens::TkIdent,
    ErrorCollector,
};

mod linked_ast;
mod parser;
mod type_system;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct CodeFilePath {
    path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CodeFile {
    id: Id<CodeFile>,
    path: CodeFilePath,
    text: Arc<str>,
}

struct KnownFiles {
    counter: IdCounter<CodeFile>,
    files: HashMap<Id<CodeFile>, CodeFile>,
    paths: BTreeMap<CodeFilePath, Id<CodeFile>>,
}

impl KnownFiles {
    pub fn new() -> Self {
        Self {
            counter: IdCounter::new(),
            files: HashMap::new(),
            paths: BTreeMap::new(),
        }
    }

    pub fn add_file(&mut self, path: PathBuf, text: Arc<str>) -> Id<CodeFile> {
        let file_id = self.counter.next();

        let file = CodeFile {
            id: file_id,
            path: CodeFilePath { path },
            text,
        };

        self.paths.insert(file.path.clone(), file_id);
        self.files.insert(file_id, file);

        file_id
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
}

pub struct ItemSet<T> {
    counter: IdCounter<T>,
    types: HashMap<Id<T>, T>,
}

impl<T> ItemSet<T> {
    pub fn new() -> Self {
        Self {
            counter: IdCounter::new(),
            types: HashMap::new(),
        }
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.types.get(&id)
    }

    pub fn add_value(&mut self, ty: T) -> Id<T> {
        let id = self.counter.next();
        self.types.insert(id, ty);
        id
    }

    pub fn allocate_id(&mut self) -> Id<T> {
        let id = self.counter.next();
        id
    }

    pub fn insert_allocated_value(&mut self, id: Id<T>, ty: T) {
        let existing = self.types.insert(id, ty);
        assert!(existing.is_none());
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ItemSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.types.iter()).finish()
    }
}

impl<T> std::ops::Index<Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.types[&index]
    }
}

impl<T> std::ops::Index<&Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: &Id<T>) -> &Self::Output {
        &self.types[index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAssignability {
    pub from: Id<TyType>,
    pub to: Id<TyType>,
}

impl TypeAssignability {
    pub fn new(from: Id<TyType>, to: Id<TyType>) -> Self {
        Self { from, to }
    }
}

pub struct TypeAssignabilityCache {
    assignability_cache: HashMap<TypeAssignability, bool>,
}

impl TypeAssignabilityCache {
    pub fn new() -> Self {
        Self {
            assignability_cache: HashMap::new(),
        }
    }

    pub fn get_assignable_to_cache(&self, from: Id<TyType>, to: Id<TyType>) -> Option<bool> {
        self.assignability_cache
            .get(&TypeAssignability::new(from, to))
            .copied()
    }

    pub fn set_assignable_to(&mut self, from: Id<TyType>, to: Id<TyType>, assignable: bool) {
        self.assignability_cache
            .insert(TypeAssignability::new(from, to), assignable);
    }
}

pub struct TypeAssignabilityQuery<'a> {
    types: &'a ItemSet<TyType>,
    type_assignability: &'a mut TypeAssignabilityCache,
    parent_queries: Vec<TypeAssignability>,
}

impl<'a> TypeAssignabilityQuery<'a> {
    pub fn new(
        types: &'a ItemSet<TyType>,
        type_assignability: &'a mut TypeAssignabilityCache,
    ) -> Self {
        Self {
            types,
            type_assignability,
            parent_queries: Vec::new(),
        }
    }

    pub fn is_assignable_to(&mut self, left: Id<TyType>, right: Id<TyType>) -> bool {
        if let Some(assignable) = self.type_assignability.get_assignable_to_cache(left, right) {
            return assignable;
        }

        let assignable = self.is_assignable_to_impl(left, right);

        self.type_assignability
            .set_assignable_to(left, right, assignable);

        assignable
    }

    fn is_assignable_to_impl(&mut self, left: Id<TyType>, right: Id<TyType>) -> bool {
        if left == right {
            return true;
        }

        if self
            .parent_queries
            .contains(&TypeAssignability::new(left, right))
        {
            return true;
        }

        self.parent_queries
            .push(TypeAssignability::new(left, right));

        let left_ty = &self.types.types[&left];
        let right_ty = &self.types.types[&right];

        let assignable = left_ty.check_assignable_to(right_ty, self);

        self.parent_queries.pop();

        assignable
    }
}

pub struct CompiledFileResults {
    pub modules: HashMap<(), ModuleNamespace>,
    pub linked_type_definitions: ItemSet<LiType>,
    pub linked_type_to_type_mapping: HashMap<Id<LiType>, Id<TyType>>,
    pub types: ItemSet<TyType>,
    pub type_assignability: TypeAssignabilityCache,
    pub errors: ErrorCollector,
}

impl CompiledFileResults {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            linked_type_definitions: ItemSet::new(),
            linked_type_to_type_mapping: HashMap::new(),
            types: ItemSet::new(),
            type_assignability: TypeAssignabilityCache::new(),
            errors: ErrorCollector::new(),
        }
    }

    pub fn compile_in_ast(&mut self, ast: &SyDeclarationBody) -> Vec<Id<TyType>> {
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
    Type(Id<LiType>),
}
