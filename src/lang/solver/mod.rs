use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use self::{linked_ast::LiType, type_system::TyType};

use super::{
    ast::items::SyDeclarationBody,
    tokens::{Span, TkIdent},
    ErrorCollector,
};

mod entity_ids;
pub use entity_ids::*;
mod linked_ast;
mod lowering;
mod type_system;

mod file_graph;
pub use file_graph::*;
mod type_assignability;
pub use type_assignability::*;

/// A "Module-associated ID", an ID that is associated with a module.
/// This allows any module to refer to another module's items.
pub type MId<T> = Id2<ModuleGroupCompilation, T>;

/// Same as MId, but can either contain a value or an ID.
pub type MIdOrVal<T> = Id2OrVal<ModuleGroupCompilation, T>;

/// A module group ID
pub type ModId = Id<ModuleGroupCompilation>;

/// A set of items addressed by both their ID, as well as their module group ID.
pub type ModItemSet<T> = GroupItemSetWithRefs<ModuleGroupCompilation, T>;

#[derive(Debug, Clone)]
pub struct TyIdOrValWithSpan {
    pub span: Span,
    pub ty: MIdOrVal<TyType>,
}

impl TyIdOrValWithSpan {
    pub fn new(ty: MIdOrVal<TyType>, span: Span) -> Self {
        Self { span, ty }
    }

    pub fn new_val(ty: TyType, span: Span) -> Self {
        Self {
            span,
            ty: MIdOrVal::Val(ty),
        }
    }

    pub fn new_id(ty: MId<TyType>, span: Span) -> Self {
        Self {
            span,
            ty: MIdOrVal::Id(ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeIdWithSpan {
    pub span: Span,
    pub id: MId<TyType>,
}

impl TypeIdWithSpan {
    pub fn new(ty: MId<TyType>, span: Span) -> Self {
        Self { span, id: ty }
    }

    pub fn as_type_id_or_val(&self) -> TyIdOrValWithSpan {
        TyIdOrValWithSpan::new_id(self.id, self.span.clone())
    }
}

pub struct NormalizedTypeData {
    pub inner_types: Vec<MId<TyType>>,
}

pub struct TypeData {
    pub types: ModItemSet<TyType>,
    pub type_assignability: TypeAssignabilityCache,
    pub type_subsetability: TypeSubsetabilityCache,
    pub already_normalized_types: HashSet<MId<TyType>>,
}

impl TypeData {
    pub fn new(types: ModItemSet<TyType>) -> Self {
        Self {
            types,
            type_assignability: TypeAssignabilityCache::new(),
            type_subsetability: TypeSubsetabilityCache::new(),
            already_normalized_types: HashSet::new(),
        }
    }
}

pub struct ModuleGroupCompilation {
    pub current_module_id: ModId,
    pub files: Vec<Id<CodeFile>>,
    pub modules: HashMap<Id<CodeFile>, ModuleNamespace>,
    pub linked_type_definitions: ModItemSet<LiType>,
    pub type_data: TypeData,
    pub linked_type_to_type_mapping: HashMap<MId<LiType>, MId<TyType>>,
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
            type_data: TypeData::new(ModItemSet::new(HashMap::new(), id)),
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
            type_data: TypeData::new(ModItemSet::new(
                get_module_item_set(&past_compilations, |comp| &comp.types),
                id,
            )),
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

        let mod_results = lowering::parse_module_decls(ast, self);

        let namespace_types = mod_results
            .namespace
            .items
            .values()
            .filter_map(|item| match item.kind {
                ModuleNamespaceItemKind::Type(id) => Some(id),
                _ => None,
            })
            .collect::<Vec<_>>();

        lowering::parse_module_data_linking(ast, self, mod_results);

        let mut types = Vec::new();

        for li_type_id in namespace_types {
            let id = lowering::get_type_id_for_linked_type_id(self, li_type_id);
            types.push(id);
        }

        types
    }
}

pub struct ModuleGroupResult {
    pub dependencies: Vec<Id<ModuleGroupResult>>,
    pub files: Vec<Id<CodeFile>>,
    pub modules: HashMap<Id<CodeFile>, ModuleNamespace>,
    pub linked_type_definitions: Arc<ItemSet<MIdOrVal<LiType>>>,
    pub linked_type_to_type_mapping: HashMap<Id<LiType>, Id<TyType>>,
    pub types: Arc<ItemSet<MIdOrVal<TyType>>>,
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
