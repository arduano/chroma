use std::{collections::HashMap, sync::Arc};

use self::{
    linked_ast::LiExpression,
    type_system::{
        TyType, TyTypeFlags, TyTypeLogic, TypeAssignabilityCache, TypeSubsetabilityCache,
    },
};

use super::{ast::items::SyDeclarationBody, tokens::Span, ErrorCollector};

mod entity_ids;
pub use entity_ids::*;
mod linked_ast;
mod lowering;
mod type_system;
pub use type_system::*;
mod namespaces;
pub use namespaces::*;
mod runtime;
pub use runtime::*;

mod file_graph;
pub use file_graph::*;

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

    pub fn try_get_flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags {
        match &self.ty {
            MIdOrVal::Id(id) => match types.get(*id) {
                Some(ty) => ty.flags(types),
                None => TyTypeFlags::new_for_unknown(),
            },
            MIdOrVal::Val(val) => val.flags(types),
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

    pub fn with_new_span(&self, span: Span) -> Self {
        Self { span, id: self.id }
    }
}

pub struct NormalizedTypeData {
    pub inner_types: Vec<MId<TyType>>,
}
pub struct TypeData {
    pub types: ModItemSet<TyType>,
    pub type_assignability: TypeAssignabilityCache,
    pub type_subsetability: TypeSubsetabilityCache,
}

impl TypeData {
    pub fn new(types: ModItemSet<TyType>) -> Self {
        Self {
            types,
            type_assignability: TypeAssignabilityCache::new(),
            type_subsetability: TypeSubsetabilityCache::new(),
        }
    }
}

pub struct ModuleGroupCompilation {
    pub current_module_id: ModId,
    pub files: Vec<Id<CodeFile>>,
    pub modules: HashMap<Id<CodeFile>, ModuleNamespace>,
    pub linked_type_definitions: ModItemSet<LiExpression>,
    pub type_data: TypeData,
    pub linked_type_to_type_mapping: HashMap<MId<LiExpression>, MId<TyType>>,
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
            .items()
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
    pub linked_type_definitions: Arc<ItemSet<MIdOrVal<LiExpression>>>,
    pub linked_type_to_type_mapping: HashMap<Id<LiExpression>, Id<TyType>>,
    pub types: Arc<ItemSet<MIdOrVal<TyType>>>,
    pub type_assignability: TypeAssignabilityCache,
    pub errors: ErrorCollector,
}
