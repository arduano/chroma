use std::{collections::HashMap, sync::Arc};

use self::{
    linked_ast::LiExpression,
    type_system::{TyType, TyTypeLogic, TypeAssignabilityCache, TypeSubsetabilityCache},
};

use super::{ast::items::SyDeclarationBody, tokens::Span, ErrorCollector};

mod entity_ids;
pub use entity_ids::*;
mod linked_ast;
mod lowering;
mod type_system;
pub use type_system::*;
mod type_system_2;
pub use type_system_2::*;
mod namespaces;
pub use namespaces::*;
mod runtime;
pub use runtime::*;

mod file_graph;
pub use file_graph::*;

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
    pub linked_type_definitions: ModItemSet<LiExpression>,
    pub type_data: TypeData,
    pub type_relationships: TypeRelationships,
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
            type_data: ModItemSet::new(HashMap::new(), id),
            type_relationships: TypeRelationships::new(),
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

        todo!();

        // Self {
        //     current_module_id: id,
        //     files: Vec::new(),
        //     modules: HashMap::new(),
        //     linked_type_definitions: ModItemSet::new(
        //         get_module_item_set(&past_compilations, |comp| &comp.linked_type_definitions),
        //         id,
        //     ),
        //     linked_type_to_type_mapping: HashMap::new(),
        //     type_data: ModItemSet::new(
        //         get_module_item_set(&past_compilations, |comp| &comp.types),
        //         id,
        //     ),
        //     type_relationships: TypeRelationships::new(),
        //     errors: ErrorCollector::new(),
        // }
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
    pub linked_type_definitions: Arc<ItemSet<MId<LiExpression>>>,
    pub linked_type_to_type_mapping: HashMap<Id<LiExpression>, Id<TyType>>,
    pub types: Arc<ItemSet<MId<TyType>>>,
    pub type_assignability: TypeAssignabilityCache,
    pub errors: ErrorCollector,
}
