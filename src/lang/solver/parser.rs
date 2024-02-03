use std::collections::HashMap;

use crate::lang::{
    ast::{
        helpers::Attempted,
        items::{SyDeclaration, SyDeclarationBody, SyExpression, SyObjectLiteralField},
    },
    entity_ids::Id,
    tokens::TkIdent,
    CompilerError,
};

use self::{
    link_type_expressions::link_type_expression_ast,
    parse_static_types::{parse_type_from_linked_type_id, TypeFromLinkedTypeCompilation},
};

use super::{
    linked_ast::*, type_system::TyType, CompiledFileResults, ModuleNamespace, ModuleNamespaceItem,
    ModuleNamespaceItemKind,
};

mod link_type_expressions;
mod parse_static_types;

pub struct ModuleParseResult {
    /// Namespace of the current module
    pub namespace: ModuleNamespace,

    /// The paths of other modules that this module references, for dependency management.
    pub module_path_refs: (),

    /// Cache of module items to compile later
    pub items_to_compile: Vec<ModuleItem>,
}

pub struct ModuleItem {
    index_in_body: usize,
    id: ModuleNamespaceItemKind,
}

pub fn parse_module_decls(
    ast: &SyDeclarationBody,
    compilation: &mut CompiledFileResults,
) -> ModuleParseResult {
    let module_path_refs = ();
    let mut namespace_items = HashMap::new();
    let mut items_to_compile = Vec::new();

    for (i, statement) in ast
        .statements
        .iter()
        .enumerate()
        .filter_map(|(i, s)| Some((i, s.as_ref().ok()?)))
    {
        match &statement.item {
            SyDeclaration::TypeDefine(ty) => {
                let ident = ty.name.clone();

                let id = ModuleNamespaceItemKind::Type(
                    compilation.linked_type_definitions.allocate_id(),
                );

                let item = ModuleNamespaceItem {
                    ident: ident.clone(),
                    kind: id,
                };

                items_to_compile.push(ModuleItem {
                    index_in_body: i,
                    id,
                });
                namespace_items.insert(ident.ident, item);
            }
            SyDeclaration::TypeFn(_) => todo!(),
        }
    }

    ModuleParseResult {
        namespace: ModuleNamespace {
            items: namespace_items,
        },
        module_path_refs,
        items_to_compile,
    }
}

pub fn parse_module_data_linking(
    ast: &SyDeclarationBody,
    compilation: &mut CompiledFileResults,
    mod_results: ModuleParseResult,
) {
    for item in mod_results.items_to_compile {
        let declaration = ast.statements[item.index_in_body]
            .as_ref()
            .expect("Parse result mismatch");

        match item.id {
            ModuleNamespaceItemKind::Type(id) => {
                let SyDeclaration::TypeDefine(declaration_item) = &declaration.item else {
                    unreachable!("Parse result mismatch")
                };

                let ty = link_type_expression_ast(
                    &declaration_item.value,
                    Some(declaration_item.name.clone()),
                    compilation,
                    &mod_results.namespace,
                );

                compilation
                    .linked_type_definitions
                    .insert_allocated_value(id, ty);
            }
            _ => todo!(),
        }
    }
}

pub fn get_type_id_for_linked_type_id(
    compilation: &mut CompiledFileResults,
    linked_ty_id: Id<LiType>,
) -> Id<TyType> {
    let mut ty_compilation = TypeFromLinkedTypeCompilation {
        linked_type_definitions: &compilation.linked_type_definitions,
        linked_type_to_type_mapping: &mut compilation.linked_type_to_type_mapping,
        types: &mut compilation.types,
        errors: &mut compilation.errors,
    };

    parse_type_from_linked_type_id(linked_ty_id, &mut ty_compilation)
}
