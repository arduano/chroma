use std::collections::HashMap;

use crate::lang::{
    ast::{
        helpers::Attempted,
        items::{
            SyDeclaration, SyDeclarationBody, SyExpression, SyObjectLiteralField, SyTypeDefine,
        },
    },
    tokens::TkIdent,
    CompilerError,
};

use self::link_type_expressions::link_type_expression_ast;

use super::{
    linked_ast::*, CompiledFileResults, ModuleNamespace, ModuleNamespaceItem,
    ModuleNamespaceItemKind,
};

mod link_type_expressions;
mod parse_static_types;

pub struct ModuleParseResult {
    /// Namespace of the current module
    namespace: ModuleNamespace,

    /// The paths of other modules that this module references, for dependency management.
    module_path_refs: (),

    /// Cache of module items to compile later
    items_to_compile: Vec<ModuleItem>,
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
                    compilation.linked_type_definitions.allocate_type(),
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

fn parse_module_data_linking(
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
                    compilation,
                    &mod_results.namespace,
                );

                compilation
                    .linked_type_definitions
                    .insert_allocated_type(id, ty);
            }
            _ => todo!(),
        }
    }
}
