use crate::lang::{
    ast::{
        helpers::Attempted,
        items::{SyDeclaration, SyDeclarationBody, SyExpression, SyObjectLiteralField},
    },
    tokens::TkIdent,
    CompilerError,
};

use self::{
    ast_to_li::link_type_expression_ast,
    parse_static_types::{parse_type_from_linked_type_id, TypeFromLinkedTypeCompilation},
};

use super::{
    linked_ast::*, type_system::TyType, MId, ModuleGroupCompilation, ModuleNamespace,
    ModuleNamespaceItem, ModuleNamespaceItemKind,
};

mod ast_to_li;
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
    kind: ModuleNamespaceItemKind,
}

pub fn parse_module_decls(
    ast: &SyDeclarationBody,
    compilation: &mut ModuleGroupCompilation,
) -> ModuleParseResult {
    let module_path_refs = ();
    let mut namespace = ModuleNamespace::new();
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
                    kind: id,
                });
                namespace.add_item(item);
            }
            SyDeclaration::TypeFn(_) => todo!(),
        }
    }

    ModuleParseResult {
        namespace,
        module_path_refs,
        items_to_compile,
    }
}

pub fn parse_module_data_linking(
    ast: &SyDeclarationBody,
    compilation: &mut ModuleGroupCompilation,
    mod_results: ModuleParseResult,
) {
    for item in mod_results.items_to_compile {
        let declaration = ast.statements[item.index_in_body]
            .as_ref()
            .expect("Parse result mismatch");

        match item.kind {
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
                    .insert_val_for_allocated_value(id, ty);
            }
            ModuleNamespaceItemKind::Unknown => {}
        }
    }
}

pub fn get_type_id_for_linked_type_id(
    compilation: &mut ModuleGroupCompilation,
    linked_ty_id: MId<LiType>,
) -> MId<TyType> {
    let mut ty_compilation = TypeFromLinkedTypeCompilation {
        current_module_id: compilation.current_module_id,
        linked_type_definitions: &compilation.linked_type_definitions,
        linked_type_to_type_mapping: &mut compilation.linked_type_to_type_mapping,
        type_data: &mut compilation.type_data,
        errors: &mut compilation.errors,
    };

    parse_type_from_linked_type_id(linked_ty_id, &mut ty_compilation)
}
