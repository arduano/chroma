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

use super::{
    linked_ast::*, CompiledFileResults, ModuleNamespace, ModuleNamespaceItem,
    ModuleNamespaceItemKind,
};

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

fn parse_module(
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
            }
            _ => todo!(),
        }
    }
}

fn make_error_li_type() -> LiType {
    return LiType::new(LiTypeKind::Unknown);
}

fn parse_ast_var_read(
    ident: &TkIdent,
    compilation: &mut CompiledFileResults,
    namespace: &ModuleNamespace,
) -> LiType {
    let item = namespace.get_ident_kind(ident);
    let Some(item) = item else {
        compilation.add_error(CompilerError::new(
            format!("\"{}\" is not defined", &ident.ident),
            ident.span.clone(),
        ));

        return make_error_li_type();
    };

    match item {
        ModuleNamespaceItemKind::Type(id) => {
            return LiType::new(LiTypeKind::StaticTypeReference(id))
        }
    }
}

fn parse_type_ast(
    ast: &Attempted<SyExpression>,
    compilation: &mut CompiledFileResults,
    namespace: &ModuleNamespace,
) -> LiType {
    let Ok(ast) = ast else {
        return make_error_li_type();
    };

    match ast {
        SyExpression::VarRead(var) => parse_ast_var_read(&var.name, compilation, namespace),
        SyExpression::StringLiteral(string) => LiType::new(LiTypeKind::String(LiString {
            string: string.literal.clone(),
        })),
        SyExpression::ObjectLiteral(obj) => {
            let mut fields = Vec::<LiStructField>::new();

            for field in obj.fields.fields.iter().flatten() {
                match field {
                    SyObjectLiteralField::KeyValue(kv) => {
                        let value = parse_type_ast(&kv.value, compilation, namespace);

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::KeyVariable(kv) => {
                        let value = parse_ast_var_read(&kv.key, compilation, namespace);

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::Spread(spread) => {
                        let value = parse_type_ast(&spread.fields, compilation, namespace);

                        fields.push(LiStructField::FieldSpread(LiStructFieldSpread {
                            spread: value,
                        }));
                    }
                    SyObjectLiteralField::ComputedKey(ckv) => {
                        let key = parse_type_ast(&ckv.key_expression, compilation, namespace);
                        let value = parse_type_ast(&ckv.value_expression, compilation, namespace);

                        fields.push(LiStructField::ComputedKeyValue(LiStructComputedKeyValue {
                            key,
                            value,
                        }));
                    }
                }
            }

            LiType::new(LiTypeKind::Struct(LiStruct { fields }))
        }
    }
}
