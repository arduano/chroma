use std::ops::Deref;

use crate::lang::tokens::{ItemWithSpan, Span};

use super::*;

fn make_error_li_type() -> LiTypeKind {
    return LiTypeKind::Unknown;
}

fn parse_ast_type_var_read(
    ident: &TkIdent,
    compilation: &mut ModuleGroupCompilation,
    namespace: &ModuleNamespace,
) -> LiTypeKind {
    // Temporary ident overwrites for hardcoded type names
    if ident.ident.deref() == "string" {
        return LiTypeKind::String(LiString { literal: None });
    }

    let item = namespace.get_ident_kind(ident);
    let Some(item) = item else {
        compilation.errors.push(CompilerError::new(
            format!("\"{}\" is not defined", &ident.ident),
            ident.span.clone(),
        ));

        return make_error_li_type();
    };

    match item {
        ModuleNamespaceItemKind::Type(id) => return LiTypeKind::StaticTypeReference(id),
        ModuleNamespaceItemKind::Unknown => return LiTypeKind::Unknown,
    }
}

pub fn link_type_expression_ast(
    ast: &Attempted<SyExpression>,
    name: Option<TkIdent>,
    compilation: &mut ModuleGroupCompilation,
    namespace: &ModuleNamespace,
) -> LiType {
    let Ok(ast) = ast else {
        return LiType::new_named(name, make_error_li_type(), Span::new_empty());
    };

    let kind = match ast {
        SyExpression::VarRead(var) => parse_ast_type_var_read(&var.name, compilation, namespace),
        SyExpression::StringLiteral(string) => LiTypeKind::String(LiString {
            literal: Some(string.literal.clone()),
        }),
        SyExpression::ObjectLiteral(obj) => {
            let mut fields = Vec::<LiStructField>::new();

            for field in obj.fields.fields.iter().flatten() {
                match field {
                    SyObjectLiteralField::KeyValue(kv) => {
                        let value =
                            link_type_expression_ast(&kv.value, None, compilation, namespace);

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::KeyVariable(kv) => {
                        let value_kind = parse_ast_type_var_read(&kv.key, compilation, namespace);
                        let value = LiType::new(value_kind, kv.key.span.clone());

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::Spread(spread) => {
                        let value =
                            link_type_expression_ast(&spread.fields, None, compilation, namespace);

                        fields.push(LiStructField::FieldSpread(LiStructFieldSpread {
                            spread: value,
                        }));
                    }
                    SyObjectLiteralField::ComputedKey(ckv) => {
                        let key = link_type_expression_ast(
                            &ckv.key_expression,
                            None,
                            compilation,
                            namespace,
                        );
                        let value = link_type_expression_ast(
                            &ckv.value_expression,
                            None,
                            compilation,
                            namespace,
                        );

                        fields.push(LiStructField::ComputedKeyValue(LiStructComputedKeyValue {
                            key,
                            value,
                        }));
                    }
                }
            }

            LiTypeKind::Struct(LiStruct { entries: fields })
        }
    };

    LiType::new_named(name, kind, ast.span().clone())
}
