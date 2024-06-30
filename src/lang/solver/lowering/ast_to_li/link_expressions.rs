use std::ops::Deref;

use crate::lang::ast::helpers::AttemptedAsRef;
use crate::lang::tokens::{ItemWithSpan, Span};

use super::super::*;

fn make_error_li_type() -> LiExpressionKind {
    return LiExpressionKind::Unknown;
}

fn parse_ast_var_read(
    ident: &TkIdent,
    compilation: &mut ModuleGroupCompilation,
    namespace: &ModuleNamespace,
) -> LiExpressionKind {
    // Temporary ident overwrites for hardcoded type names
    if ident.ident.deref() == "String" {
        return LiExpressionKind::String(LiString { literal: None });
    }
    if ident.ident.deref() == "Number" {
        return LiExpressionKind::Number(LiNumber { literal: None });
    }
    if ident.ident.deref() == "Boolean" {
        return LiExpressionKind::Boolean(LiBoolean { literal: None });
    }
    if ident.ident.deref() == "true" {
        return LiExpressionKind::Boolean(LiBoolean {
            literal: Some(true),
        });
    }
    if ident.ident.deref() == "false" {
        return LiExpressionKind::Boolean(LiBoolean {
            literal: Some(false),
        });
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
        ModuleNamespaceItemKind::Type(id) => return LiExpressionKind::StaticReference(id),
        ModuleNamespaceItemKind::Unknown => return LiExpressionKind::Unknown,
    }
}

pub fn link_expression_ast(
    ast: Attempted<&SyExpression>,
    name: Option<TkIdent>,
    compilation: &mut ModuleGroupCompilation,
    namespace: &ModuleNamespace,
) -> LiExpression {
    let Ok(ast) = ast else {
        return LiExpression::new_named(name, make_error_li_type(), Span::new_empty());
    };

    let kind = match ast {
        SyExpression::VarRead(var) => parse_ast_var_read(&var.name, compilation, namespace),
        SyExpression::StringLiteral(string) => LiExpressionKind::String(LiString {
            literal: Some(string.literal.clone()),
        }),
        SyExpression::IntLiteral(int) => LiExpressionKind::Number(LiNumber {
            literal: Some(int.literal.clone()),
        }),
        SyExpression::FloatLiteral(_float) => todo!(),
        SyExpression::ObjectLiteral(obj) => {
            let mut fields = Vec::<LiStructField>::new();

            for field in obj.braced.inner.fields.iter().flatten() {
                match field {
                    SyObjectLiteralField::KeyValue(kv) => {
                        let value = link_expression_ast(
                            kv.value.deref().value_as_ref(),
                            None,
                            compilation,
                            namespace,
                        );

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::KeyVariable(kv) => {
                        let value_kind = parse_ast_var_read(&kv.key, compilation, namespace);
                        let value = LiExpression::new(value_kind, kv.key.span.clone());

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::Spread(spread) => {
                        let value = link_expression_ast(
                            spread.fields.value_as_ref().map(|v| &**v),
                            None,
                            compilation,
                            namespace,
                        );

                        fields.push(LiStructField::FieldSpread(LiStructFieldSpread {
                            spread: value,
                        }));
                    }
                    SyObjectLiteralField::ComputedKey(ckv) => {
                        let key = link_expression_ast(
                            ckv.key.inner.value_as_ref().map(|v| &**v),
                            None,
                            compilation,
                            namespace,
                        );
                        let value = link_expression_ast(
                            ckv.value_expression.deref().value_as_ref(),
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

            LiExpressionKind::Struct(LiStruct { entries: fields })
        }
        SyExpression::Binary(binary) => {
            let left = link_expression_ast(
                binary.left.deref().value_as_ref(),
                None,
                compilation,
                namespace,
            );
            let right = link_expression_ast(
                binary.right.deref().value_as_ref(),
                None,
                compilation,
                namespace,
            );

            LiExpressionKind::BinaryExpression(LiBinaryExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: binary.operator.clone(),
            })
        }
        SyExpression::Parentheses(expr) => {
            let mut li_ty = link_expression_ast(
                expr.parens.inner.value_as_ref().map(|v| &**v),
                name,
                compilation,
                namespace,
            );
            li_ty.span = expr.parens.span();

            return li_ty;
        }
        SyExpression::Invalid => LiExpressionKind::Unknown,
    };

    LiExpression::new_named(name, kind, ast.span().clone())
}
