use super::*;

fn make_error_li_type() -> LiType {
    return LiType::new(LiTypeKind::Unknown);
}

fn parse_ast_type_var_read(
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

pub fn link_type_expression_ast(
    ast: &Attempted<SyExpression>,
    compilation: &mut CompiledFileResults,
    namespace: &ModuleNamespace,
) -> LiType {
    let Ok(ast) = ast else {
        return make_error_li_type();
    };

    match ast {
        SyExpression::VarRead(var) => parse_ast_type_var_read(&var.name, compilation, namespace),
        SyExpression::StringLiteral(string) => LiType::new(LiTypeKind::String(LiString {
            string: string.literal.clone(),
        })),
        SyExpression::ObjectLiteral(obj) => {
            let mut fields = Vec::<LiStructField>::new();

            for field in obj.fields.fields.iter().flatten() {
                match field {
                    SyObjectLiteralField::KeyValue(kv) => {
                        let value = link_type_expression_ast(&kv.value, compilation, namespace);

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::KeyVariable(kv) => {
                        let value = parse_ast_type_var_read(&kv.key, compilation, namespace);

                        fields.push(LiStructField::KeyValue(LiStructKeyValue {
                            key: kv.key.clone(),
                            value: value,
                        }));
                    }
                    SyObjectLiteralField::Spread(spread) => {
                        let value =
                            link_type_expression_ast(&spread.fields, compilation, namespace);

                        fields.push(LiStructField::FieldSpread(LiStructFieldSpread {
                            spread: value,
                        }));
                    }
                    SyObjectLiteralField::ComputedKey(ckv) => {
                        let key =
                            link_type_expression_ast(&ckv.key_expression, compilation, namespace);
                        let value =
                            link_type_expression_ast(&ckv.value_expression, compilation, namespace);

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
