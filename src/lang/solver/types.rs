use std::{ops::Deref, sync::Arc};

use crate::lang::{
    ast::{
        helpers::Attempted,
        items::{SyExpression, SyObjectLiteralField},
    },
    entity_ids::{Id, KnownItemHandler},
    solver::{ModuleScopeDecl, RuntimeScopeDecl, TyString, TyStructLiteralField},
    tokens::TkIdent,
    CompilerError, ErrorCollector,
};

use super::{IdentMatcher, TyStruct, TyType, TyTypeKind};

pub fn analyze_type_expression(
    name: Option<TkIdent>,
    ast: Arc<Attempted<SyExpression>>,
    matcher: IdentMatcher,
    types: KnownItemHandler<TyType>,
    error_collector: ErrorCollector,
) -> Id<TyType> {
    types.clone().allocate_and_fill_with(|_| async move {
        let expr = match ast.deref() {
            Attempted::Err(_) => return TyType::new(TyTypeKind::Unknown),
            Attempted::Ok(expr) => expr,
        };

        match expr {
            SyExpression::VarRead(var) => {
                let val = matcher.find(&var.name).await;

                let Some(val) = val else {
                    error_collector.push(CompilerError::new(
                        format!("Couldn't find type `{}` in scope", &var.name.ident),
                        var.name.span.clone(),
                    ));

                    return TyType::new_named(name, TyTypeKind::Unknown);
                };

                match val.deref() {
                    RuntimeScopeDecl::ModuleDecl(mod_decl) => match mod_decl.deref() {
                        ModuleScopeDecl::TypeDecl(ty_decl) => {
                            return TyType::new_named(name, TyTypeKind::Reference(ty_decl.type_id));
                        }
                    },
                }
            }
            SyExpression::StringLiteral(lit) => {
                return TyType::new_named(
                    name,
                    TyTypeKind::String(TyString::from_literal(lit.literal.value().clone())),
                );
            }
            SyExpression::ObjectLiteral(obj) => {
                // Gather the fields in reverse order
                let mut fields = Vec::<TyStructLiteralField>::new();

                let mut add_field = |name: TkIdent, ty: Id<TyType>| {
                    // Add it only if the field doesn't already exist
                    if fields.iter().any(|field| field.name.ident == name.ident) {
                        return;
                    }

                    fields.push(TyStructLiteralField::new(name, ty));
                };

                for field in obj.fields.fields.iter().rev() {
                    let Ok(field) = field else {
                        continue;
                    };

                    match field {
                        SyObjectLiteralField::KeyValue(kv) => {
                            let key = kv.key.clone();

                            let value = analyze_type_expression(
                                None,
                                kv.value.clone(),
                                matcher.clone(),
                                types.clone(),
                                error_collector.clone(),
                            );

                            add_field(key, value);
                        }
                        SyObjectLiteralField::KeyVariable(_kv) => todo!(),
                        SyObjectLiteralField::Spread(_spread) => todo!(),
                        SyObjectLiteralField::ComputedKey(_kv) => todo!(),
                    }
                }

                TyType::new_named(name, TyTypeKind::Struct(TyStruct::new_literal(fields)))
            }
        }
    })
}
