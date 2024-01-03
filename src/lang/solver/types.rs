use std::{
    ops::Deref,
    pin::{pin, Pin},
    process::Output,
    sync::Arc,
};

use futures::Future;

use crate::lang::{
    ast::{
        helpers::Attempted,
        items::{SyExpression, SyObjectLiteralField, SyTypeDefine},
    },
    entity_ids::{Id, KnownItemHandler},
    solver::{ModuleScopeDecl, RuntimeScopeDecl, TyStructLiteralField},
    tokens::TkIdent,
    CompilerError, ErrorCollector,
};

use super::{IdentMatcher, ModuleScopeIdentMatcher, TyType, TyTypeKind};

pub fn analyze_type_expression(
    ast: Arc<Attempted<SyExpression>>,
    matcher: IdentMatcher,
    types: KnownItemHandler<TyType>,
    error_collector: ErrorCollector,
) -> Pin<Box<dyn Future<Output = TyType> + Send>> {
    Box::pin(async move {
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

                    return TyType::new(TyTypeKind::Unknown);
                };

                match val.deref() {
                    RuntimeScopeDecl::ModuleDecl(mod_decl) => match mod_decl.deref() {
                        ModuleScopeDecl::TypeDecl(ty_decl) => {
                            let mut id = ty_decl.type_id;
                            loop {
                                let ty = types.get(ty_decl.type_id).await;

                                if let TyTypeKind::Reference(ty_id) = ty.kind() {
                                    id = *ty_id;
                                } else {
                                    return TyType::new(TyTypeKind::Reference(id));
                                }
                            }
                        }
                    },
                }
            }
            SyExpression::ObjectLiteral(obj) => {
                // Gather the fields in reverse order
                let mut fields = Vec::<TyStructLiteralField>::new();

                let mut add_field = |name: TkIdent, ty: TyType| {
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
                                kv.value.clone(),
                                matcher.clone(),
                                types.clone(),
                                error_collector.clone(),
                            )
                            .await;

                            add_field(key, value);
                        }
                        SyObjectLiteralField::KeyVariable(kv) => todo!(),
                        SyObjectLiteralField::Spread(spread) => todo!(),
                        SyObjectLiteralField::ComputedKey(kv) => todo!(),
                    }
                }
            }
        }

        todo!();
    })
}
