use std::collections::HashMap;

use crate::lang::{
    ast::items::{SyArithmeticBinaryOp, SyBinaryOp, SyBooleanLogicBinaryOp},
    solver::{
        lowering::try_normalize_type, type_system::*, Id, MId, ModItemSet, ModuleGroupCompilation,
        TyIdOrValWithSpan, TypeData, TypeIdWithSpan,
    },
    tokens::{ItemWithSpan, Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{LiStructField, LiType, LiTypeKind};

/// A type for encapsulating which parts of this compilation step are mutated
/// and which parts are immutable. This keeps the compiler happy.
pub struct TypeFromLinkedTypeCompilation<'a> {
    pub current_module_id: Id<ModuleGroupCompilation>,
    pub linked_type_definitions: &'a ModItemSet<LiType>,
    pub linked_type_to_type_mapping: &'a mut HashMap<MId<LiType>, MId<TyType>>,
    pub type_data: &'a mut TypeData,
    pub errors: &'a mut ErrorCollector,
}

pub fn parse_type_from_linked_type_id(
    linked_ty_id: MId<LiType>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> MId<TyType> {
    if let Some(existing_type) = compilation.linked_type_to_type_mapping.get(&linked_ty_id) {
        return *existing_type;
    }

    let allocated_id = compilation.type_data.types.allocate_id();

    compilation
        .linked_type_to_type_mapping
        .insert(linked_ty_id, allocated_id);

    let linked_ty = &compilation.linked_type_definitions[linked_ty_id];
    let value = parse_type_from_linked_type(linked_ty, compilation);
    let id = compilation
        .type_data
        .types
        .insert_for_allocated_value(allocated_id, value.ty);

    id
}

pub fn parse_type_from_linked_type(
    linked_ty: &LiType,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let ty_kind = match &linked_ty.kind {
        LiTypeKind::Number(num) => {
            if let Some(literal) = &num.literal {
                TyTypeKind::Number(TyNumber::from_literal(literal.value()))
            } else {
                TyTypeKind::Number(TyNumber::new())
            }
        }
        LiTypeKind::String(str) => {
            if let Some(literal) = &str.literal {
                TyTypeKind::String(TyString::from_literal(literal.value().clone()))
            } else {
                TyTypeKind::String(TyString::new())
            }
        }
        LiTypeKind::Struct(structure) => {
            let mut fields = Vec::<TyStructLiteralField>::new();

            let mut add_field_if_not_exists = |field: TyStructLiteralField| {
                if fields.iter().any(|f| f.name.ident == field.name.ident) {
                    return;
                }

                fields.push(field);
            };

            for entry in structure.entries.iter().rev() {
                match entry {
                    LiStructField::KeyValue(kv) => {
                        let value = parse_type_from_linked_type(&kv.value, compilation);
                        let value_id = compilation.type_data.types.get_id_for_val_or_id(value.ty);

                        let field = TyStructLiteralField {
                            name: kv.key.clone(),
                            value: TypeIdWithSpan::new(value_id, kv.value.span()),
                        };

                        add_field_if_not_exists(field);
                    }
                    LiStructField::FieldSpread(spread) => {
                        get_struct_literal_fields_from_ty_and_execute_callback(
                            &spread.spread,
                            &spread.spread.span,
                            compilation,
                            |fields| {
                                for field in fields.iter().rev() {
                                    add_field_if_not_exists(field.clone());
                                }
                            },
                        );
                    }
                    LiStructField::ComputedKeyValue(_computed) => todo!(),
                }
            }

            TyTypeKind::Struct(TyStruct::new_literal(fields))
        }
        LiTypeKind::StaticTypeReference(linked_ty_id) => {
            let ty_id = parse_type_from_linked_type_id(*linked_ty_id, compilation);
            return TyIdOrValWithSpan::new_id(ty_id, linked_ty.span());
        }
        LiTypeKind::BinaryExpression(binary) => {
            let left = parse_type_from_linked_type(&binary.left, compilation);
            let right = parse_type_from_linked_type(&binary.right, compilation);

            return resolve_binary_expression(
                left,
                &binary.operator,
                right,
                linked_ty.name.clone(),
                compilation,
            );
        }
        LiTypeKind::Unknown => TyTypeKind::Unknown,
        LiTypeKind::Never => TyTypeKind::Never,
    };

    let ty = TyType::new_named_infer_flags(
        linked_ty.name.clone(),
        ty_kind,
        linked_ty.span(),
        &compilation.type_data.types,
    );
    TyIdOrValWithSpan::new_val(ty, linked_ty.span())
}

// I really tried avoiding a callback system here, but ownership semantics make it much much more verbose.
// The type can either be owned, or a reference type and therefore you get a borrow. Instead of dealing with
// mapping a Cow through 4 layers, I'm just passing a reference to a callback.
fn get_struct_literal_fields_from_ty_and_execute_callback<'a>(
    ty: &LiType,
    ref_span: &Span,
    compilation: &'a mut TypeFromLinkedTypeCompilation,
    callback: impl FnOnce(&Vec<TyStructLiteralField>),
) {
    let spread_ty_resolved = parse_type_from_linked_type(&ty, compilation);
    let spread_ty = compilation
        .type_data
        .types
        .get_val_for_val_or_id(&spread_ty_resolved.ty);

    let Some(spread_ty) = spread_ty else {
        // Type not resolved yet, therefore this is likely a cyclical spread reference
        compilation.errors.push(CompilerError::new(
            "Cyclical type reference in struct spread",
            ref_span.clone(),
        ));
        return;
    };

    match &spread_ty.kind {
        TyTypeKind::Struct(spread_struct) => {
            let Some(literal) = &spread_struct.literal else {
                // Not a literal struct, therefore has infinite fields and shouldn't be spread
                compilation.errors.push(CompilerError::new(
                    "Expected struct literal in struct spread",
                    ref_span.clone(),
                ));
                return;
            };

            callback(&literal.fields);
        }
        _ => {
            compilation.errors.push(CompilerError::new(
                "Expected struct type in struct spread",
                ref_span.clone(),
            ));
            return;
        }
    }
}

/// Resolve a binary expression while accounting for unions. Any union types will be resolved
/// item-wise and then combined together again.
fn resolve_binary_expression(
    left: TyIdOrValWithSpan,
    operator: &SyBinaryOp,
    right: TyIdOrValWithSpan,
    name: Option<TkIdent>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let left_ty = compilation.type_data.types.get_val_for_val_or_id(&left.ty);
    let right_ty = compilation.type_data.types.get_val_for_val_or_id(&right.ty);

    let expr_span = left.span.join(&right.span);

    // Resolve unions first
    match operator {
        SyBinaryOp::BooleanLogic(SyBooleanLogicBinaryOp::Or(_)) => {
            let union = TyUnion::union_types(
                left,
                right,
                expr_span.clone(),
                name.clone(),
                compilation.type_data,
            );
            return union;
        }
        _ => {}
    }

    let Some(_left_ty) = left_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(
            TyType::new(
                TyTypeKind::Unknown,
                expr_span.clone(),
                TyTypeFlags::new_for_unknown(),
            ),
            expr_span,
        );
    };

    let Some(_right_ty) = right_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            right.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(
            TyType::new(
                TyTypeKind::Unknown,
                expr_span.clone(),
                TyTypeFlags::new_for_unknown(),
            ),
            expr_span,
        );
    };

    let left_ty_id = compilation.type_data.types.get_id_for_val_or_id(left.ty);
    let left_ty_ref = TypeIdWithSpan::new(left_ty_id, left.span.clone());
    try_normalize_type(
        &left_ty_ref,
        &mut compilation.type_data.types,
        &mut compilation.type_data.type_subsetability,
        &mut compilation.errors,
    );

    let right_ty_id = compilation.type_data.types.get_id_for_val_or_id(right.ty);
    let right_ty_ref = TypeIdWithSpan::new(right_ty_id, right.span.clone());
    try_normalize_type(
        &right_ty_ref,
        &mut compilation.type_data.types,
        &mut compilation.type_data.type_subsetability,
        &mut compilation.errors,
    );

    let left_ty = compilation.type_data.types.get(left_ty_ref.id).unwrap();
    let right_ty = compilation.type_data.types.get(right_ty_ref.id).unwrap();

    // Normalize

    enum Side {
        Left,
        Right,
    }

    match ((&left_ty.kind, Side::Left), (&right_ty.kind, Side::Right)) {
        ((TyTypeKind::Union(union1), _), (TyTypeKind::Union(union2), _)) => {
            let mut new_union = TyUnion::new();
            let union1_types = union1.types.clone(); // Keep the borrow checker happy
            let union2_types = union2.types.clone(); // Keep the borrow checker happy
            for union1_ty in &union1_types {
                for union2_ty in &union2_types {
                    let resolved = resolve_non_union_binary_expression(
                        union1_ty.as_type_id_or_val(),
                        operator,
                        union2_ty.as_type_id_or_val(),
                        compilation,
                    );

                    new_union.insert_type_normalized(
                        resolved,
                        &mut compilation.type_data.types,
                        &mut compilation.type_data.type_subsetability,
                    );
                }
            }

            if new_union.types.len() == 1 {
                let ty = &new_union.types[0];
                return TyIdOrValWithSpan::new_id(ty.id, ty.span.clone());
            } else {
                let kind = TyTypeKind::Union(new_union);
                let flags = kind.flags(&compilation.type_data.types);
                let ty = TyType::new_named(name, kind, expr_span.clone(), flags);
                return TyIdOrValWithSpan::new_val(ty, expr_span);
            }
        }
        ((_, side), (TyTypeKind::Union(union), _)) | ((TyTypeKind::Union(union), _), (_, side)) => {
            let mut new_union = TyUnion::new();
            let union_types = union.types.clone(); // Keep the borrow checker happy

            // Get the other type, and ensure that it has an ID so we don't have to clone a non-id type a lot.
            let other_ty_ref = match side {
                Side::Left => left_ty_ref,
                Side::Right => right_ty_ref,
            };
            let other_ty = other_ty_ref.as_type_id_or_val();

            for union_ty in &union_types {
                let resolved = match side {
                    Side::Left => resolve_non_union_binary_expression(
                        other_ty.clone(),
                        operator,
                        union_ty.as_type_id_or_val(),
                        compilation,
                    ),
                    Side::Right => resolve_non_union_binary_expression(
                        union_ty.as_type_id_or_val(),
                        operator,
                        other_ty.clone(),
                        compilation,
                    ),
                };

                new_union.insert_type_normalized(
                    resolved,
                    &mut compilation.type_data.types,
                    &mut compilation.type_data.type_subsetability,
                );
            }

            if new_union.types.len() == 1 {
                let ty = &new_union.types[0];
                return TyIdOrValWithSpan::new_id(ty.id, ty.span.clone());
            } else {
                let kind = TyTypeKind::Union(new_union);
                let flags = kind.flags(&compilation.type_data.types);
                let ty = TyType::new_named(name, kind, expr_span.clone(), flags);
                return TyIdOrValWithSpan::new_val(ty, expr_span);
            }
        }
        (_, _) => resolve_non_union_binary_expression(
            left_ty_ref.as_type_id_or_val(),
            operator,
            right_ty_ref.as_type_id_or_val(),
            compilation,
        ),
    }
}

fn resolve_non_union_binary_expression(
    left: TyIdOrValWithSpan,
    operator: &SyBinaryOp,
    right: TyIdOrValWithSpan,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let left_ty = compilation.type_data.types.get_val_for_val_or_id(&left.ty);
    let right_ty = compilation.type_data.types.get_val_for_val_or_id(&right.ty);

    let joined_span = left.span.join(&right.span);

    let Some(left_ty) = left_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(joined_span.clone()), joined_span);
    };

    let Some(right_ty) = right_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            right.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(joined_span.clone()), joined_span);
    };

    let op_span = operator.span();
    let expr_span = left.span.join(&right.span);

    let push_invalid_op_error = || {
        compilation.errors.push(CompilerError::new(
            "Invalid binary operation for types (2)",
            op_span.clone(),
        ));
        TyType::new_unknown(joined_span)
    };

    macro_rules! invalid_op {
        ($compilation:expr) => {{
            push_invalid_op_error();
            let ty = TyType::new_unknown(operator.span());
            return TyIdOrValWithSpan::new_val(ty, expr_span);
        }};
    }

    // Resolve per-type operators
    match (&left_ty.kind, &right_ty.kind) {
        (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => match operator {
            SyBinaryOp::Arithmetic(op) => {
                let left_lit = &self_number.literal;
                let right_lit = &other_number.literal;

                let (Some(left_lit), Some(right_lit)) = (left_lit, right_lit) else {
                    let ty = TyType::new_infer_flags(
                        TyTypeKind::Number(TyNumber::new()),
                        expr_span.clone(),
                        &compilation.type_data.types,
                    );
                    return TyIdOrValWithSpan::new_val(ty, expr_span);
                };

                let result = run_arithmetic_op_on_numbers(left_lit.value, op, right_lit.value);

                let ty = TyType::new_infer_flags(
                    TyTypeKind::Number(TyNumber::from_literal(result)),
                    expr_span.clone(),
                    &compilation.type_data.types,
                );
                return TyIdOrValWithSpan::new_val(ty, expr_span);
            }
            _ => invalid_op!(compilation),
        },
        _ => {
            compilation.errors.push(CompilerError::new(
                "Invalid binary operation for types (1)",
                op_span.clone(),
            ));
            let ty = TyType::new_unknown(op_span);
            return TyIdOrValWithSpan::new_val(ty, expr_span);
        }
    }
}

fn run_arithmetic_op_on_numbers(left: i64, operator: &SyArithmeticBinaryOp, right: i64) -> i64 {
    use SyArithmeticBinaryOp::*;

    match operator {
        Add(_) => left + right,
        Minus(_) => left - right,
        Mult(_) => left * right,
        Div(_) => left / right,
        Mod(_) => left % right,
    }
}
