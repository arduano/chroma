use std::collections::HashMap;

use crate::lang::{
    ast::items::{SyArithmeticBinaryOp, SyBinaryOp, SyBooleanLogicBinaryOp},
    solver::{type_system::*, Id, MId, ModItemSet, ModuleGroupCompilation, TyIdOrValWithSpan},
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
    pub types: &'a mut ModItemSet<TyType>,
    pub errors: &'a mut ErrorCollector,
}

pub fn parse_type_from_linked_type_id(
    linked_ty_id: MId<LiType>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> MId<TyType> {
    if let Some(existing_type) = compilation.linked_type_to_type_mapping.get(&linked_ty_id) {
        return *existing_type;
    }

    let allocated_id = compilation.types.allocate_id();

    compilation
        .linked_type_to_type_mapping
        .insert(linked_ty_id, allocated_id);

    let linked_ty = &compilation.linked_type_definitions[linked_ty_id];
    let value = parse_type_from_linked_type(linked_ty, compilation);
    let id = compilation
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
                        let value_id = compilation.types.get_id_for_val_or_id(value.ty);

                        let field = TyStructLiteralField {
                            name: kv.key.clone(),
                            value: value_id,
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

            return resolve_non_union_binary_expression(
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

    let ty = TyType::new_named(linked_ty.name.clone(), ty_kind, linked_ty.span());
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

// fn resolve_binary_expression(
//     compilation: &mut TypeFromLinkedTypeCompilation,
//     binary: &LiBinaryTypeExpression,
// ) -> TyType {
//     let left = parse_type_from_linked_type(&binary.left, compilation);
//     let right = parse_type_from_linked_type(&binary.right, compilation);
// }

fn resolve_non_union_binary_expression(
    left: TyIdOrValWithSpan,
    operator: &SyBinaryOp,
    right: TyIdOrValWithSpan,
    name: Option<TkIdent>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let left_ty = compilation.types.get_val_for_val_or_id(&left.ty);
    let right_ty = compilation.types.get_val_for_val_or_id(&right.ty);

    let joined_span = left.span.join(&right.span);

    let Some(left_ty) = left_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(
            TyType::new(TyTypeKind::Unknown, joined_span.clone()),
            joined_span,
        );
    };

    let Some(right_ty) = right_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            right.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(
            TyType::new(TyTypeKind::Unknown, joined_span.clone()),
            joined_span,
        );
    };

    let op_span = operator.span();
    let expr_span = left.span.join(&right.span);

    let push_invalid_op_error = || {
        compilation.errors.push(CompilerError::new(
            "Invalid binary operation for types",
            left.span.join(&right.span),
        ));
        TyType::new(TyTypeKind::Unknown, left.span.join(&right.span))
    };

    macro_rules! invalid_op {
        ($compilation:expr) => {{
            push_invalid_op_error();
            let ty = TyType::new(TyTypeKind::Unknown, operator.span());
            return TyIdOrValWithSpan::new_val(ty, expr_span);
        }};
    }

    // Resolve operators that apply to all types, e.g. making unions
    match operator {
        SyBinaryOp::BooleanLogic(SyBooleanLogicBinaryOp::Or(_)) => {
            let union = TyUnion::union_types(
                left,
                right,
                expr_span.clone(),
                name.clone(),
                compilation.types,
                compilation.errors,
            );
            return union;
        }
        _ => {}
    }

    // Resolve per-type operators
    match (&left_ty.kind, &right_ty.kind) {
        (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => match operator {
            SyBinaryOp::Arithmetic(op) => {
                let left_lit = &self_number.literal;
                let right_lit = &other_number.literal;

                let (Some(left_lit), Some(right_lit)) = (left_lit, right_lit) else {
                    let ty = TyType::new(TyTypeKind::Number(TyNumber::new()), expr_span.clone());
                    return TyIdOrValWithSpan::new_val(ty, expr_span);
                };

                let result = run_arithmetic_op_on_numbers(left_lit.value, op, right_lit.value);

                let ty = TyType::new(
                    TyTypeKind::Number(TyNumber::from_literal(result)),
                    expr_span.clone(),
                );
                return TyIdOrValWithSpan::new_val(ty, expr_span);
            }
            _ => invalid_op!(compilation),
        },
        _ => {
            compilation.errors.push(CompilerError::new(
                "Invalid binary operation for types",
                expr_span.clone(),
            ));
            let ty = TyType::new(TyTypeKind::Unknown, op_span);
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
