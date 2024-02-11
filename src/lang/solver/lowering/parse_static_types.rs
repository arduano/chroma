use std::{borrow::Cow, collections::HashMap};

use crate::lang::{
    ast::items::{SyArithmeticBinaryOp, SyBinaryOp},
    solver::{type_system::*, Id, MId, ModItemSet, ModuleGroupCompilation},
    tokens::{ItemWithSpan, Span},
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
        .insert_allocated_value(allocated_id, value);

    id
}

pub fn parse_type_from_linked_type(
    linked_ty: &LiType,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyType {
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
                        let value_id = compilation.types.add_value(value);

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
            let mut ty_id = parse_type_from_linked_type_id(*linked_ty_id, compilation);

            loop {
                let underlying_type = &compilation.types.get(ty_id);
                let Some(underlying_type) = underlying_type else {
                    break;
                };

                match &underlying_type.kind {
                    TyTypeKind::Reference(id) => {
                        ty_id = *id;
                    }
                    _ => break,
                }
            }

            TyTypeKind::Reference(ty_id)
        }
        LiTypeKind::BinaryExpression(binary) => {
            let left = parse_type_from_linked_type(&binary.left, compilation);
            let right = parse_type_from_linked_type(&binary.right, compilation);

            resolve_non_union_binary_expression(left, &binary.operator, right, compilation).kind
        }
        LiTypeKind::Unknown => TyTypeKind::Unknown,
        LiTypeKind::Never => TyTypeKind::Never,
    };

    TyType::new_named(linked_ty.name.clone(), ty_kind, linked_ty.span.clone())
}

/// Resolve type references to get the underlying type
fn normalize_references<'a>(ty: TyType, types: &'a ModItemSet<TyType>) -> Option<Cow<'a, TyType>> {
    // First, check if the type is a reference or not. Non reference types
    // get returned directly.
    match &ty.kind {
        TyTypeKind::Reference(id) => {
            let mut underlying_type = types.get(*id);

            loop {
                let Some(ty) = underlying_type else {
                    return None;
                };

                match &ty.kind {
                    TyTypeKind::Reference(id) => {
                        underlying_type = types.get(*id);
                    }
                    _ => return Some(Cow::Borrowed(ty)),
                }
            }
        }
        _ => Some(Cow::Owned(ty)),
    }
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
    let spread_ty = normalize_references(spread_ty_resolved, compilation.types);

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
    left: TyType,
    operator: &SyBinaryOp,
    right: TyType,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyType {
    let op_span = operator.span();
    let expr_span = left.span.join(&right.span);
    let left_span = left.span.clone();
    let right_span = right.span.clone();

    let left_resolved = normalize_references(left, compilation.types);
    let right_resolved = normalize_references(right, compilation.types);

    let Some(left_resolved) = left_resolved else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left_span.clone(),
        ));
        return TyType::new(TyTypeKind::Unknown, left_span);
    };

    let Some(right_resolved) = right_resolved else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            right_span.clone(),
        ));
        return TyType::new(TyTypeKind::Unknown, right_span);
    };

    let push_invalid_op_error = |left: &TyType, right: &TyType| {
        compilation.errors.push(CompilerError::new(
            "Invalid binary operation for types",
            left.span.join(&right.span),
        ));
        TyType::new(TyTypeKind::Unknown, left.span.join(&right.span))
    };

    macro_rules! invalid_op {
        () => {{
            push_invalid_op_error(&left_resolved, &right_resolved);
            return TyType::new(TyTypeKind::Unknown, operator.span());
        }};
    }

    match (&left_resolved.kind, &right_resolved.kind) {
        (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => match operator {
            SyBinaryOp::Arithmetic(op) => {
                let left_lit = &self_number.literal;
                let right_lit = &other_number.literal;

                let (Some(left_lit), Some(right_lit)) = (left_lit, right_lit) else {
                    return TyType::new(TyTypeKind::Number(TyNumber::new()), expr_span);
                };

                let result = run_arithmetic_op_on_numbers(left_lit.value, op, right_lit.value);

                TyType::new(
                    TyTypeKind::Number(TyNumber::from_literal(result)),
                    expr_span,
                )
            }
            _ => invalid_op!(),
        },
        _ => {
            compilation.errors.push(CompilerError::new(
                "Invalid binary operation for types",
                expr_span,
            ));
            TyType::new(TyTypeKind::Unknown, op_span)
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
