use std::collections::HashMap;

use crate::lang::{
    ast::items::{SyArithmeticBinaryOp, SyBinaryOp, SyBooleanLogicBinaryOp},
    solver::{
        type_system::*, Id, MId, ModItemSet, ModuleGroupCompilation, TyIdOrValWithSpan, TypeData,
        TypeIdWithSpan,
    },
    tokens::{ItemWithSpan, Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{LiStructField, LiType, LiTypeKind};

/// A type for encapsulating which parts of this compilation step are mutated
/// and which parts are immutable. This keeps rust happy.
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
                linked_ty.span(),
                compilation,
            );
        }
        LiTypeKind::Unknown => TyTypeKind::Any(TyAnyTypeKind::Unknown),
        LiTypeKind::Never => TyTypeKind::Never,

        LiTypeKind::TypeFnLazyValue(_) => todo!(),
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

// Using a single type, convert it to a list of variants.
// Unions will return an array of types, while single types
// will return a slice with just themselves as the only element.
fn type_to_variant_list<'a>(
    id: &'a TyIdOrValWithSpan,
    ty: &TyType,
    types: &ModItemSet<TyType>,
    type_subsetability: &mut TypeSubsetabilityCache,
) -> Vec<TyIdOrValWithSpan> {
    match &ty.kind {
        TyTypeKind::Union(union) => {
            let types = union
                .get_normalized_type_list(types, type_subsetability)
                .iter()
                .map(|ty| ty.as_type_id_or_val())
                .collect::<Vec<_>>();

            types
        }
        _ => vec![id.clone()], // TODO: Reduce allocations?
    }
}

/// Resolve a binary expression while accounting for unions. Any union types will be resolved
/// item-wise and then combined together again.
fn resolve_binary_expression(
    left: TyIdOrValWithSpan,
    operator: &SyBinaryOp,
    right: TyIdOrValWithSpan,
    name: Option<TkIdent>,
    span: Span,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let expr_span = left.span.join(&right.span);

    // Resolve unions first, unions are a special edge case
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

    // Grab the types
    let Some(left_ty) = compilation.type_data.types.get_val_for_val_or_id(&left.ty) else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(name, expr_span.clone()), expr_span);
    };
    let Some(right_ty) = compilation.type_data.types.get_val_for_val_or_id(&right.ty) else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(name, expr_span.clone()), expr_span);
    };

    // If neither is a union, directly return as-is.
    if !left_ty.is_union() && !right_ty.is_union() {
        return resolve_non_union_binary_expression(left, operator, right, name, span, compilation);
    }

    // Get the variants list for both sides
    let left_variants = type_to_variant_list(
        &left,
        left_ty,
        &compilation.type_data.types,
        &mut compilation.type_data.type_subsetability,
    );
    let right_variants = type_to_variant_list(
        &right,
        right_ty,
        &compilation.type_data.types,
        &mut compilation.type_data.type_subsetability,
    );

    // Insert them into a new union
    let mut new_union = TyUnion::new();
    for union_left_ty in left_variants.into_iter() {
        for union_right_ty in right_variants.iter() {
            let mut union_left_ty = union_left_ty.clone(); // TODO: Reduce allocations?
            union_left_ty.span = left.span.clone();
            let mut union_right_ty = union_right_ty.clone();
            union_right_ty.span = right.span.clone();

            let resolved = resolve_non_union_binary_expression(
                union_left_ty,
                operator,
                union_right_ty,
                name.clone(),
                span.clone(),
                compilation,
            );

            new_union.insert_type_normalized(
                resolved,
                &mut compilation.type_data.types,
                &mut compilation.type_data.type_subsetability,
            );
        }
    }

    // If the union is just 1 element, return that element, otherwise return the union.
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

fn resolve_non_union_binary_expression(
    left: TyIdOrValWithSpan,
    operator: &SyBinaryOp,
    right: TyIdOrValWithSpan,
    name: Option<TkIdent>,
    span: Span,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyIdOrValWithSpan {
    let left_ty = compilation.type_data.types.get_val_for_val_or_id(&left.ty);
    let right_ty = compilation.type_data.types.get_val_for_val_or_id(&right.ty);

    let Some(left_ty) = left_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            left.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(name, span.clone()), span);
    };

    let Some(right_ty) = right_ty else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            right.span.clone(),
        ));
        return TyIdOrValWithSpan::new_val(TyType::new_unknown(name, span.clone()), span);
    };

    let op_span = operator.span();

    let push_invalid_op_error = || {
        compilation.errors.push(CompilerError::new(
            "Invalid binary operation for types (2)",
            op_span.clone(),
        ));
        TyType::new_unknown(name.clone(), span.clone())
    };

    macro_rules! invalid_op {
        ($compilation:expr) => {{
            push_invalid_op_error();
            let ty = TyType::new_unknown(name, operator.span());
            return TyIdOrValWithSpan::new_val(ty, span);
        }};
    }

    // Resolve per-type operators
    match (&left_ty.kind, &right_ty.kind) {
        (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => match operator {
            SyBinaryOp::Arithmetic(op) => {
                let left_lit = &self_number.literal;
                let right_lit = &other_number.literal;

                let (Some(left_lit), Some(right_lit)) = (left_lit, right_lit) else {
                    let ty = TyType::new_named_infer_flags(
                        name,
                        TyTypeKind::Number(TyNumber::new()),
                        span.clone(),
                        &compilation.type_data.types,
                    );
                    return TyIdOrValWithSpan::new_val(ty, span);
                };

                let result = run_arithmetic_op_on_numbers(left_lit.value, op, right_lit.value);

                let ty = TyType::new_named_infer_flags(
                    name,
                    TyTypeKind::Number(TyNumber::from_literal(result)),
                    span.clone(),
                    &compilation.type_data.types,
                );
                return TyIdOrValWithSpan::new_val(ty, span);
            }
            _ => invalid_op!(compilation),
        },
        _ => {
            compilation.errors.push(CompilerError::new(
                "Invalid binary operation for types (1)",
                op_span.clone(),
            ));
            let ty = TyType::new_unknown(name, op_span);
            return TyIdOrValWithSpan::new_val(ty, span);
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
