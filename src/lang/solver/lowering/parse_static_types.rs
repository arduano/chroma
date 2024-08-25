use std::collections::HashMap;

use crate::lang::{
    ast::items::{SyArithmeticBinaryOp, SyBinaryOp, SyMetaTypeBinaryOp},
    solver::{
        type_system::{self, *},
        Id, MId, ModItemSet, ModuleGroupCompilation, TypeData,
    },
    tokens::{ItemWithSpan, Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{LiExpression, LiExpressionKind, LiStructField};

/// A type for encapsulating which parts of this compilation step are mutated
/// and which parts are immutable. This keeps rust happy.
pub struct TypeFromLinkedTypeCompilation<'a> {
    pub current_module_id: Id<ModuleGroupCompilation>,
    pub linked_type_definitions: &'a ModItemSet<LiExpression>,
    pub linked_type_to_type_mapping: &'a mut HashMap<MId<LiExpression>, TypeId>,
    pub type_data: &'a mut TypeData,
    pub type_relationships: &'a mut TypeRelationships,
    pub errors: &'a mut ErrorCollector,
}

pub fn parse_type_id_from_linked_type_id(
    linked_ty_id: MId<LiExpression>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TypeId {
    if let Some(existing_type) = compilation.linked_type_to_type_mapping.get(&linked_ty_id) {
        return *existing_type;
    }

    let linked_ty = &compilation.linked_type_definitions[linked_ty_id];
    let non_reference_linked_ty =
        get_non_reference_linked_expression(linked_ty, &compilation.linked_type_definitions);
    let Some(linked_ty) = non_reference_linked_ty.linked_ty else {
        // This is a cyclical reference, so we can't resolve it.

        // Create an unknown type
        let unknown_type = TyType::new_unknown(linked_ty.name.clone(), linked_ty.span());
        let ty_id = compilation.type_data.add_value(unknown_type);

        // Associate the unknown type with all the referencial IDs
        compilation
            .linked_type_to_type_mapping
            .insert(linked_ty_id, ty_id);
        for referencial_id in non_reference_linked_ty.referencial_ids.into_iter() {
            compilation
                .linked_type_to_type_mapping
                .insert(referencial_id, ty_id);
        }

        return ty_id;
    };

    let allocated_id = compilation.type_data.allocate_id();

    // Associate the type with all the referencial IDs
    compilation
        .linked_type_to_type_mapping
        .insert(linked_ty_id, allocated_id);
    for referencial_id in non_reference_linked_ty.referencial_ids.into_iter() {
        compilation
            .linked_type_to_type_mapping
            .insert(referencial_id, allocated_id);
    }

    let value = parse_type_from_linked_type(linked_ty, compilation);
    let id = compilation
        .type_data
        .insert_allocated_value(allocated_id, value);

    id
}

struct NonReferenceLinkedExpression<'a> {
    /// The linked expression, if it exists. It doesn't exist if the references are cyclical.
    linked_ty: Option<&'a LiExpression>,
    /// The extra references that led to the non-reference linked expression.
    referencial_ids: Vec<MId<LiExpression>>,
}

fn get_non_reference_linked_expression<'a>(
    linked_ty: &'a LiExpression,
    li_expressions: &'a ModItemSet<LiExpression>,
) -> NonReferenceLinkedExpression<'a> {
    let mut referencial_ids = Vec::new();
    let mut linked_ty = linked_ty;

    while let LiExpressionKind::StaticReference(linked_ty_id) = &linked_ty.kind {
        let linked_ty_id = *linked_ty_id;

        if referencial_ids.contains(&linked_ty_id) {
            return NonReferenceLinkedExpression {
                linked_ty: None,
                referencial_ids,
            };
        }

        referencial_ids.push(linked_ty_id);
        linked_ty = &li_expressions[linked_ty_id];
    }

    NonReferenceLinkedExpression {
        linked_ty: Some(linked_ty),
        referencial_ids,
    }
}

fn parse_type_id_from_linked_type(
    linked_ty: &LiExpression,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TypeId {
    let value = parse_type_from_linked_type(linked_ty, compilation);
    compilation.type_data.add_value(value)
}

fn parse_type_from_linked_type(
    linked_ty: &LiExpression,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyType {
    let ty_kind = match &linked_ty.kind {
        LiExpressionKind::StaticReference(_) => {
            unreachable!()
        }
        LiExpressionKind::Number(num) => {
            if let Some(literal) = &num.literal {
                TyTypeKind::Number(TyNumber::from_literal(literal.value()))
            } else {
                TyTypeKind::Number(TyNumber::new())
            }
        }
        LiExpressionKind::String(str) => {
            if let Some(literal) = &str.literal {
                TyTypeKind::String(TyString::from_literal(literal.value().clone()))
            } else {
                TyTypeKind::String(TyString::new())
            }
        }
        LiExpressionKind::Boolean(boolean) => {
            if let Some(literal) = &boolean.literal {
                TyTypeKind::Boolean(TyBoolean::from_literal(*literal))
            } else {
                TyTypeKind::Boolean(TyBoolean::new())
            }
        }
        LiExpressionKind::Struct(structure) => {
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
                        let value = parse_type_id_from_linked_type(&kv.value, compilation);

                        let field = TyStructLiteralField {
                            name: kv.key.clone(),
                            value: value,
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
        LiExpressionKind::BinaryExpression(binary) => {
            let left = parse_type_id_from_linked_type(&binary.left, compilation);
            let right = parse_type_id_from_linked_type(&binary.right, compilation);

            return resolve_binary_expression(
                left,
                &binary.operator,
                right,
                linked_ty.name.clone(),
                linked_ty.span(),
                compilation,
            );
        }
        LiExpressionKind::Unknown => TyTypeKind::Any(TyAnyTypeKind::Unknown),
        LiExpressionKind::Never => TyTypeKind::Never,

        LiExpressionKind::TypeFnLazyValue(_) => todo!(),
    };

    TyType::new_named(linked_ty.name.clone(), ty_kind, linked_ty.span())
}

// I really tried avoiding a callback system here, but ownership semantics make it much much more verbose.
// The type can either be owned, or a reference type and therefore you get a borrow. Instead of dealing with
// mapping a Cow through 4 layers, I'm just passing a reference to a callback.
fn get_struct_literal_fields_from_ty_and_execute_callback<'a>(
    ty: &LiExpression,
    ref_span: &Span,
    compilation: &'a mut TypeFromLinkedTypeCompilation,
    callback: impl FnOnce(&Vec<TyStructLiteralField>),
) {
    let spread_ty_resolved = parse_type_id_from_linked_type(&ty, compilation);
    let spread_ty = compilation.type_data.get(spread_ty_resolved);

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
    id: TypeId,
    span: &Span,
    types: &mut TypeData,
    type_relationships: &mut TypeRelationships,
    errors: &mut ErrorCollector,
) -> Option<Vec<TypeId>> {
    let ty = types.get(id)?;
    match &ty.kind {
        TyTypeKind::Union(union) => {
            let union_ids = union.get_normalized_type_list(types, type_relationships)?;
            Some(union_ids.into_owned())
        }
        _ => Some(vec![id]), // TODO: Reduce allocations?
    }
}

/// Resolve a binary expression while accounting for unions. Any union types will be resolved
/// item-wise and then combined together again.
fn resolve_binary_expression(
    left: TypeId,
    operator: &SyBinaryOp,
    right: TypeId,
    name: Option<TkIdent>,
    span: Span,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyType {
    // Resolve union-tolerant operations first. Anything that doesn't require union permutation stuff goes here.
    match operator {
        SyBinaryOp::MetaType(SyMetaTypeBinaryOp::Union(_)) => {
            let union = TyUnion::union_types(
                left,
                right,
                span.clone(),
                name.clone(),
                compilation.type_data,
            );
            return union;
        }
        SyBinaryOp::MetaType(SyMetaTypeBinaryOp::Extends(_)) => {
            let assignable = compilation.type_relationships.is_type_assignable_to_type(
                &compilation.type_data,
                left,
                right,
            );

            let ty = TyType::new_named(
                name.clone(),
                TyTypeKind::Boolean(TyBoolean::from_literal(assignable)),
                span.clone(),
            );

            return ty;
        }
        _ => {}
    }

    // Get the variants list for both sides
    let left_variants = type_to_variant_list(
        left,
        &span,
        &mut compilation.type_data,
        &mut compilation.type_relationships,
        &mut compilation.errors,
    );
    let Some(left_variants) = left_variants else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            span.clone(),
        ));
        return TyType::new_unknown(name, span.clone());
    };

    let right_variants = type_to_variant_list(
        right,
        &span,
        &mut compilation.type_data,
        &mut compilation.type_relationships,
        &mut compilation.errors,
    );
    let Some(right_variants) = right_variants else {
        compilation.errors.push(CompilerError::new(
            "Recursive type computations are not allowed",
            span.clone(),
        ));
        return TyType::new_unknown(name, span.clone());
    };

    // Insert them into a new union
    let mut new_union = TyUnion::new();
    for union_left_ty in left_variants.into_iter() {
        for &union_right_ty in right_variants.iter() {
            let resolved = resolve_non_union_binary_expression(
                union_left_ty,
                operator,
                union_right_ty,
                name.clone(),
                span.clone(),
                compilation,
            );

            new_union.insert_type_by_id_normalized(
                resolved,
                &mut compilation.type_data,
                &mut compilation.type_relationships,
            );
        }
    }

    // If the union is just 1 element, return that element, otherwise return the union.
    if new_union.types.len() == 1 {
        // TODO: Ugly
        compilation
            .type_data
            .get(new_union.types[0])
            .unwrap()
            .clone()
    } else {
        let kind = TyTypeKind::Union(new_union);
        TyType::new_named(name, kind, span.clone())
    }
}

fn resolve_non_union_binary_expression(
    left: TypeId,
    operator: &SyBinaryOp,
    right: TypeId,
    name: Option<TkIdent>,
    span: Span,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TypeId {
    let left_ty = compilation.type_data.get(left).unwrap();
    let right_ty = compilation.type_data.get(right).unwrap();

    let op_span = operator.span();

    let _push_invalid_op_error = || {
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
            return TypeId::new_val(ty, span);
        }};
    }

    macro_rules! make_return_ty {
        ($kind:expr) => {{
            let ty = TyType::new_named(name.clone(), $kind, span.clone());
            compilation.type_data.add_value(ty)
        }};
    }

    use SyBinaryOp::*;
    use TyTypeKind::*;

    // Resolve per-type operators
    match (&left_ty.kind, &right_ty.kind, operator) {
        (_, _, MetaType(SyMetaTypeBinaryOp::Extends(_))) => {
            let assignable = compilation.type_relationships.is_type_assignable_to_type(
                &compilation.type_data,
                left,
                right,
            );

            dbg!(assignable);
            dbg!(&left);
            dbg!(&right);

            return make_return_ty!(TyTypeKind::Boolean(TyBoolean::from_literal(assignable)));
        }
        (Number(self_number), Number(other_number), Arithmetic(op)) => {
            let left_lit = &self_number.literal;
            let right_lit = &other_number.literal;

            let (Some(left_lit), Some(right_lit)) = (left_lit, right_lit) else {
                return make_return_ty!(TyTypeKind::Number(TyNumber::new()));
            };

            let result = run_arithmetic_op_on_numbers(left_lit.value, op, right_lit.value);

            return make_return_ty!(TyTypeKind::Number(TyNumber::from_literal(result)));
        }
        _ => {
            compilation.errors.push(CompilerError::new(
                "Invalid binary operation for types (1)",
                op_span.clone(),
            ));
            let ty = TyType::new_unknown(name, op_span);
            return compilation.type_data.add_value(ty);
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
