use std::collections::HashMap;

use crate::lang::{
    solver::{type_system::*, Id, MId, ModItemSet, ModuleGroupCompilation},
    tokens::Span,
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
                TyTypeKind::Number(TyNumber::from_literal(literal.value() as f64))
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
                if fields.iter().any(|f| f.name == field.name) {
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
                        let fields = get_struct_literal_fields_from_ty(
                            &spread.spread,
                            &linked_ty.span,
                            compilation,
                        );

                        if let Some(fields) = fields {
                            for field in fields.iter().rev() {
                                add_field_if_not_exists(field.clone());
                            }
                        }
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
        LiTypeKind::Unknown => TyTypeKind::Unknown,
        LiTypeKind::Never => TyTypeKind::Never,
    };

    TyType::new_named(linked_ty.name.clone(), ty_kind)
}

fn get_struct_literal_fields_from_ty<'a>(
    ty: &LiType,
    ref_span: &Span,
    compilation: &'a mut TypeFromLinkedTypeCompilation,
) -> Option<&'a Vec<TyStructLiteralField>> {
    let spread_ty_resolved = parse_type_from_linked_type(&ty, compilation);
    let mut spread_id = compilation.types.add_value(spread_ty_resolved);

    loop {
        let spread_ty = &compilation.types.get(spread_id);
        let Some(spread_ty) = spread_ty else {
            // Type not resolved yet, therefore this is likely a cyclical spread reference
            compilation.errors.push(CompilerError::new(
                "Cyclical type reference in struct spread",
                ref_span.clone(),
            ));
            return None;
        };

        match &spread_ty.kind {
            TyTypeKind::Reference(id) => {
                spread_id = *id;
            }
            _ => break,
        }
    }

    let spread_ty = &compilation.types[spread_id];

    match &spread_ty.kind {
        TyTypeKind::Struct(spread_struct) => {
            let Some(literal) = &spread_struct.literal else {
                // Not a literal struct, therefore has infinite fields and shouldn't be spread
                compilation.errors.push(CompilerError::new(
                    "Expected struct literal in struct spread",
                    ref_span.clone(),
                ));
                return None;
            };

            Some(&literal.fields)
        }
        _ => {
            compilation.errors.push(CompilerError::new(
                "Expected struct type in struct spread",
                ref_span.clone(),
            ));
            None
        }
    }
}
