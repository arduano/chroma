use std::collections::HashMap;

use crate::lang::{
    entity_ids::Id,
    solver::{type_system::*, ItemSet},
    ErrorCollector,
};

use super::{LiStructField, LiType, LiTypeKind};

/// A type for encapsulating which parts of this compilation step are mutated
/// and which parts are immutable. This keeps the compiler happy.
pub struct TypeFromLinkedTypeCompilation<'a> {
    pub linked_type_definitions: &'a ItemSet<LiType>,
    pub linked_type_to_type_mapping: &'a mut HashMap<Id<LiType>, Id<TyType>>,
    pub types: &'a mut ItemSet<TyType>,
    pub errors: &'a mut ErrorCollector,
}

pub fn parse_type_from_linked_type_id(
    linked_ty_id: Id<LiType>,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> Id<TyType> {
    if let Some(existing_type) = compilation.linked_type_to_type_mapping.get(&linked_ty_id) {
        return *existing_type;
    }

    let allocated_id = compilation.types.allocate_id();

    compilation
        .linked_type_to_type_mapping
        .insert(linked_ty_id, allocated_id);

    let linked_ty = &compilation.linked_type_definitions[linked_ty_id];
    let value = parse_type_from_linked_type(linked_ty, compilation);
    compilation
        .types
        .insert_allocated_value(allocated_id, value);

    allocated_id
}

pub fn parse_type_from_linked_type(
    linked_ty: &LiType,
    compilation: &mut TypeFromLinkedTypeCompilation,
) -> TyType {
    let ty_kind = match &linked_ty.kind {
        LiTypeKind::Number(num) => {
            TyTypeKind::Number(TyNumber::from_literal(num.number.value() as f64))
        }
        LiTypeKind::String(str) => {
            TyTypeKind::String(TyString::from_literal(str.string.value().clone()))
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
                    LiStructField::ComputedKeyValue(computed) => todo!(),
                    LiStructField::FieldSpread(spread) => todo!(),
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
