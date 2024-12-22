use super::{Ty2System, Ty2TypeId, Ty2TypeVariantKind, Ty2VariantId};

pub enum NormalisingError {
    NotNormalised,
}

pub fn ensure_type_references_normalized(
    system: &mut Ty2System,
    type_id: Ty2TypeId,
) -> Result<(), NormalisingError> {
    let ty = system
        .storage
        .types
        .get(type_id)
        .ok_or(NormalisingError::NotNormalised)?;

    let backing_id = ty.backing;

    if ty.normalized_references {
        return Ok(());
    }

    let mut included_type_ids = vec![];
    let mut type_ids_to_include = vec![type_id];
    let mut final_variants = vec![];

    while let Some(ty_id) = type_ids_to_include.pop() {
        included_type_ids.push(ty_id);

        let ty = system
            .storage
            .types
            .get(type_id)
            .ok_or(NormalisingError::NotNormalised)?;

        if ty.backing != backing_id {
            panic!("Backing IDs don't match when normalizing references");
        }

        for variant_id in ty.variants.iter() {
            let variant = system
                .storage
                .type_variants
                .get(variant_id.id)
                .ok_or(NormalisingError::NotNormalised)?;

            match &variant.kind {
                Ty2TypeVariantKind::Referential(ty_id) => {
                    if included_type_ids.contains(ty_id) || type_ids_to_include.contains(ty_id) {
                        continue;
                    }

                    type_ids_to_include.push(*ty_id);
                }
                _ => {
                    final_variants.push(variant_id.clone());
                }
            }
        }
    }

    let ty = system.storage.types.get_mut(type_id).unwrap();
    ty.normalized_references = true;
    ty.variants = final_variants.clone();

    for variant in final_variants.iter() {
        let variant = system.storage.type_variants.get(variant.id).unwrap();
        match &variant.kind {
            Ty2TypeVariantKind::Referential(_) => panic!("Shouldn't have referential variants"),
            Ty2TypeVariantKind::Any
            | Ty2TypeVariantKind::String(_)
            | Ty2TypeVariantKind::Number(_)
            | Ty2TypeVariantKind::True
            | Ty2TypeVariantKind::False => {}

            Ty2TypeVariantKind::AttribSet(set) => {
                let child_ids = set.values().cloned().collect::<Vec<_>>();
                for child_id in child_ids {
                    ensure_type_references_normalized(system, child_id)?;
                }
            }

            Ty2TypeVariantKind::FunctionScope(set) => {
                let child_ids = set.values().cloned().collect::<Vec<_>>();
                for child_id in child_ids {
                    ensure_type_references_normalized(system, child_id)?;
                }
            }
        }
    }

    Ok(())
}
