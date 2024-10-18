use std::sync::Arc;

use crate::lang::solver::Id;
use crate::lang::tokens::Span;

use super::{
    find_different_key_for_merging, ordered_set::OrderedSet, DifferentKey, Ty2TypeVariantKind,
};

use super::{
    options_equal_or_true, try_merge_backing_data_kinds, Ty2BackingStructureVariant, Ty2System,
    Ty2Type, Ty2TypeId, Ty2TypeVariant, Ty2VariantChoice, Ty2VariantId,
};

pub struct MergeProcedure {
    pub resulting_type_id: Ty2TypeId,
    pub conditions: Vec<MergeCondition>,
}

pub struct MergeCondition {
    pub if_variant: Id<Ty2BackingStructureVariant>,
    pub instructions: Vec<MergeInstruction>,
}

pub enum MergeInstruction {
    ModifyVariant {
        new_variant: Id<Ty2BackingStructureVariant>,
    },
    HandleAttrSetKey {
        key: Arc<str>,
        procedure: MergeProcedure,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MergeProcedureError {
    Unresolved,
    DifferentBackingData,
}

// pub fn merge_type_into_type(
//     system: &mut Ty2System,
//     current: Ty2TypeId,
//     incoming: Ty2TypeId,
// ) -> Result<MergeProcedure, MergeProcedureError> {
//     if let Some(merged) = merge_type_into_type_without_backing_changes(system, current, incoming) {
//         // If we can merge without backing changes, we can just merge without any actions
//         return Ok(MergeProcedure {
//             resulting_type_id: merged,
//             conditions: Vec::new(),
//         });
//     }

//     let current = system
//         .storage
//         .types
//         .get(current)
//         .ok_or(MergeProcedureError::Unresolved)?;
//     let incoming = system
//         .storage
//         .types
//         .get(incoming)
//         .ok_or(MergeProcedureError::Unresolved)?;

//     let name = current.name.clone();
//     let span = current.span.clone();
//     let backing = current.backing;

//     let current_variants = current.variants.clone();
//     let incoming_variants = incoming.variants.clone();

//     let mut new_variants = Vec::new();

//     for incoming_variant in incoming_variants.iter() {
//         let matching_current_variant = current_variants
//             .iter()
//             .find(|v| v.backing_id == incoming_variant.backing_id);

//         // If there is a matching variant, attempt to merge them together as-is
//         if let Some(matching_current_variant) = matching_current_variant {
//             let merged_ids = try_merge_backing_data_kinds(
//                 matching_current_variant.backing_id,
//                 incoming_variant.backing_id,
//             );

//             let new_variant = merge_type_variant_into_type_without_backing_changes(
//                 system,
//                 matching_current_variant.id,
//                 incoming_variant.id,
//             );

//             // If they successfully merge, add them to the new variants and continue
//             if let (Some(new_variant), Some(merged_ids)) = (new_variant, merged_ids) {
//                 new_variants.push(Ty2VariantChoice {
//                     backing_id: merged_ids,
//                     id: new_variant,
//                 });
//                 continue;
//             }


//         }
//     }

//     let new_type = Ty2Type {
//         name,
//         span,
//         variants: new_variants,
//         backing,
//     };

//     Some(system.storage.types.add_value(new_type))
// }

pub fn merge_type_into_type_without_backing_changes(
    system: &mut Ty2System,
    current: Ty2TypeId,
    incoming: Ty2TypeId,
) -> Option<Ty2TypeId> {
    let current = system.storage.types.get(current)?;
    let incoming = system.storage.types.get(incoming)?;
    let name = current.name.clone();
    let span = current.span.clone();
    let backing = current.backing;

    // If the backing types differ, they can't be merged
    if !options_equal_or_true(current.backing, incoming.backing) {
        return None;
    }

    let mut pairs = Vec::new(); // Pairs to be merged together
    let mut extras = Vec::new(); // Extra types that don't need merging and can be inserted

    for incoming_variant in incoming.variants.iter() {
        // Find a matching variant in the current type
        let current_variant = current
            .variants
            .iter()
            .find(|v| v.backing_id == incoming_variant.backing_id);

        if let Some(current_variant) = current_variant {
            // If there is a matching variant, then we can merge them together.
            pairs.push((current_variant.clone(), incoming_variant.clone()));
        } else {
            // If there is no matching variant, then we can just insert it in without merging
            extras.push(incoming_variant.clone());
        }
    }

    // Include the extras
    let mut new_variants = extras;

    // Push in all the current variants that didn't get pairs
    for current_variant in current.variants.iter() {
        let is_in_pairs = pairs.iter().any(|(a, _)| a.id == current_variant.id);
        if !is_in_pairs {
            new_variants.push(Ty2VariantChoice {
                backing_id: current_variant.backing_id,
                id: current_variant.id,
            });
        }
    }

    // Merge and push in the pairs
    for (current_variant, incoming_variant) in pairs {
        let merged_ids =
            try_merge_backing_data_kinds(current_variant.backing_id, incoming_variant.backing_id);

        let new_variant = merge_type_variant_into_type_without_backing_changes(
            system,
            current_variant.id,
            incoming_variant.id,
        );

        if let (Some(new_variant), Some(merged_ids)) = (new_variant, merged_ids) {
            new_variants.push(Ty2VariantChoice {
                backing_id: merged_ids,
                id: new_variant,
            });
        } else {
            // If we couldn't merge the variants, we can't merge without backing changes
            return None;
        }
    }

    let new_type = Ty2Type {
        name,
        span,
        variants: new_variants,
        backing,
    };

    Some(system.storage.types.add_value(new_type))
}

pub fn merge_type_variant_into_type_without_backing_changes(
    system: &mut Ty2System,
    current_id: Ty2VariantId,
    incoming_id: Ty2VariantId,
) -> Option<Ty2VariantId> {
    let current = system.storage.type_variants.get(current_id)?;
    let incoming = system.storage.type_variants.get(incoming_id)?;

    match (&current.kind, &incoming.kind) {
        // Any can be merged with any
        (Ty2TypeVariantKind::Any, Ty2TypeVariantKind::Any) => Some(current_id),
        // Any can't be merged with anything else
        (Ty2TypeVariantKind::Any, _) => None,

        // String may be merged with string
        (Ty2TypeVariantKind::String(str1), Ty2TypeVariantKind::String(str2)) => {
            match (str1, str2) {
                // Merge literal types if they are the same
                (Some(str1), Some(str2)) if str1 == str2 => Some(current_id),
                (Some(_), Some(_)) => None,

                // Merge literals into opaque strings
                (Some(_), None) => Some(incoming_id),
                (None, Some(_)) => Some(current_id),

                // Merge opaque strings into opaque strings
                (None, None) => Some(current_id),
            }
        }
        // Strings can't be merged with anything else
        (Ty2TypeVariantKind::String(_), _) => None,

        // Numbers may be merged with numbers
        (Ty2TypeVariantKind::Number(num1), Ty2TypeVariantKind::Number(num2)) => {
            match (num1, num2) {
                // Merge literal types if they are the same
                (Some(num1), Some(num2)) if num1 == num2 => Some(current_id),
                (Some(_), Some(_)) => None,

                // Merge literals into opaque numbers
                (Some(_), None) => Some(incoming_id),
                (None, Some(_)) => Some(current_id),

                // Merge opaque numbers into opaque numbers
                (None, None) => Some(current_id),
            }
        }
        // Numbers can't be merged with anything else
        (Ty2TypeVariantKind::Number(_), _) => None,

        // Booleans can be merged with booleans
        (Ty2TypeVariantKind::True, Ty2TypeVariantKind::True) => Some(current_id),
        (Ty2TypeVariantKind::False, Ty2TypeVariantKind::False) => Some(current_id),
        // Booleans can't be merged with anything else
        (Ty2TypeVariantKind::True, _) => None,
        (Ty2TypeVariantKind::False, _) => None,

        // AttribSets can be merged if they have the same keys or differ by at least 1 key
        (Ty2TypeVariantKind::AttribSet(set1), Ty2TypeVariantKind::AttribSet(set2)) => {
            let different_key = find_different_key_for_merging(
                &system.storage,
                &mut system.relationships,
                &set1,
                &set2,
            );

            match different_key {
                DifferentKey::None => Some(current_id),
                DifferentKey::Unmergeable => None,
                DifferentKey::One(different_key) => {
                    let val_from_current = *set1.get(&different_key).unwrap();
                    let val_from_incoming = *set2.get(&different_key).unwrap();

                    // TODO: Avoid these clones
                    let set1 = set1.clone();
                    let span = current.span.clone();

                    // Try to merge the types
                    let merged_ty = merge_type_into_type_without_backing_changes(
                        system,
                        val_from_current,
                        val_from_incoming,
                    )?;

                    let mut new_set = OrderedSet::new();
                    for (key, value) in set1.iter() {
                        if key == &different_key {
                            new_set.insert(key.clone(), merged_ty);
                        } else {
                            new_set.insert(key.clone(), value.clone());
                        }
                    }

                    let new_variant = Ty2TypeVariant {
                        span,
                        kind: Ty2TypeVariantKind::AttribSet(new_set),
                    };

                    Some(system.storage.type_variants.add_value(new_variant))
                }
            }
        }
        // AttribSets can't be merged with anything else
        (Ty2TypeVariantKind::AttribSet(_), _) => None,

        // FunctionScopes can be merged if they have the same keys or differ by at least 1 key
        (Ty2TypeVariantKind::FunctionScope(set1), Ty2TypeVariantKind::FunctionScope(set2)) => {
            let different_key = find_different_key_for_merging(
                &system.storage,
                &mut system.relationships,
                &set1,
                &set2,
            );

            match different_key {
                DifferentKey::None => Some(current_id),
                DifferentKey::Unmergeable => None,
                DifferentKey::One(different_key) => {
                    let val_from_current = *set1.get(&different_key).unwrap();
                    let val_from_incoming = *set2.get(&different_key).unwrap();

                    // TODO: Avoid these clones
                    let set1 = set1.clone();
                    let span = current.span.clone();

                    // Try to merge the types
                    let merged_ty = merge_type_into_type_without_backing_changes(
                        system,
                        val_from_current,
                        val_from_incoming,
                    )?;

                    let mut new_set = OrderedSet::new();
                    for (key, value) in set1.iter() {
                        if key == &different_key {
                            new_set.insert(key.clone(), merged_ty);
                        } else {
                            new_set.insert(key.clone(), value.clone());
                        }
                    }

                    let new_variant = Ty2TypeVariant {
                        span,
                        kind: Ty2TypeVariantKind::FunctionScope(new_set),
                    };

                    Some(system.storage.type_variants.add_value(new_variant))
                }
            }
        }
        // FunctionScopes can't be merged with anything else
        (Ty2TypeVariantKind::FunctionScope(_), _) => None,
    }
}
