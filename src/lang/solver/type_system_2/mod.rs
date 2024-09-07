use std::sync::Arc;

use logos::Span;
use ordered_set::OrderedSet;
use relationships::TypeRelationships;

use super::{Id, ItemSet};

pub mod merging;
pub mod ordered_set;
mod relationships;

pub struct Ty2System {
    storage: Ty2SystemStorage,
    relationships: TypeRelationships,
}

impl Ty2System {
    pub fn new() -> Self {
        Self {
            storage: Ty2SystemStorage::new(),
            relationships: TypeRelationships::new(),
        }
    }

    pub fn insert_type_variant(&mut self, ty: Ty2TypeVariant) -> Ty2VariantId {
        self.storage.type_variants.add_value(ty)
    }

    pub fn insert_type(&mut self, ty: Ty2Type) -> Ty2TypeId {
        self.storage.types.add_value(ty)
    }

    pub fn insert_backing(&mut self, backing: Ty2BackingStructure) -> Ty2BackingId {
        self.storage.backings.add_value(backing)
    }

    pub fn normalize_variant_list(
        &mut self,
        variants: impl IntoIterator<Item = Ty2VariantId>,
    ) -> Vec<Ty2VariantId> {
        // let mut new_variants = Vec::new();
        todo!()
    }

    pub fn are_types_equal(&mut self, left: Ty2TypeId, right: Ty2TypeId) -> bool {
        self.relationships
            .is_type_equal_to_type(&self.storage, left, right)
    }

    pub fn merge_two_types_without_backing_changes(
        &mut self,
        left: Ty2TypeId,
        right: Ty2TypeId,
    ) -> Option<Ty2TypeId> {
        let left_ty = self.storage.types.get(left)?;
        let right_ty = self.storage.types.get(right)?;

        if let (Some(left_backing), Some(right_backing)) = (&left_ty.backing, &right_ty.backing) {
            if left_backing != right_backing {
                return None;
            }
        }

        let left_variants = left_ty.variants.iter();
        let right_variants = right_ty.variants.iter();
        let all_variants = left_variants.chain(right_variants).collect::<Vec<_>>();

        // let mut new_variants = Vec::new();

        // for variant in all_variants {

        // }

        todo!()
    }
}

pub struct Ty2SystemStorage {
    pub types: ItemSet<Ty2Type>,
    pub type_variants: ItemSet<Ty2TypeVariant>,
    pub backings: ItemSet<Ty2BackingStructure>,
}

impl Ty2SystemStorage {
    pub fn new() -> Self {
        Self {
            types: ItemSet::new(),
            type_variants: ItemSet::new(),
            backings: ItemSet::new(),
        }
    }
}

pub type Ty2TypeId = Id<Ty2Type>;
pub type Ty2VariantId = Id<Ty2TypeVariant>;
pub type Ty2BackingId = Id<Ty2BackingStructure>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty2Handle {
    pub id: Ty2TypeId,
    pub backing: Option<Ty2BackingId>,
}

impl Ty2Handle {
    pub fn new(id: Ty2TypeId, backing: Option<Ty2BackingId>) -> Self {
        Self { id, backing }
    }
}

pub struct Ty2VariantChoice {
    id: Ty2VariantId,
    backing_id: Id<Ty2BackingStructureVariant>,
}

pub struct Ty2Type {
    name: Option<String>,
    span: Option<Span>,
    variants: Vec<Ty2VariantChoice>,
    backing: Option<Ty2BackingId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty2TypeVariant {
    span: Option<Span>,
    kind: Ty2TypeVariantKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty2FieldSelect {
    id: Ty2TypeId,
    kind: Ty2FieldSelectKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty2FieldSelectKind {
    SelectBackingField,
    Constant,
    VirtualTypeOnly,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ty2TypeVariantKind {
    Any,
    String(Option<Arc<str>>),
    Number(Option<i64>),
    AttribSet(OrderedSet<Arc<str>, Ty2FieldSelect>),
    True,
    False,
}

pub struct Ty2BackingStructure {
    pub variants: ItemSet<Ty2BackingStructureVariant>,
}

pub enum Ty2BackingStructureVariant {
    Number,
    String,
    Struct(OrderedSet<Arc<str>, Ty2BackingId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DifferentKey {
    None,
    One(Arc<str>),
    Many,
}

// pub fn try_merge_variants(
//     system: &mut Ty2System,
//     left_id: Ty2VariantId,
//     right_id: Ty2VariantId,
// ) -> Option<Ty2VariantId> {
//     let left_ty = system.storage.type_variants.get(left_id)?;
//     let right_ty = system.storage.type_variants.get(right_id)?;

//     match (&left_ty.kind, &right_ty.kind) {
//         // Any has no merging logic
//         (Ty2TypeVariantKind::Any, _) => None,
//         // String has no merging logic
//         (Ty2TypeVariantKind::String(left), _) => None,
//         // Number has no merging logic
//         (Ty2TypeVariantKind::Number(left), _) => None,
//         // Bools have no merging logic
//         (Ty2TypeVariantKind::True, _) => None,
//         (Ty2TypeVariantKind::False, _) => None,

//         // AttribSet can be merged if at most 1 field differs
//         (Ty2TypeVariantKind::AttribSet(set1), Ty2TypeVariantKind::AttribSet(set2)) => {
//             // If the number of keys is different, they can't be merged
//             if set1.len() != set2.len() {
//                 return None;
//             }

//             let mut different_key = DifferentKey::None;
//             for (key1, value1) in set1.iter() {
//                 let Some(value2) = set2.get(key1) else {
//                     return None;
//                 };

//                 let backing_equal = value1.kind == value2.kind;
//                 let ty_equal = system.relationships.is_type_equal_to_type(
//                     &system.storage,
//                     value1.id,
//                     value2.id,
//                 );

//                 let equal = backing_equal || ty_equal;

//                 if !equal {
//                     match different_key {
//                         // We found one different key
//                         DifferentKey::None => different_key = DifferentKey::One(key1.clone()),
//                         // We found more than one different key, unable to merge
//                         DifferentKey::One(_) => {
//                             different_key = DifferentKey::Many;
//                             break;
//                         }
//                         // Unreachable
//                         DifferentKey::Many => unreachable!(),
//                     }
//                 }
//             }

//             // If there is zero difference (types are effectively equal), return the left id
//             if different_key == DifferentKey::None {
//                 return Some(left_id);
//             }

//             // If there are more than 1 difference, they can't be merged
//             let DifferentKey::One(different_key) = different_key else {
//                 return None;
//             };

//             let mut new_set = OrderedSet::new();
//             for (key1, value1) in set1.iter() {
//                 let Some(value2) = set2.get(key1) else {
//                     unreachable!();
//                 };

//                 if key1 == &different_key {
//                     // TODO:
//                     // let merged_ty = try_merge_variants(system, value1.id, value2.id)?;
//                     // new_set.insert(key1.clone(), value2.clone());
//                 } else {
//                     new_set.insert(key1.clone(), value1.clone());
//                 }
//             }

//             let new_variant = Ty2TypeVariant {
//                 span: left_ty.span.clone(),
//                 kind: Ty2TypeVariantKind::AttribSet(new_set),
//             };

//             Some(system.storage.type_variants.add_value(new_variant))
//         }
//         (Ty2TypeVariantKind::AttribSet(_), _) => None,
//     }
// }
