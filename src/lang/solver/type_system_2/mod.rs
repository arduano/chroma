use std::sync::Arc;

use ordered_set::OrderedSet;
use relationships::TypeRelationships;

use crate::lang::tokens::Span;

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

    pub fn are_types_equal(&mut self, left: Ty2TypeId, right: Ty2TypeId) -> bool {
        self.relationships
            .is_type_equal_to_type(&self.storage, left, right)
    }

    pub fn is_recursive_type(&mut self, ty: Ty2TypeId) -> bool {
        self.relationships
            .is_recursive_type(&self.storage, ty)
            .unwrap_or(false)
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
pub type Ty2BackingVariantId = Id<Ty2BackingStructureVariant>;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty2VariantChoice {
    id: Ty2VariantId,
    backing_id: Ty2BackingDataKind,
}

pub struct Ty2Type {
    name: Option<String>,
    span: Option<Span>,
    variants: Vec<Ty2VariantChoice>,
    backing: Option<Ty2BackingId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty2TypeVariant {
    span: Option<Span>,
    kind: Ty2TypeVariantKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty2BackingDataKind {
    AccessBackingData(Ty2BackingVariantId),
    ConstantWithMatchingBackingData(Ty2BackingVariantId),
    ConstantWithoutBackingData,
    VirtualTypeOnly,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ty2TypeVariantKind {
    Any,
    String(Option<Arc<str>>),
    Number(Option<i64>),
    AttribSet(OrderedSet<Arc<str>, Ty2TypeId>),
    FunctionScope(OrderedSet<ScopeKey, Ty2TypeId>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKey {
    Whatever(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DifferentKey<Key: Clone + Eq> {
    None,
    One(Key),
    Unmergeable,
}

fn options_equal_or_true<T: PartialEq>(left: Option<T>, right: Option<T>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => left == right,
        _ => true,
    }
}

fn try_merge_backing_data_kinds(
    left: Ty2BackingDataKind,
    right: Ty2BackingDataKind,
) -> Option<Ty2BackingDataKind> {
    use Ty2BackingDataKind::*;
    match (left, right) {
        // SelectBackingField can be mixed with ConstantWithMatchingBackingField
        (AccessBackingData(id1), AccessBackingData(id2))
        | (ConstantWithMatchingBackingData(id1), AccessBackingData(id2))
        | (AccessBackingData(id1), ConstantWithMatchingBackingData(id2)) => {
            if id1 == id2 {
                Some(AccessBackingData(id1))
            } else {
                None
            }
        }

        (ConstantWithMatchingBackingData(id1), ConstantWithMatchingBackingData(id2)) => {
            if id1 == id2 {
                Some(ConstantWithMatchingBackingData(id1))
            } else {
                None
            }
        }

        (ConstantWithoutBackingData, ConstantWithoutBackingData) => {
            Some(ConstantWithoutBackingData)
        }

        (VirtualTypeOnly, VirtualTypeOnly) => Some(VirtualTypeOnly),

        (VirtualTypeOnly, other) => Some(other),
        (other, VirtualTypeOnly) => Some(other),

        _ => None,
    }
}

fn find_different_key_for_merging<'a, Key: Clone + Eq>(
    storage: &'a Ty2SystemStorage,
    relationships: &mut TypeRelationships,
    left: &'a OrderedSet<Key, Ty2TypeId>,
    right: &'a OrderedSet<Key, Ty2TypeId>,
) -> DifferentKey<Key> {
    // The number of keys is the same
    if left.len() != right.len() {
        return DifferentKey::Unmergeable;
    }

    let mut different_key = DifferentKey::None;

    for (key1, value1) in left.iter() {
        let Some(value2) = right.get(key1) else {
            return DifferentKey::Unmergeable;
        };

        let equal = relationships.is_type_equal_to_type(storage, *value1, *value2);

        if !equal {
            match different_key {
                // We found one different key
                DifferentKey::None => different_key = DifferentKey::One(key1.clone()),
                // We found more than one different key, unable to merge
                DifferentKey::One(_) => {
                    return DifferentKey::Unmergeable;
                }
                // Unreachable
                DifferentKey::Unmergeable => unreachable!(),
            }
        }
    }

    different_key
}
