use std::sync::Arc;

use logos::Span;
use ordered_set::OrderedSet;

use super::{Id, ItemSet};

pub mod ordered_set;
mod relationships;

pub struct Ty2System {
    storage: Ty2SystemStorage,
}

impl Ty2System {
    pub fn new() -> Self {
        Self {
            storage: Ty2SystemStorage::new(),
        }
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

    pub fn insert_type_variant(&mut self, ty: Ty2TypeVariant) -> Ty2VariantId {
        self.type_variants.add_value(ty)
    }

    pub fn insert_type(&mut self, ty: Ty2Type) -> Ty2TypeId {
        self.types.add_value(ty)
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

pub struct Ty2TypeVariant {
    span: Option<Span>,
    kind: Ty2TypeVariantKind,
}

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
