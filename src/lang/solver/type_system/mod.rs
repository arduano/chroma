use std::sync::Arc;

mod number;
pub use number::*;
mod string;
pub use string::*;
mod structure;
pub use structure::*;

use crate::lang::{entity_ids::Id, tokens::TkIdent};

#[derive(Debug, Clone)]
pub struct TyType {
    name: Option<TkIdent>,
    kind: Arc<TyTypeKind>,
}

impl TyType {
    pub fn new(kind: TyTypeKind) -> Self {
        Self {
            name: None,
            kind: Arc::new(kind),
        }
    }

    pub fn new_named(name: Option<TkIdent>, kind: TyTypeKind) -> Self {
        Self {
            name,
            kind: Arc::new(kind),
        }
    }

    pub fn kind(&self) -> &TyTypeKind {
        &self.kind
    }
}

#[derive(Debug, Clone)]
pub enum TyTypeKind {
    Number(TyNumber),
    String(TyString),
    Struct(TyStruct),
    Reference(Id<TyType>),
    Never,
    Unknown,
}

trait TyTypeLogic {
    fn is_assignable_to(&self, other: &Self) -> bool;
    fn get_intersection(&self, other: &Self) -> Self;
}

impl TyTypeLogic for TyTypeKind {
    fn is_assignable_to(&self, other: &Self) -> bool {
        match (self, other) {
            (_, TyTypeKind::Unknown) => true,
            (TyTypeKind::Unknown, _) => true,

            (TyTypeKind::Never, TyTypeKind::Never) => true,
            (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => {
                self_number.is_assignable_to(other_number)
            }
            (TyTypeKind::String(self_string), TyTypeKind::String(other_string)) => {
                self_string.is_assignable_to(other_string)
            }
            (TyTypeKind::Struct(self_struct), TyTypeKind::Struct(other_struct)) => {
                self_struct.is_assignable_to(other_struct)
            }

            _ => false,
        }
    }

    fn get_intersection(&self, other: &Self) -> Self {
        match (self, other) {
            (TyTypeKind::Never, _) => TyTypeKind::Never,
            (_, TyTypeKind::Never) => TyTypeKind::Never,

            (TyTypeKind::Unknown, _) => other.clone(),
            (_, TyTypeKind::Unknown) => self.clone(),

            (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => {
                TyTypeKind::Number(self_number.get_intersection(other_number))
            }
            (TyTypeKind::String(self_string), TyTypeKind::String(other_string)) => {
                TyTypeKind::String(self_string.get_intersection(other_string))
            }
            (TyTypeKind::Struct(self_struct), TyTypeKind::Struct(other_struct)) => {
                TyTypeKind::Struct(self_struct.get_intersection(other_struct))
            }

            _ => TyTypeKind::Unknown,
        }
    }
}

impl TyTypeLogic for TyType {
    fn is_assignable_to(&self, other: &Self) -> bool {
        self.kind.is_assignable_to(&other.kind)
    }

    fn get_intersection(&self, other: &Self) -> Self {
        Self {
            name: None,
            kind: Arc::new(self.kind.get_intersection(&other.kind)),
        }
    }
}

#[derive(Debug, Clone)]
struct LiteralsList<T: PartialEq + Clone> {
    literals: Arc<[T]>,
}

impl<T: PartialEq + Clone> LiteralsList<T> {
    fn new() -> Self {
        Self {
            literals: [].into(),
        }
    }

    pub fn from_literal(literal: T) -> Self {
        Self {
            literals: [literal].into(),
        }
    }

    fn is_generic(&self) -> bool {
        self.literals.is_empty()
    }

    fn contains(&self, literal: &T) -> bool {
        self.literals.contains(literal)
    }

    fn is_assignable_to(&self, other: &Self) -> bool {
        // If other is a generic number type, then any other number type can be assigned to it.
        if other.is_generic() {
            return true;
        }

        // If self is a generic number type, then it can't be assigned to a non generic number type.
        if self.is_generic() {
            return false;
        }

        // If both are non generic number types, then self can be assigned to other if all of its
        // literals are in other.
        self.literals.iter().all(|literal| other.contains(literal))
    }

    fn get_intersection(&self, other: &Self) -> Self {
        // If both are a generic number type, then the intersection is also a generic number type.
        if self.is_generic() && other.is_generic() {
            return Self::new();
        }

        // If one is a generic number type, then the intersection is the other.
        if self.is_generic() {
            return other.clone();
        }

        if other.is_generic() {
            return self.clone();
        }

        // If both are non generic number types, then the intersection is the literals that are in
        // both.
        let mut literals = Vec::new();
        for literal in &*self.literals {
            if other.contains(literal) {
                literals.push(literal.clone());
            }
        }

        Self {
            literals: literals.into(),
        }
    }

    fn get_union(&self, other: &Self) -> Self {
        // If either is a generic type, then the union is also a generic type.
        if self.is_generic() || other.is_generic() {
            return Self::new();
        }

        // If both are non generic number types, then the union is the *unique* literals that are in either.
        let mut literals = self.literals.to_vec();
        for literal in &*other.literals {
            if !literals.contains(literal) {
                literals.push(literal.clone());
            }
        }

        Self {
            literals: literals.into(),
        }
    }
}
