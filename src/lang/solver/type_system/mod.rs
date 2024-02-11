mod number;
pub use number::*;
mod string;
pub use string::*;
mod structure;
pub use structure::*;
mod union;
pub use union::*;

use crate::lang::tokens::{Span, TkIdent};

use super::{MId, TypeAssignabilityQuery, TypeSubsetQuery};

#[derive(Debug, Clone)]
pub struct TyType {
    pub name: Option<TkIdent>,
    pub span: Span,
    pub kind: TyTypeKind,
}

impl TyType {
    pub fn new(kind: TyTypeKind, span: Span) -> Self {
        Self {
            name: None,
            kind,
            span,
        }
    }

    pub fn new_named(name: Option<TkIdent>, kind: TyTypeKind, span: Span) -> Self {
        Self { name, kind, span }
    }

    pub fn kind(&self) -> &TyTypeKind {
        &self.kind
    }

    pub fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool {
        self.kind.check_assignable_to(&other.kind, query)
    }

    pub fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        self.kind.is_substate_of(&other.kind, query)
    }
}

#[derive(Debug, Clone)]
pub enum TyTypeKind {
    Number(TyNumber),
    String(TyString),
    Struct(TyStruct),
    Union(TyUnion),
    Never,
    Unknown,
}

trait TyTypeLogic {
    /// Check if one type is assignable to another. E.g. `{ foo: string, bar: string }` is assignable to `{ foo: string }`
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool;

    /// Check if a type is a substate of another. E.g. `4` is a substate of `number`
    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool;

    fn get_intersection(&self, other: &Self) -> Self;
}

impl TyTypeLogic for TyTypeKind {
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool {
        match (self, other) {
            (_, TyTypeKind::Unknown) => true,
            (TyTypeKind::Unknown, _) => true,

            (TyTypeKind::Never, TyTypeKind::Never) => true,
            (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => {
                self_number.check_assignable_to(other_number, query)
            }
            (TyTypeKind::String(self_string), TyTypeKind::String(other_string)) => {
                self_string.check_assignable_to(other_string, query)
            }
            (TyTypeKind::Struct(self_struct), TyTypeKind::Struct(other_struct)) => {
                self_struct.check_assignable_to(other_struct, query)
            }

            (TyTypeKind::Union(self_union), TyTypeKind::Union(other_union)) => {
                self_union.check_assignable_to(other_union, query)
            }
            (TyTypeKind::Union(self_union), _) => {
                self_union.check_assignable_to_single(other, query)
            }
            (_, TyTypeKind::Union(other_union)) => {
                other_union.check_single_assignable_to_self(self, query)
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

    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        match (self, other) {
            (TyTypeKind::Never, _) => true,
            (_, TyTypeKind::Never) => false,

            (TyTypeKind::Unknown, _) => true,
            (_, TyTypeKind::Unknown) => false,

            (TyTypeKind::Number(self_number), TyTypeKind::Number(other_number)) => {
                self_number.is_substate_of(other_number, query)
            }
            (TyTypeKind::String(self_string), TyTypeKind::String(other_string)) => {
                self_string.is_substate_of(other_string, query)
            }
            (TyTypeKind::Struct(self_struct), TyTypeKind::Struct(other_struct)) => {
                self_struct.is_substate_of(other_struct, query)
            }

            (TyTypeKind::Union(self_union), TyTypeKind::Union(other_union)) => {
                self_union.is_substate_of(other_union, query)
            }
            (TyTypeKind::Union(self_union), _) => self_union.is_substate_of_single(other, query),
            (_, TyTypeKind::Union(other_union)) => {
                other_union.is_single_substate_of_self(self, query)
            }

            _ => false,
        }
    }
}

impl TyTypeLogic for TyType {
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool {
        self.kind.check_assignable_to(&other.kind, query)
    }

    fn get_intersection(&self, other: &Self) -> Self {
        Self {
            name: None,
            kind: self.kind.get_intersection(&other.kind),
            span: Span::new_empty(),
        }
    }

    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        self.kind.is_substate_of(&other.kind, query)
    }
}

pub enum TyTypeOrBorrowRef<'a> {
    Owned(TyType),
    Borrowed(&'a TyType),
    Ref(MId<TyType>),
}
