use crate::lang::tokens::{Span, TkIdent};

mod kinds;
pub use kinds::*;
mod normalization;
pub use normalization::*;
mod relationships;
pub use relationships::*;

use super::{MId, ModItemSet};

#[derive(Debug, Clone, Copy)]
pub struct TyTypeFlags {
    pub is_normalized: bool,
}

impl TyTypeFlags {
    pub fn new() -> Self {
        Self {
            is_normalized: false,
        }
    }

    pub fn new_all() -> Self {
        Self {
            is_normalized: true,
        }
    }

    pub fn new_for_unknown() -> Self {
        Self {
            is_normalized: false,
        }
    }

    pub fn normalized(mut self) -> Self {
        self.is_normalized = true;
        self
    }

    pub fn join(self, other: Self) -> Self {
        Self {
            is_normalized: self.is_normalized && other.is_normalized,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyType {
    pub name: Option<TkIdent>,
    pub span: Span,
    pub kind: TyTypeKind,
    pub flags: TyTypeFlags,
}

impl TyType {
    pub fn new(kind: TyTypeKind, span: Span, flags: TyTypeFlags) -> Self {
        Self {
            name: None,
            kind,
            span,
            flags,
        }
    }

    pub fn new_infer_flags(kind: TyTypeKind, span: Span, types: &ModItemSet<TyType>) -> Self {
        Self {
            flags: kind.flags(types),
            name: None,
            kind,
            span,
        }
    }

    pub fn new_named(
        name: Option<TkIdent>,
        kind: TyTypeKind,
        span: Span,
        flags: TyTypeFlags,
    ) -> Self {
        Self {
            name,
            kind,
            span,
            flags,
        }
    }

    pub fn new_named_infer_flags(
        name: Option<TkIdent>,
        kind: TyTypeKind,
        span: Span,
        types: &ModItemSet<TyType>,
    ) -> Self {
        Self {
            flags: kind.flags(types),
            name,
            kind,
            span,
        }
    }

    pub fn new_unknown(span: Span) -> Self {
        Self {
            name: None,
            kind: TyTypeKind::Unknown,
            span,
            flags: TyTypeFlags::new_for_unknown(),
        }
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

    pub fn get_type_dependencies(&self, types: &ModItemSet<TyType>) -> TypeDependencies {
        self.kind.get_type_dependencies(types)
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

pub trait TyTypeLogic: Sized {
    /// Check if one type is assignable to another. E.g. `{ foo: string, bar: string }` is assignable to `{ foo: string }`
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool;

    /// Check if a type is a substate of another. E.g. `4` is a substate of `number`
    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool;

    fn get_intersection(&self, other: &Self) -> Self;

    /// Try to normalize the current type. If it's already normalized, return None. If it's not possible to normalize, return an error.
    fn get_normalized(
        &self,
        ctx: &mut NormalizationQuery,
    ) -> Result<Option<Self>, NormalizationError>;

    fn flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags;

    fn get_type_dependencies(&self, types: &ModItemSet<TyType>) -> TypeDependencies;
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

    fn get_normalized(
        &self,
        ctx: &mut NormalizationQuery,
    ) -> Result<Option<Self>, NormalizationError> {
        match self {
            TyTypeKind::Number(number) => Ok(number.get_normalized(ctx)?.map(TyTypeKind::Number)),
            TyTypeKind::String(string) => Ok(string.get_normalized(ctx)?.map(TyTypeKind::String)),
            TyTypeKind::Struct(struct_ty) => {
                Ok(struct_ty.get_normalized(ctx)?.map(TyTypeKind::Struct))
            }
            TyTypeKind::Union(union) => Ok(union.get_normalized(ctx)?.map(TyTypeKind::Union)),
            TyTypeKind::Never => Ok(None),
            TyTypeKind::Unknown => Ok(None),
        }
    }

    fn flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags {
        match self {
            TyTypeKind::Number(number) => number.flags(types),
            TyTypeKind::String(string) => string.flags(types),
            TyTypeKind::Struct(struct_ty) => struct_ty.flags(types),
            TyTypeKind::Union(union) => union.flags(types),
            TyTypeKind::Never => TyTypeFlags::new_all(),
            TyTypeKind::Unknown => TyTypeFlags::new_all(),
        }
    }

    fn get_type_dependencies(&self, types: &ModItemSet<TyType>) -> TypeDependencies {
        match self {
            TyTypeKind::Number(number) => number.get_type_dependencies(types),
            TyTypeKind::String(string) => string.get_type_dependencies(types),
            TyTypeKind::Struct(struct_ty) => struct_ty.get_type_dependencies(types),
            TyTypeKind::Union(union) => union.get_type_dependencies(types),
            TyTypeKind::Never => TypeDependencies::new_empty(),
            TyTypeKind::Unknown => TypeDependencies::new_empty(),
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
            flags: self.flags.join(other.flags),
        }
    }

    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        self.kind.is_substate_of(&other.kind, query)
    }

    fn get_normalized(
        &self,
        ctx: &mut NormalizationQuery,
    ) -> Result<Option<Self>, NormalizationError> {
        let result = self.kind.get_normalized(ctx)?.map(|kind| Self {
            name: self.name.clone(),
            kind,
            span: self.span.clone(),
            flags: self.flags.normalized(),
        });

        Ok(result)
    }

    fn flags(&self, _types: &ModItemSet<TyType>) -> TyTypeFlags {
        self.flags
    }

    fn get_type_dependencies(&self, types: &ModItemSet<TyType>) -> TypeDependencies {
        self.kind.get_type_dependencies(types)
    }
}

pub enum TyTypeOrBorrowRef<'a> {
    Owned(TyType),
    Borrowed(&'a TyType),
    Ref(MId<TyType>),
}

#[derive(Debug, Clone)]
pub struct TypeDependencies {
    pub inner_types: Vec<MId<TyType>>,
}

impl TypeDependencies {
    pub fn new_empty() -> Self {
        Self {
            inner_types: Vec::new(),
        }
    }
}

impl Default for TypeDependencies {
    fn default() -> Self {
        Self::new_empty()
    }
}
