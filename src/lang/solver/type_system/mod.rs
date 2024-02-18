mod number;
use std::collections::HashSet;

pub use number::*;
mod string;
pub use string::*;
mod structure;
pub use structure::*;
mod union;
pub use union::*;

use crate::lang::tokens::{Span, TkIdent};

use super::{
    MId, ModItemSet, TypeAssignabilityQuery, TypeIdWithSpan, TypeSubsetQuery,
    TypeSubsetabilityCache,
};

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

trait TyTypeLogic: Sized {
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
        });

        Ok(result)
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
pub struct NormalizationSuccess;
#[derive(Debug, Clone)]
pub struct NormalizationError;

pub struct NormalizationQuery<'a> {
    pub types: &'a mut ModItemSet<TyType>,
    pub type_subsetability: &'a mut TypeSubsetabilityCache,
    pub already_normalized_types: &'a mut HashSet<MId<TyType>>,

    /// The required normalizations in the current ctx. If a cycle happens here, it's an error.
    required_parent_normalizations: Vec<MId<TyType>>,
    /// The parent calls outside required normalizations. If a cycle happens here, we ignore that path.
    parent_calls: Vec<MId<TyType>>,
}

impl<'a> NormalizationQuery<'a> {
    pub fn new(
        types: &'a mut ModItemSet<TyType>,
        type_subsetability: &'a mut TypeSubsetabilityCache,
        already_normalized_types: &'a mut HashSet<MId<TyType>>,
    ) -> Self {
        Self {
            types,
            type_subsetability,
            already_normalized_types,
            required_parent_normalizations: Vec::new(),
            parent_calls: Vec::new(),
        }
    }

    /// Try to normalize a required type. Return an error if it fails.
    pub fn ensure_required_type_normalized(
        &mut self,
        ty_ref: &TypeIdWithSpan,
    ) -> Result<NormalizationSuccess, NormalizationError> {
        macro_rules! bail {
            () => {
                let error_ty = TyType::new(TyTypeKind::Unknown, ty_ref.span.clone());
                self.types.replace_value(ty_ref.id, error_ty);
                return Err(NormalizationError);
            };
        }

        if self.already_normalized_types.contains(&ty_ref.id) {
            return Ok(NormalizationSuccess);
        }

        if self.required_parent_normalizations.contains(&ty_ref.id) {
            bail!();
        }

        if self.parent_calls.contains(&ty_ref.id) {
            return Ok(NormalizationSuccess);
        }

        let Some(ty) = self.types.get(ty_ref.id) else {
            bail!();
        };

        // TODO: Keep the borrow checker happy. In the future, maybe wrap all
        // the types in Arc? And have a `get` and `get_arc` function on the type sets.
        let ty = ty.clone();

        if let Some(normalized) = ty.get_normalized(self)? {
            self.types.replace_value(ty_ref.id, normalized);
        }

        self.already_normalized_types.insert(ty_ref.id);

        Ok(NormalizationSuccess)
    }

    /// Try to normalize a non required type. Same as normalizing a required type, except a
    /// new query is created with all the required parent calls being moved to the parent_calls.
    pub fn ensure_non_required_type_normalized(
        &mut self,
        ty_ref: &TypeIdWithSpan,
    ) -> Result<NormalizationSuccess, NormalizationError> {
        let mut new_parent_calls = self.parent_calls.clone();
        new_parent_calls.extend_from_slice(&self.required_parent_normalizations);

        let mut new_query = NormalizationQuery {
            types: self.types,
            type_subsetability: self.type_subsetability,
            already_normalized_types: self.already_normalized_types,
            required_parent_normalizations: Vec::new(),
            parent_calls: new_parent_calls,
        };

        new_query.ensure_required_type_normalized(ty_ref)
    }
}

#[derive(Debug, Clone)]
pub struct CantNormalize;

#[derive(Debug, Clone)]
pub struct TypeDependencies {
    pub inner_types: Vec<MId<TyType>>,
    pub normalization_deps: Result<Vec<MId<TyType>>, CantNormalize>,
}

impl TypeDependencies {
    pub fn new_empty() -> Self {
        Self {
            inner_types: Vec::new(),
            normalization_deps: Ok(Vec::new()),
        }
    }
}

impl Default for TypeDependencies {
    fn default() -> Self {
        Self::new_empty()
    }
}
