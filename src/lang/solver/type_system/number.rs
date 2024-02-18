use crate::lang::solver::{ModItemSet, TypeAssignabilityQuery, TypeSubsetQuery};

use super::{
    NormalizationError, NormalizationQuery, TyType, TyTypeFlags, TyTypeLogic, TypeDependencies,
};

#[derive(Debug, Clone)]
pub struct TyNumber {
    pub literal: Option<TyNumberLiteral>,
}

impl TyNumber {
    pub fn new() -> Self {
        Self { literal: None }
    }

    pub fn from_literal(value: i64) -> Self {
        Self {
            literal: Some(TyNumberLiteral { value }),
        }
    }
}

fn is_assignable(left: &TyNumber, right: &TyNumber) -> bool {
    match (&left.literal, &right.literal) {
        (Some(l), Some(r)) => l == r,
        (Some(_), None) => true,
        (None, Some(_)) => false,
        (None, None) => true,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberLiteral {
    pub value: i64,
}

impl TyTypeLogic for TyNumber {
    fn check_assignable_to(&self, other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        is_assignable(self, other)
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }

    fn is_substate_of(&self, other: &Self, _query: &mut TypeSubsetQuery) -> bool {
        is_assignable(self, other)
    }

    fn get_normalized(
        &self,
        _ctx: &mut NormalizationQuery,
    ) -> Result<Option<Self>, NormalizationError> {
        Ok(None)
    }

    fn flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags {
        TyTypeFlags::new_all()
    }

    fn get_type_dependencies(&self, _types: &ModItemSet<TyType>) -> TypeDependencies {
        TypeDependencies::new_empty()
    }
}
