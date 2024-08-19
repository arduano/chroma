use crate::lang::solver_old::ModItemSet;

use super::{
    TyType, TyTypeFlags, TyTypeLogic, TypeAssignabilityQuery, TypeDependencies, TypeSubsetQuery,
};

#[derive(Debug, Clone)]
pub struct TyBoolean {
    pub literal: Option<TyBooleanLiteral>,
}

impl TyBoolean {
    pub fn new() -> Self {
        Self { literal: None }
    }

    pub fn from_literal(value: bool) -> Self {
        Self {
            literal: Some(TyBooleanLiteral { value }),
        }
    }
}

fn is_assignable(left: &TyBoolean, right: &TyBoolean) -> bool {
    match (&left.literal, &right.literal) {
        (Some(l), Some(r)) => l == r,
        (Some(_), None) => true,
        (None, Some(_)) => false,
        (None, None) => true,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyBooleanLiteral {
    pub value: bool,
}

impl TyTypeLogic for TyBoolean {
    fn check_assignable_to(&self, other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        is_assignable(self, other)
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }

    fn is_substate_of(&self, other: &Self, _query: &mut TypeSubsetQuery) -> bool {
        is_assignable(self, other)
    }

    fn flags(&self, _types: &ModItemSet<TyType>) -> TyTypeFlags {
        TyTypeFlags::new_all()
    }

    fn get_type_dependencies(&self, _types: &ModItemSet<TyType>) -> TypeDependencies {
        TypeDependencies::new_empty()
    }
}
