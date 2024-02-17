use std::sync::Arc;

use crate::lang::solver::{TypeAssignabilityQuery, TypeSubsetQuery};

use super::TyTypeLogic;

#[derive(Debug, Clone)]
pub struct TyString {
    literal: Option<TyStringLiteral>,
}

impl TyString {
    pub fn new() -> Self {
        Self { literal: None }
    }

    pub fn from_literal(literal: Arc<str>) -> Self {
        Self {
            literal: Some(TyStringLiteral { value: literal }),
        }
    }
}

fn is_assignable(left: &TyString, right: &TyString) -> bool {
    match (&left.literal, &right.literal) {
        (Some(l), Some(r)) => l == r,
        (Some(_), None) => true,
        (None, Some(_)) => false,
        (None, None) => true,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyStringLiteral {
    value: Arc<str>,
}

impl TyTypeLogic for TyString {
    fn check_assignable_to(&self, other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        is_assignable(self, other)
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }

    fn is_substate_of(&self, other: &Self, _query: &mut TypeSubsetQuery) -> bool {
        is_assignable(self, other)
    }

    fn get_normalized(&self) -> Option<Self> {
        None
    }

    fn get_inner_types(&self) -> Vec<crate::lang::solver::MId<super::TyType>> {
        Vec::new()
    }
}
