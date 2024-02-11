use crate::lang::solver::TypeAssignabilityQuery;

use super::TyTypeLogic;

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

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberLiteral {
    pub value: i64,
}

impl TyTypeLogic for TyNumber {
    fn check_assignable_to(&self, _other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        match (&self.literal, &_other.literal) {
            (Some(self_literal), Some(other_literal)) => self_literal.value == other_literal.value,
            (_, None) => true,
            _ => false,
        }
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }
}
