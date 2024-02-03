use crate::lang::solver::TypeAssignabilityQuery;

use super::{LiteralsList, TyTypeLogic};

#[derive(Debug, Clone)]
pub struct TyNumber {
    literal: Option<TyNumberLiteral>,
}

impl TyNumber {
    pub fn new() -> Self {
        Self { literal: None }
    }

    pub fn from_literal(value: f64) -> Self {
        Self {
            literal: Some(TyNumberLiteral { value }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberLiteral {
    pub value: f64,
}

impl TyTypeLogic for TyNumber {
    fn check_assignable_to(&self, other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        todo!()
    }

    fn get_intersection(&self, other: &Self) -> Self {
        todo!()
    }
}
