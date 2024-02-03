use std::sync::Arc;

use crate::lang::solver::TypeAssignabilityQuery;

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

#[derive(Debug, Clone, PartialEq)]
pub struct TyStringLiteral {
    value: Arc<str>,
}

impl TyTypeLogic for TyString {
    fn check_assignable_to(&self, _other: &Self, _query: &mut TypeAssignabilityQuery) -> bool {
        todo!()
    }

    fn get_intersection(&self, _other: &Self) -> Self {
        todo!()
    }
}
