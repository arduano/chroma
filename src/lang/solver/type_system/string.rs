use std::sync::Arc;

use super::{LiteralsList, TyTypeLogic};

#[derive(Debug, Clone)]
pub struct TyString {
    literals_union: LiteralsList<TyStringLiteral>,
}

impl TyString {
    pub fn new() -> Self {
        Self {
            literals_union: LiteralsList::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyStringLiteral {
    value: Arc<str>,
}

impl TyTypeLogic for TyString {
    fn is_assignable_to(&self, other: &Self) -> bool {
        self.literals_union.is_assignable_to(&other.literals_union)
    }

    fn get_intersection(&self, other: &Self) -> Self {
        TyString {
            literals_union: self.literals_union.get_intersection(&other.literals_union),
        }
    }
}
