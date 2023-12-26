use super::{LiteralsList, TyTypeLogic};

#[derive(Debug, Clone)]
pub struct TyNumber {
    literals_union: LiteralsList<TyNumberLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyNumberLiteral {
    value: f64,
}

impl TyTypeLogic for TyNumber {
    fn is_assignable_to(&self, other: &Self) -> bool {
        self.literals_union.is_assignable_to(&other.literals_union)
    }

    fn get_intersection(&self, other: &Self) -> Self {
        TyNumber {
            literals_union: self.literals_union.get_intersection(&other.literals_union),
        }
    }
}
