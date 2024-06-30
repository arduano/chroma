use crate::lang::{
    solver::{Id, ItemSet},
    tokens::TkIdent,
};

use super::LiExpression;

pub struct LiTypeFn {
    pub name: TkIdent,
    pub args: Vec<LiTypeFnArg>,
    pub expressions: ItemSet<LiTypeFnLazyValue>,
    pub ret_expr: Id<LiTypeFnLazyValue>,
}

pub struct LiTypeFnArg {
    pub name: TkIdent,
    pub ty_constraint: LiExpression,
}

pub enum LiTypeFnLazyValue {
    LiType(LiExpression),
    // LiTypeCondition(LiTypeCondition),
}

// pub struct
