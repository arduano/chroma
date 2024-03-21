use crate::lang::{
    solver::{Id, ItemSet},
    tokens::TkIdent,
};

use super::LiType;

pub struct LiTypeFn {
    pub name: TkIdent,
    pub args: Vec<LiType>,
    pub expressions: ItemSet<LiTypeFnLazyValue>,
    pub ret_expr: Id<LiTypeFnLazyValue>,
}

pub struct LiTypeFnArg {
    pub name: TkIdent,
    pub ty_constraint: LiType,
}

pub enum LiTypeFnLazyValue {
    LiType(LiType),
    // LiTypeCondition(LiTypeCondition),
}

// pub struct 
