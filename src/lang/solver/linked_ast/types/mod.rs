use crate::lang::{
    ast::items::SyBinaryOp,
    solver::{Id, MId},
    tokens::{Span, TkIdent, TkInteger, TkString},
};

mod structure;
pub use structure::*;

use super::LiTypeFnLazyValue;

#[derive(Debug, Clone)]
pub struct LiType {
    pub name: Option<TkIdent>,
    pub kind: LiTypeKind,
    pub span: Span,
}

impl LiType {
    pub fn new(kind: LiTypeKind, span: Span) -> Self {
        Self {
            name: None,
            kind: kind,
            span,
        }
    }

    pub fn new_named(name: Option<TkIdent>, kind: LiTypeKind, span: Span) -> Self {
        Self {
            name,
            kind: kind,
            span,
        }
    }

    pub fn kind(&self) -> &LiTypeKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
pub enum LiTypeKind {
    Number(LiNumber),
    String(LiString),
    Boolean(LiBoolean),
    Struct(LiStruct),
    StaticTypeReference(MId<LiType>),
    BinaryExpression(LiBinaryTypeExpression),
    TypeFnLazyValue(Id<LiTypeFnLazyValue>),
    Unknown,
    Never,
}

#[derive(Debug, Clone)]
pub struct LiNumber {
    pub literal: Option<TkInteger>,
}

#[derive(Debug, Clone)]
pub struct LiString {
    pub literal: Option<TkString>,
}

#[derive(Debug, Clone)]
pub struct LiBoolean {
    pub literal: Option<bool>,
}

#[derive(Debug, Clone)]
pub struct LiBinaryTypeExpression {
    pub left: Box<LiType>,
    pub right: Box<LiType>,
    pub operator: SyBinaryOp,
}
