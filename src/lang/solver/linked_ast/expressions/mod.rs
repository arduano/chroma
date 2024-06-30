use crate::lang::{
    ast::items::SyBinaryOp,
    solver::{Id, MId},
    tokens::{Span, TkIdent, TkInteger, TkString},
};

mod structure;
pub use structure::*;

use super::LiTypeFnLazyValue;

#[derive(Debug, Clone)]
pub struct LiExpression {
    pub name: Option<TkIdent>,
    pub kind: LiExpressionKind,
    pub span: Span,
}

impl LiExpression {
    pub fn new(kind: LiExpressionKind, span: Span) -> Self {
        Self {
            name: None,
            kind: kind,
            span,
        }
    }

    pub fn new_named(name: Option<TkIdent>, kind: LiExpressionKind, span: Span) -> Self {
        Self {
            name,
            kind: kind,
            span,
        }
    }

    pub fn kind(&self) -> &LiExpressionKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
pub enum LiExpressionKind {
    Number(LiNumber),
    String(LiString),
    Boolean(LiBoolean),
    Struct(LiStruct),
    StaticReference(MId<LiExpression>),
    BinaryExpression(LiBinaryExpression),
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
pub struct LiBinaryExpression {
    pub left: Box<LiExpression>,
    pub right: Box<LiExpression>,
    pub operator: SyBinaryOp,
}
