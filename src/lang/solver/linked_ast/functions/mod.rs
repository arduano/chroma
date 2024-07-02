use logos::Span;

use crate::lang::tokens::{TkIdent, TkLet, TkReturn};

use super::LiExpression;

pub struct LiFunction {
    pub name: Option<TkIdent>,
    pub span: Span,
}

pub struct LiFunctionArgument {
    pub name: TkIdent,
    pub type_constraint: Option<LiExpression>,
    pub span: Span,
}

pub struct LiFunctionBody {
    pub name: TkIdent,
    pub type_constraint: Option<LiExpression>,
    pub span: Span,
}

pub struct LiFunctionBodyBlock {
    pub statements: Vec<LiBodyStatement>,
    pub block_end: LiBodyBlockEnd,
}

pub struct LiBodyStatement {
    pub kind: LiBodyStatementKind,
    pub span: Span,
}

pub enum LiBodyStatementKind {
    Expression(LiExpression),
    VariableAssignment(LiVariableAssignment),
}

pub struct LiVariableAssignment {
    pub let_token: TkLet,
    pub name: TkIdent,
    pub value: LiExpression,
}

pub struct LiBodyBlockEnd {
    pub kind: LiBodyBlockEndKind,
    pub span: Span,
}

pub enum LiBodyBlockEndKind {
    Return(LiReturnStatement),
}

pub struct LiReturnStatement {
    pub return_token: TkReturn,
    pub expr: LiExpression,
}
