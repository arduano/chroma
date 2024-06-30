use logos::Span;

use crate::lang::tokens::TkIdent;

use super::LiType;

pub struct LiFunction {
    pub name: Option<TkIdent>,
    pub span: Span,
}

pub struct LiFunctionArgument {
    pub name: TkIdent,
    pub type_constraint: Option<LiType>,
    pub span: Span,
}

pub struct LiFunctionBody {
    pub name: TkIdent,
    pub type_constraint: Option<LiType>,
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

pub enum LiBodyStatementKind {}

pub struct LiBodyBlockEnd {
    pub span: Span,
}
