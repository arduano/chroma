use builtin::RtInternalBuiltin;

use crate::lang::tokens::Span;

use super::{linked_ast::LiFunction, type_system::TyType, Id, MId};

pub mod builtin;

pub struct RtFunction {
    pub linked_function: MId<LiFunction>,
    pub args: Vec<RtFunctionArg>,
}

pub struct RtFunctionArg {
    pub ty: MId<TyType>,
}

pub struct RtBlock {
    pub statements: Vec<RtStatement>,
}

pub struct RtStatement {
    pub span: Span,
    pub kind: RtStatementKind,
}

pub enum RtStatementKind {
    InternalBuiltin {
        builtin: RtInternalBuiltin,
        args: Vec<RtStatementId>,
    },
    // ExternalBuiltin {},
    FunctionCall {
        function: MId<RtFunction>,
        args: Vec<RtStatementId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RtStatementId {
    pub block_id: Id<RtBlock>,
    pub statement_id: Id<RtStatement>,
}
