use std::fmt::Display;

use crate::lang::{
    ast::items::SyBinaryOp,
    solver::{Id, Id2, ItemSet},
    tokens::{Span, TkIdent},
};

mod literal;
pub use literal::*;

pub type StatementId = Id2<Li2ExpressionBlock, Li2ExpressionStatement>;

impl Display for StatementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{}s{}", self.group().index(), self.item().index())
    }
}

pub type BlockId = Id<Li2ExpressionBlock>;

impl Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{}", self.index())
    }
}

pub type VariableId = Id<Li2Variable>;

impl Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.index())
    }
}

pub struct Li2ExpressionFunction {
    pub span: Option<Span>,
    pub name: Option<TkIdent>,
    pub arguments: ItemSet<Li2Argument>,
    pub type_arguments: ItemSet<Li2TypeArgument>,
    pub blocks: ItemSet<Li2ExpressionBlock>,
    pub variables: ItemSet<Li2Variable>,
}

impl Display for Li2ExpressionFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn <")?;
        let mut first = true;
        for (id, _) in self.arguments.iter_sorted() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "Ty{}", id.index())?;
        }
        write!(f, ">(")?;

        let mut first = true;
        for (id, _) in self.type_arguments.iter_sorted() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "arg{}", id.index())?;
        }

        writeln!(f, ") {{")?;

        for (id, variable) in self.variables.iter_sorted() {
            writeln!(f, "  var {}: {}", id, variable.name.ident)?;
        }

        for (block_id, block) in self.blocks.iter_sorted() {
            writeln!(f, "  block {}:", block_id)?;
            for (stmt_id, statement) in block.statements.iter_sorted() {
                let statement_id = StatementId::new(block_id, stmt_id);
                writeln!(f, "    {}: {}", statement_id, statement)?;
            }
            writeln!(f, "    {}", block.end)?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

pub struct Li2Argument {
    pub span: Option<Span>,
}

pub struct Li2TypeArgument {
    pub span: Option<Span>,
}

pub struct Li2Variable {
    pub span: Option<Span>,
    pub name: TkIdent,
}

impl Display for Li2Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.ident)
    }
}

pub struct Li2ExpressionBlock {
    pub statements: ItemSet<Li2ExpressionStatement>,
    pub end: Li2BlockEnd,
}

pub struct Li2ExpressionStatement {
    pub kind: Li2ExpressionStatementKind,
    pub span: Option<Span>,
}

impl Display for Li2ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub enum Li2ValueSource {
    Argument(Id<Li2Argument>),
    Statement(StatementId),
}

impl Display for Li2ValueSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Argument(arg) => write!(f, "arg{}", arg.index()),
            Self::Statement(stmt) => write!(f, "{}", stmt),
        }
    }
}

pub enum Li2ExpressionStatementKind {
    ReadVar {
        source: VariableId,
    },
    WriteVar {
        destination: VariableId,
        value: Li2ValueSource,
    },
    BinaryOp {
        left: Li2ValueSource,
        right: Li2ValueSource,
        op: SyBinaryOp,
    },
    PrimitiveLiteral {
        literal: Li2PrimitiveLiteral,
    },
    CompositeLiteral {
        literal: Li2CompositeLiteral,
    },
    Unknown,
}

impl Display for Li2ExpressionStatementKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadVar { source } => write!(f, "read {}", source),
            Self::WriteVar {
                destination: source,
                value,
            } => write!(f, "write {} = {}", source, value),
            Self::BinaryOp { left, right, op } => write!(f, "{} {} {}", left, op, right),
            Self::PrimitiveLiteral { literal } => write!(f, "literal {}", literal),
            Self::CompositeLiteral { literal } => write!(f, "literal {}", literal),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

pub struct Li2BlockEnd {
    pub kind: Li2BlockEndKind,
    pub span: Option<Span>,
}

impl Display for Li2BlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub enum Li2BlockEndKind {
    Jump {
        to: BlockId,
    },
    JumpIf {
        cond: StatementId,
        if_true: BlockId,
        if_false: BlockId,
    },
    Return {
        value: Li2ValueSource,
    },
    Unknown,
}

impl Display for Li2BlockEndKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump { to } => write!(f, "jump {}", to),
            Self::JumpIf {
                cond,
                if_true,
                if_false,
            } => write!(f, "jump if {} then {} else {}", cond, if_true, if_false),
            Self::Return { value } => write!(f, "return {}", value),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
