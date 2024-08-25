use std::fmt::Display;

use crate::lang::{
    analysis::{AnLogicBlock, AnLogicBlockContainer},
    ast::items::SyBinaryOp,
    solver::{Id, Id2, ItemList, ItemSet},
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

pub type ArgumentId = Id<Li2Argument>;

impl Display for ArgumentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "arg{}", self.index())
    }
}

pub struct Li2ExpressionFunction {
    pub span: Option<Span>,
    pub name: Option<TkIdent>,
    pub arguments: ItemList<Li2Argument>,
    pub type_arguments: ItemList<Li2TypeArgument>,
    pub blocks: ItemList<Li2ExpressionBlock>,
    pub variables: ItemList<Li2Variable>,
}

impl Li2ExpressionFunction {
    pub fn iter_all_statements(
        &self,
    ) -> impl '_ + Iterator<Item = (StatementId, &Li2ExpressionStatement)> {
        self.blocks.iter().flat_map(|(bid, block)| {
            block
                .statements
                .iter()
                .map(move |(sid, statement)| (StatementId::new(bid, sid), statement))
        })
    }
}

impl Display for Li2ExpressionFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn <")?;
        let mut first = true;
        for (id, _) in self.arguments.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "Ty{}", id.index())?;
        }
        write!(f, ">(")?;

        let mut first = true;
        for (id, _) in self.type_arguments.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "arg{}", id.index())?;
        }

        writeln!(f, ") {{")?;

        for (id, variable) in self.variables.iter() {
            writeln!(f, "  var {}: {}", id, variable.name.ident)?;
        }

        for (block_id, block) in self.blocks.iter() {
            writeln!(f, "  block {}:", block_id)?;
            for (stmt_id, statement) in block.statements.iter() {
                let statement_id = StatementId::new(block_id, stmt_id);
                writeln!(f, "    {}: {}", statement_id, statement)?;
            }
            writeln!(f, "    {}", block.end)?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

impl AnLogicBlockContainer for Li2ExpressionFunction {
    type Block = Li2ExpressionBlock;
    type BlockId = BlockId;

    fn iter_blocks(&self) -> impl '_ + Iterator<Item = (Self::BlockId, &Self::Block)> {
        self.blocks.iter()
    }

    fn get_block(&self, id: Self::BlockId) -> Option<&Self::Block> {
        self.blocks.get(id)
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
    pub statements: ItemList<Li2ExpressionStatement>,
    pub end: Li2BlockEnd,
}

impl AnLogicBlock for Li2ExpressionBlock {
    type BlockId = BlockId;

    fn forward_out_jumps_as_vec(&self) -> Vec<Self::BlockId> {
        self.end.direction_jumps_as_vec(Li2JumpDirection::Forward)
    }

    fn cyclic_out_jumps_as_vec(&self) -> Vec<Self::BlockId> {
        self.end.direction_jumps_as_vec(Li2JumpDirection::Cyclic)
    }
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

pub enum Li2ExpressionStatementKind {
    GetArg {
        arg: Id<Li2Argument>,
    },
    ReadVar {
        source: VariableId,
    },
    WriteVar {
        destination: VariableId,
        value: StatementId,
    },
    BinaryOp {
        left: StatementId,
        right: StatementId,
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
            Self::GetArg { arg } => write!(f, "get {}", arg),
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

impl Li2BlockEnd {
    pub fn direction_jumps_as_vec(&self, direction: Li2JumpDirection) -> Vec<BlockId> {
        let mut vec = Vec::new();
        match &self.kind {
            Li2BlockEndKind::Jump { to } => {
                if to.direction == direction {
                    vec.push(to.target);
                }
            }
            Li2BlockEndKind::JumpIf {
                if_true, if_false, ..
            } => {
                if if_true.direction == direction {
                    vec.push(if_true.target);
                }
                if if_false.direction == direction {
                    vec.push(if_false.target);
                }
            }
            Li2BlockEndKind::Return { .. } => {}
            Li2BlockEndKind::Unknown => {}
        }

        vec
    }
}

impl Display for Li2BlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Li2Jump {
    pub direction: Li2JumpDirection,
    pub target: BlockId,
}

impl Li2Jump {
    pub fn new_forward(target: BlockId) -> Self {
        Self {
            direction: Li2JumpDirection::Forward,
            target,
        }
    }

    pub fn new_cyclic(target: BlockId) -> Self {
        Self {
            direction: Li2JumpDirection::Cyclic,
            target,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Li2JumpDirection {
    Forward,
    Cyclic,
}

pub enum Li2BlockEndKind {
    Jump {
        to: Li2Jump,
    },
    JumpIf {
        cond: StatementId,
        if_true: Li2Jump,
        if_false: Li2Jump,
    },
    Return {
        value: StatementId,
    },
    Unknown,
}

impl Display for Li2BlockEndKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump { to } => write!(f, "jump {}", to.target),
            Self::JumpIf {
                cond,
                if_true,
                if_false,
            } => write!(
                f,
                "jump if {} then {} else {}",
                cond, if_true.target, if_false.target
            ),
            Self::Return { value } => write!(f, "return {}", value),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
