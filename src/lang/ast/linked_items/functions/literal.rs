use std::fmt::Display;

use crate::lang::tokens::{Span, TkInteger, TkString};

#[derive(Debug, Clone)]
pub struct Li2PrimitiveLiteral {
    pub span: Option<Span>,
    pub kind: Li2PrimitiveLiteralKind,
}

impl Display for Li2PrimitiveLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            Li2PrimitiveLiteralKind::String { value } => write!(f, "{}", value.value()),
            Li2PrimitiveLiteralKind::Integer { value } => write!(f, "{}", value.value()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Li2PrimitiveLiteralKind {
    String { value: TkString },
    Integer { value: TkInteger },
}

#[derive(Debug, Clone)]
pub struct Li2CompositeLiteral {
    pub span: Option<Span>,
    pub kind: Li2CompositeLiteralKind,
}

impl Display for Li2CompositeLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match &self.kind {};

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Li2CompositeLiteralKind {}
