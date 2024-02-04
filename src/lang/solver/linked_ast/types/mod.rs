use crate::lang::{
    solver::MId,
    tokens::{TkIdent, TkInteger, TkString},
};

mod structure;
pub use structure::*;

#[derive(Debug, Clone)]
pub struct LiType {
    pub name: Option<TkIdent>,
    pub kind: LiTypeKind,
}

impl LiType {
    pub fn new(kind: LiTypeKind) -> Self {
        Self {
            name: None,
            kind: kind,
        }
    }

    pub fn new_named(name: Option<TkIdent>, kind: LiTypeKind) -> Self {
        Self { name, kind: kind }
    }

    pub fn kind(&self) -> &LiTypeKind {
        &self.kind
    }
}

#[derive(Debug, Clone)]
pub enum LiTypeKind {
    Number(LiNumber),
    String(LiString),
    Struct(LiStruct),
    StaticTypeReference(MId<LiType>),
    Unknown,
    Never,
}

#[derive(Debug, Clone)]
pub struct LiNumber {
    pub number: TkInteger,
}

#[derive(Debug, Clone)]
pub struct LiString {
    pub string: TkString,
}
