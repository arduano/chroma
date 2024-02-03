use crate::lang::tokens::TkIdent;

use super::LiType;

#[derive(Debug, Clone)]
pub struct LiStruct {
    pub entries: Vec<LiStructField>,
}

#[derive(Debug, Clone)]
pub enum LiStructField {
    KeyValue(LiStructKeyValue),
    ComputedKeyValue(LiStructComputedKeyValue),
    FieldSpread(LiStructFieldSpread),
}

#[derive(Debug, Clone)]
pub struct LiStructKeyValue {
    pub key: TkIdent,
    pub value: LiType,
}

#[derive(Debug, Clone)]
pub struct LiStructComputedKeyValue {
    pub key: LiType,
    pub value: LiType,
}

#[derive(Debug, Clone)]
pub struct LiStructFieldSpread {
    pub spread: LiType,
}
