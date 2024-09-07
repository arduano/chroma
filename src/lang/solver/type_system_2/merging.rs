use std::sync::Arc;

use crate::lang::solver::Id;

use super::Ty2BackingStructureVariant;

pub struct MergeProcedure {
    pub conditions: Vec<MergeCondition>,
}

pub struct MergeCondition {
    pub if_variant: Id<Ty2BackingStructureVariant>,
    pub instructions: Vec<MergeInstruction>,
}

pub enum MergeInstruction {
    ModifyVariant {
        new_variant: Id<Ty2BackingStructureVariant>,
    },
    HandleAttrSetKey {
        key: Arc<str>,
        procedure: MergeProcedure,
    },
}
