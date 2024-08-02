mod block_links;
pub use block_links::*;
mod variable_data_sources;
pub use variable_data_sources::*;

pub trait AnLogicBlockContainer {
    type BlockId: Copy + Eq + std::hash::Hash + std::fmt::Debug;
    type Block: AnLogicBlock<BlockId = Self::BlockId>;

    fn iter_blocks(&self) -> impl '_ + Iterator<Item = (Self::BlockId, &Self::Block)>;
    fn get_block(&self, id: Self::BlockId) -> Option<&Self::Block>;
}

pub trait AnLogicBlock {
    type BlockId: Copy + Eq + std::hash::Hash + std::fmt::Debug;

    fn forward_out_jumps_as_vec(&self) -> Vec<Self::BlockId>;
    fn cyclic_out_jumps_as_vec(&self) -> Vec<Self::BlockId>;
}
