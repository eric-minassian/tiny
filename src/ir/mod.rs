use self::block::BasicBlock;

pub mod block;
pub mod ssa;

#[derive(Debug, PartialEq, Eq)]
pub struct IntermediateRepresentation<'a> {
    ssa_count: u32,
    constant_block: BasicBlock<'a>,
    cur_block: BasicBlock<'a>,
}

impl<'a> IntermediateRepresentation<'a> {
    pub fn new() -> Self {
        Self {
            ssa_count: 0,
            constant_block: BasicBlock::new(),
            cur_block: BasicBlock::new(),
        }
    }

    pub fn from(ssa_count: u32, constant_block: BasicBlock<'a>, cur_block: BasicBlock<'a>) -> Self {
        Self {
            ssa_count,
            constant_block,
            cur_block,
        }
    }
}
