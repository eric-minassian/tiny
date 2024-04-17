use std::{cell::RefCell, rc::Rc};

use self::block::BasicBlock;

pub mod block;
pub mod ssa;

#[derive(Debug)]
pub struct IntermediateRepresentation<'a> {
    ssa_count: u32,
    constant_block: Rc<RefCell<BasicBlock<'a>>>,
    cur_block: Rc<RefCell<BasicBlock<'a>>>,
}

impl<'a> IntermediateRepresentation<'a> {
    pub fn new() -> Self {
        Self {
            ssa_count: 0,
            constant_block: Rc::new(RefCell::new(BasicBlock::new())),
            cur_block: Rc::new(RefCell::new(BasicBlock::new())),
        }
    }

    pub fn from(
        ssa_count: u32,
        constant_block: Rc<RefCell<BasicBlock<'a>>>,
        cur_block: Rc<RefCell<BasicBlock<'a>>>,
    ) -> Self {
        Self {
            ssa_count,
            constant_block,
            cur_block,
        }
    }
}
