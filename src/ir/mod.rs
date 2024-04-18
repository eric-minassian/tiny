use std::{cell::RefCell, rc::Rc};

use self::{block::BasicBlock, ssa::StaticSingleAssignment};

pub mod block;
pub mod ssa;

#[derive(Debug)]
pub struct IntermediateRepresentation<'a> {
    ssa_count: u32,
    constant_block: BasicBlock<'a>,
    cur_block: &'a mut BasicBlock<'a>,
}

impl<'a> IntermediateRepresentation<'a> {
    pub fn new() -> Self {
        let mut constant_block = BasicBlock::new();
        let mut cur_block = BasicBlock::new();

        constant_block.push_child_block(cur_block);

        Self {
            ssa_count: 1,
            constant_block: constant_block,
            cur_block: &mut cur_block,
        }
    }

    pub fn push_identifier_to_cur_block(&mut self, identifier: i32, value: i32) {
        let mut cur_block = self.cur_block.borrow_mut();
        cur_block.push_identifier(identifier, value);
    }

    pub fn add_constant(&mut self, value: u32) -> u32 {
        let mut constant_block = self.constant_block.borrow_mut();
        constant_block.push_ssa(StaticSingleAssignment::new(
            self.ssa_count,
            ssa::Operator::Const(value),
            None,
        ));

        drop(constant_block);

        let mut cur_block = self.cur_block.borrow_mut();
        cur_block.push_identifier(-1 * value as i32, self.ssa_count as i32);

        self.ssa_count += 1;

        self.ssa_count - 1
    }
}
