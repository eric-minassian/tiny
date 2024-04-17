use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use super::ssa::StaticSingleAssignment;

#[derive(Debug)]
pub struct BasicBlock<'a> {
    identifier_map: HashMap<i32, i32>,
    ssas: Vec<StaticSingleAssignment<'a>>,
    next_blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
    dominator: Option<Weak<RefCell<BasicBlock<'a>>>>,
}

impl<'a> BasicBlock<'a> {
    pub fn new() -> Self {
        Self {
            identifier_map: HashMap::new(),
            ssas: Vec::new(),
            next_blocks: Vec::new(),
            dominator: None,
        }
    }

    pub fn from(
        identifier_map: HashMap<i32, i32>,
        ssas: Vec<StaticSingleAssignment<'a>>,
        next_blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
        dominator: Option<Weak<RefCell<BasicBlock<'a>>>>,
    ) -> Self {
        Self {
            dominator,
            identifier_map,
            next_blocks,
            ssas,
        }
    }

    pub fn push_next_block(&mut self, block: Rc<RefCell<BasicBlock<'a>>>) {
        self.next_blocks.push(block);
    }
}
