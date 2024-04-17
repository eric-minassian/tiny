use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::ssa::StaticSingleAssignment;

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    identifier_map: HashMap<i32, i32>,
    ssas: Vec<Rc<StaticSingleAssignment>>,
    next_blocks: Vec<Rc<RefCell<BasicBlock>>>,
    dominator: Option<Rc<RefCell<BasicBlock>>>,
}

impl BasicBlock {
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
        ssas: Vec<Rc<StaticSingleAssignment>>,
        next_blocks: Vec<Rc<RefCell<BasicBlock>>>,
        dominator: Option<Rc<RefCell<BasicBlock>>>,
    ) -> Self {
        Self {
            identifier_map,
            ssas,
            next_blocks,
            dominator,
        }
    }

    pub fn push_next_block(&mut self, block: Rc<RefCell<BasicBlock>>) {
        self.next_blocks.push(block);
    }
}
