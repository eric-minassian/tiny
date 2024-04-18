use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::lexer::IdentifierId;

use super::{ssa::Instruction, InstructionId};

#[derive(Debug)]
pub struct Body<'a> {
    blocks: Vec<Rc<RefCell<BasicBlockData<'a>>>>,
    root: Option<Rc<RefCell<BasicBlockData<'a>>>>,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            root: None,
        }
    }

    pub fn insert_block(&mut self, block: Rc<RefCell<BasicBlockData<'a>>>) {
        if self.root.is_none() {
            self.root = Some(Rc::clone(&block));
        }

        self.blocks.push(block);
    }

    pub fn update_root(&mut self, root: Rc<RefCell<BasicBlockData<'a>>>) {
        self.root = Some(root);
    }
}

#[derive(Debug)]
pub struct BasicBlockData<'a> {
    body: Vec<Instruction<'a>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge<'a>,
    dominator: Option<Weak<RefCell<BasicBlockData<'a>>>>,
}

impl<'a> BasicBlockData<'a> {
    pub fn new() -> Self {
        Self {
            body: Vec::new(),
            identifier_map: HashMap::new(),
            edge: ControlFlowEdge::Leaf,
            dominator: None,
        }
    }

    pub fn from(body: Vec<Instruction<'a>>, edge: ControlFlowEdge<'a>) -> Self {
        Self {
            body,
            identifier_map: HashMap::new(),
            edge,
            dominator: None,
        }
    }

    // pub fn insert_instruction(&mut self, instruction: Instruction<'a>) {
    //     self.body.push(instruction);
    // }

    pub fn insert_identifier(&mut self, identifier: IdentifierId, instruction: InstructionId) {
        self.identifier_map.insert(identifier, instruction);
    }

    pub fn update_dominator(&mut self, dom: Weak<RefCell<BasicBlockData<'a>>>) {
        self.dominator = Some(dom);
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlowEdge<'a> {
    Leaf,
    Fallthrough(Rc<RefCell<BasicBlockData<'a>>>),
    Branch(Rc<RefCell<BasicBlockData<'a>>>),
    IfStmt(
        Rc<RefCell<BasicBlockData<'a>>>,
        Option<Rc<RefCell<BasicBlockData<'a>>>>,
        Rc<RefCell<BasicBlockData<'a>>>,
    ),
    Loop(
        Rc<RefCell<BasicBlockData<'a>>>,
        Rc<RefCell<BasicBlockData<'a>>>,
    ),
}
