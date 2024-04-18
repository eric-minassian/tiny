use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::lexer::IdentifierId;

use super::{ssa::Instruction, InstructionId};

#[derive(Debug)]
pub struct Body<'a> {
    root: Option<Rc<RefCell<BasicBlock<'a>>>>,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self { root: None }
    }

    pub fn update_root(&mut self, root: Rc<RefCell<BasicBlock<'a>>>) {
        self.root = Some(root);
    }
}

#[derive(Debug)]
pub struct BasicBlock<'a> {
    body: Vec<Instruction<'a>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge<'a>,
    dominator: Option<Weak<RefCell<BasicBlock<'a>>>>,
}

impl<'a> BasicBlock<'a> {
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

    pub fn update_dominator(&mut self, dom: Weak<RefCell<BasicBlock<'a>>>) {
        self.dominator = Some(dom);
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlowEdge<'a> {
    Leaf,
    Fallthrough(Rc<RefCell<BasicBlock<'a>>>),
    Branch(Rc<RefCell<BasicBlock<'a>>>),
    IfStmt(
        Rc<RefCell<BasicBlock<'a>>>,
        Option<Rc<RefCell<BasicBlock<'a>>>>,
        Rc<RefCell<BasicBlock<'a>>>,
    ),
    Loop(Rc<RefCell<BasicBlock<'a>>>, Rc<RefCell<BasicBlock<'a>>>),
}
