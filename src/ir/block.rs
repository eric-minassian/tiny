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

    pub fn from(root: Rc<RefCell<BasicBlock<'a>>>) -> Self {
        Self { root: Some(root) }
    }

    pub fn update_root(&mut self, root: Rc<RefCell<BasicBlock<'a>>>) {
        self.root = Some(root);
    }
}

impl<'a> PartialEq for Body<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.root, &other.root) {
            (Some(a), Some(b)) => *a.borrow() == *b.borrow(),
            (None, None) => true,
            _ => false,
        }
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
    pub fn new(body: Vec<Instruction<'a>>, edge: ControlFlowEdge<'a>) -> Self {
        Self {
            body,
            identifier_map: HashMap::new(),
            edge,
            dominator: None,
        }
    }

    pub fn from(
        body: Vec<Instruction<'a>>,
        identifier_map: HashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge<'a>,
        dominator: Option<Weak<RefCell<BasicBlock<'a>>>>,
    ) -> Self {
        Self {
            body,
            dominator,
            edge,
            identifier_map,
        }
    }

    pub fn insert_instruction(&mut self, instruction: Instruction<'a>) {
        self.body.push(instruction);
    }

    pub fn insert_identifier(&mut self, identifier: IdentifierId, instruction: InstructionId) {
        self.identifier_map.insert(identifier, instruction);
    }

    pub fn update_dominator(&mut self, dom: Weak<RefCell<BasicBlock<'a>>>) {
        self.dominator = Some(dom);
    }

    pub fn update_edge(&mut self, edge: ControlFlowEdge<'a>) {
        self.edge = edge;
    }
}

impl<'a> PartialEq for BasicBlock<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
            && self.identifier_map == other.identifier_map
            && self.edge == other.edge
        // @TODO: Check dominator
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

impl<'a> PartialEq for ControlFlowEdge<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ControlFlowEdge::Leaf, ControlFlowEdge::Leaf) => true,
            (ControlFlowEdge::Fallthrough(a), ControlFlowEdge::Fallthrough(b)) => {
                *a.borrow() == *b.borrow()
            }
            (ControlFlowEdge::IfStmt(a1, a2, a3), ControlFlowEdge::IfStmt(b1, b2, b3)) => {
                *a1.borrow() == *b1.borrow()
                    && a2.as_ref().map_or(b2.is_none(), |x| {
                        b2.as_ref().map_or(false, |y| *x.borrow() == *y.borrow())
                    })
                    && *a3.borrow() == *b3.borrow()
            }
            (ControlFlowEdge::Loop(a1, a2), ControlFlowEdge::Loop(b1, b2)) => {
                *a1.borrow() == *b1.borrow() && *a2.borrow() == *b2.borrow()
            }
            _ => false,
        }
    }
}
