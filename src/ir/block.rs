use std::collections::HashMap;

use super::ssa::Instruction;

#[derive(Debug, PartialEq, Clone)]
pub struct Body<'a> {
    blocks: Vec<BasicBlockData<'a>>,
    root: Option<BasicBlock>,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            root: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlock(pub usize);

#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlockData<'a> {
    body: Vec<Instruction<'a>>,
    val_table: HashMap<String, usize>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlock>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlock),
    Branch(BasicBlock),
    IfStmt(BasicBlock, Option<BasicBlock>, BasicBlock),
    Loop(BasicBlock, BasicBlock),
}
