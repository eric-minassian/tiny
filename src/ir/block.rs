use std::collections::HashMap;

use crate::lexer::IdentifierId;

use super::{ssa::Instruction, InstructionId};

#[derive(Debug, PartialEq, Clone)]
pub struct Body<'a> {
    blocks: Vec<BasicBlockData<'a>>,
    root: BasicBlock,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self {
            blocks: vec![BasicBlockData::new()],
            root: 0,
        }
    }

    pub fn get_mut_block(&mut self, block: BasicBlock) -> &mut BasicBlockData<'a> {
        &mut self.blocks[block]
    }

    pub fn insert_block(&mut self, block: BasicBlockData<'a>) -> BasicBlock {
        let block_id = self.blocks.len();
        self.blocks.push(block);
        block_id
    }
}

pub type BasicBlock = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlockData<'a> {
    body: Vec<Instruction<'a>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlock>,
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

    pub fn from(body: Vec<Instruction<'a>>, edge: ControlFlowEdge) -> Self {
        Self {
            body,
            identifier_map: HashMap::new(),
            edge,
            dominator: None,
        }
    }

    pub fn insert_instruction(&mut self, instruction: Instruction<'a>) {
        self.body.push(instruction);
    }

    pub fn insert_identifier(&mut self, identifier: IdentifierId, instruction: InstructionId) {
        self.identifier_map.insert(identifier, instruction);
    }

    pub fn update_dom(&mut self, dom: BasicBlock) {
        self.dominator = Some(dom);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlock),
    Branch(BasicBlock),
    IfStmt(BasicBlock, Option<BasicBlock>, BasicBlock),
    Loop(BasicBlock, BasicBlock),
}
