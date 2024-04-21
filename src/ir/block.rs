use std::collections::HashMap;

use crate::lexer::IdentifierId;

use super::{ssa::Instruction, InstructionId};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BasicBlockId(pub usize);

#[derive(Debug, PartialEq)]
pub struct Body<'a> {
    root: Option<BasicBlockId>,
    blocks: Vec<BasicBlock<'a>>,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self {
            root: None,
            blocks: Vec::new(),
        }
    }

    pub fn from(root: BasicBlockId, blocks: Vec<BasicBlock<'a>>) -> Self {
        Self {
            root: Some(root),
            blocks,
        }
    }

    pub fn insert_block(&mut self, block: BasicBlock<'a>) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len());
        self.blocks.push(block);
        id
    }

    pub fn update_root(&mut self, root: BasicBlockId) {
        self.root = Some(root);
    }

    pub fn get_mut_block(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock<'a>> {
        self.blocks.get_mut(id.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock<'a> {
    instructions: Vec<Instruction<'a>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlockId>,
}

impl<'a> BasicBlock<'a> {
    pub fn new(instructions: Vec<Instruction<'a>>, edge: ControlFlowEdge) -> Self {
        Self {
            instructions,
            identifier_map: HashMap::new(),
            edge,
            dominator: None,
        }
    }

    pub fn from(
        instructions: Vec<Instruction<'a>>,
        identifier_map: HashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlockId>,
    ) -> Self {
        Self {
            instructions,
            dominator,
            edge,
            identifier_map,
        }
    }

    pub fn insert_instruction(&mut self, instruction: Instruction<'a>) {
        self.instructions.push(instruction);
    }

    pub fn get_identifier(&mut self, identifier: &IdentifierId) -> Option<&InstructionId> {
        self.identifier_map.get(identifier)
    }

    pub fn insert_identifier(&mut self, identifier: IdentifierId, instruction: InstructionId) {
        self.identifier_map.insert(identifier, instruction);
    }

    pub fn update_dominator(&mut self, dom: BasicBlockId) {
        self.dominator = Some(dom);
    }

    pub fn update_edge(&mut self, edge: ControlFlowEdge) {
        self.edge = edge;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlockId),
    Branch(BasicBlockId),
    IfStmt(BasicBlockId, Option<BasicBlockId>, BasicBlockId),
    Loop(BasicBlockId, BasicBlockId),
}
