use std::collections::HashMap;

use linked_hash_set::LinkedHashSet;

use crate::lexer::IdentifierId;

use super::{ssa::Instruction, InstructionId};

pub type BasicBlockId = usize;

#[derive(Debug, PartialEq)]
pub struct Body<'a> {
    root: BasicBlockId,
    blocks: Vec<BasicBlock<'a>>,
    instruction_count: u32,
}

impl<'a> Body<'a> {
    pub fn new() -> Self {
        Self {
            root: 0,
            blocks: vec![BasicBlock::new()],
            instruction_count: 1,
        }
    }

    pub fn from(root: BasicBlockId, blocks: Vec<BasicBlock<'a>>, instruction_count: u32) -> Self {
        Self {
            root,
            blocks,
            instruction_count,
        }
    }

    pub fn get_root(&self) -> BasicBlockId {
        self.root
    }

    pub fn get_instruction_count(&self) -> u32 {
        self.instruction_count
    }

    pub fn increment_instruction_count(&mut self) {
        self.instruction_count += 1;
    }

    pub fn insert_block(&mut self, block: BasicBlock<'a>) -> BasicBlockId {
        let id = self.blocks.len();
        self.blocks.push(block);
        id
    }

    pub fn get_mut_block(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock<'a>> {
        self.blocks.get_mut(id)
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock<'a> {
    instructions: Vec<Instruction<'a>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlockId>,
    modified_identifiers: LinkedHashSet<IdentifierId>,
}

impl<'a> BasicBlock<'a> {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            identifier_map: HashMap::new(),
            edge: ControlFlowEdge::Leaf,
            dominator: None,
            modified_identifiers: LinkedHashSet::new(),
        }
    }

    pub fn from(
        instructions: Vec<Instruction<'a>>,
        identifier_map: HashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlockId>,
        modified_identifiers: LinkedHashSet<IdentifierId>,
    ) -> Self {
        Self {
            instructions,
            dominator,
            edge,
            identifier_map,
            modified_identifiers,
        }
    }

    pub fn update_instructions(&mut self, instructions: Vec<Instruction<'a>>) {
        self.instructions = instructions;
    }

    pub fn update_identifier_map(&mut self, identifier_map: HashMap<IdentifierId, InstructionId>) {
        self.identifier_map = identifier_map
    }

    pub fn update_modified_identifiers(
        &mut self,
        modified_identifiers: LinkedHashSet<IdentifierId>,
    ) {
        self.modified_identifiers = modified_identifiers;
    }

    pub fn insert_instruction(&mut self, instruction: Instruction<'a>) {
        self.instructions.push(instruction);
    }

    pub fn get_identifier(&mut self, identifier: &IdentifierId) -> Option<&InstructionId> {
        self.identifier_map.get(identifier)
    }

    pub fn insert_identifier(&mut self, identifier: IdentifierId, instruction: InstructionId) {
        self.identifier_map.insert(identifier, instruction);
        self.modified_identifiers.insert(identifier);
    }

    pub fn update_dominator(&mut self, dom: BasicBlockId) {
        self.dominator = Some(dom);
    }

    pub fn update_edge(&mut self, edge: ControlFlowEdge) {
        self.edge = edge;
    }

    pub fn get_identifier_map_copy(&self) -> HashMap<IdentifierId, InstructionId> {
        self.identifier_map.clone()
    }

    pub fn get_modified_identifiers(&self) -> &LinkedHashSet<IdentifierId> {
        &self.modified_identifiers
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
