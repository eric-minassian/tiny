use std::{collections::HashMap, rc::Rc};

use linked_hash_set::LinkedHashSet;

use crate::lexer::IdentifierId;

use super::{
    ssa::{Instruction, OperatorType},
    InstructionId,
};

pub type BasicBlockId = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    root: BasicBlockId,
    blocks: Vec<BasicBlock>,
    instruction_count: u32,
}

impl Body {
    pub fn new() -> Self {
        Self {
            root: 0,
            blocks: vec![BasicBlock::new()],
            instruction_count: 1,
        }
    }

    pub fn from(root: BasicBlockId, blocks: Vec<BasicBlock>, instruction_count: u32) -> Self {
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

    pub fn insert_block(&mut self, block: BasicBlock) -> BasicBlockId {
        let id = self.blocks.len();
        self.blocks.push(block);
        id
    }

    pub fn get_mut_block(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(id)
    }

    pub fn get_block(&self, id: BasicBlockId) -> Option<&BasicBlock> {
        self.blocks.get(id)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlock {
    instructions: Vec<Rc<Instruction>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlockId>,
    modified_identifiers: LinkedHashSet<IdentifierId>,
    dom_instr_map: HashMap<OperatorType, Rc<Instruction>>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            identifier_map: HashMap::new(),
            edge: ControlFlowEdge::Leaf,
            dominator: None,
            modified_identifiers: LinkedHashSet::new(),
            dom_instr_map: HashMap::new(),
        }
    }

    pub fn from(
        instructions: Vec<Rc<Instruction>>,
        identifier_map: HashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlockId>,
        modified_identifiers: LinkedHashSet<IdentifierId>,
        dom_instr_map: HashMap<OperatorType, Rc<Instruction>>,
    ) -> Self {
        Self {
            instructions,
            dominator,
            edge,
            identifier_map,
            modified_identifiers,
            dom_instr_map,
        }
    }

    pub fn remove_dom_instr(&mut self, op_type: OperatorType) -> Option<Rc<Instruction>> {
        self.dom_instr_map.remove(&op_type)
    }

    pub fn get_dom_instr(&self, op_type: &OperatorType) -> Option<&Rc<Instruction>> {
        self.dom_instr_map.get(op_type)
    }

    pub fn push_instr(&mut self, instr: Instruction, op_type: OperatorType) {
        let instr = Rc::new(instr);
        self.instructions.push(instr.clone());
        self.dom_instr_map.insert(op_type, instr);
    }

    pub fn update_instructions(&mut self, instructions: Vec<Rc<Instruction>>) {
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
