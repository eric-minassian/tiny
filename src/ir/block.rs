use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    next_instr_id: u32,
}

impl Body {
    pub fn new() -> Self {
        Self {
            root: 0,
            blocks: vec![BasicBlock::new()],
            next_instr_id: 1,
        }
    }

    pub fn from(root: BasicBlockId, blocks: Vec<BasicBlock>, instruction_count: u32) -> Self {
        Self {
            root,
            blocks,
            next_instr_id: instruction_count,
        }
    }

    pub fn get_root(&self) -> BasicBlockId {
        self.root
    }

    pub fn get_instruction_count(&self) -> u32 {
        self.next_instr_id
    }

    pub fn increment_instruction_count(&mut self) {
        self.next_instr_id += 1;
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
    instructions: Vec<Rc<RefCell<Instruction>>>,
    identifier_map: HashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlockId>,
    dom_instr_map: HashMap<OperatorType, Rc<RefCell<Instruction>>>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            identifier_map: HashMap::new(),
            edge: ControlFlowEdge::Leaf,
            dominator: None,
            dom_instr_map: HashMap::new(),
        }
    }

    pub fn from(
        instructions: Vec<Rc<RefCell<Instruction>>>,
        identifier_map: HashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlockId>,
        dom_instr_map: HashMap<OperatorType, Rc<RefCell<Instruction>>>,
    ) -> Self {
        Self {
            instructions,
            dominator,
            edge,
            identifier_map,
            dom_instr_map,
        }
    }

    pub fn remove_dom_instr(&mut self, op_type: OperatorType) -> Option<Rc<RefCell<Instruction>>> {
        self.dom_instr_map.remove(&op_type)
    }

    pub fn get_dom_instr(&self, op_type: &OperatorType) -> Option<&Rc<RefCell<Instruction>>> {
        self.dom_instr_map.get(op_type)
    }

    pub fn push_instr(&mut self, instr: Rc<RefCell<Instruction>>, op_type: OperatorType) {
        self.instructions.push(instr.clone());
        self.dom_instr_map.insert(op_type, instr);
    }

    pub fn push_instr_no_dom(&mut self, instr: Instruction) {
        self.instructions.push(Rc::new(RefCell::new(instr)));
    }

    pub fn push_phi_instr(&mut self, instr: Instruction) {
        self.instructions
            .insert(self.instructions.len() - 1, Rc::new(RefCell::new(instr)));
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

    pub fn get_identifier_map(&self) -> &HashMap<IdentifierId, InstructionId> {
        &self.identifier_map
    }

    pub fn get_dom_instr_map_copy(&self) -> HashMap<OperatorType, Rc<RefCell<Instruction>>> {
        self.dom_instr_map.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlockId),
    Branch(BasicBlockId),
}
