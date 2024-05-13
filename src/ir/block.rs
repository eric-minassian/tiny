use std::{cell::RefCell, rc::Rc};

use crate::lexer::IdentifierId;

use super::{
    inheriting_hashmap::InheritingHashMap,
    ssa::{Instruction, Operator, StoredBinaryOpcode},
    InstructionId,
};

pub type BasicBlockId = usize;

#[derive(Debug, PartialEq)]
pub struct Body {
    root: BasicBlockId,
    blocks: Vec<BasicBlock>,
}

impl Body {
    pub fn new() -> Self {
        Self {
            root: 0,
            blocks: vec![BasicBlock::new()],
        }
    }

    pub fn from(root: BasicBlockId, blocks: Vec<BasicBlock>) -> Self {
        Self { root, blocks }
    }

    pub fn get_root(&self) -> BasicBlockId {
        self.root
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

#[derive(Debug, PartialEq)]
pub struct BasicBlock {
    pub instructions: Vec<Rc<RefCell<Instruction>>>,
    identifier_map: InheritingHashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BasicBlockId>,
    dom_instr_map: InheritingHashMap<StoredBinaryOpcode, Rc<RefCell<Instruction>>>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            identifier_map: InheritingHashMap::new(),
            edge: ControlFlowEdge::Leaf,
            dominator: None,
            dom_instr_map: InheritingHashMap::new(),
        }
    }

    pub fn from(
        instructions: Vec<Rc<RefCell<Instruction>>>,
        identifier_map: InheritingHashMap<IdentifierId, InstructionId>,
        edge: ControlFlowEdge,
        dominator: Option<BasicBlockId>,
        dom_instr_map: InheritingHashMap<StoredBinaryOpcode, Rc<RefCell<Instruction>>>,
    ) -> Self {
        Self {
            instructions,
            dominator,
            edge,
            identifier_map,
            dom_instr_map,
        }
    }

    pub fn get_dom_instr(&self, op_type: &StoredBinaryOpcode) -> Option<Rc<RefCell<Instruction>>> {
        self.dom_instr_map.get(op_type)
    }

    pub fn push_instr(&mut self, instr: Rc<RefCell<Instruction>>) {
        self.instructions.push(instr.clone());

        let instruction_borrow = instr.borrow();
        let operator = instruction_borrow.operator();
        if let Operator::StoredBinaryOp(stored_binary_opcode, _, _) = operator {
            self.dom_instr_map
                .insert(stored_binary_opcode.clone(), instr.clone());
        }
    }

    pub fn get_identifier(&mut self, identifier: &IdentifierId) -> Option<InstructionId> {
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

    pub fn get_identifier_map(&self) -> &InheritingHashMap<IdentifierId, InstructionId> {
        &self.identifier_map
    }

    pub fn get_dom_instr_map(
        &self,
    ) -> &InheritingHashMap<StoredBinaryOpcode, Rc<RefCell<Instruction>>> {
        &self.dom_instr_map
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ControlFlowEdge {
    Leaf,
    Fallthrough(BasicBlockId),
    Branch(BasicBlockId),
}
