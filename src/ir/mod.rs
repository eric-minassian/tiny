pub mod block;
pub mod ssa;

use std::collections::HashMap;

use crate::lexer::IdentifierId;

use self::{
    block::Body,
    ssa::{Instruction, Operator},
};

#[derive(Debug)]
pub struct IrStore<'a> {
    bodies: HashMap<String, Body<'a>>,
}

impl<'a> IrStore<'a> {
    pub fn new() -> Self {
        Self {
            bodies: HashMap::new(),
        }
    }

    pub fn from(bodies: HashMap<String, Body<'a>>) -> Self {
        Self { bodies }
    }

    pub fn insert(&mut self, name: String, body: Body<'a>) {
        self.bodies.insert(name, body);
    }
}

pub type InstructionId = u32;

#[derive(Debug, PartialEq, Clone)]
pub struct ConstBody<'a> {
    val_map: HashMap<IdentifierId, InstructionId>,
    instructions: Vec<Instruction<'a>>,
}

impl<'a> ConstBody<'a> {
    pub fn new() -> Self {
        Self {
            val_map: HashMap::new(),
            instructions: Vec::new(),
        }
    }

    pub fn get_instruction_id(&self, key: u32) -> Option<&InstructionId> {
        self.val_map.get(&(key as IdentifierId))
    }

    pub fn insert(&mut self, value: u32, instruction_id: InstructionId) {
        // If value doen't exist in the map, insert it
        if !self.val_map.contains_key(&(value as IdentifierId)) {
            let instruction = Instruction::new(instruction_id, Operator::Const(value), None);

            self.val_map.insert(value as IdentifierId, instruction_id);
            self.instructions.push(instruction);
        }
    }

    pub fn get_instructions(&self) -> &Vec<Instruction<'a>> {
        &self.instructions
    }
}
