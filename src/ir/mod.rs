pub mod block;
pub mod ssa;

use std::collections::HashMap;

use crate::lexer::IdentifierId;

use self::{
    block::Body,
    ssa::{Instruction, InstructionId, Operator},
};

#[derive(Debug, PartialEq)]
pub struct IrStore<'a> {
    bodies: HashMap<String, Body<'a>>,
    instr_count: u32,
}

impl<'a> IrStore<'a> {
    pub fn new() -> Self {
        Self {
            bodies: HashMap::new(),
            instr_count: 0,
        }
    }

    pub fn from(bodies: HashMap<String, Body<'a>>, instr_count: u32) -> Self {
        Self {
            bodies,
            instr_count,
        }
    }

    pub fn get_instr_count(&self) -> u32 {
        self.instr_count
    }

    pub fn increment_instr_count(&mut self) {
        self.instr_count += 1;
    }

    pub fn insert(&mut self, name: String, body: Body<'a>) {
        self.bodies.insert(name, body);
    }

    pub fn get_mut_body(&mut self, name: &str) -> Option<&mut Body<'a>> {
        self.bodies.get_mut(name)
    }
}

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
