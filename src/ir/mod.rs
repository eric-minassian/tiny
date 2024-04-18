pub mod block;
pub mod gen;
pub mod ssa;

use std::collections::HashMap;

use self::{
    block::Body,
    ssa::{Instruction, Operator},
};

#[derive(Debug, PartialEq, Clone)]
pub struct IrStore<'a> {
    bodies: HashMap<String, Body<'a>>,
}

impl<'a> IrStore<'a> {
    pub fn new() -> Self {
        Self {
            bodies: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstBody<'a> {
    val_map: HashMap<u32, u32>,
    instructions: Vec<Instruction<'a>>,
}

impl<'a> ConstBody<'a> {
    pub fn new() -> Self {
        Self {
            val_map: HashMap::new(),
            instructions: Vec::new(),
        }
    }

    pub fn insert(&mut self, value: u32, instruction_id: u32) {
        // If value doen't exist in the map, insert it
        if !self.val_map.contains_key(&value) {
            let instruction = Instruction::new(instruction_id, Operator::Const(value), None);

            self.val_map.insert(value, instruction_id);
            self.instructions.push(instruction);
        }
    }
}
