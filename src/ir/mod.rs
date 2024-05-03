pub mod block;
pub mod ssa;

use std::collections::{HashMap, HashSet};

use crate::lexer::{IdentifierId, Number};

use self::{
    block::Body,
    ssa::{Instruction, InstructionId, Operator},
};

#[derive(Debug, PartialEq)]
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

    pub fn get_mut_body(&mut self, name: &str) -> Option<&mut Body<'a>> {
        self.bodies.get_mut(name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstBody {
    constants: HashSet<Number>,
}

impl ConstBody {
    pub fn new() -> Self {
        Self {
            constants: HashSet::new(),
        }
    }

    pub fn from(constants: HashSet<Number>) -> Self {
        Self { constants }
    }

    pub fn insert_returning_id(&mut self, value: Number) -> InstructionId {
        if !self.constants.contains(&value) {
            self.constants.insert(value);
        }

        value as InstructionId * -1
    }

    // pub fn get_instruction_id(&self, key: u32) -> Option<&InstructionId> {
    //     self.val_map.get(&(key as IdentifierId))
    // }

    // pub fn insert(&mut self, value: u32, instruction_id: InstructionId) {
    //     // If value doen't exist in the map, insert it
    //     if !self.val_map.contains_key(&(value as IdentifierId)) {
    //         let instruction = Instruction::new(instruction_id, Operator::Const(value), None);

    //         self.val_map.insert(value as IdentifierId, instruction_id);
    //         self.instructions.push(instruction);
    //     }
    // }
}
