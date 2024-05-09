pub mod block;
pub mod ssa;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::lexer::Number;

use self::{block::Body, ssa::InstructionId};

#[derive(Debug, PartialEq, Clone)]
pub struct IrStore {
    bodies: HashMap<String, Body>,
}

impl IrStore {
    pub fn new() -> Self {
        Self {
            bodies: HashMap::new(),
        }
    }

    pub fn from(bodies: HashMap<String, Body>) -> Self {
        Self { bodies }
    }

    pub fn insert(&mut self, name: String, body: Body) {
        self.bodies.insert(name, body);
    }

    pub fn get_mut_body(&mut self, name: &str) -> Option<&mut Body> {
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
}
