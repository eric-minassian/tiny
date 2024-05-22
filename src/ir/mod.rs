pub mod block;
pub mod gen;
pub mod inheriting_hashmap;
pub mod instruction;

use std::collections::{HashMap, HashSet};

use crate::lexer::Number;

use self::{block::Body, instruction::InstructionId};

#[derive(Debug, Default, PartialEq)]
pub struct IrStore {
    bodies: HashMap<String, Body>,
    pub const_block: ConstBlock,
}

impl IrStore {
    #[must_use]
    pub fn new() -> Self {
        Self {
            bodies: HashMap::new(),
            const_block: ConstBlock::new(),
        }
    }

    #[must_use]
    pub const fn from(bodies: HashMap<String, Body>, const_block: ConstBlock) -> Self {
        Self {
            bodies,
            const_block,
        }
    }

    #[must_use]
    pub fn get_body(&self, name: &str) -> Option<&Body> {
        self.bodies.get(name)
    }

    pub fn insert_body(&mut self, name: String, body: Body) {
        self.bodies.insert(name, body);
    }

    pub fn get_mut_body(&mut self, name: &str) -> Option<&mut Body> {
        self.bodies.get_mut(name)
    }

    #[must_use]
    pub fn dot(&self) -> String {
        let mut dot = String::new();

        dot.push_str("digraph input {\n");

        dot.push_str(self.const_block.dot().as_str());

        for (name, body) in &self.bodies {
            dot.push_str(body.dot(name).as_str());
        }

        for name in self.bodies.keys() {
            dot.push_str(format!("const_block -> BB0_{};\n", name).as_str());
        }

        dot.push_str("}\n");

        dot
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ConstBlock {
    constants: HashSet<Number>,
}

impl ConstBlock {
    #[must_use]
    pub fn new() -> Self {
        Self {
            constants: HashSet::new(),
        }
    }

    #[must_use]
    pub const fn from(constants: HashSet<Number>) -> Self {
        Self { constants }
    }

    pub fn insert_returning_id(&mut self, value: Number) -> InstructionId {
        if !self.constants.contains(&value) {
            self.constants.insert(value);
        }

        -(value as InstructionId)
    }

    #[must_use]
    pub fn dot(&self) -> String {
        let mut dot = String::new();

        dot.push_str("subgraph const_block {\n\tconst_block [shape=record, width=3.0, height=1.0, label=\"Const | {");

        for (i, constant) in self.constants.iter().enumerate() {
            dot.push_str(
                format!("{}: const# {}", -(*constant as InstructionId), constant).as_str(),
            );

            if i < self.constants.len() - 1 {
                dot.push_str(" | ");
            }
        }

        if self.constants.is_empty() {
            dot.push_str("empty");
        }

        dot.push_str("}\"];\n}\n");

        dot
    }
}
