pub mod block;
pub mod gen;
pub mod inheriting_hashmap;
pub mod instruction;

use std::collections::HashSet;

use crate::lexer::Number;

use self::{block::Body, instruction::InstructionId};

#[derive(Debug, Default, PartialEq)]
pub struct IrStore {
    pub(crate) bodies: Vec<(String, Body)>,
    pub(crate) const_block: ConstBlock,
}

impl IrStore {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub const fn from(bodies: Vec<(String, Body)>, const_block: ConstBlock) -> Self {
        Self {
            bodies,
            const_block,
        }
    }

    #[must_use]
    pub fn dot(&self) -> String {
        let mut dot = String::new();

        dot.push_str("digraph input {\n");

        for (name, body) in &self.bodies {
            dot.push_str(body.dot(name).as_str());
        }

        dot.push_str(self.const_block.dot().as_str());
        for (name, _) in &self.bodies {
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
        Self::default()
    }

    #[must_use]
    pub const fn from(constants: HashSet<Number>) -> Self {
        Self { constants }
    }

    pub fn insert_returning_id(&mut self, value: Number) -> InstructionId {
        self.constants.insert(value);

        -InstructionId::try_from(value).expect("Compiler error: Invalid constant")
    }

    #[must_use]
    pub fn dot(&self) -> String {
        let mut dot = String::new();

        dot.push_str("subgraph const_block {\n\tconst_block [shape=record, width=3.0, height=1.0, label=\"Const | {");

        for (i, constant) in self.constants.iter().enumerate() {
            dot.push_str(
                format!(
                    "{}: const# {}",
                    -InstructionId::try_from(*constant).expect("Compiler error: Invalid constant"),
                    constant
                )
                .as_str(),
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
