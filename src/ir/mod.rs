pub mod block;
mod dot;
pub mod gen;
pub mod inheriting_hashmap;
pub mod instruction;

use std::collections::HashSet;

use dot::{dot_edge, EdgeColor, EdgeLabel, EdgeStyle};

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

        dot.push_str("digraph ir {\n");

        dot.push_str(self.const_block.dot().as_str());

        for (name, body) in &self.bodies {
            dot.push_str(body.dot(name).as_str());
        }

        for (name, _) in &self.bodies {
            let to = format!("BB0_{}", name);
            dot.push_str(&dot_edge(
                "const_block",
                &to,
                EdgeLabel::Branch,
                EdgeColor::Black,
                EdgeStyle::Solid,
            ));
            dot.push_str(&dot_edge(
                "const_block",
                &to,
                EdgeLabel::Dom,
                EdgeColor::Blue,
                EdgeStyle::Dotted,
            ));
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

        let mut constants = self.constants.iter().collect::<Vec<_>>();
        constants.sort_unstable();
        for (i, constant) in constants.into_iter().rev().enumerate() {
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
