use std::collections::HashMap;

use super::ssa::StaticSingleAssignment;

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock<'a> {
    identifier_map: HashMap<i32, i32>,
    ssas: Vec<StaticSingleAssignment<'a>>,
    next_blocks: Vec<BasicBlock<'a>>,
}

impl<'a> BasicBlock<'a> {
    pub fn new() -> Self {
        Self {
            identifier_map: HashMap::new(),
            ssas: Vec::new(),
            next_blocks: Vec::new(),
        }
    }

    pub fn from(
        identifier_map: HashMap<i32, i32>,
        ssas: Vec<StaticSingleAssignment<'a>>,
        next_blocks: Vec<BasicBlock<'a>>,
    ) -> Self {
        Self {
            identifier_map,
            next_blocks,
            ssas,
        }
    }

    pub fn push_ssa(&mut self, ssa: StaticSingleAssignment<'a>) {
        self.ssas.push(ssa);
    }

    pub fn push_identifier(&mut self, identifier: i32, value: i32) {
        self.identifier_map.insert(identifier, value);
    }

    pub fn push_child_block(&mut self, block: BasicBlock<'a>) {
        self.next_blocks.push(block);
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::ir::ssa::Operator;

//     use super::*;

//     #[test]
//     fn basic_block_assignment() {
//         let mut constant_block = BasicBlock::from(
//             HashMap::from([(-1, 1)]),
//             vec![StaticSingleAssignment::new(1, Operator::Const(1), None)],
//             Vec::new(),
//         );

//         let main_block =
//             BasicBlock::from(HashMap::from([(-1, 1), (14, -1)]), Vec::new(), Vec::new());

//         constant_block.push_child_block(main_block);
//     }
// }
