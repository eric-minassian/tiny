use std::{cell::RefCell, rc::Rc};

use crate::lexer::IdentifierId;

use super::{
    inheriting_hashmap::InheritingHashMap,
    ssa::{Instruction, Operator, StoredBinaryOpcode},
    InstructionId,
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BlockIndex(usize);

impl BlockIndex {
    pub fn new(index: usize) -> Self {
        Self(index)
    }
}

impl From<usize> for BlockIndex {
    fn from(index: usize) -> Self {
        Self(index)
    }
}

impl From<BlockIndex> for usize {
    fn from(index: BlockIndex) -> usize {
        index.0
    }
}

impl std::fmt::Display for BlockIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct Body {
    root: BlockIndex,
    blocks: Vec<BasicBlock>,
}

impl Body {
    pub fn new() -> Self {
        Self {
            root: BlockIndex::new(0),
            blocks: vec![BasicBlock::new()],
        }
    }

    pub fn from(root: BlockIndex, blocks: Vec<BasicBlock>) -> Self {
        Self { root, blocks }
    }

    pub fn get_root(&self) -> BlockIndex {
        self.root
    }

    pub fn insert_block(&mut self, block: BasicBlock) -> BlockIndex {
        let id = self.blocks.len().into();
        self.blocks.push(block);
        id
    }

    pub fn get_mut_block(&mut self, block_index: BlockIndex) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(usize::from(block_index))
    }

    pub fn get_block(&self, block_index: BlockIndex) -> Option<&BasicBlock> {
        self.blocks.get(usize::from(block_index))
    }

    pub fn get_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str(
            "digraph {\n\tnode [shape=record];\n\trankdir=TB;\n\tranksep=1.0;\n\tnodesep=0.5;\n\n",
        );

        for (id, block) in self.blocks.iter().enumerate() {
            dot.push_str(&format!("\tBB{} [label=\"BB{} | {{", id, id));
            for (i, instr) in block.instructions.iter().enumerate() {
                dot.push_str(&format!("{}", instr.borrow()));

                if i < block.instructions.len() - 1 {
                    dot.push_str(" | ");
                }
            }

            if block.instructions.is_empty() {
                dot.push_str("\\<empty\\>");
            }

            dot.push_str("}\"];\n");

            match block.edge {
                ControlFlowEdge::Leaf => {}
                ControlFlowEdge::Fallthrough(next) => {
                    dot.push_str(&format!(
                        "\tBB{} -> BB{} [label=\"fall-through\", fontsize=10];\n",
                        id, next.0
                    ));
                }
                ControlFlowEdge::Branch(next) => {
                    dot.push_str(&format!(
                        "\tBB{} -> BB{} [label=\"branch\", fontsize=10];\n",
                        id, next.0
                    ));
                }
            }

            for instr in block.instructions.iter() {
                let temp = instr.borrow();
                let operator = temp.operator();
                if let Operator::Branch(_, block_index, _) = operator {
                    dot.push_str(&format!(
                        "\tBB{} -> BB{} [label=\"branch\", fontsize=10];\n",
                        id, block_index.0
                    ));
                }
            }

            if let Some(dominator) = block.dominator {
                dot.push_str(&format!(
                    "\tBB{} -> BB{} [style=dotted, color=blue, fontsize=10, label=\"dom\"];\n",
                    dominator.0, id
                ));
            }
        }

        dot.push_str("}\n");
        dot
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock {
    pub instructions: Vec<Rc<RefCell<Instruction>>>,
    identifier_map: InheritingHashMap<IdentifierId, InstructionId>,
    edge: ControlFlowEdge,
    dominator: Option<BlockIndex>,
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
        dominator: Option<BlockIndex>,
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

    pub fn update_dominator(&mut self, dom: BlockIndex) {
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
    Fallthrough(BlockIndex),
    Branch(BlockIndex),
}
