use std::collections::HashSet;

use crate::{
    ast::{visit::AstVisitor, Computation},
    lexer::Identifier,
};

use super::{
    block::{BlockIndex, Body},
    instruction::InstructionId,
    ConstBlock, IrStore,
};

pub struct IrGenerator {
    store: IrStore,
    const_block: ConstBlock,
}

impl IrGenerator {
    pub fn gen(ast: &Computation) -> IrStore {
        let mut generator = Self::new();

        todo!()
    }

    fn new() -> Self {
        Self {
            store: IrStore::new(),
            const_block: ConstBlock::new(),
        }
    }
}

impl AstVisitor for IrGenerator {}

pub struct IrBodyGenerator<'a> {
    body: Body,
    const_block: &'a mut ConstBlock,
    cur_block: BlockIndex,
    next_instr_id: InstructionId,
    is_main: bool,
    declared_identifiers: HashSet<Identifier>,
}
