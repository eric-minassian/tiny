use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    mem::discriminant,
    rc::Rc,
};

#[derive(Debug, PartialEq, Eq)]
pub struct IntermediateRepresentation {
    ssa_count: u32,
    constant_block: BasicBlock,
    cur_block: BasicBlock,
}

impl IntermediateRepresentation {
    pub fn new() -> Self {
        Self {
            ssa_count: 0,
            constant_block: BasicBlock::new(),
            cur_block: BasicBlock::new(),
        }
    }

    pub fn from(ssa_count: u32, constant_block: BasicBlock, cur_block: BasicBlock) -> Self {
        Self {
            ssa_count,
            constant_block,
            cur_block,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    identifier_map: HashMap<usize, u32>,
    ssas: Vec<Rc<StaticSingleAssignment>>,
    next_block: Option<Box<BasicBlock>>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            identifier_map: HashMap::new(),
            ssas: Vec::new(),
            next_block: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StaticSingleAssignment {
    id: u32,
    operator: Operator,
    dominator: Option<Rc<StaticSingleAssignment>>,
}

impl StaticSingleAssignment {
    pub fn new(id: u32, operator: Operator, dominator: Option<Rc<StaticSingleAssignment>>) -> Self {
        if let Some(ssa) = &dominator {
            assert_eq!(discriminant(&operator), discriminant(&ssa.operator))
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    pub fn check_dominators(&self, ssa: Rc<StaticSingleAssignment>) -> Option<u32> {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Const(u32),
    Add(u32, u32),
    Sub(u32, u32),
    Mul(u32, u32),
    Div(u32, u32),
    Cmp(u32, u32),
    Phi(u32, u32),
    End,
    Bra(u32),
    Bne(u32, u32),
    Beq(u32, u32),
    Ble(u32, u32),
    Blt(u32, u32),
    Bge(u32, u32),
    Bgt(u32, u32),
    Read,
    Write(u32),
    WriteNL,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn ssa_different_operators() {
        let ssa = Rc::new(StaticSingleAssignment::new(1, Operator::Const(1), None));

        StaticSingleAssignment::new(2, Operator::Add(1, 1), Some(Rc::clone(&ssa)));
    }

    #[test]
    fn ssa_dominators() {
        let ssa_1 = Rc::new(StaticSingleAssignment::new(1, Operator::Const(1), None));
        let ssa_2 = Rc::new(StaticSingleAssignment::new(
            2,
            Operator::Const(2),
            Some(Rc::clone(&ssa_1)),
        ));
    }
}
