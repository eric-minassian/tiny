use std::{mem::discriminant, rc::Rc};

use crate::lexer::RelOp;

use super::block::BasicBlockId;

pub type InstructionId = i32;

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    id: InstructionId,
    operator: Operator,
    dominator: Option<Rc<Instruction>>,
}

impl Instruction {
    pub fn new(id: InstructionId, operator: Operator, dominator: Option<Rc<Instruction>>) -> Self {
        if let Some(ssa) = &dominator {
            assert_eq!(discriminant(&operator), discriminant(&ssa.operator))
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    pub fn update_dom(&mut self, dom: Rc<Instruction>) {
        self.dominator = Some(dom);
    }

    pub fn check_dominators(&self, ssa: &Instruction) -> Option<InstructionId> {
        if self.operator == ssa.operator {
            return Some(self.id);
        }

        let mut dominator = self.dominator.clone();

        while let Some(d) = dominator {
            if d.operator == ssa.operator {
                return Some(d.id);
            }

            dominator = d.dominator.clone();
        }

        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StoredBinaryOpcode {
    Add,
    Sub,
    Mul,
    Div,
    Cmp,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BranchOpcode {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

impl From<RelOp> for BranchOpcode {
    fn from(op: RelOp) -> Self {
        match op {
            RelOp::Eq => Self::Eq,
            RelOp::Ne => Self::Ne,
            RelOp::Le => Self::Le,
            RelOp::Lt => Self::Lt,
            RelOp::Ge => Self::Ge,
            RelOp::Gt => Self::Gt,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Const(u32),
    Branch(BranchOpcode, BasicBlockId, InstructionId),
    UnconditionalBranch(BasicBlockId),
    StoredBinaryOp(StoredBinaryOpcode, InstructionId, InstructionId),
    End,
    Phi(InstructionId, InstructionId),
    Read,
    Write(InstructionId),
    WriteNL,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Cmp,
}

impl From<&StoredBinaryOpcode> for OperatorType {
    fn from(op: &StoredBinaryOpcode) -> Self {
        match op {
            StoredBinaryOpcode::Add => Self::Add,
            StoredBinaryOpcode::Sub => Self::Sub,
            StoredBinaryOpcode::Mul => Self::Mul,
            StoredBinaryOpcode::Div => Self::Div,
            StoredBinaryOpcode::Cmp => Self::Cmp,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    #[should_panic]
    fn ssa_different_operators() {
        let ssa = Instruction::new(1, Operator::Const(1), None);

        Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            Some(Rc::new(ssa)),
        );
    }

    #[test]
    fn simple_dominator_check() {
        let ssa_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            None,
        ));
        let ssa_2 = Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            None,
        );

        assert_eq!(ssa_1.check_dominators(&ssa_2), Some(1));
    }

    #[test]
    fn ssa_check_dominators() {
        let ssa_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            None,
        ));
        let ssa_2 = Rc::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(ssa_1),
        ));
        let ssa_3 = Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            Some(ssa_2.clone()),
        );
        let ssa_4 = Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 3),
            Some(Rc::new(ssa_3.clone())),
        );

        assert_eq!(ssa_2.clone().check_dominators(&ssa_3), Some(1));
        assert_eq!(ssa_2.check_dominators(&ssa_4), None);
    }
}
