use std::mem::discriminant;

use crate::lexer::RelOp;

use super::block::BasicBlockId;

pub type InstructionId = i32;

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction<'a> {
    id: InstructionId,
    operator: Operator,
    dominator: Option<&'a Instruction<'a>>,
}

impl<'a> Instruction<'a> {
    pub fn new(id: InstructionId, operator: Operator, dominator: Option<&'a Instruction>) -> Self {
        if let Some(ssa) = &dominator {
            assert_eq!(discriminant(&operator), discriminant(&ssa.operator))
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    pub fn check_dominators(&self, ssa: &Instruction) -> Option<InstructionId> {
        let mut dominator = self.dominator;

        while let Some(d) = dominator {
            if d.operator == ssa.operator {
                return Some(d.id);
            }

            dominator = d.dominator;
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
    Phi,
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
            Some(&ssa),
        );
    }

    #[test]
    fn ssa_check_dominators() {
        let ssa_1 = Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            None,
        );
        let ssa_2 = Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(&ssa_1),
        );
        let ssa_3 = Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            Some(&ssa_2),
        );
        let ssa_4 = Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 3),
            Some(&ssa_3),
        );

        assert_eq!(ssa_2.check_dominators(&ssa_3), Some(1));
        assert_eq!(ssa_2.check_dominators(&ssa_4), None);
    }
}
