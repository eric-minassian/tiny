use std::mem::discriminant;

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
pub enum Operator {
    Const(u32),
    StoredBinaryOp(StoredBinaryOpcode, InstructionId, InstructionId),
    End,
    Phi(InstructionId, InstructionId),
    Bra(InstructionId),
    Bne(InstructionId, InstructionId),
    Beq(InstructionId, InstructionId),
    Ble(InstructionId, InstructionId),
    Blt(InstructionId, InstructionId),
    Bge(InstructionId, InstructionId),
    Bgt(InstructionId, InstructionId),
    Read,
    Write(InstructionId),
    WriteNL,
}

#[cfg(test)]
mod tests {
    use super::*;

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
