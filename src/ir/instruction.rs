use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::lexer::RelOp;

use super::block::BlockIndex;

pub type InstructionId = i32;

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    id: InstructionId,
    operator: Operator,
    dominator: Option<Rc<RefCell<Instruction>>>,
}

impl Instruction {
    pub fn new(
        id: InstructionId,
        operator: Operator,
        dominator: Option<Rc<RefCell<Instruction>>>,
    ) -> Self {
        if let Some(ssa) = &dominator {
            let ssa = ssa.try_borrow().unwrap();

            assert_eq!(discriminant(&operator), discriminant(&ssa.operator))
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    pub fn id(&self) -> InstructionId {
        self.id
    }

    pub fn operator(&self) -> &Operator {
        &self.operator
    }

    pub fn update_operator(&mut self, operator: Operator) {
        self.operator = operator;
    }

    pub fn update_dom(&mut self, dom: Rc<RefCell<Instruction>>) {
        self.dominator = Some(dom);
    }

    pub fn check_dominators(&self, ssa: &Instruction) -> Option<InstructionId> {
        if self.operator == ssa.operator {
            return Some(self.id);
        }

        let mut dominator = self.dominator.clone();

        while let Some(d) = dominator {
            let d = d.try_borrow().unwrap();

            if d.operator == ssa.operator {
                return Some(d.id);
            }

            dominator = d.dominator.clone();
        }

        None
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.id, self.operator)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum StoredBinaryOpcode {
    Add,
    Sub,
    Mul,
    Div,
    Cmp,
}

impl std::fmt::Display for StoredBinaryOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoredBinaryOpcode::Add => write!(f, "add"),
            StoredBinaryOpcode::Sub => write!(f, "sub"),
            StoredBinaryOpcode::Mul => write!(f, "mul"),
            StoredBinaryOpcode::Div => write!(f, "div"),
            StoredBinaryOpcode::Cmp => write!(f, "cmp"),
        }
    }
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

impl std::fmt::Display for BranchOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BranchOpcode::Eq => write!(f, "beq"),
            BranchOpcode::Ne => write!(f, "bne"),
            BranchOpcode::Le => write!(f, "ble"),
            BranchOpcode::Lt => write!(f, "blt"),
            BranchOpcode::Ge => write!(f, "bge"),
            BranchOpcode::Gt => write!(f, "bgt"),
        }
    }
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
    Branch(BranchOpcode, BlockIndex, InstructionId),
    UnconditionalBranch(BlockIndex),
    StoredBinaryOp(StoredBinaryOpcode, InstructionId, InstructionId),
    Phi(InstructionId, InstructionId),
    End,
    Ret(InstructionId),
    Read,
    Write(InstructionId),
    WriteNL,
    GetPar { idx: u8 },
    SetPar { idx: u8, val: InstructionId },
    Jsr(InstructionId),
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Const(val) => write!(f, "const #{}", val),
            Operator::Branch(op, block_index, cmp_id) => {
                write!(f, "{} {} {}", op, block_index, cmp_id)
            }
            Operator::UnconditionalBranch(basic_block) => write!(f, "bra {}", basic_block),
            Operator::StoredBinaryOp(op, lhs, rhs) => {
                write!(f, "{:} {} {}", op, lhs, rhs)
            }
            Operator::End => write!(f, "end"),
            Operator::Ret(val) => write!(f, "ret {}", val),
            Operator::Read => write!(f, "read"),
            Operator::Write(val) => write!(f, "write {}", val),
            Operator::WriteNL => write!(f, "writeNL"),
            Operator::GetPar { idx } => write!(f, "getpar{}", idx,),
            Operator::SetPar { idx, val } => write!(f, "setpar{} {}", idx, val),
            Operator::Jsr(val) => write!(f, "jsr func#{}", val),
            Operator::Phi(lhs, rhs) => write!(f, "phi {} {}", lhs, rhs),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions_sorted::assert_eq;

    #[test]
    #[should_panic]
    fn ssa_different_operators() {
        let ssa = Instruction::new(1, Operator::Const(1), None);

        Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            Some(Rc::new(RefCell::new(ssa))),
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
        let ssa_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            None,
        )));
        let ssa_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(ssa_1),
        )));
        let ssa_3 = Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
            Some(ssa_2.clone()),
        );
        let ssa_4 = Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 3),
            Some(Rc::new(RefCell::new(ssa_3.clone()))),
        );

        assert_eq!(
            ssa_2.clone().try_borrow().unwrap().check_dominators(&ssa_3),
            Some(1)
        );
        assert_eq!(ssa_2.try_borrow().unwrap().check_dominators(&ssa_4), None);
    }
}
