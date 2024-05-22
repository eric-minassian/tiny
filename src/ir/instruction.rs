use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::{
    ast::{ExprOp, TermOp},
    lexer::RelOp,
};

use super::block::BlockIndex;

pub type InstructionId = i32;

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    id: InstructionId,
    operator: Operator,
    dominator: Option<Rc<RefCell<Instruction>>>,
}

impl Instruction {
    #[must_use]
    pub fn new(
        id: InstructionId,
        operator: Operator,
        dominator: Option<Rc<RefCell<Self>>>,
    ) -> Self {
        if let Some(ssa) = &dominator {
            let ssa = ssa
                .try_borrow()
                .expect("Compiler error: Dominator is already borrowed");

            assert_eq!(discriminant(&operator), discriminant(&ssa.operator));
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    #[must_use]
    pub const fn id(&self) -> InstructionId {
        self.id
    }

    #[must_use]
    pub const fn operator(&self) -> &Operator {
        &self.operator
    }

    pub fn update_operator(&mut self, operator: Operator) {
        self.operator = operator;
    }

    pub fn update_dom(&mut self, dom: Rc<RefCell<Self>>) {
        self.dominator = Some(dom);
    }

    #[allow(clippy::assigning_clones)]
    #[must_use]
    pub fn check_dominators(&self, ssa: &Self) -> Option<InstructionId> {
        if self.operator == ssa.operator {
            return Some(self.id);
        }

        let mut dominator = self.dominator.clone();

        while let Some(d) = dominator {
            let d = d
                .try_borrow()
                .expect("Compiler error: Dominator is already borrowed");

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum StoredBinaryOpcode {
    Add,
    Sub,
    Mul,
    Div,
    Cmp,
}

impl From<&ExprOp> for StoredBinaryOpcode {
    fn from(op: &ExprOp) -> Self {
        match op {
            ExprOp::Add => Self::Add,
            ExprOp::Sub => Self::Sub,
        }
    }
}

impl From<&TermOp> for StoredBinaryOpcode {
    fn from(op: &TermOp) -> Self {
        match op {
            TermOp::Mul => Self::Mul,
            TermOp::Div => Self::Div,
        }
    }
}

impl std::fmt::Display for StoredBinaryOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Cmp => write!(f, "cmp"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
            Self::Eq => write!(f, "beq"),
            Self::Ne => write!(f, "bne"),
            Self::Le => write!(f, "ble"),
            Self::Lt => write!(f, "blt"),
            Self::Ge => write!(f, "bge"),
            Self::Gt => write!(f, "bgt"),
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

#[derive(Debug, Clone)]
pub enum Operator {
    Const(u32),
    Branch(BranchOpcode, BlockIndex, InstructionId),
    UnconditionalBranch(BlockIndex),
    StoredBinaryOp(StoredBinaryOpcode, InstructionId, InstructionId),
    Phi(InstructionId, InstructionId),
    End,
    Ret(Option<InstructionId>), // Void functions return None
    Read,
    Write(InstructionId),
    WriteNL,
    GetPar { idx: u8 },
    SetPar { idx: u8, val: InstructionId },
    Jsr(InstructionId),
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::StoredBinaryOp(StoredBinaryOpcode::Cmp, lhs1, rhs1),
                Self::StoredBinaryOp(StoredBinaryOpcode::Cmp, lhs2, rhs2),
            )
            | (Self::Phi(lhs1, rhs1), Self::Phi(lhs2, rhs2)) => lhs1 == lhs2 && rhs1 == rhs2,
            (Self::StoredBinaryOp(op1, lhs1, rhs1), Self::StoredBinaryOp(op2, lhs2, rhs2)) => {
                op1 == op2 && ((lhs1 == lhs2 && rhs1 == rhs2) || (lhs1 == rhs2 && rhs1 == lhs2))
            }
            (Self::Const(val1), Self::Const(val2)) => val1 == val2,
            (
                Self::Branch(op1, block_index1, cmp_id1),
                Self::Branch(op2, block_index2, cmp_id2),
            ) => op1 == op2 && block_index1 == block_index2 && cmp_id1 == cmp_id2,
            (Self::UnconditionalBranch(block_index1), Self::UnconditionalBranch(block_index2)) => {
                block_index1 == block_index2
            }
            (Self::Ret(val1), Self::Ret(val2)) => val1 == val2,
            (Self::Write(val1), Self::Write(val2)) | (Self::Jsr(val1), Self::Jsr(val2)) => {
                val1 == val2
            }
            (Self::WriteNL, Self::WriteNL) | (Self::Read, Self::Read) | (Self::End, Self::End) => {
                true
            }
            (Self::GetPar { idx: idx1 }, Self::GetPar { idx: idx2 }) => idx1 == idx2,
            (
                Self::SetPar {
                    idx: idx1,
                    val: val1,
                },
                Self::SetPar {
                    idx: idx2,
                    val: val2,
                },
            ) => idx1 == idx2 && val1 == val2,
            _ => false,
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(val) => write!(f, "const #{}", val),
            Self::Branch(op, block_index, cmp_id) => {
                write!(f, "{} {} {}", op, block_index, cmp_id)
            }
            Self::UnconditionalBranch(basic_block) => write!(f, "bra {}", basic_block),
            Self::StoredBinaryOp(op, lhs, rhs) => {
                write!(f, "{:} {} {}", op, lhs, rhs)
            }
            Self::End => write!(f, "end"),
            Self::Ret(val) => match val {
                Some(val) => write!(f, "ret {}", val),
                None => write!(f, "ret void"),
            },
            Self::Read => write!(f, "read"),
            Self::Write(val) => write!(f, "write {}", val),
            Self::WriteNL => write!(f, "writeNL"),
            Self::GetPar { idx } => write!(f, "getpar{}", idx,),
            Self::SetPar { idx, val } => write!(f, "setpar{} {}", idx, val),
            Self::Jsr(val) => write!(f, "jsr func#{}", val),
            Self::Phi(lhs, rhs) => write!(f, "phi {} {}", lhs, rhs),
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

        let _ = Instruction::new(
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
