use std::mem::discriminant;

#[derive(Debug, PartialEq, Eq)]
pub struct StaticSingleAssignment<'a> {
    id: u32,
    operator: Operator,
    dominator: Option<&'a StaticSingleAssignment<'a>>,
}

impl<'a> StaticSingleAssignment<'a> {
    pub fn new(id: u32, operator: Operator, dominator: Option<&'a StaticSingleAssignment>) -> Self {
        if let Some(ssa) = &dominator {
            assert_eq!(discriminant(&operator), discriminant(&ssa.operator))
        }

        Self {
            id,
            operator,
            dominator,
        }
    }

    pub fn check_dominators(&self, ssa: &StaticSingleAssignment) -> Option<u32> {
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
        let ssa = StaticSingleAssignment::new(1, Operator::Const(1), None);

        StaticSingleAssignment::new(2, Operator::Add(1, 1), Some(&ssa));
    }

    #[test]
    fn ssa_check_dominators() {
        let ssa_1 = StaticSingleAssignment::new(1, Operator::Add(1, 1), None);
        let ssa_2 = StaticSingleAssignment::new(2, Operator::Add(1, 0), Some(&ssa_1));
        let ssa_3 = StaticSingleAssignment::new(3, Operator::Add(1, 1), Some(&ssa_2));
        let ssa_4 = StaticSingleAssignment::new(4, Operator::Add(3, 3), Some(&ssa_3));

        assert_eq!(ssa_2.check_dominators(&ssa_3), Some(1));
        assert_eq!(ssa_2.check_dominators(&ssa_4), None);
    }
}
