use std::collections::HashMap;

pub struct IntermediateRepresentation {
    ssa_count: u32,
}

impl IntermediateRepresentation {
    pub fn new() -> Self {
        Self { ssa_count: 0 }
    }
}

pub struct BasicBlock {
    identifier_map: HashMap<usize, u32>,
    ssas: Vec<StaticSingleAssignment>,
}

impl BasicBlock {
    pub fn new(identifier_map: HashMap<usize, u32>, ssas: Vec<StaticSingleAssignment>) -> Self {
        Self {
            identifier_map,
            ssas,
        }
    }
}

pub struct StaticSingleAssignment {
    id: u32,
    operator: Operator,
    dominator: Option<Box<StaticSingleAssignment>>,
}

impl StaticSingleAssignment {
    pub fn new(
        id: u32,
        operator: Operator,
        dominator: Option<Box<StaticSingleAssignment>>,
    ) -> Self {
        Self {
            id,
            operator,
            dominator,
        }
    }
}

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
