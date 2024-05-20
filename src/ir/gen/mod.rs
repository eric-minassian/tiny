use std::collections::{HashMap, HashSet};

use crate::{
    ast::{visit::AstVisitor, Computation, FuncDecl},
    lexer::Identifier,
};

use super::{
    block::{BlockIndex, Body},
    instruction::InstructionId,
    ConstBlock, IrStore,
};

pub struct IrGenerator {
    bodies: HashMap<String, Body>,
    const_block: ConstBlock,
}

impl IrGenerator {
    pub fn gen(ast: &Computation) -> IrStore {
        let mut generator = Self::new();
        generator.visit_computation(ast);

        IrStore::from(generator.bodies, generator.const_block)
    }

    fn new() -> Self {
        Self {
            bodies: HashMap::new(),
            const_block: ConstBlock::new(),
        }
    }
}

impl AstVisitor for IrGenerator {
    fn visit_computation(&mut self, computation: &Computation) {
        for func in &computation.funcs {
            self.visit_func_decl(func);
        }

        self.bodies.insert(
            "main".to_string(),
            IrBodyGenerator::gen_main(computation, &mut self.const_block),
        );
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.bodies.insert(
            func_decl.name.clone(),
            IrBodyGenerator::gen_func(func_decl, &mut self.const_block),
        );
    }
}

pub struct IrBodyGenerator<'a> {
    body: Body,
    const_block: &'a mut ConstBlock,
    cur_block: BlockIndex,
    next_instr_id: InstructionId,
    is_main: bool,
    declared_identifiers: HashSet<Identifier>,
}

impl<'a> IrBodyGenerator<'a> {
    pub fn gen_main(computation: &Computation, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = computation
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().cloned().collect());

        let mut generator = Self::new(const_block, declared_identifiers, true);
        generator.visit_computation(computation);

        todo!()
    }

    pub fn gen_func(func_decl: &FuncDecl, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = func_decl
            .body
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().cloned().collect());

        let mut generator = Self::new(const_block, declared_identifiers, false);
        generator.visit_func_decl(func_decl);

        todo!()
    }

    fn new(
        const_block: &'a mut ConstBlock,
        declared_identifiers: HashSet<Identifier>,
        is_main: bool,
    ) -> Self {
        let mut body = Body::new();

        Self {
            const_block,
            cur_block: body.insert_block(Default::default()),
            body,
            next_instr_id: 1,
            is_main,
            declared_identifiers,
        }
    }
}

impl AstVisitor for IrBodyGenerator<'_> {}
