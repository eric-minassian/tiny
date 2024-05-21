use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    ast::{visit::AstVisitor, Assignment, Block, Computation, Expression, Factor, FuncDecl, Term},
    lexer::Identifier,
};

use super::{
    block::{BlockIndex, Body, ControlFlowEdge},
    instruction::{Instruction, InstructionId, Operator, StoredBinaryOpcode},
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
    prev_val: Option<i32>,
}

impl<'a> IrBodyGenerator<'a> {
    pub fn gen_main(computation: &Computation, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = computation
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().cloned().collect());

        let mut generator = Self::new(const_block, declared_identifiers, true);
        generator.visit_computation(computation);

        generator.body
    }

    pub fn gen_func(func_decl: &FuncDecl, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = func_decl
            .body
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().cloned().collect());

        let mut generator = Self::new(const_block, declared_identifiers, false);
        generator.visit_func_decl(func_decl);

        generator.body
    }

    fn new(
        const_block: &'a mut ConstBlock,
        declared_identifiers: HashSet<Identifier>,
        is_main: bool,
    ) -> Self {
        let mut body = Body::new();
        let cur_block = body.insert_block(Default::default());
        body.set_root(cur_block);

        Self {
            const_block,
            cur_block,
            body,
            next_instr_id: 1,
            is_main,
            declared_identifiers,
            prev_val: None,
        }
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        left: InstructionId,
        right: InstructionId,
    ) -> InstructionId {
        let new_instr_id = self.next_instr_id;

        let mut new_instr = Instruction::new(
            new_instr_id,
            Operator::StoredBinaryOp(operator.clone(), left, right),
            None,
        );

        if let Some(dom_instr) = self
            .body
            .get_block_mut(self.cur_block)
            .unwrap()
            .get_dom_instr(&operator)
        {
            if let Some(dup_instr_id) = dom_instr.try_borrow().unwrap().check_dominators(&new_instr)
            {
                return dup_instr_id;
            }
        }

        if let Some(dom_instr) = self
            .body
            .get_block_mut(self.cur_block)
            .unwrap()
            .get_dom_instr(&operator)
        {
            new_instr.update_dom(dom_instr);
        };

        let new_instr = Rc::new(RefCell::new(new_instr));
        self.body
            .get_block_mut(self.cur_block)
            .unwrap()
            .push_instr(new_instr.clone());
        self.next_instr_id += 1;

        new_instr_id
    }

    fn push_end_instr(&mut self) {
        let block = self
            .body
            .get_block_mut(self.cur_block)
            .expect("Compiler Bug: Block not found");
        if block.get_edge().is_none() {
            let operator = if self.is_main {
                Operator::End
            } else {
                Operator::Ret(None)
            };
            let end_instr = Rc::new(RefCell::new(Instruction::new(
                self.next_instr_id,
                operator,
                None,
            )));
            self.next_instr_id += 1;

            block.push_instr(end_instr);
            block.set_edge(ControlFlowEdge::Leaf);
        }
    }
}

impl AstVisitor for IrBodyGenerator<'_> {
    fn visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }

        self.push_end_instr();
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.visit_expression(&assignment.expr);
        let instr_id = self.prev_val.unwrap();

        self.body
            .get_block_mut(self.cur_block)
            .unwrap()
            .insert_identifier(assignment.ident, instr_id);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.visit_term(&expression.term);
        let mut lhs = self.prev_val.unwrap();

        for (expr_op, term) in &expression.ops {
            self.visit_term(term);
            let rhs = self.prev_val.unwrap();
            lhs = self.handle_binary_op(expr_op.into(), lhs, rhs);
        }

        self.prev_val = Some(lhs);
    }

    fn visit_term(&mut self, term: &Term) {
        self.visit_factor(&term.factor);
        let mut lhs = self.prev_val.unwrap();

        for (expr_op, factor) in &term.ops {
            self.visit_factor(factor);
            let rhs = self.prev_val.unwrap();
            lhs = self.handle_binary_op(expr_op.into(), lhs, rhs);
        }

        self.prev_val = Some(lhs);
    }

    fn visit_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::Expression(expression) => self.visit_expression(expression),
            Factor::FuncCall(func_call) => self.visit_func_call(func_call),
            Factor::Ident(_) => todo!(),
            Factor::Number(num) => {
                self.prev_val = Some(self.const_block.insert_returning_id(num.clone()));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashSet, rc::Rc, vec};

    use pretty_assertions_sorted::assert_eq_sorted;

    use crate::{
        ast::{Assignment, Block, Computation, ExprOp, Expression, Factor, Statement, Term},
        ir::{
            block::{BasicBlock, Body, ControlFlowEdge},
            gen::IrBodyGenerator,
            inheriting_hashmap::InheritingHashMap,
            instruction::{Instruction, Operator, StoredBinaryOpcode},
            ConstBlock,
        },
    };

    #[test]
    fn simple_assignment() {
        /*
        let x <- 1;
        let y <- 1 + 2 + 0 - 4;
        */
        let computation = Computation {
            vars: None,
            funcs: vec![],
            body: Block {
                statements: vec![
                    Statement::Assignment(Assignment {
                        ident: 1,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Statement::Assignment(Assignment {
                        ident: 2,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![
                                (
                                    ExprOp::Add,
                                    Term {
                                        factor: Factor::Number(2),
                                        ops: vec![],
                                    },
                                ),
                                (
                                    ExprOp::Add,
                                    Term {
                                        factor: Factor::Number(0),
                                        ops: vec![],
                                    },
                                ),
                                (
                                    ExprOp::Sub,
                                    Term {
                                        factor: Factor::Number(4),
                                        ops: vec![],
                                    },
                                ),
                            ],
                        },
                    }),
                ],
            },
        };

        let mut const_block = ConstBlock::new();

        let body = IrBodyGenerator::gen_main(&computation, &mut const_block);

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -2),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(b0_insr_1.clone()),
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Sub, 2, -4),
            None,
        )));
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::End, None)));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1), (2, 3)]);
        let main_block_dom_instr_map = InheritingHashMap::from_iter([
            (StoredBinaryOpcode::Add, b0_insr_2.clone()),
            (StoredBinaryOpcode::Sub, b0_insr_3.clone()),
        ]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4],
            main_block_identifier_map,
            Some(ControlFlowEdge::Leaf),
            None,
            main_block_dom_instr_map,
        );
        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_block = ConstBlock::from(HashSet::from([0, 1, 2, 4]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_block, expected_const_block);
    }
}
