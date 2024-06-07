pub mod detector;

use std::{cell::RefCell, collections::HashSet, rc::Rc};

use crate::{
    ast::{
        visit::AstVisitor, Assignment, Computation, DefinedFuncCall, Expression, Factor, FuncCall,
        FuncDecl, IfStatement, PredefinedFuncCall, Relation, ReturnStatement, Term, WhileStatement,
    },
    lexer::{Identifier, PredefinedFunction},
    parser::error::{print_warning, Warning},
};

use self::detector::PhiDetector;

use super::{
    block::{BasicBlock, BlockIndex, Body, ControlFlowEdge},
    inheriting_hashmap::InheritingHashMap,
    instruction::{BranchOpcode, Instruction, InstructionId, Operator, StoredBinaryOpcode},
    ConstBlock, IrStore,
};

#[derive(Default)]
pub struct IrGenerator {
    bodies: Vec<(String, Body)>,
    const_block: ConstBlock,
}

impl IrGenerator {
    #[must_use]
    pub fn generate(ast: &Computation) -> IrStore {
        let mut generator = Self::default();
        generator.visit_computation(ast);

        IrStore::from(generator.bodies, generator.const_block)
    }
}

impl AstVisitor for IrGenerator {
    fn visit_computation(&mut self, computation: &Computation) {
        for func in &computation.funcs {
            self.visit_func_decl(func);
        }

        self.bodies.push((
            "main".to_string(),
            IrBodyGenerator::generate_main(computation, &mut self.const_block),
        ));
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.bodies.push((
            func_decl.name.clone(),
            IrBodyGenerator::generate_func(func_decl, &mut self.const_block),
        ));
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
    pub fn generate_main(computation: &Computation, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = computation
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().copied().collect());

        let mut generator = Self::new(const_block, declared_identifiers, true);
        generator.visit_computation(computation);
        generator.push_end_instr();

        generator.body
    }

    pub fn generate_func(func_decl: &FuncDecl, const_block: &'a mut ConstBlock) -> Body {
        let declared_identifiers = func_decl
            .body
            .vars
            .as_ref()
            .map_or_else(HashSet::new, |v| v.vars.iter().copied().collect());

        let mut generator = Self::new(const_block, declared_identifiers, false);
        generator.visit_func_decl(func_decl);
        generator.push_end_instr();

        generator.body
    }

    fn new(
        const_block: &'a mut ConstBlock,
        declared_identifiers: HashSet<Identifier>,
        is_main: bool,
    ) -> Self {
        let mut body = Body::new();
        let cur_block = body.insert_block(BasicBlock::default());

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
            Operator::StoredBinaryOp(operator, left, right),
            None,
        );

        if let Some(dom_instr) = self.get_block_mut(self.cur_block).get_dom_instr(&operator) {
            if let Some(dup_instr_id) = dom_instr
                .try_borrow()
                .expect("Compiler Bug: Dominator is already borrowed")
                .check_dominators(&new_instr)
            {
                return dup_instr_id;
            }
        }

        if let Some(dom_instr) = self.get_block_mut(self.cur_block).get_dom_instr(&operator) {
            new_instr.update_dom(dom_instr);
        };

        let new_instr = Rc::new(RefCell::new(new_instr));
        self.get_block_mut(self.cur_block).push_instr(new_instr);
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

    fn get_block_mut(&mut self, block: BlockIndex) -> &mut BasicBlock {
        self.body
            .get_block_mut(block)
            .expect("Compiler Bug: Block not found")
    }

    fn create_block(&mut self, parent_block: BlockIndex) -> BlockIndex {
        let identifier_map = InheritingHashMap::with_dominator(
            self.get_block_mut(parent_block).get_identifier_map(),
        );
        let dom_instr_map =
            InheritingHashMap::with_dominator(self.get_block_mut(parent_block).get_dom_instr_map());
        let new_block = BasicBlock::from(
            Vec::new(),
            identifier_map,
            None,
            Some(parent_block),
            dom_instr_map,
        );
        self.body.insert_block(new_block)
    }

    fn prev_val(&self) -> InstructionId {
        self.prev_val.expect("Compiler Bug: Previous value not set")
    }
}

impl AstVisitor for IrBodyGenerator<'_> {
    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        for (i, param) in func_decl.params.params.clone().into_iter().enumerate() {
            let get_param_instr_id = self.next_instr_id;
            let get_param_instr = Rc::new(RefCell::new(Instruction::new(
                get_param_instr_id,
                Operator::GetPar {
                    idx: u8::try_from(i).expect("Compiler Bug: Parameter get index out of bounds")
                        + 1,
                },
                None,
            )));
            self.next_instr_id += 1;

            self.get_block_mut(self.cur_block)
                .identifier_map
                .insert(param, get_param_instr_id);
            self.get_block_mut(self.cur_block)
                .push_instr(get_param_instr);
        }

        for var in &func_decl.params.params {
            self.declared_identifiers.insert(*var);
        }

        self.visit_func_body(&func_decl.body);
    }

    fn visit_computation(&mut self, computation: &Computation) {
        if let Some(vars) = &computation.vars {
            for var in &vars.vars {
                self.declared_identifiers.insert(*var);
            }
        }

        self.visit_block(&computation.body);
    }

    fn visit_return_statement(&mut self, return_statement: &ReturnStatement) {
        let result_id = if let Some(expr) = &return_statement.expr {
            self.visit_expression(expr);

            self.prev_val
        } else {
            None
        };

        let operator = if self.is_main {
            Operator::End
        } else {
            Operator::Ret(result_id)
        };

        let return_instr = Rc::new(RefCell::new(Instruction::new(
            self.next_instr_id,
            operator,
            None,
        )));
        self.next_instr_id += 1;

        self.get_block_mut(self.cur_block).push_instr(return_instr);
        self.get_block_mut(self.cur_block)
            .set_edge(ControlFlowEdge::Leaf);
    }

    fn visit_while_statement(&mut self, while_statement: &WhileStatement) {
        // let join_block = self.create_block(self.cur_block);
        let temp_join_block = self.get_block_mut(self.cur_block);
        let join_block = if temp_join_block.instructions.is_empty()
            && temp_join_block.identifier_map.is_empty()
            && temp_join_block.get_edge().is_none()
        {
            self.cur_block
        } else {
            let join_block = self.create_block(self.cur_block);
            self.get_block_mut(self.cur_block)
                .set_edge(ControlFlowEdge::Fallthrough(join_block));

            join_block
        };

        self.cur_block = join_block;

        let phis = PhiDetector::detect(&while_statement.block);
        for phi in &phis {
            let id_val = self
                .get_block_mut(join_block)
                .identifier_map
                .get(phi)
                .unwrap_or_else(|| {
                    print_warning(Warning::UninitializedIdentifier(*phi));

                    self.const_block.insert_returning_id(0)
                });

            let phi_id = self.next_instr_id;
            let phi_instr = Rc::new(RefCell::new(Instruction::new(
                phi_id,
                Operator::Phi(id_val, 0),
                None,
            )));
            self.get_block_mut(join_block).push_instr(phi_instr);
            self.next_instr_id += 1;

            self.get_block_mut(join_block)
                .identifier_map
                .insert(*phi, phi_id);
        }

        self.visit_relation(&while_statement.rel);
        let comparator_instruction_id = self.prev_val();
        let opposite_relop = while_statement.rel.rel_op.opposite();

        let join_block_branch_id = self.next_instr_id;
        self.next_instr_id += 1;

        let body_block = self.create_block(join_block);
        self.get_block_mut(join_block)
            .set_edge(ControlFlowEdge::Fallthrough(body_block));
        self.cur_block = body_block;

        self.visit_block(&while_statement.block);

        let body_block_end = self.cur_block;

        let phi_return = self
            .body
            .get_block(body_block_end)
            .expect("Compiler Bug: Block not found")
            .get_edge()
            .is_some_and(|edge| matches!(edge, ControlFlowEdge::Leaf));

        if !phi_return {
            self.get_block_mut(body_block_end)
                .set_edge(ControlFlowEdge::Branch(join_block));

            let body_block_end_instr = Rc::new(RefCell::new(Instruction::new(
                self.next_instr_id,
                Operator::UnconditionalBranch(join_block),
                None,
            )));
            self.next_instr_id += 1;
            self.get_block_mut(body_block_end)
                .push_instr(body_block_end_instr);
        }

        self.cur_block = join_block;

        let phi_len = phis.len();
        for (i, phi) in phis.into_iter().enumerate() {
            let new_id_val = self
                .get_block_mut(body_block_end)
                .identifier_map
                .get(&phi)
                .unwrap_or_else(|| self.const_block.insert_returning_id(0));

            let join_block_instructions_len = self.get_block_mut(join_block).instructions.len();

            let temp = self.get_block_mut(join_block).instructions
                [join_block_instructions_len - phi_len - 1 + i]
                .borrow()
                .operator()
                .clone();

            if let Operator::Phi(old_val, _) = temp {
                self.get_block_mut(join_block).instructions
                    [join_block_instructions_len - phi_len - 1 + i]
                    .borrow_mut()
                    .update_operator(Operator::Phi(old_val, new_id_val));
            } else {
                unreachable!();
            }
        }

        let escape_block = self.create_block(join_block);

        self.get_block_mut(join_block)
            .push_instr(Rc::new(RefCell::new(Instruction::new(
                join_block_branch_id,
                Operator::Branch(
                    BranchOpcode::from(opposite_relop),
                    escape_block,
                    comparator_instruction_id,
                ),
                None,
            ))));

        self.cur_block = escape_block;
    }

    #[allow(clippy::too_many_lines)]
    fn visit_if_statement(&mut self, if_statement: &IfStatement) {
        self.visit_relation(&if_statement.rel);
        let cmp_instr = self.prev_val();
        let opposite_relop = if_statement.rel.rel_op.opposite();

        let branch_block = self.cur_block;
        let branch_instruction_id = self.next_instr_id;
        self.next_instr_id += 1;

        let then_block = self.create_block(branch_block);
        self.cur_block = then_block;
        self.get_block_mut(branch_block)
            .set_edge(ControlFlowEdge::Fallthrough(then_block));

        self.visit_block(&if_statement.then_block);
        let then_block_end = self.cur_block;

        let then_phis = PhiDetector::detect(&if_statement.then_block);
        let then_phi_return = self
            .body
            .get_block(then_block_end)
            .expect("Compiler Bug: Block not found")
            .get_edge()
            .is_some_and(|edge| matches!(edge, ControlFlowEdge::Leaf));

        let mut else_phis = None;
        let mut else_phi_return = None;
        let mut else_block = None;
        let is_else_present = if_statement.else_block.is_some();

        let then_unconditional_branch_id = self.next_instr_id;
        if is_else_present && !then_phi_return {
            self.next_instr_id += 1;
        }

        if let Some(else_block_block) = &if_statement.else_block {
            let else_block_temp = self.create_block(branch_block);
            self.cur_block = else_block_temp;
            else_block = Some(else_block_temp);
            self.visit_block(else_block_block);

            else_phis = Some(PhiDetector::detect(else_block_block));
            else_phi_return = Some(
                self.body
                    .get_block(self.cur_block)
                    .expect("Compiler Bug: Block not found")
                    .get_edge()
                    .is_some_and(|edge| matches!(edge, ControlFlowEdge::Leaf)),
            );
        }
        let else_block_end = self.cur_block;

        if then_phi_return
            && is_else_present
            && else_phi_return.expect("Compiler Bug: Else block not found")
        {
            self.get_block_mut(branch_block)
                .push_instr(Rc::new(RefCell::new(Instruction::new(
                    branch_instruction_id,
                    Operator::Branch(
                        BranchOpcode::from(opposite_relop),
                        else_block_end,
                        cmp_instr,
                    ),
                    None,
                ))));

            return;
        }

        let join_block = self.create_block(branch_block);

        if is_else_present && !then_phi_return {
            self.get_block_mut(then_block_end)
                .push_instr(Rc::new(RefCell::new(Instruction::new(
                    then_unconditional_branch_id,
                    Operator::UnconditionalBranch(join_block),
                    None,
                ))));
        }

        if is_else_present {
            self.get_block_mut(self.cur_block)
                .set_edge(ControlFlowEdge::Fallthrough(join_block));
        }

        self.get_block_mut(then_block_end)
            .set_edge(if is_else_present {
                ControlFlowEdge::Branch(join_block)
            } else {
                ControlFlowEdge::Fallthrough(join_block)
            });

        self.get_block_mut(branch_block)
            .push_instr(Rc::new(RefCell::new(Instruction::new(
                branch_instruction_id,
                Operator::Branch(
                    BranchOpcode::from(opposite_relop),
                    if is_else_present {
                        else_block.expect("Compiler Bug: Else block not found")
                    } else {
                        join_block
                    },
                    cmp_instr,
                ),
                None,
            ))));

        self.cur_block = join_block;

        // Should be ordered with no duplicates
        let mut phis = then_phis
            .into_iter()
            .chain(else_phis.unwrap_or_default())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        phis.sort_unstable();
        phis.dedup();

        for phi in phis {
            let left_id_val = self
                .get_block_mut(if then_phi_return {
                    branch_block
                } else {
                    then_block_end
                })
                .identifier_map
                .get(&phi)
                .unwrap_or_else(|| self.const_block.insert_returning_id(0));

            let right_id_val = if is_else_present {
                self.get_block_mut(
                    if else_phi_return.expect("Compiler Bug: Else block not found") {
                        branch_block
                    } else {
                        else_block_end
                    },
                )
                .identifier_map
                .get(&phi)
                .unwrap_or_else(|| self.const_block.insert_returning_id(0))
            } else {
                self.get_block_mut(branch_block)
                    .identifier_map
                    .get(&phi)
                    .unwrap_or_else(|| self.const_block.insert_returning_id(0))
            };

            let phi_id = self.next_instr_id;
            let phi_instr = Rc::new(RefCell::new(Instruction::new(
                phi_id,
                Operator::Phi(left_id_val, right_id_val),
                None,
            )));
            self.get_block_mut(join_block).push_instr(phi_instr);
            self.next_instr_id += 1;

            self.get_block_mut(join_block)
                .identifier_map
                .insert(phi, phi_id);
        }
    }

    fn visit_func_call(&mut self, func_call: &FuncCall) {
        match func_call {
            FuncCall::Defined(DefinedFuncCall {
                name: _,
                ident,
                args,
            }) => {
                let mut arg_instr_ids = Vec::new();
                for arg in args {
                    self.visit_expression(arg);
                    arg_instr_ids.push(self.prev_val());
                }

                for (idx, val) in arg_instr_ids.into_iter().enumerate() {
                    let arg_instr = Rc::new(RefCell::new(Instruction::new(
                        self.next_instr_id,
                        Operator::SetPar {
                            idx: u8::try_from(idx)
                                .expect("Compiler Bug: Parameter set index out of bounds")
                                + 1,
                            val,
                        },
                        None,
                    )));
                    self.next_instr_id += 1;
                    self.get_block_mut(self.cur_block).push_instr(arg_instr);
                }

                let call_instr_id = self.next_instr_id;
                let call_instr = Rc::new(RefCell::new(Instruction::new(
                    call_instr_id,
                    Operator::Jsr(
                        i32::try_from(*ident).expect("Compiler Bug: Identifier too large"),
                    ),
                    None,
                )));
                self.next_instr_id += 1;

                self.get_block_mut(self.cur_block).push_instr(call_instr);

                self.prev_val = Some(call_instr_id);
            }
            FuncCall::Predefined(PredefinedFuncCall { func, args }) => {
                let mut arg_instr_ids = Vec::new();
                for arg in args {
                    self.visit_expression(arg);
                    arg_instr_ids.push(self.prev_val());
                }

                let operator = match func {
                    PredefinedFunction::InputNum => Operator::Read,
                    PredefinedFunction::OutputNum => Operator::Write(arg_instr_ids[0]),
                    PredefinedFunction::OutputNewLine => Operator::WriteNL,
                };

                let call_instr_id = self.next_instr_id;
                let call_instr = Rc::new(RefCell::new(Instruction::new(
                    call_instr_id,
                    operator,
                    None,
                )));
                self.next_instr_id += 1;

                self.get_block_mut(self.cur_block).push_instr(call_instr);

                self.prev_val = Some(call_instr_id);
            }
        }
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        if !self.declared_identifiers.contains(&assignment.ident) {
            print_warning(Warning::UndeclaredIdentifier(assignment.ident));

            self.declared_identifiers.insert(assignment.ident);
        }

        self.visit_expression(&assignment.expr);
        let instr_id = self.prev_val();

        self.body
            .get_block_mut(self.cur_block)
            .expect("Compiler Bug: Block not found")
            .identifier_map
            .insert(assignment.ident, instr_id);
    }

    fn visit_relation(&mut self, relation: &Relation) {
        self.visit_expression(&relation.lhs);
        let lhs = self.prev_val();

        self.visit_expression(&relation.rhs);
        let rhs = self.prev_val();

        self.prev_val = Some(self.handle_binary_op(StoredBinaryOpcode::Cmp, lhs, rhs));
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.visit_term(&expression.term);
        let mut lhs = self.prev_val();

        for (expr_op, term) in &expression.ops {
            self.visit_term(term);
            let rhs = self.prev_val();
            lhs = self.handle_binary_op(expr_op.into(), lhs, rhs);
        }

        self.prev_val = Some(lhs);
    }

    fn visit_term(&mut self, term: &Term) {
        self.visit_factor(&term.factor);
        let mut lhs = self.prev_val();

        for (expr_op, factor) in &term.ops {
            self.visit_factor(factor);
            let rhs = self.prev_val();
            lhs = self.handle_binary_op(expr_op.into(), lhs, rhs);
        }

        self.prev_val = Some(lhs);
    }

    fn visit_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::Expression(expression) => self.visit_expression(expression),
            Factor::FuncCall(func_call) => self.visit_func_call(func_call),
            Factor::Ident(ident) => {
                let ident = *ident;

                if !self.declared_identifiers.contains(&ident) {
                    print_warning(Warning::UndeclaredIdentifier(ident));

                    self.declared_identifiers.insert(ident);
                }

                self.prev_val = Some(
                    self.get_block_mut(self.cur_block)
                        .identifier_map
                        .get(&ident)
                        .unwrap_or_else(|| {
                            print_warning(Warning::UninitializedIdentifier(ident));

                            let instruction_id = self.const_block.insert_returning_id(0);
                            self.get_block_mut(self.cur_block)
                                .identifier_map
                                .insert(ident, instruction_id);

                            instruction_id
                        }),
                );
            }
            Factor::Number(num) => {
                self.prev_val = Some(self.const_block.insert_returning_id(*num));
            }
        }
    }
}
