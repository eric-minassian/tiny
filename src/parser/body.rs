use std::{cell::RefCell, collections::HashSet, iter::Peekable, rc::Rc};

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, BlockIndex, Body, ControlFlowEdge},
        inheriting_hashmap::InheritingHashMap,
        instruction::{BranchOpcode, Instruction, InstructionId, Operator, StoredBinaryOpcode},
        ConstBlock,
    },
    lexer::{IdentifierId, PredefinedFunction, RelOp, Token},
};

use super::match_token;

pub struct BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>> + Clone,
{
    tokens: &'a mut Peekable<T>,
    const_body: &'a mut ConstBlock,
    body: Body,
    cur_block: BlockIndex,
    next_instr_id: InstructionId,
}

impl<'a, T> BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>> + Clone,
{
    pub fn parse_main(tokens: &'a mut Peekable<T>, const_body: &'a mut ConstBlock) -> Body {
        let mut body_parser = Self::new(tokens, const_body);

        body_parser.stat_sequence().unwrap();

        body_parser.body
    }

    pub fn parse_func(
        tokens: &'a mut Peekable<T>,
        const_body: &'a mut ConstBlock,
        params: Vec<IdentifierId>,
    ) -> Body {
        let mut body_parser = Self::new(tokens, const_body);
        body_parser.parse_func_params(params);
        body_parser.stat_sequence().unwrap();

        body_parser.body
    }

    fn new(tokens: &'a mut Peekable<T>, const_body: &'a mut ConstBlock) -> Self {
        let mut body = Body::new();
        let cur_block = body.insert_block(BasicBlock::new());
        body.set_root(cur_block);

        Self {
            tokens,
            const_body,
            cur_block,
            body,
            next_instr_id: 1,
        }
    }

    fn get_block_mut(&mut self, index: BlockIndex) -> &mut BasicBlock {
        self.body.get_mut_block(index).unwrap()
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        match_token(&mut self.tokens, expected)
    }

    fn parse_func_params(&mut self, params: Vec<IdentifierId>) {
        for (i, param) in params.into_iter().enumerate() {
            let get_param_instr_id = self.next_instr_id;
            let get_param_instr = Rc::new(RefCell::new(Instruction::new(
                get_param_instr_id,
                Operator::GetPar { idx: i as u8 + 1 },
                None,
            )));
            self.next_instr_id += 1;

            self.get_block_mut(self.cur_block)
                .insert_identifier(param, get_param_instr_id);

            self.get_block_mut(self.cur_block)
                .push_instr(get_param_instr);
        }
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
            ControlFlowEdge::Leaf,
            Some(parent_block),
            dom_instr_map,
        );
        self.body.insert_block(new_block)
    }

    fn phi_detect(&self) -> (Vec<IdentifierId>, bool) {
        let mut tokens = self.tokens.clone();
        let mut identifier_ids = vec![];
        let mut current_block = vec![];

        let mut i = 1;

        while let Some(token) = tokens.next() {
            match token {
                Ok(Token::Let) => match tokens.next() {
                    Some(Ok(Token::Identifier(id))) => {
                        current_block.push(id);
                    }
                    _ => {
                        panic!("Expected an identifier");
                    }
                },
                Ok(Token::While | Token::If) => {
                    i += 1;
                }
                Ok(Token::Od | Token::Fi | Token::Else) => {
                    i -= 1;

                    identifier_ids.extend(current_block.iter().cloned());
                    current_block.clear();

                    if i == 0 {
                        break;
                    }
                }
                Ok(Token::Return) => {
                    current_block.clear();

                    while let Some(_) = tokens.next_if(|t| {
                        !matches!(t, Ok(Token::Fi | Token::Od | Token::Else | Token::RBrack))
                    }) {}

                    if i - 1 == 0 {
                        return (identifier_ids, true);
                    }
                }
                _ => (),
            }
        }

        let mut identifier_ids = identifier_ids.into_iter().collect::<Vec<_>>();
        identifier_ids.sort();
        identifier_ids.dedup();

        (identifier_ids, false)
    }

    fn stat_sequence(&mut self) -> Result<()> {
        self.statement()?;

        while let Some(token) = self.tokens.peek() {
            if let Ok(Token::Semicolon) = token {
                self.tokens.next();

                if let Some(Ok(token)) = self.tokens.peek() {
                    if matches!(
                        token,
                        Token::Let | Token::Call | Token::If | Token::While | Token::Return
                    ) {
                        self.statement()?;
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        let token = self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfFile)?;

        match token {
            Ok(Token::Let) => self.assignment(),
            Ok(Token::Call) => self.func_call().map(|_| ()),
            Ok(Token::If) => self.if_statement(),
            Ok(Token::While) => self.while_statement(),
            Ok(Token::Return) => self.return_statement(),
            _ => Err(Error::SyntaxError("Statement Error".into())),
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        self.match_token(Token::Return)?;

        if let Ok(result_id) = self.expression() {
            let return_instruction = Rc::new(RefCell::new(Instruction::new(
                self.next_instr_id,
                Operator::Ret(result_id),
                None,
            )));
            self.next_instr_id += 1;
            self.get_block_mut(self.cur_block)
                .push_instr(return_instruction);
        }

        let end_instr = Rc::new(RefCell::new(Instruction::new(
            self.next_instr_id,
            Operator::End,
            None,
        )));
        self.next_instr_id += 1;
        self.get_block_mut(self.cur_block).push_instr(end_instr);

        while let Some(_) = self
            .tokens
            .next_if(|t| !matches!(t, Ok(Token::Fi | Token::Od | Token::Else | Token::RBrack)))
        {
        }

        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        self.match_token(Token::While)?;

        let join_block = self.create_block(self.cur_block);
        self.get_block_mut(self.cur_block)
            .update_edge(ControlFlowEdge::Fallthrough(join_block));
        self.cur_block = join_block;

        let (phis, phi_return) = self.phi_detect();
        for phi in phis.clone() {
            let id_val = self
                .get_block_mut(join_block)
                .get_identifier(&phi)
                .unwrap_or_else(|| self.const_body.insert_returning_id(0));

            let phi_id = self.next_instr_id;
            let phi_instr = Rc::new(RefCell::new(Instruction::new(
                phi_id,
                Operator::Phi(id_val, 0),
                None,
            )));
            self.get_block_mut(join_block).push_instr(phi_instr);
            self.next_instr_id += 1;

            self.get_block_mut(join_block)
                .insert_identifier(phi, phi_id);
        }

        let (comparator_instruction_id, opposite_relop) = self.relation()?;
        self.match_token(Token::Do)?;

        let join_block_branch_id = self.next_instr_id;
        self.next_instr_id += 1;

        let body_block = self.create_block(join_block);
        self.get_block_mut(join_block)
            .update_edge(ControlFlowEdge::Fallthrough(body_block));
        self.cur_block = body_block;

        self.stat_sequence()?;
        self.match_token(Token::Od)?;

        let body_block_end = self.cur_block;

        if !phi_return {
            self.get_block_mut(body_block_end)
                .update_edge(ControlFlowEdge::Branch(join_block));

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
                .get_identifier(&phi)
                .unwrap_or_else(|| self.const_body.insert_returning_id(0));

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
                panic!("Expected Phi Operator: {:?}", temp);
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

        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.match_token(Token::If)?;

        let (comparator_instruction_id, opposite_relop) = self.relation()?;
        self.match_token(Token::Then)?;

        let (then_phis, then_phi_return) = self.phi_detect();

        let branch_block = self.cur_block;
        let branch_instruction_id = self.next_instr_id;
        self.next_instr_id += 1;

        let then_block = self.create_block(branch_block);
        self.cur_block = then_block;
        self.get_block_mut(branch_block)
            .update_edge(ControlFlowEdge::Fallthrough(then_block));

        self.stat_sequence()?;
        let then_block_end = self.cur_block;

        let mut else_phis = None;
        let mut else_phi_return = None;
        let is_else_present = matches!(
            self.tokens
                .peek()
                .ok_or_else(|| Error::UnexpectedEndOfFile)?,
            Ok(Token::Else)
        );
        if is_else_present {
            self.tokens.next();

            let (temp_else_phis, temp_else_phi_return) = self.phi_detect();
            else_phis = Some(temp_else_phis);
            else_phi_return = Some(temp_else_phi_return);

            let else_block = self.create_block(branch_block);
            self.cur_block = else_block;
            self.stat_sequence()?;
        }

        self.match_token(Token::Fi)?;

        let join_block = self.create_block(branch_block);

        // if then_phi_return && else_phi_return.unwrap_or(true) {
        //     return Ok(());
        // }

        if is_else_present && !else_phi_return.unwrap() {
            self.get_block_mut(self.cur_block)
                .update_edge(ControlFlowEdge::Fallthrough(join_block));
        }

        if !then_phi_return {
            self.get_block_mut(then_block_end)
                .update_edge(if is_else_present {
                    ControlFlowEdge::Branch(join_block)
                } else {
                    ControlFlowEdge::Fallthrough(join_block)
                });
        }

        let else_block_end = self.cur_block;

        self.get_block_mut(branch_block)
            .push_instr(Rc::new(RefCell::new(Instruction::new(
                branch_instruction_id,
                Operator::Branch(
                    BranchOpcode::from(opposite_relop),
                    if is_else_present {
                        else_block_end
                    } else {
                        join_block
                    },
                    comparator_instruction_id,
                ),
                None,
            ))));

        self.cur_block = join_block;

        // Should be ordered with no duplicates
        let mut phis = then_phis
            .into_iter()
            .chain(else_phis.unwrap_or_default().into_iter())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        phis.sort();
        phis.dedup();

        for phi in phis {
            let left_id_val = self
                .get_block_mut(if !then_phi_return {
                    then_block_end
                } else {
                    branch_block
                })
                .get_identifier(&phi)
                .unwrap_or_else(|| self.const_body.insert_returning_id(0));

            let right_id_val = if is_else_present {
                self.get_block_mut(if !else_phi_return.unwrap() {
                    else_block_end
                } else {
                    branch_block
                })
                .get_identifier(&phi)
                .unwrap_or_else(|| self.const_body.insert_returning_id(0))
            } else {
                self.get_block_mut(branch_block)
                    .get_identifier(&phi)
                    .unwrap_or_else(|| self.const_body.insert_returning_id(0))
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
                .insert_identifier(phi, phi_id);
        }

        Ok(())
    }

    fn func_call(&mut self) -> Result<InstructionId> {
        self.match_token(Token::Call)?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::Identifier(identifier) => {
                if let Ok(Token::LPar) = self
                    .tokens
                    .peek()
                    .ok_or_else(|| Error::UnexpectedEndOfFile)?
                {
                    self.match_token(Token::LPar)?;

                    if let Ok(expr) = self.expression() {
                        let mut args = vec![expr];

                        while let Some(Ok(Token::Comma)) = self.tokens.peek() {
                            self.tokens.next();
                            args.push(self.expression()?);
                        }

                        args.into_iter().enumerate().for_each(|(idx, val)| {
                            let arg_instr = Rc::new(RefCell::new(Instruction::new(
                                self.next_instr_id,
                                Operator::SetPar {
                                    idx: idx as u8 + 1,
                                    val,
                                },
                                None,
                            )));
                            self.next_instr_id += 1;
                            self.get_block_mut(self.cur_block).push_instr(arg_instr);
                        });
                    }

                    self.match_token(Token::RPar)?;
                }

                let call_instr_id = self.next_instr_id;
                let call_instr = Rc::new(RefCell::new(Instruction::new(
                    call_instr_id,
                    Operator::Jsr(identifier as i32),
                    None,
                )));
                self.next_instr_id += 1;

                self.get_block_mut(self.cur_block).push_instr(call_instr);

                Ok(call_instr_id)
            }
            Token::PredefinedFunction(func) => {
                let mut args = vec![];
                if let Ok(Token::LPar) = self
                    .tokens
                    .peek()
                    .ok_or_else(|| Error::UnexpectedEndOfFile)?
                {
                    self.match_token(Token::LPar)?;

                    if let Ok(expr) = self.expression() {
                        args.push(expr);

                        while let Some(Ok(Token::Comma)) = self.tokens.peek() {
                            self.tokens.next();
                            args.push(self.expression()?);
                        }
                    }

                    self.match_token(Token::RPar)?;
                }

                let operator = match func {
                    PredefinedFunction::InputNum => Operator::Read,
                    PredefinedFunction::OutputNum => Operator::Write(args[0]),
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

                Ok(call_instr_id)
            }

            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn assignment(&mut self) -> Result<()> {
        self.match_token(Token::Let)?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::Identifier(identifier_id) => {
                self.match_token(Token::Assignment)?;

                let instruction_id = self.expression()?;

                self.get_block_mut(self.cur_block)
                    .insert_identifier(identifier_id, instruction_id);

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<(InstructionId, RelOp)> {
        let left = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::RelOp(relop) => {
                let right = self.expression()?;
                Ok((
                    self.handle_binary_op(StoredBinaryOpcode::Cmp, left, right),
                    relop.opposite(),
                ))
            }
            _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
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

        if let Some(dom_instr) = self.get_block_mut(self.cur_block).get_dom_instr(&operator) {
            if let Some(dup_instr_id) = dom_instr.try_borrow().unwrap().check_dominators(&new_instr)
            {
                return dup_instr_id;
            }
        }

        if let Some(dom_instr) = self.get_block_mut(self.cur_block).get_dom_instr(&operator) {
            new_instr.update_dom(dom_instr);
        };

        let new_instr = Rc::new(RefCell::new(new_instr));
        self.get_block_mut(self.cur_block)
            .push_instr(new_instr.clone());
        self.next_instr_id += 1;

        new_instr_id
    }

    fn expression(&mut self) -> Result<InstructionId> {
        let mut left = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => {
                    self.tokens.next();
                    let right = self.term()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Add, left, right);
                }
                Ok(Token::Sub) => {
                    self.tokens.next();
                    let right = self.term()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Sub, left, right);
                }

                _ => break,
            }
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<InstructionId> {
        let mut left = self.factor()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Mul) => {
                    self.tokens.next();
                    let right = self.factor()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Mul, left, right);
                }
                Ok(Token::Div) => {
                    self.tokens.next();
                    let right = self.factor()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Div, left, right);
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<InstructionId> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfFile)?
        {
            Ok(Token::Identifier(id)) => {
                let instruction_id = self
                    .body
                    .get_mut_block(self.cur_block)
                    .unwrap()
                    .get_identifier(id)
                    .unwrap()
                    .clone();
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::Number(num)) => {
                let instruction_id = self.const_body.insert_returning_id(*num);
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::LPar) => {
                self.tokens.next();
                let instr_id = self.expression()?;
                match self.tokens.next() {
                    Some(Ok(Token::RPar)) => Ok(instr_id),
                    Some(Ok(token)) => Err(Error::UnexpectedToken {
                        expected: Token::RPar,
                        found: token,
                    }),
                    _ => Err(Error::SyntaxError("Expected ')'.".to_string())),
                }
            }
            Ok(Token::Call) => self.func_call(),
            Ok(token) => Err(Error::SyntaxError(format!("Unexpected token: {:?}", token))),
            Err(e) => Err(e.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions_sorted::assert_eq_sorted;
    use std::{cell::RefCell, collections::HashSet, rc::Rc};

    use crate::lexer::{PredefinedFunction, RelOp};

    #[test]
    fn simple_assignment() {
        /*
        let x <- 1;
        let y <- 1 + 2 + 0 - 4;
        let z <- 3 * 2 / 1;
        let w <- y + z;
        let w <- x * ((1 + w) / 2);
        */
        let tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(1)),
            Ok(Token::Assignment),
            Ok(Token::Number(1)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(2)),
            Ok(Token::Assignment),
            Ok(Token::Number(1)),
            Ok(Token::Add),
            Ok(Token::Number(2)),
            Ok(Token::Add),
            Ok(Token::Number(0)),
            Ok(Token::Sub),
            Ok(Token::Number(4)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(3)),
            Ok(Token::Assignment),
            Ok(Token::Number(3)),
            Ok(Token::Mul),
            Ok(Token::Number(2)),
            Ok(Token::Div),
            Ok(Token::Number(1)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(4)),
            Ok(Token::Assignment),
            Ok(Token::Identifier(2)),
            Ok(Token::Add),
            Ok(Token::Identifier(3)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(4)),
            Ok(Token::Assignment),
            Ok(Token::Identifier(1)),
            Ok(Token::Mul),
            Ok(Token::LPar),
            Ok(Token::LPar),
            Ok(Token::Number(1)),
            Ok(Token::Add),
            Ok(Token::Identifier(4)),
            Ok(Token::RPar),
            Ok(Token::Div),
            Ok(Token::Number(2)),
            Ok(Token::RPar),
        ];
        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(&mut tokens.into_iter().peekable(), &mut const_body);

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
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -3, -2),
            None,
        )));
        let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 4, -1),
            None,
        )));
        let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(
            6,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 5),
            Some(b0_insr_2.clone()),
        )));
        let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, 6),
            Some(b0_insr_6.clone()),
        )));
        let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(
            8,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 7, -2),
            Some(b0_insr_5.clone()),
        )));
        let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(
            9,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -1, 8),
            Some(b0_insr_4.clone()),
        )));

        let main_block_identifier_map =
            InheritingHashMap::from_iter([(1, -1), (2, 3), (3, 5), (4, 9)]);
        let main_block_dom_instr_map = InheritingHashMap::from_iter([
            (StoredBinaryOpcode::Add, b0_insr_7.clone()),
            (StoredBinaryOpcode::Mul, b0_insr_9.clone()),
            (StoredBinaryOpcode::Div, b0_insr_8.clone()),
            (StoredBinaryOpcode::Sub, b0_insr_3.clone()),
        ]);

        let main_block = BasicBlock::from(
            vec![
                b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6, b0_insr_7,
                b0_insr_8, b0_insr_9,
            ],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );
        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 2, 3, 4]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_duplicate_assignment() {
        /*
        let x <- 1 + 3;
        let y <- 1 + 3;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
        ];
        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 1)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_1.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone()],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );
        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn identifier_duplicate_assignment() {
        /*
        let x <- 1 + 3;
        let y <- x + 3;
        let a <- x + y;
        let z <- x + 3;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Identifier(2),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(4),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
        ];
        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -3),
            Some(b0_insr_1.clone()),
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 2),
            Some(b0_insr_2.clone()),
        )));

        let main_block_identifier_map =
            InheritingHashMap::from_iter([(1, 1), (2, 2), (3, 3), (4, 2)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_3.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3.clone()],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );
        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_branch() {
        /*
        let x <- 1;
        if x < 1 then
            let x <- 2;
        else
            let x <- 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
            Token::Semicolon,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2.into(), 1),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        then_block_identifier_map.insert(1, -2);
        let then_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let then_block = BasicBlock::from(
            Vec::new(),
            then_block_identifier_map,
            ControlFlowEdge::Branch(3.into()),
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        else_block_identifier_map.insert(1, -4);
        let else_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let else_block = BasicBlock::from(
            Vec::new(),
            else_block_identifier_map,
            ControlFlowEdge::Fallthrough(3.into()),
            Some(0.into()),
            else_block_dom_instr_map,
        );

        // Block 3
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Phi(-2, -4),
            None,
        )));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(1, 3);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b3_insr_1],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![main_block, then_block, else_block, join_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([1, 2, 4]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_duplicate_branch() {
        /*
        let x <- 1 + 4;
        if x < 1 then
            let x <- 2 + 1;
            let y <- 1 + 4;
        else
            let x <- 1 + 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
            Token::Semicolon,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -4),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -1),
            None,
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1)]);
        let main_block_dom_instr_map = InheritingHashMap::from_iter([
            (StoredBinaryOpcode::Add, b0_insr_1.clone()),
            (StoredBinaryOpcode::Cmp, b0_insr_2.clone()),
        ]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2.clone(), b0_insr_3],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -2, -1),
            Some(b0_insr_1.clone()),
        )));

        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        then_block_identifier_map.insert(1, 4);
        then_block_identifier_map.insert(2, 1);
        let mut then_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
        then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone()],
            then_block_identifier_map,
            ControlFlowEdge::Branch(3.into()),
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        else_block_identifier_map.insert(1, 1);
        let else_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let else_block = BasicBlock::from(
            Vec::new(),
            else_block_identifier_map,
            ControlFlowEdge::Fallthrough(3.into()),
            Some(0.into()),
            else_block_dom_instr_map,
        );

        // Block 3
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(5, Operator::Phi(4, 1), None)));
        let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(6, Operator::Phi(1, 0), None)));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(1, 5);
        join_block_identifier_map.insert(2, 6);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b3_insr_1, b3_insr_2],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![main_block, then_block, else_block, join_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 2, 4]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn new_identifiers_branch() {
        /*
        let x <- 1;
        if x < 1 then
            let y <- 2;
            let z <- 13;
        else
            let y <- 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Number(13),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2.into(), 1),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        then_block_identifier_map.insert(2, -2);
        then_block_identifier_map.insert(3, -13);
        let then_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let then_block = BasicBlock::from(
            Vec::new(),
            then_block_identifier_map,
            ControlFlowEdge::Branch(3.into()),
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        else_block_identifier_map.insert(2, -4);
        let else_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let else_block = BasicBlock::from(
            Vec::new(),
            else_block_identifier_map,
            ControlFlowEdge::Fallthrough(3.into()),
            Some(0.into()),
            else_block_dom_instr_map,
        );

        // Block 3
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Phi(-2, -4),
            None,
        )));
        let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::Phi(-13, 0),
            None,
        )));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(2, 3);
        join_block_identifier_map.insert(3, 4);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b3_insr_1, b3_insr_2],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![main_block, then_block, else_block, join_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 2, 4, 13]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn branch_without_else_statement() {
        /*
        let x <- 1;
        if x == 1 then
            let x <- 2;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Eq),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ne, 2.into(), 1),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        then_block_identifier_map.insert(1, -2);
        let then_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let then_block = BasicBlock::from(
            Vec::new(),
            then_block_identifier_map,
            ControlFlowEdge::Fallthrough(2.into()),
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Phi(-2, -1),
            None,
        )));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(1, 3);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b2_insr_1],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block, then_block, join_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([1, 2]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn nested_if_statements() {
        /*
        let x = 1;
        if x > 1 then
            if x < 3 then
                let x = 3;
            else
                let x = 4;
                let y = 12;
            fi;
        else
            let x = 5;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Gt),
            Token::Number(1),
            Token::Then,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(3),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(12),
            Token::Semicolon,
            Token::Fi,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(5),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Le, 5.into(), 1),
            None,
        )));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
        let main_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -3),
            Some(b0_insr_1.clone()),
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::Branch(BranchOpcode::Ge, 3.into(), 3),
            None,
        )));

        let then_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        let mut then_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
        then_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_1.clone());

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone(), b1_insr_2],
            then_block_identifier_map,
            ControlFlowEdge::Fallthrough(2.into()),
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let mut sub_then_block_identifier_map =
            InheritingHashMap::with_dominator(then_block.get_identifier_map());
        sub_then_block_identifier_map.insert(1, -3);
        let sub_then_block_dom_instr_map =
            InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

        let sub_then_block = BasicBlock::from(
            Vec::new(),
            sub_then_block_identifier_map,
            ControlFlowEdge::Branch(4.into()),
            Some(1.into()),
            sub_then_block_dom_instr_map,
        );

        // Block 3
        let mut sub_else_block_identifier_map =
            InheritingHashMap::with_dominator(then_block.get_identifier_map());
        sub_else_block_identifier_map.insert(1, -4);
        sub_else_block_identifier_map.insert(2, -12);
        let sub_else_block_dom_instr_map =
            InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

        let sub_else_block = BasicBlock::from(
            Vec::new(),
            sub_else_block_identifier_map,
            ControlFlowEdge::Fallthrough(4.into()),
            Some(1.into()),
            sub_else_block_dom_instr_map,
        );

        // Block 4
        let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::Phi(-3, -4),
            None,
        )));
        let b4_insr_2 = Rc::new(RefCell::new(Instruction::new(
            6,
            Operator::Phi(0, -12),
            None,
        )));

        let mut sub_join_block_identifier_map =
            InheritingHashMap::with_dominator(then_block.get_identifier_map());
        sub_join_block_identifier_map.insert(1, 5);
        sub_join_block_identifier_map.insert(2, 6);
        let sub_join_block_dom_instr_map =
            InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

        let sub_join_block = BasicBlock::from(
            vec![b4_insr_1.clone(), b4_insr_2.clone()],
            sub_join_block_identifier_map,
            ControlFlowEdge::Branch(6.into()),
            Some(1.into()),
            sub_join_block_dom_instr_map,
        );

        // Block 5
        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        else_block_identifier_map.insert(1, -5);
        let else_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let else_block = BasicBlock::from(
            Vec::new(),
            else_block_identifier_map,
            ControlFlowEdge::Fallthrough(6.into()),
            Some(0.into()),
            else_block_dom_instr_map,
        );

        // Block 6
        let b6_insr_1 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::Phi(5, -5),
            None,
        )));
        let b6_insr_2 = Rc::new(RefCell::new(Instruction::new(8, Operator::Phi(6, 0), None)));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(1, 7);
        join_block_identifier_map.insert(2, 8);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b6_insr_1.clone(), b6_insr_2.clone()],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![
                main_block,
                then_block,
                sub_then_block,
                sub_else_block,
                sub_join_block,
                else_block,
                join_block,
            ],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 3, 4, 5, 12]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_while_loop() {
        /*
        let x <- 1;
        while x < 3 do
            let x <- x + 1;
        od
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::While,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Do,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Od,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);

        let main_block = BasicBlock::from(
            Vec::new(),
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            InheritingHashMap::new(),
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::Phi(-1, 4),
            None,
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
            None,
        )));
        let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 3.into(), 2),
            None,
        )));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(1, 1);
        let mut join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
        join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_2.clone());

        let join_block = BasicBlock::from(
            vec![b1_insr_1.clone(), b1_insr_2.clone(), b1_insr_3],
            join_block_identifier_map,
            ControlFlowEdge::Fallthrough(2.into()),
            Some(0.into()),
            join_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::UnconditionalBranch(1.into()),
            None,
        )));

        let mut body_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        body_block_identifier_map.insert(1, 4);
        let mut body_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
        body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

        let body_block = BasicBlock::from(
            vec![b2_insr_1.clone(), b2_insr_2],
            body_block_identifier_map,
            ControlFlowEdge::Branch(1.into()),
            Some(1.into()),
            body_block_dom_instr_map,
        );

        // Block 3
        let escape_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        let escape_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

        let escape_block = BasicBlock::from(
            Vec::new(),
            escape_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(1.into()),
            escape_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![main_block, join_block, body_block, escape_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn nested_while_loop() {
        /*
        let i <- 0;
        let x <- 0;
        let y <- 0;
        let j <- i;
        while x < 10 do
            let x <- i + 1;
            let y <- j + 1;
            while j < 10 do
                let x <- j + 1;
                let y <- i + 1;
                let j <- j + 1
            od;
            let i <- i + 1
        od;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(0),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(0),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(0),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Identifier(0),
            Token::Semicolon,
            Token::While,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(10),
            Token::Do,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(0),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(3),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::While,
            Token::Identifier(3),
            Token::RelOp(RelOp::Lt),
            Token::Number(10),
            Token::Do,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(3),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(0),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Identifier(3),
            Token::Add,
            Token::Number(1),
            Token::Od,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Identifier(0),
            Token::Add,
            Token::Number(1),
            Token::Od,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let main_block_identifier_map =
            InheritingHashMap::from_iter([(0, 0), (1, 0), (2, 0), (3, 0)]);

        let main_block = BasicBlock::from(
            Vec::new(),
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            InheritingHashMap::new(),
        );

        // Block 1

        // i
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Phi(0, 7), None)));

        // x
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Phi(0, 9), None)));

        // y
        let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Phi(0, 10),
            None,
        )));

        // j
        let b1_insr_4 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::Phi(0, 11),
            None,
        )));

        let b1_insr_5 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 2, -10),
            None,
        )));
        let b1_insr_6 = Rc::new(RefCell::new(Instruction::new(
            6,
            Operator::Branch(BranchOpcode::Ge, 6.into(), 5),
            None,
        )));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        join_block_identifier_map.insert(0, 1);
        join_block_identifier_map.insert(1, 2);
        join_block_identifier_map.insert(2, 3);
        join_block_identifier_map.insert(3, 4);
        let mut join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
        join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_5.clone());

        let join_block = BasicBlock::from(
            vec![
                b1_insr_1.clone(),
                b1_insr_2.clone(),
                b1_insr_3.clone(),
                b1_insr_4.clone(),
                b1_insr_5.clone(),
                b1_insr_6.clone(),
            ],
            join_block_identifier_map,
            ControlFlowEdge::Fallthrough(2.into()),
            Some(0.into()),
            join_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
            8,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 4, -1),
            Some(b2_insr_1.clone()),
        )));

        let mut body_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        body_block_identifier_map.insert(1, 7);
        body_block_identifier_map.insert(2, 8);
        let mut body_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
        body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_2.clone());

        let body_block = BasicBlock::from(
            vec![b2_insr_1.clone(), b2_insr_2.clone()],
            body_block_identifier_map,
            ControlFlowEdge::Fallthrough(3.into()),
            Some(1.into()),
            body_block_dom_instr_map,
        );

        // Block 3

        // x
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
            9,
            Operator::Phi(7, 14),
            None,
        )));

        // y
        let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(
            10,
            Operator::Phi(8, 7),
            None,
        )));

        // j
        let b3_insr_3 = Rc::new(RefCell::new(Instruction::new(
            11,
            Operator::Phi(4, 14),
            None,
        )));

        let b3_insr_4 = Rc::new(RefCell::new(Instruction::new(
            12,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 11, -10),
            Some(b1_insr_5.clone()),
        )));
        let b3_insr_5 = Rc::new(RefCell::new(Instruction::new(
            13,
            Operator::Branch(BranchOpcode::Ge, 5.into(), 12),
            None,
        )));

        let mut join_block_2_identifier_map =
            InheritingHashMap::with_dominator(body_block.get_identifier_map());
        join_block_2_identifier_map.insert(1, 9);
        join_block_2_identifier_map.insert(2, 10);
        join_block_2_identifier_map.insert(3, 11);
        let mut join_block_2_dom_instr_map =
            InheritingHashMap::with_dominator(body_block.get_dom_instr_map());
        join_block_2_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b3_insr_4.clone());

        let join_block_2 = BasicBlock::from(
            vec![
                b3_insr_1.clone(),
                b3_insr_2.clone(),
                b3_insr_3.clone(),
                b3_insr_4.clone(),
                b3_insr_5.clone(),
            ],
            join_block_2_identifier_map,
            ControlFlowEdge::Fallthrough(4.into()),
            Some(2.into()),
            join_block_2_dom_instr_map,
        );

        // Block 4
        let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
            14,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 11, -1),
            Some(b2_insr_2.clone()),
        )));
        let b4_insr_4 = Rc::new(RefCell::new(Instruction::new(
            15,
            Operator::UnconditionalBranch(3.into()),
            None,
        )));

        let mut body_block_2_identifier_map =
            InheritingHashMap::with_dominator(join_block_2.get_identifier_map());
        body_block_2_identifier_map.insert(1, 14);
        body_block_2_identifier_map.insert(2, 7);
        body_block_2_identifier_map.insert(3, 14);
        let mut body_block_2_dom_instr_map =
            InheritingHashMap::with_dominator(join_block_2.get_dom_instr_map());
        body_block_2_dom_instr_map.insert(StoredBinaryOpcode::Add, b4_insr_1.clone());

        let body_block_2 = BasicBlock::from(
            vec![b4_insr_1.clone(), b4_insr_4.clone()],
            body_block_2_identifier_map,
            ControlFlowEdge::Branch(3.into()),
            Some(3.into()),
            body_block_2_dom_instr_map,
        );

        // Block 5
        let b5_insr_1 = Rc::new(RefCell::new(Instruction::new(
            16,
            Operator::UnconditionalBranch(1.into()),
            None,
        )));

        let mut escape_block_2_identifier_map =
            InheritingHashMap::with_dominator(join_block_2.get_identifier_map());
        escape_block_2_identifier_map.insert(0, 7);
        let escape_block_2_dom_instr_map =
            InheritingHashMap::with_dominator(join_block_2.get_dom_instr_map());

        let escape_block2 = BasicBlock::from(
            vec![b5_insr_1.clone()],
            escape_block_2_identifier_map,
            ControlFlowEdge::Branch(1.into()),
            Some(3.into()),
            escape_block_2_dom_instr_map,
        );

        // Block 6
        let escape_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        let escape_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

        let escape_block = BasicBlock::from(
            Vec::new(),
            escape_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(1.into()),
            escape_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![
                main_block,
                join_block,
                body_block,
                join_block_2,
                body_block_2,
                escape_block2,
                escape_block,
            ],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 10]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_return() {
        /*
        let x <- 1;
        return x
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(1),
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Ret(-1), None)));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::End, None)));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
        let main_block_dom_instr_map = InheritingHashMap::new();

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([1]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_void_return() {
        /*
        return
        */
        let tokens = [Token::Return];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::End, None)));

        let main_block_identifier_map = InheritingHashMap::new();
        let main_block_dom_instr_map = InheritingHashMap::new();

        let main_block = BasicBlock::from(
            vec![b0_insr_1],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block]);

        let expected_const_body = ConstBlock::new();

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn loop_body_return() {
        /*
        let x <- call InputNum;
        let y <- x;
        while x < 3 do
            let y <- y + 1;
            return x;
            let x <- x + 3
        od;
        return 1
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(1),
            Token::Semicolon,
            Token::While,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Do,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(2),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Od,
            Token::Semicolon,
            Token::Return,
            Token::Number(1),
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 1)]);
        let main_block_dom_instr_map = InheritingHashMap::new();

        let main_block = BasicBlock::from(
            vec![b0_insr_1],
            main_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            main_block_dom_instr_map,
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
            None,
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 3.into(), 2),
            None,
        )));

        let join_block_identifier_map =
            InheritingHashMap::with_dominator(main_block.get_identifier_map());
        let mut join_block_dom_instr_map =
            InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
        join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_1.clone());

        let join_block = BasicBlock::from(
            vec![b1_insr_1, b1_insr_2],
            join_block_identifier_map,
            ControlFlowEdge::Fallthrough(2.into()),
            Some(0.into()),
            join_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(5, Operator::Ret(1), None)));
        let b2_insr_3 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

        let mut body_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        body_block_identifier_map.insert(2, 4);
        let mut body_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
        body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

        let body_block = BasicBlock::from(
            vec![b2_insr_1, b2_insr_2, b2_insr_3],
            body_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(1.into()),
            body_block_dom_instr_map,
        );

        // Block 3
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(7, Operator::Ret(-1), None)));
        let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(8, Operator::End, None)));

        let escape_block_identifier_map =
            InheritingHashMap::with_dominator(join_block.get_identifier_map());
        let escape_block_dom_instr_map =
            InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

        let escape_block = BasicBlock::from(
            vec![b3_insr_1, b3_insr_2],
            escape_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(1.into()),
            escape_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![main_block, join_block, body_block, escape_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn single_branch_return() {
        /*
        let x <- call InputNum;
        if x < 3 then
            let x <- x + 1;
            return x
        else
            x <- x + 1
        fi;
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Fi,
            Token::Semicolon,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
            None,
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
            None,
        )));

        let branch_block_identifier_map = InheritingHashMap::from_iter([(1, 1)]);
        let branch_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_2.clone())]);

        let branch_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3],
            branch_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            branch_block_dom_instr_map,
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(5, Operator::Ret(4), None)));
        let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(branch_block.get_identifier_map());
        then_block_identifier_map.insert(1, 4);
        let mut then_block_dom_instr_map =
            InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
        then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

        let then_block = BasicBlock::from(
            vec![b1_insr_1, b1_insr_2, b1_insr_3],
            then_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));

        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(branch_block.get_identifier_map());
        else_block_identifier_map.insert(1, 7);
        let mut else_block_dom_instr_map =
            InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
        else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

        let else_block = BasicBlock::from(
            vec![b2_insr_1],
            else_block_identifier_map,
            ControlFlowEdge::Fallthrough(3.into()),
            Some(0.into()),
            else_block_dom_instr_map,
        );

        // Block 3
        let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(8, Operator::Phi(1, 7), None)));

        let mut join_block_identifier_map =
            InheritingHashMap::with_dominator(branch_block.get_identifier_map());
        join_block_identifier_map.insert(1, 8);
        let join_block_dom_instr_map =
            InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

        let join_block = BasicBlock::from(
            vec![b3_insr_1],
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            join_block_dom_instr_map,
        );

        let expected_body = Body::from(
            Some(0.into()),
            vec![branch_block, then_block, else_block, join_block],
        );

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn both_branch_return() {
        /*
        let x <- call InputNum;
        if x < 3 then
            let x <- x + 1;
            return x
        else
            let x <- x + 1;
            return x;
            let x <- x + 3;
        fi;
        return 1
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Fi,
            Token::Semicolon,
            Token::Return,
            Token::Number(1),
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
            None,
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
            None,
        )));

        let branch_block_identifier_map = InheritingHashMap::from_iter([(1, 1)]);
        let branch_block_dom_instr_map =
            InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_2.clone())]);

        let branch_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3],
            branch_block_identifier_map,
            ControlFlowEdge::Fallthrough(1.into()),
            None,
            branch_block_dom_instr_map,
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(5, Operator::Ret(4), None)));
        let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

        let mut then_block_identifier_map =
            InheritingHashMap::with_dominator(branch_block.get_identifier_map());
        then_block_identifier_map.insert(1, 4);
        let mut then_block_dom_instr_map =
            InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
        then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

        let then_block = BasicBlock::from(
            vec![b1_insr_1, b1_insr_2, b1_insr_3],
            then_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            then_block_dom_instr_map,
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
            None,
        )));
        let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(8, Operator::Ret(7), None)));
        let b2_insr_3 = Rc::new(RefCell::new(Instruction::new(9, Operator::End, None)));

        let mut else_block_identifier_map =
            InheritingHashMap::with_dominator(branch_block.get_identifier_map());
        else_block_identifier_map.insert(1, 7);
        let mut else_block_dom_instr_map =
            InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
        else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

        let else_block = BasicBlock::from(
            vec![b2_insr_1, b2_insr_2, b2_insr_3],
            else_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(0.into()),
            else_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![branch_block, then_block, else_block]);

        let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_predefined_function_call() {
        /*
        let x <- call InputNum();
        call OutputNum(x);
        call OutputNewLine;
        let y <- call InputNum;
        call OutputNewLine()
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNewLine),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNewLine),
            Token::LPar,
            Token::RPar,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Write(1), None)));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::WriteNL, None)));
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Read, None)));
        let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(5, Operator::WriteNL, None)));

        let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 4)]);
        let main_block_dom_instr_map = InheritingHashMap::new();

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block]);
        let expected_const_body = ConstBlock::from(HashSet::new());

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn simple_user_defined_functions() {
        /*
        let a <- 1;
        let b <- call user1(a);
        let c <- call user2;
        call user3();
        let d <- call user4();
        call user5
        let e <- call user6(a, b, c)
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Call,
            Token::Identifier(10),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Call,
            Token::Identifier(11),
            Token::Semicolon,
            Token::Call,
            Token::Identifier(12),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(4),
            Token::Assignment,
            Token::Call,
            Token::Identifier(13),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::Identifier(14),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(5),
            Token::Assignment,
            Token::Call,
            Token::Identifier(15),
            Token::LPar,
            Token::Identifier(1),
            Token::Comma,
            Token::Identifier(2),
            Token::Comma,
            Token::Identifier(3),
            Token::RPar,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::SetPar { idx: 1, val: -1 },
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Jsr(10), None)));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::Jsr(11), None)));
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Jsr(12), None)));
        let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(5, Operator::Jsr(13), None)));
        let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(6, Operator::Jsr(14), None)));
        let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::SetPar { idx: 1, val: -1 },
            None,
        )));
        let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(
            8,
            Operator::SetPar { idx: 2, val: 2 },
            None,
        )));
        let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(
            9,
            Operator::SetPar { idx: 3, val: 3 },
            None,
        )));
        let b0_insr_10 = Rc::new(RefCell::new(Instruction::new(10, Operator::Jsr(15), None)));

        let main_block_identifier_map =
            InheritingHashMap::from_iter([(1, -1), (2, 2), (3, 3), (4, 5), (5, 10)]);
        let main_block_dom_instr_map = InheritingHashMap::new();

        let main_block = BasicBlock::from(
            vec![
                b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6, b0_insr_7,
                b0_insr_8, b0_insr_9, b0_insr_10,
            ],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block]);
        let expected_const_body = ConstBlock::from(HashSet::from([1]));

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }

    #[test]
    fn commutative_cse() {
        /*
        let a <- call InputNum();
        let b <- call InputNum();
        let c <- call InputNum();
        let d <- call InputNum();

        call OutputNum(a * b + c * d);
        call OutputNum(d * c + b * a)
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(4),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(1),
            Token::Mul,
            Token::Identifier(2),
            Token::Add,
            Token::Identifier(3),
            Token::Mul,
            Token::Identifier(4),
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(4),
            Token::Mul,
            Token::Identifier(3),
            Token::Add,
            Token::Identifier(2),
            Token::Mul,
            Token::Identifier(1),
            Token::RPar,
        ];

        let mut const_body = ConstBlock::new();

        let body = BodyParser::parse_main(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Read, None)));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::Read, None)));
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Read, None)));
        let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 1, 2),
            None,
        )));
        let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(
            6,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 3, 4),
            Some(b0_insr_5.clone()),
        )));
        let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 5, 6),
            None,
        )));
        let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(8, Operator::Write(7), None)));
        let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(9, Operator::Write(7), None)));

        let main_block_identifier_map =
            InheritingHashMap::from_iter([(1, 1), (2, 2), (3, 3), (4, 4)]);
        let main_block_dom_instr_map = InheritingHashMap::from_iter([
            (StoredBinaryOpcode::Mul, b0_insr_6.clone()),
            (StoredBinaryOpcode::Add, b0_insr_7.clone()),
        ]);

        let main_block = BasicBlock::from(
            vec![
                b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6, b0_insr_7,
                b0_insr_8, b0_insr_9,
            ],
            main_block_identifier_map,
            ControlFlowEdge::Leaf,
            None,
            main_block_dom_instr_map,
        );

        let expected_body = Body::from(Some(0.into()), vec![main_block]);
        let expected_const_body = ConstBlock::from(HashSet::new());

        assert_eq_sorted!(body, expected_body);
        assert_eq_sorted!(const_body, expected_const_body);
    }
}
