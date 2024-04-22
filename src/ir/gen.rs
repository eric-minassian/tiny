use std::{collections::HashMap, iter::Peekable};

use crate::{
    error::{Error, Result},
    lexer::{Token, Tokenizer},
};

use super::{
    block::{BasicBlock, BasicBlockId, Body, ControlFlowEdge},
    ssa::{Instruction, InstructionId, Operator, StoredBinaryOpcode},
    ConstBody, IrStore,
};

pub struct BodyParser<'a> {
    tokens: &'a mut Peekable<Tokenizer<'a>>,
    body: Body<'a>,
    cur_block: BasicBlockId,
    ir_store: &'a mut IrStore<'a>,
    const_body: &'a mut ConstBody<'a>,
}

impl<'a> BodyParser<'a> {
    pub fn new(
        tokens: &'a mut Peekable<Tokenizer<'a>>,
        ir_store: &'a mut IrStore<'a>,
        const_body: &'a mut ConstBody<'a>,
    ) -> Self {
        let mut body = Body::new();
        let cur_block = body.insert_block(BasicBlock::new(Vec::new(), ControlFlowEdge::Leaf));
        body.update_root(cur_block);

        Self {
            tokens,
            body,
            cur_block,
            ir_store,
            const_body,
        }
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        let next_token = self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfTokens)??;

        if next_token != expected {
            return Err(Error::UnexpectedToken(expected, next_token));
        }

        Ok(())
    }

    pub fn parse(&mut self) -> Result<Body<'a>> {
        self.stat_sequence()?;

        Ok(self.body.clone())
    }

    fn stat_sequence(&mut self) -> Result<()> {
        self.statement()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Semicolon) => {
                    self.tokens.next();
                    match self.tokens.peek() {
                        Some(Ok(
                            Token::Let | Token::Call | Token::If | Token::While | Token::Return,
                        )) => self.statement()?,
                        _ => break,
                    }
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfTokens)?
            .clone()?
        {
            Token::Let => self.assignment(),
            Token::Call => self.func_call(),
            Token::If => self.if_statement(),
            Token::While => self.while_statement(),
            Token::Return => self.return_statement(),
            token => todo!("Propagate error for incorrect token in statement"),
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        todo!()
    }

    fn while_statement(&mut self) -> Result<()> {
        todo!()
    }

    fn get_and_set_phi(&mut self, left_block_id: BasicBlockId, right_block_id: BasicBlockId) {
        let left_identifier_map = self
            .body
            .mut_block(left_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        let right_identifier_map = self
            .body
            .mut_block(right_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        let mut new_identifier_map = HashMap::new();
        let mut new_instructions = Vec::new();

        for (identifier_id, left_instr_id) in left_identifier_map.iter() {
            if let Some(right_instr_id) = right_identifier_map.get(&identifier_id) {
                if right_instr_id != left_instr_id {
                    let new_instr_id = self.ir_store.get_instr_count();
                    self.ir_store.increment_instr_count();

                    new_instructions.push(Instruction::new(
                        new_instr_id,
                        Operator::Phi(*left_instr_id, *right_instr_id),
                        None,
                    ));

                    new_identifier_map.insert(*identifier_id, new_instr_id);
                } else {
                    new_identifier_map.insert(*identifier_id, *left_instr_id);
                }
            }
        }
        // @TODO: Fix if a new variable is assigned a value

        let block = self.body.mut_block(self.cur_block).unwrap();

        block.update_identifier_map(new_identifier_map);
        block.update_instructions(new_instructions);
    }

    fn if_statement(&mut self) -> Result<()> {
        self.match_token(Token::If)?;

        let cmp_instr_id = self.relation()?;

        self.match_token(Token::Then)?;

        // Add Branch Instruction
        let branch_block_id = self.cur_block;
        let branch_instr_id = self.ir_store.get_instr_count();
        self.ir_store.increment_instr_count();
        let branch_block_identifier_map = self
            .body
            .mut_block(branch_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        // Create new then block
        let then_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            branch_block_identifier_map.clone(),
            ControlFlowEdge::Leaf,
            Some(branch_block_id),
        ));
        self.cur_block = then_block_id;

        self.body
            .mut_block(branch_block_id)
            .unwrap()
            .update_edge(ControlFlowEdge::Fallthrough(then_block_id));

        self.stat_sequence()?;

        let then_block_end_id = self.cur_block;

        self.body
            .mut_block(branch_block_id)
            .unwrap()
            .insert_instruction(Instruction::new(
                branch_instr_id,
                Operator::Bge(self.ir_store.get_instr_count(), cmp_instr_id),
                None,
            ));

        let mut is_else = false;

        if *self
            .tokens
            .peek()
            .ok_or_else(|| Error::SyntaxError("Expected 'fi' keyword".into()))?
            == Ok(Token::Else)
        {
            self.tokens.next();
            is_else = true;

            let else_block_id = self.body.insert_block(BasicBlock::from(
                Vec::new(),
                branch_block_identifier_map,
                ControlFlowEdge::Leaf,
                Some(branch_block_id),
            ));
            self.cur_block = else_block_id;

            self.stat_sequence()?;
        }

        let else_block_end_id = self.cur_block;

        self.match_token(Token::Fi)?;

        let join_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            HashMap::new(),
            ControlFlowEdge::Leaf,
            Some(branch_block_id),
        ));

        self.body
            .mut_block(then_block_end_id)
            .as_mut()
            .unwrap()
            .update_edge(ControlFlowEdge::Branch(join_block_id));

        if is_else {
            self.body
                .mut_block(else_block_end_id)
                .as_mut()
                .unwrap()
                .update_edge(ControlFlowEdge::Fallthrough(join_block_id));
        }

        self.cur_block = join_block_id;

        if is_else {
            self.get_and_set_phi(then_block_end_id, else_block_end_id);
        } else {
            self.get_and_set_phi(then_block_end_id, branch_block_id);
        }
        Ok(())
    }

    fn func_call(&mut self) -> Result<()> {
        todo!()
    }

    fn assignment(&mut self) -> Result<()> {
        self.match_token(Token::Let)?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        {
            Token::Identifier(identifier_id) => {
                self.match_token(Token::Assignment)?;

                let instruction_id = self.expression()?;

                self.body
                    .mut_block(self.cur_block)
                    .ok_or_else(|| Error::InternalError)?
                    .insert_identifier(identifier_id, instruction_id);

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        instr_id: InstructionId,
        instr_id2: InstructionId,
    ) -> Result<InstructionId> {
        let new_instr_id = self.ir_store.get_instr_count();

        self.body
            .mut_block(self.cur_block)
            .ok_or_else(|| Error::InternalError)?
            .insert_instruction(Instruction::new(
                new_instr_id,
                Operator::StoredBinaryOp(operator, instr_id, instr_id2),
                None,
            ));

        self.ir_store.increment_instr_count();

        Ok(new_instr_id)
    }

    fn relation(&mut self) -> Result<InstructionId> {
        let instr_id = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfTokens)??
        {
            Token::RelOp(_) => {
                let instr_id2 = self.expression()?;
                Ok(self.handle_binary_op(StoredBinaryOpcode::Cmp, instr_id, instr_id2)?)
            }
            token => todo!("Propagate error for incorrect token in relop"),
        }
    }

    fn expression(&mut self) -> Result<InstructionId> {
        let mut instr_id = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id =
                        self.handle_binary_op(StoredBinaryOpcode::Add, instr_id, instr_id2)?;
                }
                Ok(Token::Sub) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id =
                        self.handle_binary_op(StoredBinaryOpcode::Sub, instr_id, instr_id2)?;
                }

                _ => break,
            }
        }

        Ok(instr_id)
    }

    fn term(&mut self) -> Result<InstructionId> {
        let mut instr_id = self.factor()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Mul) => {
                    self.tokens.next();
                    let instr_id2 = self.factor()?;
                    instr_id =
                        self.handle_binary_op(StoredBinaryOpcode::Mul, instr_id, instr_id2)?;
                }
                Ok(Token::Div) => {
                    self.tokens.next();
                    let instr_id2 = self.factor()?;
                    instr_id =
                        self.handle_binary_op(StoredBinaryOpcode::Div, instr_id, instr_id2)?;
                }
                _ => break,
            }
        }

        Ok(instr_id)
    }

    fn factor(&mut self) -> Result<InstructionId> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfTokens)?
        {
            Ok(Token::Identifier(id)) => {
                let instruction_id = self
                    .body
                    .mut_block(self.cur_block)
                    .ok_or_else(|| Error::InternalError)?
                    .get_identifier(id)
                    .ok_or_else(|| Error::InternalError)?
                    .clone();
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::Number(num)) => {
                if let Some(instruction_id) = self.const_body.get_instruction_id(*num) {
                    self.tokens.next();

                    Ok(*instruction_id)
                } else {
                    let instruction_id = self.ir_store.get_instr_count();
                    self.const_body.insert(*num, instruction_id);
                    self.ir_store.increment_instr_count();

                    self.tokens.next();

                    Ok(instruction_id)
                }
            }
            Ok(Token::LPar) => {
                self.tokens.next();
                let instr_id = self.expression()?;
                let instr_id = 12;
                match self.tokens.next() {
                    Some(Ok(Token::RPar)) => Ok(instr_id),
                    Some(Ok(token)) => Err(Error::UnexpectedToken(Token::RPar, token)),
                    _ => Err(Error::UnexpectedEndOfTokens),
                }
            }
            Ok(_) => {
                // let _ = self.func_call();
                todo!("Function call support")
            }
            Err(e) => Err(e.clone()),
        }
    }
}
