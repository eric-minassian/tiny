use std::{collections::HashMap, iter::Peekable};

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, BasicBlockId, Body, ControlFlowEdge},
        ssa::{Instruction, Operator, StoredBinaryOpcode},
        ConstBody, InstructionId, IrStore,
    },
    lexer::{Token, Tokenizer},
};

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    instruction_counter: u32,
    store: IrStore<'a>,
    const_body: ConstBody<'a>,
    cur_body: Option<Body<'a>>,
    cur_block: Option<BasicBlockId>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            instruction_counter: 1,
            store: IrStore::new(),
            const_body: ConstBody::new(),
            cur_body: None,
            cur_block: None,
        }
    }

    fn match_token(&mut self, expected: Token, message: &str) -> Result<()> {
        if self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError(message.into()))??
            != expected
        {
            return Err(Error::SyntaxError(message.into()));
        }

        Ok(())
    }

    fn push_instruction(&mut self, instruction: Instruction<'a>) {
        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(self.cur_block.unwrap())
            .unwrap()
            .insert_instruction(instruction);
    }

    fn computation(&mut self) -> Result<()> {
        self.match_token(Token::Main, "Expected 'main' keyword")?;

        if let Some(Ok(Token::Var)) = self.tokens.peek() {
            todo!()
        }

        while let Some(Ok(Token::Call)) = self.tokens.peek() {
            todo!()
        }

        self.match_token(Token::LBrack, "Expected '{' symbol")?;

        let mut cur_body = Body::new();
        let cur_block = BasicBlock::new(Vec::new(), ControlFlowEdge::Leaf);

        let cur_block = cur_body.insert_block(cur_block);

        cur_body.update_root(cur_block);

        self.cur_body = Some(cur_body);
        self.cur_block = Some(cur_block);

        self.stat_sequence()?;

        // Create Const Block
        let const_block = BasicBlock::new(
            self.const_body.get_instructions().clone(),
            ControlFlowEdge::Fallthrough(cur_block),
        );
        let const_block_id = self.cur_body.as_mut().unwrap().insert_block(const_block);

        // Update Dominator
        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(cur_block)
            .unwrap()
            .update_dominator(const_block_id);

        self.cur_body.as_mut().unwrap().update_root(const_block_id);

        self.store
            .insert("main".to_string(), self.cur_body.take().unwrap());

        self.cur_block = None;
        self.cur_body = None;

        self.match_token(Token::RBrack, "Expected '}' symbol")?;
        self.match_token(Token::Period, "Expected '.' symbol")?;

        Ok(())
    }

    fn func_body(&mut self) -> Result<()> {
        todo!()
    }

    fn formal_param(&mut self) -> Result<()> {
        todo!()
    }

    fn func_decl(&mut self) -> Result<()> {
        todo!()
    }

    fn var_decl(&mut self) -> Result<()> {
        todo!()
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
            .ok_or_else(|| Error::SyntaxError("Statement Error".into()))?
        {
            Ok(Token::Let) => self.assignment(),
            Ok(Token::Call) => self.func_call(),
            Ok(Token::If) => self.if_statement(),
            Ok(Token::While) => self.while_statement(),
            Ok(Token::Return) => self.return_statement(),
            _ => Err(Error::SyntaxError("Statement Error".into())),
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        // self.match_token(Token::Return, "Expected 'return' keyword")?;

        // match self.expression() {
        //     Ok(_) => todo!(),
        //     Err(_) => todo!(),
        // }

        todo!()
    }

    fn while_statement(&mut self) -> Result<()> {
        // self.match_token(Token::While, "Expected 'while' keyword")?;

        // let _ = self.relation()?;

        // self.match_token(Token::Do, "Expected 'do' keyword")?;

        // let _ = self.stat_sequence()?;

        // self.match_token(Token::Od, "Expected 'od' keyword")?;

        // Ok(())
        todo!()
    }

    fn get_and_set_phi(&mut self, left_block_id: BasicBlockId, right_block_id: BasicBlockId) {
        let left_identifier_map = self
            .cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(left_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        let right_identifier_map = self
            .cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(right_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        let mut new_identifier_map = HashMap::new();
        let mut new_instructions = Vec::new();

        for (identifier_id, left_instr_id) in left_identifier_map.iter() {
            if let Some(right_instr_id) = right_identifier_map.get(&identifier_id) {
                if right_instr_id != left_instr_id {
                    let new_instr_id = self.instruction_counter;
                    self.instruction_counter += 1;

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

        let mut block = self
            .cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(self.cur_block.unwrap())
            .unwrap();

        block.update_identifier_map(new_identifier_map);
        block.update_instructions(new_instructions);
    }

    fn if_statement(&mut self) -> Result<()> {
        self.match_token(Token::If, "Expected 'if' keyword")?;

        let cmp_instr_id = self.relation()?;

        self.match_token(Token::Then, "Expected 'then' keyword")?;

        // Add Branch Instruction
        let branch_block_id = self.cur_block.unwrap();
        let branch_instr_id = self.instruction_counter;
        self.instruction_counter += 1;
        let branch_block_identifier_map = self
            .cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(branch_block_id)
            .as_ref()
            .unwrap()
            .get_identifier_map_copy();

        // Create new then block
        let then_block_id = self
            .cur_body
            .as_mut()
            .unwrap()
            .insert_block(BasicBlock::from(
                Vec::new(),
                branch_block_identifier_map.clone(),
                ControlFlowEdge::Leaf,
                Some(branch_block_id),
            ));
        self.cur_block = Some(then_block_id);

        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(branch_block_id)
            .unwrap()
            .update_edge(ControlFlowEdge::Fallthrough(then_block_id));

        self.stat_sequence()?;

        let then_block_end_id = self.cur_block.unwrap();

        // if *self
        //     .tokens
        //     .peek()
        //     .ok_or_else(|| Error::SyntaxError("Expected 'fi' keyword".into()))?
        //     == Ok(Token::Else)
        // {
        if !(*self
            .tokens
            .peek()
            .ok_or_else(|| Error::SyntaxError("Expected 'fi' keyword".into()))?
            == Ok(Token::Else))
        {
            return Err(Error::SyntaxError("Temp".to_string()));
        }

        self.tokens.next();

        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(branch_block_id)
            .unwrap()
            .insert_instruction(Instruction::new(
                branch_instr_id,
                Operator::Bge(self.instruction_counter, cmp_instr_id),
                None,
            ));

        let else_block_id = self
            .cur_body
            .as_mut()
            .unwrap()
            .insert_block(BasicBlock::from(
                Vec::new(),
                branch_block_identifier_map,
                ControlFlowEdge::Leaf,
                Some(branch_block_id),
            ));
        self.cur_block = Some(else_block_id);

        self.stat_sequence()?;

        let else_block_end_id = self.cur_block.unwrap();
        // }

        self.match_token(Token::Fi, "expected 'fi' keyword".into())?;

        let join_block_id = self
            .cur_body
            .as_mut()
            .unwrap()
            .insert_block(BasicBlock::from(
                Vec::new(),
                HashMap::new(),
                ControlFlowEdge::Leaf,
                Some(branch_block_id),
            ));

        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(then_block_end_id)
            .as_mut()
            .unwrap()
            .update_edge(ControlFlowEdge::Branch(join_block_id));
        self.cur_body
            .as_mut()
            .unwrap()
            .get_mut_block(else_block_end_id)
            .as_mut()
            .unwrap()
            .update_edge(ControlFlowEdge::Fallthrough(join_block_id));

        self.cur_block = Some(join_block_id);

        self.get_and_set_phi(then_block_end_id, else_block_end_id);

        Ok(())
    }

    fn func_call(&mut self) -> Result<()> {
        // self.match_token(Token::Call, "Expected 'call' keyword")?;

        // match self
        //     .tokens
        //     .next()
        //     .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        // {
        //     Token::Identifier(id) => {
        //         // @TODO: Functions without parameters can be called with or without parentheses
        //         self.match_token(Token::LPar, "Expected '(' symbol")?;

        //         let _ = self.expression()?;

        //         while let Some(token) = self.tokens.peek() {
        //             match token {
        //                 Ok(Token::Comma) => {
        //                     let _ = self.expression()?;
        //                 }
        //                 _ => break,
        //             }
        //         }

        //         todo!()
        //     }
        //     _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        // }

        todo!()
    }

    fn assignment(&mut self) -> Result<()> {
        self.match_token(Token::Let, "Expected 'let' keyword")?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        {
            Token::Identifier(identifier_id) => {
                self.match_token(Token::Assignment, "Expected '<-' symbol")?;

                let instruction_id = self.expression()?;

                self.cur_body
                    .as_mut()
                    .unwrap()
                    .get_mut_block(self.cur_block.unwrap())
                    .unwrap()
                    .insert_identifier(identifier_id, instruction_id);

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<InstructionId> {
        let instr_id = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected a relOp".to_string()))??
        {
            Token::RelOp(_) => {
                let instr_id2 = self.expression()?;
                Ok(self.handle_binary_op(StoredBinaryOpcode::Cmp, instr_id, instr_id2))
            }
            _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
        }
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        instr_id: InstructionId,
        instr_id2: InstructionId,
    ) -> InstructionId {
        let new_instr_id = self.instruction_counter;

        self.push_instruction(Instruction::new(
            new_instr_id,
            Operator::StoredBinaryOp(operator, instr_id, instr_id2),
            None,
        ));

        self.instruction_counter += 1;

        new_instr_id
    }

    fn expression(&mut self) -> Result<InstructionId> {
        let mut instr_id = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Add, instr_id, instr_id2);
                }
                Ok(Token::Sub) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Sub, instr_id, instr_id2);
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
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Mul, instr_id, instr_id2);
                }
                Ok(Token::Div) => {
                    self.tokens.next();
                    let instr_id2 = self.factor()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Div, instr_id, instr_id2);
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
            .ok_or_else(|| Error::SyntaxError("Factor Error".to_string()))?
        {
            Ok(Token::Identifier(id)) => {
                let instruction_id = self
                    .cur_body
                    .as_mut()
                    .unwrap()
                    .get_mut_block(self.cur_block.unwrap())
                    .unwrap()
                    .get_identifier(id)
                    .unwrap()
                    .clone();
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::Number(num)) => {
                if let Some(instruction_id) = self.const_body.get_instruction_id(*num) {
                    self.tokens.next();

                    Ok(*instruction_id)
                } else {
                    let instruction_id = self.instruction_counter;
                    self.const_body.insert(*num, instruction_id);
                    self.instruction_counter += 1;

                    self.tokens.next();

                    Ok(instruction_id)
                }
            }
            Ok(Token::LPar) => {
                self.tokens.next();
                let instr_id = self.expression()?;
                match self.tokens.next() {
                    Some(Ok(Token::RPar)) => Ok(instr_id),
                    Some(Ok(token)) => Err(Error::SyntaxError(format!(
                        "Expected ')', received {:?}",
                        token
                    ))),
                    _ => Err(Error::SyntaxError("Expected ')'.".to_string())),
                }
            }
            Ok(_) => {
                let _ = self.func_call();
                todo!()
            }
            Err(e) => Err(e.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ir::ssa::{Instruction, Operator};

    use super::*;

    #[test]
    fn branch() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            if x < 1 then
                let a <- 2;
            else
                let a <- 4;
            fi;
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        // 4
        let const_block = BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(4, Operator::Const(2), None),
                Instruction::new(5, Operator::Const(4), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
            None,
        );

        //0
        let main_block = BasicBlock::from(
            vec![
                Instruction::new(
                    2,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, 1),
                    None,
                ),
                Instruction::new(3, Operator::Bge(5, 2), None),
            ],
            HashMap::from([(14, 1)]),
            ControlFlowEdge::Fallthrough(BasicBlockId(1)),
            Some(BasicBlockId(4)),
        );

        // 1
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 1), (15, 4)]),
            ControlFlowEdge::Branch(BasicBlockId(3)),
            Some(BasicBlockId(0)),
        );

        // 2
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 1), (15, 5)]),
            ControlFlowEdge::Fallthrough(BasicBlockId(3)),
            Some(BasicBlockId(0)),
        );

        // 3
        let join_block = BasicBlock::from(
            vec![Instruction::new(6, Operator::Phi(4, 5), None)],
            HashMap::from([(14, 1), (15, 6)]),
            ControlFlowEdge::Leaf,
            Some(BasicBlockId(0)),
        );

        let main_body = Body::from(
            BasicBlockId(4),
            vec![main_block, then_block, else_block, join_block, const_block],
        );
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }

    #[test]
    fn sanity_check() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            let z <- 1 + 2 + 2 - 4;
            let y <- 3 * 2 / 1;
            let d <- z + y;
            let d <- (1 + d);
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        let const_block = BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(2, Operator::Const(2), None),
                Instruction::new(5, Operator::Const(4), None),
                Instruction::new(7, Operator::Const(3), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
            None,
        );
        let main_block = BasicBlock::from(
            vec![
                Instruction::new(
                    3,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 2),
                    None,
                ),
                Instruction::new(
                    4,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 2),
                    None,
                ),
                Instruction::new(
                    6,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Sub, 4, 5),
                    None,
                ),
                Instruction::new(
                    8,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 7, 2),
                    None,
                ),
                Instruction::new(
                    9,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 8, 1),
                    None,
                ),
                Instruction::new(
                    10,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 6, 9),
                    None,
                ),
                Instruction::new(
                    11,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 10),
                    None,
                ),
            ],
            HashMap::from([(14, 1), (15, 6), (16, 9), (17, 11)]),
            ControlFlowEdge::Leaf,
            Some(BasicBlockId(1)),
        );

        let main_body = Body::from(BasicBlockId(1), vec![main_block, const_block]);
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }
}
