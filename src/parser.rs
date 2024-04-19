use std::{cell::RefCell, iter::Peekable, rc::Rc};

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, Body, ControlFlowEdge},
        ssa::{Instruction, Operator, StoredBinaryOpcode},
        ConstBody, InstructionId, IrStore,
    },
    lexer::{Token, Tokenizer},
};

macro_rules! todo_with_error {
    () => {
        eprintln!("Error: Functionality not implemented yet")
    };
}

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    instruction_counter: u32,
    store: IrStore<'a>,
    const_body: ConstBody<'a>,
    cur_body: Option<Body<'a>>,
    cur_block: Option<Rc<RefCell<BasicBlock<'a>>>>,
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

    fn computation(&mut self) -> Result<()> {
        self.match_token(Token::Main, "Expected 'main' keyword")?;

        if let Some(Ok(Token::Var)) = self.tokens.peek() {
            todo_with_error!()
        }

        while let Some(Ok(Token::Call)) = self.tokens.peek() {
            todo_with_error!()
        }

        self.match_token(Token::LBrack, "Expected '{' symbol")?;

        self.stat_sequence()?;

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
        let mut cur_body = Body::new();
        let cur_block = Rc::new(RefCell::new(BasicBlock::new(
            Vec::new(),
            ControlFlowEdge::Leaf,
        )));

        // cur_body.insert_block(Rc::clone(&cur_block));
        cur_body.update_root(Rc::clone(&cur_block));

        self.cur_body = Some(cur_body);
        self.cur_block = Some(cur_block);

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

        // Create Const Block
        let const_block = Rc::new(RefCell::new(BasicBlock::new(
            self.const_body.get_instructions().clone(),
            ControlFlowEdge::Fallthrough(Rc::clone(self.cur_block.as_ref().unwrap())),
        )));

        // Update Dominator
        self.cur_block
            .as_ref()
            .unwrap()
            .borrow_mut()
            .update_dominator(Rc::downgrade(&const_block));

        self.cur_body
            .as_mut()
            .unwrap()
            .update_root(Rc::clone(&const_block));

        self.store
            .insert("main".to_string(), self.cur_body.take().unwrap());

        self.cur_block = None;
        self.cur_body = None;

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

    fn if_statement(&mut self) -> Result<()> {
        // self.match_token(Token::If, "Expected 'if' keyword")?;

        // let _ = self.relation()?;

        // self.match_token(Token::Then, "Expected 'then' keyword")?;

        // let _ = self.stat_sequence()?;

        // if *self
        //     .tokens
        //     .peek()
        //     .ok_or_else(|| Error::SyntaxError("Expected 'fi' keyword".into()))?
        //     == Ok(Token::Else)
        // {
        //     self.tokens.next();
        //     let _ = self.stat_sequence();
        // }

        // self.match_token(Token::Fi, "expected 'fi' keyword".into())?;

        // Ok(())
        todo!()
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

                self.cur_block
                    .as_ref()
                    .unwrap()
                    .borrow_mut()
                    .insert_identifier(identifier_id, instruction_id);

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<()> {
        // let _ = self.expression()?;

        // match self
        //     .tokens
        //     .next()
        //     .ok_or_else(|| Error::SyntaxError("Expected a relOp".to_string()))??
        // {
        //     Token::RelOp(_) => {
        //         let _ = self.expression()?;

        //         todo!()
        //     }
        //     _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
        // }

        todo!()
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        instr_id: InstructionId,
        instr_id2: InstructionId,
    ) -> InstructionId {
        let new_instr_id = self.instruction_counter;

        self.cur_block
            .as_ref()
            .unwrap()
            .borrow_mut()
            .insert_instruction(Instruction::new(
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
            Ok(Token::Identifier(id)) => todo!(),
            Ok(Token::Number(num)) => {
                if let Some(instruction_id) = self.const_body.get_instruction_id(*num) {
                    self.tokens.next();

                    return Ok(*instruction_id);
                } else {
                    let instruction_id = self.instruction_counter;
                    self.const_body.insert(*num, instruction_id);
                    self.instruction_counter += 1;

                    self.tokens.next();

                    Ok(instruction_id)
                }
            }
            Ok(Token::LPar) => todo!(),
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
    fn assignment() {
        let tokens = Tokenizer::new("main {let x <- 1}.");
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        let const_block = Rc::new(RefCell::new(BasicBlock::from(
            vec![Instruction::new(1, Operator::Const(1), None)],
            HashMap::new(),
            ControlFlowEdge::Leaf,
            None,
        )));
        let main_block = Rc::new(RefCell::new(BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 1)]),
            ControlFlowEdge::Leaf,
            Some(Rc::downgrade(&const_block)),
        )));

        const_block
            .borrow_mut()
            .update_edge(ControlFlowEdge::Fallthrough(Rc::clone(&main_block)));

        let main_body = Body::from(const_block);
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }

    #[test]
    fn assignment_with_arithmetic() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            let z <- 1 + 2 + 2 - 4;
            let y <- 3 * 2 / 1;
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        let const_block = Rc::new(RefCell::new(BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(2, Operator::Const(2), None),
                Instruction::new(5, Operator::Const(4), None),
                Instruction::new(7, Operator::Const(3), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Leaf,
            None,
        )));
        let main_block = Rc::new(RefCell::new(BasicBlock::from(
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
            ],
            HashMap::from([(15, 6), (14, 1), (16, 9)]),
            ControlFlowEdge::Leaf,
            Some(Rc::downgrade(&const_block)),
        )));

        const_block
            .borrow_mut()
            .update_edge(ControlFlowEdge::Fallthrough(Rc::clone(&main_block)));

        let main_body = Body::from(const_block);
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }
}
