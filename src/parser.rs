use std::iter::Peekable;

use crate::{
    error::{Error, Result},
    lexer::{Token, Tokenizer},
};

pub enum IntermediateResult {
    Const(f64),
    Var(u32),
    Reg(u32),
}

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    instructions: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            instructions: Vec::new(),
        }
    }

    // fn load(&mut self, res: &mut IntermediateResult) {
    //     match res {
    //         IntermediateResult::Var()
    //     }
    // }

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
        todo!()
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
        todo!()
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
        self.match_token(Token::Return, "Expected 'return' keyword")?;

        match self.expression() {
            Ok(_) => todo!(),
            Err(_) => todo!(),
        }
    }

    fn while_statement(&mut self) -> Result<()> {
        self.match_token(Token::While, "Expected 'while' keyword")?;

        let _ = self.relation()?;

        self.match_token(Token::Do, "Expected 'do' keyword")?;

        let _ = self.stat_sequence()?;

        self.match_token(Token::Od, "Expected 'od' keyword")?;

        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.match_token(Token::If, "Expected 'if' keyword")?;

        let _ = self.relation()?;

        self.match_token(Token::Then, "Expected 'then' keyword")?;

        let _ = self.stat_sequence()?;

        if *self
            .tokens
            .peek()
            .ok_or_else(|| Error::SyntaxError("Expected 'fi' keyword".into()))?
            == Ok(Token::Else)
        {
            self.tokens.next();
            let _ = self.stat_sequence();
        }

        self.match_token(Token::Fi, "expected 'fi' keyword".into())?;

        Ok(())
    }

    fn func_call(&mut self) -> Result<()> {
        self.match_token(Token::Call, "Expected 'call' keyword")?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        {
            Token::Identifier(id) => {
                // @TODO: Functions without parameters can be called with or without parentheses
                self.match_token(Token::LPar, "Expected '(' symbol")?;

                let _ = self.expression()?;

                while let Some(token) = self.tokens.peek() {
                    match token {
                        Ok(Token::Comma) => {
                            let _ = self.expression()?;
                        }
                        _ => break,
                    }
                }

                todo!()
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn assignment(&mut self) -> Result<()> {
        self.match_token(Token::Let, "Expected 'let' keyword")?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        {
            Token::Identifier(id) => {
                self.match_token(Token::Assignment, "Expected '<-' symbol")?;

                let _ = self.expression()?;

                todo!()
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<()> {
        let _ = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::SyntaxError("Expected a relOp".to_string()))??
        {
            Token::RelOp(_) => {
                let _ = self.expression()?;

                todo!()
            }
            _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
        }
    }

    fn expression(&mut self) -> Result<()> {
        let _ = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => todo!(),
                Ok(Token::Sub) => todo!(),
                _ => break,
            }
        }

        todo!()
    }

    fn term(&mut self) -> Result<()> {
        let _ = self.factor()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Mul) => todo!(),
                Ok(Token::Div) => todo!(),
                _ => break,
            }
        }

        todo!()
    }

    fn factor(&mut self) -> Result<()> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::SyntaxError("Factor Error".to_string()))?
        {
            Ok(Token::Identifier(id)) => todo!(),
            Ok(Token::Number(num)) => todo!(),
            Ok(Token::LPar) => todo!(),
            Ok(_) => self.func_call(),
            Err(e) => Err(e.clone()),
        }
    }
}
