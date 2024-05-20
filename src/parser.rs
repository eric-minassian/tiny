use std::iter::Peekable;

use crate::{
    ast::{
        Assignment, Block, Computation, Expression, Factor, FormalParam, FuncBody, FuncCall,
        FuncDecl, IfStatement, Relation, ReturnStatement, Statement, Term, VarDecl, WhileStatement,
    },
    lexer::error::TokenResult,
};

pub type ParserResult<T> = Result<T, ()>;

pub struct Parser<T>
where
    T: Iterator<Item = TokenResult>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = TokenResult>,
{
    fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(tokens: T) -> ParserResult<Computation> {
        Self::new(tokens).computation()
    }

    fn computation(&mut self) -> ParserResult<Computation> {
        todo!()
    }

    fn func_body(&mut self) -> ParserResult<FuncBody> {
        todo!()
    }

    fn formal_param(&mut self) -> ParserResult<FormalParam> {
        todo!()
    }

    fn func_decl(&mut self) -> ParserResult<FuncDecl> {
        todo!()
    }

    fn var_decl(&mut self) -> ParserResult<VarDecl> {
        todo!()
    }

    fn stat_sequence(&mut self) -> ParserResult<Block> {
        todo!()
    }

    fn statement(&mut self) -> ParserResult<Statement> {
        todo!()
    }

    fn return_statement(&mut self) -> ParserResult<ReturnStatement> {
        todo!()
    }

    fn while_statement(&mut self) -> ParserResult<WhileStatement> {
        todo!()
    }

    fn if_statement(&mut self) -> ParserResult<IfStatement> {
        todo!()
    }

    fn func_call(&mut self) -> ParserResult<FuncCall> {
        todo!()
    }

    fn assignment(&mut self) -> ParserResult<Assignment> {
        todo!()
    }

    fn relation(&mut self) -> ParserResult<Relation> {
        todo!()
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        todo!()
    }

    fn term(&mut self) -> ParserResult<Term> {
        todo!()
    }

    fn factor(&mut self) -> ParserResult<Factor> {
        todo!()
    }
}
