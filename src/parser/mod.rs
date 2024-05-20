pub mod error;

use std::iter::Peekable;

use crate::{
    ast::{
        Assignment, Block, Computation, ExprOp, Expression, Factor, FormalParam, FuncBody,
        FuncCall, FuncDecl, IfStatement, OptionFrom, Relation, ReturnStatement, Statement, Term,
        TermOp, VarDecl, WhileStatement,
    },
    lexer::{error::TokenResult, Identifier, RelOp, Token, TokenType},
};

use self::error::{ParserError, ParserResult};

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

    fn next(&mut self) -> ParserResult<Token> {
        Ok(self
            .tokens
            .next()
            .ok_or(ParserError::UnexpectedEndOfFile)??)
    }

    fn peek(&mut self) -> ParserResult<Option<Token>> {
        Ok(self.tokens.peek().map(|token| token.clone()).transpose()?)
    }

    fn match_token(&mut self, expected: Token) -> ParserResult<()> {
        match self.next()? {
            token if token == expected => Ok(()),
            token => Err(ParserError::UnexpectedToken(
                vec![expected.get_type()],
                token,
            )),
        }
    }

    fn match_identifier(&mut self) -> ParserResult<Identifier> {
        match self.next()? {
            Token::Identifier(ident) => Ok(ident),
            token => Err(ParserError::UnexpectedToken(
                vec![TokenType::Identifier],
                token,
            )),
        }
    }

    fn match_rel_op(&mut self) -> ParserResult<RelOp> {
        match self.next()? {
            Token::RelOp(relop) => Ok(relop),
            token => Err(ParserError::UnexpectedToken(vec![TokenType::RelOp], token)),
        }
    }

    fn is_expression(&mut self) -> ParserResult<bool> {
        Ok(matches!(
            self.peek()?,
            Some(Token::Identifier(_) | Token::Number(_) | Token::LPar | Token::Call)
        ))
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
        self.match_token(Token::Call)?;
        let ident = self.match_identifier()?;

        let mut args = Vec::new();

        if let Some(Token::LPar) = self.peek()? {
            self.tokens.next(); // consume the left parenthesis

            if self.is_expression()? {
                args.push(self.expression()?);

                while let Some(Token::Comma) = self.peek()? {
                    self.tokens.next(); // consume the comma
                    args.push(self.expression()?);
                }
            }

            self.match_token(Token::RPar)?;
        }

        Ok(FuncCall { ident, args })
    }

    fn assignment(&mut self) -> ParserResult<Assignment> {
        todo!()
    }

    fn relation(&mut self) -> ParserResult<Relation> {
        let lhs = self.expression()?;
        let rel_op = self.match_rel_op()?;
        let rhs = self.expression()?;

        Ok(Relation { lhs, rel_op, rhs })
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        let term = self.term()?;
        let mut ops = Vec::new();

        while let Some(op) = self.peek()? {
            match ExprOp::option_from(op) {
                Some(expr_op) => {
                    self.tokens.next(); // consume the operator
                    let next_term = self.term()?;
                    ops.push((expr_op, next_term));
                }
                None => {
                    break;
                }
            }
        }

        Ok(Expression { term, ops })
    }

    fn term(&mut self) -> ParserResult<Term> {
        let factor = self.factor()?;
        let mut ops = Vec::new();

        while let Some(op) = self.peek()? {
            match TermOp::option_from(op) {
                Some(term_op) => {
                    self.tokens.next(); // consume the operator
                    let next_factor = self.factor()?;
                    ops.push((term_op, next_factor));
                }
                None => {
                    break;
                }
            }
        }

        Ok(Term { factor, ops })
    }

    fn factor(&mut self) -> ParserResult<Factor> {
        match self.peek()?.ok_or(ParserError::UnexpectedEndOfFile)? {
            Token::Identifier(ident) => {
                self.tokens.next(); // consume the identifier
                Ok(Factor::Ident(ident))
            }
            Token::Number(num) => {
                self.tokens.next(); // consume the number
                Ok(Factor::Number(num))
            }
            Token::LPar => {
                self.tokens.next(); // consume the left parenthesis
                let expr = self.expression()?;
                match self.next()? {
                    Token::RPar => Ok(Factor::Expression(Box::new(expr))),
                    token => Err(ParserError::UnexpectedToken(vec![token.get_type()], token)),
                }
            }
            Token::Call => Ok(Factor::FuncCall(self.func_call()?)),
            token => Err(ParserError::UnexpectedToken(vec![token.get_type()], token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_factor() {
        let input = [Token::Identifier(0)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(parser.factor().unwrap(), Factor::Ident(0));

        let input = [Token::Number(42)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(parser.factor().unwrap(), Factor::Number(42));

        let input = [Token::LPar, Token::Number(42), Token::RPar];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.factor().unwrap(),
            Factor::Expression(Box::new(Expression {
                term: Term {
                    factor: Factor::Number(42),
                    ops: Vec::new()
                },
                ops: Vec::new()
            }))
        );

        let input = [Token::Call, Token::Identifier(0)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.factor().unwrap(),
            Factor::FuncCall(FuncCall {
                ident: 0,
                args: Vec::new()
            })
        );
    }
}
