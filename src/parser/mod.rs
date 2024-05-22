pub mod error;

use std::iter::Peekable;

use crate::{
    ast::{
        Assignment, Block, Computation, DefinedFuncCall, ExprOp, Expression, Factor, FormalParam,
        FuncBody, FuncCall, FuncDecl, IfStatement, PredefinedFuncCall, Relation, ReturnStatement,
        Statement, Term, TermOp, VarDecl, WhileStatement,
    },
    lexer::{error::TokenResult, Identifier, Token, TokenType},
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
        Ok(self.tokens.peek().cloned().transpose()?)
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

    fn is_expression(&mut self) -> ParserResult<bool> {
        Ok(matches!(
            self.peek()?,
            Some(Token::Identifier(_) | Token::Number(_) | Token::LPar | Token::Call)
        ))
    }

    fn is_statement(&mut self) -> ParserResult<bool> {
        Ok(matches!(
            self.peek()?,
            Some(Token::Let | Token::Call | Token::If | Token::While | Token::Return)
        ))
    }

    fn computation(&mut self) -> ParserResult<Computation> {
        self.match_token(Token::Main)?;
        let vars = if self.peek()? == Some(Token::Var) {
            Some(self.var_decl()?)
        } else {
            None
        };

        let mut funcs = Vec::new();
        while let Some(Token::Void | Token::Function) = self.peek()? {
            funcs.push(self.func_decl()?);
        }

        self.match_token(Token::LBrack)?;
        let body = self.stat_sequence()?;
        self.match_token(Token::RBrack)?;

        Ok(Computation { vars, funcs, body })
    }

    fn func_body(&mut self) -> ParserResult<FuncBody> {
        let vars = if self.peek()? == Some(Token::Var) {
            Some(self.var_decl()?)
        } else {
            None
        };

        self.match_token(Token::LBrack)?;
        let body = if self.is_statement()? {
            self.stat_sequence()?
        } else {
            Block {
                statements: Vec::new(),
            }
        };
        self.match_token(Token::RBrack)?;

        Ok(FuncBody { vars, body })
    }

    fn formal_param(&mut self) -> ParserResult<FormalParam> {
        self.match_token(Token::LPar)?;
        let mut params = Vec::new();

        if let Some(Token::Identifier(ident)) = self.peek()? {
            params.push(ident);
            self.tokens.next(); // consume the identifier

            while self.peek()? == Some(Token::Comma) {
                self.tokens.next(); // consume the comma
                params.push(self.match_identifier()?);
            }
        }

        self.match_token(Token::RPar)?;

        Ok(FormalParam { params })
    }

    fn func_decl(&mut self) -> ParserResult<FuncDecl> {
        let is_void = if self.peek()? == Some(Token::Void) {
            self.tokens.next(); // consume the void token
            true
        } else {
            false
        };

        self.match_token(Token::Function)?;
        let ident = self.match_identifier()?;
        let params = self.formal_param()?;
        self.match_token(Token::Semicolon)?;
        let body = self.func_body()?;
        self.match_token(Token::Semicolon)?;

        Ok(FuncDecl {
            is_void,
            ident,
            params,
            body,
            name: ident.to_string(), // @TODO: Call tokenizer to get the name
        })
    }

    fn var_decl(&mut self) -> ParserResult<VarDecl> {
        self.match_token(Token::Var)?;
        let mut vars = vec![self.match_identifier()?];

        while self.peek()? == Some(Token::Comma) {
            self.tokens.next(); // consume the comma
            vars.push(self.match_identifier()?);
        }

        self.match_token(Token::Semicolon)?;

        Ok(VarDecl { vars })
    }

    fn stat_sequence(&mut self) -> ParserResult<Block> {
        let mut statements = vec![self.statement()?];

        while self.peek()? == Some(Token::Semicolon) {
            self.tokens.next(); // consume the semicolon

            if self.is_statement()? {
                statements.push(self.statement()?);
            } else {
                break;
            }
        }

        Ok(Block { statements })
    }

    fn statement(&mut self) -> ParserResult<Statement> {
        match self.peek()?.ok_or(ParserError::UnexpectedEndOfFile)? {
            Token::Let => Ok(Statement::Assignment(self.assignment()?)),
            Token::Call => Ok(Statement::FuncCall(self.func_call()?)),
            Token::If => Ok(Statement::IfStatement(self.if_statement()?)),
            Token::While => Ok(Statement::WhileStatement(self.while_statement()?)),
            Token::Return => Ok(Statement::ReturnStatement(self.return_statement()?)),
            token => Err(ParserError::UnexpectedToken(
                vec![
                    TokenType::Let,
                    TokenType::Call,
                    TokenType::If,
                    TokenType::While,
                    TokenType::Return,
                ],
                token,
            )),
        }
    }

    fn return_statement(&mut self) -> ParserResult<ReturnStatement> {
        self.match_token(Token::Return)?;
        let expr = if self.is_expression()? {
            Some(self.expression()?)
        } else {
            None
        };

        // Consume Unreachable Tokens
        while let Some(_) = self
            .tokens
            .next_if(|t| !matches!(t, Ok(Token::Fi | Token::Od | Token::Else | Token::RBrack)))
        {
        }

        Ok(ReturnStatement { expr })
    }

    fn while_statement(&mut self) -> ParserResult<WhileStatement> {
        self.match_token(Token::While)?;
        let rel = self.relation()?;
        self.match_token(Token::Do)?;
        let block = self.stat_sequence()?;
        self.match_token(Token::Od)?;

        Ok(WhileStatement { rel, block })
    }

    fn if_statement(&mut self) -> ParserResult<IfStatement> {
        self.match_token(Token::If)?;
        let rel = self.relation()?;
        self.match_token(Token::Then)?;
        let then_block = self.stat_sequence()?;
        let else_block = if self.peek()? == Some(Token::Else) {
            self.tokens.next(); // consume the else token
            Some(self.stat_sequence()?)
        } else {
            None
        };
        self.match_token(Token::Fi)?;

        // Consume Unreachable Tokens
        let then_returns = then_block
            .statements
            .last()
            .map(|s| matches!(s, Statement::ReturnStatement(_)))
            .unwrap_or(false);
        let else_returns = if let Some(else_block) = &else_block {
            else_block
                .statements
                .last()
                .map(|s| matches!(s, Statement::ReturnStatement(_)))
                .unwrap_or(false)
        } else {
            false
        };

        if then_returns && else_returns {
            while let Some(_) = self
                .tokens
                .next_if(|t| !matches!(t, Ok(Token::Fi | Token::Od | Token::Else | Token::RBrack)))
            {
            }
        }

        Ok(IfStatement {
            rel,
            then_block,
            else_block,
        })
    }

    fn collect_args(&mut self) -> ParserResult<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek()? == Some(Token::LPar) {
            self.tokens.next(); // consume the left parenthesis

            if self.is_expression()? {
                args.push(self.expression()?);

                while self.peek()? == Some(Token::Comma) {
                    self.tokens.next(); // consume the comma
                    args.push(self.expression()?);
                }
            }

            self.match_token(Token::RPar)?;
        }

        Ok(args)
    }

    fn func_call(&mut self) -> ParserResult<FuncCall> {
        self.match_token(Token::Call)?;

        match self.next()? {
            Token::Identifier(ident) => {
                let args = self.collect_args()?;
                Ok(FuncCall::Defined(DefinedFuncCall {
                    name: ident.to_string(), // @TODO: Call tokenizer to get the name
                    ident,
                    args,
                }))
            }
            Token::PredefinedFunction(func) => {
                let args = self.collect_args()?;
                Ok(FuncCall::Predefined(PredefinedFuncCall { func, args }))
            }
            token => Err(ParserError::UnexpectedToken(
                vec![TokenType::Identifier, TokenType::PredefinedFunction],
                token,
            )),
        }
    }

    fn assignment(&mut self) -> ParserResult<Assignment> {
        self.match_token(Token::Let)?;
        let ident = self.match_identifier()?;
        self.match_token(Token::Assignment)?;
        let expr = self.expression()?;

        Ok(Assignment { ident, expr })
    }

    fn relation(&mut self) -> ParserResult<Relation> {
        let lhs = self.expression()?;
        let rel_op = match self.next()? {
            Token::RelOp(relop) => relop,
            token => return Err(ParserError::UnexpectedToken(vec![TokenType::RelOp], token)),
        };
        let rhs = self.expression()?;

        Ok(Relation { lhs, rel_op, rhs })
    }

    fn expression(&mut self) -> ParserResult<Expression> {
        let term = self.term()?;
        let mut ops = Vec::new();

        while let Some(op) = self.peek()? {
            match ExprOp::try_from(op) {
                Ok(expr_op) => {
                    self.tokens.next(); // consume the operator
                    let next_term = self.term()?;
                    ops.push((expr_op, next_term));
                }
                Err(_) => {
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
            match TermOp::try_from(op) {
                Ok(term_op) => {
                    self.tokens.next(); // consume the operator
                    let next_factor = self.factor()?;
                    ops.push((term_op, next_factor));
                }
                Err(_) => {
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
    use crate::lexer::{PredefinedFunction, RelOp};

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
            Factor::FuncCall(FuncCall::Defined(DefinedFuncCall {
                name: "0".to_string(),
                ident: 0,
                args: Vec::new()
            }))
        );
    }

    #[test]
    fn basic_term() {
        let input = [Token::Number(1), Token::Mul, Token::Number(22)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.term().unwrap(),
            Term {
                factor: Factor::Number(1),
                ops: vec![(TermOp::Mul, Factor::Number(22))]
            }
        );

        let input = [Token::Number(22), Token::Div, Token::Number(1)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.term().unwrap(),
            Term {
                factor: Factor::Number(22),
                ops: vec![(TermOp::Div, Factor::Number(1))]
            }
        );

        let input = [
            Token::Number(1),
            Token::Mul,
            Token::Number(22),
            Token::Div,
            Token::Number(333),
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.term().unwrap(),
            Term {
                factor: Factor::Number(1),
                ops: vec![
                    (TermOp::Mul, Factor::Number(22)),
                    (TermOp::Div, Factor::Number(333))
                ]
            }
        );
    }

    #[test]
    fn basic_expression() {
        let input = [Token::Number(1), Token::Add, Token::Number(22)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.expression().unwrap(),
            Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: Vec::new()
                },
                ops: vec![(
                    ExprOp::Add,
                    Term {
                        factor: Factor::Number(22),
                        ops: Vec::new()
                    }
                )]
            }
        );

        let input = [Token::Number(22), Token::Sub, Token::Number(1)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.expression().unwrap(),
            Expression {
                term: Term {
                    factor: Factor::Number(22),
                    ops: Vec::new()
                },
                ops: vec![(
                    ExprOp::Sub,
                    Term {
                        factor: Factor::Number(1),
                        ops: Vec::new()
                    }
                )]
            }
        );

        let input = [
            Token::Number(1),
            Token::Add,
            Token::Number(22),
            Token::Sub,
            Token::Number(333),
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.expression().unwrap(),
            Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: Vec::new()
                },
                ops: vec![
                    (
                        ExprOp::Add,
                        Term {
                            factor: Factor::Number(22),
                            ops: Vec::new()
                        }
                    ),
                    (
                        ExprOp::Sub,
                        Term {
                            factor: Factor::Number(333),
                            ops: Vec::new()
                        }
                    )
                ]
            }
        );
    }

    #[test]
    fn basic_relation() {
        let input = [Token::Number(1), Token::RelOp(RelOp::Eq), Token::Number(22)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.relation().unwrap(),
            Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                },
                rel_op: RelOp::Eq,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(22),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }
            }
        );

        let input = [Token::Number(22), Token::RelOp(RelOp::Ne), Token::Number(1)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.relation().unwrap(),
            Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(22),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                },
                rel_op: RelOp::Ne,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }
            }
        );

        let input = [Token::Number(1), Token::RelOp(RelOp::Lt), Token::Number(22)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.relation().unwrap(),
            Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                },
                rel_op: RelOp::Lt,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(22),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }
            }
        );
    }

    #[test]
    fn basic_assignment() {
        let input = [
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(42),
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.assignment().unwrap(),
            Assignment {
                ident: 0,
                expr: Expression {
                    term: Term {
                        factor: Factor::Number(42),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }
            }
        );
    }

    #[test]
    fn basic_func_call() {
        let input = [Token::Call, Token::Identifier(0)];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Defined(DefinedFuncCall {
                name: "0".to_string(),
                ident: 0,
                args: Vec::new()
            })
        );

        let input = [Token::Call, Token::Identifier(0), Token::LPar, Token::RPar];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Defined(DefinedFuncCall {
                name: "0".to_string(),
                ident: 0,
                args: Vec::new()
            })
        );

        let input = [
            Token::Call,
            Token::Identifier(0),
            Token::LPar,
            Token::Number(42),
            Token::RPar,
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Defined(DefinedFuncCall {
                name: "0".to_string(),
                ident: 0,
                args: vec![Expression {
                    term: Term {
                        factor: Factor::Number(42),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }]
            })
        );

        let input = [
            Token::Call,
            Token::Identifier(0),
            Token::LPar,
            Token::Number(42),
            Token::Comma,
            Token::Number(22),
            Token::RPar,
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Defined(DefinedFuncCall {
                name: "0".to_string(),
                ident: 0,
                args: vec![
                    Expression {
                        term: Term {
                            factor: Factor::Number(42),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    },
                    Expression {
                        term: Term {
                            factor: Factor::Number(22),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    }
                ]
            })
        );
    }

    #[test]
    fn basic_predefined_func_call() {
        let input = [
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Predefined(PredefinedFuncCall {
                func: PredefinedFunction::InputNum,
                args: Vec::new()
            })
        );

        let input = [
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Number(42),
            Token::RPar,
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Predefined(PredefinedFuncCall {
                func: PredefinedFunction::OutputNum,
                args: vec![Expression {
                    term: Term {
                        factor: Factor::Number(42),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                }]
            })
        );

        let input = [
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Number(42),
            Token::Comma,
            Token::Number(22),
            Token::RPar,
        ];
        let mut parser = Parser::new(input.into_iter().map(Ok));

        assert_eq!(
            parser.func_call().unwrap(),
            FuncCall::Predefined(PredefinedFuncCall {
                func: PredefinedFunction::OutputNum,
                args: vec![
                    Expression {
                        term: Term {
                            factor: Factor::Number(42),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    },
                    Expression {
                        term: Term {
                            factor: Factor::Number(22),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    }
                ]
            })
        );
    }

    #[test]
    fn basic_if_statement() {
        let tokens = [
            Token::If,
            Token::Number(1),
            Token::RelOp(RelOp::Eq),
            Token::Number(2),
            Token::Then,
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(42),
            Token::Else,
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(22),
            Token::Semicolon,
            Token::Fi,
        ];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.if_statement().unwrap(),
            IfStatement {
                rel: Relation {
                    lhs: Expression {
                        term: Term {
                            factor: Factor::Number(1),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    },
                    rel_op: RelOp::Eq,
                    rhs: Expression {
                        term: Term {
                            factor: Factor::Number(2),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    }
                },
                then_block: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(42),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    })],
                },
                else_block: Some(Block {
                    statements: vec![Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(22),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    })],
                })
            }
        );
    }

    #[test]
    fn basic_if_statement_without_else() {
        let tokens = [
            Token::If,
            Token::Number(1),
            Token::RelOp(RelOp::Eq),
            Token::Number(2),
            Token::Then,
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(42),
            Token::Semicolon,
            Token::Fi,
        ];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.if_statement().unwrap(),
            IfStatement {
                rel: Relation {
                    lhs: Expression {
                        term: Term {
                            factor: Factor::Number(1),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    },
                    rel_op: RelOp::Eq,
                    rhs: Expression {
                        term: Term {
                            factor: Factor::Number(2),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    }
                },
                then_block: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(42),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    })],
                },
                else_block: None
            }
        );
    }

    #[test]
    fn basic_while_statement() {
        let tokens = [
            Token::While,
            Token::Number(1),
            Token::RelOp(RelOp::Eq),
            Token::Number(2),
            Token::Do,
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(42),
            Token::Od,
        ];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.while_statement().unwrap(),
            WhileStatement {
                rel: Relation {
                    lhs: Expression {
                        term: Term {
                            factor: Factor::Number(1),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    },
                    rel_op: RelOp::Eq,
                    rhs: Expression {
                        term: Term {
                            factor: Factor::Number(2),
                            ops: Vec::new()
                        },
                        ops: Vec::new()
                    }
                },
                block: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(42),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    })],
                }
            }
        );
    }

    #[test]
    fn basic_return_statement() {
        let tokens = [Token::Return, Token::Number(42)];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.return_statement().unwrap(),
            ReturnStatement {
                expr: Some(Expression {
                    term: Term {
                        factor: Factor::Number(42),
                        ops: Vec::new()
                    },
                    ops: Vec::new()
                })
            }
        );

        let tokens = [Token::Return];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.return_statement().unwrap(),
            ReturnStatement { expr: None }
        );
    }

    #[test]
    fn basic_stat_sequence() {
        let tokens = [
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(42),
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.stat_sequence().unwrap(),
            Block {
                statements: vec![
                    Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(42),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    }),
                    Statement::FuncCall(FuncCall::Predefined(PredefinedFuncCall {
                        func: PredefinedFunction::InputNum,
                        args: Vec::new()
                    }))
                ]
            }
        );
    }

    #[test]
    fn unreachable_tokens() {
        let tokens = [
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(22),
            Token::Semicolon,
            Token::Return,
            Token::Identifier(0),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(42),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.stat_sequence().unwrap(),
            Block {
                statements: vec![
                    Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(22),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    }),
                    Statement::ReturnStatement(ReturnStatement {
                        expr: Some(Expression {
                            term: Term {
                                factor: Factor::Ident(0),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        })
                    }),
                ]
            }
        );
    }

    #[test]
    fn unreachable_token_no_return_val() {
        let tokens = [
            Token::Let,
            Token::Identifier(0),
            Token::Assignment,
            Token::Number(22),
            Token::Semicolon,
            Token::Return,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(42),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.stat_sequence().unwrap(),
            Block {
                statements: vec![
                    Statement::Assignment(Assignment {
                        ident: 0,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(22),
                                ops: Vec::new()
                            },
                            ops: Vec::new()
                        }
                    }),
                    Statement::ReturnStatement(ReturnStatement { expr: None }),
                ]
            }
        );
    }

    #[test]
    fn complete_computation() {
        let tokens = [
            Token::Main,
            Token::Var,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Function,
            Token::Identifier(2),
            Token::LPar,
            Token::Identifier(3),
            Token::RPar,
            Token::Semicolon,
            Token::LBrack,
            Token::If,
            Token::Identifier(3),
            Token::RelOp(RelOp::Le),
            Token::Number(1),
            Token::Then,
            Token::Return,
            Token::Identifier(3),
            Token::Fi,
            Token::Semicolon,
            Token::Return,
            Token::Call,
            Token::Identifier(2),
            Token::LPar,
            Token::Identifier(3),
            Token::Sub,
            Token::Number(1),
            Token::RPar,
            Token::Add,
            Token::Call,
            Token::Identifier(2),
            Token::LPar,
            Token::Identifier(3),
            Token::Sub,
            Token::Number(2),
            Token::RPar,
            Token::RBrack,
            Token::Semicolon,
            Token::LBrack,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::Identifier(2),
            Token::LPar,
            Token::Identifier(1),
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
            Token::RBrack,
            Token::Period,
        ];

        let mut parser = Parser::new(tokens.into_iter().map(Ok));

        assert_eq!(
            parser.computation().unwrap(),
            Computation {
                vars: Some(VarDecl { vars: vec![1] }),
                funcs: vec![FuncDecl {
                    name: "2".to_string(),
                    is_void: false,
                    ident: 2,
                    params: FormalParam { params: vec![3] },
                    body: FuncBody {
                        vars: None,
                        body: Block {
                            statements: vec![
                                Statement::IfStatement(IfStatement {
                                    rel: Relation {
                                        lhs: Expression {
                                            term: Term {
                                                factor: Factor::Ident(3),
                                                ops: vec![]
                                            },
                                            ops: vec![]
                                        },
                                        rel_op: RelOp::Le,
                                        rhs: Expression {
                                            term: Term {
                                                factor: Factor::Number(1),
                                                ops: vec![]
                                            },
                                            ops: vec![]
                                        }
                                    },
                                    then_block: Block {
                                        statements: vec![Statement::ReturnStatement(
                                            ReturnStatement {
                                                expr: Some(Expression {
                                                    term: Term {
                                                        factor: Factor::Ident(3),
                                                        ops: vec![]
                                                    },
                                                    ops: vec![]
                                                })
                                            }
                                        )]
                                    },
                                    else_block: None
                                }),
                                Statement::ReturnStatement(ReturnStatement {
                                    expr: Some(Expression {
                                        term: Term {
                                            factor: Factor::FuncCall(FuncCall::Defined(
                                                DefinedFuncCall {
                                                    name: "2".to_string(),
                                                    ident: 2,
                                                    args: vec![Expression {
                                                        term: Term {
                                                            factor: Factor::Ident(3),
                                                            ops: vec![]
                                                        },
                                                        ops: vec![(
                                                            ExprOp::Sub,
                                                            Term {
                                                                factor: Factor::Number(1),
                                                                ops: vec![]
                                                            }
                                                        )]
                                                    }]
                                                }
                                            )),
                                            ops: vec![]
                                        },
                                        ops: vec![(
                                            ExprOp::Add,
                                            Term {
                                                factor: Factor::FuncCall(FuncCall::Defined(
                                                    DefinedFuncCall {
                                                        name: "2".to_string(),
                                                        ident: 2,
                                                        args: vec![Expression {
                                                            term: Term {
                                                                factor: Factor::Ident(3),
                                                                ops: vec![]
                                                            },
                                                            ops: vec![(
                                                                ExprOp::Sub,
                                                                Term {
                                                                    factor: Factor::Number(2),
                                                                    ops: vec![]
                                                                }
                                                            )]
                                                        }]
                                                    }
                                                )),
                                                ops: vec![]
                                            }
                                        )]
                                    })
                                })
                            ]
                        }
                    }
                }],
                body: Block {
                    statements: vec![
                        Statement::Assignment(Assignment {
                            ident: 1,
                            expr: Expression {
                                term: Term {
                                    factor: Factor::FuncCall(FuncCall::Predefined(
                                        PredefinedFuncCall {
                                            func: PredefinedFunction::InputNum,
                                            args: vec![]
                                        }
                                    )),
                                    ops: vec![]
                                },
                                ops: vec![]
                            }
                        }),
                        Statement::Assignment(Assignment {
                            ident: 1,
                            expr: Expression {
                                term: Term {
                                    factor: Factor::FuncCall(FuncCall::Defined(DefinedFuncCall {
                                        name: "2".to_string(),
                                        ident: 2,
                                        args: vec![Expression {
                                            term: Term {
                                                factor: Factor::Ident(1),
                                                ops: vec![]
                                            },
                                            ops: vec![]
                                        }]
                                    })),
                                    ops: vec![]
                                },
                                ops: vec![]
                            }
                        }),
                        Statement::FuncCall(FuncCall::Predefined(PredefinedFuncCall {
                            func: PredefinedFunction::OutputNum,
                            args: vec![Expression {
                                term: Term {
                                    factor: Factor::Ident(1),
                                    ops: vec![]
                                },
                                ops: vec![]
                            }]
                        })),
                        Statement::FuncCall(FuncCall::Predefined(PredefinedFuncCall {
                            func: PredefinedFunction::OutputNewLine,
                            args: vec![]
                        }))
                    ]
                }
            }
        )
    }
}
