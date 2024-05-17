mod body;

use std::iter::Peekable;

use crate::{
    error::{Error, Result},
    ir::{block::Body, IrStore},
    lexer::{IdentifierId, Token},
};

use self::body::BodyParser;

pub fn match_token<'a, T>(tokens: &mut Peekable<T>, expected: Token) -> Result<()>
where
    T: Iterator<Item = Result<Token>>,
{
    let next_token = tokens.next().ok_or(Error::UnexpectedEndOfFile)??;

    if next_token != expected {
        return Err(Error::UnexpectedToken {
            expected,
            found: next_token,
        });
    }

    Ok(())
}

pub struct Parser<T>
where
    T: Iterator<Item = Result<Token>> + Clone,
{
    tokens: Peekable<T>,
    store: IrStore,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Result<Token>> + Clone,
{
    pub fn parse(tokens: T) -> Result<IrStore> {
        let mut parser = Self::new(tokens);
        parser.computation()?;
        Ok(parser.store)
    }

    fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
            store: IrStore::new(),
        }
    }

    pub fn from(tokens: Peekable<T>, store: IrStore) -> Self {
        Self { tokens, store }
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        match_token(&mut self.tokens, expected)
    }

    fn computation(&mut self) -> Result<()> {
        self.match_token(Token::Main)?;

        let _var_decl = match self.tokens.peek() {
            Some(Ok(Token::Var)) => self.var_decl()?,
            _ => vec![],
        };

        while let Some(Ok(Token::Void | Token::Function)) = self.tokens.peek() {
            self.func_decl()?;
        }

        self.match_token(Token::LBrack)?;

        let main_body = BodyParser::parse_main(&mut self.tokens, &mut self.store.const_block);

        self.store.insert_body("main".to_string(), main_body);

        self.match_token(Token::RBrack)?;
        self.match_token(Token::Period)?;

        Ok(())
    }

    fn func_body(&mut self, params: Vec<IdentifierId>) -> Result<Body> {
        let _var_decl = match self.tokens.peek() {
            Some(Ok(Token::Var)) => self.var_decl()?,
            _ => vec![],
        };

        self.match_token(Token::LBrack)?;

        let body = match self.tokens.peek() {
            Some(Ok(Token::Let | Token::Call | Token::If | Token::While | Token::Return)) => {
                BodyParser::parse_func(&mut self.tokens, &mut self.store.const_block, params)
            }
            _ => Body::new(),
        };

        self.match_token(Token::RBrack)?;

        Ok(body)
    }

    fn formal_param(&mut self) -> Result<Vec<IdentifierId>> {
        self.match_token(Token::LPar)?;

        if let Some(Ok(Token::RPar)) = self.tokens.peek() {
            self.tokens.next();
            return Ok(vec![]);
        }

        let mut ids = vec![match self.tokens.next() {
            Some(Ok(Token::Identifier(id))) => id,
            _ => return Err(Error::SyntaxError("Expected identifier".to_string())),
        }];

        while let Some(Ok(Token::Comma)) = self.tokens.peek() {
            self.tokens.next();
            ids.push(match self.tokens.next() {
                Some(Ok(Token::Identifier(id))) => id,
                _ => return Err(Error::SyntaxError("Expected identifier".to_string())),
            });
        }

        self.match_token(Token::RPar)?;

        Ok(ids)
    }

    fn func_decl(&mut self) -> Result<()> {
        self.tokens.next_if_eq(&Ok(Token::Void));

        self.match_token(Token::Function)?;

        let id = match self.tokens.next() {
            Some(Ok(Token::Identifier(id))) => id,
            _ => return Err(Error::SyntaxError("Expected identifier".to_string())),
        };

        let params = self.formal_param()?;

        self.match_token(Token::Semicolon)?;

        let body = self.func_body(params)?;

        self.store.insert_body(id.to_string(), body);

        self.match_token(Token::Semicolon)?;

        Ok(())
    }

    fn var_decl(&mut self) -> Result<Vec<IdentifierId>> {
        self.match_token(Token::Var)?;

        let mut ids = vec![match self.tokens.next() {
            Some(Ok(Token::Identifier(id))) => id,
            _ => return Err(Error::SyntaxError("Expected identifier".to_string())),
        }];

        while let Some(Ok(Token::Comma)) = self.tokens.peek() {
            self.tokens.next();
            ids.push(match self.tokens.next() {
                Some(Ok(Token::Identifier(id))) => id,
                _ => return Err(Error::SyntaxError("Expected identifier".to_string())),
            });
        }

        self.match_token(Token::Semicolon)?;

        Ok(ids)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::PredefinedFunction;

    use super::*;

    #[test]
    fn simple_var_decl() {
        let tokens = [Token::Var, Token::Identifier(0), Token::Semicolon];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert_eq!(parser.var_decl().unwrap(), vec![0]);
    }

    #[test]
    fn multiple_var_decl() {
        let tokens = [
            Token::Var,
            Token::Identifier(0),
            Token::Comma,
            Token::Identifier(1),
            Token::Comma,
            Token::Identifier(4),
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert_eq!(parser.var_decl().unwrap(), vec![0, 1, 4]);
    }

    #[test]
    fn missing_var_decl() {
        let tokens = [Token::Var, Token::Semicolon];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.var_decl().is_err());

        let tokens = [Token::Var, Token::Identifier(0)];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.var_decl().is_err());

        let tokens = [Token::Var, Token::Comma, Token::Semicolon];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.var_decl().is_err());

        let tokens = [
            Token::Var,
            Token::Identifier(0),
            Token::Comma,
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.var_decl().is_err());

        let tokens = [
            Token::Var,
            Token::Identifier(0),
            Token::Comma,
            Token::Identifier(1),
            Token::Comma,
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.var_decl().is_err());
    }

    #[test]
    fn simple_func_decl() {
        let tokens = [
            Token::Function,
            Token::Identifier(0),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::LBrack,
            Token::RBrack,
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.func_decl().is_ok());
        assert_eq!(parser.store.get_body("0").unwrap(), &Body::new());
    }

    #[test]
    fn void_func_decl() {
        let tokens = [
            Token::Void,
            Token::Function,
            Token::Identifier(0),
            Token::LPar,
            Token::RPar,
            Token::Semicolon,
            Token::LBrack,
            Token::RBrack,
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.func_decl().is_ok());
        assert_eq!(parser.store.get_body("0").unwrap(), &Body::new());
    }

    #[test]
    fn func_decl_with_params_and_body() {
        let tokens = [
            Token::Function,
            Token::Identifier(0),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::Semicolon,
            Token::LBrack,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(22),
            Token::Add,
            Token::Identifier(1),
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(2),
            Token::RPar,
            Token::RBrack,
            Token::Semicolon,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.func_decl().is_ok());
        assert!(parser.store.get_body("0").is_some());
    }

    #[test]
    fn empty_formal_param() {
        let tokens = [Token::LPar, Token::RPar];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert_eq!(parser.formal_param().unwrap(), vec![]);
    }

    #[test]
    fn single_formal_param() {
        let tokens = [Token::LPar, Token::Identifier(0), Token::RPar];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert_eq!(parser.formal_param().unwrap(), vec![0]);
    }

    #[test]
    fn multiple_formal_param() {
        let tokens = [
            Token::LPar,
            Token::Identifier(0),
            Token::Comma,
            Token::Identifier(1),
            Token::Comma,
            Token::Identifier(4),
            Token::RPar,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert_eq!(parser.formal_param().unwrap(), vec![0, 1, 4]);
    }

    #[test]
    fn missing_formal_param() {
        let tokens = [Token::RPar];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.formal_param().is_err());

        let tokens = [Token::LPar];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.formal_param().is_err());

        let tokens = [Token::LPar, Token::Identifier(0)];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.formal_param().is_err());

        let tokens = [Token::LPar, Token::Comma, Token::RPar];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.formal_param().is_err());

        let tokens = [Token::Identifier(0)];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.formal_param().is_err());
    }

    #[test]
    fn empty_func_body() {
        let tokens = [Token::LBrack, Token::RBrack];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        let body = parser.func_body(vec![]).unwrap();

        assert_eq!(body, Body::new());
    }

    #[test]
    fn func_body_with_var_decl_and_statements() {
        let tokens = [
            Token::Var,
            Token::Identifier(0),
            Token::Semicolon,
            Token::LBrack,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(22),
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::RBrack,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.func_body(vec![0]).is_ok());
    }

    #[test]
    fn simple_computation() {
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
            Token::Return,
            Token::Identifier(3),
            Token::Sub,
            Token::Number(1),
            Token::RBrack,
            Token::Semicolon,
            Token::LBrack,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Call,
            Token::Identifier(2),
            Token::LPar,
            Token::Number(1),
            Token::RPar,
            Token::Semicolon,
            Token::Call,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::RBrack,
            Token::Period,
        ];

        let mut parser = Parser::from(
            tokens.into_iter().map(|token| Ok(token)).peekable(),
            IrStore::new(),
        );

        assert!(parser.computation().is_ok());
        assert!(parser.store.get_body("main").is_some());
    }
}
