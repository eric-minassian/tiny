mod body;

use std::iter::Peekable;

use crate::{
    error::{Error, Result},
    ir::{ConstBody, IrStore},
    lexer::{Token, Tokenizer},
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

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    store: IrStore,
    const_body: ConstBody,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            store: IrStore::new(),
            const_body: ConstBody::new(),
        }
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        match_token(&mut self.tokens, expected)
    }

    fn computation(&mut self) -> Result<()> {
        self.match_token(Token::Main)?;

        if let Some(Ok(Token::Var)) = self.tokens.peek() {
            todo!()
        }

        while let Some(Ok(Token::Call)) = self.tokens.peek() {
            todo!()
        }

        self.match_token(Token::LBrack)?;

        let main_body = BodyParser::parse(&mut self.tokens, &mut self.const_body);

        self.store.insert("main".to_string(), main_body);

        self.match_token(Token::RBrack)?;
        self.match_token(Token::Period)?;

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
}
