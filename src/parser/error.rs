use crate::lexer::{error::TokenError, Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum ParserError {
    InvalidToken(TokenError),
    UnexpectedToken(Vec<TokenType>, Token),
    UnexpectedEndOfFile,
}

impl From<TokenError> for ParserError {
    fn from(error: TokenError) -> Self {
        Self::InvalidToken(error)
    }
}

pub type ParserResult<T> = Result<T, ParserError>;
