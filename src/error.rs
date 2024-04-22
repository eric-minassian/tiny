use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    SyntaxError(String),
    UnexpectedToken(Token, Token),
    UnexpectedEndOfTokens,
    InternalError,
}

pub type Result<T> = std::result::Result<T, Error>;
