use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnexpectedToken { expected: Token, found: Token },
    UnexpectedEndOfFile,

    // General error during development
    SyntaxError(String),
}

pub type Result<T> = std::result::Result<T, Error>;
