use super::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenError {
    InvalidCharacter(char),
    InvalidString(String),
    UnexpectedEndOfInput,
}

pub type TokenResult = Result<Token, TokenError>;
