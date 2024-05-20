use super::Token;

#[derive(Debug, PartialEq)]
pub enum TokenError {
    InvalidCharacter(char),
    InvalidString(String),
    UnexpectedEndOfInput,
}

pub type TokenResult = Result<Token, TokenError>;
