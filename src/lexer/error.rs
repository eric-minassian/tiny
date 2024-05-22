use super::Token;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenError {
    InvalidCharacter(char),
    InvalidString(String),
    UnexpectedEndOfInput,
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenError::InvalidCharacter(c) => {
                write!(f, "Invalid character: {}", c)
            }
            TokenError::InvalidString(s) => {
                write!(f, "Invalid string: {}", s)
            }
            TokenError::UnexpectedEndOfInput => {
                write!(f, "Unexpected end of input")
            }
        }
    }
}

pub type TokenResult = Result<Token, TokenError>;
