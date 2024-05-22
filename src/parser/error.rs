use crate::lexer::{error::TokenError, Identifier, Token, TokenType};

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

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParserError::InvalidToken(error) => {
                write!(f, "Invalid token: {}", error)
            }
            ParserError::UnexpectedToken(expected, found) => {
                write!(
                    f,
                    "Unexpected token: expected one of {:?}, found {:?}",
                    expected, found
                )
            }
            ParserError::UnexpectedEndOfFile => {
                write!(f, "Unexpected end of file")
            }
        }
    }
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Warning {
    UninitializedIdentifier(Identifier),
    UndeclaredIdentifier(Identifier),
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Warning::UninitializedIdentifier(id) => {
                write!(f, "Uninitialized identifier {}, initializing to 0", id)
            }
            Warning::UndeclaredIdentifier(id) => {
                write!(f, "Undeclared identifier {}", id)
            }
        }
    }
}

pub fn print_warning(warning: Warning) {
    eprintln!("\x1b[93mWarning: {}\x1b[0m", warning);
}
