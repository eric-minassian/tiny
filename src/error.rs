use crate::lexer::{IdentifierId, Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedTokens {
    Single(TokenType),
    OneOf(Vec<TokenType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnexpectedToken {
        expected: ExpectedTokens,
        found: Token,
    },
    UnexpectedEndOfFile,

    // General error during development
    SyntaxError(String),
}

pub type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken { expected, found } => match expected {
                ExpectedTokens::Single(expected_token) => {
                    write!(
                        f,
                        "Unexpected token: expected {:?}, found {:?}",
                        expected_token, found
                    )
                }
                ExpectedTokens::OneOf(expected_tokens) => {
                    write!(
                        f,
                        "Unexpected token: expected one of {:?}, found {:?}",
                        expected_tokens, found
                    )
                }
            },
            Error::UnexpectedEndOfFile => {
                write!(f, "Unexpected end of file")
            }
            Error::SyntaxError(msg) => {
                write!(f, "Syntax error: {}", msg)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Warning {
    UninitializedIdentifier(IdentifierId),
    UndeclaredIdentifier(IdentifierId),
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
