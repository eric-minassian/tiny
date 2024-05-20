use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnexpectedToken { expected: Token, found: Token },
    UnexpectedEndOfFile,

    // General error during development
    SyntaxError(String),
}

pub type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token: expected {:?}, found {:?}",
                    expected, found
                )
            }
            Error::UnexpectedEndOfFile => {
                write!(f, "Unexpected end of file")
            }
            Error::SyntaxError(msg) => {
                write!(f, "Syntax error: {}", msg)
            }
        }
    }
}
