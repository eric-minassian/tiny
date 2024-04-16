#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    SyntaxError(String),
}

pub type Result<T> = std::result::Result<T, Error>;
