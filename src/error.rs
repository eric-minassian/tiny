#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    SyntaxError(String),
}

// the `?` operator can only be applied to values that implement `Try`
// the trait `Try` is not implemented for `&std::result::Result<Token, error::Error>`
// the trait `Try` is implemented for `std::result::Result<T, E>`rustcClick for full compiler diagnostic
// the `?` operator can only be applied to values that implement `Try`
// the trait `Try` is not implemented for `&std::result::Result<lexer::Token, error::Error>`
// the trait `Try` is implemented for `std::result::Result<T, E>`rustcClick for full compiler diagnostic

pub type Result<T> = std::result::Result<T, Error>;
