use std::{iter::Peekable, str::Chars};

use crate::error::{Error, Result};

pub type IdentifierId = usize;

const RADIX: u32 = 10;
const RESERVED_WORDS_COUNT: usize = 14;

const RESERVED_WORDS: [(&str, Token); RESERVED_WORDS_COUNT] = [
    ("let", Token::Let),
    ("call", Token::Call),
    ("if", Token::If),
    ("then", Token::Then),
    ("else", Token::Else),
    ("fi", Token::Fi),
    ("while", Token::While),
    ("do", Token::Do),
    ("od", Token::Od),
    ("return", Token::Return),
    ("var", Token::Var),
    ("void", Token::Void),
    ("function", Token::Function),
    ("main", Token::Main),
];

const BUILTIN_FUNCTIONS: [(&str, Token); 3] = [
    ("InputNum", Token::InputNum),
    ("OutputNum", Token::OutputNum),
    ("OutputNewLine", Token::OutputNewLine),
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelOp {
    Eq, // ==
    Ne, // !=
    Lt, // <
    Le, // <=
    Gt, // >
    Ge, // >=
}
impl RelOp {
    #[must_use]
    pub fn opposite(&self) -> Self {
        match self {
            RelOp::Eq => RelOp::Ne,
            RelOp::Ne => RelOp::Eq,
            RelOp::Lt => RelOp::Ge,
            RelOp::Le => RelOp::Gt,
            RelOp::Gt => RelOp::Le,
            RelOp::Ge => RelOp::Lt,
        }
    }
}

pub type Number = u32;
pub type Identifier = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Number(Number),
    Identifier(Identifier),
    RelOp(RelOp),

    Mul,        // *
    Div,        // /
    Add,        // +
    Sub,        // -
    Assignment, // <-
    LPar,       // (
    RPar,       // )
    LBrack,     // {
    RBrack,     // }
    Semicolon,  // ;
    Comma,      // ,
    Period,     // .

    Let,
    Call,
    If,
    Then,
    Else,
    Fi,
    While,
    Do,
    Od,
    Return,
    Var,
    Void,
    Function,
    Main,

    InputNum,
    OutputNum,
    OutputNewLine,
}

pub struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    identifier_map: Vec<String>,
}

impl<'a> Tokenizer<'a> {
    #[must_use]
    pub fn new(s: &'a str) -> Self {
        Self {
            chars: s.chars().peekable(),
            identifier_map: RESERVED_WORDS
                .iter()
                .map(|(s, _)| (*s).to_string())
                .collect(),
        }
    }

    fn consume_number(&mut self) -> Token {
        let mut num = 0;

        while let Some(&ch) = self.chars.peek() {
            if let Some(value) = ch.to_digit(RADIX) {
                num = num * 10 + value;
                self.chars.next();
            } else {
                break;
            }
        }

        Token::Number(num)
    }

    fn consume_alpha(&mut self) -> Token {
        let mut buffer = String::new();
        while let Some(&ch) = self.chars.peek() {
            if ch.is_ascii_lowercase() || ch.is_ascii_digit() {
                buffer.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        match self.identifier_map.iter().position(|v| v == &buffer) {
            Some(pos) if pos < RESERVED_WORDS_COUNT => RESERVED_WORDS[pos].1.clone(),
            Some(pos) => Token::Identifier(pos),
            None => {
                self.identifier_map.push(buffer);
                Token::Identifier(self.identifier_map.len() - 1)
            }
        }
    }

    fn consume_builtin_func(&mut self) -> Result<Token> {
        let mut buffer = String::new();
        while let Some(&ch) = self.chars.peek() {
            if ch.is_ascii_alphabetic() {
                buffer.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }

        match BUILTIN_FUNCTIONS.iter().find(|(v, _)| v == &buffer) {
            Some((_, token)) => Ok(token.clone()),
            None => Err(Error::SyntaxError(format!(
                "Builting Function '{}' Not Found",
                buffer
            ))),
        }
    }

    fn consume_char(&mut self, token: Token) -> Token {
        self.chars.next();
        token
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&ch) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }

        if let Some(&ch) = self.chars.peek() {
            match ch {
                'A'..='Z' => Some(self.consume_builtin_func()),
                'a'..='z' => Some(Ok(self.consume_alpha())),
                '0'..='9' => Some(Ok(self.consume_number())),

                '=' => {
                    self.chars.next();
                    match self.chars.next() {
                        Some('=') => Some(Ok(Token::RelOp(RelOp::Eq))),
                        _ => Some(Err(Error::SyntaxError(
                            "Expected '=' after '='".to_string(),
                        ))),
                    }
                }
                '!' => {
                    self.chars.next();
                    match self.chars.next() {
                        Some('=') => Some(Ok(Token::RelOp(RelOp::Ne))),
                        _ => Some(Err(Error::SyntaxError(
                            "Expected '=' after '!'".to_string(),
                        ))),
                    }
                }

                '<' => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Some(Ok(Token::RelOp(RelOp::Le)))
                        }
                        Some('-') => {
                            self.chars.next();
                            Some(Ok(Token::Assignment))
                        }
                        _ => Some(Ok(Token::RelOp(RelOp::Lt))),
                    }
                }
                '>' => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Some(Ok(Token::RelOp(RelOp::Ge)))
                        }

                        _ => Some(Ok(Token::RelOp(RelOp::Gt))),
                    }
                }

                '*' => Some(Ok(self.consume_char(Token::Mul))),
                '/' => Some(Ok(self.consume_char(Token::Div))),
                '+' => Some(Ok(self.consume_char(Token::Add))),
                '-' => Some(Ok(self.consume_char(Token::Sub))),
                '(' => Some(Ok(self.consume_char(Token::LPar))),
                ')' => Some(Ok(self.consume_char(Token::RPar))),
                '{' => Some(Ok(self.consume_char(Token::LBrack))),
                '}' => Some(Ok(self.consume_char(Token::RBrack))),
                ';' => Some(Ok(self.consume_char(Token::Semicolon))),
                ',' => Some(Ok(self.consume_char(Token::Comma))),
                '.' => Some(Ok(self.consume_char(Token::Period))),

                _ => {
                    self.chars.next();
                    Some(Err(Error::SyntaxError(format!(
                        "Unexpected Character '{}'",
                        ch
                    ))))
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_reserved_words() {
        let input = "let call if then else fi while do od return var void function main";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Token::Let,
            Token::Call,
            Token::If,
            Token::Then,
            Token::Else,
            Token::Fi,
            Token::While,
            Token::Do,
            Token::Od,
            Token::Return,
            Token::Var,
            Token::Void,
            Token::Function,
            Token::Main,
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_numbers() {
        let input = "123 456 7890";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [Token::Number(123), Token::Number(456), Token::Number(7890)];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let input = "x y foo bar hello12 foo";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Token::Identifier(14), // Assuming the reserved words occupy the first 14 slots
            Token::Identifier(15),
            Token::Identifier(16),
            Token::Identifier(17),
            Token::Identifier(18),
            Token::Identifier(16),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_builtin_functions() {
        let input = "InputNum() OutputNum(x) OutputNewLine()";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Token::InputNum,
            Token::LPar,
            Token::RPar,
            Token::OutputNum,
            Token::LPar,
            Token::Identifier(14), // Assuming the reserved words occupy the first 14 slots
            Token::RPar,
            Token::OutputNewLine,
            Token::LPar,
            Token::RPar,
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_operators_and_symbols() {
        let input = "+ - * / == != < <= > >= <- ( ) { } ; , .";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Token::Add,
            Token::Sub,
            Token::Mul,
            Token::Div,
            Token::RelOp(RelOp::Eq),
            Token::RelOp(RelOp::Ne),
            Token::RelOp(RelOp::Lt),
            Token::RelOp(RelOp::Le),
            Token::RelOp(RelOp::Gt),
            Token::RelOp(RelOp::Ge),
            Token::Assignment,
            Token::LPar,
            Token::RPar,
            Token::LBrack,
            Token::RBrack,
            Token::Semicolon,
            Token::Comma,
            Token::Period,
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_mixed_input() {
        let input = "let x <- 123; call foo()";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Token::Let,
            Token::Identifier(14), // Assuming the reserved words occupy the first 14 slots
            Token::Assignment,
            Token::Number(123),
            Token::Semicolon,
            Token::Call,
            Token::Identifier(15),
            Token::LPar,
            Token::RPar,
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(Ok(expected)));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_invalid_characters() {
        let input = "let x <- 123; call foo() #";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(14)), // Assuming the reserved words occupy the first 14 slots
            Ok(Token::Assignment),
            Ok(Token::Number(123)),
            Ok(Token::Semicolon),
            Ok(Token::Call),
            Ok(Token::Identifier(15)),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Err(Error::SyntaxError("Unexpected Character '#'".to_string())),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);

        let input = "let x = 123;";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(14)), // Assuming the reserved words occupy the first 14 slots
            Err(Error::SyntaxError("Expected '=' after '='".to_string())),
            Ok(Token::Number(123)),
            Ok(Token::Semicolon),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);

        let input = "let x ! 123;";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(14)), // Assuming the reserved words occupy the first 14 slots
            Err(Error::SyntaxError("Expected '=' after '!'".to_string())),
            Ok(Token::Number(123)),
            Ok(Token::Semicolon),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_invalid_builtin_function() {
        let input = "InputNum() OutputNum(x) OutputNewLine() OutputNotValid()";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Ok(Token::InputNum),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Ok(Token::OutputNum),
            Ok(Token::LPar),
            Ok(Token::Identifier(14)), // Assuming the reserved words occupy the first 14 slots
            Ok(Token::RPar),
            Ok(Token::OutputNewLine),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Err(Error::SyntaxError(
                "Builting Function 'OutputNotValid' Not Found".to_string(),
            )),
            Ok(Token::LPar),
            Ok(Token::RPar),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);
    }
}
