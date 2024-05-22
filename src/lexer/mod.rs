pub mod error;

use std::{collections::HashMap, iter::Peekable, str::Chars};

use self::error::{TokenError, TokenResult};

const RADIX: u32 = 10;

const RESERVED_WORDS: [(&str, Token); 14] = [
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
    (
        "InputNum",
        Token::PredefinedFunction(PredefinedFunction::InputNum),
    ),
    (
        "OutputNum",
        Token::PredefinedFunction(PredefinedFunction::OutputNum),
    ),
    (
        "OutputNewLine",
        Token::PredefinedFunction(PredefinedFunction::OutputNewLine),
    ),
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
    pub const fn opposite(&self) -> Self {
        match self {
            Self::Eq => Self::Ne,
            Self::Ne => Self::Eq,
            Self::Lt => Self::Ge,
            Self::Le => Self::Gt,
            Self::Gt => Self::Le,
            Self::Ge => Self::Lt,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredefinedFunction {
    InputNum,
    OutputNum,
    OutputNewLine,
}

pub type Number = u32;
pub type Identifier = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Number(Number),
    Identifier(Identifier),
    RelOp(RelOp),
    PredefinedFunction(PredefinedFunction),

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

    Invalid,
}

impl Token {
    #[must_use]
    pub const fn get_type(&self) -> TokenType {
        match self {
            Self::Number(_) => TokenType::Number,
            Self::Identifier(_) => TokenType::Identifier,
            Self::RelOp(_) => TokenType::RelOp,
            Self::PredefinedFunction(_) => TokenType::PredefinedFunction,

            Self::Mul => TokenType::Mul,
            Self::Div => TokenType::Div,
            Self::Add => TokenType::Add,
            Self::Sub => TokenType::Sub,
            Self::Assignment => TokenType::Assignment,
            Self::LPar => TokenType::LPar,
            Self::RPar => TokenType::RPar,
            Self::LBrack => TokenType::LBrack,
            Self::RBrack => TokenType::RBrack,
            Self::Semicolon => TokenType::Semicolon,
            Self::Comma => TokenType::Comma,
            Self::Period => TokenType::Period,

            Self::Let => TokenType::Let,
            Self::Call => TokenType::Call,
            Self::If => TokenType::If,
            Self::Then => TokenType::Then,
            Self::Else => TokenType::Else,
            Self::Fi => TokenType::Fi,
            Self::While => TokenType::While,
            Self::Do => TokenType::Do,
            Self::Od => TokenType::Od,
            Self::Return => TokenType::Return,
            Self::Var => TokenType::Var,
            Self::Void => TokenType::Void,
            Self::Function => TokenType::Function,
            Self::Main => TokenType::Main,

            Self::Invalid => TokenType::Invalid,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Number,
    Identifier,
    RelOp,
    PredefinedFunction,

    Mul,
    Div,
    Add,
    Sub,
    Assignment,
    LPar,
    RPar,
    LBrack,
    RBrack,
    Semicolon,
    Comma,
    Period,

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

    Invalid,
}

pub struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    identifier_map: HashMap<String, Identifier>,
}

impl<'a> Tokenizer<'a> {
    #[must_use]
    pub fn new(s: &'a str) -> Self {
        Self {
            chars: s.chars().peekable(),
            identifier_map: HashMap::new(),
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

        if let Some(token) = RESERVED_WORDS.iter().find(|(v, _)| v == &buffer) {
            token.1.clone()
        } else if let Some(&token) = self.identifier_map.get(&buffer) {
            Token::Identifier(token)
        } else {
            let ident =
                Identifier::try_from(self.identifier_map.len()).expect("Too many identifiers") + 1; // 0 is reserved for constant 0
            self.identifier_map.insert(buffer, ident);
            Token::Identifier(ident)
        }
    }

    fn consume_builtin_func(&mut self) -> TokenResult {
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
            None => Err(TokenError::InvalidString(buffer)),
        }
    }

    fn consume_char(&mut self, token: Token) -> Token {
        self.chars.next();
        token
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = TokenResult;

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
                        Some(next_ch) => {
                            Some(Err(TokenError::InvalidString(format!("{}{}", ch, next_ch))))
                        }
                        None => Some(Err(TokenError::UnexpectedEndOfInput)),
                    }
                }
                '!' => {
                    self.chars.next();
                    match self.chars.next() {
                        Some('=') => Some(Ok(Token::RelOp(RelOp::Ne))),
                        Some(next_ch) => {
                            Some(Err(TokenError::InvalidString(format!("{}{}", ch, next_ch))))
                        }
                        None => Some(Err(TokenError::UnexpectedEndOfInput)),
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

                ch => {
                    self.chars.next();
                    Some(Err(TokenError::InvalidCharacter(ch)))
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

    use pretty_assertions_sorted::assert_eq;

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
            Token::Identifier(1),
            Token::Identifier(2),
            Token::Identifier(3),
            Token::Identifier(4),
            Token::Identifier(5),
            Token::Identifier(3),
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
            Token::PredefinedFunction(PredefinedFunction::InputNum),
            Token::LPar,
            Token::RPar,
            Token::PredefinedFunction(PredefinedFunction::OutputNum),
            Token::LPar,
            Token::Identifier(1),
            Token::RPar,
            Token::PredefinedFunction(PredefinedFunction::OutputNewLine),
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
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(123),
            Token::Semicolon,
            Token::Call,
            Token::Identifier(2),
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
            Ok(Token::Identifier(1)),
            Ok(Token::Assignment),
            Ok(Token::Number(123)),
            Ok(Token::Semicolon),
            Ok(Token::Call),
            Ok(Token::Identifier(2)),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Err(TokenError::InvalidCharacter('#')),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);

        let input = "let x = 123;";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(1)),
            Err(TokenError::InvalidString("= ".to_string())),
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
            Ok(Token::Identifier(1)),
            Err(TokenError::InvalidString("! ".to_string())),
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
            Ok(Token::PredefinedFunction(PredefinedFunction::InputNum)),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Ok(Token::PredefinedFunction(PredefinedFunction::OutputNum)),
            Ok(Token::LPar),
            Ok(Token::Identifier(1)),
            Ok(Token::RPar),
            Ok(Token::PredefinedFunction(PredefinedFunction::OutputNewLine)),
            Ok(Token::LPar),
            Ok(Token::RPar),
            Err(TokenError::InvalidString("OutputNotValid".to_string())),
            Ok(Token::LPar),
            Ok(Token::RPar),
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);
    }
}
