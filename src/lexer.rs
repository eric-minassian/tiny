use std::{iter::Peekable, str::Chars};

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
pub enum Token {
    Number(u32),
    Identifier(usize),

    Eq,         // ==
    Ne,         // !=
    Lt,         // <
    Le,         // <=
    Gt,         // >
    Ge,         // >=
    Mul,        // *
    Div,        // /
    Add,        // +
    Sub,        // -
    Assignment, // <-
    LPar,       // (
    RPar,       // )
    LBrack,     // {
    RBRack,     // }
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

    fn consume_builtin_funct(&mut self) -> Token {
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
            Some((_, token)) => token.clone(),
            None => panic!("Syntax Error: Builtin Function Not Found"),
        }
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

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
                'A'..='Z' => Some(self.consume_builtin_funct()),
                'a'..='z' => Some(self.consume_alpha()),
                '0'..='9' => Some(self.consume_number()),

                '=' => {
                    self.chars.next();
                    match self.chars.next() {
                        Some('=') => Some(Token::Eq),
                        _ => panic!("Syntax Error"),
                    }
                }
                '!' => {
                    self.chars.next();
                    match self.chars.next() {
                        Some('=') => Some(Token::Ne),
                        _ => panic!("Syntax Error"),
                    }
                }

                '<' => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Some(Token::Le)
                        }
                        Some('-') => {
                            self.chars.next();
                            Some(Token::Assignment)
                        }
                        _ => Some(Token::Lt),
                    }
                }
                '>' => {
                    self.chars.next();
                    match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Some(Token::Ge)
                        }

                        _ => Some(Token::Gt),
                    }
                }

                '*' => {
                    self.chars.next();
                    Some(Token::Mul)
                }
                '/' => {
                    self.chars.next();
                    Some(Token::Div)
                }
                '+' => {
                    self.chars.next();
                    Some(Token::Add)
                }
                '-' => {
                    self.chars.next();
                    Some(Token::Sub)
                }
                '(' => {
                    self.chars.next();
                    Some(Token::LPar)
                }
                ')' => {
                    self.chars.next();
                    Some(Token::RPar)
                }
                '{' => {
                    self.chars.next();
                    Some(Token::LBrack)
                }
                '}' => {
                    self.chars.next();
                    Some(Token::RBRack)
                }
                ';' => {
                    self.chars.next();
                    Some(Token::Semicolon)
                }
                ',' => {
                    self.chars.next();
                    Some(Token::Comma)
                }
                '.' => {
                    self.chars.next();
                    Some(Token::Period)
                }

                _ => panic!("Syntax Error"),
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        for expected in expected_tokens.iter() {
            assert_eq!(tokenizer.next(), Some(expected.clone()));
        }

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_numbers() {
        let input = "123 456 7890";
        let mut tokenizer = Tokenizer::new(input);
        let expected_tokens = [Token::Number(123), Token::Number(456), Token::Number(7890)];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
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
            assert_eq!(tokenizer.next(), Some(expected));
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
            assert_eq!(tokenizer.next(), Some(expected));
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
            Token::Eq,
            Token::Ne,
            Token::Lt,
            Token::Le,
            Token::Gt,
            Token::Ge,
            Token::Assignment,
            Token::LPar,
            Token::RPar,
            Token::LBrack,
            Token::RBRack,
            Token::Semicolon,
            Token::Comma,
            Token::Period,
        ];

        for expected in expected_tokens {
            assert_eq!(tokenizer.next(), Some(expected));
        }

        assert_eq!(tokenizer.next(), None);
    }
}
