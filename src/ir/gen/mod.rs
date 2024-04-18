use std::iter::Peekable;

use crate::lexer::Tokenizer;

use super::{block::Body, ConstBody};

struct BodyGen<'a, 'b, 'c> {
    tokens: &'a mut Peekable<Tokenizer<'a>>,
    instruction_counter: u32,
    body: Body<'b>,
    const_body: &'c mut ConstBody<'c>,
}

impl<'a, 'b, 'c> BodyGen<'a, 'b, 'c> {
    pub fn new(
        tokens: &'a mut Peekable<Tokenizer<'a>>,
        instruction_counter: u32,
        const_body: &'c mut ConstBody<'c>,
    ) -> Self {
        Self {
            body: Body::new(),
            tokens,
            instruction_counter,
            const_body,
        }
    }

    pub fn generate_body() {
        todo!()
    }
}
