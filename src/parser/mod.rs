pub mod body;

use std::{iter::Peekable, marker::PhantomData};

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, BasicBlockId, Body, ControlFlowEdge},
        ConstBody, IrStore,
    },
    lexer::{Token, Tokenizer},
};

use self::body::BodyParser;

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    store: IrStore<'a>,
    const_body: ConstBody<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            store: IrStore::new(),
            const_body: ConstBody::new(),
        }
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        let next_token = self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfTokens)??;

        if next_token != expected {
            return Err(Error::UnexpectedToken(expected, next_token));
        }

        Ok(())
    }

    fn computation(&mut self) -> Result<()> {
        self.match_token(Token::Main)?;

        if let Some(Ok(Token::Var)) = self.tokens.peek() {
            todo!()
        }

        while let Some(Ok(Token::Call)) = self.tokens.peek() {
            todo!()
        }

        self.match_token(Token::LBrack)?;

        let mut main_body = Body::new();
        let cur_block = main_body.insert_block(BasicBlock::new(Vec::new(), ControlFlowEdge::Leaf));
        main_body.update_root(cur_block);

        let main_body_parser =
            BodyParser::new(&mut self.tokens, &mut self.store, &mut self.const_body);
        let mut main_body = main_body_parser.parse()?;

        // Create Const Block
        let const_block = BasicBlock::new(
            self.const_body.get_instructions().clone(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
        );
        let const_block_id = main_body.insert_block(const_block);

        // Update Dominator
        main_body
            .mut_block(BasicBlockId(0))
            .unwrap()
            .update_dominator(const_block_id);

        main_body.update_root(const_block_id);

        self.store.insert("main".to_string(), main_body);

        self.match_token(Token::RBrack)?;
        self.match_token(Token::Period)?;

        Ok(())
    }

    fn func_body(&mut self) -> Result<()> {
        todo!()
    }

    fn formal_param(&mut self) -> Result<()> {
        todo!()
    }

    fn func_decl(&mut self) -> Result<()> {
        todo!()
    }

    fn var_decl(&mut self) -> Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ir::ssa::{Instruction, Operator};

    use super::*;

    #[test]
    fn branch() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            if x < 1 then
                let a <- 2;
            else
                let a <- 4;
            fi;
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        // 4
        let const_block = BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(4, Operator::Const(2), None),
                Instruction::new(5, Operator::Const(4), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
            None,
        );

        //0
        let main_block = BasicBlock::from(
            vec![
                Instruction::new(
                    2,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, 1),
                    None,
                ),
                Instruction::new(3, Operator::Bge(5, 2), None),
            ],
            HashMap::from([(14, 1)]),
            ControlFlowEdge::Fallthrough(BasicBlockId(1)),
            Some(BasicBlockId(4)),
        );

        // 1
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 1), (15, 4)]),
            ControlFlowEdge::Branch(BasicBlockId(3)),
            Some(BasicBlockId(0)),
        );

        // 2
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 1), (15, 5)]),
            ControlFlowEdge::Fallthrough(BasicBlockId(3)),
            Some(BasicBlockId(0)),
        );

        // 3
        let join_block = BasicBlock::from(
            vec![Instruction::new(6, Operator::Phi(4, 5), None)],
            HashMap::from([(14, 1), (15, 6)]),
            ControlFlowEdge::Leaf,
            Some(BasicBlockId(0)),
        );

        let main_body = Body::from(
            BasicBlockId(4),
            vec![main_block, then_block, else_block, join_block, const_block],
        );
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }

    #[test]
    fn branch_without_else() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            if x < 1 then
                let x <- 2;
            fi;
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        // 3
        let const_block = BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(4, Operator::Const(2), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
            None,
        );

        //0
        let main_block = BasicBlock::from(
            vec![
                Instruction::new(
                    2,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, 1),
                    None,
                ),
                Instruction::new(3, Operator::Bge(5, 2), None),
            ],
            HashMap::from([(14, 1)]),
            ControlFlowEdge::Fallthrough(BasicBlockId(1)),
            Some(BasicBlockId(3)),
        );

        // 1
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(14, 4)]),
            ControlFlowEdge::Branch(BasicBlockId(2)),
            Some(BasicBlockId(0)),
        );

        // 2
        let join_block = BasicBlock::from(
            vec![Instruction::new(5, Operator::Phi(4, 1), None)],
            HashMap::from([(14, 5)]),
            ControlFlowEdge::Leaf,
            Some(BasicBlockId(0)),
        );

        let main_body = Body::from(
            BasicBlockId(3),
            vec![main_block, then_block, join_block, const_block],
        );
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }

    #[test]
    fn sanity_check() {
        let tokens = Tokenizer::new(
            "
        main {
            let x <- 1;
            let z <- 1 + 2 + 2 - 4;
            let y <- 3 * 2 / 1;
            let d <- z + y;
            let d <- (1 + d);
        }.",
        );
        let mut parser = Parser::new(tokens);
        parser.computation().unwrap();

        let const_block = BasicBlock::from(
            vec![
                Instruction::new(1, Operator::Const(1), None),
                Instruction::new(2, Operator::Const(2), None),
                Instruction::new(5, Operator::Const(4), None),
                Instruction::new(7, Operator::Const(3), None),
            ],
            HashMap::new(),
            ControlFlowEdge::Fallthrough(BasicBlockId(0)),
            None,
        );
        let main_block = BasicBlock::from(
            vec![
                Instruction::new(
                    3,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 2),
                    None,
                ),
                Instruction::new(
                    4,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 2),
                    None,
                ),
                Instruction::new(
                    6,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Sub, 4, 5),
                    None,
                ),
                Instruction::new(
                    8,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 7, 2),
                    None,
                ),
                Instruction::new(
                    9,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 8, 1),
                    None,
                ),
                Instruction::new(
                    10,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 6, 9),
                    None,
                ),
                Instruction::new(
                    11,
                    Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 10),
                    None,
                ),
            ],
            HashMap::from([(14, 1), (15, 6), (16, 9), (17, 11)]),
            ControlFlowEdge::Leaf,
            Some(BasicBlockId(1)),
        );

        let main_body = Body::from(BasicBlockId(1), vec![main_block, const_block]);
        let expected_ir = IrStore::from(HashMap::from([("main".to_string(), main_body)]));

        assert_eq!(parser.store, expected_ir);
    }
}
