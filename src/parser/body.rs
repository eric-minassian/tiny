use std::{collections::HashMap, iter::Peekable, rc::Rc};

use linked_hash_set::LinkedHashSet;

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, BasicBlockId, Body, ControlFlowEdge},
        ssa::{
            BranchOpcode, Instruction, InstructionId, Operator, OperatorType, StoredBinaryOpcode,
        },
        ConstBody,
    },
    lexer::{RelOp, Token},
};

use super::match_token;

pub struct BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>>,
{
    tokens: &'a mut Peekable<T>,
    const_body: &'a mut ConstBody,
    body: Body,
    cur_block: BasicBlockId,
}

impl<'a, T> BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>>,
{
    pub fn new(tokens: &'a mut Peekable<T>, const_body: &'a mut ConstBody) -> Self {
        let body = Body::new();

        Self {
            tokens,
            const_body,
            cur_block: body.get_root(),
            body,
        }
    }

    pub fn parse(self) -> Body {
        let mut parser = self;
        parser.stat_sequence().unwrap();

        parser.body
    }

    pub fn get_block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        self.body.get_mut_block(id).unwrap()
    }

    fn match_token(&mut self, expected: Token) -> Result<()> {
        match_token(&mut self.tokens, expected)
    }

    fn stat_sequence(&mut self) -> Result<()> {
        self.statement()?;

        while let Some(token) = self.tokens.peek() {
            if let Ok(Token::Semicolon) = token {
                self.tokens.next();

                if let Some(Ok(token)) = self.tokens.peek() {
                    if matches!(
                        token,
                        Token::Let | Token::Call | Token::If | Token::While | Token::Return
                    ) {
                        self.statement()?;
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        let token = self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfFile)?;

        match token {
            Ok(Token::Let) => self.assignment(),
            Ok(Token::Call) => self.func_call(),
            Ok(Token::If) => self.if_statement(),
            Ok(Token::While) => self.while_statement(),
            Ok(Token::Return) => self.return_statement(),
            _ => Err(Error::SyntaxError("Statement Error".into())),
        }
    }

    fn return_statement(&mut self) -> Result<()> {
        // self.match_token(Token::Return, "Expected 'return' keyword")?;

        // match self.expression() {
        //     Ok(_) => todo!(),
        //     Err(_) => todo!(),
        // }

        todo!()
    }

    fn while_statement(&mut self) -> Result<()> {
        // self.match_token(Token::While, "Expected 'while' keyword")?;

        // let _ = self.relation()?;

        // self.match_token(Token::Do, "Expected 'do' keyword")?;

        // let _ = self.stat_sequence()?;

        // self.match_token(Token::Od, "Expected 'od' keyword")?;

        // Ok(())
        todo!()
    }

    fn get_and_set_phi(&mut self, left_block_id: BasicBlockId, right_block_id: BasicBlockId) {
        // Retrieve mutable references to the left block and clone the modified identifiers and identifier map
        let left_block = self.get_block_mut(left_block_id);
        let left_modified_identifiers = left_block.get_modified_identifiers().clone();
        let left_identifier_map = left_block.get_identifier_map().clone();

        // Retrieve mutable references to the right block and clone the modified identifiers and identifier map
        let right_block = self.get_block_mut(right_block_id);
        let right_modified_identifiers = right_block.get_modified_identifiers().clone();
        let right_identifier_map = right_block.get_identifier_map().clone();

        // Initialize new structures for the upcoming phi nodes
        let mut new_identifier_map = HashMap::from(left_identifier_map.clone());
        let mut new_instructions = Vec::new();
        let mut modified_identifiers = LinkedHashSet::new();

        // Determine identifiers that are modified in either block
        let unified_mods = left_modified_identifiers
            .union(&right_modified_identifiers)
            .cloned();

        for id in unified_mods {
            let left_instr_id = left_identifier_map.get(&id);
            let right_instr_id = right_identifier_map.get(&id);

            // Check if the identifier is modified in both blocks
            if let (Some(&left_instr_id), Some(&right_instr_id)) = (left_instr_id, right_instr_id) {
                let new_instr_id = self.body.get_instruction_count() as i32;
                self.body.increment_instruction_count();

                new_instructions.push(Rc::new(Instruction::new(
                    new_instr_id,
                    Operator::Phi(left_instr_id, right_instr_id),
                    None,
                )));

                new_identifier_map.insert(id, new_instr_id);
                modified_identifiers.insert(id);

            // Handle cases where an identifier is modified in only one of the blocks
            } else if let Some(&instr_id) = left_instr_id.or(right_instr_id) {
                let new_instr_id = self.body.get_instruction_count() as i32;
                self.body.increment_instruction_count();

                let missing_side = self.const_body.insert_returning_id(0);
                let phi_operator = if left_instr_id.is_some() {
                    Operator::Phi(instr_id, missing_side)
                } else {
                    Operator::Phi(missing_side, instr_id)
                };

                new_instructions.push(Rc::new(Instruction::new(new_instr_id, phi_operator, None)));

                new_identifier_map.insert(id, new_instr_id);
                modified_identifiers.insert(id);
            }
        }

        let block = self.get_block_mut(self.cur_block);

        block.update_identifier_map(new_identifier_map);
        block.update_instructions(new_instructions);
        block.update_modified_identifiers(modified_identifiers);
    }

    fn if_statement(&mut self) -> Result<()> {
        // Ensure the next token is 'if' and consume it
        self.match_token(Token::If)?;

        // Evaluate the condition and get the comparator and opposite relational operator
        let (comparator_instruction_id, opposite_relop) = self.relation()?;

        // Ensure the next token is 'then' and consume it
        self.match_token(Token::Then)?;

        // Setup for branch instruction
        let current_branch_block_id = self.cur_block;
        let branch_instruction_id = self.body.get_instruction_count() as i32;
        self.body.increment_instruction_count();
        let identifier_map_copy = self
            .get_block_mut(current_branch_block_id)
            .get_identifier_map()
            .clone();
        let dominance_instruction_map_copy = self
            .get_block_mut(current_branch_block_id)
            .get_dom_instr_map_copy();

        // Create a new block for the 'then' branch
        let then_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            identifier_map_copy.clone(),
            ControlFlowEdge::Leaf,
            Some(current_branch_block_id),
            LinkedHashSet::new(),
            dominance_instruction_map_copy.clone(),
        ));
        self.cur_block = then_block_id;

        // Update the control flow from the current block to the 'then' block
        self.get_block_mut(current_branch_block_id)
            .update_edge(ControlFlowEdge::Fallthrough(then_block_id));

        // Process the statement sequence in the 'then' block
        self.stat_sequence()?;
        let then_block_end_id = self.cur_block;

        // Check if an 'else' branch is present
        let is_else_present = matches!(
            self.tokens
                .peek()
                .ok_or_else(|| Error::UnexpectedEndOfFile)?,
            Ok(Token::Else)
        );
        if is_else_present {
            self.tokens.next();

            // Create a new block for the 'else' branch
            let else_block_id = self.body.insert_block(BasicBlock::from(
                Vec::new(),
                identifier_map_copy,
                ControlFlowEdge::Leaf,
                Some(current_branch_block_id),
                LinkedHashSet::new(),
                dominance_instruction_map_copy.clone(),
            ));
            self.cur_block = else_block_id;

            // Process the statement sequence in the 'else' block
            self.stat_sequence()?;

            // Add branch instruction to jump from the 'if' condition directly to the 'else' block
            self.get_block_mut(current_branch_block_id)
                .push_instr_no_dom(Instruction::new(
                    branch_instruction_id,
                    Operator::Branch(
                        BranchOpcode::from(opposite_relop.clone()),
                        else_block_id,
                        comparator_instruction_id,
                    ),
                    None,
                ));
        }

        let else_block_end_id = self.cur_block;

        // Ensure the next token is 'fi' (end of if statement) and consume it
        self.match_token(Token::Fi)?;

        // Create a join block to consolidate the flow from 'then' and 'else'
        let join_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            HashMap::new(),
            ControlFlowEdge::Leaf,
            Some(current_branch_block_id),
            LinkedHashSet::new(),
            dominance_instruction_map_copy,
        ));

        // Update control flow for the 'then' block to the join block
        self.get_block_mut(then_block_end_id)
            .update_edge(if is_else_present {
                ControlFlowEdge::Branch(join_block_id)
            } else {
                ControlFlowEdge::Fallthrough(join_block_id)
            });

        // Update current block to the join block
        self.cur_block = join_block_id;

        // If there was an 'else' block, update its control flow to the join block
        if is_else_present {
            self.get_block_mut(else_block_end_id)
                .update_edge(ControlFlowEdge::Fallthrough(join_block_id));
            self.get_and_set_phi(then_block_end_id, else_block_end_id);
        } else {
            // Add a branch instruction to jump from the 'if' condition directly to the join block
            self.get_block_mut(current_branch_block_id)
                .push_instr_no_dom(Instruction::new(
                    branch_instruction_id,
                    Operator::Branch(
                        BranchOpcode::from(opposite_relop),
                        join_block_id,
                        comparator_instruction_id,
                    ),
                    None,
                ));

            self.get_and_set_phi(then_block_end_id, current_branch_block_id);
        }
        Ok(())
    }

    fn func_call(&mut self) -> Result<()> {
        // self.match_token(Token::Call, "Expected 'call' keyword")?;

        // match self
        //     .tokens
        //     .next()
        //     .ok_or_else(|| Error::SyntaxError("Expected Another Token".to_string()))??
        // {
        //     Token::Identifier(id) => {
        //         // @TODO: Functions without parameters can be called with or without parentheses
        //         self.match_token(Token::LPar, "Expected '(' symbol")?;

        //         let _ = self.expression()?;

        //         while let Some(token) = self.tokens.peek() {
        //             match token {
        //                 Ok(Token::Comma) => {
        //                     let _ = self.expression()?;
        //                 }
        //                 _ => break,
        //             }
        //         }

        //         todo!()
        //     }
        //     _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        // }

        todo!()
    }

    fn assignment(&mut self) -> Result<()> {
        self.match_token(Token::Let)?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::Identifier(identifier_id) => {
                self.match_token(Token::Assignment)?;

                let instruction_id = self.expression()?;

                self.get_block_mut(self.cur_block)
                    .insert_identifier(identifier_id, instruction_id);

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<(InstructionId, RelOp)> {
        let instr_id = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::RelOp(relop) => {
                let instr_id2 = self.expression()?;
                Ok((
                    self.handle_binary_op(StoredBinaryOpcode::Cmp, instr_id, instr_id2),
                    relop.opposite(),
                ))
            }
            _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
        }
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        instr_id: InstructionId,
        instr_id2: InstructionId,
    ) -> InstructionId {
        let new_instr_id = self.body.get_instruction_count() as i32;

        let operator_type = OperatorType::from(&operator);
        let mut new_instr = Instruction::new(
            new_instr_id,
            Operator::StoredBinaryOp(operator, instr_id, instr_id2),
            None,
        );

        if let Some(dom_instr) = self
            .get_block_mut(self.cur_block)
            .get_dom_instr(&operator_type)
        {
            if let Some(dup_instr_id) = dom_instr.check_dominators(&new_instr) {
                return dup_instr_id;
            }

            let dom_instr = self
                .get_block_mut(self.cur_block)
                .remove_dom_instr(operator_type.clone())
                .unwrap();

            new_instr.update_dom(dom_instr);
        }

        self.get_block_mut(self.cur_block)
            .push_instr(new_instr.clone(), operator_type);

        self.body.increment_instruction_count();

        new_instr_id
    }

    fn expression(&mut self) -> Result<InstructionId> {
        let mut instr_id = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Add, instr_id, instr_id2);
                }
                Ok(Token::Sub) => {
                    self.tokens.next();
                    let instr_id2 = self.term()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Sub, instr_id, instr_id2);
                }

                _ => break,
            }
        }

        Ok(instr_id)
    }

    fn term(&mut self) -> Result<InstructionId> {
        let mut instr_id = self.factor()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Mul) => {
                    self.tokens.next();
                    let instr_id2 = self.factor()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Mul, instr_id, instr_id2);
                }
                Ok(Token::Div) => {
                    self.tokens.next();
                    let instr_id2 = self.factor()?;
                    instr_id = self.handle_binary_op(StoredBinaryOpcode::Div, instr_id, instr_id2);
                }
                _ => break,
            }
        }

        Ok(instr_id)
    }

    fn factor(&mut self) -> Result<InstructionId> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfFile)?
        {
            Ok(Token::Identifier(id)) => {
                let instruction_id = self
                    .body
                    .get_mut_block(self.cur_block)
                    .unwrap()
                    .get_identifier(id)
                    .unwrap()
                    .clone();
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::Number(num)) => {
                let instruction_id = self.const_body.insert_returning_id(*num);
                self.tokens.next();

                Ok(instruction_id)
            }
            Ok(Token::LPar) => {
                self.tokens.next();
                let instr_id = self.expression()?;
                match self.tokens.next() {
                    Some(Ok(Token::RPar)) => Ok(instr_id),
                    Some(Ok(token)) => Err(Error::UnexpectedToken {
                        expected: Token::RPar,
                        found: token,
                    }),
                    _ => Err(Error::SyntaxError("Expected ')'.".to_string())),
                }
            }
            Ok(_) => {
                // let _ = self.func_call();
                todo!()
            }
            Err(e) => Err(e.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use std::{collections::HashSet, hash::Hash, rc::Rc};

    use crate::lexer::RelOp;

    #[test]
    fn simple_assignment() {
        /*
        let x <- 1;
        let y <- 1 + 2 + 0 - 4;
        let z <- 3 * 2 / 1;
        let w <- y + z;
        let w <- x * ((1 + w) / 2);
        */
        let tokens = [
            Ok(Token::Let),
            Ok(Token::Identifier(1)),
            Ok(Token::Assignment),
            Ok(Token::Number(1)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(2)),
            Ok(Token::Assignment),
            Ok(Token::Number(1)),
            Ok(Token::Add),
            Ok(Token::Number(2)),
            Ok(Token::Add),
            Ok(Token::Number(0)),
            Ok(Token::Sub),
            Ok(Token::Number(4)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(3)),
            Ok(Token::Assignment),
            Ok(Token::Number(3)),
            Ok(Token::Mul),
            Ok(Token::Number(2)),
            Ok(Token::Div),
            Ok(Token::Number(1)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(4)),
            Ok(Token::Assignment),
            Ok(Token::Identifier(2)),
            Ok(Token::Add),
            Ok(Token::Identifier(3)),
            Ok(Token::Semicolon),
            Ok(Token::Let),
            Ok(Token::Identifier(4)),
            Ok(Token::Assignment),
            Ok(Token::Identifier(1)),
            Ok(Token::Mul),
            Ok(Token::LPar),
            Ok(Token::LPar),
            Ok(Token::Number(1)),
            Ok(Token::Add),
            Ok(Token::Identifier(4)),
            Ok(Token::RPar),
            Ok(Token::Div),
            Ok(Token::Number(2)),
            Ok(Token::RPar),
        ];
        let mut const_body = ConstBody::new();

        let body = BodyParser::new(&mut tokens.into_iter().peekable(), &mut const_body).parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -2),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(b0_insr_1.clone()),
        ));
        let b0_insr_3 = Rc::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Sub, 2, -4),
            None,
        ));
        let b0_insr_4 = Rc::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -3, -2),
            None,
        ));
        let b0_insr_5 = Rc::new(Instruction::new(
            5,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 4, -1),
            None,
        ));
        let b0_insr_6 = Rc::new(Instruction::new(
            6,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 5),
            Some(b0_insr_2.clone()),
        ));
        let b0_insr_7 = Rc::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, 6),
            Some(b0_insr_6.clone()),
        ));
        let b0_insr_8 = Rc::new(Instruction::new(
            8,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 7, -2),
            Some(b0_insr_5.clone()),
        ));
        let b0_insr_9 = Rc::new(Instruction::new(
            9,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -1, 8),
            Some(b0_insr_4.clone()),
        ));

        let main_block = BasicBlock::from(
            vec![
                b0_insr_1,
                b0_insr_2,
                b0_insr_3.clone(),
                b0_insr_4,
                b0_insr_5,
                b0_insr_6,
                b0_insr_7.clone(),
                b0_insr_8.clone(),
                b0_insr_9.clone(),
            ],
            HashMap::from([(1, -1), (2, 3), (3, 5), (4, 9)]),
            ControlFlowEdge::Leaf,
            None,
            LinkedHashSet::from_iter([1, 2, 3, 4]),
            HashMap::from([
                (OperatorType::Add, b0_insr_7),
                (OperatorType::Mul, b0_insr_9),
                (OperatorType::Div, b0_insr_8),
                (OperatorType::Sub, b0_insr_3),
            ]),
        );
        let expected_body = Body::from(0, vec![main_block], 10);

        let expected_const_body = ConstBody::from(HashSet::from([0, 1, 2, 3, 4]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn simple_duplicate_assignment() {
        /*
        let x <- 1 + 3;
        let y <- 1 + 3;
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
        ];
        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone()],
            HashMap::from([(1, 1), (2, 1)]),
            ControlFlowEdge::Leaf,
            None,
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([(OperatorType::Add, b0_insr_1)]),
        );
        let expected_body = Body::from(0, vec![main_block], 2);

        let expected_const_body = ConstBody::from(HashSet::from([1, 3]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn identifier_duplicate_assignment() {
        /*
        let x <- 1 + 3;
        let y <- x + 3;
        let a <- x + y;
        let z <- x + 3;
        */

        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Identifier(2),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(4),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(3),
            Token::Semicolon,
        ];
        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -3),
            Some(b0_insr_1.clone()),
        ));
        let b0_insr_3 = Rc::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 2),
            Some(b0_insr_2.clone()),
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3.clone()],
            HashMap::from([(1, 1), (2, 2), (3, 3), (4, 2)]),
            ControlFlowEdge::Leaf,
            None,
            LinkedHashSet::from_iter([1, 2, 3, 4]),
            HashMap::from([(OperatorType::Add, b0_insr_3)]),
        );
        let expected_body = Body::from(0, vec![main_block], 4);

        let expected_const_body = ConstBody::from(HashSet::from([1, 3]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn simple_branch() {
        /*
        let x <- 1;
        if x < 1 then
            let x <- 2;
        else
            let x <- 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
            Token::Semicolon,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2, 1),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -2)]),
            ControlFlowEdge::Branch(3),
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -4)]),
            ControlFlowEdge::Fallthrough(3),
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let join_block = BasicBlock::from(
            vec![Rc::new(Instruction::new(3, Operator::Phi(-2, -4), None))],
            HashMap::from([(1, 3)]),
            ControlFlowEdge::Leaf,
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1)]),
        );

        let expected_body = Body::from(0, vec![main_block, then_block, else_block, join_block], 4);

        let expected_const_body = ConstBody::from(HashSet::from([1, 2, 4]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn simple_duplicate_branch() {
        /*
        let x <- 1 + 4;
        if x < 1 then
            let x <- 2 + 1;
            let y <- 1 + 4;
        else
            let x <- 1 + 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Add,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
            Token::Semicolon,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -4),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -1),
            None,
        ));
        let b0_insr_3 = Rc::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 2, 2),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2.clone(), b0_insr_3],
            HashMap::from([(1, 1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            LinkedHashSet::from_iter([1]),
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b0_insr_1.clone()),
            ]),
        );

        let b1_insr_1 = Rc::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -2, -1),
            Some(b0_insr_1.clone()),
        ));

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone()],
            HashMap::from([(1, 4), (2, 1)]),
            ControlFlowEdge::Branch(3),
            Some(0),
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b1_insr_1),
            ]),
        );
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, 1)]),
            ControlFlowEdge::Fallthrough(3),
            Some(0),
            LinkedHashSet::new(),
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b0_insr_1.clone()),
            ]),
        );
        let join_block = BasicBlock::from(
            vec![
                Rc::new(Instruction::new(5, Operator::Phi(4, 1), None)),
                Rc::new(Instruction::new(6, Operator::Phi(1, 0), None)),
            ],
            HashMap::from([(1, 5), (2, 6)]),
            ControlFlowEdge::Leaf,
            Some(0),
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b0_insr_1.clone()),
            ]),
        );

        let expected_body = Body::from(0, vec![main_block, then_block, else_block, join_block], 7);

        let expected_const_body = ConstBody::from(HashSet::from([0, 1, 2, 4]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn new_identifiers_branch() {
        /*
        let x <- 1;
        if x < 1 then
            let y <- 2;
            let z <- 13;
        else
            let y <- 4;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(3),
            Token::Assignment,
            Token::Number(13),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2, 1),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -1), (2, -2), (3, -13)]),
            ControlFlowEdge::Branch(3),
            Some(0),
            LinkedHashSet::from_iter([2, 3]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -1), (2, -4)]),
            ControlFlowEdge::Fallthrough(3),
            Some(0),
            LinkedHashSet::from_iter([2]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let join_block = BasicBlock::from(
            vec![
                Rc::new(Instruction::new(3, Operator::Phi(-2, -4), None)),
                Rc::new(Instruction::new(4, Operator::Phi(-13, 0), None)),
            ],
            HashMap::from([(1, -1), (2, 3), (3, 4)]),
            ControlFlowEdge::Leaf,
            Some(0),
            LinkedHashSet::from_iter([2, 3]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        let expected_body = Body::from(0, vec![main_block, then_block, else_block, join_block], 5);

        let expected_const_body = ConstBody::from(HashSet::from([0, 1, 2, 4, 13]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn branch_without_else_statement() {
        /*
        let x <- 1;
        if x == 1 then
            let x <- 2;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Eq),
            Token::Number(1),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(2),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ne, 2, 1),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -2)]),
            ControlFlowEdge::Fallthrough(2),
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let join_block = BasicBlock::from(
            vec![Rc::new(Instruction::new(3, Operator::Phi(-2, -1), None))],
            HashMap::from([(1, 3)]),
            ControlFlowEdge::Leaf,
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        let expected_body = Body::from(0, vec![main_block, then_block, join_block], 4);

        let expected_const_body = ConstBody::from(HashSet::from([1, 2]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }

    #[test]
    fn nested_if_statements() {
        /*
        let x = 1;
        if x > 1 then
            if x < 3 then
                let x = 3;
            else
                let x = 4;
                let y = 12;
            fi;
        else
            let x = 5;
        fi;
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Gt),
            Token::Number(1),
            Token::Then,
            Token::If,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Then,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(3),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(4),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(2),
            Token::Assignment,
            Token::Number(12),
            Token::Semicolon,
            Token::Fi,
            Token::Else,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(5),
            Token::Semicolon,
            Token::Fi,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::new(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        )
        .parse();

        let b0_insr_1 = Rc::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        ));
        let b0_insr_2 = Rc::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Le, 5, 1),
            None,
        ));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        let b1_insr_1 = Rc::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -3),
            Some(b0_insr_1.clone()),
        ));
        let b1_insr_2 = Rc::new(Instruction::new(
            4,
            Operator::Branch(BranchOpcode::Ge, 3, 3),
            None,
        ));

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone(), b1_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(2),
            Some(0),
            LinkedHashSet::new(),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );
        let sub_then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -3)]),
            ControlFlowEdge::Branch(4),
            Some(1),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );
        let sub_else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -4), (2, -12)]),
            ControlFlowEdge::Fallthrough(4),
            Some(1),
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );
        let sub_join_block = BasicBlock::from(
            vec![
                Rc::new(Instruction::new(5, Operator::Phi(-3, -4), None)),
                Rc::new(Instruction::new(6, Operator::Phi(0, -12), None)),
            ],
            HashMap::from([(1, 5), (2, 6)]),
            ControlFlowEdge::Branch(6),
            Some(1),
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -5)]),
            ControlFlowEdge::Fallthrough(6),
            Some(0),
            LinkedHashSet::from_iter([1]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let join_block = BasicBlock::from(
            vec![
                Rc::new(Instruction::new(7, Operator::Phi(5, -5), None)),
                Rc::new(Instruction::new(8, Operator::Phi(6, 0), None)),
            ],
            HashMap::from([(1, 7), (2, 8)]),
            ControlFlowEdge::Leaf,
            Some(0),
            LinkedHashSet::from_iter([1, 2]),
            HashMap::from([(OperatorType::Cmp, b0_insr_1)]),
        );

        let expected_body = Body::from(
            0,
            vec![
                main_block,
                then_block,
                sub_then_block,
                sub_else_block,
                sub_join_block,
                else_block,
                join_block,
            ],
            9,
        );

        let expected_const_body = ConstBody::from(HashSet::from([0, 1, 3, 4, 5, 12]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }
}
