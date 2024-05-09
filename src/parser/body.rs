use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    iter::Peekable,
    rc::Rc,
};

use crate::{
    error::{Error, Result},
    ir::{
        block::{BasicBlock, BasicBlockId, Body, ControlFlowEdge},
        ssa::{
            BranchOpcode, Instruction, InstructionId, Operator, OperatorType, Side,
            StoredBinaryOpcode,
        },
        ConstBody,
    },
    lexer::{IdentifierId, RelOp, Token},
};

use super::match_token;

pub struct PhiValues {
    pub left_identifier_id: Option<i32>,
    pub right_identifier_id: Option<i32>,
}

pub struct PhiBlock {
    pub side: Side,
    pub phi_map: BTreeMap<IdentifierId, PhiValues>, // Use BTreeMap to ensure deterministic order
}

impl PhiBlock {
    pub fn new() -> Self {
        Self {
            side: Side::Left,
            phi_map: BTreeMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SimpleReturn {
    pub instruction_id: InstructionId,
    pub identifier_id: Option<IdentifierId>,
}

impl SimpleReturn {
    pub fn new(instruction_id: InstructionId, identifier_id: Option<IdentifierId>) -> Self {
        Self {
            instruction_id,
            identifier_id,
        }
    }
}

#[derive(Debug)]
pub struct IdentifierUse {
    pub instruction: Rc<RefCell<Instruction>>,
    pub side: Side,
}

impl IdentifierUse {
    pub fn new(instruction: Rc<RefCell<Instruction>>, side: Side) -> Self {
        Self { instruction, side }
    }
}

pub struct BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>>,
{
    tokens: &'a mut Peekable<T>,
    const_body: &'a mut ConstBody,
    body: Body,
    cur_block: BasicBlockId,
    join_blocks: Vec<PhiBlock>,
    identifier_use_map: HashMap<IdentifierId, Vec<IdentifierUse>>,
}

impl<'a, T> BodyParser<'a, T>
where
    T: Iterator<Item = Result<Token>>,
{
    pub fn parse(tokens: &'a mut Peekable<T>, const_body: &'a mut ConstBody) -> Body {
        let mut body_parser = Self::new(tokens, const_body);
        body_parser.stat_sequence().unwrap();

        body_parser.body
    }

    fn new(tokens: &'a mut Peekable<T>, const_body: &'a mut ConstBody) -> Self {
        let body = Body::new();

        Self {
            tokens,
            const_body,
            cur_block: body.get_root(),
            body,
            join_blocks: Vec::new(),
            identifier_use_map: HashMap::new(),
        }
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
        self.match_token(Token::While)?;

        let identifier_map_copy = self
            .get_block_mut(self.cur_block)
            .get_identifier_map()
            .clone();
        let dominance_instruction_map_copy =
            self.get_block_mut(self.cur_block).get_dom_instr_map_copy();

        let join_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            identifier_map_copy.clone(),
            ControlFlowEdge::Leaf,
            Some(self.cur_block),
            dominance_instruction_map_copy.clone(),
        ));

        self.get_block_mut(self.cur_block)
            .update_edge(ControlFlowEdge::Fallthrough(join_block_id));

        self.cur_block = join_block_id;

        let (comparator_instruction_id, opposite_relop) = self.relation()?;
        let join_block_branch_id = self.body.get_instruction_count() as i32;
        self.body.increment_instruction_count();

        self.match_token(Token::Do)?;

        let mut phi_block_temp = PhiBlock::new();
        phi_block_temp.side = Side::Right;
        self.join_blocks.push(phi_block_temp);

        let join_block_identifier_map = self
            .get_block_mut(join_block_id)
            .get_identifier_map()
            .clone();
        let join_block_dominance_instruction_map =
            self.get_block_mut(join_block_id).get_dom_instr_map_copy();

        let body_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            join_block_identifier_map.clone(),
            ControlFlowEdge::Leaf,
            Some(join_block_id),
            join_block_dominance_instruction_map.clone(),
        ));

        self.get_block_mut(join_block_id)
            .update_edge(ControlFlowEdge::Fallthrough(body_block_id));
        self.cur_block = body_block_id;

        self.stat_sequence()?;

        self.match_token(Token::Od)?;

        let body_block_end_id = self.cur_block;

        self.get_block_mut(body_block_end_id)
            .update_edge(ControlFlowEdge::Branch(join_block_id));

        let body_block_branch_id = self.body.get_instruction_count() as i32;
        self.body.increment_instruction_count();
        let body_block_end_instr = Instruction::new(
            body_block_branch_id,
            Operator::UnconditionalBranch(join_block_id),
            None,
        );

        self.get_block_mut(body_block_end_id)
            .push_instr_no_dom(body_block_end_instr);

        let phi_node = self.join_blocks.pop().unwrap();

        for (id, temp_phi) in phi_node.phi_map.into_iter() {
            let new_instr_id = self.body.get_instruction_count() as i32;
            self.body.increment_instruction_count();

            let default_value = *identifier_map_copy.get(&id).unwrap_or(&0);

            // Temporary: Integrate better into handling of 0 values
            if default_value == 0
                && (temp_phi.left_identifier_id.is_none() || temp_phi.right_identifier_id.is_none())
            {
                self.const_body.insert_returning_id(0);
            }

            let operator = Operator::Phi(
                temp_phi.left_identifier_id.unwrap_or(default_value),
                temp_phi.right_identifier_id.unwrap_or(default_value),
            );

            let new_instr = Instruction::new(new_instr_id, operator, None);

            let block = self.get_block_mut(join_block_id);

            block.push_phi_instr(new_instr);
            block.insert_identifier(id, new_instr_id);

            let identifier_use = self.identifier_use_map.get_mut(&id).unwrap();

            for i in (0..identifier_use.len()).rev() {
                let identifier_use_temp = &identifier_use[i];
                let mut identifier_use_instr_mut = identifier_use_temp.instruction.borrow_mut();

                if identifier_use_instr_mut.id() < comparator_instruction_id.instruction_id {
                    break;
                }

                identifier_use_instr_mut
                    .update_identifier(new_instr_id, identifier_use_temp.side.clone());
            }

            for phi_node_temp in self.join_blocks.iter_mut() {
                if let Some(temp_phi) = phi_node_temp.phi_map.get_mut(&id) {
                    match phi_node_temp.side {
                        Side::Left => {
                            temp_phi.left_identifier_id = Some(new_instr_id);
                        }
                        Side::Right => {
                            temp_phi.right_identifier_id = Some(new_instr_id);
                        }
                    }
                } else {
                    phi_node_temp.phi_map.insert(
                        id,
                        match phi_node_temp.side {
                            Side::Left => PhiValues {
                                left_identifier_id: Some(new_instr_id),
                                right_identifier_id: None,
                            },
                            Side::Right => PhiValues {
                                left_identifier_id: None,
                                right_identifier_id: Some(new_instr_id),
                            },
                        },
                    );
                }
            }
        }

        let join_block_identifier_map = self
            .get_block_mut(join_block_id)
            .get_identifier_map()
            .clone();
        let join_block_dominance_instruction_map =
            self.get_block_mut(join_block_id).get_dom_instr_map_copy();

        let escape_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            join_block_identifier_map,
            ControlFlowEdge::Leaf,
            Some(join_block_id),
            join_block_dominance_instruction_map,
        ));

        self.get_block_mut(join_block_id)
            .push_instr_no_dom(Instruction::new(
                join_block_branch_id,
                Operator::Branch(
                    BranchOpcode::from(opposite_relop),
                    escape_block_id,
                    comparator_instruction_id.instruction_id,
                ),
                None,
            ));

        println!("AFTER WHILE: {:?}", self.identifier_use_map);

        Ok(())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.match_token(Token::If)?;

        let (comparator_instruction_id, opposite_relop) = self.relation()?;

        self.match_token(Token::Then)?;

        let branch_block_id = self.cur_block;
        let branch_instruction_id = self.body.get_instruction_count() as i32;
        self.body.increment_instruction_count();
        let identifier_map_copy = self
            .get_block_mut(branch_block_id)
            .get_identifier_map()
            .clone();
        let dominance_instruction_map_copy =
            self.get_block_mut(branch_block_id).get_dom_instr_map_copy();

        self.join_blocks.push(PhiBlock::new());

        let then_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            identifier_map_copy.clone(),
            ControlFlowEdge::Leaf,
            Some(branch_block_id),
            dominance_instruction_map_copy.clone(),
        ));

        self.get_block_mut(branch_block_id)
            .update_edge(ControlFlowEdge::Fallthrough(then_block_id));

        self.cur_block = then_block_id;
        self.stat_sequence()?;
        let then_block_end_id = self.cur_block;

        let is_else_present = matches!(
            self.tokens
                .peek()
                .ok_or_else(|| Error::UnexpectedEndOfFile)?,
            Ok(Token::Else)
        );

        if is_else_present {
            self.tokens.next();

            self.join_blocks.last_mut().unwrap().side = Side::Right;

            let else_block_id = self.body.insert_block(BasicBlock::from(
                Vec::new(),
                identifier_map_copy.clone(),
                ControlFlowEdge::Leaf,
                Some(branch_block_id),
                dominance_instruction_map_copy.clone(),
            ));

            self.cur_block = else_block_id;
            self.stat_sequence()?;
        }

        self.match_token(Token::Fi)?;

        let join_block_id = self.body.insert_block(BasicBlock::from(
            Vec::new(),
            identifier_map_copy.clone(),
            ControlFlowEdge::Leaf,
            Some(branch_block_id),
            dominance_instruction_map_copy.clone(),
        ));

        if is_else_present {
            self.get_block_mut(self.cur_block)
                .update_edge(ControlFlowEdge::Fallthrough(join_block_id));
        }

        self.get_block_mut(then_block_end_id)
            .update_edge(if is_else_present {
                ControlFlowEdge::Branch(join_block_id)
            } else {
                ControlFlowEdge::Fallthrough(join_block_id)
            });

        let else_block_end_id = self.cur_block;

        self.get_block_mut(branch_block_id)
            .push_instr_no_dom(Instruction::new(
                branch_instruction_id,
                Operator::Branch(
                    BranchOpcode::from(opposite_relop),
                    if is_else_present {
                        else_block_end_id
                    } else {
                        join_block_id
                    },
                    comparator_instruction_id.instruction_id,
                ),
                None,
            ));

        self.cur_block = join_block_id;

        let phi_node = self.join_blocks.pop().unwrap();

        for (id, temp_phi) in phi_node.phi_map.into_iter() {
            let new_instr_id = self.body.get_instruction_count() as i32;
            self.body.increment_instruction_count();

            let default_value = *identifier_map_copy.get(&id).unwrap_or(&0);

            // Temporary: Integrate better into handling of 0 values
            if default_value == 0
                && (temp_phi.left_identifier_id.is_none() || temp_phi.right_identifier_id.is_none())
            {
                self.const_body.insert_returning_id(0);
            }

            let operator = Operator::Phi(
                temp_phi.left_identifier_id.unwrap_or(default_value),
                temp_phi.right_identifier_id.unwrap_or(default_value),
            );

            let new_instr = Instruction::new(new_instr_id, operator, None);

            let block = self.get_block_mut(join_block_id);

            block.push_instr_no_dom(new_instr);
            block.insert_identifier(id, new_instr_id);

            for phi_node_temp in self.join_blocks.iter_mut() {
                if let Some(temp_phi) = phi_node_temp.phi_map.get_mut(&id) {
                    match phi_node_temp.side {
                        Side::Left => {
                            temp_phi.left_identifier_id = Some(new_instr_id);
                        }
                        Side::Right => {
                            temp_phi.right_identifier_id = Some(new_instr_id);
                        }
                    }
                } else {
                    phi_node_temp.phi_map.insert(
                        id,
                        match phi_node_temp.side {
                            Side::Left => PhiValues {
                                left_identifier_id: Some(new_instr_id),
                                right_identifier_id: None,
                            },
                            Side::Right => PhiValues {
                                left_identifier_id: None,
                                right_identifier_id: Some(new_instr_id),
                            },
                        },
                    );
                }
            }
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

                let instruction_id = self.expression()?.instruction_id;

                self.get_block_mut(self.cur_block)
                    .insert_identifier(identifier_id, instruction_id);

                if let Some(phi_node) = self.join_blocks.last_mut() {
                    let temp_phi =
                        phi_node
                            .phi_map
                            .entry(identifier_id)
                            .or_insert_with(|| PhiValues {
                                left_identifier_id: None,
                                right_identifier_id: None,
                            });

                    match phi_node.side {
                        Side::Left => {
                            temp_phi.left_identifier_id = Some(instruction_id);
                        }
                        Side::Right => {
                            temp_phi.right_identifier_id = Some(instruction_id);
                        }
                    }
                }

                Ok(())
            }
            _ => Err(Error::SyntaxError("Expected an identifier".to_string())),
        }
    }

    fn relation(&mut self) -> Result<(SimpleReturn, RelOp)> {
        let left = self.expression()?;

        match self
            .tokens
            .next()
            .ok_or_else(|| Error::UnexpectedEndOfFile)??
        {
            Token::RelOp(relop) => {
                let right = self.expression()?;
                Ok((
                    self.handle_binary_op(StoredBinaryOpcode::Cmp, left, right),
                    relop.opposite(),
                ))
            }
            _ => Err(Error::SyntaxError("Expected a relOp".to_string())),
        }
    }

    fn handle_binary_op(
        &mut self,
        operator: StoredBinaryOpcode,
        left: SimpleReturn,
        right: SimpleReturn,
    ) -> SimpleReturn {
        let new_instr_id = self.body.get_instruction_count() as i32;

        let operator_type = OperatorType::from(&operator);
        let mut new_instr = Instruction::new(
            new_instr_id,
            Operator::StoredBinaryOp(operator, left.instruction_id, right.instruction_id),
            None,
        );

        if let Some(dom_instr) = self
            .get_block_mut(self.cur_block)
            .get_dom_instr(&operator_type)
        {
            if let Some(dup_instr_id) = dom_instr.try_borrow().unwrap().check_dominators(&new_instr)
            {
                // return dup_instr_id;
                return SimpleReturn::new(dup_instr_id, None);
            }

            let dom_instr = self
                .get_block_mut(self.cur_block)
                .remove_dom_instr(operator_type.clone())
                .unwrap();

            new_instr.update_dom(dom_instr);
        }

        let new_instr = Rc::new(RefCell::new(new_instr));

        self.get_block_mut(self.cur_block)
            .push_instr(new_instr.clone(), operator_type);

        if let Some(identifier_id) = left.identifier_id {
            self.identifier_use_map
                .entry(identifier_id)
                .or_insert_with(Vec::new)
                .push(IdentifierUse::new(new_instr.clone(), Side::Left));
        }
        if let Some(identifier_id) = right.identifier_id {
            self.identifier_use_map
                .entry(identifier_id)
                .or_insert_with(Vec::new)
                // .push(new_instr.clone());
                .push(IdentifierUse::new(new_instr.clone(), Side::Right));
        }

        self.body.increment_instruction_count();

        // new_instr_id
        SimpleReturn::new(new_instr_id, None)
    }

    fn expression(&mut self) -> Result<SimpleReturn> {
        let mut left = self.term()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Add) => {
                    self.tokens.next();
                    let right = self.term()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Add, left, right);
                }
                Ok(Token::Sub) => {
                    self.tokens.next();
                    let right = self.term()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Sub, left, right);
                }

                _ => break,
            }
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<SimpleReturn> {
        let mut left = self.factor()?;

        while let Some(token) = self.tokens.peek() {
            match token {
                Ok(Token::Mul) => {
                    self.tokens.next();
                    let right = self.factor()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Mul, left, right);
                }
                Ok(Token::Div) => {
                    self.tokens.next();
                    let right = self.factor()?;
                    left = self.handle_binary_op(StoredBinaryOpcode::Div, left, right);
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<SimpleReturn> {
        match self
            .tokens
            .peek()
            .ok_or_else(|| Error::UnexpectedEndOfFile)?
        {
            Ok(Token::Identifier(id)) => {
                let identifier_id = *id;

                let instruction_id = self
                    .body
                    .get_mut_block(self.cur_block)
                    .unwrap()
                    .get_identifier(id)
                    .unwrap()
                    .clone();
                self.tokens.next();

                // Ok(instruction_id)
                Ok(SimpleReturn::new(instruction_id, Some(identifier_id)))
            }
            Ok(Token::Number(num)) => {
                let instruction_id = self.const_body.insert_returning_id(*num);
                self.tokens.next();

                // Ok(instruction_id)
                Ok(SimpleReturn::new(instruction_id, None))
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
    use std::{
        cell::RefCell,
        collections::{HashMap, HashSet},
        rc::Rc,
    };

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

        let body = BodyParser::parse(&mut tokens.into_iter().peekable(), &mut const_body);

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -2),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 0),
            Some(b0_insr_1.clone()),
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Sub, 2, -4),
            None,
        )));
        let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -3, -2),
            None,
        )));
        let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 4, -1),
            None,
        )));
        let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(
            6,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, 5),
            Some(b0_insr_2.clone()),
        )));
        let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
            7,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, 6),
            Some(b0_insr_6.clone()),
        )));
        let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(
            8,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Div, 7, -2),
            Some(b0_insr_5.clone()),
        )));
        let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(
            9,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, -1, 8),
            Some(b0_insr_4.clone()),
        )));

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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone()],
            HashMap::from([(1, 1), (2, 1)]),
            ControlFlowEdge::Leaf,
            None,
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -3),
            Some(b0_insr_1.clone()),
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 2),
            Some(b0_insr_2.clone()),
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1, b0_insr_2, b0_insr_3.clone()],
            HashMap::from([(1, 1), (2, 2), (3, 3), (4, 2)]),
            ControlFlowEdge::Leaf,
            None,
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2, 1),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 1
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -2)]),
            ControlFlowEdge::Branch(3),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 2
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -4)]),
            ControlFlowEdge::Fallthrough(3),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 3
        let join_block = BasicBlock::from(
            vec![Rc::new(RefCell::new(Instruction::new(
                3,
                Operator::Phi(-2, -4),
                None,
            )))],
            HashMap::from([(1, 3)]),
            ControlFlowEdge::Leaf,
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -4),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -1),
            None,
        )));
        let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::Branch(BranchOpcode::Ge, 2, 2),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2.clone(), b0_insr_3],
            HashMap::from([(1, 1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b0_insr_1.clone()),
            ]),
        );

        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -2, -1),
            Some(b0_insr_1.clone()),
        )));

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone()],
            HashMap::from([(1, 4), (2, 1)]),
            ControlFlowEdge::Branch(3),
            Some(0),
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
            HashMap::from([
                (OperatorType::Cmp, b0_insr_2.clone()),
                (OperatorType::Add, b0_insr_1.clone()),
            ]),
        );
        let join_block = BasicBlock::from(
            vec![
                Rc::new(RefCell::new(Instruction::new(5, Operator::Phi(4, 1), None))),
                Rc::new(RefCell::new(Instruction::new(6, Operator::Phi(1, 0), None))),
            ],
            HashMap::from([(1, 5), (2, 6)]),
            ControlFlowEdge::Leaf,
            Some(0),
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 2, 1),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -1), (2, -2), (3, -13)]),
            ControlFlowEdge::Branch(3),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -1), (2, -4)]),
            ControlFlowEdge::Fallthrough(3),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );
        let join_block = BasicBlock::from(
            vec![
                Rc::new(RefCell::new(Instruction::new(
                    3,
                    Operator::Phi(-2, -4),
                    None,
                ))),
                Rc::new(RefCell::new(Instruction::new(
                    4,
                    Operator::Phi(-13, 0),
                    None,
                ))),
            ],
            HashMap::from([(1, -1), (2, 3), (3, 4)]),
            ControlFlowEdge::Leaf,
            Some(0),
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ne, 2, 1),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 1
        let then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -2)]),
            ControlFlowEdge::Fallthrough(2),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 2
        let join_block = BasicBlock::from(
            vec![Rc::new(RefCell::new(Instruction::new(
                3,
                Operator::Phi(-2, -1),
                None,
            )))],
            HashMap::from([(1, 3)]),
            ControlFlowEdge::Leaf,
            Some(0),
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

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
            None,
        )));
        let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Le, 5, 1),
            None,
        )));

        let main_block = BasicBlock::from(
            vec![b0_insr_1.clone(), b0_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -3),
            Some(b0_insr_1.clone()),
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::Branch(BranchOpcode::Ge, 3, 3),
            None,
        )));

        let then_block = BasicBlock::from(
            vec![b1_insr_1.clone(), b1_insr_2],
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(2),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );

        // Block 2
        let sub_then_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -3)]),
            ControlFlowEdge::Branch(4),
            Some(1),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );

        // Block 3
        let sub_else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -4), (2, -12)]),
            ControlFlowEdge::Fallthrough(4),
            Some(1),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );

        // Block 4
        let sub_join_block = BasicBlock::from(
            vec![
                Rc::new(RefCell::new(Instruction::new(
                    5,
                    Operator::Phi(-3, -4),
                    None,
                ))),
                Rc::new(RefCell::new(Instruction::new(
                    6,
                    Operator::Phi(0, -12),
                    None,
                ))),
            ],
            HashMap::from([(1, 5), (2, 6)]),
            ControlFlowEdge::Branch(6),
            Some(1),
            HashMap::from([(OperatorType::Cmp, b1_insr_1.clone())]),
        );

        // Block 5
        let else_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -5)]),
            ControlFlowEdge::Fallthrough(6),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b0_insr_1.clone())]),
        );

        // Block 6
        let join_block = BasicBlock::from(
            vec![
                Rc::new(RefCell::new(Instruction::new(
                    7,
                    Operator::Phi(5, -5),
                    None,
                ))),
                Rc::new(RefCell::new(Instruction::new(8, Operator::Phi(6, 0), None))),
            ],
            HashMap::from([(1, 7), (2, 8)]),
            ControlFlowEdge::Leaf,
            Some(0),
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

    #[test]
    fn simple_while_loop() {
        /*
        let x <- 1;
        while x < 3 do
            let x <- x + 1;
        od
        */
        let tokens = [
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Number(1),
            Token::Semicolon,
            Token::While,
            Token::Identifier(1),
            Token::RelOp(RelOp::Lt),
            Token::Number(3),
            Token::Do,
            Token::Let,
            Token::Identifier(1),
            Token::Assignment,
            Token::Identifier(1),
            Token::Add,
            Token::Number(1),
            Token::Semicolon,
            Token::Od,
        ];

        let mut const_body = ConstBody::new();

        let body = BodyParser::parse(
            &mut tokens.map(|t| Ok(t)).into_iter().peekable(),
            &mut const_body,
        );

        // Block 0
        let main_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, -1)]),
            ControlFlowEdge::Fallthrough(1),
            None,
            HashMap::new(),
        );

        // Block 1
        let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
            5,
            Operator::Phi(-1, 3),
            None,
        )));
        let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
            1,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 5, -3),
            None,
        )));
        let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
            2,
            Operator::Branch(BranchOpcode::Ge, 3, 1),
            None,
        )));

        let join_block = BasicBlock::from(
            vec![b1_insr_1, b1_insr_2.clone(), b1_insr_3],
            HashMap::from([(1, 5)]),
            ControlFlowEdge::Fallthrough(2),
            Some(0),
            HashMap::from([(OperatorType::Cmp, b1_insr_2.clone())]),
        );

        // Block 2
        let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
            3,
            Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 5, -1),
            None,
        )));
        let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
            4,
            Operator::UnconditionalBranch(1),
            None,
        )));

        let body_block = BasicBlock::from(
            vec![b2_insr_1.clone(), b2_insr_2],
            HashMap::from([(1, 3)]),
            ControlFlowEdge::Branch(1),
            Some(1),
            HashMap::from([
                (OperatorType::Cmp, b1_insr_2.clone()),
                (OperatorType::Add, b2_insr_1),
            ]),
        );

        // Block 3
        let escape_block = BasicBlock::from(
            Vec::new(),
            HashMap::from([(1, 5)]),
            ControlFlowEdge::Leaf,
            Some(1),
            HashMap::from([(OperatorType::Cmp, b1_insr_2)]),
        );

        let expected_body =
            Body::from(0, vec![main_block, join_block, body_block, escape_block], 6);

        let expected_const_body = ConstBody::from(HashSet::from([1, 3]));

        assert_eq!(body, expected_body);
        assert_eq!(const_body, expected_const_body);
    }
}
