use std::{cell::RefCell, collections::HashSet, rc::Rc};

use pretty_assertions_sorted::assert_eq_sorted;
use tiny::{
    ir::{
        block::{BasicBlock, Body, ControlFlowEdge},
        gen::IrBodyGenerator,
        inheriting_hashmap::InheritingHashMap,
        instruction::{Instruction, Operator, StoredBinaryOpcode},
        ConstBlock,
    },
    lexer::Tokenizer,
    parser::Parser,
};

#[test]
fn simple_assignment() {
    let input = r#"
    main {
        let x <- 1;
        let y <- 1 + 2 + 0 - 4;
        let z <- 3 * 2 / 1;
        let w <- y + z;
        let w <- x * ((1 + w) / 2);
    }.
    "#;
    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_block = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_block);

    // Block 0
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
    let b0_insr_10 = Rc::new(RefCell::new(Instruction::new(10, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1), (2, 3), (3, 5), (4, 9)]);
    let main_block_dom_instr_map = InheritingHashMap::from_iter([
        (StoredBinaryOpcode::Add, b0_insr_7.clone()),
        (StoredBinaryOpcode::Mul, b0_insr_9.clone()),
        (StoredBinaryOpcode::Div, b0_insr_8.clone()),
        (StoredBinaryOpcode::Sub, b0_insr_3.clone()),
    ]);

    let main_block = BasicBlock::from(
        vec![
            b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6, b0_insr_7, b0_insr_8,
            b0_insr_9, b0_insr_10,
        ],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );
    let expected_body = Body::from(vec![main_block]);

    let expected_const_block = ConstBlock::from(HashSet::from([0, 1, 2, 3, 4]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_block, expected_const_block);
}

#[test]
fn simple_duplicate_assignment() {
    let input = r#"
    main {
        let x <- 1 + 3;
        let y <- 1 + 3;
    }.
    "#;
    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_block = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_block);

    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, -3),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );
    let expected_body = Body::from(vec![main_block]);

    let expected_const_block = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_block, expected_const_block);
}

#[test]
fn identifier_duplicate_assignment() {
    let input = r#"
    main {
        let x <- 1 + 3;
        let y <- x + 3;
        let a <- x + y;
        let z <- x + 3;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

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
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 2), (3, 3), (4, 2)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_3.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );
    let expected_body = Body::from(vec![main_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn undeclared_identifier() {
    let input = r#"
    main {
        let x <- y + 1
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 0, -1),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 0)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::from([0, 1]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
