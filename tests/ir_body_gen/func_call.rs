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
fn simple_predefined_function_call() {
    let input = r#"
    main {
        let x <- call InputNum();
        call OutputNum(x);
        call OutputNewLine;
        let y <- call InputNum;
        call OutputNewLine()
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Write(1), None)));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::WriteNL, None)));
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Read, None)));
    let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(5, Operator::WriteNL, None)));
    let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 4)]);
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![
            b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6,
        ],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::new());

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn simple_user_defined_functions() {
    let input = r#"
    main {
        let a <- 1;
        let b <- call user1(a);
        let c <- call user2;
        call user3();
        let d <- call user4();
        call user5;
        let e <- call user6(a, b, c)
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::SetPar { idx: 1, val: -1 },
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Jsr(3), None)));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::Jsr(5), None)));
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Jsr(6), None)));
    let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(5, Operator::Jsr(8), None)));
    let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(6, Operator::Jsr(9), None)));
    let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::SetPar { idx: 1, val: -1 },
        None,
    )));
    let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::SetPar { idx: 2, val: 2 },
        None,
    )));
    let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(
        9,
        Operator::SetPar { idx: 3, val: 3 },
        None,
    )));
    let b0_insr_10 = Rc::new(RefCell::new(Instruction::new(10, Operator::Jsr(11), None)));
    let b0_insr_11 = Rc::new(RefCell::new(Instruction::new(11, Operator::End, None)));

    let main_block_identifier_map =
        InheritingHashMap::from_iter([(1, -1), (2, 2), (4, 3), (7, 5), (10, 10)]);
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![
            b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5, b0_insr_6, b0_insr_7, b0_insr_8,
            b0_insr_9, b0_insr_10, b0_insr_11,
        ],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::from([1]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn void_func_with_return() {
    let input = r#"
    main

    function temp(x); {
        let x <- x + 1;
        return
    };

    {
        let a <- 1;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_func(&computation.funcs[0], &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::GetPar { idx: 1 },
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::Ret(None), None)));

    let func_block_identifier_map: InheritingHashMap<u32, i32> =
        InheritingHashMap::from_iter([(2, 2)]);
    let func_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_2.clone())]);

    let func_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2, b0_insr_3],
        func_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        func_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![func_block]);
    let expected_const_body = ConstBlock::from(HashSet::from([1]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
