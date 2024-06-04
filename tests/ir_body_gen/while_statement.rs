use std::{cell::RefCell, collections::HashSet, rc::Rc};

use pretty_assertions_sorted::assert_eq_sorted;
use tiny::{
    config::Config,
    ir::{
        block::{BasicBlock, Body, ControlFlowEdge},
        gen::IrBodyGenerator,
        inheriting_hashmap::InheritingHashMap,
        instruction::{BranchOpcode, Instruction, Operator, StoredBinaryOpcode},
        ConstBlock,
    },
    lexer::Tokenizer,
    parser::Parser,
};

#[test]
fn simple_while_loop() {
    let input = r#"
    main {
        let x <- 1;
        while x < 3 do
            let x <- x + 1;
        od
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

    // Block 0
    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);

    let main_block = BasicBlock::from(
        Vec::new(),
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        InheritingHashMap::new(),
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::Phi(-1, 4),
        None,
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
        None,
    )));
    let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Branch(BranchOpcode::Ge, 3.into(), 2),
        None,
    )));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 1);
    let mut join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
    join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_2.clone());

    let join_block = BasicBlock::from(
        vec![b1_insr_1.clone(), b1_insr_2.clone(), b1_insr_3],
        join_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::UnconditionalBranch(1.into()),
        None,
    )));

    let mut body_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    body_block_identifier_map.insert(1, 4);
    let mut body_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
    body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

    let body_block = BasicBlock::from(
        vec![b2_insr_1.clone(), b2_insr_2],
        body_block_identifier_map,
        Some(ControlFlowEdge::Branch(1.into())),
        Some(1.into()),
        body_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

    let escape_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    let escape_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

    let escape_block = BasicBlock::from(
        vec![b3_insr_1],
        escape_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        escape_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![main_block, join_block, body_block, escape_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn nested_while_loop() {
    let input = r#"
    main {
        let i <- 0;
        let x <- 0;
        let y <- 0;
        let j <- i;
        while x < 10 do
            let x <- i + 1;
            let y <- j + 1;
            while j < 10 do
                let x <- j + 1;
                let y <- i + 1;
                let j <- j + 1
            od;
            let i <- i + 1
        od;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

    // Block 0
    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 0), (2, 0), (3, 0), (4, 0)]);

    let main_block = BasicBlock::from(
        Vec::new(),
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        InheritingHashMap::new(),
    );

    // Block 1

    // i
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Phi(0, 7), None)));

    // x
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Phi(0, 9), None)));

    // y
    let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Phi(0, 10),
        None,
    )));

    // j
    let b1_insr_4 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Phi(0, 11),
        None,
    )));

    let b1_insr_5 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 2, -10),
        None,
    )));
    let b1_insr_6 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::Branch(BranchOpcode::Ge, 6.into(), 5),
        None,
    )));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 1);
    join_block_identifier_map.insert(2, 2);
    join_block_identifier_map.insert(3, 3);
    join_block_identifier_map.insert(4, 4);
    let mut join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
    join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_5.clone());

    let join_block = BasicBlock::from(
        vec![
            b1_insr_1.clone(),
            b1_insr_2.clone(),
            b1_insr_3.clone(),
            b1_insr_4.clone(),
            b1_insr_5.clone(),
            b1_insr_6.clone(),
        ],
        join_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 4, -1),
        Some(b2_insr_1.clone()),
    )));

    let mut body_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    body_block_identifier_map.insert(2, 7);
    body_block_identifier_map.insert(3, 8);
    let mut body_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
    body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_2.clone());

    let body_block = BasicBlock::from(
        vec![b2_insr_1.clone(), b2_insr_2.clone()],
        body_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(1.into()),
        body_block_dom_instr_map,
    );

    // Block 3

    // x
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
        9,
        Operator::Phi(7, 14),
        None,
    )));

    // y
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(
        10,
        Operator::Phi(8, 7),
        None,
    )));

    // j
    let b3_insr_3 = Rc::new(RefCell::new(Instruction::new(
        11,
        Operator::Phi(4, 14),
        None,
    )));

    let b3_insr_4 = Rc::new(RefCell::new(Instruction::new(
        12,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 11, -10),
        Some(b1_insr_5.clone()),
    )));
    let b3_insr_5 = Rc::new(RefCell::new(Instruction::new(
        13,
        Operator::Branch(BranchOpcode::Ge, 5.into(), 12),
        None,
    )));

    let mut join_block_2_identifier_map =
        InheritingHashMap::with_dominator(body_block.get_identifier_map());
    join_block_2_identifier_map.insert(2, 9);
    join_block_2_identifier_map.insert(3, 10);
    join_block_2_identifier_map.insert(4, 11);
    let mut join_block_2_dom_instr_map =
        InheritingHashMap::with_dominator(body_block.get_dom_instr_map());
    join_block_2_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b3_insr_4.clone());

    let join_block_2 = BasicBlock::from(
        vec![
            b3_insr_1.clone(),
            b3_insr_2.clone(),
            b3_insr_3.clone(),
            b3_insr_4.clone(),
            b3_insr_5.clone(),
        ],
        join_block_2_identifier_map,
        Some(ControlFlowEdge::Fallthrough(4.into())),
        Some(2.into()),
        join_block_2_dom_instr_map,
    );

    // Block 4
    let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
        14,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 11, -1),
        Some(b2_insr_2.clone()),
    )));
    let b4_insr_4 = Rc::new(RefCell::new(Instruction::new(
        15,
        Operator::UnconditionalBranch(3.into()),
        None,
    )));

    let mut body_block_2_identifier_map =
        InheritingHashMap::with_dominator(join_block_2.get_identifier_map());
    body_block_2_identifier_map.insert(2, 14);
    body_block_2_identifier_map.insert(3, 7);
    body_block_2_identifier_map.insert(4, 14);
    let mut body_block_2_dom_instr_map =
        InheritingHashMap::with_dominator(join_block_2.get_dom_instr_map());
    body_block_2_dom_instr_map.insert(StoredBinaryOpcode::Add, b4_insr_1.clone());

    let body_block_2 = BasicBlock::from(
        vec![b4_insr_1.clone(), b4_insr_4.clone()],
        body_block_2_identifier_map,
        Some(ControlFlowEdge::Branch(3.into())),
        Some(3.into()),
        body_block_2_dom_instr_map,
    );

    // Block 5
    let b5_insr_1 = Rc::new(RefCell::new(Instruction::new(
        16,
        Operator::UnconditionalBranch(1.into()),
        None,
    )));

    let mut escape_block_2_identifier_map =
        InheritingHashMap::with_dominator(join_block_2.get_identifier_map());
    escape_block_2_identifier_map.insert(1, 7);
    let escape_block_2_dom_instr_map =
        InheritingHashMap::with_dominator(join_block_2.get_dom_instr_map());

    let escape_block2 = BasicBlock::from(
        vec![b5_insr_1.clone()],
        escape_block_2_identifier_map,
        Some(ControlFlowEdge::Branch(1.into())),
        Some(3.into()),
        escape_block_2_dom_instr_map,
    );

    // Block 6
    let b6_insr_1 = Rc::new(RefCell::new(Instruction::new(17, Operator::End, None)));

    let escape_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    let escape_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

    let escape_block = BasicBlock::from(
        vec![b6_insr_1],
        escape_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        escape_block_dom_instr_map,
    );

    let expected_body = Body::from(vec![
        main_block,
        join_block,
        body_block,
        join_block_2,
        body_block_2,
        escape_block2,
        escape_block,
    ]);

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 10]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
