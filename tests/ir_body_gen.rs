use std::{cell::RefCell, collections::HashSet, rc::Rc};

use pretty_assertions_sorted::assert_eq_sorted;
use tiny::{
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
    let expected_body = Body::from(Some(0.into()), vec![main_block]);

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
    let expected_body = Body::from(Some(0.into()), vec![main_block]);

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
    let expected_body = Body::from(Some(0.into()), vec![main_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn simple_branch() {
    let input = r#"
    main {
        let x <- 1;
        if x < 1 then
            let x <- 2;
        else
            let x <- 4;
        fi;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::Branch(BranchOpcode::Ge, 2.into(), 1),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::UnconditionalBranch(3.into()),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    then_block_identifier_map.insert(1, -2);
    let then_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let then_block = BasicBlock::from(
        vec![b1_insr_1],
        then_block_identifier_map,
        Some(ControlFlowEdge::Branch(3.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    else_block_identifier_map.insert(1, -4);
    let else_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let else_block = BasicBlock::from(
        Vec::new(),
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Phi(-2, -4),
        None,
    )));
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(5, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 4);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b3_insr_1, b3_insr_2],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![main_block, then_block, else_block, join_block],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([1, 2, 4]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn simple_duplicate_branch() {
    let input = r#"
    main {
        let x <- 1 + 4;
        if x < 1 then
            let x <- 2 + 1;
            let y <- 1 + 4;
        else
            let x <- 1 + 4;
        fi;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
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
        Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1)]);
    let main_block_dom_instr_map = InheritingHashMap::from_iter([
        (StoredBinaryOpcode::Add, b0_insr_1.clone()),
        (StoredBinaryOpcode::Cmp, b0_insr_2.clone()),
    ]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1.clone(), b0_insr_2.clone(), b0_insr_3],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -2, -1),
        Some(b0_insr_1.clone()),
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::UnconditionalBranch(3.into()),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    then_block_identifier_map.insert(1, 4);
    then_block_identifier_map.insert(2, 1);
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1.clone(), b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Branch(3.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    else_block_identifier_map.insert(1, 1);
    let else_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let else_block = BasicBlock::from(
        Vec::new(),
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(6, Operator::Phi(4, 1), None)));
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(7, Operator::Phi(1, 0), None)));
    let b3_insr_3 = Rc::new(RefCell::new(Instruction::new(8, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 6);
    join_block_identifier_map.insert(2, 7);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b3_insr_1, b3_insr_2, b3_insr_3],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![main_block, then_block, else_block, join_block],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 2, 4]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn new_identifiers_branch() {
    let input = r#"
    main {
        let x <- 1;
        if x < 1 then
            let y <- 2;
            let z <- 13;
        else
            let y <- 4;
        fi;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::Branch(BranchOpcode::Ge, 2.into(), 1),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1.clone(), b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::UnconditionalBranch(3.into()),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    then_block_identifier_map.insert(2, -2);
    then_block_identifier_map.insert(3, -13);
    let then_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let then_block = BasicBlock::from(
        vec![b1_insr_1],
        then_block_identifier_map,
        Some(ControlFlowEdge::Branch(3.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    else_block_identifier_map.insert(2, -4);
    let else_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let else_block = BasicBlock::from(
        Vec::new(),
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Phi(-2, -4),
        None,
    )));
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::Phi(-13, 0),
        None,
    )));
    let b3_insr_3 = Rc::new(RefCell::new(Instruction::new(6, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(2, 4);
    join_block_identifier_map.insert(3, 5);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b3_insr_1, b3_insr_2, b3_insr_3],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![main_block, then_block, else_block, join_block],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 2, 4, 13]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn branch_without_else_statement() {
    let input = r#"
    main {
        let x <- 1;
        if x == 1 then
            let x <- 2;
        fi;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::Branch(BranchOpcode::Ne, 2.into(), 1),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1.clone(), b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    then_block_identifier_map.insert(1, -2);
    let then_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let then_block = BasicBlock::from(
        Vec::new(),
        then_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Phi(-2, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(4, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 3);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b2_insr_1, b2_insr_2],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(Some(0.into()), vec![main_block, then_block, join_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 2]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn nested_if_statements() {
    let input = r#"
    main {
        let x <- 1;
        if x > 1 then
            if x < 3 then
                let x <- 3;
            else
                let x <- 4;
                let y <- 12;
            fi;
        else
            let x <- 5;
        fi;
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(
        1,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -1),
        None,
    )));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::Branch(BranchOpcode::Le, 5.into(), 1),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, -1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_1.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1.clone(), b0_insr_2],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, -1, -3),
        Some(b0_insr_1),
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Branch(BranchOpcode::Ge, 3.into(), 3),
        None,
    )));

    let then_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::UnconditionalBranch(4.into()),
        None,
    )));

    let mut sub_then_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    sub_then_block_identifier_map.insert(1, -3);
    let sub_then_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

    let sub_then_block = BasicBlock::from(
        vec![b2_insr_1],
        sub_then_block_identifier_map,
        Some(ControlFlowEdge::Branch(4.into())),
        Some(1.into()),
        sub_then_block_dom_instr_map,
    );

    // Block 3
    let mut sub_else_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    sub_else_block_identifier_map.insert(1, -4);
    sub_else_block_identifier_map.insert(2, -12);
    let sub_else_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

    let sub_else_block = BasicBlock::from(
        Vec::new(),
        sub_else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(4.into())),
        Some(1.into()),
        sub_else_block_dom_instr_map,
    );

    // Block 4
    let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::Phi(-3, -4),
        None,
    )));
    let b4_insr_2 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::Phi(0, -12),
        None,
    )));
    let b4_insr_3 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::UnconditionalBranch(6.into()),
        None,
    )));

    let mut sub_join_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    sub_join_block_identifier_map.insert(1, 6);
    sub_join_block_identifier_map.insert(2, 7);
    let sub_join_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

    let sub_join_block = BasicBlock::from(
        vec![b4_insr_1, b4_insr_2, b4_insr_3],
        sub_join_block_identifier_map,
        Some(ControlFlowEdge::Branch(6.into())),
        Some(1.into()),
        sub_join_block_dom_instr_map,
    );

    // Block 5
    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    else_block_identifier_map.insert(1, -5);
    let else_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let else_block = BasicBlock::from(
        Vec::new(),
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(6.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 6
    let b6_insr_1 = Rc::new(RefCell::new(Instruction::new(
        9,
        Operator::Phi(6, -5),
        None,
    )));
    let b6_insr_2 = Rc::new(RefCell::new(Instruction::new(
        10,
        Operator::Phi(7, 0),
        None,
    )));
    let b6_insr_3 = Rc::new(RefCell::new(Instruction::new(11, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    join_block_identifier_map.insert(1, 9);
    join_block_identifier_map.insert(2, 10);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b6_insr_1, b6_insr_2, b6_insr_3],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![
            main_block,
            then_block,
            sub_then_block,
            sub_else_block,
            sub_join_block,
            else_block,
            join_block,
        ],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 3, 4, 5, 12]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

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
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

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

    let expected_body = Body::from(
        Some(0.into()),
        vec![main_block, join_block, body_block, escape_block],
    );

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
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

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

    let expected_body = Body::from(
        Some(0.into()),
        vec![
            main_block,
            join_block,
            body_block,
            join_block_2,
            body_block_2,
            escape_block2,
            escape_block,
        ],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 10]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn simple_return() {
    let input = r#"
    main
    
    function temp(); {
        let x <- 1;
        return x
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
        Operator::Ret(Some(-1)),
        None,
    )));

    let main_block_identifier_map = InheritingHashMap::from_iter([(2, -1)]);
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![b0_insr_1],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(Some(0.into()), vec![main_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn simple_void_return() {
    let input = r#"
    main

    function temp(); {
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
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Ret(None), None)));

    let main_block_identifier_map = InheritingHashMap::new();
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![b0_insr_1],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(Some(0.into()), vec![main_block]);

    let expected_const_body = ConstBlock::new();

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn loop_body_return() {
    let input = r#"
    main

    function temp(); {
        let x <- call InputNum;
        let y <- x;
        while x < 3 do
            let y <- y + 1;
            return x;
            let x <- x + 3
        od;
        return 1
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
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(2, 1), (3, 1)]);
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![b0_insr_1],
        main_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        main_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
        None,
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Branch(BranchOpcode::Ge, 3.into(), 2),
        None,
    )));

    let join_block_identifier_map =
        InheritingHashMap::with_dominator(main_block.get_identifier_map());
    let mut join_block_dom_instr_map =
        InheritingHashMap::with_dominator(main_block.get_dom_instr_map());
    join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_1.clone());

    let join_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
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
        Operator::Ret(Some(1)),
        None,
    )));

    let mut body_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    body_block_identifier_map.insert(3, 4);
    let mut body_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
    body_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

    let body_block = BasicBlock::from(
        vec![b2_insr_1, b2_insr_2],
        body_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        body_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::Ret(Some(-1)),
        None,
    )));

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

    let expected_body = Body::from(
        Some(0.into()),
        vec![main_block, join_block, body_block, escape_block],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn single_branch_return() {
    let input = r#"
    main

    function temp(); {
        let x <- call InputNum;
        if x < 3 then
            let x <- x + 1;
            return x
        else
            let x <- x + 1
        fi;
        return 1
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
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
        None,
    )));

    let branch_block_identifier_map = InheritingHashMap::from_iter([(2, 1)]);
    let branch_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_2.clone())]);

    let branch_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2, b0_insr_3],
        branch_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        branch_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::Ret(Some(4)),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    then_block_identifier_map.insert(2, 4);
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));

    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    else_block_identifier_map.insert(2, 6);
    let mut else_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

    let else_block = BasicBlock::from(
        vec![b2_insr_1],
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(7, Operator::Phi(1, 6), None)));
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::Ret(Some(-1)),
        None,
    )));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    join_block_identifier_map.insert(2, 7);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b3_insr_1, b3_insr_2],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![branch_block, then_block, else_block, join_block],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn both_branch_return() {
    let input = r#"
    main

    function temp(); {
        let x <- call InputNum;
        if x < 3 then
            let x <- x + 1;
            return x
        else
            let x <- x + 1;
            return x;
            let x <- x + 3;
        fi;
        return 1
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
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Branch(BranchOpcode::Ge, 2.into(), 2),
        None,
    )));

    let branch_block_identifier_map = InheritingHashMap::from_iter([(2, 1)]);
    let branch_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_2.clone())]);

    let branch_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2, b0_insr_3],
        branch_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        branch_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::Ret(Some(4)),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    then_block_identifier_map.insert(2, 4);
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::Ret(Some(6)),
        None,
    )));

    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    else_block_identifier_map.insert(2, 6);
    let mut else_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

    let else_block = BasicBlock::from(
        vec![b2_insr_1, b2_insr_2],
        else_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    let expected_body = Body::from(Some(0.into()), vec![branch_block, then_block, else_block]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn nested_both_branch_return() {
    let input = r#"
    main

    function temp(); {
        let x <- call InputNum;
        if x < 3 then
            if x > 1 then
                let x <- x + 1;
                return x;
            else
                return 1
            fi;
            let x <- x + 2;
            return x
        else
            if x < 2 then
                return x
            else
                let x <- x + 1;
            fi;
        fi;
        return x
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
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -3),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::Branch(BranchOpcode::Ge, 4.into(), 2),
        None,
    )));

    let branch_block_identifier_map = InheritingHashMap::from_iter([(2, 1)]);
    let branch_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Cmp, b0_insr_2.clone())]);

    let branch_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2.clone(), b0_insr_3],
        branch_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        branch_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -1),
        Some(b0_insr_2.clone()),
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::Branch(BranchOpcode::Le, 3.into(), 4),
        None,
    )));

    let then_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::Ret(Some(6)),
        None,
    )));

    let mut then_then_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    then_then_block_identifier_map.insert(2, 6);
    let mut then_then_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());
    then_then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());

    let then_then_block = BasicBlock::from(
        vec![b2_insr_1, b2_insr_2],
        then_then_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        then_then_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::Ret(Some(-1)),
        None,
    )));

    let then_else_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    let then_else_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());

    let then_else_block = BasicBlock::from(
        vec![b3_insr_1],
        then_else_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        then_else_block_dom_instr_map,
    );

    // Block 4
    let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
        9,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, -2),
        Some(b0_insr_2.clone()),
    )));
    let b4_insr_2 = Rc::new(RefCell::new(Instruction::new(
        10,
        Operator::Branch(BranchOpcode::Ge, 6.into(), 9),
        None,
    )));

    let else_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    let mut else_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    else_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b4_insr_1.clone());

    let else_block = BasicBlock::from(
        vec![b4_insr_1, b4_insr_2],
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(5.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 5
    let b5_insr_1 = Rc::new(RefCell::new(Instruction::new(
        11,
        Operator::Ret(Some(1)),
        None,
    )));

    let else_then_block_identifier_map =
        InheritingHashMap::with_dominator(else_block.get_identifier_map());
    let else_then_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_block.get_dom_instr_map());

    let else_then_block = BasicBlock::from(
        vec![b5_insr_1],
        else_then_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(4.into()),
        else_then_block_dom_instr_map,
    );

    // Block 6
    let b6_insr_1 = Rc::new(RefCell::new(Instruction::new(
        12,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, -1),
        None,
    )));

    let mut else_else_block_identifier_map =
        InheritingHashMap::with_dominator(else_block.get_identifier_map());
    else_else_block_identifier_map.insert(2, 12);
    let mut else_else_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_block.get_dom_instr_map());
    else_else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b6_insr_1.clone());

    let else_else_block = BasicBlock::from(
        vec![b6_insr_1],
        else_else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(7.into())),
        Some(4.into()),
        else_else_block_dom_instr_map,
    );

    // Block 7
    let b7_insr_1 = Rc::new(RefCell::new(Instruction::new(
        13,
        Operator::Phi(1, 12),
        None,
    )));

    let mut join_else_block_identifier_map =
        InheritingHashMap::with_dominator(else_block.get_identifier_map());
    join_else_block_identifier_map.insert(2, 13);
    let join_else_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_block.get_dom_instr_map());

    let join_else_block = BasicBlock::from(
        vec![b7_insr_1],
        join_else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(8.into())),
        Some(4.into()),
        join_else_block_dom_instr_map,
    );

    // Block 8
    let b8_insr_1 = Rc::new(RefCell::new(Instruction::new(
        14,
        Operator::Phi(1, 13),
        None,
    )));
    let b8_insr_2 = Rc::new(RefCell::new(Instruction::new(
        15,
        Operator::Ret(Some(14)),
        None,
    )));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    join_block_identifier_map.insert(2, 14);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b8_insr_1, b8_insr_2],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let expected_body = Body::from(
        Some(0.into()),
        vec![
            branch_block,
            then_block,
            then_then_block,
            then_else_block,
            else_block,
            else_then_block,
            else_else_block,
            join_else_block,
            join_block,
        ],
    );

    let expected_const_body = ConstBlock::from(HashSet::from([1, 2, 3]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

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

    let expected_body = Body::from(Some(0.into()), vec![main_block]);
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

    let expected_body = Body::from(Some(0.into()), vec![main_block]);
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

    let expected_body = Body::from(Some(0.into()), vec![func_block]);
    let expected_const_body = ConstBlock::from(HashSet::from([1]));

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

    let expected_body = Body::from(Some(0.into()), vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::from([0, 1]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}

#[test]
fn main_return() {
    let input = r#"
    main {
        return
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::new();
    let main_block_dom_instr_map = InheritingHashMap::new();

    let main_block = BasicBlock::from(
        vec![b0_insr_1],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let expected_body = Body::from(Some(0.into()), vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::new());

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
