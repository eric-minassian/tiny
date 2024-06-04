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
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

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

    let expected_body = Body::from(vec![main_block, then_block, else_block, join_block]);

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
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

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

    let expected_body = Body::from(vec![main_block, then_block, else_block, join_block]);

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
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

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

    let expected_body = Body::from(vec![main_block, then_block, else_block, join_block]);

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
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

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

    let expected_body = Body::from(vec![main_block, then_block, join_block]);

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
    let config = Config::default();
    let computation = Parser::parse(tokens, &config).unwrap();

    let mut const_body = ConstBlock::new();
    let body = IrBodyGenerator::generate_main(&computation, &mut const_body, &config);

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

    let expected_body = Body::from(vec![
        main_block,
        then_block,
        sub_then_block,
        sub_else_block,
        sub_join_block,
        else_block,
        join_block,
    ]);

    let expected_const_body = ConstBlock::from(HashSet::from([0, 1, 3, 4, 5, 12]));

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
