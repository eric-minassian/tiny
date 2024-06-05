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

    let expected_body = Body::from(vec![main_block]);

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

    let expected_body = Body::from(vec![main_block]);

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

    let expected_body = Body::from(vec![main_block, join_block, body_block, escape_block]);

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

    let expected_body = Body::from(vec![branch_block, then_block, else_block, join_block]);

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

    let expected_body = Body::from(vec![branch_block, then_block, else_block]);

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

    let expected_body = Body::from(vec![
        branch_block,
        then_block,
        then_then_block,
        then_else_block,
        else_block,
        else_then_block,
        else_else_block,
        join_else_block,
        join_block,
    ]);

    let expected_const_body = ConstBlock::from(HashSet::from([1, 2, 3]));

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

    let expected_body = Body::from(vec![main_block]);
    let expected_const_body = ConstBlock::from(HashSet::new());

    assert_eq_sorted!(body, expected_body);
    assert_eq_sorted!(const_body, expected_const_body);
}
