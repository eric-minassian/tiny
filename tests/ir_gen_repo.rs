use pretty_assertions_sorted::assert_eq_sorted;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};
use tiny::{
    ir::{
        block::{BasicBlock, Body, ControlFlowEdge},
        gen::IrGenerator,
        inheriting_hashmap::InheritingHashMap,
        instruction::{BranchOpcode, Instruction, Operator, StoredBinaryOpcode},
        ConstBlock, IrStore,
    },
    lexer::Tokenizer,
    parser::Parser,
};

#[test]
fn commutative_cse() {
    let input = r#"
    main {
        let a <- call InputNum();
        let b <- call InputNum();
        let c <- call InputNum();
        let d <- call InputNum();
    
        call OutputNum(a * b + c * d);
        call OutputNum(d * c + b * a)
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let ir_store = IrGenerator::generate(&computation);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Read, None)));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(3, Operator::Read, None)));
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Read, None)));
    let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 1, 2),
        None,
    )));
    let b0_insr_6 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 3, 4),
        Some(b0_insr_5.clone()),
    )));
    let b0_insr_7 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 5, 6),
        None,
    )));
    let b0_insr_8 = Rc::new(RefCell::new(Instruction::new(8, Operator::Write(7), None)));
    let b0_insr_9 = Rc::new(RefCell::new(Instruction::new(9, Operator::Write(7), None)));
    let b0_insr_10 = Rc::new(RefCell::new(Instruction::new(10, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 2), (3, 3), (4, 4)]);
    let main_block_dom_instr_map = InheritingHashMap::from_iter([
        (StoredBinaryOpcode::Mul, b0_insr_6.clone()),
        (StoredBinaryOpcode::Add, b0_insr_7.clone()),
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

    let main_body = Body::from(Some(0.into()), vec![main_block]);
    let const_block = ConstBlock::from(HashSet::new());

    let expected_ir = IrStore::from(
        HashMap::from_iter([("main".to_string(), main_body)]),
        const_block,
    );

    assert_eq_sorted!(ir_store, expected_ir);
}

#[test]
fn commutative_add_constant_prop() {
    let input = r#"
    main
    var x;
    {
        let x <- call InputNum();
        call OutputNum(1+x+1);
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let ir_store = IrGenerator::generate(&computation);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, -1, 1),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 2, -1),
        Some(b0_insr_2.clone()),
    )));
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(4, Operator::Write(3), None)));
    let b0_insr_5 = Rc::new(RefCell::new(Instruction::new(5, Operator::End, None)));

    let main_block_identifier_map = InheritingHashMap::from_iter([(1, 1)]);
    let main_block_dom_instr_map =
        InheritingHashMap::from_iter([(StoredBinaryOpcode::Add, b0_insr_3.clone())]);

    let main_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2, b0_insr_3, b0_insr_4, b0_insr_5],
        main_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        None,
        main_block_dom_instr_map,
    );

    let main_body = Body::from(Some(0.into()), vec![main_block]);
    let const_block = ConstBlock::from(HashSet::from_iter([1]));

    let expected_ir = IrStore::from(
        HashMap::from_iter([("main".to_string(), main_body)]),
        const_block,
    );

    assert_eq_sorted!(ir_store, expected_ir);
}

#[test]
fn copy_propagation() {
    let input = r#"
    main
    var a,b,c,d,e; {
        let a <- call InputNum();
        let b <- a;
        let c <- b;
        let d <- b + c;
        let e <- a + b;
        if a < 0 then
            let d <-  d + e;
            let a <- d
        else
            let d <- e
        fi;
        call OutputNum(a)
    }.
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let ir_store = IrGenerator::generate(&computation);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(
        2,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 1, 1),
        None,
    )));
    let b0_insr_3 = Rc::new(RefCell::new(Instruction::new(
        3,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 1, 0),
        None,
    )));
    let b0_insr_4 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Branch(BranchOpcode::Ge, 2.into(), 3),
        None,
    )));

    let branch_block_identifier_map =
        InheritingHashMap::from_iter([(1, 1), (2, 1), (3, 1), (4, 2), (5, 2)]);
    let branch_block_dom_instr_map = InheritingHashMap::from_iter([
        (StoredBinaryOpcode::Add, b0_insr_2.clone()),
        (StoredBinaryOpcode::Cmp, b0_insr_3.clone()),
    ]);

    let branch_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2.clone(), b0_insr_3, b0_insr_4],
        branch_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        branch_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 2, 2),
        Some(b0_insr_2),
    )));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::UnconditionalBranch(3.into()),
        None,
    )));

    let mut then_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    then_block_identifier_map.insert(1, 5);
    then_block_identifier_map.insert(4, 5);
    let mut then_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    then_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b1_insr_1.clone());

    let then_block = BasicBlock::from(
        vec![b1_insr_1, b1_insr_2],
        then_block_identifier_map,
        Some(ControlFlowEdge::Branch(3.into())),
        Some(0.into()),
        then_block_dom_instr_map,
    );

    // Block 2
    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    else_block_identifier_map.insert(4, 2);
    let else_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let else_block = BasicBlock::from(
        vec![],
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(0.into()),
        else_block_dom_instr_map,
    );

    // Block 3
    let b3_insr_1 = Rc::new(RefCell::new(Instruction::new(7, Operator::Phi(5, 1), None)));
    let b3_insr_2 = Rc::new(RefCell::new(Instruction::new(8, Operator::Phi(5, 2), None)));
    let b3_insr_3 = Rc::new(RefCell::new(Instruction::new(9, Operator::Write(7), None)));
    let b3_insr_4 = Rc::new(RefCell::new(Instruction::new(10, Operator::End, None)));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    join_block_identifier_map.insert(1, 7);
    join_block_identifier_map.insert(4, 8);
    let join_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let join_block = BasicBlock::from(
        vec![b3_insr_1, b3_insr_2, b3_insr_3, b3_insr_4],
        join_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    let main_body = Body::from(
        Some(0.into()),
        vec![branch_block, then_block, else_block, join_block],
    );
    let const_block = ConstBlock::from(HashSet::from_iter([0]));

    let expected_ir = IrStore::from(
        HashMap::from_iter([("main".to_string(), main_body)]),
        const_block,
    );

    assert_eq_sorted!(ir_store, expected_ir);
}

#[test]
fn nested_if_while() {
    let input = r#"
    main
    var x,k,j,m;
    {
    let x <- call InputNum();
    let k <- call InputNum();
    while x < 10 do
        let j <- 0;
        let m <- 0;
    
        let x <- x + 1;
        if k > 5 then
            while j < 15 do
                let j <- j + 1;
            od;
            let k <- 0;
        else
            let k <- k + 1;
            while m < 20 do
                let m <- m * 2;
            od;
        fi;
    od;
    call OutputNum(x);
    call OutputNum(k);
    }
    .
    "#;

    let tokens = Tokenizer::new(input);
    let computation = Parser::parse(tokens).unwrap();

    let ir_store = IrGenerator::generate(&computation);

    // Block 0
    let b0_insr_1 = Rc::new(RefCell::new(Instruction::new(1, Operator::Read, None)));
    let b0_insr_2 = Rc::new(RefCell::new(Instruction::new(2, Operator::Read, None)));

    let initial_block_identifier_map = InheritingHashMap::from_iter([(1, 1), (2, 2)]);
    let initial_block_dom_instr_map = InheritingHashMap::new();

    let initial_block = BasicBlock::from(
        vec![b0_insr_1, b0_insr_2],
        initial_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(1.into())),
        None,
        initial_block_dom_instr_map,
    );

    // Block 1
    let b1_insr_1 = Rc::new(RefCell::new(Instruction::new(3, Operator::Phi(1, 9), None)));
    let b1_insr_2 = Rc::new(RefCell::new(Instruction::new(
        4,
        Operator::Phi(2, 24),
        None,
    )));
    let b1_insr_3 = Rc::new(RefCell::new(Instruction::new(
        5,
        Operator::Phi(0, 25),
        None,
    )));
    let b1_insr_4 = Rc::new(RefCell::new(Instruction::new(
        6,
        Operator::Phi(0, 26),
        None,
    )));
    let b1_insr_5 = Rc::new(RefCell::new(Instruction::new(
        7,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 3, -10),
        None,
    )));
    let b1_insr_6 = Rc::new(RefCell::new(Instruction::new(
        8,
        Operator::Branch(BranchOpcode::Ge, 12.into(), 7),
        None,
    )));

    let mut join_block_identifier_map =
        InheritingHashMap::with_dominator(initial_block.get_identifier_map());
    join_block_identifier_map.insert(1, 3);
    join_block_identifier_map.insert(2, 4);
    join_block_identifier_map.insert(3, 5);
    join_block_identifier_map.insert(4, 6);
    let mut join_block_dom_instr_map =
        InheritingHashMap::with_dominator(initial_block.get_dom_instr_map());
    join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b1_insr_5.clone());

    let join_block = BasicBlock::from(
        vec![
            b1_insr_1,
            b1_insr_2,
            b1_insr_3,
            b1_insr_4,
            b1_insr_5.clone(),
            b1_insr_6,
        ],
        join_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(2.into())),
        Some(0.into()),
        join_block_dom_instr_map,
    );

    // Block 2
    let b2_insr_1 = Rc::new(RefCell::new(Instruction::new(
        9,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 3, -1),
        None,
    )));
    let b2_insr_2 = Rc::new(RefCell::new(Instruction::new(
        10,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 4, -5),
        Some(b1_insr_5.clone()),
    )));
    let b2_insr_3 = Rc::new(RefCell::new(Instruction::new(
        11,
        Operator::Branch(BranchOpcode::Le, 7.into(), 10),
        None,
    )));

    let mut branch_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    branch_block_identifier_map.insert(1, 9);
    branch_block_identifier_map.insert(3, 0);
    branch_block_identifier_map.insert(4, 0);
    let mut branch_block_dom_instr_map =
        InheritingHashMap::with_dominator(join_block.get_dom_instr_map());
    branch_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b2_insr_1.clone());
    branch_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b2_insr_2.clone());

    let branch_block = BasicBlock::from(
        vec![b2_insr_1.clone(), b2_insr_2.clone(), b2_insr_3],
        branch_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(3.into())),
        Some(1.into()),
        branch_block_dom_instr_map,
    );

    // Block 3
    let then_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    let then_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let then_block = BasicBlock::from(
        vec![],
        then_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(4.into())),
        Some(2.into()),
        then_block_dom_instr_map,
    );

    // Block 4
    let b4_insr_1 = Rc::new(RefCell::new(Instruction::new(
        12,
        Operator::Phi(0, 15),
        None,
    )));
    let b4_insr_2 = Rc::new(RefCell::new(Instruction::new(
        13,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 12, -15),
        Some(b2_insr_2.clone()),
    )));
    let b4_insr_3 = Rc::new(RefCell::new(Instruction::new(
        14,
        Operator::Branch(BranchOpcode::Ge, 6.into(), 13),
        None,
    )));

    let mut then_join_block_identifier_map =
        InheritingHashMap::with_dominator(then_block.get_identifier_map());
    then_join_block_identifier_map.insert(3, 12);
    let mut then_join_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_block.get_dom_instr_map());
    then_join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b4_insr_2.clone());

    let then_join_block = BasicBlock::from(
        vec![b4_insr_1, b4_insr_2, b4_insr_3],
        then_join_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(5.into())),
        Some(3.into()),
        then_join_block_dom_instr_map,
    );

    // Block 5
    let b5_insr_1 = Rc::new(RefCell::new(Instruction::new(
        15,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 12, -1),
        Some(b2_insr_1.clone()),
    )));
    let b5_insr_2 = Rc::new(RefCell::new(Instruction::new(
        16,
        Operator::UnconditionalBranch(4.into()),
        None,
    )));

    let mut then_loop_block_identifier_map =
        InheritingHashMap::with_dominator(then_join_block.get_identifier_map());
    then_loop_block_identifier_map.insert(3, 15);
    let mut then_loop_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_join_block.get_dom_instr_map());
    then_loop_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b5_insr_1.clone());

    let then_loop_block = BasicBlock::from(
        vec![b5_insr_1, b5_insr_2],
        then_loop_block_identifier_map,
        Some(ControlFlowEdge::Branch(4.into())),
        Some(4.into()),
        then_loop_block_dom_instr_map,
    );

    // Block 6
    let b6_insr_1 = Rc::new(RefCell::new(Instruction::new(
        17,
        Operator::UnconditionalBranch(11.into()),
        None,
    )));

    let mut then_end_block_identifier_map =
        InheritingHashMap::with_dominator(then_join_block.get_identifier_map());
    then_end_block_identifier_map.insert(2, 0);
    let then_end_block_dom_instr_map =
        InheritingHashMap::with_dominator(then_join_block.get_dom_instr_map());

    let then_end_block = BasicBlock::from(
        vec![b6_insr_1],
        then_end_block_identifier_map,
        Some(ControlFlowEdge::Branch(11.into())),
        Some(4.into()),
        then_end_block_dom_instr_map,
    );

    // Block 7
    let b7_insr_1 = Rc::new(RefCell::new(Instruction::new(
        18,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Add, 4, -1),
        Some(b2_insr_1.clone()),
    )));

    let mut else_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    else_block_identifier_map.insert(2, 18);
    let mut else_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());
    else_block_dom_instr_map.insert(StoredBinaryOpcode::Add, b7_insr_1.clone());

    let else_block = BasicBlock::from(
        vec![b7_insr_1],
        else_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(8.into())),
        Some(2.into()),
        else_block_dom_instr_map,
    );

    // Block 8
    let b8_insr_1 = Rc::new(RefCell::new(Instruction::new(
        19,
        Operator::Phi(0, 22),
        None,
    )));
    let b8_insr_2 = Rc::new(RefCell::new(Instruction::new(
        20,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Cmp, 19, -20),
        Some(b2_insr_2.clone()),
    )));
    let b8_insr_3 = Rc::new(RefCell::new(Instruction::new(
        21,
        Operator::Branch(BranchOpcode::Ge, 10.into(), 20),
        None,
    )));

    let mut else_join_block_identifier_map =
        InheritingHashMap::with_dominator(else_block.get_identifier_map());
    else_join_block_identifier_map.insert(4, 19);
    let mut else_join_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_block.get_dom_instr_map());
    else_join_block_dom_instr_map.insert(StoredBinaryOpcode::Cmp, b8_insr_2.clone());

    let else_join_block = BasicBlock::from(
        vec![b8_insr_1, b8_insr_2, b8_insr_3],
        else_join_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(9.into())),
        Some(7.into()),
        else_join_block_dom_instr_map,
    );

    // Block 9
    let b9_insr_1 = Rc::new(RefCell::new(Instruction::new(
        22,
        Operator::StoredBinaryOp(StoredBinaryOpcode::Mul, 19, -2),
        None,
    )));
    let b9_insr_2 = Rc::new(RefCell::new(Instruction::new(
        23,
        Operator::UnconditionalBranch(8.into()),
        None,
    )));

    let mut else_loop_block_identifier_map =
        InheritingHashMap::with_dominator(else_join_block.get_identifier_map());
    else_loop_block_identifier_map.insert(4, 22);
    let mut else_loop_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_join_block.get_dom_instr_map());
    else_loop_block_dom_instr_map.insert(StoredBinaryOpcode::Mul, b9_insr_1.clone());

    let else_loop_block = BasicBlock::from(
        vec![b9_insr_1, b9_insr_2],
        else_loop_block_identifier_map,
        Some(ControlFlowEdge::Branch(8.into())),
        Some(8.into()),
        else_loop_block_dom_instr_map,
    );

    // Block 10
    let else_end_block_identifier_map =
        InheritingHashMap::with_dominator(else_join_block.get_identifier_map());
    let else_end_block_dom_instr_map =
        InheritingHashMap::with_dominator(else_join_block.get_dom_instr_map());

    let else_end_block = BasicBlock::from(
        vec![],
        else_end_block_identifier_map,
        Some(ControlFlowEdge::Fallthrough(11.into())),
        Some(8.into()),
        else_end_block_dom_instr_map,
    );

    // Block 11
    let b11_insr_1 = Rc::new(RefCell::new(Instruction::new(
        24,
        Operator::Phi(0, 18),
        None,
    )));
    let b11_insr_2 = Rc::new(RefCell::new(Instruction::new(
        25,
        Operator::Phi(12, 0),
        None,
    )));
    let b11_insr_3 = Rc::new(RefCell::new(Instruction::new(
        26,
        Operator::Phi(0, 19),
        None,
    )));
    let b11_insr_4 = Rc::new(RefCell::new(Instruction::new(
        27,
        Operator::UnconditionalBranch(1.into()),
        None,
    )));

    let mut loop_end_block_identifier_map =
        InheritingHashMap::with_dominator(branch_block.get_identifier_map());
    loop_end_block_identifier_map.insert(2, 24);
    loop_end_block_identifier_map.insert(3, 25);
    loop_end_block_identifier_map.insert(4, 26);
    let loop_end_block_dom_instr_map =
        InheritingHashMap::with_dominator(branch_block.get_dom_instr_map());

    let loop_end_block = BasicBlock::from(
        vec![b11_insr_1, b11_insr_2, b11_insr_3, b11_insr_4],
        loop_end_block_identifier_map,
        Some(ControlFlowEdge::Branch(1.into())),
        Some(2.into()),
        loop_end_block_dom_instr_map,
    );

    // Block 12
    let b12_insr_1 = Rc::new(RefCell::new(Instruction::new(28, Operator::Write(3), None)));
    let b12_insr_2 = Rc::new(RefCell::new(Instruction::new(29, Operator::Write(4), None)));
    let b12_insr_3 = Rc::new(RefCell::new(Instruction::new(30, Operator::End, None)));

    let end_block_identifier_map =
        InheritingHashMap::with_dominator(join_block.get_identifier_map());
    let end_block_dom_instr_map = InheritingHashMap::with_dominator(join_block.get_dom_instr_map());

    let end_block = BasicBlock::from(
        vec![b12_insr_1, b12_insr_2, b12_insr_3],
        end_block_identifier_map,
        Some(ControlFlowEdge::Leaf),
        Some(1.into()),
        end_block_dom_instr_map,
    );

    let main_body = Body::from(
        Some(0.into()),
        vec![
            initial_block,
            join_block,
            branch_block,
            then_block,
            then_join_block,
            then_loop_block,
            then_end_block,
            else_block,
            else_join_block,
            else_loop_block,
            else_end_block,
            loop_end_block,
            end_block,
        ],
    );
    let const_block = ConstBlock::from(HashSet::from_iter([0, 1, 2, 5, 10, 15, 20]));

    let expected_ir = IrStore::from(
        HashMap::from_iter([("main".to_string(), main_body)]),
        const_block,
    );

    assert_eq_sorted!(ir_store, expected_ir);

    // panic!("{:#?}", ir_store);
}
