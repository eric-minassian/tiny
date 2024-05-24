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
