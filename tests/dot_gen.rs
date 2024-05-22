use std::fs;

use pretty_assertions_sorted::assert_eq;
use tiny::{ir::gen::IrGenerator, lexer::Tokenizer, parser::Parser};

// @TODO: Use regex to automatically parse dot files for comparison instead of manually writing them

const TEST_FILES_DIR: &str = "tests/fixtures/";

fn generate_dot(filename: &str) -> String {
    let path = format!("{}{}", TEST_FILES_DIR, filename);
    let buffer = fs::read_to_string(path).unwrap();

    let tokens = Tokenizer::new(&buffer);
    let ast = Parser::parse(tokens).unwrap();

    IrGenerator::generate(&ast).dot()
}

fn expected_dot(filename: &str) -> String {
    let path = format!("{}{}", TEST_FILES_DIR, filename);
    fs::read_to_string(path).unwrap()
}

fn compare_dot(filename: &str) {
    let dot = generate_dot(&format!("{}.tiny", filename));
    let expected_dot = expected_dot(&format!("{}.dot", filename));

    assert_eq!(dot, expected_dot);
}

// #[test]
// fn commutative_cse() {
//     compare_dot("commutative-cse");
// }

// #[test]
// fn fibonacci() {
//     compare_dot("fibonacci");
// }
