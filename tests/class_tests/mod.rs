use std::fs;
use std::path::Path;

use tiny::ir::gen::IrGenerator;
use tiny::lexer::Tokenizer;
use tiny::parser::Parser;

#[test]
fn fixtures() {
    let fixtures_dir = Path::new("tests/class_tests");
    let files = fs::read_dir(fixtures_dir).unwrap();

    for file in files {
        let file = file.unwrap();
        let path = file.path();
        let ext = path.extension().unwrap();
        if ext != "tiny" {
            continue;
        }

        let input = fs::read_to_string(&path).unwrap();
        let tokens = Tokenizer::new(&input);
        let computation = Parser::parse(tokens).unwrap();

        let ir_store = IrGenerator::generate(&computation);
        let ir_dot = ir_store.dot();

        let ir_path = path.with_extension("dot");
        let ir_dot_file = fs::read_to_string(&ir_path).unwrap();

        assert_eq!(ir_dot, ir_dot_file);
    }
}
