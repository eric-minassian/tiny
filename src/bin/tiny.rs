use std::{env, fs, process};

use tiny::{ir::gen::IrGenerator, lexer::Tokenizer, parser::Parser};

fn main() {
    let filename = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: tiny <filename>");
        process::exit(1);
    });

    let file = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        process::exit(1);
    });

    match Parser::parse(Tokenizer::new(&file)) {
        Ok(ast) => {
            let ir = IrGenerator::generate(&ast);
            print!("{}", ir.dot());
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}
