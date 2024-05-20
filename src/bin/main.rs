use std::{env, fs, process};

use tiny::{lexer::Tokenizer, parsera::Parser};

fn main() {
    let filename = env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: tiny <filename>");
        process::exit(1);
    });

    let file = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        process::exit(1);
    });

    let ir = Parser::parse(Tokenizer::new(&file));

    match ir {
        Ok(ir_store) => {
            print!("{}", ir_store.dot());
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}
