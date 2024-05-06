use std::{env, fs, process};

use tiny::lexer::Tokenizer;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let file = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading file: {}", e);
        process::exit(1);
    });

    let tokenizer = Tokenizer::new(&file);

    // Tokenize
    for token in tokenizer {
        println!("{:?}", token);
    }
}
