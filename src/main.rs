mod ast;
mod core;
mod desugar;
mod interpreter;
mod lexer;
mod parser;

use std::process;

use desugar::{desugar_program, erase_program};
use lexer::Token;
use parser::{ParseState, parse};

const INPUT: &str = r#"
main = do
    double := \x => do
        intermediate := x
        intermediate
    end
    y := 42
    z := double(y)
    z
end

foo = do 
    fn := \x => \y => \z => do 

    end
end
"#;

fn main() -> anyhow::Result<()> {
    let lexed = Token::lex(INPUT)?;
    let mut state = ParseState::new(lexed);

    let (program, errors) = parse(&mut state);

    // Print all errors
    if !errors.is_empty() {
        eprintln!("Found {} error(s):\n", errors.len());
        for error in &errors {
            eprintln!("{error}\n");
        }
    }

    // Run if we have a valid program
    match program {
        Some(prog) if errors.is_empty() => {
            // Desugar the program
            let desugared = desugar_program(prog);

            println!("Running program...\n");
            interpreter::run_core(desugared);
        }
        Some(_) => {
            // Had errors but recovered enough to find main
            process::exit(1);
        }
        None => {
            process::exit(1);
        }
    }

    Ok(())
}
