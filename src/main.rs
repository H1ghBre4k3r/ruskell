mod ast;
mod core;
mod desugar;
mod interpreter;
mod lexer;
mod parser;
mod types;

use std::process;

use desugar::desugar_program;
use lexer::Token;
use parser::{ParseState, parse};
use types::Infer;

const INPUT: &str = r#"
main = do
    cons := \x, y => do
        x
    end
    get := cons(42)
    z := get(13)
    z
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

            // Type check the desugared program
            println!("Type checking...\n");
            let mut infer = Infer::new();
            match infer.infer_program(&desugared) {
                Ok(type_env) => {
                    println!("✓ Type checking passed!");

                    // Print inferred types for main and functions
                    if let Some(main_scheme) = type_env.lookup("main") {
                        println!("  main : {}", main_scheme.ty.pretty());
                    }
                    for func in &desugared.functions {
                        if let Some(scheme) = type_env.lookup(&func.name.value) {
                            println!("  {} : {}", func.name.value, scheme.ty.pretty());
                        }
                    }
                    println!();

                    println!("Running program...\n");
                    interpreter::run(desugared);
                }
                Err(type_errors) => {
                    eprintln!(
                        "Type checking failed with {} error(s):\n",
                        type_errors.len()
                    );
                    for error in type_errors {
                        eprintln!("{}\n", error);
                    }
                    process::exit(1);
                }
            }
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
