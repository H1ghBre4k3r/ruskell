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
use types::validate_and_type_check;

const INPUT: &str = r#"
main = do
    default := 999
    x := 5
    result := case x of
        0 => 17
        1 => 19
        2 => 41
        n => times10(x)
        _ => default
    end
    result
end

times10 x = x * 10
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

            // Validate and type check the desugared program
            println!("Validating and type checking...\n");
            match validate_and_type_check(desugared.clone()) {
                Ok(type_env) => {
                    println!("✓ Validation and type checking passed!");

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
                Err(validation_errors) => {
                    eprintln!(
                        "Validation failed with {} error(s):\n",
                        validation_errors.len()
                    );
                    for error in validation_errors {
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
