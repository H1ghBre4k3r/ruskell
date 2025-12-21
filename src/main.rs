mod ast;
mod core;
mod desugar;
mod interpreter;
mod lexer;
mod parser;
mod types;

use std::fs;
use std::process;

use clap::{Parser, Subcommand};
use desugar::desugar_program;
use lexer::Token;
use parser::{ParseState, parse};
use types::validate_and_type_check;

#[derive(Parser)]
#[command(name = "rsk")]
#[command(version, about = "Ruskell programming language toolchain", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run a Ruskell program
    Run {
        /// Path to .rsk file
        file: String,
    },
    /// Type check a program without running it
    Check {
        /// Path to .rsk file
        file: String,
    },
    /// Format a Ruskell source file (not yet implemented)
    Fmt {
        /// Path to .rsk file
        file: String,
        /// Format in place
        #[arg(short, long)]
        in_place: bool,
    },
    /// Start interactive REPL (not yet implemented)
    Repl,
    /// Compile to executable (not yet implemented)
    Build {
        /// Path to .rsk file
        file: String,
        /// Output path
        #[arg(short, long)]
        output: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    let exit_code = match cli.command {
        Command::Run { file } => cmd_run(&file),
        Command::Check { file } => cmd_check(&file),
        Command::Fmt { file, in_place } => cmd_fmt(&file, in_place),
        Command::Repl => cmd_repl(),
        Command::Build { file, output } => cmd_build(&file, output),
    };

    process::exit(exit_code);
}

fn read_source_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Failed to read file '{}': {}", path, e))
}

fn cmd_run(file: &str) -> i32 {
    // Read source
    let source = match read_source_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            return 2;
        }
    };

    // Lex
    let tokens = match Token::lex(&source) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            return 3;
        }
    };

    // Parse
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);

    if !errors.is_empty() {
        eprintln!("Found {} parse error(s):\n", errors.len());
        for error in &errors {
            eprintln!("{}\n", error);
        }
        return 4;
    }

    let program = match program {
        Some(p) => p,
        None => {
            eprintln!("Failed to parse program");
            return 4;
        }
    };

    // Desugar
    let desugared = desugar_program(program);

    // Type check
    let type_env = match validate_and_type_check(desugared.clone()) {
        Ok(env) => env,
        Err(validation_errors) => {
            eprintln!(
                "Type checking failed with {} error(s):\n",
                validation_errors.len()
            );
            for error in validation_errors {
                eprintln!("{}\n", error);
            }
            return 5;
        }
    };

    // Print type information
    println!("Type checking passed!");
    if let Some(main_scheme) = type_env.lookup("main") {
        println!("  main : {}", main_scheme.ty.pretty());
    }
    for func in &desugared.functions {
        if let Some(scheme) = type_env.lookup(&func.name.value) {
            println!("  {} : {}", func.name.value, scheme.ty.pretty());
        }
    }
    println!();

    // Interpret
    println!("Running program...\n");
    interpreter::run(desugared);

    0
}

fn cmd_check(file: &str) -> i32 {
    // Read source
    let source = match read_source_file(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            return 2;
        }
    };

    // Lex
    let tokens = match Token::lex(&source) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            return 3;
        }
    };

    // Parse
    let mut state = ParseState::new(tokens);
    let (program, errors) = parse(&mut state);

    if !errors.is_empty() {
        eprintln!("Found {} parse error(s):\n", errors.len());
        for error in &errors {
            eprintln!("{}\n", error);
        }
        return 4;
    }

    let program = match program {
        Some(p) => p,
        None => {
            eprintln!("Failed to parse program");
            return 4;
        }
    };

    // Desugar
    let desugared = desugar_program(program);

    // Type check
    match validate_and_type_check(desugared.clone()) {
        Ok(type_env) => {
            println!("✓ Type check passed for '{}'", file);

            // Print inferred types
            if let Some(main_scheme) = type_env.lookup("main") {
                println!("  main : {}", main_scheme.ty.pretty());
            }
            for func in &desugared.functions {
                if let Some(scheme) = type_env.lookup(&func.name.value) {
                    println!("  {} : {}", func.name.value, scheme.ty.pretty());
                }
            }

            0
        }
        Err(validation_errors) => {
            eprintln!("Type checking failed in '{}':\n", file);
            for error in validation_errors {
                eprintln!("{}\n", error);
            }
            5
        }
    }
}

fn cmd_fmt(_file: &str, _in_place: bool) -> i32 {
    eprintln!("Error: Code formatter not yet implemented");
    eprintln!("This feature is planned for Phase 2 of the binary toolchain.");
    1
}

fn cmd_repl() -> i32 {
    eprintln!("Error: REPL not yet implemented");
    eprintln!("This feature is planned for Phase 3 of the binary toolchain.");
    1
}

fn cmd_build(_file: &str, _output: Option<String>) -> i32 {
    eprintln!("Error: Code generation not yet implemented");
    eprintln!("The program can be type-checked, but compilation to native code");
    eprintln!("requires LLVM backend (Phase 4 of roadmap).");
    1
}
