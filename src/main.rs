mod ast;
mod interpreter;
mod lexer;
mod parser;

use std::process;

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
    fn := \x => \y => \z do 

    end
end
"#;

fn main() -> anyhow::Result<()> {
    let lexed = Token::lex(INPUT)?;
    let mut state = ParseState::new(lexed);

    let program = match parse(&mut state) {
        Ok(program) => program,
        Err(e) => {
            println!("{e}");
            process::exit(-1);
        }
    };

    println!("{program:#?}");

    interpreter::run(program);

    Ok(())
}
