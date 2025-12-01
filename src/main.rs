mod ast;
mod interpreter;
mod lexer;
mod parser;

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
"#;

fn main() -> anyhow::Result<()> {
    let lexed = Token::lex(INPUT)?;
    let mut state = ParseState::new(lexed);

    let program = parse(&mut state).expect("failed to parse program");

    println!("{program:#?}");

    interpreter::run(program);

    Ok(())
}
