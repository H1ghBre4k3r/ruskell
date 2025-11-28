mod ast;
mod interpreter;
mod lexer;
mod parser;

use interpreter::simulate;
use lexer::Token;
use parser::{ParseState, parse};

const INPUT: &str = r#"

main = do
    foo
end

foo = do 
    bar := baz
    bar
end

baz = do 
    42
end
"#;

fn main() -> anyhow::Result<()> {
    let lexed = Token::lex(INPUT)?;
    let mut state = ParseState::new(lexed);

    let program = parse(&mut state).expect("failed to parse program");

    simulate(program);

    Ok(())
}
