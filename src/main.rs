mod ast;
mod interpreter;
mod lexer;
mod parser;

use ast::{Function, Parse, Program};
use interpreter::simulate;
use lexer::Token;
use parser::ParseState;

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

    let mut functions: Vec<Function> = vec![];

    while state.has_next() {
        functions.push(Function::parse(&mut state));
    }

    let main = match functions.iter().find(|func| func.name.value == "main") {
        Some(main) => main.clone(),
        None => todo!(),
    };

    let functions = functions
        .into_iter()
        .filter(|func| func.name.value != "main")
        .collect();

    let prog = Program { main, functions };

    simulate(prog);

    Ok(())
}
