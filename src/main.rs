use std::{collections::HashMap, process};

#[lachs_derive::token]
enum Token {
    #[terminal("do")]
    Do,
    #[terminal("end")]
    End,
    #[literal("[a-zA-Z']*")]
    Ident,
    #[literal("[0-9]*")]
    Integer,
    #[literal(r#""([^"\\]|\\.)*""#)]
    StringLiteral,
    #[terminal("=")]
    Equals,
    #[terminal(":")]
    Colon,
    #[terminal("::")]
    DoubleColon,
}

struct ParseState {
    tokens: Vec<Token>,
    index: usize,
}

impl ParseState {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.has_next() {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    pub fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }
}

const INPUT: &str = r#"

main = do
    foo
end

foo = do 1 end

"#;

#[derive(Debug, Clone)]
enum Expression {
    SingularExpression(SingularExpression),
}

impl Expression {
    pub fn eval(&self, scope: &SimulationScope) -> RValue {
        match self {
            Expression::SingularExpression(singular_expression) => match singular_expression {
                SingularExpression::Ident(ident) => {
                    let Some(function_value) = scope.functions.get(&ident.value) else {
                        todo!()
                    };

                    function_value.run(&[], scope)
                }
                SingularExpression::Integer(integer) => RValue::Integer(integer.clone()),
                SingularExpression::String(string_literal) => {
                    RValue::String(string_literal.clone())
                }
            },
        }
    }
}

impl Parse for Expression {
    fn parse(state: &mut ParseState) -> Self {
        match state.next() {
            Some(next) => match next {
                Token::Ident(ident) => {
                    Expression::SingularExpression(SingularExpression::Ident(ident))
                }
                Token::Integer(integer) => {
                    Expression::SingularExpression(SingularExpression::Integer(integer))
                }
                Token::StringLiteral(string_literal) => {
                    Expression::SingularExpression(SingularExpression::String(string_literal))
                }
                token => todo!("unexpected {token:?}"),
            },
            None => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
enum RValue {
    Integer(Integer),
    String(StringLiteral),
}

#[derive(Debug, Clone)]
enum SingularExpression {
    Ident(Ident),
    Integer(Integer),
    String(StringLiteral),
}

#[derive(Debug, Clone)]
struct Function {
    name: Ident,
    // TODO: args
    args: (),
    expression: Vec<Expression>,
}

impl Function {
    pub fn run(&self, params: &[Expression], scope: &SimulationScope) -> RValue {
        let Function {
            name,
            args,
            expression,
        } = self;

        let mut i = 0;

        while i < expression.len() {
            let expr = &expression[i];

            if i == expression.len() - 1 {
                return expr.eval(scope);
            } else {
                expr.eval(scope);
            }

            i += 1;
        }

        todo!()
    }
}

#[derive(Debug, Clone)]
struct Program {
    main: Function,
    functions: Vec<Function>,
}

trait Parse {
    fn parse(state: &mut ParseState) -> Self;
}

impl Parse for Function {
    fn parse(state: &mut ParseState) -> Self {
        let Some(Token::Ident(fn_ident)) = state.next() else {
            todo!()
        };

        let Some(Token::Equals(_)) = state.next() else {
            todo!()
        };

        let Some(Token::Do(_)) = state.next() else {
            todo!()
        };

        let expr = Expression::parse(state);

        let Some(Token::End(_)) = state.next() else {
            todo!()
        };

        Function {
            name: fn_ident,
            args: (),
            expression: vec![expr],
        }
    }
}

struct SimulationScope {
    functions: HashMap<String, Function>,
}

impl SimulationScope {
    fn new(functions: Vec<Function>) -> Self {
        Self {
            functions: functions
                .into_iter()
                .map(|func| (func.name.value.clone(), func))
                .collect::<HashMap<_, _>>(),
        }
    }
}

fn simulate(Program { main, functions }: Program) {
    let scope = SimulationScope::new(functions);

    let return_value = main.run(&[], &scope);

    match return_value {
        RValue::Integer(integer) => process::exit(
            integer
                .value
                .parse()
                .expect("This should, by definition, be an integer"),
        ),
        RValue::String(string_literal) => todo!(),
    }
}

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
