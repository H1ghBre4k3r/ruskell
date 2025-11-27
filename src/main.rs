use std::{collections::HashMap, process};

#[lachs::token]
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
    #[terminal(":=")]
    Assign,
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

foo = do 
    bar := baz
    bar
end

baz = do 
    42
end
"#;

#[derive(Debug, Clone)]
enum Expression {
    SingularExpression(SingularExpression),
    Assignment(Assignment),
}

impl Expression {
    pub fn eval(&self, scope: &mut SimulationScope) -> RValue {
        match self {
            Expression::SingularExpression(singular_expression) => match singular_expression {
                SingularExpression::Ident(ident) => {
                    let function_value = scope.resolve(&ident.value);

                    function_value.run(&[], scope)
                }
                SingularExpression::Integer(integer) => RValue::Integer(integer.clone()),
                SingularExpression::String(string_literal) => {
                    RValue::String(string_literal.clone())
                }
            },
            Expression::Assignment(Assignment { name, value }) => {
                let func = Function {
                    name: name.clone(),
                    args: (),
                    expression: vec![*value.clone()],
                };

                scope.add(&name.value, func);
                RValue::Unit
            }
        }
    }
}

impl Parse for Expression {
    fn parse(state: &mut ParseState) -> Self {
        match state.next() {
            Some(next) => match next {
                Token::Ident(ident) => match state.peek() {
                    Some(Token::Assign(_)) => {
                        state.next();
                        Expression::Assignment(Assignment {
                            name: ident,
                            value: Box::new(Expression::parse(state)),
                        })
                    }
                    _ => Expression::SingularExpression(SingularExpression::Ident(ident)),
                },
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
    Unit,
}

#[derive(Debug, Clone)]
enum SingularExpression {
    Ident(Ident),
    Integer(Integer),
    String(StringLiteral),
}

#[derive(Debug, Clone)]
struct Assignment {
    name: Ident,
    value: Box<Expression>,
}

#[derive(Debug, Clone)]
struct Function {
    name: Ident,
    // TODO: args
    args: (),
    expression: Vec<Expression>,
}

impl Function {
    pub fn run(&self, params: &[Expression], scope: &mut SimulationScope) -> RValue {
        let Function {
            name,
            args,
            expression,
        } = self;

        scope.enter();

        let mut i = 0;

        while i < expression.len() {
            let expr = &expression[i];

            if i == expression.len() - 1 {
                let val = expr.eval(scope);
                scope.leave();
                return val;
            } else {
                expr.eval(scope);
            }

            i += 1;
        }

        todo!("there should always be an exit expression")
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

        let mut expression = vec![];

        loop {
            if let Some(Token::End(_)) = state.peek() {
                break;
            }

            expression.push(Expression::parse(state));
        }

        let Some(Token::End(_)) = state.next() else {
            todo!()
        };

        Function {
            name: fn_ident,
            args: (),
            expression,
        }
    }
}

struct SimulationScope {
    scopes: Vec<HashMap<String, Function>>,
}

impl SimulationScope {
    fn new(functions: Vec<Function>) -> Self {
        Self {
            scopes: vec![
                functions
                    .into_iter()
                    .map(|func| (func.name.value.clone(), func))
                    .collect::<HashMap<_, _>>(),
            ],
        }
    }

    fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn leave(&mut self) {
        self.scopes.pop();
    }

    fn resolve(&self, name: impl ToString) -> Function {
        let key = name.to_string();
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(&key))
            .expect("function should be somewhere")
            .get(&key)
            .unwrap()
            .clone()
    }

    fn add(&mut self, name: impl ToString, func: Function) {
        self.scopes
            .last_mut()
            .expect("Ok, this would be fucked")
            .insert(name.to_string(), func);
    }
}

fn simulate(Program { main, functions }: Program) {
    let mut scope = SimulationScope::new(functions);

    let return_value = main.run(&[], &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(
            integer
                .value
                .parse()
                .expect("This should, by definition, be an integer"),
        ),
        RValue::Unit => process::exit(0),
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
