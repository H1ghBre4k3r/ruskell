use crate::interpreter::{RValue, SimulationScope};
use crate::lexer::{Ident, Integer, StringLiteral, Token};
use crate::parser::ParseState;

#[derive(Debug, Clone)]
pub enum Expression {
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

pub trait Parse {
    fn parse(state: &mut ParseState) -> Self;
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
pub enum SingularExpression {
    Ident(Ident),
    Integer(Integer),
    String(StringLiteral),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Ident,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub args: (),
    pub expression: Vec<Expression>,
}

impl Function {
    pub fn run(&self, _params: &[Expression], scope: &mut SimulationScope) -> RValue {
        let Function {
            name: _,
            args: _,
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

#[derive(Debug, Clone)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
}
