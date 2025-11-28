use crate::interpreter::{RValue, SimulationScope};
use crate::lexer::{Ident, Integer, StringLiteral};

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

#[derive(Debug, Clone)]
pub struct Program {
    pub main: Function,
    pub functions: Vec<Function>,
}
