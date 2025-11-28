use std::collections::HashMap;
use std::process;

use crate::ast::{Function, Program};
use crate::lexer::{Integer, StringLiteral};

#[derive(Debug, Clone)]
pub enum RValue {
    Integer(Integer),
    String(StringLiteral),
    Unit,
}

type ScopeFrame = HashMap<String, Function>;

pub struct SimulationScope {
    frames: Vec<ScopeFrame>,
}

impl SimulationScope {
    pub fn new(functions: Vec<Function>) -> Self {
        Self {
            frames: vec![
                functions
                    .into_iter()
                    .map(|func| (func.name.value.clone(), func))
                    .collect::<HashMap<_, _>>(),
            ],
        }
    }

    pub fn enter(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn leave(&mut self) {
        self.frames.pop();
    }

    pub fn resolve(&self, name: impl ToString) -> Function {
        let key = name.to_string();
        self.frames
            .iter()
            .rev()
            .find(|scope| scope.contains_key(&key))
            .expect("function should be somewhere")
            .get(&key)
            .unwrap()
            .clone()
    }

    pub fn add(&mut self, name: impl ToString, func: Function) {
        self.frames
            .last_mut()
            .expect("Ok, this would be fucked")
            .insert(name.to_string(), func);
    }
}

pub fn simulate(Program { main, functions }: Program) {
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
        RValue::String(_string_literal) => todo!(),
    }
}
