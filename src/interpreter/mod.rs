mod core_eval;
mod eval;
mod scope;
mod value;

pub use scope::Scope;
pub use value::RValue;

use std::process;

use crate::ast::Program;
use crate::core::CoreProgram;

/// Run a surface AST program and exit with the appropriate exit code
pub fn run(Program { main, functions }: Program<()>) {
    let mut scope = Scope::new(functions);

    let return_value = main.lambda.run(&[], &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::Lambda(_) => process::exit(0),
        RValue::CoreLambda(_) => process::exit(0),
    }
}

/// Run a core AST program and exit with the appropriate exit code
pub fn run_core(CoreProgram { main, functions }: CoreProgram<()>) {
    let mut scope = Scope::new_core(functions);

    let return_value = main.lambda.run(RValue::Unit, &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::Lambda(_) => process::exit(0),
        RValue::CoreLambda(_) => process::exit(0),
    }
}
