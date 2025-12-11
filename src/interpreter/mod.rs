mod core_eval;
mod scope;
mod value;

pub use scope::Scope;
pub use value::RValue;

use std::process;

use crate::core::CoreProgram;

/// Run a program and exit with the appropriate exit code
pub fn run(CoreProgram { main, functions }: CoreProgram<()>) {
    let mut scope = Scope::new(functions);

    let return_value = main.lambda.run(RValue::Unit, &mut scope);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::CoreLambda(_) => process::exit(0),
        // Legacy: shouldn't happen with Core AST
        RValue::Lambda(_) => process::exit(0),
    }
}
