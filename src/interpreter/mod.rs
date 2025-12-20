mod core_eval;
mod scope;
mod value;

pub use scope::Scope;
pub use value::{CapturedEnv, RValue};

use std::collections::HashMap;
use std::process;

use crate::core::CoreProgram;

/// Run a program and exit with the appropriate exit code
pub fn run(CoreProgram { main, functions }: CoreProgram<()>) {
    let mut scope = Scope::new(functions);

    // Main function doesn't capture anything (it's top-level)
    let empty_capture = CapturedEnv(HashMap::new());
    let return_value = main.lambda.run(RValue::Unit, &mut scope, &empty_capture);

    match return_value {
        RValue::Integer(integer) => process::exit(integer.value as i32),
        RValue::Unit => process::exit(0),
        RValue::Bool(b) => process::exit(if b { 0 } else { 1 }),
        RValue::String(_string_literal) => todo!("string return not implemented"),
        RValue::CoreLambda(_, _) => process::exit(0),
        // Legacy: shouldn't happen with Core AST
        RValue::Lambda(_) => process::exit(0),
    }
}
