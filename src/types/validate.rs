use std::fmt;

use lachs::Span;

use super::env::TypeEnv;
use super::error::TypeError;
use super::infer::Infer;
use super::ty::Type;
use crate::core::CoreProgram;

/// Validation errors - program-level semantic checks
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationError {
    /// Main function doesn't have name "main"
    MissingMain { span: Span },
    /// Main function has wrong type signature
    InvalidMainType {
        expected: Type,
        found: Type,
        span: Span,
    },
    /// Type error from type inference
    TypeError(TypeError),
}

impl ValidationError {
    pub fn missing_main(span: Span) -> Self {
        ValidationError::MissingMain { span }
    }

    pub fn invalid_main_type(expected: Type, found: Type, span: Span) -> Self {
        ValidationError::InvalidMainType {
            expected,
            found,
            span,
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValidationError::MissingMain { span } => {
                let msg = "program must have a 'main' function";
                if span.source.is_empty() {
                    write!(f, "Validation error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(msg))
                }
            }
            ValidationError::InvalidMainType {
                expected,
                found,
                span,
            } => {
                let msg = format!(
                    "main function has invalid type\n  expected: {}\n  found: {}",
                    expected, found
                );
                if span.source.is_empty() {
                    write!(f, "Validation error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(&msg))
                }
            }
            ValidationError::TypeError(err) => write!(f, "{}", err),
        }
    }
}

impl From<TypeError> for ValidationError {
    fn from(err: TypeError) -> Self {
        ValidationError::TypeError(err)
    }
}

/// Validate and type check a program
///
/// This performs:
/// 1. Structural validation (main exists)
/// 2. Type inference
/// 3. Semantic validation (main has correct type)
pub fn validate_and_type_check(program: CoreProgram<()>) -> Result<TypeEnv, Vec<ValidationError>> {
    let mut errors = Vec::new();

    // Step 1: Check main exists (structural validation)
    if program.main.name.value != "main" {
        errors.push(ValidationError::missing_main(program.main.name.position));
        // Cannot continue without main
        return Err(errors);
    }

    // Step 2: Type check the program
    let mut infer = Infer::new();
    let type_env = match infer.infer_program(&program) {
        Ok(env) => env,
        Err(type_errors) => {
            // Convert type errors to validation errors
            errors.extend(type_errors.into_iter().map(ValidationError::from));
            return Err(errors);
        }
    };

    // Step 3: Validate main has correct type
    // Main should be: () -> Int or () -> ()
    if let Some(main_scheme) = type_env.lookup("main") {
        // Instantiate the type scheme to get a concrete type
        let main_type = &main_scheme.ty;

        let is_valid = match main_type {
            Type::Func(param, ret) => {
                matches!(**param, Type::Unit) && matches!(**ret, Type::Int | Type::Unit)
            }
            _ => false,
        };

        if !is_valid {
            let expected = Type::func(Type::Unit, Type::Int);
            errors.push(ValidationError::invalid_main_type(
                expected,
                main_type.clone(),
                program.main.name.position,
            ));
        }
    }

    if errors.is_empty() {
        Ok(type_env)
    } else {
        Err(errors)
    }
}
