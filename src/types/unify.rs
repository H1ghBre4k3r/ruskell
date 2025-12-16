use std::fmt;

use super::subst::Substitution;
use super::ty::{Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyError {
    Mismatch { expected: Type, found: Type },
    OccursCheck { var: TypeVar, ty: Type },
}

impl fmt::Display for UnifyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnifyError::Mismatch { expected, found } => {
                write!(
                    f,
                    "type mismatch: expected {}, found {}",
                    expected.pretty(),
                    found.pretty()
                )
            }
            UnifyError::OccursCheck { var, ty } => {
                write!(
                    f,
                    "occurs check: cannot construct infinite type {} = {}",
                    Type::Var(var.clone()).pretty(),
                    ty.pretty()
                )
            }
        }
    }
}

fn occurs_in(var: &TypeVar, ty: &Type) -> bool {
    match ty {
        Type::Int | Type::String | Type::Unit | Type::Bool => false,
        Type::Var(v) => v == var,
        Type::Func(t1, t2) => occurs_in(var, t1) || occurs_in(var, t2),
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<Substitution, UnifyError> {
    match (t1, t2) {
        // Same concrete types unify with empty substitution
        (Type::Int, Type::Int) => Ok(Substitution::empty()),
        (Type::String, Type::String) => Ok(Substitution::empty()),
        (Type::Unit, Type::Unit) => Ok(Substitution::empty()),
        (Type::Bool, Type::Bool) => Ok(Substitution::empty()),

        // Type variable unification
        (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(Substitution::empty()),
        (Type::Var(v), t) | (t, Type::Var(v)) => {
            if occurs_in(v, t) {
                Err(UnifyError::OccursCheck {
                    var: v.clone(),
                    ty: t.clone(),
                })
            } else {
                Ok(Substitution::singleton(v.clone(), t.clone()))
            }
        }

        // Function types
        (Type::Func(t1a, t1b), Type::Func(t2a, t2b)) => {
            let s1 = unify(t1a, t2a)?;
            let t1b_subst = s1.apply(t1b);
            let t2b_subst = s1.apply(t2b);
            let s2 = unify(&t1b_subst, &t2b_subst)?;
            Ok(s2.compose(&s1))
        }

        // Mismatches
        _ => Err(UnifyError::Mismatch {
            expected: t1.clone(),
            found: t2.clone(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_identical_int() {
        let result = unify(&Type::Int, &Type::Int);
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_identical_string() {
        let result = unify(&Type::String, &Type::String);
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_identical_unit() {
        let result = unify(&Type::Unit, &Type::Unit);
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_identical_bool() {
        let result = unify(&Type::Bool, &Type::Bool);
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_var_with_concrete() {
        let var = TypeVar::new(0);
        let result = unify(&Type::Var(var.clone()), &Type::Int);
        assert_eq!(result, Ok(Substitution::singleton(var, Type::Int)));
    }

    #[test]
    fn test_unify_concrete_with_var() {
        let var = TypeVar::new(0);
        let result = unify(&Type::Int, &Type::Var(var.clone()));
        assert_eq!(result, Ok(Substitution::singleton(var, Type::Int)));
    }

    #[test]
    fn test_unify_two_vars() {
        let var1 = TypeVar::new(0);
        let var2 = TypeVar::new(1);
        let result = unify(&Type::Var(var1.clone()), &Type::Var(var2.clone()));
        assert_eq!(result, Ok(Substitution::singleton(var1, Type::Var(var2))));
    }

    #[test]
    fn test_unify_same_var() {
        let var = TypeVar::new(0);
        let result = unify(&Type::Var(var.clone()), &Type::Var(var));
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_occurs_check_direct() {
        let var = TypeVar::new(0);
        let ty = Type::func(Type::Var(var.clone()), Type::Int);
        let result = unify(&Type::Var(var.clone()), &ty);
        assert!(matches!(result, Err(UnifyError::OccursCheck { .. })));
    }

    #[test]
    fn test_unify_occurs_check_nested() {
        let var = TypeVar::new(0);
        let ty = Type::func(Type::Int, Type::Var(var.clone()));
        let result = unify(&Type::Var(var.clone()), &ty);
        assert!(matches!(result, Err(UnifyError::OccursCheck { .. })));
    }

    #[test]
    fn test_unify_function_types_identical() {
        let ty = Type::func(Type::Int, Type::String);
        let result = unify(&ty, &ty);
        assert_eq!(result, Ok(Substitution::empty()));
    }

    #[test]
    fn test_unify_function_types_with_vars() {
        let var1 = TypeVar::new(0);
        let var2 = TypeVar::new(1);
        let t1 = Type::func(Type::Var(var1.clone()), Type::Int);
        let t2 = Type::func(Type::String, Type::Var(var2.clone()));

        let result = unify(&t1, &t2).unwrap();

        // Should unify var1 with String and var2 with Int
        assert_eq!(result.apply(&Type::Var(var1)), Type::String);
        assert_eq!(result.apply(&Type::Var(var2)), Type::Int);
    }

    #[test]
    fn test_unify_nested_functions() {
        let var = TypeVar::new(0);
        let t1 = Type::func(Type::func(Type::Int, Type::Var(var.clone())), Type::String);
        let t2 = Type::func(Type::func(Type::Int, Type::Bool), Type::String);

        let result = unify(&t1, &t2).unwrap();
        assert_eq!(result.apply(&Type::Var(var)), Type::Bool);
    }

    #[test]
    fn test_unify_mismatch_concrete() {
        let result = unify(&Type::Int, &Type::String);
        assert!(matches!(result, Err(UnifyError::Mismatch { .. })));
    }

    #[test]
    fn test_unify_mismatch_shape() {
        let ty_func = Type::func(Type::Int, Type::Int);
        let result = unify(&Type::Int, &ty_func);
        assert!(matches!(result, Err(UnifyError::Mismatch { .. })));
    }

    #[test]
    fn test_unify_complex_substitution_threading() {
        // Test that substitutions are properly threaded through function unification
        // (α -> β) ~ (Int -> String)
        let var_a = TypeVar::new(0);
        let var_b = TypeVar::new(1);
        let t1 = Type::func(Type::Var(var_a.clone()), Type::Var(var_b.clone()));
        let t2 = Type::func(Type::Int, Type::String);

        let result = unify(&t1, &t2).unwrap();

        // Both variables should be unified
        assert_eq!(result.apply(&Type::Var(var_a)), Type::Int);
        assert_eq!(result.apply(&Type::Var(var_b)), Type::String);
    }

    #[test]
    fn test_occurs_check_helper() {
        let var = TypeVar::new(0);

        // Occurs directly
        assert!(occurs_in(&var, &Type::Var(var.clone())));

        // Doesn't occur
        let other_var = TypeVar::new(1);
        assert!(!occurs_in(&var, &Type::Var(other_var)));
        assert!(!occurs_in(&var, &Type::Int));

        // Occurs in function
        let ty = Type::func(Type::Int, Type::Var(var.clone()));
        assert!(occurs_in(&var, &ty));

        // Occurs nested
        let ty = Type::func(Type::func(Type::Var(var.clone()), Type::Int), Type::String);
        assert!(occurs_in(&var, &ty));
    }
}
