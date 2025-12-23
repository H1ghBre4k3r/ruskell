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

/// Check if a type variable occurs within a type (occurs check).
///
/// The occurs check prevents the creation of infinite types by ensuring
/// we never create a substitution like `'t0 = 't0 -> Int`.
///
/// # Why Is This Important?
///
/// Without the occurs check, unification could create infinite types:
///
/// ```text
/// Unify('t0, 't0 -> Int):
///   Without occurs check: [t0 := 't0 -> Int]
///   
///   Applying this gives:
///   't0 = 't0 -> Int
///       = ('t0 -> Int) -> Int      // substitute 't0
///       = (('t0 -> Int) -> Int) -> Int    // substitute again
///       = ...                       // infinite!
/// ```
///
/// The occurs check detects this situation and fails unification.
///
/// # Arguments
///
/// * `var` - The type variable to search for
/// * `ty` - The type to search within
///
/// # Returns
///
/// `true` if `var` appears anywhere in `ty`, `false` otherwise
///
/// # Examples
///
/// ```text
/// occurs_in('t0, Int) = false
/// occurs_in('t0, 't0) = true
/// occurs_in('t0, 't0 -> Int) = true
/// occurs_in('t0, 't1 -> 't2) = false
/// ```
fn occurs_in(var: &TypeVar, ty: &Type) -> bool {
    match ty {
        Type::Int | Type::String | Type::Unit | Type::Bool => false,
        Type::Var(v) => v == var,
        Type::Func(t1, t2) => occurs_in(var, t1) || occurs_in(var, t2),
    }
}

/// Unify two types, finding a substitution that makes them equal.
///
/// This is the core of the Hindley-Milner type inference algorithm. Unification
/// solves type equations by finding assignments to type variables that make
/// two types identical.
///
/// # Algorithm
///
/// The algorithm works by structural recursion on the type structure:
///
/// ## Base Cases (Concrete Types)
///
/// ```text
/// Unify(Int, Int) = ∅          // Empty substitution
/// Unify(String, String) = ∅
/// Unify(Bool, Bool) = ∅
/// Unify(Unit, Unit) = ∅
/// Unify(Int, String) = Error   // Type mismatch
/// ```
///
/// ## Type Variables
///
/// ```text
/// Unify('t0, 't0) = ∅          // Same variable
/// Unify('t0, 't1) = [t0 := 't1]   // Different variables
/// Unify('t0, Int) = [t0 := Int]   // Variable with concrete type
/// Unify('t0, 't0 -> Int) = Error  // Occurs check fails!
/// ```
///
/// Before creating a substitution `[var := type]`, we perform an **occurs check**
/// to ensure `var` doesn't appear in `type`. This prevents infinite types.
///
/// ## Function Types
///
/// ```text
/// Unify(t1 -> t2, t3 -> t4):
///   1. Unify t1 with t3, getting S1
///   2. Apply S1 to t2 and t4
///   3. Unify S1(t2) with S1(t4), getting S2
///   4. Return S2 ∘ S1 (composition)
/// ```
///
/// Example:
/// ```text
/// Unify('t0 -> 't1, Int -> Bool):
///   Step 1: Unify 't0 with Int → [t0 := Int]
///   Step 2: Apply [t0 := Int] to 't1 and Bool → 't1, Bool
///   Step 3: Unify 't1 with Bool → [t1 := Bool]
///   Step 4: Compose: [t0 := Int, t1 := Bool]
/// ```
///
/// # Why Composition Matters
///
/// We must apply S1 before unifying the return types because S1 might affect them:
///
/// ```text
/// Unify('t0 -> 't0, Int -> 't1):
///   Step 1: Unify 't0 with Int → S1 = [t0 := Int]
///   Step 2: Apply S1: 't0 becomes Int
///   Step 3: Unify Int with 't1 → S2 = [t1 := Int]
///   Result: [t0 := Int, t1 := Int]
///
/// Without applying S1, we'd incorrectly get [t0 := Int, t1 := 't0]
/// ```
///
/// # Arguments
///
/// * `t1` - First type to unify
/// * `t2` - Second type to unify
///
/// # Returns
///
/// * `Ok(Substitution)` - A substitution that makes t1 equal to t2
/// * `Err(UnifyError::Mismatch)` - Types cannot be unified (e.g., Int vs String)
/// * `Err(UnifyError::OccursCheck)` - Would create infinite type
///
/// # Examples
///
/// ```text
/// // Success cases
/// unify(Int, Int) = Ok(∅)
/// unify('t0, Int) = Ok([t0 := Int])
/// unify('t0 -> 't1, Int -> Bool) = Ok([t0 := Int, t1 := Bool])
///
/// // Error cases
/// unify(Int, String) = Err(Mismatch)
/// unify('t0, 't0 -> Int) = Err(OccursCheck)
/// ```
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
