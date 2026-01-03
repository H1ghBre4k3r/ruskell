//! # Type Substitutions for Unification
//!
//! This module implements substitutions used in Hindley-Milner
//! type inference to represent type variable assignments.
//!
//! ## Overview
//!
//! A substitution maps type variables to types, representing
//! assignments discovered during type inference. For example:
//!
//! - After inferring `\x => x + 1`, we might have substitution: `'a := Int`
//! - After inferring `\x => x`, we might have: `'a := 'b` (different vars unified)
//!
//! ## Substitution Operations
//!
//! The `Substitution` struct supports:
//!
//! - **Creation**: Empty or singleton (single mapping) substitutions
//! - **Application**: Apply substitution to a type (replace all type variables)
//! - **Composition**: Combine two substitutions (compose two mappings)
//!
//! ## Substitution Application
//!
//! When we apply a substitution to a type, we replace every type
//! variable that appears in the substitution with its mapped type:
//!
//! ```text
//! // Substitution: {'a := Int}
//!
//! Apply to 'a:
//!   'a → Int
//!
//! Apply to 'a -> 'a:
//!   'a -> 'a → Int -> Int
//!
//! Apply to 'a -> 'b:
//!   'a -> 'b → Int -> 'b
//! ```
//!
//! ## Substitution Composition
//!
//! When composing substitutions S1 and S2 (S2 ∘ S1):
//!
//! ```text
//! (S2 ∘ S1)(t) = S2(S1(t))
//!
//! // Meaning: first apply S1, then apply S2 to the result
//!
//! // Example:
//! S1 = {'a := 'b}
//! S2 = {'b := Int}
//!
//! Compose S2 ∘ S1:
//!   For 'a in S1: apply S2 to 'b → Int
//!   Result: {'a := Int, 'b := Int}
//! ```
//!
//! Composition is used in type inference to combine substitutions
//! from different parts of the expression. For example:
//!
//! ```text
//! // Inferring: (x + 1) (x + 2)
//!
//! 1. Infer x + 1: Get S1 = {'a := Int} (x is Int)
//! 2. Infer x + 2: Get S2 = {'b := Int} (x is Int)
//! 3. Check types unify: Int = Int → OK
//! 4. Compose: S2 ∘ S1 = {'a := Int, 'b := Int}
//! ```
//!
//! ## Composition Order
//!
//! Composition is **not** commutative: S1 ∘ S2 ≠ S2 ∘ S1.
//!
//! The standard convention in Hindley-Milner is:
//!
//! ```text
//! result = S2.compose(&S1)  // Means: apply S1 first, then S2
//! // Or: result = S2 ∘ S1
//! ```
//!
//! This order matters because later substitutions may affect
//! types substituted by earlier ones.
//!
//! ## Related Modules
//!
//! - [`crate::types::ty`] - Type definitions that substitutions apply to
//! - [`crate::types::unify`] - Unification algorithm that produces substitutions
//! - [`crate::types::env`] - Type environments that substitutions are applied to

use std::collections::HashMap;

use super::ty::{Type, TypeVar};

/// A substitution mapping type variables to types.
///
/// Substitutions represent assignments of type variables discovered
/// during type inference. Internally stored as a hash map.
///
/// # Fields
///
/// * `0` - The underlying HashMap mapping `TypeVar` → `Type`
///
/// # Example
///
/// ```text
/// // Substitution with one mapping: 'a := Int
/// let var_a = TypeVar::new(0);
/// let subst = Substitution::singleton(var_a, Type::Int);
/// // subst.0 = {var_a: Int}
///
/// // Substitution with multiple mappings:
/// let var_b = TypeVar::new(1);
/// subst.extend(Substitution::singleton(var_b, Type::String));
/// // subst.0 = {var_a: Int, var_b: String}
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution(pub HashMap<TypeVar, Type>);

impl Substitution {
    /// Create an empty substitution (no mappings).
    ///
    /// # Returns
    ///
    /// A new substitution with no variable mappings
    ///
    /// # Example
    ///
    /// ```text
    /// let subst = Substitution::empty();
    /// // Applying empty substitution to any type returns the same type
    /// assert_eq!(subst.apply(&Type::Int), Type::Int);
    /// ```
    pub fn empty() -> Self {
        Substitution(HashMap::new())
    }

    /// Create a substitution with a single mapping.
    ///
    /// # Arguments
    ///
    /// * `var` - The type variable to map
    /// * `ty` - The type to map the variable to
    ///
    /// # Returns
    ///
    /// A new substitution with one mapping: `var := ty`
    ///
    /// # Example
    ///
    /// ```text
    /// let var_a = TypeVar::new(0);
    /// let subst = Substitution::singleton(var_a.clone(), Type::Int);
    /// // subst = {'a := Int}
    ///
    /// // Apply to type variable:
    /// assert_eq!(subst.apply(&Type::Var(var_a)), Type::Int);
    /// ```
    pub fn singleton(var: TypeVar, ty: Type) -> Self {
        let mut map = HashMap::new();
        map.insert(var, ty);
        Substitution(map)
    }

    /// Apply this substitution to a type.
    ///
    /// Recursively replaces all type variables in the type
    /// with their mapped values from this substitution.
    ///
    /// # Arguments
    ///
    /// * `ty` - The type to apply substitution to
    ///
    /// # Returns
    ///
    /// A new type with all substitutions applied
    ///
    /// # Algorithm
    ///
    /// ```text
    /// apply(t):
    ///   match t:
    ///     Int, String, Unit, Bool:
    ///       return t  // Concrete types unchanged
    ///
    ///     Var(v):
    ///       if v in subst:
    ///         return subst[v]  // Replace with mapped type
    ///       else:
    ///         return Var(v)  // Not in substitution, keep
    ///
    ///     Func(t1, t2):
    ///       return Func(apply(t1), apply(t2))  // Recurse
    /// ```
    ///
    /// # Examples
    ///
    /// ```text
    /// // Substitution: {'a := Int, 'b := String}
    ///
    /// // Apply to type variable:
    /// apply('a) = Int
    /// apply('b) = String
    /// apply('c) = 'c  // Not in substitution
    ///
    /// // Apply to function type:
    /// apply('a -> 'b) = Int -> String
    /// apply('a -> 'a) = Int -> Int
    /// apply('a -> 'c) = Int -> 'c
    ///
    /// // Apply to nested function:
    /// apply(('a -> 'b) -> 'a) = (Int -> String) -> Int
    /// ```
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Int | Type::String | Type::Unit | Type::Bool => ty.clone(),
            Type::Var(v) => self.0.get(v).cloned().unwrap_or_else(|| ty.clone()),
            Type::List(elem_ty) => Type::List(Box::new(self.apply(elem_ty))),
            Type::Func(t1, t2) => Type::func(self.apply(t1), self.apply(t2)),
        }
    }

    /// Compose this substitution with another (this ∘ other).
    ///
    /// Returns a new substitution that represents applying `other`
    /// first, then applying `this` to the result.
    ///
    /// Note: This is `self.compose(&other)` = `self ∘ other`,
    /// which means "apply other first, then apply self".
    ///
    /// # Arguments
    ///
    /// * `other` - The substitution to apply first
    ///
    /// # Returns
    ///
    /// A new substitution representing `self ∘ other`
    ///
    /// # Algorithm
    ///
    /// ```text
    /// compose(S1, S2) = S1 ∘ S2
    ///
    /// 1. For each (var, ty) in S1:
    ///      result[var] = S2.apply(ty)  // Apply S2 to the mapped type
    ///
    /// 2. For each (var, ty) in S2:
    ///      if var not in result:
    ///        result[var] = ty  // Keep mappings not overridden
    ///
    /// 3. Return result
    /// ```
    ///
    /// # Example
    ///
    /// ```text
    /// S1 = {'a := 'b}
    /// S2 = {'b := Int}
    ///
    /// Compose S1 ∘ S2:
    ///   1. For 'a := 'b in S1: apply S2 to 'b → Int
    ///      result = {'a := Int}
    ///   2. For 'b := Int in S2: 'b not in result → add it
    ///      result = {'a := Int, 'b := Int}
    ///   3. Return result
    ///
    /// // Check: (S1 ∘ S2)('a) = S1(S2('a)) = S1('a) = 'b = Int ✓
    /// ```
    ///
    /// # Order Matters
    ///
    /// ```text
    /// S1 = {'a := Int}
    /// S2 = {'a := String}
    ///
    /// S1 ∘ S2 = {'a := Int}      // S2's mapping is overridden
    /// S2 ∘ S1 = {'a := String}   // S1's mapping is overridden
    /// ```
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = HashMap::new();

        // Apply other to all mappings in self
        for (var, ty) in &self.0 {
            result.insert(var.clone(), other.apply(ty));
        }

        // Add mappings from other not in result
        for (var, ty) in &other.0 {
            if !result.contains_key(var) {
                result.insert(var.clone(), ty.clone());
            }
        }

        Substitution(result)
    }

    /// Insert a new mapping into this substitution.
    ///
    /// If the variable already has a mapping, it's replaced.
    ///
    /// # Arguments
    ///
    /// * `var` - The type variable to map
    /// * `ty` - The type to map the variable to
    ///
    /// # Example
    ///
    /// ```text
    /// let mut subst = Substitution::empty();
    /// subst.insert(var_a, Type::Int);
    /// // subst = {'a := Int}
    ///
    /// // Overwrite:
    /// subst.insert(var_a, Type::String);
    /// // subst = {'a := String}
    /// ```
    #[allow(dead_code)]
    pub fn insert(&mut self, var: TypeVar, ty: Type) {
        self.0.insert(var, ty);
    }

    /// Extend this substitution with mappings from another.
    ///
    /// If a variable exists in both, the other's mapping is used.
    ///
    /// # Arguments
    ///
    /// * `other` - The substitution to add mappings from
    ///
    /// # Example
    ///
    /// ```text
    /// let mut subst1 = Substitution::singleton(var_a, Type::Int);
    /// let subst2 = Substitution::singleton(var_b, Type::String);
    ///
    /// subst1.extend(subst2);
    /// // subst1 = {'a := Int, 'b := String}
    /// ```
    #[allow(dead_code)]
    pub fn extend(&mut self, other: Substitution) {
        for (var, ty) in other.0 {
            self.0.insert(var, ty);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_substitution() {
        let subst = Substitution::empty();
        let ty = Type::Int;
        assert_eq!(subst.apply(&ty), ty);
    }

    #[test]
    fn test_singleton_substitution() {
        let var = TypeVar::new(0);
        let subst = Substitution::singleton(var.clone(), Type::Int);
        let ty = Type::Var(var);
        assert_eq!(subst.apply(&ty), Type::Int);
    }

    #[test]
    fn test_apply_to_function() {
        let var = TypeVar::new(0);
        let subst = Substitution::singleton(var.clone(), Type::Int);
        let ty = Type::func(Type::Var(var), Type::String);
        assert_eq!(subst.apply(&ty), Type::func(Type::Int, Type::String));
    }

    #[test]
    fn test_apply_preserves_unbound_vars() {
        let var1 = TypeVar::new(0);
        let var2 = TypeVar::new(1);
        let subst = Substitution::singleton(var1.clone(), Type::Int);
        let ty = Type::Var(var2.clone());
        assert_eq!(subst.apply(&ty), Type::Var(var2));
    }

    #[test]
    fn test_compose_substitutions() {
        let var1 = TypeVar::new(0);
        let var2 = TypeVar::new(1);

        let s1 = Substitution::singleton(var1.clone(), Type::Var(var2.clone()));
        let s2 = Substitution::singleton(var2.clone(), Type::Int);

        let composed = s1.compose(&s2);

        let ty = Type::Var(var1);
        assert_eq!(composed.apply(&ty), Type::Int);
    }

    #[test]
    fn test_compose_order() {
        let var = TypeVar::new(0);

        let s1 = Substitution::singleton(var.clone(), Type::Int);
        let s2 = Substitution::singleton(var.clone(), Type::String);

        let result = s1.compose(&s2);
        let ty = Type::Var(var);
        assert_eq!(result.apply(&ty), Type::Int);
    }

    #[test]
    fn test_substitution_idempotent() {
        let var = TypeVar::new(0);
        let subst = Substitution::singleton(var.clone(), Type::Int);
        let ty = Type::Var(var);

        let once = subst.apply(&ty);
        let twice = subst.apply(&once);
        assert_eq!(once, twice);
    }
}
