use std::collections::HashMap;

use super::ty::{Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution(pub HashMap<TypeVar, Type>);

impl Substitution {
    pub fn empty() -> Self {
        Substitution(HashMap::new())
    }

    pub fn singleton(var: TypeVar, ty: Type) -> Self {
        let mut map = HashMap::new();
        map.insert(var, ty);
        Substitution(map)
    }

    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Int | Type::String | Type::Unit | Type::Bool => ty.clone(),
            Type::Var(v) => self.0.get(v).cloned().unwrap_or_else(|| ty.clone()),
            Type::Func(t1, t2) => Type::func(self.apply(t1), self.apply(t2)),
        }
    }

    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = HashMap::new();

        for (var, ty) in &self.0 {
            result.insert(var.clone(), other.apply(ty));
        }

        for (var, ty) in &other.0 {
            if !result.contains_key(var) {
                result.insert(var.clone(), ty.clone());
            }
        }

        Substitution(result)
    }

    pub fn insert(&mut self, var: TypeVar, ty: Type) {
        self.0.insert(var, ty);
    }

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
