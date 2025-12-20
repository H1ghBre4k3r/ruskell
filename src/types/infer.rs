use std::collections::HashMap;
use std::fmt;

use lachs::Span;

use super::env::TypeEnv;
use super::subst::Substitution;
use super::ty::{Type, TypeScheme, TypeVar};
use super::unify::{UnifyError, unify};
use crate::core::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
        context: Option<String>,
    },
    OccursCheck {
        var: TypeVar,
        ty: Type,
        span: Span,
    },
}

impl TypeError {
    pub fn unbound_variable(name: String, span: Span) -> Self {
        TypeError::UnboundVariable { name, span }
    }

    pub fn type_mismatch(expected: Type, found: Type, span: Span) -> Self {
        TypeError::TypeMismatch {
            expected,
            found,
            span,
            context: None,
        }
    }

    pub fn with_context(mut self, context: String) -> Self {
        if let TypeError::TypeMismatch { context: ctx, .. } = &mut self {
            *ctx = Some(context);
        }
        self
    }

    pub fn occurs_check(var: TypeVar, ty: Type, span: Span) -> Self {
        TypeError::OccursCheck { var, ty, span }
    }

    pub fn from_unify_error(err: UnifyError, span: Span) -> Self {
        match err {
            UnifyError::Mismatch { expected, found } => {
                TypeError::type_mismatch(expected, found, span)
            }
            UnifyError::OccursCheck { var, ty } => TypeError::occurs_check(var, ty, span),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::UnboundVariable { name, span } => {
                let msg = format!("unbound variable: {}", name);
                // Check if span has source attached
                if span.source.is_empty() {
                    write!(f, "Type error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(&msg))
                }
            }
            TypeError::TypeMismatch {
                expected,
                found,
                span,
                context,
            } => {
                let msg = format!(
                    "type mismatch: expected {}, found {}",
                    expected.pretty(),
                    found.pretty()
                );
                let full_msg = if let Some(ctx) = context {
                    format!("{}\n  Note: {}", msg, ctx)
                } else {
                    msg
                };
                // Check if span has source attached
                if span.source.is_empty() {
                    write!(f, "Type error: {}", full_msg)
                } else {
                    write!(f, "{}", span.to_string(&full_msg))
                }
            }
            TypeError::OccursCheck { var, ty, span } => {
                let msg = format!(
                    "cannot construct infinite type: {} = {}",
                    Type::Var(var.clone()).pretty(),
                    ty.pretty()
                );
                // Check if span has source attached
                if span.source.is_empty() {
                    write!(f, "Type error: {}", msg)
                } else {
                    write!(f, "{}", span.to_string(&msg))
                }
            }
        }
    }
}

impl std::error::Error for TypeError {}

pub struct Infer {
    next_var: usize,
}

impl Infer {
    pub fn new() -> Self {
        Infer { next_var: 0 }
    }

    fn fresh_var(&mut self) -> TypeVar {
        let id = self.next_var;
        self.next_var += 1;
        TypeVar::new(id)
    }

    fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        let subst: HashMap<_, _> = scheme
            .vars
            .iter()
            .map(|v| (v.clone(), Type::Var(self.fresh_var())))
            .collect();
        Substitution(subst).apply(&scheme.ty)
    }

    fn generalize(&self, env: &TypeEnv, ty: &Type) -> TypeScheme {
        let free_in_env = env.free_type_vars();
        let free_in_ty = ty.free_type_vars();
        let vars: Vec<_> = free_in_ty.difference(&free_in_env).cloned().collect();
        TypeScheme {
            vars,
            ty: ty.clone(),
        }
    }

    pub fn infer_expr(
        &mut self,
        env: &TypeEnv,
        expr: &CoreExpr<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        match expr {
            CoreExpr::Unit(_) => Ok((Substitution::empty(), Type::Unit)),

            CoreExpr::Integer(_) => Ok((Substitution::empty(), Type::Int)),

            CoreExpr::String(_) => Ok((Substitution::empty(), Type::String)),

            CoreExpr::Ident(ident) => match env.lookup(&ident.value) {
                Some(scheme) => {
                    let ty = self.instantiate(scheme);
                    Ok((Substitution::empty(), ty))
                }
                None => Err(TypeError::unbound_variable(
                    ident.value.clone(),
                    ident.position.clone(),
                )),
            },

            CoreExpr::Lambda(lambda) => self.infer_lambda(env, lambda),

            CoreExpr::FunctionCall(call) => self.infer_call(env, call),

            CoreExpr::BinaryOp(binop) => self.infer_binop(env, binop),
        }
    }

    fn infer_lambda(
        &mut self,
        env: &TypeEnv,
        lambda: &CoreLambda<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        match &lambda.param {
            CoreLambdaParam::Unit(_) => {
                let (s, body_ty) = self.infer_lambda_body(env, &lambda.body)?;
                Ok((s, Type::func(Type::Unit, body_ty)))
            }
            CoreLambdaParam::Ident(ident) => {
                let param_ty = Type::Var(self.fresh_var());
                let param_scheme = TypeScheme::monomorphic(param_ty.clone());
                let env1 = env.extend(ident.value.clone(), param_scheme);
                let (s, body_ty) = self.infer_lambda_body(&env1, &lambda.body)?;
                let param_ty_subst = s.apply(&param_ty);
                Ok((s, Type::func(param_ty_subst, body_ty)))
            }
        }
    }

    fn infer_lambda_body(
        &mut self,
        env: &TypeEnv,
        body: &CoreLambdaBody<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        match body {
            CoreLambdaBody::Expression(expr) => self.infer_expr(env, expr),
            CoreLambdaBody::Block(stmts) => self.infer_block(env, stmts),
        }
    }

    fn infer_call(
        &mut self,
        env: &TypeEnv,
        call: &CoreFunctionCall<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        let (s1, func_ty) = self.infer_expr(env, &call.func)?;
        let env1 = env.apply_subst(&s1);
        let (s2, arg_ty) = self.infer_expr(&env1, &call.arg)?;

        let result_ty = Type::Var(self.fresh_var());
        let expected_func_ty = Type::func(arg_ty, result_ty.clone());

        let func_ty_subst = s2.apply(&func_ty);
        let s3 = unify(&func_ty_subst, &expected_func_ty)
            .map_err(|e| TypeError::from_unify_error(e, call.position.clone()))?;

        let final_subst = s3.compose(&s2).compose(&s1);
        let final_ty = final_subst.apply(&result_ty);

        Ok((final_subst, final_ty))
    }

    fn infer_binop(
        &mut self,
        env: &TypeEnv,
        binop: &crate::core::CoreBinaryOp<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        // All arithmetic operators work on Int -> Int -> Int
        let (s1, left_ty) = self.infer_expr(env, &binop.left)?;
        let env1 = env.apply_subst(&s1);
        let (s2, right_ty) = self.infer_expr(&env1, &binop.right)?;

        // Unify left with Int
        let left_ty_subst = s2.apply(&left_ty);
        let s3 = unify(&left_ty_subst, &Type::Int)
            .map_err(|e| TypeError::from_unify_error(e, binop.position.clone()))?;

        // Unify right with Int
        let right_ty_subst = s3.apply(&right_ty);
        let s4 = unify(&right_ty_subst, &Type::Int)
            .map_err(|e| TypeError::from_unify_error(e, binop.position.clone()))?;

        let final_subst = s4.compose(&s3).compose(&s2).compose(&s1);

        Ok((final_subst, Type::Int))
    }

    fn infer_statement(
        &mut self,
        env: &TypeEnv,
        stmt: &CoreStatement<()>,
    ) -> Result<(Substitution, Type, TypeEnv), TypeError> {
        match stmt {
            CoreStatement::Assignment(assign) => self.infer_assignment(env, assign),
            CoreStatement::Expression(expr) => {
                let (s, t) = self.infer_expr(env, expr)?;
                let env1 = env.apply_subst(&s);
                Ok((s, t, env1))
            }
        }
    }

    fn infer_assignment(
        &mut self,
        env: &TypeEnv,
        assign: &CoreAssignment<()>,
    ) -> Result<(Substitution, Type, TypeEnv), TypeError> {
        let (s1, t1) = self.infer_expr(env, &assign.value)?;
        let env1 = env.apply_subst(&s1);
        let scheme = self.generalize(&env1, &t1);
        let env2 = env1.extend(assign.name.value.clone(), scheme);
        Ok((s1, Type::Unit, env2))
    }

    fn infer_block(
        &mut self,
        env: &TypeEnv,
        stmts: &[CoreStatement<()>],
    ) -> Result<(Substitution, Type), TypeError> {
        if stmts.is_empty() {
            return Ok((Substitution::empty(), Type::Unit));
        }

        let mut subst = Substitution::empty();
        let mut current_env = env.clone();
        let mut last_type = Type::Unit;

        for stmt in stmts {
            let (s, t, new_env) = self.infer_statement(&current_env, stmt)?;
            subst = s.compose(&subst);
            current_env = new_env;
            last_type = t;
        }

        Ok((subst, last_type))
    }

    pub fn infer_function(
        &mut self,
        env: &TypeEnv,
        func: &CoreFunction<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        self.infer_lambda(env, &func.lambda)
    }

    pub fn infer_program(&mut self, program: &CoreProgram<()>) -> Result<TypeEnv, Vec<TypeError>> {
        let mut env = TypeEnv::empty();
        let mut errors = Vec::new();

        // Infer types for all functions first (excluding main)
        for func in &program.functions {
            match self.infer_function(&env, func) {
                Ok((_, ty)) => {
                    let scheme = self.generalize(&env, &ty);
                    env = env.extend(func.name.value.clone(), scheme);
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }

        // Infer type for main
        match self.infer_function(&env, &program.main) {
            Ok((_, ty)) => {
                let scheme = self.generalize(&env, &ty);
                env = env.extend(program.main.name.value.clone(), scheme);
            }
            Err(err) => {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(env)
        } else {
            Err(errors)
        }
    }
}

impl Default for Infer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to create test expressions
    fn unit_expr() -> CoreExpr<()> {
        CoreExpr::Unit(CoreUnit {
            position: lachs::Span::default(),
            info: (),
        })
    }

    fn int_expr(value: i128) -> CoreExpr<()> {
        CoreExpr::Integer(CoreInteger {
            value,
            position: lachs::Span::default(),
            info: (),
        })
    }

    fn string_expr(value: &str) -> CoreExpr<()> {
        CoreExpr::String(CoreString {
            value: value.to_string(),
            position: lachs::Span::default(),
            info: (),
        })
    }

    fn ident_expr(name: &str) -> CoreExpr<()> {
        CoreExpr::Ident(CoreIdent {
            value: name.to_string(),
            position: lachs::Span::default(),
            info: (),
        })
    }

    fn lambda_expr(param: &str, body: CoreExpr<()>) -> CoreExpr<()> {
        CoreExpr::Lambda(CoreLambda {
            param: CoreLambdaParam::Ident(CoreIdent {
                value: param.to_string(),
                position: lachs::Span::default(),
                info: (),
            }),
            body: CoreLambdaBody::Expression(Box::new(body)),
            position: lachs::Span::default(),
            info: (),
        })
    }

    fn call_expr(func: CoreExpr<()>, arg: CoreExpr<()>) -> CoreExpr<()> {
        CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(func),
            arg: Box::new(arg),
            position: lachs::Span::default(),
            info: (),
        })
    }

    #[test]
    fn test_infer_unit_literal() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        let expr = unit_expr();
        let result = infer.infer_expr(&env, &expr);
        assert_eq!(result, Ok((Substitution::empty(), Type::Unit)));
    }

    #[test]
    fn test_infer_integer_literal() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        let expr = int_expr(42);
        let result = infer.infer_expr(&env, &expr);
        assert_eq!(result, Ok((Substitution::empty(), Type::Int)));
    }

    #[test]
    fn test_infer_string_literal() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        let expr = string_expr("hello");
        let result = infer.infer_expr(&env, &expr);
        assert_eq!(result, Ok((Substitution::empty(), Type::String)));
    }

    #[test]
    fn test_infer_unbound_variable() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        let expr = ident_expr("x");
        let result = infer.infer_expr(&env, &expr);
        assert!(matches!(result, Err(TypeError::UnboundVariable { .. })));
    }

    #[test]
    fn test_infer_variable_monomorphic() {
        let mut infer = Infer::new();
        let env =
            TypeEnv::with_bindings(vec![("x".to_string(), TypeScheme::monomorphic(Type::Int))]);
        let expr = ident_expr("x");
        let result = infer.infer_expr(&env, &expr);
        assert_eq!(result, Ok((Substitution::empty(), Type::Int)));
    }

    #[test]
    fn test_infer_identity_function() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        // \x => x
        let expr = lambda_expr("x", ident_expr("x"));
        let (_, ty) = infer.infer_expr(&env, &expr).unwrap();

        // Should be a function from some type to the same type
        match ty {
            Type::Func(t1, t2) => {
                assert_eq!(*t1, *t2);
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_infer_const_function() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        // \x => \y => x
        let expr = lambda_expr("x", lambda_expr("y", ident_expr("x")));
        let (_, ty) = infer.infer_expr(&env, &expr).unwrap();

        // Should be a -> b -> a
        match ty {
            Type::Func(t1, t2) => match *t2 {
                Type::Func(_, t3) => {
                    assert_eq!(*t1, *t3);
                }
                _ => panic!("Expected nested function type"),
            },
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_infer_simple_application() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        // (\x => x)(42)
        let expr = call_expr(lambda_expr("x", ident_expr("x")), int_expr(42));
        let (_, ty) = infer.infer_expr(&env, &expr).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_infer_nested_application() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        // (\x => \y => x)(42)("hello")
        let inner = call_expr(
            lambda_expr("x", lambda_expr("y", ident_expr("x"))),
            int_expr(42),
        );
        let expr = call_expr(inner, string_expr("hello"));
        let (_, ty) = infer.infer_expr(&env, &expr).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_infer_let_polymorphism() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // First, create id function: \x => x
        let id_expr = lambda_expr("x", ident_expr("x"));
        let (_, id_ty) = infer.infer_expr(&env, &id_expr).unwrap();

        // Generalize it
        let id_scheme = infer.generalize(&env, &id_ty);
        let env1 = env.extend("id".to_string(), id_scheme);

        // Use id with Int: id(42)
        let use1 = call_expr(ident_expr("id"), int_expr(42));
        let (_, ty1) = infer.infer_expr(&env1, &use1).unwrap();
        assert_eq!(ty1, Type::Int);

        // Use id with String: id("hello")
        let use2 = call_expr(ident_expr("id"), string_expr("hello"));
        let (_, ty2) = infer.infer_expr(&env1, &use2).unwrap();
        assert_eq!(ty2, Type::String);
    }

    #[test]
    fn test_infer_assignment() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let assign = CoreAssignment {
            name: CoreIdent {
                value: "x".to_string(),
                position: lachs::Span::default(),
                info: (),
            },
            value: Box::new(int_expr(42)),
            position: lachs::Span::default(),
            info: (),
        };

        let (s, ty, new_env) = infer.infer_assignment(&env, &assign).unwrap();
        assert_eq!(s, Substitution::empty());
        assert_eq!(ty, Type::Unit);

        // Check that x is now in the environment
        let x_scheme = new_env.lookup("x").unwrap();
        assert_eq!(x_scheme.ty, Type::Int);
    }

    #[test]
    fn test_infer_block_single_statement() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let stmts = vec![CoreStatement::Expression(int_expr(42))];
        let (_, ty) = infer.infer_block(&env, &stmts).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_infer_block_multiple_statements() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let assign1 = CoreStatement::Assignment(CoreAssignment {
            name: CoreIdent {
                value: "x".to_string(),
                position: lachs::Span::default(),
                info: (),
            },
            value: Box::new(int_expr(42)),
            position: lachs::Span::default(),
            info: (),
        });

        let assign2 = CoreStatement::Assignment(CoreAssignment {
            name: CoreIdent {
                value: "y".to_string(),
                position: lachs::Span::default(),
                info: (),
            },
            value: Box::new(ident_expr("x")),
            position: lachs::Span::default(),
            info: (),
        });

        let final_expr = CoreStatement::Expression(ident_expr("y"));

        let stmts = vec![assign1, assign2, final_expr];
        let (_, ty) = infer.infer_block(&env, &stmts).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_fresh_var_uniqueness() {
        let mut infer = Infer::new();
        let v1 = infer.fresh_var();
        let v2 = infer.fresh_var();
        let v3 = infer.fresh_var();

        assert_ne!(v1, v2);
        assert_ne!(v2, v3);
        assert_ne!(v1, v3);
    }

    #[test]
    fn test_instantiate_monomorphic() {
        let mut infer = Infer::new();
        let scheme = TypeScheme::monomorphic(Type::Int);
        let ty = infer.instantiate(&scheme);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_instantiate_polymorphic() {
        let mut infer = Infer::new();
        let var = TypeVar::new(100);
        let scheme = TypeScheme::polymorphic(vec![var.clone()], Type::Var(var));

        // Instantiate twice - should get different fresh vars
        let ty1 = infer.instantiate(&scheme);
        let ty2 = infer.instantiate(&scheme);

        // Both should be type variables, but different ones
        match (ty1, ty2) {
            (Type::Var(v1), Type::Var(v2)) => {
                assert_ne!(v1, v2);
            }
            _ => panic!("Expected type variables"),
        }
    }

    #[test]
    fn test_generalize_no_free_vars() {
        let infer = Infer::new();
        let env = TypeEnv::empty();
        let ty = Type::Int;
        let scheme = infer.generalize(&env, &ty);
        assert!(scheme.vars.is_empty());
    }

    #[test]
    fn test_generalize_with_free_vars() {
        let infer = Infer::new();
        let env = TypeEnv::empty();
        let var = TypeVar::new(0);
        let ty = Type::func(Type::Var(var.clone()), Type::Int);
        let scheme = infer.generalize(&env, &ty);
        assert_eq!(scheme.vars.len(), 1);
        assert!(scheme.vars.contains(&var));
    }

    // Error message tests
    #[test]
    fn test_error_unbound_variable_has_span() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();
        let span = Span::default();
        let expr = CoreExpr::Ident(CoreIdent {
            value: "undefined_var".to_string(),
            position: span.clone(),
            info: (),
        });

        let result = infer.infer_expr(&env, &expr);
        match result {
            Err(TypeError::UnboundVariable { name, .. }) => {
                assert_eq!(name, "undefined_var");
            }
            _ => panic!("Expected UnboundVariable error"),
        }
    }

    #[test]
    fn test_error_type_mismatch_display() {
        let span = Span::default();
        let err = TypeError::type_mismatch(Type::Int, Type::String, span);
        let msg = format!("{}", err);
        assert!(msg.contains("type mismatch"));
        assert!(msg.contains("Int"));
        assert!(msg.contains("String"));
    }

    #[test]
    fn test_error_with_context() {
        let span = Span::default();
        let err = TypeError::type_mismatch(Type::Int, Type::String, span)
            .with_context("in function application".to_string());

        let msg = format!("{}", err);
        assert!(msg.contains("type mismatch"));
        assert!(msg.contains("Note:"));
        assert!(msg.contains("in function application"));
    }

    #[test]
    fn test_error_occurs_check_display() {
        let span = Span::default();
        let var = TypeVar::new(0);
        let ty = Type::func(Type::Var(var.clone()), Type::Int);
        let err = TypeError::occurs_check(var, ty, span);

        let msg = format!("{}", err);
        assert!(msg.contains("infinite type"));
    }

    #[test]
    fn test_function_call_type_error_has_span() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // Try to call an integer as a function: 42(10)
        let call_span = Span::default();
        let call = CoreExpr::FunctionCall(CoreFunctionCall {
            func: Box::new(int_expr(42)),
            arg: Box::new(int_expr(10)),
            position: call_span,
            info: (),
        });

        let result = infer.infer_expr(&env, &call);
        match result {
            Err(TypeError::TypeMismatch { .. }) => {
                // Success - we got a type mismatch error
            }
            _ => panic!("Expected TypeMismatch error, got: {:?}", result),
        }
    }

    #[test]
    fn test_infer_binary_addition() {
        use crate::ast::expression::BinOpKind;
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // 5 + 3
        let binop = CoreExpr::BinaryOp(crate::core::CoreBinaryOp {
            op: BinOpKind::Add,
            left: Box::new(int_expr(5)),
            right: Box::new(int_expr(3)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn test_infer_binary_mixed_types_error() {
        use crate::ast::expression::BinOpKind;
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // 5 + "hello" should fail
        let binop = CoreExpr::BinaryOp(crate::core::CoreBinaryOp {
            op: BinOpKind::Add,
            left: Box::new(int_expr(5)),
            right: Box::new(string_expr("hello")),
            position: Span::default(),
            info: (),
        });

        let result = infer.infer_expr(&env, &binop);
        assert!(result.is_err());
    }

    #[test]
    fn test_infer_binary_with_variables() {
        use crate::ast::expression::BinOpKind;
        let mut infer = Infer::new();
        let env = TypeEnv::empty()
            .extend("x".to_string(), TypeScheme::monomorphic(Type::Int))
            .extend("y".to_string(), TypeScheme::monomorphic(Type::Int));

        // x * y
        let binop = CoreExpr::BinaryOp(crate::core::CoreBinaryOp {
            op: BinOpKind::Mul,
            left: Box::new(ident_expr("x")),
            right: Box::new(ident_expr("y")),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Int);
    }
}
