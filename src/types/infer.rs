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

            CoreExpr::Boolean(_) => Ok((Substitution::empty(), Type::Bool)),

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

            CoreExpr::UnaryOp(unop) => self.infer_unaryop(env, unop),

            CoreExpr::IfThenElse(if_expr) => self.infer_if_then_else(env, if_expr),
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
        use crate::ast::expression::BinOpKind;

        match binop.op {
            // Arithmetic operators: Int -> Int -> Int
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div => {
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

            // Comparison operators: Int -> Int -> Bool
            BinOpKind::Eq
            | BinOpKind::NotEq
            | BinOpKind::Lt
            | BinOpKind::Gt
            | BinOpKind::LtEq
            | BinOpKind::GtEq => {
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

                Ok((final_subst, Type::Bool))
            }

            // Logical operators: Bool -> Bool -> Bool
            BinOpKind::And | BinOpKind::Or => {
                let (s1, left_ty) = self.infer_expr(env, &binop.left)?;
                let env1 = env.apply_subst(&s1);
                let (s2, right_ty) = self.infer_expr(&env1, &binop.right)?;

                // Unify left with Bool
                let left_ty_subst = s2.apply(&left_ty);
                let s3 = unify(&left_ty_subst, &Type::Bool)
                    .map_err(|e| TypeError::from_unify_error(e, binop.position.clone()))?;

                // Unify right with Bool
                let right_ty_subst = s3.apply(&right_ty);
                let s4 = unify(&right_ty_subst, &Type::Bool)
                    .map_err(|e| TypeError::from_unify_error(e, binop.position.clone()))?;

                let final_subst = s4.compose(&s3).compose(&s2).compose(&s1);

                Ok((final_subst, Type::Bool))
            }
        }
    }

    fn infer_unaryop(
        &mut self,
        env: &TypeEnv,
        unop: &crate::core::CoreUnaryOp<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        use crate::ast::expression::UnaryOpKind;

        match unop.op {
            UnaryOpKind::Not => {
                let (s1, operand_ty) = self.infer_expr(env, &unop.operand)?;

                // Unify operand with Bool
                let s2 = unify(&operand_ty, &Type::Bool)
                    .map_err(|e| TypeError::from_unify_error(e, unop.position.clone()))?;

                let final_subst = s2.compose(&s1);

                Ok((final_subst, Type::Bool))
            }
        }
    }

    fn infer_if_then_else(
        &mut self,
        env: &TypeEnv,
        if_expr: &crate::core::CoreIfThenElse<()>,
    ) -> Result<(Substitution, Type), TypeError> {
        // 1. Infer condition type
        let (s1, cond_ty) = self.infer_expr(env, &if_expr.condition)?;

        // 2. Unify condition with Bool
        let s2 = unify(&cond_ty, &Type::Bool).map_err(|e| {
            TypeError::from_unify_error(e, if_expr.condition.position())
                .with_context("condition must be a boolean".to_string())
        })?;

        // 3. Compose and apply to environment
        let s3 = s2.compose(&s1);
        let env1 = env.apply_subst(&s3);

        // 4. Infer then branch
        let (s4, then_ty) = self.infer_expr(&env1, &if_expr.then_expr)?;
        let s5 = s4.compose(&s3);
        let env2 = env.apply_subst(&s5);

        // 5. Infer else branch
        let (s6, else_ty) = self.infer_expr(&env2, &if_expr.else_expr)?;
        let s7 = s6.compose(&s5);

        // 6. Unify both branches
        let then_ty_subst = s7.apply(&then_ty);
        let else_ty_subst = s7.apply(&else_ty);

        let s8 = unify(&then_ty_subst, &else_ty_subst).map_err(|e| {
            TypeError::from_unify_error(e, if_expr.else_expr.position())
                .with_context("if branches must have the same type".to_string())
        })?;

        // 7. Final type and substitution
        let s9 = s8.compose(&s7);
        let result_ty = s9.apply(&then_ty_subst);

        Ok((s9, result_ty))
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
        // Start with builtins in the environment
        let mut env = TypeEnv::empty();

        // Add print: forall a. a -> Unit
        let type_var = self.fresh_var();
        let print_type = Type::func(Type::Var(type_var.clone()), Type::Unit);
        env = env.extend(
            "print".to_string(),
            TypeScheme::polymorphic(vec![type_var], print_type),
        );

        let mut errors = Vec::new();

        // Pass 1: Add all functions with fresh type variables (enables recursion)
        // This allows functions to reference themselves and each other
        let mut all_functions = Vec::new();
        for func in &program.functions {
            let fresh_var = self.fresh_var();
            env = env.extend(
                func.name.value.clone(),
                TypeScheme::monomorphic(Type::Var(fresh_var)),
            );
            all_functions.push(func);
        }
        // Add main with a fresh type variable
        let fresh_var = self.fresh_var();
        env = env.extend(
            program.main.name.value.clone(),
            TypeScheme::monomorphic(Type::Var(fresh_var)),
        );
        all_functions.push(&program.main);

        // Pass 2: Infer types for all functions with recursive environment
        // Collect substitutions and apply them to the environment
        let mut global_subst = Substitution::empty();
        let mut inferred_types: HashMap<String, Type> = HashMap::new();
        for func in all_functions {
            // Apply accumulated substitutions to environment before inferring
            let env_subst = env.apply_subst(&global_subst);

            match self.infer_function(&env_subst, func) {
                Ok((subst, ty)) => {
                    // Compose substitutions
                    global_subst = subst.compose(&global_subst);
                    // Apply accumulated substitution to the inferred type
                    let ty_resolved = global_subst.apply(&ty);
                    inferred_types.insert(func.name.value.clone(), ty_resolved.clone());

                    // Update the environment with the resolved type
                    // This allows subsequent functions to see the concrete type
                    env = env.extend(
                        func.name.value.clone(),
                        TypeScheme::monomorphic(ty_resolved),
                    );
                }
                Err(err) => {
                    errors.push(err);
                }
            }
        }

        // Pass 3: Build final environment with generalized types
        let mut final_env = TypeEnv::empty();
        for func in &program.functions {
            if let Some(ty) = inferred_types.get(&func.name.value) {
                let scheme = self.generalize(&final_env, ty);
                final_env = final_env.extend(func.name.value.clone(), scheme);
            }
        }
        // Add main
        if let Some(ty) = inferred_types.get(&program.main.name.value) {
            let scheme = self.generalize(&final_env, ty);
            final_env = final_env.extend(program.main.name.value.clone(), scheme);
        }

        if errors.is_empty() {
            Ok(final_env)
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
    use crate::ast::expression::{BinOpKind, UnaryOpKind};

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

    // ===== Boolean and Comparison Operator Type Tests =====

    #[test]
    fn test_infer_boolean_true() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let bool_expr = boolean_expr(true);
        let (_, ty) = infer.infer_expr(&env, &bool_expr).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_boolean_false() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let bool_expr = boolean_expr(false);
        let (_, ty) = infer.infer_expr(&env, &bool_expr).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_comparison_equal() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Eq,
            left: Box::new(int_expr(5)),
            right: Box::new(int_expr(10)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_comparison_less_than() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Lt,
            left: Box::new(int_expr(5)),
            right: Box::new(int_expr(10)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_all_comparison_operators() {
        let ops = vec![
            BinOpKind::Eq,
            BinOpKind::NotEq,
            BinOpKind::Lt,
            BinOpKind::Gt,
            BinOpKind::LtEq,
            BinOpKind::GtEq,
        ];

        for op in ops {
            let mut infer = Infer::new();
            let env = TypeEnv::empty();

            let binop = CoreExpr::BinaryOp(CoreBinaryOp {
                op,
                left: Box::new(int_expr(1)),
                right: Box::new(int_expr(2)),
                position: Span::default(),
                info: (),
            });

            let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
            assert_eq!(ty, Type::Bool, "Failed for operator: {:?}", op);
        }
    }

    #[test]
    fn test_infer_comparison_with_variables() {
        let mut infer = Infer::new();
        let mut env = TypeEnv::empty();

        // Add x: Int and y: Int to environment
        env = env.extend("x".to_string(), TypeScheme::monomorphic(Type::Int));
        env = env.extend("y".to_string(), TypeScheme::monomorphic(Type::Int));

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Lt,
            left: Box::new(ident_expr("x")),
            right: Box::new(ident_expr("y")),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_comparison_type_error() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // Try to compare Int with String
        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Eq,
            left: Box::new(int_expr(5)),
            right: Box::new(string_expr("hello")),
            position: Span::default(),
            info: (),
        });

        let result = infer.infer_expr(&env, &binop);
        assert!(result.is_err(), "Expected type error");
    }

    #[test]
    fn test_infer_boolean_binding() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // result := 5 < 10
        let assignment = CoreAssignment {
            name: CoreIdent {
                value: "result".to_string(),
                position: Span::default(),
                info: (),
            },
            value: Box::new(CoreExpr::BinaryOp(CoreBinaryOp {
                op: BinOpKind::Lt,
                left: Box::new(int_expr(5)),
                right: Box::new(int_expr(10)),
                position: Span::default(),
                info: (),
            })),
            position: Span::default(),
            info: (),
        };

        let (_, _, new_env) = infer.infer_assignment(&env, &assignment).unwrap();

        // Check that result has type Bool
        let result_scheme = new_env.lookup("result").unwrap();
        assert_eq!(result_scheme.ty, Type::Bool);
    }

    // ===== Logical Operator Type Tests =====

    #[test]
    fn test_infer_logical_not() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let unop = CoreExpr::UnaryOp(CoreUnaryOp {
            op: UnaryOpKind::Not,
            operand: Box::new(boolean_expr(true)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &unop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_logical_and() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::And,
            left: Box::new(boolean_expr(true)),
            right: Box::new(boolean_expr(false)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_logical_or() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Or,
            left: Box::new(boolean_expr(false)),
            right: Box::new(boolean_expr(true)),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_logical_not_type_error() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // Try to apply ! to an integer
        let unop = CoreExpr::UnaryOp(CoreUnaryOp {
            op: UnaryOpKind::Not,
            operand: Box::new(int_expr(42)),
            position: Span::default(),
            info: (),
        });

        let result = infer.infer_expr(&env, &unop);
        assert!(result.is_err(), "Expected type error for !42");
    }

    #[test]
    fn test_infer_logical_and_type_error_left() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // Try: 5 && true (left is not bool)
        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::And,
            left: Box::new(int_expr(5)),
            right: Box::new(boolean_expr(true)),
            position: Span::default(),
            info: (),
        });

        let result = infer.infer_expr(&env, &binop);
        assert!(result.is_err(), "Expected type error for 5 && true");
    }

    #[test]
    fn test_infer_logical_and_type_error_right() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // Try: true && 10 (right is not bool)
        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::And,
            left: Box::new(boolean_expr(true)),
            right: Box::new(int_expr(10)),
            position: Span::default(),
            info: (),
        });

        let result = infer.infer_expr(&env, &binop);
        assert!(result.is_err(), "Expected type error for true && 10");
    }

    #[test]
    fn test_infer_logical_with_variables() {
        let mut infer = Infer::new();
        let mut env = TypeEnv::empty();

        // Add x: Bool and y: Bool to environment
        env = env.extend("x".to_string(), TypeScheme::monomorphic(Type::Bool));
        env = env.extend("y".to_string(), TypeScheme::monomorphic(Type::Bool));

        let binop = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::And,
            left: Box::new(ident_expr("x")),
            right: Box::new(ident_expr("y")),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &binop).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    fn test_infer_complex_logical_expression() {
        let mut infer = Infer::new();
        let env = TypeEnv::empty();

        // (!true) || (false && true)
        let not_expr = CoreExpr::UnaryOp(CoreUnaryOp {
            op: UnaryOpKind::Not,
            operand: Box::new(boolean_expr(true)),
            position: Span::default(),
            info: (),
        });

        let and_expr = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::And,
            left: Box::new(boolean_expr(false)),
            right: Box::new(boolean_expr(true)),
            position: Span::default(),
            info: (),
        });

        let or_expr = CoreExpr::BinaryOp(CoreBinaryOp {
            op: BinOpKind::Or,
            left: Box::new(not_expr),
            right: Box::new(and_expr),
            position: Span::default(),
            info: (),
        });

        let (_, ty) = infer.infer_expr(&env, &or_expr).unwrap();
        assert_eq!(ty, Type::Bool);
    }

    // Helper functions for tests

    fn boolean_expr(value: bool) -> CoreExpr<()> {
        CoreExpr::Boolean(CoreBoolean {
            value,
            position: Span::default(),
            info: (),
        })
    }
}
