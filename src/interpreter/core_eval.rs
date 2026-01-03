//! # Core AST Interpreter
//!
//! This module implements the tree-walking interpreter for the Core AST.
//! It evaluates desugared expressions directly, which is more efficient
//! than converting back to Surface AST.
//!
//! ## Overview
//!
//! The interpreter implements a call-by-value (eager) evaluation strategy:
//!
//! - **Expression evaluation**: Recursively evaluates expressions to produce values
//! - **Statement evaluation**: Executes statements for side effects, returning a value
//! - **Closures**: Captures environment for proper lexical scoping
//! - **Builtins**: Special-cased implementations for `print` and `toString`
//!
//! ## Evaluation Strategy
//!
//! The interpreter uses **call-by-value** (eager) evaluation:
//!
//! ```text
//! Evaluate function call:
//!   1. Evaluate function expression (get closure)
//!   2. Evaluate argument expression
//!   3. Apply closure to argument
//!
//! // Example:
//! add(2 * 3, 4 + 5)
//! // Step 1: Evaluate "add" -> closure for add
//! // Step 2: Evaluate "2 * 3" -> 6
//! // Step 3: Evaluate "4 + 5" -> 9
//! // Step 4: Apply add closure to (6, 9) -> 15
//! ```
//!
//! ## Closures and Lexical Scoping
//!
//! When a lambda is evaluated, it captures the current environment
//! in a closure, enabling proper lexical scoping:
//!
//! ```text
//! makeAdder = \x => \y => x + y
//! add5 = makeAdder(5)
//! add5(3)  // Returns 8 (x is 5 from closure)
//! ```
//!
//! The closure captures `x=5` when `makeAdder(5)` is evaluated,
//! and that captured environment is used when `add5(3)` runs.
//!
//! ## Built-in Functions
//!
//! Two built-in functions are provided:
//!
//! - **`print`**: Prints value to stdout, returns unit
//!   - Prints integers, strings, booleans, and unit directly
//!   - Prints functions/lambdas as `"<function>"`
//!   - Returns `()`
//!
//! - **`toString`**: Converts any value to string
//!   - Integers: `"42"` → `"42"`
//!   - Strings: `"hello"` → `"hello"` (no quotes)
//!   - Booleans: `true` → `"true"`
//!   - Unit: `()` → `"()"`
//!   - Functions: `"<function>"`
//!
//! ## Type Safety
//!
//! The interpreter assumes type-safe code after type checking:
//! - It will **panic** if operations encounter wrong types
//! - For example: `true + 1` will panic (not type error message)
//! - This is acceptable because type checking should catch these errors
//!
//! ## Short-Circuit Evaluation
//!
//! Logical operators (`&&`, `||`) implement short-circuit evaluation:
//!
//! ```text
//! false && panic()  // Doesn't evaluate panic(), returns false
//! true || panic()    // Doesn't evaluate panic(), returns true
//! ```
//!
//! This is required for idiomatic Ruskell code (e.g., checking
//! if a value exists before using it).
//!
//! ## Pipeline Position
//!
//! ```text
//! Parser → Surface AST → Desugaring → Core AST → [INTERPRETER] → Program Result
//! ```
//!
//! ## Related Modules
//!
//! - [`crate::core`] - Core AST definitions that are evaluated
//! - [`crate::interpreter::scope`] - Scope management for variables
//! - [`crate::interpreter::value`] - Runtime value representations

use std::collections::HashMap;
use std::fmt::Debug;

use crate::core::*;

use super::scope::Scope;
use super::value::RValue;

impl<T> CoreExpr<T>
where
    T: Clone + Debug,
{
    /// Evaluate a Core expression to a runtime value.
    ///
    /// This is the main evaluation function that recursively traverses
    /// the expression tree, applying the call-by-value evaluation strategy.
    ///
    /// # Arguments
    ///
    /// * `scope` - Mutable reference to the current variable scope
    ///
    /// # Returns
    ///
    /// The runtime value produced by evaluating this expression
    ///
    /// # Evaluation Rules
    ///
    /// ```text
    /// // Literals evaluate to themselves:
    /// eval(Unit) = Unit
    /// eval(Integer(42)) = Integer(42)
    /// eval(String("hello")) = String("hello")
    /// eval(Boolean(true)) = Boolean(true)
    ///
    /// // Variables look up in scope:
    /// eval(Ident("x")) = scope["x"]
    ///
    /// // Lambdas capture environment:
    /// eval(\x => expr) = Closure(scope, \x => expr)
    ///
    /// // Function calls evaluate function and arg, then apply:
    /// eval(f(arg)) = eval(f).apply(eval(arg))
    ///
    /// // Binary ops evaluate operands, then apply operation:
    /// eval(x + y) = eval(x) + eval(y)
    ///
    /// // Unary ops evaluate operand, then apply operation:
    /// eval(!x) = !eval(x)
    ///
    /// // If-then-else evaluates condition, then appropriate branch:
    /// eval(if c then t else e) = eval(t)  if eval(c) is true
    ///                            = eval(e)  if eval(c) is false
    /// ```
    ///
    /// # Panic Conditions
    ///
    /// The interpreter will panic if:
    /// - Undefined variable is referenced (should be caught by type checker)
    /// - Type mismatch in operation (should be caught by type checker)
    /// - Division by zero (runtime error)
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            CoreExpr::Unit(_) => RValue::Unit,
            CoreExpr::Ident(ident) => {
                // Special handling for cons implementation
                if ident.value == "__CONS_IMPL__" {
                    // This is the body of the cons lambda's second argument
                    // At this point, scope should have:
                    // - __cons_elem (from captured env)
                    // - __cons_list_arg (the parameter)
                    let elem = scope
                        .resolve("__cons_elem")
                        .expect("cons: missing captured element");
                    let list_val = scope
                        .resolve("__cons_list_arg")
                        .expect("cons: missing list argument");

                    match list_val {
                        RValue::List(mut elements) => {
                            // Prepend the element
                            let mut new_list = vec![elem];
                            new_list.append(&mut elements);
                            RValue::List(new_list)
                        }
                        _ => panic!("cons: second argument must be a list, got: {:?}", list_val),
                    }
                } else {
                    scope
                        .resolve(&ident.value)
                        .unwrap_or_else(|| panic!("undefined identifier: {}", ident.value))
                }
            }
            CoreExpr::Integer(integer) => RValue::Integer(crate::ast::expression::Integer {
                value: integer.value,
                position: integer.position.clone(),
                info: integer.info.clone(),
            }),
            CoreExpr::String(string) => RValue::String(crate::ast::expression::StringLiteral {
                value: string.value.clone(),
                position: string.position.clone(),
                info: string.info.clone(),
            }),
            CoreExpr::Boolean(boolean) => RValue::Bool(boolean.value),
            CoreExpr::List(list) => {
                let elements = list.elements.iter().map(|e| e.eval(scope)).collect();
                RValue::List(elements)
            }
            CoreExpr::Lambda(lambda) => {
                // Capture the current environment for the closure
                let captured = scope.capture();
                RValue::CoreLambda(lambda.clone(), captured)
            }
            CoreExpr::FunctionCall(call) => {
                let func_value = call.func.eval(scope);
                let arg_value = call.arg.eval(scope);

                match func_value {
                    RValue::CoreLambda(lambda, captured) => lambda.run(arg_value, scope, &captured),
                    RValue::Builtin(builtin) => {
                        use super::value::Builtin;
                        use crate::ast::expression::StringLiteral;
                        match builtin {
                            Builtin::Print => {
                                // Print the value to stdout
                                match &arg_value {
                                    RValue::Unit => println!("()"),
                                    RValue::Integer(i) => println!("{}", i.value),
                                    RValue::String(s) => println!("{}", s.value), // Print without quotes
                                    RValue::Bool(b) => println!("{}", b),
                                    RValue::List(elements) => {
                                        print!("[");
                                        for (i, elem) in elements.iter().enumerate() {
                                            if i > 0 {
                                                print!(", ");
                                            }
                                            match elem {
                                                RValue::Unit => print!("()"),
                                                RValue::Integer(int) => print!("{}", int.value),
                                                RValue::String(s) => print!("\"{}\"", s.value),
                                                RValue::Bool(b) => print!("{}", b),
                                                RValue::List(_) => print!("<list>"),
                                                RValue::CoreLambda(_, _) => print!("<function>"),
                                                RValue::Lambda(_) => print!("<function>"),
                                                RValue::Builtin(_) => print!("<builtin>"),
                                            }
                                        }
                                        println!("]");
                                    }
                                    RValue::CoreLambda(_, _) => println!("<function>"),
                                    RValue::Lambda(_) => println!("<function>"),
                                    RValue::Builtin(_) => println!("<builtin>"),
                                }
                                RValue::Unit
                            }
                            Builtin::ToString => {
                                // Convert value to string
                                let string_value = match &arg_value {
                                    RValue::Unit => "()".to_string(),
                                    RValue::Integer(i) => i.value.to_string(),
                                    RValue::String(s) => s.value.clone(),
                                    RValue::Bool(b) => b.to_string(),
                                    RValue::List(elements) => {
                                        let mut result = "[".to_string();
                                        for (i, elem) in elements.iter().enumerate() {
                                            if i > 0 {
                                                result.push_str(", ");
                                            }
                                            let elem_str = match elem {
                                                RValue::Unit => "()".to_string(),
                                                RValue::Integer(int) => int.value.to_string(),
                                                RValue::String(s) => format!("\"{}\"", s.value),
                                                RValue::Bool(b) => b.to_string(),
                                                RValue::List(_) => "<list>".to_string(),
                                                RValue::CoreLambda(_, _) => {
                                                    "<function>".to_string()
                                                }
                                                RValue::Lambda(_) => "<function>".to_string(),
                                                RValue::Builtin(_) => "<builtin>".to_string(),
                                            };
                                            result.push_str(&elem_str);
                                        }
                                        result.push(']');
                                        result
                                    }
                                    RValue::CoreLambda(_, _) => "<function>".to_string(),
                                    RValue::Lambda(_) => "<function>".to_string(),
                                    RValue::Builtin(_) => "<builtin>".to_string(),
                                };
                                RValue::String(StringLiteral {
                                    value: string_value,
                                    position: call.position.clone(),
                                    info: call.info.clone(),
                                })
                            }
                            Builtin::ListIsEmpty => match arg_value {
                                RValue::List(elements) => RValue::Bool(elements.is_empty()),
                                _ => panic!("__list_isEmpty expects list, got: {:?}", arg_value),
                            },
                            Builtin::ListHead => match arg_value {
                                RValue::List(elements) => {
                                    if elements.is_empty() {
                                        panic!("Runtime error: head of empty list")
                                    } else {
                                        elements[0].clone()
                                    }
                                }
                                _ => panic!("__list_head expects list, got: {:?}", arg_value),
                            },
                            Builtin::ListTail => match arg_value {
                                RValue::List(elements) => {
                                    if elements.is_empty() {
                                        panic!("Runtime error: tail of empty list")
                                    } else {
                                        RValue::List(elements[1..].to_vec())
                                    }
                                }
                                _ => panic!("__list_tail expects list, got: {:?}", arg_value),
                            },
                            Builtin::ListCons => {
                                // cons is curried: cons(element)(list)
                                // Return a lambda that captures the element and prepends it
                                use crate::core::{
                                    CoreExpr, CoreIdent, CoreLambda, CoreLambdaBody,
                                    CoreLambdaParam,
                                };
                                use crate::interpreter::value::CapturedEnv;

                                // Capture the element in the closure environment
                                let mut env = HashMap::new();
                                env.insert("__cons_elem".to_string(), arg_value);

                                // Create a lambda that takes the list and prepends the element
                                // Body: construct new list with element at front
                                RValue::CoreLambda(
                                    CoreLambda {
                                        param: CoreLambdaParam::Ident(CoreIdent {
                                            value: "__cons_list_arg".to_string(),
                                            position: call.position.clone(),
                                            info: call.info.clone(),
                                        }),
                                        // The body will be evaluated later when the lambda is called
                                        // For now, we create a special marker that will be handled
                                        // in the lambda evaluation
                                        body: CoreLambdaBody::Expression(Box::new(
                                            CoreExpr::Ident(CoreIdent {
                                                value: "__CONS_IMPL__".to_string(),
                                                position: call.position.clone(),
                                                info: call.info.clone(),
                                            }),
                                        )),
                                        position: call.position.clone(),
                                        info: call.info.clone(),
                                    },
                                    CapturedEnv(env),
                                )
                            }
                            Builtin::MatchFailure => {
                                // Pattern match failure - this should be unreachable in well-typed code
                                panic!(
                                    "Runtime error: pattern match failure (non-exhaustive patterns)"
                                )
                            }
                        }
                    }
                    other => panic!("cannot call non-function value: {:?}", other),
                }
            }
            CoreExpr::BinaryOp(binop) => {
                use crate::ast::expression::BinOpKind;

                match binop.op {
                    // Arithmetic operators need integer operands
                    BinOpKind::Add
                    | BinOpKind::Sub
                    | BinOpKind::Mul
                    | BinOpKind::Div
                    | BinOpKind::Lt
                    | BinOpKind::Gt
                    | BinOpKind::LtEq
                    | BinOpKind::GtEq => {
                        let left = binop.left.eval(scope);
                        let right = binop.right.eval(scope);

                        let left_val = match left {
                            RValue::Integer(i) => i.value,
                            _ => panic!("left operand must be integer"),
                        };

                        let right_val = match right {
                            RValue::Integer(i) => i.value,
                            _ => panic!("right operand must be integer"),
                        };

                        match binop.op {
                            BinOpKind::Add => RValue::Integer(crate::ast::expression::Integer {
                                value: left_val + right_val,
                                position: binop.position.clone(),
                                info: binop.info.clone(),
                            }),
                            BinOpKind::Sub => RValue::Integer(crate::ast::expression::Integer {
                                value: left_val - right_val,
                                position: binop.position.clone(),
                                info: binop.info.clone(),
                            }),
                            BinOpKind::Mul => RValue::Integer(crate::ast::expression::Integer {
                                value: left_val * right_val,
                                position: binop.position.clone(),
                                info: binop.info.clone(),
                            }),
                            BinOpKind::Div => {
                                if right_val == 0 {
                                    panic!("division by zero");
                                }
                                RValue::Integer(crate::ast::expression::Integer {
                                    value: left_val / right_val,
                                    position: binop.position.clone(),
                                    info: binop.info.clone(),
                                })
                            }
                            BinOpKind::Lt => RValue::Bool(left_val < right_val),
                            BinOpKind::Gt => RValue::Bool(left_val > right_val),
                            BinOpKind::LtEq => RValue::Bool(left_val <= right_val),
                            BinOpKind::GtEq => RValue::Bool(left_val >= right_val),
                            _ => unreachable!(),
                        }
                    }

                    // Equality operators work on any type (polymorphic)
                    BinOpKind::Eq | BinOpKind::NotEq => {
                        let left = binop.left.eval(scope);
                        let right = binop.right.eval(scope);

                        let is_equal = values_equal(&left, &right);
                        match binop.op {
                            BinOpKind::Eq => RValue::Bool(is_equal),
                            BinOpKind::NotEq => RValue::Bool(!is_equal),
                            _ => unreachable!(),
                        }
                    }

                    // Logical operators need boolean operands with short-circuit
                    BinOpKind::And => {
                        let left = binop.left.eval(scope);
                        match left {
                            RValue::Bool(false) => RValue::Bool(false), // Short-circuit
                            RValue::Bool(true) => {
                                let right = binop.right.eval(scope);
                                match right {
                                    RValue::Bool(b) => RValue::Bool(b),
                                    _ => panic!("&& requires boolean operands"),
                                }
                            }
                            _ => panic!("&& requires boolean operands"),
                        }
                    }

                    BinOpKind::Or => {
                        let left = binop.left.eval(scope);
                        match left {
                            RValue::Bool(true) => RValue::Bool(true), // Short-circuit
                            RValue::Bool(false) => {
                                let right = binop.right.eval(scope);
                                match right {
                                    RValue::Bool(b) => RValue::Bool(b),
                                    _ => panic!("|| requires boolean operands"),
                                }
                            }
                            _ => panic!("|| requires boolean operands"),
                        }
                    }

                    // String concatenation
                    BinOpKind::Concat => {
                        use crate::ast::expression::StringLiteral;
                        let left = binop.left.eval(scope);
                        let right = binop.right.eval(scope);

                        let left_str = match left {
                            RValue::String(s) => s.value,
                            _ => panic!("++ left operand must be string"),
                        };

                        let right_str = match right {
                            RValue::String(s) => s.value,
                            _ => panic!("++ right operand must be string"),
                        };

                        RValue::String(StringLiteral {
                            value: left_str + &right_str,
                            position: binop.position.clone(),
                            info: binop.info.clone(),
                        })
                    }
                }
            }

            CoreExpr::UnaryOp(unop) => {
                use crate::ast::expression::UnaryOpKind;

                match unop.op {
                    UnaryOpKind::Not => {
                        let operand = unop.operand.eval(scope);
                        match operand {
                            RValue::Bool(b) => RValue::Bool(!b),
                            _ => panic!("! operator requires boolean operand"),
                        }
                    }
                }
            }

            CoreExpr::IfThenElse(if_expr) => {
                let condition = if_expr.condition.eval(scope);
                match condition {
                    RValue::Bool(true) => if_expr.then_expr.eval(scope),
                    RValue::Bool(false) => if_expr.else_expr.eval(scope),
                    _ => panic!("if condition must be boolean"),
                }
            }
        }
    }
}

impl<T> CoreStatement<T>
where
    T: Clone + Debug,
{
    /// Evaluate a Core statement to a runtime value.
    ///
    /// Statements are executed for their side effects, but
    /// they also return a value (the value of last expression
    /// in the statement, or the assigned value for assignments).
    ///
    /// # Arguments
    ///
    /// * `scope` - Mutable reference to the current variable scope
    ///
    /// # Returns
    ///
    /// The value produced by executing this statement
    ///
    /// # Statement Semantics
    ///
    /// ```text
    /// // Assignment:
    /// eval(x := expr):
    ///   1. Evaluate expr to get value
    ///   2. Bind x to value in current scope (shadows if exists)
    ///   3. Return the bound value
    ///
    /// // Expression statement:
    /// eval(expr):
    ///   Evaluate expr and return its value
    ///   (Used for side effects like print(42))
    /// ```
    pub fn eval(&self, scope: &mut Scope<T>) -> RValue<T> {
        match self {
            CoreStatement::Assignment(assignment) => {
                let value = assignment.value.eval(scope);
                scope.add(assignment.name.value.clone(), value.clone());
                value
            }
            CoreStatement::Expression(expr) => expr.eval(scope),
        }
    }
}

impl<T> CoreLambda<T>
where
    T: Clone + Debug,
{
    /// Run a lambda closure with an argument.
    ///
    /// This is called when a function value is applied to an argument.
    /// It sets up the appropriate scopes and evaluates the lambda body.
    ///
    /// # Arguments
    ///
    /// * `arg` - The argument value to apply the closure to
    /// * `scope` - Mutable reference to the current scope
    /// * `captured` - The environment captured when the closure was created
    ///
    /// # Returns
    ///
    /// The value produced by evaluating the lambda body
    ///
    /// # Execution Model
    ///
    /// ```text
    /// run(arg, scope, captured):
    ///   1. Enter new scope with captured environment as parent
    ///   2. Enter new scope for the lambda's parameters
    ///   3. Bind the parameter
    ///   4. Evaluate body in this combined scope
    ///   5. Leave the parameter scope
    ///   6. Leave the captured scope
    ///   7. Return result
    /// ```
    ///
    /// # Scope Hierarchy
    ///
    /// During lambda execution, scopes are arranged as:
    ///
    /// ```text
    /// [Parameter Scope]  <- Innermost (x, y, etc.)
    ///   [Captured Scope]  <- Environment where lambda was defined
    ///     [Global Scope]   <- Builtins and top-level bindings
    /// ```
    ///
    /// # Example
    ///
    /// ```text
    /// // Closure captured x=5:
    /// // makeAdder = \x => \y => x + y
    /// // add5 = makeAdder(5)
    ///
    /// // When running add5(3):
    /// // 1. Enter captured scope (where x=5)
    /// // 2. Enter parameter scope (where y=3)
    /// // 3. Evaluate: x + y = 5 + 3 = 8
    /// // 4. Return 8
    /// ```
    pub fn run(
        &self,
        arg: RValue<T>,
        scope: &mut Scope<T>,
        captured: &super::value::CapturedEnv<T>,
    ) -> RValue<T> {
        // First, restore the captured environment
        scope.with_captured(captured);

        // Then enter a new scope for the lambda's parameters
        scope.enter();

        // Bind the parameter
        match &self.param {
            CoreLambdaParam::Unit(_) => {
                // Unit parameter - no binding needed, but check it's unit
                if !matches!(arg, RValue::Unit) {
                    panic!("expected unit argument");
                }
            }
            CoreLambdaParam::Ident(ident) => {
                scope.add(ident.value.clone(), arg);
            }
        }

        // Evaluate the body
        let result = match &self.body {
            CoreLambdaBody::Expression(expr) => expr.eval(scope),
            CoreLambdaBody::Block(stmts) => {
                let mut result = RValue::Unit;
                for stmt in stmts {
                    result = stmt.eval(scope);
                }
                result
            }
        };

        // Leave the parameter scope
        scope.leave();
        // Leave the captured environment scope
        scope.leave();
        result
    }
}

/// Helper function to check if two runtime values are equal
fn values_equal<T>(a: &RValue<T>, b: &RValue<T>) -> bool {
    match (a, b) {
        (RValue::Unit, RValue::Unit) => true,
        (RValue::Integer(ia), RValue::Integer(ib)) => ia.value == ib.value,
        (RValue::String(sa), RValue::String(sb)) => sa.value == sb.value,
        (RValue::Bool(ba), RValue::Bool(bb)) => ba == bb,
        (RValue::List(la), RValue::List(lb)) => {
            if la.len() != lb.len() {
                false
            } else {
                la.iter().zip(lb.iter()).all(|(x, y)| values_equal(x, y))
            }
        }
        // Lambdas and CoreLambdas can't be compared for equality
        (RValue::Lambda(_), RValue::Lambda(_)) => false,
        (RValue::CoreLambda(_, _), RValue::CoreLambda(_, _)) => false,
        (RValue::Builtin(ba), RValue::Builtin(bb)) => ba == bb,
        // Different types are never equal
        _ => false,
    }
}
