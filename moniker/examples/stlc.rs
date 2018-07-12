//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus
//!
//! We use [bidirectional type checking](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
//! to get some level of type inference.

extern crate im;
#[macro_use]
extern crate moniker;
#[macro_use]
extern crate proptest;

use im::HashMap;
use moniker::{BoundTerm, Embed, FreeVar, Scope, Var};
use std::rc::Rc;

/// Types
#[derive(Debug, Clone, BoundTerm)]
pub enum Type {
    /// Integers
    Int,
    /// Floating point numbers
    Float,
    /// Strings
    String,
    /// Function types
    Arrow(RcType, RcType),
}

/// Reference counted types
#[derive(Debug, Clone, BoundTerm)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

/// Literal values
#[derive(Debug, Clone, BoundTerm)]
pub enum Literal {
    /// Integer literals
    Int(i32),
    /// Floating point literals
    Float(f32),
    /// String literals
    String(String),
}

/// Expressions
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Annotated expressions
    Ann(RcExpr, RcType),
    /// Literals
    Literal(Literal),
    /// Variables
    Var(Var<String>),
    /// Lambda expressions, with an optional type annotation for the parameter
    Lam(Scope<(FreeVar<String>, Embed<Option<RcType>>), RcExpr>),
    /// Function application
    App(RcExpr, RcExpr),
}

/// Reference counted expressions
#[derive(Debug, Clone, BoundTerm)]
pub struct RcExpr {
    pub inner: Rc<Expr>,
}

impl From<Expr> for RcExpr {
    fn from(src: Expr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl RcExpr {
    // FIXME: auto-derive this somehow!
    fn subst(&self, name: &FreeVar<String>, replacement: &RcExpr) -> RcExpr {
        match *self.inner {
            Expr::Ann(ref expr, ref ty) => {
                RcExpr::from(Expr::Ann(expr.subst(name, replacement), ty.clone()))
            },
            Expr::Literal(_) => self.clone(),
            Expr::Var(Var::Free(ref n)) if name == n => replacement.clone(),
            Expr::Var(_) => self.clone(),
            Expr::Lam(ref scope) => RcExpr::from(Expr::Lam(Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: scope.unsafe_body.subst(name, replacement),
            })),
            Expr::App(ref fun, ref arg) => RcExpr::from(Expr::App(
                fun.subst(name, replacement),
                arg.subst(name, replacement),
            )),
        }
    }
}

/// Evaluate an expression into its normal form
pub fn eval(expr: &RcExpr) -> RcExpr {
    match *expr.inner {
        Expr::Ann(ref expr, _) => eval(expr),
        Expr::Literal(_) | Expr::Var(_) | Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun).inner {
            Expr::Lam(ref scope) => {
                let ((name, _), body) = scope.clone().unbind();
                eval(&body.subst(&name, &eval(arg)))
            },
            _ => expr.clone(),
        },
    }
}

/// A context containing a series of type annotations
type Context = HashMap<FreeVar<String>, RcType>;

/// Check that a (potentially ambiguous) expression conforms to a given type
pub fn check(context: &Context, expr: &RcExpr, expected_ty: &RcType) -> Result<(), String> {
    match (&*expr.inner, &*expected_ty.inner) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            if let ((name, Embed(None)), body) = scope.clone().unbind() {
                check(&context.insert(name, param_ty.clone()), &body, ret_ty)?;
                return Ok(());
            }
        },
        (_, _) => {},
    }

    let inferred_ty = infer(&context, expr)?;

    if RcType::term_eq(&inferred_ty, expected_ty) {
        Ok(())
    } else {
        Err(format!(
            "type mismatch - found `{:?}` but expected `{:?}`",
            inferred_ty, expected_ty
        ))
    }
}

/// Synthesize the types of unambiguous expressions
pub fn infer(context: &Context, expr: &RcExpr) -> Result<RcType, String> {
    match *expr.inner {
        Expr::Ann(ref expr, ref ty) => {
            check(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Literal(Literal::Int(_)) => Ok(RcType::from(Type::Int)),
        Expr::Literal(Literal::Float(_)) => Ok(RcType::from(Type::Float)),
        Expr::Literal(Literal::String(_)) => Ok(RcType::from(Type::String)),
        Expr::Var(Var::Free(ref name)) => match context.get(name) {
            Some(term) => Ok((*term).clone()),
            None => Err(format!("`{:?}` not found", name)),
        },
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(ref scope) => match scope.clone().unbind() {
            ((name, Embed(Some(ann))), body) => {
                let body_ty = infer(&context.insert(name, ann.clone()), &body)?;
                Ok(RcType::from(Type::Arrow(ann, body_ty)))
            },
            ((name, Embed(None)), _) => {
                Err(format!("type annotation needed for argument `{:?}`", name))
            },
        },
        Expr::App(ref fun, ref arg) => match *infer(context, fun)?.inner {
            Type::Arrow(ref param_ty, ref ret_ty) => {
                let arg_ty = infer(context, arg)?;
                if RcType::term_eq(param_ty, &arg_ty) {
                    Ok(ret_ty.clone())
                } else {
                    Err(format!(
                        "argument type mismatch - found `{:?}` but expected `{:?}`",
                        arg_ty, param_ty,
                    ))
                }
            },
            _ => Err(format!("`{:?}` is not a function", fun)),
        },
    }
}

/// Proptest strategies
mod arb {
    use proptest::prelude::*;

    use super::*;

    /// `RcType` strategies
    pub mod ty {
        use super::*;

        pub fn any() -> impl Strategy<Value = RcType> {
            prop_oneof![
                Just(RcType::from(Type::Int)),
                Just(RcType::from(Type::Float)),
                Just(RcType::from(Type::String)),
            ].prop_recursive(8, 256, 2, |inner| {
                (inner.clone(), inner.clone())
                    .prop_map(|(param, body)| RcType::from(Type::Arrow(param, body)))
            })
        }
    }

    /// `Literal` strategies
    pub mod literal {
        use super::*;

        pub fn int(val: impl Strategy<Value = i32>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::Int)
        }

        pub fn float(val: impl Strategy<Value = f32>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::Float)
        }

        pub fn string(val: impl Strategy<Value = String>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::String)
        }

        pub fn any_int() -> impl Strategy<Value = Literal> {
            int(proptest::arbitrary::any::<i32>())
        }

        pub fn any_float() -> impl Strategy<Value = Literal> {
            float(proptest::arbitrary::any::<f32>())
        }

        pub fn any_string() -> impl Strategy<Value = Literal> {
            string(proptest::arbitrary::any::<String>())
        }

        pub fn any() -> impl Strategy<Value = Literal> {
            prop_oneof![any_int(), any_float(), any_string()]
        }
    }

    /// `RcExpr` strategies
    pub mod expr {
        use super::*;

        pub fn literal(literal: impl Strategy<Value = Literal>) -> impl Strategy<Value = RcExpr> {
            literal.prop_map(|lit| RcExpr::from(Expr::Literal(lit)))
        }

        pub fn any_literal() -> impl Strategy<Value = RcExpr> {
            literal(literal::any())
        }

        pub fn any() -> impl Strategy<Value = RcExpr> {
            prop_oneof![any_literal()]
        }
    }
}

// Property tests

proptest! {
    #[test]
    fn check_int(expr in arb::expr::literal(arb::literal::any_int())) {
        check(&Context::new(), &expr, &RcType::from(Type::Int)).unwrap();
    }

    #[test]
    fn check_float(expr in arb::expr::literal(arb::literal::any_float())) {
        check(&Context::new(), &expr, &RcType::from(Type::Float)).unwrap();
    }

    #[test]
    fn check_string(expr in arb::expr::literal(arb::literal::any_string())) {
        check(&Context::new(), &expr, &RcType::from(Type::String)).unwrap();
    }

    #[test]
    fn infer_int(expr in arb::expr::literal(arb::literal::any_int())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::Int));
    }

    #[test]
    fn infer_float(expr in arb::expr::literal(arb::literal::any_float())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::Float));
    }

    #[test]
    fn infer_string(expr in arb::expr::literal(arb::literal::any_string())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::String));
    }

    #[test]
    fn infer_app_literal_is_err(
        expr1 in arb::expr::any_literal(),
        expr2 in arb::expr::any_literal(),
    ) {
        infer(&Context::new(), &RcExpr::from(Expr::App(expr1, expr2))).unwrap_err();
    }
}

// Unit tests

#[test]
fn test_infer() {
    // expr = (\x : Int -> x)
    let expr = RcExpr::from(Expr::Lam(Scope::new(
        (FreeVar::user("x"), Embed(Some(RcType::from(Type::Int)))),
        RcExpr::from(Expr::Var(Var::Free(FreeVar::user("x")))),
    )));

    assert_term_eq!(
        infer(&Context::new(), &expr).unwrap(),
        RcType::from(Type::Arrow(
            RcType::from(Type::Int),
            RcType::from(Type::Int)
        )),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
