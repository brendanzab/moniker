//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus
//!
//! We use [bidirectional type checking](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
//! to get some level of type inference.

extern crate im;
#[macro_use]
extern crate moniker;

use im::HashMap;
use moniker::{Binder, BoundTerm, Embed, FreeVar, Scope, Var};
use std::rc::Rc;

/// Types
///
/// ```text
/// t ::= Int                   integer types
///     | Float                 floating point types
///     | String                string types
///     | t -> t                function types
/// ```
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
///
/// ```text
/// e ::= x             variables
///     | e : t         expressions annotated with types
///     | \x => e       anonymous functions
///     | \x : t => e   anonymous functions (with type annotation)
///     | e₁ e₂         function application
/// ```
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Annotated expressions
    Ann(RcExpr, RcType),
    /// Literals
    Literal(Literal),
    /// Variables
    Var(Var<String>),
    /// Lambda expressions, with an optional type annotation for the parameter
    Lam(Scope<(Binder<String>, Embed<Option<RcType>>), RcExpr>),
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
    fn subst<N>(&self, name: &N, replacement: &RcExpr) -> RcExpr
    where
        Var<String>: PartialEq<N>,
    {
        match *self.inner {
            Expr::Ann(ref expr, ref ty) => {
                RcExpr::from(Expr::Ann(expr.subst(name, replacement), ty.clone()))
            },
            Expr::Var(ref var) if var == name => replacement.clone(),
            Expr::Var(_) | Expr::Literal(_) => self.clone(),
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
                let ((binder, _), body) = scope.clone().unbind();
                eval(&body.subst(&binder, &eval(arg)))
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
            if let ((Binder(free_var), Embed(None)), body) = scope.clone().unbind() {
                check(&context.insert(free_var, param_ty.clone()), &body, ret_ty)?;
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
        Expr::Var(Var::Free(ref free_var)) => match context.get(free_var) {
            Some(term) => Ok((*term).clone()),
            None => Err(format!("`{:?}` not found in `{:?}`", free_var, context)),
        },
        Expr::Var(Var::Bound(_, _, _)) => panic!("encountered a bound variable"),
        Expr::Lam(ref scope) => match scope.clone().unbind() {
            ((Binder(free_var), Embed(Some(ann))), body) => {
                let body_ty = infer(&context.insert(free_var, ann.clone()), &body)?;
                Ok(RcType::from(Type::Arrow(ann, body_ty)))
            },
            ((binder, Embed(None)), _) => Err(format!(
                "type annotation needed for parameter `{:?}`",
                binder
            )),
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

#[test]
fn test_infer() {
    // expr = (\x : Int -> x)
    let expr = RcExpr::from(Expr::Lam(Scope::new(
        (Binder::user("x"), Embed(Some(RcType::from(Type::Int)))),
        RcExpr::from(Expr::Var(Var::user("x"))),
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
