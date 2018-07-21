//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with multibinders

#[macro_use]
extern crate moniker;

use moniker::{Binder, Scope, Var};
use std::rc::Rc;

/// Expressions
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Lambda expressions
    Lam(Scope<Vec<Binder<String>>, RcExpr>),
    /// Function application
    App(RcExpr, Vec<RcExpr>),
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
    fn substs<N>(&self, mappings: &[(N, RcExpr)]) -> RcExpr
    where
        Var<String>: PartialEq<N>,
    {
        match *self.inner {
            Expr::Var(ref var) => match mappings.iter().find(|&(name, _)| var == name) {
                Some((_, ref replacement)) => replacement.clone(),
                None => self.clone(),
            },
            Expr::Lam(ref scope) => RcExpr::from(Expr::Lam(Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: scope.unsafe_body.substs(mappings),
            })),
            Expr::App(ref fun, ref args) => RcExpr::from(Expr::App(
                fun.substs(mappings),
                args.iter().map(|arg| arg.substs(mappings)).collect(),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    ArgumentCountMismatch { expected: usize, given: usize },
}

/// Evaluate an expression into its normal form
pub fn eval(expr: &RcExpr) -> Result<RcExpr, EvalError> {
    match *expr.inner {
        Expr::Var(_) | Expr::Lam(_) => Ok(expr.clone()),
        Expr::App(ref fun, ref args) => match *eval(fun)?.inner {
            Expr::Lam(ref scope) => {
                let (binders, body) = scope.clone().unbind();

                if binders.len() != args.len() {
                    Err(EvalError::ArgumentCountMismatch {
                        expected: binders.len(),
                        given: args.len(),
                    })
                } else {
                    let mappings = <_>::zip(
                        binders.into_iter(),
                        args.iter().map(|arg| eval(arg).unwrap()),
                    ).collect::<Vec<_>>();

                    eval(&body.substs(&mappings))
                }
            },
            _ => Ok(expr.clone()),
        },
    }
}

#[test]
fn test_eval_const_lhs() {
    // expr = (fn(x, y) -> y)(a, b)
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            vec![Binder::user("x"), Binder::user("y")],
            RcExpr::from(Expr::Var(Var::user("y"))),
        ))),
        vec![
            RcExpr::from(Expr::Var(Var::user("a"))),
            RcExpr::from(Expr::Var(Var::user("b"))),
        ],
    ));

    assert_term_eq!(
        eval(&expr).unwrap(),
        RcExpr::from(Expr::Var(Var::user("b"))),
    );
}

#[test]
fn test_eval_const_rhs() {
    // expr = (fn(x, y) -> x)(a, b)
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            vec![Binder::user("x"), Binder::user("y")],
            RcExpr::from(Expr::Var(Var::user("x"))),
        ))),
        vec![
            RcExpr::from(Expr::Var(Var::user("a"))),
            RcExpr::from(Expr::Var(Var::user("b"))),
        ],
    ));

    assert_term_eq!(
        eval(&expr).unwrap(),
        RcExpr::from(Expr::Var(Var::user("a"))),
    );
}

fn main() {}
