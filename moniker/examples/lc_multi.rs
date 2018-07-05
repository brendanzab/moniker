//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with multibinders

#[macro_use]
extern crate moniker;

use moniker::{FreeVar, Multi, Scope, Var};
use std::rc::Rc;

/// Expressions
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Lambda expressions
    Lam(Scope<Multi<FreeVar<String>>, RcExpr>),
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
    fn substs(&self, mappings: &[(FreeVar<String>, RcExpr)]) -> RcExpr {
        match *self.inner {
            Expr::Var(Var::Free(ref n)) => match mappings.iter().find(|&(n2, _)| n == n2) {
                Some((_, ref subst_expr)) => subst_expr.clone(),
                None => self.clone(),
            },
            Expr::Var(_) => self.clone(),
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
                let (Multi(params), body) = scope.clone().unbind();

                if params.len() != args.len() {
                    Err(EvalError::ArgumentCountMismatch {
                        expected: params.len(),
                        given: args.len(),
                    })
                } else {
                    let mappings = <_>::zip(
                        params.into_iter(),
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
fn test_eval() {
    // expr = (fn(x, y) -> y)(a, b)
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            Multi(vec![FreeVar::user("x"), FreeVar::user("y")]),
            RcExpr::from(Expr::Var(Var::Free(FreeVar::user("y")))),
        ))),
        vec![
            RcExpr::from(Expr::Var(Var::Free(FreeVar::user("a")))),
            RcExpr::from(Expr::Var(Var::Free(FreeVar::user("b")))),
        ],
    ));

    assert_term_eq!(
        eval(&expr).unwrap(),
        RcExpr::from(Expr::Var(Var::Free(FreeVar::user("b")))),
    );
}

fn main() {}
