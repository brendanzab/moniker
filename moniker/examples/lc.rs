//! An example of using the `moniker` library to implement the untyped lambda
//! calculus

#[macro_use]
extern crate moniker;

use moniker::{Binder, Scope, Var};
use std::rc::Rc;

/// Expressions
///
/// ```text
/// e ::= x          variables
///     | \x => e    anonymous functions
///     | e₁ e₂      function application
/// ````
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Lambda expressions
    Lam(Scope<Binder<String>, RcExpr>),
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
    fn subst<N: PartialEq<Var<String>>>(&self, name: &N, replacement: &RcExpr) -> RcExpr {
        match *self.inner {
            Expr::Var(ref var) if name == var => replacement.clone(),
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
        Expr::Var(_) | Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun).inner {
            Expr::Lam(ref scope) => {
                let (binder, body) = scope.clone().unbind();
                eval(&body.subst(&binder, &eval(arg)))
            },
            _ => expr.clone(),
        },
    }
}

#[test]
fn test_eval() {
    use moniker::FreeVar;

    let x = FreeVar::fresh(Some(String::from("x")));
    let y = FreeVar::fresh(Some(String::from("y")));

    // expr = (\x -> x) y
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            Binder(x.clone()),
            RcExpr::from(Expr::Var(Var::Free(x.clone()))),
        ))),
        RcExpr::from(Expr::Var(Var::Free(y.clone()))),
    ));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::Free(y.clone()))));
}

fn main() {}
