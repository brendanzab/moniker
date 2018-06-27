//! An example of using the `moniker` library to implement the untyped lambda
//! calculus

#[macro_use]
extern crate moniker;

use moniker::{Binder, FreeVar, Scope, Subst, Var};
use std::rc::Rc;

/// Expressions
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
    fn subst<N>(&self, name: &N, replacement: &RcExpr) -> RcExpr
    where
        Var<String>: PartialEq<N>,
    {
        match *self.inner {
            Expr::Var(ref n) if n == name => replacement.clone(),
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

// TODO: Implement this, then figure out how to derive it!
impl Subst<String, RcExpr> for RcExpr {
    fn subst(&mut self, name: &FreeVar<String>, replacement: &RcExpr) {
        match Rc::make_mut(&mut self.inner) {
            Expr::Var(ref mut var) => var.subst(name, replacement),
            Expr::Lam(ref mut scope) => scope.subst(name, replacement),
            Expr::App(ref mut fun, ref mut arg) => {
                fun.subst(name, replacement);
                arg.subst(name, replacement);
            },
        }
    }

    fn substs(&mut self, mappings: &[(FreeVar<String>, RcExpr)]) {
        match Rc::make_mut(&mut self.inner) {
            Expr::Var(ref mut var) => var.substs(mappings),
            Expr::Lam(ref mut scope) => scope.substs(mappings),
            Expr::App(ref mut fun, ref mut arg) => {
                fun.substs(mappings);
                arg.substs(mappings);
            },
        }
    }
}

/// Evaluate an expression into its normal form
pub fn eval(expr: &RcExpr) -> RcExpr {
    match *expr.inner {
        Expr::Var(_) | Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun).inner {
            Expr::Lam(ref scope) => {
                let (name, body) = scope.clone().unbind();
                eval(&body.subst(&name, &eval(arg)))
            },
            _ => expr.clone(),
        },
    }
}

#[test]
fn test_eval() {
    // expr = (\x -> x) y
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            Binder::user("x"),
            RcExpr::from(Expr::Var(Var::user("x"))),
        ))),
        RcExpr::from(Expr::Var(Var::user("y"))),
    ));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))),);
}

fn main() {}
