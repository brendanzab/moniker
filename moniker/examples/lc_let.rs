//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with nested let bindings

#[macro_use]
extern crate moniker;

use moniker::{Embed, FreeVar, Nest, Scope, Var};
use std::rc::Rc;

/// Expressions
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Lambda expressions
    Lam(Scope<FreeVar<String>, RcExpr>),
    /// Function application
    App(RcExpr, RcExpr),
    /// Nested let bindings
    Let(Scope<Nest<(FreeVar<String>, Embed<RcExpr>)>, RcExpr>),
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
            Expr::App(ref fun, ref arg) => {
                RcExpr::from(Expr::App(fun.substs(mappings), arg.substs(mappings)))
            },
            Expr::Let(ref scope) => RcExpr::from(Expr::Let(Scope {
                unsafe_pattern: Nest {
                    unsafe_patterns: scope
                        .unsafe_pattern
                        .unsafe_patterns
                        .iter()
                        .map(|&(ref n, Embed(ref value))| {
                            (n.clone(), Embed(value.substs(mappings)))
                        })
                        .collect(),
                },
                unsafe_body: scope.unsafe_body.substs(mappings),
            })),
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
                eval(&body.substs(&[(name, eval(arg))]))
            },
            _ => expr.clone(),
        },
        Expr::Let(ref scope) => {
            let (bindings, body) = scope.clone().unbind();
            let mut mappings = Vec::with_capacity(bindings.unsafe_patterns.len());

            for (name, Embed(value)) in bindings.unnest() {
                let value = eval(&value.substs(&mappings));
                mappings.push((name, value));
            }

            eval(&body.substs(&mappings))
        },
    }
}

#[test]
fn test_eval() {
    // expr = (\x -> x) y
    let expr = RcExpr::from(Expr::App(
        RcExpr::from(Expr::Lam(Scope::new(
            FreeVar::user("x"),
            RcExpr::from(Expr::Var(Var::user("x"))),
        ))),
        RcExpr::from(Expr::Var(Var::user("y"))),
    ));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))));
}

#[test]
fn test_eval_let() {
    // expr =
    //      let id = \x -> x
    //          foo =  y
    //          bar = id foo
    //      in bar
    let expr = RcExpr::from(Expr::Let(Scope::new(
        Nest::new(vec![
            (
                FreeVar::user("id"),
                Embed(RcExpr::from(Expr::Lam(Scope::new(
                    FreeVar::user("x"),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                )))),
            ),
            (
                FreeVar::user("foo"),
                Embed(RcExpr::from(Expr::Var(Var::user("y")))),
            ),
            (
                FreeVar::user("bar"),
                Embed(RcExpr::from(Expr::App(
                    RcExpr::from(Expr::Var(Var::user("id"))),
                    RcExpr::from(Expr::Var(Var::user("foo"))),
                ))),
            ),
        ]),
        RcExpr::from(Expr::Var(Var::user("bar"))),
    )));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))));
}

fn main() {}
