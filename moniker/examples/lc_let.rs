//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with nested let bindings

#[macro_use]
extern crate moniker;

use moniker::{Binder, Embed, FreeVar, Nest, Scope, Subst, Var};
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
    /// Nested let bindings
    Let(Scope<Nest<(Binder<String>, Embed<RcExpr>)>, RcExpr>),
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
            Expr::Var(ref n) => match mappings.iter().find(|&(n2, _)| n == n2) {
                Some((_, ref subst_expr)) => subst_expr.clone(),
                None => self.clone(),
            },
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

// TODO: Implement this, then figure out how to derive it!
impl Subst<String, RcExpr> for RcExpr {
    fn subst(&mut self, name: &FreeVar<String>, replacement: &RcExpr) {
        unimplemented!()
    }

    fn substs(&mut self, mappings: &[(FreeVar<String>, RcExpr)]) {
        unimplemented!()
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
            Binder::user("x"),
            RcExpr::from(Expr::Var(Var::user("x"))),
        ))),
        RcExpr::from(Expr::Var(Var::user("y"))),
    ));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))),);
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
                Binder::user("id"),
                Embed(RcExpr::from(Expr::Lam(Scope::new(
                    Binder::user("x"),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                )))),
            ),
            (
                Binder::user("foo"),
                Embed(RcExpr::from(Expr::Var(Var::user("y")))),
            ),
            (
                Binder::user("bar"),
                Embed(RcExpr::from(Expr::App(
                    RcExpr::from(Expr::Var(Var::user("id"))),
                    RcExpr::from(Expr::Var(Var::user("foo"))),
                ))),
            ),
        ]),
        RcExpr::from(Expr::Var(Var::user("bar"))),
    )));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))),);
}

fn main() {}
