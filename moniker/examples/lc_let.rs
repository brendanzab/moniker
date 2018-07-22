//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with nested let bindings

#[macro_use]
extern crate moniker;

use moniker::{Binder, Embed, Nest, Scope, Var};
use std::rc::Rc;

/// Expressions
///
/// ```text
/// e ::= x                             variables
///     | \x => e                       anonymous functions
///     | e₁ e₂                         function application
///     | let* x₁=e₁, ..., xₙ=eₙ in e   nested let bindings
/// ````
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
            Expr::Var(ref var) => match mappings.iter().find(|&(name, _)| var == name) {
                Some((_, ref replacement)) => replacement.clone(),
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

/// Evaluate an expression into its normal form
pub fn eval(expr: &RcExpr) -> RcExpr {
    match *expr.inner {
        Expr::Var(_) | Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun).inner {
            Expr::Lam(ref scope) => {
                let (binder, body) = scope.clone().unbind();
                eval(&body.substs(&[(binder, eval(arg))]))
            },
            _ => expr.clone(),
        },
        Expr::Let(ref scope) => {
            let (bindings, body) = scope.clone().unbind();
            let mut mappings = Vec::with_capacity(bindings.unsafe_patterns.len());

            for (binder, Embed(value)) in bindings.unnest() {
                let value = eval(&value.substs(&mappings));
                mappings.push((binder, value));
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
