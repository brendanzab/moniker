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
    Lam(Scope<FreeVar<String>, Rc<Expr>>),
    /// Function application
    App(Rc<Expr>, Rc<Expr>),
    /// Nested let bindings
    Let(Scope<Nest<(FreeVar<String>, Embed<Rc<Expr>>)>, Rc<Expr>>),
}

// FIXME: auto-derive this somehow!
fn substs(expr: &Rc<Expr>, mappings: &[(FreeVar<String>, Rc<Expr>)]) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(ref n)) => match mappings.iter().find(|&(n2, _)| n == n2) {
            Some((_, ref subst_expr)) => subst_expr.clone(),
            None => expr.clone(),
        },
        Expr::Var(_) => expr.clone(),
        Expr::Lam(ref scope) => Rc::new(Expr::Lam(Scope {
            unsafe_pattern: scope.unsafe_pattern.clone(),
            unsafe_body: substs(&scope.unsafe_body, mappings),
        })),
        Expr::App(ref fun, ref arg) => {
            Rc::new(Expr::App(substs(fun, mappings), substs(arg, mappings)))
        },
        Expr::Let(ref scope) => Rc::new(Expr::Let(Scope {
            unsafe_pattern: Nest {
                unsafe_patterns: scope
                    .unsafe_pattern
                    .unsafe_patterns
                    .iter()
                    .map(|&(ref n, Embed(ref value))| (n.clone(), Embed(substs(value, mappings))))
                    .collect(),
            },
            unsafe_body: substs(&scope.unsafe_body, mappings),
        })),
    }
}

/// Evaluate an expression into its normal form
pub fn eval(expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(_)) => expr.clone(),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun) {
            Expr::Lam(ref scope) => {
                let (name, body) = scope.clone().unbind();
                eval(&substs(&body, &[(name, eval(arg))]))
            },
            _ => expr.clone(),
        },
        Expr::Let(ref scope) => {
            let (bindings, body) = scope.clone().unbind();
            let mut mappings = Vec::with_capacity(bindings.unsafe_patterns.len());

            for (name, Embed(value)) in bindings.unnest() {
                let value = eval(&substs(&value, &mappings));
                mappings.push((name, value));
            }

            eval(&substs(&body, &mappings))
        },
    }
}

#[test]
fn test_eval() {
    // expr = (\x -> x) y
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::new(
            FreeVar::user("x"),
            Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
        ))),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    ));

    assert_term_eq!(
        eval(&expr),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    );
}

#[test]
fn test_eval_let() {
    // expr =
    //      let id = \x -> x
    //          foo =  y
    //          bar = id foo
    //      in bar
    let expr = Rc::new(Expr::Let(Scope::new(
        Nest::new(vec![
            (
                FreeVar::user("id"),
                Embed(Rc::new(Expr::Lam(Scope::new(
                    FreeVar::user("x"),
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
                )))),
            ),
            (
                FreeVar::user("foo"),
                Embed(Rc::new(Expr::Var(Var::Free(FreeVar::user("y"))))),
            ),
            (
                FreeVar::user("bar"),
                Embed(Rc::new(Expr::App(
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("id")))),
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("foo")))),
                ))),
            ),
        ]),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("bar")))),
    )));

    assert_term_eq!(
        eval(&expr),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    );
}

fn main() {}
