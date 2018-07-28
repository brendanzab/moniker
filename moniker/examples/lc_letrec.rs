//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with mutually recursive bindings.

#[macro_use]
extern crate moniker;

use moniker::{Binder, BoundTerm, Embed, Rec, Scope, Var};
use std::rc::Rc;

/// Expressions
///
/// ```text
/// e ::= x                             variables
///     | \x => e                       anonymous functions
///     | e₁ e₂                         function application
///     | let x₁=e₁, ..., xₙ=eₙ in e    mutually recursive let bindings
/// ````
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Variables
    Var(Var<String>),
    /// Lambda expressions
    Lam(Scope<Binder<String>, RcExpr>),
    /// Function application
    App(RcExpr, RcExpr),
    /// Mutually recursive let bindings
    LetRec(Scope<Rec<Vec<(Binder<String>, Embed<RcExpr>)>>, RcExpr>),
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
            Expr::LetRec(ref scope) => RcExpr::from(Expr::LetRec(Scope {
                unsafe_pattern: Rec {
                    unsafe_pattern: scope
                        .unsafe_pattern
                        .unsafe_pattern
                        .iter()
                        .map(|&(ref n, Embed(ref value))| {
                            (n.clone(), Embed(value.subst(name, replacement)))
                        })
                        .collect(),
                },
                unsafe_body: scope.unsafe_body.subst(name, replacement),
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
                eval(&body.subst(&binder, &eval(arg)))
            },
            _ => expr.clone(),
        },
        Expr::LetRec(ref scope) => {
            let (bindings, mut body) = scope.clone().unbind();
            let bindings = bindings.unrec();

            // substitute the variable definitions all (once) throughout the body
            for &(ref binder, Embed(ref binding)) in &bindings {
                body = body.subst(binder, binding);
            }

            // garbage collect, if possible
            // FIXME: `free_vars` is slow! We probably want this to be faster - see issue #10
            let fvs = body.free_vars();
            if bindings.iter().any(|&(Binder(ref fv), _)| fvs.contains(fv)) {
                RcExpr::from(Expr::LetRec(Scope::new(Rec::new(bindings), body)))
            } else {
                eval(&body)
            }
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
fn test_eval_let_rec() {
    // expr =
    //      letrec
    //          test = id x
    //          id =  \x -> x
    //      in
    //          test
    let expr = RcExpr::from(Expr::LetRec(Scope::new(
        Rec::new(vec![
            (
                Binder::user("test"),
                Embed(RcExpr::from(Expr::App(
                    RcExpr::from(Expr::Var(Var::user("id"))),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                ))),
            ),
            (
                Binder::user("id"),
                Embed(RcExpr::from(Expr::Lam(Scope::new(
                    Binder::user("x"),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                )))),
            ),
        ]),
        RcExpr::from(Expr::Var(Var::user("test"))),
    )));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("x"))));
}

fn main() {}
