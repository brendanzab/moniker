//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with `letrec` bindings.

#[macro_use]
extern crate moniker;

use moniker::{BoundTerm, Embed, FreeVar, Multi, Rec, Scope, Var};
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
    /// Mutually recursive let bindings
    LetRec(Scope<Rec<Multi<(FreeVar<String>, Embed<RcExpr>)>>, RcExpr>),
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
    fn subst(&self, name: &FreeVar<String>, replacement: &RcExpr) -> RcExpr {
        match *self.inner {
            Expr::Var(Var::Free(ref n)) if name == n => replacement.clone(),
            Expr::Var(_) => self.clone(),
            Expr::Lam(ref scope) => RcExpr::from(Expr::Lam(Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: scope.unsafe_body.subst(name, replacement),
            })),
            Expr::App(ref fun, ref arg) => RcExpr::from(Expr::App(
                fun.subst(name, replacement),
                arg.subst(name, replacement),
            )),
            Expr::LetRec(ref scope) => {
                let Multi(ref bindings) = scope.unsafe_pattern.unsafe_pattern;

                RcExpr::from(Expr::LetRec(Scope {
                    unsafe_pattern: Rec {
                        unsafe_pattern: Multi(
                            bindings
                                .iter()
                                .map(|&(ref n, Embed(ref value))| {
                                    (n.clone(), Embed(value.subst(name, replacement)))
                                })
                                .collect(),
                        ),
                    },
                    unsafe_body: scope.unsafe_body.subst(name, replacement),
                }))
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
        Expr::LetRec(ref scope) => {
            let (bindings, mut body) = scope.clone().unbind();
            let Multi(bindings) = bindings.unrec();

            // substitute the variable definitions all (once) throughout the body
            for &(ref name, Embed(ref binding)) in &bindings {
                body = body.subst(name, binding);
            }

            // garbage collect, if possible
            // FIXME: `free_vars` is slow! We probably want this to be faster - see issue #10
            let fvs = body.free_vars();
            if bindings.iter().any(|&(ref name, _)| fvs.contains(name)) {
                RcExpr::from(Expr::LetRec(Scope::new(Rec::new(&Multi(bindings)), body)))
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
            FreeVar::user("x"),
            RcExpr::from(Expr::Var(Var::user("x"))),
        ))),
        RcExpr::from(Expr::Var(Var::user("y"))),
    ));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("y"))));
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
        Rec::new(&Multi(vec![
            (
                FreeVar::user("test"),
                Embed(RcExpr::from(Expr::App(
                    RcExpr::from(Expr::Var(Var::user("id"))),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                ))),
            ),
            (
                FreeVar::user("id"),
                Embed(RcExpr::from(Expr::Lam(Scope::new(
                    FreeVar::user("x"),
                    RcExpr::from(Expr::Var(Var::user("x"))),
                )))),
            ),
        ])),
        RcExpr::from(Expr::Var(Var::user("test"))),
    )));

    assert_term_eq!(eval(&expr), RcExpr::from(Expr::Var(Var::user("x"))));
}

fn main() {}
