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
    Lam(Scope<FreeVar<String>, Rc<Expr>>),
    /// Function application
    App(Rc<Expr>, Rc<Expr>),
    /// Mutually recursive let bindings
    LetRec(Scope<Rec<Multi<(FreeVar<String>, Embed<Rc<Expr>>)>>, Rc<Expr>>),
}

// FIXME: auto-derive this somehow!
fn subst(expr: &Rc<Expr>, subst_name: &FreeVar<String>, subst_expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(ref n)) if subst_name == n => subst_expr.clone(),
        Expr::Var(_) => expr.clone(),
        Expr::Lam(ref scope) => Rc::new(Expr::Lam(Scope {
            unsafe_pattern: scope.unsafe_pattern.clone(),
            unsafe_body: subst(&scope.unsafe_body, subst_name, subst_expr),
        })),
        Expr::App(ref fun, ref arg) => Rc::new(Expr::App(
            subst(fun, subst_name, subst_expr),
            subst(arg, subst_name, subst_expr),
        )),
        Expr::LetRec(ref scope) => {
            let Multi(ref bindings) = scope.unsafe_pattern.unsafe_pattern;

            Rc::new(Expr::LetRec(Scope {
                unsafe_pattern: Rec {
                    unsafe_pattern: Multi(
                        bindings
                            .iter()
                            .map(|&(ref n, Embed(ref value))| {
                                (n.clone(), Embed(subst(value, subst_name, subst_expr)))
                            })
                            .collect(),
                    ),
                },
                unsafe_body: subst(&scope.unsafe_body, subst_name, subst_expr),
            }))
        },
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
                eval(&subst(&body, &name, &eval(arg)))
            },
            _ => expr.clone(),
        },
        Expr::LetRec(ref scope) => {
            let (bindings, mut body) = scope.clone().unbind();
            let Multi(bindings) = bindings.unrec();

            // substitute the variable definitions all (once) throughout the body
            for &(ref name, Embed(ref binding)) in &bindings {
                body = subst(&body, name, binding);
            }

            // garbage collect, if possible
            // FIXME: `free_vars` is slow! We probably want this to be faster - see issue #10
            let fvs = body.free_vars();
            if bindings.iter().any(|&(ref name, _)| fvs.contains(name)) {
                Rc::new(Expr::LetRec(Scope::new(Rec::new(&Multi(bindings)), body)))
            } else {
                eval(&body)
            }
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
fn test_eval_let_rec() {
    // expr =
    //      letrec
    //          test = id x
    //          id =  \x -> x
    //      in
    //          test
    let expr = Rc::new(Expr::LetRec(Scope::new(
        Rec::new(&Multi(vec![
            (
                FreeVar::user("test"),
                Embed(Rc::new(Expr::App(
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("id")))),
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
                ))),
            ),
            (
                FreeVar::user("id"),
                Embed(Rc::new(Expr::Lam(Scope::new(
                    FreeVar::user("x"),
                    Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
                )))),
            ),
        ])),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("test")))),
    )));

    assert_term_eq!(
        eval(&expr),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
    );
}

fn main() {}
