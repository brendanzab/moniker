//! An example of using the `moniker` library to implement the untyped lambda
//! calculus with multibinders

#[macro_use]
extern crate moniker;

use moniker::{FreeVar, Multi, Scope, Var};
use std::rc::Rc;

#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    Var(Var),
    Lam(Scope<Multi<FreeVar>, Rc<Expr>>),
    App(Rc<Expr>, Vec<Rc<Expr>>),
}

// FIXME: auto-derive this somehow!
fn substs(expr: &Rc<Expr>, mappings: &[(FreeVar, Rc<Expr>)]) -> Rc<Expr> {
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
        Expr::App(ref fun, ref args) => Rc::new(Expr::App(
            substs(fun, mappings),
            args.iter().map(|arg| substs(arg, mappings)).collect(),
        )),
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    ArgumentCountMismatch { expected: usize, given: usize },
}

pub fn eval(expr: &Rc<Expr>) -> Result<Rc<Expr>, EvalError> {
    match **expr {
        Expr::Var(Var::Free(_)) => Ok(expr.clone()),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => Ok(expr.clone()),
        Expr::App(ref fun, ref args) => match *eval(fun)? {
            Expr::Lam(ref scope) => {
                let (Multi(params), body) = scope.clone().unbind();

                if params.len() != args.len() {
                    Err(EvalError::ArgumentCountMismatch {
                        expected: params.len(),
                        given: args.len(),
                    })
                } else {
                    eval(&substs(
                        &body,
                        &<_>::zip(
                            params.into_iter(),
                            args.iter().map(|arg| eval(arg).unwrap()),
                        ).collect::<Vec<_>>(),
                    ))
                }
            },
            _ => Ok(expr.clone()),
        },
    }
}

#[test]
fn test_eval() {
    // expr = (fn(x, y) -> y)(a, b)
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::new(
            Multi(vec![FreeVar::user("x"), FreeVar::user("y")]),
            Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
        ))),
        vec![
            Rc::new(Expr::Var(Var::Free(FreeVar::user("a")))),
            Rc::new(Expr::Var(Var::Free(FreeVar::user("b")))),
        ],
    ));

    assert_term_eq!(
        eval(&expr).unwrap(),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("b")))),
    );
}

fn main() {}
