//! An example of using the `nameless` library to implement the untyped lambda
//! calculus

#[macro_use]
extern crate nameless;

use std::rc::Rc;
use nameless::{BoundTerm, Name, Scope, Var};

#[derive(Debug, Clone)]
pub enum Env {
    Empty,
    Extend(Rc<Env>, Name, Rc<Expr>),
}

fn extend(env: Rc<Env>, name: Name, expr: Rc<Expr>) -> Rc<Env> {
    Rc::new(Env::Extend(env, name, expr))
}

fn lookup<'a>(mut env: &'a Rc<Env>, name: &Name) -> Option<&'a Rc<Expr>> {
    while let Env::Extend(ref next_env, ref curr_name, ref expr) = **env {
        if Name::term_eq(curr_name, name) {
            return Some(expr);
        } else {
            env = next_env;
        }
    }
    None
}

#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    Var(Var),
    Lam(Scope<Name, Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn eval(env: &Rc<Env>, expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => lookup(env, name).unwrap_or(expr).clone(),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(env, fun) {
            Expr::Lam(ref scope) => {
                let (name, body) = nameless::unbind(scope.clone());
                eval(&extend(env.clone(), name, eval(env, arg)), &body)
            },
            _ => expr.clone(),
        },
    }
}

#[test]
fn test_eval() {
    // expr = (\x -> x) y
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::bind(
            Name::user("x"),
            Rc::new(Expr::Var(Var::Free(Name::user("x")))),
        ))),
        Rc::new(Expr::Var(Var::Free(Name::user("y")))),
    ));

    assert_term_eq!(
        eval(&Rc::new(Env::Empty), &expr),
        Rc::new(Expr::Var(Var::Free(Name::user("y")))),
    );
}

fn main() {}
