//! An example of using the `nameless` library to implement the untyped lambda
//! calculus

#[macro_use]
extern crate nameless;

use nameless::{BoundTerm, Embed, FreeVar, FreshState, Nest, Scope, Var};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Env {
    Empty,
    Extend(Rc<Env>, FreeVar, Rc<Expr>),
}

fn extend(env: Rc<Env>, name: FreeVar, expr: Rc<Expr>) -> Rc<Env> {
    Rc::new(Env::Extend(env, name, expr))
}

fn lookup<'a>(mut env: &'a Rc<Env>, name: &FreeVar) -> Option<&'a Rc<Expr>> {
    while let Env::Extend(ref next_env, ref curr_name, ref expr) = **env {
        if FreeVar::term_eq(curr_name, name) {
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
    Lam(Scope<FreeVar, Rc<Expr>>),
    Let(Scope<Nest<(FreeVar, Embed<Rc<Expr>>)>, Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn eval(env: &Rc<Env>, expr: &Rc<Expr>, fresh_state: &mut FreshState) -> Rc<Expr> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => lookup(env, name).unwrap_or(expr).clone(),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => expr.clone(),
        Expr::Let(ref scope) => {
            let (bindings, body) = scope.clone().unbind(fresh_state);
            let mut env = env.clone();
            for (name, Embed(value)) in bindings.unnest() {
                let value = eval(&env, &value, fresh_state);
                env = extend(env, name, value);
            }
            eval(&env, &body, fresh_state)
        },
        Expr::App(ref fun, ref arg) => match *eval(env, fun, fresh_state) {
            Expr::Lam(ref scope) => {
                let (name, body) = scope.clone().unbind(fresh_state);
                let env = extend(env.clone(), name, eval(env, arg, fresh_state));
                eval(&env, &body, fresh_state)
            },
            _ => expr.clone(),
        },
    }
}

#[test]
fn test_eval() {
    let mut fresh_state = FreshState::new();

    // expr = (\x -> x) y
    let expr = Rc::new(Expr::App(
        Rc::new(Expr::Lam(Scope::new(
            FreeVar::user("x"),
            Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
        ))),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    ));

    assert_term_eq!(
        eval(&Rc::new(Env::Empty), &expr, &mut fresh_state),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    );
}

#[test]
fn test_eval_let() {
    let mut fresh_state = FreshState::new();

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
        eval(&Rc::new(Env::Empty), &expr, &mut fresh_state),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("y")))),
    );
}

fn main() {}
