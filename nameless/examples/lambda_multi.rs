//! An example of using the `nameless` library to implement the untyped lambda
//! calculus with multibinders

#[macro_use]
extern crate nameless;

use std::rc::Rc;
use nameless::{Bound, BoundPattern, BoundTerm, GenId, PatternIndex, Scope, ScopeState, Var};

/// The name of a free variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    User(String),
    Gen(GenId),
}

impl Name {
    pub fn user<S: Into<String>>(name: S) -> Name {
        Name::User(name.into())
    }
}

impl BoundTerm for Name {
    type Free = Name;

    fn term_eq(&self, other: &Name) -> bool {
        match (self, other) {
            (&Name::User(ref lhs), &Name::User(ref rhs)) => lhs == rhs,
            (&Name::Gen(ref lhs), &Name::Gen(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl BoundPattern for Name {
    type Free = Name;

    fn pattern_eq(&self, _other: &Name) -> bool {
        true
    }

    fn freshen(&mut self) -> Vec<Name> {
        *self = match *self {
            Name::User(_) => Name::Gen(GenId::fresh()),
            Name::Gen(_) => return vec![self.clone()],
        };
        vec![self.clone()]
    }

    fn rename(&mut self, perm: &[Name]) {
        assert_eq!(perm.len(), 1); // FIXME: assert
        *self = perm[0].clone(); // FIXME: double clone
    }

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
        match Name::term_eq(self, name) {
            true => Some(Bound {
                scope: state.depth(),
                pattern: PatternIndex(0),
            }),
            false => None,
        }
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free> {
        match name.scope == state.depth() {
            true => {
                assert_eq!(name.pattern, PatternIndex(0));
                Some(self.clone())
            },
            false => None,
        }
    }
}

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
    Var(Var<Name>),
    Lam(Scope<Vec<Name>, Rc<Expr>>),
    App(Rc<Expr>, Vec<Rc<Expr>>),
}

#[derive(Debug, Clone)]
pub enum EvalError {
    ArgumentCountMismatch { expected: usize, given: usize },
}

pub fn eval(env: &Rc<Env>, expr: &Rc<Expr>) -> Result<Rc<Expr>, EvalError> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => Ok(lookup(env, name).unwrap_or(expr).clone()),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => Ok(expr.clone()),
        Expr::App(ref fun, ref args) => match *eval(env, fun)? {
            Expr::Lam(ref scope) => {
                let (params, body) = nameless::unbind(scope.clone());

                if params.len() != args.len() {
                    Err(EvalError::ArgumentCountMismatch {
                        expected: params.len(),
                        given: args.len(),
                    })
                } else {
                    let mut acc_env = env.clone();
                    for (param_name, arg) in <_>::zip(params.into_iter(), args.iter()) {
                        acc_env = extend(acc_env, param_name, eval(env, arg)?);
                    }
                    eval(&acc_env, &body)
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
        Rc::new(Expr::Lam(Scope::bind(
            vec![Name::user("x"), Name::user("y")],
            Rc::new(Expr::Var(Var::Free(Name::user("y")))),
        ))),
        vec![
            Rc::new(Expr::Var(Var::Free(Name::user("a")))),
            Rc::new(Expr::Var(Var::Free(Name::user("b")))),
        ],
    ));

    assert_term_eq!(
        eval(&Rc::new(Env::Empty), &expr).unwrap(),
        Rc::new(Expr::Var(Var::Free(Name::user("b")))),
    );
}

fn main() {}
