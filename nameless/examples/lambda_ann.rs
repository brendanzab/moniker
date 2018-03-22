//! An example of using the `nameless` library to implement the untyped lambda
//! calculus

#[macro_use]
extern crate nameless;

use std::rc::Rc;
use nameless::{Bound, BoundPattern, BoundTerm, Embed, GenId, PatternIndex, Scope, ScopeState, Var};

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

    fn close_term<P: BoundPattern<Free = Name>>(&mut self, _: ScopeState, _: &P) {}
    fn open_term<P: BoundPattern<Free = Name>>(&mut self, _: ScopeState, _: &P) {}
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

    fn close_pattern<P: BoundPattern<Free = Name>>(&mut self, _: ScopeState, _: &P) {}

    fn open_pattern<P: BoundPattern<Free = Name>>(&mut self, _: ScopeState, _: &P) {}

    fn on_free(&self, state: ScopeState, name: &Name) -> Option<Bound> {
        match name == self {
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
pub enum Context {
    Empty,
    Extend(Rc<Context>, Name, Rc<Type>),
}

fn extend(context: Rc<Context>, name: Name, expr: Rc<Type>) -> Rc<Context> {
    Rc::new(Context::Extend(context, name, expr))
}

fn lookup<'a>(mut context: &'a Rc<Context>, name: &Name) -> Option<&'a Rc<Type>> {
    while let Context::Extend(ref next_context, ref curr_name, ref expr) = **context {
        if Name::term_eq(curr_name, name) {
            return Some(expr);
        } else {
            context = next_context;
        }
    }
    None
}

#[derive(Debug, Clone, BoundTerm)]
pub enum Type {
    Base,
    Arrow(Rc<Type>, Rc<Type>),
}

#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    Var(Var<Name>),
    Lam(Scope<(Name, Embed<Rc<Type>>), Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn infer(context: &Rc<Context>, expr: &Rc<Expr>) -> Option<Rc<Type>> {
    match **expr {
        Expr::Var(Var::Free(ref name)) => lookup(context, name).cloned(),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(ref scope) => {
            let ((name, Embed(ann)), body) = nameless::unbind(scope.clone());
            let body_ty = infer(&extend(context.clone(), name, ann.clone()), &body)?;
            Some(Rc::new(Type::Arrow(ann, body_ty)))
        },
        Expr::App(ref fun, ref arg) => match *infer(context, fun)? {
            Type::Arrow(ref t1, ref t2) => {
                let arg_ty = infer(context, arg)?;
                match Type::term_eq(t1, &arg_ty) {
                    true => Some(t2.clone()),
                    false => None,
                }
            },
            _ => None,
        },
    }
}

#[test]
fn test_eval() {
    // expr = (\x -> x) y
    let expr = Rc::new(Expr::Lam(Scope::bind(
        (Name::user("x"), Embed(Rc::new(Type::Base))),
        Rc::new(Expr::Var(Var::Free(Name::user("x")))),
    )));

    // assert_term_eq!(
    //     eval(&Rc::new(Context::Empty), &expr),
    //     Rc::new(Expr::Var(Var::Free(Name::user("y")))),
    // );
}

fn main() {}
