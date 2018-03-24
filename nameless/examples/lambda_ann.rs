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
    Ann(Rc<Expr>, Rc<Type>),
    Var(Var<Name>),
    Lam(Scope<(Name, Embed<Option<Rc<Type>>>), Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn check(context: &Rc<Context>, expr: &Rc<Expr>, expected_ty: &Rc<Type>) -> Result<(), String> {
    match (&**expr, &**expected_ty) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            if let ((name, Embed(None)), body) = nameless::unbind(scope.clone()) {
                let inner_context = extend(context.clone(), name, param_ty.clone());
                check(&inner_context, &body, ret_ty)?;
                return Ok(());
            }
        },
        (_, _) => {},
    }

    let inferred_ty = infer(&context, expr)?;

    if Type::term_eq(&inferred_ty, expected_ty) {
        Ok(())
    } else {
        Err(format!(
            "type mismatch - found `{:?}` but expected `{:?}`",
            inferred_ty, expected_ty
        ))
    }
}

pub fn infer(context: &Rc<Context>, expr: &Rc<Expr>) -> Result<Rc<Type>, String> {
    match **expr {
        Expr::Ann(ref expr, ref ty) => {
            check(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Var(Var::Free(ref name)) => lookup(context, name)
            .cloned()
            .ok_or(format!("`{:?}` not found", name)),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(ref scope) => match nameless::unbind(scope.clone()) {
            ((name, Embed(Some(ann))), body) => {
                let body_ty = infer(&extend(context.clone(), name, ann.clone()), &body)?;
                Ok(Rc::new(Type::Arrow(ann, body_ty)))
            },
            ((name, Embed(None)), _) => {
                Err(format!("type annotation needed for argument `{:?}`", name))
            },
        },
        Expr::App(ref fun, ref arg) => match *infer(context, fun)? {
            Type::Arrow(ref param_ty, ref ret_ty) => {
                let arg_ty = infer(context, arg)?;
                if Type::term_eq(param_ty, &arg_ty) {
                    Ok(ret_ty.clone())
                } else {
                    Err(format!(
                        "argument type mismatch - found `{:?}` but expected `{:?}`",
                        arg_ty, param_ty,
                    ))
                }
            },
            _ => Err(format!("`{:?}` is not a function", fun)),
        },
    }
}

#[test]
fn test_infer() {
    // expr = (\x -> x)
    let expr = Rc::new(Expr::Lam(Scope::bind(
        (Name::user("x"), Embed(Some(Rc::new(Type::Base)))),
        Rc::new(Expr::Var(Var::Free(Name::user("x")))),
    )));

    assert_term_eq!(
        infer(&Rc::new(Context::Empty), &expr).unwrap(),
        Rc::new(Type::Arrow(Rc::new(Type::Base), Rc::new(Type::Base))),
    );
}

fn main() {}
