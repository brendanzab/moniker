//! An example of using the `nameless` library to implement the simply typed
//! lambda calculus
//!
//! We use bidirectional type checking to get some level of type inference

#[macro_use]
extern crate nameless;

use nameless::{BoundTerm, Embed, FreeVar, Scope, Var};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Context {
    Empty,
    Extend(Rc<Context>, FreeVar, Rc<Type>),
}

fn extend(context: Rc<Context>, name: FreeVar, expr: Rc<Type>) -> Rc<Context> {
    Rc::new(Context::Extend(context, name, expr))
}

fn lookup<'a>(mut context: &'a Rc<Context>, name: &FreeVar) -> Option<&'a Rc<Type>> {
    while let Context::Extend(ref next_context, ref curr_name, ref expr) = **context {
        if FreeVar::term_eq(curr_name, name) {
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
    Var(Var),
    Lam(Scope<(FreeVar, Embed<Option<Rc<Type>>>), Rc<Expr>>),
    App(Rc<Expr>, Rc<Expr>),
}

pub fn check(context: &Rc<Context>, expr: &Rc<Expr>, expected_ty: &Rc<Type>) -> Result<(), String> {
    match (&**expr, &**expected_ty) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            if let ((name, Embed(None)), body) = scope.clone().unbind() {
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
        Expr::Lam(ref scope) => match scope.clone().unbind() {
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
    let expr = Rc::new(Expr::Lam(Scope::new(
        (FreeVar::user("x"), Embed(Some(Rc::new(Type::Base)))),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
    )));

    assert_term_eq!(
        infer(&Rc::new(Context::Empty), &expr).unwrap(),
        Rc::new(Type::Arrow(Rc::new(Type::Base), Rc::new(Type::Base))),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
