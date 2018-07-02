//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus
//!
//! We use bidirectional type checking to get some level of type inference

extern crate im;
#[macro_use]
extern crate moniker;

use im::HashMap;
use moniker::{BoundTerm, Embed, FreeVar, Scope, Var};
use std::rc::Rc;

type Context = HashMap<FreeVar, Rc<Type>>;

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

// FIXME: auto-derive this somehow!
fn subst(expr: &Rc<Expr>, subst_name: &FreeVar, subst_expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Ann(ref expr, ref ty) => {
            Rc::new(Expr::Ann(subst(expr, subst_name, subst_expr), ty.clone()))
        },
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
    }
}

pub fn eval(expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Ann(ref expr, _) => eval(expr),
        Expr::Var(Var::Free(_)) => expr.clone(),
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun) {
            Expr::Lam(ref scope) => {
                let ((name, _), body) = scope.clone().unbind();
                eval(&subst(&body, &name, &eval(arg)))
            },
            _ => expr.clone(),
        },
    }
}

pub fn check(context: &Context, expr: &Rc<Expr>, expected_ty: &Rc<Type>) -> Result<(), String> {
    match (&**expr, &**expected_ty) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            if let ((name, Embed(None)), body) = scope.clone().unbind() {
                check(&context.insert(name, param_ty.clone()), &body, ret_ty)?;
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

pub fn infer(context: &Context, expr: &Rc<Expr>) -> Result<Rc<Type>, String> {
    match **expr {
        Expr::Ann(ref expr, ref ty) => {
            check(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Var(Var::Free(ref name)) => match context.get(name) {
            Some(term) => Ok((*term).clone()),
            None => Err(format!("`{:?}` not found", name)),
        },
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(ref scope) => match scope.clone().unbind() {
            ((name, Embed(Some(ann))), body) => {
                let body_ty = infer(&context.insert(name, ann.clone()), &body)?;
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
        infer(&Context::new(), &expr).unwrap(),
        Rc::new(Type::Arrow(Rc::new(Type::Base), Rc::new(Type::Base))),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
