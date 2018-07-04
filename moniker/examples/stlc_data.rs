//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus, with the ability to construct compound datatypes like
//! records and variants
//!
//! We use [bidirectional type checking](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
//! to get some level of type inference.

extern crate im;
#[macro_use]
extern crate moniker;

use im::HashMap;
use moniker::{BoundTerm, Embed, FreeVar, Scope, Var};
use std::rc::Rc;

/// Types
#[derive(Debug, Clone, BoundTerm)]
pub enum Type {
    /// Integers
    Int,
    /// Floating point numbers
    Float,
    /// Strings
    String,
    /// Function types
    Arrow(Rc<Type>, Rc<Type>),
    /// Record types
    Record(Vec<(String, Rc<Type>)>),
    /// Variant types
    Variant(Vec<(String, Rc<Type>)>),
}

/// Literal values
#[derive(Debug, Clone, BoundTerm)]
pub enum Literal {
    /// Integer literals
    Int(i32),
    /// Floating point literals
    Float(f32),
    /// String literals
    String(String),
}

/// Expressions
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Annotated expressions
    Ann(Rc<Expr>, Rc<Type>),
    /// Literals
    Literal(Literal),
    /// Variables
    Var(Var<String>),
    /// Lambda expressions, with an optional type annotation for the parameter
    Lam(Scope<(FreeVar<String>, Embed<Option<Rc<Type>>>), Rc<Expr>>),
    /// Function application
    App(Rc<Expr>, Rc<Expr>),
    /// Record values
    Record(Vec<(String, Rc<Expr>)>),
    /// Field projection on records
    Proj(Rc<Expr>, String),
    /// Variant introduction
    Tag(String, Rc<Expr>),
}

// FIXME: auto-derive this somehow!
fn subst(expr: &Rc<Expr>, subst_name: &FreeVar<String>, subst_expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Ann(ref expr, ref ty) => {
            Rc::new(Expr::Ann(subst(expr, subst_name, subst_expr), ty.clone()))
        },
        Expr::Literal(_) => expr.clone(),
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
        Expr::Record(ref fields) => {
            let fields = fields
                .iter()
                .map(|&(ref label, ref elem)| (label.clone(), subst(elem, subst_name, subst_expr)))
                .collect();

            Rc::new(Expr::Record(fields))
        },
        Expr::Proj(ref expr, ref label) => Rc::new(Expr::Proj(
            subst(expr, subst_name, subst_expr),
            label.clone(),
        )),
        Expr::Tag(ref label, ref expr) => Rc::new(Expr::Tag(
            label.clone(),
            subst(expr, subst_name, subst_expr),
        )),
    }
}

/// Evaluate an expression into its normal form
pub fn eval(expr: &Rc<Expr>) -> Rc<Expr> {
    match **expr {
        Expr::Ann(ref expr, _) => eval(expr),
        Expr::Literal(_) => expr.clone(),
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
        Expr::Record(ref fields) => {
            let fields = fields
                .iter()
                .map(|&(ref label, ref elem)| (label.clone(), eval(elem)))
                .collect();

            Rc::new(Expr::Record(fields))
        },
        Expr::Proj(ref expr, ref label) => {
            let expr = eval(expr);

            if let Expr::Record(ref fields) = *expr {
                if let Some(&(_, ref e)) = fields.iter().find(|&(ref l, _)| l == label) {
                    return e.clone();
                }
            }

            expr
        },
        Expr::Tag(ref label, ref expr) => Rc::new(Expr::Tag(label.clone(), eval(expr))),
    }
}

/// A context containing a series of type annotations
type Context = HashMap<FreeVar<String>, Rc<Type>>;

/// Check that a (potentially ambiguous) expression can conforms to a given
/// expected type
pub fn check(context: &Context, expr: &Rc<Expr>, expected_ty: &Rc<Type>) -> Result<(), String> {
    match (&**expr, &**expected_ty) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            if let ((name, Embed(None)), body) = scope.clone().unbind() {
                check(&context.insert(name, param_ty.clone()), &body, ret_ty)?;
                return Ok(());
            }
        },
        (&Expr::Tag(ref label, ref expr), &Type::Variant(ref variants)) => {
            return match variants.iter().find(|&(l, _)| l == label) {
                None => Err(format!(
                    "variant type did not contain the label `{}`",
                    label
                )),
                Some(&(_, ref ty)) => check(context, expr, ty),
            };
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

/// Synthesize the types of unambiguous expressions
pub fn infer(context: &Context, expr: &Rc<Expr>) -> Result<Rc<Type>, String> {
    match **expr {
        Expr::Ann(ref expr, ref ty) => {
            check(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Literal(Literal::Int(_)) => Ok(Rc::new(Type::Int)),
        Expr::Literal(Literal::Float(_)) => Ok(Rc::new(Type::Float)),
        Expr::Literal(Literal::String(_)) => Ok(Rc::new(Type::String)),
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
        Expr::Record(ref elems) => Ok(Rc::new(Type::Record(
            elems
                .iter()
                .map(|&(ref label, ref expr)| Ok((label.clone(), infer(context, expr)?)))
                .collect::<Result<_, String>>()?,
        ))),
        Expr::Proj(ref expr, ref label) => match *infer(context, expr)? {
            Type::Record(ref elems) => match elems.iter().find(|&(l, _)| l == label) {
                Some(&(_, ref ty)) => Ok(ty.clone()),
                None => Err(format!("field `{}` not found in type", label)),
            },
            _ => Err("record expected".to_string()),
        },
        Expr::Tag(_, _) => Err("type annotations needed".to_string()),
    }
}

#[test]
fn test_infer() {
    // expr = (\x : Int -> x)
    let expr = Rc::new(Expr::Lam(Scope::new(
        (FreeVar::user("x"), Embed(Some(Rc::new(Type::Int)))),
        Rc::new(Expr::Var(Var::Free(FreeVar::user("x")))),
    )));

    assert_term_eq!(
        infer(&Context::new(), &expr).unwrap(),
        Rc::new(Type::Arrow(Rc::new(Type::Int), Rc::new(Type::Int))),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
