//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus with records, variants, and iso-recursive types.
//!
//! We use [bidirectional type checking](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
//! to get some level of type inference.

extern crate im;
#[macro_use]
extern crate moniker;
#[macro_use]
extern crate proptest;

use im::HashMap;
use moniker::{BoundTerm, Embed, FreeVar, Rec, Scope, Var};
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
    /// Type variables
    Var(Var<String>), // TODO: Separate identifier namespaces? See issue #8
    /// Function types
    Arrow(RcType, RcType),
    /// Record types
    Record(Vec<(String, RcType)>),
    /// Variant types
    Variant(Vec<(String, RcType)>),
    /// Recursive types
    Rec(Scope<Rec<(FreeVar<String>, Embed<RcType>)>, ()>),
}

/// Reference counted types
#[derive(Debug, Clone, BoundTerm)]
pub struct RcType {
    pub inner: Rc<Type>,
}

impl From<Type> for RcType {
    fn from(src: Type) -> RcType {
        RcType {
            inner: Rc::new(src),
        }
    }
}

impl RcType {
    // FIXME: auto-derive this somehow!
    fn subst(&self, name: &FreeVar<String>, replacement: &RcType) -> RcType {
        match *self.inner {
            Type::Int | Type::Float | Type::String => self.clone(),
            Type::Var(Var::Free(ref n)) if name == n => replacement.clone(),
            Type::Var(_) => self.clone(),
            Type::Arrow(ref param, ref body) => RcType::from(Type::Arrow(
                param.subst(name, replacement),
                body.subst(name, replacement),
            )),
            Type::Record(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref elem)| (label.clone(), elem.subst(name, replacement)))
                    .collect();

                RcType::from(Type::Record(fields))
            },
            Type::Variant(ref variants) => {
                let variants = variants
                    .iter()
                    .map(|&(ref label, ref elem)| (label.clone(), elem.subst(name, replacement)))
                    .collect();

                RcType::from(Type::Variant(variants))
            },
            Type::Rec(ref scope) => {
                let (ref n, Embed(ref ann)) = scope.unsafe_pattern.unsafe_pattern;
                RcType::from(Type::Rec(Scope {
                    unsafe_pattern: Rec {
                        unsafe_pattern: (n.clone(), Embed(ann.subst(name, replacement))),
                    },
                    unsafe_body: (),
                }))
            },
        }
    }
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
    Ann(RcExpr, RcType),
    /// Literals
    Literal(Literal),
    /// Variables
    Var(Var<String>), // TODO: Separate identifier namespaces? See issue #8
    /// Lambda expressions, with an optional type annotation for the parameter
    Lam(Scope<(FreeVar<String>, Embed<Option<RcType>>), RcExpr>),
    /// Function application
    App(RcExpr, RcExpr),
    /// Record values
    Record(Vec<(String, RcExpr)>),
    /// Field projection on records
    Proj(RcExpr, String),
    /// Variant introduction
    Tag(String, RcExpr),
    /// Fold a recursive type
    Fold(RcType, RcExpr),
    /// Unfold a recursive type
    Unfold(RcType, RcExpr),
}

/// Reference counted expressions
#[derive(Debug, Clone, BoundTerm)]
pub struct RcExpr {
    pub inner: Rc<Expr>,
}

impl From<Expr> for RcExpr {
    fn from(src: Expr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl RcExpr {
    // FIXME: auto-derive this somehow!
    fn subst(&self, name: &FreeVar<String>, replacement: &RcExpr) -> RcExpr {
        match *self.inner {
            Expr::Ann(ref expr, ref ty) => {
                RcExpr::from(Expr::Ann(expr.subst(name, replacement), ty.clone()))
            },
            Expr::Literal(_) => self.clone(),
            Expr::Var(Var::Free(ref n)) if name == n => replacement.clone(),
            Expr::Var(_) => self.clone(),
            Expr::Lam(ref scope) => RcExpr::from(Expr::Lam(Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: scope.unsafe_body.subst(name, replacement),
            })),
            Expr::App(ref fun, ref arg) => RcExpr::from(Expr::App(
                fun.subst(name, replacement),
                arg.subst(name, replacement),
            )),
            Expr::Record(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref elem)| (label.clone(), elem.subst(name, replacement)))
                    .collect();

                RcExpr::from(Expr::Record(fields))
            },
            Expr::Proj(ref expr, ref label) => {
                RcExpr::from(Expr::Proj(expr.subst(name, replacement), label.clone()))
            },
            Expr::Tag(ref label, ref expr) => {
                RcExpr::from(Expr::Tag(label.clone(), expr.subst(name, replacement)))
            },
            Expr::Fold(ref ty, ref expr) => {
                RcExpr::from(Expr::Fold(ty.clone(), expr.subst(name, replacement)))
            },
            Expr::Unfold(ref ty, ref expr) => {
                RcExpr::from(Expr::Unfold(ty.clone(), expr.subst(name, replacement)))
            },
        }
    }
}

/// A context containing a series of type annotations
type Context = HashMap<FreeVar<String>, RcType>;

/// Evaluate an expression into its normal form
pub fn eval(expr: &RcExpr) -> RcExpr {
    match *expr.inner {
        Expr::Ann(ref expr, _) => eval(expr),
        Expr::Literal(_) | Expr::Var(_) | Expr::Lam(_) => expr.clone(),
        Expr::App(ref fun, ref arg) => match *eval(fun).inner {
            Expr::Lam(ref scope) => {
                let ((name, _), body) = scope.clone().unbind();
                eval(&body.subst(&name, &eval(arg)))
            },
            _ => expr.clone(),
        },
        Expr::Record(ref fields) => {
            let fields = fields
                .iter()
                .map(|&(ref label, ref elem)| (label.clone(), eval(elem)))
                .collect();

            RcExpr::from(Expr::Record(fields))
        },
        Expr::Proj(ref expr, ref label) => {
            let expr = eval(expr);

            if let Expr::Record(ref fields) = *expr.inner {
                if let Some(&(_, ref e)) = fields.iter().find(|&(ref l, _)| l == label) {
                    return e.clone();
                }
            }

            expr
        },
        Expr::Tag(ref label, ref expr) => RcExpr::from(Expr::Tag(label.clone(), eval(expr))),
        Expr::Fold(ref ty, ref expr) => RcExpr::from(Expr::Fold(ty.clone(), eval(expr))),
        Expr::Unfold(ref ty, ref expr) => {
            let expr = eval(expr);
            if let Expr::Fold(_, ref expr) = *expr.inner {
                return expr.clone();
            }
            RcExpr::from(Expr::Unfold(ty.clone(), expr))
        },
    }
}

/// Check that a (potentially ambiguous) expression can conforms to a given
/// expected type
pub fn check(context: &Context, expr: &RcExpr, expected_ty: &RcType) -> Result<(), String> {
    match (&*expr.inner, &*expected_ty.inner) {
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

    if RcType::term_eq(&inferred_ty, expected_ty) {
        Ok(())
    } else {
        Err(format!(
            "type mismatch - found `{:?}` but expected `{:?}`",
            inferred_ty, expected_ty
        ))
    }
}

/// Synthesize the types of unambiguous expressions
pub fn infer(context: &Context, expr: &RcExpr) -> Result<RcType, String> {
    match *expr.inner {
        Expr::Ann(ref expr, ref ty) => {
            check(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Literal(Literal::Int(_)) => Ok(RcType::from(Type::Int)),
        Expr::Literal(Literal::Float(_)) => Ok(RcType::from(Type::Float)),
        Expr::Literal(Literal::String(_)) => Ok(RcType::from(Type::String)),
        Expr::Var(Var::Free(ref name)) => match context.get(name) {
            Some(term) => Ok((*term).clone()),
            None => Err(format!("`{:?}` not found", name)),
        },
        Expr::Var(Var::Bound(ref name, _)) => panic!("encountered a bound variable: {:?}", name),
        Expr::Lam(ref scope) => match scope.clone().unbind() {
            ((name, Embed(Some(ann))), body) => {
                let body_ty = infer(&context.insert(name, ann.clone()), &body)?;
                Ok(RcType::from(Type::Arrow(ann, body_ty)))
            },
            ((name, Embed(None)), _) => {
                Err(format!("type annotation needed for argument `{:?}`", name))
            },
        },
        Expr::App(ref fun, ref arg) => match *infer(context, fun)?.inner {
            Type::Arrow(ref param_ty, ref ret_ty) => {
                let arg_ty = infer(context, arg)?;
                if RcType::term_eq(param_ty, &arg_ty) {
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
        Expr::Record(ref elems) => Ok(RcType::from(Type::Record(
            elems
                .iter()
                .map(|&(ref label, ref expr)| Ok((label.clone(), infer(context, expr)?)))
                .collect::<Result<_, String>>()?,
        ))),
        Expr::Proj(ref expr, ref label) => match *infer(context, expr)?.inner {
            Type::Record(ref elems) => match elems.iter().find(|&(l, _)| l == label) {
                Some(&(_, ref ty)) => Ok(ty.clone()),
                None => Err(format!("field `{}` not found in type", label)),
            },
            _ => Err("record expected".to_string()),
        },
        Expr::Tag(_, _) => Err("type annotations needed".to_string()),
        Expr::Fold(ref ty, ref expr) => match *ty.inner {
            Type::Rec(ref scope) => {
                let (name, Embed(body_ty)) = scope.clone().unbind().0.unrec();
                check(context, expr, &body_ty.subst(&name, ty))?;
                Ok(ty.clone())
            },
            _ => Err(format!("found `{:?}` but expected a recursive type", ty)),
        },
        Expr::Unfold(ref ty, ref expr) => match *ty.inner {
            Type::Rec(ref scope) => {
                let (name, Embed(body_ty)) = scope.clone().unbind().0.unrec();
                check(context, expr, ty)?;
                Ok(body_ty.subst(&name, ty))
            },
            _ => Err(format!("found `{:?}` but expected a recursive type", ty)),
        },
    }
}

/// Proptest strategies
mod arb {
    use proptest::prelude::*;

    use super::*;

    /// `RcType` strategies
    pub mod ty {
        use super::*;

        pub fn any() -> impl Strategy<Value = RcType> {
            prop_oneof![
                Just(RcType::from(Type::Int)),
                Just(RcType::from(Type::Float)),
                Just(RcType::from(Type::String)),
            ].prop_recursive(8, 256, 2, |inner| {
                (inner.clone(), inner.clone())
                    .prop_map(|(param, body)| RcType::from(Type::Arrow(param, body)))
            })
        }
    }

    /// `Literal` strategies
    pub mod literal {
        use super::*;

        pub fn int(val: impl Strategy<Value = i32>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::Int)
        }

        pub fn float(val: impl Strategy<Value = f32>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::Float)
        }

        pub fn string(val: impl Strategy<Value = String>) -> impl Strategy<Value = Literal> {
            val.prop_map(Literal::String)
        }

        pub fn any_int() -> impl Strategy<Value = Literal> {
            int(proptest::arbitrary::any::<i32>())
        }

        pub fn any_float() -> impl Strategy<Value = Literal> {
            float(proptest::arbitrary::any::<f32>())
        }

        pub fn any_string() -> impl Strategy<Value = Literal> {
            string(proptest::arbitrary::any::<String>())
        }

        pub fn any() -> impl Strategy<Value = Literal> {
            prop_oneof![any_int(), any_float(), any_string()]
        }
    }

    /// `RcExpr` strategies
    pub mod expr {
        use super::*;

        pub fn literal(literal: impl Strategy<Value = Literal>) -> impl Strategy<Value = RcExpr> {
            literal.prop_map(|lit| RcExpr::from(Expr::Literal(lit)))
        }

        pub fn any_literal() -> impl Strategy<Value = RcExpr> {
            literal(literal::any())
        }

        pub fn any() -> impl Strategy<Value = RcExpr> {
            prop_oneof![any_literal()]
        }
    }
}

// Property tests

proptest! {
    #[test]
    fn check_int(expr in arb::expr::literal(arb::literal::any_int())) {
        check(&Context::new(), &expr, &RcType::from(Type::Int)).unwrap();
    }

    #[test]
    fn check_float(expr in arb::expr::literal(arb::literal::any_float())) {
        check(&Context::new(), &expr, &RcType::from(Type::Float)).unwrap();
    }

    #[test]
    fn check_string(expr in arb::expr::literal(arb::literal::any_string())) {
        check(&Context::new(), &expr, &RcType::from(Type::String)).unwrap();
    }

    #[test]
    fn infer_int(expr in arb::expr::literal(arb::literal::any_int())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::Int));
    }

    #[test]
    fn infer_float(expr in arb::expr::literal(arb::literal::any_float())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::Float));
    }

    #[test]
    fn infer_string(expr in arb::expr::literal(arb::literal::any_string())) {
        assert_term_eq!(infer(&Context::new(), &expr).unwrap(), RcType::from(Type::String));
    }

    #[test]
    fn infer_app_literal_is_err(
        expr1 in arb::expr::any_literal(),
        expr2 in arb::expr::any_literal(),
    ) {
        infer(&Context::new(), &RcExpr::from(Expr::App(expr1, expr2))).unwrap_err();
    }
}

#[test]
fn test_infer() {
    // expr = (\x : Int -> x)
    let expr = RcExpr::from(Expr::Lam(Scope::new(
        (FreeVar::user("x"), Embed(Some(RcType::from(Type::Int)))),
        RcExpr::from(Expr::Var(Var::Free(FreeVar::user("x")))),
    )));

    assert_term_eq!(
        infer(&Context::new(), &expr).unwrap(),
        RcType::from(Type::Arrow(
            RcType::from(Type::Int),
            RcType::from(Type::Int)
        )),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
