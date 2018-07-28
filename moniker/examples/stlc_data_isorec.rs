//! An example of using the `moniker` library to implement the simply typed
//! lambda calculus with records, variants, literals, pattern matching, and
//! iso-recursive types.
//!
//! We use [bidirectional type checking](http://www.davidchristiansen.dk/tutorials/bidirectional.pdf)
//! to get some level of type inference.
//!
//! To implement pattern matching we referred to:
//!
//! - [The Locally Nameless Representation (Section 7.3)](https://www.chargueraud.org/research/2009/ln/main.pdf)
//! - [Towards a practical programming language based on dependent type theory (Chapter 2)]()

extern crate im;
#[macro_use]
extern crate moniker;

use im::HashMap;
use moniker::{Binder, BoundTerm, Embed, FreeVar, Rec, Scope, Var};
use std::rc::Rc;

/// Types
///
/// ```text
/// t ::= Int                   integer types
///     | Float                 floating point types
///     | String                string types
///     | t -> t                function types
///     | {l₁:t₁, ..., lₙ:tₙ}   record types
///     | <l₁:t₁, ..., lₙ:tₙ>   variant types
///     | rec x => t            recursive type
/// ```
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
    Rec(Scope<Rec<(Binder<String>, Embed<RcType>)>, ()>),
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
    fn subst<N: PartialEq<Var<String>>>(&self, name: &N, replacement: &RcType) -> RcType {
        match *self.inner {
            Type::Var(ref var) if name == var => replacement.clone(),
            Type::Var(_) | Type::Int | Type::Float | Type::String => self.clone(),
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
#[derive(Debug, Clone, PartialEq, BoundTerm, BoundPattern)]
pub enum Literal {
    /// Integer literals
    Int(i32),
    /// Floating point literals
    Float(f32),
    /// String literals
    String(String),
}

/// Patterns
///
/// ```text
/// p ::= _                     wildcard patterns
///     | x                     pattern variables
///     | p : t                 patterns annotated with types
///     | {l₁=p₁, ..., lₙ=pₙ}   record patterns
///     | <l=p>                 tag patterns
/// ```
#[derive(Debug, Clone, BoundPattern)]
pub enum Pattern {
    /// Wildcard patterns
    Wildcard,
    /// Patterns annotated with types
    Ann(RcPattern, Embed<RcType>),
    /// Literal patterns
    Literal(Literal),
    /// Patterns that bind variables
    Binder(Binder<String>),
    /// Record patterns
    Record(Vec<(String, RcPattern)>),
    /// Tag pattern
    Tag(String, RcPattern),
}

/// Reference counted patterns
#[derive(Debug, Clone, BoundPattern)]
pub struct RcPattern {
    pub inner: Rc<Pattern>,
}

impl From<Pattern> for RcPattern {
    fn from(src: Pattern) -> RcPattern {
        RcPattern {
            inner: Rc::new(src),
        }
    }
}

/// Expressions
///
/// ```text
/// e ::= x                                 variables
///     | e : t                             expressions annotated with types
///     | \p => e                           anonymous functions
///     | e₁ e₂                             function application
///     | let p₁=e₁, ..., pₙ=eₙ in e        mutually recursive let bindings
///     | {l₁=e₁, ..., lₙ=eₙ}               record expressions
///     | e.l                               record projections
///     | <l=e>                             tag expressions
///     | case e of p₁=>e₁, ..., pₙ=>eₙ     case expressions
///     | fold t => e                       fold expressions
///     | unfold t => e                     unfold expressions
/// ```
#[derive(Debug, Clone, BoundTerm)]
pub enum Expr {
    /// Annotated expressions
    Ann(RcExpr, RcType),
    /// Literals
    Literal(Literal),
    /// Variables
    Var(Var<String>), // TODO: Separate identifier namespaces? See issue #8
    /// Lambda expressions, with an optional type annotation for the parameter
    Lam(Scope<RcPattern, RcExpr>),
    /// Function application
    App(RcExpr, RcExpr),
    /// Record values
    Record(Vec<(String, RcExpr)>),
    /// Field projection on records
    Proj(RcExpr, String),
    /// Variant introduction
    Tag(String, RcExpr),
    /// Case expressions
    Case(RcExpr, Vec<Scope<RcPattern, RcExpr>>),
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
    fn substs<N: PartialEq<Var<String>>>(&self, mappings: &[(N, RcExpr)]) -> RcExpr {
        match *self.inner {
            Expr::Ann(ref expr, ref ty) => {
                RcExpr::from(Expr::Ann(expr.substs(mappings), ty.clone()))
            },
            Expr::Var(ref var) => match mappings.iter().find(|&(name, _)| name == var) {
                Some((_, ref replacement)) => replacement.clone(),
                None => self.clone(),
            },
            Expr::Literal(_) => self.clone(),
            Expr::Lam(ref scope) => RcExpr::from(Expr::Lam(Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: scope.unsafe_body.substs(mappings),
            })),
            Expr::App(ref fun, ref arg) => {
                RcExpr::from(Expr::App(fun.substs(mappings), arg.substs(mappings)))
            },
            Expr::Record(ref fields) => {
                let fields = fields
                    .iter()
                    .map(|&(ref label, ref elem)| (label.clone(), elem.substs(mappings)))
                    .collect();

                RcExpr::from(Expr::Record(fields))
            },
            Expr::Proj(ref expr, ref label) => {
                RcExpr::from(Expr::Proj(expr.substs(mappings), label.clone()))
            },
            Expr::Tag(ref label, ref expr) => {
                RcExpr::from(Expr::Tag(label.clone(), expr.substs(mappings)))
            },
            Expr::Case(ref expr, ref clauses) => RcExpr::from(Expr::Case(
                expr.substs(mappings),
                clauses
                    .iter()
                    .map(|scope| Scope {
                        unsafe_pattern: scope.unsafe_pattern.clone(), // subst?
                        unsafe_body: scope.unsafe_body.substs(mappings),
                    })
                    .collect(),
            )),
            Expr::Fold(ref ty, ref expr) => {
                RcExpr::from(Expr::Fold(ty.clone(), expr.substs(mappings)))
            },
            Expr::Unfold(ref ty, ref expr) => {
                RcExpr::from(Expr::Unfold(ty.clone(), expr.substs(mappings)))
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
                let (pattern, body) = scope.clone().unbind();
                match match_expr(&pattern, &eval(arg)) {
                    None => expr.clone(), // stuck
                    Some(mappings) => eval(&body.substs(&mappings)),
                }
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
        Expr::Case(ref arg, ref clauses) => {
            let arg = eval(arg);
            for clause in clauses {
                let (pattern, body) = clause.clone().unbind();
                if let Some(mappings) = match_expr(&pattern, &arg) {
                    return eval(&body.substs(&mappings));
                }
            }
            RcExpr::from(Expr::Case(arg, clauses.clone())) // stuck
        },
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

/// If the pattern matches the expression, this function returns the
/// substitutions needed to apply the pattern to some body expression
///
/// We assume that the given expression has been evaluated first!
pub fn match_expr(pattern: &RcPattern, expr: &RcExpr) -> Option<Vec<(FreeVar<String>, RcExpr)>> {
    match (&*pattern.inner, &*expr.inner) {
        (&Pattern::Ann(ref pattern, _), _) => match_expr(pattern, expr),
        (&Pattern::Literal(ref pattern_lit), &Expr::Literal(ref expr_lit))
            if pattern_lit == expr_lit =>
        {
            Some(vec![])
        },
        (&Pattern::Binder(Binder(ref free_var)), _) => Some(vec![(free_var.clone(), expr.clone())]),
        (&Pattern::Record(ref pattern_fields), &Expr::Record(ref expr_fields))
            if pattern_fields.len() == expr_fields.len() =>
        {
            // FIXME: allow out-of-order fields in records
            let mut mappings = Vec::new();
            for (pattern_field, expr_field) in <_>::zip(pattern_fields.iter(), expr_fields.iter()) {
                if pattern_field.0 != expr_field.0 {
                    return None;
                } else {
                    mappings.extend(match_expr(&pattern_field.1, &expr_field.1)?);
                }
            }
            Some(mappings)
        }
        (&Pattern::Tag(ref pattern_label, ref pattern), &Expr::Tag(ref expr_label, ref expr))
            if pattern_label == expr_label =>
        {
            match_expr(pattern, expr)
        },
        (_, _) => None,
    }
}

/// Check that a (potentially ambiguous) expression can conforms to a given
/// expected type
pub fn check_expr(context: &Context, expr: &RcExpr, expected_ty: &RcType) -> Result<(), String> {
    match (&*expr.inner, &*expected_ty.inner) {
        (&Expr::Lam(ref scope), &Type::Arrow(ref param_ty, ref ret_ty)) => {
            let (pattern, body) = scope.clone().unbind();
            let bindings = check_pattern(context, &pattern, param_ty)?;
            return check_expr(&(context + &bindings), &body, ret_ty);
        },
        (&Expr::Tag(ref label, ref expr), &Type::Variant(ref variants)) => {
            return match variants.iter().find(|&(l, _)| l == label) {
                None => Err(format!(
                    "variant type did not contain the label `{}`",
                    label
                )),
                Some(&(_, ref ty)) => check_expr(context, expr, ty),
            };
        },
        (&Expr::Case(ref expr, ref clauses), _) => {
            let expr_ty = infer_expr(context, expr)?;
            for clause in clauses {
                let (pattern, body) = clause.clone().unbind();
                let bindings = check_pattern(context, &pattern, &expr_ty)?;
                check_expr(&(context + &bindings), &body, expected_ty)?;
            }
            return Ok(());
        },
        (_, _) => {},
    }

    let inferred_ty = infer_expr(&context, expr)?;

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
pub fn infer_expr(context: &Context, expr: &RcExpr) -> Result<RcType, String> {
    match *expr.inner {
        Expr::Ann(ref expr, ref ty) => {
            check_expr(context, expr, ty)?;
            Ok(ty.clone())
        },
        Expr::Literal(Literal::Int(_)) => Ok(RcType::from(Type::Int)),
        Expr::Literal(Literal::Float(_)) => Ok(RcType::from(Type::Float)),
        Expr::Literal(Literal::String(_)) => Ok(RcType::from(Type::String)),
        Expr::Var(Var::Free(ref free_var)) => match context.get(free_var) {
            Some(term) => Ok((*term).clone()),
            None => Err(format!("`{:?}` not found in `{:?}`", free_var, context)),
        },
        Expr::Var(Var::Bound(_, _, _)) => panic!("encountered a bound variable"),
        Expr::Lam(ref scope) => {
            let (pattern, body) = scope.clone().unbind();
            let (ann, bindings) = infer_pattern(context, &pattern)?;
            let body_ty = infer_expr(&(context + &bindings), &body)?;
            Ok(RcType::from(Type::Arrow(ann, body_ty)))
        },
        Expr::App(ref fun, ref arg) => match *infer_expr(context, fun)?.inner {
            Type::Arrow(ref param_ty, ref ret_ty) => {
                let arg_ty = infer_expr(context, arg)?;
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
        Expr::Record(ref fields) => Ok(RcType::from(Type::Record(
            fields
                .iter()
                .map(|&(ref label, ref expr)| Ok((label.clone(), infer_expr(context, expr)?)))
                .collect::<Result<_, String>>()?,
        ))),
        Expr::Proj(ref expr, ref label) => match *infer_expr(context, expr)?.inner {
            Type::Record(ref fields) => match fields.iter().find(|&(l, _)| l == label) {
                Some(&(_, ref ty)) => Ok(ty.clone()),
                None => Err(format!("field `{}` not found in type", label)),
            },
            _ => Err("record expected".to_string()),
        },
        Expr::Tag(_, _) => Err("type annotations needed".to_string()),
        Expr::Case(_, _) => Err("type annotations needed".to_string()),
        Expr::Fold(ref ty, ref expr) => match *ty.inner {
            Type::Rec(ref scope) => {
                let (binder, Embed(body_ty)) = scope.clone().unbind().0.unrec();
                check_expr(context, expr, &body_ty.subst(&binder, ty))?;
                Ok(ty.clone())
            },
            _ => Err(format!("found `{:?}` but expected a recursive type", ty)),
        },
        Expr::Unfold(ref ty, ref expr) => match *ty.inner {
            Type::Rec(ref scope) => {
                let (binder, Embed(body_ty)) = scope.clone().unbind().0.unrec();
                check_expr(context, expr, ty)?;
                Ok(body_ty.subst(&binder, ty))
            },
            _ => Err(format!("found `{:?}` but expected a recursive type", ty)),
        },
    }
}

// TODO: Check pattern coverage/exhaustiveness (ie. if a series of patterns
// cover all cases)

/// Synthesize the types of unambiguous patterns
///
/// This function also returns a telescope that can be used to extend the typing
/// context with additional bindings that the pattern introduces.
pub fn check_pattern(
    context: &Context,
    pattern: &RcPattern,
    expected_ty: &RcType,
) -> Result<Context, String> {
    match (&*pattern.inner, &*expected_ty.inner) {
        (&Pattern::Binder(Binder(ref free_var)), _) => {
            return Ok(Context::singleton(free_var.clone(), expected_ty.clone()));
        },
        (&Pattern::Tag(ref label, ref pattern), &Type::Variant(ref variants)) => {
            return match variants.iter().find(|&(l, _)| l == label) {
                None => Err(format!(
                    "variant type did not contain the label `{}`",
                    label
                )),
                Some(&(_, ref ty)) => check_pattern(context, pattern, ty),
            };
        },
        (_, _) => {},
    }

    let (inferred_ty, telescope) = infer_pattern(&context, pattern)?;

    // FIXME: allow out-of-order fields in records
    if RcType::term_eq(&inferred_ty, expected_ty) {
        Ok(telescope)
    } else {
        Err(format!(
            "type mismatch - found `{:?}` but expected `{:?}`",
            inferred_ty, expected_ty
        ))
    }
}

/// Check that a (potentially ambiguous) pattern conforms to a given type
///
/// This function also returns a telescope that can be used to extend the typing
/// context with additional bindings that the pattern introduces.
pub fn infer_pattern(context: &Context, expr: &RcPattern) -> Result<(RcType, Context), String> {
    match *expr.inner {
        Pattern::Wildcard => Err("type annotations needed".to_string()),
        Pattern::Ann(ref pattern, Embed(ref ty)) => {
            let telescope = check_pattern(context, pattern, ty)?;
            Ok((ty.clone(), telescope))
        },
        Pattern::Literal(Literal::Int(_)) => Ok((RcType::from(Type::Int), Context::new())),
        Pattern::Literal(Literal::Float(_)) => Ok((RcType::from(Type::Float), Context::new())),
        Pattern::Literal(Literal::String(_)) => Ok((RcType::from(Type::String), Context::new())),
        Pattern::Binder(_) => Err("type annotations needed".to_string()),
        Pattern::Record(ref fields) => {
            let mut telescope = Context::new();

            let fields = fields
                .iter()
                .map(|&(ref label, ref pattern)| {
                    let (pattern_ty, pattern_telescope) = infer_pattern(context, pattern)?;
                    telescope.extend(pattern_telescope);
                    Ok((label.clone(), pattern_ty))
                })
                .collect::<Result<_, String>>()?;

            Ok((RcType::from(Type::Record(fields)), telescope))
        },
        Pattern::Tag(_, _) => Err("type annotations needed".to_string()),
    }
}

#[test]
fn test_infer() {
    // expr = (\x : Int -> x)
    let expr = RcExpr::from(Expr::Lam(Scope::new(
        RcPattern::from(Pattern::Ann(
            RcPattern::from(Pattern::Binder(Binder::user("x"))),
            Embed(RcType::from(Type::Int)),
        )),
        RcExpr::from(Expr::Var(Var::user("x"))),
    )));

    assert_term_eq!(
        infer_expr(&Context::new(), &expr).unwrap(),
        RcType::from(Type::Arrow(
            RcType::from(Type::Int),
            RcType::from(Type::Int)
        )),
    );
}

// TODO: Use property testing for this!
// http://janmidtgaard.dk/papers/Midtgaard-al%3AICFP17-full.pdf

fn main() {}
