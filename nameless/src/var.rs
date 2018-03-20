use std::fmt;

use {AlphaEq, Debruijn, FreeName, Named, Pattern, Term};

/// A variable that can either be free or bound
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Var<F, B> {
    /// A free variable
    Free(F),
    /// A variable that is bound by a lambda or pi binder
    Bound(Named<F, B>),
}

impl<F: AlphaEq, B: AlphaEq> AlphaEq for Var<F, B> {
    fn alpha_eq(&self, other: &Var<F, B>) -> bool {
        match (self, other) {
            (&Var::Free(ref lhs), &Var::Free(ref rhs)) => F::alpha_eq(lhs, rhs),
            (&Var::Bound(ref lhs), &Var::Bound(ref rhs)) => Named::alpha_eq(lhs, rhs),
            (_, _) => false,
        }
    }
}

impl<F: FreeName, B> Term for Var<F, B> {
    type FreeName = F;
    type BoundName = B;

    fn close_at<P1>(&mut self, index: Debruijn, pattern: &P1)
    where
        P1: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        *self = match *self {
            Var::Bound(_) => return,
            Var::Free(ref name) => match pattern.on_free(index, name) {
                Some(index) => Var::Bound(Named::new(name.clone(), index)),
                None => return,
            },
        };
    }

    fn open_at<P1>(&mut self, index: Debruijn, pattern: &P1)
    where
        P1: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        *self = match *self {
            Var::Free(_) => return,
            Var::Bound(Named { ref inner, .. }) => match pattern.on_bound(index, inner) {
                Some(name) => Var::Free(name),
                None => return,
            },
        };
    }
}

impl<F: fmt::Display, B: fmt::Display> fmt::Display for Var<F, B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Bound(ref bound) if f.alternate() => write!(f, "{}{}", bound.name, bound.inner),
            Var::Bound(Named { ref name, .. }) | Var::Free(ref name) => write!(f, "{}", name),
        }
    }
}
