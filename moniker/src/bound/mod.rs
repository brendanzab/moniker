use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use binder::Binder;
use bound_var::{BinderIndex, BoundVar, ScopeOffset};
use free_var::FreeVar;
use var::Var;

#[cfg(feature = "codespan")]
mod codespan;
#[cfg(feature = "im")]
mod im;
#[cfg(feature = "num-bigint")]
mod num_bigint;

#[derive(Debug, Copy, Clone)]
pub struct ScopeState {
    depth: u32,
}

impl ScopeState {
    pub fn new() -> ScopeState {
        ScopeState { depth: 0 }
    }

    pub fn depth(&self) -> ScopeOffset {
        ScopeOffset(self.depth)
    }

    pub fn incr(mut self) -> ScopeState {
        self.depth += 1;
        self
    }
}

/// Terms that may contain variables that can be bound by patterns
pub trait BoundTerm<N> {
    /// Alpha equivalence for terms
    fn term_eq(&self, other: &Self) -> bool;

    /// Close the term using the supplied binders
    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]);

    /// Open the term using the supplied binders
    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]);

    /// Visit each variable in the term, calling the `on_var` callback on each
    /// of them in turn
    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>));

    /// Visit each variable in the term, calling the `on_var` callback on each
    /// of them in turn
    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>));

    /// Returns the set of free variables in this term
    fn free_vars(&self) -> HashSet<FreeVar<N>>
    where
        N: Eq + Hash + Clone,
    {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_) => {},
            Var::Free(ref free_var) => {
                free_vars.insert(free_var.clone());
            },
        });
        free_vars
    }
}

impl<N: PartialEq> BoundTerm<N> for FreeVar<N> {
    fn term_eq(&self, other: &FreeVar<N>) -> bool {
        self == other
    }

    fn close_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn open_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N: PartialEq + Clone> BoundTerm<N> for Var<N> {
    fn term_eq(&self, other: &Var<N>) -> bool {
        self == other
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        // NOTE: Working around NLL
        *self = match *self {
            Var::Bound(_) => return,
            Var::Free(ref free_var) => {
                let binder_index = binders
                    .iter()
                    .enumerate()
                    .find(|&(_, binder)| binder == free_var)
                    .map(|(i, _)| BinderIndex(i as u32));

                match binder_index {
                    Some(binder_index) => Var::Bound(BoundVar {
                        scope: state.depth(),
                        binder: binder_index,
                        pretty_name: free_var.pretty_name.clone(),
                    }),
                    None => return,
                }
            },
        };
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        // NOTE: Working around NLL
        *self = match *self {
            Var::Bound(ref bound_var) if bound_var.scope == state.depth() => {
                match binders.get(bound_var.binder.to_usize()) {
                    Some(&Binder(ref free_var)) => Var::Free(free_var.clone()),
                    None => {
                        // FIXME: better error?
                        panic!(
                            "too few variables in pattern: expected at least {}",
                            bound_var.binder,
                        );
                    },
                }
            },
            Var::Bound(_) | Var::Free(_) => return,
        };
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        on_var(self);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        on_var(self);
    }
}

// Implementations for common types

macro_rules! impl_bound_term_partial_eq {
    ($T:ty) => {
        impl<N> BoundTerm<N> for $T {
            fn term_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn close_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

            fn open_term(&mut self, _: ScopeState, _: &[Binder<N>]) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
        }
    };
}

impl_bound_term_partial_eq!(());
impl_bound_term_partial_eq!(String);
impl_bound_term_partial_eq!(str);
impl_bound_term_partial_eq!(char);
impl_bound_term_partial_eq!(bool);
impl_bound_term_partial_eq!(u8);
impl_bound_term_partial_eq!(u16);
impl_bound_term_partial_eq!(u32);
impl_bound_term_partial_eq!(u64);
impl_bound_term_partial_eq!(usize);
impl_bound_term_partial_eq!(i8);
impl_bound_term_partial_eq!(i16);
impl_bound_term_partial_eq!(i32);
impl_bound_term_partial_eq!(i64);
impl_bound_term_partial_eq!(isize);
impl_bound_term_partial_eq!(f32);
impl_bound_term_partial_eq!(f64);

impl<N, T> BoundTerm<N> for Option<T>
where
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &Option<T>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => T::term_eq(lhs, rhs),
            (&None, &None) => true,
            (_, _) => false,
        }
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        if let Some(ref mut inner) = *self {
            inner.close_term(state, binders);
        }
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        if let Some(ref mut inner) = *self {
            inner.open_term(state, binders);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        if let Some(ref inner) = *self {
            inner.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        if let Some(ref mut inner) = *self {
            inner.visit_mut_vars(on_var);
        }
    }
}

impl<N, T> BoundTerm<N> for Box<T>
where
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &Box<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::close_term(self, state, binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::open_term(self, state, binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        T::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        T::visit_mut_vars(self, on_var);
    }
}

impl<N, T> BoundTerm<N> for Rc<T>
where
    T: BoundTerm<N> + Clone,
{
    fn term_eq(&self, other: &Rc<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::close_term(Rc::make_mut(self), state, binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::open_term(Rc::make_mut(self), state, binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        T::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        T::visit_mut_vars(Rc::make_mut(self), on_var);
    }
}

impl<N, T> BoundTerm<N> for Arc<T>
where
    T: BoundTerm<N> + Clone,
{
    fn term_eq(&self, other: &Arc<T>) -> bool {
        T::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::close_term(Arc::make_mut(self), state, binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        T::open_term(Arc::make_mut(self), state, binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        T::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        T::visit_mut_vars(Arc::make_mut(self), on_var);
    }
}

impl<N, T1, T2> BoundTerm<N> for (T1, T2)
where
    T1: BoundTerm<N>,
    T2: BoundTerm<N>,
{
    fn term_eq(&self, other: &(T1, T2)) -> bool {
        T1::term_eq(&self.0, &other.0) && T2::term_eq(&self.1, &other.1)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.close_term(state, binders);
        self.1.close_term(state, binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.open_term(state, binders);
        self.1.open_term(state, binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
    }
}

impl<N, T1, T2, T3> BoundTerm<N> for (T1, T2, T3)
where
    T1: BoundTerm<N>,
    T2: BoundTerm<N>,
    T3: BoundTerm<N>,
{
    fn term_eq(&self, other: &(T1, T2, T3)) -> bool {
        T1::term_eq(&self.0, &other.0)
            && T2::term_eq(&self.1, &other.1)
            && T3::term_eq(&self.2, &other.2)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.close_term(state, binders);
        self.1.close_term(state, binders);
        self.2.close_term(state, binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.open_term(state, binders);
        self.1.open_term(state, binders);
        self.2.open_term(state, binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        self.0.visit_vars(on_var);
        self.1.visit_vars(on_var);
        self.2.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        self.0.visit_mut_vars(on_var);
        self.1.visit_mut_vars(on_var);
        self.2.visit_mut_vars(on_var);
    }
}

impl<N, T> BoundTerm<N> for [T]
where
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self {
            elem.close_term(state, binders);
        }
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self {
            elem.open_term(state, binders);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        for elem in self {
            elem.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        for elem in self {
            elem.visit_mut_vars(on_var);
        }
    }
}

impl<N, T> BoundTerm<N> for Vec<T>
where
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        <[T]>::close_term(self, state, binders)
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        <[T]>::open_term(self, state, binders)
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        <[T]>::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        <[T]>::visit_mut_vars(self, on_var);
    }
}

/// Patterns that bind variables in terms
pub trait BoundPattern<N> {
    /// Alpha equivalence for patterns
    fn pattern_eq(&self, other: &Self) -> bool;

    /// Close the terms in the pattern using the supplied binders
    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]);

    /// Open the terms in the pattern using the supplied binders
    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]);

    /// Visit each of the binders in the term, calling the `on_binder` callback
    /// on each of them in turn
    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>));

    /// Visit each of the binders in the term, calling the `on_binder` callback
    /// on each of them in turn
    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>));

    /// Returns the binders in this pattern
    fn binders(&self) -> Vec<Binder<N>>
    where
        N: Clone,
    {
        let mut binders = Vec::new();
        self.visit_binders(&mut |binder| {
            binders.push(binder.clone());
        });
        binders
    }
}

impl<N> BoundPattern<N> for Binder<N>
where
    N: Clone + Eq + Hash,
{
    fn pattern_eq(&self, _: &Binder<N>) -> bool {
        true
    }

    fn close_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn open_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        on_binder(self)
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        on_binder(self)
    }
}

// Implementations for common types

macro_rules! impl_bound_pattern_partial_eq {
    ($T:ty) => {
        impl<N> BoundPattern<N> for $T {
            fn pattern_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn close_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

            fn open_pattern(&mut self, _: ScopeState, _: &[Binder<N>]) {}

            fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

            fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
        }
    };
}

impl_bound_pattern_partial_eq!(());
impl_bound_pattern_partial_eq!(String);
impl_bound_pattern_partial_eq!(str);
impl_bound_pattern_partial_eq!(char);
impl_bound_pattern_partial_eq!(bool);
impl_bound_pattern_partial_eq!(u8);
impl_bound_pattern_partial_eq!(u16);
impl_bound_pattern_partial_eq!(u32);
impl_bound_pattern_partial_eq!(u64);
impl_bound_pattern_partial_eq!(usize);
impl_bound_pattern_partial_eq!(i8);
impl_bound_pattern_partial_eq!(i16);
impl_bound_pattern_partial_eq!(i32);
impl_bound_pattern_partial_eq!(i64);
impl_bound_pattern_partial_eq!(isize);
impl_bound_pattern_partial_eq!(f32);
impl_bound_pattern_partial_eq!(f64);

impl<N, P> BoundPattern<N> for Option<P>
where
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Option<P>) -> bool {
        match (self, other) {
            (&Some(ref lhs), &Some(ref rhs)) => P::pattern_eq(lhs, rhs),
            (&None, &None) => true,
            (_, _) => false,
        }
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        if let Some(ref mut inner) = *self {
            inner.close_pattern(state, binders);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        if let Some(ref mut inner) = *self {
            inner.open_pattern(state, binders);
        }
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        if let Some(ref inner) = *self {
            inner.visit_binders(on_binder);
        }
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        if let Some(ref mut inner) = *self {
            inner.visit_mut_binders(on_binder);
        }
    }
}

impl<N, P1, P2> BoundPattern<N> for (P1, P2)
where
    P1: BoundPattern<N>,
    P2: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &(P1, P2)) -> bool {
        P1::pattern_eq(&self.0, &other.0) && P2::pattern_eq(&self.1, &other.1)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.close_pattern(state, binders);
        self.1.close_pattern(state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.open_pattern(state, binders);
        self.1.open_pattern(state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        self.0.visit_binders(on_binder);
        self.1.visit_binders(on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        self.0.visit_mut_binders(on_binder);
        self.1.visit_mut_binders(on_binder);
    }
}

impl<N, P1, P2, P3> BoundPattern<N> for (P1, P2, P3)
where
    P1: BoundPattern<N>,
    P2: BoundPattern<N>,
    P3: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &(P1, P2, P3)) -> bool {
        P1::pattern_eq(&self.0, &other.0)
            && P2::pattern_eq(&self.1, &other.1)
            && P3::pattern_eq(&self.2, &other.2)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.close_pattern(state, binders);
        self.1.close_pattern(state, binders);
        self.2.close_pattern(state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.0.open_pattern(state, binders);
        self.1.open_pattern(state, binders);
        self.2.open_pattern(state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        self.0.visit_binders(on_binder);
        self.1.visit_binders(on_binder);
        self.2.visit_binders(on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        self.0.visit_mut_binders(on_binder);
        self.1.visit_mut_binders(on_binder);
        self.2.visit_mut_binders(on_binder);
    }
}

impl<N, P> BoundPattern<N> for Box<P>
where
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Box<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::close_pattern(self, state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::open_pattern(self, state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        P::visit_binders(self, on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        P::visit_mut_binders(self, on_binder);
    }
}

impl<N, P> BoundPattern<N> for Rc<P>
where
    P: BoundPattern<N> + Clone,
{
    fn pattern_eq(&self, other: &Rc<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::close_pattern(Rc::make_mut(self), state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::open_pattern(Rc::make_mut(self), state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        P::visit_binders(self, on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        P::visit_mut_binders(Rc::make_mut(self), on_binder);
    }
}

impl<N, P> BoundPattern<N> for Arc<P>
where
    P: BoundPattern<N> + Clone,
{
    fn pattern_eq(&self, other: &Arc<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::close_pattern(Arc::make_mut(self), state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        P::open_pattern(Arc::make_mut(self), state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        P::visit_binders(self, on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        P::visit_mut_binders(Arc::make_mut(self), on_binder);
    }
}

impl<N, P> BoundPattern<N> for [P]
where
    N: Clone,
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &[P]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| P::pattern_eq(lhs, rhs))
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self {
            elem.close_pattern(state, binders);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self {
            elem.open_pattern(state, binders);
        }
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        for elem in self {
            elem.visit_binders(on_binder);
        }
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        for elem in self {
            elem.visit_mut_binders(on_binder);
        }
    }
}

impl<N, P> BoundPattern<N> for Vec<P>
where
    N: Clone,
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Vec<P>) -> bool {
        <[P]>::pattern_eq(self, other)
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        <[P]>::close_pattern(self, state, binders);
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        <[P]>::open_pattern(self, state, binders);
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        <[P]>::visit_binders(self, on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        <[P]>::visit_mut_binders(self, on_binder);
    }
}
