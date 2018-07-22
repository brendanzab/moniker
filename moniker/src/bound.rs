#[cfg(feature = "codespan")]
use codespan::{
    ByteIndex, ByteOffset, ColumnIndex, ColumnNumber, ColumnOffset, LineIndex, LineNumber,
    LineOffset, Span,
};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use binder::{Binder, BinderIndex, BinderOffset};
use free_var::FreeVar;
use var::{ScopeOffset, Var};

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
    /// Alpha equivalence in a term context
    fn term_eq(&self, other: &Self) -> bool;

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>);

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>);

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>));

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>));

    /// Returns the set of free variables in this term
    fn free_vars(&self) -> HashSet<FreeVar<N>>
    where
        N: Eq + Hash + Clone,
    {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_, _, _) => {},
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

    fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

impl<N: PartialEq + Clone> BoundTerm<N> for Var<N> {
    fn term_eq(&self, other: &Var<N>) -> bool {
        self == other
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        // NOTE: Working around NLL
        *self = match *self {
            Var::Bound(_, _, _) => return,
            Var::Free(ref free_var) => match pattern.find_binder_index(free_var) {
                Ok(binder_index) => {
                    Var::Bound(state.depth(), binder_index, free_var.ident().cloned())
                },
                Err(_) => return,
            },
        };
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        // NOTE: Working around NLL
        *self = match *self {
            Var::Bound(scope, binder_index, _) if scope == state.depth() => {
                match pattern.find_binder_at_offset(binder_index.0) {
                    Ok(Binder(binder)) => Var::Free(binder),
                    Err(_) => {
                        // FIXME: better error?
                        panic!(
                            "too few variables in pattern: expected at least {}",
                            binder_index,
                        );
                    },
                }
            },
            Var::Bound(_, _, _) | Var::Free(_) => return,
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

            fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

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

#[cfg(feature = "codespan")]
macro_rules! impl_bound_term_ignore {
    ($T:ty) => {
        impl<N> BoundTerm<N> for $T {
            fn term_eq(&self, _: &$T) -> bool {
                true
            }

            fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
        }
    };
}

#[cfg(feature = "codespan")]
impl_bound_term_ignore!(ByteIndex);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(ByteOffset);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(ColumnIndex);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(ColumnNumber);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(ColumnOffset);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(LineIndex);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(LineNumber);
#[cfg(feature = "codespan")]
impl_bound_term_ignore!(LineOffset);

#[cfg(feature = "codespan")]
impl<N, T> BoundTerm<N> for Span<T> {
    fn term_eq(&self, _: &Span<T>) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        if let Some(ref mut inner) = *self {
            inner.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        if let Some(ref mut inner) = *self {
            inner.open_term(state, pattern);
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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::close_term(self, state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::open_term(self, state, pattern);
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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::close_term(Rc::make_mut(self), state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::open_term(Rc::make_mut(self), state, pattern);
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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::close_term(Arc::make_mut(self), state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        T::open_term(Arc::make_mut(self), state, pattern);
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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
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

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.close_term(state, pattern);
        self.1.close_term(state, pattern);
        self.2.close_term(state, pattern);
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.open_term(state, pattern);
        self.1.open_term(state, pattern);
        self.2.open_term(state, pattern);
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
    T: BoundTerm<N> + Clone,
{
    fn term_eq(&self, other: &[T]) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        for elem in self {
            elem.close_term(state, pattern);
        }
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        for elem in self {
            elem.open_term(state, pattern);
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
    T: BoundTerm<N> + Clone,
{
    fn term_eq(&self, other: &Vec<T>) -> bool {
        <[T]>::term_eq(self, other)
    }

    fn close_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        <[T]>::close_term(self, state, pattern)
    }

    fn open_term(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        <[T]>::open_term(self, state, pattern)
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        <[T]>::visit_vars(self, on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        <[T]>::visit_mut_vars(self, on_var);
    }
}

pub type Permutations<N> = HashMap<Binder<N>, Binder<N>>;

/// Patterns that bind variables in terms
pub trait BoundPattern<N> {
    /// Alpha equivalence in a pattern context
    fn pattern_eq(&self, other: &Self) -> bool;

    fn freshen(&mut self, permutations: &mut Permutations<N>);

    fn swaps(&mut self, permutations: &Permutations<N>);

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>);

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>);

    /// Find the index of the pattern that binds the given free variable
    ///
    /// If we failed to find a pattern variable corresponding to the free
    /// variable we return a pattern variable offset describing how many
    /// pattern variables we passed over.
    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset>;

    /// Find the pattern variable at the given offset
    ///
    /// If we finished traversing over the pattern without finding a pattern
    /// we return the offset that still remains.
    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset>;
}

impl<N> BoundPattern<N> for Binder<N>
where
    N: Clone + Eq + Hash,
{
    fn pattern_eq(&self, _: &Binder<N>) -> bool {
        true
    }

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        let fresh = self.clone().fresh();
        permutations.insert(self.clone(), fresh.clone());
        *self = fresh;
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        *self = permutations
            .get(self)
            .cloned()
            // TODO: better error here?
            .expect("pattern not found in permutation");
    }

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        if self == free_var {
            Ok(BinderIndex(BinderOffset(0)))
        } else {
            Err(BinderOffset(1))
        }
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        if offset == BinderOffset(0) {
            Ok(self.clone())
        } else {
            Err(offset - BinderOffset(1))
        }
    }
}

// Implementations for common types

macro_rules! impl_bound_pattern_partial_eq {
    ($T:ty) => {
        impl<N> BoundPattern<N> for $T {
            fn pattern_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn freshen(&mut self, _: &mut Permutations<N>) {}

            fn swaps(&mut self, _: &Permutations<N>) {}

            fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn find_binder_index(&self, _: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
                Err(BinderOffset(0))
            }

            fn find_binder_at_offset(
                &self,
                offset: BinderOffset,
            ) -> Result<Binder<N>, BinderOffset> {
                Err(offset)
            }
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

#[cfg(feature = "codespan")]
macro_rules! impl_bound_pattern_ignore {
    ($T:ty) => {
        impl<N> BoundPattern<N> for $T {
            fn pattern_eq(&self, _: &$T) -> bool {
                true
            }

            fn freshen(&mut self, _: &mut Permutations<N>) {}

            fn swaps(&mut self, _: &Permutations<N>) {}

            fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

            fn find_binder_index(&self, _: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
                Err(BinderOffset(0))
            }

            fn find_binder_at_offset(
                &self,
                offset: BinderOffset,
            ) -> Result<Binder<N>, BinderOffset> {
                Err(offset)
            }
        }
    };
}

#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(ByteIndex);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(ByteOffset);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(ColumnIndex);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(ColumnNumber);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(ColumnOffset);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(LineIndex);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(LineNumber);
#[cfg(feature = "codespan")]
impl_bound_pattern_ignore!(LineOffset);

#[cfg(feature = "codespan")]
impl<N, T> BoundPattern<N> for Span<T> {
    fn pattern_eq(&self, _: &Span<T>) -> bool {
        true
    }

    fn freshen(&mut self, _: &mut Permutations<N>) {}

    fn swaps(&mut self, _: &Permutations<N>) {}

    fn close_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl BoundPattern<N>) {}

    fn find_binder_index(&self, _: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        Err(BinderOffset(0))
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        Err(offset)
    }
}

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

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        if let Some(ref mut inner) = *self {
            inner.freshen(permutations);
        }
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        if let Some(ref mut inner) = *self {
            inner.swaps(permutations);
        }
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        if let Some(ref mut inner) = *self {
            inner.close_pattern(state, pattern);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        if let Some(ref mut inner) = *self {
            inner.open_pattern(state, pattern);
        }
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        match *self {
            None => Err(BinderOffset(0)),
            Some(ref inner) => inner.find_binder_index(free_var),
        }
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        match *self {
            None => Err(offset),
            Some(ref inner) => inner.find_binder_at_offset(offset),
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

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        self.0.freshen(permutations);
        self.1.freshen(permutations);
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        self.0.swaps(permutations);
        self.1.swaps(permutations);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.close_pattern(state, pattern);
        self.1.close_pattern(state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        self.0.open_pattern(state, pattern);
        self.1.open_pattern(state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        let mut skipped = BinderOffset(0);

        match self.0.find_binder_index(free_var) {
            Ok(binder_index) => return Ok(binder_index + skipped),
            Err(next_skipped) => skipped += next_skipped,
        }
        match self.1.find_binder_index(free_var) {
            Ok(binder_index) => return Ok(binder_index + skipped),
            Err(next_skipped) => skipped += next_skipped,
        }

        Err(skipped)
    }

    fn find_binder_at_offset(&self, mut offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        match self.0.find_binder_at_offset(offset) {
            Ok(binder) => return Ok(binder),
            Err(next_offset) => offset = next_offset,
        }
        match self.1.find_binder_at_offset(offset) {
            Ok(binder) => return Ok(binder),
            Err(next_offset) => offset = next_offset,
        }

        Err(offset)
    }
}

impl<N, P> BoundPattern<N> for Box<P>
where
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Box<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        P::freshen(self, permutations)
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        P::swaps(self, permutations)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::close_pattern(self, state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::open_pattern(self, state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        P::find_binder_index(self, free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        P::find_binder_at_offset(self, offset)
    }
}

impl<N, P> BoundPattern<N> for Rc<P>
where
    P: BoundPattern<N> + Clone,
{
    fn pattern_eq(&self, other: &Rc<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        P::freshen(Rc::make_mut(self), permutations)
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        P::swaps(Rc::make_mut(self), permutations)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::close_pattern(Rc::make_mut(self), state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::open_pattern(Rc::make_mut(self), state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        P::find_binder_index(self, free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        P::find_binder_at_offset(self, offset)
    }
}

impl<N, P> BoundPattern<N> for Arc<P>
where
    P: BoundPattern<N> + Clone,
{
    fn pattern_eq(&self, other: &Arc<P>) -> bool {
        P::pattern_eq(self, other)
    }

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        P::freshen(Arc::make_mut(self), permutations)
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        P::swaps(Arc::make_mut(self), permutations);
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::close_pattern(Arc::make_mut(self), state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        P::open_pattern(Arc::make_mut(self), state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        P::find_binder_index(self, free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        P::find_binder_at_offset(self, offset)
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

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        for elem in self {
            elem.freshen(permutations);
        }
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        for elem in self {
            elem.swaps(permutations);
        }
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        for elem in self {
            elem.close_pattern(state, pattern);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        for elem in self {
            elem.open_pattern(state, pattern);
        }
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        let mut skipped = BinderOffset(0);
        for elem in self {
            match elem.find_binder_index(free_var) {
                Ok(binder_index) => return Ok(binder_index + skipped),
                Err(next_skipped) => skipped += next_skipped,
            }
        }
        Err(skipped)
    }

    fn find_binder_at_offset(&self, mut offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        for elem in self {
            match elem.find_binder_at_offset(offset) {
                Ok(binder) => return Ok(binder),
                Err(next_offset) => offset = next_offset,
            }
        }
        Err(offset)
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

    fn freshen(&mut self, permutations: &mut Permutations<N>) {
        <[P]>::freshen(self, permutations);
    }

    fn swaps(&mut self, permutations: &Permutations<N>) {
        <[P]>::swaps(self, permutations)
    }

    fn close_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        <[P]>::close_pattern(self, state, pattern);
    }

    fn open_pattern(&mut self, state: ScopeState, pattern: &impl BoundPattern<N>) {
        <[P]>::open_pattern(self, state, pattern);
    }

    fn find_binder_index(&self, free_var: &FreeVar<N>) -> Result<BinderIndex, BinderOffset> {
        <[P]>::find_binder_index(self, free_var)
    }

    fn find_binder_at_offset(&self, offset: BinderOffset) -> Result<Binder<N>, BinderOffset> {
        <[P]>::find_binder_at_offset(self, offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn binder(ident: &str) -> Binder<&str> {
        Binder::user(ident)
    }

    fn free_var(ident: &str) -> FreeVar<&str> {
        FreeVar::user(ident)
    }

    mod binder {
        use super::*;

        mod find_binder_index {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(
                    binder("a").find_binder_index(&free_var("b")),
                    Err(BinderOffset(1)),
                );
            }

            #[test]
            fn test_found() {
                assert_eq!(
                    binder("a").find_binder_index(&free_var("a")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }
        }

        mod find_binder_at_offset {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(
                    binder("a").find_binder_at_offset(BinderOffset(2)),
                    Err(BinderOffset(1)),
                );
            }

            #[test]
            fn test_found() {
                assert_eq!(
                    binder("a").find_binder_at_offset(BinderOffset(0)),
                    Ok(binder("a")),
                );
            }
        }
    }

    mod unit {
        use super::*;

        mod find_binder_index {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(().find_binder_index(&free_var("a")), Err(BinderOffset(0)));
            }
        }

        mod find_binder_at_offset {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(
                    BoundPattern::<&str>::find_binder_at_offset(&(), BinderOffset(2)),
                    Err(BinderOffset(2))
                );
            }
        }
    }

    mod opt {
        use super::*;

        mod find_binder_index {
            use super::*;

            #[test]
            fn test_none_not_found() {
                assert_eq!(
                    None::<()>.find_binder_index(&free_var("a")),
                    Err(BinderOffset(0)),
                );
            }

            #[test]
            fn test_some_not_found() {
                assert_eq!(
                    Some(binder("a")).find_binder_index(&free_var("b")),
                    Err(BinderOffset(1)),
                );
            }

            #[test]
            fn test_some_found() {
                assert_eq!(
                    Some(binder("a")).find_binder_index(&free_var("a")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }
        }

        mod find_binder_at_offset {
            use super::*;

            #[test]
            fn test_none_not_found() {
                assert_eq!(
                    BoundPattern::<&str>::find_binder_at_offset(&None::<()>, BinderOffset(2)),
                    Err(BinderOffset(2))
                );
            }

            #[test]
            fn test_some_not_found() {
                assert_eq!(
                    Some(binder("a")).find_binder_at_offset(BinderOffset(2)),
                    Err(BinderOffset(1))
                );
            }

            #[test]
            fn test_some_found() {
                assert_eq!(
                    Some(binder("a")).find_binder_at_offset(BinderOffset(0)),
                    Ok(binder("a"))
                );
            }
        }
    }

    mod pair {
        use super::*;

        mod find_binder_index {
            use super::*;

            #[test]
            fn test_0_found() {
                assert_eq!(
                    (binder("a"), binder("b")).find_binder_index(&free_var("a")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }

            #[test]
            fn test_1_found() {
                assert_eq!(
                    (binder("a"), binder("b")).find_binder_index(&free_var("b")),
                    Ok(BinderIndex(BinderOffset(1))),
                );
            }

            #[test]
            fn test_opt_1_found() {
                assert_eq!(
                    ((), Some(binder("b"))).find_binder_index(&free_var("b")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }
        }

        mod find_binder_at_offset {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(
                    (binder("a"), binder("b")).find_binder_at_offset(BinderOffset(2)),
                    Err(BinderOffset(0))
                );
            }

            #[test]
            fn test_found() {
                assert_eq!(
                    (binder("a"), binder("b")).find_binder_at_offset(BinderOffset(1)),
                    Ok(binder("b"))
                );
            }
        }
    }

    mod vec {
        use super::*;

        mod find_binder_index {
            use super::*;

            #[test]
            fn test_0_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")].find_binder_index(&free_var("a")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }

            #[test]
            fn test_1_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")].find_binder_index(&free_var("b")),
                    Ok(BinderIndex(BinderOffset(1))),
                );
            }

            #[test]
            fn test_2_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")].find_binder_index(&free_var("c")),
                    Ok(BinderIndex(BinderOffset(2))),
                );
            }

            #[test]
            fn test_not_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")].find_binder_index(&free_var("d")),
                    Err(BinderOffset(3)),
                );
            }

            #[test]
            fn test_opt_1_found() {
                assert_eq!(
                    vec![None, Some(binder("b")), Some(binder("c"))]
                        .find_binder_index(&free_var("b")),
                    Ok(BinderIndex(BinderOffset(0))),
                );
            }

            #[test]
            fn test_opt_2_found() {
                assert_eq!(
                    vec![None, Some(binder("b")), Some(binder("c"))]
                        .find_binder_index(&free_var("c")),
                    Ok(BinderIndex(BinderOffset(1))),
                );
            }
        }

        mod find_binder_at_offset {
            use super::*;

            #[test]
            fn test_not_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")]
                        .find_binder_at_offset(BinderOffset(4)),
                    Err(BinderOffset(1))
                );
            }

            #[test]
            fn test_found() {
                assert_eq!(
                    vec![binder("a"), binder("b"), binder("c")]
                        .find_binder_at_offset(BinderOffset(1)),
                    Ok(binder("b"))
                );
            }

            #[test]
            fn test_opt_not_found() {
                assert_eq!(
                    vec![Some(binder("a")), None, Some(binder("c"))]
                        .find_binder_at_offset(BinderOffset(2)),
                    Err(BinderOffset(0))
                );
            }

            #[test]
            fn test_opt_found() {
                assert_eq!(
                    vec![Some(binder("a")), None, Some(binder("c"))]
                        .find_binder_at_offset(BinderOffset(1)),
                    Ok(binder("c"))
                );
            }
        }
    }
}
