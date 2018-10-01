use codespan::{
    ByteIndex, ByteOffset, ColumnIndex, ColumnNumber, ColumnOffset, LineIndex, LineNumber,
    LineOffset, Span,
};

use super::*;

macro_rules! impl_bound_term_ignore {
    ($T:ty) => {
        impl<N: Clone + PartialEq> BoundTerm<N> for $T {
            fn term_eq(&self, _: &$T) -> bool {
                true
            }

            fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
        }
    };
}

impl_bound_term_ignore!(ByteIndex);
impl_bound_term_ignore!(ByteOffset);
impl_bound_term_ignore!(ColumnIndex);
impl_bound_term_ignore!(ColumnNumber);
impl_bound_term_ignore!(ColumnOffset);
impl_bound_term_ignore!(LineIndex);
impl_bound_term_ignore!(LineNumber);
impl_bound_term_ignore!(LineOffset);

impl<N: Clone + PartialEq, T> BoundTerm<N> for Span<T> {
    fn term_eq(&self, _: &Span<T>) -> bool {
        true
    }

    fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

    fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

    fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

    fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
}

macro_rules! impl_bound_pattern_ignore {
    ($T:ty) => {
        impl<N: Clone + PartialEq> BoundPattern<N> for $T {
            fn pattern_eq(&self, _: &$T) -> bool {
                true
            }

            fn close_pattern(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

            fn open_pattern(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

            fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

            fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
        }
    };
}

impl_bound_pattern_ignore!(ByteIndex);
impl_bound_pattern_ignore!(ByteOffset);
impl_bound_pattern_ignore!(ColumnIndex);
impl_bound_pattern_ignore!(ColumnNumber);
impl_bound_pattern_ignore!(ColumnOffset);
impl_bound_pattern_ignore!(LineIndex);
impl_bound_pattern_ignore!(LineNumber);
impl_bound_pattern_ignore!(LineOffset);

impl<N: Clone + PartialEq, T> BoundPattern<N> for Span<T> {
    fn pattern_eq(&self, _: &Span<T>) -> bool {
        true
    }

    fn close_pattern(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

    fn open_pattern(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

    fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

    fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
}
