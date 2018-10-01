use num_bigint::{BigInt, BigUint};

use super::*;

macro_rules! impl_bound_term_partial_eq {
    ($T:ty) => {
        impl<N: Clone + PartialEq> BoundTerm<N> for $T {
            fn term_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
        }
    };
}

impl_bound_term_partial_eq!(BigInt);
impl_bound_term_partial_eq!(BigUint);

macro_rules! impl_bound_pattern_partial_eq {
    ($T:ty) => {
        impl<N: Clone + PartialEq> BoundPattern<N> for $T {
            fn pattern_eq(&self, other: &$T) -> bool {
                self == other
            }

            fn close_pattern(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

            fn open_pattern(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

            fn visit_binders(&self, _: &mut impl FnMut(&Binder<N>)) {}

            fn visit_mut_binders(&mut self, _: &mut impl FnMut(&mut Binder<N>)) {}
        }
    };
}

impl_bound_pattern_partial_eq!(BigInt);
impl_bound_pattern_partial_eq!(BigUint);
