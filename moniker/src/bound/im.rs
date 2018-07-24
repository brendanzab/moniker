#[cfg(feature = "im")]
extern crate im;

use im::Vector;

use super::*;

impl<N, T> BoundTerm<N> for Vector<T>
where
    T: Clone + BoundTerm<N>,
{
    fn term_eq(&self, other: &Vector<T>) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| T::term_eq(lhs, rhs))
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self.iter_mut() {
            elem.close_term(state, binders);
        }
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self.iter_mut() {
            elem.open_term(state, binders);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        for elem in self.iter() {
            elem.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        for elem in self.iter_mut() {
            elem.visit_mut_vars(on_var);
        }
    }
}

impl<N, P> BoundPattern<N> for Vector<P>
where
    N: Clone,
    P: Clone + BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Vector<P>) -> bool {
        self.len() == other.len()
            && <_>::zip(self.iter(), other.iter()).all(|(lhs, rhs)| P::pattern_eq(lhs, rhs))
    }

    fn close_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self.iter_mut() {
            elem.close_pattern(state, binders);
        }
    }

    fn open_pattern(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        for elem in self.iter_mut() {
            elem.open_pattern(state, binders);
        }
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        for elem in self.iter() {
            elem.visit_binders(on_binder);
        }
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        for elem in self.iter_mut() {
            elem.visit_mut_binders(on_binder);
        }
    }
}
