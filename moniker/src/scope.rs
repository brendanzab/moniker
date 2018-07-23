use std::hash::Hash;

use binder::Binder;
use bound::{BoundPattern, BoundTerm, Permutations, ScopeState};
use var::Var;

/// A bound scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<P, T> {
    /// The pattern that binds the body of the scope
    ///
    /// You can access this directly, but only if you understand what you are
    /// doing! Prefer calling `Scope::unbind` instead.
    pub unsafe_pattern: P,
    /// The body of the scope
    ///
    /// You can access this directly, but only if you understand what you are
    /// doing! Prefer calling `Scope::unbind` instead.
    pub unsafe_body: T,
}

impl<P, T> Scope<P, T> {
    /// Create a new scope by binding a term with the given pattern
    pub fn new<N>(pattern: P, mut body: T) -> Scope<P, T>
    where
        N: Clone,
        P: BoundPattern<N>,
        T: BoundTerm<N>,
    {
        body.close_term(ScopeState::new(), &pattern.binders());

        Scope {
            unsafe_pattern: pattern,
            unsafe_body: body,
        }
    }

    /// Unbind a term, returning the freshened pattern and body
    pub fn unbind<N>(self) -> (P, T)
    where
        N: Clone + Eq + Hash,
        P: BoundPattern<N>,
        T: BoundTerm<N>,
    {
        let mut pattern = self.unsafe_pattern;
        let mut body = self.unsafe_body;

        {
            let mut permutations = Permutations::new();
            pattern.freshen(&mut permutations); // FIXME: `permutations` is unused here!
            body.open_term(ScopeState::new(), &pattern.binders());
        }

        (pattern, body)
    }

    /// Simultaneously unbind two terms
    ///
    /// The fresh names in the first pattern with be used for the second pattern
    pub fn unbind2<N, P2, T2>(self, other: Scope<P2, T2>) -> (P, T, P2, T2)
    where
        N: Clone + Eq + Hash,
        P: BoundPattern<N>,
        T: BoundTerm<N>,
        P2: BoundPattern<N>,
        T2: BoundTerm<N>,
    {
        let mut self_pattern = self.unsafe_pattern;
        let mut self_body = self.unsafe_body;
        let mut other_pattern = other.unsafe_pattern;
        let mut other_body = other.unsafe_body;

        {
            let mut permutations = Permutations::new();
            self_pattern.freshen(&mut permutations);
            other_pattern.swaps(&permutations);
            self_body.open_term(ScopeState::new(), &self_pattern.binders());
            other_body.open_term(ScopeState::new(), &other_pattern.binders());
        }

        (self_pattern, self_body, other_pattern, other_body)
    }
}

impl<N, P, T> BoundTerm<N> for Scope<P, T>
where
    P: BoundPattern<N>,
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &Scope<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.unsafe_pattern.close_pattern(state, binders);
        self.unsafe_body.close_term(state.incr(), binders);
    }

    fn open_term(&mut self, state: ScopeState, binders: &[Binder<N>]) {
        self.unsafe_pattern.open_pattern(state, binders);
        self.unsafe_body.open_term(state.incr(), binders);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        self.unsafe_body.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        self.unsafe_body.visit_mut_vars(on_var);
    }
}
