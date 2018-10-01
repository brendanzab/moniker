use std::hash::Hash;

use binder::Binder;
use bound::{BoundPattern, BoundTerm, OnBoundFn, OnFreeFn, ScopeState};
use free_var::FreeVar;
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
        N: Clone + PartialEq,
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

        // Freshen the pattern in preparation for opening
        pattern.visit_mut_binders(&mut |binder| {
            *binder = Binder(FreeVar::fresh(binder.0.pretty_name.clone()));
        });
        // Use the freshened binders when opening the body
        body.open_term(ScopeState::new(), &pattern.binders());

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
        use std::collections::HashMap;

        // FIXME: Just porting the behavior of Unbound here. Is there _any_ way
        // we can make this faster and less allocation-heavy? :/

        let mut self_pattern = self.unsafe_pattern;
        let mut other_pattern = other.unsafe_pattern;
        let mut self_body = self.unsafe_body;
        let mut other_body = other.unsafe_body;

        // Freshen the patterns
        {
            let self_binders = self_pattern.binders();
            let other_binders = other_pattern.binders();

            // Can't simultaneously unbind patterns of differing lengths!
            // TODO: return an error rather than panicking?
            assert_eq!(self_binders.len(), other_binders.len());

            // Get the permutation that takes us from from the binders in
            // `other_pattern` to the binders `self_pattern`
            let other_to_self =
                <_>::zip(other_binders.iter().cloned(), self_binders.iter().cloned())
                    .collect::<HashMap<_, _>>();

            // Freshen `self_pattern` while also getting the permutation that
            // takes us from from the binders in `self_pattern` to the fresh
            // binders
            let mut self_to_fresh = HashMap::new();
            self_pattern.visit_mut_binders(&mut |binder| {
                let fresh = Binder(FreeVar::fresh(binder.0.pretty_name.clone()));
                self_to_fresh.insert(binder.clone(), fresh.clone());
                *binder = fresh;
            });

            // Apply the composition of the permutations in order to freshen
            // the binders in `other_pattern`
            other_pattern.visit_mut_binders(&mut |binder| {
                if let Some(new_binder) = other_to_self.get(binder) {
                    *binder = new_binder.clone();
                }
                if let Some(new_binder) = self_to_fresh.get(binder) {
                    *binder = new_binder.clone();
                }
            })
        }

        // Finally, use the binders from the freshened patterns to open the body terms
        self_body.open_term(ScopeState::new(), &self_pattern.binders());
        other_body.open_term(ScopeState::new(), &other_pattern.binders());

        (self_pattern, self_body, other_pattern, other_body)
    }
}

impl<N, P, T> BoundTerm<N> for Scope<P, T>
where
    N: Clone + PartialEq,
    P: BoundPattern<N>,
    T: BoundTerm<N>,
{
    fn term_eq(&self, other: &Scope<P, T>) -> bool {
        P::pattern_eq(&self.unsafe_pattern, &other.unsafe_pattern)
            && T::term_eq(&self.unsafe_body, &other.unsafe_body)
    }

    fn close_term(&mut self, state: ScopeState, on_free: &impl OnFreeFn<N>) {
        self.unsafe_pattern.close_pattern(state, on_free);
        self.unsafe_body.close_term(state.incr(), on_free);
    }

    fn open_term(&mut self, state: ScopeState, on_bound: &impl OnBoundFn<N>) {
        self.unsafe_pattern.open_pattern(state, on_bound);
        self.unsafe_body.open_term(state.incr(), on_bound);
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<N>)) {
        self.unsafe_body.visit_vars(on_var);
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<N>)) {
        self.unsafe_body.visit_mut_vars(on_var);
    }
}
