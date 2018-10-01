use binder::Binder;
use bound::{BoundPattern, OnBoundFn, OnFreeFn, ScopeState};

/// Nested binding patterns
///
/// Contrast with `Multi`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nest<P> {
    pub unsafe_patterns: Vec<P>,
}

impl<P> Nest<P> {
    /// Nest a term with the given patterns
    pub fn new<N>(patterns: Vec<P>) -> Nest<P>
    where
        N: Clone + PartialEq,
        P: BoundPattern<N>,
    {
        let mut binders = Vec::with_capacity(patterns.len());
        // FIXME: Avoid allocating new vector
        let mut nested_patterns = Vec::<P>::with_capacity(patterns.len());

        for mut pattern in patterns {
            pattern.close_pattern(ScopeState::new(), &binders);
            binders.push(pattern.binders());
            nested_patterns.push(pattern);
        }

        Nest {
            unsafe_patterns: nested_patterns,
        }
    }

    /// Unnest a term, returning the freshened patterns
    pub fn unnest<N>(self) -> Vec<P>
    where
        N: Clone + PartialEq,
        P: BoundPattern<N>,
    {
        let mut binders = Vec::with_capacity(self.unsafe_patterns.len());
        // FIXME: Avoid allocating new vector
        let mut unnested_patterns = Vec::<P>::with_capacity(self.unsafe_patterns.len());

        for mut pattern in self.unsafe_patterns {
            pattern.open_pattern(ScopeState::new(), &binders);
            binders.push(pattern.binders());
            unnested_patterns.push(pattern);
        }

        unnested_patterns
    }
}

impl<N, P> BoundPattern<N> for Nest<P>
where
    N: Clone + PartialEq,
    P: BoundPattern<N>,
{
    fn pattern_eq(&self, other: &Nest<P>) -> bool {
        <[P]>::pattern_eq(&self.unsafe_patterns, &other.unsafe_patterns)
    }

    fn close_pattern(&mut self, mut state: ScopeState, on_free: &impl OnFreeFn<N>) {
        for elem in &mut self.unsafe_patterns {
            elem.close_pattern(state, on_free);
            state = state.incr();
        }
    }

    fn open_pattern(&mut self, mut state: ScopeState, on_bound: &impl OnBoundFn<N>) {
        for elem in &mut self.unsafe_patterns {
            elem.open_pattern(state, on_bound);
            state = state.incr();
        }
    }

    fn visit_binders(&self, on_binder: &mut impl FnMut(&Binder<N>)) {
        <[P]>::visit_binders(&self.unsafe_patterns, on_binder);
    }

    fn visit_mut_binders(&mut self, on_binder: &mut impl FnMut(&mut Binder<N>)) {
        <[P]>::visit_mut_binders(&mut self.unsafe_patterns, on_binder);
    }
}
