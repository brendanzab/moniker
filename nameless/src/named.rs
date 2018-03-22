use std::hash::{Hash, Hasher};

use {AlphaEq, Bound, Free, Pattern, PatternIndex, ScopeState, Term};

/// A type annotated with a name for debugging purposes
///
/// The name is ignored for alpha equality comparisons
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named<N, T> {
    pub name: N,
    pub inner: T,
}

impl<N, T> Named<N, T> {
    pub fn new(name: N, inner: T) -> Named<N, T> {
        Named { name, inner }
    }
}

impl<N, T: AlphaEq> AlphaEq for Named<N, T> {
    fn alpha_eq(&self, other: &Named<N, T>) -> bool {
        T::alpha_eq(&self.inner, &other.inner)
    }
}

impl<T: Term> Term for Named<T::Free, T> {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.inner.close_term_at(state, pattern);
    }

    fn open_term_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P) {
        self.inner.open_term_at(state, pattern);
    }
}

impl<N: Free, T: Term<Free = N>> Pattern for Named<N, T> {
    type NamePerm = N;

    fn freshen(&mut self) -> N {
        self.name.freshen();
        self.name.clone()
    }

    fn rename(&mut self, perm: &N) {
        self.name = perm.clone(); // FIXME: double clone
    }

    fn on_free(&self, state: ScopeState, name: &Self::Free) -> Option<Bound> {
        match *name == self.name {
            true => Some(Bound {
                scope: state.depth(),
                pattern: PatternIndex(0),
            }),
            false => None,
        }
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free> {
        match name.scope == state.depth() {
            true => {
                assert_eq!(name.pattern, PatternIndex(0));
                Some(self.name.clone())
            },
            false => None,
        }
    }
}

impl<N, T: Hash> Hash for Named<N, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}
