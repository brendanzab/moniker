use {Bound, PatternIndex, ScopeState, Term};

pub trait Pattern: Term {
    // TODO: This is kinda yuck - need to figure out a better way!
    type NamePerm;

    fn freshen(&mut self) -> Self::NamePerm;
    fn rename(&mut self, perm: &Self::NamePerm);

    /// A callback that is used when `unbind`ing `Scope`s to replace free names
    /// with bound names based on the contents of the pattern
    fn on_free(&self, state: ScopeState, name: &Self::Free) -> Option<Bound>;

    /// A callback that is used when `bind`ing `Scope`s to replace bound names
    /// with free names based on the contents of the pattern
    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<Self::Free>;
}

impl<T: Pattern + Clone> Pattern for [T] {
    type NamePerm = Vec<T::NamePerm>;

    fn freshen(&mut self) -> Vec<T::NamePerm> {
        self.iter_mut().map(|pattern| pattern.freshen()).collect()
    }

    fn rename(&mut self, perm: &Vec<T::NamePerm>) {
        assert_eq!(self.len(), perm.len());

        for (pattern, perm) in <_>::zip(self.iter_mut(), perm.iter()) {
            pattern.rename(perm);
        }
    }

    fn on_free(&self, state: ScopeState, name: &T::Free) -> Option<Bound> {
        self.iter()
            .enumerate()
            .filter_map(|(i, pattern)| {
                pattern.on_free(state, name).map(|bound| {
                    assert_eq!(bound.pattern, PatternIndex(0));
                    Bound {
                        pattern: PatternIndex(i as u32),
                        ..bound
                    }
                })
            })
            .next()
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<T::Free> {
        self.get(name.pattern.0 as usize).and_then(|pattern| {
            pattern.on_bound(
                state,
                Bound {
                    pattern: PatternIndex(0),
                    ..name
                },
            )
        })
    }
}

impl<T: Pattern + Clone> Pattern for Vec<T> {
    type NamePerm = Vec<T::NamePerm>;

    fn freshen(&mut self) -> Vec<T::NamePerm> {
        <[T]>::freshen(self)
    }

    fn rename(&mut self, perm: &Vec<T::NamePerm>) {
        <[T]>::rename(self, perm)
    }

    fn on_free(&self, state: ScopeState, name: &T::Free) -> Option<Bound> {
        <[T]>::on_free(self, state, name)
    }

    fn on_bound(&self, state: ScopeState, name: Bound) -> Option<T::Free> {
        <[T]>::on_bound(self, state, name)
    }
}
