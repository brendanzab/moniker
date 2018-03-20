use {BoundName, Debruijn, PatternIndex, Term};

pub trait Pattern: Term {
    type NamePerm;

    fn freshen(&mut self) -> Self::NamePerm;
    fn rename(&mut self, perm: &Self::NamePerm);
    fn on_free(&self, index: Debruijn, name: &Self::FreeName) -> Option<BoundName>;
    fn on_bound(&self, index: Debruijn, name: BoundName) -> Option<Self::FreeName>;
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

    fn on_free(&self, index: Debruijn, name: &T::FreeName) -> Option<BoundName> {
        self.iter()
            .enumerate()
            .filter_map(|(i, pattern)| {
                pattern.on_free(index, name).map(|bound| {
                    assert_eq!(bound.pattern, PatternIndex(0));
                    BoundName {
                        pattern: PatternIndex(i as u32),
                        ..bound
                    }
                })
            })
            .next()
    }

    fn on_bound(&self, index: Debruijn, name: BoundName) -> Option<T::FreeName> {
        self.get(name.pattern.0 as usize).and_then(|pattern| {
            pattern.on_bound(
                index,
                BoundName {
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

    fn on_free(&self, index: Debruijn, name: &T::FreeName) -> Option<BoundName> {
        <[T]>::on_free(self, index, name)
    }

    fn on_bound(&self, index: Debruijn, name: BoundName) -> Option<T::FreeName> {
        <[T]>::on_bound(self, index, name)
    }
}
