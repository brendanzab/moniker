use {Debruijn, Term};

pub trait Pattern: Term {
    type NamePerm;

    fn freshen(&mut self) -> Self::NamePerm;
    fn rename(&mut self, perm: &Self::NamePerm);
    fn on_free(&self, index: Debruijn, name: &Self::FreeName) -> Option<Self::BoundName>;
    fn on_bound(&self, index: Debruijn, name: &Self::BoundName) -> Option<Self::FreeName>;
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

    fn on_free(&self, index: Debruijn, name: &T::FreeName) -> Option<T::BoundName> {
        self.iter()
            .enumerate()
            .filter_map(|(_i, pattern)| pattern.on_free(index, name)) // FIXME: use binding number
            .next() // FIXME: return bind
    }

    fn on_bound(&self, index: Debruijn, name: &T::BoundName) -> Option<T::FreeName> {
        self.iter()
            .enumerate()
            .filter_map(|(_i, pattern)| pattern.on_bound(index, name)) // FIXME: use binding number
            .next()
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

    fn on_free(&self, index: Debruijn, name: &T::FreeName) -> Option<T::BoundName> {
        <[T]>::on_free(self, index, name)
    }

    fn on_bound(&self, index: Debruijn, name: &T::BoundName) -> Option<T::FreeName> {
        <[T]>::on_bound(self, index, name)
    }
}
