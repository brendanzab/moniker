use std::rc::Rc;

use {Debruijn, FreeName, Pattern};

#[derive(Debug, Copy, Clone)]
pub struct ScopeState {
    depth: u32,
}

impl ScopeState {
    pub fn new() -> ScopeState {
        ScopeState { depth: 0 }
    }

    pub fn depth(&self) -> Debruijn {
        Debruijn(self.depth)
    }

    pub fn incr(mut self) -> ScopeState {
        self.depth += 1;
        self
    }
}

pub trait Term {
    type FreeName: FreeName;

    fn close<P: Pattern<FreeName = Self::FreeName>>(&mut self, pattern: &P) {
        self.close_at(ScopeState::new(), pattern);
    }

    fn open<P: Pattern<FreeName = Self::FreeName>>(&mut self, pattern: &P) {
        self.open_at(ScopeState::new(), pattern);
    }

    fn close_at<P: Pattern<FreeName = Self::FreeName>>(&mut self, state: ScopeState, pattern: &P);
    fn open_at<P: Pattern<FreeName = Self::FreeName>>(&mut self, state: ScopeState, pattern: &P);
}

impl<T: Term> Term for Option<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.close_at(state, pattern);
        }
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.open_at(state, pattern);
        }
    }
}

impl<T: Term> Term for Box<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).close_at(state, pattern);
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).open_at(state, pattern);
    }
}

impl<T: Term + Clone> Term for Rc<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).close_at(state, pattern);
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).open_at(state, pattern);
    }
}

impl<T: Term + Clone> Term for [T] {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.close_at(state, pattern);
        }
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.open_at(state, pattern);
        }
    }
}

impl<T: Term + Clone> Term for Vec<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::close_at(self, state, pattern)
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::open_at(self, state, pattern)
    }
}
