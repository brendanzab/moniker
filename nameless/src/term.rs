use std::rc::Rc;

use {Debruijn, Pattern};

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
    type Free;

    fn close_term<P: Pattern<Free = Self::Free>>(&mut self, pattern: &P) {
        self.close_term_at(ScopeState::new(), pattern);
    }

    fn open_term<P: Pattern<Free = Self::Free>>(&mut self, pattern: &P) {
        self.open_term_at(ScopeState::new(), pattern);
    }

    fn close_term_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P);
    fn open_term_at<P: Pattern<Free = Self::Free>>(&mut self, state: ScopeState, pattern: &P);
}

impl<T: Term> Term for Option<T> {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.close_term_at(state, pattern);
        }
    }

    fn open_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.open_term_at(state, pattern);
        }
    }
}

impl<T: Term> Term for Box<T> {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).close_term_at(state, pattern);
    }

    fn open_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        (**self).open_term_at(state, pattern);
    }
}

impl<T: Term + Clone> Term for Rc<T> {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).close_term_at(state, pattern);
    }

    fn open_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        Rc::make_mut(self).open_term_at(state, pattern);
    }
}

impl<T: Term + Clone> Term for [T] {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.close_term_at(state, pattern);
        }
    }

    fn open_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        for elem in self {
            elem.open_term_at(state, pattern);
        }
    }
}

impl<T: Term + Clone> Term for Vec<T> {
    type Free = T::Free;

    fn close_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::close_term_at(self, state, pattern)
    }

    fn open_term_at<P: Pattern<Free = T::Free>>(&mut self, state: ScopeState, pattern: &P) {
        <[T]>::open_term_at(self, state, pattern)
    }
}
