use std::rc::Rc;

use {Debruijn, FreeName, Pattern};

pub trait Term {
    type FreeName: FreeName;

    fn close<P: Pattern<FreeName = Self::FreeName>>(&mut self, pattern: &P) {
        self.close_at(Debruijn(0), pattern);
    }

    fn open<P: Pattern<FreeName = Self::FreeName>>(&mut self, pattern: &P) {
        self.open_at(Debruijn(0), pattern);
    }

    fn close_at<P: Pattern<FreeName = Self::FreeName>>(&mut self, index: Debruijn, pattern: &P);
    fn open_at<P: Pattern<FreeName = Self::FreeName>>(&mut self, index: Debruijn, pattern: &P);
}

impl<T: Term> Term for Option<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.close_at(index, pattern);
        }
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        if let Some(ref mut inner) = *self {
            inner.open_at(index, pattern);
        }
    }
}

impl<T: Term> Term for Box<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        (**self).close_at(index, pattern);
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        (**self).open_at(index, pattern);
    }
}

impl<T: Term + Clone> Term for Rc<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        Rc::make_mut(self).close_at(index, pattern);
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        Rc::make_mut(self).open_at(index, pattern);
    }
}

impl<T: Term + Clone> Term for [T] {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        for elem in self {
            elem.close_at(index, pattern);
        }
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        for elem in self {
            elem.open_at(index, pattern);
        }
    }
}

impl<T: Term + Clone> Term for Vec<T> {
    type FreeName = T::FreeName;

    fn close_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        <[T]>::close_at(self, index, pattern)
    }

    fn open_at<P: Pattern<FreeName = T::FreeName>>(&mut self, index: Debruijn, pattern: &P) {
        <[T]>::open_at(self, index, pattern)
    }
}
