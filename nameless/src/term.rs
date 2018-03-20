use std::rc::Rc;

use {Debruijn, FreeName, Pattern};

pub trait Term {
    type FreeName: FreeName;
    type BoundName;

    fn close<P>(&mut self, pattern: &P)
    where
        P: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.close_at(Debruijn(0), pattern);
    }

    fn open<P>(&mut self, pattern: &P)
    where
        P: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.open_at(Debruijn(0), pattern);
    }

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>;

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = Self::FreeName, BoundName = Self::BoundName>;
}

impl<T: Term> Term for Option<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        if let Some(ref mut inner) = *self {
            inner.close_at(index, pattern);
        }
    }

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        if let Some(ref mut inner) = *self {
            inner.open_at(index, pattern);
        }
    }
}

impl<T: Term> Term for Box<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        (**self).close_at(index, pattern);
    }

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        (**self).open_at(index, pattern);
    }
}

impl<T: Term + Clone> Term for Rc<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        Rc::make_mut(self).close_at(index, pattern);
    }

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        Rc::make_mut(self).open_at(index, pattern);
    }
}

impl<T: Term + Clone> Term for [T] {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        for elem in self {
            elem.close_at(index, pattern);
        }
    }

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        for elem in self {
            elem.open_at(index, pattern);
        }
    }
}

impl<T: Term + Clone> Term for Vec<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        <[T]>::close_at(self, index, pattern)
    }

    fn open_at<P>(&mut self, index: Debruijn, pattern: &P)
    where
        P: Pattern<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        <[T]>::open_at(self, index, pattern)
    }
}
