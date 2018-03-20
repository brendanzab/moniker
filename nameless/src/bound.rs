use std::rc::Rc;

use {Binder, Debruijn, FreeName};

pub trait Bound {
    type FreeName: FreeName;
    type BoundName;

    fn close<B>(&mut self, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.close_at(Debruijn(0), binder);
    }

    fn open<B>(&mut self, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>,
    {
        self.open_at(Debruijn(0), binder);
    }

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>;

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = Self::FreeName, BoundName = Self::BoundName>;
}

impl<T: Bound> Bound for Option<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        if let Some(ref mut inner) = *self {
            inner.close_at(index, binder);
        }
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        if let Some(ref mut inner) = *self {
            inner.open_at(index, binder);
        }
    }
}

impl<T: Bound> Bound for Box<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        (**self).close_at(index, binder);
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        (**self).open_at(index, binder);
    }
}

impl<T: Bound + Clone> Bound for Rc<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        Rc::make_mut(self).close_at(index, binder);
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        Rc::make_mut(self).open_at(index, binder);
    }
}

impl<T: Bound + Clone> Bound for [T] {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        for elem in self {
            elem.close_at(index, binder);
        }
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        for elem in self {
            elem.open_at(index, binder);
        }
    }
}

impl<T: Bound + Clone> Bound for Vec<T> {
    type FreeName = T::FreeName;
    type BoundName = T::BoundName;

    fn close_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        <[T]>::close_at(self, index, binder)
    }

    fn open_at<B>(&mut self, index: Debruijn, binder: &B)
    where
        B: Binder<FreeName = T::FreeName, BoundName = T::BoundName>,
    {
        <[T]>::open_at(self, index, binder)
    }
}
