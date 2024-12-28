use std::collections::LinkedList;

pub trait ContainerHelpers<T> {
    type Base;
    fn make(v: T) -> Self::Base;
    fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&mut T) -> bool;
}

impl<T: Clone> ContainerHelpers<T> for LinkedList<T> {
    type Base = LinkedList<T>;

    fn make(v: T) -> Self::Base {
        let mut res = Self::new();
        res.push_back(v);
        res
    }

    fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        let mut new_container = Self::Base::new();
        while let Some(mut item) = self.pop_front() {
            if f(&mut item) {
                new_container.push_back(item);
            }
        }
        *self = new_container;
    }
}
