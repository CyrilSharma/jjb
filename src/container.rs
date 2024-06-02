use std::{collections::LinkedList, ops::Add};

#[derive(Clone)]
pub struct Container<T> {
    pub container: LinkedList<T>
}

// This was supposed to be done so I could swap out the underlying container,
// But it might've been easier to just implement Operations on a type.
impl<T> Container<T> {
    pub fn new() -> Self {
        Self { container: LinkedList::new() }
    }

    pub fn len(&self) -> usize {
        self.container.len()
    }

    pub fn make(v: T) -> Self {
        let mut res = Self::new();
        res.push_back(v);
        res
    }

    pub fn push_back(&mut self, v: T) {
        self.container.push_back(v);
    }

    pub fn push_front(&mut self, v: T) {
        self.container.push_front(v);
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.container.pop_front()
    }

    pub fn append(&mut self, mut v: Self) {
        self.container.append(&mut v.container);
    }

    pub fn back(&self) -> Option<&T> {
        self.container.back()
    }

    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.container.back_mut()
    }

    pub fn pop_back(&mut self) -> Option<T> {
        self.container.pop_back()
    }

    pub fn retain<F>(&mut self, mut f: F)
        where F: FnMut(&mut T) -> bool {
        let mut new_container = LinkedList::new();
        while let Some(mut item) = self.container.pop_front() {
            if f(&mut item) {
                new_container.push_back(item);
            }
        }
        self.container = new_container;
    }
}

// Explicit Default implementation
impl<T> Default for Container<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct ContainerIter<'a, T> {
    iter: std::collections::linked_list::Iter<'a, T>,
}

impl<'a, T> Iterator for ContainerIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<T> Container<T> {
    pub fn iter(&self) -> ContainerIter<T> {
        ContainerIter {
            iter: self.container.iter(),
        }
    }
}

impl<T> FromIterator<T> for Container<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut container = LinkedList::new();
        for item in iter {
            container.push_back(item);
        }
        Self { container }
    }
}

impl<'a, T> IntoIterator for &'a Container<T> {
    type Item = &'a T;
    type IntoIter = ContainerIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone)]
pub struct ContainerIntoIter<T> {
    current: Option<std::collections::linked_list::IntoIter<T>>,
}

impl<T> Iterator for ContainerIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iter) = &mut self.current {
            iter.next()
        } else {
            None
        }
    }
}

impl<T> IntoIterator for Container<T> {
    type Item = T;
    type IntoIter = ContainerIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        ContainerIntoIter {
            current: Some(self.container.into_iter())
        }
    }
}

impl<T> Add for Container<T> {
    type Output = Self;
    fn add(self, mut other: Self) -> Self {
        let mut new_container = self.container;
        new_container.append(&mut other.container);
        Self { container: new_container }
    }
}

pub struct ContainerIterMut<'a, T> {
    iter: std::collections::linked_list::IterMut<'a, T>,
}

impl<'a, T> Iterator for ContainerIterMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<T> Container<T> {
    pub fn iter_mut(&mut self) -> ContainerIterMut<T> {
        ContainerIterMut {
            iter: self.container.iter_mut(),
        }
    }
}

impl<'a, T> IntoIterator for &'a mut Container<T> {
    type Item = &'a mut T;
    type IntoIter = ContainerIterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}


impl<'a, T> DoubleEndedIterator for ContainerIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()
    }
}


impl<'a, T> DoubleEndedIterator for ContainerIterMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()
    }
}
