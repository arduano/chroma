use std::{
    cell::UnsafeCell, collections::HashMap, marker::PhantomData, mem::MaybeUninit, num::NonZeroU32,
};

use super::Id2;

pub struct Id<T>(NonZeroU32, PhantomData<T>);

impl<T> Id<T> {
    pub fn new(index: NonZeroU32) -> Self {
        Self(index, PhantomData)
    }

    pub fn index(&self) -> NonZeroU32 {
        self.0
    }

    pub fn for_group<M>(self, group: Id<M>) -> Id2<M, T> {
        Id2::new(group, self)
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = std::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "{}({})", name, self.0)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Eq for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

pub struct IdCounter<T> {
    counter: Id<T>,
}

impl<T> IdCounter<T> {
    pub fn new() -> Self {
        Self {
            counter: Id::new(NonZeroU32::MIN),
        }
    }

    pub fn next(&mut self) -> Id<T> {
        let id = self.counter;
        self.counter = Id::new(self.counter.0.checked_add(1).unwrap());
        id
    }
}
