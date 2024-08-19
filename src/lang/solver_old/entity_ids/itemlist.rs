use std::num::NonZeroU32;

use super::{Id, IdCounter};

pub struct ItemList<T> {
    counter: IdCounter<T>,
    items: Vec<T>,
}

impl<T> ItemList<T> {
    pub fn new() -> Self {
        Self {
            counter: IdCounter::new(),
            items: Vec::new(),
        }
    }

    pub fn get_index_for_id(&self, id: Id<T>) -> usize {
        id.index().get() as usize - 1
    }

    pub fn id_for_index(&self, index: usize) -> Id<T> {
        Id::new(NonZeroU32::new(index as u32 + 1).unwrap())
    }

    pub fn as_slice(&self) -> &[T] {
        &self.items
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(id.index().get() as usize - 1)
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.items.get_mut(id.index().get() as usize - 1)
    }

    pub fn add_value(&mut self, val: T) -> Id<T> {
        self.items.push(val);
        self.counter.next()
    }

    pub fn replace_value(&mut self, id: Id<T>, val: T) {
        let slot = self
            .get_mut(id)
            .expect("Tried to replace non-existing value");
        *slot = val;
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, val)| (self.id_for_index(i), val))
    }

    pub fn keys<'a>(&'a self) -> impl Iterator<Item = Id<T>> + 'a {
        (0..self.items.len()).map(|i| self.id_for_index(i))
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn first_id(&self) -> Option<Id<T>> {
        if self.items.is_empty() {
            return None;
        }

        Some(self.id_for_index(0))
    }

    pub fn last_id(&self) -> Option<Id<T>> {
        if self.items.is_empty() {
            return None;
        }

        Some(self.id_for_index(self.items.len() - 1))
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ItemList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let entries = self.iter();
        f.debug_map().entries(entries).finish()
    }
}

impl<T> std::ops::Index<Id<T>> for ItemList<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.get(index).expect("Indexing non-existent ID")
    }
}

impl<T> std::ops::Index<&Id<T>> for ItemList<T> {
    type Output = T;

    fn index(&self, index: &Id<T>) -> &Self::Output {
        &self.get(*index).expect("Indexing non-existent ID")
    }
}
