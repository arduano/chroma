use std::{collections::HashMap, num::NonZeroU32};

use super::{Id, IdCounter};

pub struct ItemSet<T> {
    counter: IdCounter<T>,
    items: Vec<Option<T>>,
}

impl<T> ItemSet<T> {
    pub fn new() -> Self {
        Self {
            counter: IdCounter::new(),
            items: Vec::new(),
        }
    }

    fn get_slot(&self, id: Id<T>) -> Option<&Option<T>> {
        self.items.get(id.index().get() as usize - 1)
    }

    fn get_slot_mut(&mut self, id: Id<T>) -> Option<&mut Option<T>> {
        self.items.get_mut(id.index().get() as usize - 1)
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.get_slot(id)?.as_ref()
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.get_slot_mut(id)?.as_mut()
    }

    pub fn add_value(&mut self, val: T) -> Id<T> {
        self.items.push(Some(val));
        self.counter.next()
    }

    pub fn replace_value(&mut self, id: Id<T>, val: T) {
        let slot = self
            .get_mut(id)
            .expect("Tried to replace non-existing value");
        *slot = val;
    }

    pub fn allocate_id(&mut self) -> Id<T> {
        let id = self.counter.next();
        self.items.push(None);
        id
    }

    pub fn insert_allocated_value(&mut self, id: Id<T>, val: T) {
        let slot = self
            .get_slot_mut(id)
            .expect("Tried to insert an ID that doesn't exist");
        if !slot.is_none() {
            panic!("Inserting to allocated ID twice");
        }
        *slot = Some(val);
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        let start = 0;
        let end = self.items.len();
        (start..end).filter_map(|i| {
            if let Some(val) = self.items[i].as_ref() {
                Some((Id::new(NonZeroU32::new(i as u32 + 1).unwrap()), val))
            } else {
                None
            }
        })
    }

    pub fn keys<'a>(&'a self) -> impl Iterator<Item = Id<T>> + 'a {
        self.iter().map(|(id, _)| id)
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.iter().map(|(_, val)| val)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ItemSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let entries = self.iter();
        f.debug_map().entries(entries).finish()
    }
}

impl<T> std::ops::Index<Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.get(index).expect("Indexing non-existent ID")
    }
}

impl<T> std::ops::Index<&Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: &Id<T>) -> &Self::Output {
        &self.get(*index).expect("Indexing non-existent ID")
    }
}
