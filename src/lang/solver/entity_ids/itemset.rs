use std::collections::HashMap;

use super::{Id, IdCounter};

pub struct ItemSet<T> {
    counter: IdCounter<T>,
    items: HashMap<Id<T>, T>,
}

impl<T> ItemSet<T> {
    pub fn new() -> Self {
        Self {
            counter: IdCounter::new(),
            items: HashMap::new(),
        }
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(&id)
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.items.get_mut(&id)
    }

    pub fn add_value(&mut self, val: T) -> Id<T> {
        let id = self.counter.next();
        self.items.insert(id, val);
        id
    }

    pub fn replace_value(&mut self, id: Id<T>, val: T) {
        let existing = self.items.insert(id, val);
        debug_assert!(existing.is_some(), "Tried to replace non-existing value");
    }

    pub fn allocate_id(&mut self) -> Id<T> {
        let id = self.counter.next();
        id
    }

    pub fn insert_allocated_value(&mut self, id: Id<T>, val: T) {
        let existing = self.items.insert(id, val);
        assert!(existing.is_none());
    }

    pub fn keys(&self) -> impl Iterator<Item = &Id<T>> {
        self.items.keys()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ItemSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut keys_sorted = self.items.keys().collect::<Vec<_>>();
        keys_sorted.sort_unstable();

        let entries = keys_sorted
            .iter()
            .map(|key| {
                let val = &self.items[key];
                (key, val)
            })
            .collect::<Vec<_>>();

        f.debug_map().entries(entries).finish()
    }
}

impl<T> std::ops::Index<Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.items[&index]
    }
}

impl<T> std::ops::Index<&Id<T>> for ItemSet<T> {
    type Output = T;

    fn index(&self, index: &Id<T>) -> &Self::Output {
        &self.items[index]
    }
}
