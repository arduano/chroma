use std::{collections::HashMap, sync::Arc};

use super::{Id, Id2, ItemSet};

pub struct GroupItemSet<M, T> {
    other_groups: HashMap<Id<M>, Arc<ItemSet<T>>>,
    current_group_id: Id<M>,
    current_group: ItemSet<T>,
}

impl<M, T> GroupItemSet<M, T> {
    pub fn new(other_groups: HashMap<Id<M>, Arc<ItemSet<T>>>, current_group_id: Id<M>) -> Self {
        Self {
            other_groups,
            current_group_id,
            current_group: ItemSet::new(),
        }
    }

    pub fn get(&self, id: Id2<M, T>) -> Option<&T> {
        if id.group() == self.current_group_id {
            self.current_group.get(id.item())
        } else {
            self.other_groups[&id.group()].get(id.item())
        }
    }

    pub fn add_value(&mut self, val: T) -> Id2<M, T> {
        let id = self.current_group.add_value(val);
        Id2::new(self.current_group_id, id)
    }

    pub fn replace_value(&mut self, id: Id2<M, T>, val: T) {
        assert_eq!(id.group(), self.current_group_id);
        self.current_group.replace_value(id.item(), val);
    }

    pub fn allocate_id(&mut self) -> Id2<M, T> {
        let id = self.current_group.allocate_id();
        Id2::new(self.current_group_id, id)
    }

    pub fn insert_allocated_value(&mut self, id: Id2<M, T>, val: T) -> Id2<M, T> {
        assert_eq!(id.group(), self.current_group_id);
        self.current_group.insert_allocated_value(id.item(), val);
        Id2::new(self.current_group_id, id.item())
    }

    pub fn keys(&self) -> impl '_ + Iterator<Item = Id2<M, T>> {
        let current_group_keys = self
            .current_group
            .keys()
            .map(move |id| Id2::new(self.current_group_id, id));

        let other_group_keys = self
            .other_groups
            .iter()
            .map(|(k, v)| v.keys().map(move |id| Id2::new(*k, id)))
            .flatten();

        current_group_keys.chain(other_group_keys)
    }
}

impl<M, T> std::ops::Index<Id2<M, T>> for GroupItemSet<M, T> {
    type Output = T;

    fn index(&self, index: Id2<M, T>) -> &Self::Output {
        if index.group() == self.current_group_id {
            &self.current_group[index.item()]
        } else {
            &self.other_groups[&index.group()][index.item()]
        }
    }
}

impl<M, T> std::ops::Index<&Id2<M, T>> for GroupItemSet<M, T> {
    type Output = T;

    fn index(&self, index: &Id2<M, T>) -> &Self::Output {
        if index.group() == self.current_group_id {
            &self.current_group[index.item()]
        } else {
            &self.other_groups[&index.group()][index.item()]
        }
    }
}

impl<M, T: std::fmt::Debug> std::fmt::Debug for GroupItemSet<M, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut keys_sorted = self.keys().collect::<Vec<_>>();
        keys_sorted.sort_unstable();

        let entries = keys_sorted
            .iter()
            .map(|key| {
                let val = &self[key];
                (key, val)
            })
            .collect::<Vec<_>>();

        f.debug_map().entries(entries).finish()
    }
}
