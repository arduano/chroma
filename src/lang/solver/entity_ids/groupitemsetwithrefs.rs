use std::{collections::HashMap, sync::Arc};

use super::{GroupItemSet, Id, Id2, Id2OrVal, ItemSet};

pub struct GroupItemSetWithRefs<M, T> {
    item_set: GroupItemSet<M, Id2OrVal<M, T>>,
}

fn port_id_in<T, M>(id: Id2<M, T>) -> Id2<M, Id2OrVal<M, T>> {
    Id2::new(id.group(), Id::new(id.item().index()))
}

fn port_id_out<T, M>(id: Id2<M, Id2OrVal<M, T>>) -> Id2<M, T> {
    Id2::new(id.group(), Id::new(id.item().index()))
}

impl<M, T> GroupItemSetWithRefs<M, T> {
    pub fn new(
        other_groups: HashMap<Id<M>, Arc<ItemSet<Id2OrVal<M, T>>>>,
        current_group_id: Id<M>,
    ) -> Self {
        Self {
            item_set: GroupItemSet::new(other_groups, current_group_id),
        }
    }

    pub fn get(&self, id: Id2<M, T>) -> Option<&T> {
        let mut id = port_id_in(id);
        loop {
            match self.item_set.get(id) {
                Some(Id2OrVal::Id(new_id)) => {
                    id = port_id_in(*new_id);
                }
                Some(Id2OrVal::Val(val)) => return Some(val),
                None => return None,
            }
        }
    }

    pub fn add_value(&mut self, val: T) -> Id2<M, T> {
        let id = self.item_set.add_value(Id2OrVal::Val(val));
        port_id_out(id)
    }

    pub fn replace_value(&mut self, id: Id2<M, T>, val: T) {
        self.item_set
            .replace_value(port_id_in(id), Id2OrVal::Val(val));
    }

    pub fn get_id_for_val_or_id(&mut self, val: Id2OrVal<M, T>) -> Id2<M, T> {
        match val {
            Id2OrVal::Id(id) => id,
            Id2OrVal::Val(val) => self.add_value(val),
        }
    }

    pub fn get_val_for_val_or_id<'a>(&'a self, val: &'a Id2OrVal<M, T>) -> Option<&'a T> {
        match val {
            Id2OrVal::Id(id) => self.get(*id),
            Id2OrVal::Val(val) => Some(val),
        }
    }

    pub fn allocate_id(&mut self) -> Id2<M, T> {
        let id = self.item_set.allocate_id();
        port_id_out(id)
    }

    pub fn insert_val_for_allocated_value(&mut self, id: Id2<M, T>, val: T) -> Id2<M, T> {
        let insert_id = port_id_in(id);

        let id = self
            .item_set
            .insert_allocated_value(insert_id, Id2OrVal::Val(val));

        port_id_out(id)
    }

    pub fn insert_ref_for_allocated_value(
        &mut self,
        id: Id2<M, T>,
        reference: Id2<M, T>,
    ) -> Id2<M, T> {
        let insert_id = port_id_in(id);

        let id = self
            .item_set
            .insert_allocated_value(insert_id, Id2OrVal::Id(reference));

        port_id_out(id)
    }

    pub fn insert_for_allocated_value(&mut self, id: Id2<M, T>, val: Id2OrVal<M, T>) -> Id2<M, T> {
        let insert_id = port_id_in(id);
        let id = self.item_set.insert_allocated_value(insert_id, val);
        port_id_out(id)
    }

    pub fn keys(&self) -> impl '_ + Iterator<Item = Id2<M, T>> {
        self.item_set.keys().map(port_id_out)
    }
}

impl<M, T> std::ops::Index<Id2<M, T>> for GroupItemSetWithRefs<M, T> {
    type Output = T;

    fn index(&self, index: Id2<M, T>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<M, T> std::ops::Index<&Id2<M, T>> for GroupItemSetWithRefs<M, T> {
    type Output = T;

    fn index(&self, index: &Id2<M, T>) -> &Self::Output {
        self.get(*index).unwrap()
    }
}

impl<M, T: std::fmt::Debug> std::fmt::Debug for GroupItemSetWithRefs<M, T> {
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
