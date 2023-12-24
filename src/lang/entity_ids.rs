use std::{
    collections::BTreeMap,
    marker::PhantomData,
    sync::{Arc, RwLock},
};

struct Id<T>(u32, PhantomData<T>);

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(std::any::type_name::<T>())
            .field(&self.0)
            .finish()
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl Copy for Id<()> {}

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

// #[derive(Debug, Clone)]
// pub struct KnownItemHandler<K, V> {
//     items: Arc<RwLock<Vec<Arc<V>>>>,
//     _marker: PhantomData<K>,
// }

// impl<K, V> KnownItemHandler<K, V> {
//     pub fn new() -> Self {
//         Self {
//             items: Arc::new(RwLock::new(Vec::new())),
//             _marker: PhantomData,
//         }
//     }

//     pub fn get(&self, id: Id<K>) -> Option<Arc<V>> {
//         self.items.read().unwrap().get(&id).cloned()
//     }

//     pub fn insert(&self, id: Id<K>, value: V) {}
// }
