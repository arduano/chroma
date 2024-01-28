use std::{
    cell::UnsafeCell, collections::BTreeMap, marker::PhantomData, mem::MaybeUninit,
    num::NonZeroU32, rc::Rc, sync::Arc,
};

use futures::{stream, Future, Stream, StreamExt};
use futures_intrusive::sync::ManualResetEvent;

pub struct Id<T>(NonZeroU32, PhantomData<T>);

impl<T> Id<T> {
    pub fn new(index: NonZeroU32) -> Self {
        Self(index, PhantomData)
    }

    pub fn index(&self) -> NonZeroU32 {
        self.0
    }
}

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

struct KnownItem<T> {
    ready: ManualResetEvent,
    item: UnsafeCell<MaybeUninit<T>>,
}

unsafe impl<T> Send for KnownItem<T> {}
unsafe impl<T> Sync for KnownItem<T> {}

impl<T> KnownItem<T> {
    pub fn new() -> Self {
        Self {
            ready: ManualResetEvent::new(false),
            item: UnsafeCell::new(MaybeUninit::uninit()),
        }
    }

    pub fn get(&self) -> Option<&T> {
        if self.ready.is_set() {
            Some(unsafe { (&*self.item.get()).assume_init_ref() })
        } else {
            None
        }
    }

    pub fn set(&self, value: T) {
        unsafe {
            (&mut *self.item.get()).write(value);
        }
        self.ready.set();
    }

    pub async fn wait_then_get(&self) -> &T {
        self.ready.wait().await;
        unsafe { (&*self.item.get()).assume_init_ref() }
    }
}
