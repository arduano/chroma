use std::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit, sync::Arc};

use futures::Future;
use futures_intrusive::sync::ManualResetEvent;

pub struct Id<T>(u32, PhantomData<T>);

impl<T> Id<T> {
    pub fn new(index: u32) -> Self {
        Self(index, PhantomData)
    }

    pub fn index(&self) -> u32 {
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

#[derive(Clone)]
pub struct KnownItemHandler<T: 'static + Send + Sync> {
    items: Arc<boxcar::Vec<KnownItem<Arc<T>>>>,
}

impl<T: 'static + Send + Sync> KnownItemHandler<T> {
    pub fn new() -> Self {
        Self {
            items: Arc::new(boxcar::Vec::new()),
        }
    }

    pub fn allocate_and_fill_with<F: Future<Output = T> + Send + Sync>(
        &self,
        fill: impl 'static + Send + Sync + FnOnce(Id<T>) -> F,
    ) -> Id<T> {
        let items = self.items.clone();
        let index = items.push(KnownItem::new());

        tokio::spawn(async move {
            let value = fill(Id::new(index as u32)).await;
            items[index].set(Arc::new(value));
        });

        Id::new(index as u32)
    }

    pub async fn get(&self, id: Id<T>) -> Option<Arc<T>> {
        let item = &self.items[id.index() as usize];
        if let Some(item) = item.get() {
            Some(item.clone())
        } else {
            None
        }
    }

    pub async fn get_complete(&self, id: Id<T>) -> Arc<T> {
        let item = &self.items[id.index() as usize];
        item.wait_then_get().await.clone()
    }
}
