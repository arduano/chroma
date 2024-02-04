use super::Id;

pub struct Id2<M, T>(Id<M>, Id<T>);

impl<M, T> Id2<M, T> {
    pub fn new(group: Id<M>, item: Id<T>) -> Self {
        Self(group, item)
    }

    pub fn group(&self) -> Id<M> {
        self.0
    }

    pub fn item(&self) -> Id<T> {
        self.1
    }
}

impl<M, T> std::fmt::Debug for Id2<M, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name_m = std::any::type_name::<M>().split("::").last().unwrap();
        let name_t = std::any::type_name::<T>().split("::").last().unwrap();
        write!(
            f,
            "[{}, {}]({},{})",
            name_m,
            name_t,
            self.0.index(),
            self.1.index()
        )
    }
}

impl<M, T> Clone for Id2<M, T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<M, T> Copy for Id2<M, T> {}

impl<M, T> Eq for Id2<M, T> {}

impl<M, T> PartialEq for Id2<M, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl<M, T> Ord for Id2<M, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.0.cmp(&other.0) {
            std::cmp::Ordering::Equal => self.1.cmp(&other.1),
            ord => ord,
        }
    }
}

impl<M, T> PartialOrd for Id2<M, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<M, T> std::hash::Hash for Id2<M, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}
