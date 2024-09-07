#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OrderedSet<K: PartialEq, V> {
    map: Vec<(K, V)>,
}

impl<K: PartialEq, V> OrderedSet<K, V> {
    pub fn new() -> Self {
        Self { map: Vec::new() }
    }

    pub fn contains(&self, key: &K) -> bool {
        self.map.iter().any(|(k, _)| k == key)
    }

    pub fn insert(&mut self, key: K, value: V) {
        let index = self.map.iter().position(|(k, _)| k == &key);
        if let Some(index) = index {
            self.map[index] = (key, value);
        } else {
            self.map.push((key, value));
        }
    }

    pub fn iter(&self) -> impl '_ + Iterator<Item = (&K, &V)> {
        self.map.iter().map(|(k, v)| (k, v))
    }

    pub fn keys(&self) -> impl '_ + Iterator<Item = &K> {
        self.map.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl '_ + Iterator<Item = &V> {
        self.map.iter().map(|(_, v)| v)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.map
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.map
            .iter_mut()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let index = self.map.iter().position(|(k, _)| k == key);
        if let Some(index) = index {
            Some(self.map.remove(index).1)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}
