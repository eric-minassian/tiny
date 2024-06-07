use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

#[derive(Debug, Default, PartialEq)]
pub struct InheritingHashMap<K, V>
where
    K: Eq + Hash,
{
    inner: Rc<RefCell<_InheritingHashMap<K, V>>>,
}

impl<K, V> InheritingHashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn new_inner(map: _InheritingHashMap<K, V>) -> Self {
        Self {
            inner: Rc::new(RefCell::new(map)),
        }
    }

    #[must_use]
    pub fn new() -> Self {
        Self::new_inner(_InheritingHashMap::new())
    }

    #[must_use]
    pub fn with_dominator(dom: &Self) -> Self {
        Self::new_inner(_InheritingHashMap::with_dominator(dom))
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.inner.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<V> {
        self.inner.borrow().get(key)
    }

    pub fn is_empty(&self) -> bool {
        self.inner.borrow().local.is_empty()
    }
}

impl<K, V> Clone for InheritingHashMap<K, V>
where
    K: Eq + Hash,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<K, V> FromIterator<(K, V)> for InheritingHashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self::new_inner(_InheritingHashMap::from_iter(iter))
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct _InheritingHashMap<K, V>
where
    K: Eq + Hash,
{
    dominator: Option<InheritingHashMap<K, V>>,
    local: HashMap<K, V>,
}

impl<K, V> _InheritingHashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            dominator: None,
            local: HashMap::new(),
        }
    }

    #[must_use]
    pub fn with_dominator(dom: &InheritingHashMap<K, V>) -> Self {
        Self {
            dominator: Some(dom.clone()),
            local: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.local.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<V> {
        self.local
            .get(key)
            .cloned()
            .or_else(|| self.dominator.as_ref().and_then(|dom| dom.get(key)))
    }
}

impl<K, V> FromIterator<(K, V)> for _InheritingHashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = Self::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert("key".to_string(), "value".to_string());

        assert_eq!(map_1.get(&"key".to_string()), Some("value".to_string()));

        let mut map_2 = InheritingHashMap::with_dominator(&map_1);
        map_2.insert("hello".to_string(), "world".to_string());

        assert_eq!(map_2.get(&"hello".to_string()), Some("world".to_string()));
        assert_eq!(map_2.get(&"key".to_string()), Some("value".to_string()));
    }

    #[test]
    fn simple_int_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert(1, 2);

        assert_eq!(map_1.get(&1), Some(2));

        let mut map_2 = InheritingHashMap::with_dominator(&map_1);
        map_2.insert(3, 4);

        assert_eq!(map_2.get(&3), Some(4));
        assert_eq!(map_2.get(&1), Some(2));
        assert_eq!(map_1.get(&3), None);
        assert_eq!(map_1.get(&1), Some(2));
    }

    #[test]
    fn overwrite_key_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert("key".to_string(), "value".to_string());

        assert_eq!(map_1.get(&"key".to_string()), Some("value".to_string()));

        let mut map_2 = InheritingHashMap::with_dominator(&map_1);
        map_2.insert("key".to_string(), "new_value".to_string());

        assert_eq!(map_2.get(&"key".to_string()), Some("new_value".to_string()));
        assert_eq!(map_1.get(&"key".to_string()), Some("value".to_string()));
    }

    #[test]
    fn update_dominator_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert("key".to_string(), "value".to_string());

        let mut map_2 = InheritingHashMap::with_dominator(&map_1);
        map_2.insert("key".to_string(), "new_value".to_string());

        map_1.insert("key".to_string(), "newer_value".to_string());
        map_1.insert("hello".to_string(), "world".to_string());

        assert_eq!(
            map_1.get(&"key".to_string()),
            Some("newer_value".to_string())
        );
        assert_eq!(map_2.get(&"key".to_string()), Some("new_value".to_string()));

        assert_eq!(map_1.get(&"hello".to_string()), Some("world".to_string()));
        assert_eq!(map_2.get(&"hello".to_string()), Some("world".to_string()));
    }

    #[test]
    fn multiple_dominator_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert("key".to_string(), "value".to_string());

        let map_2 = InheritingHashMap::with_dominator(&map_1);
        let map_3 = InheritingHashMap::with_dominator(&map_2);

        assert_eq!(map_1.get(&"key".to_string()), Some("value".to_string()));
        assert_eq!(map_2.get(&"key".to_string()), Some("value".to_string()));
        assert_eq!(map_3.get(&"key".to_string()), Some("value".to_string()));
    }

    #[test]
    fn from_iter_inheriting_hashmap() {
        let map = InheritingHashMap::from_iter([
            ("key".to_string(), "value".to_string()),
            ("hello".to_string(), "world".to_string()),
        ]);

        assert_eq!(map.get(&"key".to_string()), Some("value".to_string()));
        assert_eq!(map.get(&"hello".to_string()), Some("world".to_string()));
    }
}
