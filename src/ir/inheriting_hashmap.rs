use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq)]
pub(crate) struct InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    dominator: Option<Rc<RefCell<HashMap<K, V>>>>,
    local: Rc<RefCell<HashMap<K, V>>>,
}

impl<K, V> InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        Self {
            dominator: None,
            local: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn with_dominator(map: &InheritingHashMap<K, V>) -> Self {
        Self {
            dominator: map.dominator.clone(),
            local: map.local.clone(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.local.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<V> {
        if let Some(value) = self.local.borrow().get(key) {
            return Some(value.clone());
        }

        if let Some(dominator) = &self.dominator {
            if let Some(value) = dominator.borrow().get(key) {
                return Some(value.clone());
            }
        }

        None
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
    fn overwrite_key_inheriting_hashmap() {
        let mut map_1 = InheritingHashMap::new();
        map_1.insert("key".to_string(), "value".to_string());

        assert_eq!(map_1.get(&"key".to_string()), Some("value".to_string()));

        let mut map_2 = InheritingHashMap::with_dominator(&map_1);
        map_2.insert("key".to_string(), "new_value".to_string());

        assert_eq!(map_2.get(&"key".to_string()), Some("new_value".to_string()));
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
        assert_eq!(
            map_2.get(&"key".to_string()),
            Some("newer_value".to_string())
        );

        assert_eq!(map_1.get(&"hello".to_string()), Some("world".to_string()));
        assert_eq!(map_2.get(&"hello".to_string()), Some("world".to_string()));
    }
}
