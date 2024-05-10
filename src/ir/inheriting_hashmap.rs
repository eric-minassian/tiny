use std::{collections::HashMap, fmt};

#[derive(Clone)]
pub struct InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    dominator: Option<*const InheritingHashMap<K, V>>,
    local: HashMap<K, V>,
}

impl<K, V> InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        Self {
            dominator: None,
            local: HashMap::new(),
        }
    }

    pub fn with_dominator(dominator: &InheritingHashMap<K, V>) -> Self {
        Self {
            dominator: Some(dominator),
            local: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.local.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<V> {
        if let Some(value) = self.local.get(key) {
            return Some(value.clone());
        }

        if let Some(dominator) = self.dominator {
            unsafe {
                // Safety: The user must ensure that the dominator lives longer than this map
                return (*dominator).get(key);
            }
        }

        None
    }
}

impl<K, V> InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash + Clone,
    V: Clone,
{
    pub fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = InheritingHashMap::new();

        for (k, v) in iter {
            map.insert(k, v);
        }

        map
    }
}

impl<K, V> PartialEq for InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash + Clone,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        // First check if local maps are equal
        if self.local != other.local {
            return false;
        }

        // Then check dominator equivalence
        match (self.dominator, other.dominator) {
            (Some(self_dom), Some(other_dom)) => {
                unsafe {
                    // Safety: The user must ensure that any dereference is valid and that
                    // the lifetime constraints are respected.
                    // This assumes that the structure pointed to by the dominators are valid.
                    // It recursively checks for equality of pointed-to dominators.
                    (*self_dom).local == (*other_dom).local
                }
            }
            (None, None) => true,
            _ => false,
        }
    }
}

impl<K, V> fmt::Debug for InheritingHashMap<K, V>
where
    K: Eq + std::hash::Hash + fmt::Debug + Clone,
    V: fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InheritingHashMap")
            .field("local", &self.local)
            .field(
                "dominator",
                &self.dominator.map(|dom| unsafe {
                    // Safety: This assumes that the dominator pointer is valid and that
                    // the lifetime of the referenced value outlives this debug print.
                    // To avoid dereferencing invalid memory, ensure dominators are valid.
                    // Also, we are not printing deeply to avoid recursion issues.
                    if !dom.is_null() {
                        // We print the shallow contents to avoid deep recursion
                        format!("{:?} ", (*dom).local)
                    } else {
                        "null".to_string()
                    }
                }),
            )
            .finish()
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
