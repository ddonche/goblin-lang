//! The object store: every object by id, in creation order, with the ids of
//! each class kept alongside so `:objects("Rabbit")` reads them directly.
//!
//! Reads go through `Deref` to the map. Every change that adds or removes an
//! object goes through the methods here, so the class lists stay right.
//! `get_mut` changes an object's fields; an object's class only changes by
//! `insert` or `replace`.

use crate::value::Value;
use indexmap::{IndexMap, IndexSet};
use rustc_hash::{FxBuildHasher, FxHashMap};

type Map = IndexMap<String, Value, FxBuildHasher>;

#[derive(Default, Clone, Debug)]
pub struct ObjectStore {
    map: Map,
    /// Class name -> ids of that class's objects, in creation order.
    by_class: FxHashMap<String, IndexSet<String, FxBuildHasher>>,
}

/// The class an entry is filed under: an object's class. Overlay records
/// (`overlay:` ids) carry their overlay's name and are not filed.
fn class_of<'a>(uuid: &str, v: &'a Value) -> Option<&'a str> {
    match v {
        Value::Object(o) if !uuid.starts_with(crate::vm::OVERLAY_RECORD) => Some(o.class_name.as_str()),
        _ => None,
    }
}

impl std::ops::Deref for ObjectStore {
    type Target = Map;
    fn deref(&self) -> &Map { &self.map }
}

impl<'a> IntoIterator for &'a ObjectStore {
    type Item = (&'a String, &'a Value);
    type IntoIter = indexmap::map::Iter<'a, String, Value>;
    fn into_iter(self) -> Self::IntoIter { self.map.iter() }
}

impl ObjectStore {
    fn file(&mut self, uuid: &str, class: &str) {
        match self.by_class.get_mut(class) {
            Some(set) => { set.insert(uuid.to_string()); }
            None => {
                let mut set = IndexSet::default();
                set.insert(uuid.to_string());
                self.by_class.insert(class.to_string(), set);
            }
        }
    }

    fn unfile(&mut self, uuid: &str, class: &str) {
        if let Some(set) = self.by_class.get_mut(class) {
            set.shift_remove(uuid);
            if set.is_empty() { self.by_class.remove(class); }
        }
    }

    /// Add an object, or replace the one with this id (keeping its place).
    pub fn insert(&mut self, uuid: String, v: Value) -> Option<Value> {
        let new_class = class_of(&uuid, &v).map(str::to_string);
        let old = self.map.insert(uuid.clone(), v);
        let old_class = old.as_ref().and_then(|o| class_of(&uuid, o));
        if old_class != new_class.as_deref() {
            if let Some(c) = old_class.map(str::to_string) { self.unfile(&uuid, &c); }
            if let Some(c) = &new_class { self.file(&uuid, c); }
        }
        old
    }

    /// Put `v` in place of the object with this id (`overwrite!`).
    pub fn replace(&mut self, uuid: &str, v: Value) {
        if self.map.contains_key(uuid) { self.insert(uuid.to_string(), v); }
    }

    pub fn shift_remove(&mut self, uuid: &str) -> Option<Value> {
        let old = self.map.shift_remove(uuid)?;
        if let Some(c) = class_of(uuid, &old).map(str::to_string) { self.unfile(uuid, &c); }
        Some(old)
    }

    /// Keep only the objects `keep` says yes to.
    pub fn retain(&mut self, mut keep: impl FnMut(&String, &mut Value) -> bool) {
        let before = self.map.len();
        self.map.retain(|k, v| keep(k, v));
        if self.map.len() == before { return; }
        // Once per class list, not once per object (a region drops
        // thousands of copies a tick), and without copying the ids.
        let map = &self.map;
        for set in self.by_class.values_mut() {
            set.retain(|u| map.contains_key(u.as_str()));
        }
        self.by_class.retain(|_, set| !set.is_empty());
    }

    /// An object to change in place. Its fields may change; its class must
    /// not (use `replace`).
    pub fn get_mut(&mut self, uuid: &str) -> Option<&mut Value> { self.map.get_mut(uuid) }

    /// The ids of a class's objects, in creation order.
    pub fn of_class(&self, class: &str) -> impl Iterator<Item = &str> {
        self.by_class.get(class).into_iter().flat_map(|s| s.iter().map(String::as_str))
    }

    /// Where the objects of the classes `wanted` says yes to sit in the
    /// store, in creation order; looks only at those classes' lists.
    pub fn places_of(&self, mut wanted: impl FnMut(&str) -> bool) -> Vec<usize> {
        let mut at: Vec<usize> = Vec::new();
        for (class, set) in &self.by_class {
            if wanted(class) { at.extend(set.iter().filter_map(|u| self.map.get_index_of(u.as_str()))); }
        }
        at.sort_unstable();
        at
    }

    /// How many objects a class has.
    pub fn count_of(&self, class: &str) -> usize {
        self.by_class.get(class).map_or(0, |s| s.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::ObjectVal;

    fn obj(class: &str, uuid: &str) -> Value {
        Value::Object(Box::new(ObjectVal {
            class_name: class.into(),
            fields: Default::default(),
            readonly_fields: Default::default(),
            trait_fields: Default::default(),
            uuid: uuid.into(),
        }))
    }

    #[test]
    fn classes_follow_inserts_removals_and_replacements() {
        let mut s = ObjectStore::default();
        for (c, u) in [("A", "1"), ("B", "2"), ("A", "3"), ("A", "4")] { s.insert(u.into(), obj(c, u)); }
        assert_eq!(s.of_class("A").collect::<Vec<_>>(), ["1", "3", "4"]);
        s.shift_remove("3");
        assert_eq!(s.of_class("A").collect::<Vec<_>>(), ["1", "4"]);
        s.replace("1", obj("B", "1"));
        assert_eq!(s.of_class("A").collect::<Vec<_>>(), ["4"]);
        assert_eq!(s.count_of("B"), 2);
        s.retain(|k, _| k != "2" && k != "4");
        assert_eq!(s.count_of("A"), 0);
        assert_eq!(s.of_class("B").collect::<Vec<_>>(), ["1"]);
        s.insert("overlay:1:7".into(), obj("Sick", "overlay:1:7"));
        assert_eq!(s.count_of("Sick"), 0);
    }
}
