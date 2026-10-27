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

// ── Overlays ─────────────────────────────────────────────────────────────────

use crate::session::OverlayInstance;

/// Every overlay on an object, in the order they were put on, with where
/// each host's overlays and each overlay name's instances sit, so a lookup
/// by host or by name reads its own list instead of every overlay.
///
/// Reads and field changes (strength, age, ...) go through `Deref` /
/// `DerefMut` to the slice; adding, removing and renaming go through the
/// methods here. An overlay's host never changes.
#[derive(Default, Clone, Debug)]
pub struct OverlayList {
    items: Vec<OverlayInstance>,
    by_host: FxHashMap<String, Vec<usize>>,
    by_name: FxHashMap<String, Vec<usize>>,
}

impl std::ops::Deref for OverlayList {
    type Target = [OverlayInstance];
    fn deref(&self) -> &[OverlayInstance] { &self.items }
}

impl std::ops::DerefMut for OverlayList {
    fn deref_mut(&mut self) -> &mut [OverlayInstance] { &mut self.items }
}

impl<'a> IntoIterator for &'a OverlayList {
    type Item = &'a OverlayInstance;
    type IntoIter = std::slice::Iter<'a, OverlayInstance>;
    fn into_iter(self) -> Self::IntoIter { self.items.iter() }
}

fn list_push(map: &mut FxHashMap<String, Vec<usize>>, key: &str, at: usize) {
    match map.get_mut(key) {
        Some(v) => v.push(at),
        None => { map.insert(key.to_string(), vec![at]); }
    }
}

fn list_drop(map: &mut FxHashMap<String, Vec<usize>>, key: &str, at: usize) {
    if let Some(v) = map.get_mut(key) {
        v.retain(|&p| p != at);
        if v.is_empty() { map.remove(key); }
    }
}

/// Moves every place in the lists to where `to` says it went now
/// (`usize::MAX`: removed), keeping each list in order.
fn list_remap(map: &mut FxHashMap<String, Vec<usize>>, to: &[usize]) {
    for v in map.values_mut() {
        v.retain_mut(|p| { *p = to[*p]; *p != usize::MAX });
    }
    map.retain(|_, v| !v.is_empty());
}

impl OverlayList {
    pub fn push(&mut self, inst: OverlayInstance) {
        let at = self.items.len();
        list_push(&mut self.by_host, &inst.host_uuid, at);
        list_push(&mut self.by_name, &inst.overlay_name, at);
        self.items.push(inst);
    }

    pub fn remove(&mut self, at: usize) -> OverlayInstance {
        let inst = self.items.remove(at);
        let to: Vec<usize> = (0..=self.items.len())
            .map(|p| if p == at { usize::MAX } else if p > at { p - 1 } else { p })
            .collect();
        list_remap(&mut self.by_host, &to);
        list_remap(&mut self.by_name, &to);
        inst
    }

    /// Take out the overlays at `places` (ascending, no repeats) in one go,
    /// and give them back in that order.
    pub fn take_at(&mut self, places: &[usize]) -> Vec<OverlayInstance> {
        if places.is_empty() { return Vec::new(); }
        let mut gone = vec![false; self.items.len()];
        for &p in places { if p < gone.len() { gone[p] = true; } }
        let mut to = Vec::with_capacity(self.items.len());
        let mut next = 0;
        for &g in &gone {
            if g { to.push(usize::MAX); } else { to.push(next); next += 1; }
        }
        let mut taken = Vec::with_capacity(places.len());
        let mut kept = Vec::with_capacity(next);
        for (inst, g) in std::mem::take(&mut self.items).into_iter().zip(gone) {
            if g { taken.push(inst); } else { kept.push(inst); }
        }
        self.items = kept;
        list_remap(&mut self.by_host, &to);
        list_remap(&mut self.by_name, &to);
        taken
    }

    /// Keep only the overlays `keep` says yes to.
    pub fn retain(&mut self, mut keep: impl FnMut(&OverlayInstance) -> bool) {
        let mut to = Vec::with_capacity(self.items.len());
        let mut next = 0;
        for inst in &self.items {
            if keep(inst) { to.push(next); next += 1; } else { to.push(usize::MAX); }
        }
        if next == self.items.len() { return; }
        let mut i = 0;
        self.items.retain(|_| { let k = to[i] != usize::MAX; i += 1; k });
        list_remap(&mut self.by_host, &to);
        list_remap(&mut self.by_name, &to);
    }

    /// Give the overlay at `at` another name (a transition).
    pub fn rename(&mut self, at: usize, name: String) {
        let old = std::mem::replace(&mut self.items[at].overlay_name, name);
        list_drop(&mut self.by_name, &old, at);
        let name = self.items[at].overlay_name.clone();
        let v = self.by_name.entry(name).or_default();
        let i = v.partition_point(|&p| p < at);
        v.insert(i, at);
    }

    /// Where a host's overlays sit, in the order they were put on.
    pub fn on_host(&self, host: &str) -> &[usize] {
        self.by_host.get(host).map_or(&[], Vec::as_slice)
    }

    /// Where an overlay name's instances sit, in the order they were put on.
    pub fn named(&self, name: &str) -> &[usize] {
        self.by_name.get(name).map_or(&[], Vec::as_slice)
    }

    /// Where the overlay `name` on `host` sits.
    pub fn find(&self, host: &str, name: &str) -> Option<usize> {
        self.on_host(host).iter().copied().find(|&p| self.items[p].overlay_name == name)
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

    fn ov(host: &str, name: &str) -> OverlayInstance {
        OverlayInstance {
            overlay_name: name.into(), host_var: String::new(), host_uuid: host.into(),
            strength: 1.0, age: 0, ticks_remaining: None, count: 1,
            applied: Vec::new(), extra_fields: Default::default(), des_id: goblin_des::index::OverlayInstanceId(0),
        }
    }

    #[test]
    fn overlay_lists_follow_pushes_removals_and_renames() {
        let mut l = OverlayList::default();
        for (h, n) in [("a", "Sick"), ("b", "Sick"), ("a", "Cold"), ("c", "Sick")] { l.push(ov(h, n)); }
        assert_eq!(l.named("Sick"), &[0, 1, 3]);
        assert_eq!(l.find("a", "Cold"), Some(2));
        l.push(ov("d", "Sick"));
        let t = l.take_at(&[1, 4]);
        assert_eq!(t.len(), 2);
        assert_eq!(t[1].host_uuid, "d");
        assert_eq!(l.named("Sick"), &[0, 2]);
        assert_eq!(l.on_host("a"), &[0, 1]);
        l.rename(0, "Cold".into());
        assert_eq!(l.named("Cold"), &[0, 1]);
        assert_eq!(l.named("Sick"), &[2]);
        l.retain(|i| i.host_uuid != "a");
        assert_eq!(l.len(), 1);
        assert_eq!(l.named("Sick"), &[0]);
        assert!(l.named("Cold").is_empty());
        assert_eq!(l.find("c", "Sick"), Some(0));
    }
}
