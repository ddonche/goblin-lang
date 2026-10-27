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
    /// Ownership: each object whose `owner` field holds another object, by
    /// that owner, and the other way round. Kept by every change that goes
    /// through the methods here; a field write calls `refile_owner`.
    /// `retain` leaves the entries of the objects it drops (a region drops
    /// its visitors' copies every tick and puts them back the next), so
    /// readers skip ids that are not in `map`; they are cleared once there
    /// are twice as many as the most objects the store has held (`peak`).
    contents: FxHashMap<String, IndexSet<String, FxBuildHasher>>,
    owner_of: FxHashMap<String, String>,
    peak: usize,
    /// Equality indexes, made the first time something looks objects up by
    /// a field's value (`target >> meadow == self >> meadow`): by (class,
    /// field). Brought up to date with the objects changed since (`dirty`)
    /// when next used.
    indexes: FxHashMap<(String, String), FieldIndex>,
    dirty: Vec<String>,
    /// Too many changes to track one by one: the indexes are made again.
    stale: bool,
}

/// A field's value as far as `==` can tell values apart: values that are
/// equal get the same key (numbers of any kind by their f64 value, which may
/// also give some unequal ones the same key; that only costs a check).
/// Objects are equal when they are the same object, so they key by id.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FieldKey { Num(u64), Str(String), Bool(bool), Nil, Obj(String) }

pub fn field_key(v: &Value) -> Option<FieldKey> {
    let num = |f: f64| FieldKey::Num(if f == 0.0 { 0 } else { f.to_bits() });
    Some(match v {
        Value::Int(i) => num(*i as f64),
        Value::Float(f) => num(*f),
        Value::Pct(p) => num(*p),
        Value::Str(s) => FieldKey::Str(s.to_string()),
        Value::Bool(b) => FieldKey::Bool(*b),
        Value::Nil => FieldKey::Nil,
        Value::Object(o) => FieldKey::Obj(o.uuid.to_string()),
        Value::Ref(u) => FieldKey::Obj(u.to_string()),
        _ => return None,
    })
}

/// Where an object is filed in one index.
#[derive(Clone, Debug, PartialEq)]
enum Filed { Key(FieldKey), Other, Missing }

/// The objects of one class by one field's value.
#[derive(Default, Clone, Debug)]
struct FieldIndex {
    by: FxHashMap<FieldKey, IndexSet<String, FxBuildHasher>>,
    /// Objects whose value has no key (a list, an object): always candidates.
    other: IndexSet<String, FxBuildHasher>,
    filed: FxHashMap<String, Filed>,
}

impl FieldIndex {
    fn unfile(&mut self, uuid: &str) {
        match self.filed.remove(uuid) {
            Some(Filed::Key(k)) => {
                if let Some(set) = self.by.get_mut(&k) {
                    set.shift_remove(uuid);
                    if set.is_empty() { self.by.remove(&k); }
                }
            }
            Some(Filed::Other) => { self.other.shift_remove(uuid); }
            _ => {}
        }
    }

    fn file(&mut self, uuid: &str, at: Filed) {
        match &at {
            Filed::Key(k) => { self.by.entry(k.clone()).or_default().insert(uuid.to_string()); }
            Filed::Other => { self.other.insert(uuid.to_string()); }
            Filed::Missing => {}
        }
        self.filed.insert(uuid.to_string(), at);
    }
}

/// Where `v` goes in the index of `field` (None: not an object of `class`).
fn filed_as(v: Option<&Value>, class: &str, field: &str) -> Option<Filed> {
    let Some(Value::Object(o)) = v else { return None };
    if o.class_name != class { return None; }
    Some(match o.fields.get(field) {
        None => Filed::Missing,
        Some(f) => match field_key(f) { Some(k) => Filed::Key(k), None => Filed::Other },
    })
}

/// The field that says what an object belongs to (what contains it).
pub const OWNER_FIELD: &str = "owner";

/// The id of the object an `owner` field names, if it names one.
fn owner_in(v: &Value) -> Option<&str> {
    let Value::Object(o) = v else { return None };
    owner_in_value(o.fields.get(OWNER_FIELD)?)
}

/// The id of the object a field value names, if it names one.
fn owner_in_value(f: &Value) -> Option<&str> {
    match f {
        Value::Ref(u) if !u.starts_with(crate::vm::OVERLAY_RECORD) => Some(u.as_str()),
        Value::Object(x) => Some(x.uuid.as_str()),
        _ => None,
    }
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

    /// Files `uuid` under the owner its `owner` field names now.
    fn set_owner(&mut self, uuid: &str, owner: Option<&str>) {
        if self.owner_of.get(uuid).map(String::as_str) == owner { return; }
        if let Some(old) = self.owner_of.remove(uuid) {
            if let Some(set) = self.contents.get_mut(&old) {
                set.shift_remove(uuid);
                if set.is_empty() { self.contents.remove(&old); }
            }
        }
        if let Some(o) = owner {
            match self.contents.get_mut(o) {
                Some(set) => { set.insert(uuid.to_string()); }
                None => {
                    let mut set = IndexSet::default();
                    set.insert(uuid.to_string());
                    self.contents.insert(o.to_string(), set);
                }
            }
            self.owner_of.insert(uuid.to_string(), o.to_string());
        }
    }

    /// After a write to an object's fields: files it under the owner its
    /// `owner` field names now.
    pub fn refile_owner(&mut self, uuid: &str) {
        let owner = self.map.get(uuid).and_then(owner_in).map(str::to_string);
        self.set_owner(uuid, owner.as_deref());
    }

    /// The id of the object `uuid`'s `owner` field names, if it names one.
    pub fn holder_of(&self, uuid: &str) -> Option<&str> {
        if !self.map.contains_key(uuid) { return None; }
        self.owner_of.get(uuid).map(String::as_str)
    }

    /// `holder_of` for an object known to be in the store.
    pub fn holder_of_present(&self, uuid: &str) -> Option<&str> {
        self.owner_of.get(uuid).map(String::as_str)
    }

    /// About how many objects `owner` contains (copies dropped by `retain`
    /// may still count).
    pub fn contents_len(&self, owner: &str) -> usize {
        self.contents.get(owner).map_or(0, |s| s.len())
    }

    /// The ids of what `owner` contains (the objects whose `owner` field
    /// names it), in the order they came to it.
    pub fn contents_of(&self, owner: &str) -> impl Iterator<Item = &str> {
        let map = &self.map;
        self.contents.get(owner).into_iter().flat_map(|s| s.iter().map(String::as_str)).filter(move |u| map.contains_key(*u))
    }

    /// Everything `uuid` contains, at any depth (what it holds, what those
    /// hold, ...), in the order they were put there, each once.
    pub fn all_contents_of(&self, uuid: &str) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        seen.insert(uuid.to_string());
        let mut i = 0;
        let mut frontier = vec![uuid.to_string()];
        while i < frontier.len() {
            let here = frontier[i].clone();
            i += 1;
            for c in self.contents_of(&here) {
                if seen.insert(c.to_string()) {
                    out.push(c.to_string());
                    frontier.push(c.to_string());
                }
            }
        }
        out
    }

    /// Before `uuid` is erased: what it contains moves up to what contains
    /// it (its own `owner`, nil when it has none), so nothing is left
    /// belonging to an object that is gone. An `on erase:` action that wants
    /// something else for them runs before this and moves or erases them.
    pub fn move_contents_up(&mut self, uuid: &str) {
        let Some(kids) = self.contents.get(uuid).map(|s| s.iter().cloned().collect::<Vec<_>>()) else { return };
        let up = match self.map.get(uuid) {
            Some(Value::Object(o)) => o.fields.get(OWNER_FIELD).cloned().unwrap_or(Value::Nil),
            _ => Value::Nil,
        };
        let up = if owner_in_value(&up) == Some(uuid) { Value::Nil } else { up };
        for k in kids {
            self.touched(&k);
            if let Some(Value::Object(o)) = self.map.get_mut(&k) {
                std::rc::Rc::make_mut(&mut o.fields).insert(OWNER_FIELD.to_string(), up.clone());
            }
            self.refile_owner(&k);
        }
    }

    /// Add an object, or replace the one with this id (keeping its place).
    pub fn insert(&mut self, uuid: String, v: Value) -> Option<Value> {
        self.touched(&uuid);
        let new_class = class_of(&uuid, &v).map(str::to_string);
        match owner_in(&v) {
            Some(o) => self.set_owner(&uuid, Some(o)),
            None => if self.owner_of.contains_key(&uuid) { self.set_owner(&uuid, None); },
        }
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
        self.touched(uuid);
        let old = self.map.shift_remove(uuid)?;
        if self.owner_of.contains_key(uuid) { self.set_owner(uuid, None); }
        if let Some(c) = class_of(uuid, &old).map(str::to_string) { self.unfile(uuid, &c); }
        Some(old)
    }

    /// Keep only the objects `keep` says yes to.
    pub fn retain(&mut self, mut keep: impl FnMut(&String, &mut Value) -> bool) {
        let before = self.map.len();
        self.peak = self.peak.max(before);
        // `keep` may change the objects it keeps.
        if !self.indexes.is_empty() { self.stale = true; self.dirty.clear(); }
        self.map.retain(|k, v| keep(k, v));
        if self.map.len() == before { return; }
        // Once per class list, not once per object (a region drops
        // thousands of copies a tick), and without copying the ids.
        let map = &self.map;
        for set in self.by_class.values_mut() {
            set.retain(|u| map.contains_key(u.as_str()));
        }
        self.by_class.retain(|_, set| !set.is_empty());
        if self.owner_of.len() > 2 * self.peak + 64 {
            self.owner_of.retain(|u, _| map.contains_key(u.as_str()));
            let owner_of = &self.owner_of;
            for set in self.contents.values_mut() {
                set.retain(|u| owner_of.contains_key(u.as_str()));
            }
            self.contents.retain(|_, set| !set.is_empty());
        }
    }

    /// An object to change in place. Its fields may change; its class must
    /// not (use `replace`).
    pub fn get_mut(&mut self, uuid: &str) -> Option<&mut Value> {
        self.touched(uuid);
        self.map.get_mut(uuid)
    }

    /// `uuid` may change: the indexes look at it again when next used.
    fn touched(&mut self, uuid: &str) {
        if self.indexes.is_empty() || self.stale { return; }
        if self.dirty.len() > self.map.len() + 64 { self.stale = true; self.dirty.clear(); return; }
        self.dirty.push(uuid.to_string());
    }

    /// The places of the objects of `class` whose `field` may equal a value
    /// with this key (and those whose value has no key), in creation order.
    /// Objects without the field are left out.
    /// Below this many objects in a class, checking each one is cheaper
    /// than keeping an index.
    pub const INDEX_FROM: usize = 32;

    pub fn with_field(&mut self, class: &str, field: &str, key: &FieldKey) -> Vec<usize> {
        let id = (class.to_string(), field.to_string());
        if self.stale {
            self.stale = false;
            self.indexes.clear();
        }
        if !self.indexes.contains_key(&id) {
            let mut ix = FieldIndex::default();
            for u in self.of_class(class) {
                if let Some(f) = filed_as(self.map.get(u), class, field) { ix.file(u, f); }
            }
            self.indexes.insert(id.clone(), ix);
        }
        if !self.dirty.is_empty() {
            let dirty = std::mem::take(&mut self.dirty);
            for ((c, f), ix) in self.indexes.iter_mut() {
                for u in &dirty {
                    let now = filed_as(self.map.get(u.as_str()), c, f);
                    if ix.filed.get(u.as_str()) == now.as_ref() { continue; }
                    ix.unfile(u);
                    if let Some(now) = now { ix.file(u, now); }
                }
            }
        }
        let ix = &self.indexes[&id];
        let mut at: Vec<usize> = ix.by.get(key).into_iter().flatten().chain(ix.other.iter())
            .filter_map(|u| self.map.get_index_of(u.as_str()))
            .collect();
        at.sort_unstable();
        at
    }

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

    fn owned(class: &str, uuid: &str, owner: Option<&str>) -> Value {
        let mut v = obj(class, uuid);
        if let (Value::Object(o), Some(w)) = (&mut v, owner) {
            std::rc::Rc::make_mut(&mut o.fields).insert(OWNER_FIELD.into(), Value::Ref(w.into()));
        }
        v
    }

    #[test]
    fn contents_follow_writes_removals_and_erases() {
        let mut s = ObjectStore::default();
        s.insert("hero".into(), owned("P", "hero", None));
        s.insert("bag".into(), owned("B", "bag", Some("hero")));
        s.insert("a".into(), owned("I", "a", Some("bag")));
        s.insert("b".into(), owned("I", "b", Some("bag")));
        assert_eq!(s.contents_of("bag").collect::<Vec<_>>(), ["a", "b"]);
        // A field write, then refile.
        if let Some(Value::Object(o)) = s.get_mut("a") {
            std::rc::Rc::make_mut(&mut o.fields).insert(OWNER_FIELD.into(), Value::Ref("hero".into()));
        }
        s.refile_owner("a");
        assert_eq!(s.contents_of("bag").collect::<Vec<_>>(), ["b"]);
        assert_eq!(s.contents_of("hero").collect::<Vec<_>>(), ["bag", "a"]);
        // Erasing the bag: b moves up to the hero.
        s.move_contents_up("bag");
        s.shift_remove("bag");
        assert_eq!(s.contents_of("hero").collect::<Vec<_>>(), ["a", "b"]);
        assert_eq!(s.contents_of("bag").count(), 0);
        s.retain(|k, _| k != "a");
        assert_eq!(s.contents_of("hero").collect::<Vec<_>>(), ["b"]);
        // Erasing the top holder leaves its contents with nil.
        s.move_contents_up("hero");
        s.shift_remove("hero");
        assert_eq!(s.contents_of("hero").count(), 0);
        assert!(matches!(s.get("b"), Some(Value::Object(o)) if matches!(o.fields.get(OWNER_FIELD), Some(Value::Nil))));
    }

    #[test]
    fn contents_skip_dropped_copies_and_take_them_back() {
        // A region drops its visitors' copies after a tick and puts them
        // back the next: the lists don't show them meanwhile, and keep
        // their places when they return.
        let mut s = ObjectStore::default();
        s.insert("town".into(), owned("T", "town", None));
        for u in ["a", "b", "c"] { s.insert(u.into(), owned("P", u, Some("town"))); }
        s.retain(|k, _| k != "a" && k != "b");
        assert_eq!(s.contents_of("town").collect::<Vec<_>>(), ["c"]);
        assert_eq!(s.holder_of("a"), None);
        assert_eq!(s.all_contents_of("town"), ["c"]);
        s.insert("b".into(), owned("P", "b", Some("town")));
        s.insert("a".into(), owned("P", "a", Some("town")));
        assert_eq!(s.contents_of("town").collect::<Vec<_>>(), ["a", "b", "c"]);
        // Back with another owner, or none.
        s.retain(|k, _| k != "a" && k != "b");
        s.insert("a".into(), owned("P", "a", None));
        s.insert("b".into(), owned("P", "b", Some("c")));
        assert_eq!(s.contents_of("town").collect::<Vec<_>>(), ["c"]);
        assert_eq!(s.contents_of("c").collect::<Vec<_>>(), ["b"]);
        assert_eq!(s.holder_of("a"), None);
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
