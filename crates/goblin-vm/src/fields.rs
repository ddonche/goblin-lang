//! An object's fields: their values in a list, and their names in a layout
//! the objects of one class share.
//!
//! A layout is reached from the empty one by adding names in order, and
//! each step is kept, so objects whose fields were made in the same order
//! share one layout (one `Rc`). A copy of an object copies its values only.
//! A field read can then remember where it found a field for a layout and
//! go straight there next time (see `Vm::get_plain_field`).

use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::value::Value;

/// Field names in order. Shared by every field list made the same way.
pub struct Layout {
    names: Vec<String>,
    /// For larger layouts: where each name is.
    index: Option<FxHashMap<String, u32>>,
    /// The layouts one more name leads to.
    next: RefCell<FxHashMap<String, Rc<Layout>>>,
}

/// Above this many names a layout finds a name by hashing, not by looking
/// through the list.
const SCAN: usize = 8;

thread_local! {
    static EMPTY: Rc<Layout> = Rc::new(Layout { names: Vec::new(), index: None, next: RefCell::new(FxHashMap::default()) });
}

impl Layout {
    #[inline]
    fn position(&self, name: &str) -> Option<usize> {
        match &self.index {
            Some(ix) => ix.get(name).map(|i| *i as usize),
            // The first byte first: most names differ there, and it saves
            // a call to compare the rest.
            None => self.names.iter().position(|n| n.as_bytes().first() == name.as_bytes().first() && n == name),
        }
    }

    /// The layout with `name` added at the end.
    fn with(self: &Rc<Layout>, name: &str) -> Rc<Layout> {
        if let Some(l) = self.next.borrow().get(name) { return l.clone(); }
        let mut names = self.names.clone();
        names.push(name.to_string());
        let index = (names.len() > SCAN).then(|| names.iter().enumerate().map(|(i, n)| (n.clone(), i as u32)).collect());
        let l = Rc::new(Layout { names, index, next: RefCell::new(FxHashMap::default()) });
        self.next.borrow_mut().insert(name.to_string(), l.clone());
        l
    }

    /// The layout for these names in this order.
    fn of<'a>(names: impl Iterator<Item = &'a str>) -> Rc<Layout> {
        let mut l = EMPTY.with(|e| e.clone());
        for n in names { l = l.with(n); }
        l
    }
}

/// An object's fields, by name, in the order they were made.
#[derive(Clone)]
pub struct FieldMap {
    layout: Rc<Layout>,
    vals: Vec<Value>,
}

impl Default for FieldMap {
    fn default() -> Self { FieldMap { layout: EMPTY.with(|e| e.clone()), vals: Vec::new() } }
}

impl FieldMap {
    pub fn new() -> Self { Self::default() }

    /// Fields with these names, in this order, holding `vals`. `guess` is
    /// the layout of the last ones built this way (usually an object of the
    /// same class): when the names match it is used as it is, so building
    /// many alike objects walks no layouts.
    pub fn from_names<'a>(names: impl ExactSizeIterator<Item = &'a str> + Clone, vals: Vec<Value>, guess: &mut Option<Rc<Layout>>) -> Self {
        let layout = match guess {
            Some(g) if g.names.len() == names.len() && g.names.iter().zip(names.clone()).all(|(a, b)| a == b) => g.clone(),
            _ => {
                let l = Layout::of(names);
                *guess = Some(l.clone());
                l
            }
        };
        FieldMap { layout, vals }
    }

    /// Which layout this is, for a cache that remembers where a field was.
    #[inline]
    pub fn layout_id(&self) -> usize { Rc::as_ptr(&self.layout) as usize }

    #[inline]
    pub fn len(&self) -> usize { self.vals.len() }
    #[inline]
    pub fn is_empty(&self) -> bool { self.vals.is_empty() }

    #[inline]
    pub fn get_index_of(&self, name: &str) -> Option<usize> { self.layout.position(name) }

    #[inline]
    pub fn get(&self, name: &str) -> Option<&Value> { self.get_index_of(name).map(|i| &self.vals[i]) }

    #[inline]
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        let i = self.get_index_of(name)?;
        Some(&mut self.vals[i])
    }

    pub fn get_full(&self, name: &str) -> Option<(usize, &String, &Value)> {
        let i = self.get_index_of(name)?;
        Some((i, &self.layout.names[i], &self.vals[i]))
    }

    pub fn get_key_value(&self, name: &str) -> Option<(&String, &Value)> {
        let i = self.get_index_of(name)?;
        Some((&self.layout.names[i], &self.vals[i]))
    }

    #[inline]
    pub fn contains_key(&self, name: &str) -> bool { self.get_index_of(name).is_some() }

    #[inline]
    pub fn get_index(&self, i: usize) -> Option<(&String, &Value)> {
        Some((self.layout.names.get(i)?, &self.vals[i]))
    }

    #[inline]
    pub fn get_index_mut(&mut self, i: usize) -> Option<(&String, &mut Value)> {
        let n = self.layout.names.get(i)?;
        Some((n, &mut self.vals[i]))
    }

    /// The value at a place the caller found before for this layout.
    #[inline]
    pub fn value_at(&self, i: usize) -> &Value { &self.vals[i] }

    /// Sets `name`, keeping its place; a new name goes at the end.
    pub fn insert(&mut self, name: String, v: Value) -> Option<Value> {
        match self.get_index_of(&name) {
            Some(i) => Some(std::mem::replace(&mut self.vals[i], v)),
            None => {
                self.layout = self.layout.with(&name);
                self.vals.push(v);
                None
            }
        }
    }

    /// Takes `name` out; the fields after it move up.
    pub fn shift_remove(&mut self, name: &str) -> Option<Value> {
        let i = self.get_index_of(name)?;
        let v = self.vals.remove(i);
        let names = self.layout.names.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, n)| n.as_str());
        self.layout = Layout::of(names);
        Some(v)
    }

    pub fn swap_remove(&mut self, name: &str) -> Option<Value> { self.shift_remove(name) }

    pub fn iter(&self) -> Iter<'_> { Iter { names: self.layout.names.iter(), vals: self.vals.iter() } }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (&String, &mut Value)> + ExactSizeIterator {
        self.layout.names.iter().zip(self.vals.iter_mut())
    }

    pub fn keys(&self) -> std::slice::Iter<'_, String> { self.layout.names.iter() }
    pub fn values(&self) -> std::slice::Iter<'_, Value> { self.vals.iter() }
    pub fn values_mut(&mut self) -> std::slice::IterMut<'_, Value> { self.vals.iter_mut() }

    pub fn first(&self) -> Option<(&String, &Value)> { self.get_index(0) }
    pub fn last(&self) -> Option<(&String, &Value)> { self.len().checked_sub(1).and_then(|i| self.get_index(i)) }

    pub fn clear(&mut self) { *self = Self::default(); }

    /// Keeps the fields `keep` says yes to, in order.
    pub fn retain(&mut self, mut keep: impl FnMut(&String, &mut Value) -> bool) {
        let mut names = Vec::with_capacity(self.vals.len());
        let mut vals = Vec::with_capacity(self.vals.len());
        let mut changed = false;
        for (n, mut v) in self.layout.names.iter().zip(std::mem::take(&mut self.vals)) {
            if keep(n, &mut v) { names.push(n.as_str()); vals.push(v); } else { changed = true; }
        }
        if changed { self.layout = Layout::of(names.into_iter()); }
        self.vals = vals;
    }
}

/// The fields in order, with their names.
#[derive(Clone)]
pub struct Iter<'a> {
    names: std::slice::Iter<'a, String>,
    vals: std::slice::Iter<'a, Value>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a String, &'a Value);
    #[inline]
    fn next(&mut self) -> Option<Self::Item> { Some((self.names.next()?, self.vals.next()?)) }
    fn size_hint(&self) -> (usize, Option<usize>) { self.vals.size_hint() }
}
impl DoubleEndedIterator for Iter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> { Some((self.names.next_back()?, self.vals.next_back()?)) }
}
impl ExactSizeIterator for Iter<'_> {}

impl<'a> IntoIterator for &'a FieldMap {
    type Item = (&'a String, &'a Value);
    type IntoIter = Iter<'a>;
    fn into_iter(self) -> Iter<'a> { self.iter() }
}

impl IntoIterator for FieldMap {
    type Item = (String, Value);
    type IntoIter = std::iter::Zip<std::vec::IntoIter<String>, std::vec::IntoIter<Value>>;
    fn into_iter(self) -> Self::IntoIter { self.layout.names.clone().into_iter().zip(self.vals) }
}

impl FromIterator<(String, Value)> for FieldMap {
    fn from_iter<I: IntoIterator<Item = (String, Value)>>(it: I) -> Self {
        let mut m = FieldMap::default();
        m.extend(it);
        m
    }
}

impl Extend<(String, Value)> for FieldMap {
    fn extend<I: IntoIterator<Item = (String, Value)>>(&mut self, it: I) {
        for (k, v) in it { self.insert(k, v); }
    }
}

/// Equal when they have the same fields with equal values, in any order
/// (as a map compares).
impl PartialEq for FieldMap {
    fn eq(&self, o: &FieldMap) -> bool {
        if self.len() != o.len() { return false; }
        if Rc::ptr_eq(&self.layout, &o.layout) { return self.vals == o.vals; }
        self.iter().all(|(k, v)| o.get(k) == Some(v))
    }
}

impl std::fmt::Debug for FieldMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.debug_map().entries(self.iter()).finish() }
}

impl std::ops::Index<&str> for FieldMap {
    type Output = Value;
    fn index(&self, name: &str) -> &Value { self.get(name).expect("no such field") }
}
