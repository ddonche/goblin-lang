/// Collection CRUD operations for Goblin.
///
/// All operations are immutable: they take a Value (or &CollectionValue) and
/// return a new Value. The caller allocates a new stash for the result.
/// The only exception is overwrite!, which is in session.rs.
///
/// Suffix legend:
///   _first   → operate on element at index 0
///   _last    → operate on element at index len-1
///   _at(i)   → operate at a specific integer index
///   _random  → pick/operate on a random element
///   _where   → operate on elements matching a predicate (handled at VM level)
///   _all     → operate on all elements (scan)
///   _between → operate on elements in an index range
///   _matching → operate on elements matching a string/pattern
use std::rc::Rc;

use crate::error::GoblinError;
use crate::value::{
    BackendHint, ChunkedSeq, CollectionLayout, CollectionMeta, CollectionValue, RingBuf, Value,
};

// ── Internal: flatten any layout to Vec<Value> ────────────────────────────────

pub fn to_vec(coll: &CollectionValue) -> Vec<Value> {
    match &coll.layout {
        CollectionLayout::FlatArray(v)       => v.as_ref().clone(),
        CollectionLayout::RingBuf(rb)        => rb.to_vec(),
        CollectionLayout::ChunkedSeq(cs)     => cs.to_flat(),
        CollectionLayout::SmallMap(pairs)    => pairs.iter().map(|(k, v)| {
            // Represent as [{key: k, value: v}] for sequential access.
            // For now, return values only (consistent with Goblin `values` semantics).
            let _ = k;
            v.clone()
        }).collect(),
        CollectionLayout::HashMapBackend(m)  => m.values().cloned().collect(),
    }
}

pub fn to_pairs(coll: &CollectionValue) -> Vec<(Value, Value)> {
    match &coll.layout {
        CollectionLayout::SmallMap(pairs)   => pairs.as_ref().clone(),
        CollectionLayout::HashMapBackend(m) => m.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
        CollectionLayout::FlatArray(v)      => v.iter().enumerate()
            .map(|(i, val)| (Value::Int(i as i64), val.clone()))
            .collect(),
        CollectionLayout::RingBuf(rb)       => rb.to_vec().into_iter().enumerate()
            .map(|(i, val)| (Value::Int(i as i64), val))
            .collect(),
        CollectionLayout::ChunkedSeq(cs)    => cs.to_flat().into_iter().enumerate()
            .map(|(i, val)| (Value::Int(i as i64), val))
            .collect(),
    }
}

fn is_map(coll: &CollectionValue) -> bool {
    matches!(&coll.layout,
        CollectionLayout::SmallMap(_) | CollectionLayout::HashMapBackend(_))
}

// ── Build a new CollectionValue from a Vec, respecting adaptive hints ─────────

fn build_seq(items: Vec<Value>, meta: CollectionMeta) -> CollectionValue {
    let len = items.len();
    let hint = meta.choose_layout();
    let layout = match hint {
        BackendHint::RingBuf => {
            CollectionLayout::RingBuf(Rc::new(RingBuf::from_vec(items)))
        }
        BackendHint::ChunkedSeq => {
            CollectionLayout::ChunkedSeq(Rc::new(ChunkedSeq::from_flat(&items)))
        }
        _ => CollectionLayout::FlatArray(Rc::new(items)),
    };
    CollectionValue { layout, meta: CollectionMeta { len, ..meta } }
}

fn build_map(pairs: Vec<(Value, Value)>, meta: CollectionMeta) -> CollectionValue {
    let len = pairs.len();
    let layout = if len > 16 {
        use std::collections::HashMap;
        let mut m = HashMap::new();
        for (k, v) in pairs {
            m.insert(k, v);
        }
        CollectionLayout::HashMapBackend(Rc::new(m))
    } else {
        CollectionLayout::SmallMap(Rc::new(pairs))
    };
    CollectionValue { layout, meta: CollectionMeta { len, ..meta } }
}

// ── Index resolution ──────────────────────────────────────────────────────────

fn resolve_seq_index(idx: i64, len: usize) -> Result<usize, GoblinError> {
    let i = if idx < 0 {
        len as i64 + idx
    } else {
        idx
    };
    if i < 0 || i as usize >= len {
        Err(GoblinError::IndexOutOfBounds { index: idx, len })
    } else {
        Ok(i as usize)
    }
}

fn get_seq(items: &[Value], idx: i64) -> Result<Value, GoblinError> {
    let i = resolve_seq_index(idx, items.len())?;
    Ok(items[i].clone())
}

// ── GetIndex / SetIndex (used directly by the VM for [] and {} syntax) ────────

/// Get an element from a collection by key.
/// For sequences: key must be Int.
/// For maps: key can be any Value.
pub fn get_index(coll_val: &Value, key: &Value) -> Result<Value, GoblinError> {
    match coll_val {
        Value::Collection(c) => {
            if is_map(c) {
                let pairs = to_pairs(c);
                pairs.into_iter()
                    .find(|(k, _)| k == key)
                    .map(|(_, v)| v)
                    .ok_or(GoblinError::KeyNotFound)
            } else {
                let idx = match key {
                    Value::Int(n) => *n,
                    _ => return Err(GoblinError::type_error("int", key.type_name(), "index")),
                };
                let items = to_vec(c);
                get_seq(&items, idx)
            }
        }
        Value::Str(s) => {
            let idx = match key {
                Value::Int(n) => *n,
                _ => return Err(GoblinError::type_error("int", key.type_name(), "string index")),
            };
            let chars: Vec<char> = s.chars().collect();
            let i = resolve_seq_index(idx, chars.len())?;
            Ok(Value::Str(chars[i].to_string()))
        }
        _ => Err(GoblinError::type_error("collection or str", coll_val.type_name(), "index")),
    }
}

/// Update a collection at a key, returning a new Value (|! semantics).
pub fn set_index(coll_val: Value, key: &Value, new_val: Value) -> Result<Value, GoblinError> {
    match coll_val {
        Value::Collection(c) => {
            if is_map(&c) {
                let mut pairs = to_pairs(&c);
                let meta = c.meta.clone();
                if let Some(entry) = pairs.iter_mut().find(|(k, _)| k == key) {
                    entry.1 = new_val;
                } else {
                    pairs.push((key.clone(), new_val));
                }
                Ok(Value::Collection(Rc::new(build_map(pairs, meta))))
            } else {
                let idx = match key {
                    Value::Int(n) => *n,
                    _ => return Err(GoblinError::type_error("int", key.type_name(), "index")),
                };
                let mut items = to_vec(&c);
                let i = resolve_seq_index(idx, items.len())?;
                items[i] = new_val;
                let mut meta = c.meta.clone();
                meta.mid_hits += 1;
                Ok(Value::Collection(Rc::new(build_seq(items, meta))))
            }
        }
        _ => Err(GoblinError::type_error("collection", coll_val.type_name(), "set_index")),
    }
}

// ── grab family ──────────────────────────────────────────────────────────────

pub fn grab(coll: &CollectionValue) -> Value {
    Value::Collection(Rc::new(coll.clone()))
}

pub fn grab_first(coll: &CollectionValue) -> Result<Value, GoblinError> {
    if is_map(coll) {
        return Err(GoblinError::Runtime("grab_first not supported on maps".into()));
    }
    let items = to_vec(coll);
    if items.is_empty() { return Ok(Value::Nil); }
    Ok(items[0].clone())
}

pub fn grab_last(coll: &CollectionValue) -> Result<Value, GoblinError> {
    if is_map(coll) {
        return Err(GoblinError::Runtime("grab_last not supported on maps".into()));
    }
    let items = to_vec(coll);
    if items.is_empty() { return Ok(Value::Nil); }
    Ok(items[items.len() - 1].clone())
}

pub fn grab_at(coll: &CollectionValue, idx: i64) -> Result<Value, GoblinError> {
    let items = to_vec(coll);
    get_seq(&items, idx)
}

pub fn grab_between(coll: &CollectionValue, start: i64, end: i64) -> Result<Value, GoblinError> {
    let items = to_vec(coll);
    let len = items.len();
    let s = resolve_seq_index(start, len)?;
    let e = resolve_seq_index(end, len)? + 1;
    let slice = items[s..e].to_vec();
    Ok(Value::Collection(Rc::new(CollectionValue::from_flat(slice))))
}

pub fn grab_all(coll: &CollectionValue) -> Value {
    Value::Collection(Rc::new(coll.clone()))
}

// grab_where, grab_matching — require a predicate (Closure/Function).
// These are dispatched from builtins.rs where the VM can be called back.

// ── put family ────────────────────────────────────────────────────────────────

pub fn put_first(coll: &CollectionValue, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.front_hits += 1;
    items.insert(0, val);
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn put_last(coll: &CollectionValue, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.back_hits += 1;
    items.push(val);
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn put_at(coll: &CollectionValue, idx: i64, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.mid_hits += 1;
    let i = resolve_seq_index(idx, items.len())?;
    items.insert(i, val);
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn put(coll: &CollectionValue, key: Value, val: Value) -> Result<Value, GoblinError> {
    if is_map(coll) {
        let mut pairs = to_pairs(coll);
        let meta = coll.meta.clone();
        pairs.push((key, val));
        Ok(Value::Collection(Rc::new(build_map(pairs, meta))))
    } else {
        put_last(coll, val)
    }
}

// ── update family ─────────────────────────────────────────────────────────────

pub fn update_at(coll: &CollectionValue, idx: i64, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.mid_hits += 1;
    let i = resolve_seq_index(idx, items.len())?;
    items[i] = val;
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn update_first(coll: &CollectionValue, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.front_hits += 1;
    if items.is_empty() { return Err(GoblinError::IndexOutOfBounds { index: 0, len: 0 }); }
    items[0] = val;
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn update_last(coll: &CollectionValue, val: Value) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.back_hits += 1;
    let last = items.len();
    if last == 0 { return Err(GoblinError::IndexOutOfBounds { index: -1, len: 0 }); }
    items[last - 1] = val;
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn update(coll: &CollectionValue, key: &Value, val: Value) -> Result<Value, GoblinError> {
    if is_map(coll) {
        let mut pairs = to_pairs(coll);
        let meta = coll.meta.clone();
        match pairs.iter_mut().find(|(k, _)| k == key) {
            Some(entry) => entry.1 = val,
            None => return Err(GoblinError::KeyNotFound),
        }
        Ok(Value::Collection(Rc::new(build_map(pairs, meta))))
    } else {
        match key {
            Value::Int(i) => update_at(coll, *i, val),
            _ => Err(GoblinError::type_error("int or map", key.type_name(), "update")),
        }
    }
}

// ── delete family ─────────────────────────────────────────────────────────────

pub fn delete_at(coll: &CollectionValue, idx: i64) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.mid_hits += 1;
    let i = resolve_seq_index(idx, items.len())?;
    items.remove(i);
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn delete_first(coll: &CollectionValue) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.front_hits += 1;
    if items.is_empty() { return Ok(Value::Collection(Rc::new(coll.clone()))); }
    items.remove(0);
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn delete_last(coll: &CollectionValue) -> Result<Value, GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.back_hits += 1;
    if items.is_empty() { return Ok(Value::Collection(Rc::new(coll.clone()))); }
    items.pop();
    meta.len = items.len();
    Ok(Value::Collection(Rc::new(build_seq(items, meta))))
}

pub fn delete(coll: &CollectionValue, key: &Value) -> Result<Value, GoblinError> {
    if is_map(coll) {
        let mut pairs = to_pairs(coll);
        let meta = coll.meta.clone();
        pairs.retain(|(k, _)| k != key);
        Ok(Value::Collection(Rc::new(build_map(pairs, meta))))
    } else {
        match key {
            Value::Int(i) => delete_at(coll, *i),
            _ => Err(GoblinError::type_error("int or map key", key.type_name(), "delete")),
        }
    }
}

// ── reap family (remove + return the element) ─────────────────────────────────

pub fn reap_first(coll: &CollectionValue) -> Result<(Value, Value), GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.front_hits += 1;
    if items.is_empty() {
        return Ok((Value::Nil, Value::Collection(Rc::new(coll.clone()))));
    }
    let elem = items.remove(0);
    meta.len = items.len();
    Ok((elem, Value::Collection(Rc::new(build_seq(items, meta)))))
}

pub fn reap_last(coll: &CollectionValue) -> Result<(Value, Value), GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.back_hits += 1;
    if items.is_empty() {
        return Ok((Value::Nil, Value::Collection(Rc::new(coll.clone()))));
    }
    let elem = items.pop().unwrap();
    meta.len = items.len();
    Ok((elem, Value::Collection(Rc::new(build_seq(items, meta)))))
}

pub fn reap_at(coll: &CollectionValue, idx: i64) -> Result<(Value, Value), GoblinError> {
    let mut items = to_vec(coll);
    let mut meta = coll.meta.clone();
    meta.mid_hits += 1;
    let i = resolve_seq_index(idx, items.len())?;
    let elem = items.remove(i);
    meta.len = items.len();
    Ok((elem, Value::Collection(Rc::new(build_seq(items, meta)))))
}

pub fn reap(coll: &CollectionValue, key: &Value) -> Result<(Value, Value), GoblinError> {
    if is_map(coll) {
        let mut pairs = to_pairs(coll);
        let meta = coll.meta.clone();
        if let Some(pos) = pairs.iter().position(|(k, _)| k == key) {
            let (_, v) = pairs.remove(pos);
            Ok((v, Value::Collection(Rc::new(build_map(pairs, meta)))))
        } else {
            Ok((Value::Nil, Value::Collection(Rc::new(coll.clone()))))
        }
    } else {
        match key {
            Value::Int(i) => reap_at(coll, *i),
            _ => Err(GoblinError::type_error("int or map key", key.type_name(), "reap")),
        }
    }
}

// reap_random, reap_where, reap_all — require random/predicate access;
// dispatched from builtins.rs.

// ── Query operations ──────────────────────────────────────────────────────────

pub fn has(coll: &CollectionValue, key: &Value) -> bool {
    if is_map(coll) {
        to_pairs(coll).iter().any(|(k, _)| k == key)
    } else {
        match key {
            Value::Int(i) => {
                let len = coll.meta.len as i64;
                *i >= -len && *i < len
            }
            _ => false,
        }
    }
}

pub fn count(coll: &CollectionValue) -> usize {
    coll.meta.len
}

pub fn keys(coll: &CollectionValue) -> Vec<Value> {
    if is_map(coll) {
        to_pairs(coll).into_iter().map(|(k, _)| k).collect()
    } else {
        (0..coll.meta.len).map(|i| Value::Int(i as i64)).collect()
    }
}

pub fn values(coll: &CollectionValue) -> Vec<Value> {
    to_vec(coll)
}

pub fn pairs_vec(coll: &CollectionValue) -> Vec<Value> {
    to_pairs(coll).into_iter().map(|(k, v)| {
        let pair = vec![(Value::Str("key".into()), k), (Value::Str("value".into()), v)];
        Value::Collection(Rc::new(CollectionValue::from_map(pair)))
    }).collect()
}

pub fn reverse(coll: &CollectionValue) -> Value {
    let mut items = to_vec(coll);
    items.reverse();
    let meta = CollectionMeta { len: items.len(), ..Default::default() };
    Value::Collection(Rc::new(build_seq(items, meta)))
}

pub fn sort_values(coll: &CollectionValue) -> Value {
    let mut items = to_vec(coll);
    items.sort_by(|a, b| compare_for_sort(a, b));
    let meta = CollectionMeta { len: items.len(), ..Default::default() };
    Value::Collection(Rc::new(build_seq(items, meta)))
}

fn compare_for_sort(a: &Value, b: &Value) -> std::cmp::Ordering {
    match (a, b) {
        (Value::Int(x), Value::Int(y))     => x.cmp(y),
        (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
        (Value::Str(x), Value::Str(y))     => x.cmp(y),
        _ => std::cmp::Ordering::Equal,
    }
}

pub fn unique(coll: &CollectionValue) -> Value {
    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();
    for v in to_vec(coll) {
        // Use debug repr as a cheap hash key for Value (no proper Hash without wrapper).
        let key = format!("{:?}", v);
        if seen.insert(key) {
            items.push(v);
        }
    }
    let meta = CollectionMeta { len: items.len(), ..Default::default() };
    Value::Collection(Rc::new(build_seq(items, meta)))
}

pub fn flatten(coll: &CollectionValue) -> Value {
    let mut result = Vec::new();
    for v in to_vec(coll) {
        match v {
            Value::Collection(c) => result.extend(to_vec(&c)),
            other => result.push(other),
        }
    }
    let meta = CollectionMeta { len: result.len(), ..Default::default() };
    Value::Collection(Rc::new(build_seq(result, meta)))
}

pub fn zip_collections(a: &CollectionValue, b: &CollectionValue) -> Value {
    let av = to_vec(a);
    let bv = to_vec(b);
    let pairs: Vec<Value> = av.into_iter().zip(bv.into_iter()).map(|(x, y)| {
        Value::Collection(Rc::new(CollectionValue::from_flat(vec![x, y])))
    }).collect();
    let meta = CollectionMeta { len: pairs.len(), ..Default::default() };
    Value::Collection(Rc::new(build_seq(pairs, meta)))
}

pub fn slice_collection(coll: &CollectionValue, start: i64, end: i64) -> Result<Value, GoblinError> {
    grab_between(coll, start, end)
}
