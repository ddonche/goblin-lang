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

fn compile_regex(pat: &str) -> Result<regex::Regex, GoblinError> {
    regex::Regex::new(pat).map_err(|e| GoblinError::Runtime(format!("invalid regex: {}", e)))
}

// ── Position and Operation enums ──────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Position {
    First,
    Last,
    At(Value),
    Where(String),
    All,
    Random,
    Matching(String),
    Between(String, String),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Get,
    Put(Value),
    Update(Value),
    Delete,
    Reap,
}

// ── Helper: fmt_value_raw ─────────────────────────────────────────────────────

pub fn fmt_value_raw(v: &Value) -> String {
    match v {
        Value::Str(s)   => s.clone(),
        Value::Int(n)   => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Char(c)  => c.to_string(),
        Value::Bool(b)  => b.to_string(),
        Value::Nil      => "nil".into(),
        Value::Big(d)   => d.to_string(),
        Value::Pct(p)   => format!("{}%", p),
        _               => "<complex>".into(),
    }
}

// ── Helper: rng_bounded ───────────────────────────────────────────────────────

pub fn rng_bounded(session: &mut crate::session::Session, bound: usize) -> usize {
    if bound <= 1 { return 0; }
    let bound64 = bound as u64;
    loop {
        let x = (session.next_u128() >> 64) as u64;
        let m = (x as u128).wrapping_mul(bound64 as u128);
        let l = m as u64;
        let t = bound64.wrapping_neg() % bound64;
        if l >= t { return (m >> 64) as usize; }
    }
}

// ── Helper: value_to_map_key ──────────────────────────────────────────────────

pub fn value_to_map_key(v: &Value) -> Option<String> {
    match v {
        Value::Str(s)   => Some(s.clone()),
        Value::Int(n)   => Some(n.to_string()),
        Value::Float(f) => Some(f.to_string()),
        Value::Bool(b)  => Some(b.to_string()),
        Value::Char(c)  => Some(c.to_string()),
        Value::Nil      => Some("nil".into()),
        _               => None,
    }
}

// ── Utility: resolve negative seq index ──────────────────────────────────────

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

// ── Main dispatch ─────────────────────────────────────────────────────────────

pub fn collection_operation(
    coll: &Value,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    match coll {
        Value::Array(xs) => array_op(xs, pos, op, session).map(into_collection),
        Value::Collection(c) => collection_value_op(c, pos, op, session),
        Value::Map(m)    => map_op_btree(m, pos, op, session).map(into_collection),
        Value::MapOrd(m) => map_op_indexed(m, pos, op, session).map(into_collection),
        Value::Str(s)    => str_op(s, pos, op, session),
        other => Err(GoblinError::type_error(
            "array, map, or str",
            other.type_name(),
            "collection_operation",
        )),
    }
}

// ── Array dispatch ────────────────────────────────────────────────────────────

fn array_op(
    xs: &Vec<Value>,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    match (pos, op) {
        // ── Get / Reap ──────────────────────────────────────────────────────
        (Position::First, Operation::Get) | (Position::First, Operation::Reap) => {
            xs.first().cloned().ok_or(GoblinError::IndexOutOfBounds { index: 0, len: 0 })
        }
        (Position::Last, Operation::Get) | (Position::Last, Operation::Reap) => {
            xs.last().cloned().ok_or(GoblinError::IndexOutOfBounds { index: -1, len: 0 })
        }
        (Position::At(idx_val), Operation::Get) | (Position::At(idx_val), Operation::Reap) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            Ok(xs[i].clone())
        }
        (Position::Random, Operation::Get) | (Position::Random, Operation::Reap) => {
            if xs.is_empty() { return Ok(Value::Nil); }
            let i = rng_bounded(session, xs.len());
            Ok(xs[i].clone())
        }
        (Position::All, Operation::Get) | (Position::All, Operation::Reap) => {
            Ok(Value::Array(xs.clone()))
        }
        (Position::Where(pred), Operation::Get) | (Position::Where(pred), Operation::Reap) => {
            let result: Vec<Value> = xs.iter()
                .filter(|v| fmt_value_raw(v) == pred)
                .cloned()
                .collect();
            Ok(Value::Array(result))
        }
        (Position::Matching(pat), Operation::Get) | (Position::Matching(pat), Operation::Reap) => {
            let re = compile_regex(&pat)?;
            let result: Vec<Value> = xs.iter()
                .filter(|v| {
                    match v {
                        Value::Str(s) => re.is_match(s),
                        Value::Char(c) => re.is_match(&c.to_string()),
                        _ => false,
                    }
                })
                .cloned()
                .collect();
            Ok(Value::Array(result))
        }
        (Position::Between(start_pat, end_pat), Operation::Get) |
        (Position::Between(start_pat, end_pat), Operation::Reap) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let matches_start = |v: &Value| match v { Value::Str(s) => start_re.is_match(s), Value::Char(c) => start_re.is_match(&c.to_string()), _ => false };
            let matches_end   = |v: &Value| match v { Value::Str(s) => end_re.is_match(s),   Value::Char(c) => end_re.is_match(&c.to_string()),   _ => false };
            let start_indices: Vec<usize> = xs.iter().enumerate().filter(|(_, v)| matches_start(v)).map(|(i, _)| i).collect();
            let end_indices:   Vec<usize> = xs.iter().enumerate().filter(|(_, v)| matches_end(v)).map(|(i, _)| i).collect();
            let mut result = Vec::new();
            for &si in &start_indices {
                if let Some(&ei) = end_indices.iter().find(|&&e| e > si) {
                    for i in si+1..ei { result.push(xs[i].clone()); }
                }
            }
            Ok(Value::Array(result))
        }

        // ── Put ─────────────────────────────────────────────────────────────
        (Position::First, Operation::Put(v)) => {
            let mut new_xs = vec![v];
            new_xs.extend_from_slice(xs);
            Ok(Value::Array(new_xs))
        }
        (Position::Last, Operation::Put(v)) => {
            let mut new_xs = xs.clone();
            new_xs.push(v);
            Ok(Value::Array(new_xs))
        }
        (Position::At(idx_val), Operation::Put(v)) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            let mut new_xs = xs.clone();
            new_xs.insert(i, v);
            Ok(Value::Array(new_xs))
        }
        (Position::Random, Operation::Put(v)) => {
            let mut new_xs = xs.clone();
            let i = if new_xs.is_empty() { 0 } else { rng_bounded(session, new_xs.len() + 1) };
            new_xs.insert(i, v);
            Ok(Value::Array(new_xs))
        }
        (Position::All, Operation::Put(v)) => {
            match v {
                Value::Array(arr) => Ok(Value::Array(arr)),
                _ => Err(GoblinError::type_error("array", v.type_name(), "put-all on array expects array value")),
            }
        }
        (Position::Where(pred), Operation::Put(v)) => {
            let mut new_xs = Vec::new();
            for elem in xs {
                if fmt_value_raw(elem) == pred {
                    new_xs.push(v.clone());
                }
                new_xs.push(elem.clone());
            }
            Ok(Value::Array(new_xs))
        }
        (Position::Matching(pat), Operation::Put(v)) => {
            let re = compile_regex(&pat)?;
            let elem_matches = |elem: &Value| match elem { Value::Str(s) => re.is_match(s), Value::Char(c) => re.is_match(&c.to_string()), _ => false };
            let mut new_xs = Vec::new();
            for elem in xs {
                new_xs.push(elem.clone());
                if elem_matches(elem) { new_xs.push(v.clone()); }
            }
            Ok(Value::Array(new_xs))
        }
        (Position::Between(start_pat, end_pat), Operation::Put(v)) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let matches_start = |e: &Value| match e { Value::Str(s) => start_re.is_match(s), Value::Char(c) => start_re.is_match(&c.to_string()), _ => false };
            let matches_end   = |e: &Value| match e { Value::Str(s) => end_re.is_match(s),   Value::Char(c) => end_re.is_match(&c.to_string()),   _ => false };
            let start_indices: Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_start(e)).map(|(i, _)| i).collect();
            let end_indices:   Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_end(e)).map(|(i, _)| i).collect();
            let mut ranges = Vec::new();
            for &si in &start_indices {
                if let Some(&ei) = end_indices.iter().find(|&&e| e > si) { ranges.push((si, ei)); }
            }
            let mut result = Vec::with_capacity(xs.len() + ranges.len());
            let mut last_end = 0;
            for (_, ei) in &ranges {
                for i in last_end..=*ei { result.push(xs[i].clone()); }
                result.push(v.clone());
                last_end = ei + 1;
            }
            for i in last_end..xs.len() { result.push(xs[i].clone()); }
            Ok(Value::Array(result))
        }

        // ── Update ──────────────────────────────────────────────────────────
        (Position::First, Operation::Update(v)) => {
            if xs.is_empty() { return Err(GoblinError::IndexOutOfBounds { index: 0, len: 0 }); }
            let mut new_xs = xs.clone();
            new_xs[0] = v;
            Ok(Value::Array(new_xs))
        }
        (Position::Last, Operation::Update(v)) => {
            if xs.is_empty() { return Err(GoblinError::IndexOutOfBounds { index: -1, len: 0 }); }
            let mut new_xs = xs.clone();
            let last = new_xs.len() - 1;
            new_xs[last] = v;
            Ok(Value::Array(new_xs))
        }
        (Position::At(idx_val), Operation::Update(v)) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            let mut new_xs = xs.clone();
            new_xs[i] = v;
            Ok(Value::Array(new_xs))
        }
        (Position::Random, Operation::Update(v)) => {
            if xs.is_empty() { return Ok(Value::Array(xs.clone())); }
            let i = rng_bounded(session, xs.len());
            let mut new_xs = xs.clone();
            new_xs[i] = v;
            Ok(Value::Array(new_xs))
        }
        (Position::All, Operation::Update(v)) => {
            match v {
                Value::Array(arr) => Ok(Value::Array(arr)),
                _ => Err(GoblinError::type_error("array", v.type_name(), "update-all on array expects array value")),
            }
        }
        (Position::Where(pred), Operation::Update(v)) => {
            let new_xs = xs.iter().map(|e| {
                if fmt_value_raw(e) == pred { v.clone() } else { e.clone() }
            }).collect();
            Ok(Value::Array(new_xs))
        }
        (Position::Matching(pat), Operation::Update(v)) => {
            let re = compile_regex(&pat)?;
            let elem_matches = |e: &Value| match e { Value::Str(s) => re.is_match(s), Value::Char(c) => re.is_match(&c.to_string()), _ => false };
            let new_xs = xs.iter().map(|e| if elem_matches(e) { v.clone() } else { e.clone() }).collect();
            Ok(Value::Array(new_xs))
        }
        (Position::Between(start_pat, end_pat), Operation::Update(v)) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let matches_start = |e: &Value| match e { Value::Str(s) => start_re.is_match(s), Value::Char(c) => start_re.is_match(&c.to_string()), _ => false };
            let matches_end   = |e: &Value| match e { Value::Str(s) => end_re.is_match(s),   Value::Char(c) => end_re.is_match(&c.to_string()),   _ => false };
            let start_indices: Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_start(e)).map(|(i, _)| i).collect();
            let end_indices:   Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_end(e)).map(|(i, _)| i).collect();
            let mut result = xs.to_vec();
            for &si in &start_indices {
                if let Some(&ei) = end_indices.iter().find(|&&e| e > si) {
                    for i in si+1..ei { result[i] = v.clone(); }
                }
            }
            Ok(Value::Array(result))
        }

        // ── Delete ──────────────────────────────────────────────────────────
        (Position::First, Operation::Delete) => {
            if xs.is_empty() { return Ok(Value::Array(xs.clone())); }
            Ok(Value::Array(xs[1..].to_vec()))
        }
        (Position::Last, Operation::Delete) => {
            if xs.is_empty() { return Ok(Value::Array(xs.clone())); }
            Ok(Value::Array(xs[..xs.len()-1].to_vec()))
        }
        (Position::At(idx_val), Operation::Delete) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            let mut new_xs = xs.clone();
            new_xs.remove(i);
            Ok(Value::Array(new_xs))
        }
        (Position::Random, Operation::Delete) => {
            if xs.is_empty() { return Ok(Value::Array(xs.clone())); }
            let i = rng_bounded(session, xs.len());
            let mut new_xs = xs.clone();
            new_xs.remove(i);
            Ok(Value::Array(new_xs))
        }
        (Position::All, Operation::Delete) => {
            Ok(Value::Array(Vec::new()))
        }
        (Position::Where(pred), Operation::Delete) => {
            let new_xs = xs.iter()
                .filter(|e| fmt_value_raw(e) != pred)
                .cloned()
                .collect();
            Ok(Value::Array(new_xs))
        }
        (Position::Matching(pat), Operation::Delete) => {
            let re = compile_regex(&pat)?;
            let elem_matches = |e: &Value| match e { Value::Str(s) => re.is_match(s), Value::Char(c) => re.is_match(&c.to_string()), _ => false };
            let new_xs = xs.iter().filter(|e| !elem_matches(e)).cloned().collect();
            Ok(Value::Array(new_xs))
        }
        (Position::Between(start_pat, end_pat), Operation::Delete) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let matches_start = |e: &Value| match e { Value::Str(s) => start_re.is_match(s), Value::Char(c) => start_re.is_match(&c.to_string()), _ => false };
            let matches_end   = |e: &Value| match e { Value::Str(s) => end_re.is_match(s),   Value::Char(c) => end_re.is_match(&c.to_string()),   _ => false };
            let start_indices: Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_start(e)).map(|(i, _)| i).collect();
            let end_indices:   Vec<usize> = xs.iter().enumerate().filter(|(_, e)| matches_end(e)).map(|(i, _)| i).collect();
            let mut exclude = std::collections::HashSet::new();
            for &si in &start_indices {
                if let Some(&ei) = end_indices.iter().find(|&&e| e > si) {
                    for i in si+1..ei { exclude.insert(i); }
                }
            }
            let new_xs = xs.iter().enumerate().filter(|(i, _)| !exclude.contains(i)).map(|(_, e)| e.clone()).collect();
            Ok(Value::Array(new_xs))
        }
    }
}

// ── Map dispatch (BTreeMap) ────────────────────────────────────────────────────

fn map_op_btree(
    m: &std::collections::BTreeMap<String, Value>,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    match (pos, op) {
        // Get
        (Position::First, Operation::Get) | (Position::First, Operation::Reap) => {
            m.iter().next().map(|(_, v)| v.clone())
                .ok_or(GoblinError::IndexOutOfBounds { index: 0, len: 0 })
        }
        (Position::Last, Operation::Get) | (Position::Last, Operation::Reap) => {
            m.iter().last().map(|(_, v)| v.clone())
                .ok_or(GoblinError::IndexOutOfBounds { index: -1, len: 0 })
        }
        (Position::At(key_val), Operation::Get) | (Position::At(key_val), Operation::Reap) => {
            let key = value_to_map_key(&key_val)
                .ok_or_else(|| GoblinError::type_error("string-compatible", key_val.type_name(), "map key"))?;
            m.get(&key).cloned().ok_or(GoblinError::KeyNotFound)
        }
        (Position::Random, Operation::Get) | (Position::Random, Operation::Reap) => {
            if m.is_empty() { return Ok(Value::Nil); }
            let i = rng_bounded(session, m.len());
            Ok(m.values().nth(i).cloned().unwrap_or(Value::Nil))
        }
        (Position::All, Operation::Get) | (Position::All, Operation::Reap) => {
            Ok(Value::Map(m.clone()))
        }
        (Position::Where(pred), Operation::Get) | (Position::Where(pred), Operation::Reap) => {
            let result: Vec<Value> = m.values()
                .filter(|v| fmt_value_raw(v) == pred)
                .cloned()
                .collect();
            Ok(Value::Array(result))
        }
        (Position::Matching(pat), Operation::Get) | (Position::Matching(pat), Operation::Reap) => {
            let re = compile_regex(&pat)?;
            let result: std::collections::BTreeMap<String, Value> = m.iter()
                .filter(|(k, _)| re.is_match(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Ok(Value::Map(result))
        }
        // Put
        (Position::At(key_val), Operation::Put(v)) => {
            let key = value_to_map_key(&key_val)
                .ok_or_else(|| GoblinError::type_error("string-compatible", key_val.type_name(), "map key"))?;
            let mut new_m = m.clone();
            new_m.insert(key, v);
            Ok(Value::Map(new_m))
        }
        (Position::First, Operation::Put(_)) | (Position::Last, Operation::Put(_)) => {
            Err(GoblinError::Runtime("put_first/put_last not meaningful for maps".into()))
        }
        // Update
        (Position::First, Operation::Update(v)) => {
            let first_key = m.iter().next().ok_or(GoblinError::IndexOutOfBounds { index: 0, len: 0 })?.0.clone();
            let mut new_m = m.clone();
            new_m.insert(first_key, v);
            Ok(Value::Map(new_m))
        }
        (Position::At(key_val), Operation::Update(v)) => {
            let key = value_to_map_key(&key_val)
                .ok_or_else(|| GoblinError::type_error("string-compatible", key_val.type_name(), "map key"))?;
            if !m.contains_key(&key) { return Err(GoblinError::KeyNotFound); }
            let mut new_m = m.clone();
            new_m.insert(key, v);
            Ok(Value::Map(new_m))
        }
        (Position::Random, Operation::Update(v)) => {
            if m.is_empty() { return Ok(Value::Map(m.clone())); }
            let i = rng_bounded(session, m.len());
            let key = m.keys().nth(i).cloned().unwrap();
            let mut new_m = m.clone();
            new_m.insert(key, v);
            Ok(Value::Map(new_m))
        }
        (Position::Where(pred), Operation::Update(v)) => {
            let mut new_m = m.clone();
            for (k, val) in m { if fmt_value_raw(val) == pred { new_m.insert(k.clone(), v.clone()); } }
            Ok(Value::Map(new_m))
        }
        (Position::Matching(pat), Operation::Update(v)) => {
            let re = compile_regex(&pat)?;
            let mut new_m = m.clone();
            for k in m.keys() { if re.is_match(k) { new_m.insert(k.clone(), v.clone()); } }
            Ok(Value::Map(new_m))
        }
        // Delete
        (Position::First, Operation::Delete) => {
            if m.is_empty() { return Ok(Value::Map(m.clone())); }
            let first_key = m.iter().next().map(|(k, _)| k.clone()).unwrap();
            let mut new_m = m.clone();
            new_m.remove(&first_key);
            Ok(Value::Map(new_m))
        }
        (Position::Last, Operation::Delete) => {
            if m.is_empty() { return Ok(Value::Map(m.clone())); }
            let last_key = m.iter().last().map(|(k, _)| k.clone()).unwrap();
            let mut new_m = m.clone();
            new_m.remove(&last_key);
            Ok(Value::Map(new_m))
        }
        (Position::At(key_val), Operation::Delete) => {
            let key = value_to_map_key(&key_val)
                .ok_or_else(|| GoblinError::type_error("string-compatible", key_val.type_name(), "map key"))?;
            let mut new_m = m.clone();
            new_m.remove(&key);
            Ok(Value::Map(new_m))
        }
        (Position::All, Operation::Delete) => {
            Ok(Value::Map(std::collections::BTreeMap::new()))
        }
        (Position::Where(pred), Operation::Delete) => {
            let new_m = m.iter()
                .filter(|(_, v)| fmt_value_raw(v) != pred)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Ok(Value::Map(new_m))
        }
        (Position::Matching(pat), Operation::Delete) => {
            let re = compile_regex(&pat)?;
            let new_m = m.iter().filter(|(k, _)| !re.is_match(k)).map(|(k, v)| (k.clone(), v.clone())).collect();
            Ok(Value::Map(new_m))
        }
        // All + Put/Update: replace entire map (v must be a Map)
        (Position::All, Operation::Put(v)) | (Position::All, Operation::Update(v)) => {
            match v {
                Value::Map(m2) => Ok(Value::Map(m2)),
                _ => Err(GoblinError::type_error("map", v.type_name(), "put/update-all on map expects map value")),
            }
        }
        // Random + Put: update a random key's value (same as Random + Update)
        (Position::Random, Operation::Put(v)) => {
            if m.is_empty() { return Ok(Value::Map(m.clone())); }
            let i = rng_bounded(session, m.len());
            let key = m.keys().nth(i).cloned().unwrap();
            let mut new_m = m.clone();
            new_m.insert(key, v);
            Ok(Value::Map(new_m))
        }
        // Last + Put/Update/Delete: not meaningful for maps
        (Position::Last, Operation::Put(_)) | (Position::Last, Operation::Update(_)) | (Position::Last, Operation::Delete) => {
            Err(GoblinError::Runtime("operation not meaningful for maps in 'last' position".into()))
        }
        // Where + Put: not meaningful for maps
        (Position::Where(_), Operation::Put(_)) => {
            Err(GoblinError::Runtime("put_where is not meaningful for maps".into()))
        }
        // Matching + Put: use pattern as literal key if it looks like a plain identifier
        (Position::Matching(pat), Operation::Put(v)) => {
            if pat.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == ' ') {
                let mut new_m = m.clone();
                new_m.insert(pat, v);
                Ok(Value::Map(new_m))
            } else {
                Err(GoblinError::Runtime("put_matching with complex regex is not meaningful for maps".into()))
            }
        }
        // Between: find keys lexicographically between matched start/end keys
        (Position::Between(start_pat, end_pat), op) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let start_keys: Vec<String> = m.keys().filter(|k| start_re.is_match(k)).cloned().collect();
            let end_keys:   Vec<String> = m.keys().filter(|k| end_re.is_match(k)).cloned().collect();
            if start_keys.is_empty() || end_keys.is_empty() {
                return Err(GoblinError::Runtime("start or end pattern did not match any keys".into()));
            }
            let mut between: Vec<String> = Vec::new();
            for sk in &start_keys {
                for ek in &end_keys {
                    for k in m.keys() {
                        if k > sk && k < ek && !start_re.is_match(k) && !end_re.is_match(k) {
                            between.push(k.clone());
                        }
                    }
                }
            }
            between.sort();
            between.dedup();
            if between.is_empty() {
                return Err(GoblinError::Runtime("no keys found between matched patterns".into()));
            }
            match op {
                Operation::Get | Operation::Reap => {
                    let result: std::collections::BTreeMap<String, Value> = between.iter()
                        .map(|k| (k.clone(), m.get(k).unwrap().clone()))
                        .collect();
                    Ok(Value::Map(result))
                }
                Operation::Delete => {
                    let mut new_m = m.clone();
                    for k in &between { new_m.remove(k); }
                    Ok(Value::Map(new_m))
                }
                Operation::Update(v) => {
                    let mut new_m = m.clone();
                    for k in &between { new_m.insert(k.clone(), v.clone()); }
                    Ok(Value::Map(new_m))
                }
                Operation::Put(_) => {
                    Err(GoblinError::Runtime("put_between is not meaningful for maps".into()))
                }
            }
        }
        _ => Err(GoblinError::Runtime("unsupported map operation".into())),
    }
}

// ── Map dispatch (IndexMap) ───────────────────────────────────────────────────

fn map_op_indexed(
    m: &indexmap::IndexMap<String, Value>,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    // Convert to BTreeMap, run op, convert any Map result back to MapOrd.
    // This mirrors the interpreter's MapOrd → Map → result → MapOrd delegation exactly.
    let as_btree: std::collections::BTreeMap<String, Value> = m.iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    let result = map_op_btree(&as_btree, pos, op, session)?;
    Ok(match result {
        Value::Map(m2) => {
            let ord: indexmap::IndexMap<String, Value> = m2.into_iter().collect();
            Value::MapOrd(ord)
        }
        other => other,
    })
}

// ── String dispatch ───────────────────────────────────────────────────────────

fn str_op(
    s: &String,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    let chars: Vec<char> = s.chars().collect();
    match (pos, op) {
        // Get
        (Position::First, Operation::Get) | (Position::First, Operation::Reap) => {
            chars.first().map(|c| Value::Char(*c))
                .ok_or(GoblinError::IndexOutOfBounds { index: 0, len: 0 })
        }
        (Position::Last, Operation::Get) | (Position::Last, Operation::Reap) => {
            chars.last().map(|c| Value::Char(*c))
                .ok_or(GoblinError::IndexOutOfBounds { index: -1, len: 0 })
        }
        (Position::At(idx_val), Operation::Get) | (Position::At(idx_val), Operation::Reap) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "string index")),
            };
            let i = resolve_seq_index(idx, chars.len())?;
            Ok(Value::Char(chars[i]))
        }
        (Position::Random, Operation::Get) | (Position::Random, Operation::Reap) => {
            if chars.is_empty() { return Ok(Value::Nil); }
            let i = rng_bounded(session, chars.len());
            Ok(Value::Char(chars[i]))
        }
        (Position::All, Operation::Get) | (Position::All, Operation::Reap) => {
            Ok(Value::Str(s.clone()))
        }
        (Position::Where(needle), Operation::Get) | (Position::Where(needle), Operation::Reap) => {
            let result: Vec<Value> = chars.iter()
                .filter(|&&c| c.to_string() == needle)
                .map(|&c| Value::Char(c))
                .collect();
            Ok(Value::Array(result))
        }
        (Position::Matching(pat), Operation::Get) | (Position::Matching(pat), Operation::Reap) => {
            let re = compile_regex(&pat)?;
            let result: Vec<Value> = re.find_iter(s).map(|m| Value::Str(m.as_str().to_string())).collect();
            Ok(Value::Array(result))
        }
        (Position::Between(start_pat, end_pat), Operation::Get) |
        (Position::Between(start_pat, end_pat), Operation::Reap) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let start_matches: Vec<_> = start_re.find_iter(s).collect();
            let end_matches:   Vec<_> = end_re.find_iter(s).collect();
            let mut results = Vec::new();
            for sm in &start_matches {
                if let Some(em) = end_matches.iter().find(|e| e.start() > sm.end()) {
                    results.push(Value::Str(s[sm.end()..em.start()].to_string()));
                }
            }
            Ok(Value::Array(results))
        }

        // Put
        (Position::First, Operation::Put(v)) => {
            let prefix = value_to_str_for_concat(&v);
            Ok(Value::Str(prefix + s))
        }
        (Position::Last, Operation::Put(v)) => {
            let suffix = value_to_str_for_concat(&v);
            Ok(Value::Str(s.clone() + &suffix))
        }
        (Position::At(idx_val), Operation::Put(v)) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "string index")),
            };
            let i = resolve_seq_index(idx, chars.len())?;
            let ins = value_to_str_for_concat(&v);
            let mut result: String = chars[..i].iter().collect();
            result.push_str(&ins);
            let tail: String = chars[i..].iter().collect();
            result.push_str(&tail);
            Ok(Value::Str(result))
        }

        // Update
        (Position::First, Operation::Update(v)) => {
            if chars.is_empty() { return Ok(Value::Str(s.clone())); }
            let repl = value_to_str_for_concat(&v);
            let rest: String = chars[1..].iter().collect();
            Ok(Value::Str(repl + &rest))
        }
        (Position::Last, Operation::Update(v)) => {
            if chars.is_empty() { return Ok(Value::Str(s.clone())); }
            let repl = value_to_str_for_concat(&v);
            let front: String = chars[..chars.len()-1].iter().collect();
            Ok(Value::Str(front + &repl))
        }
        (Position::At(idx_val), Operation::Update(v)) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "string index")),
            };
            let i = resolve_seq_index(idx, chars.len())?;
            let repl = value_to_str_for_concat(&v);
            let mut result: String = chars[..i].iter().collect();
            result.push_str(&repl);
            let tail: String = chars[i+1..].iter().collect();
            result.push_str(&tail);
            Ok(Value::Str(result))
        }
        (Position::Where(needle), Operation::Update(v)) => {
            let repl = value_to_str_for_concat(&v);
            Ok(Value::Str(s.replace(&needle, &repl)))
        }

        // Delete
        (Position::First, Operation::Delete) => {
            if chars.is_empty() { return Ok(Value::Str(s.clone())); }
            Ok(Value::Str(chars[1..].iter().collect()))
        }
        (Position::Last, Operation::Delete) => {
            if chars.is_empty() { return Ok(Value::Str(s.clone())); }
            Ok(Value::Str(chars[..chars.len()-1].iter().collect()))
        }
        (Position::At(idx_val), Operation::Delete) => {
            let idx = match idx_val {
                Value::Int(n) => n,
                other => return Err(GoblinError::type_error("int", other.type_name(), "string index")),
            };
            let i = resolve_seq_index(idx, chars.len())?;
            let mut new_chars = chars.clone();
            new_chars.remove(i);
            Ok(Value::Str(new_chars.iter().collect()))
        }
        (Position::Where(needle), Operation::Delete) => {
            Ok(Value::Str(s.replace(&needle, "")))
        }
        (Position::Matching(pat), Operation::Delete) => {
            let re = compile_regex(&pat)?;
            Ok(Value::Str(re.replace_all(s, "").to_string()))
        }
        (Position::Matching(pat), Operation::Update(v)) => {
            let re = compile_regex(&pat)?;
            let repl = value_to_str_for_concat(&v);
            Ok(Value::Str(re.replace_all(s, repl.as_str()).to_string()))
        }
        (Position::Matching(pat), Operation::Put(v)) => {
            let re = compile_regex(&pat)?;
            let ins = value_to_str_for_concat(&v);
            let mut result = String::new();
            let mut last_end = 0;
            for m in re.find_iter(s) {
                result.push_str(&s[last_end..m.end()]);
                result.push_str(&ins);
                last_end = m.end();
            }
            result.push_str(&s[last_end..]);
            Ok(Value::Str(result))
        }
        (Position::Between(start_pat, end_pat), Operation::Delete) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let start_matches: Vec<_> = start_re.find_iter(s).collect();
            let end_matches:   Vec<_> = end_re.find_iter(s).collect();
            let mut result = String::new();
            let mut last_pos = 0;
            for sm in &start_matches {
                if let Some(em) = end_matches.iter().find(|e| e.start() > sm.end()) {
                    result.push_str(&s[last_pos..sm.start()]);
                    last_pos = em.end();
                }
            }
            result.push_str(&s[last_pos..]);
            Ok(Value::Str(result))
        }
        (Position::Between(start_pat, end_pat), Operation::Update(v)) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let repl = value_to_str_for_concat(&v);
            let start_matches: Vec<_> = start_re.find_iter(s).collect();
            let end_matches:   Vec<_> = end_re.find_iter(s).collect();
            let mut result = String::new();
            let mut last_pos = 0;
            for sm in &start_matches {
                if let Some(em) = end_matches.iter().find(|e| e.start() > sm.end()) {
                    result.push_str(&s[last_pos..sm.end()]);
                    result.push_str(&repl);
                    last_pos = em.start();
                }
            }
            result.push_str(&s[last_pos..]);
            Ok(Value::Str(result))
        }
        (Position::Between(start_pat, end_pat), Operation::Put(v)) => {
            let start_re = compile_regex(&start_pat)?;
            let end_re   = compile_regex(&end_pat)?;
            let ins = value_to_str_for_concat(&v);
            let start_matches: Vec<_> = start_re.find_iter(s).collect();
            let end_matches:   Vec<_> = end_re.find_iter(s).collect();
            let mut result = String::new();
            let mut last_pos = 0;
            for sm in &start_matches {
                if let Some(em) = end_matches.iter().find(|e| e.start() > sm.end()) {
                    result.push_str(&s[last_pos..em.end()]);
                    result.push_str(&ins);
                    last_pos = em.end();
                }
            }
            result.push_str(&s[last_pos..]);
            Ok(Value::Str(result))
        }
        // Random + Put/Update/Delete
        (Position::Random, Operation::Put(v)) => {
            if chars.is_empty() { return Err(GoblinError::Runtime("empty string".into())); }
            let i = rng_bounded(session, chars.len());
            let ins = match v { Value::Str(s) => s, _ => return Err(GoblinError::type_error("string", v.type_name(), "put_random on string")) };
            let mut result: String = chars[..i].iter().collect();
            result.push_str(&ins);
            let tail: String = chars[i..].iter().collect();
            result.push_str(&tail);
            Ok(Value::Str(result))
        }
        (Position::Random, Operation::Update(v)) => {
            if chars.is_empty() { return Err(GoblinError::Runtime("empty string".into())); }
            let i = rng_bounded(session, chars.len());
            let repl = match v { Value::Str(s) => s, _ => return Err(GoblinError::type_error("string", v.type_name(), "update_random on string")) };
            let mut result: String = chars[..i].iter().collect();
            result.push_str(&repl);
            let tail: String = chars[i+1..].iter().collect();
            result.push_str(&tail);
            Ok(Value::Str(result))
        }
        (Position::Random, Operation::Delete) => {
            if chars.is_empty() { return Err(GoblinError::Runtime("empty string".into())); }
            let i = rng_bounded(session, chars.len());
            let mut new_chars = chars.clone();
            new_chars.remove(i);
            Ok(Value::Str(new_chars.iter().collect()))
        }
        // All + Delete: return empty string
        (Position::All, Operation::Delete) => {
            Ok(Value::Str(String::new()))
        }
        // All + Put/Update: replace entire string (v must be string)
        (Position::All, Operation::Put(v)) | (Position::All, Operation::Update(v)) => {
            match v {
                Value::Str(s) => Ok(Value::Str(s)),
                _ => Err(GoblinError::type_error("string", v.type_name(), "put/update-all on string expects string value")),
            }
        }
        _ => Err(GoblinError::Runtime("unsupported string operation".into())),
    }
}

fn value_to_str_for_concat(v: &Value) -> String {
    match v {
        Value::Str(s)  => s.clone(),
        Value::Char(c) => c.to_string(),
        other          => fmt_value_raw(other),
    }
}

// ── Utility: has_element / element_count ─────────────────────────────────────

pub fn has_element(v: &Value, needle: &Value) -> bool {
    match v {
        Value::Array(xs)  => xs.iter().any(|x| x == needle),
        Value::Map(m)     => {
            if let Value::Str(k) = needle { m.contains_key(k) }
            else { m.values().any(|x| x == needle) }
        }
        Value::MapOrd(m)  => {
            if let Value::Str(k) = needle { m.contains_key(k) }
            else { m.values().any(|x| x == needle) }
        }
        Value::Str(s)     => {
            if let Value::Str(sub) = needle { s.contains(sub.as_str()) }
            else if let Value::Char(c) = needle { s.contains(*c) }
            else { false }
        }
        Value::Collection(c) => has(c, needle),
        _ => false,
    }
}

pub fn element_count(v: &Value) -> usize {
    match v {
        Value::Array(xs)  => xs.len(),
        Value::Map(m)     => m.len(),
        Value::MapOrd(m)  => m.len(),
        Value::Str(s)     => s.chars().count(),
        Value::Collection(c) => count(c),
        _ => 0,
    }
}

// ── Internal: flatten any layout to Vec<Value> ────────────────────────────────

pub fn to_vec(coll: &CollectionValue) -> Vec<Value> {
    match &coll.layout {
        CollectionLayout::FlatArray(v)       => v.as_ref().clone(),
        CollectionLayout::RingBuf(rb)        => rb.to_vec(),
        CollectionLayout::ChunkedSeq(cs)     => cs.to_flat(),
        CollectionLayout::SmallMap(pairs)    => pairs.iter().map(|(k, v)| {
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

pub fn is_map_collection(coll: &CollectionValue) -> bool {
    is_map(coll)
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

/// Convert any collection-like Value into a canonical Value::Collection.
/// Scalars (Int, Str, Bool, etc.) pass through unchanged — they are element values, not collections.
pub fn into_collection(v: Value) -> Value {
    match v {
        Value::Array(xs) => Value::Collection(Rc::new(CollectionValue::from_flat(xs))),
        Value::Map(m) => {
            let pairs: Vec<(Value, Value)> = m.into_iter()
                .map(|(k, v)| (Value::Str(k), v))
                .collect();
            Value::Collection(Rc::new(CollectionValue::from_map(pairs)))
        }
        Value::MapOrd(m) => {
            let pairs: Vec<(Value, Value)> = m.into_iter()
                .map(|(k, v)| (Value::Str(k), v))
                .collect();
            Value::Collection(Rc::new(CollectionValue::from_map(pairs)))
        }
        other => other,
    }
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

// ── Canonical collection dispatch: routes Value::Collection by its backend ────

fn update_meta_for_op(meta: &CollectionMeta, pos: &Position, _op: &Operation) -> CollectionMeta {
    let mut m = meta.clone();
    match pos {
        Position::First               => m.front_hits = m.front_hits.saturating_add(1),
        Position::Last                => m.back_hits  = m.back_hits.saturating_add(1),
        Position::At(_) | Position::Random => m.mid_hits = m.mid_hits.saturating_add(1),
        Position::All | Position::Where(_) | Position::Matching(_) | Position::Between(..) => {
            m.scan_hits = m.scan_hits.saturating_add(1);
        }
    }
    m
}

fn collection_value_op(
    c: &CollectionValue,
    pos: Position,
    op: Operation,
    session: &mut crate::session::Session,
) -> Result<Value, GoblinError> {
    let meta = update_meta_for_op(&c.meta, &pos, &op);
    let result = if is_map(c) {
        // SmallMap / HashMapBackend → map semantics
        let pairs = to_pairs(c);
        let as_btree: std::collections::BTreeMap<String, Value> = pairs
            .iter()
            .filter_map(|(k, v)| value_to_map_key(k).map(|s| (s, v.clone())))
            .collect();
        map_op_btree(&as_btree, pos, op, session)?
    } else {
        // FlatArray / RingBuf / ChunkedSeq → sequence semantics
        let xs = to_vec(c);
        array_op(&xs, pos, op, session)?
    };
    Ok(match result {
        Value::Array(arr) => {
            Value::Collection(Rc::new(build_seq(arr, meta)))
        }
        Value::Map(m2) => {
            let pairs: Vec<(Value, Value)> = m2.into_iter()
                .map(|(k, v)| (Value::Str(k), v))
                .collect();
            Value::Collection(Rc::new(build_map(pairs, meta)))
        }
        other => other, // scalar: Get/Reap element, or Str from str_op
    })
}

// ── GetIndex / SetIndex (used directly by the VM for [] and {} syntax) ────────

/// Get an element from a collection by key.
/// For sequences: key must be Int.
/// For maps: key can be any Value.
pub fn get_index(coll_val: &Value, key: &Value) -> Result<Value, GoblinError> {
    match coll_val {
        Value::Array(xs) => {
            let idx = match key {
                Value::Int(n) => *n,
                _ => return Err(GoblinError::type_error("int", key.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            Ok(xs[i].clone())
        }
        Value::Map(m) => {
            let k = match key {
                Value::Str(s) => s.clone(),
                other => value_to_map_key(other)
                    .ok_or_else(|| GoblinError::type_error("string", other.type_name(), "map key"))?,
            };
            m.get(&k).cloned().ok_or(GoblinError::KeyNotFound)
        }
        Value::MapOrd(m) => {
            let k = match key {
                Value::Str(s) => s.clone(),
                other => value_to_map_key(other)
                    .ok_or_else(|| GoblinError::type_error("string", other.type_name(), "map key"))?,
            };
            m.get(&k).cloned().ok_or(GoblinError::KeyNotFound)
        }
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
                let i = resolve_seq_index(idx, items.len())?;
                Ok(items[i].clone())
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
        Value::Array(mut xs) => {
            let idx = match key {
                Value::Int(n) => *n,
                _ => return Err(GoblinError::type_error("int", key.type_name(), "array index")),
            };
            let i = resolve_seq_index(idx, xs.len())?;
            xs[i] = new_val;
            Ok(Value::Array(xs))
        }
        Value::Map(mut m) => {
            let k = match key {
                Value::Str(s) => s.clone(),
                other => value_to_map_key(other)
                    .ok_or_else(|| GoblinError::type_error("string", other.type_name(), "map key"))?,
            };
            m.insert(k, new_val);
            Ok(Value::Map(m))
        }
        Value::MapOrd(mut m) => {
            let k = match key {
                Value::Str(s) => s.clone(),
                other => value_to_map_key(other)
                    .ok_or_else(|| GoblinError::type_error("string", other.type_name(), "map key"))?,
            };
            m.insert(k, new_val);
            Ok(Value::MapOrd(m))
        }
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

// ── Legacy grab family ────────────────────────────────────────────────────────

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
    let i = resolve_seq_index(idx, items.len())?;
    Ok(items[i].clone())
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

// ── Legacy put family ─────────────────────────────────────────────────────────

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

// ── Legacy update family ──────────────────────────────────────────────────────

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

// ── Legacy delete family ──────────────────────────────────────────────────────

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

// ── Legacy reap family ────────────────────────────────────────────────────────

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

// ── Legacy query operations ───────────────────────────────────────────────────

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
