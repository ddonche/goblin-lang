//! The built-in event queue.
//!
//! ```text
//! q | :agenda()                      // an empty queue
//! id | :schedule!(q, day, event)     // add an event on a day; gives its id
//! :unschedule!(q, id)                // take a pending event out
//! evs | :due!(q, day)                // remove and give every event due on
//!                                    // or before `day`, earliest day first,
//!                                    // and in the order scheduled within a day
//! ```
//!
//! An agenda is an ordinary map, so it copies, compares, freezes and crosses
//! to actors and `:swarm` like any other value:
//! `{"next": id, "days": [day, …], "at": {day: [[id, event], …]}, "pending": {id: day}}`.
//! `days` is sorted and holds the days that have events. The calls change the
//! agenda in place when nothing else shares it.

use std::rc::Rc;

use crate::error::GoblinError;
use crate::value::{BuiltinId, CollectionLayout, CollectionValue, Value};

fn s(k: &str) -> Value { Value::Str(k.into()) }

fn map(pairs: Vec<(Value, Value)>) -> Value { Value::Collection(Rc::new(CollectionValue::from_map(pairs))) }

fn list(items: Vec<Value>) -> Value { Value::Collection(Rc::new(CollectionValue::from_flat(items))) }

/// `:agenda()`.
pub fn new() -> Value {
    map(vec![
        (s("next"), Value::Int(1)),
        (s("days"), list(Vec::new())),
        (s("at"), map(Vec::new())),
        (s("pending"), map(Vec::new())),
    ])
}

fn err(name: &str, msg: &str) -> GoblinError { GoblinError::Runtime(format!("{name}: {msg}")) }

fn not_agenda(name: &str) -> GoblinError {
    err(name, "the first argument must be an agenda (make one with :agenda())")
}

// ── maps and lists, changed in place ────────────────────────────────────────

fn as_map(v: &mut Value) -> Option<&mut CollectionValue> {
    match v {
        Value::Collection(rc) if rc.is_map() => Some(Rc::make_mut(rc)),
        _ => None,
    }
}

fn map_get<'a>(c: &'a mut CollectionValue, k: &Value) -> Option<&'a mut Value> {
    match &mut c.layout {
        CollectionLayout::SmallMap(p) => Rc::make_mut(p).iter_mut().find(|(pk, _)| pk == k).map(|(_, v)| v),
        CollectionLayout::HashMapBackend(m) => Rc::make_mut(m).get_mut(k),
        _ => None,
    }
}

fn map_insert(c: &mut CollectionValue, k: Value, v: Value) {
    match &mut c.layout {
        CollectionLayout::SmallMap(p) => {
            let pairs = Rc::make_mut(p);
            match pairs.iter_mut().find(|(pk, _)| *pk == k) {
                Some(slot) => slot.1 = v,
                None => pairs.push((k, v)),
            }
            // Same switch-over as map literals: more than 16 keys hash.
            if pairs.len() > 16 {
                let mut m: indexmap::IndexMap<Value, Value, _> = Default::default();
                m.extend(pairs.drain(..));
                c.layout = CollectionLayout::HashMapBackend(Rc::new(m));
            }
        }
        CollectionLayout::HashMapBackend(m) => { Rc::make_mut(m).insert(k, v); }
        _ => {}
    }
    c.meta.len = map_len(c);
}

fn map_remove(c: &mut CollectionValue, k: &Value) -> Option<Value> {
    let out = match &mut c.layout {
        CollectionLayout::SmallMap(p) => {
            let pairs = Rc::make_mut(p);
            let i = pairs.iter().position(|(pk, _)| pk == k)?;
            Some(pairs.remove(i).1)
        }
        CollectionLayout::HashMapBackend(m) => Rc::make_mut(m).shift_remove(k),
        _ => None,
    };
    c.meta.len = map_len(c);
    out
}

fn map_len(c: &CollectionValue) -> usize {
    match &c.layout {
        CollectionLayout::SmallMap(p) => p.len(),
        CollectionLayout::HashMapBackend(m) => m.len(),
        _ => c.meta.len,
    }
}

/// A list's items to change in place. A list in another layout (after it was
/// used as a queue, say) is turned back into a plain list first.
fn with_list<R>(v: &mut Value, f: impl FnOnce(&mut Vec<Value>) -> R) -> Option<R> {
    let Value::Collection(rc) = v else { return None };
    if rc.is_map() { return None; }
    if !matches!(rc.layout, CollectionLayout::FlatArray(_)) {
        *rc = Rc::new(CollectionValue::from_flat(crate::collections::to_vec(rc)));
    }
    let c = Rc::make_mut(rc);
    let CollectionLayout::FlatArray(items) = &mut c.layout else { unreachable!() };
    let items = Rc::make_mut(items);
    let r = f(items);
    c.meta.len = items.len();
    Some(r)
}

// ── the agenda's parts ──────────────────────────────────────────────────────

struct Parts<'a> {
    next: &'a mut Value,
    days: &'a mut Value,
    at: &'a mut CollectionValue,
    pending: &'a mut CollectionValue,
}

fn parts<'a>(q: &'a mut Value, name: &str) -> Result<Parts<'a>, GoblinError> {
    let c = as_map(q).ok_or_else(|| not_agenda(name))?;
    // Borrow the four fields at once: find each one's position first.
    let CollectionLayout::SmallMap(p) = &mut c.layout else { return Err(not_agenda(name)) };
    let pairs = Rc::make_mut(p);
    let pos = |k: &str| pairs.iter().position(|(pk, _)| matches!(pk, Value::Str(s) if s.as_str() == k));
    let (Some(n), Some(d), Some(a), Some(pe)) = (pos("next"), pos("days"), pos("at"), pos("pending")) else {
        return Err(not_agenda(name));
    };
    let mut slots: Vec<Option<&mut Value>> = pairs.iter_mut().map(|(_, v)| Some(v)).collect();
    let next = slots[n].take().unwrap();
    let days = slots[d].take().unwrap();
    let at = as_map(slots[a].take().unwrap()).ok_or_else(|| not_agenda(name))?;
    let pending = as_map(slots[pe].take().unwrap()).ok_or_else(|| not_agenda(name))?;
    if !matches!(next, Value::Int(_)) { return Err(not_agenda(name)); }
    Ok(Parts { next, days, at, pending })
}

fn day_of(v: &Value, name: &str) -> Result<i64, GoblinError> {
    match v {
        Value::Int(d) => Ok(*d),
        other => Err(err(name, &format!("the day must be a whole number, not {}", other.type_name()))),
    }
}

fn schedule(q: &mut Value, day: &Value, event: &Value) -> Result<Value, GoblinError> {
    const NAME: &str = "schedule!";
    let day = day_of(day, NAME)?;
    let p = parts(q, NAME)?;
    let Value::Int(id) = *p.next else { unreachable!() };
    *p.next = Value::Int(id + 1);
    let entry = list(vec![Value::Int(id), event.clone()]);
    match map_get(p.at, &Value::Int(day)) {
        Some(bucket) => { with_list(bucket, |xs| xs.push(entry)).ok_or_else(|| not_agenda(NAME))?; }
        None => {
            map_insert(p.at, Value::Int(day), list(vec![entry]));
            with_list(p.days, |xs| {
                let i = xs.partition_point(|x| matches!(x, Value::Int(d) if *d < day));
                xs.insert(i, Value::Int(day));
            }).ok_or_else(|| not_agenda(NAME))?;
        }
    }
    map_insert(p.pending, Value::Int(id), Value::Int(day));
    Ok(Value::Int(id))
}

fn unschedule(q: &mut Value, id: &Value) -> Result<Value, GoblinError> {
    const NAME: &str = "unschedule!";
    let p = parts(q, NAME)?;
    // An id that isn't pending (already due, already taken out, or never
    // given) changes nothing.
    let Some(Value::Int(day)) = map_remove(p.pending, id) else { return Ok(Value::Nil) };
    let key = Value::Int(day);
    let bucket = map_get(p.at, &key).ok_or_else(|| not_agenda(NAME))?;
    let left = with_list(bucket, |xs| {
        xs.retain(|e| crate::collections::get_index(e, &Value::Int(0)).map_or(true, |x| x != *id));
        xs.len()
    }).ok_or_else(|| not_agenda(NAME))?;
    if left == 0 {
        map_remove(p.at, &key);
        with_list(p.days, |xs| xs.retain(|d| *d != key)).ok_or_else(|| not_agenda(NAME))?;
    }
    Ok(Value::Nil)
}

fn due(q: &mut Value, day: &Value) -> Result<Value, GoblinError> {
    const NAME: &str = "due!";
    let day = day_of(day, NAME)?;
    let p = parts(q, NAME)?;
    let ready: Vec<Value> = with_list(p.days, |xs| {
        let n = xs.partition_point(|x| matches!(x, Value::Int(d) if *d <= day));
        xs.drain(..n).collect()
    }).ok_or_else(|| not_agenda(NAME))?;
    let mut out = Vec::new();
    for d in ready {
        let Some(Value::Collection(bucket)) = map_remove(p.at, &d) else { continue };
        for entry in crate::collections::to_vec(&bucket) {
            let id = crate::collections::get_index(&entry, &Value::Int(0))?;
            map_remove(p.pending, &id);
            out.push(crate::collections::get_index(&entry, &Value::Int(1))?);
        }
    }
    Ok(list(out))
}

/// One of `:schedule!`, `:unschedule!`, `:due!` on the agenda at `q`,
/// changing it in place. Gives the call's value.
pub fn call(id: BuiltinId, q: &mut Value, args: &[Value]) -> Result<Value, GoblinError> {
    match (id, args) {
        (BuiltinId::Schedule, [day, event]) => schedule(q, day, event),
        (BuiltinId::Unschedule, [id]) => unschedule(q, id),
        (BuiltinId::Due, [day]) => due(q, day),
        (BuiltinId::Schedule, _) => Err(GoblinError::ArityMismatch { expected: 3, got: args.len() + 1, name: "schedule!".into() }),
        (BuiltinId::Unschedule, _) => Err(GoblinError::ArityMismatch { expected: 2, got: args.len() + 1, name: "unschedule!".into() }),
        (BuiltinId::Due, _) => Err(GoblinError::ArityMismatch { expected: 2, got: args.len() + 1, name: "due!".into() }),
        _ => Err(GoblinError::Runtime(format!("{id:?} is not an agenda call"))),
    }
}

/// The value at `keys` below `root`, to change in place (copying what is
/// shared on the way down).
pub fn path_mut<'a>(root: &'a mut Value, keys: &[Value]) -> Result<&'a mut Value, GoblinError> {
    let mut cur = root;
    for k in keys {
        let Value::Collection(rc) = cur else {
            return Err(GoblinError::Runtime("agenda: the path to the agenda must go through maps and lists".into()));
        };
        let c = Rc::make_mut(rc);
        let len = c.len();
        let next = match &mut c.layout {
            CollectionLayout::SmallMap(p) => Rc::make_mut(p).iter_mut().find(|(pk, _)| pk == k).map(|(_, v)| v),
            CollectionLayout::HashMapBackend(m) => Rc::make_mut(m).get_mut(k),
            layout => {
                let Value::Int(i) = k else {
                    return Err(GoblinError::Runtime("agenda: a list index must be a whole number".into()));
                };
                let i = if *i < 0 { *i + len as i64 } else { *i };
                if i < 0 || i as usize >= len { None } else {
                    let i = i as usize;
                    match layout {
                        CollectionLayout::FlatArray(v) => Rc::make_mut(v).get_mut(i),
                        CollectionLayout::RingBuf(rb) => Rc::make_mut(rb).get_mut(i),
                        CollectionLayout::ChunkedSeq(cs) => Rc::make_mut(cs).get_mut(i),
                        _ => None,
                    }
                }
            }
        };
        cur = next.ok_or_else(|| GoblinError::Runtime(format!(
            "agenda: nothing at {} on the path to the agenda", crate::collections::fmt_value_raw(k))))?;
    }
    Ok(cur)
}
