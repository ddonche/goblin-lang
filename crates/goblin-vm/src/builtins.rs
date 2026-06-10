/// Builtin function dispatch for the Goblin VM.
///
/// All builtins follow the same calling convention:
///   - receive a Vec<Tether> of arguments
///   - receive &mut Session to allocate results
///   - return a Tether pointing to the result stash
use std::rc::Rc;

use crate::collections;
use crate::error::GoblinError;
use crate::session::Session;
use crate::value::{BuiltinId, CollectionValue, Tether, Value};

pub fn call_builtin(
    id: BuiltinId,
    args: Vec<Tether>,
    session: &mut Session,
) -> Result<Tether, GoblinError> {
    let result = dispatch(id, args, session)?;
    Ok(session.alloc_value(result))
}

fn dispatch(id: BuiltinId, args: Vec<Tether>, session: &mut Session) -> Result<Value, GoblinError> {
    let read = |i: usize| -> Result<Value, GoblinError> {
        let t = args.get(i).ok_or_else(|| GoblinError::Runtime(
            format!("builtin {:?}: expected arg {i}", id)
        ))?;
        session.read_value(t)
    };
    let expect_n = |n: usize| -> Result<(), GoblinError> {
        if args.len() != n {
            Err(GoblinError::ArityMismatch { expected: n, got: args.len(), name: format!("{:?}", id) })
        } else { Ok(()) }
    };

    match id {
        // ── Memory ───────────────────────────────────────────────────────────
        BuiltinId::MemId => {
            expect_n(1)?;
            let addr = session.mem_id_raw(&args[0]);
            let pairs = vec![
                (Value::Str("slot".into()),       Value::Int(addr.slot as i64)),
                (Value::Str("generation".into()), Value::Int(addr.generation as i64)),
            ];
            Ok(Value::Collection(Rc::new(CollectionValue::from_map(pairs))))
        }
        BuiltinId::MemAddr => {
            expect_n(1)?;
            let s = session.mem_addr_hex(&args[0])?;
            Ok(Value::Str(s))
        }
        BuiltinId::MemTotal => {
            Ok(Value::Int(session.stash_count() as i64))
        }
        BuiltinId::MemHuman => {
            Ok(Value::Str(format!("{} stashes", session.stash_count())))
        }
        BuiltinId::Gc => {
            session.gc_sweep();
            Ok(Value::Nil)
        }

        // ── Math ──────────────────────────────────────────────────────────────
        BuiltinId::Abs => {
            expect_n(1)?;
            match read(0)? {
                Value::Int(n)   => Ok(Value::Int(n.abs())),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                other => Err(GoblinError::type_error("number", other.type_name(), "abs")),
            }
        }
        BuiltinId::Min => {
            if args.len() < 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "min".into() });
            }
            Ok(numeric_min(read(0)?, read(1)?)?)
        }
        BuiltinId::Max => {
            if args.len() < 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "max".into() });
            }
            Ok(numeric_max(read(0)?, read(1)?)?)
        }
        BuiltinId::Floor => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Int(f.floor() as i64),
                other => return Err(GoblinError::type_error("number", other.type_name(), "floor")),
            })
        }
        BuiltinId::Ceil => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Int(f.ceil() as i64),
                other => return Err(GoblinError::type_error("number", other.type_name(), "ceil")),
            })
        }
        BuiltinId::Round => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Int(f.round() as i64),
                other => return Err(GoblinError::type_error("number", other.type_name(), "round")),
            })
        }
        BuiltinId::Sqrt => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Float((n as f64).sqrt()),
                Value::Float(f) => Value::Float(f.sqrt()),
                other => return Err(GoblinError::type_error("number", other.type_name(), "sqrt")),
            })
        }
        BuiltinId::Pow => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "pow".into() });
            }
            Ok(match (read(0)?, read(1)?) {
                (Value::Int(b), Value::Int(e)) if e >= 0 => Value::Int(b.pow(e as u32)),
                (Value::Int(b), Value::Float(e)) => Value::Float((b as f64).powf(e)),
                (Value::Float(b), Value::Float(e)) => Value::Float(b.powf(e)),
                (Value::Float(b), Value::Int(e)) => Value::Float(b.powi(e as i32)),
                _ => return Err(GoblinError::type_error("number", "mixed", "pow")),
            })
        }

        // ── String builtins (interpreter-aligned) ─────────────────────────────
        BuiltinId::Lower => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.to_lowercase())
        }
        BuiltinId::Upper => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.to_uppercase())
        }
        BuiltinId::Title => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| {
                s.split_whitespace()
                    .map(|word| {
                        let mut chars = word.chars();
                        match chars.next() {
                            None => String::new(),
                            Some(c) => {
                                let upper: String = c.to_uppercase().collect();
                                let rest: String = chars.collect::<String>().to_lowercase();
                                upper + &rest
                            }
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            })
        }
        BuiltinId::Slug => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| {
                let lower = s.to_lowercase();
                let slug: String = lower.chars().map(|c| {
                    if c.is_alphanumeric() { c } else { '-' }
                }).collect();
                let mut result = String::new();
                let mut prev_hyphen = false;
                for c in slug.chars() {
                    if c == '-' {
                        if !prev_hyphen { result.push('-'); }
                        prev_hyphen = true;
                    } else {
                        result.push(c);
                        prev_hyphen = false;
                    }
                }
                result.trim_matches('-').to_string()
            })
        }
        BuiltinId::Mixed => {
            expect_n(1)?;
            let v = read(0)?;
            let seed = session.next_u128();
            mixed_case(&v, seed)
        }
        BuiltinId::Raw => {
            expect_n(1)?;
            Ok(read(0)?)
        }
        BuiltinId::Trim => {
            expect_n(1)?;
            let trim_chars: &[char] = &[
                ' ', '\t', '\n', '\r',
                '\u{00A0}', '\u{FEFF}', '\u{200B}',
                '\u{200C}', '\u{200D}', '\u{2060}', '\u{180E}',
            ];
            map_str_1(&read(0)?, &|s: &str| s.trim_matches(trim_chars).to_string())
        }
        BuiltinId::TrimLead => {
            expect_n(1)?;
            let trim_chars: &[char] = &[
                ' ', '\t', '\n', '\r',
                '\u{00A0}', '\u{FEFF}', '\u{200B}',
                '\u{200C}', '\u{200D}', '\u{2060}', '\u{180E}',
            ];
            map_str_1(&read(0)?, &|s: &str| s.trim_start_matches(trim_chars).to_string())
        }
        BuiltinId::TrimTrail => {
            expect_n(1)?;
            let trim_chars: &[char] = &[
                ' ', '\t', '\n', '\r',
                '\u{00A0}', '\u{FEFF}', '\u{200B}',
                '\u{200C}', '\u{200D}', '\u{2060}', '\u{180E}',
            ];
            map_str_1(&read(0)?, &|s: &str| s.trim_end_matches(trim_chars).to_string())
        }
        BuiltinId::Find => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "find".into() });
            }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sub)) => {
                    Ok(match s.find(sub.as_str()) {
                        Some(byte_idx) => Value::Int(s[..byte_idx].chars().count() as i64),
                        None => Value::Nil,
                    })
                }
                (Value::Array(arr), needle_val) => {
                    Ok(match arr.iter().position(|x| x == &needle_val) {
                        Some(i) => Value::Int(i as i64),
                        None => Value::Nil,
                    })
                }
                (other, _) => Err(GoblinError::type_error("str or array", other.type_name(), "find")),
            }
        }
        BuiltinId::FindAll => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "find_all".into() });
            }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sub)) => {
                    let mut results = Vec::new();
                    let mut search_start = 0usize;
                    while search_start < s.len() {
                        if let Some(idx) = s[search_start..].find(sub.as_str()) {
                            let abs_byte = search_start + idx;
                            let char_idx = s[..abs_byte].chars().count();
                            results.push(Value::Int(char_idx as i64));
                            search_start = abs_byte + sub.len().max(1);
                        } else { break; }
                    }
                    Ok(Value::Array(results))
                }
                (other, _) => Err(GoblinError::type_error("str", other.type_name(), "find_all")),
            }
        }
        BuiltinId::Ord => {
            expect_n(1)?;
            match read(0)? {
                Value::Char(c) => Ok(Value::Int(c as i64)),
                Value::Str(s) => {
                    let mut chars = s.chars();
                    match (chars.next(), chars.next()) {
                        (Some(c), None) => Ok(Value::Int(c as i64)),
                        _ => Err(GoblinError::Runtime("ord: string must be a single character".into())),
                    }
                }
                other => Err(GoblinError::type_error("char or single-char str", other.type_name(), "ord")),
            }
        }

        // ── String legacy ─────────────────────────────────────────────────────
        BuiltinId::Len => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Str(s)        => Value::Int(s.chars().count() as i64),
                Value::Array(a)      => Value::Int(a.len() as i64),
                Value::Map(m)        => Value::Int(m.len() as i64),
                Value::MapOrd(m)     => Value::Int(m.len() as i64),
                Value::Seq(s)        => Value::Int(s.len() as i64),
                Value::Collection(c) => Value::Int(c.meta.len as i64),
                other => return Err(GoblinError::type_error("str or collection", other.type_name(), "len")),
            })
        }
        BuiltinId::ToString | BuiltinId::ToStr => {
            expect_n(1)?;
            Ok(Value::Str(value_to_str(&read(0)?)))
        }
        BuiltinId::ToUpperCase => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                other => Err(GoblinError::type_error("str", other.type_name(), "to_uppercase")),
            }
        }
        BuiltinId::ToLowerCase => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                other => Err(GoblinError::type_error("str", other.type_name(), "to_lowercase")),
            }
        }
        BuiltinId::Split => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "split".into() });
            }
            let s = match read(0)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("str", other.type_name(), "split")),
            };
            let sep = match read(1)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("str", other.type_name(), "split separator")),
            };
            let parts: Vec<Value> = s.split(sep.as_str()).map(|p| Value::Str(p.to_string())).collect();
            Ok(Value::Array(parts))
        }
        BuiltinId::Join => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "join".into() });
            }
            let sep = match read(1)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("str", other.type_name(), "join separator")),
            };
            match read(0)? {
                Value::Array(items) => {
                    let parts: Vec<String> = items.iter().map(value_to_str).collect();
                    Ok(Value::Str(parts.join(&sep)))
                }
                Value::Collection(c) => {
                    let parts: Vec<String> = collections::to_vec(&c).iter().map(value_to_str).collect();
                    Ok(Value::Str(parts.join(&sep)))
                }
                other => Err(GoblinError::type_error("array or collection", other.type_name(), "join")),
            }
        }
        BuiltinId::Contains => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "contains".into() });
            }
            let haystack = read(0)?; let needle = read(1)?;
            Ok(match (haystack, needle) {
                (Value::Str(s), Value::Str(n)) => Value::Bool(s.contains(n.as_str())),
                (Value::Array(arr), v)         => Value::Bool(arr.iter().any(|x| x == &v)),
                (Value::Collection(c), v)      => Value::Bool(collections::to_vec(&c).iter().any(|x| x == &v)),
                _ => Value::Bool(false),
            })
        }
        BuiltinId::StartsWith => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "starts_with".into() });
            }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(p)) => Ok(Value::Bool(s.starts_with(p.as_str()))),
                _ => Ok(Value::Bool(false)),
            }
        }
        BuiltinId::EndsWith => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ends_with".into() });
            }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(p)) => Ok(Value::Bool(s.ends_with(p.as_str()))),
                _ => Ok(Value::Bool(false)),
            }
        }
        BuiltinId::Replace => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "replace".into() });
            }
            match (read(0)?, read(1)?, read(2)?) {
                (Value::Str(s), Value::Str(from), Value::Str(to)) => Ok(Value::Str(s.replace(from.as_str(), &to))),
                _ => Err(GoblinError::type_error("str", "mixed", "replace")),
            }
        }

        // ── Maps ──────────────────────────────────────────────────────────────
        BuiltinId::Keys => {
            expect_n(1)?;
            match read(0)? {
                Value::Map(m)    => Ok(Value::Array(m.keys().cloned().map(Value::Str).collect())),
                Value::MapOrd(m) => Ok(Value::Array(m.keys().cloned().map(Value::Str).collect())),
                Value::Collection(c) => Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::keys(&c))))),
                other => Err(GoblinError::type_error("map or collection", other.type_name(), "keys")),
            }
        }
        BuiltinId::Values => {
            expect_n(1)?;
            match read(0)? {
                Value::Map(m)    => Ok(Value::Array(m.values().cloned().collect())),
                Value::MapOrd(m) => Ok(Value::Array(m.values().cloned().collect())),
                Value::Collection(c) => Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::values(&c))))),
                other => Err(GoblinError::type_error("map or collection", other.type_name(), "values")),
            }
        }
        BuiltinId::Items => {
            expect_n(1)?;
            match read(0)? {
                Value::Map(m) => {
                    let pairs: Vec<Value> = m.iter()
                        .map(|(k, v)| Value::Pair(Box::new(Value::Str(k.clone())), Box::new(v.clone())))
                        .collect();
                    Ok(Value::Array(pairs))
                }
                Value::MapOrd(m) => {
                    let pairs: Vec<Value> = m.iter()
                        .map(|(k, v)| Value::Pair(Box::new(Value::Str(k.clone())), Box::new(v.clone())))
                        .collect();
                    Ok(Value::Array(pairs))
                }
                other => Err(GoblinError::type_error("map", other.type_name(), "items")),
            }
        }

        // ── Collections (interpreter-aligned) ─────────────────────────────────
        BuiltinId::Has => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "has".into() });
            }
            let container = read(0)?;
            let needle = read(1)?;
            Ok(Value::Bool(match (&container, &needle) {
                (Value::Str(s), Value::Str(sub)) => s.contains(sub.as_str()),
                (Value::Array(arr), v)           => arr.iter().any(|x| x == v),
                (Value::Map(m), Value::Str(k))   => m.contains_key(k),
                (Value::MapOrd(m), Value::Str(k))=> m.contains_key(k),
                (Value::Seq(s), v)               => s.items.iter().any(|x| x == v),
                (Value::Collection(c), v)        => collections::has(c, v),
                (Value::Nil, _) | (Value::Unit, _) => false,
                _ => false,
            }))
        }
        BuiltinId::Count => {
            match args.len() {
                1 => Ok(match read(0)? {
                    Value::Array(a)      => Value::Int(a.len() as i64),
                    Value::Map(m)        => Value::Int(m.len() as i64),
                    Value::MapOrd(m)     => Value::Int(m.len() as i64),
                    Value::Seq(s)        => Value::Int(s.len() as i64),
                    Value::Str(s)        => Value::Int(s.chars().count() as i64),
                    Value::Collection(c) => Value::Int(collections::count(&c) as i64),
                    other => return Err(GoblinError::type_error("collection or str", other.type_name(), "count")),
                }),
                2 => match (read(0)?, read(1)?) {
                    (Value::Str(s), Value::Str(sub)) => {
                        let count = if sub.is_empty() { 0 } else {
                            let mut n = 0usize;
                            let mut start = 0;
                            while let Some(idx) = s[start..].find(sub.as_str()) {
                                n += 1;
                                start += idx + sub.len();
                            }
                            n
                        };
                        Ok(Value::Int(count as i64))
                    }
                    _ => Err(GoblinError::Runtime("count(str, sub): expected two strings".into())),
                },
                n => Err(GoblinError::ArityMismatch { expected: 1, got: n, name: "count".into() }),
            }
        }
        BuiltinId::Shuffle => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(mut items) => {
                    fisher_yates_shuffle(&mut items, session);
                    Ok(Value::Array(items))
                }
                Value::Str(s) => {
                    let mut chars: Vec<char> = s.chars().collect();
                    let n = chars.len();
                    for i in (1..n).rev() {
                        let j = rng_bounded(session, (i + 1) as u64) as usize;
                        chars.swap(i, j);
                    }
                    Ok(Value::Str(chars.iter().collect()))
                }
                Value::Int(n) => {
                    let s = n.to_string();
                    let mut chars: Vec<char> = s.chars().collect();
                    let len = chars.len();
                    for i in (1..len).rev() {
                        let j = rng_bounded(session, (i + 1) as u64) as usize;
                        chars.swap(i, j);
                    }
                    let result: String = chars.iter().collect();
                    Ok(Value::Int(result.parse::<i64>().unwrap_or(n)))
                }
                other => Err(GoblinError::type_error("array, str, or int", other.type_name(), "shuffle")),
            }
        }
        BuiltinId::Sort => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(mut items) => {
                    items.sort_by(|a, b| fmt_value_raw(a).cmp(&fmt_value_raw(b)));
                    Ok(Value::Array(items))
                }
                Value::Str(s) => {
                    let mut chars: Vec<char> = s.chars().collect();
                    chars.sort();
                    Ok(Value::Str(chars.iter().collect()))
                }
                Value::Collection(c) => Ok(collections::sort_values(&c)),
                other => Err(GoblinError::type_error("array or str", other.type_name(), "sort")),
            }
        }
        BuiltinId::Freq => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    let mut freq: indexmap::IndexMap<String, Value> = indexmap::IndexMap::new();
                    for item in &items {
                        let key = fmt_value_raw(item);
                        let count = freq.get(&key).and_then(|v| {
                            if let Value::Int(n) = v { Some(*n) } else { None }
                        }).unwrap_or(0);
                        freq.insert(key, Value::Int(count + 1));
                    }
                    Ok(Value::MapOrd(freq))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "freq")),
            }
        }
        BuiltinId::Mode => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    if items.is_empty() { return Ok(Value::Nil); }
                    let mut freq: std::collections::HashMap<String, (i64, Value)> = std::collections::HashMap::new();
                    let mut order: Vec<String> = Vec::new();
                    for item in &items {
                        let key = fmt_value_raw(item);
                        let entry = freq.entry(key.clone()).or_insert_with(|| {
                            order.push(key.clone());
                            (0, item.clone())
                        });
                        entry.0 += 1;
                    }
                    let max_count = freq.values().map(|(c, _)| *c).max().unwrap_or(0);
                    for key in &order {
                        if let Some((count, val)) = freq.get(key) {
                            if *count == max_count {
                                return Ok(val.clone());
                            }
                        }
                    }
                    Ok(Value::Nil)
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "mode")),
            }
        }
        BuiltinId::SampleWeighted => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "sample_weighted".into() });
            }
            let items = match read(0)? {
                Value::Array(a) => a,
                other => return Err(GoblinError::type_error("array", other.type_name(), "sample_weighted")),
            };
            let weights_vec: Vec<(String, Value)> = match read(1)? {
                Value::Map(m)    => m.into_iter().collect(),
                Value::MapOrd(m) => m.into_iter().collect(),
                other => return Err(GoblinError::type_error("map", other.type_name(), "sample_weighted weights")),
            };
            if items.is_empty() { return Ok(Value::Nil); }
            let mut cumulative: Vec<f64> = Vec::new();
            let mut total = 0.0f64;
            for item in &items {
                let key = fmt_value_raw(item);
                let w = weights_vec.iter().find(|(k, _)| k == &key)
                    .and_then(|(_, v)| match v {
                        Value::Float(f) => Some(*f),
                        Value::Int(n) => Some(*n as f64),
                        _ => None,
                    })
                    .unwrap_or(1.0);
                total += w;
                cumulative.push(total);
            }
            let r = (rng_bounded(session, 1_000_000) as f64 / 1_000_000.0) * total;
            for (i, &cum) in cumulative.iter().enumerate() {
                if r <= cum { return Ok(items[i].clone()); }
            }
            Ok(items.last().cloned().unwrap_or(Value::Nil))
        }
        BuiltinId::Map => {
            Err(GoblinError::NotImplemented { feature: "map builtin requires VM callback support" })
        }
        BuiltinId::Unique => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    let mut seen = std::collections::BTreeSet::new();
                    let result: Vec<Value> = items.into_iter().filter(|item| seen.insert(fmt_value_raw(item))).collect();
                    Ok(Value::Array(result))
                }
                Value::Collection(c) => Ok(collections::unique(&c)),
                other => Err(GoblinError::type_error("array", other.type_name(), "unique")),
            }
        }
        BuiltinId::Dups => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    let mut freq: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
                    for item in &items {
                        *freq.entry(fmt_value_raw(item)).or_insert(0) += 1;
                    }
                    let mut seen = std::collections::BTreeSet::new();
                    let result: Vec<Value> = items.into_iter().filter(|item| {
                        let k = fmt_value_raw(item);
                        freq.get(&k).copied().unwrap_or(0) > 1 && seen.insert(k)
                    }).collect();
                    Ok(Value::Array(result))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "dups")),
            }
        }

        // ── Collections grab (legacy) ─────────────────────────────────────────
        BuiltinId::Grab => {
            expect_n(1)?;
            Ok(collections::grab(&*require_collection(read(0)?, "grab")?))
        }
        BuiltinId::GrabFirst => {
            expect_n(1)?;
            collections::grab_first(&*require_collection(read(0)?, "grab_first")?)
        }
        BuiltinId::GrabLast => {
            expect_n(1)?;
            collections::grab_last(&*require_collection(read(0)?, "grab_last")?)
        }
        BuiltinId::GrabAt => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "grab_at".into() }); }
            let coll = require_collection(read(0)?, "grab_at")?;
            let idx = require_int(read(1)?, "grab_at index")?;
            collections::grab_at(&coll, idx)
        }
        BuiltinId::GrabBetween => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "grab_between".into() }); }
            let coll = require_collection(read(0)?, "grab_between")?;
            let start = require_int(read(1)?, "grab_between start")?;
            let end   = require_int(read(2)?, "grab_between end")?;
            collections::grab_between(&coll, start, end)
        }
        BuiltinId::GrabAll => {
            expect_n(1)?;
            Ok(collections::grab_all(&*require_collection(read(0)?, "grab_all")?))
        }
        BuiltinId::GrabRandom => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab_random")?;
            let items = collections::to_vec(&coll);
            if items.is_empty() { return Ok(Value::Nil); }
            let idx = rng_bounded(session, items.len() as u64) as usize;
            Ok(items[idx].clone())
        }
        BuiltinId::GrabWhere | BuiltinId::GrabMatching => {
            Err(GoblinError::NotImplemented { feature: "grab_where / grab_matching require VM predicate callback" })
        }

        // ── Collections put (legacy) ──────────────────────────────────────────
        BuiltinId::Put => {
            if args.len() == 2 {
                let coll = require_collection(read(0)?, "put")?;
                let val = read(1)?;
                collections::put(&coll, Value::Int(coll.meta.len as i64), val)
            } else if args.len() == 3 {
                let coll = require_collection(read(0)?, "put")?;
                let key = read(1)?;
                let val = read(2)?;
                collections::put(&coll, key, val)
            } else {
                Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put".into() })
            }
        }
        BuiltinId::PutFirst => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_first".into() }); }
            collections::put_first(&*require_collection(read(0)?, "put_first")?, read(1)?)
        }
        BuiltinId::PutLast => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_last".into() }); }
            collections::put_last(&*require_collection(read(0)?, "put_last")?, read(1)?)
        }
        BuiltinId::PutAt => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "put_at".into() }); }
            let coll = require_collection(read(0)?, "put_at")?;
            let idx = require_int(read(1)?, "put_at index")?;
            collections::put_at(&coll, idx, read(2)?)
        }

        // ── Collections update (legacy) ───────────────────────────────────────
        BuiltinId::Update => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update".into() }); }
            let coll = require_collection(read(0)?, "update")?;
            let key = read(1)?;
            collections::update(&coll, &key, read(2)?)
        }
        BuiltinId::UpdateFirst => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_first".into() }); }
            collections::update_first(&*require_collection(read(0)?, "update_first")?, read(1)?)
        }
        BuiltinId::UpdateLast => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_last".into() }); }
            collections::update_last(&*require_collection(read(0)?, "update_last")?, read(1)?)
        }
        BuiltinId::UpdateAt => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update_at".into() }); }
            let coll = require_collection(read(0)?, "update_at")?;
            let idx = require_int(read(1)?, "update_at index")?;
            collections::update_at(&coll, idx, read(2)?)
        }

        // ── Collections delete (legacy) ───────────────────────────────────────
        BuiltinId::Delete => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete".into() }); }
            let coll = require_collection(read(0)?, "delete")?;
            collections::delete(&coll, &read(1)?)
        }
        BuiltinId::DeleteFirst => {
            expect_n(1)?;
            collections::delete_first(&*require_collection(read(0)?, "delete_first")?)
        }
        BuiltinId::DeleteLast => {
            expect_n(1)?;
            collections::delete_last(&*require_collection(read(0)?, "delete_last")?)
        }
        BuiltinId::DeleteAt => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete_at".into() }); }
            let coll = require_collection(read(0)?, "delete_at")?;
            let idx = require_int(read(1)?, "delete_at index")?;
            collections::delete_at(&coll, idx)
        }
        BuiltinId::DeleteWhere | BuiltinId::DeleteAll => {
            Err(GoblinError::NotImplemented { feature: "delete_where / delete_all require VM predicate callback" })
        }

        // ── Collections reap (legacy) ─────────────────────────────────────────
        BuiltinId::Reap => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap".into() }); }
            let coll = require_collection(read(0)?, "reap")?;
            let (elem, new_coll) = collections::reap(&coll, &read(1)?)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapFirst => {
            expect_n(1)?;
            let rc = require_collection(read(0)?, "reap_first")?; let (elem, new_coll) = collections::reap_first(&*rc)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapLast => {
            expect_n(1)?;
            let rc = require_collection(read(0)?, "reap_last")?; let (elem, new_coll) = collections::reap_last(&*rc)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapAt => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_at".into() }); }
            let coll = require_collection(read(0)?, "reap_at")?;
            let idx = require_int(read(1)?, "reap_at index")?;
            let (elem, new_coll) = collections::reap_at(&coll, idx)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapRandom => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "reap_random")?;
            let items = collections::to_vec(&coll);
            if items.is_empty() {
                return Ok(Value::Collection(Rc::new(CollectionValue::from_flat(
                    vec![Value::Nil, Value::Collection(Rc::new(CollectionValue::empty_array()))]
                ))));
            }
            let idx = rng_bounded(session, items.len() as u64) as usize;
            let (elem, new_coll) = collections::reap_at(&coll, idx as i64)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapWhere | BuiltinId::ReapAll => {
            Err(GoblinError::NotImplemented { feature: "reap_where / reap_all require VM predicate callback" })
        }

        // ── Collections — new Position×Operation matrix ───────────────────────
        // Get family
        BuiltinId::GetFirst => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::First, collections::Operation::Get, session)
        }
        BuiltinId::GetLast => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::Last, collections::Operation::Get, session)
        }
        BuiltinId::GetAt => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "get_at".into() }); }
            let coll = read(0)?;
            let key = read(1)?;
            collections::collection_operation(&coll, collections::Position::At(key), collections::Operation::Get, session)
        }
        BuiltinId::GetWhere => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "get_where".into() }); }
            let coll = read(0)?;
            let pred = match read(1)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("string", other.type_name(), "get_where predicate")),
            };
            collections::collection_operation(&coll, collections::Position::Where(pred), collections::Operation::Get, session)
        }
        BuiltinId::GetAll => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::All, collections::Operation::Get, session)
        }
        BuiltinId::GetMatching => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "get_matching".into() }); }
            let coll = read(0)?;
            let pat = match read(1)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("string", other.type_name(), "get_matching pattern")),
            };
            collections::collection_operation(&coll, collections::Position::Matching(pat), collections::Operation::Get, session)
        }
        BuiltinId::GetBetween => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "get_between".into() }); }
            let coll = read(0)?;
            let start = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "get_between start")) };
            let end   = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "get_between end")) };
            collections::collection_operation(&coll, collections::Position::Between(start, end), collections::Operation::Get, session)
        }
        BuiltinId::GetRandom => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::Random, collections::Operation::Get, session)
        }

        // Put family (new)
        BuiltinId::PutWhere => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "put_where".into() }); }
            let coll = read(0)?;
            let pred = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "put_where predicate")) };
            let val = read(2)?;
            collections::collection_operation(&coll, collections::Position::Where(pred), collections::Operation::Put(val), session)
        }
        BuiltinId::PutMatching => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "put_matching".into() }); }
            let coll = read(0)?;
            let pat = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "put_matching pattern")) };
            let val = read(2)?;
            collections::collection_operation(&coll, collections::Position::Matching(pat), collections::Operation::Put(val), session)
        }
        BuiltinId::PutBetween => {
            if args.len() != 4 { return Err(GoblinError::ArityMismatch { expected: 4, got: args.len(), name: "put_between".into() }); }
            let coll = read(0)?;
            let start = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "put_between start")) };
            let end   = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "put_between end")) };
            let val = read(3)?;
            collections::collection_operation(&coll, collections::Position::Between(start, end), collections::Operation::Put(val), session)
        }
        BuiltinId::PutRandom => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_random".into() }); }
            let coll = read(0)?;
            let val = read(1)?;
            collections::collection_operation(&coll, collections::Position::Random, collections::Operation::Put(val), session)
        }
        BuiltinId::PutAll => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_all".into() }); }
            let coll = read(0)?;
            let val = read(1)?;
            collections::collection_operation(&coll, collections::Position::All, collections::Operation::Put(val), session)
        }

        // Update family (new)
        BuiltinId::UpdateAll => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_all".into() }); }
            let coll = read(0)?;
            let val = read(1)?;
            collections::collection_operation(&coll, collections::Position::All, collections::Operation::Update(val), session)
        }
        BuiltinId::UpdateWhere => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update_where".into() }); }
            let coll = read(0)?;
            let pred = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "update_where predicate")) };
            let val = read(2)?;
            collections::collection_operation(&coll, collections::Position::Where(pred), collections::Operation::Update(val), session)
        }
        BuiltinId::UpdateMatching => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update_matching".into() }); }
            let coll = read(0)?;
            let pat = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "update_matching pattern")) };
            let val = read(2)?;
            collections::collection_operation(&coll, collections::Position::Matching(pat), collections::Operation::Update(val), session)
        }
        BuiltinId::UpdateBetween => {
            if args.len() != 4 { return Err(GoblinError::ArityMismatch { expected: 4, got: args.len(), name: "update_between".into() }); }
            let coll = read(0)?;
            let start = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "update_between start")) };
            let end   = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "update_between end")) };
            let val = read(3)?;
            collections::collection_operation(&coll, collections::Position::Between(start, end), collections::Operation::Update(val), session)
        }
        BuiltinId::UpdateRandom => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_random".into() }); }
            let coll = read(0)?;
            let val = read(1)?;
            collections::collection_operation(&coll, collections::Position::Random, collections::Operation::Update(val), session)
        }

        // Delete family (new)
        BuiltinId::DeleteMatching => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete_matching".into() }); }
            let coll = read(0)?;
            let pat = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "delete_matching pattern")) };
            collections::collection_operation(&coll, collections::Position::Matching(pat), collections::Operation::Delete, session)
        }
        BuiltinId::DeleteBetween => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "delete_between".into() }); }
            let coll = read(0)?;
            let start = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "delete_between start")) };
            let end   = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "delete_between end")) };
            collections::collection_operation(&coll, collections::Position::Between(start, end), collections::Operation::Delete, session)
        }
        BuiltinId::DeleteRandom => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::Random, collections::Operation::Delete, session)
        }

        // Reap family (new)
        BuiltinId::ReapFirst2 => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::First, collections::Operation::Reap, session)
        }
        BuiltinId::ReapLast2 => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::Last, collections::Operation::Reap, session)
        }
        BuiltinId::ReapAt2 => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_at2".into() }); }
            let coll = read(0)?;
            let key = read(1)?;
            collections::collection_operation(&coll, collections::Position::At(key), collections::Operation::Reap, session)
        }
        BuiltinId::ReapWhere2 => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_where".into() }); }
            let coll = read(0)?;
            let pred = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "reap_where predicate")) };
            collections::collection_operation(&coll, collections::Position::Where(pred), collections::Operation::Reap, session)
        }
        BuiltinId::ReapMatching => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_matching".into() }); }
            let coll = read(0)?;
            let pat = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "reap_matching pattern")) };
            collections::collection_operation(&coll, collections::Position::Matching(pat), collections::Operation::Reap, session)
        }
        BuiltinId::ReapBetween => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "reap_between".into() }); }
            let coll = read(0)?;
            let start = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "reap_between start")) };
            let end   = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("string", other.type_name(), "reap_between end")) };
            collections::collection_operation(&coll, collections::Position::Between(start, end), collections::Operation::Reap, session)
        }
        BuiltinId::ReapRandom2 => {
            expect_n(1)?;
            let coll = read(0)?;
            collections::collection_operation(&coll, collections::Position::Random, collections::Operation::Reap, session)
        }

        // ── Collections query (legacy) ─────────────────────────────────────────
        BuiltinId::Pairs => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "pairs")?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::pairs_vec(&*coll)))))
        }
        BuiltinId::IsEmpty => {
            expect_n(1)?;
            match read(0)? {
                Value::Collection(c) => Ok(Value::Bool(c.is_empty())),
                Value::Array(a)      => Ok(Value::Bool(a.is_empty())),
                Value::Map(m)        => Ok(Value::Bool(m.is_empty())),
                Value::MapOrd(m)     => Ok(Value::Bool(m.is_empty())),
                Value::Str(s)        => Ok(Value::Bool(s.is_empty())),
                Value::Nil           => Ok(Value::Bool(true)),
                _                    => Ok(Value::Bool(false)),
            }
        }
        BuiltinId::Reverse => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(mut items) => { items.reverse(); Ok(Value::Array(items)) }
                Value::Str(s) => Ok(Value::Str(s.chars().rev().collect())),
                other => { let c = require_collection(other, "reverse")?; Ok(collections::reverse(&*c)) }
            }
        }
        BuiltinId::SortBy | BuiltinId::Filter | BuiltinId::Reduce
        | BuiltinId::Any | BuiltinId::All | BuiltinId::FindIndex => {
            Err(GoblinError::NotImplemented { feature: "higher-order collection ops require VM predicate callback" })
        }
        BuiltinId::Zip => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "zip".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Array(a), Value::Array(b)) => {
                    let pairs: Vec<Value> = a.into_iter().zip(b.into_iter())
                        .map(|(x, y)| Value::Pair(Box::new(x), Box::new(y)))
                        .collect();
                    Ok(Value::Array(pairs))
                }
                (a, b) => { let ca = require_collection(a, "zip")?; let cb = require_collection(b, "zip")?; Ok(collections::zip_collections(&*ca, &*cb)) }
            }
        }
        BuiltinId::Flatten => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    let mut result = Vec::new();
                    for item in items {
                        match item {
                            Value::Array(inner) => result.extend(inner),
                            other => result.push(other),
                        }
                    }
                    Ok(Value::Array(result))
                }
                other => { let c = require_collection(other, "flatten")?; Ok(collections::flatten(&*c)) }
            }
        }
        BuiltinId::Slice => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "slice".into() }); }
            let coll = require_collection(read(0)?, "slice")?;
            let start = require_int(read(1)?, "slice start")?;
            let end   = require_int(read(2)?, "slice end")?;
            collections::slice_collection(&coll, start, end)
        }

        // ── Range ──────────────────────────────────────────────────────────────
        BuiltinId::Range => {
            match args.len() {
                1 => {
                    let n = require_int(read(0)?, "range")?;
                    Ok(Value::Array((0..n).map(Value::Int).collect()))
                }
                2 => {
                    let start = require_int(read(0)?, "range start")?;
                    let end   = require_int(read(1)?, "range end")?;
                    Ok(Value::Array((start..end).map(Value::Int).collect()))
                }
                n => Err(GoblinError::ArityMismatch { expected: 1, got: n, name: "range".into() }),
            }
        }

        // ── I/O ───────────────────────────────────────────────────────────────
        BuiltinId::Print => {
            let parts: Result<Vec<String>, _> = args.iter().map(|t| session.read_value(t).map(|v| value_to_str(&v))).collect();
            print!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Println => {
            let parts: Result<Vec<String>, _> = args.iter().map(|t| session.read_value(t).map(|v| value_to_str(&v))).collect();
            println!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Eprint => {
            let parts: Result<Vec<String>, _> = args.iter().map(|t| session.read_value(t).map(|v| value_to_str(&v))).collect();
            eprint!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Eprintln => {
            let parts: Result<Vec<String>, _> = args.iter().map(|t| session.read_value(t).map(|v| value_to_str(&v))).collect();
            eprintln!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }

        // ── Type checks ───────────────────────────────────────────────────────
        BuiltinId::IsNil        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Nil))) }
        BuiltinId::IsBool       => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Bool(_)))) }
        BuiltinId::IsInt        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Int(_)))) }
        BuiltinId::IsFloat      => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Float(_)))) }
        BuiltinId::IsStr        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Str(_)))) }
        BuiltinId::IsArray      => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Array(_) | Value::Collection(_)))) }
        BuiltinId::IsMap        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Map(_) | Value::MapOrd(_)))) }
        BuiltinId::IsCollection => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Collection(_) | Value::Array(_) | Value::Map(_) | Value::MapOrd(_) | Value::Seq(_)))) }
        BuiltinId::IsFunction   => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Function(_) | Value::Closure(_)))) }

        // ── Conversions ───────────────────────────────────────────────────────
        BuiltinId::ToInt => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Int(f as i64),
                Value::Bool(b)  => Value::Int(b as i64),
                Value::Str(s)   => Value::Int(s.trim().parse::<i64>()
                    .map_err(|_| GoblinError::Runtime(format!("cannot convert {:?} to int", s)))?),
                other => return Err(GoblinError::type_error("number or str", other.type_name(), "to_int")),
            })
        }
        BuiltinId::ToFloat => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Float(f) => Value::Float(f),
                Value::Int(n)   => Value::Float(n as f64),
                Value::Bool(b)  => Value::Float(b as i64 as f64),
                Value::Str(s)   => Value::Float(s.trim().parse::<f64>()
                    .map_err(|_| GoblinError::Runtime(format!("cannot convert {:?} to float", s)))?),
                other => return Err(GoblinError::type_error("number or str", other.type_name(), "to_float")),
            })
        }
        BuiltinId::ToBool => {
            expect_n(1)?;
            Ok(Value::Bool(read(0)?.is_truthy()))
        }

        // ── Meta ──────────────────────────────────────────────────────────────
        BuiltinId::TypeOf => {
            expect_n(1)?;
            Ok(Value::Str(read(0)?.type_name().to_string()))
        }
        BuiltinId::Assert => {
            if args.is_empty() { return Err(GoblinError::ArityMismatch { expected: 1, got: 0, name: "assert".into() }); }
            if !read(0)?.is_truthy() {
                let msg = if args.len() > 1 {
                    match read(1)? { Value::Str(s) => s, v => value_to_str(&v) }
                } else { "assertion failed".to_string() };
                return Err(GoblinError::Runtime(msg));
            }
            Ok(Value::Nil)
        }
        BuiltinId::Panic => {
            let msg = if args.is_empty() { "panic!".to_string() } else {
                match session.read_value(&args[0])? { Value::Str(s) => s, v => value_to_str(&v) }
            };
            Err(GoblinError::Runtime(msg))
        }

        // ── Lorem ipsum (stub) ────────────────────────────────────────────────
        BuiltinId::Ipsum | BuiltinId::IpsumSentences | BuiltinId::IpsumParagraphs | BuiltinId::IpsumFull => {
            Ok(Value::Str("Lorem ipsum dolor sit amet.".into()))
        }

        // ── Process (stub) ────────────────────────────────────────────────────
        BuiltinId::RunCmd => {
            Err(GoblinError::NotImplemented { feature: "run_cmd: process execution not yet implemented in VM" })
        }

        // ── Request/Response (stub) ───────────────────────────────────────────
        BuiltinId::ReqMethod | BuiltinId::ReqPath | BuiltinId::ReqQuery
        | BuiltinId::ReqBody | BuiltinId::ReqHeader | BuiltinId::Cookie
        | BuiltinId::SetStatus | BuiltinId::SetHeader | BuiltinId::SetCookie => {
            Err(GoblinError::NotImplemented { feature: "HTTP request/response builtins require HTTP context" })
        }

        // ── pack / unpack ─────────────────────────────────────────────────────
        BuiltinId::Pack => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            Ok(pack_value(v))
        }
        BuiltinId::Unpack => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            Ok(match v {
                Value::Int(n) => Value::Array(n.to_string().chars().map(|c| Value::Str(c.to_string())).collect()),
                Value::Str(s) => Value::Array(s.chars().map(Value::Char).collect()),
                other => other,
            })
        }

        // ── secure_pick / secure_shuffle ──────────────────────────────────────
        BuiltinId::SecurePick => {
            if args.is_empty() { return Ok(Value::Nil); }
            let cfg = session.read_value(&args[0])?;
            let m = match cfg {
                Value::Map(ref m) => m.clone(),
                _ => return Err(GoblinError::type_error("map", cfg.type_name(), "secure_pick")),
            };
            let count = map_get_int(&m, "count_expr")
                .or_else(|| map_get_int(&m, "count"))
                .unwrap_or(1) as usize;
            if count == 0 { return Ok(Value::Array(vec![])); }
            let allow_dups = map_get_bool(&m, "allow_dups").unwrap_or(false);
            if let Some(src) = m.get("src") {
                let items: Vec<Value> = match src {
                    Value::Array(a) => a.clone(),
                    Value::Str(s)   => s.chars().map(|c| Value::Str(c.to_string())).collect(),
                    _ => return Err(GoblinError::Runtime("secure_pick: unsupported src type".into())),
                };
                if items.is_empty() {
                    return Err(GoblinError::Runtime("secure_pick: cannot pick from empty collection".into()));
                }
                let out = csprng_pick_from_slice(&items, count, allow_dups, session);
                return Ok(if count == 1 { out.into_iter().next().unwrap_or(Value::Nil) } else { Value::Array(out) });
            }
            if m.contains_key("range_start") && m.contains_key("range_end") {
                let start  = map_get_int(&m, "range_start").unwrap_or(0);
                let end_v  = map_get_int(&m, "range_end").unwrap_or(0);
                let incl   = map_get_bool(&m, "range_inclusive").unwrap_or(false);
                let end_i  = if incl { end_v + 1 } else { end_v };
                let range_size = (end_i - start).max(0) as usize;
                if range_size == 0 {
                    return Err(GoblinError::Runtime("secure_pick: empty range".into()));
                }
                let out: Vec<Value> = (0..count)
                    .map(|_| Value::Int(start + rng_bounded(session, range_size as u64) as i64))
                    .collect();
                return Ok(if count == 1 { out.into_iter().next().unwrap_or(Value::Nil) } else { Value::Array(out) });
            }
            Err(GoblinError::Runtime("secure_pick: needs 'src' or range_start/range_end".into()))
        }

        BuiltinId::SecureShuffle => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            match v {
                Value::Array(mut items) => {
                    fisher_yates_shuffle(&mut items, session);
                    Ok(Value::Array(items))
                }
                Value::Str(s) => {
                    let mut chars: Vec<char> = s.chars().collect();
                    let n = chars.len();
                    for i in (1..n).rev() {
                        let j = rng_bounded(session, (i + 1) as u64) as usize;
                        chars.swap(i, j);
                    }
                    Ok(Value::Str(chars.into_iter().collect()))
                }
                _ => Err(GoblinError::type_error("array or string", v.type_name(), "secure_shuffle")),
            }
        }

        // ── String extras ─────────────────────────────────────────────────────
        BuiltinId::Lines => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            match v {
                Value::Str(s) => Ok(Value::Array(s.lines().map(|l| Value::Str(l.to_string())).collect())),
                _ => Err(GoblinError::type_error("string", v.type_name(), "lines")),
            }
        }
        BuiltinId::Words => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            match v {
                Value::Str(s) => Ok(Value::Array(s.split_whitespace().map(|w| Value::Str(w.to_string())).collect())),
                _ => Err(GoblinError::type_error("string", v.type_name(), "words")),
            }
        }
        BuiltinId::Chars => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            match v {
                Value::Str(s) => Ok(Value::Array(s.chars().map(Value::Char).collect())),
                _ => Err(GoblinError::type_error("string", v.type_name(), "chars")),
            }
        }
        BuiltinId::Format => {
            if args.is_empty() { return Ok(Value::Nil); }
            let v = session.read_value(&args[0])?;
            let decimals = if args.len() > 1 {
                match session.read_value(&args[1])? { Value::Int(n) => n as usize, _ => 2 }
            } else { 2 };
            let n = match v { Value::Float(f) => f, Value::Int(i) => i as f64, _ => return Ok(Value::Nil) };
            Ok(Value::Str(format!("{:.prec$}", n, prec = decimals)))
        }
        BuiltinId::Pad | BuiltinId::PadLeft => {
            if args.len() < 2 { return Ok(Value::Nil); }
            let s = match session.read_value(&args[0])? { Value::Str(s) => s, v => fmt_value_raw(&v) };
            let width = match session.read_value(&args[1])? { Value::Int(n) => n as usize, _ => 0 };
            Ok(Value::Str(format!("{:>width$}", s)))
        }
        BuiltinId::PadRight => {
            if args.len() < 2 { return Ok(Value::Nil); }
            let s = match session.read_value(&args[0])? { Value::Str(s) => s, v => fmt_value_raw(&v) };
            let width = match session.read_value(&args[1])? { Value::Int(n) => n as usize, _ => 0 };
            Ok(Value::Str(format!("{:<width$}", s)))
        }
        BuiltinId::Repeat => {
            if args.len() < 2 { return Ok(Value::Nil); }
            let s = match session.read_value(&args[0])? { Value::Str(s) => s, v => fmt_value_raw(&v) };
            let n = match session.read_value(&args[1])? { Value::Int(n) => n as usize, _ => 0 };
            Ok(Value::Str(s.repeat(n)))
        }

        // ── Higher-order (stub — need VM callback) ────────────────────────────
        BuiltinId::MapFn | BuiltinId::FilterFn | BuiltinId::ReduceFn | BuiltinId::ForEachFn => {
            Err(GoblinError::NotImplemented { feature: "higher-order map/filter/reduce require VM callback support" })
        }
    }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn require_collection(v: Value, op: &'static str) -> Result<std::rc::Rc<CollectionValue>, GoblinError> {
    match v {
        Value::Collection(c) => Ok(c),
        other => Err(GoblinError::type_error("collection", other.type_name(), op)),
    }
}

fn require_int(v: Value, op: &'static str) -> Result<i64, GoblinError> {
    match v {
        Value::Int(n) => Ok(n),
        other => Err(GoblinError::type_error("int", other.type_name(), op)),
    }
}

fn numeric_min(a: Value, b: Value) -> Result<Value, GoblinError> {
    Ok(match (&a, &b) {
        (Value::Int(x), Value::Int(y))     => Value::Int(*x.min(y)),
        (Value::Float(x), Value::Float(y)) => Value::Float(x.min(*y)),
        (Value::Int(x), Value::Float(y))   => Value::Float((*x as f64).min(*y)),
        (Value::Float(x), Value::Int(y))   => Value::Float(x.min(*y as f64)),
        _ => return Err(GoblinError::type_error("number", b.type_name(), "min")),
    })
}

fn numeric_max(a: Value, b: Value) -> Result<Value, GoblinError> {
    Ok(match (&a, &b) {
        (Value::Int(x), Value::Int(y))     => Value::Int(*x.max(y)),
        (Value::Float(x), Value::Float(y)) => Value::Float(x.max(*y)),
        (Value::Int(x), Value::Float(y))   => Value::Float((*x as f64).max(*y)),
        (Value::Float(x), Value::Int(y))   => Value::Float(x.max(*y as f64)),
        _ => return Err(GoblinError::type_error("number", b.type_name(), "max")),
    })
}

pub fn value_to_str(v: &Value) -> String {
    match v {
        Value::Nil           => "nil".to_string(),
        Value::Unit          => "()".to_string(),
        Value::Bool(b)       => b.to_string(),
        Value::Int(n)        => n.to_string(),
        Value::Float(f)      => f.to_string(),
        Value::Big(d)        => d.to_string(),
        Value::Pct(p)        => format!("{}%", p),
        Value::Char(c)       => c.to_string(),
        Value::Str(s)        => s.clone(),
        Value::Formatted(v, _) => value_to_str(v),
        Value::Array(items) => {
            let parts: Vec<String> = items.iter().map(value_to_str).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Map(m) => {
            let parts: Vec<String> = m.iter().map(|(k, v)| format!("{}: {}", k, value_to_str(v))).collect();
            format!("{{{}}}", parts.join(", "))
        }
        Value::MapOrd(m) => {
            let parts: Vec<String> = m.iter().map(|(k, v)| format!("{}: {}", k, value_to_str(v))).collect();
            format!("{{{}}}", parts.join(", "))
        }
        Value::Pair(k, v)    => format!("({}, {})", value_to_str(k), value_to_str(v)),
        Value::Seq(s) => {
            let parts: Vec<String> = s.items.iter().map(value_to_str).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::CtrlSkip      => "<skip>".to_string(),
        Value::CtrlStop      => "<stop>".to_string(),
        Value::CtrlReturn(v) => format!("<return {}>", value_to_str(v)),
        Value::Object { class_name, .. } => format!("<{}>", class_name),
        Value::Ref(s)        => format!("<ref {}>", s),
        Value::GridRef { grid_id, x, y } => format!("<gridref {}[{},{}]>", grid_id, x, y),
        Value::Enum { enum_name, variant_name, .. } => format!("{}.{}", enum_name, variant_name),
        Value::Class { name } => format!("<class {}>", name),
        Value::Collection(c) => {
            let items: Vec<String> = collections::to_vec(c).iter().map(value_to_str).collect();
            format!("[{}]", items.join(", "))
        }
        Value::Function(f)   => format!("<fn {}>", f.name),
        Value::Closure(c)    => format!("<closure {}>", c.func.name),
        Value::Builtin(b)    => format!("<builtin {:?}>", b),
    }
}

/// map_str_1: Apply a string transform that works on Str, Char, and Array of Str/Char.
fn map_str_1(v: &Value, f: &dyn Fn(&str) -> String) -> Result<Value, GoblinError> {
    match v {
        Value::Str(s) => Ok(Value::Str(f(s))),
        Value::Char(ch) => {
            let out = f(&ch.to_string());
            let mut iter = out.chars();
            match (iter.next(), iter.next()) {
                (Some(c), None) => Ok(Value::Char(c)),
                _ => Ok(Value::Str(out)),
            }
        }
        Value::Array(xs) => {
            let mut out = Vec::with_capacity(xs.len());
            for it in xs {
                out.push(map_str_1(it, f)?);
            }
            Ok(Value::Array(out))
        }
        other => Err(GoblinError::type_error("string/char", other.type_name(), "string builtin")),
    }
}

/// Mixed case: flip case of each character using PRNG seed.
fn mixed_case(v: &Value, seed: u128) -> Result<Value, GoblinError> {
    fn splitmix(x: u128, i: usize) -> bool {
        let mut h = x.wrapping_add(i as u128).wrapping_mul(0x9e3779b97f4a7c15);
        h ^= h >> 30;
        h = h.wrapping_mul(0xbf58476d1ce4e5b9);
        h ^= h >> 27;
        (h >> 63) & 1 == 1
    }
    match v {
        Value::Str(s) => {
            let result: String = s.chars().enumerate().map(|(i, c)| {
                if splitmix(seed, i) { c.to_uppercase().next().unwrap_or(c) }
                else { c.to_lowercase().next().unwrap_or(c) }
            }).collect();
            Ok(Value::Str(result))
        }
        Value::Char(ch) => {
            let c = if splitmix(seed, 0) { ch.to_uppercase().next().unwrap_or(*ch) }
                    else { ch.to_lowercase().next().unwrap_or(*ch) };
            Ok(Value::Char(c))
        }
        other => Err(GoblinError::type_error("string/char", other.type_name(), "mixed")),
    }
}

fn fmt_value_raw(v: &Value) -> String {
    match v {
        Value::Str(s)   => s.clone(),
        Value::Int(n)   => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Char(c)  => c.to_string(),
        Value::Bool(b)  => b.to_string(),
        Value::Nil      => "nil".into(),
        _               => v.type_name().to_string(),
    }
}

fn fisher_yates_shuffle(items: &mut Vec<Value>, session: &mut Session) {
    let n = items.len();
    for i in (1..n).rev() {
        let j = rng_bounded(session, (i + 1) as u64) as usize;
        items.swap(i, j);
    }
}

fn rng_bounded(session: &mut Session, bound: u64) -> u64 {
    if bound <= 1 { return 0; }
    loop {
        let x = (session.next_u128() >> 64) as u64;
        let m = (x as u128).wrapping_mul(bound as u128);
        let l = m as u64;
        let t = bound.wrapping_neg() % bound;
        if l >= t { return (m >> 64) as u64; }
    }
}

fn pack_value(v: Value) -> Value {
    match v {
        Value::Array(xs) if xs.is_empty() => Value::Int(0),
        Value::Array(xs) => {
            let all_digits = xs.iter().all(|e| matches!(e, Value::Int(n) if *n >= 0 && *n <= 9));
            if all_digits {
                let mut acc: i128 = 0;
                for e in &xs {
                    let d = match e { Value::Int(n) => *n as i128, _ => unreachable!() };
                    match acc.checked_mul(10).and_then(|a| a.checked_add(d)) {
                        Some(v) => acc = v,
                        None => return Value::Nil,
                    }
                }
                Value::Int(acc as i64)
            } else {
                let mut out = String::new();
                for e in &xs {
                    match e {
                        Value::Str(s)   => out.push_str(s),
                        Value::Char(c)  => out.push(*c),
                        Value::Int(n)   => out.push_str(&n.to_string()),
                        Value::Float(f) => out.push_str(&f.to_string()),
                        Value::Bool(b)  => out.push_str(if *b { "true" } else { "false" }),
                        _ => return Value::Nil,
                    }
                }
                Value::Str(out)
            }
        }
        Value::Int(_) | Value::Str(_) | Value::Char(_) => v,
        _ => Value::Nil,
    }
}

fn csprng_pick_from_slice(items: &[Value], count: usize, allow_dups: bool, session: &mut Session) -> Vec<Value> {
    if allow_dups {
        (0..count).map(|_| items[rng_bounded(session, items.len() as u64) as usize].clone()).collect()
    } else {
        let mut idxs: Vec<usize> = (0..items.len()).collect();
        let mut out = Vec::with_capacity(count.min(items.len()));
        for i in 0..count.min(items.len()) {
            let j = i + rng_bounded(session, (items.len() - i) as u64) as usize;
            idxs.swap(i, j);
            out.push(items[idxs[i]].clone());
        }
        out
    }
}

fn map_get_int(m: &std::collections::BTreeMap<String, Value>, key: &str) -> Option<i64> {
    match m.get(key)? {
        Value::Int(n) => Some(*n),
        Value::Float(f) => Some(*f as i64),
        _ => None,
    }
}

fn map_get_bool(m: &std::collections::BTreeMap<String, Value>, key: &str) -> Option<bool> {
    match m.get(key)? {
        Value::Bool(b) => Some(*b),
        _ => None,
    }
}
