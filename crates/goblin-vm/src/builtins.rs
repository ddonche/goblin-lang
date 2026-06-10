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
            if args.is_empty() { return Err(GoblinError::ArityMismatch { expected: 1, got: 0, name: "min".into() }); }
            let vals: Vec<Value> = if args.len() == 1 {
                match read(0)? {
                    Value::Array(xs) => { if xs.is_empty() { return Err(GoblinError::Runtime("min: empty array".into())); } xs }
                    other => return Err(GoblinError::type_error("array", other.type_name(), "min")),
                }
            } else {
                (0..args.len()).map(|i| session.read_value(&args[i])).collect::<Result<Vec<_>, _>>()?
            };
            let mut m = to_f64_val(&vals[0])?;
            for v in &vals[1..] { let f = to_f64_val(v)?; if f < m { m = f; } }
            Ok(Value::Float(m))
        }
        BuiltinId::Max => {
            if args.is_empty() { return Err(GoblinError::ArityMismatch { expected: 1, got: 0, name: "max".into() }); }
            let vals: Vec<Value> = if args.len() == 1 {
                match read(0)? {
                    Value::Array(xs) => { if xs.is_empty() { return Err(GoblinError::Runtime("max: empty array".into())); } xs }
                    other => return Err(GoblinError::type_error("array", other.type_name(), "max")),
                }
            } else {
                (0..args.len()).map(|i| session.read_value(&args[i])).collect::<Result<Vec<_>, _>>()?
            };
            let mut m = to_f64_val(&vals[0])?;
            for v in &vals[1..] { let f = to_f64_val(v)?; if f > m { m = f; } }
            Ok(Value::Float(m))
        }
        BuiltinId::Avg => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(xs) => {
                    if xs.is_empty() { return Ok(Value::Float(0.0)); }
                    let mut acc = 0.0f64;
                    for v in &xs { acc += to_f64_val(v)?; }
                    Ok(Value::Float(acc / xs.len() as f64))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "avg")),
            }
        }
        BuiltinId::Sum => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(xs) => {
                    let mut acc = 0.0f64;
                    for v in &xs { acc += to_f64_val(v)?; }
                    Ok(Value::Float(acc))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "sum")),
            }
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
        BuiltinId::Clamp => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "clamp".into() }); }
            let (x_val, lo_val, hi_val) = (read(0)?, read(1)?, read(2)?);
            let x  = to_f64_val(&x_val)?;
            let lo = to_f64_val(&lo_val)?;
            let hi = to_f64_val(&hi_val)?;
            if lo > hi { return Err(GoblinError::Runtime("clamp: lo must be <= hi".into())); }
            let y = if x < lo { lo } else if x > hi { hi } else { x };
            let all_int = matches!(x_val, Value::Int(_)) && matches!(lo_val, Value::Int(_)) && matches!(hi_val, Value::Int(_));
            Ok(if all_int { Value::Int(y as i64) } else { Value::Float(y) })
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
                let mut out = String::with_capacity(s.len());
                let mut last_dash = false;
                for ch in s.chars() {
                    if ch.is_ascii_alphanumeric() {
                        out.push(ch.to_ascii_lowercase());
                        last_dash = false;
                    } else if !last_dash {
                        out.push('-');
                        last_dash = true;
                    }
                }
                out.trim_matches('-').to_string()
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
            map_str_1(&read(0)?, &|s: &str| {
                let mut out = String::with_capacity(s.len());
                for ch in s.chars() {
                    match ch { '{' => { out.push('{'); out.push('{'); } '}' => { out.push('}'); out.push('}'); } _ => out.push(ch) }
                }
                out
            })
        }
        BuiltinId::Trim => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.trim_matches(|c: char|
                c.is_whitespace()
                || c == '\u{00A0}' || c == '\u{FEFF}' || c == '\u{200B}'
                || c == '\u{200C}' || c == '\u{200D}' || c == '\u{2060}'
                || c == '\u{180E}'
            ).to_string())
        }
        BuiltinId::TrimLead => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.trim_start_matches(|c: char|
                c.is_whitespace()
                || c == '\u{00A0}' || c == '\u{FEFF}' || c == '\u{200B}'
                || c == '\u{200C}' || c == '\u{200D}' || c == '\u{2060}'
                || c == '\u{180E}'
            ).to_string())
        }
        BuiltinId::TrimTrail => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.trim_end_matches(|c: char|
                c.is_whitespace()
                || c == '\u{00A0}' || c == '\u{FEFF}' || c == '\u{200B}'
                || c == '\u{200C}' || c == '\u{200D}' || c == '\u{2060}'
                || c == '\u{180E}'
            ).to_string())
        }
        BuiltinId::Find => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "find".into() });
            }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sub)) => {
                    Ok(match s.find(sub.as_str()) {
                        Some(i) => Value::Int(i as i64),
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
                    if sub.is_empty() {
                        return Ok(Value::Array(vec![]));
                    }
                    let mut out = Vec::new();
                    let mut start = 0usize;
                    while let Some(pos) = s[start..].find(sub.as_str()) {
                        let idx = start + pos;
                        out.push(Value::Int(idx as i64));
                        start = idx + sub.len();
                    }
                    Ok(Value::Array(out))
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
            let parts: Vec<Value> = if sep.is_empty() {
                s.chars().map(|c| Value::Str(c.to_string())).collect()
            } else if sep.starts_with("r/") && sep.len() > 2 {
                let pattern = &sep[2..];
                let re = regex::Regex::new(pattern).map_err(|e| GoblinError::Runtime(format!("split: invalid regex: {}", e)))?;
                re.split(&s).map(|t| Value::Str(t.to_string())).collect()
            } else {
                s.split(sep.as_str()).map(|p| Value::Str(p.to_string())).collect()
            };
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
                Value::Str(s) => {
                    let out: String = s.chars().enumerate().map(|(i, c)| {
                        if i > 0 { format!("{}{}", sep, c) } else { c.to_string() }
                    }).collect();
                    Ok(Value::Str(out))
                }
                Value::Array(items) => {
                    let mut out = String::new();
                    for (i, v) in items.iter().enumerate() {
                        if i > 0 { out.push_str(&sep); }
                        match v {
                            Value::Str(s)  => out.push_str(s),
                            Value::Char(c) => out.push(*c),
                            other => out.push_str(&value_to_str(other)),
                        }
                    }
                    Ok(Value::Str(out))
                }
                Value::Collection(c) => {
                    let parts: Vec<String> = collections::to_vec(&c).iter().map(value_to_str).collect();
                    Ok(Value::Str(parts.join(&sep)))
                }
                other => Err(GoblinError::type_error("str, array or collection", other.type_name(), "join")),
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
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "starts_with".into() }); }
            let text = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "starts_with")) };
            Ok(Value::Bool(match read(1)? {
                Value::Array(needles) => needles.iter().any(|n| if let Value::Str(ns) = n { text.starts_with(ns.as_str()) } else { false }),
                Value::Str(p) => text.starts_with(p.as_str()),
                _ => false,
            }))
        }
        BuiltinId::EndsWith => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ends_with".into() }); }
            let text = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ends_with")) };
            Ok(Value::Bool(match read(1)? {
                Value::Array(needles) => needles.iter().any(|n| if let Value::Str(ns) = n { text.ends_with(ns.as_str()) } else { false }),
                Value::Str(p) => text.ends_with(p.as_str()),
                _ => false,
            }))
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

        BuiltinId::Before => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "before".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sep)) => Ok(Value::Str(match s.find(sep.as_str()) {
                    Some(i) => s[..i].to_string(),
                    None    => s,
                })),
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "before")),
            }
        }
        BuiltinId::After => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "after".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sep)) => Ok(Value::Str(match s.find(sep.as_str()) {
                    Some(i) => s[i + sep.len()..].to_string(),
                    None    => String::new(),
                })),
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "after")),
            }
        }
        BuiltinId::BeforeLast => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "before_last".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sep)) => Ok(Value::Str(match s.rfind(sep.as_str()) {
                    Some(i) => s[..i].to_string(),
                    None    => s,
                })),
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "before_last")),
            }
        }
        BuiltinId::AfterLast => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "after_last".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(s), Value::Str(sep)) => Ok(Value::Str(match s.rfind(sep.as_str()) {
                    Some(i) => s[i + sep.len()..].to_string(),
                    None    => String::new(),
                })),
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "after_last")),
            }
        }
        BuiltinId::KeepBefore => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "keep_before".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(text), Value::Str(delim)) => {
                    if delim.is_empty() { return Ok(Value::Str(text)); }
                    Ok(Value::Str(match text.find(delim.as_str()) {
                        Some(pos) => text[..pos].to_string(),
                        None => text,
                    }))
                }
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "keep_before")),
            }
        }
        BuiltinId::KeepAfter => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "keep_after".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(text), Value::Str(delim)) => {
                    if delim.is_empty() { return Ok(Value::Str(String::new())); }
                    Ok(Value::Str(match text.find(delim.as_str()) {
                        Some(pos) => { let start = pos + delim.len(); if start <= text.len() { text[start..].to_string() } else { String::new() } }
                        None => String::new(),
                    }))
                }
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "keep_after")),
            }
        }
        BuiltinId::KeepBetween => {
            if args.len() < 3 || args.len() > 4 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "keep_between".into() }); }
            let text  = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "keep_between")) };
            let open  = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "keep_between open")) };
            let close = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "keep_between close")) };
            let mut include_delims = false;
            let mut allow_eof_close = true;
            if args.len() == 4 {
                if let Value::Map(m) = read(3)? {
                    if let Some(Value::Bool(b)) = m.get("include_delims") { include_delims = *b; }
                    if let Some(Value::Bool(b)) = m.get("allow_eof_close") { allow_eof_close = *b; }
                }
            }
            if open.is_empty() || close.is_empty() { return Ok(Value::Str(String::new())); }
            let open_pos = match text.find(open.as_str()) {
                Some(p) => p,
                None => return Ok(Value::Str(String::new())),
            };
            let search_start = open_pos + open.len();
            let close_pos = match text[search_start..].find(close.as_str()) {
                Some(p) => search_start + p,
                None => if allow_eof_close { text.len() } else { return Ok(Value::Str(String::new())); }
            };
            let out = if include_delims {
                let end = if close_pos < text.len() { close_pos + close.len() } else { text.len() };
                text.get(open_pos..end).unwrap_or("").to_string()
            } else {
                text.get(search_start..close_pos).unwrap_or("").to_string()
            };
            Ok(Value::Str(out))
        }
        BuiltinId::SanitizeBom => {
            expect_n(1)?;
            let s = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "sanitize_bom")) };
            let out = if s.starts_with('\u{FEFF}') {
                s.trim_start_matches('\u{FEFF}').to_string()
            } else {
                let bytes = s.as_bytes();
                if bytes.len() >= 3 && bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF {
                    s[3..].to_string()
                } else { s }
            };
            Ok(Value::Str(out))
        }
        BuiltinId::NormalizeNewlines => {
            expect_n(1)?;
            let s = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "normalize_newlines")) };
            Ok(Value::Str(s.replace("\r\n", "\n").replace("\r", "\n")))
        }
        BuiltinId::IgnoreWhere => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ignore_where".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(text), Value::Str(needle)) => {
                    Ok(Value::Str(if needle.is_empty() { text } else { text.replace(needle.as_str(), "") }))
                }
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "ignore_where")),
            }
        }
        BuiltinId::IgnoreLinesWhere => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ignore_lines_where".into() }); }
            match (read(0)?, read(1)?) {
                (Value::Str(text), Value::Str(prefix)) => {
                    if prefix.is_empty() { return Ok(Value::Str(text)); }
                    let mut out = String::with_capacity(text.len());
                    for line in text.split_inclusive('\n') {
                        let no_nl = line.strip_suffix('\n').unwrap_or(line);
                        if !no_nl.starts_with(prefix.as_str()) { out.push_str(line); }
                    }
                    Ok(Value::Str(out))
                }
                (a, _) => Err(GoblinError::type_error("str", a.type_name(), "ignore_lines_where")),
            }
        }

        BuiltinId::IsMatching => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "is_matching".into() }); }
            let text    = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "is_matching")) };
            let pattern = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "is_matching pattern")) };
            let re = regex::Regex::new(&pattern).map_err(|e| GoblinError::Runtime(format!("is_matching: invalid regex: {e}")))?;
            Ok(Value::Bool(re.is_match(&text)))
        }
        BuiltinId::CountMatching => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "count_matching".into() }); }
            let text    = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "count_matching")) };
            let pattern = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "count_matching pattern")) };
            let re = regex::Regex::new(&pattern).map_err(|e| GoblinError::Runtime(format!("count_matching: invalid regex: {e}")))?;
            Ok(Value::Int(re.find_iter(&text).count() as i64))
        }
        BuiltinId::IgnoreMatching => {
            if args.len() < 2 || args.len() > 3 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ignore_matching".into() }); }
            let text    = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_matching")) };
            let pattern = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_matching pattern")) };
            let pat = if args.len() == 3 { regex_with_flags(&pattern, &read(2)?) } else { pattern };
            let re = regex::Regex::new(&pat).map_err(|e| GoblinError::Runtime(format!("ignore_matching: invalid regex: {e}")))?;
            Ok(Value::Str(re.replace_all(&text, "").to_string()))
        }
        BuiltinId::IgnoreLinesMatching => {
            if args.len() < 2 || args.len() > 3 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "ignore_lines_matching".into() }); }
            let text    = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_lines_matching")) };
            let pattern = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_lines_matching pattern")) };
            let pat = if args.len() == 3 { regex_with_flags(&pattern, &read(2)?) } else { pattern };
            let re = regex::Regex::new(&pat).map_err(|e| GoblinError::Runtime(format!("ignore_lines_matching: invalid regex: {e}")))?;
            let mut out = String::with_capacity(text.len());
            for line in text.split_inclusive('\n') {
                let no_nl = line.strip_suffix('\n').unwrap_or(line);
                if !re.is_match(no_nl) { out.push_str(line); }
            }
            Ok(Value::Str(out))
        }
        BuiltinId::KeepMatching => {
            if args.len() < 2 || args.len() > 3 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "keep_matching".into() }); }
            let text    = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "keep_matching")) };
            let pattern = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "keep_matching pattern")) };
            let pat = if args.len() == 3 { regex_with_flags(&pattern, &read(2)?) } else { pattern };
            let re = regex::Regex::new(&pat).map_err(|e| GoblinError::Runtime(format!("keep_matching: invalid regex: {e}")))?;
            let mut out = String::new();
            for m in re.find_iter(&text) { out.push_str(m.as_str()); }
            Ok(Value::Str(out))
        }

        BuiltinId::JsonParse => {
            expect_n(1)?;
            let s = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "json_parse")) };
            let jv: serde_json::Value = serde_json::from_str(&s).map_err(|e| GoblinError::Runtime(format!("json_parse failed: {e}")))?;
            Ok(json_to_value(&jv))
        }
        BuiltinId::JsonStringify => {
            expect_n(1)?;
            let v = read(0)?;
            let jv = value_to_json(&v);
            Ok(Value::Str(serde_json::to_string(&jv).unwrap_or_default()))
        }
        BuiltinId::JsonStringifyPretty => {
            expect_n(1)?;
            let v = read(0)?;
            let jv = value_to_json(&v);
            Ok(Value::Str(serde_json::to_string_pretty(&jv).unwrap_or_default()))
        }

        BuiltinId::IgnoreBetween => {
            if args.len() < 3 || args.len() > 4 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "ignore_between".into() }); }
            let text  = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_between")) };
            let open  = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_between open")) };
            let close = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_between close")) };
            let mut include_delims = true;
            let mut allow_nested = false;
            let mut allow_eof_close = true;
            if args.len() == 4 {
                match read(3)? {
                    Value::Nil => {}
                    Value::Map(m) => {
                        if let Some(Value::Bool(b)) = m.get("include_delims") { include_delims = *b; }
                        if let Some(Value::Bool(b)) = m.get("allow_nested") { allow_nested = *b; }
                        if let Some(Value::Bool(b)) = m.get("allow_eof_close") { allow_eof_close = *b; }
                    }
                    other => return Err(GoblinError::type_error("map or nil", other.type_name(), "ignore_between opts")),
                }
            }
            if open.is_empty() || close.is_empty() { return Ok(Value::Str(text)); }
            let mut out = String::with_capacity(text.len());
            let mut i = 0usize;
            while i < text.len() {
                if let Some(rel) = text[i..].find(open.as_str()) {
                    let start = i + rel;
                    out.push_str(&text[i..start]);
                    let mut k = start + open.len();
                    let mut depth = 1usize;
                    let mut close_pos: Option<usize> = None;
                    while k <= text.len() {
                        let next_open  = text[k..].find(open.as_str()).map(|r| k + r);
                        let next_close = text[k..].find(close.as_str()).map(|r| k + r);
                        match (next_open, next_close) {
                            (_, None) => { if allow_eof_close { close_pos = Some(text.len()); } break; }
                            (None, Some(c)) => { close_pos = Some(c); break; }
                            (Some(o), Some(c)) => {
                                if allow_nested && o < c { depth += 1; k = o + open.len(); }
                                else { close_pos = Some(c); break; }
                            }
                        }
                        if close_pos.is_some() && allow_nested && depth > 1 {
                            depth -= 1;
                            k = close_pos.unwrap() + close.len();
                            close_pos = None;
                        }
                    }
                    if let Some(cpos) = close_pos {
                        if include_delims {
                            i = cpos + close.len();
                        } else {
                            out.push_str(&text[start..start + open.len()]);
                            out.push_str(&text[cpos..cpos + close.len()]);
                            i = cpos + close.len();
                        }
                    } else {
                        out.push_str(&text[start..]);
                        break;
                    }
                } else {
                    out.push_str(&text[i..]);
                    break;
                }
            }
            Ok(Value::Str(out))
        }
        BuiltinId::IgnoreBlocks => {
            let n = args.len();
            if n < 3 || n > 4 { return Err(GoblinError::ArityMismatch { expected: 3, got: n, name: "ignore_blocks".into() }); }
            let text  = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks")) };
            let open  = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks open")) };
            let close = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks close")) };
            let mut include_delims  = true;
            let mut require_bol     = true;
            let mut leading_blanks  = true;
            let mut allow_eof_close = true;
            if n == 4 {
                match read(3)? {
                    Value::Nil => {}
                    Value::Map(m) => {
                        if let Some(Value::Bool(b)) = m.get("include_delims")    { include_delims  = *b; }
                        if let Some(Value::Bool(b)) = m.get("require_bol")       { require_bol     = *b; }
                        if let Some(Value::Bool(b)) = m.get("leading_blanks_ok") { leading_blanks  = *b; }
                        if let Some(Value::Bool(b)) = m.get("allow_eof_close")   { allow_eof_close = *b; }
                    }
                    other => return Err(GoblinError::type_error("map or nil", other.type_name(), "ignore_blocks opts")),
                }
            }
            if open.is_empty() || close.is_empty() { return Ok(Value::Str(text)); }
            let bytes = text.as_bytes();
            let mut out = String::with_capacity(text.len());
            let mut i: usize = 0;
            while i < text.len() {
                if let Some(rel) = text[i..].find(&*open) {
                    let abs = i + rel;
                    let at_bol = if abs == 0 { true } else {
                        let mut k = abs;
                        if leading_blanks { while k > 0 && bytes[k-1] != b'\n' && (bytes[k-1] == b' ' || bytes[k-1] == b'\t') { k -= 1; } }
                        k == 0 || bytes[k-1] == b'\n'
                    };
                    if !require_bol || at_bol {
                        let mut j = abs + open.len();
                        let mut close_pos: Option<usize> = None;
                        while j <= text.len() {
                            if let Some(relc) = text[j..].find(&*close) {
                                let cabs = j + relc;
                                let c_at_bol = if cabs == 0 { true } else {
                                    let mut k = cabs;
                                    if leading_blanks { while k > 0 && bytes[k-1] != b'\n' && (bytes[k-1] == b' ' || bytes[k-1] == b'\t') { k -= 1; } }
                                    k == 0 || bytes[k-1] == b'\n'
                                };
                                if !require_bol || c_at_bol { close_pos = Some(cabs); break; } else { j = cabs + 1; }
                            } else {
                                if allow_eof_close { close_pos = Some(text.len()); }
                                break;
                            }
                        }
                        out.push_str(&text[i..abs]);
                        if let Some(cpos) = close_pos {
                            if include_delims {
                                i = cpos + close.len();
                            } else {
                                out.push_str(&text[abs..abs+open.len()]);
                                out.push_str(&text[cpos..cpos+close.len()]);
                                i = cpos + close.len();
                            }
                        } else {
                            out.push_str(&text[abs..]);
                            break;
                        }
                        continue;
                    } else {
                        out.push_str(&text[i..abs]);
                        i = abs + 1;
                        continue;
                    }
                } else {
                    out.push_str(&text[i..]);
                    break;
                }
            }
            Ok(Value::Str(out))
        }
        BuiltinId::Env => {
            expect_n(1)?;
            let name = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "env")) };
            Ok(Value::Str(std::env::var(&name).unwrap_or_default()))
        }

        BuiltinId::Pct => {
            expect_n(1)?;
            match read(0)? {
                Value::Pct(p)   => Ok(Value::Pct(p)),
                Value::Int(n)   => Ok(Value::Pct(n as f64)),
                Value::Float(f) => Ok(Value::Pct(f)),
                Value::Str(s) => {
                    let trimmed = s.trim();
                    let cleaned: String = trimmed.chars().filter(|&c| c != '_').collect();
                    if cleaned.ends_with('%') {
                        let num = cleaned[..cleaned.len()-1].trim();
                        if let Ok(f) = num.parse::<f64>() {
                            return Ok(Value::Pct(f / 100.0));
                        }
                    } else if let Ok(f) = cleaned.parse::<f64>() {
                        return Ok(Value::Pct(f));
                    }
                    Err(GoblinError::Runtime(format!("pct: invalid string '{}'", s)))
                }
                other => Err(GoblinError::type_error("number or str", other.type_name(), "pct")),
            }
        }

        BuiltinId::Between => {
            if args.len() != 3 { return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "between".into() }); }
            let s     = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "between")) };
            let left  = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "between left")) };
            let right = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "between right")) };
            if let Some(i) = s.find(&*left) {
                let jstart = i + left.len();
                if let Some(jrel) = s[jstart..].find(&*right) {
                    return Ok(Value::Str(s[jstart..jstart+jrel].to_string()));
                }
            }
            Ok(Value::Str(String::new()))
        }

        BuiltinId::IsControl => {
            expect_n(1)?;
            let v = read(0)?;
            Ok(Value::Bool(matches!(v, Value::Nil)))
        }

        BuiltinId::IgnoreBlocksFirst => {
            let n = args.len();
            if n < 3 || n > 4 { return Err(GoblinError::ArityMismatch { expected: 3, got: n, name: "ignore_blocks_first".into() }); }
            let text  = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks_first")) };
            let open  = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks_first open")) };
            let close = match read(2)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "ignore_blocks_first close")) };
            let mut include_delims  = true;
            let mut require_bol     = true;
            let mut leading_blanks  = true;
            let mut allow_eof_close = true;
            if n == 4 {
                if let Value::Map(m) = read(3)? {
                    if let Some(Value::Bool(b)) = m.get("include_delims")    { include_delims  = *b; }
                    if let Some(Value::Bool(b)) = m.get("require_bol")       { require_bol     = *b; }
                    if let Some(Value::Bool(b)) = m.get("leading_blanks_ok") { leading_blanks  = *b; }
                    if let Some(Value::Bool(b)) = m.get("allow_eof_close")   { allow_eof_close = *b; }
                }
            }
            if open.is_empty() || close.is_empty() { return Ok(Value::Str(text)); }
            let bytes = text.as_bytes();
            let mut i = 0usize;
            let mut open_pos: Option<usize> = None;
            while i < text.len() {
                if let Some(rel) = text[i..].find(&*open) {
                    let abs = i + rel;
                    let at_bol = if abs == 0 { true } else {
                        let mut k = abs;
                        if leading_blanks { while k > 0 && bytes[k-1] != b'\n' && (bytes[k-1] == b' ' || bytes[k-1] == b'\t') { k -= 1; } }
                        k == 0 || bytes[k-1] == b'\n'
                    };
                    if !require_bol || at_bol { open_pos = Some(abs); break; } else { i = abs + 1; }
                } else { break; }
            }
            let abs = match open_pos { Some(v) => v, None => return Ok(Value::Str(text)) };
            let mut j = abs + open.len();
            let mut close_pos: Option<usize> = None;
            while j <= text.len() {
                if let Some(relc) = text[j..].find(&*close) {
                    let cabs = j + relc;
                    let c_at_bol = if cabs == 0 { true } else {
                        let mut k = cabs;
                        if leading_blanks { while k > 0 && bytes[k-1] != b'\n' && (bytes[k-1] == b' ' || bytes[k-1] == b'\t') { k -= 1; } }
                        k == 0 || bytes[k-1] == b'\n'
                    };
                    if !require_bol || c_at_bol { close_pos = Some(cabs); break; } else { j = cabs + 1; }
                } else { if allow_eof_close { close_pos = Some(text.len()); } break; }
            }
            let cpos = match close_pos { Some(v) => v, None => return Ok(Value::Str(text)) };
            let mut out = String::with_capacity(text.len());
            out.push_str(&text[..abs]);
            if include_delims {
                out.push_str(&text[cpos + close.len()..]);
            } else {
                out.push_str(&text[abs..abs+open.len()]);
                out.push_str(&text[cpos..cpos+close.len()]);
                out.push_str(&text[cpos+close.len()..]);
            }
            Ok(Value::Str(out))
        }

        BuiltinId::Pick => {
            expect_n(1)?;
            let cfg = match read(0)? { Value::Map(m) => m, other => return Err(GoblinError::type_error("map", other.type_name(), "pick")) };
            let get_num = |k: &str| -> Option<f64> { match cfg.get(k)? { Value::Float(n) => Some(*n), Value::Int(i) => Some(*i as f64), Value::Str(s) => s.parse::<f64>().ok(), _ => None } };
            let count_f = get_num("count_expr").or_else(|| get_num("count")).unwrap_or(1.0);
            let n_out = count_f as usize;
            let allow_dups = match cfg.get("allow_dups") { Some(Value::Bool(b)) => *b, _ => false };
            // collection source
            let src = cfg.get("src");
            match src {
                Some(Value::Array(arr)) => {
                    if arr.is_empty() { return Err(GoblinError::Runtime("pick: empty src array".into())); }
                    if !allow_dups && n_out > arr.len() { return Err(GoblinError::Runtime(format!("pick: requested {} but only {} available", n_out, arr.len()))); }
                    let out: Vec<Value> = if allow_dups {
                        (0..n_out).map(|_| arr[rng_bounded(session, arr.len() as u64) as usize].clone()).collect()
                    } else {
                        let mut idxs: Vec<usize> = (0..arr.len()).collect();
                        let mut result = Vec::with_capacity(n_out);
                        for i in 0..n_out { let j = i + rng_bounded(session, (arr.len() - i) as u64) as usize; idxs.swap(i, j); result.push(arr[idxs[i]].clone()); }
                        result
                    };
                    if n_out == 1 { Ok(out.into_iter().next().unwrap_or(Value::Nil)) } else { Ok(Value::Array(out)) }
                }
                Some(Value::Map(map)) => {
                    let entries: Vec<(String, Value)> = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    if entries.is_empty() { return Err(GoblinError::Runtime("pick: empty src map".into())); }
                    if !allow_dups && n_out > entries.len() { return Err(GoblinError::Runtime(format!("pick: requested {} but only {} available", n_out, entries.len()))); }
                    let out: Vec<Value> = if allow_dups {
                        (0..n_out).map(|_| { let (k, v) = &entries[rng_bounded(session, entries.len() as u64) as usize]; let mut m = std::collections::BTreeMap::new(); m.insert(k.clone(), v.clone()); Value::Map(m) }).collect()
                    } else {
                        let mut idxs: Vec<usize> = (0..entries.len()).collect();
                        let mut result = Vec::with_capacity(n_out);
                        for i in 0..n_out { let j = i + rng_bounded(session, (entries.len() - i) as u64) as usize; idxs.swap(i, j); let (k, v) = &entries[idxs[i]]; let mut m = std::collections::BTreeMap::new(); m.insert(k.clone(), v.clone()); result.push(Value::Map(m)); }
                        result
                    };
                    if n_out == 1 { Ok(out.into_iter().next().unwrap_or(Value::Nil)) } else { Ok(Value::Array(out)) }
                }
                // numeric range
                _ => {
                    let range_start = get_num("range_start").unwrap_or(1.0) as i64;
                    let range_end   = get_num("range_end").unwrap_or(100.0) as i64;
                    if range_end <= range_start { return Err(GoblinError::Runtime("pick: range_end must be > range_start".into())); }
                    let range = (range_end - range_start) as u64;
                    let out: Vec<Value> = (0..n_out).map(|_| Value::Int(range_start + rng_bounded(session, range) as i64)).collect();
                    if n_out == 1 { Ok(out.into_iter().next().unwrap_or(Value::Nil)) } else { Ok(Value::Array(out)) }
                }
            }
        }

        BuiltinId::ReadJson => {
            expect_n(1)?;
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "read_json path")) };
            let txt = std::fs::read_to_string(&path).map_err(|e| GoblinError::Runtime(format!("read_json: {}", e)))?;
            let jv: serde_json::Value = serde_json::from_str(&txt).map_err(|e| GoblinError::Runtime(format!("read_json parse: {}", e)))?;
            Ok(json_to_value(&jv))
        }

        BuiltinId::WriteText => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "write_text".into() }); }
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "write_text path")) };
            let text = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "write_text text")) };
            std::fs::write(&path, text).map_err(|e| GoblinError::Runtime(format!("write_text: {}", e)))?;
            Ok(Value::Nil)
        }

        BuiltinId::AppendFile => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "append_file".into() }); }
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "append_file path")) };
            let text = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "append_file text")) };
            use std::io::Write;
            let mut file = std::fs::OpenOptions::new().create(true).append(true).open(&path).map_err(|e| GoblinError::Runtime(format!("append_file: {}", e)))?;
            file.write_all(text.as_bytes()).map_err(|e| GoblinError::Runtime(format!("append_file write: {}", e)))?;
            Ok(Value::Nil)
        }

        BuiltinId::WriteJson => {
            let n = args.len();
            if n < 2 || n > 3 { return Err(GoblinError::ArityMismatch { expected: 2, got: n, name: "write_json".into() }); }
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "write_json path")) };
            let val  = read(1)?;
            let pretty = if n == 3 { matches!(read(2)?, Value::Bool(true)) } else { false };
            let jv = value_to_json(&val);
            let text = if pretty { serde_json::to_string_pretty(&jv) } else { serde_json::to_string(&jv) }.map_err(|e| GoblinError::Runtime(format!("write_json: {}", e)))?;
            std::fs::write(&path, text).map_err(|e| GoblinError::Runtime(format!("write_json write: {}", e)))?;
            Ok(Value::Nil)
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
            let result = match &container {
                Value::Str(s) => match &needle {
                    Value::Str(sub) => s.contains(sub.as_str()),
                    Value::Char(c)  => s.contains(*c),
                    _ => return Err(GoblinError::type_error("str or char", needle.type_name(), "has needle for string")),
                },
                Value::Array(arr) => arr.iter().any(|x| x == &needle),
                Value::Seq(s)     => s.items.iter().any(|x| x == &needle),
                Value::Map(m) => {
                    let k = value_to_map_key(&needle);
                    m.contains_key(&k)
                }
                Value::MapOrd(m) => {
                    let k = value_to_map_key(&needle);
                    m.contains_key(&k)
                }
                Value::Collection(c) => collections::has(c, &needle),
                Value::Nil | Value::Unit => false,
                _ => return Err(GoblinError::type_error("string, array, or map", container.type_name(), "has")),
            };
            Ok(Value::Bool(result))
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
                Value::Str(s) => {
                    let mut v: Vec<char> = s.chars().collect();
                    let n = v.len();
                    for i in 0..n {
                        let j = i + rng_bounded(session, (n - i) as u64) as usize;
                        v.swap(i, j);
                    }
                    Ok(Value::Str(v.into_iter().collect()))
                }
                Value::Int(n) => {
                    // interpreter: explode digits, Fisher-Yates, repack with overflow check, preserve sign
                    let neg = n < 0;
                    let mut m = if neg { -(n as i128) } else { n as i128 };
                    let mut digs: Vec<i64> = if m == 0 { vec![0] } else {
                        let mut tmp = Vec::new();
                        while m > 0 { tmp.push((m % 10) as i64); m /= 10; }
                        tmp.reverse();
                        tmp
                    };
                    let nd = digs.len();
                    for i in 0..nd {
                        let j = i + rng_bounded(session, (nd - i) as u64) as usize;
                        digs.swap(i, j);
                    }
                    let mut acc: i128 = 0;
                    for d in digs {
                        match acc.checked_mul(10).and_then(|a| a.checked_add(d as i128)) {
                            Some(v) => acc = v,
                            None => return Ok(Value::Nil),
                        }
                    }
                    if neg { acc = -acc; }
                    Ok(Value::Int(acc as i64))
                }
                Value::Array(mut items) => {
                    fisher_yates_shuffle(&mut items, session);
                    Ok(Value::Array(items))
                }
                other => {
                    // Seq/Collection: treat as array-like
                    Err(GoblinError::type_error("string, int, or array", other.type_name(), "shuffle"))
                }
            }
        }
        BuiltinId::Sort => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => {
                    let mut v: Vec<char> = s.chars().collect();
                    v.sort_unstable();
                    Ok(Value::Str(v.into_iter().collect()))
                }
                Value::Int(n) => {
                    // interpreter: sort digits ascending, preserve sign, repack with overflow check
                    let neg = n < 0;
                    let mut m = if neg { -(n as i128) } else { n as i128 };
                    let mut digs: Vec<i64> = if m == 0 { vec![0] } else {
                        let mut tmp = Vec::new();
                        while m > 0 { tmp.push((m % 10) as i64); m /= 10; }
                        tmp.reverse();
                        tmp
                    };
                    digs.sort_unstable();
                    let mut acc: i128 = 0;
                    for d in digs {
                        match acc.checked_mul(10).and_then(|a| a.checked_add(d as i128)) {
                            Some(v) => acc = v,
                            None => return Ok(Value::Nil),
                        }
                    }
                    if neg { acc = -acc; }
                    Ok(Value::Int(acc as i64))
                }
                Value::Array(mut items) => {
                    items.sort_by(|a, b| fmt_value_raw(a).cmp(&fmt_value_raw(b)));
                    Ok(Value::Array(items))
                }
                Value::Collection(c) => Ok(collections::sort_values(&c)),
                other => Err(GoblinError::type_error("str, int, or array", other.type_name(), "sort")),
            }
        }
        BuiltinId::Freq => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => {
                    let mut cnt = std::collections::BTreeMap::<char, i64>::new();
                    for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
                    let mut m = std::collections::BTreeMap::<String, Value>::new();
                    for (c, n) in cnt { m.insert(c.to_string(), Value::Int(n)); }
                    Ok(Value::Map(m))
                }
                Value::Array(items) => {
                    let mut tally = std::collections::BTreeMap::<String, i64>::new();
                    for item in &items { *tally.entry(fmt_value_raw(item)).or_insert(0) += 1; }
                    let mut m = std::collections::BTreeMap::<String, Value>::new();
                    for (k, n) in tally { m.insert(k, Value::Int(n)); }
                    Ok(Value::Map(m))
                }
                other => Err(GoblinError::type_error("string or array", other.type_name(), "freq")),
            }
        }
        BuiltinId::Mode => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(items) => {
                    if items.is_empty() {
                        return Err(GoblinError::Runtime("mode: empty array".into()));
                    }
                    let mut counts = std::collections::BTreeMap::<String, i64>::new();
                    for v in &items { *counts.entry(fmt_value_raw(v)).or_insert(0) += 1; }
                    let mut best_k = String::new();
                    let mut best_n = -1i64;
                    for (k, n) in &counts { if *n > best_n { best_n = *n; best_k = k.clone(); } }
                    let mut m = std::collections::BTreeMap::<String, Value>::new();
                    m.insert(best_k, Value::Int(best_n));
                    Ok(Value::Map(m))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "mode")),
            }
        }
        BuiltinId::SampleWeighted => {
            expect_n(1)?;
            let cfg = match read(0)? {
                Value::Map(m) => m,
                other => return Err(GoblinError::type_error("map config", other.type_name(), "sample_weighted")),
            };
            let src_val = cfg.get("src").cloned().ok_or_else(|| GoblinError::Runtime("sample_weighted: missing 'src'".into()))?;
            let xs: Vec<Value> = match src_val {
                Value::Array(a) => a,
                other => return Err(GoblinError::type_error("array", other.type_name(), "sample_weighted src")),
            };
            let wt_val = cfg.get("weights").cloned().ok_or_else(|| GoblinError::Runtime("sample_weighted: missing 'weights'".into()))?;
            let ws: Vec<Value> = match wt_val {
                Value::Array(a) => a,
                other => return Err(GoblinError::type_error("array", other.type_name(), "sample_weighted weights")),
            };
            if xs.is_empty() { return Err(GoblinError::Runtime("sample_weighted: src is empty".into())); }
            if xs.len() != ws.len() { return Err(GoblinError::Runtime(format!("sample_weighted: src len {} != weights len {}", xs.len(), ws.len()))); }
            let n_out: usize = match cfg.get("count") {
                None => 1,
                Some(Value::Int(i)) if *i > 0 => *i as usize,
                Some(Value::Float(f)) if *f > 0.0 && f.fract() == 0.0 => *f as usize,
                _ => return Err(GoblinError::Runtime("sample_weighted: count must be positive int".into())),
            };
            let mut cum: Vec<f64> = Vec::with_capacity(ws.len());
            let mut sum = 0.0f64;
            for w in &ws {
                let wf = match w {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    other => return Err(GoblinError::type_error("number", other.type_name(), "sample_weighted weight")),
                };
                if wf < 0.0 { return Err(GoblinError::Runtime("sample_weighted: weights must be >= 0".into())); }
                sum += wf;
                cum.push(sum);
            }
            if sum == 0.0 { return Err(GoblinError::Runtime("sample_weighted: all weights are zero".into())); }
            let mut out = Vec::with_capacity(n_out);
            for _ in 0..n_out {
                let r = rng_u01(session) * sum;
                let mut lo = 0usize;
                let mut hi = cum.len();
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    if r < cum[mid] { hi = mid; } else { lo = mid + 1; }
                }
                out.push(xs[lo].clone());
            }
            Ok(Value::Array(out))
        }
        BuiltinId::Map => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "map".into() }); }
            let action = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str (action name)", other.type_name(), "map")) };
            let bid = crate::compiler::builtin_by_name(&action)
                .ok_or_else(|| GoblinError::Runtime(format!("map: unknown action '{}'", action)))?;
            match read(0)? {
                Value::Str(s) => {
                    let mut out = String::new();
                    let mut any_non_text = false;
                    let mut results = vec![];
                    for c in s.chars() {
                        let t = session.alloc_value(Value::Char(c));
                        let rt = call_builtin(bid, vec![t], session)?;
                        let v = session.read_value(&rt)?;
                        match &v { Value::Char(_) | Value::Str(_) => {} _ => { any_non_text = true; } }
                        results.push(v);
                    }
                    if !any_non_text {
                        for r in results { match r { Value::Char(c) => out.push(c), Value::Str(ts) => out.push_str(&ts), _ => {} } }
                        Ok(Value::Str(out))
                    } else {
                        Ok(Value::Array(results))
                    }
                }
                Value::Array(xs) => {
                    let mut out = vec![];
                    for v in xs {
                        let t = session.alloc_value(v);
                        let rt = call_builtin(bid, vec![t], session)?;
                        out.push(session.read_value(&rt)?);
                    }
                    Ok(Value::Array(out))
                }
                other => Err(GoblinError::type_error("str or array", other.type_name(), "map")),
            }
        }
        BuiltinId::Unique => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => {
                    let mut seen = std::collections::BTreeSet::new();
                    let mut out = String::new();
                    for c in s.chars() { if seen.insert(c) { out.push(c); } }
                    Ok(Value::Str(out))
                }
                Value::Array(items) => {
                    let mut seen = std::collections::BTreeSet::new();
                    let result: Vec<Value> = items.into_iter().filter(|item| seen.insert(fmt_value_raw(item))).collect();
                    Ok(Value::Array(result))
                }
                Value::Collection(c) => Ok(collections::unique(&c)),
                other => Err(GoblinError::type_error("string or array", other.type_name(), "unique")),
            }
        }
        BuiltinId::Dups => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => {
                    let mut cnt = std::collections::BTreeMap::<char, usize>::new();
                    for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
                    let mut out = String::new();
                    for (c, n) in cnt { if n >= 2 { out.push(c); } }
                    Ok(Value::Str(out))
                }
                Value::Array(items) => {
                    let mut cnt = std::collections::BTreeMap::<String, (usize, Value)>::new();
                    for item in &items {
                        let k = fmt_value_raw(item);
                        cnt.entry(k).and_modify(|e| e.0 += 1).or_insert((1, item.clone()));
                    }
                    let mut out = Vec::new();
                    for (_, (n, exemplar)) in cnt { if n >= 2 { out.push(exemplar); } }
                    Ok(Value::Array(out))
                }
                other => Err(GoblinError::type_error("string or array", other.type_name(), "dups")),
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
        BuiltinId::DeleteWhere => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete_where".into() }); }
            let pred = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "delete_where pred")) };
            let is_ident = pred.chars().all(|c| c.is_alphanumeric() || c == '_');
            match read(0)? {
                Value::Array(mut xs) => {
                    xs.retain(|v| {
                        if !is_ident {
                            fmt_value_raw(v) != pred
                        } else if let Some(bid) = crate::compiler::builtin_by_name(&pred) {
                            let t = session.alloc_value(v.clone());
                            match call_builtin(bid, vec![t], session) {
                                Ok(rt) => !matches!(session.read_value(&rt), Ok(Value::Bool(true))),
                                _ => true,
                            }
                        } else { true }
                    });
                    Ok(Value::Array(xs))
                }
                Value::Map(mut m) => {
                    m.retain(|_, v| {
                        if !is_ident {
                            fmt_value_raw(v) != pred
                        } else if let Some(bid) = crate::compiler::builtin_by_name(&pred) {
                            let t = session.alloc_value(v.clone());
                            match call_builtin(bid, vec![t], session) {
                                Ok(rt) => !matches!(session.read_value(&rt), Ok(Value::Bool(true))),
                                _ => true,
                            }
                        } else { true }
                    });
                    Ok(Value::Map(m))
                }
                other => Err(GoblinError::type_error("array or map", other.type_name(), "delete_where")),
            }
        }
        BuiltinId::DeleteAll => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(_) => Ok(Value::Array(vec![])),
                Value::Map(_) => Ok(Value::Map(std::collections::BTreeMap::new())),
                Value::MapOrd(_) => Ok(Value::MapOrd(indexmap::IndexMap::new())),
                other => Err(GoblinError::type_error("array or map", other.type_name(), "delete_all")),
            }
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
        BuiltinId::ReapWhere => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_where".into() }); }
            let pred = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "reap_where pred")) };
            let is_ident = pred.chars().all(|c| c.is_alphanumeric() || c == '_');
            let matches_pred = |v: &Value, session: &mut Session| -> bool {
                if !is_ident { return fmt_value_raw(v) == pred; }
                if let Some(bid) = crate::compiler::builtin_by_name(&pred) {
                    let t = session.alloc_value(v.clone());
                    match call_builtin(bid, vec![t], session) {
                        Ok(rt) => matches!(session.read_value(&rt), Ok(Value::Bool(true))),
                        _ => false,
                    }
                } else { false }
            };
            match read(0)? {
                Value::Array(mut xs) => {
                    let mut reaped = vec![];
                    xs.retain(|v| { if matches_pred(v, session) { reaped.push(v.clone()); false } else { true } });
                    Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![Value::Array(reaped), Value::Array(xs)]))))
                }
                other => Err(GoblinError::type_error("array", other.type_name(), "reap_where")),
            }
        }
        BuiltinId::ReapAll => {
            expect_n(1)?;
            match read(0)? {
                Value::Array(xs) => Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![Value::Array(xs), Value::Array(vec![])])))),
                other => Err(GoblinError::type_error("array", other.type_name(), "reap_all")),
            }
        }

        BuiltinId::ReapSample => {
            expect_n(1)?;
            let cfg = match read(0)? { Value::Map(m) => m, other => return Err(GoblinError::type_error("map", other.type_name(), "reap")) };
            let n_out: usize = match cfg.get("count") { None => 1, Some(Value::Int(n)) if *n > 0 => *n as usize, _ => return Err(GoblinError::Runtime("reap: count must be a positive integer".into())) };
            match cfg.get("src") {
                Some(Value::Array(arr)) => {
                    if arr.is_empty() { return Err(GoblinError::Runtime("reap: empty src".into())); }
                    if n_out > arr.len() { return Err(GoblinError::Runtime(format!("reap: requested {} but only {} available", n_out, arr.len()))); }
                    let mut idxs: Vec<usize> = (0..arr.len()).collect();
                    let mut items = Vec::with_capacity(n_out);
                    for i in 0..n_out { let j = i + rng_bounded(session, (arr.len() - i) as u64) as usize; idxs.swap(i, j); items.push(arr[idxs[i]].clone()); }
                    if n_out == 1 { Ok(items.pop().unwrap()) } else { Ok(Value::Array(items)) }
                }
                Some(Value::Map(map)) => {
                    let entries: Vec<(String, Value)> = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    if entries.is_empty() { return Err(GoblinError::Runtime("reap: empty src map".into())); }
                    if n_out > entries.len() { return Err(GoblinError::Runtime(format!("reap: requested {} but only {} available", n_out, entries.len()))); }
                    let mut idxs: Vec<usize> = (0..entries.len()).collect();
                    let mut items = Vec::with_capacity(n_out);
                    for i in 0..n_out { let j = i + rng_bounded(session, (entries.len() - i) as u64) as usize; idxs.swap(i, j); let (k, v) = &entries[idxs[i]]; let mut m = std::collections::BTreeMap::new(); m.insert(k.clone(), v.clone()); items.push(Value::Map(m)); }
                    if n_out == 1 { Ok(items.pop().unwrap()) } else { Ok(Value::Array(items)) }
                }
                _ => Err(GoblinError::Runtime("reap: src must be an array or map".into())),
            }
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
                other => { let c = require_collection(other, "reverse")?; Ok(collections::reverse(&*c)) }
            }
        }
        BuiltinId::ReverseChars => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| s.chars().rev().collect())
        }
        BuiltinId::Minimize => {
            expect_n(1)?;
            map_str_1(&read(0)?, &|s: &str| {
                let mut out = String::new();
                let mut in_ws = false;
                for ch in s.chars() {
                    if ch.is_whitespace() {
                        if !in_ws { out.push(' '); in_ws = true; }
                    } else { in_ws = false; out.push(ch); }
                }
                out.trim().to_string()
            })
        }
        BuiltinId::ParseBool => {
            expect_n(1)?;
            let s = match read(0)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("str", other.type_name(), "parse_bool")),
            };
            match s.to_ascii_lowercase().as_str() {
                "true"  => Ok(Value::Bool(true)),
                "false" => Ok(Value::Bool(false)),
                _ => Err(GoblinError::Runtime(format!("parse_bool: expected \"true\" or \"false\", got \"{}\"", s))),
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
        BuiltinId::IsBig        => { expect_n(1)?; Ok(Value::Bool(false)) } // no Big in VM
        BuiltinId::IsPct        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Pct(_)))) }
        BuiltinId::IsNum        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Int(_) | Value::Float(_) | Value::Pct(_)))) }
        BuiltinId::IsChar       => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Char(_)))) }
        BuiltinId::IsPair       => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Pair(_, _)))) }
        BuiltinId::IsSeq        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Seq(_)))) }
        BuiltinId::IsUnit       => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Unit))) }
        BuiltinId::IsAlnum      => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Char(c) => c.is_ascii_alphanumeric(),
                Value::Str(s)  => !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric()),
                _ => false,
            }))
        }
        BuiltinId::IsAlpha      => {
            expect_n(1)?;
            let is_ascii_alpha = |c: char| (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
            Ok(Value::Bool(match read(0)? {
                Value::Char(c) => is_ascii_alpha(c),
                Value::Str(s)  => !s.is_empty() && s.chars().all(is_ascii_alpha),
                _ => false,
            }))
        }
        BuiltinId::IsDigit      => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Char(c) => c >= '0' && c <= '9',
                Value::Str(s)  => !s.is_empty() && s.chars().all(|c| c >= '0' && c <= '9'),
                _ => false,
            }))
        }
        BuiltinId::IsWhitespace => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Char(c) => c.is_ascii_whitespace(),
                Value::Str(s)  => !s.is_empty() && s.chars().all(|c| c.is_ascii_whitespace()),
                _ => false,
            }))
        }
        BuiltinId::IsEven => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Int(n) => n % 2 == 0,
                Value::Float(f) if f.is_finite() && f.fract() == 0.0 => (f as i64) % 2 == 0,
                _ => false,
            }))
        }
        BuiltinId::IsOdd => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Int(n) => n % 2 != 0,
                Value::Float(f) if f.is_finite() && f.fract() == 0.0 => (f as i64) % 2 != 0,
                _ => false,
            }))
        }
        BuiltinId::IsMultipleOf => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "is_multiple_of".into() }); }
            Ok(Value::Bool(match (read(0)?, read(1)?) {
                (Value::Int(x), Value::Int(k)) => k != 0 && x % k == 0,
                (Value::Float(xf), Value::Float(kf)) if xf.fract() == 0.0 && kf.fract() == 0.0 => {
                    let k = kf as i64; k != 0 && (xf as i64) % k == 0
                }
                (Value::Float(xf), Value::Int(k)) if xf.fract() == 0.0 => k != 0 && (xf as i64) % k == 0,
                (Value::Int(x), Value::Float(kf)) if kf.fract() == 0.0 => { let k = kf as i64; k != 0 && x % k == 0 }
                _ => false,
            }))
        }
        BuiltinId::IsPositive => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Int(n)   => n > 0,
                Value::Float(f) => f.is_finite() && f > 0.0,
                Value::Pct(p)   => p > 0.0,
                _ => false,
            }))
        }
        BuiltinId::IsNegative => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Int(n)   => n < 0,
                Value::Float(f) => f.is_finite() && f < 0.0,
                Value::Pct(p)   => p < 0.0,
                _ => false,
            }))
        }
        BuiltinId::IsNix => {
            expect_n(1)?;
            Ok(Value::Bool(match read(0)? {
                Value::Nil => true,
                Value::Str(s) => s.is_empty() || s.chars().all(|c| c.is_whitespace()),
                Value::Array(a) => a.is_empty(),
                Value::Map(m) => m.is_empty(),
                Value::MapOrd(m) => m.is_empty(),
                _ => false,
            }))
        }

        // ── Conversions ───────────────────────────────────────────────────────
        BuiltinId::ToInt => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) | Value::Pct(f) => Value::Int(f.trunc() as i64),
                Value::Bool(b)  => Value::Int(b as i64),
                Value::Char(c)  => Value::Int(c as u32 as i64),
                Value::Str(s)   => {
                    let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
                    cleaned.parse::<i64>()
                        .map(Value::Int)
                        .unwrap_or_else(|_| cleaned.parse::<f64>()
                            .map(|f| Value::Int(f.trunc() as i64))
                            .unwrap_or(Value::Nil))
                }
                Value::Nil => Value::Nil,
                other => return Err(GoblinError::type_error("number or str", other.type_name(), "to_int")),
            })
        }
        BuiltinId::ToFloat => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Float(f) => Value::Float(f),
                Value::Pct(f)   => Value::Float(f),
                Value::Int(n)   => Value::Float(n as f64),
                Value::Bool(b)  => Value::Float(b as i64 as f64),
                Value::Str(s)   => {
                    let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
                    Value::Float(cleaned.parse::<f64>()
                        .map_err(|_| GoblinError::Runtime(format!("cannot convert {:?} to float", s)))?)
                }
                Value::Nil => Value::Nil,
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
        BuiltinId::RandSeed => {
            expect_n(1)?;
            // interpreter just seeds the RNG — in VM we ignore since session handles it
            Ok(Value::Nil)
        }
        BuiltinId::Roll => {
            expect_n(1)?;
            let cfg = match read(0)? {
                Value::Map(m) => m,
                other => return Err(GoblinError::type_error("map config", other.type_name(), "roll")),
            };
            let cast_i64 = |v: &Value| -> Option<i64> {
                match v { Value::Int(i) => Some(*i), Value::Float(f) => Some(f.trunc() as i64), _ => None }
            };
            let count  = cast_i64(cfg.get("count").ok_or_else(|| GoblinError::Runtime("roll: missing 'count'".into()))?)
                .ok_or_else(|| GoblinError::Runtime("roll: 'count' must be integer-like".into()))?;
            let sides  = cast_i64(cfg.get("sides").ok_or_else(|| GoblinError::Runtime("roll: missing 'sides'".into()))?)
                .ok_or_else(|| GoblinError::Runtime("roll: 'sides' must be integer-like".into()))?;
            let modifier   = cfg.get("modifier").or_else(|| cfg.get("mod")).and_then(|v| cast_i64(v)).unwrap_or(0);
            let keep_high  = cfg.get("keep_high").and_then(|v| cast_i64(v)).unwrap_or(0);
            let drop_low   = cfg.get("drop_low").and_then(|v| cast_i64(v)).unwrap_or(0);
            let reroll_eq  = cfg.get("reroll_eq").and_then(|v| cast_i64(v));
            let explode    = cfg.get("explode").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let adv        = cfg.get("adv").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let dis        = cfg.get("dis").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let clamp_lo   = cfg.get("clamp_min").and_then(|v| cast_i64(v));
            let clamp_hi   = cfg.get("clamp_max").and_then(|v| cast_i64(v));
            if sides < 1 { return Err(GoblinError::Runtime("roll: sides must be >= 1".into())); }
            let mut roll_one = |s: i64| -> i64 {
                let mut r = rng_bounded(session, s as u64) as i64 + 1;
                if let Some(face) = reroll_eq { if r == face { r = rng_bounded(session, s as u64) as i64 + 1; } }
                if explode {
                    let mut total = r; let mut last = r; let mut guard = 0usize;
                    while last == s && guard < 1024 { let e = rng_bounded(session, s as u64) as i64 + 1; total += e; last = e; guard += 1; }
                    total
                } else { r }
            };
            let result_num: i64 = if adv || dis {
                let a = roll_one(sides); let b = roll_one(sides);
                let mut total = if adv { a.max(b) } else { a.min(b) } + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) { let (lo, hi) = if lo <= hi { (lo,hi) } else { (hi,lo) }; if total < lo { total = lo; } if total > hi { total = hi; } }
                total
            } else {
                let mut vals: Vec<i64> = (0..count).map(|_| roll_one(sides)).collect();
                let kept_sum: i64 = if keep_high > 0 {
                    let mut xs = vals.clone(); xs.sort_unstable_by(|a,b| b.cmp(a));
                    xs.into_iter().take(keep_high.max(0) as usize).sum()
                } else if drop_low > 0 {
                    let mut xs = vals.clone(); xs.sort_unstable();
                    xs.into_iter().skip(drop_low.max(0) as usize).sum()
                } else { vals.iter().sum() };
                let mut total = kept_sum + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) { let (lo, hi) = if lo <= hi { (lo,hi) } else { (hi,lo) }; if total < lo { total = lo; } if total > hi { total = hi; } }
                total
            };
            Ok(Value::Int(result_num))
        }
        BuiltinId::RollDetail => {
            expect_n(1)?;
            let cfg = match read(0)? {
                Value::Map(m) => m,
                other => return Err(GoblinError::type_error("map config", other.type_name(), "roll_detail")),
            };
            let cast_i64 = |v: &Value| -> Option<i64> {
                match v { Value::Int(i) => Some(*i), Value::Float(f) => Some(f.trunc() as i64), _ => None }
            };
            let count    = cast_i64(cfg.get("count").ok_or_else(|| GoblinError::Runtime("roll_detail: missing 'count'".into()))?)
                .ok_or_else(|| GoblinError::Runtime("roll_detail: 'count' must be integer-like".into()))?;
            let sides    = cast_i64(cfg.get("sides").ok_or_else(|| GoblinError::Runtime("roll_detail: missing 'sides'".into()))?)
                .ok_or_else(|| GoblinError::Runtime("roll_detail: 'sides' must be integer-like".into()))?;
            let modifier   = cfg.get("modifier").or_else(|| cfg.get("mod")).and_then(|v| cast_i64(v)).unwrap_or(0);
            let keep_high  = cfg.get("keep_high").and_then(|v| cast_i64(v)).unwrap_or(0);
            let drop_low   = cfg.get("drop_low").and_then(|v| cast_i64(v)).unwrap_or(0);
            let reroll_eq  = cfg.get("reroll_eq").and_then(|v| cast_i64(v));
            let explode    = cfg.get("explode").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let adv        = cfg.get("adv").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let dis        = cfg.get("dis").map(|v| matches!(v, Value::Bool(true))).unwrap_or(false);
            let clamp_lo   = cfg.get("clamp_min").and_then(|v| cast_i64(v));
            let clamp_hi   = cfg.get("clamp_max").and_then(|v| cast_i64(v));
            if sides < 1 { return Err(GoblinError::Runtime("roll_detail: sides must be >= 1".into())); }
            if (adv || dis) && count != 1 { return Err(GoblinError::Runtime("roll_detail: adv/dis requires count=1".into())); }
            if keep_high > 0 && drop_low > 0 { return Err(GoblinError::Runtime("roll_detail: cannot combine keep_high and drop_low".into())); }
            let mut roll_one = |s: i64| -> i64 {
                let mut r = rng_bounded(session, s as u64) as i64 + 1;
                if let Some(face) = reroll_eq { if r == face { r = rng_bounded(session, s as u64) as i64 + 1; } }
                if explode {
                    let mut total = r; let mut last = r; let mut guard = 0usize;
                    while last == s && guard < 1024 { let e = rng_bounded(session, s as u64) as i64 + 1; total += e; last = e; guard += 1; }
                    total
                } else { r }
            };
            use std::collections::BTreeMap as BM;
            let detail: Value = if adv || dis {
                let a = roll_one(sides); let b = roll_one(sides);
                let chosen = if adv { a.max(b) } else { a.min(b) };
                let dropped_val = if adv { a.min(b) } else { a.max(b) };
                let mut total = chosen + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) { let (lo,hi) = if lo<=hi{(lo,hi)}else{(hi,lo)}; if total<lo{total=lo;} if total>hi{total=hi;} }
                let mut out = BM::<String,Value>::new();
                out.insert("count".into(), Value::Int(1));
                out.insert("sides".into(), Value::Int(sides));
                out.insert("modifier".into(), Value::Int(modifier));
                out.insert("values".into(), Value::Array(vec![Value::Int(a), Value::Int(b)]));
                out.insert("kept".into(), Value::Array(vec![Value::Int(chosen)]));
                out.insert("dropped".into(), Value::Array(vec![Value::Int(dropped_val)]));
                out.insert("sum".into(), Value::Int(chosen));
                out.insert("total".into(), Value::Int(total));
                out.insert("adv".into(), Value::Bool(adv));
                out.insert("dis".into(), Value::Bool(dis));
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi)); }
                Value::Map(out)
            } else {
                let vals: Vec<i64> = (0..count).map(|_| roll_one(sides)).collect();
                let mut keep_mask = vec![true; vals.len()];
                if keep_high > 0 {
                    let k = keep_high as usize;
                    let mut idxs: Vec<usize> = (0..vals.len()).collect();
                    idxs.sort_unstable_by(|&i,&j| vals[j].cmp(&vals[i]));
                    for &i in idxs.iter().skip(k.min(vals.len())) { keep_mask[i] = false; }
                } else if drop_low > 0 {
                    let d = drop_low as usize;
                    let mut idxs: Vec<usize> = (0..vals.len()).collect();
                    idxs.sort_unstable_by(|&i,&j| vals[i].cmp(&vals[j]));
                    for &i in idxs.iter().take(d.min(vals.len())) { keep_mask[i] = false; }
                }
                let mut kept_vals: Vec<i64> = Vec::new();
                let mut dropped_vals: Vec<i64> = Vec::new();
                for (i, &v) in vals.iter().enumerate() {
                    if keep_mask[i] { kept_vals.push(v); } else { dropped_vals.push(v); }
                }
                let kept_sum: i64 = kept_vals.iter().sum();
                let mut total = kept_sum + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) { let (lo,hi) = if lo<=hi{(lo,hi)}else{(hi,lo)}; if total<lo{total=lo;} if total>hi{total=hi;} }
                let mut out = BM::<String,Value>::new();
                out.insert("count".into(), Value::Int(count));
                out.insert("sides".into(), Value::Int(sides));
                out.insert("modifier".into(), Value::Int(modifier));
                out.insert("values".into(), Value::Array(vals.into_iter().map(Value::Int).collect()));
                out.insert("kept".into(), Value::Array(kept_vals.into_iter().map(Value::Int).collect()));
                out.insert("dropped".into(), Value::Array(dropped_vals.into_iter().map(Value::Int).collect()));
                out.insert("sum".into(), Value::Int(kept_sum));
                out.insert("total".into(), Value::Int(total));
                if keep_high > 0 { out.insert("keep_high".into(), Value::Int(keep_high)); }
                if drop_low  > 0 { out.insert("drop_low".into(),  Value::Int(drop_low)); }
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi)); }
                Value::Map(out)
            };
            Ok(detail)
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
                Value::Int(n) if n < 0 => Value::Nil,
                Value::Int(0) => Value::Array(vec![Value::Int(0)]),
                Value::Int(n) => {
                    let mut val = n as i128;
                    let mut out = Vec::new();
                    while val > 0 { out.push(Value::Int((val % 10) as i64)); val /= 10; }
                    out.reverse();
                    Value::Array(out)
                }
                Value::Str(s) => Value::Array(s.chars().map(|c| Value::Str(c.to_string())).collect()),
                _ => Value::Nil,
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
            // count: prefer count_expr (dynamic), then count (static), default 1
            let count_f = map_get_f64(&m, "count_expr").or_else(|| map_get_f64(&m, "count")).unwrap_or(1.0);
            let n_out = count_f as usize;
            if n_out == 0 { return Ok(Value::Array(vec![])); }
            let finish = |mut items: Vec<Value>| -> Value {
                if n_out == 1 { items.pop().unwrap_or(Value::Nil) } else { Value::Array(items) }
            };
            // src collection (Array or Str-as-char-array)
            let allow_dups_default = !m.contains_key("src"); // dups default true for ranges
            let allow_dups = map_get_bool(&m, "allow_dups").unwrap_or(allow_dups_default);
            if let Some(src) = m.get("src") {
                let items: Vec<Value> = match src {
                    Value::Array(a) => a.clone(),
                    Value::Str(s) => s.chars().map(|c| Value::Str(c.to_string())).collect(),
                    Value::Map(sm) => sm.iter().map(|(k, v)| {
                        let mut pair = std::collections::BTreeMap::new();
                        pair.insert(k.clone(), v.clone());
                        Value::Map(pair)
                    }).collect(),
                    _ => return Err(GoblinError::Runtime("secure_pick: unsupported src type".into())),
                };
                if items.is_empty() {
                    return Err(GoblinError::Runtime("secure_pick: cannot pick from empty collection".into()));
                }
                let out = csprng_pick_from_slice(&items, n_out, allow_dups, session);
                return Ok(finish(out));
            }
            // range form
            if m.contains_key("range_start") && m.contains_key("range_end") {
                let incl = map_get_bool(&m, "range_inclusive").unwrap_or(false);
                // char range: both sides are single-char strings
                let a_str = m.get("range_start").and_then(|v| if let Value::Str(s) = v { Some(s.clone()) } else { None });
                let b_str = m.get("range_end").and_then(|v| if let Value::Str(s) = v { Some(s.clone()) } else { None });
                if let (Some(a_s), Some(b_s)) = (a_str, b_str) {
                    if a_s.len() == 1 && b_s.len() == 1 {
                        let sc = a_s.chars().next().unwrap();
                        let ec = b_s.chars().next().unwrap();
                        let mut pool: Vec<char> = if incl { (sc..=ec).collect() } else { (sc..ec).collect() };
                        if pool.is_empty() { return Err(GoblinError::Runtime("secure_pick: empty char range".into())); }
                        let picked: Vec<char> = if allow_dups {
                            (0..n_out).map(|_| pool[rng_bounded(session, pool.len() as u64) as usize]).collect()
                        } else {
                            for i in 0..n_out.min(pool.len()) {
                                let j = i + rng_bounded(session, (pool.len() - i) as u64) as usize;
                                pool.swap(i, j);
                            }
                            pool[..n_out.min(pool.len())].to_vec()
                        };
                        // interpreter: n_out==1 → Str of 1 char; else → Str of all chars
                        return Ok(if n_out == 1 {
                            Value::Str(picked[0].to_string())
                        } else {
                            Value::Str(picked.iter().collect())
                        });
                    }
                }
                // numeric range
                let a = map_get_f64(&m, "range_start").unwrap_or(0.0) as i64;
                let b = map_get_f64(&m, "range_end").unwrap_or(0.0) as i64;
                let (lo, hi) = if a <= b { (a, b) } else { (b, a) };
                let pool: Vec<i64> = if incl { (lo..=hi).collect() } else { (lo..hi).collect() };
                if pool.is_empty() { return Err(GoblinError::Runtime("secure_pick: empty range".into())); }
                let out_vals: Vec<Value> = if allow_dups {
                    (0..n_out).map(|_| Value::Int(pool[rng_bounded(session, pool.len() as u64) as usize])).collect()
                } else {
                    let mut p = pool.clone();
                    for i in 0..n_out.min(p.len()) {
                        let j = i + rng_bounded(session, (p.len() - i) as u64) as usize;
                        p.swap(i, j);
                    }
                    p[..n_out.min(p.len())].iter().map(|&v| Value::Int(v)).collect()
                };
                return Ok(finish(out_vals));
            }
            Err(GoblinError::Runtime("secure_pick: needs 'src' or range".into()))
        }

        BuiltinId::SecureRandom => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "secure_random".into() }); }
            let (min, max) = match (read(0)?, read(1)?) {
                (Value::Int(a), Value::Int(b)) => (a, b),
                (a, b) => return Err(GoblinError::Runtime(format!("secure_random: expected Int, Int, got {}, {}", a.type_name(), b.type_name()))),
            };
            if min > max { return Err(GoblinError::Runtime(format!("secure_random: min ({min}) > max ({max})"))); }
            let range = (max as i128 - min as i128 + 1) as u64;
            let offset = rng_bounded(session, range);
            Ok(Value::Int(min + offset as i64))
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
                Value::Str(s) => Ok(Value::Array(s.split('\n').map(|l| Value::Str(l.to_string())).collect())),
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

        BuiltinId::IsType => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "is_type".into() }); }
            let recv = read(0)?;
            let type_name = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "is_type")) };
            let ok = match (&recv, type_name.as_str()) {
                (Value::Int(_), "int") => true,
                (Value::Float(_), "float") => true,
                (Value::Bool(_), "bool") => true,
                (Value::Str(_), "str") => true,
                (Value::Big(_), "big") => true,
                (Value::Pct(_), "pct") => true,
                (Value::Int(_) | Value::Float(_) | Value::Big(_) | Value::Pct(_), "int" | "float" | "big" | "pct") => true,
                (Value::Str(s), "int") => { let c: String = s.trim().chars().filter(|&c| c != '_').collect(); c.parse::<i64>().is_ok() }
                (Value::Str(s), "float") => { let c: String = s.trim().chars().filter(|&c| c != '_').collect(); c.parse::<f64>().is_ok() }
                _ => false,
            };
            Ok(Value::Bool(ok))
        }

        BuiltinId::IsBoundName => {
            // In the VM there's no dynamic scope lookup by name string; return false
            expect_n(1)?;
            Ok(Value::Bool(false))
        }

        BuiltinId::Invoke | BuiltinId::Summon | BuiltinId::Provoke => {
            Err(GoblinError::NotImplemented { feature: "invoke/summon/provoke require dynamic action dispatch (VM limitation)" })
        }

        BuiltinId::YallParse => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "yall_parse".into() }); }
            let text  = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "yall_parse")) };
            let label = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "yall_parse label")) };
            let parsed = goblin_yall::yall_parse(&text, &label)
                .map_err(|e| GoblinError::Runtime(format!("yall_parse failed: {e}")))?;
            Ok(yall_to_value(&parsed))
        }
        BuiltinId::YallParseFile => {
            expect_n(1)?;
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "yall_parse_file")) };
            let parsed = goblin_yall::yall_parse_file(&path)
                .map_err(|e| GoblinError::Runtime(format!("yall_parse_file failed: {e}")))?;
            Ok(yall_to_value(&parsed))
        }
        BuiltinId::YallWrite => {
            expect_n(1)?;
            let v = read(0)?;
            let yv = value_to_yall(&v);
            Ok(Value::Str(goblin_yall::yall_write(&yv)))
        }
        BuiltinId::YallWriteFile => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "yall_write_file".into() }); }
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "yall_write_file path")) };
            let v = read(1)?;
            let yv = value_to_yall(&v);
            goblin_yall::yall_write_file(&path, &yv)
                .map_err(|e| GoblinError::Runtime(format!("yall_write_file failed: {e}")))?;
            Ok(Value::Nil)
        }
        BuiltinId::YallPretty => {
            expect_n(1)?;
            let v = read(0)?;
            let yv = value_to_yall(&v);
            Ok(Value::Str(goblin_yall::yall_stringify(&yv)))
        }
        BuiltinId::YallMinify => {
            expect_n(1)?;
            let v = read(0)?;
            let yv = value_to_yall(&v);
            Ok(Value::Str(goblin_yall::yall_minify(&yv)))
        }

        BuiltinId::CreateDir => {
            expect_n(1)?;
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "create_dir")) };
            std::fs::create_dir_all(&path).map_err(|e| GoblinError::Runtime(format!("create_dir failed: {e}")))?;
            Ok(Value::Unit)
        }

        BuiltinId::CopyFile => {
            if args.len() != 2 { return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "copy_file".into() }); }
            let src = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "copy_file src")) };
            let dst = match read(1)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "copy_file dst")) };
            if let Some(parent) = std::path::Path::new(&dst).parent() {
                if !parent.as_os_str().is_empty() {
                    std::fs::create_dir_all(parent).map_err(|e| GoblinError::Runtime(format!("copy_file: failed to create parent dirs: {e}")))?;
                }
            }
            std::fs::copy(&src, &dst).map_err(|e| GoblinError::Runtime(format!("copy_file failed: {e}")))?;
            Ok(Value::Unit)
        }

        BuiltinId::DeletePath => {
            expect_n(1)?;
            let path = match read(0)? { Value::Str(s) => s, other => return Err(GoblinError::type_error("str", other.type_name(), "delete_path")) };
            let p = std::path::Path::new(&path);
            if !p.exists() { return Err(GoblinError::Runtime(format!("delete_path: path does not exist: {}", path))); }
            if p.is_dir() {
                std::fs::remove_dir_all(&path).map_err(|e| GoblinError::Runtime(format!("delete_path failed: {e}")))?;
            } else {
                std::fs::remove_file(&path).map_err(|e| GoblinError::Runtime(format!("delete_path failed: {e}")))?;
            }
            Ok(Value::Unit)
        }

        BuiltinId::MdToHtml => {
            Err(GoblinError::NotImplemented { feature: "md_to_html: requires comrak dependency (not included in VM)" })
        }

        BuiltinId::HighlightCode => {
            Err(GoblinError::NotImplemented { feature: "highlight_code: requires syntect dependency (not included in VM)" })
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

fn to_f64_val(v: &Value) -> Result<f64, GoblinError> {
    match v {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        other => Err(GoblinError::type_error("number", other.type_name(), "numeric op")),
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

fn fmt_num_trim(f: f64) -> String {
    if f.is_finite() && f.fract() == 0.0 {
        format!("{}", f as i64)
    } else {
        let s = format!("{}", f);
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    }
}

pub fn value_to_str(v: &Value) -> String {
    match v {
        Value::Nil             => "nil".to_string(),
        Value::Unit            => String::new(),
        Value::Bool(b)         => b.to_string(),
        Value::Int(n)          => n.to_string(),
        Value::Float(f)        => fmt_num_trim(*f),
        Value::Pct(p)          => fmt_num_trim(*p),
        Value::Big(d)          => d.to_string(),
        Value::Char(c)         => c.to_string(),
        Value::Str(s)          => s.clone(),
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
    // Exact match to interpreter: SplitMix-style stateless mixing from (seed ^ i*constant)
    fn is_upper(seed: u128, i: usize) -> bool {
        let mut x = seed ^ ((i as u128).wrapping_mul(0x9E37_79B9_7F4A_7C15));
        x ^= x >> 30;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 27;
        x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
        x ^= x >> 31;
        (x & 1) == 1
    }
    let to_mixed = |s: &str, seed: u128| -> String {
        let mut out = String::with_capacity(s.len());
        for (i, ch) in s.chars().enumerate() {
            if ch.is_alphabetic() {
                if is_upper(seed, i) { out.extend(ch.to_uppercase()); }
                else { out.extend(ch.to_lowercase()); }
            } else {
                out.push(ch);
            }
        }
        out
    };
    match v {
        Value::Str(s) => Ok(Value::Str(to_mixed(s, seed))),
        Value::Char(ch) => {
            let s = to_mixed(&ch.to_string(), seed);
            let mut iter = s.chars();
            Ok(match (iter.next(), iter.next()) {
                (Some(c), None) => Value::Char(c),
                _ => Value::Str(s),
            })
        }
        Value::Array(xs) => {
            let mut out = Vec::with_capacity(xs.len());
            for item in xs {
                out.push(match item {
                    Value::Str(s)  => Value::Str(to_mixed(s, seed)),
                    Value::Char(c) => {
                        let s = to_mixed(&c.to_string(), seed);
                        let mut iter = s.chars();
                        match (iter.next(), iter.next()) {
                            (Some(ch), None) => Value::Char(ch),
                            _ => Value::Str(s),
                        }
                    }
                    other => return Err(GoblinError::type_error("string/char", other.type_name(), "mixed")),
                });
            }
            Ok(Value::Array(out))
        }
        other => Err(GoblinError::type_error("string/char", other.type_name(), "mixed")),
    }
}

fn value_to_map_key(v: &Value) -> String {
    match v {
        Value::Str(s)   => s.clone(),
        Value::Char(c)  => c.to_string(),
        Value::Int(n)   => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b)  => b.to_string(),
        other           => fmt_value_raw(other),
    }
}

fn fmt_value_raw(v: &Value) -> String {
    fmt_value_depth(v, 0)
}

fn fmt_value_depth(v: &Value, depth: usize) -> String {
    if depth > 20 { return "[too deep]".to_string(); }
    match v {
        Value::Str(s)   => s.clone(),
        Value::Char(c)  => c.to_string(),
        Value::Int(n)   => n.to_string(),
        Value::Float(f) => fmt_num_trim(*f),
        Value::Pct(p)   => fmt_num_trim(*p),
        Value::Bool(b)  => if *b { "true".into() } else { "false".into() },
        Value::Nil      => "nil".into(),
        Value::Unit     => String::new(),
        Value::Array(xs) => {
            let mut s = String::from("[");
            for (i, val) in xs.iter().enumerate() {
                if i > 0 { s.push_str(", "); }
                s.push_str(&fmt_value_depth(val, depth + 1));
            }
            s.push(']');
            s
        }
        Value::Map(m) => {
            let mut s = String::from("{");
            let mut first = true;
            for (k, val) in m.iter() {
                if !first { s.push_str(", "); }
                first = false;
                s.push_str(k);
                s.push_str(": ");
                s.push_str(&fmt_value_depth(val, depth + 1));
            }
            s.push('}');
            s
        }
        Value::MapOrd(m) => {
            let mut s = String::from("{");
            let mut first = true;
            for (k, val) in m.iter() {
                if !first { s.push_str(", "); }
                first = false;
                s.push_str(k);
                s.push_str(": ");
                s.push_str(&fmt_value_depth(val, depth + 1));
            }
            s.push('}');
            s
        }
        Value::Pair(a, b) => format!("({}, {})", fmt_value_depth(a, depth + 1), fmt_value_depth(b, depth + 1)),
        Value::Collection(c) => {
            let items = collections::to_vec(c);
            let mut s = String::from("[");
            for (i, val) in items.iter().enumerate() {
                if i > 0 { s.push_str(", "); }
                s.push_str(&fmt_value_depth(val, depth + 1));
            }
            s.push(']');
            s
        }
        _ => v.type_name().to_string(),
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

fn rng_u01(session: &mut Session) -> f64 {
    let x = (session.next_u128() >> 64) as u64;
    (x as f64) / (u64::MAX as f64)
}

fn json_to_value(v: &serde_json::Value) -> Value {
    match v {
        serde_json::Value::Null        => Value::Nil,
        serde_json::Value::Bool(b)     => Value::Bool(*b),
        serde_json::Value::Number(n)   => {
            if let Some(i) = n.as_i64() { Value::Int(i) }
            else if let Some(f) = n.as_f64() { Value::Float(f) }
            else { Value::Float(0.0) }
        }
        serde_json::Value::String(s)   => Value::Str(s.clone()),
        serde_json::Value::Array(xs)   => Value::Array(xs.iter().map(json_to_value).collect()),
        serde_json::Value::Object(obj) => {
            let m: std::collections::BTreeMap<String, Value> = obj.iter().map(|(k, v)| (k.clone(), json_to_value(v))).collect();
            Value::Map(m)
        }
    }
}

fn value_to_json(v: &Value) -> serde_json::Value {
    match v {
        Value::Int(i)    => serde_json::Value::Number(serde_json::Number::from(*i)),
        Value::Float(f)  => serde_json::Value::Number(serde_json::Number::from_f64(*f).unwrap_or_else(|| serde_json::Number::from_f64(0.0).unwrap())),
        Value::Pct(p)    => serde_json::Value::Number(serde_json::Number::from_f64(*p).unwrap_or_else(|| serde_json::Number::from_f64(0.0).unwrap())),
        Value::Str(s)    => serde_json::Value::String(s.clone()),
        Value::Char(c)   => serde_json::Value::String(c.to_string()),
        Value::Bool(b)   => serde_json::Value::Bool(*b),
        Value::Array(xs) => serde_json::Value::Array(xs.iter().map(value_to_json).collect()),
        Value::Map(m)    => {
            let mut obj = serde_json::Map::new();
            for (k, val) in m { obj.insert(k.clone(), value_to_json(val)); }
            serde_json::Value::Object(obj)
        }
        Value::MapOrd(m) => {
            let mut obj = serde_json::Map::new();
            for (k, val) in m.iter() { obj.insert(k.clone(), value_to_json(val)); }
            serde_json::Value::Object(obj)
        }
        Value::Pair(a, b) => serde_json::Value::Array(vec![value_to_json(a), value_to_json(b)]),
        Value::Nil | Value::Unit => serde_json::Value::Null,
        _ => serde_json::Value::String(fmt_value_raw(v)),
    }
}

fn regex_with_flags(pattern: &str, flags_val: &Value) -> String {
    let mut f_i = false; let mut f_m = false; let mut f_s = false;
    if let Value::Map(m) = flags_val {
        if let Some(Value::Bool(b)) = m.get("i") { f_i = *b; }
        if let Some(Value::Bool(b)) = m.get("m") { f_m = *b; }
        if let Some(Value::Bool(b)) = m.get("s") { f_s = *b; }
    }
    let mut f = String::new();
    if f_i { f.push('i'); } if f_m { f.push('m'); } if f_s { f.push('s'); }
    if f.is_empty() { pattern.to_string() } else { format!("(?{}){}", f, pattern) }
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

fn map_get_f64(m: &std::collections::BTreeMap<String, Value>, key: &str) -> Option<f64> {
    match m.get(key)? {
        Value::Int(n)   => Some(*n as f64),
        Value::Float(f) => Some(*f),
        _ => None,
    }
}

fn yall_to_value(v: &goblin_yall::YallValue) -> Value {
    match v {
        goblin_yall::YallValue::Null     => Value::Nil,
        goblin_yall::YallValue::Bool(b)  => Value::Bool(*b),
        goblin_yall::YallValue::Int(i)   => Value::Int(*i),
        goblin_yall::YallValue::Float(f) => Value::Float(*f),
        goblin_yall::YallValue::Str(s)   => Value::Str(s.clone()),
        goblin_yall::YallValue::Array(items) => Value::Array(items.iter().map(yall_to_value).collect()),
        goblin_yall::YallValue::Map(map) => {
            let mut out = indexmap::IndexMap::new();
            for (k, v2) in map.iter() { out.insert(k.clone(), yall_to_value(v2)); }
            Value::MapOrd(out)
        }
    }
}

fn value_to_yall(v: &Value) -> goblin_yall::YallValue {
    match v {
        Value::Nil          => goblin_yall::YallValue::Null,
        Value::Bool(b)      => goblin_yall::YallValue::Bool(*b),
        Value::Int(i)       => goblin_yall::YallValue::Int(*i),
        Value::Float(f)     => goblin_yall::YallValue::Float(*f),
        Value::Str(s)       => goblin_yall::YallValue::Str(s.clone()),
        Value::Array(arr)   => goblin_yall::YallValue::Array(arr.iter().map(value_to_yall).collect()),
        Value::Map(map) => {
            let mut out = indexmap::IndexMap::new();
            for (k, v2) in map.iter() { out.insert(k.clone(), value_to_yall(v2)); }
            goblin_yall::YallValue::Map(out)
        }
        Value::MapOrd(map) => {
            let mut out = indexmap::IndexMap::new();
            for (k, v2) in map.iter() { out.insert(k.clone(), value_to_yall(v2)); }
            goblin_yall::YallValue::Map(out)
        }
        _ => goblin_yall::YallValue::Str(fmt_value_raw(v)),
    }
}
