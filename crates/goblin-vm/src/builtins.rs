/// Builtin function dispatch for the Goblin VM.
///
/// All builtins follow the same calling convention:
///   - receive a Vec<Tether> of arguments
///   - receive &mut Session to allocate results
///   - return a Tether pointing to the result stash
///
/// Builtins that require predicate callbacks (grab_where, reap_where, etc.)
/// are not handled here; they require VM-level call support and are dispatched
/// from vm.rs via specialised opcodes or a callback path (future work).
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
    // Helper: read the nth arg as a Value.
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
        // ── Memory introspection ──────────────────────────────────────────────
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
            let a = read(0)?; let b = read(1)?;
            Ok(numeric_min(a, b)?)
        }
        BuiltinId::Max => {
            if args.len() < 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "max".into() });
            }
            let a = read(0)?; let b = read(1)?;
            Ok(numeric_max(a, b)?)
        }
        BuiltinId::Floor => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Float(f.floor()),
                other => return Err(GoblinError::type_error("number", other.type_name(), "floor")),
            })
        }
        BuiltinId::Ceil => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Float(f.ceil()),
                other => return Err(GoblinError::type_error("number", other.type_name(), "ceil")),
            })
        }
        BuiltinId::Round => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Int(n)   => Value::Int(n),
                Value::Float(f) => Value::Float(f.round()),
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
            let base = read(0)?; let exp = read(1)?;
            Ok(match (base, exp) {
                (Value::Int(b), Value::Int(e)) if e >= 0 => Value::Int(b.pow(e as u32)),
                (Value::Int(b), Value::Float(e)) => Value::Float((b as f64).powf(e)),
                (Value::Float(b), Value::Float(e)) => Value::Float(b.powf(e)),
                (Value::Float(b), Value::Int(e)) => Value::Float(b.powi(e as i32)),
                _ => return Err(GoblinError::type_error("number", "mixed", "pow")),
            })
        }

        // ── String ────────────────────────────────────────────────────────────
        BuiltinId::Len => {
            expect_n(1)?;
            Ok(match read(0)? {
                Value::Str(s)        => Value::Int(s.chars().count() as i64),
                Value::Collection(c) => Value::Int(c.meta.len as i64),
                other => return Err(GoblinError::type_error("str or collection", other.type_name(), "len")),
            })
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
        BuiltinId::Trim => {
            expect_n(1)?;
            match read(0)? {
                Value::Str(s) => Ok(Value::Str(s.trim().to_string())),
                other => Err(GoblinError::type_error("str", other.type_name(), "trim")),
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
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(parts))))
        }
        BuiltinId::Join => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "join".into() });
            }
            let coll = match read(0)? {
                Value::Collection(c) => c,
                other => return Err(GoblinError::type_error("collection", other.type_name(), "join")),
            };
            let sep = match read(1)? {
                Value::Str(s) => s,
                other => return Err(GoblinError::type_error("str", other.type_name(), "join separator")),
            };
            let parts: Vec<String> = collections::to_vec(&coll).iter().map(value_to_str).collect();
            Ok(Value::Str(parts.join(&sep)))
        }
        BuiltinId::Contains => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "contains".into() });
            }
            let haystack = read(0)?; let needle = read(1)?;
            Ok(match (haystack, needle) {
                (Value::Str(s), Value::Str(n)) => Value::Bool(s.contains(n.as_str())),
                (Value::Collection(c), v) => Value::Bool(
                    collections::to_vec(&c).iter().any(|x| x == &v)
                ),
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
                (Value::Str(s), Value::Str(from), Value::Str(to)) => {
                    Ok(Value::Str(s.replace(from.as_str(), &to)))
                }
                _ => Err(GoblinError::type_error("str", "mixed", "replace")),
            }
        }

        // ── Collections — grab ────────────────────────────────────────────────
        BuiltinId::Grab => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab")?;
            Ok(collections::grab(&coll))
        }
        BuiltinId::GrabFirst => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab_first")?;
            collections::grab_first(&coll)
        }
        BuiltinId::GrabLast => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab_last")?;
            collections::grab_last(&coll)
        }
        BuiltinId::GrabAt => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "grab_at".into() });
            }
            let coll = require_collection(read(0)?, "grab_at")?;
            let idx = require_int(read(1)?, "grab_at index")?;
            collections::grab_at(&coll, idx)
        }
        BuiltinId::GrabBetween => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "grab_between".into() });
            }
            let coll = require_collection(read(0)?, "grab_between")?;
            let start = require_int(read(1)?, "grab_between start")?;
            let end   = require_int(read(2)?, "grab_between end")?;
            collections::grab_between(&coll, start, end)
        }
        BuiltinId::GrabAll => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab_all")?;
            Ok(collections::grab_all(&coll))
        }
        // grab_random, grab_where, grab_matching — predicate/random; VM-level dispatch needed
        BuiltinId::GrabRandom => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "grab_random")?;
            let items = collections::to_vec(&coll);
            if items.is_empty() { return Ok(Value::Nil); }
            // Simple pseudo-random using system time bits.
            let idx = pseudo_random_index(items.len());
            Ok(items[idx].clone())
        }
        BuiltinId::GrabWhere | BuiltinId::GrabMatching => {
            Err(GoblinError::NotImplemented { feature: "grab_where / grab_matching require VM predicate callback" })
        }

        // ── Collections — put ─────────────────────────────────────────────────
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
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_first".into() });
            }
            let coll = require_collection(read(0)?, "put_first")?;
            let val = read(1)?;
            collections::put_first(&coll, val)
        }
        BuiltinId::PutLast => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "put_last".into() });
            }
            let coll = require_collection(read(0)?, "put_last")?;
            let val = read(1)?;
            collections::put_last(&coll, val)
        }
        BuiltinId::PutAt => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "put_at".into() });
            }
            let coll = require_collection(read(0)?, "put_at")?;
            let idx = require_int(read(1)?, "put_at index")?;
            let val = read(2)?;
            collections::put_at(&coll, idx, val)
        }

        // ── Collections — update ──────────────────────────────────────────────
        BuiltinId::Update => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update".into() });
            }
            let coll = require_collection(read(0)?, "update")?;
            let key = read(1)?;
            let val = read(2)?;
            collections::update(&coll, &key, val)
        }
        BuiltinId::UpdateFirst => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_first".into() });
            }
            let coll = require_collection(read(0)?, "update_first")?;
            let val = read(1)?;
            collections::update_first(&coll, val)
        }
        BuiltinId::UpdateLast => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "update_last".into() });
            }
            let coll = require_collection(read(0)?, "update_last")?;
            let val = read(1)?;
            collections::update_last(&coll, val)
        }
        BuiltinId::UpdateAt => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "update_at".into() });
            }
            let coll = require_collection(read(0)?, "update_at")?;
            let idx = require_int(read(1)?, "update_at index")?;
            let val = read(2)?;
            collections::update_at(&coll, idx, val)
        }

        // ── Collections — delete ──────────────────────────────────────────────
        BuiltinId::Delete => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete".into() });
            }
            let coll = require_collection(read(0)?, "delete")?;
            let key = read(1)?;
            collections::delete(&coll, &key)
        }
        BuiltinId::DeleteFirst => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "delete_first")?;
            collections::delete_first(&coll)
        }
        BuiltinId::DeleteLast => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "delete_last")?;
            collections::delete_last(&coll)
        }
        BuiltinId::DeleteAt => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "delete_at".into() });
            }
            let coll = require_collection(read(0)?, "delete_at")?;
            let idx = require_int(read(1)?, "delete_at index")?;
            collections::delete_at(&coll, idx)
        }
        BuiltinId::DeleteWhere | BuiltinId::DeleteAll => {
            Err(GoblinError::NotImplemented { feature: "delete_where / delete_all require VM predicate callback" })
        }

        // ── Collections — reap ────────────────────────────────────────────────
        BuiltinId::Reap => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap".into() });
            }
            let coll = require_collection(read(0)?, "reap")?;
            let key = read(1)?;
            let (elem, new_coll) = collections::reap(&coll, &key)?;
            // Return a pair [elem, new_collection].
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapFirst => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "reap_first")?;
            let (elem, new_coll) = collections::reap_first(&coll)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapLast => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "reap_last")?;
            let (elem, new_coll) = collections::reap_last(&coll)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapAt => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "reap_at".into() });
            }
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
            let idx = pseudo_random_index(items.len());
            let (elem, new_coll) = collections::reap_at(&coll, idx as i64)?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(vec![elem, new_coll]))))
        }
        BuiltinId::ReapWhere | BuiltinId::ReapAll => {
            Err(GoblinError::NotImplemented { feature: "reap_where / reap_all require VM predicate callback" })
        }

        // ── Collections — query ───────────────────────────────────────────────
        BuiltinId::Has => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "has".into() });
            }
            let coll = require_collection(read(0)?, "has")?;
            let key = read(1)?;
            Ok(Value::Bool(collections::has(&coll, &key)))
        }
        BuiltinId::Keys => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "keys")?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::keys(&coll)))))
        }
        BuiltinId::Values => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "values")?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::values(&coll)))))
        }
        BuiltinId::Pairs => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "pairs")?;
            Ok(Value::Collection(Rc::new(CollectionValue::from_flat(collections::pairs_vec(&coll)))))
        }
        BuiltinId::Count => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "count")?;
            Ok(Value::Int(collections::count(&coll) as i64))
        }
        BuiltinId::IsEmpty => {
            expect_n(1)?;
            match read(0)? {
                Value::Collection(c) => Ok(Value::Bool(c.is_empty())),
                Value::Str(s)        => Ok(Value::Bool(s.is_empty())),
                Value::Nil           => Ok(Value::Bool(true)),
                _                    => Ok(Value::Bool(false)),
            }
        }
        BuiltinId::Reverse => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "reverse")?;
            Ok(collections::reverse(&coll))
        }
        BuiltinId::Sort => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "sort")?;
            Ok(collections::sort_values(&coll))
        }
        BuiltinId::SortBy | BuiltinId::Map | BuiltinId::Filter | BuiltinId::Reduce
        | BuiltinId::Any | BuiltinId::All | BuiltinId::Find | BuiltinId::FindIndex => {
            Err(GoblinError::NotImplemented { feature: "higher-order collection ops require VM predicate callback" })
        }
        BuiltinId::Zip => {
            if args.len() != 2 {
                return Err(GoblinError::ArityMismatch { expected: 2, got: args.len(), name: "zip".into() });
            }
            let a = require_collection(read(0)?, "zip")?;
            let b = require_collection(read(1)?, "zip")?;
            Ok(collections::zip_collections(&a, &b))
        }
        BuiltinId::Flatten => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "flatten")?;
            Ok(collections::flatten(&coll))
        }
        BuiltinId::Unique => {
            expect_n(1)?;
            let coll = require_collection(read(0)?, "unique")?;
            Ok(collections::unique(&coll))
        }
        BuiltinId::Slice => {
            if args.len() != 3 {
                return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "slice".into() });
            }
            let coll = require_collection(read(0)?, "slice")?;
            let start = require_int(read(1)?, "slice start")?;
            let end   = require_int(read(2)?, "slice end")?;
            collections::slice_collection(&coll, start, end)
        }

        // ── I/O ───────────────────────────────────────────────────────────────
        BuiltinId::Print => {
            let parts: Result<Vec<String>, _> = args.iter()
                .map(|t| session.read_value(t).map(|v| value_to_str(&v)))
                .collect();
            print!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Println => {
            let parts: Result<Vec<String>, _> = args.iter()
                .map(|t| session.read_value(t).map(|v| value_to_str(&v)))
                .collect();
            println!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Eprint => {
            let parts: Result<Vec<String>, _> = args.iter()
                .map(|t| session.read_value(t).map(|v| value_to_str(&v)))
                .collect();
            eprint!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }
        BuiltinId::Eprintln => {
            let parts: Result<Vec<String>, _> = args.iter()
                .map(|t| session.read_value(t).map(|v| value_to_str(&v)))
                .collect();
            eprintln!("{}", parts?.join(" "));
            Ok(Value::Nil)
        }

        // ── Type checks ───────────────────────────────────────────────────────
        BuiltinId::IsNil        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Nil))) }
        BuiltinId::IsBool       => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Bool(_)))) }
        BuiltinId::IsInt        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Int(_)))) }
        BuiltinId::IsFloat      => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Float(_)))) }
        BuiltinId::IsStr        => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Str(_)))) }
        BuiltinId::IsCollection => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Collection(_)))) }
        BuiltinId::IsFunction   => { expect_n(1)?; Ok(Value::Bool(matches!(read(0)?, Value::Function(_) | Value::Closure(_)))) }

        // ── Conversions ───────────────────────────────────────────────────────
        BuiltinId::ToString | BuiltinId::ToStr => {
            expect_n(1)?;
            Ok(Value::Str(value_to_str(&read(0)?)))
        }
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
            if args.is_empty() {
                return Err(GoblinError::ArityMismatch { expected: 1, got: 0, name: "assert".into() });
            }
            let cond = read(0)?;
            if !cond.is_truthy() {
                let msg = if args.len() > 1 {
                    match read(1)? {
                        Value::Str(s) => s,
                        v => value_to_str(&v),
                    }
                } else {
                    "assertion failed".to_string()
                };
                return Err(GoblinError::Runtime(msg));
            }
            Ok(Value::Nil)
        }
        BuiltinId::Panic => {
            let msg = if args.is_empty() {
                "panic!".to_string()
            } else {
                match session.read_value(&args[0])? {
                    Value::Str(s) => s,
                    v => value_to_str(&v),
                }
            };
            Err(GoblinError::Runtime(msg))
        }
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

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

fn value_to_str(v: &Value) -> String {
    match v {
        Value::Nil        => "nil".to_string(),
        Value::Bool(b)    => b.to_string(),
        Value::Int(n)     => n.to_string(),
        Value::Float(f)   => f.to_string(),
        Value::Str(s)     => s.clone(),
        Value::Collection(c) => {
            let items: Vec<String> = collections::to_vec(c).iter().map(value_to_str).collect();
            format!("[{}]", items.join(", "))
        }
        Value::Function(f)  => format!("<fn {}>", f.name),
        Value::Closure(c)   => format!("<closure {}>", c.func.name),
        Value::Builtin(b)   => format!("<builtin {:?}>", b),
    }
}

fn pseudo_random_index(len: usize) -> usize {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .subsec_nanos() as usize;
    nanos % len
}
