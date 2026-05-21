//! Memory introspection builtins for Goblin.
//!
//! Exposed to Goblin as:
//!   :mem_addr(x)   -> "0x..."
//!   :mem_total()   -> total bytes (stubbed for now)

use crate::{Session, Value, Diag, Span};

use std::collections::BTreeMap;
use indexmap::IndexMap;

/// Return a pointer to "where this value's data lives" in memory.
///
/// - For heap-backed types, we try to return the backing buffer / map / fields.
/// - For everything else, we fall back to the address of the `Value` itself.
fn ptr_for_value(v: &Value) -> *const u8 {
    match v {
        // Heap-backed scalar-ish types

        // String: pointer to the underlying UTF-8 buffer.
        Value::Str(s) => s.as_ptr() as *const u8,

        // Formatted: treat the inner value as the "real" payload.
        Value::Formatted(inner, _) => {
            // inner: &Box<Value> → &Value → *const Value
            let inner_ref: &Value = inner.as_ref();
            inner_ref as *const Value as *const u8
        }

        // Arrays and sequences

        // Array: pointer to the slice backing the Vec<...>.
        Value::Array(xs) => xs.as_ptr() as *const u8,

        // Seq: treat the Seq struct itself as the stash (we don't peek inside).
        Value::Seq(seq) => (seq as *const _) as *const u8,

        // Maps

        // BTreeMap-backed map.
        Value::Map(m) => (m as *const BTreeMap<String, Value>) as *const u8,

        // IndexMap-backed map.
        Value::MapOrd(m) => (m as *const IndexMap<String, Value>) as *const u8,

        // Composite pairs

        // Pair: treat the first element as the anchor "stash".
        Value::Pair(a, _) => {
            // a: &Box<Value> → &Value → *const Value
            let a_ref: &Value = a.as_ref();
            a_ref as *const Value as *const u8
        }

        // Objects and enums

        // Object: use the address of its fields map as the stash location.
        Value::Object { fields, .. } => {
            (fields as *const IndexMap<String, Value>) as *const u8
        }

        // Enum with optional fields: use fields map if present, else fall back.
        Value::Enum { fields: Some(f), .. } => {
            (f as *const IndexMap<String, Value>) as *const u8
        }

        Value::Enum { fields: None, .. } => {
            v as *const Value as *const u8
        }

        // Everything else (ints, floats, bools, pct, big, char, nil/unit/ctrl, etc.)
        // → use the address of this Value instance.
        _ => v as *const Value as *const u8,
    }
}

/// :mem_addr(x)
///
/// Returns a hex string representing the memory address associated with `x`.
/// For heap-backed values (strings, arrays, maps, objects, enums with fields),
/// this points at the backing storage / fields. For immediates and control
/// values, it's the address of the `Value` itself.
///
/// If called with no args, returns an empty string.
pub fn mem_addr(
    _sess: &mut Session,
    args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    let v = match args.get(0) {
        Some(v) => v,
        None => {
            // No args: return "" instead of erroring for now.
            return Ok(Value::Str(String::new()));
        }
    };

    let ptr = ptr_for_value(v);
    let addr_str = format!("{:#p}", ptr); // e.g. "0x00007ffdf123abcd"

    Ok(Value::Str(addr_str))
}

/// :mem_total()
///
/// Returns the total number of bytes currently in use.
///
/// This is a stub that returns 0 for now so the builtin exists and compiles.
/// Once the heap / stash layer exposes real size accounting, wire it up here.
pub fn mem_total(
    _sess: &mut Session,
    _args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    // TODO: replace this with real heap accounting later.
    Ok(Value::Int(0))
}
