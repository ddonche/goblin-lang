//! Built-in map helpers: keys, values, items.

use crate::{Session, Value, Diag, Span};
use goblin_diagnostics::{Diagnostic, Severity};
use crate::diagnostics::rtcode;

/// keys(map) -> Array of String keys
pub fn keys(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    // arity
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Use: keys(map).")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    match &args[0] {
        Value::Map(m) => {
            let mut out = Vec::with_capacity(m.len());
            for k in m.keys() {
                out.push(Value::Str(k.clone()));
            }
            Ok(Value::Array(out))
        }
        Value::MapOrd(m) => {
            // preserves insertion/order semantics of MapOrd
            let mut out = Vec::with_capacity(m.len());
            for k in m.keys() {
                out.push(Value::Str(k.clone()));
            }
            Ok(Value::Array(out))
        }
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                "type-mismatch",
                "‘keys’ expects a Map.",
                sp.clone(),
            )
            .with_help("Example: keys({a:1, b:2}) → [\"a\",\"b\"].")
            .with_link("https://goblinlang.org/docs/errors#T0205"),
        ),
    }
}

/// values(map) -> Array of values
pub fn values(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    // arity
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Use: values(map).")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    match &args[0] {
        Value::Map(m) => {
            let mut out = Vec::with_capacity(m.len());
            for v in m.values() {
                out.push(v.clone());
            }
            Ok(Value::Array(out))
        }
        Value::MapOrd(m) => {
            let mut out = Vec::with_capacity(m.len());
            for v in m.values() {
                out.push(v.clone());
            }
            Ok(Value::Array(out))
        }
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                "type-mismatch",
                "‘values’ expects a Map.",
                sp.clone(),
            )
            .with_help("Example: values({a:1, b:2}) → [1,2].")
            .with_link("https://goblinlang.org/docs/errors#T0205"),
        ),
    }
}

/// items(map) -> Array of Pair(key:String, value)
pub fn items(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    // arity
    if args.len() != 1 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                sp.clone(),
            )
            .with_help("Use: items(map).")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    match &args[0] {
        Value::Map(m) => {
            let mut out = Vec::with_capacity(m.len());
            for (k, v) in m.iter() {
                out.push(Value::Pair(Box::new(Value::Str(k.clone())), Box::new(v.clone())));
            }
            Ok(Value::Array(out))
        }
        Value::MapOrd(m) => {
            let mut out = Vec::with_capacity(m.len());
            for (k, v) in m.iter() {
                out.push(Value::Pair(Box::new(Value::Str(k.clone())), Box::new(v.clone())));
            }
            Ok(Value::Array(out))
        }
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                "type-mismatch",
                "‘items’ expects a Map.",
                sp.clone(),
            )
            .with_help("Example: items({a:1}) → [(\"a\",1)].")
            .with_link("https://goblinlang.org/docs/errors#T0205"),
        ),
    }
}