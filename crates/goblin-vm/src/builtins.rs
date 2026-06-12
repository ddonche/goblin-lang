//! Builtin free-call implementations for the Goblin VM.
//! Each builtin matches the interpreter's behaviour exactly.

use crate::value::Value;
use rust_decimal::Decimal;

pub type BuiltinResult = Result<Value, String>;

/// Dispatch a builtin call by name.
pub fn call(name: &str, args: &[Value]) -> BuiltinResult {
    match name {
        "abs" => builtin_abs(args),
        _ => Err(format!("unknown builtin '{name}'")),
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn arity(name: &str, args: &[Value], expected: usize) -> BuiltinResult {
    if args.len() != expected {
        Err(format!("{name} expects {expected} argument(s), got {}", args.len()))
    } else {
        Ok(Value::Nil) // sentinel; caller discards
    }
}

macro_rules! check_arity {
    ($name:expr, $args:expr, $n:expr) => {
        arity($name, $args, $n)?;
    };
}

// ── abs ──────────────────────────────────────────────────────────────────────
// interpreter: crates/goblin-interpreter/src/lib.rs:12509

fn builtin_abs(args: &[Value]) -> BuiltinResult {
    check_arity!("abs", args, 1);
    match &args[0] {
        Value::Int(i)   => Ok(Value::Int(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        Value::Pct(p)   => Ok(Value::Float(p.abs())),
        Value::Big(d)   => Ok(Value::Big(d.abs())),
        _ => Err("abs requires a numeric value (int, float, big, or pct)".to_string()),
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use rust_decimal::Decimal;

    #[test]
    fn abs_int_positive() {
        assert_eq!(call("abs", &[Value::Int(5)]).unwrap(), Value::Int(5));
    }

    #[test]
    fn abs_int_negative() {
        assert_eq!(call("abs", &[Value::Int(-5)]).unwrap(), Value::Int(5));
    }

    #[test]
    fn abs_float() {
        match call("abs", &[Value::Float(-3.14)]).unwrap() {
            Value::Float(f) => assert!((f - 3.14).abs() < 1e-10),
            _ => panic!("expected Float"),
        }
    }

    #[test]
    fn abs_pct() {
        match call("abs", &[Value::Pct(-50.0)]).unwrap() {
            Value::Float(f) => assert!((f - 50.0).abs() < 1e-10),
            _ => panic!("expected Float"),
        }
    }

    #[test]
    fn abs_big() {
        let d = Decimal::new(-314, 2);
        match call("abs", &[Value::Big(d)]).unwrap() {
            Value::Big(r) => assert_eq!(r, Decimal::new(314, 2)),
            _ => panic!("expected Big"),
        }
    }

    #[test]
    fn abs_wrong_type() {
        assert!(call("abs", &[Value::Str("hello".into())]).is_err());
    }

    #[test]
    fn abs_wrong_arity() {
        assert!(call("abs", &[]).is_err());
        assert!(call("abs", &[Value::Int(1), Value::Int(2)]).is_err());
    }
}
