//! Durations (owner, 2026-10-07).
//!
//! A duration is a whole number of nanoseconds that remembers the unit it was
//! written in, so `90s` prints as `90s`. `s`, `m`, `h`, `d` and `w` have fixed
//! lengths. Months and years vary, so `mo` and `y` exist only once the program
//! declares them with a unit declaration, e.g. `unit calendar | 1mo == 30d xx`.
//!
//! Durations add and subtract with durations (the result keeps the smaller
//! unit: `2h + 30m` is `150m`), multiply and divide by a number, and a
//! duration divided by a duration is a plain number. Mixing a duration with a
//! plain number in `+`, `-` or a comparison is an error.

use crate::error::GoblinError;
use crate::session::Session;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Duration {
    /// Length in nanoseconds.
    pub ns: i128,
    /// The unit it is shown in (`s`, `m`, `h`, `d`, `w`, or a declared unit).
    pub unit: String,
    /// Nanoseconds in one `unit`.
    pub unit_ns: i128,
}

const NS_PER_S: i128 = 1_000_000_000;

fn fixed_unit_ns(unit: &str) -> Option<i128> {
    Some(match unit {
        "s" => NS_PER_S,
        "m" => 60 * NS_PER_S,
        "h" => 3_600 * NS_PER_S,
        "d" => 86_400 * NS_PER_S,
        "w" => 604_800 * NS_PER_S,
        _ => return None,
    })
}

fn unit_word(unit: &str) -> &'static str {
    match unit { "mo" => "Months", "y" => "Years", _ => "This unit's lengths" }
}

/// Nanoseconds in one `unit`: fixed for s/m/h/d/w, otherwise looked up in the
/// unit declarations the program has run so far.
pub fn unit_ns(unit: &str, session: &Session) -> Result<i128, GoblinError> {
    if let Some(n) = resolve(unit, session, 0) { return Ok(n); }
    let declared = session.unit_registry.values()
        .any(|d| d.conversions.iter().any(|(from, _, to, _)| from == unit || to == unit));
    if declared {
        return Err(GoblinError::Runtime(format!(
            "R0505: duration-overflow: unit '{unit}' is declared, but its length is too long to represent or is not defined from s, m, h, d or w")));
    }
    Err(GoblinError::Runtime(format!(
        "R0101: unknown-unit: duration unit '{unit}' is not declared. {} vary in length, so declare it before use, e.g. unit calendar | 1mo == 30d; 1y == 365d xx",
        unit_word(unit))))
}

fn resolve(unit: &str, session: &Session, depth: usize) -> Option<i128> {
    if let Some(n) = fixed_unit_ns(unit) { return Some(n); }
    if depth > 8 { return None; }
    for decl in session.unit_registry.values() {
        for (from, from_n, to, to_n) in &decl.conversions {
            // `1 mo == 30 d` defines mo from d; `30 d == 1 mo` works too.
            let (count, per, base) = if from == unit && to != unit {
                (*to_n, *from_n, to.as_str())
            } else if to == unit && from != unit {
                (*from_n, *to_n, from.as_str())
            } else {
                continue;
            };
            if per == 0.0 { continue; }
            let Some(base_ns) = resolve(base, session, depth + 1) else { continue };
            // one `unit` = count/per `base`
            let exact = count.fract() == 0.0 && per.fract() == 0.0 && count.abs() < 1.0e30 && per.abs() < 1.0e30;
            let whole = if exact { (count as i128).checked_mul(base_ns) } else { None };
            let ns = match whole {
                Some(n) if n % per as i128 == 0 => n / per as i128,
                _ => {
                    let f = (count / per * base_ns as f64).round();
                    // too long to represent: treat the unit as unusable
                    if !f.is_finite() || f.abs() >= 1.0e38 { continue; }
                    f as i128
                }
            };
            if ns > 0 { return Some(ns); }
        }
    }
    None
}

fn big_f(d: &rust_decimal::Decimal) -> f64 {
    use rust_decimal::prelude::ToPrimitive;
    d.to_f64().unwrap_or(f64::NAN)
}

fn make(ns: i128, unit: String, unit_ns: i128) -> Value {
    Value::Duration(Box::new(Duration { ns, unit, unit_ns }))
}

/// `90s`: the number written before the unit, and the unit.
pub fn literal(n: &Value, unit: &str, session: &Session) -> Result<Value, GoblinError> {
    let per = unit_ns(unit, session)?;
    let ns = match n {
        Value::Int(i) => (*i as i128).checked_mul(per).ok_or_else(overflow)?,
        Value::Float(f) => scale_f(per, *f)?,
        other => return Err(GoblinError::type_error("number", other.type_name(), "duration")),
    };
    Ok(make(ns, unit.to_string(), per))
}

fn overflow() -> GoblinError {
    GoblinError::Runtime("R0505: duration-overflow: the duration is too long to represent".into())
}

fn scale_f(ns: i128, f: f64) -> Result<i128, GoblinError> {
    let r = (ns as f64 * f).round();
    if !r.is_finite() || r.abs() >= 1.0e38 { return Err(overflow()); }
    Ok(r as i128)
}

/// Text form: the length in its own unit, e.g. `90s`, `150m`, `1.5h`.
pub fn display(d: &Duration) -> String {
    if d.ns % d.unit_ns == 0 {
        format!("{}{}", d.ns / d.unit_ns, d.unit)
    } else {
        format!("{}{}", crate::builtins::fmt_num_trim(d.ns as f64 / d.unit_ns as f64), d.unit)
    }
}

/// `:int(2h)` is 2: the count in its own unit, truncated.
pub fn count_int(d: &Duration) -> Value {
    let q = d.ns / d.unit_ns;
    if let Ok(i) = i64::try_from(q) { Value::Int(i) } else { Value::Float(q as f64) }
}

pub fn count_float(d: &Duration) -> Value {
    Value::Float(d.ns as f64 / d.unit_ns as f64)
}

fn op_symbol(op: &str) -> &str {
    match op { "add" => "+", "sub" => "-", "mul" => "*", "div" => "/", "rem" => "%", o => o }
}

fn mismatch(op: &str, a: &Value, b: &Value) -> GoblinError {
    let sym = op_symbol(op);
    let hint = match op {
        "add" | "sub" => "write the number as a duration, e.g. 2h + 30m",
        "mul" => "multiply a duration by a number, e.g. 2h * 3",
        "div" => "divide a duration by a number or by another duration",
        _ => "durations only add, subtract, multiply or divide by a number, and divide by a duration",
    };
    GoblinError::Runtime(format!(
        "T0205: type-mismatch: cannot use '{sym}' with {} and {}; {hint}",
        a.type_name(), b.type_name()))
}

/// Arithmetic where at least one side is a duration.
pub fn binop(op: &str, a: &Value, b: &Value) -> Result<Value, GoblinError> {
    match (op, a, b) {
        ("add" | "sub", Value::Duration(x), Value::Duration(y)) => {
            let ns = if op == "add" { x.ns.checked_add(y.ns) } else { x.ns.checked_sub(y.ns) }
                .ok_or_else(overflow)?;
            let keep = if y.unit_ns < x.unit_ns { y } else { x };
            Ok(make(ns, keep.unit.clone(), keep.unit_ns))
        }
        ("mul", Value::Duration(x), n) | ("mul", n, Value::Duration(x)) if !matches!(n, Value::Duration(_)) => {
            let ns = match n {
                Value::Int(i) => x.ns.checked_mul(*i as i128).ok_or_else(overflow)?,
                Value::Float(f) => scale_f(x.ns, *f)?,
                Value::Big(d) => scale_f(x.ns, big_f(d))?,
                _ => return Err(mismatch(op, a, b)),
            };
            Ok(make(ns, x.unit.clone(), x.unit_ns))
        }
        ("div", Value::Duration(x), Value::Duration(y)) => {
            if y.ns == 0 { return Err(GoblinError::DivisionByZero); }
            if x.ns % y.ns == 0 {
                let q = x.ns / y.ns;
                Ok(i64::try_from(q).map(Value::Int).unwrap_or(Value::Float(q as f64)))
            } else {
                Ok(Value::Float(x.ns as f64 / y.ns as f64))
            }
        }
        ("div", Value::Duration(x), n) => {
            let ns = match n {
                Value::Int(0) => return Err(GoblinError::DivisionByZero),
                Value::Int(i) => {
                    let i = *i as i128;
                    // round half away from zero
                    let q = x.ns / i;
                    let r = x.ns % i;
                    if 2 * r.abs() >= i.abs() { q + if (x.ns < 0) == (i < 0) { 1 } else { -1 } } else { q }
                }
                Value::Big(d) if d.is_zero() => return Err(GoblinError::DivisionByZero),
                Value::Float(f) if *f == 0.0 => return Err(GoblinError::DivisionByZero),
                Value::Float(_) | Value::Big(_) => {
                    let f = match n { Value::Big(d) => big_f(d), Value::Float(f) => *f, _ => unreachable!() };
                    let r = (x.ns as f64 / f).round();
                    if !r.is_finite() || r.abs() >= 1.0e38 { return Err(overflow()); }
                    r as i128
                }
                _ => return Err(mismatch(op, a, b)),
            };
            Ok(make(ns, x.unit.clone(), x.unit_ns))
        }
        _ => Err(mismatch(op, a, b)),
    }
}

pub fn neg(d: &Duration) -> Value {
    make(-d.ns, d.unit.clone(), d.unit_ns)
}
