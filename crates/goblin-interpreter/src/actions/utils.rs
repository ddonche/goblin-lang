//! Shared helpers for actions modules (strings, collections, etc.)

use crate::{Session, Value, Diag, Span};
use goblin_diagnostics::{Diagnostic, Severity};
use crate::diagnostics::rtcode;

/// Treat Array/Seq uniformly as a slice of Values.
#[inline]
pub fn as_array_like<'a>(v: &'a Value) -> Option<&'a [Value]> {
    match v {
        Value::Array(xs) => Some(xs.as_slice()),
        Value::Seq(xs)   => xs.as_slice(), // relies on your Seq::as_slice()
        _ => None,
    }
}

/// Arity checker with your standard R0301 diagnostic.
pub fn arity(expected: usize, args_len: usize, label: &str, sp: &Span) -> Result<(), Diag> {
    if args_len != expected {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("{label} expects {expected} argument(s), got {args_len}"),
                sp.clone(),
            )
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    Ok(())
}

/// Require a string Value, else T0205.
pub fn want_str<'a>(v: &'a Value, label: &str, sp: &Span) -> Result<&'a str, Diag> {
    if let Value::Str(s) = v {
        Ok(s.as_str())
    } else {
        Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::TYPE_MISMATCH, // T0205
                "type-mismatch",
                &format!("{label} expects a string."),
                sp.clone(),
            )
            .with_help("Pass a string value.")
            .with_link("https://goblinlang.org/docs/errors#T0205"),
        )
    }
}

/// Require a numeric (Float) Value, else numeric-expected.
pub fn want_num(v: &Value, label: &str, sp: &Span) -> Result<f64, Diag> {
    match v {
        Value::Float(n) => Ok(*n),
        _ => Err(need_number(label, sp.clone())),
    }
}

/// Require a boolean Value, else T0203.
pub fn want_bool(v: &Value, label: &str, sp: &Span) -> Result<bool, Diag> {
    match v {
        Value::Bool(b) => Ok(*b),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::BOOLEAN_EXPECTED, // T0203
                "boolean-expected",
                format!("{label} must be a boolean (true or false)"),
                sp.clone(),
            )
            .with_help("Use `true` or `false`, or an expression that evaluates to a boolean.")
            .with_link("https://goblinlang.org/docs/errors#T0203"),
        ),
    }
}

/// Character-count for Unicode scalar semantics.
#[inline]
pub fn char_len(s: &str) -> usize { s.chars().count() }

/// RNG: unbiased index in [0, len).
#[inline]
pub fn rng_index(sess: &mut Session, len: usize) -> usize {
    if len == 0 { return 0; }
    rng_bounded(sess, len as u64) as usize
}

/// RNG: uniform double in [0,1) with 53 bits.
#[inline]
pub fn rng_u01(sess: &mut Session) -> f64 {
    let bits53 = (sess.next_u128() >> 75) as u64; // keep top 53 random bits
    (bits53 as f64) / ((1u64 << 53) as f64)
}

/// Numeric expected diagnostic (R0201) used by want_num and elsewhere.
pub fn need_number(what: &str, span: Span) -> Diagnostic {
    Diagnostic::new_with_code(
        Severity::Error,
        "R0201",
        "type-mismatch",
        &format!("{what} expects a numeric value"),
        span.clone(),
    )
    .with_help("Provide an Int, Float, Big, or Pct value.")
    .with_help("Example: x = 42 or x = 3.14")
    .with_link("https://goblinlang.org/docs/errors#R0201")
}

/// Generic runtime error builder (kept for parity with original helpers).
pub fn rt(code: &'static str, message: impl Into<String>, sp: Span) -> Diag {
    Diagnostic::new_with_code(
        Severity::Error,
        code,
        "runtime-error",
        message,
        sp,
    )
}

// ---- private RNG plumbing --------------------------------------------------

#[inline]
fn rng_u64(sess: &mut Session) -> u64 {
    // use the high 64 bits; LCG low bits are the problem
    (sess.next_u128() >> 64) as u64
}

// Lemire's unbiased bounded integer (uniform in [0, bound))
#[inline]
fn rng_bounded(sess: &mut Session, bound: u64) -> u64 {
    if bound == 0 { return 0; }
    loop {
        let x = rng_u64(sess);
        let m = (x as u128).wrapping_mul(bound as u128);
        let l = m as u64;
        let t = bound.wrapping_neg() % bound; // threshold to avoid bias
        if l >= t {
            return (m >> 64) as u64;
        }
        // else retry
    }
}

// Useful if you need dice-style rolls elsewhere.
#[allow(dead_code)]
#[inline]
fn rng_roll_1_to_s(sess: &mut Session, sides: i64) -> i64 {
    debug_assert!(sides > 0);
    (rng_bounded(sess, sides as u64) as i64) + 1
}
