use crate::{Session, Value, Diag, Span};
use goblin_diagnostics::{Diagnostic, Severity};

use crate::actions::utils::{
    arity,
    as_array_like,
    want_str,
    want_num,
    want_bool,
    char_len,
    rng_index,
    rng_u01,
};

const RAW_SENTINEL: &str = "\u{001E}RAW:";

// HELPERS
#[inline]

// Replace your existing map_str_1 with this version.

fn map_str_1(
    v: &Value,
    label: &str,
    f: &dyn Fn(&str) -> String,
    sp: &Span
) -> Result<Value, Diag> {
    // String input → String output
    if let Value::Str(s) = v {
        return Ok(Value::Str(f(s)));
    }

    // Char input → Char if result is 1 scalar, else String
    if let Value::Char(ch) = v {
        let out = f(&ch.to_string());
        let mut iter = out.chars();
        if let (Some(c0), None) = (iter.next(), iter.next()) {
            return Ok(Value::Char(c0));
        } else {
            return Ok(Value::Str(out));
        }
    }

    // Array/Seq input → map over elements (must be Str or Char)
    if let Some(xs) = as_array_like(v) {
        let mut out = Vec::with_capacity(xs.len());
        for it in xs {
            match it {
                Value::Str(s) => out.push(Value::Str(f(s))),
                Value::Char(ch) => {
                    let out_s = f(&ch.to_string());
                    let mut iter = out_s.chars();
                    if let (Some(c0), None) = (iter.next(), iter.next()) {
                        out.push(Value::Char(c0));
                    } else {
                        out.push(Value::Str(out_s));
                    }
                }
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            &format!("{label} expects a string/char (or array/seq of strings/chars)"),
                            sp.clone(),
                        )
                        .with_help("Pass a string/char or an array/seq of strings/chars.")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            }
        }
        return Ok(Value::Array(out));
    }

    // Not a string/char/collection
    Err(
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
            "type-mismatch",
            &format!("{label} expects a string/char (or array/seq of strings/chars)"),
            sp.clone(),
        )
        .with_help("Pass a string/char or an array/seq of strings/chars.")
        .with_link("https://goblinlang.org/docs/errors#T0205"),
    )
}

pub fn lower(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "lower", sp)?;
    map_str_1(&args[0], "lower", &|s| s.to_lowercase(), sp)
}

pub fn upper(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "upper", sp)?;
    map_str_1(&args[0], "upper", &|s| s.to_uppercase(), sp)
}

pub fn title(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "title", sp)?;
    let v = &args[0];

    // Apply transformation to string or array of strings
    map_str_1(v, "title", &|s| {
        let mut out = String::with_capacity(s.len());
        for (i, w) in s.split_whitespace().enumerate() {
            if i > 0 {
                out.push(' ');
            }
            let mut chs = w.chars();
            if let Some(first) = chs.next() {
                out.extend(first.to_uppercase());
                let rest: String = chs.collect();
                out.push_str(&rest.to_lowercase());
            }
        }
        out
    }, sp)
}

pub fn slug(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "slug", sp)?;
    let v = &args[0];

    map_str_1(v, "slug", &|s| {
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
    }, sp)
}

pub fn mixed(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "mixed", sp)?;
    let v = &args[0];

    // draw one seed from the session RNG, then do pure mixing per char
    let seed = sess.next_u128();

    let to_mixed = move |s: &str| -> String {
        let mut out = String::with_capacity(s.len());
        for (i, ch) in s.chars().enumerate() {
            // SplitMix-style stateless mixing from (seed ^ i)
            let mut x = seed ^ ((i as u128).wrapping_mul(0x9E37_79B9_7F4A_7C15));
            x ^= x >> 30;
            x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
            x ^= x >> 27;
            x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
            x ^= x >> 31;

            let upper = (x & 1) == 1;
            if ch.is_alphabetic() {
                if upper {
                    out.extend(ch.to_uppercase());
                } else {
                    out.extend(ch.to_lowercase());
                }
            } else {
                out.push(ch);
            }
        }
        out
    };

    map_str_1(v, "mixed", &to_mixed, sp)
}

pub fn raw(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "raw", sp)?;
    map_str_1(&args[0], "raw", &|s| {
        // Just return the string as-is, no sentinel needed
        s.to_string()
    }, sp)
}

pub fn trim(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "trim", sp)?;
    map_str_1(&args[0], "trim", &|s| {
        s.trim_matches(|c: char|
            c.is_whitespace()
            || c == '\u{00A0}' // NBSP
            || c == '\u{FEFF}' // BOM / ZWNBSP
            || c == '\u{200B}' // ZERO WIDTH SPACE
            || c == '\u{200C}' // ZWNJ
            || c == '\u{200D}' // ZWJ
            || c == '\u{2060}' // WORD JOINER
            || c == '\u{180E}' // MVS (legacy)
        ).to_string()
    }, sp)
}

pub fn trim_lead(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "trim_lead", sp)?;
    
    map_str_1(&args[0], "trim_lead", &|s| {
        s.trim_start_matches(|c: char|
            c.is_whitespace()
            || c == '\u{00A0}' // NBSP
            || c == '\u{FEFF}' // BOM / ZWNBSP
            || c == '\u{200B}' // ZERO WIDTH SPACE
            || c == '\u{200C}' // ZWNJ
            || c == '\u{200D}' // ZWJ
            || c == '\u{2060}' // WORD JOINER
            || c == '\u{180E}' // MVS (legacy)
        ).to_string()
    }, sp)
}

pub fn trim_trail(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "trim_trail", sp)?;
    map_str_1(&args[0], "trim_trail", &|s| {
        s.trim_end_matches(|c: char|
            c.is_whitespace()
            || c == '\u{00A0}'
            || c == '\u{FEFF}'
            || c == '\u{200B}'
            || c == '\u{200C}'
            || c == '\u{200D}'
            || c == '\u{2060}'
            || c == '\u{180E}'
        ).to_string()
    }, sp)
}

// find(s, sub) -> first index (Int, 0-based) or Nil
pub fn find(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘find’ takes exactly 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let s   = want_str(&args[0], "find", sp)?;
    let sub = want_str(&args[1], "find", sp)?;
    Ok(match s.find(sub) {
        Some(i) => Value::Int(i as i64),
        None => Value::Nil,
    })
}

// find_all(s, sub) -> Array[Int] (non-overlapping)
pub fn find_all(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘find_all’ takes exactly 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    let s   = want_str(&args[0], "find_all", sp)?;
    let sub = want_str(&args[1], "find_all", sp)?;

    if sub.is_empty() {
        return Ok(Value::Array(vec![]));
    }

    let mut out = Vec::new();
    let mut start = 0usize;
    while let Some(pos) = s[start..].find(sub) {
        let idx = start + pos;
        out.push(Value::Int(idx as i64));
        start = idx + sub.len(); // non-overlapping
    }
    Ok(Value::Array(out))
}

pub fn ord(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    // Same arity style as `lower`, `upper`, etc.
    arity(1, args.len(), "ord", sp)?;

    let v = &args[0];

    // Extract exactly one char, or fail.
    let ch_opt: Option<char> = match v {
        Value::Char(c) => Some(*c),

        Value::Str(s) => {
            let mut it = s.chars();
            match (it.next(), it.next()) {
                // exactly one Unicode scalar
                (Some(c0), None) => Some(c0),
                // empty or multi-char string → not allowed for ord
                _ => None,
            }
        }

        _ => None,
    };

    if let Some(ch) = ch_opt {
        // Rust char is a Unicode scalar; cast to u32, then i64 for Value::Int
        Ok(Value::Int(ch as u32 as i64))
    } else {
        Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                "type-mismatch",
                "‘ord’ expects a char or single-character string.",
                sp.clone(),
            )
            .with_help("Pass a Char or a Str of length 1, e.g. ord('A') or ord(\"A\").")
            .with_link("https://goblinlang.org/docs/errors#T0205"),
        )
    }
}
