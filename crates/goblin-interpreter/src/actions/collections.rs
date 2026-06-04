//! Polymorphic collection helpers: has, count

use crate::{Session, Value, Diag, Span};
use goblin_diagnostics::{Diagnostic, Severity};
use crate::diagnostics::rtcode;

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

use std::collections::{BTreeMap, BTreeSet};
use crate::call_action_by_name;
use crate::fmt_value_raw;

/// has(container, needle) -> Bool
/// - String: substring or char membership
/// - Array/Seq: element equality
/// - Map/MapOrd: key presence (string or char key)
/// - Nil/Unit: false
pub fn has(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘has’ takes exactly 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let hay = &args[0];
    let needle = &args[1];

    let ok = match hay {
        // String membership: substring or char
        Value::Str(s) => match needle {
            Value::Str(sub) => s.contains(sub),
            Value::Char(ch) => s.contains(*ch),
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "For strings, ‘has’ expects Str or Char as the second argument.",
                        sp.clone(),
                    )
                    .with_help("Use: has(\"abcdef\",\"cd\") or has(\"abc\", 'b').")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            }
        },

        // Array membership: element equality
        Value::Array(xs) => xs.iter().any(|v| v == needle),

        // Seq membership: element equality (via as_slice Option)
        Value::Seq(xs) => xs
            .as_slice()
            .map(|sl| sl.iter().any(|v| v == needle))
            .unwrap_or(false),

        // Map membership: key presence (any scalar key)
        Value::Map(m) => {
            let key = match needle {
                Value::Str(s)   => s.clone(),
                Value::Char(c)  => c.to_string(),
                Value::Int(n)   => n.to_string(),
                Value::Float(n) => n.to_string(),
                Value::Big(n)   => n.to_string(),
                Value::Bool(b)  => b.to_string(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "For maps, 'has' expects a string, char, number, or bool key.",
                            sp.clone(),
                        )
                        .with_help("Use: has({a:1}, \"a\") or has(seen, 7).")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };
            m.contains_key(&key)
        },

        // Ordered map membership: identical behavior
        Value::MapOrd(m) => {
            let key = match needle {
                Value::Str(s)   => s.clone(),
                Value::Char(c)  => c.to_string(),
                Value::Int(n)   => n.to_string(),
                Value::Float(n) => n.to_string(),
                Value::Big(n)   => n.to_string(),
                Value::Bool(b)  => b.to_string(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "For maps, 'has' expects a string, char, number, or bool key.",
                            sp.clone(),
                        )
                        .with_help("Use: has({a:1}, \"a\") or has(seen, 7).")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };
            m.contains_key(&key)
        },

        // Nil/Unit → false
        Value::Nil | Value::Unit => false,

        // Not supported types → clear error
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘has’ is defined for strings, arrays/sequences, and maps.",
                    sp.clone(),
                )
                .with_help("Examples: has([1,2,3], 2), has(\"abc\",\"b\"), has({a:1}, \"a\").")
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            );
        }
    };

    Ok(Value::Bool(ok))
}

/// count(x) -> Int length
/// count(s, sub) -> Int occurrences (non-overlapping)
pub fn count(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    match args.len() {
        // count(x) -> length
        1 => {
            let n = match &args[0] {
                Value::Nil        => 0,
                Value::Unit       => 0,
                Value::Str(s)     => s.chars().count(),
                Value::Array(xs)  => xs.len(),
                Value::Seq(xs)    => xs.as_slice().map(|sl| sl.len()).unwrap_or(0),
                Value::Map(m)     => m.len(),
                Value::MapOrd(m)  => m.len(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘count’ expects a string or collection",
                            sp.clone(),
                        )
                        .with_help("Pass a string, array/seq, or map to ‘count’.")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };
            Ok(Value::Int(n as i64))
        }

        // count(s, sub) -> substring occurrences
        2 => {
            let s = if let Value::Str(s) = &args[0] {
                s.as_str()
            } else {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "‘count(s, sub)’ expects ‘s’ to be a string.",
                        sp.clone(),
                    )
                    .with_help("Use: count(\"abracadabra\", \"abra\").")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            };
            let sub = if let Value::Str(su) = &args[1] {
                su.as_str()
            } else {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "‘count(s, sub)’ expects ‘sub’ to be a string.",
                        sp.clone(),
                    )
                    .with_help("Use: count(\"abracadabra\", \"abra\").")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            };

            if sub.is_empty() {
                return Ok(Value::Int(0));
            }

            let mut n = 0usize;
            let mut start = 0usize;
            while let Some(pos) = s[start..].find(sub) {
                n += 1;
                start = start + pos + sub.len();
            }
            Ok(Value::Int(n as i64))
        }

        k => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 1 or 2, got {})", k),
                sp.clone(),
            )
            .with_help("‘count’ takes 1 or 2 arguments.")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        ),
    }
}

// shuffle(x)
pub fn shuffle(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "shuffle", sp)?;

    let out = match &args[0] {
        // string: shuffle characters
        Value::Str(s) => {
            let mut v: Vec<char> = s.chars().collect();
            for i in 0..v.len() {
                let j = i + rng_index(sess, v.len() - i);
                v.swap(i, j);
            }
            Value::Str(v.into_iter().collect())
        }

        // integer: shuffle decimal digits, preserve sign, return Int
        Value::Int(n) => {
            let neg = *n < 0;
            let mut m = if neg { -*n } else { *n };

            // explode digits
            let mut digs: Vec<i64> = if m == 0 {
                vec![0]
            } else {
                let mut tmp = Vec::new();
                while m > 0 {
                    tmp.push((m % 10) as i64);
                    m /= 10;
                }
                tmp.reverse();
                tmp
            };

            // Fisher–Yates on digits
            for i in 0..digs.len() {
                let j = i + rng_index(sess, digs.len() - i);
                digs.swap(i, j);
            }

            // repack with checked math
            let mut acc: i128 = 0;
            for d in digs {
                match acc.checked_mul(10).and_then(|a| a.checked_add(d as i128)) {
                    Some(v) => acc = v,
                    None => return Ok(Value::Nil), // overflow → Nil (matches your arm)
                }
            }
            if neg { acc = -acc; }
            Value::Int(acc as i64)
        }

        // array-like: shuffle elements
        _ => {
            let s = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘shuffle’ expects a string or collection (array/seq).",
                    sp.clone(),
                )
                .with_help("Pass a string to shuffle characters, or an array/seq to shuffle elements.")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;
            let mut v = s.to_vec();
            for i in 0..v.len() {
                let j = i + rng_index(sess, v.len() - i);
                v.swap(i, j);
            }
            Value::Array(v)
        }
    };

    Ok(out)
}

// sort(x)
pub fn sort(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "sort", sp)?;

    let out = match &args[0] {
        // string: sort chars (Unicode scalar)
        Value::Str(s) => {
            let mut v: Vec<char> = s.chars().collect();
            v.sort_unstable();
            Value::Str(v.into_iter().collect())
        }

        // integer: sort digits ascending, preserve sign, return Int
        Value::Int(n) => {
            let neg = *n < 0;
            let mut m = if neg { -*n } else { *n };

            // explode digits
            let mut digs: Vec<i64> = if m == 0 {
                vec![0]
            } else {
                let mut tmp = Vec::new();
                while m > 0 {
                    tmp.push((m % 10) as i64);
                    m /= 10;
                }
                tmp.reverse();
                tmp
            };

            digs.sort_unstable();

            // repack with checked math
            let mut acc: i128 = 0;
            for d in digs {
                match acc.checked_mul(10).and_then(|a| a.checked_add(d as i128)) {
                    Some(v) => acc = v,
                    None => return Ok(Value::Nil), // overflow → Nil
                }
            }
            if neg { acc = -acc; }
            Value::Int(acc as i64)
        }

        // array-like: sort by fmt_value_raw
        _ => {
            let s = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘sort’ expects a string or collection (array/seq).",
                    sp.clone(),
                )
                .with_help("Pass a string to sort characters, or an array/seq to sort elements.")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;
            let mut v = s.to_vec();
            v.sort_by(|a, b| fmt_value_raw(a).cmp(&fmt_value_raw(b)));
            Value::Array(v)
        }
    };

    Ok(out)
}

// freq(x)
pub fn freq(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "freq", sp)?;

    let out = match &args[0] {
        Value::Str(s) => {
            let mut cnt = BTreeMap::<char, i64>::new();
            for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
            let mut m = BTreeMap::<String, Value>::new();
            for (c, n) in cnt { m.insert(c.to_string(), Value::Int(n)); }
            Value::Map(m)
        }
        _ => {
            let xs = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘freq’ expects a string or collection (array/seq).",
                    sp.clone(),
                )
                .with_help("Pass a string to count character frequency, or an array/seq to count element frequency.")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;
            let mut tally = BTreeMap::<String, i64>::new();
            for v in xs { *tally.entry(fmt_value_raw(v)).or_insert(0) += 1; }
            let mut out = BTreeMap::<String, Value>::new();
            for (k, n) in tally { out.insert(k, Value::Int(n)); }
            Value::Map(out)
        }
    };

    Ok(out)
}

// mode(x) -> Map { <repr>: count }
pub fn mode(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "mode", sp)?;

    let xs = as_array_like(&args[0]).ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
            "type-mismatch",
            "‘mode’ expects an array/seq.",
            sp.clone(),
        )
        .with_help("Pass an array or seq of values to compute the most frequent element.")
        .with_link("https://goblinlang.org/docs/errors#T0205")
    })?;

    if xs.is_empty() {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::EMPTY_ARRAY, // R0404
                "empty-array",
                "mode of empty array",
                sp.clone(),
            )
            .with_help("Provide at least one element.")
            .with_link("https://goblinlang.org/docs/errors#R0404"),
        );
    }

    let mut counts = BTreeMap::<String, i64>::new();
    for v in xs {
        let k = fmt_value_raw(v);
        *counts.entry(k).or_insert(0) += 1;
    }
    let mut best_k = String::new();
    let mut best_n = -1i64;
    for (k, n) in counts.iter() {
        if *n > best_n { best_n = *n; best_k = k.clone(); }
    }
    Ok(Value::Map(vec![(best_k, Value::Int(best_n))].into_iter().collect()))
}

// sample_weighted({ src, weights, count? })
pub fn sample_weighted(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "sample_weighted", sp)?;

    // config map
    let cfg = match &args[0] {
        Value::Map(m) => m,
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "config-expected",
                    "‘sample_weighted’ expects a config object (map).",
                    sp.clone(),
                )
                .with_help("Pass a map like { src: [...], weights: [...], count: 3 }.")
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    // src
    let s = cfg.get("src").ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
            "missing-field",
            "missing required field ‘src’.",
            sp.clone(),
        )
        .with_help("Provide a collection in ‘src’, e.g. { src: [\"a\",\"b\"], weights: [1,2] }")
        .with_link("https://goblinlang.org/docs/errors#R0403")
    })?;
    let xs = as_array_like(s).ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
            "type-mismatch",
            "‘src’ must be an array or seq.",
            sp.clone(),
        )
        .with_help("Example: { src: [10, 20, 30], weights: [1, 2, 3] }")
        .with_link("https://goblinlang.org/docs/errors#T0205")
    })?;

    // weights
    let wsv = cfg.get("weights").ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
            "missing-field",
            "missing required field ‘weights’.",
            sp.clone(),
        )
        .with_help("Provide an array/seq of numeric weights matching the length of ‘src’.")
        .with_link("https://goblinlang.org/docs/errors#R0403")
    })?;
    let ws = as_array_like(wsv).ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
            "type-mismatch",
            "‘weights’ must be an array or seq.",
            sp.clone(),
        )
        .with_help("Example: { src: [\"a\",\"b\"], weights: [0.3, 0.7] }")
        .with_link("https://goblinlang.org/docs/errors#T0205")
    })?;

    if xs.is_empty() {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                "empty-collection",
                "cannot sample from an empty ‘src’.",
                sp.clone(),
            )
            .with_help("Provide at least one element in ‘src’.")
            .with_link("https://goblinlang.org/docs/errors#R0701"),
        );
    }
    if xs.len() != ws.len() {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WEIGHTS_LEN_MISMATCH, // R0708
                "length-mismatch",
                "‘weights’ length must match ‘src’.",
                sp.clone(),
            )
            .with_help(&format!("src has {}, weights has {}.", xs.len(), ws.len()))
            .with_help("Make both arrays the same length.")
            .with_link("https://goblinlang.org/docs/errors#R0708"),
        );
    }

    // count
    let n_out: usize = match cfg.get("count") {
        None => 1,
        Some(Value::Float(n)) if *n > 0.0 && n.fract() == 0.0 => *n as usize,
        Some(Value::Int(i)) if *i > 0 => *i as usize,
        Some(_) => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                    "positive-int-expected",
                    "‘sample_weighted.count’ must be a positive integer.",
                    sp.clone(),
                )
                .with_help("Use an integer ≥ 1, e.g. { count: 3 }")
                .with_link("https://goblinlang.org/docs/errors#T0202"),
            )
        }
    };

    // cumulative weights
    let mut cum = Vec::with_capacity(ws.len());
    let mut sum = 0.0;
    for w in ws {
        let w = want_num(w, "weights", sp)?; // emits NUMERIC_EXPECTED if not numeric
        if w < 0.0 {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                    "math-domain",
                    "weights must be ≥ 0.",
                    sp.clone(),
                )
                .with_help("Remove negative weights or clamp them to zero.")
                .with_link("https://goblinlang.org/docs/errors#R0207"),
            );
        }
        sum += w;
        cum.push(sum);
    }
    if sum == 0.0 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                "math-domain",
                "all weights are zero.",
                sp.clone(),
            )
            .with_help("At least one weight must be > 0.")
            .with_link("https://goblinlang.org/docs/errors#R0207"),
        );
    }

    // sample with replacement
    let mut out = Vec::with_capacity(n_out);
    for _ in 0..n_out {
        let r = rng_u01(sess) * sum; // in [0,sum)
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

pub fn map(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(2, args.len(), "map", sp)?;
    if args.len() != 2 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                "wrong-arity",
                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                sp.clone(),
            )
            .with_help("‘map’ takes exactly 2 arguments: (collection, actionName).")
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }

    let action = match &args[1] {
        Value::Str(s) => s.clone(),
        _ => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "action-name-string-expected",
                    "‘map’ expects the action name as a string.",
                    sp.clone(),
                )
                .with_help(r#"Example: map(["a","b"], "upper")"#)
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    // String → map over chars; collapse back to string if all results are Char/Str
    if let Value::Str(s) = &args[0] {
        let mut results: Vec<Value> = Vec::with_capacity(char_len(s));
        let mut any_non_text = false;
        for c in s.chars() {
            let r = call_action_by_name(sess, &action, vec![Value::Char(c)], sp.clone())?;
            match r {
                Value::Char(_) | Value::Str(_) => {}
                _ => { any_non_text = true; }
            }
            results.push(r);
        }
        if !results.is_empty() && !any_non_text {
            let mut out = String::new();
            for r in results {
                match r {
                    Value::Char(ch) => out.push(ch),
                    Value::Str(ts)  => out.push_str(&ts),
                    _ => unreachable!(),
                }
            }
            return Ok(Value::Str(out));
        }
        return Ok(Value::Array(results));
    }

    // Array/Seq
    let xs = match as_array_like(&args[0]) {
        Some(a) => a,
        None => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "collection-expected",
                    "‘map’ expects array/seq/string as first argument.",
                    sp.clone(),
                )
                .with_help(r#"Examples: map(["a","b"], "upper"), map(chars("abc"), "upper")"#)
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            )
        }
    };

    let mut out = Vec::with_capacity(xs.len());
    for v in xs {
        let r = call_action_by_name(sess, &action, vec![v.clone()], sp.clone())?;
        out.push(r);
    }
    Ok(Value::Array(out))
}

// unique(x)
pub fn unique(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "unique", sp)?;

    let out = match &args[0] {
        Value::Str(s) => {
            use std::collections::BTreeSet;
            let mut seen = BTreeSet::new();
            let mut out = String::new();
            for c in s.chars() {
                if seen.insert(c) { out.push(c); }
            }
            Value::Str(out)
        }
        _ => {
            let s = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘unique’ expects array/seq/string.",
                    sp.clone(),
                )
                .with_help("Pass a string or a collection (array/seq).")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;
            use std::collections::BTreeSet;
            let mut seen = BTreeSet::<String>::new();
            let mut outv = Vec::with_capacity(s.len());
            for v in s {
                let k = fmt_value_raw(v);
                if seen.insert(k) { outv.push(v.clone()); }
            }
            Value::Array(outv)
        }
    };

    Ok(out)
}

// dups(x)
pub fn dups(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "dups", sp)?;

    let out = match &args[0] {
        Value::Str(s) => {
            use std::collections::BTreeMap;
            let mut cnt = BTreeMap::<char, usize>::new();
            for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
            let mut out = String::new();
            for (c, n) in cnt { if n >= 2 { out.push(c); } }
            Value::Str(out)
        }
        _ => {
            let s = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘dups’ expects array/seq/string.",
                    sp.clone(),
                )
                .with_help("Pass a string or a collection (array/seq).")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;
            use std::collections::BTreeMap;
            let mut cnt = BTreeMap::<String, (usize, Value)>::new();
            for v in s {
                let k = fmt_value_raw(v);
                cnt.entry(k).and_modify(|e| e.0 += 1).or_insert((1, v.clone()));
            }
            let mut out = Vec::new();
            for (_, (n, exemplar)) in cnt {
                if n >= 2 { out.push(exemplar); }
            }
            Value::Array(out)
        }
    };

    Ok(out)
}