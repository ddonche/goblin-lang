use crate::{Session, Value, Diag, Span};
use goblin_diagnostics::{Diagnostic, Severity};

#[inline]
fn as_array_like<'a>(v: &'a Value) -> Option<&'a [Value]> {
    match v {
        Value::Array(xs) => Some(xs.as_slice()),
        Value::Seq(xs)   => xs.as_slice(),   // uses your Seq::as_slice()
        _ => None,
    }
}

fn map_str_1(v: &Value, label: &str, f: &dyn Fn(&str) -> String, sp: &Span)
    -> Result<Value, Diag>
{
    match v {
        Value::Str(s) => Ok(Value::Str(f(s))),
        _ => {
            if let Some(xs) = as_array_like(v) { // handles Array or Seq
                let mut out = Vec::with_capacity(xs.len());
                for it in xs {
                    match it {
                        Value::Str(s) => out.push(Value::Str(f(s))),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    &format!("{label} expects a string (or array/seq of strings)"),
                                    sp.clone(),
                                )
                                .with_help("Pass a string or an array/seq of strings.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                    }
                }
                Ok(Value::Array(out))
            } else {
                Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        &format!("{label} expects a string (or array/seq of strings)"),
                        sp.clone(),
                    )
                    .with_help("Pass a string or an array/seq of strings.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                )
            }
        }
    }
}

fn arity(expected: usize, args_len: usize, label: &str, sp: &Span) -> Result<(), Diag> {
    if args_len != expected {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::WRONG_ARITY, // whatever code you use
                "wrong-arity",
                &format!("{label} expects {expected} argument(s), got {args_len}"),
                sp.clone(),
            )
            .with_link("https://goblinlang.org/docs/errors#R0301"),
        );
    }
    Ok(())
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

pub fn raw(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    arity(1, args.len(), "raw", sp)?;
    map_str_1(&args[0], "raw", &|s| {
        let mut out = String::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                '{' => { out.push('{'); out.push('{'); }
                '}' => { out.push('}'); out.push('}'); }
                _   => out.push(ch),
            }
        }
        out
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
