use std::collections::BTreeMap;

use crate::value::YallValue;

/// Write Y’all respecting full v1.0 spec.
/// Top-level is always a block map.
pub fn yall_write(value: &YallValue) -> String {
    let mut out = String::new();
    write_value(&mut out, value, 0, true);
    out
}

fn write_value(out: &mut String, v: &YallValue, indent: usize, top_level: bool) {
    match v {
        YallValue::Map(m) => {
            if top_level {
                write_block_map(out, m, indent);
            } else {
                write_inline_or_block_map(out, m, indent);
            }
        }

        YallValue::Array(a) => {
            write_inline_or_block_array(out, a, indent);
        }

        YallValue::Str(s) => {
            write_string(out, s);
        }

        YallValue::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        YallValue::Null => out.push_str("null"),
        YallValue::Int(i) => out.push_str(&i.to_string()),
        YallValue::Float(f) => out.push_str(&f.to_string()),
    }
}

// ----------------------------------------------------
// STRING RULES
// ----------------------------------------------------

fn write_string(out: &mut String, s: &str) {
    if is_bare_ok(s) {
        out.push_str(s);
    } else {
        out.push('"');
        for ch in s.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '"' => out.push_str("\\\""),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                c => out.push(c),
            }
        }
        out.push('"');
    }
}

/// Bare strings may ONLY contain:
/// - a-zA-Z0-9
/// - _
/// - -
/// - # (not comment start)
/// - no spaces, no punctuation, no colons, no braces
fn is_bare_ok(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    for ch in s.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '#' => {}
            _ => return false,
        }
    }

    true
}

// ----------------------------------------------------
// MAP WRITING
// ----------------------------------------------------

fn write_block_map(
    out: &mut String,
    map: &BTreeMap<String, YallValue>,
    indent: usize,
) {
    for (key, val) in map {
        write_indent(out, indent);
        out.push_str(key);
        out.push(':');

        match val {
            YallValue::Map(m) if !m.is_empty() => {
                out.push('\n');
                write_block_map(out, m, indent + 2);
            }
            YallValue::Array(a) if !a.is_empty() => {
                out.push('\n');
                write_block_list(out, a, indent + 2);
            }
            other => {
                if is_simple_scalar(other) {
                    out.push(' ');
                    write_value(out, other, indent, false);
                    out.push('\n');
                } else {
                    out.push('\n');
                    write_value(out, other, indent + 2, false);
                    out.push('\n');
                }
            }
        }
    }
}

fn is_simple_scalar(v: &YallValue) -> bool {
    matches!(
        v,
        YallValue::Null
            | YallValue::Bool(_)
            | YallValue::Int(_)
            | YallValue::Float(_)
            | YallValue::Str(_)
    )
}

fn write_block_list(out: &mut String, arr: &[YallValue], indent: usize) {
    for item in arr {
        write_indent(out, indent);
        out.push_str("- ");

        match item {
            // Sequence of maps:
            // routes:
            //   - slug: foo
            //     href: /default/foo.html
            YallValue::Map(m) => {
                let mut iter = m.iter();

                // First key goes on the same line as the dash.
                if let Some((first_k, first_v)) = iter.next() {
                    out.push_str(first_k);
                    out.push_str(": ");
                    write_value(out, first_v, indent + 2, false);
                    out.push('\n');
                } else {
                    // Empty map entry: just end the dash line.
                    out.push('\n');
                }

                // Remaining keys go on following indented lines.
                for (k, v) in iter {
                    write_indent(out, indent + 2);
                    out.push_str(k);
                    out.push_str(": ");
                    write_value(out, v, indent + 2, false);
                    out.push('\n');
                }
            }

            // Simple scalars – keep previous behavior.
            _ if is_simple_scalar(item) => {
                write_value(out, item, indent, false);
                out.push('\n');
            }

            // Complex non-map values (arrays of arrays, etc.)
            other => {
                out.push('\n');
                write_value(out, other, indent + 2, false);
                out.push('\n');
            }
        }
    }
}

// ----------------------------------------------------
// INLINE DECISION LOGIC
// ----------------------------------------------------

/// If map is small and simple → inline `{ key: value }`
/// Else → block format
fn write_inline_or_block_map(
    out: &mut String,
    map: &BTreeMap<String, YallValue>,
    indent: usize,
) {
    if try_inline_map(map) {
        write_inline_map(out, map);
    } else {
        write_block_map(out, map, indent);
    }
}

fn try_inline_map(map: &BTreeMap<String, YallValue>) -> bool {
    if map.is_empty() {
        return true;
    }
    if map.len() > 3 {
        return false;
    }

    for (_k, v) in map {
        if !is_simple_scalar(v) {
            return false;
        }
        if let YallValue::Str(s) = v {
            if s.len() > 20 {
                return false;
            }
        }
    }
    true
}

fn write_inline_map(out: &mut String, map: &BTreeMap<String, YallValue>) {
    out.push('{');
    let mut first = true;
    for (k, v) in map {
        if !first {
            out.push_str(", ");
        }
        first = false;
        out.push_str(k);
        out.push_str(": ");
        write_value(out, v, 0, false);
    }
    out.push('}');
}

// ----------------------------------------------------
// INLINE ARRAY
// ----------------------------------------------------

fn write_inline_or_block_array(
    out: &mut String,
    arr: &[YallValue],
    indent: usize,
) {
    if try_inline_array(arr) {
        write_inline_array(out, arr);
    } else {
        write_block_list(out, arr, indent);
    }
}

fn try_inline_array(arr: &[YallValue]) -> bool {
    if arr.is_empty() {
        return true;
    }
    if arr.len() > 5 {
        return false;
    }
    for v in arr {
        if !is_simple_scalar(v) {
            return false;
        }
        if let YallValue::Str(s) = v {
            if s.len() > 20 {
                return false;
            }
        }
    }
    true
}

fn write_inline_array(out: &mut String, arr: &[YallValue]) {
    out.push('[');
    let mut first = true;
    for v in arr {
        if !first {
            out.push_str(", ");
        }
        first = false;
        write_value(out, v, 0, false);
    }
    out.push(']');
}

// ----------------------------------------------------

fn write_indent(out: &mut String, indent: usize) {
    for _ in 0..indent {
        out.push(' ');
    }
}

/// Minimal inline writer for compact output.
/// Maps become `{k:v,k2:v2}`
/// Arrays become `[a,b,c]`
pub fn minify(val: &YallValue) -> String {
    match val {
        YallValue::Map(m) => {
            let mut out = String::from("{");
            let mut first = true;
            for (k, v) in m {
                if !first { out.push(','); }
                first = false;
                out.push_str(k);
                out.push(':');
                out.push_str(&minify(v));
            }
            out.push('}');
            out
        }
        YallValue::Array(arr) => {
            let mut out = String::from("[");
            let mut first = true;
            for v in arr {
                if !first { out.push(','); }
                first = false;
                out.push_str(&minify(v));
            }
            out.push(']');
            out
        }
        YallValue::Str(s) => {
            // Always quote in minify
            let mut o = String::new();
            write_string(&mut o, s);
            o
        }
        YallValue::Int(i) => i.to_string(),
        YallValue::Float(f) => f.to_string(),
        YallValue::Bool(b) => if *b { "true".into() } else { "false".into() },
        YallValue::Null => "null".into(),
    }
}

