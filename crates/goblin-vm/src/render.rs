//! Render mode: `<{ render }>` template pages.
//!
//! A render page mixes literal output text with real Goblin code delimited by
//! `<{ ... }>`. This module detects render-mode source (content-based, not by
//! filename) and transpiles it into ordinary Goblin source text that builds up
//! an output string, which is then compiled and executed like any other
//! Goblin module.

use crate::compiler::compile_repl_snippet;
use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

const OUT_VAR: &str = "__render_out";

const CONTROL_KEYWORDS: &[&str] = &[
    "for", "if", "unless", "while", "repeat", "else", "elif", "judge", "xx", "end",
];

/// True if `source`'s first meaningful content is the `<{ render }>` directive.
pub fn is_render_source(source: &str) -> bool {
    // Strip UTF-8 BOM if present (Windows editors sometimes add it)
    let source = source.trim_start_matches('\u{FEFF}');
    let rest = match source.trim_start().strip_prefix("<{") {
        Some(r) => r.trim_start(),
        None => return false,
    };
    let rest = match rest.strip_prefix("render") {
        Some(r) => r.trim_start(),
        None => return false,
    };
    rest.starts_with("}>")
}

/// Everything after the leading `<{ render }>` directive. Caller must have
/// already confirmed `is_render_source(source)`.
fn render_body(source: &str) -> &str {
    let trimmed = source.trim_start();
    let close = trimmed.find("}>").expect("is_render_source already matched the directive");
    &trimmed[close + 2..]
}

fn escape_goblin_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

/// A `<{ ... }>` block is control flow (spliced verbatim) if it opens/closes a
/// block (`for`, `if`, `xx`, ...) or is itself a bind statement (`name | value`).
/// Otherwise it's an expression whose stringified value gets appended to output.
fn is_control_chunk(code: &str) -> bool {
    let first_word = code.split_whitespace().next().unwrap_or("");
    if CONTROL_KEYWORDS.contains(&first_word) {
        return true;
    }
    if let Some(pipe_pos) = code.find('|') {
        let is_double_pipe = code.as_bytes().get(pipe_pos + 1) == Some(&b'|')
            || (pipe_pos > 0 && code.as_bytes()[pipe_pos - 1] == b'|');
        if !is_double_pipe {
            let head = code[..pipe_pos].trim();
            let looks_like_names = !head.is_empty()
                && head.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_')
                && head.chars().all(|c| c.is_alphanumeric() || c == '_' || c == ',' || c.is_whitespace());
            if looks_like_names {
                return true;
            }
        }
    }
    false
}

/// Transpile a render-page body (text after the `<{ render }>` directive) into
/// literal Goblin source that builds `__render_out` and returns it.
fn transpile(body: &str) -> String {
    let mut out = String::new();
    out.push_str(OUT_VAR);
    out.push_str(" | \"\"\n");

    let mut rest = body;
    loop {
        let Some(open) = rest.find("<{") else {
            if !rest.is_empty() {
                emit_literal(&mut out, rest);
            }
            break;
        };
        let literal = &rest[..open];
        if !literal.is_empty() {
            emit_literal(&mut out, literal);
        }
        let after_open = &rest[open + 2..];
        let Some(close) = after_open.find("}>") else {
            // Unterminated block: treat the remainder as literal text.
            emit_literal(&mut out, after_open);
            break;
        };
        let code = after_open[..close].trim();
        if !code.is_empty() {
            if is_control_chunk(code) {
                out.push_str(code);
                out.push('\n');
            } else {
                out.push_str(OUT_VAR);
                out.push_str(" | ");
                out.push_str(OUT_VAR);
                out.push_str(" + str(");
                out.push_str(code);
                out.push_str(")\n");
            }
        }
        rest = &after_open[close + 2..];
    }

    out.push_str(OUT_VAR);
    out.push('\n');
    out
}

fn emit_literal(out: &mut String, text: &str) {
    out.push_str(OUT_VAR);
    out.push_str(" | ");
    out.push_str(OUT_VAR);
    out.push_str(" + \"");
    out.push_str(&escape_goblin_string(text));
    out.push_str("\"\n");
}

fn map_to_pairs(data: Value) -> Result<Vec<(String, Value)>, GoblinError> {
    match data {
        Value::Map(m) => Ok(m.into_iter().collect()),
        Value::MapOrd(m) => Ok(m.into_iter().collect()),
        Value::Nil => Ok(vec![]),
        other => Err(GoblinError::Runtime(format!(
            "render_template: data must be a map, got {}",
            other.type_name()
        ))),
    }
}

/// Read `path`, confirm it's a render-mode page, transpile it, bind `data`'s
/// keys as globals, run it in an isolated session, and return the output string.
pub fn render_template(path: &str, data: Value) -> Result<Value, GoblinError> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| GoblinError::Runtime(format!("render_template: {e}")))?;

    if !is_render_source(&source) {
        return Err(GoblinError::Runtime(format!(
            "render_template: '{path}' is not a render-mode file (must start with <{{ render }}>)"
        )));
    }

    let transpiled = transpile(render_body(&source));

    let tokens = goblin_lexer::lex(&transpiled, path).map_err(|diags| {
        GoblinError::Runtime(format!(
            "render_template: lex error in '{path}': {}",
            diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")
        ))
    })?;
    let parser = goblin_parser::Parser::new(&tokens);
    let module = parser.parse_module().map_err(|diags| {
        GoblinError::Runtime(format!(
            "render_template: parse error in '{path}': {}",
            diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")
        ))
    })?;

    let data_pairs = map_to_pairs(data)?;
    let mut known_globals: Vec<String> = data_pairs.iter().map(|(k, _)| k.clone()).collect();

    let compiled = compile_repl_snippet(&module, &known_globals).map_err(|e| {
        GoblinError::Runtime(format!("render_template: compile error in '{path}': {e}"))
    })?;

    for name in &compiled.global_names {
        if !known_globals.contains(name) {
            known_globals.push(name.clone());
        }
    }

    let mut session = Session::new(GcMode::Auto);
    session.global_names = known_globals.clone();
    for decl in compiled.classes {
        session.classes.insert(decl.name.clone(), decl);
    }
    for decl in compiled.enums {
        session.enums.insert(decl.name.clone(), decl);
    }
    for (i, (_, value)) in data_pairs.into_iter().enumerate() {
        let t = session.alloc_value(value);
        session.set_global(i, t);
    }

    let mut vm = Vm::new(session);
    let result = vm
        .execute_repl(compiled.entry, known_globals.len())
        .map_err(|e| GoblinError::Runtime(format!("render_template: runtime error in '{path}': {e}")))?;

    match result {
        Value::Str(s) => Ok(Value::Str(s)),
        other => Ok(Value::Str(crate::builtins::fmt_value_raw(&other))),
    }
}
