//! Render mode: `<{ render }>` template pages.
//!
//! A render page mixes literal output text with real Goblin code delimited by
//! `<{ ... }>`. This module detects render-mode source (content-based, not by
//! filename) and transpiles it into ordinary Goblin source text that builds up
//! an output string, which is then compiled and executed like any other
//! Goblin module.

use crate::compiler::compile_repl_snippet_for_file;
use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

const OUT_VAR: &str = "__render_out";

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

/// Transpile a render-page body (text after the `<{ render }>` directive) into
/// literal Goblin source that builds `__render_out` and returns it.
///
/// Model: everything OUTSIDE `<{ }>` is raw HTML appended verbatim to output.
/// Everything INSIDE `<{ }>` is plain Goblin code, spliced verbatim.
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
            out.push_str(code);
            out.push('\n');
        }
        rest = &after_open[close + 2..];
    }

    out.push_str(OUT_VAR);
    out.push('\n');
    out
}

fn emit_literal(out: &mut String, text: &str) {
    // Triple-quoted strings are verbatim in Goblin — no escape processing, no
    // interpolation. Split on """ (closing delimiter) to handle that edge case.
    for (i, chunk) in text.split("\"\"\"").enumerate() {
        if i > 0 {
            out.push_str(OUT_VAR);
            out.push_str(" | ");
            out.push_str(OUT_VAR);
            out.push_str(" + \"\\\"\\\"\\\"\"\n");
        }
        if !chunk.is_empty() {
            out.push_str(OUT_VAR);
            out.push_str(" | ");
            out.push_str(OUT_VAR);
            out.push_str(" + \"\"\"");
            out.push_str(chunk);
            out.push_str("\"\"\"\n");
        }
    }
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

    let compiled = compile_repl_snippet_for_file(&module, &known_globals, path).map_err(|e| {
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
