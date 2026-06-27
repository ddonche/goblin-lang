//! Render mode: `<{ render }>` template pages.
//!
//! The entire template compiles as one Goblin program so loops can span blocks.
//! HTML outside `<{ }>` becomes verbatim triple-quoted string appends.
//! Code inside `<{ }>` runs as Goblin: bare expressions auto-output their
//! value; statements (binds, loops, declarations) run silently.

use crate::compiler::compile_repl_snippet;
use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

const OUT: &str = "__render_out";

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

fn render_body(source: &str) -> &str {
    let trimmed = source.trim_start();
    let close = trimmed.find("}>").expect("is_render_source already matched");
    &trimmed[close + 2..]
}

/// Emit a raw HTML chunk as a triple-quoted Goblin string append.
/// Triple-quoted strings are verbatim — no escape processing, no interpolation.
/// Split on `"""` (the only sequence that would close the literal) to be safe.
fn emit_html(out: &mut String, html: &str) {
    for (i, chunk) in html.split("\"\"\"").enumerate() {
        if i > 0 {
            // re-emit the literal """ via a regular escaped string
            out.push_str(OUT);
            out.push_str(" |= ");
            out.push_str(OUT);
            out.push_str(" + \"\\\"\\\"\\\"\"\n");
        }
        if !chunk.is_empty() {
            out.push_str(OUT);
            out.push_str(" |= ");
            out.push_str(OUT);
            out.push_str(" + \"\"\"");
            out.push_str(chunk);
            out.push_str("\"\"\"\n");
        }
    }
}

/// Return true if `code` is a single bare expression statement (not a bind,
/// loop, declaration, etc.). Uses the actual Goblin parser — no keyword lists.
fn is_single_expression(code: &str) -> bool {
    let tokens = match goblin_lexer::lex(code, "<render>") {
        Ok(t) => t,
        Err(_) => return false,
    };
    let parser = goblin_parser::Parser::new(&tokens);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(_) => return false,
    };
    matches!(module.items.as_slice(), [goblin_ast::Stmt::Expr(_)])
}

/// Transpile the render body into a single Goblin program.
fn transpile(body: &str) -> String {
    let mut out = String::new();
    out.push_str(OUT);
    out.push_str(" | \"\"\n");

    let mut rest = body;
    loop {
        let Some(open) = rest.find("<{") else {
            if !rest.is_empty() {
                emit_html(&mut out, rest);
            }
            break;
        };
        if open > 0 {
            emit_html(&mut out, &rest[..open]);
        }
        let after_open = &rest[open + 2..];
        let Some(close) = after_open.find("}>") else {
            if !after_open.is_empty() {
                emit_html(&mut out, after_open);
            }
            break;
        };
        let code = after_open[..close].trim();
        if !code.is_empty() {
            if is_single_expression(code) {
                // Bare expression — output its value, same as REPL behaviour.
                out.push_str(OUT);
                out.push_str(" |= ");
                out.push_str(OUT);
                out.push_str(" + :str(");
                out.push_str(code);
                out.push_str(")\n");
            } else {
                // Statement, declaration, loop, etc. — run verbatim.
                out.push_str(code);
                out.push('\n');
            }
        }
        rest = &after_open[close + 2..];
    }

    out.push_str(OUT);
    out.push('\n');
    out
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
    // known_globals = only the data variables — OUT is declared by the transpiled `__render_out | ""`
    // and must NOT be pre-declared here or the compiler will see a duplicate-local.
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
