//! Render mode: `<{ render }>` template pages.
//!
//! Model: everything inside `<{ }>` is plain Goblin code. Everything outside
//! is raw HTML written directly to the output — no Goblin processing at all.

use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

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

/// Everything after the leading `<{ render }>` directive.
fn render_body(source: &str) -> &str {
    let trimmed = source.trim_start();
    let close = trimmed.find("}>").expect("is_render_source already matched");
    &trimmed[close + 2..]
}

enum Segment<'a> {
    Html(&'a str),
    Code(&'a str),
}

fn segments(body: &str) -> Vec<Segment<'_>> {
    let mut out = Vec::new();
    let mut rest = body;
    loop {
        let Some(open) = rest.find("<{") else {
            if !rest.is_empty() { out.push(Segment::Html(rest)); }
            break;
        };
        if open > 0 { out.push(Segment::Html(&rest[..open])); }
        let after_open = &rest[open + 2..];
        let Some(close) = after_open.find("}>") else {
            if !after_open.is_empty() { out.push(Segment::Html(after_open)); }
            break;
        };
        let code = after_open[..close].trim();
        if !code.is_empty() { out.push(Segment::Code(code)); }
        rest = &after_open[close + 2..];
    }
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

/// Read `path`, confirm it is a render-mode page, and execute it.
/// HTML outside `<{ }>` is written directly to the output string.
/// Goblin code inside `<{ }>` runs in a shared session across all blocks.
pub fn render_template(path: &str, data: Value) -> Result<Value, GoblinError> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| GoblinError::Runtime(format!("render_template: {e}")))?;

    if !is_render_source(&source) {
        return Err(GoblinError::Runtime(format!(
            "render_template: '{path}' is not a render-mode file (must start with <{{ render }}>)"
        )));
    }

    let data_pairs = map_to_pairs(data)?;
    let mut known_globals: Vec<String> = data_pairs.iter().map(|(k, _)| k.clone()).collect();

    let mut session = Session::new(GcMode::Auto);
    session.global_names = known_globals.clone();
    for (i, (_, value)) in data_pairs.into_iter().enumerate() {
        let t = session.alloc_value(value);
        session.set_global(i, t);
    }

    let mut vm = Vm::new(session);
    let mut output = String::new();

    for seg in segments(render_body(&source)) {
        match seg {
            Segment::Html(text) => output.push_str(text),
            Segment::Code(code) => {
                let tokens = goblin_lexer::lex(code, path).map_err(|diags| {
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
                let compiled = compile_repl_snippet(&module, &known_globals).map_err(|e| {
                    GoblinError::Runtime(format!("render_template: compile error in '{path}': {e}"))
                })?;
                for name in &compiled.global_names {
                    if !known_globals.contains(name) {
                        known_globals.push(name.clone());
                        vm.session.global_names.push(name.clone());
                    }
                }
                for decl in compiled.classes {
                    vm.session.classes.insert(decl.name.clone(), decl);
                }
                for decl in compiled.enums {
                    vm.session.enums.insert(decl.name.clone(), decl);
                }
                vm.execute_repl(compiled.entry, known_globals.len()).map_err(|e| {
                    GoblinError::Runtime(format!("render_template: runtime error in '{path}': {e}"))
                })?;
            }
        }
    }

    Ok(Value::Str(output))
}
