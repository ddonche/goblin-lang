//! High-level entry point: parse source → compile → execute.
use crate::compiler::Compiler;
use crate::error::GoblinError;
use crate::session::{GcMode, ResponseState, Session};
use crate::value::Value;
use crate::vm::Vm;

/// Resolve `{#ns::key}` and `{#ns::key@source}` templates in a string using the box_store.
/// Matches interpreter's resolve_box_template behavior: if key found with Str value, substitute;
/// otherwise keep the literal `{#...}` text.
pub(crate) fn resolve_box_template_vm(s: &str, box_store: &std::collections::HashMap<String, Value>) -> String {
    let mut out = String::new();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '{' && i + 1 < chars.len() && chars[i + 1] == '#' {
            let start = i + 1;
            let mut j = start;
            while j < chars.len() && chars[j] != '}' { j += 1; }
            if j < chars.len() {
                let inner: String = chars[start..j].iter().collect();
                let inner = inner.trim().trim_start_matches('#');
                let key = if let Some(at) = inner.find('@') { &inner[..at] } else { inner };
                if key.contains("::") {
                    if let Some(Value::Str(v)) = box_store.get(key) {
                        out.push_str(v);
                        i = j + 1;
                        continue;
                    }
                }
            }
        }
        out.push(chars[i]);
        i += 1;
    }
    out
}

/// Read a box.toml file and populate session.box_store, matching interpreter's load_box_toml.
/// Resolves {#ns::key} templates and drops self-referential entries.
pub fn load_box_toml_into_session(session: &mut Session, path: &std::path::Path) -> Result<(), GoblinError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| GoblinError::Runtime(format!("cannot read {}: {}", path.display(), e)))?;
    let table: toml::Table = content.parse()
        .map_err(|e| GoblinError::Runtime(format!("invalid TOML in {}: {}", path.display(), e)))?;

    for (namespace, section) in &table {
        if let toml::Value::Table(fields) = section {
            for (key, val) in fields {
                let v = match val {
                    toml::Value::String(s)  => Value::Str(s.clone()),
                    toml::Value::Integer(i) => Value::Int(*i),
                    toml::Value::Float(f)   => Value::Float(*f),
                    toml::Value::Boolean(b) => Value::Bool(*b),
                    other                   => Value::Str(other.to_string()),
                };
                session.box_store.insert(format!("{}::{}", namespace, key), v);
            }
        }
    }

    // Resolve {#ns::key} templates — fixed-point iteration so chained refs resolve
    for _ in 0..10 {
        let mut changed = false;
        let keys: Vec<String> = session.box_store.keys().cloned().collect();
        for k in keys {
            if let Some(Value::Str(s)) = session.box_store.get(&k).cloned() {
                if s.contains("{#") {
                    let resolved = resolve_box_template_vm(&s, &session.box_store);
                    if resolved != s {
                        session.box_store.insert(k, Value::Str(resolved));
                        changed = true;
                    }
                }
            }
        }
        if !changed { break; }
    }

    // Drop self-referential entries (can never resolve)
    session.box_store.retain(|k, v| {
        if let Value::Str(s) = v {
            let self_ref = format!("{{#{}}}", k);
            let self_ref_ann = format!("{{#{}@", k);
            !(s.contains(&self_ref) || s.contains(&self_ref_ann))
        } else {
            true
        }
    });

    Ok(())
}

pub fn compile_class_methods_pub(class: &goblin_ast::ClassDecl, session: &mut Session) {
    compile_class_methods(class, session);
}

fn compile_class_methods(class: &goblin_ast::ClassDecl, session: &mut Session) {
    use crate::compiler::Compiler;
    for action in &class.actions {
        // Build a pseudo-ActionDecl with 'self' prepended as the first parameter
        let self_param = goblin_ast::Param {
            name: "self".to_string(),
            type_name: None,
            default: None,
            span: action.span.clone(),
        };
        let mut params = vec![self_param];
        params.extend(action.params.clone());
        let pseudo = goblin_ast::ActionDecl {
            name: action.name.clone(),
            params,
            body: action.body.clone(),
            span: action.span.clone(),
            ret: action.ret.clone(),
        };
        let mut compiler = Compiler::new();
        compiler.is_class_method = true;
        match compiler.compile_action(&pseudo) {
            Ok(func) => {
                session.compiled_methods.insert(
                    (class.name.clone(), action.name.clone()),
                    std::rc::Rc::new(func),
                );
            }
            Err(_) => {}
        }
    }
}

pub fn execute_source(source: &str) -> Result<Value, GoblinError> {
    // Lex
    let tokens = goblin_lexer::lex(source, "<source>")
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<lex>".into(),
        })?;

    // Parse
    let parser = goblin_parser::Parser::new(&tokens);
    let module = parser.parse_module()
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<parse>".into(),
        })?;

    // Compile
    let compiled = Compiler::new().compile_module(&module)?;
    // (no args injection needed for execute_source — use execute_source_with_args for CLI runs)

    // Execute
    let mut session = Session::new(GcMode::Auto);
    session.global_names = compiled.global_names;
    for decl in &compiled.classes {
        compile_class_methods(decl, &mut session);
    }
    for decl in compiled.classes {
        // Merge logic: if incoming decl has no actions/decision/judge/transitions
        // (i.e., it's a matrix-synthesized class), preserve those from the existing class
        // and also preserve fields that the matrix didn't redefine — matching interpreter behavior.
        let merged = if decl.actions.is_empty() && decl.decision.is_none() && decl.judge.is_none() && decl.transitions.is_empty() {
            if let Some(existing) = session.classes.get(&decl.name) {
                let mut merged = decl.clone();
                merged.actions = existing.actions.clone();
                merged.decision = existing.decision.clone();
                merged.judge = existing.judge.clone();
                merged.transitions = existing.transitions.clone();
                if merged.capacity.is_none() { merged.capacity = existing.capacity.clone(); }
                let matrix_field_names: std::collections::HashSet<String> =
                    merged.fields.iter().map(|f| f.name.clone()).collect();
                let extra_fields: Vec<_> = existing.fields.iter()
                    .filter(|f| !matrix_field_names.contains(&f.name))
                    .cloned()
                    .collect();
                merged.fields.extend(extra_fields);
                merged
            } else { decl }
        } else { decl };
        session.classes.insert(merged.name.clone(), merged);
    }
    for decl in compiled.enums   { session.enums.insert(decl.name.clone(), decl); }
    let mut vm = Vm::new(session);
    vm.execute(compiled.entry)
}

/// Like `execute_source` but injects CLI arguments as a global `args` array before running.
/// Matches interpreter behavior: `goblin run main.gbln foo bar` → `args = ["foo", "bar"]`.
pub fn execute_source_with_args(source: &str, extra_args: Vec<String>) -> Result<Value, GoblinError> {
    let tokens = goblin_lexer::lex(source, "<source>")
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<lex>".into(),
        })?;

    let module = goblin_parser::Parser::new(&tokens).parse_module()
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<parse>".into(),
        })?;

    // Pre-declare "args" so scripts that use it without `let args = ...` can reference it
    // as a global (resolve_load checks self.globals; if absent it returns UndefinedVariable).
    let compiled = Compiler::new().with_globals(&["args"]).compile_module(&module)?;

    let mut session = Session::new(GcMode::Auto);
    session.global_names = compiled.global_names;
    for decl in &compiled.classes { compile_class_methods(decl, &mut session); }
    for decl in compiled.classes {
        let merged = if decl.actions.is_empty() && decl.decision.is_none() && decl.judge.is_none() && decl.transitions.is_empty() {
            if let Some(existing) = session.classes.get(&decl.name) {
                let mut merged = decl.clone();
                merged.actions = existing.actions.clone();
                merged.decision = existing.decision.clone();
                merged.judge = existing.judge.clone();
                merged.transitions = existing.transitions.clone();
                if merged.capacity.is_none() { merged.capacity = existing.capacity.clone(); }
                let matrix_field_names: std::collections::HashSet<String> =
                    merged.fields.iter().map(|f| f.name.clone()).collect();
                let extra_fields: Vec<_> = existing.fields.iter()
                    .filter(|f| !matrix_field_names.contains(&f.name))
                    .cloned()
                    .collect();
                merged.fields.extend(extra_fields);
                merged
            } else { decl }
        } else { decl };
        session.classes.insert(merged.name.clone(), merged);
    }
    for decl in compiled.enums { session.enums.insert(decl.name.clone(), decl); }

    // Inject CLI args as global `args` before execution (matches interpreter behavior).
    if let Some(idx) = session.global_names.iter().position(|n| n == "args") {
        let args_val = Value::Array(extra_args.into_iter().map(Value::Str).collect());
        let tether = session.alloc_value(args_val);
        session.set_global(idx, tether);
    }

    let mut vm = Vm::new(session);
    vm.execute(compiled.entry)
}

/// Like `execute_source` but captures print/say output and returns it with the response state.
/// Used by goblin-host to run scripts in-process with the VM engine.
/// `source_file` is used in error messages; pass "" when the path is not known.
pub fn execute_source_api(source: &str, source_file: &str) -> Result<(String, ResponseState), GoblinError> {
    let tokens = goblin_lexer::lex(source, if source_file.is_empty() { "<source>" } else { source_file })
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<lex>".into(),
        })?;

    let parser = goblin_parser::Parser::new(&tokens);
    let module = parser.parse_module()
        .map_err(|diags| GoblinError::CompileError {
            message: diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"),
            span_debug: "<parse>".into(),
        })?;

    let compiled = Compiler::new().for_file(source_file).compile_module(&module)?;

    let mut session = Session::new(GcMode::Auto);
    session.global_names = compiled.global_names;
    session.enable_output_capture();
    for decl in &compiled.classes {
        compile_class_methods(decl, &mut session);
    }
    for decl in compiled.classes {
        let merged = if decl.actions.is_empty() && decl.decision.is_none() && decl.judge.is_none() && decl.transitions.is_empty() {
            if let Some(existing) = session.classes.get(&decl.name) {
                let mut merged = decl.clone();
                merged.actions = existing.actions.clone();
                merged.decision = existing.decision.clone();
                merged.judge = existing.judge.clone();
                merged.transitions = existing.transitions.clone();
                if merged.capacity.is_none() { merged.capacity = existing.capacity.clone(); }
                let matrix_field_names: std::collections::HashSet<String> =
                    merged.fields.iter().map(|f| f.name.clone()).collect();
                let extra_fields: Vec<_> = existing.fields.iter()
                    .filter(|f| !matrix_field_names.contains(&f.name))
                    .cloned()
                    .collect();
                merged.fields.extend(extra_fields);
                merged
            } else { decl }
        } else { decl };
        session.classes.insert(merged.name.clone(), merged);
    }
    for decl in compiled.enums { session.enums.insert(decl.name.clone(), decl); }
    let mut vm = Vm::new(session);
    vm.execute(compiled.entry)?;
    let output = vm.session.take_output();
    let response = vm.session.response.clone();
    Ok((output, response))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(src: &str) -> Result<Value, GoblinError> {
        execute_source(src)
    }

    #[test]
    fn hello_world() {
        run("x | 42").unwrap();
    }

    #[test]
    fn if_simple() {
        let src = std::fs::read_to_string("../../tests/if.gbln").unwrap_or_else(|_|
            "x | 10\nif x > 5 => say \"big\"\nsay \"done\"".into()
        );
        run(&src).unwrap();
    }
}

    #[test]
    fn debug_if_ast() {
        let src = "x | 10\nif x > 5 => say \"big\"\nsay \"done\"";
        let toks = goblin_lexer::lex(src, "<test>").unwrap();
        let parser = goblin_parser::Parser::new(&toks);
        let module = parser.parse_module().unwrap();
        for stmt in &module.items {
            eprintln!("{:#?}", stmt);
        }
    }

    #[test]
    fn password_gbln() {
        let src = std::fs::read_to_string("../../api/password.gbln").unwrap();
        match execute_source(&src) {
            Ok(v) => eprintln!("password OK: {:?}", v),
            Err(e) => eprintln!("password ERR: {:#?}", e),
        }
    }

    #[test]
    fn simple_secure_pick() {
        let src = r#"act test
    x | secure_pick 1 from 3..8
    say x
xx
test()"#;
        match execute_source(src) {
            Ok(v) => eprintln!("secure_pick OK: {:?}", v),
            Err(e) => eprintln!("secure_pick ERR: {:#?}", e),
        }
    }

    #[test]
    fn tuple_bind_test() {
        let src = r#"act test
    n, l, s | [ secure_pick 1 from 3..8, secure_pick 1 from 5..12, secure_pick 1 from 2..5 ]
    say n
    say l
    say s
xx
test()"#;
        match execute_source(src) {
            Ok(v) => eprintln!("tuple OK: {:?}", v),
            Err(e) => eprintln!("tuple ERR: {:#?}", e),
        }
    }

    #[test]
    fn password_step_by_step() {
        let tests = vec![
            ("step1", r#"act test
    symbols | raw "!@#$%^&*()-_=+[];:<>?"
    say symbols
xx
test()"#),
            ("step2", r#"act test
    n, l, s | [ secure_pick 1 from 3..8, secure_pick 1 from 5..12, secure_pick 1 from 2..5 ]
    say n
    say l
    say s
xx
test()"#),
            ("step3", r#"act test
    n, l, s | [ secure_pick 1 from 3..8, secure_pick 1 from 5..12, secure_pick 1 from 2..5 ]
    nums_arr | secure_pick {n} from 0...9 with dups
    say nums_arr
xx
test()"#),
        ];
        for (name, src) in tests {
            match execute_source(src) {
                Ok(_) => eprintln!("{} OK", name),
                Err(e) => eprintln!("{} ERR: {:?}", name, e),
            }
        }
    }

    #[test]
    fn password_step4() {
        let src = r#"act test
    symbols | raw "!@#$%^&*()-_=+[];:<>?"
    n, l, s | [ secure_pick 1 from 3..8, secure_pick 1 from 5..12, secure_pick 1 from 2..5 ]
    nums_arr    | secure_pick {n} from 0...9 with dups
    letters_arr | secure_pick {l} from "a"..."z" with dups
    sym_arr     | secure_pick {s} from symbols with dups
    say nums_arr
    say letters_arr
    say sym_arr
xx
test()"#;
        match execute_source(src) {
            Ok(_) => eprintln!("step4 OK"),
            Err(e) => eprintln!("step4 ERR: {:?}", e),
        }
    }

    #[test]
    fn password_step5() {
        let src = r#"act test
    symbols | raw "!@#$%^&*()-_=+[];:<>?"
    n, l, s | [ secure_pick 1 from 3..8, secure_pick 1 from 5..12, secure_pick 1 from 2..5 ]
    nums_arr    | secure_pick {n} from 0...9 with dups
    letters_arr | secure_pick {l} from "a"..."z" with dups
    sym_arr     | secure_pick {s} from symbols with dups
    numbers  | :pack(nums_arr).str
    say numbers
xx
test()"#;
        match execute_source(src) {
            Ok(_) => eprintln!("step5 OK"),
            Err(e) => eprintln!("step5 ERR: {:?}", e),
        }
    }

    #[test]
    fn char_range_pick() {
        let src = r#"act test
    letters_arr | secure_pick 3 from "a"..."z" with dups
    say letters_arr
xx
test()"#;
        match execute_source(src) {
            Ok(v) => eprintln!("char_range OK: {:?}", v),
            Err(e) => eprintln!("char_range ERR: {:?}", e),
        }
    }
