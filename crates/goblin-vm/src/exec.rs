//! High-level entry point: parse source → compile → execute.
use crate::compiler::Compiler;
use crate::error::GoblinError;
use crate::session::{GcMode, ResponseState, Session};
use crate::value::Value;
use crate::vm::Vm;

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

/// Like `execute_source_api` but pre-seeds a `fields` global from a JSON string.
/// Used by goblin-host when running a Goblin Form action file.
pub fn execute_source_api_with_fields(source: &str, fields_json: &str) -> Result<(String, ResponseState), GoblinError> {
    use crate::compiler::compile_repl_snippet;

    let fields_value = if fields_json.is_empty() {
        Value::Map(Default::default())
    } else {
        let jv: serde_json::Value = serde_json::from_str(fields_json)
            .map_err(|e| GoblinError::Runtime(format!("form fields parse error: {e}")))?;
        crate::builtins::json_to_value(&jv)
    };

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

    let known = vec!["fields".to_string()];
    let compiled = compile_repl_snippet(&module, &known)?;

    let mut session = Session::new(GcMode::Auto);
    session.global_names = compiled.global_names;
    session.enable_output_capture();
    for decl in compiled.classes { session.classes.insert(decl.name.clone(), decl); }
    for decl in compiled.enums   { session.enums.insert(decl.name.clone(), decl); }

    let t = session.alloc_value(fields_value);
    session.set_global(0, t);

    let mut vm = Vm::new(session);
    vm.execute_repl(compiled.entry, 1)?;
    let output = vm.session.take_output();
    let response = vm.session.response.clone();
    Ok((output, response))
}

/// Like `execute_source` but captures print/say output and returns it with the response state.
/// Used by goblin-host to run scripts in-process with the VM engine.
pub fn execute_source_api(source: &str) -> Result<(String, ResponseState), GoblinError> {
    let tokens = goblin_lexer::lex(source, "<source>")
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

    let compiled = Compiler::new().compile_module(&module)?;

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
