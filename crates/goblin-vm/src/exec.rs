//! High-level entry point: parse source → compile → execute.
use crate::compiler::Compiler;
use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

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
    for decl in compiled.classes { session.classes.insert(decl.name.clone(), decl); }
    for decl in compiled.enums   { session.enums.insert(decl.name.clone(), decl); }
    let mut vm = Vm::new(session);
    vm.execute(compiled.entry)
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
