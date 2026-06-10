//! High-level entry point: parse source → compile → execute.
use crate::compiler::Compiler;
use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::Value;
use crate::vm::Vm;

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
    let mut compiler = Compiler::new();
    let func = compiler.compile_module(&module)?;

    // Execute
    let session = Session::new(GcMode::Auto);
    let mut vm = Vm::new(session);
    vm.execute(func)
}
