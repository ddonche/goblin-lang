use wasm_bindgen::prelude::*;
use goblin_vm::{
    session::{GcMode, Session},
    compiler::Compiler,
    vm::Vm,
    exec::compile_class_methods_pub,
};

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct GoblinRepl {
    vm: Vm,
    n_globals: usize,
}

#[wasm_bindgen]
impl GoblinRepl {
    #[wasm_bindgen(constructor)]
    pub fn new() -> GoblinRepl {
        let mut session = Session::new(GcMode::Auto);
        session.enable_output_capture();
        GoblinRepl {
            vm: Vm::new(session),
            n_globals: 0,
        }
    }

    pub fn run(&mut self, code: &str) -> String {
        if self.vm.session.output_buf.is_none() {
            self.vm.session.enable_output_capture();
        }

        let tokens = match goblin_lexer::lex(code, "<repl>") {
            Ok(t) => t,
            Err(diags) => {
                return diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n");
            }
        };

        let module = match goblin_parser::Parser::new(&tokens).parse_module() {
            Ok(m) => m,
            Err(diags) => {
                return diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n");
            }
        };

        let compiled = match Compiler::new().compile_module(&module) {
            Ok(c) => c,
            Err(e) => return format!("Compile error: {e}"),
        };

        for decl in &compiled.classes {
            compile_class_methods_pub(decl, &mut self.vm.session);
        }
        for decl in compiled.classes {
            self.vm.session.classes.insert(decl.name.clone(), decl);
        }
        for decl in compiled.enums {
            self.vm.session.enums.insert(decl.name.clone(), decl);
        }

        self.vm.session.global_names = compiled.global_names;

        let n = self.n_globals;
        match self.vm.execute_repl(compiled.entry, n) {
            Ok(_) => {
                self.n_globals = self.vm.session.globals.len();
            }
            Err(e) => {
                let _ = self.vm.session.take_output();
                self.vm.session.enable_output_capture();
                return format!("Runtime error: {e}");
            }
        }

        let out = self.vm.session.take_output();
        self.vm.session.enable_output_capture();
        out
    }

    pub fn reset(&mut self) {
        let mut session = Session::new(GcMode::Auto);
        session.enable_output_capture();
        self.vm = Vm::new(session);
        self.n_globals = 0;
    }
}
