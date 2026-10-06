//! Compiled-module cache for imports.
//!
//! A hosted app imports the same modules on every request, and lexing,
//! parsing, compiling and quickening them was most of a request's cost. The
//! cache keeps each compiled module per thread (compiled code holds `Rc`s)
//! and reuses it while the file is unchanged (same modification time and
//! length) and the importing session's globals are the same as when it was
//! compiled, since global slot numbers depend on them. Anything else
//! recompiles, so a cached module behaves exactly like a fresh one.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use std::time::SystemTime;

use crate::error::GoblinError;
use crate::value::FunctionObject;

/// An imported module, ready to run.
pub struct ImportedModule {
    pub entry: Rc<FunctionObject>,
    pub classes: Vec<goblin_ast::ClassDecl>,
    pub enums: Vec<goblin_ast::EnumDecl>,
    pub global_names: Rc<Vec<String>>,
}

struct Cached {
    stamp: (SystemTime, u64),
    globals_before: Vec<String>,
    entry: Rc<FunctionObject>,
    classes: Vec<goblin_ast::ClassDecl>,
    enums: Vec<goblin_ast::EnumDecl>,
    global_names: Rc<Vec<String>>,
}

thread_local! {
    static CACHE: RefCell<HashMap<(String, String, Option<String>), Cached>> = RefCell::new(HashMap::new());
}

fn stamp(path: &Path) -> Option<(SystemTime, u64)> {
    let md = std::fs::metadata(path).ok()?;
    Some((md.modified().ok()?, md.len()))
}

/// Whether imports are cached (GOBLIN_MODULE_CACHE=0 turns it off, for
/// measuring it).
fn enabled() -> bool {
    crate::reqenv::var("GOBLIN_MODULE_CACHE").as_deref() != Some("0")
}

/// The cached compilation of `path`, if it is still valid for an importer
/// whose globals are `globals_before`.
pub fn lookup(
    path: &Path,
    globals_before: &[String],
    module_prefix: &str,
    owner_glam: &Option<String>,
) -> Option<ImportedModule> {
    if !enabled() { return None; }
    let st = stamp(path)?;
    CACHE.with(|c| {
        let c = c.borrow();
        let key = (path.to_string_lossy().into_owned(), module_prefix.to_string(), owner_glam.clone());
        c.get(&key).filter(|m| m.stamp == st && m.globals_before == globals_before).map(|m| ImportedModule {
            entry: m.entry.clone(),
            classes: m.classes.clone(),
            enums: m.enums.clone(),
            global_names: m.global_names.clone(),
        })
    })
}

/// Lex, parse and compile `path`, and cache the result.
pub fn compile_import(
    path: &Path,
    globals_before: &[String],
    module_prefix: &str,
    owner_glam: Option<String>,
    quicken: impl FnOnce(&mut FunctionObject),
) -> Result<ImportedModule, GoblinError> {
    let key = (path.to_string_lossy().into_owned(), module_prefix.to_string(), owner_glam.clone());
    let st = if enabled() { stamp(path) } else { None };

    let source = std::fs::read_to_string(path)
        .map_err(|e| GoblinError::Runtime(format!("import '{}': {}", path.display(), e)))?;
    let tokens = goblin_lexer::lex(&source, &path.to_string_lossy())
        .map_err(|diags| GoblinError::Runtime(diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")))?;
    let module = goblin_parser::Parser::new(&tokens).parse_module()
        .map_err(|diags| GoblinError::Runtime(diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")))?;
    let compiled = crate::compiler::Compiler::new()
        .with_initial_globals(globals_before.to_vec())
        .with_global_prefix(Some(module_prefix.to_string()))
        .with_glam_namespace(owner_glam)
        .for_file(&path.to_string_lossy())
        .compile_module(&module)
        .map_err(|e| GoblinError::Runtime(format!("import compile error: {:?}", e)))?;
    let mut entry = compiled.entry;
    quicken(&mut entry);
    let entry = Rc::new(entry);
    let global_names = Rc::new(compiled.global_names);

    if let Some(st) = st {
        CACHE.with(|c| c.borrow_mut().insert(key, Cached {
            stamp: st,
            globals_before: globals_before.to_vec(),
            entry: entry.clone(),
            classes: compiled.classes.clone(),
            enums: compiled.enums.clone(),
            global_names: global_names.clone(),
        }));
    }
    Ok(ImportedModule { entry, classes: compiled.classes, enums: compiled.enums, global_names })
}
