//! A startup script: run once when a server starts (`goblin start --startup
//! FILE`, or `GOBLIN_STARTUP=FILE`). Its top-level variables are copied into
//! every request's VM before the request runs, and the actors it starts live
//! until the server stops.
//!
//! Requests never share memory with the startup script or with each other:
//! each one gets its own copy of the variables (a frozen value stays one
//! shared frozen snapshot, and an actor or channel handle still names the same
//! actor or channel), so requests and the startup actors only talk through
//! messages.

use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use crate::actor::ActorShared;
use crate::compiler::Compiler;
use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::session::Session;

struct Startup {
    vars: Vec<(String, PValue)>,
    actors: Mutex<Vec<Arc<ActorShared>>>,
}

static STARTUP: OnceLock<Startup> = OnceLock::new();

fn compile_err(msg: String, at: &str) -> GoblinError {
    GoblinError::CompileError { message: msg, span_debug: at.into() }
}

/// Runs the startup script once. Its own top-level variables are kept for
/// requests; the actors it started are kept running.
pub fn run(path: &Path) -> Result<(), GoblinError> {
    if STARTUP.get().is_some() {
        return Err(GoblinError::Runtime("the startup script has already run".into()));
    }
    let file = path.display().to_string();
    let src = std::fs::read_to_string(path)
        .map_err(|e| GoblinError::Runtime(format!("startup script {file}: {e}")))?;
    let tokens = goblin_lexer::lex(&src, &file)
        .map_err(|d| compile_err(d.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("\n"), "<lex>"))?;
    let module = goblin_parser::Parser::new(&tokens).parse_module()
        .map_err(|d| compile_err(d.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("\n"), "<parse>"))?;
    let compiled = Compiler::new().for_file(&file).compile_module(&module)?;
    let own = compiled.global_names.clone();
    let (mut vm, entry) = crate::exec::prepare_entry(compiled, path, Vec::new())?;
    vm.execute(entry)?;

    let mut vars = Vec::new();
    {
        let s = &vm.session;
        let mut tp = ToPortable::new(s);
        for (i, name) in own.iter().enumerate() {
            let Some(Some(t)) = s.globals.get(i) else { continue };
            let v = s.get_stash(t)?.value.clone();
            let v = match v {
                crate::value::Value::Ref(u) => s.object_store.get(u.as_str()).cloned()
                    .unwrap_or(crate::value::Value::Ref(u)),
                other => other,
            };
            let p = tp.value(&v).map_err(|e| GoblinError::Runtime(format!(
                "startup script: variable `{name}` can't be given to requests: {e}")))?;
            vars.push((name.clone(), p));
        }
    }
    let actors = std::mem::take(&mut vm.session.owned_actors);
    drop(vm);
    let _ = STARTUP.set(Startup { vars, actors: Mutex::new(actors) });
    Ok(())
}

/// The startup script's variable names, in order, when one has run.
pub fn names() -> Vec<String> {
    STARTUP.get().map(|s| s.vars.iter().map(|(n, _)| n.clone()).collect()).unwrap_or_default()
}

/// A compiler that already knows the startup variables, so a request script
/// (and the modules it imports) can use them.
pub fn compiler() -> Compiler {
    Compiler::new().with_initial_globals(names())
}

/// Gives a request's session its own copy of each startup variable.
pub fn bind(session: &mut Session) {
    let Some(st) = STARTUP.get() else { return };
    let mut fp = FromPortable::new();
    for (name, p) in &st.vars {
        let Some(idx) = session.global_names.iter().position(|n| n == name) else { continue };
        let v = fp.value(p, session);
        let t = crate::swarm::bind(session, v);
        session.ensure_globals(idx + 1);
        session.set_global(idx, t);
    }
}

/// Stops the startup actors: each finishes the messages it accepted, then ends.
pub fn finish() {
    if let Some(st) = STARTUP.get() {
        let mut a = st.actors.lock().unwrap_or_else(|e| e.into_inner());
        crate::actor::finish_owned(&mut a);
    }
}
