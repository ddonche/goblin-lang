//! Bounded swarms: `:swarm(items, action, workers, context)` and `:swarm!`
//! (concurrency spec, Sections II, IX and XIII).
//!
//! The calling session is copied once into a portable image (its globals,
//! actions, classes, tokens and stores). Each worker thread rebuilds its own
//! isolated session from that image and runs `action(item, context)` for the
//! items it takes from a shared queue. Every task starts from the image's
//! state, so tasks cannot see each other's changes. Results come back in input
//! order, and so does whatever each task printed. The first failing task stops
//! the others from starting new items, and the swarm reports that failure.
//!
//! A frozen context (`:freeze`) is converted once and shared by every worker;
//! each worker rebuilds it once for reading.
//!
//! `:swarm` and `:swarm!` share this engine. Both currently tear every worker
//! down before returning, which is the stronger `:swarm!` contract; `:swarm`
//! is allowed to keep workers for reuse later.

use std::collections::{BTreeMap, HashMap};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use crate::error::GoblinError;
use crate::portable::{FromPortable, PFunction, PValue, ToPortable};
use crate::session::{GcMode, Session};
use crate::value::{CollectionValue, Value};
use crate::vm::Vm;

/// Stack for each worker thread; the VM recurses on the native stack for
/// nested actions, as the main thread does.
const WORKER_STACK: usize = 64 * 1024 * 1024;

/// Everything a worker needs to run the program's actions.
struct Image {
    globals: Vec<Option<PValue>>,
    global_names: Vec<String>,
    global_type_locks: HashMap<u32, String>,
    global_hard_type_locks: HashMap<u32, String>,
    gc_mode: GcMode,
    token_store: BTreeMap<String, BTreeMap<String, PValue>>,
    object_store: Vec<(String, PValue)>,
    box_store: Vec<(String, PValue)>,
    named_values: Vec<(String, PValue)>,
    action_file_map: Vec<(String, Vec<(String, PValue)>)>,
    compiled_methods: Vec<((String, String), Arc<PFunction>)>,
    classes: HashMap<String, goblin_ast::ClassDecl>,
    enums: HashMap<String, goblin_ast::EnumDecl>,
    unit_registry: HashMap<String, goblin_ast::UnitDecl>,
    module_aliases: HashMap<(String, String), String>,
    action_needs: HashMap<String, HashMap<String, String>>,
    imported: std::collections::HashSet<String>,
    base_dir: std::path::PathBuf,
    project_root: std::path::PathBuf,
}

fn image_of(session: &Session, tp: &mut ToPortable) -> Result<Image, GoblinError> {
    let mut globals = Vec::with_capacity(session.globals.len());
    for g in &session.globals {
        globals.push(match g {
            Some(t) => Some(tp.value(&session.get_stash(t)?.value)?),
            None => None,
        });
    }
    let mut token_store = BTreeMap::new();
    for (ns, m) in &session.token_store {
        let mut out = BTreeMap::new();
        for (k, v) in m { out.insert(k.clone(), tp.value(v)?); }
        token_store.insert(ns.clone(), out);
    }
    let pairs = |tp: &mut ToPortable, m: &HashMap<String, Value>| -> Result<Vec<(String, PValue)>, GoblinError> {
        m.iter().map(|(k, v)| Ok((k.clone(), tp.value(v)?))).collect()
    };
    let object_store = pairs(tp, &session.object_store)?;
    let box_store = pairs(tp, &session.box_store)?;
    let named_values = pairs(tp, &session.named_values)?;
    let mut action_file_map = Vec::new();
    for (file, list) in &session.action_file_map {
        let mut out = Vec::with_capacity(list.len());
        for (n, v) in list { out.push((n.clone(), tp.value(v)?)); }
        action_file_map.push((file.clone(), out));
    }
    let mut compiled_methods = Vec::new();
    for (k, f) in &session.compiled_methods {
        compiled_methods.push((k.clone(), tp.function(f)?));
    }
    Ok(Image {
        globals,
        global_names: session.global_names.clone(),
        global_type_locks: session.global_type_locks.clone(),
        global_hard_type_locks: session.global_hard_type_locks.clone(),
        gc_mode: session.gc_mode,
        token_store,
        object_store,
        box_store,
        named_values,
        action_file_map,
        compiled_methods,
        classes: session.classes.clone(),
        enums: session.enums.clone(),
        unit_registry: session.unit_registry.clone(),
        module_aliases: session.module_aliases.clone(),
        action_needs: session.action_needs.clone(),
        imported: session.imported.clone(),
        base_dir: session.base_dir.clone(),
        project_root: session.project_root.clone(),
    })
}

/// A value going into a variable: objects live in the object store and the
/// variable holds a reference, as `StoreLocal` does.
fn bind(session: &mut Session, v: Value) -> crate::value::Tether {
    if let Value::Object { ref uuid, .. } = v {
        let uuid = uuid.clone();
        session.object_store.insert(uuid.clone(), v);
        session.alloc_value(Value::Ref(uuid))
    } else {
        session.alloc_value(v)
    }
}

fn build_session(img: &Image, fp: &mut FromPortable, worker_id: usize) -> Session {
    let mut s = Session::new(img.gc_mode).with_worker_id(worker_id);
    s.global_names = img.global_names.clone();
    s.global_type_locks = img.global_type_locks.clone();
    s.global_hard_type_locks = img.global_hard_type_locks.clone();
    s.classes = img.classes.clone();
    s.enums = img.enums.clone();
    s.unit_registry = img.unit_registry.clone();
    s.module_aliases = img.module_aliases.clone();
    s.action_needs = img.action_needs.clone();
    s.imported = img.imported.clone();
    s.base_dir = img.base_dir.clone();
    s.project_root = img.project_root.clone();
    for ((c, m), f) in &img.compiled_methods {
        let f = fp.function(f, &mut s);
        s.compiled_methods.insert((c.clone(), m.clone()), f);
    }
    for (k, v) in &img.named_values {
        let v = fp.value(v, &mut s);
        s.named_values.insert(k.clone(), v);
    }
    for (file, list) in &img.action_file_map {
        let list = list.iter().map(|(n, v)| (n.clone(), fp.value(v, &mut s))).collect();
        s.action_file_map.insert(file.clone(), list);
    }
    for (k, v) in &img.object_store {
        let v = fp.value(v, &mut s);
        s.object_store.insert(k.clone(), v);
    }
    for (k, v) in &img.box_store {
        let v = fp.value(v, &mut s);
        s.box_store.insert(k.clone(), v);
    }
    for (ns, m) in &img.token_store {
        let m = m.iter().map(|(k, v)| (k.clone(), fp.value(v, &mut s))).collect();
        s.token_store.insert(ns.clone(), m);
    }
    s.ensure_globals(img.globals.len());
    for (i, g) in img.globals.iter().enumerate() {
        if let Some(p) = g {
            let v = fp.value(p, &mut s);
            let t = bind(&mut s, v);
            s.set_global(i, t);
        }
    }
    s
}

/// The state every task in a worker starts from.
struct Pristine {
    globals: Vec<Option<Value>>,
    token_store: BTreeMap<String, BTreeMap<String, Value>>,
    object_store: HashMap<String, Value>,
    box_store: HashMap<String, Value>,
}

impl Pristine {
    fn capture(s: &Session) -> Self {
        Pristine {
            globals: s.globals.iter()
                .map(|g| g.as_ref().and_then(|t| s.get_stash(t).ok()).map(|st| st.value.clone()))
                .collect(),
            token_store: s.token_store.clone(),
            object_store: s.object_store.clone(),
            box_store: s.box_store.clone(),
        }
    }

    fn restore(&self, s: &mut Session) {
        s.globals.truncate(self.globals.len());
        for (i, g) in self.globals.iter().enumerate() {
            match (g, s.globals.get(i).cloned().flatten()) {
                (Some(v), Some(t)) if s.overwrite(&t, v.clone()).is_ok() => {}
                (Some(v), _) => { let t = s.alloc_value(v.clone()); s.set_global(i, t); }
                (None, _) => { if i < s.globals.len() { s.globals[i] = None; } }
            }
        }
        s.token_store = self.token_store.clone();
        s.object_store = self.object_store.clone();
        s.box_store = self.box_store.clone();
        s.response = Default::default();
    }
}

fn task_seed(base: u128, i: usize) -> u128 {
    // splitmix-style mix, so each item's random sequence depends only on the
    // swarm and the item's position, never on which worker ran it.
    let mut z = base ^ ((i as u128).wrapping_add(1).wrapping_mul(0x9E3779B97F4A7C15F39CC0605CEDC835));
    z = (z ^ (z >> 61)).wrapping_mul(0xBF58476D1CE4E5B9_94D049BB133111EB);
    z ^= z >> 59;
    z | 1
}

struct Outcome {
    output: String,
    result: Result<PValue, String>,
}

/// Run a bounded swarm. `items` are the inputs in order; `context`, when
/// given, is passed to every call as the second argument.
pub fn run(
    items: Vec<Value>,
    action: Value,
    workers: i64,
    context: Option<Value>,
    session: &mut Session,
    _dispose: bool,
) -> Result<Value, GoblinError> {
    if workers < 1 {
        return Err(GoblinError::Runtime(format!(
            "C0104: swarm-workers: the worker count must be a positive whole number, got {workers}")));
    }
    if !matches!(action, Value::Function(_) | Value::Closure(_) | Value::Str(_)) {
        return Err(GoblinError::Runtime(format!(
            "C0105: swarm-action: the action must be an action, got {}", action.type_name())));
    }
    if items.is_empty() {
        return Ok(Value::Collection(std::rc::Rc::new(CollectionValue::from_flat(Vec::new()))));
    }

    // Convert everything once, in the calling session.
    let (image, p_items, p_action, p_context) = {
        let mut tp = ToPortable::new(session);
        let image = image_of(session, &mut tp)?;
        let p_items = items.iter().map(|v| tp.value(v)).collect::<Result<Vec<_>, _>>()?;
        let p_action = tp.value(&action)?;
        let p_context = match &context {
            Some(c) => Some(session.frozen_shared(c).map(PValue::Frozen).map_or_else(|| tp.value(c), Ok)?),
            None => None,
        };
        (image, p_items, p_action, p_context)
    };
    let base_seed = session.next_u128();

    let n = p_items.len();
    let threads = (workers as usize).min(n);
    let next = AtomicUsize::new(0);
    let cancel = AtomicBool::new(false);
    let outcomes: Mutex<Vec<Option<Outcome>>> = Mutex::new((0..n).map(|_| None).collect());
    let setup_error: Mutex<Option<String>> = Mutex::new(None);

    std::thread::scope(|sc| {
        for w in 0..threads {
            let (image, p_items, p_action, p_context) = (&image, &p_items, &p_action, &p_context);
            let (next, cancel, outcomes, setup_error) = (&next, &cancel, &outcomes, &setup_error);
            let spawned = std::thread::Builder::new()
                .name(format!("goblin-swarm-{w}"))
                .stack_size(WORKER_STACK)
                .spawn_scoped(sc, move || {
                    let mut fp = FromPortable::new();
                    let session = build_session(image, &mut fp, w + 1);
                    let pristine = Pristine::capture(&session);
                    let mut vm = Vm::new(session);
                    let action = fp.value(p_action, &mut vm.session);
                    let context = p_context.as_ref().map(|c| fp.value(c, &mut vm.session));
                    let mut first = true;
                    loop {
                        if cancel.load(Ordering::SeqCst) { break; }
                        let i = next.fetch_add(1, Ordering::SeqCst);
                        if i >= n { break; }
                        if !first { pristine.restore(&mut vm.session); }
                        first = false;
                        vm.reset_for_task();
                        vm.session.rng_state = task_seed(base_seed, i);
                        vm.session.output_buf = Some(String::new());
                        let mut args = vec![fp.value(&p_items[i], &mut vm.session)];
                        if let Some(c) = &context { args.push(c.clone()); }
                        let call = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            vm.call_callable(action.clone(), args)
                        }));
                        let output = vm.session.take_output();
                        let result = match call {
                            Ok(Ok(v)) => ToPortable::new(&vm.session).value(&v).map_err(|e| e.to_string()),
                            Ok(Err(e)) => Err(e.to_string()),
                            Err(_) => Err("the task crashed (internal error)".to_string()),
                        };
                        if result.is_err() { cancel.store(true, Ordering::SeqCst); }
                        outcomes.lock().unwrap()[i] = Some(Outcome { output, result });
                    }
                });
            if let Err(e) = spawned {
                cancel.store(true, Ordering::SeqCst);
                *setup_error.lock().unwrap() = Some(format!(
                    "C0106: swarm-resources: could not start worker thread {w}: {e}"));
                break;
            }
        }
    });

    if let Some(e) = setup_error.into_inner().unwrap() {
        return Err(GoblinError::Runtime(e));
    }

    // Results and printed output, in input order.
    let mut fp = FromPortable::new();
    let mut results = Vec::with_capacity(n);
    let mut failure: Option<(usize, String)> = None;
    for (i, o) in outcomes.into_inner().unwrap().into_iter().enumerate() {
        let Some(o) = o else { continue };
        if !o.output.is_empty() { session.write_output(&o.output, false); }
        match o.result {
            Ok(p) => { if failure.is_none() { results.push(fp.value(&p, session)); } }
            Err(e) => { if failure.is_none() { failure = Some((i, e)); } }
        }
    }
    if let Some((i, e)) = failure {
        return Err(GoblinError::Runtime(format!(
            "C0103: swarm-task-failed: the task for item {i} failed: {e}")));
    }
    Ok(Value::Collection(std::rc::Rc::new(CollectionValue::from_flat(results))))
}

/// `:freeze(value)`: a recursively read-only snapshot, converted once into
/// the form workers share.
pub fn freeze(v: Value, session: &mut Session) -> Result<Value, GoblinError> {
    if let Value::Frozen(_) = v { return Ok(v); }
    let shared = Arc::new(ToPortable::new(session).value(&v)?);
    session.remember_frozen(&v, &shared);
    Ok(Value::Frozen(std::rc::Rc::new(crate::value::Frozen { value: v, shared })))
}
