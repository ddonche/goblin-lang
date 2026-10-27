//! Which globals an actor's code can reach.
//!
//! An actor gets a copy of the globals its code can use, not all of them: a
//! list of actor handles kept in a global by the program's top-level code is
//! not copied into every actor it starts. Its code is its handler, its
//! stop condition, the functions in its state, and every named action,
//! method, import and class field default of the program (any of them can
//! be called by name, or runs when an object is made).
//! A global is reachable when such code reads or writes it, names it in a
//! string constant (interpolation, `alias::var`), or when it holds code
//! itself (a closure kept in a reachable global). A plain string that is
//! just a global's name (`"mail"`, a map key) only reaches that global when
//! it holds an action, since a plain string can be called but cannot read
//! a variable by itself. Builtins that find a
//! variable by a name computed at run time (`:clone_object("x")`, DES ticks
//! and links, `:resolve_token`) make every global reachable.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::opcode::{OpData, Opcode};
use crate::session::Session;
use crate::value::{BuiltinId, CollectionLayout, CollectionValue, FunctionObject, Value};

/// Collections with at least this many entries remember that they hold no
/// code, so the next actor started does not walk them again.
const REMEMBER: usize = 64;

/// What the program's own actions reach, worked out once per program.
pub(crate) struct Base {
    all: bool,
    slots: Vec<bool>,
    seen: HashSet<*const FunctionObject>,
    /// Global slots by the names a string constant can use for them: the
    /// full name, and the part after a module prefix.
    tokens: HashMap<String, Vec<usize>>,
    /// Globals named by a plain string: reached only while they hold code.
    maybe: Vec<usize>,
    /// Field defaults compiled for the walk, kept alive so their addresses
    /// in `seen` are never reused by other functions.
    _defaults: Vec<Rc<FunctionObject>>,
}

impl Base {
    pub(crate) fn of(session: &Session) -> Base {
        let mut tokens: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, name) in session.global_names.iter().enumerate() {
            tokens.entry(name.clone()).or_default().push(i);
            if let Some((_, short)) = name.rsplit_once("::") {
                tokens.entry(short.to_string()).or_default().push(i);
            }
        }
        let empty = HashSet::new();
        let mut w = Walk {
            base_seen: &empty,
            tokens: &tokens,
            seen: HashSet::new(),
            slots: vec![false; session.globals.len()],
            all: false,
            pending: Vec::new(),
            objects: HashSet::new(),
            maybe: Vec::new(),
        };
        for v in session.named_values.values() { w.value(v, session); }
        for list in session.action_file_map.values() {
            for (_, v) in list { w.value(v, session); }
        }
        for f in session.compiled_methods.values() { w.func(f); }
        // Field defaults that are not plain literals run when an object is
        // made, so the globals they read count too.
        let mut defaults = Vec::new();
        for class in session.classes.values() {
            for expr in class.fields.iter().filter_map(|f| f.default.as_ref()) {
                if crate::vm::eval_default_expr(expr).is_some() { continue; }
                match crate::vm::compile_field_default(expr, &session.global_names) {
                    Ok(f) => { w.func(&f); defaults.push(f); }
                    Err(_) => w.all = true,
                }
            }
        }
        // The values these globals hold now are walked for each actor, as
        // they can change between actors.
        let (all, slots, seen, maybe) = (w.all, w.slots, w.seen, w.maybe);
        Base { all, slots, seen, tokens, maybe, _defaults: defaults }
    }
}

/// The globals one actor can reach, given the program's `Base`.
/// `None` means all of them.
pub(crate) fn reachable(base: &Base, roots: &[&Value], session: &Session) -> Option<Vec<bool>> {
    if base.all { return None; }
    let mut slots = base.slots.clone();
    slots.resize(session.globals.len(), false);
    let pending = slots.iter().enumerate().filter(|(_, m)| **m).map(|(i, _)| i).collect();
    let mut w = Walk {
        base_seen: &base.seen,
        tokens: &base.tokens,
        seen: HashSet::new(),
        slots,
        all: false,
        pending,
        objects: HashSet::new(),
        maybe: base.maybe.clone(),
    };
    for r in roots { w.value(r, session); }
    loop {
        while let Some(i) = w.pending.pop() {
            if w.all { return None; }
            let Some(Some(t)) = session.globals.get(i) else { continue };
            let Ok(stash) = session.get_stash(t) else { continue };
            let v = stash.value.clone();
            w.value(&v, session);
        }
        // Globals named by a plain string count when they hold code now.
        let maybe = std::mem::take(&mut w.maybe);
        for i in maybe {
            if holds_code(session, i) { w.mark(i); }
        }
        if w.pending.is_empty() { break; }
    }
    if w.all { None } else { Some(w.slots) }
}

struct Walk<'a> {
    base_seen: &'a HashSet<*const FunctionObject>,
    tokens: &'a HashMap<String, Vec<usize>>,
    seen: HashSet<*const FunctionObject>,
    slots: Vec<bool>,
    all: bool,
    /// Slots newly reached whose values are still to be walked.
    pending: Vec<usize>,
    objects: HashSet<String>,
    /// Globals named by a plain string constant, decided by what they hold.
    maybe: Vec<usize>,
}

/// Whether global `i` holds code (an action or closure), or nothing yet.
fn holds_code(session: &Session, i: usize) -> bool {
    let Some(Some(t)) = session.globals.get(i) else { return true };
    match session.get_stash(t) {
        Ok(stash) => matches!(stash.value, Value::Function(_) | Value::Closure(_) | Value::Nil),
        Err(_) => true,
    }
}

impl Walk<'_> {
    fn mark(&mut self, i: usize) {
        if let Some(m) = self.slots.get_mut(i) {
            if !*m { *m = true; self.pending.push(i); }
        }
    }

    fn func(&mut self, f: &Rc<FunctionObject>) {
        let key = Rc::as_ptr(f);
        if self.all || self.base_seen.contains(&key) || !self.seen.insert(key) { return; }
        for op in &f.bytecode {
            match *op {
                Opcode::LoadGlobal(i) | Opcode::StoreGlobal(i) | Opcode::GetTypeLockGlobal(i)
                | Opcode::StoreLockGlobal(i, _) | Opcode::CastBangGlobal(i, _)
                | Opcode::CastMemberGlobal(i, _) => self.mark(i as usize),
                Opcode::CallBuiltin(id, _) if finds_by_name(id) => { self.all = true; return; }
                _ => {}
            }
        }
        if f.op_data.iter().any(|d| !matches!(d, OpData::Name(_) | OpData::Fused(_) | OpData::QuickSet(_) | OpData::Repeat(_))) { self.all = true; return; }
        // Labels hold the source text of an index path, for error messages
        // only (`st{"biz"}[b]`): they read nothing by name, so their words
        // count as plain words (a constant can be shared with a real use).
        let labels: HashSet<usize> = f.bytecode.iter()
            .filter_map(|op| match *op {
                Opcode::CheckPath(_, idx) | Opcode::IndexGet(idx) | Opcode::KeyGet(idx) => Some(idx as usize),
                _ => None,
            })
            .collect();
        for (i, c) in f.constants.iter().enumerate() {
            if labels.contains(&i) { self.label(c) } else { self.constant(c) }
        }
    }

    /// A function's constant: nested functions, and names in its strings.
    fn constant(&mut self, c: &Value) {
        match c {
            Value::Str(s) => self.names_in(s.as_str()),
            Value::Function(f) => self.func(f),
            Value::Closure(c) => self.func(&c.func),
            Value::Array(a) => for v in a { self.constant(v) },
            Value::Map(m) => for v in m.values() { self.constant(v) },
            Value::MapOrd(m) => for v in m.values() { self.constant(v) },
            Value::Pair(a, b) => { self.constant(a); self.constant(b) }
            Value::Formatted(v, _) | Value::CtrlReturn(v) => self.constant(v),
            Value::Collection(c) => for v in items(c) { self.constant(v) },
            _ => {}
        }
    }

    /// A label constant: its words, as plain words.
    fn label(&mut self, c: &Value) {
        match c {
            Value::Str(s) => self.words(s.as_str(), false),
            Value::Array(a) => for v in a { self.label(v) },
            other => self.constant(other),
        }
    }

    fn names_in(&mut self, s: &str) {
        // A template (`"{x}"`) or a module path (`"m::x"`) can read any
        // global it names; a plain word can only call one.
        self.words(s, s.contains('{') || s.contains("::"));
    }

    fn words(&mut self, s: &str, reads: bool) {
        for word in s.split(|c: char| !(c.is_alphanumeric() || c == '_')) {
            if word.is_empty() { continue; }
            if let Some(slots) = self.tokens.get(word) {
                for &i in slots {
                    if reads { self.mark(i) } else { self.maybe.push(i) }
                }
            }
        }
    }

    /// A value: the code it holds. Returns whether it holds none (no
    /// function, closure, object or frozen value anywhere inside).
    fn value(&mut self, v: &Value, session: &Session) -> bool {
        if self.all { return false; }
        match v {
            Value::Function(f) => { self.func(f); false }
            Value::Closure(c) => {
                self.func(&c.func);
                for cell in &c.upvalues {
                    if let Ok(s) = session.get_stash(&cell.get()) {
                        let v = s.value.clone();
                        self.value(&v, session);
                    }
                }
                false
            }
            Value::Array(a) => self.all_plain(a.iter(), session),
            Value::Seq(s) => self.all_plain(s.items.iter(), session),
            Value::Map(m) => self.all_plain(m.values(), session),
            Value::MapOrd(m) => self.all_plain(m.values(), session),
            Value::Pair(a, b) => self.value(a, session) & self.value(b, session),
            Value::Formatted(v, _) | Value::CtrlReturn(v) => self.value(v, session),
            Value::Object(o) => {
                if self.objects.insert(o.uuid.clone()) {
                    for f in o.fields.values() { self.value(f, session); }
                }
                false
            }
            Value::Ref(uuid) => {
                if self.objects.insert(uuid.clone()) {
                    if let Some(obj) = session.object_store.get(uuid) {
                        let obj = obj.clone();
                        self.value(&obj, session);
                    }
                }
                false
            }
            Value::Enum(e) => match &e.fields {
                Some(f) => self.all_plain(f.values(), session),
                None => true,
            },
            Value::Frozen(f) => { self.value(&f.value, session); false }
            Value::Collection(c) => self.collection(c, session),
            _ => true,
        }
    }

    fn all_plain<'v>(&mut self, it: impl Iterator<Item = &'v Value>, session: &Session) -> bool {
        let mut plain = true;
        for v in it { plain &= self.value(v, session); }
        plain
    }

    fn collection(&mut self, c: &Rc<CollectionValue>, session: &Session) -> bool {
        let key = Rc::as_ptr(c);
        if session.plain_collections.borrow().contains_key(&key) { return true; }
        let plain = match &c.layout {
            CollectionLayout::SmallMap(p) => {
                let mut plain = true;
                for (k, v) in p.iter() { plain &= self.value(k, session); plain &= self.value(v, session); }
                plain
            }
            CollectionLayout::HashMapBackend(m) => {
                let mut plain = true;
                for (k, v) in m.iter() { plain &= self.value(k, session); plain &= self.value(v, session); }
                plain
            }
            _ => self.all_plain(items(c).into_iter(), session),
        };
        if plain && len(c) >= REMEMBER {
            // Holding the collection keeps it unchanged: a change to it in
            // place copies it first (`Rc::make_mut`).
            let mut known = session.plain_collections.borrow_mut();
            known.insert(key, c.clone());
            let kept = &session.plain_collections_kept;
            if known.len() > 2 * kept.get().max(256) {
                known.retain(|_, c| Rc::strong_count(c) > 1);
                kept.set(known.len());
            }
        }
        plain
    }
}

/// The values of an array-like collection (keys and values of a map).
fn items(c: &CollectionValue) -> Vec<&Value> {
    match &c.layout {
        CollectionLayout::FlatArray(v) => v.iter().collect(),
        CollectionLayout::RingBuf(r) => r.buf.iter().collect(),
        CollectionLayout::ChunkedSeq(s) => s.chunks.iter().flat_map(|c| c.iter()).collect(),
        CollectionLayout::SmallMap(p) => p.iter().flat_map(|(k, v)| [k, v]).collect(),
        CollectionLayout::HashMapBackend(m) => m.iter().flat_map(|(k, v)| [k, v]).collect(),
    }
}

fn len(c: &CollectionValue) -> usize {
    match &c.layout {
        CollectionLayout::FlatArray(v) => v.len(),
        CollectionLayout::RingBuf(r) => r.buf.len(),
        CollectionLayout::ChunkedSeq(s) => s.len,
        CollectionLayout::SmallMap(p) => p.len(),
        CollectionLayout::HashMapBackend(m) => m.len(),
    }
}

/// Builtins that look a variable up by a name made at run time.
fn finds_by_name(id: BuiltinId) -> bool {
    matches!(id,
        BuiltinId::CloneObject | BuiltinId::LinkScore | BuiltinId::DecisionDebug
        | BuiltinId::QueryByIdent | BuiltinId::Tick | BuiltinId::ResolveToken
        | BuiltinId::Objects | BuiltinId::ObjectsIn | BuiltinId::Overlays | BuiltinId::OverlaysOf
        | BuiltinId::OverlayStrength | BuiltinId::OwnedBy | BuiltinId::OwnsTree
        | BuiltinId::DeleteOverlaysOn)
}
