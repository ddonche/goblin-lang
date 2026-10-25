//! Values that can cross worker threads (concurrency spec, Section IV).
//!
//! VM values share their insides through `Rc`, which must never cross an OS
//! thread. A `PValue` is the same value graph rebuilt from owned data and
//! `Arc`, so it is `Send + Sync`. Workers receive copies (`to_portable` in
//! the sending session, `from_portable` in the receiving one) and frozen
//! snapshots, which are converted once and then shared by reference.
//!
//! Functions and closures cross too, so a worker can run the program's own
//! actions: their bytecode and constants are copied, and a closure's captured
//! variables are copied by value. Grid references cannot cross yet and are an
//! explicit error.

use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
use std::sync::Arc;

use crate::error::GoblinError;
use crate::session::Session;
use crate::value::{
    ChunkedSeq, Closure, EnumVal, ObjectVal, CollectionLayout, CollectionMeta, CollectionValue, FormatSpec, Frozen,
    FunctionObject, GStr, GoblinDateTime, RingBuf, Seq, UpvalueCell, UpvalueDescriptor, Value, BuiltinId,
};

#[derive(Debug, Clone)]
pub enum PValue {
    Nil,
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Big(rust_decimal::Decimal),
    Pct(f64),
    Char(char),
    Str(Arc<str>),
    DateTime(GoblinDateTime),
    Duration(crate::duration::Duration),
    Formatted(Box<PValue>, FormatSpec),
    Array(Vec<PValue>),
    Map(BTreeMap<String, PValue>),
    MapOrd(Vec<(String, PValue)>),
    Pair(Box<PValue>, Box<PValue>),
    Seq(Vec<PValue>),
    CtrlSkip,
    CtrlStop,
    CtrlReturn(Box<PValue>),
    Object {
        class_name: String,
        fields: Vec<(String, PValue)>,
        readonly_fields: std::collections::BTreeSet<String>,
        trait_fields: std::collections::BTreeSet<String>,
        uuid: String,
    },
    Enum { enum_name: String, variant_name: String, fields: Option<Vec<(String, PValue)>> },
    Class { name: String },
    Collection(PLayout, CollectionMeta),
    Function(Arc<PFunction>),
    Closure(Arc<PFunction>, Vec<PValue>),
    Builtin(BuiltinId),
    /// A frozen snapshot: shared by every worker that receives it.
    Frozen(Arc<PValue>),
    /// An actor, channel or future handle: already thread-safe.
    Handle(crate::actor::Handle),
    /// A variable whose object was handed to an actor: it stays dead.
    HandedOff { uuid: String, var: String },
    /// An object that stays with the session that started an actor (it was
    /// not handed over); the name it was stored under.
    KeptHome { uuid: String, var: String },
    /// A collection or long string holding no session state, converted once
    /// and shared: every actor on one worker reads the same copy.
    Shared(Arc<PValue>),
}

#[derive(Debug, Clone)]
pub enum PLayout {
    FlatArray(Vec<PValue>),
    RingBuf { buf: Vec<PValue>, head: usize, len: usize },
    ChunkedSeq { chunks: Vec<Vec<PValue>>, len: usize },
    SmallMap(Vec<(PValue, PValue)>),
    HashMapBackend(Vec<(PValue, PValue)>),
}

#[derive(Debug)]
pub struct PFunction {
    pub bytecode: Vec<crate::opcode::Opcode>,
    pub op_data: Vec<crate::opcode::OpData>,
    pub constants: Vec<PValue>,
    pub locals: usize,
    pub params: usize,
    pub required_params: usize,
    pub name: String,
    pub upvalue_descriptors: Vec<UpvalueDescriptor>,
    pub line_numbers: Vec<u32>,
    pub local_names: Vec<String>,
    pub owner_glam: Option<String>,
    pub source_file: String,
    pub global_names: Vec<String>,
}

fn unsupported(what: &str) -> GoblinError {
    GoblinError::Runtime(format!(
        "C0101: cannot-cross-workers: {what} cannot be copied to another worker"))
}

/// Converts values out of one session. Functions and shared collections are
/// converted once each, however often they appear.
pub struct ToPortable<'s> {
    session: &'s Session,
    funcs: HashMap<*const FunctionObject, Arc<PFunction>>,
    share: bool,
    /// Short texts converted so far: map keys and words like "wage" repeat
    /// in every item, so each is converted once.
    strs: std::collections::HashSet<Arc<str>>,
    /// For an actor: the objects handed to it. Any other object of the
    /// session stays behind instead of crossing as a copy.
    home: Option<std::collections::HashSet<String>>,
}

/// Texts up to this long are converted once per conversion and shared.
const SHORT_STR: usize = 64;

/// Strings at least this long are shared like collections.
const SHARED_STR: usize = 256;

impl<'s> ToPortable<'s> {
    pub fn new(session: &'s Session) -> Self {
        ToPortable { session, funcs: HashMap::new(), share: false, strs: Default::default(), home: None }
    }

    /// Convert collections and long strings that hold no session state once,
    /// keeping the copy in the session for the next actor started from it.
    pub fn sharing_values(mut self) -> Self { self.share = true; self }

    pub fn is_sharing(&self) -> bool { self.share }

    /// Only the objects in `handed` cross; the session's other objects stay
    /// where they are (an actor owns what it was handed, nothing more).
    pub(crate) fn only_objects(mut self, handed: std::collections::HashSet<String>) -> Self { self.home = Some(handed); self }

    /// An object of this session that does not go with the actor.
    fn stays_home(&self, uuid: &str) -> Option<PValue> {
        let h = self.home.as_ref()?;
        if h.contains(uuid) || !self.session.object_store.contains_key(uuid) { return None; }
        let var = self.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_else(|| "this object".to_string());
        Some(PValue::KeptHome { uuid: uuid.to_string(), var })
    }

    fn text(&mut self, s: &str) -> Arc<str> {
        if s.len() > SHORT_STR { return Arc::from(s); }
        if let Some(a) = self.strs.get(s) { return a.clone(); }
        let a: Arc<str> = Arc::from(s);
        self.strs.insert(a.clone());
        a
    }

    pub fn value(&mut self, v: &Value) -> Result<PValue, GoblinError> {
        Ok(match v {
            Value::Nil => PValue::Nil,
            Value::Unit => PValue::Unit,
            Value::Bool(b) => PValue::Bool(*b),
            Value::Int(i) => PValue::Int(*i),
            Value::Float(f) => PValue::Float(*f),
            Value::Big(d) => PValue::Big(*d),
            Value::Pct(p) => PValue::Pct(*p),
            Value::Char(c) => PValue::Char(*c),
            Value::Str(s) if self.share && s.as_str().len() >= SHARED_STR => self.shared(v)?,
            Value::Str(s) => PValue::Str(self.text(s.as_str())),
            Value::DateTime(d) => PValue::DateTime(*d.clone()),
            Value::Duration(d) => PValue::Duration((**d).clone()),
            Value::Formatted(inner, spec) => PValue::Formatted(Box::new(self.value(inner)?), spec.clone()),
            Value::Array(a) => PValue::Array(self.values(a)?),
            Value::Map(m) => {
                let mut out = BTreeMap::new();
                for (k, v) in m { out.insert(k.clone(), self.value(v)?); }
                PValue::Map(out)
            }
            Value::MapOrd(m) => PValue::MapOrd(self.fields(m)?),
            Value::Pair(a, b) => PValue::Pair(Box::new(self.value(a)?), Box::new(self.value(b)?)),
            Value::Seq(s) => PValue::Seq(self.values(&s.items)?),
            Value::CtrlSkip => PValue::CtrlSkip,
            Value::CtrlStop => PValue::CtrlStop,
            Value::CtrlReturn(v) => PValue::CtrlReturn(Box::new(self.value(v)?)),
            Value::Object(o) if self.stays_home(&o.uuid).is_some() => self.stays_home(&o.uuid).unwrap(),
            Value::Object(o) => PValue::Object {
                class_name: o.class_name.clone(),
                fields: self.fields(&o.fields)?,
                readonly_fields: o.readonly_fields.clone(),
                trait_fields: o.trait_fields.clone(),
                uuid: o.uuid.clone(),
            },
            // An object reference crosses as the object itself.
            Value::Ref(uuid) if self.stays_home(uuid).is_some() => self.stays_home(uuid).unwrap(),
            Value::Ref(uuid) => match self.session.object_store.get(uuid) {
                Some(obj) => self.value(&obj.clone())?,
                None => match self.session.handed_off.get(uuid.as_str()) {
                    Some(var) => PValue::HandedOff { uuid: uuid.clone(), var: var.clone() },
                    None => return Err(crate::actor::missing_object(self.session, uuid)),
                },
            },
            Value::GridRef { .. } => return Err(unsupported("a grid reference")),
            Value::Enum(e) => PValue::Enum {
                enum_name: e.enum_name.clone(),
                variant_name: e.variant_name.clone(),
                fields: match &e.fields { Some(f) => Some(self.fields(f)?), None => None },
            },
            Value::Class { name } => PValue::Class { name: name.clone() },
            Value::Collection(_) if self.share => self.shared(v)?,
            Value::Collection(c) => self.collection(c)?,
            Value::Function(f) => PValue::Function(self.function(f)?),
            Value::Closure(c) => {
                let func = self.function(&c.func)?;
                let mut ups = Vec::with_capacity(c.upvalues.len());
                for cell in &c.upvalues {
                    let t = cell.get();
                    let v = self.session.get_stash(&t)?.value.clone();
                    ups.push(self.value(&v)?);
                }
                PValue::Closure(func, ups)
            }
            Value::Builtin(b) => PValue::Builtin(*b),
            Value::Frozen(f) => PValue::Frozen(f.shared.clone()),
            Value::Handle(h) => PValue::Handle(h.clone()),
        })
    }

    /// A collection or long string, from the session's cache when it was
    /// converted before. The cache holds the value too, so a change to it in
    /// place copies it first (`Rc::make_mut`) and the cached copy stays true.
    fn shared(&mut self, v: &Value) -> Result<PValue, GoblinError> {
        let key = match v {
            Value::Collection(c) => Rc::as_ptr(c) as *const (),
            Value::Str(s) => s.ptr(),
            _ => unreachable!("only collections and strings are shared"),
        };
        if let Some((_, p)) = self.session.portable_values.borrow().get(&key) {
            return Ok(PValue::Shared(p.clone()));
        }
        let p = match v {
            Value::Collection(c) => self.collection(c)?,
            Value::Str(s) => PValue::Str(self.text(s.as_str())),
            _ => unreachable!(),
        };
        if !inert(&p) { return Ok(p); }
        let p = Arc::new(p);
        let mut cache = self.session.portable_values.borrow_mut();
        cache.insert(key, (v.clone(), p.clone()));
        let kept = &self.session.portable_values_kept;
        if cache.len() > 2 * kept.get().max(256) {
            // Forget values nothing but the cache still holds.
            cache.retain(|_, (v, _)| value_refs(v) > 1);
            kept.set(cache.len());
        }
        Ok(PValue::Shared(p))
    }

    fn values(&mut self, vs: &[Value]) -> Result<Vec<PValue>, GoblinError> {
        vs.iter().map(|v| self.value(v)).collect()
    }

    fn fields(&mut self, m: &indexmap::IndexMap<String, Value>) -> Result<Vec<(String, PValue)>, GoblinError> {
        let mut out = Vec::with_capacity(m.len());
        for (k, v) in m { out.push((k.clone(), self.value(v)?)); }
        Ok(out)
    }

    fn pairs<'a>(&mut self, it: impl Iterator<Item = (&'a Value, &'a Value)>) -> Result<Vec<(PValue, PValue)>, GoblinError> {
        let mut out = Vec::new();
        for (k, v) in it { out.push((self.value(k)?, self.value(v)?)); }
        Ok(out)
    }

    fn collection(&mut self, c: &CollectionValue) -> Result<PValue, GoblinError> {
        let layout = match &c.layout {
            CollectionLayout::FlatArray(v) => PLayout::FlatArray(self.values(v)?),
            CollectionLayout::RingBuf(r) => PLayout::RingBuf { buf: self.values(&r.buf)?, head: r.head, len: r.len },
            CollectionLayout::ChunkedSeq(s) => {
                let mut chunks = Vec::with_capacity(s.chunks.len());
                for ch in &s.chunks { chunks.push(self.values(ch)?); }
                PLayout::ChunkedSeq { chunks, len: s.len }
            }
            CollectionLayout::SmallMap(p) => PLayout::SmallMap(self.pairs(p.iter().map(|(k, v)| (k, v)))?),
            CollectionLayout::HashMapBackend(m) => PLayout::HashMapBackend(self.pairs(m.iter())?),
        };
        Ok(PValue::Collection(layout, c.meta.clone()))
    }

    pub fn function(&mut self, f: &Rc<FunctionObject>) -> Result<Arc<PFunction>, GoblinError> {
        let key = Rc::as_ptr(f);
        if let Some(p) = self.funcs.get(&key) { return Ok(p.clone()); }
        // Compiled code does not change, so the session keeps the copy for
        // the next actor or swarm. Only once its module has finished
        // compiling: before that its global names are not final.
        let cacheable = f.global_names.get().is_some();
        if cacheable {
            if let Some((_, p)) = self.session.portable_funcs.borrow().get(&key) {
                self.funcs.insert(key, p.clone());
                return Ok(p.clone());
            }
        }
        let constants = self.values(&f.constants)?;
        let p = Arc::new(PFunction {
            bytecode: f.bytecode.clone(),
            op_data: f.op_data.clone(),
            constants,
            locals: f.locals,
            params: f.params,
            required_params: f.required_params,
            name: f.name.clone(),
            upvalue_descriptors: f.upvalue_descriptors.clone(),
            line_numbers: f.line_numbers.clone(),
            local_names: f.local_names.clone(),
            owner_glam: f.owner_glam.clone(),
            source_file: f.source_file.clone(),
            global_names: f.global_names().to_vec(),
        });
        self.funcs.insert(key, p.clone());
        if cacheable {
            self.session.portable_funcs.borrow_mut().insert(key, (f.clone(), p.clone()));
        }
        Ok(p)
    }
}

/// Rebuilds portable values inside one session. Shared functions and frozen
/// snapshots are rebuilt once per session.
pub struct FromPortable {
    funcs: HashMap<*const PFunction, Rc<FunctionObject>>,
    /// Reuse functions already rebuilt on this thread by other sessions
    /// (actors on one worker share their compiled code).
    shared: bool,
    /// Rebuilt frozen values, and whether each holds a closure.
    frozen: HashMap<*const PValue, (Rc<Frozen>, bool)>,
    /// Shared values already rebuilt in this session.
    values: HashMap<*const PValue, Value>,
    global_names: HashMap<Vec<String>, Rc<std::cell::OnceCell<Vec<String>>>>,
    /// Short strings already rebuilt, so repeated keys and names ("id",
    /// "money", ...) share one copy instead of each allocating its own.
    strs: HashMap<Box<str>, GStr>,
    /// Objects rebuilt go into the session's object store, and the value
    /// holds a reference (an actor's starting state, at any depth).
    adopt: bool,
}

/// Strings up to this long are shared by content when rebuilt.
const INTERN_STR: usize = 32;
/// At most this many distinct short strings are remembered.
const INTERN_MAX: usize = 4096;

impl Default for FromPortable {
    fn default() -> Self { Self::new() }
}

impl FromPortable {
    pub fn new() -> Self {
        FromPortable { funcs: HashMap::new(), shared: false, frozen: HashMap::new(), values: HashMap::new(), global_names: HashMap::new(), strs: HashMap::new(), adopt: false }
    }

    /// Before converting into a new session, drop rebuilt frozen values that
    /// hold closures: their captured variables live in the old session.
    /// Frozen values without closures hold no session state and are reused,
    /// as are converted functions.
    pub fn forget_values(&mut self) {
        self.frozen.retain(|_, (_, cells)| !*cells);
        self.values.clear();
    }

    /// While on, every object rebuilt is stored in the session's object
    /// store and stands as a reference, however deep it sits.
    pub(crate) fn adopting(&mut self, on: bool) { self.adopt = on; }

    pub fn value(&mut self, p: &PValue, session: &mut Session) -> Value {
        match p {
            PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid } if self.adopt => {
                let fields = Rc::new(self.fields(fields, session));
                session.object_store.insert(uuid.clone(), Value::Object(Box::new(ObjectVal {
                    class_name: class_name.clone(),
                    fields,
                    readonly_fields: readonly_fields.clone(),
                    trait_fields: trait_fields.clone(),
                    uuid: uuid.clone(),
                })));
                Value::Ref(uuid.clone())
            }
            PValue::Nil => Value::Nil,
            PValue::Unit => Value::Unit,
            PValue::Bool(b) => Value::Bool(*b),
            PValue::Int(i) => Value::Int(*i),
            PValue::Float(f) => Value::Float(*f),
            PValue::Big(d) => Value::Big(*d),
            PValue::Pct(x) => Value::Pct(*x),
            PValue::Char(c) => Value::Char(*c),
            PValue::Str(s) => Value::Str(self.str(s)),
            PValue::DateTime(d) => Value::DateTime(Box::new(d.clone())),
            PValue::Duration(d) => Value::Duration(Box::new(d.clone())),
            PValue::Formatted(inner, spec) => Value::Formatted(Box::new(self.value(inner, session)), spec.clone()),
            PValue::Array(a) => Value::Array(self.values(a, session)),
            PValue::Map(m) => Value::Map(m.iter().map(|(k, v)| (k.clone(), self.value(v, session))).collect()),
            PValue::MapOrd(m) => Value::MapOrd(Box::new(self.fields(m, session))),
            PValue::Pair(a, b) => Value::Pair(Box::new(self.value(a, session)), Box::new(self.value(b, session))),
            PValue::Seq(s) => Value::Seq(Seq::from_vec(self.values(s, session))),
            PValue::CtrlSkip => Value::CtrlSkip,
            PValue::CtrlStop => Value::CtrlStop,
            PValue::CtrlReturn(v) => Value::CtrlReturn(Box::new(self.value(v, session))),
            PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid } => Value::Object(Box::new(ObjectVal {
                class_name: class_name.clone(),
                fields: Rc::new(self.fields(fields, session)),
                readonly_fields: readonly_fields.clone(),
                trait_fields: trait_fields.clone(),
                uuid: uuid.clone(),
            })),
            PValue::Enum { enum_name, variant_name, fields } => Value::Enum(Box::new(EnumVal {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields.as_ref().map(|f| self.fields(f, session)),
            })),
            PValue::Class { name } => Value::Class { name: name.clone() },
            PValue::Collection(layout, meta) => {
                let layout = match layout {
                    PLayout::FlatArray(v) => CollectionLayout::FlatArray(Rc::new(self.values(v, session))),
                    PLayout::RingBuf { buf, head, len } => CollectionLayout::RingBuf(Rc::new(RingBuf {
                        buf: self.values(buf, session), head: *head, len: *len,
                    })),
                    PLayout::ChunkedSeq { chunks, len } => CollectionLayout::ChunkedSeq(Rc::new(ChunkedSeq {
                        chunks: chunks.iter().map(|c| self.values(c, session)).collect(), len: *len,
                    })),
                    PLayout::SmallMap(p) => CollectionLayout::SmallMap(Rc::new(self.pairs(p, session))),
                    PLayout::HashMapBackend(p) => CollectionLayout::HashMapBackend(Rc::new(
                        self.pairs(p, session).into_iter().collect())),
                };
                Value::Collection(Rc::new(CollectionValue { layout, meta: meta.clone() }))
            }
            PValue::Function(f) => Value::Function(self.function(f, session)),
            PValue::Closure(f, ups) => {
                let func = self.function(f, session);
                // Captured variables live in the arena; the cleanup must trace them.
                session.has_cells = true;
                let upvalues = ups.iter().map(|u| {
                    let v = self.value(u, session);
                    UpvalueCell::new(session.alloc_value(v))
                }).collect();
                Value::Closure(Rc::new(Closure { func, upvalues }))
            }
            PValue::Builtin(b) => Value::Builtin(*b),
            PValue::Frozen(shared) => Value::Frozen(self.frozen(shared, session)),
            PValue::Handle(h) => Value::Handle(h.clone()),
            PValue::HandedOff { uuid, var } => {
                session.handed_off.insert(uuid.clone(), var.clone());
                Value::Ref(uuid.clone())
            }
            PValue::KeptHome { uuid, var } => {
                session.kept_home.insert(uuid.clone(), var.clone());
                Value::Ref(uuid.clone())
            }
            PValue::Shared(inner) => self.shared_value(inner, session),
        }
    }

    /// A shared value: rebuilt once per session, and once per thread when
    /// this conversion shares with the thread's other sessions. It holds no
    /// session state, and a change to it copies it first.
    fn shared_value(&mut self, inner: &Arc<PValue>, session: &mut Session) -> Value {
        let key = Arc::as_ptr(inner);
        if let Some(v) = self.values.get(&key) { return v.clone(); }
        let v = if self.shared {
            match THREAD_VALUES.with(|m| m.borrow().map.get(&key).map(|(_, v)| v.clone())) {
                Some(v) => v,
                None => {
                    let v = self.value(inner, session);
                    THREAD_VALUES.with(|m| m.borrow_mut().insert(key, inner.clone(), v.clone()));
                    v
                }
            }
        } else {
            self.value(inner, session)
        };
        self.values.insert(key, v.clone());
        v
    }

    /// A frozen snapshot, rebuilt once per session however often it arrives.
    pub fn frozen(&mut self, shared: &Arc<PValue>, session: &mut Session) -> Rc<Frozen> {
        let key = Arc::as_ptr(shared);
        if let Some((f, _)) = self.frozen.get(&key) { return f.clone(); }
        let value = self.value(shared, session);
        let f = Rc::new(Frozen { value, shared: shared.clone() });
        self.frozen.insert(key, (f.clone(), has_closure(shared)));
        f
    }

    /// A rebuilt string; short ones are shared with earlier equal ones.
    fn str(&mut self, s: &str) -> GStr {
        if s.len() > INTERN_STR { return GStr::new(s.to_string()); }
        if let Some(g) = self.strs.get(s) { return g.clone(); }
        let g = GStr::new(s.to_string());
        if self.strs.len() < INTERN_MAX { self.strs.insert(s.into(), g.clone()); }
        g
    }

    fn values(&mut self, vs: &[PValue], session: &mut Session) -> Vec<Value> {
        vs.iter().map(|v| self.value(v, session)).collect()
    }

    fn fields(&mut self, m: &[(String, PValue)], session: &mut Session) -> indexmap::IndexMap<String, Value> {
        m.iter().map(|(k, v)| (k.clone(), self.value(v, session))).collect()
    }

    fn pairs(&mut self, p: &[(PValue, PValue)], session: &mut Session) -> Vec<(Value, Value)> {
        p.iter().map(|(k, v)| (self.value(k, session), self.value(v, session))).collect()
    }

    /// Rebuilt functions are shared with the other sessions on this thread.
    /// A function is read-only once built and its constants are plain
    /// values, so sessions on one thread can use the same copy.
    pub fn sharing_functions(mut self) -> Self { self.shared = true; self }

    pub fn is_sharing(&self) -> bool { self.shared }

    pub fn function(&mut self, f: &Arc<PFunction>, session: &mut Session) -> Rc<FunctionObject> {
        let key = Arc::as_ptr(f);
        if let Some(r) = self.funcs.get(&key) { return r.clone(); }
        if self.shared && f.constants.iter().all(inert) {
            if let Some(r) = THREAD_FUNCS.with(|m| m.borrow().map.get(&key).map(|(_, r)| r.clone())) {
                self.funcs.insert(key, r.clone());
                return r;
            }
            let r = self.build_function(f, session);
            THREAD_FUNCS.with(|m| m.borrow_mut().insert(key, f.clone(), r.clone()));
            return r;
        }
        self.build_function(f, session)
    }

    fn build_function(&mut self, f: &Arc<PFunction>, session: &mut Session) -> Rc<FunctionObject> {
        let key = Arc::as_ptr(f);
        let names = self.global_names.entry(f.global_names.clone()).or_insert_with(|| {
            let cell = std::cell::OnceCell::new();
            let _ = cell.set(f.global_names.clone());
            Rc::new(cell)
        }).clone();
        let constants = f.constants.iter().map(|c| self.value(c, session)).collect();
        let r = Rc::new(FunctionObject {
            bytecode: f.bytecode.clone(),
            op_data: f.op_data.clone(),
            constants,
            locals: f.locals,
            params: f.params,
            required_params: f.required_params,
            name: f.name.clone(),
            upvalue_descriptors: f.upvalue_descriptors.clone(),
            line_numbers: f.line_numbers.clone(),
            local_names: f.local_names.clone(),
            owner_glam: f.owner_glam.clone(),
            source_file: f.source_file.clone(),
            global_names: names,
        });
        self.funcs.insert(key, r.clone());
        r
    }
}

/// Functions rebuilt on this thread, shared by the sessions living on it.
struct ThreadFuncs {
    map: HashMap<*const PFunction, (Arc<PFunction>, Rc<FunctionObject>)>,
    /// Size after the last clean-up; the next one runs when it doubles.
    kept: usize,
}

impl ThreadFuncs {
    fn insert(&mut self, key: *const PFunction, p: Arc<PFunction>, r: Rc<FunctionObject>) {
        self.map.insert(key, (p, r));
        if self.map.len() > 2 * self.kept.max(512) {
            // Forget functions no session uses any more.
            self.map.retain(|_, (_, r)| Rc::strong_count(r) > 1);
            self.kept = self.map.len();
        }
    }
}

/// Shared values rebuilt on this thread, used by the sessions living on it.
struct ThreadValues {
    map: HashMap<*const PValue, (Arc<PValue>, Value)>,
    kept: usize,
}

impl ThreadValues {
    fn insert(&mut self, key: *const PValue, p: Arc<PValue>, v: Value) {
        self.map.insert(key, (p, v));
        if self.map.len() > 2 * self.kept.max(512) {
            self.map.retain(|_, (_, v)| value_refs(v) > 1);
            self.kept = self.map.len();
        }
    }
}

/// How many holders share a collection's or string's buffer.
fn value_refs(v: &Value) -> usize {
    match v {
        Value::Collection(c) => Rc::strong_count(c),
        Value::Str(s) => s.refs(),
        _ => usize::MAX,
    }
}

thread_local! {
    static THREAD_VALUES: std::cell::RefCell<ThreadValues> =
        std::cell::RefCell::new(ThreadValues { map: HashMap::new(), kept: 0 });
}

thread_local! {
    static THREAD_FUNCS: std::cell::RefCell<ThreadFuncs> =
        std::cell::RefCell::new(ThreadFuncs { map: HashMap::new(), kept: 0 });
}

/// A constant that holds no session state: safe in a function shared by
/// several sessions on one thread.
pub(crate) fn inert(p: &PValue) -> bool {
    match p {
        PValue::Nil | PValue::Unit | PValue::Bool(_) | PValue::Int(_) | PValue::Float(_)
        | PValue::Big(_) | PValue::Pct(_) | PValue::Char(_) | PValue::Str(_)
        | PValue::DateTime(_) | PValue::Duration(_) | PValue::CtrlSkip | PValue::CtrlStop
        | PValue::Builtin(_) | PValue::Class { .. } | PValue::Function(_) | PValue::Shared(_) => true,
        PValue::Formatted(v, _) | PValue::CtrlReturn(v) => inert(v),
        PValue::Array(a) | PValue::Seq(a) => a.iter().all(inert),
        PValue::Map(m) => m.values().all(inert),
        PValue::MapOrd(m) => m.iter().all(|(_, v)| inert(v)),
        PValue::Pair(a, b) => inert(a) && inert(b),
        PValue::Enum { fields, .. } => fields.as_ref().is_none_or(|f| f.iter().all(|(_, v)| inert(v))),
        PValue::Collection(layout, _) => match layout {
            PLayout::FlatArray(v) => v.iter().all(inert),
            PLayout::RingBuf { buf, .. } => buf.iter().all(inert),
            PLayout::ChunkedSeq { chunks, .. } => chunks.iter().all(|c| c.iter().all(inert)),
            PLayout::SmallMap(p) | PLayout::HashMapBackend(p) => p.iter().all(|(k, v)| inert(k) && inert(v)),
        },
        PValue::Object { .. } | PValue::Closure(..) | PValue::Frozen(_) | PValue::Handle(_)
        | PValue::HandedOff { .. } | PValue::KeptHome { .. } => false,
    }
}

/// Whether a portable value holds a closure anywhere inside it.
fn has_closure(p: &PValue) -> bool {
    let any = |vs: &[PValue]| vs.iter().any(has_closure);
    let any_pairs = |ps: &[(PValue, PValue)]| ps.iter().any(|(k, v)| has_closure(k) || has_closure(v));
    let any_fields = |fs: &[(String, PValue)]| fs.iter().any(|(_, v)| has_closure(v));
    match p {
        PValue::Closure(..) => true,
        PValue::Formatted(v, _) | PValue::CtrlReturn(v) => has_closure(v),
        PValue::Array(a) | PValue::Seq(a) => any(a),
        PValue::Map(m) => m.values().any(has_closure),
        PValue::MapOrd(m) => any_fields(m),
        PValue::Pair(a, b) => has_closure(a) || has_closure(b),
        PValue::Object { fields, .. } => any_fields(fields),
        PValue::Enum { fields: Some(f), .. } => any_fields(f),
        PValue::Collection(layout, _) => match layout {
            PLayout::FlatArray(v) => any(v),
            PLayout::RingBuf { buf, .. } => any(buf),
            PLayout::ChunkedSeq { chunks, .. } => chunks.iter().any(|c| any(c)),
            PLayout::SmallMap(p) | PLayout::HashMapBackend(p) => any_pairs(p),
        },
        PValue::Frozen(inner) => has_closure(inner),
        _ => false,
    }
}
