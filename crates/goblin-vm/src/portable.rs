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
    ChunkedSeq, Closure, CollectionLayout, CollectionMeta, CollectionValue, FormatSpec, Frozen,
    FunctionObject, GoblinDateTime, RingBuf, Seq, UpvalueCell, UpvalueDescriptor, Value, BuiltinId,
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
    Str(String),
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
}

impl<'s> ToPortable<'s> {
    pub fn new(session: &'s Session) -> Self {
        ToPortable { session, funcs: HashMap::new() }
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
            Value::Str(s) => PValue::Str(s.clone()),
            Value::DateTime(d) => PValue::DateTime(d.clone()),
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
            Value::Object { class_name, fields, readonly_fields, trait_fields, uuid } => PValue::Object {
                class_name: class_name.clone(),
                fields: self.fields(fields)?,
                readonly_fields: readonly_fields.clone(),
                trait_fields: trait_fields.clone(),
                uuid: uuid.clone(),
            },
            // An object reference crosses as the object itself.
            Value::Ref(uuid) => match self.session.object_store.get(uuid) {
                Some(obj) => self.value(&obj.clone())?,
                None => return Err(GoblinError::Runtime(format!("dangling ref: uuid {uuid} not in object_store"))),
            },
            Value::GridRef { .. } => return Err(unsupported("a grid reference")),
            Value::Enum { enum_name, variant_name, fields } => PValue::Enum {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: match fields { Some(f) => Some(self.fields(f)?), None => None },
            },
            Value::Class { name } => PValue::Class { name: name.clone() },
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
        })
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
        let constants = self.values(&f.constants)?;
        let p = Arc::new(PFunction {
            bytecode: f.bytecode.clone(),
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
        Ok(p)
    }
}

/// Rebuilds portable values inside one session. Shared functions and frozen
/// snapshots are rebuilt once per session.
pub struct FromPortable {
    funcs: HashMap<*const PFunction, Rc<FunctionObject>>,
    frozen: HashMap<*const PValue, Rc<Frozen>>,
    global_names: HashMap<Vec<String>, Rc<std::cell::OnceCell<Vec<String>>>>,
}

impl Default for FromPortable {
    fn default() -> Self { Self::new() }
}

impl FromPortable {
    pub fn new() -> Self {
        FromPortable { funcs: HashMap::new(), frozen: HashMap::new(), global_names: HashMap::new() }
    }

    pub fn value(&mut self, p: &PValue, session: &mut Session) -> Value {
        match p {
            PValue::Nil => Value::Nil,
            PValue::Unit => Value::Unit,
            PValue::Bool(b) => Value::Bool(*b),
            PValue::Int(i) => Value::Int(*i),
            PValue::Float(f) => Value::Float(*f),
            PValue::Big(d) => Value::Big(*d),
            PValue::Pct(x) => Value::Pct(*x),
            PValue::Char(c) => Value::Char(*c),
            PValue::Str(s) => Value::Str(s.clone()),
            PValue::DateTime(d) => Value::DateTime(d.clone()),
            PValue::Duration(d) => Value::Duration(Box::new(d.clone())),
            PValue::Formatted(inner, spec) => Value::Formatted(Box::new(self.value(inner, session)), spec.clone()),
            PValue::Array(a) => Value::Array(self.values(a, session)),
            PValue::Map(m) => Value::Map(m.iter().map(|(k, v)| (k.clone(), self.value(v, session))).collect()),
            PValue::MapOrd(m) => Value::MapOrd(self.fields(m, session)),
            PValue::Pair(a, b) => Value::Pair(Box::new(self.value(a, session)), Box::new(self.value(b, session))),
            PValue::Seq(s) => Value::Seq(Seq::from_vec(self.values(s, session))),
            PValue::CtrlSkip => Value::CtrlSkip,
            PValue::CtrlStop => Value::CtrlStop,
            PValue::CtrlReturn(v) => Value::CtrlReturn(Box::new(self.value(v, session))),
            PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid } => Value::Object {
                class_name: class_name.clone(),
                fields: Rc::new(self.fields(fields, session)),
                readonly_fields: readonly_fields.clone(),
                trait_fields: trait_fields.clone(),
                uuid: uuid.clone(),
            },
            PValue::Enum { enum_name, variant_name, fields } => Value::Enum {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields.as_ref().map(|f| self.fields(f, session)),
            },
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
                let upvalues = ups.iter().map(|u| {
                    let v = self.value(u, session);
                    UpvalueCell::new(session.alloc_value(v))
                }).collect();
                Value::Closure(Rc::new(Closure { func, upvalues }))
            }
            PValue::Builtin(b) => Value::Builtin(*b),
            PValue::Frozen(shared) => Value::Frozen(self.frozen(shared, session)),
        }
    }

    /// A frozen snapshot, rebuilt once per session however often it arrives.
    pub fn frozen(&mut self, shared: &Arc<PValue>, session: &mut Session) -> Rc<Frozen> {
        let key = Arc::as_ptr(shared);
        if let Some(f) = self.frozen.get(&key) { return f.clone(); }
        let value = self.value(shared, session);
        let f = Rc::new(Frozen { value, shared: shared.clone() });
        self.frozen.insert(key, f.clone());
        f
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

    pub fn function(&mut self, f: &Arc<PFunction>, session: &mut Session) -> Rc<FunctionObject> {
        let key = Arc::as_ptr(f);
        if let Some(r) = self.funcs.get(&key) { return r.clone(); }
        let names = self.global_names.entry(f.global_names.clone()).or_insert_with(|| {
            let cell = std::cell::OnceCell::new();
            let _ = cell.set(f.global_names.clone());
            Rc::new(cell)
        }).clone();
        let constants = f.constants.iter().map(|c| self.value(c, session)).collect();
        let r = Rc::new(FunctionObject {
            bytecode: f.bytecode.clone(),
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
