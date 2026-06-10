use std::collections::HashMap;
use std::rc::Rc;

/// Logical address of a stash in the arena.
/// `slot` is index into Session.arena; `generation` detects stale addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address {
    pub slot: u32,
    pub generation: u32,
}

/// A runtime tether: lives in a VM slot (local / global).
/// Points to a stash via its Address.
///
/// name → (slot) → Tether → Stash → Value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tether {
    pub addr: Address,
}

/// The user-facing value that lives inside a stash.
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),

    /// Unified collections (arrays, maps, stacks, queues).
    Collection(Rc<CollectionValue>),

    /// A compiled Goblin function.
    Function(Rc<FunctionObject>),

    /// A built-in native function.
    Builtin(BuiltinId),
}

// Value::Float contains f64 which does not implement Eq/Hash, but we still need
// Value as a HashMap key for SmallMap / HashMapBackend.  We provide a manual
// implementation that panics on NaN and treats -0.0 == +0.0.
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil,            Value::Nil)            => true,
            (Value::Bool(a),        Value::Bool(b))        => a == b,
            (Value::Int(a),         Value::Int(b))         => a == b,
            (Value::Float(a),       Value::Float(b))       => a.to_bits() == b.to_bits(),
            (Value::Str(a),         Value::Str(b))         => a == b,
            (Value::Collection(a),  Value::Collection(b))  => Rc::ptr_eq(a, b),
            (Value::Function(a),    Value::Function(b))    => Rc::ptr_eq(a, b),
            (Value::Builtin(a),     Value::Builtin(b))     => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Nil           => {}
            Value::Bool(b)       => b.hash(state),
            Value::Int(n)        => n.hash(state),
            Value::Float(f)      => f.to_bits().hash(state),
            Value::Str(s)        => s.hash(state),
            Value::Collection(c) => (Rc::as_ptr(c) as usize).hash(state),
            Value::Function(f)   => (Rc::as_ptr(f) as usize).hash(state),
            Value::Builtin(b)    => b.hash(state),
        }
    }
}

/// Numeric IDs for all built-in functions dispatched by CallBuiltin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    // Memory introspection
    MemId,
    MemAddr,
    Gc,

    // Arithmetic helpers (called as functions, not inlined ops)
    Abs,
    Min,
    Max,

    // String
    Len,
    ToString,

    // Collections
    Grab,
    GrabFirst,
    GrabLast,
    GrabAt,
    GrabRandom,
    GrabWhere,
    GrabAll,
    GrabBetween,
    GrabMatching,

    Put,
    PutFirst,
    PutLast,
    PutAt,

    Update,
    UpdateFirst,
    UpdateLast,
    UpdateAt,

    Delete,
    DeleteFirst,
    DeleteLast,
    DeleteAt,
    DeleteWhere,
    DeleteAll,

    Reap,
    ReapFirst,
    ReapLast,
    ReapAt,
    ReapRandom,
    ReapWhere,
    ReapAll,

    Has,
    Keys,
    Values,
    Pairs,
    Count,

    // I/O (stubs for now)
    Print,
    Println,
    Eprint,

    // Type checks
    IsNil,
    IsBool,
    IsInt,
    IsFloat,
    IsStr,
    IsCollection,
    IsFunction,

    // Conversions
    ToInt,
    ToFloat,
    ToStr,
    ToBool,
}

/// One arena cell: stores a Value plus metadata.
#[derive(Debug)]
pub struct Stash {
    pub value: Rc<Value>,
    pub tether_count: usize,
    pub generation: u32,
}

// ──────────────────────────────────────────────────────────────────────────────
// Collections
// ──────────────────────────────────────────────────────────────────────────────

/// High-level collection value: semantic content + adaptive backend.
#[derive(Debug, Clone)]
pub struct CollectionValue {
    pub layout: CollectionLayout,
    pub meta: CollectionMeta,
}

/// The concrete backend layout Goblin uses for this collection.
#[derive(Debug, Clone)]
pub enum CollectionLayout {
    /// Default: contiguous Vec of Values.
    FlatArray(Rc<Vec<Value>>),

    /// Queue/stack/deque-optimised ring buffer.
    RingBuf(Rc<RingBuf>),

    /// Rope-style layout for large, edit-heavy sequences.
    ChunkedSeq(Rc<ChunkedSeq>),

    /// Tiny maps as a Vec of (key, value) pairs (< 16 entries).
    SmallMap(Rc<Vec<(Value, Value)>>),

    /// General-purpose hash-map backend.
    HashMapBackend(Rc<HashMap<Value, Value>>),
}

/// Ring buffer for queue/stack semantics.
#[derive(Debug, Clone)]
pub struct RingBuf {
    pub buf: Vec<Value>,
    pub head: usize,
    pub len: usize,
}

impl RingBuf {
    pub fn new() -> Self {
        RingBuf { buf: Vec::new(), head: 0, len: 0 }
    }

    pub fn push_back(&mut self, v: Value) {
        if self.len == self.buf.len() {
            // Grow: copy into a new contiguous buffer.
            let new_cap = (self.buf.len() * 2).max(4);
            let mut new_buf = Vec::with_capacity(new_cap);
            for i in 0..self.len {
                new_buf.push(self.buf[(self.head + i) % self.buf.len()].clone());
            }
            new_buf.push(v);
            self.buf = new_buf;
            self.head = 0;
            self.len += 1;
        } else {
            let tail = (self.head + self.len) % self.buf.len();
            self.buf[tail] = v;
            self.len += 1;
        }
    }

    pub fn push_front(&mut self, v: Value) {
        if self.len == self.buf.len() {
            let new_cap = (self.buf.len() * 2).max(4);
            let mut new_buf = Vec::with_capacity(new_cap);
            new_buf.push(v);
            for i in 0..self.len {
                new_buf.push(self.buf[(self.head + i) % self.buf.len()].clone());
            }
            self.buf = new_buf;
            self.head = 0;
            self.len += 1;
        } else {
            self.head = if self.head == 0 { self.buf.len() - 1 } else { self.head - 1 };
            self.buf[self.head] = v;
            self.len += 1;
        }
    }

    pub fn get(&self, i: usize) -> Option<&Value> {
        if i >= self.len { return None; }
        Some(&self.buf[(self.head + i) % self.buf.len()])
    }

    pub fn to_vec(&self) -> Vec<Value> {
        (0..self.len).map(|i| self.buf[(self.head + i) % self.buf.len()].clone()).collect()
    }
}

/// Chunked / rope-style layout for large, edit-heavy sequences.
#[derive(Debug, Clone)]
pub struct ChunkedSeq {
    pub chunks: Vec<Vec<Value>>,
    pub len: usize,
}

impl ChunkedSeq {
    pub const CHUNK_SIZE: usize = 32;

    pub fn from_flat(flat: &[Value]) -> Self {
        let chunks = flat.chunks(Self::CHUNK_SIZE).map(|c| c.to_vec()).collect();
        ChunkedSeq { chunks, len: flat.len() }
    }

    pub fn to_flat(&self) -> Vec<Value> {
        self.chunks.iter().flat_map(|c| c.iter().cloned()).collect()
    }

    pub fn get(&self, idx: usize) -> Option<&Value> {
        let mut remaining = idx;
        for chunk in &self.chunks {
            if remaining < chunk.len() {
                return Some(&chunk[remaining]);
            }
            remaining -= chunk.len();
        }
        None
    }
}

/// Lightweight usage metadata to drive adaptive layout selection.
#[derive(Debug, Clone, Default)]
pub struct CollectionMeta {
    pub len: usize,
    pub front_hits: u32,
    pub back_hits: u32,
    pub mid_hits: u32,
    pub random_hits: u32,
    pub scan_hits: u32,
    pub backend_hint: BackendHint,
}

impl CollectionMeta {
    /// Decide the best layout for the NEXT stash created from this collection.
    pub fn choose_layout(&self) -> BackendHint {
        if self.backend_hint != BackendHint::Auto {
            return self.backend_hint;
        }

        let front_back = self.front_hits + self.back_hits;
        let mid = self.mid_hits;

        // Large + mid-heavy → ChunkedSeq
        if self.len > 256 && mid > front_back {
            return BackendHint::ChunkedSeq;
        }

        // Queue/stack dominates → RingBuf
        if front_back > mid && front_back > 4 {
            return BackendHint::RingBuf;
        }

        BackendHint::Auto // stays FlatArray
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BackendHint {
    Auto,
    FlatArray,
    RingBuf,
    ChunkedSeq,
    SmallMap,
    HashMapBackend,
}

impl Default for BackendHint {
    fn default() -> Self { BackendHint::Auto }
}

// ──────────────────────────────────────────────────────────────────────────────
// FunctionObject — compiled Goblin function
// ──────────────────────────────────────────────────────────────────────────────

/// A compiled Goblin function produced by the bytecode compiler.
#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub bytecode: Vec<crate::vm::Opcode>,
    pub constants: Vec<Value>,
    pub locals: usize,
    pub params: usize,
    pub name: String,
}
