use std::collections::BTreeMap;
use std::rc::Rc;
use indexmap::IndexMap;
use rust_decimal::Decimal;

// ──────────────────────────────────────────────
// Arena primitives
// ──────────────────────────────────────────────

/// Logical address of a stash in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address {
    pub slot: u32,
    pub generation: u32,
}

/// A runtime tether: lives in a VM slot and points to a stash.
/// Chain: name → (slot index) → Tether → Stash → Value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tether {
    pub addr: Address,
}

/// One arena cell: stores a Value plus GC metadata.
#[derive(Debug)]
pub struct Stash {
    pub value: Rc<Value>,
    pub tether_count: usize,
    pub generation: u32,
}

// ──────────────────────────────────────────────
// Value — all user-facing types
// ──────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Value {
    // ── Scalars ──────────────────────────────
    Int(i64),
    Float(f64),
    /// Exact decimal (rust_decimal).
    Big(Decimal),
    Str(String),
    Char(char),
    Bool(bool),
    /// Percentage: 50.0 == 50 %.
    Pct(f64),
    /// Value with attached display format.
    Formatted(Box<Value>, FormatSpec),
    /// Divmod pair produced by `><`.
    Pair(Box<Value>, Box<Value>),
    Nil,
    Unit,

    // ── Adaptive collection ───────────────────
    /// Arrays, maps, queues, stacks — all backed by an adaptive layout.
    /// The physical layout is chosen by the VM; the semantic value is fixed.
    Collection(Rc<CollectionValue>),

    // ── Control-flow sentinels (VM-internal) ──
    CtrlSkip,
    CtrlStop,
    CtrlReturn(Box<Value>),

    // ── Object system ─────────────────────────
    Object {
        class_name: String,
        fields: IndexMap<String, Value>,
    },
    /// Handle into the session's object store (mutable objects).
    Ref(String),

    GridRef { grid_id: String, x: i32, y: i32 },

    Enum {
        enum_name: String,
        variant_name: String,
        fields: Option<IndexMap<String, Value>>,
    },

    Class { name: String },
}

// ──────────────────────────────────────────────
// FormatSpec
// ──────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct FormatSpec {
    pub decimals: u32,
    pub sep_thousands: Option<char>,
    pub sep_decimal: char,
}

// ──────────────────────────────────────────────
// Adaptive Collection
// ──────────────────────────────────────────────

/// Semantic kind: tells the builtin layer whether this collection is
/// sequence-like or map-like, and whether map order is preserved.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionKind {
    /// Ordered by position (Array / Seq).
    Seq,
    /// String-keyed, insertion-ordered (MapOrd / JSON-style).
    MapOrd,
    /// String-keyed, sorted by key (Map).
    MapSorted,
}

/// The physical backend layout — invisible to user code.
/// Chosen by the VM each time a new stash is created.
#[derive(Debug, Clone)]
pub enum CollectionLayout {
    /// Default for small/mid sequential data. Vec<Value>.
    FlatArray(Rc<Vec<Value>>),

    /// Queue/stack optimised layout (front and back O(1)).
    RingBuf(Rc<RingBuf>),

    /// Chunked rope-style layout for large, edit-heavy sequences.
    ChunkedSeq(Rc<ChunkedSeq>),

    /// Tiny string-keyed map stored as Vec<(String, Value)>.
    SmallMap(Rc<Vec<(String, Value)>>),

    /// General string-keyed hash map (large maps / frequent lookups).
    HashMap(Rc<BTreeMap<String, Value>>),

    /// Insertion-ordered string map.
    IndexMap(Rc<IndexMap<String, Value>>),
}

/// High-level collection value: semantic kind + physical layout + usage meta.
#[derive(Debug, Clone)]
pub struct CollectionValue {
    pub kind: CollectionKind,
    pub layout: CollectionLayout,
    pub meta: CollectionMeta,
}

impl CollectionValue {
    /// Construct a new empty sequence.
    pub fn new_seq() -> Self {
        CollectionValue {
            kind: CollectionKind::Seq,
            layout: CollectionLayout::FlatArray(Rc::new(Vec::new())),
            meta: CollectionMeta::default(),
        }
    }

    /// Construct from a Vec<Value> (sequence).
    pub fn from_vec(v: Vec<Value>) -> Self {
        let len = v.len();
        CollectionValue {
            kind: CollectionKind::Seq,
            layout: CollectionLayout::FlatArray(Rc::new(v)),
            meta: CollectionMeta { len, ..Default::default() },
        }
    }

    /// Construct from a BTreeMap (sorted map).
    pub fn from_btree(m: BTreeMap<String, Value>) -> Self {
        let len = m.len();
        CollectionValue {
            kind: CollectionKind::MapSorted,
            layout: CollectionLayout::HashMap(Rc::new(m)),
            meta: CollectionMeta { len, ..Default::default() },
        }
    }

    /// Construct from an IndexMap (insertion-ordered map).
    pub fn from_indexmap(m: IndexMap<String, Value>) -> Self {
        let len = m.len();
        CollectionValue {
            kind: CollectionKind::MapOrd,
            layout: CollectionLayout::IndexMap(Rc::new(m)),
            meta: CollectionMeta { len, ..Default::default() },
        }
    }

    pub fn len(&self) -> usize { self.meta.len }
    pub fn is_empty(&self) -> bool { self.meta.len == 0 }

    /// Materialize as a Vec<Value> slice (for seq operations).
    pub fn as_seq_slice(&self) -> Option<&[Value]> {
        match &self.layout {
            CollectionLayout::FlatArray(v) => Some(v.as_slice()),
            _ => None,
        }
    }

    /// Materialize to owned Vec<Value> (copies if needed).
    pub fn to_vec(&self) -> Vec<Value> {
        match &self.layout {
            CollectionLayout::FlatArray(v) => v.as_ref().clone(),
            CollectionLayout::RingBuf(rb) => rb.to_vec(),
            CollectionLayout::ChunkedSeq(cs) => cs.to_vec(),
            _ => Vec::new(),
        }
    }

    /// Get a value by string key (map operations).
    pub fn map_get(&self, key: &str) -> Option<&Value> {
        match &self.layout {
            CollectionLayout::SmallMap(pairs) => {
                pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v)
            }
            CollectionLayout::HashMap(m) => m.get(key),
            CollectionLayout::IndexMap(m) => m.get(key),
            _ => None,
        }
    }

    /// Iterate key-value pairs (map operations).
    pub fn map_iter(&self) -> Vec<(String, Value)> {
        match &self.layout {
            CollectionLayout::SmallMap(pairs) => pairs.as_ref().clone(),
            CollectionLayout::HashMap(m) => {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            }
            CollectionLayout::IndexMap(m) => {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            }
            _ => Vec::new(),
        }
    }
}

// ──────────────────────────────────────────────
// RingBuf / ChunkedSeq (adaptive backends)
// ──────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct RingBuf {
    pub buf: Vec<Value>,
    pub head: usize,
    pub len: usize,
}

impl RingBuf {
    pub fn to_vec(&self) -> Vec<Value> {
        let mut v = Vec::with_capacity(self.len);
        for i in 0..self.len {
            v.push(self.buf[(self.head + i) % self.buf.len()].clone());
        }
        v
    }
}

#[derive(Debug, Clone)]
pub struct ChunkedSeq {
    pub chunks: Vec<Vec<Value>>,
    pub len: usize,
}

impl ChunkedSeq {
    pub fn to_vec(&self) -> Vec<Value> {
        self.chunks.iter().flatten().cloned().collect()
    }
}

// ──────────────────────────────────────────────
// Collection usage metadata (drives adaptive switching)
// ──────────────────────────────────────────────

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BackendHint {
    #[default]
    Auto,
    FlatArray,
    RingBuf,
    ChunkedSeq,
    SmallMap,
    HashMap,
    IndexMap,
}

// ──────────────────────────────────────────────
// PartialEq for Value
// f64 is PartialEq but not Eq; Collection uses Rc pointer equality.
// ──────────────────────────────────────────────

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a),   Value::Int(b))   => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Big(a),   Value::Big(b))   => a == b,
            (Value::Str(a),   Value::Str(b))   => a == b,
            (Value::Char(a),  Value::Char(b))  => a == b,
            (Value::Bool(a),  Value::Bool(b))  => a == b,
            (Value::Pct(a),   Value::Pct(b))   => a == b,
            (Value::Nil,      Value::Nil)      => true,
            (Value::Unit,     Value::Unit)     => true,
            (Value::CtrlSkip, Value::CtrlSkip) => true,
            (Value::CtrlStop, Value::CtrlStop) => true,
            (Value::Collection(a), Value::Collection(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}
