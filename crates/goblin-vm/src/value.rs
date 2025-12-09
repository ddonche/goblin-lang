use std::collections::HashMap;
use std::rc::Rc;

/// Logical address of a stash in the arena.
/// `slot` is index into Session.arena.
/// `generation` lets us detect stale mem_addr results.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address {
    pub slot: u32,
    pub generation: u32,
}

/// A runtime tether: this is what lives in a VM slot (local/global/etc.).
/// It points to a stash via its Address.
///
/// Goblin-level picture:
/// name -> (slot) -> Tether -> Stash -> Value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tether {
    pub addr: Address,
}

/// The actual user-facing value that lives *inside* a stash.
/// This is what you mean when you say "the value of x is 10".
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),

    /// Unified collections (arrays, maps, stacks, queues, sets, etc.).
    ///
    /// Semantically: "a collection value" stored in this stash.
    /// Physically: an adaptive backend chosen by the VM.
    Collection(Rc<CollectionValue>),

    // Later:
    // Function(FunctionId),
    // Builtin(BuiltinId),
}

/// One arena cell: stores a Value plus metadata.
/// Stashes are immutable except via `overwrite!` (to be implemented later).
#[derive(Debug)]
pub struct Stash {
    pub value: Rc<Value>, // actual data
    pub tether_count: usize,
    pub generation: u32,
}

/// High-level collection value: semantic content + adaptive backend.
///
/// - `layout` is how the data is physically stored (FlatArray, RingBuf, etc.)
/// - `meta` tracks usage so the VM can pick better layouts for new stashes later.
#[derive(Debug, Clone)]
pub struct CollectionValue {
    pub layout: CollectionLayout,
    pub meta: CollectionMeta,
}

/// The concrete backend layout Goblin is using for this collection.
///
/// This is VM-internal; Goblin code just sees "a collection".
#[derive(Debug, Clone)]
pub enum CollectionLayout {
    /// Default for small/mid-sized sequential data.
    /// Backed by a contiguous Vec of Values.
    FlatArray(Rc<Vec<Value>>),

    /// Queue/stack/deque-optimized layout (front/back ops).
    RingBuf(Rc<RingBuf>),

    /// Chunked / rope-style layout for large, edit-heavy sequences.
    ChunkedSeq(Rc<ChunkedSeq>),

    /// Tiny maps stored as a small Vec of (key, value) pairs.
    SmallMap(Rc<Vec<(Value, Value)>>),

    /// General-purpose hash-map backend for larger maps / heavy lookups.
    HashMapBackend(Rc<HashMap<Value, Value>>),
}

/// Ring buffer for queue/stack semantics.
/// Minimal for now; can evolve later without changing the public model.
#[derive(Debug, Clone)]
pub struct RingBuf {
    pub buf: Vec<Value>,
    pub head: usize,
    pub len: usize,
}

/// Very simple chunked sequence placeholder.
/// v1 can be a Vec of chunks; later this can become a real rope/tree.
#[derive(Debug, Clone)]
pub struct ChunkedSeq {
    pub chunks: Vec<Vec<Value>>,
    pub len: usize,
}

/// Lightweight usage metadata to drive adaptive layout decisions.
///
/// All the CRUD + suffix ops (`*_first`, `*_last`, `*_at`, `*_random`,
/// `*_where`, `*_all`) will eventually bump these counters, but we don't
/// need the ops layer yet for this to compile and stay consistent.
#[derive(Debug, Clone, Default)]
pub struct CollectionMeta {
    /// Logical length of the collection (number of elements or entries).
    pub len: usize,

    /// How often this collection is hit at the front/back/middle positionally.
    /// All verbs (put/update/delete/reap) with *_first/_last/_at suffixes
    /// will eventually roll into these counters.
    pub front_hits: u32,   // *_first
    pub back_hits: u32,    // *_last
    pub mid_hits: u32,     // *_at

    /// Random and scan-y operations:
    /// - *_random
    /// - *_where, *_all
    pub random_hits: u32,  // *_random
    pub scan_hits: u32,    // *_where, *_all

    /// Optional hint when a layout has effectively "stabilized".
    pub backend_hint: BackendHint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendHint {
    /// Let the planner decide based purely on meta.
    Auto,
    /// Manual override / lock (debug/optimization escape hatches).
    FlatArray,
    RingBuf,
    ChunkedSeq,
    SmallMap,
    HashMapBackend,
}

impl Default for BackendHint {
    fn default() -> Self {
        BackendHint::Auto
    }
}
