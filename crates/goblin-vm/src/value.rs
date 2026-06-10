use std::rc::Rc;
use rust_decimal::Decimal;
use indexmap::IndexMap;

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

/// Format spec for the `Formatted` value variant.
/// Controls how numeric values are displayed.
#[derive(Clone, Debug, PartialEq)]
pub struct FormatSpec {
    pub decimals: u32,
    pub sep_thousands: Option<char>,
    pub sep_decimal: char,
}

/// The actual user-facing value that lives *inside* a stash.
/// This is what you mean when you say "the value of x is 10".
///
/// Mirrors the interpreter's Value enum exactly, except:
/// - Array, Map, MapOrd, Seq are unified into Collection(CollectionValue)
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Big(Decimal),
    Pct(f64),
    Char(char),
    Str(String),

    /// Formatted display wrapper.
    Formatted(Box<Value>, FormatSpec),

    /// Pair of two values — produced by the >< (divmod) operator.
    Pair(Box<Value>, Box<Value>),

    /// Unified collection value (arrays, maps, stacks, queues, sets, etc.).
    /// Replaces interpreter's Array, Map, MapOrd, Seq.
    Collection(Rc<CollectionValue>),

    // ----- Control flow values -----
    CtrlSkip,
    CtrlStop,
    CtrlReturn(Box<Value>),

    // ----- Object system -----
    /// A class instance. Fields stored by name.
    Object {
        class_name: String,
        fields: IndexMap<String, Value>,
        readonly_fields: std::collections::BTreeSet<String>,
        trait_fields: std::collections::BTreeSet<String>,
        uuid: String,
    },

    /// Handle into Session::object_store.
    Ref(String),

    /// Grid coordinate reference into a named GridWorld.
    GridRef {
        grid_id: String,
        x: i32,
        y: i32,
    },

    /// An enum variant value.
    Enum {
        enum_name: String,
        variant_name: String,
        fields: Option<IndexMap<String, Value>>,
    },

    /// A class descriptor (the class itself, not an instance).
    Class {
        name: String,
    },
}

/// One arena cell: stores a Value plus metadata.
#[derive(Debug)]
pub struct Stash {
    pub value: Rc<Value>,
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
    FlatArray(Rc<Vec<Value>>),

    /// Queue/stack/deque-optimized layout.
    RingBuf(Rc<RingBuf>),

    /// Chunked / rope-style layout for large, edit-heavy sequences.
    ChunkedSeq(Rc<ChunkedSeq>),

    /// Tiny maps stored as a Vec of (key, value) pairs.
    SmallMap(Rc<Vec<(String, Value)>>),

    /// General-purpose IndexMap backend for larger maps.
    IndexMapBackend(Rc<IndexMap<String, Value>>),
}

/// Ring buffer for queue/stack semantics.
#[derive(Debug, Clone)]
pub struct RingBuf {
    pub buf: Vec<Value>,
    pub head: usize,
    pub len: usize,
}

/// Very simple chunked sequence placeholder.
#[derive(Debug, Clone)]
pub struct ChunkedSeq {
    pub chunks: Vec<Vec<Value>>,
    pub len: usize,
}

/// Lightweight usage metadata to drive adaptive layout decisions.
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendHint {
    Auto,
    FlatArray,
    RingBuf,
    ChunkedSeq,
    SmallMap,
    IndexMapBackend,
}

impl Default for BackendHint {
    fn default() -> Self {
        BackendHint::Auto
    }
}

impl Value {
    /// Return a short string describing the kind of value.
    pub fn kind_str(&self) -> &'static str {
        match self {
            Value::Nil              => "nil",
            Value::Unit             => "unit",
            Value::Bool(_)          => "bool",
            Value::Int(_)           => "int",
            Value::Float(_)         => "float",
            Value::Big(_)           => "big",
            Value::Pct(_)           => "pct",
            Value::Char(_)          => "char",
            Value::Str(_)           => "str",
            Value::Formatted(_, _)  => "formatted",
            Value::Pair(_, _)       => "pair",
            Value::Collection(_)    => "collection",
            Value::CtrlSkip         => "ctrl:skip",
            Value::CtrlStop         => "ctrl:stop",
            Value::CtrlReturn(_)    => "ctrl:return",
            Value::Object { .. }    => "object",
            Value::Ref(_)           => "ref",
            Value::GridRef { .. }   => "gridref",
            Value::Enum { .. }      => "enum",
            Value::Class { .. }     => "class",
        }
    }
}
