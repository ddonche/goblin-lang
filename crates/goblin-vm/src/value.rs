use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

// ──────────────────────────────────────────────────────────────────────────────
// DateTime support
// ──────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GoblinDateKind { DateTime, Date, Time }

#[derive(Debug, Clone)]
pub struct GoblinDateTime {
    pub utc:  chrono::DateTime<chrono::Utc>,
    pub tz:   Option<String>,
    pub kind: GoblinDateKind,
}
impl PartialEq for GoblinDateTime {
    fn eq(&self, other: &Self) -> bool { self.utc == other.utc }
}
impl Eq for GoblinDateTime {}
impl std::hash::Hash for GoblinDateTime {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.utc.timestamp_nanos_opt().hash(state);
    }
}

/// Logical address of a stash in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address {
    pub slot: u32,
    pub generation: u32,
}

/// A runtime tether: lives in a VM slot (local / global / upvalue).
/// Points to a stash via its Address.
///
/// name → (slot) → Tether → Stash → Value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tether {
    pub addr: Address,
}

// ──────────────────────────────────────────────────────────────────────────────
// Format spec and Seq (interpreter-compatible)
// ──────────────────────────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq)]
pub struct FormatSpec {
    pub decimals: u32,
    pub sep_thousands: Option<char>,
    pub sep_decimal: char,
}

/// A simple seq type for the VM (wraps Vec<Value>).
#[derive(Clone, Debug, PartialEq)]
pub struct Seq {
    pub items: Vec<Value>,
}

impl Seq {
    pub fn from_vec(v: Vec<Value>) -> Self { Seq { items: v } }
    pub fn len(&self) -> usize { self.items.len() }
    pub fn as_slice(&self) -> Option<&[Value]> { Some(self.items.as_slice()) }
    pub fn to_vec(&self) -> Vec<Value> { self.items.clone() }
    pub fn is_empty(&self) -> bool { self.items.is_empty() }
}

/// The user-facing value stored inside a stash.
#[derive(Debug, Clone)]
pub enum Value {
    // ── Primitives ──────────────────────────────────────────────────────────
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

    // ── Structured ──────────────────────────────────────────────────────────
    Formatted(Box<Value>, FormatSpec),
    Array(Vec<Value>),
    Map(std::collections::BTreeMap<String, Value>),
    MapOrd(indexmap::IndexMap<String, Value>),
    Pair(Box<Value>, Box<Value>),
    Seq(Seq),

    // ── Control flow values ─────────────────────────────────────────────────
    CtrlSkip,
    CtrlStop,
    CtrlReturn(Box<Value>),

    // ── Object/type system ───────────────────────────────────────────────────
    Object {
        class_name: String,
        fields: Rc<indexmap::IndexMap<String, Value>>,
        readonly_fields: std::collections::BTreeSet<String>,
        trait_fields: std::collections::BTreeSet<String>,
        uuid: String,
    },
    Ref(String),
    GridRef { grid_id: String, x: i32, y: i32 },
    Enum {
        enum_name: String,
        variant_name: String,
        fields: Option<indexmap::IndexMap<String, Value>>,
    },
    Class { name: String },

    // ── Canonical VM adaptive collection type ───────────────────────────────
    /// Unified adaptive collection: FlatArray, RingBuf, ChunkedSeq, SmallMap, HashMapBackend.
    Collection(Rc<CollectionValue>),

    // ── VM-only ──────────────────────────────────────────────────────────────
    /// A compiled Goblin function (no captured upvalues).
    Function(Rc<FunctionObject>),

    /// A compiled Goblin function with captured upvalues.
    Closure(Rc<Closure>),

    /// A built-in native function.
    Builtin(BuiltinId),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil           => "nil",
            Value::Unit          => "unit",
            Value::Bool(_)       => "bool",
            Value::Int(_)        => "int",
            Value::Float(_)      => "float",
            Value::Big(_)        => "big",
            Value::Pct(_)        => "pct",
            Value::Char(_)       => "char",
            Value::Str(_)        => "str",
            Value::DateTime(_)   => "datetime",
            Value::Formatted(..) => "formatted",
            Value::Array(_)      => "array",
            Value::Map(_)        => "map",
            Value::MapOrd(_)     => "map",
            Value::Pair(..)      => "pair",
            Value::Seq(_)        => "seq",
            Value::CtrlSkip      => "ctrl_skip",
            Value::CtrlStop      => "ctrl_stop",
            Value::CtrlReturn(_) => "ctrl_return",
            Value::Object { .. } => "object",
            Value::Ref(_)        => "ref",
            Value::GridRef { .. } => "grid_ref",
            Value::Enum { .. }   => "enum",
            Value::Class { .. }  => "class",
            // Collections are the VM's representation of array/map literals;
            // user-visible type names match the interpreter.
            Value::Collection(c) => if c.is_map() { "map" } else { "array" },
            Value::Function(_)   => "function",
            Value::Closure(_)    => "closure",
            Value::Builtin(_)    => "builtin",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil           => false,
            Value::Unit          => false,
            Value::Bool(b)       => *b,
            Value::Int(n)        => *n != 0,
            Value::Float(f)      => *f != 0.0,
            Value::Big(d)        => !d.is_zero(),
            Value::Pct(p)        => *p != 0.0,
            Value::Char(c)       => *c != '\0',
            Value::Str(s)        => !s.is_empty(),
            Value::DateTime(_)   => true,
            Value::Formatted(v, _) => v.is_truthy(),
            Value::Array(a)      => !a.is_empty(),
            Value::Map(m)        => !m.is_empty(),
            Value::MapOrd(m)     => !m.is_empty(),
            Value::Pair(..)      => true,
            Value::Seq(s)        => !s.is_empty(),
            Value::CtrlSkip      => false,
            Value::CtrlStop      => false,
            Value::CtrlReturn(_) => false,
            Value::Object { .. } => true,
            Value::Ref(_)        => true,
            Value::GridRef { .. } => true,
            Value::Enum { .. }   => true,
            Value::Class { .. }  => true,
            Value::Collection(c) => c.meta.len > 0,
            Value::Function(_)   => true,
            Value::Closure(_)    => true,
            Value::Builtin(_)    => true,
        }
    }
}

// Value needs PartialEq + Eq + Hash for use as map keys in SmallMap / HashMapBackend.
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil,              Value::Nil)              => true,
            (Value::Unit,             Value::Unit)             => true,
            (Value::Bool(a),          Value::Bool(b))          => a == b,
            (Value::Int(a),           Value::Int(b))           => a == b,
            (Value::Float(a),         Value::Float(b))         => a.to_bits() == b.to_bits(),
            (Value::Big(a),           Value::Big(b))           => a == b,
            (Value::Pct(a),           Value::Pct(b))           => a.to_bits() == b.to_bits(),
            (Value::Char(a),          Value::Char(b))          => a == b,
            (Value::Str(a),           Value::Str(b))           => a == b,
            (Value::DateTime(a),      Value::DateTime(b))      => a == b,
            (Value::Array(a),         Value::Array(b))         => a == b,
            (Value::Map(a),           Value::Map(b))           => a == b,
            (Value::Pair(ak, av),     Value::Pair(bk, bv))     => ak == bk && av == bv,
            (Value::CtrlSkip,         Value::CtrlSkip)         => true,
            (Value::CtrlStop,         Value::CtrlStop)         => true,
            (Value::CtrlReturn(a),    Value::CtrlReturn(b))    => a == b,
            (Value::Ref(a),           Value::Ref(b))           => a == b,
            (Value::GridRef { grid_id: ga, x: xa, y: ya },
             Value::GridRef { grid_id: gb, x: xb, y: yb }) => ga == gb && xa == xb && ya == yb,
            (Value::Collection(a),    Value::Collection(b)) if Rc::ptr_eq(a, b) => true,
            // Containers compare structurally, whatever their representation
            // (legacy Array/Map/MapOrd/Seq or an Rc-backed Collection).
            (a, b) if a.is_container() && b.is_container() => containers_eq(a, b),
            (Value::Function(a),      Value::Function(b))      => Rc::ptr_eq(a, b),
            (Value::Closure(a),       Value::Closure(b))       => Rc::ptr_eq(a, b),
            (Value::Builtin(a),       Value::Builtin(b))       => a == b,
            (Value::Enum { enum_name: en_a, variant_name: vn_a, .. },
             Value::Enum { enum_name: en_b, variant_name: vn_b, .. }) => en_a == en_b && vn_a == vn_b,
            _ => false,
        }
    }
}
impl Eq for Value {}

/// Key text used to compare map keys across representations (legacy maps have
/// String keys, map collections have Value keys).
pub fn map_key_text(k: &Value) -> String {
    match k {
        Value::Str(s)   => s.clone(),
        Value::Int(n)   => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b)  => b.to_string(),
        Value::Char(c)  => c.to_string(),
        Value::Nil      => "nil".into(),
        other           => format!("{:?}", other),
    }
}

fn containers_eq(a: &Value, b: &Value) -> bool {
    if a.is_seq_like() && b.is_seq_like() {
        let (Some(x), Some(y)) = (a.seq_items(), b.seq_items()) else { return false };
        return x.len() == y.len() && x.iter().zip(y.iter()).all(|(p, q)| p == q);
    }
    if a.is_map_like() && b.is_map_like() {
        if a.container_len() != b.container_len() { return false; }
        let Some(entries) = a.map_entries() else { return false };
        return entries.iter().all(|(k, v)| b.map_lookup(k).map_or(false, |w| &w == v));
    }
    false
}

// ── Representation-independent views of array-like and map-like values ─────

impl Value {
    /// Array-like: legacy Array, Seq, or a non-map Collection.
    pub fn is_seq_like(&self) -> bool {
        match self {
            Value::Array(_) | Value::Seq(_) => true,
            Value::Collection(c) => !c.is_map(),
            _ => false,
        }
    }

    /// Map-like: legacy Map / MapOrd, or a map Collection.
    pub fn is_map_like(&self) -> bool {
        match self {
            Value::Map(_) | Value::MapOrd(_) => true,
            Value::Collection(c) => c.is_map(),
            _ => false,
        }
    }

    pub fn is_container(&self) -> bool { self.is_seq_like() || self.is_map_like() }

    /// Element count of an array-like or map-like value (0 otherwise). O(1).
    pub fn container_len(&self) -> usize {
        match self {
            Value::Array(xs)     => xs.len(),
            Value::Seq(s)        => s.len(),
            Value::Map(m)        => m.len(),
            Value::MapOrd(m)     => m.len(),
            Value::Collection(c) => c.len(),
            _ => 0,
        }
    }

    /// The elements of an array-like value; borrowed when the backing store is
    /// already contiguous (Array, Seq, FlatArray), so no copy on the hot path.
    pub fn seq_items(&self) -> Option<std::borrow::Cow<'_, [Value]>> {
        use std::borrow::Cow;
        match self {
            Value::Array(xs) => Some(Cow::Borrowed(xs.as_slice())),
            Value::Seq(s)    => Some(Cow::Borrowed(s.items.as_slice())),
            Value::Collection(c) => match &c.layout {
                CollectionLayout::FlatArray(v)   => Some(Cow::Borrowed(v.as_slice())),
                CollectionLayout::RingBuf(r)     => Some(Cow::Owned(r.to_vec())),
                CollectionLayout::ChunkedSeq(cs) => Some(Cow::Owned(cs.to_flat())),
                _ => None,
            },
            _ => None,
        }
    }

    /// The entries of a map-like value as (key text, value), in the map's own
    /// order (insertion order for MapOrd / SmallMap, sorted for Map).
    pub fn map_entries(&self) -> Option<Vec<(String, Value)>> {
        match self {
            Value::Map(m)    => Some(m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()),
            Value::MapOrd(m) => Some(m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()),
            Value::Collection(c) => match &c.layout {
                CollectionLayout::SmallMap(p) =>
                    Some(p.iter().map(|(k, v)| (map_key_text(k), v.clone())).collect()),
                CollectionLayout::HashMapBackend(m) =>
                    Some(m.iter().map(|(k, v)| (map_key_text(k), v.clone())).collect()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Look up a key (by its text) in a map-like value.
    pub fn map_lookup(&self, key: &str) -> Option<Value> {
        match self {
            Value::Map(m)    => m.get(key).cloned(),
            Value::MapOrd(m) => m.get(key).cloned(),
            Value::Collection(c) => match &c.layout {
                CollectionLayout::SmallMap(p) =>
                    p.iter().find(|(k, _)| map_key_text(k) == key).map(|(_, v)| v.clone()),
                CollectionLayout::HashMapBackend(m) => m.get(&Value::Str(key.to_string())).cloned()
                    .or_else(|| m.iter().find(|(k, _)| map_key_text(k) == key).map(|(_, v)| v.clone())),
                _ => None,
            },
            _ => None,
        }
    }

    /// A map-like value as an insertion-ordered legacy map (for builtins that
    /// take option/config maps).
    pub fn to_index_map(&self) -> Option<indexmap::IndexMap<String, Value>> {
        self.map_entries().map(|e| e.into_iter().collect())
    }

    /// A map-like value as a sorted legacy map.
    pub fn to_btree_map(&self) -> Option<std::collections::BTreeMap<String, Value>> {
        self.map_entries().map(|e| e.into_iter().collect())
    }

    /// Rewrite a Collection into its legacy equivalent (Array / MapOrd); other
    /// values pass through. For builtins that only understand legacy shapes.
    pub fn into_legacy(self) -> Value {
        match &self {
            Value::Collection(c) if c.is_map() => Value::MapOrd(self.to_index_map().unwrap_or_default()),
            Value::Collection(_) => Value::Array(self.seq_items().map(|x| x.into_owned()).unwrap_or_default()),
            _ => self,
        }
    }
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Containers are equal across representations (see PartialEq), so they
        // hash by kind and length only, never by variant or pointer.
        if self.is_seq_like() { 0xA5u8.hash(state); self.container_len().hash(state); return; }
        if self.is_map_like() { 0x5Au8.hash(state); self.container_len().hash(state); return; }
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Nil           => {}
            Value::Unit          => {}
            Value::Bool(b)       => b.hash(state),
            Value::Int(n)        => n.hash(state),
            Value::Float(f)      => f.to_bits().hash(state),
            Value::Big(d)        => d.hash(state),
            Value::Pct(p)        => p.to_bits().hash(state),
            Value::Char(c)       => c.hash(state),
            Value::Str(s)        => s.hash(state),
            Value::DateTime(dt)  => dt.hash(state),
            Value::Formatted(v, _) => v.hash(state),
            Value::Array(a)      => { for x in a { x.hash(state); } }
            Value::Map(m)        => { for (k, v) in m { k.hash(state); v.hash(state); } }
            Value::MapOrd(m)     => { for (k, v) in m { k.hash(state); v.hash(state); } }
            Value::Pair(k, v)    => { k.hash(state); v.hash(state); }
            Value::Seq(s)        => { for x in &s.items { x.hash(state); } }
            Value::CtrlSkip      => {}
            Value::CtrlStop      => {}
            Value::CtrlReturn(v) => v.hash(state),
            Value::Object { uuid, .. } => uuid.hash(state),
            Value::Ref(s)        => s.hash(state),
            Value::GridRef { grid_id, x, y } => { grid_id.hash(state); x.hash(state); y.hash(state); }
            Value::Enum { enum_name, variant_name, .. } => {
                enum_name.hash(state);
                variant_name.hash(state);
            }
            Value::Class { name } => name.hash(state),
            Value::Collection(c) => (Rc::as_ptr(c) as usize).hash(state),
            Value::Function(f)   => (Rc::as_ptr(f) as usize).hash(state),
            Value::Closure(c)    => (Rc::as_ptr(c) as usize).hash(state),
            Value::Builtin(b)    => b.hash(state),
        }
    }
}

// ──────────────────────────────────────────────────────────────────────────────
// Stash
// ──────────────────────────────────────────────────────────────────────────────

/// One arena cell: stores a Value plus metadata.
#[derive(Debug)]
pub struct Stash {
    pub value: Value,
    pub tether_count: usize,
    pub generation: u32,
}

// ──────────────────────────────────────────────────────────────────────────────
// FunctionObject and Closure
// ──────────────────────────────────────────────────────────────────────────────

/// Describes how one upvalue is sourced when a closure is created.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpvalueDescriptor {
    /// Capture the Tether currently in the enclosing frame's locals[slot].
    Local(u8),
    /// Forward upvalue[idx] from the enclosing closure.
    Upvalue(u8),
}

/// A compiled Goblin function produced by the bytecode compiler.
#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub bytecode: Vec<crate::opcode::Opcode>,
    pub constants: Vec<Value>,
    /// Total number of local slots (parameters + other locals).
    pub locals: usize,
    /// Number of parameter slots (always the first `params` locals).
    pub params: usize,
    /// Arguments a caller must pass (the rest have defaults).
    pub required_params: usize,
    pub name: String,
    /// How to populate upvalues when this function is wrapped in a Closure.
    pub upvalue_descriptors: Vec<UpvalueDescriptor>,
    /// Source line number for each bytecode instruction (parallel to bytecode).
    pub line_numbers: Vec<u32>,
    /// Name for each local slot (slot index → name), for string interpolation.
    pub local_names: Vec<String>,
    /// The GLAM namespace this function was defined in (top-level actions loaded
    /// via `use <namespace>` only), used by `:need()` to resolve action needs.
    pub owner_glam: Option<String>,
    /// Source file this function was compiled from (empty = unknown).
    pub source_file: String,
    /// Global slot names for this function's compilation unit — parallel to session.globals.
    /// Used by string interpolation to resolve {varname} by name within the right module scope.
    /// Shared by every function of one compilation unit and filled in once the
    /// unit has finished compiling (the list only ever grows, so later entries
    /// keep the indices they would have had in an earlier snapshot).
    pub global_names: std::rc::Rc<std::cell::OnceCell<Vec<String>>>,
}

impl FunctionObject {
    /// Global slot names of this function's compilation unit.
    pub fn global_names(&self) -> &[String] {
        self.global_names.get().map(|v| v.as_slice()).unwrap_or(&[])
    }
}

/// A compiled module: the entry function plus class/enum metadata collected
/// at compile time so the VM can pre-register them before execution.
pub struct CompiledModule {
    pub entry: FunctionObject,
    pub classes: Vec<goblin_ast::ClassDecl>,
    pub enums: Vec<goblin_ast::EnumDecl>,
    /// Names for each global slot (slot index → name), for string interpolation.
    pub global_names: Vec<String>,
}

/// An upvalue cell: a shared, heap-allocated slot that can be closed over.
///
/// - While the enclosing frame is alive: `Open` — points to a stack slot
///   (we snapshot the Tether at closure creation time for v1 simplicity).
/// - After closure creation: `Closed` — holds the captured Tether directly.
///
/// v1 Note: We use snapshot semantics (each closure gets its own copy of the
/// captured tether at the time of MakeClosure). Shared mutable upvalues
/// (where inner and outer both see mutations) require full open/close upvalue
/// cells; that is a planned future improvement.
#[derive(Debug, Clone)]
pub struct UpvalueCell(pub Rc<RefCell<Tether>>);

impl UpvalueCell {
    pub fn new(t: Tether) -> Self {
        UpvalueCell(Rc::new(RefCell::new(t)))
    }
    pub fn get(&self) -> Tether {
        self.0.borrow().clone()
    }
    pub fn set(&self, t: Tether) {
        *self.0.borrow_mut() = t;
    }
}

/// A function together with its captured upvalue cells.
#[derive(Debug, Clone)]
pub struct Closure {
    pub func: Rc<FunctionObject>,
    pub upvalues: Vec<UpvalueCell>,
}

// ──────────────────────────────────────────────────────────────────────────────
// BuiltinId
// ──────────────────────────────────────────────────────────────────────────────

/// Numeric IDs for all built-in functions dispatched by CallBuiltin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    // Memory introspection
    MemId,
    MemAddr,
    MemTotal,
    MemHuman,
    Gc,
    GcMode,
    StashCount,
    TetherCount,

    // Arithmetic helpers (called as functions)
    Abs,
    Min,
    Max,
    Avg,
    Sum,
    Floor,
    Ceil,
    Round,
    Sqrt,
    Clamp,
    Pow,

    // String builtins (new interpreter-aligned)
    Lower,
    Upper,
    Title,
    Slug,
    Mixed,
    Raw,
    Trim,
    TrimLead,
    TrimTrail,
    Find,
    FindAll,
    Ord,

    // String ops
    Len,
    ToString,
    Split,
    Join,
    StartsWith,
    EndsWith,
    Before,
    After,
    BeforeLast,
    AfterLast,
    KeepBefore,
    KeepAfter,
    KeepBetween,
    SanitizeBom,
    NormalizeNewlines,
    IgnoreWhere,
    IgnoreLinesWhere,
    IgnoreMatching,
    IgnoreLinesMatching,
    IsMatching,
    CountMatching,
    KeepMatching,
    JsonParse,
    JsonStringify,
    JsonStringifyPretty,
    IgnoreBetween,
    IgnoreBlocks,
    Env,

    // Maps
    Keys,
    Values,
    Items,

    // Collections (new interpreter-aligned)
    Has,
    Count,
    Shuffle,
    Sort,
    Freq,
    Mode,
    SampleWeighted,
    Map,
    Unique,
    Dups,

    // Collections — put family (legacy)
    Put,
    PutFirst,
    PutLast,
    PutAt,

    // Collections — update family (legacy)
    Update,
    UpdateFirst,
    UpdateLast,
    UpdateAt,

    // Collections — delete family (legacy)
    Delete,
    DeleteFirst,
    DeleteLast,
    DeleteAt,
    DeleteWhere,
    DeleteAll,

    // Collections — reap family (legacy)
    Reap,
    ReapFirst,
    ReapLast,
    ReapAt,
    ReapWhere,

    // Collections — new Position×Operation matrix (interpreter-aligned)
    // Get family
    GetFirst, GetLast, GetAt, GetWhere, GetAll, GetMatching, GetBetween, GetRandom,
    // Put family (new)
     PutMatching, PutBetween, PutRandom,
    // Update family (new)
    UpdateAll, UpdateWhere, UpdateMatching, UpdateBetween, UpdateRandom,
    // Delete family (new)
    DeleteMatching, DeleteBetween, DeleteRandom,
    // Reap family (new — avoid name collision with legacy ReapFirst etc.)
    ReapFirst2, ReapLast2, ReapAt2, ReapWhere2, ReapMatching, ReapBetween,

    // Collections — query (legacy)
    
    IsEmpty,
    Reverse,
    ReverseChars,
    Minimize,
    ParseBool,
    SortBy,
    Filter,
    Reduce,
    Any,
    All,
    Slice,

    // I/O
    
    Println,

    // Type checks
    IsNil,
    IsBool,
    IsInt,
    IsFloat,
    IsStr,
    IsArray,
    IsMap,
    IsFunction,
    IsBig,
    IsPct,
    IsNum,
    IsChar,
    IsPair,
    IsSeq,
    IsUnit,
    IsAlnum,
    IsAlpha,
    IsDigit,
    IsWhitespace,
    IsEven,
    IsOdd,
    IsMultipleOf,
    IsPositive,
    IsNegative,
    IsNix,

    // Conversions
    ToInt,
    ToFloat,
    ToStr,
    ToBool,

    // Meta
    TypeOf,
    Panic,

    // Range
    Range,

    // Process
    RunCmd,
    Roll,
    RollDetail,
    RandSeed,

    // Request (HTTP context)
    ReqMethod,
    ReqPath,
    ReqQuery,
    ReqBody,
    ReqHeader,
    Cookie,

    // Response
    SetStatus,
    SetHeader,
    SetCookie,

    // CSPRNG
    SecurePick,
    SecureRandom,
    SecureShuffle,

    // Pack/unpack
    Pack,
    Unpack,

    // Map higher-order
    MapFn,
    FilterFn,
    ReduceFn,
    ForEachFn,
    ToForIter,
    RepeatPrep,
    ApiEcho,
    StrictEq,

    // String extras
    Lines,
    Words,
    Chars,
    Format,

    // Missing builtins
    Pct,
    Between,
    IsControl,
    IgnoreBlocksFirst,
    Pick,
    ReadJson,
    WriteText,
    AppendFile,
    WriteJson,
    ReapSample,
    ReapBang,
    IsType,
    IsBoundName,
    Invoke,
    Summon,
    Provoke,
    Need,
    YallParse,
    YallParseFile,
    YallWrite,
    YallWriteFile,
    YallPretty,
    YallMinify,
    CreateDir,
    CopyFile,
    DeletePath,
    MdToHtml,
    HighlightCode,
    ToBig,
    ToMap,
    ReadText,
    CastI8,
    CastI16,
    CastI32,
    CastI64,
    CastU8,
    CastU16,
    CastU32,
    CastU64,
    CastF32,
    CastF64,

    // Filesystem / path
    FileExists,
    IsFile,
    IsDir,
    Basename,
    Dirname,
    Stem,
    Ext,
    PathJoin,
    PathSplit,
    PathNormalize,
    PathFixSeparators,
    PathRelativeTo,
    Walk,
    ListDirs,
    EscapeHtml,
    UrlDecode,
    UrlEncode,
    UuidV4,
    UuidV7,
    Pathfind,

    // Interactive input
    AskInput,

    // Dice string parsing
    RollStr,
    RollDetailStr,

    // Type/format builtins
    ValType,
    FormatInfo,
    ClearFormat,
    Backend,
    Metrics,

    // Process
    ZipDir,

    // Sentinel: emitted for non-bang calls to I/O-mutation builtins.
    // Always errors at runtime with "mutation-operator-required".
    RequiresBang,

    // Token store
    RegisterToken,
    ResolveToken,
    ClearToken,
    ClearTokens,
    ClearAllTokens,
    ListTokens,

    // DES / overlay builtins
    DecisionDebug,
    OverlaysOf,
    OverlayStrength,
    LinkScore,
    OwnedBy,
    OwnsTree,
    CloneObject,
    DeleteObject,
    DeleteOverlaysOn,

    // Date/time — full implementation
    DtNow,
    DtEpochMs,
    DtEpochS,
    DtUtcNow,
    DtLocalNow,
    DtToday,
    DtTomorrow,
    DtYesterday,
    DtFromEpochMs,
    DtToIso,
    DtFromIso,
    DtToEpochMs,
    DtFormatDatetime,
    DtFormatDate,
    DtFormatTime,
    DtYear,
    DtMonth,
    DtDay,
    DtHour,
    DtMinute,
    DtSecond,
    DtWeekday,
    DtAddDuration,
    DtSince,
    DtUntil,
    DtTimezone,
    DtToTimezone,

    // Date/time constructors (now fully implemented)
    CastDate,
    CastTime,
    CastDatetime,
    CastDuration,

    // Object/overlay query builtins
    Objects,
    Overlays,
    // Query all objects of a given class/overlay by name string (for `repeat ClassName`)
    QueryByIdent,

    // DES tick
    Tick,

    // Grid
    Grid,
    GridGet,
    GridSet,
    GridVoid,
    GridTileGet,
    GridTileSet,
    GridRegionGet,
    GridRegionSet,
    GridDefaultGet,
    GridDefaultSet,
    GridNeighbors,
    GridOccupied,
    GridUnoccupied,
    GridOccupiedCount,
    GridUnoccupiedCount,
    GridCount,
    GridOccupiedBy,
    GridHas,
    GridInfo,
    GridTileInfo,
    GridRegionInfo,

    // Compiler-synthesized builtins for AST nodes
    // slice expr: (recv, start_or_nil, end_or_nil) → array/str
    SliceExpr,
    // slice3 expr: (recv, start_or_nil, end_or_nil, step_or_nil) → array/str
    Slice3Expr,
    // grid[x,y] expr: (grid_str_or_ref, x, y) → GridRef
    Index2Expr,
    // EnumVariant expr: (enum_name_str, variant_name_str, fields_map_or_nil) → Enum
    EnumVariantExpr,
    // LiteralToken expr: (module_str, ident_str) → Value from token store
    LiteralTokenExpr,
    // BoxVar expr: (namespace_str, name_str) → Value from box_store
    BoxVarExpr,
    // BoxBind expr: (namespace_str, name_str, value) → stores in box_store, returns Nil
    BoxBindExpr,

    // Missing builtins
    Tokenize,
    Get,

    // Outbound HTTP
    HttpGet,
    HttpPost,
    HttpPut,
    HttpDelete,
    HttpRequest,

    // Postgres (shared goblin-db crate, same as the interpreter)
    DbQuery,
    DbQueryOne,
    DbExec,

    // Render mode
    RenderTemplate,
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

impl CollectionValue {
    pub fn empty_array() -> Self {
        CollectionValue {
            layout: CollectionLayout::FlatArray(Rc::new(Vec::new())),
            meta: CollectionMeta::default(),
        }
    }

    pub fn from_flat(items: Vec<Value>) -> Self {
        let len = items.len();
        CollectionValue {
            layout: CollectionLayout::FlatArray(Rc::new(items)),
            meta: CollectionMeta { len, ..Default::default() },
        }
    }

    pub fn from_map(pairs: Vec<(Value, Value)>) -> Self {
        let len = pairs.len();
        CollectionValue {
            layout: CollectionLayout::SmallMap(Rc::new(pairs)),
            meta: CollectionMeta { len, ..Default::default() },
        }
    }

    /// Logical length of this collection.
    pub fn len(&self) -> usize {
        self.meta.len
    }

    pub fn is_empty(&self) -> bool {
        self.meta.len == 0
    }

    /// True for the map layouts (SmallMap / HashMapBackend).
    pub fn is_map(&self) -> bool {
        matches!(self.layout, CollectionLayout::SmallMap(_) | CollectionLayout::HashMapBackend(_))
    }
}

/// The concrete backend layout Goblin uses for this collection.
#[derive(Debug, Clone)]
pub enum CollectionLayout {
    FlatArray(Rc<Vec<Value>>),
    RingBuf(Rc<RingBuf>),
    ChunkedSeq(Rc<ChunkedSeq>),
    SmallMap(Rc<Vec<(Value, Value)>>),
    HashMapBackend(Rc<indexmap::IndexMap<Value, Value>>),
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

    pub fn from_vec(v: Vec<Value>) -> Self {
        let len = v.len();
        RingBuf { buf: v, head: 0, len }
    }

    /// Copies the elements, in order, into a buffer twice the size (padded
    /// with nil so `buf.len()` is the capacity), with `extra` slots free.
    fn grow(&mut self) -> Vec<Value> {
        let new_cap = (self.buf.len() * 2).max(4);
        let mut new_buf: Vec<Value> = Vec::with_capacity(new_cap);
        let cap = self.buf.len().max(1);
        for i in 0..self.len {
            let src = (self.head + i) % cap;
            new_buf.push(std::mem::replace(&mut self.buf[src], Value::Nil));
        }
        new_buf
    }

    pub fn push_back(&mut self, v: Value) {
        if self.len == self.buf.len() {
            // `buf.len()` is the capacity: padding the new buffer keeps the
            // next pushes from regrowing (and copying) every time.
            let mut new_buf = self.grow();
            let new_cap = new_buf.capacity().max(self.len + 1);
            new_buf.push(v);
            new_buf.resize(new_cap, Value::Nil);
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
            let mut items = self.grow();
            let new_cap = items.capacity().max(self.len + 1);
            items.insert(0, v);
            items.resize(new_cap, Value::Nil);
            self.buf = items;
            self.head = 0;
            self.len += 1;
        } else {
            self.head = if self.head == 0 { self.buf.len() - 1 } else { self.head - 1 };
            self.buf[self.head] = v;
            self.len += 1;
        }
    }

    pub fn pop_front(&mut self) -> Option<Value> {
        if self.len == 0 { return None; }
        let v = self.buf[self.head].clone();
        self.head = (self.head + 1) % self.buf.len().max(1);
        self.len -= 1;
        Some(v)
    }

    pub fn pop_back(&mut self) -> Option<Value> {
        if self.len == 0 { return None; }
        self.len -= 1;
        let tail = (self.head + self.len) % self.buf.len().max(1);
        Some(self.buf[tail].clone())
    }

    pub fn get(&self, i: usize) -> Option<&Value> {
        if i >= self.len || self.buf.is_empty() { return None; }
        Some(&self.buf[(self.head + i) % self.buf.len()])
    }

    pub fn get_mut(&mut self, i: usize) -> Option<&mut Value> {
        if i >= self.len || self.buf.is_empty() { return None; }
        let cap = self.buf.len();
        Some(&mut self.buf[(self.head + i) % cap])
    }

    pub fn to_vec(&self) -> Vec<Value> {
        (0..self.len).filter_map(|i| self.get(i).cloned()).collect()
    }
}

impl Default for RingBuf {
    fn default() -> Self { RingBuf::new() }
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
        let mut rem = idx;
        for chunk in &self.chunks {
            if rem < chunk.len() {
                return Some(&chunk[rem]);
            }
            rem -= chunk.len();
        }
        None
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut Value> {
        let mut rem = idx;
        for chunk in &mut self.chunks {
            if rem < chunk.len() {
                return Some(&mut chunk[rem]);
            }
            rem -= chunk.len();
        }
        None
    }

    pub fn set(&self, idx: usize, val: Value) -> Option<Self> {
        let mut new_chunks = self.chunks.clone();
        let mut rem = idx;
        for chunk in &mut new_chunks {
            if rem < chunk.len() {
                chunk[rem] = val;
                return Some(ChunkedSeq { chunks: new_chunks, len: self.len });
            }
            rem -= chunk.len();
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
        if self.len > 256 && mid > front_back {
            return BackendHint::ChunkedSeq;
        }
        if front_back > mid && front_back > 4 {
            return BackendHint::RingBuf;
        }
        BackendHint::Auto
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
// Numeric binary operators shared by the VM's generic arithmetic opcodes
// ──────────────────────────────────────────────────────────────────────────────

/// `+ - * %` on int / float / big operands. Ints that overflow promote to big
/// (exactly); any operation with a big is done in exact decimal arithmetic and
/// yields a big (spec: "any with big -> big"); a float with an int yields a
/// float. Returns None for operand kinds this does not cover (pct, strings…),
/// which the caller handles itself.
pub fn numeric_binop(op: &str, a: &Value, b: &Value) -> Option<Result<Value, crate::error::GoblinError>> {
    use crate::error::GoblinError;
    use rust_decimal::Decimal;
    use rust_decimal::prelude::{FromPrimitive, ToPrimitive};

    fn to_dec(v: &Value) -> Option<Decimal> {
        match v {
            Value::Big(d)   => Some(*d),
            Value::Int(n)   => Some(Decimal::from(*n)),
            Value::Float(f) => Decimal::from_f64(*f),
            _ => None,
        }
    }
    fn to_f64(v: &Value) -> Option<f64> {
        match v {
            Value::Int(n)   => Some(*n as f64),
            Value::Float(f) => Some(*f),
            Value::Big(d)   => d.to_f64(),
            _ => None,
        }
    }
    let float_op = |x: f64, y: f64| -> Value {
        Value::Float(match op { "add" => x + y, "sub" => x - y, "mul" => x * y, _ => x % y })
    };

    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            let r = match op {
                "add" => x.checked_add(*y),
                "sub" => x.checked_sub(*y),
                "mul" => x.checked_mul(*y),
                _ => {
                    if *y == 0 { return Some(Err(GoblinError::DivisionByZero)); }
                    x.checked_rem(*y)
                }
            };
            Some(Ok(match r {
                Some(n) => Value::Int(n),
                // Overflow: redo exactly in decimal (falls back to float only
                // beyond the decimal range).
                None => big_op(op, Decimal::from(*x), Decimal::from(*y))
                    .map(Value::Big)
                    .unwrap_or_else(|| float_op(*x as f64, *y as f64)),
            }))
        }
        (Value::Big(_), Value::Int(_) | Value::Big(_) | Value::Float(_))
        | (Value::Int(_) | Value::Float(_), Value::Big(_)) => {
            let (x, y) = (to_dec(a)?, to_dec(b)?);
            if op == "rem" && y.is_zero() { return Some(Err(GoblinError::DivisionByZero)); }
            Some(Ok(big_op(op, x, y).map(Value::Big)
                .unwrap_or_else(|| float_op(to_f64(a).unwrap_or(f64::NAN), to_f64(b).unwrap_or(f64::NAN)))))
        }
        (Value::Float(_), Value::Float(_) | Value::Int(_)) | (Value::Int(_), Value::Float(_)) => {
            Some(Ok(float_op(to_f64(a)?, to_f64(b)?)))
        }
        _ => None,
    }
}

fn big_op(op: &str, x: rust_decimal::Decimal, y: rust_decimal::Decimal) -> Option<rust_decimal::Decimal> {
    match op {
        "add" => x.checked_add(y),
        "sub" => x.checked_sub(y),
        "mul" => x.checked_mul(y),
        _     => x.checked_rem(y),
    }
}
