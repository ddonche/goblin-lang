use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

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

    // ── Legacy VM collection type (kept for backward compat) ─────────────────
    /// Unified collections (arrays, maps, stacks, queues).
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
            Value::Collection(_) => "collection",
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
            (Value::Array(a),         Value::Array(b))         => a == b,
            (Value::Map(a),           Value::Map(b))           => a == b,
            (Value::MapOrd(a),        Value::MapOrd(b))        => a == b,
            (Value::Pair(ak, av),     Value::Pair(bk, bv))     => ak == bk && av == bv,
            (Value::Seq(a),           Value::Seq(b))           => a == b,
            (Value::CtrlSkip,         Value::CtrlSkip)         => true,
            (Value::CtrlStop,         Value::CtrlStop)         => true,
            (Value::CtrlReturn(a),    Value::CtrlReturn(b))    => a == b,
            (Value::Ref(a),           Value::Ref(b))           => a == b,
            (Value::Collection(a),    Value::Collection(b))    => Rc::ptr_eq(a, b),
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

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
    pub value: Rc<Value>,
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
    ToUpperCase,
    ToLowerCase,
    Split,
    Join,
    Contains,
    StartsWith,
    EndsWith,
    Replace,
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

    // Collections — grab family (legacy)
    Grab,
    GrabFirst,
    GrabLast,
    GrabAt,
    GrabRandom,
    GrabWhere,
    GrabAll,
    GrabBetween,
    GrabMatching,

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
    ReapRandom,
    ReapWhere,
    ReapAll,

    // Collections — new Position×Operation matrix (interpreter-aligned)
    // Get family
    GetFirst, GetLast, GetAt, GetWhere, GetAll, GetMatching, GetBetween, GetRandom,
    // Put family (new)
    PutWhere, PutMatching, PutBetween, PutRandom, PutAll,
    // Update family (new)
    UpdateAll, UpdateWhere, UpdateMatching, UpdateBetween, UpdateRandom,
    // Delete family (new)
    DeleteMatching, DeleteBetween, DeleteRandom,
    // Reap family (new — avoid name collision with legacy ReapFirst etc.)
    ReapFirst2, ReapLast2, ReapAt2, ReapWhere2, ReapMatching, ReapBetween, ReapRandom2,

    // Collections — query (legacy)
    Pairs,
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
    FindIndex,
    Zip,
    Flatten,
    Slice,

    // I/O
    Print,
    Println,
    Eprint,
    Eprintln,

    // Type checks
    IsNil,
    IsBool,
    IsInt,
    IsFloat,
    IsStr,
    IsArray,
    IsMap,
    IsCollection,
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
    Assert,
    Panic,

    // Range
    Range,

    // Lorem ipsum
    Ipsum,
    IpsumSentences,
    IpsumParagraphs,
    IpsumFull,

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

    // String extras
    Lines,
    Words,
    Chars,
    Format,
    Pad,
    PadLeft,
    PadRight,
    Repeat,

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

    ArrayPush,
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
    PathRelativeTo,
    Walk,
    ListDirs,
    EscapeHtml,
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

    // Date/time type locks (not yet implemented — match interpreter error)
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
}

/// The concrete backend layout Goblin uses for this collection.
#[derive(Debug, Clone)]
pub enum CollectionLayout {
    FlatArray(Rc<Vec<Value>>),
    RingBuf(Rc<RingBuf>),
    ChunkedSeq(Rc<ChunkedSeq>),
    SmallMap(Rc<Vec<(Value, Value)>>),
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

    pub fn from_vec(v: Vec<Value>) -> Self {
        let len = v.len();
        RingBuf { buf: v, head: 0, len }
    }

    pub fn push_back(&mut self, v: Value) {
        if self.len == self.buf.len() {
            let new_cap = (self.buf.len() * 2).max(4);
            let mut new_buf: Vec<Value> = Vec::with_capacity(new_cap);
            for i in 0..self.len {
                let src = (self.head + i) % self.buf.len().max(1);
                new_buf.push(self.buf[src].clone());
            }
            new_buf.push(v);
            self.buf = new_buf;
            self.head = 0;
            self.len += 1;
        } else {
            let tail = (self.head + self.len) % self.buf.len();
            if tail < self.buf.len() {
                self.buf[tail] = v;
            } else {
                self.buf.push(v);
            }
            self.len += 1;
        }
    }

    pub fn push_front(&mut self, v: Value) {
        if self.len == self.buf.len() {
            let new_cap = (self.buf.len() * 2).max(4);
            let mut new_buf: Vec<Value> = Vec::with_capacity(new_cap);
            new_buf.push(v);
            for i in 0..self.len {
                let src = (self.head + i) % self.buf.len().max(1);
                new_buf.push(self.buf[src].clone());
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
