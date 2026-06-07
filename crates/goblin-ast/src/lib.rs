// ---- version = "0.19.0"
//! Abstract Syntax Tree (AST) for Goblin — aligned to the current parser.
use goblin_diagnostics::Span;

pub type Ident = (String, Span);

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindMode {
    Tether,    // |
    Retether,  // |=
    Shadow,    // [=
}

#[derive(Debug, Clone)]
pub struct BindStmt {
    pub name: Ident,
    pub expr: Expr,
    pub is_imm: bool,      // preceded by `imm`
    pub is_local: bool,    // preceded by `local`
    pub mode: BindMode,    // Tether | Retether | Shadow
    pub span: Span,
    pub class_name: Option<String>,
    pub lock_type: Option<String>, // type suffix on declaration: age.i32 | 46
}

#[derive(Debug, Clone)]
pub struct TupleBindStmt {
    pub names: Vec<Ident>,
    pub expr: Expr,
    pub is_imm: bool,
    pub is_local: bool,
    pub mode: BindMode,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Class(ClassDecl),
    Action(ActionDecl),
    Bind(BindStmt),
    TupleBind(TupleBindStmt),
    Enum(EnumDecl),
    Import(ImportStmt),
    Judge(JudgeStmt),
    JudgeAll(JudgeAllStmt),
    Sweep(SweepStmt),
    Return(ReturnStmt),
    OverlayDef(OverlayDefStmt),
    OverlayApply(OverlayApplyStmt),
    OverlayDetach(OverlayDetachStmt),
    LinkDef(LinkDefStmt),
    ObjectLinkDef(ObjectLinkDefStmt),
    LinkOffset(LinkOffsetStmt),
    ClearLink(ClearLinkStmt),
    /// `VarName score | decision against Class by [ formula ]`
    ObjectDecision(String, DecisionDef),
    UnitDecl(UnitDecl),

    Block {
        stmts: Vec<Stmt>,
        span: Span,
    },
    BoxBind {
        namespace: String,
        name: String,
        expr: Expr,
        mode: BindMode,
        span: Span,
    },
}

// ── Ownership AST nodes ──────────────────────────────────────────────────────

/// How capacity is measured on an owning object.
#[derive(Debug, Clone)]
pub enum CapacityDecl {
    /// `capacity: N` — owner can hold at most N objects.
    Count(u64),
    /// `capacity from field_name: N` — sum of `field_name` across owned objects
    /// must not exceed N.
    Field { field_name: String, limit: f64 },
}

/// `unit name | types: a, b; N a = M b end` — user-defined unit conversion.
#[derive(Debug, Clone)]
pub struct UnitDecl {
    pub name: String,
    /// All type names in this unit family.
    pub types: Vec<String>,
    /// Conversion rules: (from_type, from_count, to_type, to_count)
    /// e.g. `1 lb = 16 oz` → ("lb", 1.0, "oz", 16.0)
    pub conversions: Vec<(String, f64, String, f64)>,
    pub span: Span,
}

/// `link ClassName [channel] by [ formula ]` — defines a class-level link formula.
/// If channel is None, defaults to "default".
#[derive(Debug, Clone)]
pub struct LinkDefStmt {
    pub class_name: String,
    /// Named channel (e.g. "border", "trade", "culture"). None = "default".
    pub channel: Option<String>,
    /// The formula expression. May reference `self` and `target` as identifiers.
    pub formula: Expr,
    pub span: Span,
}

/// `ObjectName link [channel] by [ formula ]` — defines an object-level link formula override.
#[derive(Debug, Clone)]
pub struct ObjectLinkDefStmt {
    pub object_var: String,
    /// Named channel. None = "default".
    pub channel: Option<String>,
    /// The formula expression. May reference `self` and `target` as identifiers.
    pub formula: Expr,
    pub span: Span,
}

/// `VarA link to VarB on channel offset value [for N ticks]` — pair-level link offset.
#[derive(Debug, Clone)]
pub struct LinkOffsetStmt {
    pub from_var: String,
    pub to_var: String,
    pub channel: String,
    pub offset: f64,
    /// None = permanent. Some(N) = expires after N ticks.
    pub ticks: Option<u32>,
    pub span: Span,
}

/// `clear link VarA to VarB on channel` — removes all offsets on a pair+channel.
#[derive(Debug, Clone)]
pub struct ClearLinkStmt {
    pub from_var: String,
    pub to_var: String,
    pub channel: String,
    pub span: Span,
}

// ── Overlay AST nodes ────────────────────────────────────────────────────────

/// How an overlay propagates to new hosts each tick.
#[derive(Debug, Clone)]
pub enum SpreadRule {
    /// `spreads through channel at rate` — only to hosts with link score > threshold on channel.
    Channel { channel: String, rate: f64 },
    /// `spreads to all ClassName at rate` — broadcasts to all live objects of that class.
    All { class_name: String, rate: f64 },
    /// `spreads to nearby at rate` — spatial proximity (requires map feature).
    Nearby { rate: f64 },
    /// `spreads through ownership at rate` — follows owner_id chain.
    Ownership { rate: f64 },
    /// `spreads where condition at rate` — predicate-based spread.
    Predicate { condition: Expr, rate: f64 },
}

/// What happens when an overlay is applied to a host that already carries it.
#[derive(Debug, Clone, PartialEq)]
pub enum OverlayApplyBehavior {
    /// First application wins. Subsequent applications are silently dropped. Default.
    Caps,
    /// New application overwrites the existing instance unconditionally.
    Replaces,
    /// Each application increments a counter on the existing instance.
    /// Optional semantic label replaces "count" in diagnostics and queries.
    Stacks { label: Option<String> },
}

impl Default for OverlayApplyBehavior {
    fn default() -> Self { OverlayApplyBehavior::Caps }
}

/// `overlay Name | ... end` — defines an overlay type.
#[derive(Debug, Clone)]
pub struct OverlayDefStmt {
    pub name: String,
    /// Class names this overlay may occupy. Empty = any host.
    pub host_types: Vec<String>,
    /// Spread rules — multiple modes may be declared on one overlay.
    pub spread_rules: Vec<SpreadRule>,
    /// Strength lost per tick.
    pub decay_rate: f64,
    /// (field_name, expr) — effective modifier applied at read time during trait evaluation.
    /// Expr may reference `self >> count` (or the stacks label) for dynamic scaling.
    /// Never written to host fields. Base value is always clean.
    pub modifiers: Vec<(String, Expr)>,
    pub conflict_rules: Vec<OverlayConflictRule>,
    pub spawn_rules: Vec<OverlaySpawnRule>,
    /// Transition declarations — what identity changes this overlay can undergo.
    /// Same model as object transitions. Triggered by threshold, executed by runtime.
    /// Overlay transitions operate on overlay records, not objects.
    pub transitions: Vec<TransitionDef>,
    /// Default duration in ticks. None = permanent.
    pub default_duration: Option<u32>,
    /// What happens when applied to a host that already carries this overlay.
    pub apply_behavior: OverlayApplyBehavior,
    /// User-defined fields declared on the overlay (e.g. kind: "language")
    pub extra_fields: Vec<(String, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct OverlayConflictRule {
    pub opponent: String,
    /// Multiplier on the weaker overlay's decay rate during conflict. Default 2.0.
    pub suppress_rate: f64,
}

#[derive(Debug, Clone)]
pub struct OverlaySpawnRule {
    pub condition: Expr,
    pub spawn_overlay: String,
    pub spawn_strength: f64,
}

/// `overlay Name on target at strength` — applies an overlay instance to a host.
#[derive(Debug, Clone)]
pub struct OverlayApplyStmt {
    pub overlay_name: String,
    /// Expression that evaluates to the host object variable name.
    pub host_expr: Expr,
    /// Occupation strength (0..1). Defaults to 1.0 if omitted.
    pub strength: f64,
    /// Override duration in ticks. None = use overlay default.
    pub duration_override: Option<u32>,
    pub span: Span,
}

/// `detach Name from target` — removes an overlay instance from a host.
#[derive(Debug, Clone)]
pub struct OverlayDetachStmt {
    pub overlay_name: String,
    pub host_expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub actions: Vec<ActionDecl>,
    /// Optional decision formula declared inside the class body.
    pub decision: Option<DecisionDef>,
    /// Optional judge block declared inside the class body.
    pub judge: Option<JudgeStmt>,
    /// Transition declarations — what identity changes this class can undergo.
    pub transitions: Vec<TransitionDef>,
    /// Optional capacity declaration — how many/how much this class can own.
    /// None = infinite capacity.
    pub capacity: Option<CapacityDecl>,
    pub span: Span,
}

// ── Transition AST nodes ─────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransitionKind {
    Spawn,
    Erase,
    Split,
    Fracture,
    Merge,
    Absorb,
    Subjugate,
    Mutate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverlayContinuity {
    Split,    // distribute overlays to all successors
    Transfer, // move overlays to primary successor
    Drop,     // remove all overlays
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkContinuity {
    Inherit, // transfer links to successors
    Reset,   // drop all links
}

/// One named successor state block: `first: ... end`, `carries: ... end`, etc.
#[derive(Debug, Clone)]
pub struct SuccessorDef {
    /// Label: "first", "second", "child", "fragment", "carries"
    pub label: String,
    /// Field assignments. Expressions may reference `self` and `target`.
    pub fields: Vec<(String, Expr)>,
    pub span: Span,
}

/// A full transition declaration inside a class or overlay body.
/// When declared on an overlay, the runtime executes the transition against
/// the overlay record rather than the object store. No agency is implied —
/// overlay transitions are always threshold-triggered by the runtime.
#[derive(Debug, Clone)]
pub struct TransitionDef {
    pub kind: TransitionKind,
    /// Trigger condition. Evaluated each tick with `self` (and `target` for binary transitions).
    pub trigger: Expr,
    /// For absorb/subjugate/merge — the class of object to act on.
    pub target_class: Option<String>,
    /// Class name(s) for successors (split, fracture, spawn, mutate).
    /// Split may produce multiple classes; others produce one.
    pub into_classes: Vec<String>,
    /// Successor state blocks.
    pub successors: Vec<SuccessorDef>,
    pub overlay_rule: OverlayContinuity,
    pub link_rule: LinkContinuity,
    pub span: Span,
}

// ── Decision AST nodes ───────────────────────────────────────────────────────

/// `score | decision against TargetClass by [ formula ]`
/// Declared inside a class body. The runtime evaluates this formula
/// against all valid targets each tick and selects the best one.
#[derive(Debug, Clone)]
pub struct DecisionDef {
    /// The class of objects to evaluate as potential targets.
    pub target_class: String,
    /// Formula expression. References `self` and `target` as identifiers.
    pub formula: Expr,
    /// Derived theoretical minimum (computed at registration).
    pub formula_min: f64,
    /// Derived theoretical maximum (computed at registration).
    pub formula_max: f64,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub name: String,
    pub private: bool,
    pub nullable: bool,      // true if field has `?` suffix
    pub readonly: bool,      // true if field has `!` prefix
    pub raw: bool,           // true if field has `~` prefix — opts out of trait inference
    pub relation: Option<RelationDef>,
    pub default: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum RelationDef {
    Of { class_name: String, as_name: String },
    With { class_name: String },
    Re { class_name: String },
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub items: ImportItems,
    pub alias: Option<String>,  // Only used for single-path imports
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportItems {
    /// Single path: import game/hero
    Path(String),
    /// Dynamic path: import "../site/portals/{portal}/manifest.imports"
    Expr(Expr),
    /// Multiple items from source: import { hero, Combat } from game
    Named {
        items: Vec<ImportItem>,
        source: String,
    },
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,  // For: import { hero as h } from game
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_name: Option<String>,
    pub default: Option<Expr>, // AST Expr (not PExpr)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ActionBody {
    Block(Vec<Stmt>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: ActionBody,
    pub span: Span,
    pub ret: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub values: Vec<Expr>, // expressions; empty = bare return
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum JudgeArmBody {
    Expr(Expr),
    Stmts(Vec<Stmt>),
}

/// One arm of a **statement-form** judge.
/// `condition = None` means this is the implicit `else` arm.
#[derive(Debug, Clone)]
pub struct JudgeArmStmt {
    pub condition: Option<Box<Expr>>, // None for `else`
    pub body: JudgeArmBody,
    pub span: Span,
}

/// Statement-form judge node. Independent from the expression form (`Expr::Judge`).
#[derive(Debug, Clone)]
pub struct JudgeStmt {
    pub arms: Vec<JudgeArmStmt>,          // one or more arms
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JudgeAllStmt {
    pub arms: Vec<JudgeArmStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SweepMode { Match, All }

#[derive(Debug, Clone)]
pub enum SweepArmKind {
    Pattern(String),                          // "needle" :
    Range { start: String, end: String },     // "a" ... "b" :
    AllBody,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SweepArmRepeat {
    All,    // default: operate on every match
    First,  // only the first match in this file
    Last,   // only the last match in this file
}

#[derive(Debug, Clone)]
pub struct SweepArm {
    pub kind: SweepArmKind,
    pub repeat: SweepArmRepeat,
    pub body: Vec<Stmt>,                      // ordinary statements
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SweepStmt {
    pub mode: SweepMode,
    pub targets: Vec<Expr>,
    pub arms: Vec<SweepArm>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<FieldDecl>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JudgeArm {
    pub condition: Option<Box<Expr>>,  // None for else
    pub value: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals & identifiers
    Nil(Span),
    Bool(bool, Span),
    Number(String, Span),    // raw text as produced by lexer/parser (e.g., "10", "3.14")
    Str(String, Span),
    Char(char, Span),
    Ident(String, Span),
    Slice(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>, Span),
    Slice3(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>, Span),

    // Collections & objects
    Array(Vec<Expr>, Span),
    Object(Vec<(String, Expr)>, Span),

    // Property & indexing
    Member(Box<Expr>, String, Span),      // obj.name
    OptMember(Box<Expr>, String, Span),   // obj?.name
    Index(Box<Expr>, Box<Expr>, Span),    // obj[idx]  — positional (arrays)
    IndexMap(Box<Expr>, Box<Expr>, Span), // obj{key}  — keyed (maps)
    Index2(Box<Expr>, Box<Expr>, Box<Expr>, Span), // grid[x, y]

    // Calls
    Call(Box<Expr>, String, Vec<Expr>, Span),    // recv.name(args)
    OptCall(Box<Expr>, String, Vec<Expr>, Span), // recv?.name(args)
    FreeCall(String, Vec<Expr>, Span),           // name(args)
    NsCall(String, String, Vec<Expr>, Span),     // Ns::name(args)

    // Operators
    Prefix(String, Box<Expr>, Span),
    Postfix(Box<Expr>, String, Span),
    Binary(Box<Expr>, String, Box<Expr>, Span),

    // Other
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Option<Vec<(String, Expr)>>,
        span: Span,
    },
    Judge {
        using: Option<Box<Expr>>,
        header: Option<Box<Expr>>,
        arms: Vec<JudgeArm>,
        all: bool,
        span: Span,
    },
    Block {
        stmts: Vec<Stmt>,
        span: Span,
    },
    LiteralToken { module: String, ident: String, span: Span },
    BoxVar {
        namespace: String,
        name: String,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Nil(sp) => sp,
            Expr::Bool(_, sp) => sp,
            Expr::Number(_, sp) => sp,
            Expr::Str(_, sp) => sp,
            Expr::Char(_, sp) => sp,
            Expr::Ident(_, sp) => sp,
            Expr::Slice(_, _, _, sp) => sp,
            Expr::Slice3(_, _, _, _, sp) => sp,
            Expr::Array(_, sp) => sp,
            Expr::Object(_, sp) => sp,
            Expr::Member(_, _, sp) => sp,
            Expr::OptMember(_, _, sp) => sp,
            Expr::Index(_, _, sp) => sp,
            Expr::IndexMap(_, _, sp) => sp,
            Expr::Index2(_, _, _, sp) => sp,
            Expr::Call(_, _, _, sp) => sp,
            Expr::OptCall(_, _, _, sp) => sp,
            Expr::FreeCall(_, _, sp) => sp,
            Expr::NsCall(_, _, _, sp) => sp,
            Expr::Prefix(_, _, sp) => sp,
            Expr::Postfix(_, _, sp) => sp,
            Expr::Binary(_, _, _, sp) => sp,
            Expr::EnumVariant { span, .. } => span,
            Expr::Judge { span, .. } => span,
            Expr::Block { span, .. } => span,
            Expr::LiteralToken { span, .. } => span,
            Expr::BoxVar { span, .. } => span,
        }
    }
}