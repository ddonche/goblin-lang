use crate::value::BuiltinId;

/// The full Goblin bytecode instruction set.
///
/// Design: register-stack hybrid, same class as CPython 3.12 / Lua 5.x / Wren.
/// Locals live in indexed slots; expressions use the operand stack.
///
/// Quickening: generic ops (Add, Sub, …) are replaced at runtime with
/// specialised forms (AddInt, AddFloat, Concat) once operand types are known.
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    // ── Literals ───────────────────────────────────────────────────────────────
    /// Push constants[idx] onto the stack (allocates a new stash).
    LoadConst(u16),
    /// Push Value::Nil.
    LoadNil,
    /// Push Value::Unit (implicit return value — matches interpreter semantics).
    LoadUnit,
    /// Push Value::Bool(true).
    LoadTrue,
    /// Push Value::Bool(false).
    LoadFalse,

    // ── Locals (tether slots) ──────────────────────────────────────────────────
    /// Push a copy of the tether in locals[slot].
    LoadLocal(u16),
    /// `x` as the argument of `x |= f(x)`: like LoadLocal, but when only
    /// this variable holds its value (no nested action, nothing pending),
    /// the value is moved out to the call, so the callee can change it in
    /// place instead of copying it. x is stored right after the call.
    TakeLocal(u16),
    /// Pop top and store into locals[slot].
    /// Implements |, |=, and [= — the compiler chooses the opcode at compile time;
    /// at runtime they are all the same operation (store tether into slot).
    StoreLocal(u16),
    /// Like StoreLocal, but a new binding (`x | v`, `[=`, a loop's name):
    /// never changes a value a nested action captured from an earlier one.
    BindLocal(u16),

    // ── Globals ────────────────────────────────────────────────────────────────
    /// Push a copy of the tether in globals[idx].
    LoadGlobal(u16),
    /// Pop top and store into globals[idx].
    StoreGlobal(u16),

    // ── Upvalues (closures) ────────────────────────────────────────────────────
    /// Push a copy of the current closure's captured upvalue[idx].
    LoadUpvalue(u8),
    /// Pop top and overwrite the current closure's captured upvalue[idx].
    StoreUpvalue(u8),

    // ── Stack manipulation ─────────────────────────────────────────────────────
    /// Discard the top-of-stack tether.
    Pop,
    /// Duplicate top-of-stack.
    Dup,

    // ── overwrite! ─────────────────────────────────────────────────────────────
    /// Pop (new_val_tether, target_tether) and call session.overwrite.
    /// Stack before: [..., target, new_val]
    Overwrite,
    /// Pop the variable just loaded; error if it holds a frozen snapshot.
    /// The u16 is the constant index of the variable's name.
    GuardMutable(u16),

    // ── Arithmetic (generic — quickened at runtime) ────────────────────────────
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,

    // ── Quickened arithmetic ───────────────────────────────────────────────────
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    RemInt,
    NegInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    NegFloat,

    // ── String ────────────────────────────────────────────────────────────────
    Concat,

    // ── Comparison ────────────────────────────────────────────────────────────
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // ── Logical ───────────────────────────────────────────────────────────────
    Not,

    // ── Control flow ──────────────────────────────────────────────────────────
    /// Unconditional jump by a signed relative offset (from instruction AFTER this one).
    Jump(i16),
    /// Pop top; if falsy, jump by offset.
    JumpIfFalse(i16),
    /// Pop top; if truthy, jump by offset (used for short-circuit `or`).
    JumpIfTrue(i16),

    // ── Collections ───────────────────────────────────────────────────────────
    /// Pop N values from the stack (pushed first-to-last) → build FlatArray collection.
    MakeArray(u16),
    /// Pop N*2 values (key0, val0, key1, val1, …) → build SmallMap collection.
    MakeMap(u16),

    /// Pop key, pop collection → push element (or Nil if absent).
    GetIndex,
    /// Pop new_val, pop key, pop collection → push updated collection (|! semantics).
    SetIndex,

    /// `x[k]` in source: GetIndex that errors when x is a map (maps take `{}`).
    /// constants[idx] = how x reads in the source, for the message.
    IndexGet(u16),
    /// `x{k}` in source: GetIndex that errors when x is an array or string.
    KeyGet(u16),
    /// Before a write through `root[k1]{k2}…` (n segments): checks each level
    /// takes the bracket the source used, without consuming anything.
    /// Stack: [root, key1, …, keyN]. constants[idx] = array of strings, one
    /// per segment: "[" or "{" followed by the level's source text, or "."
    /// for a `>>` field.
    CheckPath(u8, u16),
    /// Before `name |= v`: stack [v, old]. Pops old; errors (R1300) when old
    /// is a live object and v is not one. constants[idx] = the name.
    GuardObjRebind(u16),
    /// `LoadLocal(slot); GuardObjRebind(idx)` in one step: errors (R1300)
    /// when local `slot` holds a live object and the value on top of the
    /// stack is not one. The stack is left as it was.
    GuardRebindLocal(u16, u16),
    /// Like GetIndex but key is constants[idx] (string member access).
    GetMember(u16),
    /// Pop new_val, pop object → push updated object with field set. Key is constants[idx].
    SetField(u16),
    /// Nested `update!` through an lvalue path of `n` segments.
    /// Stack: [root, key1, …, keyN, new_val] → push the updated root.
    /// Bit i of the mask marks segment i as a `>>` field access (key is a
    /// field-name string) rather than an index.
    UpdatePath(u8, u16),
    /// UpdatePath whose root was loaded from a variable that the result is
    /// stored back into: writes through in place when the VM holds the only
    /// reference to the root's stash, and otherwise acts as UpdatePath.
    UpdatePathMut(u8, u16),
    /// Jump by the offset when local slot `slot` already holds a value; used by
    /// parameter defaults, whose slots stay unset when the caller omits them.
    JumpIfLocalSet(u16, i16),
    /// Inside an `:objects(…)` / `:overlays(…)` predicate: when local 0 (`it`)
    /// is an object with the field constants[idx], push that field's value and
    /// jump by the offset (past the name's ordinary load); otherwise fall through.
    LoadItFieldOrJump(u16, i16),
    /// Pop a map value, instantiate a class object from it. constants[idx] = class name string.
    ClassInstantiate(u16),
    /// Stack: [recv, arg0..arg_{argc-1}]. constants[name_idx] = method name. Pop all, push result.
    /// After call, if result is Object with same uuid as recv, overwrite recv's storage.
    CallMethod(u16, u8),

    // ── Function calls ─────────────────────────────────────────────────────────
    /// Call with argc positional args.
    /// Stack layout before: [..., func_tether, arg0, arg1, …, arg_{argc-1}]
    /// Stack layout after:  [..., return_tether]
    Call(u8),

    /// Return from the current function.
    /// Stack must have exactly one tether above stack_base (the return value).
    /// If the stack is empty above stack_base, returns Nil.
    Return,

    /// Call a builtin function with argc args.
    /// Args are popped; result is pushed.
    CallBuiltin(BuiltinId, u8),
    /// CallBuiltin for `name!(x, …)` where `x` is a variable the result is
    /// stored back into: changes x's collection in place when the VM holds
    /// the only reference to its stash (leaving x's operand as the result),
    /// and otherwise acts as CallBuiltin.
    CallBuiltinMut(BuiltinId, u8),
    /// `name!(x[k1]…[kN], args…)` where x is a variable: stack is
    /// [x, k1…kN, args…] (argc counts the target as one argument). Changes
    /// the element at the path in place when the VM holds the only reference
    /// to x's stash (leaving x's operand), and otherwise reads the element,
    /// calls the builtin and writes the result back along the path as
    /// `update!` does, pushing x's new value. The caller stores it into x.
    CallBuiltinMutPath(BuiltinId, u8, u8),

    // ── Closures ──────────────────────────────────────────────────────────────
    /// Create a closure from constants[func_idx] (a FunctionObject).
    /// The upvalue list is populated according to the FunctionObject's
    /// upvalue_descriptors: each descriptor says "capture local slot N" or
    /// "forward upvalue N from the current closure".
    MakeClosure(u16),

    /// Pop top → push Value::Pct(v as f64 / 100.0)  (postfix %)
    ToPct,
    /// Pop two values → push Value::Pair(a, b)  (>< divmod operator)
    MakePair,
    /// Pop two Int values → push Value::Array of range  (.. / ... operators)
    MakeRange,
    /// Pop two Int values → push Value::Array of inclusive range  (... operator)
    MakeRangeInclusive,

    // ── Quickening placeholder ─────────────────────────────────────────────────
    /// Unused at parse time; inserted by the quickening pass.
    /// Represents an inlined type-specialised dispatch.
    Quick(u8),

    /// Push a catch frame. If any error occurs between TryBegin and TryEnd,
    /// unwind to this frame, push error as string, jump to catch_ip.
    /// offset is relative to the instruction AFTER TryBegin.
    TryBegin(i16),
    /// Normal completion of a try block — pop the catch frame.
    TryEnd,

    /// Import a file: constants[path_idx] is the path string.
    /// Resolves against session.base_dir, lexes/parses/compiles/runs the file.
    /// Pushes nothing (import is for side effects / populating globals).
    ImportFile(u16),

    /// `import path as ns` — imports a file and registers all of its top-level
    /// actions under the namespace alias so `ns::action` dispatch works.
    /// constants[path_idx] = file path, constants[ns_idx] = namespace alias string.
    ImportFileAs(u16, u16),

    /// Load a GLAM: constants[ns_idx] is the glam namespace string.
    /// Reads glams/<ns>/glam.toml (if present) to populate session.action_needs,
    /// then imports glams/<ns>/<ns>.gbln with owner_glam = Some(ns) stamped onto
    /// its top-level actions. Pushes nothing.
    UseGlam(u16),

    // ── DES statement opcodes (their data is in the function's `op_data`) ──
    /// Register an overlay definition in session.overlay_defs.
    OverlayDef(u16),
    /// Apply an overlay to a host (host value must be on stack).
    OverlayApply(u16),
    /// Detach an overlay from a host (host variable name on stack).
    OverlayDetach(u16),
    /// Register a class-level link definition.
    LinkDef(u16),
    /// Register an object-level link definition override.
    ObjectLinkDef(u16),
    /// Apply a link offset.
    LinkOffset(u16),
    /// Clear link offsets.
    ClearLink(u16),
    /// Register an object decision formula.
    ObjectDecision(u16),
    /// Register a unit declaration.
    UnitDecl(u16),

    /// Peek at top-of-stack and register it into session.named_values[constants[name_idx]].
    /// Does NOT pop — the value stays on stack for the subsequent StoreLocal.
    RegisterAction(u16),

    /// Runtime named-value lookup: load session.named_values[constants[name_idx]] onto stack.
    /// Used as fallback for namespace-qualified calls (import X as ns; ns::action()).
    LoadNamed(u16),

    /// Interpolate a string constant: constants[idx] is a raw template string.
    /// Looks up {ident} placeholders in the current locals/globals scope at runtime.
    StringInterp(u16),
    /// Like StringInterp, with the values of n placeholders resolved at
    /// compile time: constants[idx] = [template, name1, …, nameN] and the
    /// stack holds the N values (last on top).
    StringInterpVals(u16, u8),

    /// Load a field from self (locals[0], which is the receiver object in a class method).
    /// constants[idx] is the field name string. Pushes nil if self is not an Object or field absent.
    SelfField(u16),

    // ── Type-lock opcodes ─────────────────────────────────────────────────────
    /// Tether with type lock for a local slot: pop value, cast to lock_type, store in slot,
    /// register lock in BOTH type_locks and hard_type_locks of the current frame.
    StoreLockLocal(u16, u16),
    /// Same as StoreLockLocal but for a global slot (by index).
    StoreLockGlobal(u16, u16),

    /// Cast-bang for a local: read value from slot, cast to cast_type, store back, update
    /// type_locks (NOT hard_type_locks). Push new value onto stack.
    CastBangLocal(u16, u16),
    /// Same as CastBangLocal but for a global slot.
    CastBangGlobal(u16, u16),

    /// Get the type lock for a local variable: if type_lock is set push it as a string
    /// (qualified for collections), otherwise fall through to normal TypeOf on the value.
    GetTypeLockLocal(u16),
    /// Same as GetTypeLockLocal but for a global slot.
    GetTypeLockGlobal(u16),

    /// Cast member with hard-lock check for a local: read from slot, check hard_type_lock
    /// (error R0215 on mismatch), cast value, push result. Does NOT store back.
    CastMemberLocal(u16, u16),
    /// Same as CastMemberLocal but for a global slot.
    CastMemberGlobal(u16, u16),

    /// Pop RHS; if it is a Collection/Array of exactly N elements, push them in
    /// reverse order (element 0 on top). If empty collection, broadcast empty
    /// arrays. If scalar (non-collection), broadcast N copies. Arity mismatch on
    /// non-empty wrong-length array.
    TupleSplit(u8),

    /// Pop value from stack top and write it to session.box_store["{ns}::{name}"].
    /// ns_idx and name_idx are constant-pool indices into the enclosing function.
    StoreBox(u16, u16, bool),
    /// Before `#ns::name | v` evaluates v: the GLAM namespace/[provides]
    /// check. constants[ns_idx], constants[name_idx] are strings.
    BoxCheck(u16, u16),

    /// Collect-loop accumulator append: pops the top-of-stack element and appends
    /// it to the array in locals[slot] without a full clone.
    /// If tether_count == 1 (sole owner), mutates in place (O(1)).
    /// Otherwise falls back to clone + alloc (O(N)).
    ArrayPushToLocal(u16),
    /// `x |= x + piece` with x in locals[slot]: pops the piece; when x is
    /// text that only this variable holds and the piece is text, adds it to
    /// x in place and pushes x. Otherwise pushes x and the piece and adds
    /// them as `Add` does. The guard and StoreLocal of `|=` follow.
    AppendLocal(u16),
    /// A short run of simple steps done as one: a path read like
    /// `x{"a"}[i]{"b"}`, or the start of a write through one.
    /// op_data[idx] = OpData::Fused, which also holds the original steps;
    /// they run one by one whenever the quick way can't be taken.
    Fused(u16),
    /// `UpdatePathMut(n, 0)` after a fused check (op_data[idx]): the same
    /// write, trying each map key at the slot the check found.
    UpdatePathHint(u8, u16),
    /// Before `:update!(path, e)` with `e` plain arithmetic on numbers:
    /// does the whole statement at once and skips its steps, or does
    /// nothing so they run. op_data[idx] = OpData::QuickSet.
    QuickSet(u16),
    /// One pass of a `repeat` loop (op_data[idx] = OpData::Repeat): moves
    /// the hidden counter on, leaves the loop when it reaches the limit,
    /// and binds the pass's names for the loop's kind. Only a condition
    /// loop runs the steps right after this (its test); the others skip them.
    RepeatNext(u16),
    /// Start of a `repeat`: pops the value it runs over and sets up the
    /// loop's state (kind, items, limit, counter) in the call frame, from
    /// op_data[idx] (a `Repeat`).
    RepeatStart(u16),
    /// In a condition `repeat`, after its test passed: idx = the counter.
    RepeatIdx(u16),
}

impl Opcode {
    /// Returns the opcode name as a static string (for the disassembler).
    pub fn name(&self) -> &'static str {
        match self {
            Opcode::LoadConst(_)    => "LoadConst",
            Opcode::LoadNil         => "LoadNil",
            Opcode::LoadUnit        => "LoadUnit",
            Opcode::LoadTrue        => "LoadTrue",
            Opcode::LoadFalse       => "LoadFalse",
            Opcode::LoadLocal(_)    => "LoadLocal",
            Opcode::TakeLocal(_)    => "TakeLocal",
            Opcode::StoreLocal(_)   => "StoreLocal",
            Opcode::BindLocal(_)    => "BindLocal",
            Opcode::LoadGlobal(_)   => "LoadGlobal",
            Opcode::StoreGlobal(_)  => "StoreGlobal",
            Opcode::LoadUpvalue(_)  => "LoadUpvalue",
            Opcode::StoreUpvalue(_) => "StoreUpvalue",
            Opcode::Pop             => "Pop",
            Opcode::Dup             => "Dup",
            Opcode::Overwrite       => "Overwrite",
            Opcode::GuardMutable(_) => "GuardMutable",
            Opcode::Add             => "Add",
            Opcode::Sub             => "Sub",
            Opcode::Mul             => "Mul",
            Opcode::Div             => "Div",
            Opcode::Rem             => "Rem",
            Opcode::Neg             => "Neg",
            Opcode::AddInt          => "AddInt",
            Opcode::SubInt          => "SubInt",
            Opcode::MulInt          => "MulInt",
            Opcode::DivInt          => "DivInt",
            Opcode::RemInt          => "RemInt",
            Opcode::NegInt          => "NegInt",
            Opcode::AddFloat        => "AddFloat",
            Opcode::SubFloat        => "SubFloat",
            Opcode::MulFloat        => "MulFloat",
            Opcode::DivFloat        => "DivFloat",
            Opcode::NegFloat        => "NegFloat",
            Opcode::Concat          => "Concat",
            Opcode::Eq              => "Eq",
            Opcode::Ne              => "Ne",
            Opcode::Lt              => "Lt",
            Opcode::Le              => "Le",
            Opcode::Gt              => "Gt",
            Opcode::Ge              => "Ge",
            Opcode::Not             => "Not",
            Opcode::Jump(_)         => "Jump",
            Opcode::JumpIfFalse(_)  => "JumpIfFalse",
            Opcode::JumpIfTrue(_)   => "JumpIfTrue",
            Opcode::MakeArray(_)    => "MakeArray",
            Opcode::MakeMap(_)      => "MakeMap",
            Opcode::GetIndex        => "GetIndex",
            Opcode::IndexGet(_)     => "IndexGet",
            Opcode::KeyGet(_)       => "KeyGet",
            Opcode::CheckPath(..)   => "CheckPath",
            Opcode::GuardObjRebind(_) => "GuardObjRebind",
            Opcode::GuardRebindLocal(..) => "GuardRebindLocal",
            Opcode::SetIndex        => "SetIndex",
            Opcode::GetMember(_)    => "GetMember",
            Opcode::SetField(_)     => "SetField",
            Opcode::UpdatePath(..)  => "UpdatePath",
            Opcode::UpdatePathMut(..) => "UpdatePathMut",
            Opcode::Fused(_) => "Fused",
            Opcode::UpdatePathHint(..) => "UpdatePathHint",
            Opcode::QuickSet(_) => "QuickSet",
            Opcode::RepeatNext(_) => "RepeatNext",
            Opcode::RepeatStart(_) => "RepeatStart",
            Opcode::RepeatIdx(_) => "RepeatIdx",
            Opcode::CallBuiltinMut(..) => "CallBuiltinMut",
            Opcode::CallBuiltinMutPath(..) => "CallBuiltinMutPath",
            Opcode::JumpIfLocalSet(..) => "JumpIfLocalSet",
            Opcode::LoadItFieldOrJump(..) => "LoadItFieldOrJump",
            Opcode::StringInterpVals(..) => "StringInterpVals",
            Opcode::ClassInstantiate(_) => "ClassInstantiate",
            Opcode::CallMethod(_, _)    => "CallMethod",
            Opcode::Call(_)         => "Call",
            Opcode::Return          => "Return",
            Opcode::CallBuiltin(..) => "CallBuiltin",
            Opcode::MakeClosure(_)  => "MakeClosure",
            Opcode::ToPct           => "ToPct",
            Opcode::MakePair        => "MakePair",
            Opcode::MakeRange       => "MakeRange",
            Opcode::MakeRangeInclusive => "MakeRangeInclusive",
            Opcode::Quick(_)        => "Quick",
            Opcode::TryBegin(_)       => "TryBegin",
            Opcode::TryEnd            => "TryEnd",
            Opcode::ImportFile(_)     => "ImportFile",
            Opcode::ImportFileAs(..)  => "ImportFileAs",
            Opcode::UseGlam(_)        => "UseGlam",
            Opcode::OverlayDef(_)     => "OverlayDef",
            Opcode::OverlayApply(_) => "OverlayApply",
            Opcode::OverlayDetach(_) => "OverlayDetach",
            Opcode::LinkDef(_)        => "LinkDef",
            Opcode::ObjectLinkDef(_)  => "ObjectLinkDef",
            Opcode::LinkOffset(_)     => "LinkOffset",
            Opcode::ClearLink(_)      => "ClearLink",
            Opcode::ObjectDecision(_) => "ObjectDecision",
            Opcode::UnitDecl(_)       => "UnitDecl",
            Opcode::RegisterAction(_) => "RegisterAction",
            Opcode::LoadNamed(_)      => "LoadNamed",
            Opcode::StringInterp(_)   => "StringInterp",
            Opcode::SelfField(_)      => "SelfField",
            Opcode::StoreLockLocal(..)   => "StoreLockLocal",
            Opcode::StoreLockGlobal(..)  => "StoreLockGlobal",
            Opcode::CastBangLocal(..)    => "CastBangLocal",
            Opcode::CastBangGlobal(..)   => "CastBangGlobal",
            Opcode::GetTypeLockLocal(_)  => "GetTypeLockLocal",
            Opcode::GetTypeLockGlobal(_) => "GetTypeLockGlobal",
            Opcode::CastMemberLocal(..)  => "CastMemberLocal",
            Opcode::CastMemberGlobal(..) => "CastMemberGlobal",
            Opcode::TupleSplit(_)        => "TupleSplit",
            Opcode::StoreBox(..)         => "StoreBox",
            Opcode::BoxCheck(..)         => "BoxCheck",
            Opcode::ArrayPushToLocal(_)  => "ArrayPushToLocal",
            Opcode::AppendLocal(_)       => "AppendLocal",
        }
    }
}

/// Data for the few opcodes that need more than small numbers, kept beside
/// the bytecode (`FunctionObject::op_data`) so `Opcode` stays a plain
/// copyable value and the VM loop never clones or drops heap data per
/// instruction. The opcode holds the index.
#[derive(Debug, Clone)]
pub enum OpData {
    /// A type name, for the type-lock and cast opcodes.
    Name(String),
    OverlayDef(Box<goblin_ast::OverlayDefStmt>),
    OverlayApply { overlay_name: String, host_var_name: String, strength: f64, duration_override: Option<u32> },
    OverlayDetach { overlay_name: String, host_var_name: String },
    LinkDef(Box<goblin_ast::LinkDefStmt>),
    ObjectLinkDef(Box<goblin_ast::ObjectLinkDefStmt>),
    LinkOffset(Box<goblin_ast::LinkOffsetStmt>),
    ClearLink(Box<goblin_ast::ClearLinkStmt>),
    ObjectDecision { var_name: String, def: Box<goblin_ast::DecisionDef> },
    UnitDecl(Box<goblin_ast::UnitDecl>),
    Fused(Box<Fused>),
    QuickSet(Box<QuickSet>),
    Repeat(RepeatSlots),
}

/// The locals a `repeat` loop keeps, and where its pass goes.
#[derive(Debug, Clone)]
pub struct RepeatSlots {
    /// Which of the function's loops this is: its state (kind, items,
    /// limit, counter) lives in the call frame under this number.
    pub id: u16,
    pub item: u16,
    pub key: u16,
    pub val: u16,
    pub idx: u16,
    /// Steps of the condition test that follow RepeatNext.
    pub cond_len: u32,
    /// Steps from just after RepeatNext to the loop's exit.
    pub exit: u32,
}

/// A whole `:update!(path, e)` statement (see `Opcode::QuickSet`).
#[derive(Debug, Clone)]
pub struct QuickSet {
    /// op_data index of the statement's fused check (the target path).
    pub check: u16,
    /// The new value, from the steps `e` compiled to.
    pub expr: QExpr,
    /// Steps to skip when done: the rest of the statement.
    pub skip: u32,
}

/// Number arithmetic over constants, locals and fused path reads.
#[derive(Debug, Clone)]
pub enum QExpr {
    Const(u16),
    Local(u16),
    /// op_data index of a fused read.
    Read(u16),
    /// A read of the statement's own target path: its old value, taken
    /// where the write lands.
    Target,
    Add(Box<QExpr>, Box<QExpr>),
    Sub(Box<QExpr>, Box<QExpr>),
    Mul(Box<QExpr>, Box<QExpr>),
}

/// Steps fused into one `Opcode::Fused`.
#[derive(Debug, Clone)]
pub struct Fused {
    pub kind: FusedKind,
    /// The local the path starts from.
    pub root: u16,
    pub segs: Vec<FusedSeg>,
    /// The original steps, run instead when the quick way can't be taken.
    pub ops: Vec<Opcode>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FusedKind {
    /// `LoadLocal(root)`, then per level a key load and `KeyGet`/`IndexGet`:
    /// pushes the value at the path.
    Read,
    /// `LoadLocal(root) GuardMutable(name) LoadLocal(root)`, the key loads,
    /// `CheckPath(n, info)`: leaves the root and the keys on the stack for
    /// the write that follows.
    Check { guard: u16 },
}

#[derive(Debug, Clone)]
pub struct FusedSeg {
    pub key: FusedKey,
    /// `{}` (a map key) rather than `[]`.
    pub brace: bool,
    /// Where the key was found in this level's map last time.
    pub slot: SlotHint,
}

/// A remembered map slot. Atomic only because compiled code is shared with
/// worker threads; each use is a plain load or store.
#[derive(Debug, Default)]
pub struct SlotHint(std::sync::atomic::AtomicU32);

impl SlotHint {
    #[inline(always)]
    pub fn get(&self) -> u32 { self.0.load(std::sync::atomic::Ordering::Relaxed) }
    #[inline(always)]
    pub fn set(&self, i: u32) { self.0.store(i, std::sync::atomic::Ordering::Relaxed) }
}

impl Clone for SlotHint {
    fn clone(&self) -> Self { SlotHint(std::sync::atomic::AtomicU32::new(self.get())) }
}

#[derive(Debug, Clone, Copy)]
pub enum FusedKey {
    Const(u16),
    Local(u16),
}
