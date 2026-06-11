use crate::value::BuiltinId;

/// The full Goblin bytecode instruction set.
///
/// Design: register-stack hybrid, same class as CPython 3.12 / Lua 5.x / Wren.
/// Locals live in indexed slots; expressions use the operand stack.
///
/// Quickening: generic ops (Add, Sub, …) are replaced at runtime with
/// specialised forms (AddInt, AddFloat, Concat) once operand types are known.
#[derive(Debug, Clone)]
pub enum Opcode {
    // ── Literals ───────────────────────────────────────────────────────────────
    /// Push constants[idx] onto the stack (allocates a new stash).
    LoadConst(u16),
    /// Push Value::Nil.
    LoadNil,
    /// Push Value::Bool(true).
    LoadTrue,
    /// Push Value::Bool(false).
    LoadFalse,

    // ── Locals (tether slots) ──────────────────────────────────────────────────
    /// Push a copy of the tether in locals[slot].
    LoadLocal(u8),
    /// Pop top and store into locals[slot].
    /// Implements |, |=, and [= — the compiler chooses the opcode at compile time;
    /// at runtime they are all the same operation (store tether into slot).
    StoreLocal(u8),

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

    /// Like GetIndex but key is constants[idx] (string member access).
    GetMember(u16),
    /// Pop new_val, pop object → push updated object with field set. Key is constants[idx].
    SetField(u16),
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

    // ── DES statement opcodes (carry AST data inline) ──────────────────────
    /// Register an overlay definition in session.overlay_defs.
    OverlayDef(Box<goblin_ast::OverlayDefStmt>),
    /// Apply an overlay to a host (host value must be on stack).
    OverlayApply { overlay_name: String, strength: f64, duration_override: Option<u32> },
    /// Detach an overlay from a host (host variable name on stack).
    OverlayDetach { overlay_name: String },
    /// Register a class-level link definition.
    LinkDef(Box<goblin_ast::LinkDefStmt>),
    /// Register an object-level link definition override.
    ObjectLinkDef(Box<goblin_ast::ObjectLinkDefStmt>),
    /// Apply a link offset (from_var, to_var, channel, offset, ticks on stack as consts).
    LinkOffset(Box<goblin_ast::LinkOffsetStmt>),
    /// Clear link offsets.
    ClearLink(Box<goblin_ast::ClearLinkStmt>),
    /// Register an object decision formula.
    ObjectDecision { var_name: String, def: Box<goblin_ast::DecisionDef> },
    /// Register a unit declaration.
    UnitDecl(Box<goblin_ast::UnitDecl>),

    /// Peek at top-of-stack and register it into session.named_values[constants[name_idx]].
    /// Does NOT pop — the value stays on stack for the subsequent StoreLocal.
    RegisterAction(u16),

    /// Interpolate a string constant: constants[idx] is a raw template string.
    /// Looks up {ident} placeholders in the current locals/globals scope at runtime.
    StringInterp(u16),

    /// Load a field from self (locals[0], which is the receiver object in a class method).
    /// constants[idx] is the field name string. Pushes nil if self is not an Object or field absent.
    SelfField(u16),
}

impl Opcode {
    /// Returns the opcode name as a static string (for the disassembler).
    pub fn name(&self) -> &'static str {
        match self {
            Opcode::LoadConst(_)    => "LoadConst",
            Opcode::LoadNil         => "LoadNil",
            Opcode::LoadTrue        => "LoadTrue",
            Opcode::LoadFalse       => "LoadFalse",
            Opcode::LoadLocal(_)    => "LoadLocal",
            Opcode::StoreLocal(_)   => "StoreLocal",
            Opcode::LoadGlobal(_)   => "LoadGlobal",
            Opcode::StoreGlobal(_)  => "StoreGlobal",
            Opcode::LoadUpvalue(_)  => "LoadUpvalue",
            Opcode::StoreUpvalue(_) => "StoreUpvalue",
            Opcode::Pop             => "Pop",
            Opcode::Dup             => "Dup",
            Opcode::Overwrite       => "Overwrite",
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
            Opcode::SetIndex        => "SetIndex",
            Opcode::GetMember(_)    => "GetMember",
            Opcode::SetField(_)     => "SetField",
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
            Opcode::OverlayDef(_)     => "OverlayDef",
            Opcode::OverlayApply {..} => "OverlayApply",
            Opcode::OverlayDetach {..} => "OverlayDetach",
            Opcode::LinkDef(_)        => "LinkDef",
            Opcode::ObjectLinkDef(_)  => "ObjectLinkDef",
            Opcode::LinkOffset(_)     => "LinkOffset",
            Opcode::ClearLink(_)      => "ClearLink",
            Opcode::ObjectDecision {..} => "ObjectDecision",
            Opcode::UnitDecl(_)       => "UnitDecl",
            Opcode::RegisterAction(_) => "RegisterAction",
            Opcode::StringInterp(_)   => "StringInterp",
            Opcode::SelfField(_)      => "SelfField",
        }
    }
}
