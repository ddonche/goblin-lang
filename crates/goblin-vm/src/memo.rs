//! Calls to actions that only read, remembered while a DES decision is
//! worked out.
//!
//! A decision runs its `where` condition and its formula once per target.
//! A part like `strength(self)` gives the same answer for every target, and
//! `strength(target >> owner)` the same for every estate of one holder, but
//! the action ran each time. While a decision is worked out nothing changes
//! the world, so a call to an action that only reads (no writes to fields or
//! variables outside its own, nothing random, no output, and only calls to
//! other such actions) gives the same value for the same arguments: the
//! first answer is kept and used again. An action that doesn't only read
//! runs every time, and while it runs nothing is kept or used, since it may
//! change what the others read.

use std::rc::Rc;
use rustc_hash::FxHashMap;
use crate::opcode::{Opcode, OpData, FusedKind};
use crate::value::{BuiltinId, FunctionObject, Value};

/// One argument, as far as the answer can depend on it.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum Arg {
    Nil,
    Bool(bool),
    Int(i64),
    Float(u64),
    Str(String),
    /// An object: by its id (nothing changes it while the memo is on).
    Obj(String),
}

impl Arg {
    pub(crate) fn of(v: &Value) -> Option<Arg> {
        Some(match v {
            Value::Nil => Arg::Nil,
            Value::Bool(b) => Arg::Bool(*b),
            Value::Int(i) => Arg::Int(*i),
            Value::Float(f) => Arg::Float(f.to_bits()),
            Value::Str(s) => Arg::Str(s.to_string()),
            Value::Ref(u) => Arg::Obj(u.clone()),
            Value::Object(o) => Arg::Obj(o.uuid.clone()),
            _ => return None,
        })
    }
}

pub(crate) type Key = (usize, Vec<Arg>);

/// What a running call does with the memo.
pub(crate) enum Mark {
    /// Keep its answer under this key, if nothing that doesn't only read
    /// ran since the call started (`epoch`).
    Keep(Box<(Key, u64)>),
    /// It may change things: nothing is kept or used until it returns.
    Changes,
}

#[derive(Default)]
pub(crate) struct Memo {
    pub(crate) answers: FxHashMap<Key, Value>,
    /// Goes up whenever something that may change things starts or ends.
    pub(crate) epoch: u64,
    /// Calls that may change things now running.
    pub(crate) changing: u32,
}

impl Memo {
    pub(crate) fn changed(&mut self) {
        self.answers.clear();
        self.epoch += 1;
    }
}

/// Builtins that only read: no effects, no randomness, no output. The
/// object and overlay queries are here: their predicates are the caller's
/// own closures, checked with it.
pub(crate) fn builtin_reads(id: BuiltinId) -> bool {
    use std::sync::OnceLock;
    static IDS: OnceLock<Vec<BuiltinId>> = OnceLock::new();
    IDS.get_or_init(|| {
        let mut ids: Vec<BuiltinId> = crate::compiler::READ_ONLY_BUILTINS.iter()
            .chain(["objects", "overlays", "overlays_of", "type_of", "typeof"].iter())
            .filter_map(|n| crate::compiler::builtin_by_name(n))
            .collect();
        ids.extend([BuiltinId::ObjectsIn, BuiltinId::Objects, BuiltinId::Overlays]);
        ids
    }).contains(&id)
}

/// Whether a function only reads, judged from its code: every step reads
/// or works on its own locals, every builtin it calls only reads, and every
/// closure it makes and every action it loads from a global only reads too.
/// A function that calls anything else (a function given as an argument, a
/// method) doesn't count.
pub(crate) fn reads_only(f: &Rc<FunctionObject>, globals: &dyn Fn(u16) -> Option<Rc<FunctionObject>>, seen: &mut Vec<usize>) -> bool {
    let me = Rc::as_ptr(f) as usize;
    if seen.contains(&me) { return true; }
    seen.push(me);
    if f.calls_values { return false; }
    for op in &f.bytecode {
        let ok = match *op {
            Opcode::LoadConst(_) | Opcode::LoadNil | Opcode::LoadUnit | Opcode::LoadTrue | Opcode::LoadFalse
            | Opcode::LoadLocal(_) | Opcode::TakeLocal(_) | Opcode::StoreLocal(_) | Opcode::BindLocal(_)
            | Opcode::LoadUpvalue(_) | Opcode::Pop | Opcode::Dup
            | Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Rem | Opcode::Neg
            | Opcode::AddInt | Opcode::SubInt | Opcode::MulInt | Opcode::DivInt | Opcode::RemInt | Opcode::NegInt
            | Opcode::AddFloat | Opcode::SubFloat | Opcode::MulFloat | Opcode::DivFloat | Opcode::NegFloat
            | Opcode::Concat | Opcode::Eq | Opcode::Ne | Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge | Opcode::Not
            | Opcode::Jump(_) | Opcode::JumpIfFalse(_) | Opcode::JumpIfTrue(_)
            | Opcode::MakeArray(_) | Opcode::MakeMap(_) | Opcode::GetIndex | Opcode::IndexGet(_) | Opcode::KeyGet(_)
            | Opcode::GetMember(_) | Opcode::JumpIfLocalSet(..) | Opcode::LoadItFieldOrJump(..)
            | Opcode::GuardRebindLocal(..) | Opcode::GuardObjRebind(_) | Opcode::GuardMutable(_)
            | Opcode::Call(_) | Opcode::Return
            | Opcode::ToPct | Opcode::MakePair | Opcode::MakeRange | Opcode::MakeRangeInclusive
            | Opcode::TryBegin(_) | Opcode::TryEnd | Opcode::StringInterpVals(..) | Opcode::SelfField(_)
            | Opcode::TupleSplit(_) | Opcode::ArrayPushToLocal(_) | Opcode::AppendLocal(_)
            | Opcode::RepeatNext(_) | Opcode::RepeatStart(..) | Opcode::RepeatIdx(..) => true,
            Opcode::CallBuiltin(id, _) => builtin_reads(id),
            Opcode::Fused(i) => matches!(f.op_data.get(i as usize), Some(OpData::Fused(x)) if matches!(x.kind, FusedKind::Read)),
            Opcode::MakeClosure(i) => match f.constants.get(i as usize) {
                Some(Value::Function(g)) => reads_only(g, globals, seen),
                _ => false,
            },
            Opcode::LoadGlobal(g) => match globals(g) {
                Some(g) => reads_only(&g, globals, seen),
                None => true,
            },
            _ => false,
        };
        if !ok { return false; }
    }
    true
}
