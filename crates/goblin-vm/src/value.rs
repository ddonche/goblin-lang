use std::collections::BTreeMap;
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

    /// Arrays/maps hold *tethers* to element/value stashes.
    /// That keeps the model consistent: everything lives in a stash, and
    /// collections just store more tethers.
    Array(Vec<Tether>),
    Map(BTreeMap<Tether, Tether>),

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
