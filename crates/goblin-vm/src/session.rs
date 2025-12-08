use std::rc::Rc;
use slab::Slab;

use crate::value::{Stash, Address, Tether, Value};

/// GC modes from the blueprint:
/// - Off: fastest, no automatic sweeps
/// - Manual: only sweep when asked
/// - Auto: incremental sweeps on allocation watermark / tick
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcMode {
    Off,
    Manual,
    Auto,
}

/// A VM session: owns the arena of stashes and globals.
/// Each worker will have its own Session (isolated heap).
pub struct Session {
    pub arena: Slab<Stash>,
    /// Globals are tethers, not raw values. They point into the arena.
    pub globals: Vec<Tether>,
    pub gc_mode: GcMode,
    /// Bumped on sweeps to help invalidate stale Addresses.
    pub generation: u32,
}

impl Session {
    /// Create a new session with the given GC mode.
    pub fn new(gc_mode: GcMode) -> Self {
        Session {
            arena: Slab::new(),
            globals: Vec::new(),
            gc_mode,
            generation: 0,
        }
    }

    /// Allocate a new stash for a Value and return a Tether to it.
    /// This is the canonical way to create values in the VM.
    ///
    /// Callers will usually store the returned Tether in a local/global slot.
    pub fn alloc_value(&mut self, value: Value) -> Tether {
        let stash = Stash {
            value: Rc::new(value),
            tether_count: 1,      // caller holds the first tether
            generation: self.generation,
        };

        let slot = self.arena.insert(stash) as u32;

        Tether {
            addr: Address {
                slot,
                generation: self.generation,
            },
        }
    }

    /// Get immutable access to the Stash for a given Tether.
    /// Panics if stale or out-of-range for now; later we’ll return Result.
    pub fn get_stash(&self, t: &Tether) -> &Stash {
        let addr = t.addr;
        let stash = &self.arena[addr.slot as usize];

        // Stale mem_addr detection (basic)
        if stash.generation != addr.generation {
            panic!("stale Address detected (mem_addr generation mismatch)");
        }

        stash
    }

    /// Convenience: clone the Value payload (for read-only introspection).
    pub fn read_value(&self, t: &Tether) -> Value {
        self.get_stash(t).value.as_ref().clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn alloc_and_read_value() {
        let mut sess = Session::new(GcMode::Off);
        let t = sess.alloc_value(Value::Int(42));

        let v = sess.read_value(&t);
        match v {
            Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("unexpected value"),
        }
    }
}
