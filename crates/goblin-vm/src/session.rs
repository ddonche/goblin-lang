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
    /// Heap arena: index == Address.slot
    pub arena: Slab<Stash>,

    /// Globals are tethers, not raw values. They point into the arena.
    pub globals: Vec<Tether>,

    /// GC behavior for this session.
    pub gc_mode: GcMode,

    /// Monotonic generation counter.
    next_generation: u32,

    /// Simple allocation counter for Auto GC mode.
    alloc_since_last_gc: usize,

    /// Watermark: how many allocations before an Auto sweep.
    /// You can tune this; starting with something like 10_000 is fine.
    gc_watermark: usize,
}

impl Session {
    /// Create a new session with the given GC mode.
    pub fn new(gc_mode: GcMode) -> Self {
        Session {
            arena: Slab::new(),
            globals: Vec::new(),
            gc_mode,
            next_generation: 1,
            alloc_since_last_gc: 0,
            gc_watermark: 10_000, // TODO: make configurable later
        }
    }

    // ------------------------------------------------------------
    // LEVEL 1.2 — Allocation / Access
    // ------------------------------------------------------------

    /// Allocate a new stash for a Value and return a Tether to it.
    /// This is the canonical way to create values in the VM.
    ///
    /// Callers will usually store the returned Tether in a local/global slot.
    /// NOTE: We treat the returned Tether as the first (and only) live tether.
    /// Internal: check GC mode and maybe run a sweep after an allocation.
    fn maybe_gc_on_alloc(&mut self) {
        match self.gc_mode {
            GcMode::Off => {
                // no-op
            }
            GcMode::Manual => {
                // only explicit gc_sweep() / :gc() calls
            }
            GcMode::Auto => {
                self.alloc_since_last_gc += 1;
                if self.alloc_since_last_gc >= self.gc_watermark {
                    self.gc_sweep();
                    self.alloc_since_last_gc = 0;
                }
            }
        }
    }
        
    pub fn alloc_value(&mut self, value: Value) -> Tether {
        let gen = self.next_generation;
        self.next_generation = self.next_generation.wrapping_add(1);

        let stash = Stash {
            value: Rc::new(value),
            tether_count: 1, // caller holds the first tether
            generation: gen,
        };

        // Slab index is our Address.slot.
        let slot = self.arena.insert(stash) as u32;

        // Auto-GC hook (no-op in Off / Manual).
        self.maybe_gc_on_alloc();

        Tether {
            addr: Address { slot, generation: gen },
        }
    }

    /// Return the raw Address {slot, generation} for a tether.
    /// This is the VM-side backing for :mem_id(x).
    pub fn mem_id_raw(&self, t: &Tether) -> Address {
        // This does NOT validate; the builtin should call get_stash()
        // first if it wants to ensure it's not stale.
        t.addr
    }

    /// Return a hex string of the stash's physical address.
    /// This is the VM-side backing for :mem_addr(x).
    pub fn mem_addr_hex(&self, t: &Tether) -> String {
        let stash_ref: &Stash = self.get_stash(t);
        let ptr = stash_ref as *const Stash as usize;
        format!("0x{:x}", ptr)
    }

    /// Internal helper: resolve an Address to a live stash, with stale detection.
    /// For now this panics on stale / out-of-range; later we can switch to Result<_,Diag>.
    fn resolve_live_stash(&self, addr: Address) -> &Stash {
        let stash = &self.arena[addr.slot as usize];

        // Stale mem_id detection (generation mismatch).
        if stash.generation != addr.generation {
            panic!(
                "stale Address detected (mem_id generation mismatch: stored {}, addr {})",
                stash.generation, addr.generation
            );
        }

        stash
    }

    /// Mutable version of resolve_live_stash.
    fn resolve_live_stash_mut(&mut self, addr: Address) -> &mut Stash {
        let stash = &mut self.arena[addr.slot as usize];

        if stash.generation != addr.generation {
            panic!(
                "stale Address detected (mem_id generation mismatch: stored {}, addr {})",
                stash.generation, addr.generation
            );
        }

        stash
    }

    /// Get immutable access to the Stash for a given Tether.
    /// Panics if stale or out-of-range for now; later we’ll return Result.
    pub fn get_stash(&self, t: &Tether) -> &Stash {
        self.resolve_live_stash(t.addr)
    }

    /// Get mutable access to the Stash for a given Tether.
    pub fn get_stash_mut(&mut self, t: &Tether) -> &mut Stash {
        self.resolve_live_stash_mut(t.addr)
    }

    /// Convenience: clone the Value payload (for read-only introspection).
    pub fn read_value(&self, t: &Tether) -> Value {
        self.get_stash(t).value.as_ref().clone()
    }

    /// When a new tether is created that points at the same stash,
    /// you MUST call this to keep tether_count accurate.
    pub fn inc_tether(&mut self, t: &Tether) {
        let stash = self.get_stash_mut(t);
        stash.tether_count = stash.tether_count.saturating_add(1);
    }

    /// When a tether is dropped, call this to decrement the count.
    /// When tether_count reaches 0, the stash is considered abandoned
    /// and may be reclaimed on the next GC sweep.
    pub fn dec_tether(&mut self, t: &Tether) {
        let stash = self.get_stash_mut(t);
        if stash.tether_count > 0 {
            stash.tether_count -= 1;
        }
        // We do NOT immediately free here; gc_sweep() decides when to reclaim.
    }

    // ------------------------------------------------------------
    // LEVEL 1.3 — GC Skeleton
    // ------------------------------------------------------------

    /// Sweep abandoned stashes (tether_count == 0) and recycle their slots.
    /// This is a simple, stop-the-world sweep; incremental/auto logic comes later.
    ///
    /// NOTE: We rely on `next_generation` to give each new stash a fresh generation,
    /// so stale addresses are detected when a removed slot is reused with a new gen.
    pub fn gc_sweep(&mut self) {
        let mut to_remove = Vec::new();

        for (key, stash) in self.arena.iter() {
            if stash.tether_count == 0 {
                to_remove.push(key);
            }
        }

        for key in to_remove {
            self.arena.remove(key);
        }

        // We do NOT reset or bump generations here; each new allocation gets a new
        // generation from `next_generation`. That is effectively the "per-slot generation"
        // option from the blueprint: same slot + different generation => stale Address.
    }

    // ------------------------------------------------------------
    // LEVEL 1.4 — overwrite! primitive
    // ------------------------------------------------------------

    /// Direct stash mutation primitive.
    /// This is the ONLY place Goblin mutates stash values in place.
    ///
    /// Later, when workers exist, we’ll enforce "owned-by-this-worker" here.
    pub fn overwrite(&mut self, t: &Tether, new_value: Value) {
        let stash = self.get_stash_mut(t);
        stash.value = Rc::new(new_value);
    }

    // ------------------------------------------------------------
    // Deep-copy helper (for future workers / zero-copy boundary)
    // ------------------------------------------------------------

    /// Deep-copy a Value graph into this Session and return a new Tether.
    ///
    /// For now this is shallow for collections; we’ll expand it when
    /// CollectionValue + tethers-inside-collections are fully wired.
    pub fn clone_value_into_session(&mut self, value: &Value) -> Tether {
        match value {
            Value::Nil
            | Value::Bool(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::Str(_) => {
                self.alloc_value(value.clone())
            }

            Value::Collection(coll_rc) => {
                // TEMP: shallow clone of collection payload.
                // Later: walk internal tethers and deep-copy their targets.
                let cloned = Value::Collection(coll_rc.clone());
                self.alloc_value(cloned)
            }

            // Add other Value variants as they appear.
        }
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

    #[test]
    fn generation_diff_detects_stale_address() {
        let mut sess = Session::new(GcMode::Off);
        let t1 = sess.alloc_value(Value::Int(1));

        // Clone the tether to simulate a stale one keeping the old Address.
        let stale = Tether { addr: t1.addr };

        // Drop real stash to free its slot.
        sess.dec_tether(&t1);
        sess.gc_sweep();

        // Allocate a new value, which may reuse the same slot with a new generation.
        let _t2 = sess.alloc_value(Value::Int(2));

        // Using the stale tether should panic (generation mismatch).
        let result = std::panic::catch_unwind(|| {
            sess.read_value(&stale);
        });
        assert!(result.is_err());
    }
}
