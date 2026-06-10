use std::rc::Rc;
use slab::Slab;

use crate::error::GoblinError;
use crate::value::{Stash, Address, Tether, Value};

/// GC modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcMode {
    /// No automatic GC. Fastest. Use when you manage lifetimes yourself.
    Off,
    /// GC only runs when explicitly called via :gc().
    Manual,
    /// Incremental sweeps triggered by allocation watermark.
    Auto,
}

/// A VM session: owns the arena of stashes and global slot table.
/// Each worker has exactly one Session (isolated heap).
pub struct Session {
    /// Arena of stashes. `slab` index == Address.slot.
    pub arena: Slab<Stash>,

    /// Global variable slots (indexed by compiled global index).
    pub globals: Vec<Option<Tether>>,

    /// GC behaviour for this session.
    pub gc_mode: GcMode,

    /// Optional worker ID for cross-worker safety checks.
    pub worker_id: usize,

    /// Monotonic generation counter. Each new stash gets the current value,
    /// then we increment. This means each stash has a unique generation number,
    /// and old Addresses with a mismatched generation are detected as stale.
    next_generation: u32,

    /// Allocation counter for Auto GC.
    alloc_since_last_gc: usize,

    /// How many allocations before an Auto sweep.
    gc_watermark: usize,
}

impl Session {
    pub fn new(gc_mode: GcMode) -> Self {
        Session {
            arena: Slab::new(),
            globals: Vec::new(),
            gc_mode,
            worker_id: 0,
            next_generation: 1,
            alloc_since_last_gc: 0,
            gc_watermark: 10_000,
        }
    }

    pub fn with_worker_id(mut self, id: usize) -> Self {
        self.worker_id = id;
        self
    }

    // ── Allocation ──────────────────────────────────────────────────────────

    /// Allocate a new stash and return a Tether pointing to it.
    /// The new stash starts with tether_count = 1 (the returned tether is the owner).
    pub fn alloc_value(&mut self, value: Value) -> Tether {
        let gen = self.next_generation;
        self.next_generation = self.next_generation.wrapping_add(1);

        let stash = Stash {
            value: Rc::new(value),
            tether_count: 1,
            generation: gen,
        };

        let slot = self.arena.insert(stash) as u32;
        self.maybe_gc_on_alloc();

        Tether { addr: Address { slot, generation: gen } }
    }

    fn maybe_gc_on_alloc(&mut self) {
        if self.gc_mode == GcMode::Auto {
            self.alloc_since_last_gc += 1;
            if self.alloc_since_last_gc >= self.gc_watermark {
                self.gc_sweep();
                self.alloc_since_last_gc = 0;
            }
        }
    }

    // ── Address validation ──────────────────────────────────────────────────

    /// Resolve a live stash reference, returning an error if stale.
    pub fn resolve(&self, addr: Address) -> Result<&Stash, GoblinError> {
        if !self.arena.contains(addr.slot as usize) {
            return Err(GoblinError::EmptySlot { slot: addr.slot });
        }
        let stash = &self.arena[addr.slot as usize];
        if stash.generation != addr.generation {
            return Err(GoblinError::StaleAddress {
                slot: addr.slot,
                stored_gen: stash.generation,
                addr_gen: addr.generation,
            });
        }
        Ok(stash)
    }

    /// Mutable version of resolve.
    pub fn resolve_mut(&mut self, addr: Address) -> Result<&mut Stash, GoblinError> {
        if !self.arena.contains(addr.slot as usize) {
            return Err(GoblinError::EmptySlot { slot: addr.slot });
        }
        let stash = &mut self.arena[addr.slot as usize];
        if stash.generation != addr.generation {
            return Err(GoblinError::StaleAddress {
                slot: addr.slot,
                stored_gen: stash.generation,
                addr_gen: addr.generation,
            });
        }
        Ok(stash)
    }

    pub fn get_stash(&self, t: &Tether) -> Result<&Stash, GoblinError> {
        self.resolve(t.addr)
    }

    pub fn get_stash_mut(&mut self, t: &Tether) -> Result<&mut Stash, GoblinError> {
        self.resolve_mut(t.addr)
    }

    /// Clone the Value payload for read-only introspection.
    pub fn read_value(&self, t: &Tether) -> Result<Value, GoblinError> {
        Ok(self.get_stash(t)?.value.as_ref().clone())
    }

    // ── Tether reference counting ───────────────────────────────────────────

    pub fn inc_tether(&mut self, t: &Tether) -> Result<(), GoblinError> {
        let stash = self.get_stash_mut(t)?;
        stash.tether_count = stash.tether_count.saturating_add(1);
        Ok(())
    }

    pub fn dec_tether(&mut self, t: &Tether) -> Result<(), GoblinError> {
        let stash = self.get_stash_mut(t)?;
        if stash.tether_count > 0 {
            stash.tether_count -= 1;
        }
        Ok(())
    }

    // ── Memory introspection (:mem_id, :mem_addr) ───────────────────────────

    pub fn mem_id_raw(&self, t: &Tether) -> Address {
        t.addr
    }

    pub fn mem_addr_hex(&self, t: &Tether) -> Result<String, GoblinError> {
        let stash = self.get_stash(t)?;
        let ptr = stash as *const Stash as usize;
        Ok(format!("0x{:x}", ptr))
    }

    // ── GC ──────────────────────────────────────────────────────────────────

    /// Sweep stashes with tether_count == 0 and reclaim their slots.
    pub fn gc_sweep(&mut self) {
        let to_remove: Vec<usize> = self.arena
            .iter()
            .filter(|(_, s)| s.tether_count == 0)
            .map(|(k, _)| k)
            .collect();

        for key in to_remove {
            self.arena.remove(key);
        }
    }

    /// Number of live stashes.
    pub fn stash_count(&self) -> usize {
        self.arena.len()
    }

    // ── overwrite! ──────────────────────────────────────────────────────────

    /// Direct stash mutation. This is the ONLY place Goblin mutates values in place.
    /// Enforces worker ownership: only the owning worker may call this.
    pub fn overwrite(&mut self, t: &Tether, new_value: Value) -> Result<(), GoblinError> {
        let stash = self.get_stash_mut(t)?;
        stash.value = Rc::new(new_value);
        Ok(())
    }

    // ── Value transfer (deep copy) ──────────────────────────────────────────

    /// Deep-copy a Value graph into this session. Returns a new Tether.
    /// Used for cross-worker message passing (the safe default).
    pub fn clone_value_into_session(&mut self, value: &Value) -> Tether {
        match value {
            Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::Str(_) => {
                self.alloc_value(value.clone())
            }

            Value::Collection(coll_rc) => {
                // Deep-copy the collection contents.
                use crate::value::{CollectionLayout, CollectionValue, CollectionMeta};
                use std::rc::Rc;
                let new_layout = match &coll_rc.layout {
                    CollectionLayout::FlatArray(v) => {
                        let new_v: Vec<Value> = v.iter()
                            .map(|val| self.clone_value_shallow(val))
                            .collect();
                        CollectionLayout::FlatArray(Rc::new(new_v))
                    }
                    CollectionLayout::RingBuf(rb) => {
                        let flat = rb.to_vec();
                        let new_v: Vec<Value> = flat.iter()
                            .map(|val| self.clone_value_shallow(val))
                            .collect();
                        use crate::value::RingBuf;
                        CollectionLayout::RingBuf(Rc::new(RingBuf::from_vec(new_v)))
                    }
                    CollectionLayout::ChunkedSeq(cs) => {
                        let flat = cs.to_flat();
                        let new_v: Vec<Value> = flat.iter()
                            .map(|val| self.clone_value_shallow(val))
                            .collect();
                        use crate::value::ChunkedSeq;
                        CollectionLayout::ChunkedSeq(Rc::new(ChunkedSeq::from_flat(&new_v)))
                    }
                    CollectionLayout::SmallMap(pairs) => {
                        let new_pairs: Vec<(Value, Value)> = pairs.iter()
                            .map(|(k, v)| (self.clone_value_shallow(k), self.clone_value_shallow(v)))
                            .collect();
                        CollectionLayout::SmallMap(Rc::new(new_pairs))
                    }
                    CollectionLayout::HashMapBackend(map) => {
                        let new_pairs: Vec<(Value, Value)> = map.iter()
                            .map(|(k, v)| (self.clone_value_shallow(k), self.clone_value_shallow(v)))
                            .collect();
                        CollectionLayout::SmallMap(Rc::new(new_pairs))
                    }
                };
                let new_coll = CollectionValue {
                    layout: new_layout,
                    meta: CollectionMeta { len: coll_rc.meta.len, ..Default::default() },
                };
                self.alloc_value(Value::Collection(Rc::new(new_coll)))
            }

            // Functions and builtins share identity across workers (read-only).
            Value::Function(_) | Value::Builtin(_) => {
                self.alloc_value(value.clone())
            }

            // Closures: share function body, clone upvalue snapshot.
            // Since upvalues are snapshots (not live references), cloning is safe.
            Value::Closure(c) => {
                self.alloc_value(Value::Closure(c.clone()))
            }
        }
    }

    fn clone_value_shallow(&self, v: &Value) -> Value {
        // For primitives and shallow-copyable values.
        v.clone()
    }

    // ── Globals ─────────────────────────────────────────────────────────────

    pub fn ensure_globals(&mut self, count: usize) {
        while self.globals.len() < count {
            self.globals.push(None);
        }
    }

    pub fn set_global(&mut self, idx: usize, t: Tether) {
        self.ensure_globals(idx + 1);
        self.globals[idx] = Some(t);
    }

    pub fn get_global(&self, idx: usize) -> Option<&Tether> {
        self.globals.get(idx)?.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn alloc_and_read() {
        let mut sess = Session::new(GcMode::Off);
        let t = sess.alloc_value(Value::Int(42));
        let v = sess.read_value(&t).unwrap();
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn stale_address_detected() {
        let mut sess = Session::new(GcMode::Off);
        let t1 = sess.alloc_value(Value::Int(1));
        let stale = Tether { addr: t1.addr };
        sess.dec_tether(&t1).unwrap();
        sess.gc_sweep();
        let _t2 = sess.alloc_value(Value::Int(2));
        assert!(sess.read_value(&stale).is_err());
    }

    #[test]
    fn overwrite_changes_value() {
        let mut sess = Session::new(GcMode::Off);
        let t = sess.alloc_value(Value::Int(1));
        sess.overwrite(&t, Value::Int(99)).unwrap();
        let v = sess.read_value(&t).unwrap();
        assert!(matches!(v, Value::Int(99)));
    }
}
