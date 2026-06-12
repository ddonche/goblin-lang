use std::rc::Rc;
use slab::Slab;

use crate::value::{Stash, Address, Tether, Value};

/// GC modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcMode {
    /// No automatic sweeps — fastest (Sheriff default).
    Off,
    /// Only sweep when :gc() is called explicitly.
    Manual,
    /// Incremental sweeps triggered by allocation watermark.
    Auto,
}

/// A VM session: owns the arena of stashes for one worker.
/// Each worker has its own Session — no sharing, no locks.
pub struct Session {
    pub arena: Slab<Stash>,
    pub globals: Vec<Tether>,
    pub gc_mode: GcMode,
    next_generation: u32,
    alloc_since_last_gc: usize,
    gc_watermark: usize,
}

impl Session {
    pub fn new(gc_mode: GcMode) -> Self {
        Session {
            arena: Slab::new(),
            globals: Vec::new(),
            gc_mode,
            next_generation: 1,
            alloc_since_last_gc: 0,
            gc_watermark: 10_000,
        }
    }

    // ── Allocation ────────────────────────────────────────────────────

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
        if let GcMode::Auto = self.gc_mode {
            self.alloc_since_last_gc += 1;
            if self.alloc_since_last_gc >= self.gc_watermark {
                self.gc_sweep();
                self.alloc_since_last_gc = 0;
            }
        }
    }

    // ── Stash access ──────────────────────────────────────────────────

    pub fn get_stash(&self, t: &Tether) -> &Stash {
        let stash = &self.arena[t.addr.slot as usize];
        assert_eq!(
            stash.generation, t.addr.generation,
            "stale Address (slot {}, gen {} vs {})",
            t.addr.slot, t.addr.generation, stash.generation
        );
        stash
    }

    pub fn get_stash_mut(&mut self, t: &Tether) -> &mut Stash {
        let slot = t.addr.slot as usize;
        let gen  = t.addr.generation;
        let stash = &mut self.arena[slot];
        assert_eq!(stash.generation, gen, "stale Address (slot {slot}, gen {gen} vs {})", stash.generation);
        stash
    }

    /// Clone the value payload (read-only).
    pub fn read_value(&self, t: &Tether) -> Value {
        self.get_stash(t).value.as_ref().clone()
    }

    /// :mem_addr(x) — raw pointer address of the stash (diagnostic only).
    pub fn mem_addr_hex(&self, t: &Tether) -> String {
        let ptr = self.get_stash(t) as *const Stash as usize;
        format!("0x{:x}", ptr)
    }

    /// :mem_id(x) — logical {slot, generation}.
    pub fn mem_id(&self, t: &Tether) -> Address { t.addr }

    // ── Tether counting ───────────────────────────────────────────────

    pub fn inc_tether(&mut self, t: &Tether) {
        self.get_stash_mut(t).tether_count = self.get_stash_mut(t).tether_count.saturating_add(1);
    }

    pub fn dec_tether(&mut self, t: &Tether) {
        let stash = self.get_stash_mut(t);
        if stash.tether_count > 0 { stash.tether_count -= 1; }
    }

    // ── overwrite! — the only mutation primitive ───────────────────────

    /// Direct stash mutation (overwrite!).  Caller must own the stash.
    pub fn overwrite(&mut self, t: &Tether, new_value: Value) {
        self.get_stash_mut(t).value = Rc::new(new_value);
    }

    // ── GC ────────────────────────────────────────────────────────────

    pub fn gc_sweep(&mut self) {
        let dead: Vec<usize> = self.arena.iter()
            .filter(|(_, s)| s.tether_count == 0)
            .map(|(k, _)| k)
            .collect();
        for k in dead { self.arena.remove(k); }
    }

    // ── Deep-copy helper ──────────────────────────────────────────────

    pub fn clone_value_into_session(&mut self, value: &Value) -> Tether {
        self.alloc_value(value.clone())
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
        match sess.read_value(&t) {
            Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("wrong value"),
        }
    }

    #[test]
    fn stale_address_panics() {
        let mut sess = Session::new(GcMode::Off);
        let t1 = sess.alloc_value(Value::Int(1));
        let stale = Tether { addr: t1.addr };
        sess.dec_tether(&t1);
        sess.gc_sweep();
        let _t2 = sess.alloc_value(Value::Int(2));
        let result = std::panic::catch_unwind(|| {
            sess.read_value(&stale);
        });
        assert!(result.is_err());
    }
}
