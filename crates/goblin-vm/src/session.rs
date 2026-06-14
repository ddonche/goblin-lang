use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
use slab::Slab;

use crate::error::GoblinError;
use crate::grid::GridStore;
use crate::value::{Stash, Address, Tether, Value};

pub use goblin_ast::{ClassDecl, EnumDecl};
pub use goblin_des::store::EntityStore;
pub use goblin_des::index::{EntityIndex, LinkId, OverlayInstanceId};
pub use goblin_des::tick::TickRunner;

// ── Overlay runtime types ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct OverlayDef {
    pub name: String,
    pub host_types: Vec<String>,
    pub decay_rate: f64,
    pub default_duration: Option<u32>,
    pub apply_behavior: OverlayApplyBehavior,
    pub modifiers: Vec<(String, goblin_ast::Expr)>,
    pub conflict_rules: Vec<(String, f64)>,     // (opponent, suppress_rate)
    pub spread_rules: Vec<goblin_ast::SpreadRule>,
    pub spawn_rules: Vec<goblin_ast::OverlaySpawnRule>,
    pub transitions: Vec<goblin_ast::TransitionDef>,
    pub extra_fields: indexmap::IndexMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OverlayApplyBehavior { Caps, Replaces, Stacks { label: Option<String> } }

impl Default for OverlayApplyBehavior { fn default() -> Self { OverlayApplyBehavior::Caps } }

#[derive(Debug, Clone)]
pub struct OverlayInstance {
    pub overlay_name: String,
    pub host_var: String,
    pub host_uuid: String,
    pub strength: f64,
    pub age: u32,
    pub ticks_remaining: Option<u32>,
    pub count: u64,
    pub original_values: Vec<(String, Value)>,
    pub extra_fields: indexmap::IndexMap<String, Value>,
    pub des_id: OverlayInstanceId,
}

// ── Link runtime types ───────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct LinkDef {
    pub class_name: String,
    pub channel: String,
    pub formula: goblin_ast::Expr,
    pub formula_min: f64,
    pub formula_max: f64,
}

#[derive(Debug, Clone)]
pub struct LinkOffset {
    pub value: f64,
    pub ticks_remaining: Option<u32>,
}

/// HTTP response state accumulated during a request.
#[derive(Debug, Clone, Default)]
pub struct ResponseState {
    pub status: Option<i64>,
    pub headers: indexmap::IndexMap<String, String>,
    pub cookies: Vec<String>,
}

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
    /// Name for each global slot (slot index → name), for string interpolation.
    pub global_names: Vec<String>,

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

    /// PRNG state for builtins like shuffle/mixed. LCG/MCG.
    pub rng_state: u128,

    /// Token store: namespace → key → value.
    pub token_store: BTreeMap<String, BTreeMap<String, Value>>,

    /// Object store: uuid → Value (for DES/overlay system).
    pub object_store: HashMap<String, Value>,

    /// Active overlay instances.
    pub overlay_instances: Vec<OverlayInstance>,

    /// HTTP response state.
    pub response: ResponseState,

    /// Grid worlds.
    pub grid_store: GridStore,

    /// Class definitions registered at runtime.
    pub classes: HashMap<String, ClassDecl>,

    /// Pre-compiled class methods: (class_name, method_name) -> FunctionObject
    pub compiled_methods: HashMap<(String, String), std::rc::Rc<crate::value::FunctionObject>>,

    /// Enum definitions registered at runtime.
    pub enums: HashMap<String, EnumDecl>,

    /// Base directory for resolving import paths.
    pub base_dir: std::path::PathBuf,

    /// Set of already-imported paths (to avoid re-importing).
    pub imported: std::collections::HashSet<String>,

    // ── DES / Overlay / Link state ───────────────────────────────────────────
    pub overlay_defs: HashMap<String, OverlayDef>,
    pub link_defs: HashMap<(String, String), LinkDef>,
    pub object_link_defs: HashMap<(String, String), LinkDef>,
    pub link_offsets: HashMap<(String, String, String), Vec<LinkOffset>>,
    pub object_decisions: HashMap<String, goblin_ast::DecisionDef>,
    pub unit_registry: HashMap<String, goblin_ast::UnitDecl>,
    pub des_store: EntityStore,
    pub des_index: EntityIndex,
    pub des_tick_runner: TickRunner,
    pub des_overlay_id_counter: u32,
    pub des_link_id_counter: u32,
    pub des_link_ids: HashMap<(String, String, String), LinkId>,

    /// Named function registry for `invoke`/`summon`/`provoke`.
    /// Top-level action declarations are registered here by name.
    pub named_values: HashMap<String, Value>,

    /// Box store: namespace::name → Value (cross-module mutable state).
    pub box_store: HashMap<String, Value>,

    /// Output buffer — when Some, say/print write here instead of stdout.
    /// Used by the WASM REPL to capture output.
    pub output_buf: Option<String>,
}

impl Session {
    pub fn new(gc_mode: GcMode) -> Self {
        // Seed from system time if available, else use a fixed constant.
        #[cfg(target_arch = "wasm32")]
        let seed = 0x123456789abcdef0u128 | 1;

        #[cfg(not(target_arch = "wasm32"))]
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.subsec_nanos() as u128 | ((d.as_secs() as u128) << 32))
            .unwrap_or(0x123456789abcdef0u128)
            | 1; // MCG requires odd seed
        Session {
            arena: Slab::new(),
            globals: Vec::new(),
            global_names: Vec::new(),
            gc_mode,
            worker_id: 0,
            next_generation: 1,
            alloc_since_last_gc: 0,
            gc_watermark: 10_000,
            rng_state: seed,
            token_store: BTreeMap::new(),
            object_store: HashMap::new(),
            overlay_instances: Vec::new(),
            response: ResponseState::default(),
            grid_store: GridStore::new(),
            classes: HashMap::new(),
            compiled_methods: HashMap::new(),
            enums: HashMap::new(),
            base_dir: std::env::current_dir().unwrap_or_default(),
            imported: std::collections::HashSet::new(),
            overlay_defs: HashMap::new(),
            link_defs: HashMap::new(),
            object_link_defs: HashMap::new(),
            link_offsets: HashMap::new(),
            object_decisions: HashMap::new(),
            unit_registry: HashMap::new(),
            des_store: EntityStore::new(),
            des_index: EntityIndex::default(),
            des_tick_runner: TickRunner::new(),
            des_overlay_id_counter: 0,
            des_link_id_counter: 0,
            des_link_ids: HashMap::new(),
            named_values: HashMap::new(),
            box_store: HashMap::new(),
            output_buf: None,
        }
    }

    /// Enable output capture (used by WASM REPL).
    pub fn enable_output_capture(&mut self) {
        self.output_buf = Some(String::new());
    }

    /// Take the captured output, leaving the buffer empty.
    pub fn take_output(&mut self) -> String {
        self.output_buf.take().unwrap_or_default()
    }

    /// Write a line to the output buffer if capture is enabled, else to stdout.
    pub fn write_output(&mut self, s: &str, newline: bool) {
        if let Some(ref mut buf) = self.output_buf {
            buf.push_str(s);
            if newline { buf.push('\n'); }
        } else if newline {
            println!("{}", s);
        } else {
            print!("{}", s);
        }
    }

    /// LCG/MCG PRNG — returns next pseudo-random u128.
    pub fn next_u128(&mut self) -> u128 {
        self.rng_state = self.rng_state
            .wrapping_mul(0x2360ED051FC65DA44385DF649FCCF645);
        self.rng_state
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
        let v = self.get_stash(t)?.value.as_ref().clone();
        // Auto-deref Ref through object_store (matches interpreter's get_var semantics).
        match v {
            Value::Ref(ref uuid) => self.object_store.get(uuid.as_str())
                .cloned()
                .ok_or_else(|| crate::error::GoblinError::Runtime(
                    format!("dangling ref: uuid {} not in object_store", uuid)
                )),
            other => Ok(other),
        }
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

    /// Mark-sweep GC: free all stashes whose slot is NOT in `live_slots`.
    /// Called by the VM after collecting all reachable tether addresses.
    pub fn gc_mark_sweep(&mut self, live_slots: &std::collections::HashSet<u32>) {
        let to_remove: Vec<usize> = self.arena
            .iter()
            .filter(|(slot, _)| !live_slots.contains(&(*slot as u32)))
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
            // Primitives and simple values — clone directly
            Value::Nil | Value::Unit | Value::Bool(_) | Value::Int(_) | Value::Float(_)
            | Value::Big(_) | Value::Pct(_) | Value::Char(_) | Value::Str(_)
            | Value::CtrlSkip | Value::CtrlStop
            | Value::Ref(_) | Value::GridRef { .. } | Value::Class { .. } => {
                self.alloc_value(value.clone())
            }

            // CtrlReturn: deep clone the inner value
            Value::CtrlReturn(inner) => {
                let inner_cloned = self.clone_value_shallow(inner);
                self.alloc_value(Value::CtrlReturn(Box::new(inner_cloned)))
            }

            // Formatted: deep clone inner
            Value::Formatted(inner, spec) => {
                let inner_cloned = self.clone_value_shallow(inner);
                self.alloc_value(Value::Formatted(Box::new(inner_cloned), spec.clone()))
            }

            // Array: deep clone elements
            Value::Array(items) => {
                let new_items: Vec<Value> = items.iter()
                    .map(|v| self.clone_value_shallow(v))
                    .collect();
                self.alloc_value(Value::Array(new_items))
            }

            // Map: deep clone values
            Value::Map(m) => {
                let new_m: std::collections::BTreeMap<String, Value> = m.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::Map(new_m))
            }

            // MapOrd: deep clone values
            Value::MapOrd(m) => {
                let new_m: indexmap::IndexMap<String, Value> = m.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::MapOrd(new_m))
            }

            // Pair: deep clone both
            Value::Pair(k, v) => {
                let k2 = self.clone_value_shallow(k);
                let v2 = self.clone_value_shallow(v);
                self.alloc_value(Value::Pair(Box::new(k2), Box::new(v2)))
            }

            // Seq: deep clone items
            Value::Seq(seq) => {
                let new_items: Vec<Value> = seq.items.iter()
                    .map(|v| self.clone_value_shallow(v))
                    .collect();
                self.alloc_value(Value::Seq(crate::value::Seq::from_vec(new_items)))
            }

            // Object: deep clone fields
            Value::Object { class_name, fields, readonly_fields, trait_fields, uuid } => {
                let new_fields: indexmap::IndexMap<String, Value> = fields.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::Object {
                    class_name: class_name.clone(),
                    fields: std::rc::Rc::new(new_fields),
                    readonly_fields: readonly_fields.clone(),
                    trait_fields: trait_fields.clone(),
                    uuid: uuid.clone(),
                })
            }

            // Enum: deep clone fields
            Value::Enum { enum_name, variant_name, fields } => {
                let new_fields = fields.as_ref().map(|f| {
                    f.iter().map(|(k, v)| (k.clone(), self.clone_value_shallow(v))).collect()
                });
                self.alloc_value(Value::Enum {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    fields: new_fields,
                })
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
