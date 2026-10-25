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

/// The smallest arena size that triggers a collection.
pub const GC_MIN_THRESHOLD: usize = 1 << 16;

/// Roughly how many bytes of strings and collections may be stored into new
/// slots before a collection runs, however few slots that took: a loop
/// rewriting a large string makes one dead copy per pass.
pub const GC_BYTE_BUDGET: usize = 64 << 20;

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
    pub global_names: Rc<Vec<String>>,

    /// GC behaviour for this session.
    pub gc_mode: GcMode,

    /// Optional worker ID for cross-worker safety checks.
    pub worker_id: usize,

    /// Actors this session started; they end when it does (`actor.rs`).
    pub owned_actors: Vec<std::sync::Arc<crate::actor::ActorShared>>,
    /// Portable copies of this session's compiled functions, made once and
    /// reused by every actor or swarm started from it (`ToPortable::function`).
    /// Holding the `Rc` keeps each key's address from being reused.
    pub(crate) portable_funcs: std::cell::RefCell<HashMap<*const crate::value::FunctionObject,
        (std::rc::Rc<crate::value::FunctionObject>, std::sync::Arc<crate::portable::PFunction>)>>,
    /// Portable copies of this session's collections and long strings that
    /// hold no session state, keyed by buffer (`ToPortable::sharing_values`).
    /// Holding the value keeps its address and, through copy-on-write, its
    /// contents unchanged while it is cached.
    pub(crate) portable_values: std::cell::RefCell<HashMap<*const (),
        (Value, std::sync::Arc<crate::portable::PValue>)>>,
    /// Size of `portable_values` after its last clean-up.
    pub(crate) portable_values_kept: std::cell::Cell<usize>,
    /// Large collections known to hold no code, so working out an actor's
    /// globals does not walk them again (`reach.rs`). Holding each keeps it
    /// unchanged: a change to it copies it first.
    pub(crate) plain_collections: std::cell::RefCell<HashMap<*const crate::value::CollectionValue,
        Rc<crate::value::CollectionValue>>>,
    pub(crate) plain_collections_kept: std::cell::Cell<usize>,
    /// In an actor: the globals left out of its copy because its code
    /// cannot reach them, so reading one says so.
    pub(crate) omitted_globals: Vec<bool>,
    /// The program tables last converted for an actor (`swarm::program_of`).
    pub(crate) portable_program: std::cell::RefCell<Option<crate::swarm::ProgramCache>>,
    /// Objects handed to an actor, and the variable each came from, so
    /// reading one says where it went.
    pub handed_off: HashMap<String, String>,
    /// In an actor: objects that stayed with the program that started it
    /// (uuid -> the variable that names them there), for a clear error.
    pub kept_home: HashMap<String, String>,
    /// The main program's world of regions (`tick(regions)`).
    pub world: Option<crate::world::World>,
    /// In a region, while it runs a world tick.
    pub(crate) region: Option<crate::world::RegionCtx>,
    /// Compiled field defaults that are not plain literals, per (class, field).
    /// Cleared whenever a class is (re)declared.
    pub field_defaults: HashMap<(String, String), std::rc::Rc<crate::value::FunctionObject>>,

    /// Monotonic generation counter. Each new stash gets the current value,
    /// then we increment. This means each stash has a unique generation number,
    /// and old Addresses with a mismatched generation are detected as stale.
    next_generation: u32,

    /// Arena size at which the VM runs its next mark-sweep: twice what the
    /// last one kept, and never below `GC_MIN_THRESHOLD`.
    gc_threshold: usize,

    /// Approximate bytes stored into slots since the last collection.
    gc_bytes: usize,

    /// Set once a closure has captured a variable; until then no value can
    /// hold an arena slot, and the collector skips tracing values.
    pub has_cells: bool,

    /// PRNG state for builtins like shuffle/mixed. LCG/MCG.
    pub rng_state: u128,

    /// Token store: namespace → key → value.
    pub token_store: BTreeMap<String, BTreeMap<String, Value>>,
    /// Sweeps in progress (see sweep.rs).
    pub sweeps: crate::sweep::Sweeps,

    /// Object store: uuid → Value (for DES/overlay system).
    /// Objects in creation order, so DES passes and `:objects()` visit them
    /// the same way every run (the interpreter keeps creation order too).
    /// Fx hashing: object ids are long strings looked up on every field
    /// read through a reference; order is insertion order either way.
    pub object_store: indexmap::IndexMap<String, Value, rustc_hash::FxBuildHasher>,
    /// The variable each object was first stored in (uuid -> name, and
    /// whether that variable is top-level). DES
    /// overlays report their host by this name (`host_var`), as the
    /// interpreter does, also for overlays that spread or spawn onto it.
    pub object_names: HashMap<String, (String, bool)>,

    /// Active overlay instances.
    pub overlay_instances: Vec<OverlayInstance>,

    /// HTTP response state.
    pub response: ResponseState,

    /// Grid worlds.
    pub grid_store: GridStore,

    /// Class definitions registered at runtime.
    pub classes: Rc<HashMap<String, ClassDecl>>,

    /// Pre-compiled class methods: (class_name, method_name) -> FunctionObject
    pub compiled_methods: Rc<HashMap<(String, String), std::rc::Rc<crate::value::FunctionObject>>>,
    /// DES ticks run so far (`every N ticks` counts on it).
    pub des_ticks: i64,

    /// Enum definitions registered at runtime.
    pub enums: Rc<HashMap<String, EnumDecl>>,

    /// Base directory for resolving relative `import` paths.
    /// Changes to the importing file's directory during nested imports.
    pub base_dir: std::path::PathBuf,

    /// Project root — where `glams/` lives.
    /// Set once at session creation, never modified.
    /// `use X` always resolves `glams/X/` relative to this.
    pub project_root: std::path::PathBuf,

    /// Set of already-imported paths (to avoid re-importing).
    pub imported: Rc<std::collections::HashSet<String>>,

    // ── DES / Overlay / Link state ───────────────────────────────────────────
    // Definitions are shared (Rc), so a tick that copies a table copies
    // pointers, and each formula keeps one address for the tick-expr cache.
    pub overlay_defs: HashMap<String, Rc<OverlayDef>>,
    pub link_defs: HashMap<(String, String), Rc<LinkDef>, rustc_hash::FxBuildHasher>,
    pub object_link_defs: HashMap<(String, String), Rc<LinkDef>, rustc_hash::FxBuildHasher>,
    pub link_offsets: HashMap<(String, String, String), Vec<LinkOffset>, rustc_hash::FxBuildHasher>,
    pub object_decisions: HashMap<String, Rc<goblin_ast::DecisionDef>>,
    pub unit_registry: Rc<HashMap<String, goblin_ast::UnitDecl>>,
    pub des_store: EntityStore,
    pub des_index: EntityIndex,
    pub des_tick_runner: TickRunner,
    pub des_overlay_id_counter: u32,
    pub des_link_id_counter: u32,
    pub des_link_ids: HashMap<(String, String, String), LinkId>,

    /// Type lock: current cast type per global slot index (updated by cast-bang and declaration).
    pub global_type_locks: Rc<HashMap<u32, String>>,
    /// Hard type lock: declared lock type per global slot index (set only at declaration, never changes).
    pub global_hard_type_locks: Rc<HashMap<u32, String>>,

    /// Named function registry for `invoke`/`summon`/`provoke` and GLAM namespace dispatch.
    /// Actions are stored under both bare names ("create_shard") and qualified names
    /// ("graveyard::create_shard") when registered during a `use glam` import.
    pub named_values: Rc<HashMap<String, Value>>,

    /// The namespace of the GLAM currently being imported, if any.
    /// Set by UseGlam before importing the GLAM entry file; cleared after.
    /// Nested use statements save/restore this so inner GLAMs register correctly.
    pub current_glam_ns: Option<String>,
    /// While a `use`d GLAM's top-level code runs: its namespace, and the
    /// names its glam.toml `[provides]` lists (None: no list). Box writes
    /// outside them raise B0102/B0105, as in the interpreter.
    pub box_namespace: Option<String>,
    pub box_provides: Option<std::collections::HashSet<String>>,
    /// `import path as alias`: (importing file, alias) → the module's global-name
    /// prefix, so `alias::var` reads the module's live global.
    pub module_aliases: Rc<HashMap<(String, String), String>>,
    /// `import path as alias`: the path (resolved against its directory) →
    /// the module's global-name prefix. A request imports the same few
    /// modules from many files; this resolves each path on disk once.
    pub import_paths: HashMap<std::path::PathBuf, String>,

    /// Box store: namespace::name → Value (cross-module mutable state).
    pub box_store: HashMap<String, Value>,

    /// Action needs declared in glam.toml [needs.actions]: glam_namespace -> (need_name -> "provider_ns::action").
    pub action_needs: Rc<HashMap<String, HashMap<String, String>>>,

    /// Tracks which (bare_name, Value) pairs were registered from each source file.
    /// Used by UseGlam to retroactively register qualified "ns::name" keys even when
    /// the GLAM entry file was already imported (import guard) without a namespace context.
    pub action_file_map: Rc<HashMap<String, Vec<(String, Value)>>>,

    /// Output buffer — when Some, say/print write here instead of stdout.
    /// Used by the WASM REPL to capture output.
    pub output_buf: Option<String>,
}

impl Session {
    /// `#ns::name | v` (tether) or `|= v`: the interpreter's Box write rules.
    /// A GLAM's top-level code writes only into its own namespace (B0102)
    /// and only names its `[provides]` lists (B0105); a tether to a name
    /// already set is B0103.
    pub fn box_write(&mut self, ns: &str, name: &str, val: Value, tether: bool) -> Result<(), GoblinError> {
        self.box_check(ns, name)?;
        let key = format!("{ns}::{name}");
        if tether && self.box_store.contains_key(&key) {
            return Err(GoblinError::Runtime(format!(
                "B0103: box-already-set: Box variable '#{key}' is already set — use '|=' to reassign")));
        }
        self.box_store.insert(key, val);
        Ok(())
    }

    /// The namespace/[provides] half of `box_write`, which a `#ns::x | v`
    /// statement checks before evaluating v (as the interpreter does).
    pub fn box_check(&self, ns: &str, name: &str) -> Result<(), GoblinError> {
        if let Some(cur) = &self.box_namespace {
            if cur != ns {
                return Err(GoblinError::Runtime(format!(
                    "B0102: box-namespace-violation: GLAM '{cur}' cannot write into namespace '{ns}'")));
            }
            if let Some(provides) = &self.box_provides {
                if !provides.contains(name) {
                    return Err(GoblinError::Runtime(format!(
                        "B0105: box-undeclared-provide: GLAM '{cur}' cannot publish '#{ns}::{name}' — '{name}' is not declared in [provides]")));
                }
            }
        }
        Ok(())
    }

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
            global_names: Default::default(),
            gc_mode,
            worker_id: 0,
            owned_actors: Vec::new(),
            portable_funcs: Default::default(),
            portable_values: Default::default(),
            portable_values_kept: Default::default(),
            portable_program: Default::default(),
            plain_collections: Default::default(),
            plain_collections_kept: Default::default(),
            omitted_globals: Vec::new(),
            handed_off: HashMap::new(),
            kept_home: HashMap::new(),
            world: None,
            region: None,
            field_defaults: HashMap::new(),
            next_generation: 1,
            gc_threshold: GC_MIN_THRESHOLD,
            gc_bytes: 0,
            has_cells: false,
            rng_state: seed,
            token_store: BTreeMap::new(),
            sweeps: Default::default(),
            object_store: Default::default(),
            object_names: HashMap::new(),
            overlay_instances: Vec::new(),
            response: ResponseState::default(),
            grid_store: GridStore::new(),
            classes: Default::default(),
            compiled_methods: Default::default(),
            des_ticks: 0,
            enums: Default::default(),
            base_dir: std::env::current_dir().unwrap_or_default(),
            project_root: std::env::current_dir().unwrap_or_default(),
            imported: Default::default(),
            overlay_defs: HashMap::new(),
            link_defs: Default::default(),
            object_link_defs: Default::default(),
            link_offsets: Default::default(),
            object_decisions: HashMap::new(),
            unit_registry: Default::default(),
            des_store: EntityStore::new(),
            des_index: EntityIndex::default(),
            des_tick_runner: TickRunner::new(),
            des_overlay_id_counter: 0,
            des_link_id_counter: 0,
            des_link_ids: HashMap::new(),
            global_type_locks: Default::default(),
            global_hard_type_locks: Default::default(),
            named_values: Default::default(),
            current_glam_ns: None,
            box_namespace: None,
            box_provides: None,
            module_aliases: Default::default(),
            import_paths: HashMap::new(),
            box_store: HashMap::new(),
            action_needs: Default::default(),
            action_file_map: Default::default(),
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
    /// End every actor this session started, after each finishes the
    /// messages it already accepted.
    pub fn finish_actors(&mut self) {
        crate::actor::finish_owned(&mut self.owned_actors);
    }

    /// Cancel every actor this session started (without waiting for them).
    pub fn cancel_actors(&mut self) {
        crate::actor::cancel_owned(&mut self.owned_actors);
    }

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

        // Text counts once: a string already charged, now shared with another
        // variable (`copy | page`), costs no new memory.
        self.gc_bytes += match &value {
            Value::Str(text) if text.take_uncharged() => text.len(),
            Value::Collection(c) => c.len() * 32,
            _ => 0,
        };

        let stash = Stash {
            value,
            tether_count: 1,
            generation: gen,
        };

        let slot = self.arena.insert(stash) as u32;

        Tether { addr: Address { slot, generation: gen } }
    }

    /// Whether the arena has grown enough for the VM to collect.
    #[inline]
    pub fn gc_due(&self) -> bool {
        self.arena.len() >= self.gc_threshold || self.gc_bytes >= GC_BYTE_BUDGET
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
    /// The errors `read_value` would give, without copying the value: a
    /// statement like a bare `den` must still fail when `den` was handed
    /// to an actor.
    pub fn check_readable(&self, t: &Tether) -> Result<(), GoblinError> {
        match &self.get_stash(t)?.value {
            Value::Ref(uuid) if !self.object_store.contains_key(uuid.as_str()) =>
                Err(crate::actor::missing_object(self, uuid)),
            _ => Ok(()),
        }
    }

    pub fn read_value(&self, t: &Tether) -> Result<Value, GoblinError> {
        // Auto-deref Ref through object_store (matches interpreter's get_var semantics).
        match &self.get_stash(t)?.value {
            Value::Ref(uuid) => self.object_store.get(uuid.as_str())
                .cloned()
                .ok_or_else(|| crate::actor::missing_object(self, uuid)),
            Value::Frozen(f) => Ok(f.value.clone()),
            other => Ok(other.clone_fast()),
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
    pub fn gc_mark_sweep(&mut self, live_slots: &[bool]) {
        let to_remove: Vec<usize> = self.arena
            .iter()
            .filter(|(slot, _)| !live_slots.get(*slot).copied().unwrap_or(false))
            .map(|(k, _)| k)
            .collect();
        for key in to_remove {
            self.arena.remove(key);
        }
        self.gc_threshold = (self.arena.len() * 2).max(GC_MIN_THRESHOLD);
        self.gc_bytes = 0;
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
        stash.value = new_value;
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
            | Value::Ref(_) | Value::GridRef { .. } | Value::Class { .. }
            | Value::DateTime(_) | Value::Duration(_) | Value::Frozen(_) | Value::Handle(_) => {
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
                self.alloc_value(Value::MapOrd(Box::new(new_m)))
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
            Value::Object(o) => {
                let new_fields: indexmap::IndexMap<String, Value> = o.fields.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::Object(Box::new(crate::value::ObjectVal {
                    class_name: o.class_name.clone(),
                    fields: std::rc::Rc::new(new_fields),
                    readonly_fields: o.readonly_fields.clone(),
                    trait_fields: o.trait_fields.clone(),
                    uuid: o.uuid.clone(),
                })))
            }

            // Enum: deep clone fields
            Value::Enum(e) => {
                let new_fields = e.fields.as_ref().map(|f| {
                    f.iter().map(|(k, v)| (k.clone(), self.clone_value_shallow(v))).collect()
                });
                self.alloc_value(Value::Enum(Box::new(crate::value::EnumVal {
                    enum_name: e.enum_name.clone(),
                    variant_name: e.variant_name.clone(),
                    fields: new_fields,
                })))
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

    /// The error for reading a global slot that holds nothing.
    pub fn missing_global(&self, idx: usize) -> GoblinError {
        if self.omitted_globals.get(idx).copied().unwrap_or(false) {
            let name = self.global_names.get(idx).map(|n| n.rsplit("::").next().unwrap_or(n)).unwrap_or("?");
            return GoblinError::Runtime(format!(
                "C0121: actor-global-omitted: `{name}` was not copied into this actor or swarm task, because none of its code uses it by name; pass the value in instead (an actor's state or message, a swarm task's item or context)"));
        }
        GoblinError::Runtime(format!("global slot {idx} is uninitialized"))
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
    fn new_text_is_charged_to_the_gc_budget_once() {
        let mut sess = Session::new(GcMode::Off);
        let text: crate::value::GStr = "x".repeat(1000).into();
        // New text stored while another copy is still on the stack (the
        // compiler's `Dup` before a store) is still new memory.
        let on_stack = text.clone();
        sess.alloc_value(Value::Str(text));
        assert_eq!(sess.gc_bytes, 1000);
        // Storing the same text in another variable costs nothing new.
        sess.alloc_value(Value::Str(on_stack.clone()));
        assert_eq!(sess.gc_bytes, 1000);
        // Changing a shared copy makes new text, which counts again.
        let mut changed = on_stack;
        changed.make_mut().push('y');
        sess.alloc_value(Value::Str(changed));
        assert_eq!(sess.gc_bytes, 2001);
    }

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

impl Drop for Session {
    fn drop(&mut self) {
        if !self.owned_actors.is_empty() { self.finish_actors(); }
    }
}
