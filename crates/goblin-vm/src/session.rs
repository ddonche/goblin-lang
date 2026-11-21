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

/// An overlay kind's lines as written, shared (`OverlayDef::code`).
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct OverlayCode {
    pub modifiers: Vec<(String, goblin_ast::Expr)>,
    pub spread_rules: Vec<goblin_ast::SpreadRule>,
    pub spawn_rules: Vec<goblin_ast::OverlaySpawnRule>,
    pub transitions: Vec<goblin_ast::TransitionDef>,
    /// `each tick:` changes, made every tick and never taken back.
    pub each_tick: Vec<(String, goblin_ast::Expr)>,
    /// `spread: chance where condition`.
    pub spreads: Vec<goblin_ast::OverlaySpread>,
    /// The blocks as written, so another session can compile them again.
    pub actions: Vec<goblin_ast::ActionDecl>,
    /// `phase: NAME` blocks: spreads, `every` blocks and transitions run
    /// only in that phase of a `tick()` block.
    pub phases: Vec<goblin_ast::OverlayPhase>,
}

// ── Overlay runtime types ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct OverlayDef {
    /// Its lines as written (spreads, transitions, blocks...): shared by
    /// the kinds made from one another, so a new kind costs no copy of
    /// them and its formulas are the ones already compiled.
    pub code: Rc<OverlayCode>,
    pub name: String,
    pub host_types: Vec<String>,
    pub decay_rate: f64,
    pub default_duration: Option<u32>,
    pub apply_behavior: OverlayApplyBehavior,
    pub conflict_rules: Vec<(String, f64)>,     // (opponent, suppress_rate)
    pub extra_fields: indexmap::IndexMap<String, Value>,
    /// `on end: action`: called with the host when the overlay runs out.
    pub on_end: Option<String>,
    /// `~intensity: n`: what a new copy starts at.
    pub intensity: f64,
    /// `conflicts: a, b`: can't share a host with this overlay.
    pub conflicts: Vec<String>,
    /// `overlay calm <^> Mood`: its overlay class, and the class's `dup:`
    /// (what it does on a host that has another member).
    pub class: Option<String>,
    pub class_dup: OverlayApplyBehavior,
    /// The overlay's blocks (`on start`, `every N ticks #k`, `on end`),
    /// compiled as methods taking the host, `intensity` and the overlay's
    /// own data fields.
    pub methods: HashMap<String, Rc<crate::value::FunctionObject>>,
    pub on_start: Option<String>,
    pub every: Vec<(u64, String)>,
    /// What the kinds a transition makes from this one are named after:
    /// its overlay class in lower case, else its own declared name
    /// (`language_76`, `plague_3`).
    pub family: String,
    /// Made by a transition while the program ran, not declared: it goes
    /// along with its overlays to other regions, which don't know it.
    pub generated: bool,
    /// Made while actors ran (by an `overlay` line in a region, or in the
    /// program once it started actors): their images do not have it, so it
    /// goes along with its overlays too.
    pub travels: bool,
    /// The kinds a transition made this one from (two for a merge); none
    /// for a declared overlay.
    pub parents: Vec<String>,
    /// The variables of the action whose code made this overlay, as they
    /// were when its `overlay` line ran: its lines see them, as code
    /// written in an action does. None for one made at the top level.
    pub maker: Vec<(String, Value)>,
}

impl OverlayDef {
    /// The names an overlay's code sees after `self` and `intensity`: its
    /// own fields, then the maker's variables.
    pub fn own_names(&self) -> Vec<String> {
        self.extra_fields.keys().cloned().chain(self.maker.iter().map(|(n, _)| n.clone())).collect()
    }
    /// Their values for this copy (`fields`: the copy's own).
    pub fn own_values(&self, fields: &indexmap::IndexMap<String, Value>) -> Vec<Value> {
        self.extra_fields.keys().map(|k| fields.get(k).cloned().unwrap_or(Value::Nil))
            .chain(self.maker.iter().map(|(_, v)| v.clone())).collect()
    }
    /// Its phase's block, in a phase; None at the tick's end (its own lines).
    fn phase_of(&self, phase: Option<&str>) -> Option<Option<&goblin_ast::OverlayPhase>> {
        phase.map(|p| self.code.phases.iter().find(|x| x.name == p))
    }
    /// The spreads that run now: in a phase, that phase's.
    pub fn spreads_in(&self, phase: Option<&str>) -> &[goblin_ast::OverlaySpread] {
        match self.phase_of(phase) { None => &self.code.spreads, Some(p) => p.map(|p| &p.spreads[..]).unwrap_or(&[]) }
    }
    /// The `every N ticks` blocks that run now.
    pub fn every_in(&self, phase: Option<&str>) -> &[(u64, String)] {
        match self.phase_of(phase) { None => &self.every, Some(p) => p.map(|p| &p.every[..]).unwrap_or(&[]) }
    }
    /// The transitions that run now.
    pub fn transitions_in(&self, phase: Option<&str>) -> &[goblin_ast::TransitionDef] {
        match self.phase_of(phase) { None => &self.code.transitions, Some(p) => p.map(|p| &p.transitions[..]).unwrap_or(&[]) }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum OverlayApplyBehavior { Caps, Replaces, Stacks { label: Option<String> }, Stronger }

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
    /// What this overlay's `modifies:` has added to each field. A timed
    /// overlay takes exactly that back when it ends, so other changes to
    /// the field meanwhile stay.
    pub applied: Vec<(String, f64)>,
    pub extra_fields: indexmap::IndexMap<String, Value>,
    pub des_id: OverlayInstanceId,
}

// ── Link runtime types ───────────────────────────────────────────────────────

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LinkDef {
    pub class_name: String,
    pub channel: String,
    pub formula: goblin_ast::Expr,
    /// Whether `by [ formula ]` was written (else the link starts at 0,
    /// kept inside its range).
    pub has_formula: bool,
    /// The values the link can take (None only for an object's own
    /// formula, which takes its class's range).
    pub range: Option<goblin_ast::LinkRange>,
    /// The number the formula gives when it is a plain number (`score =>
    /// .3`, or a block's start without `score`): read without running it.
    #[serde(default)]
    pub constant: Option<f64>,
}

impl LinkDef {
    /// The number a formula that is a plain number gives, as the compiler
    /// makes it (a whole number up to 2^53 is an Int, else it is a float).
    /// None for anything else, which is run as a formula.
    pub fn constant_of(formula: &goblin_ast::Expr) -> Option<f64> {
        let goblin_ast::Expr::Number(raw, _) = formula else { return None };
        let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
        if !raw.contains('.') && !raw.contains('e') && !raw.contains('E') {
            let i = cleaned.parse::<i64>().ok()?;
            return (i.unsigned_abs() <= 9_007_199_254_740_992).then_some(i as f64);
        }
        cleaned.parse::<f64>().ok()
    }
}

/// The smallest arena size that triggers a collection.
pub const GC_MIN_THRESHOLD: usize = 1 << 13;

/// Roughly how many bytes of strings and collections may be stored into new
/// slots before a collection runs, however few slots that took: a loop
/// rewriting a large string makes one dead copy per pass.
pub const GC_BYTE_BUDGET: usize = 64 << 20;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LinkOffset {
    pub value: f64,
    pub ticks_remaining: Option<u32>,
}

type LinkMap = HashMap<(String, String, String), Vec<LinkOffset>, rustc_hash::FxBuildHasher>;

/// A key of two or three texts, looked up without making `String`s: a
/// `(String, String)` key borrows as `dyn Key2`, so `(&str, &str)` finds it.
/// It hashes as the tuple of strings does.
pub trait Key2 { fn k0(&self) -> &str; fn k1(&self) -> &str; }
impl Key2 for (String, String) { fn k0(&self) -> &str { &self.0 } fn k1(&self) -> &str { &self.1 } }
impl Key2 for (&str, &str) { fn k0(&self) -> &str { self.0 } fn k1(&self) -> &str { self.1 } }
impl<'a> std::borrow::Borrow<dyn Key2 + 'a> for (String, String) { fn borrow(&self) -> &(dyn Key2 + 'a) { self } }
impl std::hash::Hash for dyn Key2 + '_ { fn hash<H: std::hash::Hasher>(&self, h: &mut H) { self.k0().hash(h); self.k1().hash(h); } }
impl PartialEq for dyn Key2 + '_ { fn eq(&self, o: &Self) -> bool { self.k0() == o.k0() && self.k1() == o.k1() } }
impl Eq for dyn Key2 + '_ {}

pub trait Key3 { fn k0(&self) -> &str; fn k1(&self) -> &str; fn k2(&self) -> &str; }
impl Key3 for (String, String, String) { fn k0(&self) -> &str { &self.0 } fn k1(&self) -> &str { &self.1 } fn k2(&self) -> &str { &self.2 } }
impl Key3 for (&str, &str, &str) { fn k0(&self) -> &str { self.0 } fn k1(&self) -> &str { self.1 } fn k2(&self) -> &str { self.2 } }
impl<'a> std::borrow::Borrow<dyn Key3 + 'a> for (String, String, String) { fn borrow(&self) -> &(dyn Key3 + 'a) { self } }
impl std::hash::Hash for dyn Key3 + '_ { fn hash<H: std::hash::Hasher>(&self, h: &mut H) { self.k0().hash(h); self.k1().hash(h); self.k2().hash(h); } }
impl PartialEq for dyn Key3 + '_ { fn eq(&self, o: &Self) -> bool { self.k0() == o.k0() && self.k1() == o.k1() && self.k2() == o.k2() } }
impl Eq for dyn Key3 + '_ {}

/// Every change made to a link, by (from, to, name). Reads go straight to
/// the map. It also answers "which objects does this one have a link to"
/// from an index built when first asked and dropped on any change.
#[derive(Default)]
pub struct LinkTable {
    map: LinkMap,
    from: std::cell::RefCell<Option<HashMap<String, Vec<(String, String)>, rustc_hash::FxBuildHasher>>>,
}

impl LinkTable {
    /// The objects `from` has a `name` link change to, in no set order.
    pub fn targets(&self, from: &str, name: &str) -> Vec<String> {
        let mut out = Vec::new();
        self.each_target(from, name, |b| out.push(b.to_string()));
        out
    }

    /// Calls `f` with each object `from` has a `name` link change to, in no
    /// set order.
    pub fn each_target(&self, from: &str, name: &str, mut f: impl FnMut(&str)) {
        let mut idx = self.from.borrow_mut();
        let idx = idx.get_or_insert_with(|| {
            let mut m: HashMap<String, Vec<(String, String)>, rustc_hash::FxBuildHasher> = Default::default();
            for (a, b, n) in self.map.keys() {
                match m.get_mut(a.as_str()) {
                    Some(v) => v.push((b.clone(), n.clone())),
                    None => { m.insert(a.clone(), vec![(b.clone(), n.clone())]); }
                }
            }
            m
        });
        if let Some(v) = idx.get(from) {
            for (b, n) in v { if n == name { f(b); } }
        }
    }
}

/// `link_offsets.entry(key).or_default()`, keeping the index.
pub struct LinkEntry<'a> { t: &'a mut LinkTable, k: (String, String, String) }
impl<'a> LinkEntry<'a> {
    pub fn or_default(self) -> &'a mut Vec<LinkOffset> {
        let LinkEntry { t, k } = self;
        if !t.map.contains_key(&k) { t.indexed_add(&k); }
        t.map.entry(k).or_default()
    }
}

// Changes that keep the index (`targets`) up to date instead of dropping
// it: the index is rebuilt only after a change made through the map itself.
impl LinkTable {
    fn indexed_add(&mut self, k: &(String, String, String)) {
        if let Some(idx) = self.from.get_mut() {
            match idx.get_mut(k.0.as_str()) {
                Some(v) => v.push((k.1.clone(), k.2.clone())),
                None => { idx.insert(k.0.clone(), vec![(k.1.clone(), k.2.clone())]); }
            }
        }
    }
    fn indexed_remove(&mut self, k: &(String, String, String)) {
        if let Some(idx) = self.from.get_mut() {
            if let Some(v) = idx.get_mut(k.0.as_str()) {
                if let Some(p) = v.iter().position(|(b, n)| *b == k.1 && *n == k.2) { v.swap_remove(p); }
                if v.is_empty() { idx.remove(k.0.as_str()); }
            }
        }
    }
    pub fn entry(&mut self, k: (String, String, String)) -> LinkEntry<'_> { LinkEntry { t: self, k } }
    pub fn insert(&mut self, k: (String, String, String), v: Vec<LinkOffset>) -> Option<Vec<LinkOffset>> {
        if !self.map.contains_key(&k) { self.indexed_add(&k); }
        self.map.insert(k, v)
    }
    pub fn remove(&mut self, k: &(String, String, String)) -> Option<Vec<LinkOffset>> {
        let old = self.map.remove(k);
        if old.is_some() { self.indexed_remove(k); }
        old
    }
    /// Every change, to change in place (the keys stay).
    pub fn iter_mut(&mut self) -> std::collections::hash_map::IterMut<'_, (String, String, String), Vec<LinkOffset>> { self.map.iter_mut() }
    pub fn retain(&mut self, mut keep: impl FnMut(&(String, String, String), &mut Vec<LinkOffset>) -> bool) {
        let mut gone: Vec<(String, String, String)> = Vec::new();
        let indexed = self.from.get_mut().is_some();
        let before = self.map.len();
        // Past a quarter of the changes gone, the index is made again when
        // next asked for instead.
        let mut many = false;
        self.map.retain(|k, v| {
            let y = keep(k, v);
            if !y && indexed && !many {
                if gone.len() * 4 >= before { many = true; gone.clear(); } else { gone.push(k.clone()); }
            }
            y
        });
        if many { *self.from.get_mut() = None; return; }
        for k in &gone { self.indexed_remove(k); }
    }
}

impl std::ops::Deref for LinkTable {
    type Target = LinkMap;
    fn deref(&self) -> &LinkMap { &self.map }
}

impl std::ops::DerefMut for LinkTable {
    fn deref_mut(&mut self) -> &mut LinkMap { *self.from.get_mut() = None; &mut self.map }
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

    /// When the session started, for `:total_time()`.
    pub script_start: std::time::Instant,
    /// The last `:split_time()` call (or the start), for `:split_time()`.
    pub last_split: std::time::Instant,

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
    /// Objects this region moved to another one (`:move!`), and the
    /// variable each was stored under, so reading one says where it went.
    pub(crate) moved_away: HashMap<String, String>,
    /// While a DES action runs: the objects it changed, so their erase
    /// transitions are checked as soon as it finishes.
    pub(crate) des_touched: Option<Vec<crate::value::ObjId>>,
    /// Objects whose `on erase:` action is running: not erased again.
    pub(crate) des_erasing: Vec<String>,
    /// The main program's world of regions (`:tick()`).
    pub world: Option<crate::world::World>,
    /// In a region, while it runs a world tick.
    pub(crate) region: Option<crate::world::RegionCtx>,
    /// In a region: its copies of the other regions' objects, kept between
    /// ticks so the next tick rebuilds only the fields that changed.
    pub(crate) remote_copies: rustc_hash::FxHashMap<crate::value::ObjId, crate::world::Copy>,
    /// In a region: the `owner` this region gave other regions' objects last
    /// tick. Their regions set it at the start of this tick, after the
    /// copies here were made, so the copies are given it too.
    pub(crate) owners_sent: Vec<(crate::value::ObjId, Value)>,
    /// In a region: its objects as they went into its last snapshot.
    pub(crate) sent_objects: rustc_hash::FxHashMap<crate::value::ObjId, crate::world::Sent>,
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
    /// PRNG state for `:event` tellings, apart from `rng_state` so that
    /// narrating never changes what the program draws.
    pub event_rng: u128,

    /// Token store: namespace → key → value.
    pub token_store: BTreeMap<String, BTreeMap<String, Value>>,
    /// Sweeps in progress (see sweep.rs).
    pub sweeps: crate::sweep::Sweeps,

    /// Object store: uuid → Value (for DES/overlay system).
    /// Objects in creation order, so DES passes and `:objects()` visit them
    /// the same way every run (the interpreter keeps creation order too).
    /// Fx hashing: object ids are long strings looked up on every field
    /// read through a reference; order is insertion order either way.
    pub object_store: crate::store::ObjectStore,
    /// The variable each object was first stored in (uuid -> name, and
    /// whether that variable is top-level). DES
    /// overlays report their host by this name (`host_var`), as the
    /// interpreter does, also for overlays that spread or spawn onto it.
    pub object_names: HashMap<String, (String, bool), rustc_hash::FxBuildHasher>,

    /// Active overlay instances.
    pub overlay_instances: crate::store::OverlayList,

    /// HTTP response state.
    pub response: ResponseState,

    /// Grid worlds.
    pub grid_store: GridStore,

    /// Class definitions registered at runtime.
    pub classes: Rc<HashMap<String, ClassDecl, rustc_hash::FxBuildHasher>>,

    /// Pre-compiled class methods: (class_name, method_name) -> FunctionObject
    pub compiled_methods: Rc<HashMap<(String, String), std::rc::Rc<crate::value::FunctionObject>>>,
    /// DES ticks run so far (`every N ticks` counts on it).
    pub des_ticks: i64,
    /// The phase of a `tick()` block running now (None: the tick's end).
    pub phase: Option<std::rc::Rc<str>>,
    /// Where the event log goes (`:event_log(path)`), None when it is off.
    pub event_log: Option<crate::events::Sink>,
    /// Each phase's view of the classes (`crate::tick::run_phase`), with
    /// the classes it was made from.
    pub phase_views: HashMap<String, (Rc<HashMap<String, ClassDecl, rustc_hash::FxBuildHasher>>, Rc<HashMap<String, ClassDecl, rustc_hash::FxBuildHasher>>)>,
    /// While a phase runs: the classes its view (now `classes`) was made
    /// from, which hold what the view leaves out (their actions).
    pub phase_base: Option<Rc<HashMap<String, ClassDecl, rustc_hash::FxBuildHasher>>>,
    /// What `:metrics()` reports: objects and overlays made, tick time.
    pub des_metrics: crate::metrics::DesMetrics,
    /// In a region: its objects that transitioned this tick (see
    /// `transition::object_transitions`), reported with the tick's result.
    pub transitioned: Vec<String>,

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
    pub overlay_defs: rustc_hash::FxHashMap<String, Rc<OverlayDef>>,
    pub link_defs: HashMap<(String, String), Rc<LinkDef>, rustc_hash::FxBuildHasher>,
    pub object_link_defs: HashMap<(String, String), Rc<LinkDef>, rustc_hash::FxBuildHasher>,
    pub link_offsets: LinkTable,
    pub object_decisions: HashMap<String, Rc<goblin_ast::DecisionDef>>,
    /// Overlay classes (`<^>Mood |`) by name, for members declared in
    /// other files (`overlay calm <^> Mood`).
    pub overlay_classes: HashMap<String, Rc<goblin_ast::OverlayDefStmt>>,
    pub unit_registry: Rc<HashMap<String, goblin_ast::UnitDecl>>,
    pub des_store: EntityStore,
    pub des_index: EntityIndex,
    pub des_tick_runner: TickRunner,
    pub des_overlay_id_counter: u32,
    /// How many overlay kinds each family has made by transitions.
    pub kind_counts: HashMap<String, u64>,
    /// The `overlay` lines that have run (by their place in the source).
    pub overlay_lines_run: std::collections::HashSet<String>,
    /// The overlay kinds in the order they were made (declared, or made
    /// while the program ran), for `:objects(Government)`.
    pub kind_order: Vec<String>,
    /// Generated overlay kinds converted to cross threads, made once each.
    /// While a world runs on regions: the grid writes made here since the
    /// last round, sent to the other regions (`world::round`).
    pub(crate) grid_log: Option<Vec<crate::grid::GridWrite>>,
    pub(crate) portable_kinds: std::cell::RefCell<HashMap<String, std::sync::Arc<crate::des_image::POverlayDef>>>,
    /// Overlay kinds' lines converted for other threads, by the address of
    /// this session's copy (kept, so the address stays its own).
    pub(crate) portable_code: std::cell::RefCell<HashMap<usize, (Rc<OverlayCode>, std::sync::Arc<OverlayCode>)>>,
    /// Overlay kinds' lines from other threads, by the address of theirs
    /// (kept, likewise): kinds that shared lines there share them here.
    pub(crate) code_from_portable: HashMap<usize, (std::sync::Arc<OverlayCode>, Rc<OverlayCode>)>,
    /// `globals_cell`'s list, with the names it was made from.
    pub(crate) globals_cell: std::cell::RefCell<Option<((usize, usize), Rc<std::cell::OnceCell<Vec<String>>>)>>,
    /// Their blocks compiled, by those lines and the kind's field names.
    pub(crate) methods_from_portable: HashMap<(usize, Vec<String>), HashMap<String, Rc<crate::value::FunctionObject>>>,
    /// The overlay kinds' variables (`OverlayDef::maker`), converted once
    /// for as long as the session holds them: by the collection and whether
    /// objects went by id. Kinds a transition makes share their maker's.
    pub(crate) portable_makers: std::cell::RefCell<HashMap<(usize, bool), (Value, std::sync::Arc<crate::portable::PValue>)>>,
    /// And rebuilt once from another session's copy, by that copy.
    pub(crate) rebuilt_makers: HashMap<usize, (std::sync::Arc<crate::portable::PValue>, Value)>,
    pub des_link_id_counter: u32,
    pub des_link_ids: HashMap<(String, String, String), LinkId>,

    /// Type lock: current cast type per global slot index (updated by cast-bang and declaration).
    pub global_type_locks: Rc<HashMap<u32, String>>,
    /// Hard type lock: declared lock type per global slot index (set only at declaration, never changes).
    pub global_hard_type_locks: Rc<HashMap<u32, String>>,

    /// Named function registry for `invoke`/`summon`/`provoke` and GLAM namespace dispatch.
    /// Actions are stored under both bare names ("create_shard") and qualified names
    /// ("graveyard::create_shard") when registered during a `use glam` import.
    pub named_values: Rc<HashMap<String, Value, rustc_hash::FxBuildHasher>>,

    /// The namespace of the GLAM currently being imported, if any.
    /// Set by UseGlam before importing the GLAM entry file; cleared after.
    /// Nested use statements save/restore this so inner GLAMs register correctly.
    pub current_glam_ns: Option<String>,
    /// While a `use`d GLAM's top-level code runs: its namespace, and the
    /// names its glam.toml `[provides]` lists (None: no list). Box writes
    /// outside them raise B0102/B0105, as in the interpreter.
    pub box_namespace: Option<String>,
    pub box_provides: Option<std::collections::HashSet<String>>,
    /// Each GLAM's `[provides] values` list: the only names its code may
    /// publish, at any time (B0105).
    pub glam_provides: HashMap<String, std::collections::HashSet<String>>,
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
    /// The program's global names as a shared list for compiled formulas
    /// (`Compiler::with_shared_globals`), made again when the names change.
    pub(crate) fn globals_cell(&self) -> Rc<std::cell::OnceCell<Vec<String>>> {
        let key = (Rc::as_ptr(&self.global_names) as usize, self.global_names.len());
        if let Some((k, c)) = self.globals_cell.borrow().as_ref() { if *k == key { return c.clone(); } }
        let c = Rc::new(std::cell::OnceCell::new());
        let _ = c.set(self.global_names.to_vec());
        *self.globals_cell.borrow_mut() = Some((key, c.clone()));
        c
    }

    /// The next overlay copy's id. While a world runs, every region and
    /// the program number copies apart (region k of n takes the ids
    /// k, k + n + 1, ..., the program n, 2n + 1, ...), so a copy that
    /// moves to another region, with its host or by spreading, never has
    /// the id of one already there.
    pub(crate) fn next_overlay_id(&mut self) -> OverlayInstanceId {
        let (k, n) = match (&self.region, &self.world) {
            (Some(c), _) => (c.regions.iter().position(|&r| r == c.me).unwrap_or(0) as u32, c.regions.len() as u32 + 1),
            (None, Some(w)) => { let n = w.region_count() as u32; (n, n + 1) }
            _ => (0, 1),
        };
        let mut c = self.des_overlay_id_counter;
        c += (k + n - c % n) % n;
        self.des_overlay_id_counter = c + 1;
        OverlayInstanceId(c)
    }

    /// Whether an object of `class` is one of `wanted`: that class, or,
    /// for `uses diplomacy`, any class that uses the interface.
    pub fn class_matches(&self, wanted: &str, class: &str) -> bool {
        match wanted.strip_prefix("uses ") {
            Some(iface) => self.classes.get(class).is_some_and(|c| c.uses.iter().any(|u| u == iface)),
            None => wanted == class,
        }
    }

    /// Where the objects of `wanted` (a class, or `uses diplomacy`) sit in
    /// the store, in creation order.
    pub fn places_of_class(&self, wanted: &str) -> Vec<usize> {
        let store = &self.object_store;
        if wanted.starts_with("uses ") {
            return store.places_of(|c| self.class_matches(wanted, c));
        }
        store.of_class_ids(wanted).filter_map(|u| store.get_index_of(u)).collect()
    }

    /// Notes that the object `uuid` changed, when a DES action is running.
    #[inline]
    pub(crate) fn touch(&mut self, uuid: &(impl crate::value::IdKey + ?Sized)) {
        // Text no id has names no object here: nothing to check.
        if let Some(t) = &mut self.des_touched { if let Some(id) = uuid.id() { t.push(id.into_owned()); } }
    }

    /// Binds an object value to a name: the store takes it, and a DES
    /// action notes the change. Binding the very object the store holds
    /// (same fields, not a changed copy) changes nothing.
    #[inline]
    /// Bind an object: a new one goes into the store; one already there is
    /// that entry, so a copy of it (taken from a list or an earlier read)
    /// never replaces what it holds now.
    pub(crate) fn put_object(&mut self, uuid: &crate::value::ObjId, val: Value) {
        if self.object_store.contains_key(uuid) { return; }
        self.touch(uuid);
        if matches!(val, Value::Object(_)) { self.des_metrics.objects_made += 1; }
        self.object_store.insert(uuid.clone(), val);
    }

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
        let started = std::time::Instant::now();
        Session {
            script_start: started,
            last_split: started,
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
            moved_away: HashMap::new(),
            des_touched: None,
            des_erasing: Vec::new(),
            kept_home: HashMap::new(),
            world: None,
            region: None,
            remote_copies: Default::default(),
            owners_sent: Vec::new(),
            sent_objects: Default::default(),
            field_defaults: HashMap::new(),
            next_generation: 1,
            gc_threshold: GC_MIN_THRESHOLD,
            gc_bytes: 0,
            has_cells: false,
            rng_state: seed,
            event_rng: crate::events::event_seed(seed),
            token_store: BTreeMap::new(),
            sweeps: Default::default(),
            object_store: Default::default(),
            object_names: Default::default(),
            overlay_instances: Default::default(),
            response: ResponseState::default(),
            grid_store: GridStore::new(),
            classes: Default::default(),
            compiled_methods: Default::default(),
            des_ticks: 0,
            phase_views: Default::default(),
            phase_base: None,
            phase: None,
            event_log: None,
            des_metrics: Default::default(),
            transitioned: Vec::new(),
            enums: Default::default(),
            base_dir: std::env::current_dir().unwrap_or_default(),
            project_root: std::env::current_dir().unwrap_or_default(),
            imported: Default::default(),
            overlay_defs: Default::default(),
            link_defs: Default::default(),
            object_link_defs: Default::default(),
            link_offsets: Default::default(),
            object_decisions: HashMap::new(),
            overlay_classes: HashMap::new(),
            unit_registry: Default::default(),
            des_store: EntityStore::new(),
            des_index: EntityIndex::default(),
            des_tick_runner: TickRunner::new(),
            des_overlay_id_counter: 0,
            kind_counts: HashMap::new(),
            overlay_lines_run: Default::default(),
            kind_order: Vec::new(),
            portable_kinds: Default::default(),
            portable_code: Default::default(),
            code_from_portable: Default::default(),
            globals_cell: Default::default(),
            methods_from_portable: Default::default(),
            grid_log: None,
            portable_makers: Default::default(),
            rebuilt_makers: Default::default(),
            des_link_id_counter: 0,
            des_link_ids: HashMap::new(),
            global_type_locks: Default::default(),
            global_hard_type_locks: Default::default(),
            named_values: Default::default(),
            current_glam_ns: None,
            box_namespace: None,
            box_provides: None,
            glam_provides: Default::default(),
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
    /// A grid write, kept while a world runs on regions.
    pub(crate) fn note_grid(&mut self, w: crate::grid::GridWrite) {
        if let Some(log) = self.grid_log.as_mut() { log.push(w); }
    }

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
    #[inline]
    pub fn resolve(&self, addr: Address) -> Result<&Stash, GoblinError> {
        // One slab lookup (`get`), not a `contains` and then an index.
        let Some(stash) = self.arena.get(addr.slot as usize) else {
            return Err(GoblinError::EmptySlot { slot: addr.slot });
        };
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
    #[inline]
    pub fn resolve_mut(&mut self, addr: Address) -> Result<&mut Stash, GoblinError> {
        // One slab lookup (`get_mut`), not a `contains` and then an index.
        let Some(stash) = self.arena.get_mut(addr.slot as usize) else {
            return Err(GoblinError::EmptySlot { slot: addr.slot });
        };
        if stash.generation != addr.generation {
            return Err(GoblinError::StaleAddress {
                slot: addr.slot,
                stored_gen: stash.generation,
                addr_gen: addr.generation,
            });
        }
        Ok(stash)
    }

    #[inline]
    pub fn get_stash(&self, t: &Tether) -> Result<&Stash, GoblinError> {
        self.resolve(t.addr)
    }

    #[inline]
    pub fn get_stash_mut(&mut self, t: &Tether) -> Result<&mut Stash, GoblinError> {
        self.resolve_mut(t.addr)
    }

    /// Clone the Value payload for read-only introspection.
    /// The errors `read_value` would give, without copying the value: a
    /// statement like a bare `den` must still fail when `den` was handed
    /// to an actor.
    pub fn check_readable(&self, t: &Tether) -> Result<(), GoblinError> {
        match &self.get_stash(t)?.value {
            Value::Ref(uuid) if !self.object_store.contains_key(uuid) =>
                crate::actor::gone_object(self, uuid).map(|_| ()),
            _ => Ok(()),
        }
    }

    pub fn read_value(&self, t: &Tether) -> Result<Value, GoblinError> {
        // Auto-deref Ref through object_store (matches interpreter's get_var semantics).
        match &self.get_stash(t)?.value {
            Value::Ref(uuid) => match self.object_store.get(uuid) {
                Some(v) => Ok(v.clone()),
                None => crate::actor::gone_object(self, uuid),
            },
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

    /// Frees the stashes of `locals` (a returning call's variables) that
    /// only they hold. A captured variable has a second holder (its
    /// closure's cell) and stays; a stale tether finds its slot gone or
    /// reused under another generation, as after a collection.
    pub fn free_sole<'a>(&mut self, locals: impl Iterator<Item = &'a Tether>) {
        for t in locals {
            let slot = t.addr.slot as usize;
            if self.arena.get(slot).is_some_and(|s| s.generation == t.addr.generation && s.tether_count == 1) {
                self.arena.remove(slot);
            }
        }
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
                self.alloc_value(Value::Array(Into::into(new_items)))
            }

            // Map: deep clone values
            Value::Map(m) => {
                let new_m: std::collections::BTreeMap<String, Value> = m.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::Map(Into::into(new_m)))
            }

            // MapOrd: deep clone values
            Value::MapOrd(m) => {
                let new_m: indexmap::IndexMap<String, Value> = m.iter()
                    .map(|(k, v)| (k.clone(), self.clone_value_shallow(v)))
                    .collect();
                self.alloc_value(Value::MapOrd(Into::into(Box::new(new_m))))
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
                let new_fields: crate::value::FieldMap = o.fields.iter()
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
