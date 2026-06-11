use std::collections::{BTreeMap, HashMap};

use crate::error::GoblinError;
use crate::grid::GridStore;
use crate::value::Value;

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

/// A VM session: owns globals and all runtime state.
/// Each worker has exactly one Session.
pub struct Session {
    /// Global variable slots (indexed by compiled global index).
    pub globals: Vec<Option<Value>>,
    /// Name for each global slot (slot index → name), for string interpolation.
    pub global_names: Vec<String>,

    /// Optional worker ID for cross-worker safety checks.
    pub worker_id: usize,

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
}

impl Session {
    pub fn new() -> Self {
        // Seed from system time if available, else use a fixed constant.
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.subsec_nanos() as u128 | ((d.as_secs() as u128) << 32))
            .unwrap_or(0x123456789abcdef0u128)
            | 1; // MCG requires odd seed
        Session {
            globals: Vec::new(),
            global_names: Vec::new(),
            worker_id: 0,
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

    // ── Globals ─────────────────────────────────────────────────────────────

    pub fn ensure_globals(&mut self, count: usize) {
        while self.globals.len() < count {
            self.globals.push(None);
        }
    }

    pub fn set_global(&mut self, idx: usize, v: Value) {
        self.ensure_globals(idx + 1);
        self.globals[idx] = Some(v);
    }

    pub fn get_global(&self, idx: usize) -> Option<&Value> {
        self.globals.get(idx)?.as_ref()
    }

    /// Deref a Value::Ref through object_store.
    pub fn deref_value(&self, v: Value) -> Value {
        match v {
            Value::Ref(ref uuid) => self.object_store.get(uuid).cloned().unwrap_or(v),
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn globals_work() {
        let mut sess = Session::new();
        sess.set_global(0, Value::Int(42));
        let v = sess.get_global(0).cloned().unwrap();
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn deref_ref() {
        let mut sess = Session::new();
        sess.object_store.insert("abc".to_string(), Value::Int(99));
        let v = sess.deref_value(Value::Ref("abc".to_string()));
        assert!(matches!(v, Value::Int(99)));

    }

}
