use crate::store::{EntityHandle, EntityStore};
use crate::index::EntityIndex;

/// Tick phase. The TickRunner moves through phases in order each tick.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TickPhase {
    /// Overlay durations update; expired overlays removed.
    OverlayDecay,
    /// Overlay trait modifiers applied to pending fields.
    OverlayApply,
    /// Decision formulas evaluated; scores computed.
    DecisionEval,
    /// Judge maps scores to actions; actions execute (writes go to pending).
    ActionExec,
    /// Transition triggers evaluated against committed state.
    TransitionEval,
    /// Committed <-> pending swap for all live entities.
    Swap,
    /// Indexes updated from changes reported by swap.
    IndexSync,
}

impl TickPhase {
    const ORDER: &'static [TickPhase] = &[
        TickPhase::OverlayDecay,
        TickPhase::OverlayApply,
        TickPhase::DecisionEval,
        TickPhase::ActionExec,
        TickPhase::TransitionEval,
        TickPhase::Swap,
        TickPhase::IndexSync,
    ];
}

/// Statistics collected per tick. Useful for debugging and profiling.
#[derive(Debug, Default, Clone)]
pub struct TickStats {
    pub tick_number: u64,
    pub entities_evaluated: usize,
    pub overlays_decayed: usize,
    pub overlays_removed: usize,
    pub actions_executed: usize,
    pub transitions_fired: usize,
    pub fields_swapped: usize,
    pub index_updates: usize,
}

/// TickRunner owns tick sequencing and the double-buffer swap.
///
/// It does not own the actual simulation logic (that lives in the interpreter).
/// Instead it provides the correct sequencing and exposes hooks the interpreter
/// calls in order:
///
///   runner.begin_tick()
///   -- interpreter runs overlay decay, apply, decisions, actions, transitions
///   runner.end_tick(store, index)   <- performs swap + index sync
///
/// This keeps DES responsible for the swap invariant and nothing else.
pub struct TickRunner {
    pub tick_number: u64,
    pub stats: TickStats,
}

impl TickRunner {
    pub fn new() -> Self {
        Self {
            tick_number: 0,
            stats: TickStats::default(),
        }
    }

    /// Call at the start of each tick. Resets per-tick stats.
    pub fn begin_tick(&mut self) {
        self.tick_number += 1;
        self.stats = TickStats { tick_number: self.tick_number, ..Default::default() };
    }

    /// Call after all simulation phases complete and pending writes have been
    /// flushed to object_store by the interpreter (Session::des_flush_pending).
    /// Marks transition candidates and returns tick stats.
    pub fn end_tick(&mut self, store: &mut EntityStore, index: &mut EntityIndex) -> TickStats {
        let handles: Vec<EntityHandle> = store.live_handles().collect();
        for handle in handles {
            if let Some(entity) = store.get_mut(handle) {
                self.stats.fields_swapped += entity.pending.len(); // count any unflushed
                index.mark_transition_candidate(&entity.class_name.clone(), handle);
                self.stats.entities_evaluated += 1;
            }
        }
        self.stats.clone()
    }
}

impl Default for TickRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of a transition execution. The caller (interpreter) uses this to
/// apply the correct index mutations after the transition runs.
#[derive(Debug)]
pub enum TransitionResult {
    /// Entity was erased. Remove from all indexes.
    Erased { handle: EntityHandle },
    /// Entity stays; its class changed in-place (mutate).
    Mutated { handle: EntityHandle, old_class: String, new_class: String },
    /// Original ceases; successors are new handles (split / merge).
    Replaced { originals: Vec<EntityHandle>, successors: Vec<EntityHandle> },
    /// Original stays; a new fragment was created (fracture / spawn).
    Fragmented { primary: EntityHandle, fragment: EntityHandle },
    /// Primary absorbs target. Target is erased.
    Absorbed { primary: EntityHandle, target: EntityHandle },
    /// Target subjugated. Both remain; ownership link created.
    Subjugated { dominant: EntityHandle, subject: EntityHandle },
}

/// Apply a TransitionResult to the store and index.
/// The interpreter calls this after it has already computed successor field
/// values and put them into the store via `store.create(...)` or `store.erase(...)`.
pub fn apply_transition(
    result: &TransitionResult,
    store: &mut EntityStore,
    index: &mut EntityIndex,
) {
    match result {
        TransitionResult::Erased { handle } => {
            if let Some(entity) = store.get(*handle) {
                let class = entity.class_name.clone();
                index.remove_all(*handle, &class, None);
            }
            store.erase(*handle);
        }

        TransitionResult::Mutated { handle, old_class, new_class } => {
            index.remove_class(old_class, *handle);
            index.insert_class(new_class, *handle);
            index.unmark_transition_candidate(old_class, *handle);
            index.mark_transition_candidate(new_class, *handle);
        }

        TransitionResult::Replaced { originals, successors } => {
            for &orig in originals {
                if let Some(entity) = store.get(orig) {
                    let class = entity.class_name.clone();
                    index.remove_all(orig, &class, None);
                }
                store.erase(orig);
            }
            for &succ in successors {
                if let Some(entity) = store.get(succ) {
                    let class = entity.class_name.clone();
                    index.insert_class(&class, succ);
                }
            }
        }

        TransitionResult::Fragmented { primary: _, fragment } => {
            if let Some(entity) = store.get(*fragment) {
                let class = entity.class_name.clone();
                index.insert_class(&class, *fragment);
            }
        }

        TransitionResult::Absorbed { primary: _, target } => {
            if let Some(entity) = store.get(*target) {
                let class = entity.class_name.clone();
                index.remove_all(*target, &class, None);
            }
            store.erase(*target);
        }

        TransitionResult::Subjugated { dominant, subject } => {
            index.set_owner(*subject, *dominant);
        }
    }
}
