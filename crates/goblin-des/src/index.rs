use std::collections::{HashMap, HashSet};
use crate::store::EntityHandle;

/// All fast-lookup indexes maintained by DES. Every index is a view into
/// EntityStore — the store is canonical, indexes are derived.
///
/// Design rule: indexes update atomically when the store changes.
/// No index is ever "eventually consistent" with the store.
#[derive(Debug, Default)]
pub struct EntityIndex {
    // ── Class membership ────────────────────────────────────────────────────
    /// class_name -> set of live handles with that class.
    pub by_class: HashMap<String, HashSet<EntityHandle>>,

    // ── Ownership ───────────────────────────────────────────────────────────
    /// owner_handle -> set of handles directly owned by that entity.
    pub owned_by: HashMap<EntityHandle, HashSet<EntityHandle>>,
    /// entity_handle -> its direct owner's handle (inverse of owned_by).
    pub owner_of: HashMap<EntityHandle, EntityHandle>,

    // ── Overlays ────────────────────────────────────────────────────────────
    /// host_handle -> ordered list of overlay instance ids on that host.
    pub overlays_on_host: HashMap<EntityHandle, Vec<OverlayInstanceId>>,
    /// overlay_def_name -> set of host handles currently carrying it.
    pub hosts_of_overlay: HashMap<String, HashSet<EntityHandle>>,

    // ── Links ───────────────────────────────────────────────────────────────
    /// source_handle -> list of outgoing link ids.
    pub links_out: HashMap<EntityHandle, Vec<LinkId>>,
    /// target_handle -> list of incoming link ids.
    pub links_in: HashMap<EntityHandle, Vec<LinkId>>,

    // ── Grid ────────────────────────────────────────────────────────────────
    /// handle -> (grid_id, x, y) for entities placed on a grid.
    pub grid_pos: HashMap<EntityHandle, (String, i32, i32)>,
    /// (grid_id, x, y) -> set of handles at that cell.
    pub grid_cell: HashMap<(String, i32, i32), HashSet<EntityHandle>>,

    // ── Transition candidates ────────────────────────────────────────────────
    /// class_name -> set of handles eligible for transition evaluation.
    /// Subset of by_class; updated when traits cross thresholds.
    pub transition_candidates: HashMap<String, HashSet<EntityHandle>>,
}

/// Opaque id for an overlay instance. The overlay runtime owns the actual
/// instance data; the index just tracks which instances are on which host.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct OverlayInstanceId(pub u32);

/// Opaque id for a link. The link runtime owns the actual link data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LinkId(pub u32);

impl EntityIndex {
    // ── Class index ──────────────────────────────────────────────────────────

    pub fn insert_class(&mut self, class: &str, handle: EntityHandle) {
        self.by_class.entry(class.to_string()).or_default().insert(handle);
    }

    pub fn remove_class(&mut self, class: &str, handle: EntityHandle) {
        if let Some(set) = self.by_class.get_mut(class) {
            set.remove(&handle);
            if set.is_empty() {
                self.by_class.remove(class);
            }
        }
    }

    pub fn handles_for_class(&self, class: &str) -> &HashSet<EntityHandle> {
        static EMPTY: std::sync::OnceLock<HashSet<EntityHandle>> = std::sync::OnceLock::new();
        self.by_class.get(class).unwrap_or_else(|| EMPTY.get_or_init(HashSet::new))
    }

    // ── Ownership index ──────────────────────────────────────────────────────

    pub fn set_owner(&mut self, entity: EntityHandle, owner: EntityHandle) {
        // Remove from previous owner if any.
        if let Some(prev_owner) = self.owner_of.get(&entity).copied() {
            if let Some(set) = self.owned_by.get_mut(&prev_owner) {
                set.remove(&entity);
            }
        }
        self.owner_of.insert(entity, owner);
        self.owned_by.entry(owner).or_default().insert(entity);
    }

    pub fn clear_owner(&mut self, entity: EntityHandle) {
        if let Some(owner) = self.owner_of.remove(&entity) {
            if let Some(set) = self.owned_by.get_mut(&owner) {
                set.remove(&entity);
            }
        }
    }

    pub fn owned_by(&self, owner: EntityHandle) -> &HashSet<EntityHandle> {
        static EMPTY: std::sync::OnceLock<HashSet<EntityHandle>> = std::sync::OnceLock::new();
        self.owned_by.get(&owner).unwrap_or_else(|| EMPTY.get_or_init(HashSet::new))
    }

    pub fn owner_of(&self, entity: EntityHandle) -> Option<EntityHandle> {
        self.owner_of.get(&entity).copied()
    }

    // ── Overlay index ────────────────────────────────────────────────────────

    pub fn add_overlay(&mut self, host: EntityHandle, overlay_name: &str, id: OverlayInstanceId) {
        self.overlays_on_host.entry(host).or_default().push(id);
        self.hosts_of_overlay.entry(overlay_name.to_string()).or_default().insert(host);
    }

    pub fn remove_overlay(&mut self, host: EntityHandle, overlay_name: &str, id: OverlayInstanceId) {
        if let Some(list) = self.overlays_on_host.get_mut(&host) {
            list.retain(|&x| x != id);
            if list.is_empty() {
                self.overlays_on_host.remove(&host);
            }
        }
        if let Some(set) = self.hosts_of_overlay.get_mut(overlay_name) {
            set.remove(&host);
            if set.is_empty() {
                self.hosts_of_overlay.remove(overlay_name);
            }
        }
    }

    pub fn overlays_on(&self, host: EntityHandle) -> &[OverlayInstanceId] {
        self.overlays_on_host.get(&host).map(|v| v.as_slice()).unwrap_or(&[])
    }

    pub fn hosts_of(&self, overlay_name: &str) -> &HashSet<EntityHandle> {
        static EMPTY: std::sync::OnceLock<HashSet<EntityHandle>> = std::sync::OnceLock::new();
        self.hosts_of_overlay.get(overlay_name).unwrap_or_else(|| EMPTY.get_or_init(HashSet::new))
    }

    // ── Link index ───────────────────────────────────────────────────────────

    pub fn add_link(&mut self, source: EntityHandle, target: EntityHandle, id: LinkId) {
        self.links_out.entry(source).or_default().push(id);
        self.links_in.entry(target).or_default().push(id);
    }

    pub fn remove_link(&mut self, source: EntityHandle, target: EntityHandle, id: LinkId) {
        if let Some(list) = self.links_out.get_mut(&source) {
            list.retain(|&x| x != id);
        }
        if let Some(list) = self.links_in.get_mut(&target) {
            list.retain(|&x| x != id);
        }
    }

    // ── Grid index ───────────────────────────────────────────────────────────

    pub fn place_on_grid(&mut self, handle: EntityHandle, grid_id: &str, x: i32, y: i32) {
        // Remove from previous cell if any.
        if let Some((old_grid, ox, oy)) = self.grid_pos.get(&handle).cloned() {
            if let Some(set) = self.grid_cell.get_mut(&(old_grid, ox, oy)) {
                set.remove(&handle);
            }
        }
        let cell_key = (grid_id.to_string(), x, y);
        self.grid_pos.insert(handle, (grid_id.to_string(), x, y));
        self.grid_cell.entry(cell_key).or_default().insert(handle);
    }

    pub fn remove_from_grid(&mut self, handle: EntityHandle) {
        if let Some((grid_id, x, y)) = self.grid_pos.remove(&handle) {
            if let Some(set) = self.grid_cell.get_mut(&(grid_id, x, y)) {
                set.remove(&handle);
            }
        }
    }

    pub fn grid_position(&self, handle: EntityHandle) -> Option<(&str, i32, i32)> {
        self.grid_pos.get(&handle).map(|(g, x, y)| (g.as_str(), *x, *y))
    }

    pub fn handles_at_cell(&self, grid_id: &str, x: i32, y: i32) -> &HashSet<EntityHandle> {
        static EMPTY: std::sync::OnceLock<HashSet<EntityHandle>> = std::sync::OnceLock::new();
        self.grid_cell
            .get(&(grid_id.to_string(), x, y))
            .unwrap_or_else(|| EMPTY.get_or_init(HashSet::new))
    }

    // ── Transition candidates ────────────────────────────────────────────────

    pub fn mark_transition_candidate(&mut self, class: &str, handle: EntityHandle) {
        self.transition_candidates.entry(class.to_string()).or_default().insert(handle);
    }

    pub fn unmark_transition_candidate(&mut self, class: &str, handle: EntityHandle) {
        if let Some(set) = self.transition_candidates.get_mut(class) {
            set.remove(&handle);
        }
    }

    pub fn candidates_for_class(&self, class: &str) -> &HashSet<EntityHandle> {
        static EMPTY: std::sync::OnceLock<HashSet<EntityHandle>> = std::sync::OnceLock::new();
        self.transition_candidates
            .get(class)
            .unwrap_or_else(|| EMPTY.get_or_init(HashSet::new))
    }

    // ── Full entity removal ──────────────────────────────────────────────────

    /// Remove all index entries for a handle. Call before erasing from EntityStore.
    pub fn remove_all(&mut self, handle: EntityHandle, class: &str, overlay_name: Option<&str>) {
        self.remove_class(class, handle);
        self.clear_owner(handle);
        // Remove all entities this handle owned.
        if let Some(owned) = self.owned_by.remove(&handle) {
            for child in owned {
                self.owner_of.remove(&child);
            }
        }
        self.overlays_on_host.remove(&handle);
        if let Some(name) = overlay_name {
            if let Some(set) = self.hosts_of_overlay.get_mut(name) {
                set.remove(&handle);
            }
        }
        self.links_out.remove(&handle);
        self.links_in.remove(&handle);
        self.remove_from_grid(handle);
        self.unmark_transition_candidate(class, handle);
    }
}
