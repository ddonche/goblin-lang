// grid_store.rs
//
// Runtime grid state owned by Session.
//
// This is NOT an action module — it is core runtime state, the same way
// object_store, overlay_instances, and link_defs are core runtime state.
//
// Phase 1: sparse HashMap-backed storage, double-buffer tick support.
// Phase 2 (later): compact u32-indexed flat arrays for large worlds.

use std::collections::HashMap;
use crate::Value;

// ── Neighbor mode ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NeighborMode {
    Four,    // cardinal only
    Eight,   // cardinal + diagonal
    Hex,     // offset hex grid
    Wrapped, // 8-neighbor with toroidal wrap
}

impl NeighborMode {
    pub fn from_int(n: i64) -> Option<Self> {
        match n {
            4 => Some(Self::Four),
            8 => Some(Self::Eight),
            _ => None,
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "4"       => Some(Self::Four),
            "8"       => Some(Self::Eight),
            "hex"     => Some(Self::Hex),
            "wrapped" => Some(Self::Wrapped),
            _ => None,
        }
    }
}

// ── Cell state ───────────────────────────────────────────────────────────────

/// What a cell contains in a given layer.
#[derive(Debug, Clone, PartialEq)]
pub enum CellState {
    /// Cell has an assigned value (object UUID string, raw integer, etc.)
    Occupied(Value),
    /// Cell is empty but reachable.
    Unoccupied,
    /// Cell cannot hold any value — blocked from occupancy, spread, neighbors.
    Void,
}

impl CellState {
    pub fn is_void(&self) -> bool {
        matches!(self, CellState::Void)
    }
    pub fn is_occupied(&self) -> bool {
        matches!(self, CellState::Occupied(_))
    }
}

// ── Grid layer ───────────────────────────────────────────────────────────────

/// A single named map layer (e.g. "owner", "terrain", "plague").
///
/// Cells are sparse: absent means Unoccupied.
/// Void cells are stored explicitly so queries exclude them.
#[derive(Debug, Clone)]
pub struct GridLayer {
    pub name: String,
    /// Sparse cell data. Key = (x, y). Missing = Unoccupied.
    cells: HashMap<(i32, i32), CellState>,
}

impl GridLayer {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            cells: HashMap::new(),
        }
    }

    pub fn get(&self, x: i32, y: i32) -> &CellState {
        self.cells.get(&(x, y)).unwrap_or(&CellState::Unoccupied)
    }

    pub fn set(&mut self, x: i32, y: i32, state: CellState) {
        match &state {
            CellState::Unoccupied => {
                // Remove the entry — unoccupied is the default.
                self.cells.remove(&(x, y));
            }
            _ => {
                self.cells.insert((x, y), state);
            }
        }
    }

    pub fn void(&mut self, x: i32, y: i32) {
        self.cells.insert((x, y), CellState::Void);
    }

    pub fn is_void(&self, x: i32, y: i32) -> bool {
        matches!(self.cells.get(&(x, y)), Some(CellState::Void))
    }

    /// All occupied (non-void, non-empty) cells in this layer.
    pub fn occupied_cells(&self) -> impl Iterator<Item = ((i32, i32), &Value)> {
        self.cells.iter().filter_map(|(&coord, state)| {
            if let CellState::Occupied(v) = state {
                Some((coord, v))
            } else {
                None
            }
        })
    }

    /// Count of cells matching a specific value in this layer.
    pub fn count_value(&self, target: &Value) -> usize {
        self.cells.values().filter(|s| {
            matches!(s, CellState::Occupied(v) if v == target)
        }).count()
    }

    /// All cells set to a specific value.
    pub fn cells_with_value(&self, target: &Value) -> Vec<(i32, i32)> {
        self.cells.iter().filter_map(|(&coord, state)| {
            if matches!(state, CellState::Occupied(v) if v == target) {
                Some(coord)
            } else {
                None
            }
        }).collect()
    }

    /// Snapshot of current cells — used for tick_db double-buffer.
    pub fn snapshot(&self) -> Self {
        Self {
            name: self.name.clone(),
            cells: self.cells.clone(),
        }
    }
}

// ── GridWorld ────────────────────────────────────────────────────────────────

/// The full spatial system for a single named world.
///
/// A world has:
///   - dimensions (width × height)
///   - neighbor mode
///   - named layers (each layer is an independent spatial map)
///   - a double-buffer snapshot (populated during tick_db)
///
/// The "owner" layer is the canonical object placement layer.
/// All other layers are user-defined (terrain, plague, heat, etc.).
#[derive(Debug, Clone)]
pub struct GridWorld {
    pub name: String,
    pub width: i32,
    pub height: i32,
    pub neighbor_mode: NeighborMode,
    /// Named map layers. "owner" is always present after construction.
    layers: HashMap<String, GridLayer>,
    /// Double-buffer snapshot: Some(_) while tick_db is active.
    snapshot: Option<HashMap<String, GridLayer>>,
}

impl GridWorld {
    pub fn new(name: impl Into<String>, width: i32, height: i32, neighbor_mode: NeighborMode) -> Self {
        let mut layers = HashMap::new();
        layers.insert("owner".into(), GridLayer::new("owner"));
        Self {
            name: name.into(),
            width,
            height,
            neighbor_mode,
            layers,
            snapshot: None,
        }
    }

    // ── Bounds ────────────────────────────────────────────────────────────────

    pub fn in_bounds(&self, x: i32, y: i32) -> bool {
        x >= 0 && y >= 0 && x < self.width && y < self.height
    }

    // ── Layer access ─────────────────────────────────────────────────────────

    /// Get a layer by name, creating it if it doesn't exist.
    pub fn layer_or_create(&mut self, name: &str) -> &mut GridLayer {
        if !self.layers.contains_key(name) {
            self.layers.insert(name.to_string(), GridLayer::new(name));
        }
        self.layers.get_mut(name).unwrap()
    }

    pub fn layer(&self, name: &str) -> Option<&GridLayer> {
        self.layers.get(name)
    }

    pub fn layer_mut(&mut self, name: &str) -> Option<&mut GridLayer> {
        self.layers.get_mut(name)
    }

    pub fn layer_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.layers.keys().cloned().collect();
        names.sort();
        names
    }

    // ── Cell get/set/void ────────────────────────────────────────────────────

    pub fn get(&self, x: i32, y: i32, layer: &str) -> &CellState {
        self.layers
            .get(layer)
            .map(|l| l.get(x, y))
            .unwrap_or(&CellState::Unoccupied)
    }

    pub fn set(&mut self, x: i32, y: i32, layer: &str, state: CellState) {
        self.layer_or_create(layer).set(x, y, state);
    }

    pub fn void_cell(&mut self, x: i32, y: i32) {
        // Void applies to ALL layers simultaneously — a void cell is void everywhere.
        for layer in self.layers.values_mut() {
            layer.void(x, y);
        }
    }

    pub fn is_void(&self, x: i32, y: i32) -> bool {
        // A cell is void if any layer marks it void.
        self.layers.values().any(|l| l.is_void(x, y))
    }

    // ── Neighbor calculation ─────────────────────────────────────────────────

    /// Returns all valid, non-void neighbor coordinates for (x, y).
    pub fn neighbors(&self, x: i32, y: i32) -> Vec<(i32, i32)> {
        let candidates = match self.neighbor_mode {
            NeighborMode::Four => vec![
                (x,     y - 1),
                (x - 1, y    ),
                (x + 1, y    ),
                (x,     y + 1),
            ],
            NeighborMode::Eight | NeighborMode::Wrapped => vec![
                (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                (x - 1, y    ),              (x + 1, y    ),
                (x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
            ],
            NeighborMode::Hex => {
                // Offset hex (even-row offset)
                if y % 2 == 0 {
                    vec![
                        (x - 1, y - 1), (x, y - 1),
                        (x - 1, y    ), (x + 1, y),
                        (x - 1, y + 1), (x, y + 1),
                    ]
                } else {
                    vec![
                        (x, y - 1), (x + 1, y - 1),
                        (x - 1, y), (x + 1, y    ),
                        (x, y + 1), (x + 1, y + 1),
                    ]
                }
            }
        };

        candidates
            .into_iter()
            .map(|(nx, ny)| {
                // Wrapped mode: toroidal wrap
                if self.neighbor_mode == NeighborMode::Wrapped {
                    let wx = nx.rem_euclid(self.width);
                    let wy = ny.rem_euclid(self.height);
                    (wx, wy)
                } else {
                    (nx, ny)
                }
            })
            .filter(|&(nx, ny)| {
                self.in_bounds(nx, ny) && !self.is_void(nx, ny)
            })
            .collect()
    }

    // ── Query helpers ────────────────────────────────────────────────────────

    /// All occupied cells in a layer.
    pub fn occupied(&self, layer: &str) -> Vec<(i32, i32)> {
        self.layers
            .get(layer)
            .map(|l| l.occupied_cells().map(|(c, _)| c).collect())
            .unwrap_or_default()
    }

    /// Count of cells set to a specific value in a layer.
    pub fn count_value(&self, layer: &str, target: &Value) -> usize {
        self.layers
            .get(layer)
            .map(|l| l.count_value(target))
            .unwrap_or(0)
    }

    /// All cells with a specific value in a layer.
    pub fn cells_with_value(&self, layer: &str, target: &Value) -> Vec<(i32, i32)> {
        self.layers
            .get(layer)
            .map(|l| l.cells_with_value(target))
            .unwrap_or_default()
    }

    // ── Double-buffer tick ───────────────────────────────────────────────────

    /// Snapshot current state. Called at the start of tick_db().
    /// All reads during tick_db() come from the snapshot.
    /// All writes go to the live layers.
    pub fn tick_db_begin(&mut self) {
        let snap: HashMap<String, GridLayer> = self.layers
            .iter()
            .map(|(k, v)| (k.clone(), v.snapshot()))
            .collect();
        self.snapshot = Some(snap);
    }

    /// Commit: snapshot is discarded, live layers are canonical.
    pub fn tick_db_commit(&mut self) {
        self.snapshot = None;
    }

    /// Read from snapshot if active, otherwise from live layer.
    /// Used by spread/CA builtins during a tick_db pass.
    pub fn get_snapshot(&self, x: i32, y: i32, layer: &str) -> &CellState {
        if let Some(ref snap) = self.snapshot {
            snap.get(layer)
                .map(|l| l.get(x, y))
                .unwrap_or(&CellState::Unoccupied)
        } else {
            self.get(x, y, layer)
        }
    }

    pub fn has_snapshot(&self) -> bool {
        self.snapshot.is_some()
    }
}

// ── GridStore ────────────────────────────────────────────────────────────────

/// Container for all GridWorld instances in a Session.
/// Owned by Session, passed as &mut to grid action builtins.
#[derive(Debug, Default)]
pub struct GridStore {
    worlds: HashMap<String, GridWorld>,
}

impl GridStore {
    pub fn new() -> Self {
        Self {
            worlds: HashMap::new(),
        }
    }

    pub fn insert(&mut self, world: GridWorld) {
        self.worlds.insert(world.name.clone(), world);
    }

    pub fn get(&self, name: &str) -> Option<&GridWorld> {
        self.worlds.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut GridWorld> {
        self.worlds.get_mut(name)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.worlds.contains_key(name)
    }

    pub fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.worlds.keys().cloned().collect();
        names.sort();
        names
    }
}
