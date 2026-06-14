// grid.rs — Grid data structures for the Goblin VM.
// Adapted from crates/goblin-interpreter/src/actions/grid_store.rs
// Uses the VM's Value type from crates/goblin-vm/src/value.rs.

use std::collections::HashMap;
use crate::value::Value;

// ── Neighbor mode ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NeighborMode {
    Four,
    Eight,
    Hex,
    Wrapped,
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

// ── Cell state ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum CellState {
    Occupied(Value),
    Unoccupied,
    Void,
}

impl CellState {
    pub fn is_void(&self) -> bool { matches!(self, CellState::Void) }
    pub fn is_occupied(&self) -> bool { matches!(self, CellState::Occupied(_)) }
}

// ── GridLayer (cell-level sparse storage) ────────────────────────────────────

#[derive(Debug, Clone)]
pub struct GridLayer {
    pub name: String,
    cells: HashMap<(i32, i32), CellState>,
}

impl GridLayer {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), cells: HashMap::new() }
    }

    pub fn get(&self, x: i32, y: i32) -> &CellState {
        self.cells.get(&(x, y)).unwrap_or(&CellState::Unoccupied)
    }

    pub fn set(&mut self, x: i32, y: i32, state: CellState) {
        match &state {
            CellState::Unoccupied => { self.cells.remove(&(x, y)); }
            _ => { self.cells.insert((x, y), state); }
        }
    }

    pub fn void(&mut self, x: i32, y: i32) {
        self.cells.insert((x, y), CellState::Void);
    }

    pub fn is_void(&self, x: i32, y: i32) -> bool {
        matches!(self.cells.get(&(x, y)), Some(CellState::Void))
    }

    pub fn occupied_cells(&self) -> impl Iterator<Item = ((i32, i32), &Value)> {
        self.cells.iter().filter_map(|(&coord, state)| {
            if let CellState::Occupied(v) = state { Some((coord, v)) } else { None }
        })
    }

    pub fn count_value(&self, target: &Value) -> usize {
        self.cells.values().filter(|s| {
            matches!(s, CellState::Occupied(v) if v == target)
        }).count()
    }

    pub fn cells_with_value(&self, target: &Value) -> Vec<(i32, i32)> {
        self.cells.iter().filter_map(|(&coord, state)| {
            if matches!(state, CellState::Occupied(v) if v == target) { Some(coord) } else { None }
        }).collect()
    }

    pub fn snapshot(&self) -> Self {
        Self { name: self.name.clone(), cells: self.cells.clone() }
    }
}

// ── GridTile ──────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct GridTile {
    state: HashMap<String, Value>,
}

impl GridTile {
    pub fn new() -> Self {
        Self { state: HashMap::new() }
    }

    pub fn get(&self, layer: &str) -> Option<&Value> {
        self.state.get(layer)
    }

    pub fn set(&mut self, layer: &str, value: Value) {
        match value {
            Value::Nil => { self.state.remove(layer); }
            v => { self.state.insert(layer.to_string(), v); }
        }
    }

    pub fn layer_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.state.keys().cloned().collect();
        names.sort();
        names
    }
}

// ── GridRegion ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct GridRegion {
    state: HashMap<String, Value>,
}

impl GridRegion {
    pub fn new() -> Self {
        Self { state: HashMap::new() }
    }

    pub fn get(&self, layer: &str) -> Option<&Value> {
        self.state.get(layer)
    }

    pub fn set(&mut self, layer: &str, value: Value) {
        match value {
            Value::Nil => { self.state.remove(layer); }
            v => { self.state.insert(layer.to_string(), v); }
        }
    }

    pub fn layer_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.state.keys().cloned().collect();
        names.sort();
        names
    }
}

// ── Hierarchy config ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct HierarchyConfig {
    pub tile_w: i32,
    pub tile_h: i32,
    pub tile_cols: i32,
    pub tile_rows: i32,
    pub tile_count: i32,
    pub region_dim: i32,
    pub region_count: i32,
    pub tiles_per_region_edge: i32,
}

impl HierarchyConfig {
    pub fn try_new(
        world_w: i32, world_h: i32,
        tile_w: i32, tile_h: i32,
        region_count: i32,
    ) -> Result<Self, String> {
        if world_w % tile_w != 0 {
            return Err(format!(
                "grid width {} is not divisible by tile width {}", world_w, tile_w
            ));
        }
        if world_h % tile_h != 0 {
            return Err(format!(
                "grid height {} is not divisible by tile height {}", world_h, tile_h
            ));
        }

        let tile_cols = world_w / tile_w;
        let tile_rows = world_h / tile_h;
        let total_tiles = tile_cols * tile_rows;

        let region_dim = (region_count as f64).sqrt() as i32;
        if region_dim * region_dim != region_count {
            return Err(format!(
                "region count {} is not a perfect square (valid: 1, 4, 9, 16, 25, 36, 49, 64...)",
                region_count
            ));
        }

        if tile_cols % region_dim != 0 {
            return Err(format!(
                "tile columns {} is not divisible by region dimension {}", tile_cols, region_dim
            ));
        }
        if tile_rows % region_dim != 0 {
            return Err(format!(
                "tile rows {} is not divisible by region dimension {}", tile_rows, region_dim
            ));
        }

        let tiles_per_region_edge = tile_cols / region_dim;

        Ok(Self {
            tile_w,
            tile_h,
            tile_cols,
            tile_rows,
            tile_count: total_tiles,
            region_dim,
            region_count,
            tiles_per_region_edge,
        })
    }

    pub fn try_default(world_w: i32, world_h: i32) -> Option<Self> {
        Self::try_new(world_w, world_h, 32, 32, 16).ok()
    }

    pub fn cell_to_tile(&self, x: i32, y: i32) -> (i32, i32) {
        (x / self.tile_w, y / self.tile_h)
    }

    pub fn tile_to_region(&self, tx: i32, ty: i32) -> (i32, i32) {
        (tx / self.tiles_per_region_edge, ty / self.tiles_per_region_edge)
    }

    pub fn cell_to_region(&self, x: i32, y: i32) -> (i32, i32) {
        let (tx, ty) = self.cell_to_tile(x, y);
        self.tile_to_region(tx, ty)
    }

    pub fn tile_index(&self, tx: i32, ty: i32) -> usize {
        (ty * self.tile_cols + tx) as usize
    }

    pub fn region_index(&self, rx: i32, ry: i32) -> usize {
        (ry * self.region_dim + rx) as usize
    }
}

// ── GridWorld ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct GridWorld {
    pub name: String,
    pub width: i32,
    pub height: i32,
    pub neighbor_mode: NeighborMode,
    pub hierarchy: Option<HierarchyConfig>,
    tiles: Vec<GridTile>,
    regions: Vec<GridRegion>,
    layers: HashMap<String, GridLayer>,
    world_defaults: HashMap<String, Value>,
    snapshot: Option<HashMap<String, GridLayer>>,
}

impl GridWorld {
    pub fn new(
        name: impl Into<String>,
        width: i32,
        height: i32,
        neighbor_mode: NeighborMode,
        hierarchy: Option<HierarchyConfig>,
    ) -> Self {
        let (tiles, regions) = if let Some(ref h) = hierarchy {
            (
                vec![GridTile::new(); h.tile_count as usize],
                vec![GridRegion::new(); h.region_count as usize],
            )
        } else {
            (vec![], vec![])
        };

        let mut layers = HashMap::new();
        layers.insert("owner".into(), GridLayer::new("owner"));

        Self {
            name: name.into(),
            width,
            height,
            neighbor_mode,
            hierarchy,
            tiles,
            regions,
            layers,
            world_defaults: HashMap::new(),
            snapshot: None,
        }
    }

    pub fn in_bounds(&self, x: i32, y: i32) -> bool {
        x >= 0 && y >= 0 && x < self.width && y < self.height
    }

    pub fn has_hierarchy(&self) -> bool {
        self.hierarchy.is_some()
    }

    pub fn tile_at_cell(&self, x: i32, y: i32) -> Option<&GridTile> {
        let h = self.hierarchy.as_ref()?;
        let (tx, ty) = h.cell_to_tile(x, y);
        let idx = h.tile_index(tx, ty);
        self.tiles.get(idx)
    }

    pub fn tile_at_cell_mut(&mut self, x: i32, y: i32) -> Option<&mut GridTile> {
        let (idx, _) = {
            let h = self.hierarchy.as_ref()?;
            let (tx, ty) = h.cell_to_tile(x, y);
            (h.tile_index(tx, ty), ())
        };
        self.tiles.get_mut(idx)
    }

    pub fn tile_at(&self, tx: i32, ty: i32) -> Option<&GridTile> {
        let h = self.hierarchy.as_ref()?;
        let idx = h.tile_index(tx, ty);
        self.tiles.get(idx)
    }

    pub fn tile_at_mut(&mut self, tx: i32, ty: i32) -> Option<&mut GridTile> {
        let (idx, _) = {
            let h = self.hierarchy.as_ref()?;
            (h.tile_index(tx, ty), ())
        };
        self.tiles.get_mut(idx)
    }

    pub fn region_at_cell(&self, x: i32, y: i32) -> Option<&GridRegion> {
        let h = self.hierarchy.as_ref()?;
        let (rx, ry) = h.cell_to_region(x, y);
        let idx = h.region_index(rx, ry);
        self.regions.get(idx)
    }

    pub fn region_at_cell_mut(&mut self, x: i32, y: i32) -> Option<&mut GridRegion> {
        let (idx, _) = {
            let h = self.hierarchy.as_ref()?;
            let (rx, ry) = h.cell_to_region(x, y);
            (h.region_index(rx, ry), ())
        };
        self.regions.get_mut(idx)
    }

    pub fn region_at(&self, rx: i32, ry: i32) -> Option<&GridRegion> {
        let h = self.hierarchy.as_ref()?;
        let idx = h.region_index(rx, ry);
        self.regions.get(idx)
    }

    pub fn region_at_mut(&mut self, rx: i32, ry: i32) -> Option<&mut GridRegion> {
        let (idx, _) = {
            let h = self.hierarchy.as_ref()?;
            (h.region_index(rx, ry), ())
        };
        self.regions.get_mut(idx)
    }

    pub fn resolve(&self, x: i32, y: i32, layer: &str) -> Option<&Value> {
        if let Some(cell_layer) = self.layers.get(layer) {
            if let CellState::Occupied(v) = cell_layer.get(x, y) {
                return Some(v);
            }
        }
        if let Some(tile) = self.tile_at_cell(x, y) {
            if let Some(v) = tile.get(layer) {
                return Some(v);
            }
        }
        if let Some(region) = self.region_at_cell(x, y) {
            if let Some(v) = region.get(layer) {
                return Some(v);
            }
        }
        self.world_defaults.get(layer)
    }

    pub fn set_world_default(&mut self, layer: &str, value: Value) {
        match value {
            Value::Nil => { self.world_defaults.remove(layer); }
            v => { self.world_defaults.insert(layer.to_string(), v); }
        }
    }

    pub fn get_world_default(&self, layer: &str) -> Option<&Value> {
        self.world_defaults.get(layer)
    }

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
        for layer in self.layers.values_mut() {
            layer.void(x, y);
        }
    }

    pub fn is_void(&self, x: i32, y: i32) -> bool {
        self.layers.values().any(|l| l.is_void(x, y))
    }

    pub fn neighbors(&self, x: i32, y: i32) -> Vec<(i32, i32)> {
        let candidates = match self.neighbor_mode {
            NeighborMode::Four => vec![
                (x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1),
            ],
            NeighborMode::Eight | NeighborMode::Wrapped => vec![
                (x-1, y-1), (x, y-1), (x+1, y-1),
                (x-1, y  ),            (x+1, y  ),
                (x-1, y+1), (x, y+1), (x+1, y+1),
            ],
            NeighborMode::Hex => {
                if y % 2 == 0 {
                    vec![
                        (x-1, y-1), (x, y-1),
                        (x-1, y  ), (x+1, y),
                        (x-1, y+1), (x, y+1),
                    ]
                } else {
                    vec![
                        (x, y-1), (x+1, y-1),
                        (x-1, y), (x+1, y  ),
                        (x, y+1), (x+1, y+1),
                    ]
                }
            }
        };

        candidates
            .into_iter()
            .map(|(nx, ny)| {
                if self.neighbor_mode == NeighborMode::Wrapped {
                    (nx.rem_euclid(self.width), ny.rem_euclid(self.height))
                } else {
                    (nx, ny)
                }
            })
            .filter(|&(nx, ny)| self.in_bounds(nx, ny) && !self.is_void(nx, ny))
            .collect()
    }

    pub fn occupied(&self, layer: &str) -> Vec<(i32, i32)> {
        self.layers
            .get(layer)
            .map(|l| l.occupied_cells().map(|(c, _)| c).collect())
            .unwrap_or_default()
    }

    pub fn unoccupied_cells(&self) -> Vec<(i32, i32)> {
        let mut result = Vec::new();
        for y in 0..self.height {
            for x in 0..self.width {
                if !self.is_void(x, y) {
                    if let CellState::Unoccupied = self.get(x, y, "owner") {
                        result.push((x, y));
                    }
                }
            }
        }
        result
    }

    pub fn count_value(&self, layer: &str, target: &Value) -> usize {
        self.layers.get(layer).map(|l| l.count_value(target)).unwrap_or(0)
    }

    pub fn cells_with_value(&self, layer: &str, target: &Value) -> Vec<(i32, i32)> {
        self.layers.get(layer).map(|l| l.cells_with_value(target)).unwrap_or_default()
    }

    pub fn tick_db_begin(&mut self) {
        let snap = self.layers.iter().map(|(k, v)| (k.clone(), v.snapshot())).collect();
        self.snapshot = Some(snap);
    }

    pub fn tick_db_commit(&mut self) {
        self.snapshot = None;
    }

    pub fn get_snapshot(&self, x: i32, y: i32, layer: &str) -> &CellState {
        if let Some(ref snap) = self.snapshot {
            snap.get(layer).map(|l| l.get(x, y)).unwrap_or(&CellState::Unoccupied)
        } else {
            self.get(x, y, layer)
        }
    }

    pub fn has_snapshot(&self) -> bool {
        self.snapshot.is_some()
    }
}

// ── GridStore ─────────────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub struct GridStore {
    worlds: HashMap<String, GridWorld>,
}

impl GridStore {
    pub fn new() -> Self { Self { worlds: HashMap::new() } }

    pub fn insert(&mut self, world: GridWorld) {
        self.worlds.insert(world.name.clone(), world);
    }

    pub fn get(&self, name: &str) -> Option<&GridWorld> { self.worlds.get(name) }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut GridWorld> { self.worlds.get_mut(name) }

    pub fn contains(&self, name: &str) -> bool { self.worlds.contains_key(name) }

    pub fn names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.worlds.keys().cloned().collect();
        names.sort();
        names
    }
}
