// actions/grid.rs
//
// Grid builtins — implementing the Goblin Grid System spec exactly.
//
// Builtins:
//
//   World construction
//     grid(name, width, height)
//     grid(name, width, height, mode)
//     grid(name, width, height, mode, tile_w, tile_h, regions)  ← from parser sub-block
//
//   Cell read / write
//     grid_get(world, x, y, layer)               → Value | nil
//     grid_set(world, x, y, layer, value)        → Unit
//     grid_void(world, x, y)                     → Unit
//
//   Tile read / write
//     grid_tile_get(world, tx, ty, layer)        → Value | nil
//     grid_tile_set(world, tx, ty, layer, value) → Unit
//
//   Region read / write
//     grid_region_get(world, rx, ry, layer)        → Value | nil
//     grid_region_set(world, rx, ry, layer, value) → Unit
//
//   World defaults
//     grid_default_set(world, layer, value)        → Unit
//     grid_default_get(world, layer)               → Value | nil
//
//   Neighbors
//     grid_neighbors(world, x, y)                  → Array of GridRef
//
//   Queries  (spec names)
//     grid_occupied(world)                         → Array of GridRef  (owner layer)
//     grid_unoccupied(world)                       → Array of GridRef  (owner layer)
//     grid_occupied_count(world)                   → Int
//     grid_unoccupied_count(world)                 → Int
//     grid_count(world, value)                     → Int  (counts owner layer)
//     grid_occupied_by(world, value)               → Array of GridRef
//     grid_has(world, value)                       → Bool
//
//   Introspection
//     grid_info(world)                             → Map
//     grid_tile_info(world, tx, ty)                → Map
//     grid_region_info(world, rx, ry)              → Map

use crate::{Diag, Session, Span, Value};
use crate::diagnostics::rtcode;
use crate::actions::grid_store::{CellState, GridWorld, HierarchyConfig, NeighborMode};
use goblin_diagnostics::{Diagnostic, Severity};

// ── Internal helpers ──────────────────────────────────────────────────────────

fn want_str_arg<'a>(args: &'a [Value], pos: usize, label: &str, sp: &Span) -> Result<&'a str, Diag> {
    match args.get(pos) {
        Some(Value::Str(s)) => Ok(s.as_str()),
        Some(other) => Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch",
            &format!("{} must be a string, got {:?}", label, other), sp.clone(),
        ).with_help(&format!("Pass a string for {}.", label))
         .with_link("https://goblinlang.org/docs/errors#T0205")),
        None => Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("missing argument: {}", label), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301")),
    }
}

fn want_coord(args: &[Value], pos: usize, label: &str, sp: &Span) -> Result<i32, Diag> {
    match args.get(pos) {
        Some(Value::Int(n)) => i32::try_from(*n).map_err(|_| Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch",
            &format!("{} is out of coordinate range", label), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#T0205")),
        Some(other) => Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch",
            &format!("{} must be an integer, got {:?}", label, other), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#T0205")),
        None => Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("missing argument: {}", label), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301")),
    }
}

fn world_name_from(v: &Value, label: &str, sp: &Span) -> Result<String, Diag> {
    match v {
        Value::Str(s) => Ok(s.clone()),
        Value::GridRef { grid_id, .. } => Ok(grid_id.clone()),
        other => Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch",
            &format!("{} must be a world name (string) or GridRef, got {:?}", label, other), sp.clone(),
        ).with_help("Pass the world name as a string.")
         .with_link("https://goblinlang.org/docs/errors#T0205")),
    }
}

fn require_world_mut<'s>(sess: &'s mut Session, name: &str, sp: &Span) -> Result<&'s mut GridWorld, Diag> {
    if sess.grid_store.get(name).is_none() {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::UNKNOWN_IDENT, "unknown-grid",
            &format!("no grid named '{}'", name), sp.clone(),
        ).with_help("Create the grid first with: world | grid(\"world\", 1024, 1024)")
         .with_link("https://goblinlang.org/docs/errors#R0101"));
    }
    Ok(sess.grid_store.get_mut(name).unwrap())
}

fn require_world<'s>(sess: &'s Session, name: &str, sp: &Span) -> Result<&'s GridWorld, Diag> {
    sess.grid_store.get(name).ok_or_else(|| Diagnostic::new_with_code(
        Severity::Error, rtcode::UNKNOWN_IDENT, "unknown-grid",
        &format!("no grid named '{}'", name), sp.clone(),
    ).with_help("Create the grid first with: world | grid(\"world\", 1024, 1024)")
     .with_link("https://goblinlang.org/docs/errors#R0101"))
}

fn require_in_bounds(world: &GridWorld, x: i32, y: i32, sp: &Span) -> Result<(), Diag> {
    if !world.in_bounds(x, y) {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::INVALID_INDEX, "grid-out-of-bounds",
            &format!("coordinate ({}, {}) is out of bounds for grid '{}' ({}x{})",
                x, y, world.name, world.width, world.height), sp.clone(),
        ).with_help("Coordinates must be within [0, width) and [0, height).")
         .with_link("https://goblinlang.org/docs/errors#R0401"));
    }
    Ok(())
}

fn require_tile_in_bounds(world: &GridWorld, tx: i32, ty: i32, sp: &Span) -> Result<(), Diag> {
    let h = world.hierarchy.as_ref().ok_or_else(|| Diagnostic::new_with_code(
        Severity::Error, "G0003", "grid-no-hierarchy",
        &format!("grid '{}' has no tile/region hierarchy", world.name), sp.clone(),
    ).with_help("Declare a hierarchy in grid: tile 32 by 32 / regions 16")
     .with_link("https://goblinlang.org/docs/errors#G0003"))?;
    if tx < 0 || ty < 0 || tx >= h.tile_cols || ty >= h.tile_rows {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::INVALID_INDEX, "grid-tile-out-of-bounds",
            &format!("tile ({}, {}) is out of bounds ({}x{} tiles)", tx, ty, h.tile_cols, h.tile_rows), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0401"));
    }
    Ok(())
}

fn require_region_in_bounds(world: &GridWorld, rx: i32, ry: i32, sp: &Span) -> Result<(), Diag> {
    let h = world.hierarchy.as_ref().ok_or_else(|| Diagnostic::new_with_code(
        Severity::Error, "G0003", "grid-no-hierarchy",
        &format!("grid '{}' has no tile/region hierarchy", world.name), sp.clone(),
    ).with_link("https://goblinlang.org/docs/errors#G0003"))?;
    if rx < 0 || ry < 0 || rx >= h.region_dim || ry >= h.region_dim {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::INVALID_INDEX, "grid-region-out-of-bounds",
            &format!("region ({}, {}) is out of bounds ({}x{} regions)", rx, ry, h.region_dim, h.region_dim), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0401"));
    }
    Ok(())
}

fn make_gridref(grid_id: &str, x: i32, y: i32) -> Value {
    Value::GridRef { grid_id: grid_id.to_string(), x, y }
}

// ── grid (world construction) ─────────────────────────────────────────────────

/// grid(name, width, height)
/// grid(name, width, height, mode)
/// grid(name, width, height, mode, tile_w, tile_h, region_count)  ← from parser sub-block
///
/// Returns the world name as a string.
pub fn grid(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 3 || (args.len() > 4 && args.len() != 7) {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid expects 3 or 4 arguments, got {}", args.len()), sp.clone(),
        ).with_help("Usage: world | grid(\"world\", 1024, 1024) or world | grid(\"world\", 1024, 1024, 8)")
         .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let name   = want_str_arg(args, 0, "grid name", sp)?.to_string();
    let width  = want_coord(args, 1, "width", sp)?;
    let height = want_coord(args, 2, "height", sp)?;

    if width <= 0 || height <= 0 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "grid-bad-size",
            &format!("grid dimensions must be positive, got {}x{}", width, height), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#T0205"));
    }

    let mode = match args.get(3) {
        None | Some(Value::Int(-1)) => NeighborMode::Eight,
        Some(Value::Int(n)) => NeighborMode::from_int(*n).ok_or_else(|| Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "grid-bad-mode",
            &format!("unknown neighbor mode {}; use 4 or 8", n), sp.clone(),
        ).with_help("Valid integer modes: 4 (cardinal), 8 (cardinal + diagonal).")
         .with_link("https://goblinlang.org/docs/errors#T0205"))?,
        Some(Value::Str(s)) => NeighborMode::from_str(s).ok_or_else(|| Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "grid-bad-mode",
            &format!("unknown neighbor mode \"{}\"; use \"hex\" or \"wrapped\"", s), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#T0205"))?,
        Some(other) => return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch",
            &format!("neighbor mode must be an integer or string, got {:?}", other), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#T0205")),
    };

    if sess.grid_store.contains(&name) {
        return Err(Diagnostic::new_with_code(
            Severity::Error, "G0001", "grid-already-exists",
            &format!("a grid named '{}' already exists", name), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#G0001"));
    }

    let hierarchy = if args.len() == 7 {
        let tile_w  = want_coord(args, 4, "tile_w", sp)?;
        let tile_h  = want_coord(args, 5, "tile_h", sp)?;
        let regions = want_coord(args, 6, "regions", sp)?;

        if tile_w == -1 && tile_h == -1 && regions == -1 {
            HierarchyConfig::try_default(width, height)
        } else {
            let tw = if tile_w  == -1 { 32 } else { tile_w };
            let th = if tile_h  == -1 { 32 } else { tile_h };
            let rc = if regions == -1 { 16 } else { regions };
            match HierarchyConfig::try_new(width, height, tw, th, rc) {
                Ok(h) => Some(h),
                Err(msg) => return Err(Diagnostic::new_with_code(
                    Severity::Error, "G0004", "grid-bad-hierarchy",
                    &format!("invalid grid hierarchy: {}", msg), sp.clone(),
                ).with_link("https://goblinlang.org/docs/errors#G0004")),
            }
        }
    } else {
        HierarchyConfig::try_default(width, height)
    };

    let world = GridWorld::new(&name, width, height, mode, hierarchy);
    sess.grid_store.insert(world);
    Ok(Value::Str(name))
}

// ── grid_get / grid_set / grid_void ──────────────────────────────────────────

pub fn grid_get(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 4 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_get expects 4 arguments (world, x, y, layer), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();

    let world = require_world(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;

    if world.is_void(x, y) {
        return Err(Diagnostic::new_with_code(
            Severity::Error, "G0002", "grid-void-access",
            &format!("cell ({}, {}) is void in grid '{}'", x, y, grid_id), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#G0002"));
    }

    match world.resolve(x, y, &layer) {
        Some(v) => Ok(v.clone()),
        None => Ok(Value::Nil),
    }
}

pub fn grid_set(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 5 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_set expects 5 arguments (world, x, y, layer, value), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();
    let value = args[4].clone();

    let world = require_world_mut(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;

    if world.is_void(x, y) {
        return Err(Diagnostic::new_with_code(
            Severity::Error, "G0002", "grid-void-access",
            &format!("cannot set value on void cell ({}, {}) in grid '{}'", x, y, grid_id), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#G0002"));
    }

    let state = match value {
        Value::Nil => CellState::Unoccupied,
        v => CellState::Occupied(v),
    };
    world.set(x, y, &layer, state);
    Ok(Value::Unit)
}

pub fn grid_void(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_void expects 3 arguments (world, x, y), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let world = require_world_mut(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;
    world.void_cell(x, y);
    Ok(Value::Unit)
}

// ── grid_tile_get / grid_tile_set ─────────────────────────────────────────────

pub fn grid_tile_get(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 4 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_tile_get expects 4 arguments (world, tx, ty, layer), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let tx = want_coord(args, 1, "tx", sp)?;
    let ty = want_coord(args, 2, "ty", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    require_tile_in_bounds(world, tx, ty, sp)?;
    match world.tile_at(tx, ty).and_then(|t| t.get(layer)) {
        Some(v) => Ok(v.clone()),
        None => Ok(Value::Nil),
    }
}

pub fn grid_tile_set(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 5 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_tile_set expects 5 arguments (world, tx, ty, layer, value), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let tx = want_coord(args, 1, "tx", sp)?;
    let ty = want_coord(args, 2, "ty", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();
    let value = args[4].clone();
    let world = require_world_mut(sess, &grid_id, sp)?;
    require_tile_in_bounds(world, tx, ty, sp)?;
    world.tile_at_mut(tx, ty).unwrap().set(&layer, value);
    Ok(Value::Unit)
}

// ── grid_region_get / grid_region_set ────────────────────────────────────────

pub fn grid_region_get(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 4 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_region_get expects 4 arguments (world, rx, ry, layer), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let rx = want_coord(args, 1, "rx", sp)?;
    let ry = want_coord(args, 2, "ry", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    require_region_in_bounds(world, rx, ry, sp)?;
    match world.region_at(rx, ry).and_then(|r| r.get(layer)) {
        Some(v) => Ok(v.clone()),
        None => Ok(Value::Nil),
    }
}

pub fn grid_region_set(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 5 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_region_set expects 5 arguments (world, rx, ry, layer, value), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let rx = want_coord(args, 1, "rx", sp)?;
    let ry = want_coord(args, 2, "ry", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();
    let value = args[4].clone();
    let world = require_world_mut(sess, &grid_id, sp)?;
    require_region_in_bounds(world, rx, ry, sp)?;
    world.region_at_mut(rx, ry).unwrap().set(&layer, value);
    Ok(Value::Unit)
}

// ── grid_default_set / grid_default_get ──────────────────────────────────────

pub fn grid_default_set(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_default_set expects 3 arguments (world, layer, value), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?.to_string();
    let value = args[2].clone();
    require_world_mut(sess, &grid_id, sp)?.set_world_default(&layer, value);
    Ok(Value::Unit)
}

pub fn grid_default_get(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_default_get expects 2 arguments (world, layer), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?;
    match require_world(sess, &grid_id, sp)?.get_world_default(layer) {
        Some(v) => Ok(v.clone()),
        None => Ok(Value::Nil),
    }
}

// ── grid_neighbors ────────────────────────────────────────────────────────────

pub fn grid_neighbors(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_neighbors expects 3 arguments (world, x, y), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;
    let refs: Vec<Value> = world.neighbors(x, y).into_iter()
        .map(|(nx, ny)| make_gridref(&grid_id, nx, ny))
        .collect();
    Ok(Value::Array(refs))
}

// ── Query builtins (spec names) ───────────────────────────────────────────────

/// grid_occupied(world) — all occupied cells in the owner layer
pub fn grid_occupied(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_occupied expects 1 argument (world), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    let refs: Vec<Value> = world.occupied("owner").into_iter()
        .map(|(x, y)| make_gridref(&grid_id, x, y))
        .collect();
    Ok(Value::Array(refs))
}

/// grid_unoccupied(world) — all unoccupied (non-void) cells in the owner layer
pub fn grid_unoccupied(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_unoccupied expects 1 argument (world), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    let refs: Vec<Value> = world.unoccupied_cells().into_iter()
        .map(|(x, y)| make_gridref(&grid_id, x, y))
        .collect();
    Ok(Value::Array(refs))
}

/// grid_occupied_count(world)
pub fn grid_occupied_count(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_occupied_count expects 1 argument (world), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    Ok(Value::Int(world.occupied("owner").len() as i64))
}

/// grid_unoccupied_count(world)
pub fn grid_unoccupied_count(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_unoccupied_count expects 1 argument (world), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    Ok(Value::Int(world.unoccupied_cells().len() as i64))
}

/// grid_count(world, value) — counts cells in owner layer matching value
pub fn grid_count(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_count expects 2 arguments (world, value), got {}", args.len()), sp.clone(),
        ).with_help("Usage: grid_count(world, 1) or grid_count(world, Russia)")
         .with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let target = &args[1];
    let world = require_world(sess, &grid_id, sp)?;
    Ok(Value::Int(world.count_value("owner", target) as i64))
}

/// grid_occupied_by(world, value) — cells in owner layer matching value
pub fn grid_occupied_by(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_occupied_by expects 2 arguments (world, value), got {}", args.len()), sp.clone(),
        ).with_help("Usage: grid_occupied_by(world, Russia)")
         .with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let target = &args[1];
    let world = require_world(sess, &grid_id, sp)?;
    let refs: Vec<Value> = world.cells_with_value("owner", target).into_iter()
        .map(|(x, y)| make_gridref(&grid_id, x, y))
        .collect();
    Ok(Value::Array(refs))
}

/// grid_has(world, value) — true if any cell in owner layer has this value
pub fn grid_has(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_has expects 2 arguments (world, value), got {}", args.len()), sp.clone(),
        ).with_help("Usage: grid_has(world, Plague)")
         .with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let target = &args[1];
    let world = require_world(sess, &grid_id, sp)?;
    Ok(Value::Bool(world.count_value("owner", target) > 0))
}

// ── tick_db support ───────────────────────────────────────────────────────────

/// Called by the runtime's tick_db() — snapshots all grids in the session.
pub fn all_grids_tick_db_begin(sess: &mut Session) {
    for name in sess.grid_store.names() {
        if let Some(world) = sess.grid_store.get_mut(&name) {
            world.tick_db_begin();
        }
    }
}

/// Called by the runtime's tick_db() after evaluation — commits all grids.
pub fn all_grids_tick_db_commit(sess: &mut Session) {
    for name in sess.grid_store.names() {
        if let Some(world) = sess.grid_store.get_mut(&name) {
            world.tick_db_commit();
        }
    }
}

// ── Introspection ─────────────────────────────────────────────────────────────

pub fn grid_info(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_info expects 1 argument (world), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;

    let mode_str = match world.neighbor_mode {
        NeighborMode::Four    => "4",
        NeighborMode::Eight   => "8",
        NeighborMode::Hex     => "hex",
        NeighborMode::Wrapped => "wrapped",
    };

    let layer_arr = Value::Array(world.layer_names().into_iter().map(Value::Str).collect());
    let mut map = std::collections::BTreeMap::new();
    map.insert("name".into(),         Value::Str(world.name.clone()));
    map.insert("width".into(),        Value::Int(world.width as i64));
    map.insert("height".into(),       Value::Int(world.height as i64));
    map.insert("mode".into(),         Value::Str(mode_str.into()));
    map.insert("layers".into(),       layer_arr);
    map.insert("has_snapshot".into(), Value::Bool(world.has_snapshot()));

    if let Some(ref h) = world.hierarchy {
        map.insert("has_hierarchy".into(), Value::Bool(true));
        map.insert("tile_w".into(),        Value::Int(h.tile_w as i64));
        map.insert("tile_h".into(),        Value::Int(h.tile_h as i64));
        map.insert("tile_cols".into(),     Value::Int(h.tile_cols as i64));
        map.insert("tile_rows".into(),     Value::Int(h.tile_rows as i64));
        map.insert("tile_count".into(),    Value::Int(h.tile_count as i64));
        map.insert("region_count".into(),  Value::Int(h.region_count as i64));
        map.insert("region_dim".into(),    Value::Int(h.region_dim as i64));
    } else {
        map.insert("has_hierarchy".into(), Value::Bool(false));
    }

    Ok(Value::Map(map))
}

pub fn grid_tile_info(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_tile_info expects 3 arguments (world, tx, ty), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let tx = want_coord(args, 1, "tx", sp)?;
    let ty = want_coord(args, 2, "ty", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    require_tile_in_bounds(world, tx, ty, sp)?;
    let h = world.hierarchy.as_ref().unwrap();
    let tile = world.tile_at(tx, ty).unwrap();
    let (rx, ry) = h.tile_to_region(tx, ty);
    let mut map = std::collections::BTreeMap::new();
    map.insert("tx".into(),       Value::Int(tx as i64));
    map.insert("ty".into(),       Value::Int(ty as i64));
    map.insert("cell_x".into(),   Value::Int((tx * h.tile_w) as i64));
    map.insert("cell_y".into(),   Value::Int((ty * h.tile_h) as i64));
    map.insert("region_x".into(), Value::Int(rx as i64));
    map.insert("region_y".into(), Value::Int(ry as i64));
    map.insert("layers".into(),   Value::Array(tile.layer_names().into_iter().map(Value::Str).collect()));
    Ok(Value::Map(map))
}

pub fn grid_region_info(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error, rtcode::WRONG_ARITY, "wrong-arity",
            &format!("grid_region_info expects 3 arguments (world, rx, ry), got {}", args.len()), sp.clone(),
        ).with_link("https://goblinlang.org/docs/errors#R0301"));
    }
    let grid_id = world_name_from(&args[0], "world", sp)?;
    let rx = want_coord(args, 1, "rx", sp)?;
    let ry = want_coord(args, 2, "ry", sp)?;
    let world = require_world(sess, &grid_id, sp)?;
    require_region_in_bounds(world, rx, ry, sp)?;
    let h = world.hierarchy.as_ref().unwrap();
    let region = world.region_at(rx, ry).unwrap();
    let tile_x = rx * h.tiles_per_region_edge;
    let tile_y = ry * h.tiles_per_region_edge;
    let mut map = std::collections::BTreeMap::new();
    map.insert("rx".into(),             Value::Int(rx as i64));
    map.insert("ry".into(),             Value::Int(ry as i64));
    map.insert("cell_x".into(),         Value::Int((tile_x * h.tile_w) as i64));
    map.insert("cell_y".into(),         Value::Int((tile_y * h.tile_h) as i64));
    map.insert("tile_x".into(),         Value::Int(tile_x as i64));
    map.insert("tile_y".into(),         Value::Int(tile_y as i64));
    map.insert("tiles_per_edge".into(), Value::Int(h.tiles_per_region_edge as i64));
    map.insert("layers".into(),         Value::Array(region.layer_names().into_iter().map(Value::Str).collect()));
    Ok(Value::Map(map))
}
