// actions/grid.rs
//
// Grid builtins exposed to Goblin as :name(...).
//
// All functions receive (&mut Session, &[Value], &Span) and return Result<Value, Diag>,
// matching the convention of every other action module.
//
// Phase 1 builtins:
//
//   World construction
//     :grid_new(name, width, height)               -- 4-neighbor default
//     :grid_new(name, width, height, mode)          -- mode: 4, 8, "hex", "wrapped"
//
//   Cell references
//     :grid_ref(world, x, y)                       -- produces Value::GridRef
//
//   Read / write
//     :grid_get(world, x, y, layer)                -- returns Value or Nil
//     :grid_set(world, x, y, layer, value)         -- sets cell, returns Unit
//     :grid_void(world, x, y)                      -- marks cell void, returns Unit
//
//   Neighbors
//     :grid_neighbors(world, x, y)                 -- returns Array of GridRef
//
//   Tick
//     :grid_tick_begin(world)                      -- snapshot for tick_db
//     :grid_tick_commit(world)                     -- commit and discard snapshot
//
//   Queries
//     :grid_occupied(world, layer)                 -- Array of GridRef (occupied)
//     :grid_occupied_count(world, layer)           -- Int
//     :grid_count(world, layer, value)             -- Int (cells matching value)
//     :grid_cells_with(world, layer, value)        -- Array of GridRef
//
//   Introspection
//     :grid_info(world)                            -- Map of world metadata

use crate::{Diag, Session, Span, Value};
use crate::diagnostics::rtcode;
use crate::actions::grid_store::{CellState, GridWorld, NeighborMode};
use goblin_diagnostics::{Diagnostic, Severity};

// ── Internal helpers ─────────────────────────────────────────────────────────

/// Pull a string argument by position.
fn want_str_arg<'a>(args: &'a [Value], pos: usize, label: &str, sp: &Span) -> Result<&'a str, Diag> {
    match args.get(pos) {
        Some(Value::Str(s)) => Ok(s.as_str()),
        Some(other) => Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            &format!("{} must be a string, got {:?}", label, other),
            sp.clone(),
        )
        .with_help(&format!("Pass a string for {}.", label))
        .with_link("https://goblinlang.org/docs/errors#T0205")),
        None => Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("missing argument: {}", label),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301")),
    }
}

/// Pull an i32 coordinate argument.
fn want_coord(args: &[Value], pos: usize, label: &str, sp: &Span) -> Result<i32, Diag> {
    match args.get(pos) {
        Some(Value::Int(n)) => {
            i32::try_from(*n).map_err(|_| Diagnostic::new_with_code(
                Severity::Error,
                rtcode::TYPE_MISMATCH,
                "type-mismatch",
                &format!("{} is out of coordinate range", label),
                sp.clone(),
            )
            .with_link("https://goblinlang.org/docs/errors#T0205"))
        }
        Some(other) => Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            &format!("{} must be an integer, got {:?}", label, other),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0205")),
        None => Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("missing argument: {}", label),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301")),
    }
}

/// Resolve a world name from a Value::Str or Value::GridRef.
fn world_name_from(v: &Value, label: &str, sp: &Span) -> Result<String, Diag> {
    match v {
        Value::Str(s) => Ok(s.clone()),
        Value::GridRef { grid_id, .. } => Ok(grid_id.clone()),
        other => Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            &format!("{} must be a world name (string) or GridRef, got {:?}", label, other),
            sp.clone(),
        )
        .with_help("Pass the world name as a string, e.g. \"world\".")
        .with_link("https://goblinlang.org/docs/errors#T0205")),
    }
}

/// Require a world to exist and return it mutably.
fn require_world_mut<'s>(sess: &'s mut Session, name: &str, sp: &Span) -> Result<&'s mut GridWorld, Diag> {
    if sess.grid_store.get(name).is_none() {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::UNKNOWN_IDENT,
            "unknown-grid",
            &format!("no grid named '{}'", name),
            sp.clone(),
        )
        .with_help("Create the grid first with :grid_new(name, width, height).")
        .with_link("https://goblinlang.org/docs/errors#R0101"));
    }
    Ok(sess.grid_store.get_mut(name).unwrap())
}

/// Require a world to exist and return it immutably.
fn require_world<'s>(sess: &'s Session, name: &str, sp: &Span) -> Result<&'s GridWorld, Diag> {
    sess.grid_store.get(name).ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            rtcode::UNKNOWN_IDENT,
            "unknown-grid",
            &format!("no grid named '{}'", name),
            sp.clone(),
        )
        .with_help("Create the grid first with :grid_new(name, width, height).")
        .with_link("https://goblinlang.org/docs/errors#R0101")
    })
}

/// Validate that (x, y) is in bounds for a world and not void.
fn require_in_bounds(world: &GridWorld, x: i32, y: i32, sp: &Span) -> Result<(), Diag> {
    if !world.in_bounds(x, y) {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::INVALID_INDEX,
            "grid-out-of-bounds",
            &format!("coordinate ({}, {}) is out of bounds for grid '{}' ({}x{})",
                x, y, world.name, world.width, world.height),
            sp.clone(),
        )
        .with_help("Coordinates must be within [0, width) and [0, height).")
        .with_link("https://goblinlang.org/docs/errors#R0401"));
    }
    Ok(())
}

/// Convert a (grid_id, x, y) triple to a Value::GridRef.
fn make_gridref(grid_id: &str, x: i32, y: i32) -> Value {
    Value::GridRef {
        grid_id: grid_id.to_string(),
        x,
        y,
    }
}

// ── :grid_new ────────────────────────────────────────────────────────────────

/// :grid_new(name, width, height)
/// :grid_new(name, width, height, mode)
///
/// mode: 4 (int), 8 (int), "hex" (str), "wrapped" (str)
///
/// Returns the world name as a string (so you can bind it: world | :grid_new("world", 1024, 1024))
pub fn grid_new(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() < 3 || args.len() > 4 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_new expects 3 or 4 arguments (name, width, height[, mode]), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_new(\"world\", 1024, 1024) or :grid_new(\"world\", 1024, 1024, 8)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let name = want_str_arg(args, 0, "grid name", sp)?.to_string();
    let width = want_coord(args, 1, "width", sp)?;
    let height = want_coord(args, 2, "height", sp)?;

    if width <= 0 || height <= 0 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "grid-bad-size",
            &format!("grid dimensions must be positive, got {}x{}", width, height),
            sp.clone(),
        )
        .with_help("Use positive integers for width and height.")
        .with_link("https://goblinlang.org/docs/errors#T0205"));
    }

    let mode = if let Some(mode_val) = args.get(3) {
        match mode_val {
            Value::Int(n) => {
                NeighborMode::from_int(*n).ok_or_else(|| Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "grid-bad-mode",
                    &format!("unknown neighbor mode {}; use 4 or 8", n),
                    sp.clone(),
                )
                .with_help("Valid integer modes: 4 (cardinal), 8 (cardinal + diagonal).")
                .with_link("https://goblinlang.org/docs/errors#T0205"))?
            }
            Value::Str(s) => {
                NeighborMode::from_str(s).ok_or_else(|| Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH,
                    "grid-bad-mode",
                    &format!("unknown neighbor mode \"{}\"; use \"hex\" or \"wrapped\"", s),
                    sp.clone(),
                )
                .with_help("Valid string modes: \"hex\", \"wrapped\".")
                .with_link("https://goblinlang.org/docs/errors#T0205"))?
            }
            other => return Err(Diagnostic::new_with_code(
                Severity::Error,
                rtcode::TYPE_MISMATCH,
                "type-mismatch",
                &format!("neighbor mode must be an integer or string, got {:?}", other),
                sp.clone(),
            )
            .with_link("https://goblinlang.org/docs/errors#T0205")),
        }
    } else {
        NeighborMode::Eight // spec default
    };

    if sess.grid_store.contains(&name) {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            "G0001",
            "grid-already-exists",
            &format!("a grid named '{}' already exists", name),
            sp.clone(),
        )
        .with_help("Use a different name or remove the existing grid first.")
        .with_link("https://goblinlang.org/docs/errors#G0001"));
    }

    let world = GridWorld::new(&name, width, height, mode);
    sess.grid_store.insert(world);

    Ok(Value::Str(name))
}

// ── :grid_ref ────────────────────────────────────────────────────────────────

/// :grid_ref(world, x, y)
///
/// Returns a Value::GridRef. Does NOT validate bounds here — a ref is just a
/// coordinate. Bounds are checked at get/set time.
pub fn grid_ref(_sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_ref expects 3 arguments (world, x, y), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_ref(\"world\", 40, 22)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;

    Ok(make_gridref(&grid_id, x, y))
}

// ── :grid_get ────────────────────────────────────────────────────────────────

/// :grid_get(world, x, y, layer)
///
/// Returns the Value at (x, y) in the given layer, or Nil if unoccupied.
/// Errors if out of bounds or void.
pub fn grid_get(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 4 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_get expects 4 arguments (world, x, y, layer), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_get(\"world\", 40, 22, \"owner\")")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();

    let world = require_world(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;

    if world.is_void(x, y) {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            "G0002",
            "grid-void-access",
            &format!("cell ({}, {}) is void in grid '{}'", x, y, grid_id),
            sp.clone(),
        )
        .with_help("Void cells cannot hold values. Check :grid_void() usage.")
        .with_link("https://goblinlang.org/docs/errors#G0002"));
    }

    // If tick_db is active, reads come from snapshot.
    let state = if world.has_snapshot() {
        world.get_snapshot(x, y, &layer).clone()
    } else {
        world.get(x, y, &layer).clone()
    };

    match state {
        CellState::Occupied(v) => Ok(v),
        CellState::Unoccupied => Ok(Value::Nil),
        CellState::Void => Ok(Value::Nil), // already guarded above
    }
}

// ── :grid_set ────────────────────────────────────────────────────────────────

/// :grid_set(world, x, y, layer, value)
///
/// Sets (x, y) in the given layer to value. Returns Unit.
/// Nil clears the cell (sets to Unoccupied).
pub fn grid_set(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 5 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_set expects 5 arguments (world, x, y, layer, value), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_set(\"world\", 40, 22, \"owner\", Russia)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;
    let layer = want_str_arg(args, 3, "layer", sp)?.to_string();
    let value = args[4].clone();

    {
        let world = require_world_mut(sess, &grid_id, sp)?;
        require_in_bounds(world, x, y, sp)?;

        if world.is_void(x, y) {
            return Err(Diagnostic::new_with_code(
                Severity::Error,
                "G0002",
                "grid-void-access",
                &format!("cannot set value on void cell ({}, {}) in grid '{}'", x, y, grid_id),
                sp.clone(),
            )
            .with_help("Void cells cannot hold values.")
            .with_link("https://goblinlang.org/docs/errors#G0002"));
        }

        let state = match value {
            Value::Nil => CellState::Unoccupied,
            v => CellState::Occupied(v),
        };

        world.set(x, y, &layer, state);
    }

    Ok(Value::Unit)
}

// ── :grid_void ───────────────────────────────────────────────────────────────

/// :grid_void(world, x, y)
///
/// Marks a cell as void across all layers. Returns Unit.
pub fn grid_void(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_void expects 3 arguments (world, x, y), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_void(\"world\", 4, 9)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;

    let world = require_world_mut(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;
    world.void_cell(x, y);

    Ok(Value::Unit)
}

// ── :grid_neighbors ──────────────────────────────────────────────────────────

/// :grid_neighbors(world, x, y)
///
/// Returns an Array of Value::GridRef for all valid, non-void neighbors.
pub fn grid_neighbors(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_neighbors expects 3 arguments (world, x, y), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_neighbors(\"world\", 40, 22)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let x = want_coord(args, 1, "x", sp)?;
    let y = want_coord(args, 2, "y", sp)?;

    let world = require_world(sess, &grid_id, sp)?;
    require_in_bounds(world, x, y, sp)?;

    let refs: Vec<Value> = world
        .neighbors(x, y)
        .into_iter()
        .map(|(nx, ny)| make_gridref(&grid_id, nx, ny))
        .collect();

    Ok(Value::Array(refs))
}

// ── :grid_tick_begin / :grid_tick_commit ─────────────────────────────────────

/// :grid_tick_begin(world)
///
/// Snapshots all layers. During tick_db(), reads use the snapshot;
/// writes go to live layers. Returns Unit.
pub fn grid_tick_begin(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_tick_begin expects 1 argument (world), got {}", args.len()),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world_mut(sess, &grid_id, sp)?;
    world.tick_db_begin();

    Ok(Value::Unit)
}

/// :grid_tick_commit(world)
///
/// Discards the snapshot and makes live layers canonical. Returns Unit.
pub fn grid_tick_commit(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_tick_commit expects 1 argument (world), got {}", args.len()),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world_mut(sess, &grid_id, sp)?;
    world.tick_db_commit();

    Ok(Value::Unit)
}

// ── Query builtins ───────────────────────────────────────────────────────────

/// :grid_occupied(world, layer)
///
/// Returns an Array of GridRef for all occupied (non-void, non-nil) cells
/// in the given layer.
pub fn grid_occupied(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_occupied expects 2 arguments (world, layer), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_occupied(\"world\", \"owner\")")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?.to_string();
    let world = require_world(sess, &grid_id, sp)?;

    let refs: Vec<Value> = world
        .occupied(&layer)
        .into_iter()
        .map(|(x, y)| make_gridref(&grid_id, x, y))
        .collect();

    Ok(Value::Array(refs))
}

/// :grid_occupied_count(world, layer)
///
/// Returns Int count of occupied cells in the given layer.
pub fn grid_occupied_count(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_occupied_count expects 2 arguments (world, layer), got {}", args.len()),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?.to_string();
    let world = require_world(sess, &grid_id, sp)?;

    let count = world.occupied(&layer).len();
    Ok(Value::Int(count as i64))
}

/// :grid_count(world, layer, value)
///
/// Returns Int count of cells in the given layer whose value equals `value`.
pub fn grid_count(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_count expects 3 arguments (world, layer, value), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_count(\"world\", \"state\", 1)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?.to_string();
    let target = &args[2];
    let world = require_world(sess, &grid_id, sp)?;

    let count = world.count_value(&layer, target);
    Ok(Value::Int(count as i64))
}

/// :grid_cells_with(world, layer, value)
///
/// Returns an Array of GridRef for all cells in the given layer whose
/// value equals `value`.
pub fn grid_cells_with(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 3 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_cells_with expects 3 arguments (world, layer, value), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :grid_cells_with(\"world\", \"state\", 1)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let layer = want_str_arg(args, 1, "layer", sp)?.to_string();
    let target = &args[2];
    let world = require_world(sess, &grid_id, sp)?;

    let refs: Vec<Value> = world
        .cells_with_value(&layer, target)
        .into_iter()
        .map(|(x, y)| make_gridref(&grid_id, x, y))
        .collect();

    Ok(Value::Array(refs))
}

// ── :grid_info ───────────────────────────────────────────────────────────────

/// :grid_info(world)
///
/// Returns a Map with world metadata:
///   { "name", "width", "height", "mode", "layers", "has_snapshot" }
pub fn grid_info(sess: &mut Session, args: &[Value], sp: &Span) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("grid_info expects 1 argument (world), got {}", args.len()),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let grid_id = world_name_from(&args[0], "world", sp)?;
    let world = require_world(sess, &grid_id, sp)?;

    let mode_str = match world.neighbor_mode {
        NeighborMode::Four    => "4",
        NeighborMode::Eight   => "8",
        NeighborMode::Hex     => "hex",
        NeighborMode::Wrapped => "wrapped",
    };

    let layer_arr = Value::Array(
        world.layer_names().into_iter().map(Value::Str).collect()
    );

    let mut map = std::collections::BTreeMap::new();
    map.insert("name".into(),         Value::Str(world.name.clone()));
    map.insert("width".into(),        Value::Int(world.width as i64));
    map.insert("height".into(),       Value::Int(world.height as i64));
    map.insert("mode".into(),         Value::Str(mode_str.into()));
    map.insert("layers".into(),       layer_arr);
    map.insert("has_snapshot".into(), Value::Bool(world.has_snapshot()));

    Ok(Value::Map(map))
}
