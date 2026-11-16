//! The grid follows the DES: a cell that names an object (on any layer of
//! any grid) goes with that object through its transitions.
//!
//! - absorb, merge: the cells go to the absorber / the merged object
//!   (`hand_over`, called where the old object leaves the world).
//! - split: the cells are divided between the two successors (`divide`).
//! - fracture: the fragment takes the far side of the cells (`divide`).
//! - erase: the cells become unclaimed.
//! - subjugate, mutate, spawn: the cells stay where they are.
//!
//! A division grows both parts at once from two seed cells through the
//! object's own cells (neighbors as the grid counts them; void cells never
//! join), so each part is one connected piece where the land allows it.
//! Each cell takes a random number of steps to cross (the program's dice),
//! so the new border wanders instead of running straight. A
//! split's seeds are the two cells farthest apart (the first successor
//! takes the side nearer the top-left cell); a fracture's are the cell
//! nearest the middle (stays) and the cell farthest from it (the fragment).
//! Pieces cut off from both seeds (islands) go whole to the nearer seed.
//! Every change is a grid write, so regions see it like any other.

use crate::grid::{CellState, GridWrite};
use crate::session::Session;
use crate::value::Value;
use std::collections::HashMap;

/// The cells naming `id`, by grid and layer (both sorted), cells by row.
fn cells_of(s: &Session, id: &str) -> Vec<((String, String), Vec<(i32, i32)>)> {
    let mut out = Vec::new();
    for g in s.grid_store.names() {
        let Some(world) = s.grid_store.get(&g) else { continue };
        let mut layers = world.layer_names();
        layers.sort();
        for l in layers {
            let Some(layer) = world.layer(&l) else { continue };
            let cells = layer.cells_of_object(id);
            if cells.is_empty() { continue; }
            out.push(((g.clone(), l), cells));
        }
    }
    out
}

fn set(s: &mut Session, grid: &str, layer: &str, x: i32, y: i32, to: Option<&str>) {
    let val = match to { Some(u) => Value::Ref(u.into()), None => Value::Nil };
    if let Some(world) = s.grid_store.get_mut(grid) {
        let state = match &val { Value::Nil => CellState::Unoccupied, v => CellState::Occupied(v.clone()) };
        world.set(x, y, layer, state);
    }
    s.note_grid(GridWrite::Cell { grid: grid.to_string(), x, y, layer: layer.to_string(), val });
}

/// Every cell naming `from` now names `to` (nil: unclaimed).
pub(crate) fn hand_over(s: &mut Session, from: &str, to: Option<&str>) {
    if s.grid_store.names().is_empty() { return; }
    for ((g, l), cells) in cells_of(s, from) {
        for (x, y) in cells { set(s, &g, &l, x, y, to); }
    }
}

fn d2(a: (i32, i32), b: (i32, i32)) -> i64 {
    let (dx, dy) = ((a.0 - b.0) as i64, (a.1 - b.1) as i64);
    dx * dx + dy * dy
}

fn farthest(cells: &[(i32, i32)], from: (i32, i32)) -> (i32, i32) {
    *cells.iter().max_by_key(|&&c| (d2(c, from), std::cmp::Reverse((c.1, c.0)))).unwrap()
}

/// Divides `from`'s cells between `a` and `b`. With `fracture`, `from` keeps
/// its side (`a` is `from`) and `b`, the fragment, takes the far side.
pub(crate) fn divide(s: &mut Session, from: &str, a: &str, b: &str, fracture: bool) {
    if s.grid_store.names().is_empty() { return; }
    for ((g, l), cells) in cells_of(s, from) {
        let (seed_a, seed_b) = if fracture {
            let n = cells.len() as f64;
            let (sx, sy) = cells.iter().fold((0.0, 0.0), |(x, y), c| (x + c.0 as f64, y + c.1 as f64));
            let (mx, my) = (sx / n, sy / n);
            let mid = *cells.iter().min_by(|p, q| {
                let dp = (p.0 as f64 - mx).powi(2) + (p.1 as f64 - my).powi(2);
                let dq = (q.0 as f64 - mx).powi(2) + (q.1 as f64 - my).powi(2);
                dp.partial_cmp(&dq).unwrap()
            }).unwrap();
            (mid, farthest(&cells, mid))
        } else {
            let far = farthest(&cells, cells[0]);
            (farthest(&cells, far), far)
        };
        let side = grow(s, &g, &cells, seed_a, seed_b);
        for (c, to_b) in cells.iter().zip(side) {
            if to_b { set(s, &g, &l, c.0, c.1, Some(b)); }
            else if !fracture { set(s, &g, &l, c.0, c.1, Some(a)); }
        }
    }
}

/// For each cell, whether it goes to the second seed. Both grow at once
/// through the cells, each cell costing a random 1 to 24 steps to cross
/// (the program's dice), so the line between them wanders instead of
/// running straight. Cells neither reaches go, piece by piece, to the
/// seed nearer the piece's first cell.
fn grow(s: &mut Session, grid: &str, cells: &[(i32, i32)], seed_a: (i32, i32), seed_b: (i32, i32)) -> Vec<bool> {
    let cost: HashMap<(i32, i32), u32> = cells.iter().map(|&c| (c, 1 + crate::collections::rng_bounded(s, 24) as u32)).collect();
    let Some(world) = s.grid_store.get(grid) else { return vec![false; cells.len()] };
    let mut side: HashMap<(i32, i32), bool> = HashMap::new();
    let flood = |starts: Vec<((i32, i32), bool)>, side: &mut HashMap<(i32, i32), bool>| {
        use std::cmp::Reverse;
        let mut best: HashMap<(i32, i32), u32> = HashMap::new();
        let mut heap: std::collections::BinaryHeap<Reverse<(u32, i32, i32, bool)>> = std::collections::BinaryHeap::new();
        for (c, b) in starts {
            if side.contains_key(&c) { continue; }
            best.insert(c, 0);
            heap.push(Reverse((0, c.1, c.0, b)));
        }
        while let Some(Reverse((d, y, x, b))) = heap.pop() {
            let c = (x, y);
            if side.contains_key(&c) { continue; }
            side.insert(c, b);
            for n in world.neighbors(x, y) {
                let Some(k) = cost.get(&n) else { continue };
                if side.contains_key(&n) { continue; }
                let nd = d + k;
                if best.get(&n).is_none_or(|&o| nd < o) { best.insert(n, nd); heap.push(Reverse((nd, n.1, n.0, b))); }
            }
        }
    };
    if seed_a == seed_b { flood(vec![(seed_a, false)], &mut side); } else { flood(vec![(seed_a, false), (seed_b, true)], &mut side); }
    for &c in cells {
        if side.contains_key(&c) { continue; }
        let to_b = d2(c, seed_b) < d2(c, seed_a);
        flood(vec![(c, to_b)], &mut side);
    }
    cells.iter().map(|c| side.get(c).copied().unwrap_or(false)).collect()
}
