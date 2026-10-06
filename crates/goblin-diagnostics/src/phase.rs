//! Opt-in timing of where a run spends its time, for benchmarking.
//!
//! With `GOBLIN_PHASE_TIMES=1` the engines time module imports, database
//! calls and string interpolation. Time is exclusive: a database call made
//! while a module is being imported counts as database time, not import time.
//! `goblin run` prints the totals to stderr when it exits, and goblin-host
//! prints them after each VM request. The totals are process-wide, so they
//! only describe one request when requests run one at a time.

use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;
use std::time::Instant;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Phase {
    Import = 0,
    Db = 1,
    Interp = 2,
}

const NAMES: [&str; 3] = ["import", "db", "interp"];
static TOTALS: [AtomicU64; 3] = [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)];
static CALLS: [AtomicU64; 3] = [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)];

thread_local! {
    static STACK: RefCell<Vec<(Phase, Instant)>> = RefCell::new(Vec::new());
}

pub fn enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var("GOBLIN_PHASE_TIMES").map(|v| v == "1").unwrap_or(false))
}

fn credit(p: Phase, since: Instant, now: Instant) {
    TOTALS[p as usize].fetch_add(now.duration_since(since).as_nanos() as u64, Ordering::Relaxed);
}

/// Times `p` until the guard drops, pausing the phase it interrupts.
pub fn enter(p: Phase) -> Guard {
    if !enabled() {
        return Guard(false);
    }
    CALLS[p as usize].fetch_add(1, Ordering::Relaxed);
    let now = Instant::now();
    STACK.with(|s| {
        let mut s = s.borrow_mut();
        if let Some(top) = s.last_mut() {
            credit(top.0, top.1, now);
            top.1 = now;
        }
        s.push((p, now));
    });
    Guard(true)
}

pub struct Guard(bool);

impl Drop for Guard {
    fn drop(&mut self) {
        if !self.0 {
            return;
        }
        let now = Instant::now();
        STACK.with(|s| {
            let mut s = s.borrow_mut();
            if let Some((p, start)) = s.pop() {
                credit(p, start, now);
            }
            if let Some(top) = s.last_mut() {
                top.1 = now;
            }
        });
    }
}

/// `phase-times: import=1.20ms/3 db=3.40ms/12 interp=0.50ms/40` (time and
/// number of calls), then resets.
pub fn take_report() -> String {
    let parts: Vec<String> = (0..NAMES.len())
        .map(|i| format!("{}={:.2}ms/{}", NAMES[i],
            TOTALS[i].swap(0, Ordering::Relaxed) as f64 / 1e6, CALLS[i].swap(0, Ordering::Relaxed)))
        .collect();
    format!("phase-times: {}", parts.join(" "))
}
