//! Performance regression tests for the VM.
//!
//! Each case runs a small program at size `n` and at `8 * n` and checks that
//! the larger run costs well under the 64x a quadratic algorithm would. These
//! guard the pathologies found while running Campfire on the VM:
//! - list push/read/update loops copied the whole list on every operation;
//! - `for x in xs` read each element by copying the list;
//! - RingBuf growth copied every element on every push;
//! - the lexer re-validated the rest of the file for every character of a
//!   string literal;
//! - map writes rebuilt the map;
//! - `name!(x[k], …)` copied the element at the path.
//!
//! Timing is the best of three runs, and the bound (24x for 8x the work)
//! leaves room for noise while still failing for quadratic behaviour.
//! Run with `cargo test --release -p goblin-cli --test perf_regression`.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

fn run(bin: &Path, dir: &Path, name: &str, src: &str) -> Duration {
    let file = dir.join(name);
    std::fs::write(&file, src).unwrap();
    let mut best = Duration::MAX;
    for _ in 0..3 {
        let start = Instant::now();
        let out = Command::new(bin).arg("run").arg("--vm").arg(&file).output().unwrap();
        let took = start.elapsed();
        assert!(out.status.success(), "{name} failed: {}", String::from_utf8_lossy(&out.stderr));
        best = best.min(took);
    }
    best
}

/// `N` becomes the problem size; `REPEAT` becomes that many lines of
/// `s |= "…"` string literals.
fn expand(body: &str, n: usize) -> String {
    let line = "s |= \"<p class=x>hello world, this is text</p>\"\n";
    body.replace("REPEAT", &line.repeat(n)).replace("N", &n.to_string())
}

/// `body` uses `N` for the problem size.
fn check_scaling(case: &str, n: usize, body: &str) {
    let bin = PathBuf::from(env!("CARGO_BIN_EXE_goblin"));
    let dir = std::env::temp_dir().join(format!("goblin-perf-{}-{case}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    // Process start-up and the empty program are the same at both sizes.
    let base = run(&bin, &dir, "empty.gbln", "say(1)\n");
    let small = run(&bin, &dir, "small.gbln", &expand(body, n));
    let large = run(&bin, &dir, "large.gbln", &expand(body, 8 * n));
    let _ = std::fs::remove_dir_all(&dir);
    let floor = Duration::from_millis(5);
    let small_work = small.saturating_sub(base).max(floor);
    let large_work = large.saturating_sub(base);
    let ratio = large_work.as_secs_f64() / small_work.as_secs_f64();
    eprintln!("{case}: n={n} {small:?}, 8n {large:?} (start-up {base:?}), ratio {ratio:.1}");
    assert!(
        ratio < 24.0,
        "{case}: 8x the work took {ratio:.1}x as long ({small:?} -> {large:?}); expected linear"
    );
}

#[test]
fn list_push_is_linear() {
    check_scaling("push", 20_000, "l | []\ni | 0\nwhile i < N\n    :put_last!(l, i)\n    i |= i + 1\nxx\nsay(:len(l))\n");
}

#[test]
fn list_push_front_is_linear() {
    check_scaling("push_front", 20_000, "l | []\ni | 0\nwhile i < N\n    :put_first!(l, i)\n    i |= i + 1\nxx\nsay(:len(l))\n");
}

#[test]
fn list_index_read_is_linear() {
    check_scaling(
        "read",
        20_000,
        "l | []\ni | 0\nwhile i < N\n    :put_last!(l, i)\n    i |= i + 1\nxx\ns | 0\nj | 0\nwhile j < N\n    s |= s + l[j]\n    j |= j + 1\nxx\nsay(s)\n",
    );
}

#[test]
fn list_update_is_linear() {
    check_scaling(
        "update",
        20_000,
        "l | []\ni | 0\nwhile i < N\n    :put_last!(l, 0)\n    i |= i + 1\nxx\nj | 0\nwhile j < N\n    :update!(l[j], j)\n    j |= j + 1\nxx\nsay(l[N - 1])\n",
    );
}

#[test]
fn list_build_in_action_is_linear() {
    check_scaling(
        "action",
        20_000,
        "act build(n)\n    l | []\n    i | 0\n    while i < n\n        :put_last!(l, i)\n        i |= i + 1\n    xx\n    return l\nxx\nact total(l)\n    s | 0\n    for x in l\n        s |= s + x\n    xx\n    return s\nxx\nsay(total(build(N)))\n",
    );
}

#[test]
fn map_update_is_linear() {
    check_scaling(
        "map",
        5_000,
        "m | {}\ni | 0\nwhile i < N\n    :update!(m{\"k\" + :str(i)}, i)\n    i |= i + 1\nxx\nsay(:len(:keys(m)))\n",
    );
}

#[test]
fn nested_update_is_linear() {
    check_scaling(
        "nested",
        10_000,
        "rows | []\ni | 0\nwhile i < N\n    :put_last!(rows, 0)\n    i |= i + 1\nxx\nm | {\"rows\": rows}\nrows |= []\nj | 0\nwhile j < N\n    :update!(m{\"rows\"}[j], j)\n    j |= j + 1\nxx\nsay(m{\"rows\"}[N - 1])\n",
    );
}

#[test]
fn nested_push_is_linear() {
    check_scaling(
        "nested_push",
        10_000,
        "m | {\"rows\": []}\ni | 0\nwhile i < N\n    :put_last!(m{\"rows\"}, i)\n    i |= i + 1\nxx\nsay(:len(m{\"rows\"}))\n",
    );
}

#[test]
fn string_literals_lex_linearly() {
    // Many single-line string literals, as templates and HTML helpers have.
    // Each character of a literal used to re-validate the rest of the file.
    check_scaling(
        "lex",
        2_000,
        "s | \"\"\nREPEATsay(:len(s))\n",
    );
}
