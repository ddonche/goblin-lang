//! Runtime for the `sweep` statement.
//!
//! The compiler turns a sweep into a loop over these steps (see
//! `Compiler::compile_sweep`):
//!
//! - `begin`: resolve the targets into a work list (files first, expanded from
//!   directories, then in-memory text), returning a sweep id.
//! - `next`: find the next match in the current target and return the index
//!   of the arm to run (or -1 when every target is done); `self_text` gives the
//!   text the arm sees as `self`.
//! - `apply`: after the arm body ran, splice the arm's `self` back into the
//!   buffer (or keep the buffer, for `skip`) and move past the match.
//! - `end`: discard the sweep (`stop`).
//!
//! Matching follows the interpreter: pattern arms fire at most once per target
//! and see the whole buffer; range arms (`"a" ... "b"`) run over each span in
//! document order (`first` only on the first one), and `last` range arms run
//! afterwards on the last span. A changed file target is written back when its
//! arms are done; in-memory targets are never written anywhere.

use std::collections::HashMap;

use crate::error::GoblinError;
use crate::value::Value;

/// One arm of a sweep, as the compiler describes it.
#[derive(Debug, Clone)]
pub enum ArmKind {
    Pattern(String),
    Range { start: String, end: String, repeat: Repeat },
    All,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Repeat { All, First, Last }

enum Target { File(String), Memory(String) }

#[derive(PartialEq)]
enum Phase { Forward, Last(usize), AllMode, Done }

pub struct SweepState {
    arms: Vec<ArmKind>,
    all_mode: bool,
    targets: Vec<Target>,
    next_target: usize,
    /// The target being swept: (label/path, is_file).
    current: Option<(String, bool)>,
    buffer: String,
    original: String,
    phase: Phase,
    cursor: usize,
    pattern_used: Vec<bool>,
    first_used: Vec<bool>,
    /// The match handed to the arm body: (arm, scope or None for the whole
    /// buffer, end of the match in the buffer before the arm ran).
    pending: Option<(usize, Option<(usize, usize)>, usize)>,
    /// Per arm: the last search result still known to hold (see `Found`).
    found: Vec<Option<Found>>,
    /// Search every arm afresh at every step (tests compare against it).
    no_memo: bool,
}

#[derive(Default)]
pub struct Sweeps {
    next_id: i64,
    live: HashMap<i64, SweepState>,
}

/// The first `needle` starting at or after `from`.
fn find(hay: &[u8], needle: &[u8], from: usize) -> Option<usize> {
    if needle.is_empty() || from > hay.len() { return None; }
    memchr::memmem::find(&hay[from..], needle).map(|i| i + from)
}

/// The first match of an arm starting in `[from, before)`, as
/// `find`/`find_span` would report it from `from`; `None` when no match
/// starts before `before`.
fn find_arm_before(arm: &ArmKind, hay: &[u8], from: usize, before: usize) -> Option<Option<(usize, usize)>> {
    let first = |n: &[u8]| -> Option<usize> {
        if n.is_empty() || from >= before { return None; }
        let lim = (before + n.len() - 1).min(hay.len());
        if from > lim { return None; }
        find(&hay[..lim], n, from)
    };
    match arm {
        ArmKind::Pattern(n) => first(n.as_bytes()).map(|s| Some((s, s + n.len()))),
        ArmKind::Range { start, end, .. } => {
            if end.is_empty() { return None; }
            let s = first(start.as_bytes())?;
            Some(find(hay, end.as_bytes(), s + start.len()).map(|e| (s, e + end.len())))
        }
        ArmKind::All => None,
    }
}

/// An arm's match from `from` on: `find` for a pattern, `find_span` for a range.
fn find_arm(arm: &ArmKind, hay: &[u8], from: usize) -> Option<(usize, usize)> {
    match arm {
        ArmKind::Pattern(n) => find(hay, n.as_bytes(), from).map(|s| (s, s + n.len())),
        ArmKind::Range { start, end, .. } => find_span(hay, start, end, from),
        ArmKind::All => None,
    }
}

/// What a search for one arm found: `res` is that arm's first match starting
/// at or after `from` in the current buffer. Kept between steps so an arm is
/// not searched again over text it has already been searched over.
#[derive(Clone, Copy, Debug)]
struct Found { from: usize, res: Option<(usize, usize)> }

/// Is `path` possibly an existing file or directory? Text longer than the
/// longest path the system accepts, or containing a NUL, cannot be one, so
/// it needs no disk check (the check would say no anyway).
fn may_be_path(t: &str) -> bool {
    #[cfg(unix)]
    { t.len() <= 4096 && !t.as_bytes().contains(&0) }
    #[cfg(not(unix))]
    { !t.as_bytes().contains(&0) }
}

/// The next `start`...`end` span at or after `from`: (start, end exclusive).
fn find_span(hay: &[u8], start: &str, end: &str, from: usize) -> Option<(usize, usize)> {
    let (sb, eb) = (start.as_bytes(), end.as_bytes());
    if sb.is_empty() || eb.is_empty() { return None; }
    let s = find(hay, sb, from)?;
    let e = find(hay, eb, s + sb.len())?;
    Some((s, e + eb.len()))
}

fn walk_files(dir: &std::path::Path, out: &mut Vec<String>) -> Result<(), GoblinError> {
    let rd = std::fs::read_dir(dir)
        .map_err(|e| GoblinError::Runtime(format!("sweep: failed to read directory '{}': {}", dir.display(), e)))?;
    for entry in rd {
        let entry = entry
            .map_err(|e| GoblinError::Runtime(format!("sweep: failed to enumerate directory '{}': {}", dir.display(), e)))?;
        let path = entry.path();
        if path.is_dir() {
            walk_files(&path, out)?;
        } else if path.is_file() {
            out.push(path.to_string_lossy().replace('\\', "/"));
        }
    }
    Ok(())
}

/// Parse the compiler's arm description: an array of [kind, a, b, repeat]
/// where kind is "pattern" / "range" / "all" and repeat is "all" / "first" / "last".
pub fn parse_arms(spec: &Value) -> Vec<ArmKind> {
    let items = spec.seq_items().map(|c| c.into_owned()).unwrap_or_default();
    items.iter().map(|arm| {
        let f = arm.seq_items().map(|c| c.into_owned()).unwrap_or_default();
        let s = |i: usize| match f.get(i) { Some(Value::Str(s)) => s.clone(), _ => String::new().into() };
        match s(0).as_str() {
            "pattern" => ArmKind::Pattern(s(1).to_string()),
            "range" => ArmKind::Range {
                start: s(1).to_string(),
                end: s(2).to_string(),
                repeat: match s(3).as_str() { "first" => Repeat::First, "last" => Repeat::Last, _ => Repeat::All },
            },
            _ => ArmKind::All,
        }
    }).collect()
}

impl Sweeps {
    pub fn begin(&mut self, arms: Vec<ArmKind>, all_mode: bool, target_vals: Vec<Value>) -> Result<i64, GoblinError> {
        // Targets as strings (arrays of strings spread out; scalars stringified).
        let mut raw: Vec<String> = Vec::new();
        for v in target_vals {
            match v {
                Value::Str(s) => raw.push(s.to_string()),
                v if v.is_seq_like() => {
                    for e in v.seq_items().map(|c| c.into_owned()).unwrap_or_default() {
                        match e {
                            Value::Str(s) => raw.push(s.to_string()),
                            other => return Err(GoblinError::Runtime(format!(
                                "sweep: target array contains a non-string value ({})", other.type_name()))),
                        }
                    }
                }
                Value::Int(n) => raw.push(n.to_string()),
                Value::Float(f) => raw.push(f.to_string()),
                Value::Big(d) => raw.push(d.to_string()),
                Value::Pct(p) => raw.push(p.to_string()),
                Value::Char(c) => raw.push(c.to_string()),
                Value::Bool(b) => raw.push(b.to_string()),
                other => return Err(GoblinError::Runtime(format!(
                    "sweep: targets must be strings, arrays of strings, or scalars, got {}", other.type_name()))),
            }
        }
        // Existing files/directories are swept on disk; anything else is text.
        let mut files: Vec<String> = Vec::new();
        let mut mem: Vec<String> = Vec::new();
        for t in raw {
            let p = std::path::Path::new(&t);
            if !may_be_path(&t) {
                mem.push(t);
            } else if p.is_dir() {
                walk_files(p, &mut files)?;
            } else if p.is_file() {
                files.push(p.to_string_lossy().replace('\\', "/"));
            } else {
                mem.push(t);
            }
        }
        let mut targets: Vec<Target> = files.into_iter().map(Target::File).collect();
        targets.extend(mem.into_iter().map(Target::Memory));

        let n = arms.len();
        let id = self.next_id;
        self.next_id += 1;
        self.live.insert(id, SweepState {
            arms, all_mode, targets, next_target: 0, current: None,
            buffer: String::new(), original: String::new(), phase: Phase::Done, cursor: 0,
            pattern_used: vec![false; n], first_used: vec![false; n], pending: None,
            found: vec![None; n], no_memo: false,
        });
        Ok(id)
    }

    fn state(&mut self, id: i64) -> Result<&mut SweepState, GoblinError> {
        self.live.get_mut(&id).ok_or_else(|| GoblinError::Runtime("sweep: no such sweep".into()))
    }

    /// The arm to run next, or -1 when the sweep is finished (and dropped).
    pub fn next(&mut self, id: i64) -> Result<i64, GoblinError> {
        loop {
            let st = self.state(id)?;
            if st.current.is_none() {
                if st.next_target >= st.targets.len() {
                    self.live.remove(&id);
                    return Ok(-1);
                }
                let (label, is_file, text) = match &st.targets[st.next_target] {
                    Target::File(p) => {
                        let text = std::fs::read_to_string(p)
                            .map_err(|e| GoblinError::Runtime(format!("sweep: failed to read file '{}': {}", p, e)))?;
                        (p.clone(), true, text)
                    }
                    Target::Memory(t) => ("<memory>".to_string(), false, t.clone()),
                };
                st.next_target += 1;
                st.current = Some((label, is_file));
                // Only a file needs the original, to tell whether to write it back.
                st.original = if is_file { text.clone() } else { String::new() };
                st.buffer = text;
                st.cursor = 0;
                st.pattern_used.iter_mut().for_each(|b| *b = false);
                st.first_used.iter_mut().for_each(|b| *b = false);
                st.found.iter_mut().for_each(|f| *f = None);
                st.phase = if st.all_mode { Phase::AllMode } else { Phase::Forward };
            }

            match st.phase {
                Phase::AllMode => {
                    if let Some(i) = st.arms.iter().position(|a| matches!(a, ArmKind::All)) {
                        st.pending = Some((i, Some((0, st.buffer.len())), st.buffer.len()));
                        st.phase = Phase::Done;
                        return Ok(i as i64);
                    }
                    st.phase = Phase::Done;
                }
                Phase::Forward => {
                    if let Some((s, e, arm, is_pattern)) = st.forward_match() {
                        st.pending = Some((arm, if is_pattern { None } else { Some((s, e)) }, e));
                        return Ok(arm as i64);
                    }
                    st.phase = Phase::Last(0);
                }
                Phase::Last(from) => {
                    let mut found = None;
                    for k in from..st.arms.len() {
                        if let ArmKind::Range { start, end, repeat: Repeat::Last } = &st.arms[k] {
                            let hb = st.buffer.as_bytes();
                            let mut cursor = 0usize;
                            let mut last = None;
                            while let Some((s, e)) = find_span(hb, start, end, cursor) {
                                last = Some((s, e));
                                cursor = e;
                            }
                            if let Some(span) = last { found = Some((k, span)); break; }
                        }
                    }
                    match found {
                        Some((k, span)) => {
                            st.phase = Phase::Last(k + 1);
                            st.pending = Some((k, Some(span), span.1));
                            return Ok(k as i64);
                        }
                        None => st.phase = Phase::Done,
                    }
                }
                Phase::Done => {
                    // Target finished: write a changed file back.
                    let (label, is_file) = st.current.take().unwrap();
                    if is_file && st.buffer != st.original {
                        std::fs::write(&label, &st.buffer)
                            .map_err(|e| GoblinError::Runtime(format!("sweep: failed to write file '{}': {}", label, e)))?;
                    }
                }
            }
        }
    }

    /// The text the pending arm sees as `self`.
    pub fn self_text(&mut self, id: i64) -> Result<String, GoblinError> {
        let st = self.state(id)?;
        Ok(match st.pending {
            Some((_, Some((s, e)), _)) => st.buffer[s..e].to_string(),
            _ => st.buffer.clone(),
        })
    }

    /// Finish the pending arm: `skip` keeps the buffer, otherwise the arm's
    /// `self` (when it is still a string) replaces the matched text.
    pub fn apply(&mut self, id: i64, new_self: Value, skip: bool) -> Result<(), GoblinError> {
        let st = self.state(id)?;
        let Some((arm, scope, end)) = st.pending.take() else { return Ok(()) };
        if !skip {
            if let Value::Str(new) = new_self {
                st.shift_found(scope, new.len());
                match scope {
                    // In place: only the text after the match moves.
                    Some((s, e)) => st.buffer.replace_range(s..e, &new),
                    None => st.buffer = new.into_string(),
                }
            }
        }
        if st.phase == Phase::Forward {
            match &st.arms[arm] {
                ArmKind::Pattern(_) => st.pattern_used[arm] = true,
                ArmKind::Range { repeat: Repeat::First, .. } => st.first_used[arm] = true,
                _ => {}
            }
            // Move past the match (even after skip), clamped to the new length.
            let len = st.buffer.len();
            if len == 0 { st.phase = Phase::Last(0); } else { st.cursor = end.min(len); }
        }
        Ok(())
    }

    pub fn end(&mut self, id: i64) {
        self.live.remove(&id);
    }
}

impl SweepState {
    /// The earliest match at or after the cursor among the pattern arms and the
    /// all/first range arms: (start, end, arm, is_pattern).
    fn forward_match(&mut self) -> Option<(usize, usize, usize, bool)> {
        let hb = self.buffer.as_bytes();
        let cursor = self.cursor;
        if cursor >= hb.len() { return None; }
        let mut best: Option<(usize, usize, usize, bool)> = None;
        for (i, arm) in self.arms.iter().enumerate() {
            match arm {
                ArmKind::Pattern(_) => { if self.pattern_used[i] { continue; } }
                ArmKind::Range { repeat, .. } => {
                    if *repeat == Repeat::Last { continue; }
                    if *repeat == Repeat::First && self.first_used[i] { continue; }
                }
                ArmKind::All => continue,
            }
            let res = if self.no_memo {
                find_arm(arm, hb, cursor)
            } else {
                let res = match self.found[i] {
                    // Known from at or before the cursor: still good unless
                    // the match it found is now behind the cursor.
                    Some(Found { from, res }) if from <= cursor => match res {
                        Some((s, _)) if s < cursor => find_arm(arm, hb, cursor),
                        r => r,
                    },
                    // Known only from further on (text before `from` changed):
                    // look for a match starting before it first.
                    Some(Found { from, res }) => match find_arm_before(arm, hb, cursor, from) {
                        Some(r) => r,
                        None => res,
                    },
                    None => find_arm(arm, hb, cursor),
                };
                self.found[i] = Some(Found { from: cursor, res });
                res
            };
            if let Some((s, e)) = res {
                let c = (s, e, i, matches!(arm, ArmKind::Pattern(_)));
                if best.map_or(true, |b| c.0 < b.0) { best = Some(c); }
            }
        }
        best
    }

    /// The buffer is about to change: `scope` (or the whole buffer, for
    /// `None`) is replaced by `new_len` bytes. Text after the scope only moves,
    /// so a search result that lies wholly after it still holds, shifted, for
    /// matches starting where that text now begins; anything else is dropped.
    fn shift_found(&mut self, scope: Option<(usize, usize)>, new_len: usize) {
        let Some((s, e)) = scope else {
            self.found.iter_mut().for_each(|f| *f = None);
            return;
        };
        let boundary = s + new_len;
        let shift = |p: usize| p + boundary - e;
        for f in self.found.iter_mut() {
            *f = match *f {
                Some(Found { from, res }) if from <= s => match res {
                    None => Some(Found { from: boundary, res: None }),
                    Some((ms, me)) if ms >= e => Some(Found { from: boundary, res: Some((shift(ms), shift(me))) }),
                    Some(_) => None,
                },
                _ => None,
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Rng(u64);
    impl Rng {
        fn next(&mut self) -> u64 { self.0 ^= self.0 << 13; self.0 ^= self.0 >> 7; self.0 ^= self.0 << 17; self.0 }
        fn below(&mut self, n: u64) -> usize { (self.next() % n) as usize }
        fn text(&mut self, max: u64) -> String {
            let n = self.below(max + 1);
            (0..n).map(|_| ['a', 'b', 'c', '<', '>', '\n'][self.below(6)]).collect()
        }
        fn needle(&mut self) -> String {
            let n = 1 + self.below(3);
            (0..n).map(|_| ['a', 'b', 'c', '<', '>'][self.below(5)]).collect()
        }
    }

    /// Run one sweep to the end; the trace is every (arm, self text) handed out.
    fn run(arms: &[ArmKind], all_mode: bool, target: &str, no_memo: bool, seed: u64) -> Vec<(i64, String)> {
        let mut sw = Sweeps::default();
        let id = sw.begin(arms.to_vec(), all_mode, vec![Value::Str(target.into())]).unwrap();
        sw.state(id).unwrap().no_memo = no_memo;
        let mut rng = Rng(seed);
        let mut trace = Vec::new();
        for _ in 0..10_000 {
            let arm = sw.next(id).unwrap();
            if arm < 0 { return trace; }
            let me = sw.self_text(id).unwrap();
            trace.push((arm, me.clone()));
            match rng.below(4) {
                0 => sw.apply(id, Value::Nil, true).unwrap(),
                1 => sw.apply(id, Value::Str(me.into()), false).unwrap(),
                _ => { let t = rng.text(6); sw.apply(id, Value::Str(t.into()), false).unwrap() }
            }
        }
        panic!("sweep did not finish");
    }

    #[test]
    fn remembered_searches_match_searching_afresh() {
        let mut rng = Rng(0x9E3779B97F4A7C15);
        for case in 0..20_000u64 {
            let n_arms = 1 + rng.below(4);
            let arms: Vec<ArmKind> = (0..n_arms).map(|_| match rng.below(5) {
                0 | 1 => ArmKind::Pattern(rng.needle()),
                _ => ArmKind::Range {
                    start: rng.needle(),
                    end: rng.needle(),
                    repeat: [Repeat::All, Repeat::All, Repeat::First, Repeat::Last][rng.below(4)],
                },
            }).collect();
            let target = rng.text(40);
            let a = run(&arms, false, &target, true, case);
            let b = run(&arms, false, &target, false, case);
            assert_eq!(a, b, "arms {arms:?} target {target:?}");
        }
    }

    #[test]
    fn fast_find_matches_byte_loop() {
        let mut rng = Rng(12345);
        for _ in 0..20_000 {
            let hay = rng.text(30);
            let needle = rng.needle();
            let from = rng.below(hay.len() as u64 + 2);
            let slow = if needle.is_empty() || from > hay.len() { None } else {
                (from..=hay.len().saturating_sub(needle.len())).find(|&i| i + needle.len() <= hay.len() && &hay.as_bytes()[i..i + needle.len()] == needle.as_bytes())
            };
            assert_eq!(find(hay.as_bytes(), needle.as_bytes(), from), slow, "{hay:?} {needle:?} {from}");
        }
    }
}
