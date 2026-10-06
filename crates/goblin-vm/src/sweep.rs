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
}

#[derive(Default)]
pub struct Sweeps {
    next_id: i64,
    live: HashMap<i64, SweepState>,
}

fn find(hay: &[u8], needle: &[u8], from: usize) -> Option<usize> {
    if needle.is_empty() || from > hay.len() { return None; }
    let mut i = from;
    while i + needle.len() <= hay.len() {
        if &hay[i..i + needle.len()] == needle { return Some(i); }
        i += 1;
    }
    None
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
        let s = |i: usize| match f.get(i) { Some(Value::Str(s)) => s.clone(), _ => String::new() };
        match s(0).as_str() {
            "pattern" => ArmKind::Pattern(s(1)),
            "range" => ArmKind::Range {
                start: s(1),
                end: s(2),
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
                Value::Str(s) => raw.push(s),
                v if v.is_seq_like() => {
                    for e in v.seq_items().map(|c| c.into_owned()).unwrap_or_default() {
                        match e {
                            Value::Str(s) => raw.push(s),
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
            if p.is_dir() {
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
                st.original = text.clone();
                st.buffer = text;
                st.cursor = 0;
                st.pattern_used.iter_mut().for_each(|b| *b = false);
                st.first_used.iter_mut().for_each(|b| *b = false);
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
                st.buffer = match scope {
                    Some((s, e)) => {
                        let mut out = String::with_capacity(st.buffer.len() - (e - s) + new.len());
                        out.push_str(&st.buffer[..s]);
                        out.push_str(&new);
                        out.push_str(&st.buffer[e..]);
                        out
                    }
                    None => new,
                };
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
        if self.cursor >= hb.len() { return None; }
        let mut best: Option<(usize, usize, usize, bool)> = None;
        for (i, arm) in self.arms.iter().enumerate() {
            let cand = match arm {
                ArmKind::Pattern(n) => {
                    if self.pattern_used[i] { continue; }
                    find(hb, n.as_bytes(), self.cursor).map(|s| (s, s + n.len(), i, true))
                }
                ArmKind::Range { start, end, repeat } => {
                    if *repeat == Repeat::Last { continue; }
                    if *repeat == Repeat::First && self.first_used[i] { continue; }
                    find_span(hb, start, end, self.cursor).map(|(s, e)| (s, e, i, false))
                }
                ArmKind::All => None,
            };
            if let Some(c) = cand {
                if best.map_or(true, |b| c.0 < b.0) { best = Some(c); }
            }
        }
        best
    }
}
