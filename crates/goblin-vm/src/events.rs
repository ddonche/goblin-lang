//! The event log (`:event_log(path)`): one line for each thing the DES
//! did, as it happens: `tick 12 | Polity kingdom_3 | raid | Province p_44`.
//! The main program writes the lines to the file; a region keeps its own
//! and hands them back with its tick, and the main program writes them in
//! region order.

use std::io::Write;

use crate::error::GoblinError;
use crate::value::Value;
use crate::vm::Vm;

pub enum Sink {
    File(std::io::BufWriter<std::fs::File>, Box<Timelines>),
    /// A region's lines for this tick, still carrying their keys (they are
    /// indexed when the main program writes them).
    Lines(Vec<String>),
}

/// Marks a key in a line as it is built: `Realm Tesh\u{1}<id>`. Keys say
/// which objects and overlay kinds a line is about; the file never shows them.
const KEY: char = '\u{1}';

/// What `:timeline` reads: for each line written, where it starts in the
/// file and its tick; for each key, its lines; for each object or overlay
/// kind a transition made, where it came from.
#[derive(Default)]
pub struct Timelines {
    path: String,
    bytes: u64,
    lines: Vec<(u64, i64)>,
    by_key: std::collections::HashMap<String, Vec<u32>>,
    /// child key -> (parent keys, the line it was born on)
    born: std::collections::HashMap<String, (Vec<String>, u32)>,
}

/// A line without its keys, and its keys by segment (who, what, whom...).
fn strip(line: &str) -> (String, Vec<Vec<String>>) {
    let mut text = String::with_capacity(line.len());
    let mut segs: Vec<Vec<String>> = vec![Vec::new()];
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        if c == KEY {
            let mut k = String::new();
            while let Some(&n) = chars.peek() {
                if n == KEY || n == ',' || n == '|' || n == ' ' { break; }
                k.push(n);
                chars.next();
            }
            if !k.is_empty() { segs.last_mut().unwrap().push(k); }
            continue;
        }
        if c == '|' || c == ',' { segs.push(Vec::new()); }
        text.push(c);
    }
    (text, segs)
}

fn same_sort(a: &str, b: &str) -> bool { a.starts_with("kind:") == b.starts_with("kind:") }

impl Timelines {
    fn note_what(&mut self, tick: i64, line_len: usize, segs: Vec<Vec<String>>, what: &str) {
        let at = self.lines.len() as u32;
        self.lines.push((self.bytes, tick));
        self.bytes += line_len as u64 + 1;
        // segs: [tick] [who] [what] [whom, ...] [telling]
        let who: Vec<String> = segs.get(1).cloned().unwrap_or_default();
        let whom: Vec<Vec<String>> = segs.iter().skip(3).cloned().collect();
        let mut seen = std::collections::HashSet::new();
        for k in segs.iter().flatten() {
            if seen.insert(k.clone()) { self.by_key.entry(k.clone()).or_default().push(at); }
        }
        let (children, mut parents): (Vec<String>, Vec<String>) = match what {
            "split" | "fracture" | "spawn" | "mutate" => (whom.iter().flatten().cloned().collect(), who.clone()),
            "merge" if !whom.is_empty() => {
                let mut ps = who.clone();
                for w in &whom[..whom.len() - 1] { ps.extend(w.iter().cloned()); }
                (whom[whom.len() - 1].clone(), ps)
            }
            _ => return,
        };
        parents.dedup();
        for c in children {
            if parents.contains(&c) || self.born.contains_key(&c) { continue; }
            let ps: Vec<String> = parents.iter().filter(|p| same_sort(p, &c)).cloned().collect();
            if !ps.is_empty() { self.born.insert(c, (ps, at)); }
        }
    }

    /// The lines of `key` and of its forebears up to its birth, in order.
    fn lines_of(&self, key: &str) -> Vec<u32> {
        let mut out: Vec<u32> = Vec::new();
        let mut todo: Vec<(String, u32)> = vec![(key.to_string(), u32::MAX)];
        let mut done = std::collections::HashSet::new();
        while let Some((k, until)) = todo.pop() {
            if !done.insert((k.clone(), until)) { continue; }
            if let Some(ls) = self.by_key.get(&k) { out.extend(ls.iter().copied().filter(|&l| l <= until)); }
            if let Some((ps, at)) = self.born.get(&k) {
                let at = (*at).min(until);
                for p in ps { todo.push((p.clone(), at)); }
            }
        }
        out.sort_unstable();
        out.dedup();
        out
    }
}

impl Sink {
    fn put(&mut self, line: String) -> Result<(), GoblinError> {
        match self {
            Sink::File(f, t) => {
                let (text, segs) = strip(&line);
                let tick = text.strip_prefix("tick ").and_then(|r| r.split(' ').next()).and_then(|n| n.parse().ok()).unwrap_or(0);
                let what = text.split(" | ").nth(2).unwrap_or("").to_string();
                t.note_what(tick, text.len(), segs, &what);
                writeln!(f, "{text}").map_err(|e| GoblinError::Runtime(format!("R1252: event-log-write: can't write the event log: {e}")))
            }
            Sink::Lines(v) => { v.push(line); Ok(()) }
        }
    }
}

/// `:event_log(path)` starts the log (a new file, written as events
/// happen); `:event_log(nil)` stops it.
pub fn start(session: &mut crate::session::Session, path: Option<&str>) -> Result<(), GoblinError> {
    stop(session)?;
    let Some(path) = path else { return Ok(()) };
    let f = std::fs::File::create(path)
        .map_err(|e| GoblinError::Runtime(format!("R1252: event-log-write: can't open {path} for the event log: {e}")))?;
    session.event_log = Some(Sink::File(std::io::BufWriter::new(f), Box::new(Timelines { path: path.to_string(), ..Default::default() })));
    Ok(())
}

pub fn stop(session: &mut crate::session::Session) -> Result<(), GoblinError> {
    if let Some(Sink::File(mut f, _)) = session.event_log.take() {
        f.flush().map_err(|e| GoblinError::Runtime(format!("R1252: event-log-write: can't write the event log: {e}")))?;
    }
    Ok(())
}

/// Writes lines a region handed back.
pub fn put_lines(session: &mut crate::session::Session, lines: Vec<String>) -> Result<(), GoblinError> {
    let Some(sink) = session.event_log.as_mut() else { return Ok(()) };
    for l in lines { sink.put(l)?; }
    Ok(())
}

/// Ends a tick: what was written so far reaches the file.
pub fn flush(session: &mut crate::session::Session) -> Result<(), GoblinError> {
    if let Some(Sink::File(f, _)) = session.event_log.as_mut() {
        f.flush().map_err(|e| GoblinError::Runtime(format!("R1252: event-log-write: can't write the event log: {e}")))?;
    }
    Ok(())
}

#[inline]
pub fn on(vm: &Vm) -> bool { vm.session.event_log.is_some() }

/// An object as the log names it: its class, then its `name` field, else
/// the variable it is bound to, else its id.
pub fn who(vm: &Vm, uuid: &str) -> String {
    match vm.session.object_store.get(uuid) {
        Some(Value::Object(o)) => named(vm, &o.class_name, uuid, o.fields.get("name")),
        _ => named(vm, "", uuid, None),
    }
}

fn named(vm: &Vm, class: &str, uuid: &str, name: Option<&Value>) -> String {
    let name = match name {
        Some(Value::Str(s)) if !s.is_empty() => s.to_string(),
        _ => vm.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_else(|| uuid.to_string()),
    };
    if class.is_empty() { format!("{name}{KEY}{uuid}") } else { format!("{class} {name}{KEY}{uuid}") }
}

/// An object given as a value: by reference, or a copy of one that is
/// gone (a transition's retired object).
pub fn who_value(vm: &Vm, v: &Value) -> Option<String> {
    match v {
        Value::Ref(u) => Some(who(vm, u)),
        Value::Object(o) if vm.session.object_store.contains_key(&o.uuid) => Some(who(vm, &o.uuid)),
        Value::Object(o) => Some(named(vm, &o.class_name, &o.uuid, o.fields.get("name"))),
        _ => None,
    }
}

/// An overlay as the log names it: `flu on Town a`.
pub fn overlay(vm: &Vm, inst: &crate::session::OverlayInstance) -> String {
    overlay_on(vm, &inst.overlay_name, &inst.host_uuid)
}

/// Overlay kind `name` on object `host`, as the log names it.
pub fn overlay_on(vm: &Vm, name: &str, host: &str) -> String {
    format!("{name} on {}{KEY}kind:{name}", who(vm, host))
}

/// One event: `who` did `what` to `whom` (none, one or more).
pub fn log(vm: &mut Vm, who: String, what: &str, whom: &[String]) -> Result<(), GoblinError> {
    let tick = vm.session.des_ticks;
    let line = if whom.is_empty() {
        format!("tick {tick} | {who} | {what}")
    } else {
        format!("tick {tick} | {who} | {what} | {}", whom.join(", "))
    };
    match vm.session.event_log.as_mut() {
        Some(sink) => sink.put(line),
        None => Ok(()),
    }
}

/// The `:event` stream's seed, from the program's seed.
pub fn event_seed(seed: u128) -> u128 {
    (seed ^ 0xA5A5_5A5A_C3C3_3C3C_0F0F_F0F0_9696_6969u128) | 1
}

fn named_in(session: &crate::session::Session, v: &Value) -> Option<String> {
    let name_of = |class: &str, uuid: &str, name: Option<&Value>| {
        let n = match name {
            Some(Value::Str(s)) if !s.is_empty() => s.to_string(),
            _ => session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_else(|| uuid.to_string()),
        };
        if class.is_empty() { format!("{n}{KEY}{uuid}") } else { format!("{class} {n}{KEY}{uuid}") }
    };
    match v {
        Value::Str(s) => Some(s.to_string()),
        Value::Ref(u) => match session.object_store.get(u) {
            Some(Value::Object(o)) => Some(name_of(&o.class_name, u, o.fields.get("name"))),
            _ => Some(name_of("", u, None)),
        },
        Value::Object(o) => match session.object_store.get(&o.uuid) {
            Some(Value::Object(live)) => Some(name_of(&live.class_name, &o.uuid, live.fields.get("name"))),
            _ => Some(name_of(&o.class_name, &o.uuid, o.fields.get("name"))),
        },
        _ => None,
    }
}

/// `:event(who, what)`, `:event(who, what, whom)`, `:event(who, what,
/// whom, telling)`: one line of narration in the event log,
/// `tick 812 | Kingdom Arveth | ruler died | Person Toren | "of a fever"`.
/// It changes nothing in the program. `who` is an object or a text; `whom`
/// an object, a text, a list of them, or nil; the telling a text, or a map
/// of tellings to weights, one drawn by weight (a weight of 0 can't be
/// drawn; all 0, and nothing is written). The draw has its own random
/// stream, so narrating never changes what the program draws. Returns the
/// telling written (nil when none). With the log off it writes nothing
/// but still draws, so the program sees the same telling either way.
pub fn event(args: Vec<Value>, session: &mut crate::session::Session) -> Result<Value, GoblinError> {
    let bad = |m: String| GoblinError::Runtime(format!("R1253: event: {m}"));
    if args.len() < 2 || args.len() > 4 {
        return Err(GoblinError::ArityMismatch { expected: 4, got: args.len(), name: "event".into() });
    }
    // A map of tellings keeps the order it was written in.
    let mut tellings: Option<Vec<(String, Value)>> = None;
    let args: Vec<Value> = args.into_iter().enumerate().map(|(i, a)| match a {
        Value::Collection(c) if i == 3 && crate::collections::is_map_collection(&c) => {
            tellings = Some(crate::collections::to_pairs(&c).into_iter()
                .map(|(k, v)| (crate::collections::value_to_map_key(&k).unwrap_or_else(|| crate::collections::fmt_value_raw(&k)), v))
                .collect());
            Value::Nil
        }
        Value::Map(m) if i == 3 => {
            tellings = Some(m.into_iter().map(|(k, v)| (k, v)).collect());
            Value::Nil
        }
        Value::Collection(c) => Value::Array(Into::into(crate::collections::to_vec(&c))),
        a => a,
    }).collect();
    let who = named_in(session, &args[0])
        .ok_or_else(|| bad(format!("who must be an object or a text, not {}", args[0].type_name())))?;
    let what = match &args[1] {
        Value::Str(s) if !s.is_empty() => s.to_string(),
        other => return Err(bad(format!("what must be a text, not {}", other.type_name()))),
    };
    let mut whom: Vec<String> = Vec::new();
    match args.get(2) {
        None | Some(Value::Nil) => {}
        Some(Value::Array(xs)) => for x in xs {
            if matches!(x, Value::Nil) { continue; }
            whom.push(named_in(session, x).ok_or_else(|| bad(format!("whom must be objects or texts, not {}", x.type_name())))?);
        },
        Some(x) => whom.push(named_in(session, x).ok_or_else(|| bad(format!("whom must be an object, a text or a list, not {}", x.type_name())))?),
    }
    let telling: Option<String> = match (args.get(3), tellings) {
        (_, Some(m)) => {
            let mut opts: Vec<(String, f64)> = Vec::new();
            let mut sum = 0.0;
            for (k, w) in m.iter() {
                let w = match w {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    Value::Pct(p) => *p,
                    other => return Err(bad(format!("the weight of {k:?} must be a number, not {}", other.type_name()))),
                };
                if !(w >= 0.0) || !w.is_finite() { return Err(bad(format!("the weight of {k:?} must be 0 or more, not {w}"))); }
                sum += w;
                opts.push((k.to_string(), w));
            }
            session.event_rng = session.event_rng.wrapping_mul(0x2360ED051FC65DA44385DF649FCCF645);
            if sum <= 0.0 { return Ok(Value::Nil); }
            let r = (((session.event_rng >> 64) as u64) as f64) / (u64::MAX as f64) * sum;
            let mut acc = 0.0;
            let mut pick = None;
            for (k, w) in &opts {
                if *w <= 0.0 { continue; }
                acc += w;
                pick = Some(k.clone());
                if r < acc { break; }
            }
            pick
        }
        (None | Some(Value::Nil), None) => None,
        (Some(Value::Str(s)), None) => Some(s.to_string()),
        (Some(other), None) => return Err(bad(format!("the telling must be a text or a map of tellings to weights, not {}", other.type_name()))),
    };
    let tick = session.des_ticks;
    let mut line = format!("tick {tick} | {who} | {what}");
    if !whom.is_empty() { line.push_str(&format!(" | {}", whom.join(", "))); }
    if let Some(t) = &telling { line.push_str(&format!(" | {t:?}")); }
    if let Some(sink) = session.event_log.as_mut() { sink.put(line)?; }
    Ok(telling.map(|t| Value::Str(t.into())).unwrap_or(Value::Nil))
}

/// An overlay kind's name as a line names it (with its key).
pub fn kind(name: &str) -> String { format!("{name}{KEY}kind:{name}") }

/// `:timeline(x)` / `:timeline(x, from, to)`: the event log's lines about
/// `x` (an object, or an overlay kind by name), and about what it came
/// from up to the line it was born on (a split's, fracture's, merge's,
/// spawn's or mutation's), in order; with `from`/`to`, only those ticks.
/// It reads the lines back from the log's file.
pub fn timeline(args: Vec<Value>, session: &mut crate::session::Session) -> Result<Value, GoblinError> {
    let bad = |m: String| GoblinError::Runtime(format!("R1254: timeline: {m}"));
    if args.is_empty() || args.len() == 2 || args.len() > 3 {
        return Err(GoblinError::ArityMismatch { expected: 3, got: args.len(), name: "timeline".into() });
    }
    let key = match &args[0] {
        Value::Ref(u) => u.to_string(),
        Value::Object(o) => o.uuid.to_string(),
        Value::Str(s) => format!("kind:{s}"),
        other => return Err(bad(format!("give an object or an overlay kind's name, not {}", other.type_name()))),
    };
    let tick = |v: &Value| match v { Value::Int(i) => Ok(Some(*i)), Value::Nil => Ok(None), other => Err(bad(format!("ticks are whole numbers, not {}", other.type_name()))) };
    let (from, to) = if args.len() == 3 { (tick(&args[1])?, tick(&args[2])?) } else { (None, None) };
    let Some(Sink::File(f, t)) = session.event_log.as_mut() else {
        return Err(bad("the event log is off; start it with `:event_log(path)` before the history you want".into()));
    };
    f.flush().map_err(|e| bad(format!("can't read the event log: {e}")))?;
    let picks: Vec<u64> = t.lines_of(&key).into_iter()
        .map(|l| t.lines[l as usize])
        .filter(|(_, tk)| from.is_none_or(|a| *tk >= a) && to.is_none_or(|b| *tk <= b))
        .map(|(off, _)| off).collect();
    use std::io::{BufRead, Seek, SeekFrom};
    let file = std::fs::File::open(&t.path).map_err(|e| bad(format!("can't read the event log {}: {e}", t.path)))?;
    let mut r = std::io::BufReader::new(file);
    let mut out = Vec::with_capacity(picks.len());
    for off in picks {
        r.seek(SeekFrom::Start(off)).map_err(|e| bad(format!("can't read the event log: {e}")))?;
        let mut line = String::new();
        r.read_line(&mut line).map_err(|e| bad(format!("can't read the event log: {e}")))?;
        out.push(Value::Str(line.trim_end_matches('\n').to_string().into()));
    }
    Ok(Value::Array(Into::into(out)))
}
