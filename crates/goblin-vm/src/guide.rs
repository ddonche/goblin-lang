//! `:guide(start, step, milestones, options)`: a guided run. From the
//! checkpoint `start`, Goblin looks for a history that passes every
//! milestone in order, each by its tick.
//!
//! It runs copies of the world one after another. Each copy goes back to a
//! checkpoint with new dice and runs `every` ticks (`step` runs one), asking
//! after each tick whether the milestone is reached. At the end of the
//! stretch the copy says how close it came, and the closest copies go on
//! from there. A copy that reaches the milestone is checkpointed there, and
//! the next milestone starts from those copies. When every copy is past a
//! milestone's tick without reaching it, the milestone before is looked for
//! again with other dice. Nothing in the world is changed by Goblin: every
//! copy is the sim itself, run on.

use crate::checkpoint::{checkpoint_to, restore_from};
use crate::error::GoblinError;
use crate::value::{CollectionValue, Value};
use crate::vm::Vm;
use std::rc::Rc;

fn err(m: impl Into<String>) -> GoblinError { GoblinError::Runtime(m.into()) }

fn items(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::Array(xs) => Some(xs.clone()),
        Value::Collection(c) if !c.is_map() => Some(crate::collections::to_vec(c)),
        _ => None,
    }
}

fn is_map(v: &Value) -> bool {
    match v {
        Value::Map(_) | Value::MapOrd(_) => true,
        Value::Collection(c) => c.is_map(),
        _ => false,
    }
}

fn field(m: &Value, k: &str) -> Option<Value> {
    match crate::collections::get_index(m, &Value::Str(k.into())) {
        Ok(Value::Nil) | Err(_) => None,
        Ok(v) => Some(v),
    }
}

fn list(xs: Vec<Value>) -> Value { Value::Collection(Rc::new(CollectionValue::from_flat(xs))) }

fn map(pairs: Vec<(&str, Value)>) -> Value {
    Value::Collection(Rc::new(CollectionValue::from_map(
        pairs.into_iter().map(|(k, v)| (Value::Str(k.into()), v)).collect())))
}

fn whole(v: &Value, what: &str) -> Result<i64, GoblinError> {
    match v {
        Value::Int(n) => Ok(*n),
        other => Err(err(format!("R1266: guide: {what} is a whole number, not {}", other.type_name()))),
    }
}

fn number(v: &Value) -> Option<f64> {
    match v {
        Value::Int(n) => Some(*n as f64),
        Value::Float(f) | Value::Pct(f) => Some(*f),
        _ => None,
    }
}

struct Milestone {
    name: String,
    by: i64,
    reached: Value,
    closeness: Option<Value>,
}

/// A copy of the world kept to go on from.
#[derive(Clone)]
struct Entry {
    /// Its checkpoint.
    path: String,
    /// The try that made it; 0 for the start.
    id: usize,
    score: f64,
    /// The milestones it has passed: (name, tick, checkpoint, try).
    chain: Vec<(String, i64, String, usize)>,
    /// Its checkpoint is a stretch's end, which Goblin removes once no
    /// copy goes on from it.
    stretch: bool,
}

struct Row {
    tried: usize,
    milestone: String,
    from_try: usize,
    from: i64,
    to: i64,
    outcome: &'static str,
    closeness: Option<f64>,
}

pub(crate) fn guide(vm: &mut Vm, args: &[Value]) -> Result<Value, GoblinError> {
    if !(3..=4).contains(&args.len()) {
        return Err(GoblinError::ArityMismatch { expected: 4, got: args.len(), name: "guide".into() });
    }
    if vm.session.region.is_some() || crate::actor::on_worker() {
        return Err(err("R1266: guide: only the program runs a guided run, not an actor or a region"));
    }
    let start = match &args[0] {
        Value::Str(p) => p.to_string(),
        other => return Err(err(format!("R1266: guide: the start is a checkpoint's file path, not {}", other.type_name()))),
    };
    let step = args[1].clone();
    let list_of = items(&args[2]).ok_or_else(|| err(format!("R1266: guide: the milestones are a list, not {}", args[2].type_name())))?;
    if list_of.is_empty() { return Err(err("R1266: guide: there are no milestones")); }
    let mut ms = Vec::with_capacity(list_of.len());
    for (i, m) in list_of.iter().enumerate() {
        if !is_map(m) { return Err(err(format!("R1266: guide: milestone {} is a map, not {}", i + 1, m.type_name()))); }
        let name = match field(m, "name") { Some(Value::Str(s)) => s.to_string(), _ => format!("{}", i + 1) };
        let by = whole(&field(m, "by").ok_or_else(|| err(format!("R1266: guide: milestone {name} has no `by` (the tick it is reached by)")))?, "`by`")?;
        let reached = field(m, "reached").ok_or_else(|| err(format!("R1266: guide: milestone {name} has no `reached` (the action that says it is reached)")))?;
        ms.push(Milestone { name, by, reached, closeness: field(m, "closeness") });
    }
    let opts = args.get(3).cloned().unwrap_or(Value::Nil);
    if !matches!(opts, Value::Nil) && !is_map(&opts) {
        return Err(err(format!("R1266: guide: the options are a map, not {}", opts.type_name())));
    }
    let opt = |k: &str, d: i64| -> Result<i64, GoblinError> {
        match field(&opts, k) { Some(v) => whole(&v, &format!("`{k}`")), None => Ok(d) }
    };
    let copies = opt("copies", 8)?;
    let every = opt("every", 10)?;
    let keep = opt("keep", (copies + 1) / 2)?;
    let most = opt("tries", 1000)?;
    if copies < 1 || every < 1 || keep < 1 || most < 1 {
        return Err(err("R1266: guide: `copies`, `every`, `keep` and `tries` are 1 or more"));
    }
    let (copies, keep, most) = (copies as usize, keep as usize, most as usize);
    let dir = match field(&opts, "dir") {
        Some(Value::Str(s)) => s.to_string(),
        Some(other) => return Err(err(format!("R1266: guide: `dir` is a folder path, not {}", other.type_name()))),
        None => "guide".to_string(),
    };

    // The program's dice choose every copy's dice, so the same seed finds
    // the same history.
    let base = vm.session.rng_state;
    let mut rows: Vec<Row> = Vec::new();
    let mut tried = 0usize;
    let mut tries_of = vec![0usize; ms.len()];
    let first = Entry { path: start.clone(), id: 0, score: 0.0, chain: Vec::new(), stretch: false };
    // starts[i]: the copies milestone i is looked for from.
    let mut starts: Vec<Vec<Entry>> = vec![vec![first]];
    let mut stage = 0usize;
    let mut gave_up = false;
    // Every milestone checkpoint made; only the found history's stay.
    let mut made: Vec<String> = Vec::new();
    let drop_file = |e: &Entry| { if e.stretch { let _ = std::fs::remove_file(&e.path); } };
    'stages: while stage < ms.len() {
        let m = &ms[stage];
        let mut pool = starts[stage].clone();
        loop {
            let mut won: Vec<Entry> = Vec::new();
            let mut kept: Vec<(Entry, usize)> = Vec::new();
            for c in 0..copies {
                if tried >= most { gave_up = true; break; }
                tried += 1;
                tries_of[stage] += 1;
                let parent = &pool[c % pool.len()];
                let seed = crate::swarm::task_seed(base, tried);
                let from = restore_from(vm, &parent.path, Some(seed))?;
                let (outcome, to) = loop {
                    let before = vm.session.des_ticks;
                    vm.call_callable(step.clone(), Vec::new())?;
                    let t = vm.session.des_ticks;
                    if t <= before { return Err(err("R1266: guide: the step ran no tick (it calls `:tick()` once)")); }
                    match vm.call_callable(m.reached.clone(), Vec::new())? {
                        Value::Bool(true) => break ("reached", t),
                        Value::Bool(false) => {}
                        other => return Err(err(format!("R1266: guide: `reached` of milestone {} gave {}, not true or false", m.name, other.type_name()))),
                    }
                    if t >= m.by { break ("too late", t); }
                    if t - from >= every { break ("kept", t); }
                };
                let mut row = Row { tried, milestone: m.name.clone(), from_try: parent.id, from, to, outcome, closeness: None };
                match outcome {
                    "reached" => {
                        let path = format!("{dir}/{}_{}_{tried}.gcp", stage + 1, m.name);
                        checkpoint_to(vm, &path)?;
                        made.push(path.clone());
                        let mut chain = parent.chain.clone();
                        chain.push((m.name.clone(), to, path.clone(), tried));
                        won.push(Entry { path, id: tried, score: 0.0, chain, stretch: false });
                    }
                    "kept" => {
                        let score = match &m.closeness {
                            None => 0.0,
                            Some(f) => {
                                let v = vm.call_callable(f.clone(), Vec::new())?;
                                number(&v).ok_or_else(|| err(format!("R1266: guide: `closeness` of milestone {} gave {}, not a number", m.name, v.type_name())))?
                            }
                        };
                        row.closeness = Some(score);
                        let path = format!("{dir}/try_{tried}.gcp");
                        checkpoint_to(vm, &path)?;
                        kept.push((Entry { path, id: tried, score, chain: parent.chain.clone(), stretch: true }, rows.len()));
                    }
                    _ => {}
                }
                rows.push(row);
            }
            // The copies this round went on from are done with.
            for e in &pool { drop_file(e); }
            if !won.is_empty() {
                for (e, _) in &kept { drop_file(e); }
                starts.truncate(stage + 1);
                starts.push(won);
                stage += 1;
                continue 'stages;
            }
            if gave_up {
                for (e, _) in &kept { drop_file(e); }
                break 'stages;
            }
            if kept.is_empty() {
                // A dead end: no copy can reach this milestone in time from
                // here, so the one before is looked for again.
                if stage > 0 {
                    starts.truncate(stage);
                    stage -= 1;
                }
                continue 'stages;
            }
            // The closest go on; a tie keeps the earlier try.
            kept.sort_by(|a, b| b.0.score.partial_cmp(&a.0.score).unwrap_or(std::cmp::Ordering::Equal));
            for (e, row) in kept.iter().skip(keep) {
                rows[*row].outcome = "dropped";
                drop_file(e);
            }
            kept.truncate(keep);
            pool = kept.into_iter().map(|(e, _)| e).collect();
        }
    }

    // The world is left where the history found ends: at the last milestone
    // passed, or at the start when none was.
    let best = starts.last().and_then(|s| s.first()).cloned();
    let chain = best.as_ref().map(|e| e.chain.clone()).unwrap_or_default();
    // Only the found history's checkpoints stay.
    let keep_paths: std::collections::HashSet<&str> = chain.iter().map(|(_, _, p, _)| p.as_str()).collect();
    for p in &made { if !keep_paths.contains(p.as_str()) { let _ = std::fs::remove_file(p); } }
    match &best {
        Some(e) => { restore_from(vm, &e.path, None)?; }
        None => { restore_from(vm, &start, None)?; }
    }

    let passed = chain.len();
    let milestones = ms.iter().enumerate().map(|(i, m)| {
        let mut f = vec![("name", Value::Str(m.name.as_str().into())), ("tries", Value::Int(tries_of[i] as i64))];
        match chain.get(i) {
            Some((_, t, p, n)) => {
                f.push(("tick", Value::Int(*t)));
                f.push(("try", Value::Int(*n as i64)));
                f.push(("checkpoint", Value::Str(p.as_str().into())));
            }
            None => { f.push(("tick", Value::Nil)); f.push(("try", Value::Nil)); f.push(("checkpoint", Value::Nil)); }
        }
        map(f)
    }).collect();
    let log = rows.iter().map(|r| map(vec![
        ("try", Value::Int(r.tried as i64)),
        ("milestone", Value::Str(r.milestone.as_str().into())),
        ("from_try", Value::Int(r.from_try as i64)),
        ("from", Value::Int(r.from)),
        ("to", Value::Int(r.to)),
        ("outcome", Value::Str(r.outcome.into())),
        ("closeness", r.closeness.map(Value::Float).unwrap_or(Value::Nil)),
    ])).collect();
    Ok(map(vec![
        ("reached", Value::Bool(passed == ms.len())),
        ("passed", Value::Int(passed as i64)),
        ("tries", Value::Int(tried as i64)),
        ("milestones", list(milestones)),
        ("log", list(log)),
    ]))
}
