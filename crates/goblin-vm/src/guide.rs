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
use crate::portable::{FromPortable, PValue};
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
    /// What its history's `:event` tellings were, stretch by stretch.
    log: Option<Rc<LogNode>>,
}

/// One stretch's event lines, after the stretches before it.
struct LogNode {
    lines: Vec<String>,
    parent: Option<Rc<LogNode>>,
}

/// One copy to run: the try it is, the checkpoint it goes on from, its dice.
struct Job {
    tried: usize,
    parent: String,
    seed: u128,
}

/// What every copy of a round runs.
struct Spec<'a> {
    stage: usize,
    name: &'a str,
    by: i64,
    every: i64,
    dir: &'a str,
    step: &'a PValue,
    reached: &'a PValue,
    closeness: Option<&'a PValue>,
    logging: bool,
}

/// What a copy did.
struct Done {
    outcome: &'static str,
    from: i64,
    to: i64,
    score: Option<f64>,
    /// Its checkpoint, when it reached the milestone or is kept.
    path: Option<String>,
    output: String,
    events: Vec<String>,
}

/// Runs a round's copies side by side, each in its own session built from
/// `image`; their results in the jobs' order.
fn run_round(image: &crate::swarm::Image, spec: &Spec, jobs: &[Job], threads: usize) -> Vec<Result<Done, String>> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let next = AtomicUsize::new(0);
    let out: std::sync::Mutex<Vec<Option<Result<Done, String>>>> = std::sync::Mutex::new((0..jobs.len()).map(|_| None).collect());
    std::thread::scope(|sc| {
        for w in 0..threads.min(jobs.len()) {
            let (next, out) = (&next, &out);
            let spawned = std::thread::Builder::new()
                .name(format!("goblin-guide-{w}"))
                .stack_size(64 * 1024 * 1024)
                .spawn_scoped(sc, move || {
                    let mut fp = FromPortable::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::SeqCst);
                        if i >= jobs.len() { break; }
                        fp.forget_values();
                        let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| run_copy(image, &mut fp, w + 1, spec, &jobs[i])));
                        let r = match r {
                            Ok(Ok(d)) => Ok(d),
                            Ok(Err(e)) => Err(e.to_string()),
                            Err(_) => Err(format!("R1266: guide: try {} crashed (internal error)", jobs[i].tried)),
                        };
                        out.lock().unwrap_or_else(|e| e.into_inner())[i] = Some(r);
                    }
                });
            if spawned.is_err() { break; }
        }
    });
    out.into_inner().unwrap_or_else(|e| e.into_inner()).into_iter().enumerate()
        .map(|(i, r)| r.unwrap_or_else(|| Err(format!("R1266: guide: try {} could not start a thread", jobs[i].tried))))
        .collect()
}

/// One copy: back to its checkpoint with its dice, then on, a tick at a
/// time, until the milestone is reached, its tick has passed, or the
/// stretch is over.
fn run_copy(image: &crate::swarm::Image, fp: &mut FromPortable, worker: usize, spec: &Spec, job: &Job) -> Result<Done, GoblinError> {
    let mut vm = Vm::new(crate::swarm::build_session(image, fp, worker));
    vm.session.output_buf = Some(String::new());
    if spec.logging { vm.session.event_log = Some(crate::events::Sink::Lines(Vec::new())); }
    let step = fp.value(spec.step, &mut vm.session);
    let reached = fp.value(spec.reached, &mut vm.session);
    let closeness = spec.closeness.map(|c| fp.value(c, &mut vm.session));
    let from = restore_from(&mut vm, &job.parent, Some(job.seed))?;
    let (outcome, to) = loop {
        let before = vm.session.des_ticks;
        vm.call_callable(step.clone(), Vec::new())?;
        let t = vm.session.des_ticks;
        if t <= before { return Err(err("R1266: guide: the step ran no tick (it calls `:tick()` once)")); }
        match vm.call_callable(reached.clone(), Vec::new())? {
            Value::Bool(true) => break ("reached", t),
            Value::Bool(false) => {}
            other => return Err(err(format!("R1266: guide: `reached` of milestone {} gave {}, not true or false", spec.name, other.type_name()))),
        }
        if t >= spec.by { break ("too late", t); }
        if t - from >= spec.every { break ("kept", t); }
    };
    let mut score = None;
    let path = match outcome {
        "reached" => Some(format!("{}/{}_{}_{}.gcp", spec.dir, spec.stage + 1, spec.name, job.tried)),
        "kept" => {
            score = Some(match &closeness {
                None => 0.0,
                Some(f) => {
                    let v = vm.call_callable(f.clone(), Vec::new())?;
                    number(&v).ok_or_else(|| err(format!("R1266: guide: `closeness` of milestone {} gave {}, not a number", spec.name, v.type_name())))?
                }
            });
            Some(format!("{}/try_{}.gcp", spec.dir, job.tried))
        }
        _ => None,
    };
    if let Some(p) = &path { checkpoint_to(&mut vm, p)?; }
    let events = match vm.session.event_log.take() { Some(crate::events::Sink::Lines(v)) => v, _ => Vec::new() };
    let output = vm.session.take_output();
    Ok(Done { outcome, from, to, score, path, output, events })
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
    // Every copy runs in a session of its own, built from the program as it
    // is now: what one copy leaves in a plain variable no other copy sees,
    // so the history found is the same however many run side by side.
    let (image, p_step, p_ms) = {
        let mut tp = crate::portable::ToPortable::new(&vm.session);
        let image = crate::swarm::image_of(&vm.session, &mut tp, None)?;
        let p_step = tp.value(&step)?;
        let mut p_ms = Vec::with_capacity(ms.len());
        for m in &ms {
            let c = match &m.closeness { Some(c) => Some(tp.value(c)?), None => None };
            p_ms.push((tp.value(&m.reached)?, c));
        }
        (image, p_step, p_ms)
    };
    let logging = vm.session.event_log.is_some();
    // As many side by side as there are cores, or `GOBLIN_GUIDE_THREADS`.
    let cores = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1);
    let threads = std::env::var("GOBLIN_GUIDE_THREADS").ok().and_then(|v| v.parse::<usize>().ok()).unwrap_or(cores).min(copies).max(1);
    let mut rows: Vec<Row> = Vec::new();
    let mut tried = 0usize;
    let mut tries_of = vec![0usize; ms.len()];
    let first = Entry { path: start.clone(), id: 0, score: 0.0, chain: Vec::new(), stretch: false, log: None };
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
            let mut jobs = Vec::with_capacity(copies);
            for c in 0..copies {
                if tried >= most { gave_up = true; break; }
                tried += 1;
                tries_of[stage] += 1;
                let parent = &pool[c % pool.len()];
                jobs.push(Job { tried, parent: parent.path.clone(), seed: crate::swarm::task_seed(base, tried) });
            }
            let spec = Spec { stage, name: &m.name, by: m.by, every, dir: &dir, step: &p_step, reached: &p_ms[stage].0, closeness: p_ms[stage].1.as_ref(), logging };
            let done = run_round(&image, &spec, &jobs, threads);
            for (c, (job, res)) in jobs.iter().zip(done).enumerate() {
                let parent = &pool[c % pool.len()];
                let r = res.map_err(err)?;
                if !r.output.is_empty() { vm.session.write_output(&r.output, false); }
                let log = if logging { Some(Rc::new(LogNode { lines: r.events, parent: parent.log.clone() })) } else { None };
                rows.push(Row { tried: job.tried, milestone: m.name.clone(), from_try: parent.id, from: r.from, to: r.to, outcome: r.outcome, closeness: r.score });
                match (r.outcome, r.path) {
                    ("reached", Some(path)) => {
                        made.push(path.clone());
                        let mut chain = parent.chain.clone();
                        chain.push((m.name.clone(), r.to, path.clone(), job.tried));
                        won.push(Entry { path, id: job.tried, score: 0.0, chain, stretch: false, log });
                    }
                    ("kept", Some(path)) => {
                        kept.push((Entry { path, id: job.tried, score: r.score.unwrap_or(0.0), chain: parent.chain.clone(), stretch: true, log }, rows.len() - 1));
                    }
                    _ => {}
                }
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
    // The event log tells the history found, from the start to where the
    // world is now; the other tries' tellings are left out.
    if let Some(node) = best.as_ref().and_then(|e| e.log.clone()) {
        let mut parts: Vec<Rc<LogNode>> = Vec::new();
        let mut at = Some(node);
        while let Some(n) = at { at = n.parent.clone(); parts.push(n); }
        for n in parts.iter().rev() { crate::events::put_lines(&mut vm.session, n.lines.clone())?; }
        crate::events::flush(&mut vm.session)?;
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
