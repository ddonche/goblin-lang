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
    File(std::io::BufWriter<std::fs::File>),
    /// A region's lines for this tick.
    Lines(Vec<String>),
}

impl Sink {
    fn put(&mut self, line: String) -> Result<(), GoblinError> {
        match self {
            Sink::File(f) => writeln!(f, "{line}").map_err(|e| GoblinError::Runtime(format!("R1252: event-log-write: can't write the event log: {e}"))),
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
    session.event_log = Some(Sink::File(std::io::BufWriter::new(f)));
    Ok(())
}

pub fn stop(session: &mut crate::session::Session) -> Result<(), GoblinError> {
    if let Some(Sink::File(mut f)) = session.event_log.take() {
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
    if let Some(Sink::File(f)) = session.event_log.as_mut() {
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
    if class.is_empty() { name } else { format!("{class} {name}") }
}

/// An object given as a value: by reference, or a copy of one that is
/// gone (a transition's retired object).
pub fn who_value(vm: &Vm, v: &Value) -> Option<String> {
    match v {
        Value::Ref(u) => Some(who(vm, u)),
        Value::Object(o) if vm.session.object_store.contains_key(&*o.uuid) => Some(who(vm, &o.uuid)),
        Value::Object(o) => Some(named(vm, &o.class_name, &o.uuid, o.fields.get("name"))),
        _ => None,
    }
}

/// An overlay as the log names it: `flu on Town a`.
pub fn overlay(vm: &Vm, inst: &crate::session::OverlayInstance) -> String {
    format!("{} on {}", inst.overlay_name, who(vm, &inst.host_uuid))
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
        if class.is_empty() { n } else { format!("{class} {n}") }
    };
    match v {
        Value::Str(s) => Some(s.to_string()),
        Value::Ref(u) => match session.object_store.get(u) {
            Some(Value::Object(o)) => Some(name_of(&o.class_name, u, o.fields.get("name"))),
            _ => Some(name_of("", u, None)),
        },
        Value::Object(o) => match session.object_store.get(&*o.uuid) {
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
        Value::Collection(c) => Value::Array(crate::collections::to_vec(&c)),
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
