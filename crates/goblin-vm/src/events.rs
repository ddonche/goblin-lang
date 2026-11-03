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
