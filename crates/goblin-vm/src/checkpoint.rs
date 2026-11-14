//! `:checkpoint(path)` and `:restore(path)`: the DES world at a tick,
//! written to a file and read back.
//!
//! A checkpoint holds everything a run of the world goes on from: every
//! object with its fields and traits, the overlays and their kinds, the
//! links, the tick count and the dice, of the program and of every region.
//! The program's plain variables are not in it: what has to last lives in
//! objects. Restored, the world goes on exactly as it would have from the
//! tick it was saved at.

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::des_image::{DesImage, POverlayDef};
use crate::error::GoblinError;
use crate::portable::{FromPortable, PLayout, PValue, ToPortable};
use crate::session::Session;
use crate::value::{BackendHint, CollectionMeta, FormatSpec, GoblinDateKind, GoblinDateTime, Value};
use crate::vm::Vm;

const MAGIC: &[u8; 8] = b"GOBLINCP";
/// Changes whenever what a checkpoint holds changes.
const FORMAT: u32 = 3;

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

// ── Values on disk ───────────────────────────────────────────────────────────

/// A `PValue` as it is written: what can't be written (code, actors,
/// channels) is an error when it is met.
#[derive(Serialize, Deserialize)]
enum CV {
    Nil,
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Big([u8; 16]),
    Pct(f64),
    Char(char),
    Str(String),
    DateTime { secs: i64, nanos: u32, tz: Option<String>, kind: u8 },
    Duration { ns: i128, unit: String, unit_ns: i128 },
    Formatted(Box<CV>, u32, Option<char>, char),
    Array(Vec<CV>),
    Map(Vec<(String, CV)>),
    MapOrd(Vec<(String, CV)>),
    Pair(Box<CV>, Box<CV>),
    Seq(Vec<CV>),
    CtrlSkip,
    CtrlStop,
    CtrlReturn(Box<CV>),
    Object { class_name: String, fields: Vec<(String, CV)>, readonly: Vec<String>, traits: Vec<String>, uuid: String, stored: Vec<(u64, CV)> },
    Enum { enum_name: String, variant_name: String, fields: Option<Vec<(String, CV)>> },
    Class(String),
    Collection(CLayout, [u32; 5], u64, u8),
    Frozen(Box<CV>),
    HandedOff { uuid: String, var: String },
    KeptHome { uuid: String, var: String },
    Moved { uuid: String, var: String },
    ObjRef(String),
    Shared(Box<CV>),
}

#[derive(Serialize, Deserialize)]
enum CLayout {
    FlatArray(Vec<CV>),
    RingBuf { buf: Vec<CV>, head: u64, len: u64 },
    ChunkedSeq { chunks: Vec<Vec<CV>>, len: u64 },
    SmallMap(Vec<(CV, CV)>),
    HashMapBackend(Vec<(CV, CV)>),
}

fn hint_to(h: BackendHint) -> u8 {
    match h {
        BackendHint::Auto => 0, BackendHint::FlatArray => 1, BackendHint::RingBuf => 2,
        BackendHint::ChunkedSeq => 3, BackendHint::SmallMap => 4, BackendHint::HashMapBackend => 5,
    }
}

fn hint_from(n: u8) -> BackendHint {
    match n {
        1 => BackendHint::FlatArray, 2 => BackendHint::RingBuf, 3 => BackendHint::ChunkedSeq,
        4 => BackendHint::SmallMap, 5 => BackendHint::HashMapBackend, _ => BackendHint::Auto,
    }
}

fn cant(what: &str) -> String {
    format!("R1265: checkpoint-holds-code: the world holds {what} (in an object's field, an overlay's data or a message on its way), which a checkpoint can't hold; keep plain values there (numbers, text, lists, maps, objects)")
}

impl CV {
    fn of(p: &PValue) -> Result<CV, String> {
        let all = |v: &[PValue]| v.iter().map(CV::of).collect::<Result<Vec<_>, _>>();
        let named = |v: &[(String, PValue)]| v.iter().map(|(k, x)| Ok((k.clone(), CV::of(x)?))).collect::<Result<Vec<_>, String>>();
        Ok(match p {
            PValue::Nil => CV::Nil,
            PValue::Unit => CV::Unit,
            PValue::Bool(b) => CV::Bool(*b),
            PValue::Int(i) => CV::Int(*i),
            PValue::Float(f) => CV::Float(*f),
            PValue::Big(d) => CV::Big(d.serialize()),
            PValue::Pct(x) => CV::Pct(*x),
            PValue::Char(c) => CV::Char(*c),
            PValue::Str(s) => CV::Str(s.to_string()),
            PValue::DateTime(d) => CV::DateTime {
                secs: d.utc.timestamp(), nanos: d.utc.timestamp_subsec_nanos(), tz: d.tz.clone(),
                kind: match d.kind { GoblinDateKind::DateTime => 0, GoblinDateKind::Date => 1, GoblinDateKind::Time => 2 },
            },
            PValue::Duration(d) => CV::Duration { ns: d.ns, unit: d.unit.clone(), unit_ns: d.unit_ns },
            PValue::Formatted(v, f) => CV::Formatted(Box::new(CV::of(v)?), f.decimals, f.sep_thousands, f.sep_decimal),
            PValue::Array(v) => CV::Array(all(v)?),
            PValue::Map(m) => CV::Map(m.iter().map(|(k, x)| Ok((k.clone(), CV::of(x)?))).collect::<Result<Vec<_>, String>>()?),
            PValue::MapOrd(m) => CV::MapOrd(named(m)?),
            PValue::Pair(a, b) => CV::Pair(Box::new(CV::of(a)?), Box::new(CV::of(b)?)),
            PValue::Seq(v) => CV::Seq(all(v)?),
            PValue::CtrlSkip => CV::CtrlSkip,
            PValue::CtrlStop => CV::CtrlStop,
            PValue::CtrlReturn(v) => CV::CtrlReturn(Box::new(CV::of(v)?)),
            PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid, stored } => CV::Object {
                class_name: class_name.clone(),
                fields: named(fields)?,
                readonly: readonly_fields.iter().cloned().collect(),
                traits: trait_fields.iter().cloned().collect(),
                uuid: uuid.clone(),
                stored: stored.iter().map(|(i, x)| Ok((*i as u64, CV::of(x)?))).collect::<Result<Vec<_>, String>>()?,
            },
            PValue::Enum { enum_name, variant_name, fields } => CV::Enum {
                enum_name: enum_name.clone(), variant_name: variant_name.clone(),
                fields: match fields { Some(f) => Some(named(f)?), None => None },
            },
            PValue::Class { name } => CV::Class(name.clone()),
            PValue::Collection(l, m) => {
                let pairs = |v: &[(PValue, PValue)]| v.iter().map(|(a, b)| Ok((CV::of(a)?, CV::of(b)?))).collect::<Result<Vec<_>, String>>();
                let l = match l {
                    PLayout::FlatArray(v) => CLayout::FlatArray(all(v)?),
                    PLayout::RingBuf { buf, head, len } => CLayout::RingBuf { buf: all(buf)?, head: *head as u64, len: *len as u64 },
                    PLayout::ChunkedSeq { chunks, len } => CLayout::ChunkedSeq {
                        chunks: chunks.iter().map(|c| all(c)).collect::<Result<Vec<_>, _>>()?, len: *len as u64,
                    },
                    PLayout::SmallMap(v) => CLayout::SmallMap(pairs(v)?),
                    PLayout::HashMapBackend(v) => CLayout::HashMapBackend(pairs(v)?),
                };
                CV::Collection(l, [m.front_hits, m.back_hits, m.mid_hits, m.random_hits, m.scan_hits], m.len as u64, hint_to(m.backend_hint))
            }
            PValue::Frozen(v) => CV::Frozen(Box::new(CV::of(v)?)),
            PValue::HandedOff { uuid, var } => CV::HandedOff { uuid: uuid.clone(), var: var.clone() },
            PValue::KeptHome { uuid, var } => CV::KeptHome { uuid: uuid.clone(), var: var.clone() },
            PValue::Moved { uuid, var } => CV::Moved { uuid: uuid.clone(), var: var.clone() },
            PValue::ObjRef(u) => CV::ObjRef(u.clone()),
            PValue::Shared(v) => CV::Shared(Box::new(CV::of(v)?)),
            PValue::Function(f) | PValue::Closure(f, _) => return Err(cant(&format!("code (the action `{}`)", f.name))),
            PValue::Builtin(_) => return Err(cant("a builtin")),
            PValue::Handle(_) => return Err(cant("an actor, channel or future")),
            PValue::World(_) => return Err(cant("a world tick")),
        })
    }

    fn into_p(self) -> PValue {
        let all = |v: Vec<CV>| v.into_iter().map(CV::into_p).collect::<Vec<_>>();
        let named = |v: Vec<(String, CV)>| v.into_iter().map(|(k, x)| (k, x.into_p())).collect::<Vec<_>>();
        match self {
            CV::Nil => PValue::Nil,
            CV::Unit => PValue::Unit,
            CV::Bool(b) => PValue::Bool(b),
            CV::Int(i) => PValue::Int(i),
            CV::Float(f) => PValue::Float(f),
            CV::Big(b) => PValue::Big(rust_decimal::Decimal::deserialize(b)),
            CV::Pct(x) => PValue::Pct(x),
            CV::Char(c) => PValue::Char(c),
            CV::Str(s) => PValue::Str(Arc::from(s.as_str())),
            CV::DateTime { secs, nanos, tz, kind } => PValue::DateTime(GoblinDateTime {
                utc: chrono::DateTime::from_timestamp(secs, nanos).unwrap_or_default(),
                tz,
                kind: match kind { 1 => GoblinDateKind::Date, 2 => GoblinDateKind::Time, _ => GoblinDateKind::DateTime },
            }),
            CV::Duration { ns, unit, unit_ns } => PValue::Duration(crate::duration::Duration { ns, unit, unit_ns }),
            CV::Formatted(v, decimals, sep_thousands, sep_decimal) => PValue::Formatted(Box::new(v.into_p()), FormatSpec { decimals, sep_thousands, sep_decimal }),
            CV::Array(v) => PValue::Array(all(v)),
            CV::Map(m) => PValue::Map(m.into_iter().map(|(k, x)| (k, x.into_p())).collect::<BTreeMap<_, _>>()),
            CV::MapOrd(m) => PValue::MapOrd(named(m)),
            CV::Pair(a, b) => PValue::Pair(Box::new(a.into_p()), Box::new(b.into_p())),
            CV::Seq(v) => PValue::Seq(all(v)),
            CV::CtrlSkip => PValue::CtrlSkip,
            CV::CtrlStop => PValue::CtrlStop,
            CV::CtrlReturn(v) => PValue::CtrlReturn(Box::new(v.into_p())),
            CV::Object { class_name, fields, readonly, traits, uuid, stored } => PValue::Object {
                class_name,
                fields: named(fields),
                readonly_fields: readonly.into_iter().collect::<BTreeSet<_>>(),
                trait_fields: traits.into_iter().collect::<BTreeSet<_>>(),
                uuid,
                stored: stored.into_iter().map(|(i, x)| (i as usize, x.into_p())).collect(),
            },
            CV::Enum { enum_name, variant_name, fields } => PValue::Enum { enum_name, variant_name, fields: fields.map(named) },
            CV::Class(name) => PValue::Class { name },
            CV::Collection(l, hits, len, hint) => {
                let pairs = |v: Vec<(CV, CV)>| v.into_iter().map(|(a, b)| (a.into_p(), b.into_p())).collect::<Vec<_>>();
                let l = match l {
                    CLayout::FlatArray(v) => PLayout::FlatArray(all(v)),
                    CLayout::RingBuf { buf, head, len } => PLayout::RingBuf { buf: all(buf), head: head as usize, len: len as usize },
                    CLayout::ChunkedSeq { chunks, len } => PLayout::ChunkedSeq { chunks: chunks.into_iter().map(all).collect(), len: len as usize },
                    CLayout::SmallMap(v) => PLayout::SmallMap(pairs(v)),
                    CLayout::HashMapBackend(v) => PLayout::HashMapBackend(pairs(v)),
                };
                let meta = CollectionMeta {
                    len: len as usize, front_hits: hits[0], back_hits: hits[1], mid_hits: hits[2],
                    random_hits: hits[3], scan_hits: hits[4], backend_hint: hint_from(hint),
                };
                PValue::Collection(l, meta)
            }
            CV::Frozen(v) => PValue::Frozen(Arc::new(v.into_p())),
            CV::HandedOff { uuid, var } => PValue::HandedOff { uuid, var },
            CV::KeptHome { uuid, var } => PValue::KeptHome { uuid, var },
            CV::Moved { uuid, var } => PValue::Moved { uuid, var },
            CV::ObjRef(u) => PValue::ObjRef(u),
            CV::Shared(v) => PValue::Shared(Arc::new(v.into_p())),
        }
    }
}

impl Serialize for PValue {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        CV::of(self).map_err(serde::ser::Error::custom)?.serialize(s)
    }
}

impl<'de> Deserialize<'de> for PValue {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<PValue, D::Error> {
        Ok(CV::deserialize(d)?.into_p())
    }
}

// ── One session's part of the world ─────────────────────────────────────────

/// What one session (the program, or a region) holds of the world.
#[derive(Serialize, Deserialize)]
pub(crate) struct SessionState {
    /// Its objects, in the order they were made.
    objects: Vec<(String, PValue)>,
    /// The overlay kinds, link and decision definitions, the overlays on
    /// its objects, the names they were stored under, the link changes.
    des: DesImage,
    kind_counts: Vec<(String, u64)>,
    kind_order: Vec<String>,
    lines_run: Vec<String>,
    ticks: i64,
    link_ids: u32,
    /// The dice: what the program draws, and what `:event` tellings draw.
    rng: u128,
    event_rng: u128,
    objects_made: u64,
    overlays_made: u64,
    moved_away: Vec<(String, String)>,
    kept_home: Vec<(String, String)>,
    handed_off: Vec<(String, String)>,
    /// In a region: the owners it gave other regions' objects last tick.
    owners_sent: Vec<(String, PValue)>,
}

fn sorted(m: &std::collections::HashMap<String, String>) -> Vec<(String, String)> {
    let mut v: Vec<(String, String)> = m.iter().map(|(k, x)| (k.clone(), x.clone())).collect();
    v.sort();
    v
}

/// The session's part of the world, as it is now.
pub(crate) fn session_state(s: &Session) -> Result<SessionState, GoblinError> {
    let mut tp = ToPortable::new(s).objects_by_id();
    let mut objects = Vec::with_capacity(s.object_store.len());
    for (u, v) in s.object_store.iter() { objects.push((u.to_string(), tp.value(v)?)); }
    let ids: HashSet<String> = s.object_store.keys().map(|k| k.to_string()).collect();
    let des = crate::des_image::des_image_of(s, &mut tp, Some(&ids))?;
    let mut owners_sent = Vec::with_capacity(s.owners_sent.len());
    for (u, v) in &s.owners_sent { owners_sent.push((u.to_string(), tp.value(v)?)); }
    drop(tp);
    let mut kind_counts: Vec<(String, u64)> = s.kind_counts.iter().map(|(k, n)| (k.clone(), *n)).collect();
    kind_counts.sort();
    let mut lines_run: Vec<String> = s.overlay_lines_run.iter().cloned().collect();
    lines_run.sort();
    Ok(SessionState {
        objects, des, kind_counts, kind_order: s.kind_order.clone(), lines_run,
        ticks: s.des_ticks, link_ids: s.des_link_id_counter, rng: s.rng_state, event_rng: s.event_rng,
        objects_made: s.des_metrics.objects_made, overlays_made: s.des_metrics.overlays_made,
        moved_away: sorted(&s.moved_away), kept_home: sorted(&s.kept_home), handed_off: sorted(&s.handed_off),
        owners_sent,
    })
}

/// Replaces the session's part of the world with `st`.
pub(crate) fn load_session_state(st: &SessionState, s: &mut Session) {
    s.object_store = Default::default();
    s.overlay_instances = Default::default();
    s.object_names.clear();
    s.link_offsets = Default::default();
    s.des_link_ids.clear();
    s.remote_copies.clear();
    s.sent_objects.clear();
    s.portable_kinds.borrow_mut().clear();
    s.phase_views.clear();
    s.transitioned.clear();
    s.des_erasing.clear();
    s.moved_away = st.moved_away.iter().cloned().collect();
    s.kept_home = st.kept_home.iter().cloned().collect();
    s.handed_off = st.handed_off.iter().cloned().collect();
    let mut fp = FromPortable::new();
    for (u, p) in &st.objects {
        let v = fp.value(p, s);
        s.object_store.insert(crate::value::ObjId::from(u.as_str()), v);
    }
    crate::des_image::install(&st.des, &mut fp, s);
    s.owners_sent = st.owners_sent.iter().map(|(u, p)| (crate::value::ObjId::from(u.as_str()), fp.value(p, s))).collect();
    s.kind_counts = st.kind_counts.iter().cloned().collect();
    s.kind_order = st.kind_order.clone();
    s.overlay_lines_run = st.lines_run.iter().cloned().collect();
    s.des_ticks = st.ticks;
    s.des_link_id_counter = st.link_ids;
    s.rng_state = st.rng;
    s.event_rng = st.event_rng;
    s.des_metrics.objects_made = st.objects_made;
    s.des_metrics.overlays_made = st.overlays_made;
}

// ── The file ─────────────────────────────────────────────────────────────────

#[derive(Serialize, Deserialize)]
struct Checkpoint {
    program: SessionState,
    /// The program's variables that name an object of the world (variable,
    /// object): they name it again once restored.
    names: Vec<(String, String)>,
    /// The regions, when the world runs on them.
    world: Option<crate::world::WorldState>,
}

fn path_arg(args: &[Value], name: &str) -> Result<String, GoblinError> {
    match args {
        [Value::Str(p)] => Ok(p.to_string()),
        [other] => Err(err(format!("R1263: checkpoint: `:{name}` takes a file path, not {}", other.type_name()))),
        _ => Err(GoblinError::ArityMismatch { expected: 1, got: args.len(), name: name.into() }),
    }
}

/// `:checkpoint(path)`: writes the world as it is now to `path`. Returns
/// the tick it was taken at.
pub(crate) fn checkpoint(vm: &mut Vm, args: &[Value]) -> Result<Value, GoblinError> {
    let path = path_arg(args, "checkpoint")?;
    if vm.session.region.is_some() || crate::actor::on_worker() {
        return Err(err("R1263: checkpoint: only the program takes a checkpoint, not an actor or a region"));
    }
    let world = crate::world::world_state(vm)?;
    let cp = Checkpoint { program: session_state(&vm.session)?, names: object_names(&vm.session), world };
    let tick = vm.session.des_ticks;
    let mut out = Vec::with_capacity(1 << 16);
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&FORMAT.to_le_bytes());
    // The overlay kinds go once, ahead of the rest: every session and every
    // overlay of a kind names it by its place in that table.
    KINDS_OUT.with(|k| *k.borrow_mut() = Some(KindsOut::default()));
    let body = bincode::serialize(&cp);
    let kinds = KINDS_OUT.with(|k| k.borrow_mut().take()).unwrap_or_default();
    let body = body.map_err(|e| err(e.to_string()))?;
    {
        use std::io::Write;
        let mut z = flate2::write::DeflateEncoder::new(&mut out, flate2::Compression::fast());
        bincode::serialize_into(&mut z, &kinds.blobs).map_err(|e| err(e.to_string()))?;
        z.write_all(&body).map_err(|e| err(e.to_string()))?;
        z.finish().map_err(|e| err(e.to_string()))?;
    }
    if let Some(dir) = std::path::Path::new(&path).parent() {
        if !dir.as_os_str().is_empty() && !dir.exists() {
            std::fs::create_dir_all(dir).map_err(|e| err(format!("R1263: checkpoint: can't make the folder {}: {e}", dir.display())))?;
        }
    }
    std::fs::write(&path, out).map_err(|e| err(format!("R1263: checkpoint: can't write {path}: {e}")))?;
    Ok(Value::Int(tick))
}

/// `:restore(path)`: the world goes back to the checkpoint at `path`. What
/// it holds now is gone. Returns the tick the checkpoint was taken at.
pub(crate) fn restore(vm: &mut Vm, args: &[Value]) -> Result<Value, GoblinError> {
    let path = path_arg(args, "restore")?;
    if vm.session.region.is_some() || crate::actor::on_worker() {
        return Err(err("R1264: restore: only the program restores a checkpoint, not an actor or a region"));
    }
    let bytes = std::fs::read(&path).map_err(|e| err(format!("R1264: restore: can't read {path}: {e}")))?;
    if bytes.len() < 12 || &bytes[..8] != MAGIC {
        return Err(err(format!("R1264: restore: {path} is not a checkpoint")));
    }
    let format = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    if format != FORMAT {
        return Err(err(format!("R1264: restore: {path} was written by another version of Goblin (checkpoint format {format}, this one reads {FORMAT})")));
    }
    let bad = |e: bincode::Error| err(format!("R1264: restore: {path} can't be read: {e}"));
    let mut raw = Vec::with_capacity(bytes.len() * 8);
    {
        use std::io::Read;
        flate2::read::DeflateDecoder::new(&bytes[12..]).read_to_end(&mut raw)
            .map_err(|e| err(format!("R1264: restore: {path} can't be read: {e}")))?;
    }
    let mut rest = &raw[..];
    let blobs: Vec<Vec<u8>> = bincode::deserialize_from(&mut rest).map_err(bad)?;
    let mut kinds = Vec::with_capacity(blobs.len());
    for b in &blobs { kinds.push(Arc::new(bincode::deserialize::<POverlayDef>(b).map_err(bad)?)); }
    KINDS_IN.with(|k| *k.borrow_mut() = kinds);
    let cp: Result<Checkpoint, _> = bincode::deserialize(rest);
    KINDS_IN.with(|k| k.borrow_mut().clear());
    let cp = cp.map_err(bad)?;
    crate::world::end_world(vm);
    load_session_state(&cp.program, &mut vm.session);
    name_objects(&mut vm.session, &cp.names);
    if let Some(w) = cp.world {
        crate::world::load_world(vm, w)?;
        // Making the regions drew nothing that counts: the dice are as saved.
        vm.session.rng_state = cp.program.rng;
        vm.session.event_rng = cp.program.event_rng;
    }
    Ok(Value::Int(vm.session.des_ticks))
}

/// The program's variables that hold an object of the world.
fn object_names(s: &Session) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (i, g) in s.globals.iter().enumerate() {
        let Some(t) = g else { continue };
        let Ok(st) = s.get_stash(t) else { continue };
        let u = match &st.value { Value::Ref(u) => u.as_str(), Value::Object(o) => o.uuid.as_str(), _ => continue };
        if !s.object_store.contains_key(u) { continue; }
        if let Some(n) = s.global_names.get(i) { out.push((n.clone(), u.to_string())); }
    }
    out
}

/// Each of those variables names its object again.
fn name_objects(s: &mut Session, names: &[(String, String)]) {
    for (n, u) in names {
        let Some(i) = s.global_names.iter().position(|g| g == n) else { continue };
        let v = Value::Ref(crate::value::ObjId::from(u.as_str()));
        match s.globals.get(i).cloned().flatten() {
            Some(t) => { if let Ok(st) = s.get_stash_mut(&t) { st.value = v; } }
            None => { let t = s.alloc_value(v); s.set_global(i, t); }
        }
    }
}

// ── Overlay kinds, written once ─────────────────────────────────────────────

/// While a checkpoint is written: each distinct kind, written out, and its
/// place in the table (also by address, so a shared kind is written once).
#[derive(Default)]
struct KindsOut {
    blobs: Vec<Vec<u8>>,
    by_bytes: std::collections::HashMap<Vec<u8>, u32>,
    by_addr: std::collections::HashMap<*const POverlayDef, u32>,
}

thread_local! {
    static KINDS_OUT: std::cell::RefCell<Option<KindsOut>> = const { std::cell::RefCell::new(None) };
    static KINDS_IN: std::cell::RefCell<Vec<Arc<POverlayDef>>> = const { std::cell::RefCell::new(Vec::new()) };
}

fn kind_place(d: &POverlayDef) -> Result<u32, String> {
    let addr = d as *const POverlayDef;
    if let Some(n) = KINDS_OUT.with(|k| k.borrow().as_ref().and_then(|t| t.by_addr.get(&addr).copied())) { return Ok(n); }
    let bytes = bincode::serialize(d).map_err(|e| e.to_string())?;
    KINDS_OUT.with(|k| {
        let mut k = k.borrow_mut();
        let t = k.as_mut().ok_or_else(|| "an overlay kind outside a checkpoint".to_string())?;
        let n = match t.by_bytes.get(&bytes) {
            Some(n) => *n,
            None => {
                let n = t.blobs.len() as u32;
                t.blobs.push(bytes.clone());
                t.by_bytes.insert(bytes, n);
                n
            }
        };
        t.by_addr.insert(addr, n);
        Ok(n)
    })
}

fn kind_at(n: u32) -> Result<Arc<POverlayDef>, String> {
    KINDS_IN.with(|k| k.borrow().get(n as usize).cloned()).ok_or_else(|| format!("no overlay kind {n}"))
}

/// `Option<Arc<POverlayDef>>` as its place in the kinds table.
pub(crate) mod kind_opt {
    use super::*;
    pub fn serialize<S: serde::Serializer>(v: &Option<Arc<POverlayDef>>, s: S) -> Result<S::Ok, S::Error> {
        let n = match v { Some(d) => Some(kind_place(d).map_err(serde::ser::Error::custom)?), None => None };
        n.serialize(s)
    }
    pub fn deserialize<'de, D: serde::Deserializer<'de>>(d: D) -> Result<Option<Arc<POverlayDef>>, D::Error> {
        match Option::<u32>::deserialize(d)? {
            Some(n) => Ok(Some(kind_at(n).map_err(serde::de::Error::custom)?)),
            None => Ok(None),
        }
    }
}

/// `Vec<POverlayDef>` as places in the kinds table.
pub(crate) mod kind_vec {
    use super::*;
    pub fn serialize<S: serde::Serializer>(v: &[POverlayDef], s: S) -> Result<S::Ok, S::Error> {
        let ns = v.iter().map(kind_place).collect::<Result<Vec<u32>, String>>().map_err(serde::ser::Error::custom)?;
        ns.serialize(s)
    }
    pub fn deserialize<'de, D: serde::Deserializer<'de>>(d: D) -> Result<Vec<POverlayDef>, D::Error> {
        Vec::<u32>::deserialize(d)?.into_iter().map(|n| kind_at(n).map(|a| (*a).clone())).collect::<Result<Vec<_>, String>>().map_err(serde::de::Error::custom)
    }
}
