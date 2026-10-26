//! `tick(regions)`: one tick of a world split into regions.
//!
//! A region is an actor whose state holds DES objects. `tick(regions)` runs
//! one DES tick in every region at the same time and returns when all have
//! finished. During a tick a region's rules see the other regions' objects
//! as they were at the end of the previous tick. What a region does to an
//! object of another region (a field a decision's action changes, an
//! overlay that spreads onto it) and what it posts to another region are
//! sent to the owner and applied at the start of the next tick, in a fixed
//! order: by sending region, then in the order sent. So a world gives the
//! same result on any number of threads.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

use indexmap::IndexMap;

use crate::des_image::{overlay_from_p, overlay_to_p, POverlay};
use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::value::Value;
use crate::vm::Vm;

/// A region's objects and overlays at the end of a tick.
#[derive(Debug, Default)]
pub struct Snapshot {
    objects: Vec<(String, PValue)>,
    overlays: Vec<POverlay>,
}

/// A change to one field of another region's object.
#[derive(Debug, Clone)]
pub enum Change {
    /// Numbers add up: what this region added to (or took from) the field.
    AddInt(i64),
    AddFloat(f64),
    /// Anything else: the value it set.
    Set(PValue),
}

/// Something one region sends another, applied at the start of next tick.
#[derive(Debug, Clone)]
pub enum Item {
    Change { uuid: String, field: String, change: Change },
    Overlay(POverlay),
    /// Spread onto an overlay another region's object already has: the
    /// strength it adds there.
    Reinforce { host: String, overlay: String, amount: f64 },
    Post(PValue),
}

#[derive(Debug, Clone)]
pub enum Dest { Object(String), Actor(u64) }

/// What a region gets for one tick.
pub struct WorldIn {
    tick: i64,
    /// Only report the objects (the first call, before any tick).
    pub(crate) prime: bool,
    inbox: Vec<Item>,
    remote: Vec<Arc<Snapshot>>,
    regions: Vec<u64>,
    out: Mutex<Option<WorldOut>>,
}

impl std::fmt::Debug for WorldIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<world tick {}>", self.tick)
    }
}

/// What a region hands back after its tick.
pub struct WorldOut {
    snapshot: Arc<Snapshot>,
    sent: Vec<(Dest, Item)>,
}

/// While a region runs a world tick: who else is in the world, which of
/// the objects it holds belong to other regions, and what it sends.
pub struct RegionCtx {
    pub(crate) regions: Vec<u64>,
    pub(crate) remote: HashSet<String>,
    /// The overlays those objects have: (overlay, host).
    pub(crate) remote_overlays: HashSet<(String, String)>,
    pub(crate) sent: Vec<(Dest, Item)>,
}

/// The main program's side: the regions of the current world, their last
/// snapshots, and what each gets at the start of the next tick.
#[derive(Default)]
pub struct World {
    ids: Vec<u64>,
    tick: i64,
    snaps: Vec<Arc<Snapshot>>,
    inbox: Vec<Vec<Item>>,
    /// The regions' objects as the main program sees them (read-only
    /// copies in its object store, replaced after every tick).
    view: HashSet<String>,
}

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

// ── Main program ─────────────────────────────────────────────────────────────

/// `tick(regions)`.
pub(crate) fn tick_regions(vm: &mut Vm, list: Value) -> Result<Value, GoblinError> {
    let items: Vec<Value> = match &list {
        Value::Array(a) => a.clone(),
        Value::Seq(s) => s.to_vec(),
        Value::Collection(c) => crate::collections::to_vec(c),
        other => return Err(err(format!(
            "C0123: tick-regions: tick(regions) takes a list of region actors, got {}", other.type_name()))),
    };
    let mut actors = Vec::with_capacity(items.len());
    for v in &items {
        match crate::actor::actor_of(v) {
            Some(a) => actors.push(a),
            None => return Err(err(format!(
                "C0123: tick-regions: tick(regions) takes a list of region actors; one of them is {}", v.type_name()))),
        }
    }
    let ids: Vec<u64> = actors.iter().map(|a| crate::actor::actor_id(a)).collect();
    if ids.iter().collect::<HashSet<_>>().len() != ids.len() {
        return Err(err("C0123: tick-regions: the same region is in the list twice"));
    }
    let fresh = vm.session.world.as_ref().is_none_or(|w| w.ids != ids);
    if fresh {
        if let Some(old) = vm.session.world.take() {
            vm.session.object_store.retain(|k, _| !old.view.contains(k));
        }
        vm.session.world = Some(World { ids: ids.clone(), tick: 0, snaps: Vec::new(), inbox: vec![Vec::new(); ids.len()], view: HashSet::new() });
        // Before the first tick, every region reports its objects, so the
        // first tick already sees the whole world.
        let outs = round(vm, &actors, &ids, true)?;
        let w = vm.session.world.as_mut().unwrap();
        w.snaps = outs.into_iter().map(|o| o.snapshot).collect();
    }
    let outs = round(vm, &actors, &ids, false)?;
    let w = vm.session.world.as_mut().unwrap();
    w.tick += 1;
    // Who owns what, after this tick.
    let mut owner: HashMap<&str, usize> = HashMap::new();
    for (i, o) in outs.iter().enumerate() {
        for (u, _) in &o.snapshot.objects { owner.insert(u.as_str(), i); }
    }
    let by_id: HashMap<u64, usize> = ids.iter().enumerate().map(|(i, id)| (*id, i)).collect();
    let mut inbox: Vec<Vec<Item>> = vec![Vec::new(); ids.len()];
    for o in &outs {
        for (dest, item) in &o.sent {
            let to = match dest {
                Dest::Object(u) => owner.get(u.as_str()).copied(),
                Dest::Actor(a) => by_id.get(a).copied(),
            };
            // An object that is gone by now (erased this tick) gets nothing.
            if let Some(i) = to { inbox[i].push(item.clone()); }
        }
    }
    w.inbox = inbox;
    w.snaps = outs.into_iter().map(|o| o.snapshot).collect();
    let n = w.tick;
    refresh_view(vm);
    Ok(Value::Int(n))
}

/// The main program's copies of the regions' objects, as of this tick, so
/// `:objects()`, `say` and reports read the whole world.
fn refresh_view(vm: &mut Vm) {
    let Some(w) = vm.session.world.as_mut() else { return };
    let old = std::mem::take(&mut w.view);
    let snaps = w.snaps.clone();
    vm.session.object_store.retain(|k, _| !old.contains(k));
    let mut fp = FromPortable::new();
    let mut view = HashSet::new();
    for s in &snaps {
        for (u, p) in &s.objects {
            let v = fp.value(p, &mut vm.session);
            vm.session.object_store.insert(u.clone(), v);
            view.insert(u.clone());
        }
    }
    if let Some(w) = vm.session.world.as_mut() { w.view = view; }
}

/// Sends every region its part of one tick and waits for all of them.
fn round(vm: &mut Vm, actors: &[Arc<crate::actor::ActorShared>], ids: &[u64], prime: bool) -> Result<Vec<WorldOut>, GoblinError> {
    let w = vm.session.world.as_mut().unwrap();
    let tick = w.tick + 1;
    let mut sent = Vec::with_capacity(actors.len());
    for (i, a) in actors.iter().enumerate() {
        let remote: Vec<Arc<Snapshot>> = if prime { Vec::new() } else {
            w.snaps.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, s)| s.clone()).collect()
        };
        let inbox = if prime { Vec::new() } else { std::mem::take(&mut w.inbox[i]) };
        let job = Arc::new(WorldIn { tick, prime, inbox, remote, regions: ids.to_vec(), out: Mutex::new(None) });
        let reply = crate::actor::send_world(a, job.clone())?;
        sent.push((job, reply));
    }
    // Every region finishes before anything else happens (the barrier).
    let mut outs = Vec::with_capacity(sent.len());
    let mut failed: Option<GoblinError> = None;
    for (job, reply) in sent {
        match crate::actor::wait_reply(reply, &mut vm.session) {
            Ok(()) => match job.out.lock().unwrap_or_else(|e| e.into_inner()).take() {
                Some(o) => outs.push(o),
                None => { failed.get_or_insert(err("C0123: tick-regions: a region gave no result")); }
            },
            Err(e) => { failed.get_or_insert(e); }
        }
    }
    match failed {
        Some(e) => Err(e),
        None => Ok(outs),
    }
}

/// The regions' overlays, for the main program's `:overlays()`.
pub(crate) fn view_overlays(vm: &mut Vm) -> Vec<crate::session::OverlayInstance> {
    let Some(w) = vm.session.world.as_ref() else { return Vec::new() };
    let snaps = w.snaps.clone();
    let mut fp = FromPortable::new();
    let mut out = Vec::new();
    for s in &snaps {
        for o in &s.overlays { out.push(overlay_from_p(o, &mut fp, &mut vm.session)); }
    }
    out
}

// ── Inside a region ──────────────────────────────────────────────────────────

/// One world tick inside a region: what other regions sent, then the DES
/// tick with the other regions' objects in view, then the region's handler
/// with `{"tick": n}`. Returns its snapshot and what it sends.
pub(crate) fn region_tick(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn) -> Result<(), GoblinError> {
    vm.session.region = Some(RegionCtx { regions: job.regions.clone(), remote: HashSet::new(), remote_overlays: HashSet::new(), sent: Vec::new() });
    let r = region_steps(vm, state, handler, job);
    let ctx = vm.session.region.take();
    // Whatever happened, no other region's object stays here.
    if let Some(c) = &ctx {
        if !c.remote.is_empty() {
            vm.session.object_store.retain(|k, _| !c.remote.contains(k));
            vm.session.overlay_instances.retain(|i| !c.remote.contains(&i.host_uuid));
        }
    }
    r?;
    let snapshot = snapshot_of(vm)?;
    let sent = ctx.map(|c| c.sent).unwrap_or_default();
    *job.out.lock().unwrap_or_else(|e| e.into_inner()) = Some(WorldOut { snapshot: Arc::new(snapshot), sent });
    Ok(())
}

fn call_handler(vm: &mut Vm, state: &mut Value, handler: &Value, msg: Value) -> Result<(), GoblinError> {
    let st = std::mem::replace(state, Value::Nil);
    let next = vm.call_callable(handler.clone(), vec![st, msg])?;
    if matches!(next, Value::Nil) {
        return Err(err("the handler returned nil; it must return the actor's next state"));
    }
    *state = crate::actor::adopt_value(next, &mut vm.session);
    Ok(())
}

fn region_steps(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn) -> Result<(), GoblinError> {
    if job.prime { return Ok(()); }
    // The state is held on the stack while the tick runs, so memory cleaned
    // up meanwhile (`:gc()`, automatic collection) keeps what it holds.
    let root = vm.stack.len();
    vm.stack.push(crate::vm::Operand::Val(state.clone()));
    let r = region_tick_steps(vm, state, handler, job, root);
    vm.stack.truncate(root);
    r?;
    // 5. The region's own code for this tick.
    let mut m = IndexMap::new();
    m.insert("tick".to_string(), Value::Int(job.tick));
    call_handler(vm, state, handler, Value::MapOrd(Box::new(m)))
}

fn region_tick_steps(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn, root: usize) -> Result<(), GoblinError> {
    let mut fp = FromPortable::new();
    // 1. What the other regions sent last tick, in order.
    for item in &job.inbox {
        match item {
            Item::Change { uuid, field, change } => apply_change(vm, uuid, field, change, &mut fp),
            Item::Overlay(o) => {
                if vm.session.object_store.contains_key(o.host()) {
                    let inst = overlay_from_p(o, &mut fp, &mut vm.session);
                    crate::tick::overlay_push(vm, inst);
                }
            }
            Item::Reinforce { host, overlay, amount } => {
                if let Some(i) = vm.session.overlay_instances.iter_mut()
                    .find(|i| &i.host_uuid == host && &i.overlay_name == overlay) {
                    i.strength = (i.strength + amount).min(1.0);
                }
            }
            Item::Post(p) => {
                // Its objects get new ids, as in any message: the copy is
                // independent of the sender's objects.
                let m = crate::actor::snapshot(p, &mut vm.session);
                let m = crate::actor::adopt_value(m, &mut vm.session);
                call_handler(vm, state, handler, m)?;
                vm.stack[root] = crate::vm::Operand::Val(state.clone());
            }
        }
    }
    // 2. The other regions' objects, as they were at the end of last tick.
    let mut before: Vec<(String, IndexMap<String, Value>)> = Vec::new();
    for snap in &job.remote {
        for (uuid, p) in &snap.objects {
            if vm.session.object_store.contains_key(uuid) { continue; }
            let v = fp.value(p, &mut vm.session);
            if let Value::Object(o) = &v { before.push((uuid.clone(), (*o.fields).clone())); }
            vm.session.object_store.insert(uuid.clone(), v);
            if let Some(c) = vm.session.region.as_mut() { c.remote.insert(uuid.clone()); }
        }
        if let Some(c) = vm.session.region.as_mut() {
            for o in &snap.overlays {
                if c.remote.contains(o.host()) { c.remote_overlays.insert((o.name().to_string(), o.host().to_string())); }
            }
        }
    }
    // 3. The DES tick, numbered as the world's.
    vm.session.des_ticks = job.tick - 1;
    crate::tick::run_tick(vm)?;
    // 4. What this tick did to other regions' objects goes to their owners.
    let mut changes = Vec::new();
    {
        let mut tp = ToPortable::new(&vm.session);
        for (uuid, old) in &before {
            let Some(Value::Object(o)) = vm.session.object_store.get(uuid) else { continue };
            for (k, new) in o.fields.iter() {
                let change = match (old.get(k), new) {
                    (Some(a), b) if a == b => continue,
                    (Some(Value::Int(a)), Value::Int(b)) => Change::AddInt(b.wrapping_sub(*a)),
                    (Some(Value::Int(a)), Value::Float(b)) => Change::AddFloat(b - *a as f64),
                    (Some(Value::Float(a)), Value::Float(b)) => Change::AddFloat(b - a),
                    (Some(Value::Float(a)), Value::Int(b)) => Change::AddFloat(*b as f64 - a),
                    (_, b) => Change::Set(tp.value(b)?),
                };
                changes.push((Dest::Object(uuid.clone()), Item::Change { uuid: uuid.clone(), field: k.clone(), change }));
            }
        }
    }
    if let Some(c) = vm.session.region.as_mut() {
        c.sent.extend(changes);
        let remote = std::mem::take(&mut c.remote);
        vm.session.object_store.retain(|k, _| !remote.contains(k));
        vm.session.overlay_instances.retain(|i| !remote.contains(&i.host_uuid));
    }
    Ok(())
}

fn apply_change(vm: &mut Vm, uuid: &str, field: &str, change: &Change, fp: &mut FromPortable) {
    let new = match change {
        Change::Set(p) => Some(fp.value(p, &mut vm.session)),
        _ => None,
    };
    let Some(Value::Object(o)) = vm.session.object_store.get_mut(uuid) else { return };
    let is_trait = o.trait_fields.contains(field);
    let fields = std::rc::Rc::make_mut(&mut o.fields);
    let Some(cur) = fields.get_mut(field) else { return };
    let next = match (change, &*cur) {
        (Change::AddInt(d), Value::Int(x)) => Value::Int(x.wrapping_add(*d)),
        (Change::AddInt(d), Value::Float(x)) => Value::Float(x + *d as f64),
        (Change::AddFloat(d), Value::Int(x)) => Value::Float(*x as f64 + d),
        (Change::AddFloat(d), Value::Float(x)) => Value::Float(x + d),
        (Change::Set(_), _) => new.unwrap(),
        // A number change to a field that is no longer a number: ignored.
        _ => return,
    };
    // A trait stays in 0..1, as it does for a write in the region itself.
    *cur = match next {
        Value::Float(f) if is_trait => Value::Float(f.clamp(0.0, 1.0)),
        Value::Int(i) if is_trait => Value::Float((i as f64).clamp(0.0, 1.0)),
        other => other,
    };
}

/// The region's own objects and overlays, ready for the other regions.
fn snapshot_of(vm: &Vm) -> Result<Snapshot, GoblinError> {
    let mut tp = ToPortable::new(&vm.session);
    let mut objects = Vec::with_capacity(vm.session.object_store.len());
    for (u, v) in &vm.session.object_store {
        if matches!(v, Value::Object(_)) { objects.push((u.clone(), tp.value(v)?)); }
    }
    let mut overlays = Vec::with_capacity(vm.session.overlay_instances.len());
    for i in &vm.session.overlay_instances { overlays.push(overlay_to_p(i, &mut tp)?); }
    Ok(Snapshot { objects, overlays })
}

/// An overlay that lands on another region's object: sent to its owner
/// instead of applied here. Returns true when it was sent.
pub(crate) fn send_overlay(vm: &mut Vm, inst: &crate::session::OverlayInstance) -> bool {
    let remote = vm.session.region.as_ref().is_some_and(|c| c.remote.contains(&inst.host_uuid));
    if !remote { return false; }
    let p = { let mut tp = ToPortable::new(&vm.session); overlay_to_p(inst, &mut tp) };
    if let (Ok(p), Some(c)) = (p, vm.session.region.as_mut()) {
        c.sent.push((Dest::Object(inst.host_uuid.clone()), Item::Overlay(p)));
    }
    true
}

/// Spread onto another region's object that already has this overlay:
/// the owner adds `amount` to it next tick. Returns true when sent.
pub(crate) fn reinforce_remote(vm: &mut Vm, overlay: &str, host: &str, amount: f64) -> bool {
    let Some(c) = vm.session.region.as_mut() else { return false };
    if !c.remote_overlays.contains(&(overlay.to_string(), host.to_string())) { return false; }
    c.sent.push((Dest::Object(host.to_string()),
        Item::Reinforce { host: host.to_string(), overlay: overlay.to_string(), amount }));
    true
}

/// The main program sees the regions' objects as of the last tick and
/// cannot change them: a change would be lost at the next tick (C0124).
pub(crate) fn check_view_write(s: &crate::session::Session, uuid: &str) -> Result<(), GoblinError> {
    if s.world.as_ref().is_some_and(|w| w.view.contains(uuid)) {
        return Err(err("C0124: region-object-read-only: this object belongs to a region; the program that runs tick(regions) sees it as of the last tick and cannot change it; send the region a message to change it"));
    }
    Ok(())
}

/// Whether this object belongs to another region (in view during a tick).
pub(crate) fn is_remote(vm: &Vm, uuid: &str) -> bool {
    vm.session.region.as_ref().is_some_and(|c| c.remote.contains(uuid))
        || vm.session.world.as_ref().is_some_and(|w| w.view.contains(uuid))
}
