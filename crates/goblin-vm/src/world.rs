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
    /// An object another region moved here (`:move!`). It is applied before
    /// anything else this tick, so what was sent to it finds it here.
    Arrive(Box<Arrival>),
    /// From the program that runs the world: move this object to region `to`.
    MoveTo { uuid: String, to: u64 },
    /// From the program that runs the world: erase this object
    /// (`delete_object!`), take every overlay off it (`delete_overlays_on!`),
    /// or take one overlay off it (`detach`).
    Erase { uuid: String },
    ClearOverlays { host: String },
    Detach { host: String, overlay: String },
}

/// An object on its way to another region, with what goes along with it:
/// its overlays, the name it was stored under, its link offsets and its own
/// decision.
#[derive(Debug, Clone)]
pub struct Arrival {
    uuid: String,
    object: PValue,
    overlays: Vec<POverlay>,
    name: Option<(String, bool)>,
    links: Vec<((String, String, String), Vec<crate::session::LinkOffset>)>,
    /// The object's own link formulas (`obj link trade by [ ... ]`).
    link_defs: Vec<((String, String), crate::session::LinkDef)>,
    decision: Option<goblin_ast::DecisionDef>,
}

#[derive(Debug, Clone)]
pub enum Dest { Object(String), Actor(u64) }

/// What a region gets for one tick.
pub struct WorldIn {
    tick: i64,
    /// The region this is sent to.
    me: u64,
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
    pub(crate) me: u64,
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
    /// The regions' objects as the main program sees them (copies in its
    /// object store, replaced after every tick; what the program writes to
    /// them goes to their regions, see `send_to_owner`).
    view: HashSet<String>,
    /// Objects moving between regions this tick: they are in no region's
    /// snapshot until they arrive, but the main program still sees them.
    /// (uuid, object, index of the region it goes to.)
    in_flight: Vec<(String, PValue, usize)>,
    /// The overlays those objects carry, for the main program's `:overlays()`.
    in_flight_overlays: Vec<POverlay>,
    /// The same objects by the region they go to: the other regions see
    /// them there on the tick they arrive.
    flying: Vec<Arc<Snapshot>>,
    /// Overlays the program put on the regions' objects since the last
    /// tick: their regions apply them next tick; the program sees them now.
    pending_overlays: Vec<POverlay>,
    /// Overlays the program took off the regions' objects since the last
    /// tick: (host, overlay; None for all of the host's). Their regions take
    /// them off next tick; the program no longer sees them.
    dropped_overlays: Vec<(String, Option<String>)>,
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
        // A new list (other regions, or the same in another order): what is
        // on its way to a region still in it goes there. An object on its
        // way to a region left out would be lost, so that is an error.
        let at = |id: &u64| ids.iter().position(|x| x == id);
        if let Some(old) = vm.session.world.as_ref() {
            for (k, id) in old.ids.iter().enumerate() {
                if at(id).is_none() && old.inbox[k].iter().any(|it| matches!(it, Item::Arrive(_))) {
                    return Err(err("C0123: tick-regions: an object is on its way to a region that is not in this list; tick the list it was moved in once more first"));
                }
                if at(id).is_none() && !old.inbox[k].is_empty() {
                    return Err(err("C0123: tick-regions: a change is on its way to a region that is not in this list; tick the list it was made in once more first"));
                }
            }
        }
        let mut inbox = vec![Vec::new(); ids.len()];
        let mut flying = vec![Arc::new(Snapshot::default()); ids.len()];
        let (mut in_flight, mut in_flight_overlays) = (Vec::new(), Vec::new());
        if let Some(old) = vm.session.world.take() {
            vm.session.object_store.retain(|k, _| !old.view.contains(k));
            for (k, items) in old.inbox.into_iter().enumerate() {
                if let Some(j) = at(&old.ids[k]) { inbox[j] = items; }
            }
            for (k, f) in old.flying.into_iter().enumerate() {
                if let Some(j) = at(&old.ids[k]) { flying[j] = f; }
            }
            for (u, p, k) in old.in_flight {
                if let Some(j) = at(&old.ids[k]) { in_flight.push((u, p, j)); }
            }
            in_flight_overlays = old.in_flight_overlays;
        }
        vm.session.world = Some(World { ids: ids.clone(), tick: 0, snaps: Vec::new(), inbox, view: HashSet::new(), in_flight, in_flight_overlays, flying, pending_overlays: Vec::new(), dropped_overlays: Vec::new() });
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
    // An object on its way somewhere belongs to where it goes: what is sent
    // to it this tick follows it there.
    let mut in_flight = Vec::new();
    let mut in_flight_overlays = Vec::new();
    let mut flying: Vec<Snapshot> = (0..ids.len()).map(|_| Snapshot::default()).collect();
    for o in &outs {
        for (dest, item) in &o.sent {
            if let (Dest::Actor(a), Item::Arrive(x)) = (dest, item) {
                if let Some(&j) = by_id.get(a) {
                    owner.insert(x.uuid.as_str(), j);
                    in_flight.push((x.uuid.clone(), x.object.clone(), j));
                    in_flight_overlays.extend(x.overlays.iter().cloned());
                    flying[j].objects.push((x.uuid.clone(), x.object.clone()));
                    flying[j].overlays.extend(x.overlays.iter().cloned());
                }
            }
        }
    }
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
    w.in_flight = in_flight;
    w.in_flight_overlays = in_flight_overlays;
    w.flying = flying.into_iter().map(Arc::new).collect();
    w.snaps = outs.into_iter().map(|o| o.snapshot).collect();
    w.pending_overlays.clear();
    w.dropped_overlays.clear();
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
    let flying: Vec<(String, PValue)> = w.in_flight.iter().map(|(u, p, _)| (u.clone(), p.clone())).collect();
    // Last tick's copies are brought up to date field by field (see
    // `refresh_copy`); they go back in snapshot order, as before.
    let mut copies: HashMap<String, Value> = HashMap::with_capacity(old.len());
    for u in &old {
        if let Some(slot) = vm.session.object_store.get_mut(u) {
            copies.insert(u.clone(), std::mem::replace(slot, Value::Nil));
        }
    }
    vm.session.object_store.retain(|k, _| !old.contains(k));
    let mut fp = FromPortable::new();
    let mut view = HashSet::new();
    for s in &snaps {
        for (u, p) in &s.objects {
            let v = match copies.remove(u) {
                Some(mut v) => if refresh_copy(&mut v, p, &mut fp, &mut vm.session) { v } else { fp.value(p, &mut vm.session) },
                None => fp.value(p, &mut vm.session),
            };
            vm.session.object_store.insert(u.clone(), v);
            view.insert(u.clone());
        }
    }
    for (u, p) in &flying {
        let v = match copies.remove(u) {
            Some(mut v) => if refresh_copy(&mut v, p, &mut fp, &mut vm.session) { v } else { fp.value(p, &mut vm.session) },
            None => fp.value(p, &mut vm.session),
        };
        vm.session.object_store.insert(u.clone(), v);
        view.insert(u.clone());
    }
    if let Some(w) = vm.session.world.as_mut() { w.view = view; }
}

/// Sends every region its part of one tick and waits for all of them.
fn round(vm: &mut Vm, actors: &[Arc<crate::actor::ActorShared>], ids: &[u64], prime: bool) -> Result<Vec<WorldOut>, GoblinError> {
    let w = vm.session.world.as_mut().unwrap();
    let tick = w.tick + 1;
    let mut sent = Vec::with_capacity(actors.len());
    for (i, a) in actors.iter().enumerate() {
        // The other regions' objects, and those on their way to them.
        let remote: Vec<Arc<Snapshot>> = if prime { Vec::new() } else {
            w.snaps.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, s)| s.clone())
                .chain(w.flying.iter().enumerate()
                    .filter(|(j, f)| *j != i && !f.objects.is_empty()).map(|(_, f)| f.clone()))
                .collect()
        };
        let inbox = if prime { Vec::new() } else { std::mem::take(&mut w.inbox[i]) };
        let job = Arc::new(WorldIn { tick, me: ids[i], prime, inbox, remote, regions: ids.to_vec(), out: Mutex::new(None) });
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
    let flying = w.in_flight_overlays.clone();
    // Put on since the last tick: newer than anything taken off (taking
    // one off removes it from this list).
    let pending = w.pending_overlays.clone();
    // What the program erased or took off since the last tick is gone here.
    let dropped = w.dropped_overlays.clone();
    let view = w.view.clone();
    let gone = |o: &POverlay| !view.contains(o.host())
        || dropped.iter().any(|(h, n)| h == o.host() && n.as_deref().is_none_or(|n| n == o.name()));
    for s in &snaps {
        for o in &s.overlays { if !gone(o) { out.push(overlay_from_p(o, &mut fp, &mut vm.session)); } }
    }
    for o in &flying { if !gone(o) { out.push(overlay_from_p(o, &mut fp, &mut vm.session)); } }
    for o in &pending { if view.contains(o.host()) { out.push(overlay_from_p(o, &mut fp, &mut vm.session)); } }
    out
}

// ── Inside a region ──────────────────────────────────────────────────────────

/// One world tick inside a region: what other regions sent, then the DES
/// tick with the other regions' objects in view, then the region's handler
/// with `{"tick": n}`. Returns its snapshot and what it sends.
pub(crate) fn region_tick(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn) -> Result<(), GoblinError> {
    vm.session.region = Some(RegionCtx { regions: job.regions.clone(), me: job.me, remote: HashSet::new(), remote_overlays: HashSet::new(), sent: Vec::new() });
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
    // 1. What the other regions sent last tick: first the objects that move
    // here, then the rest in order.
    for item in &job.inbox {
        if let Item::Arrive(a) = item { arrive(vm, a, &mut fp); }
    }
    for item in &job.inbox {
        // An object that moved away earlier in this list (a message's
        // handler, or the main program, moved it): what was sent to it
        // follows it to its new region.
        let to_object = match item {
            Item::Change { uuid, .. } | Item::Erase { uuid } => Some(uuid.as_str()),
            Item::Overlay(o) => Some(o.host()),
            Item::Reinforce { host, .. } | Item::ClearOverlays { host } | Item::Detach { host, .. } => Some(host.as_str()),
            _ => None,
        };
        if let Some(u) = to_object {
            if !vm.session.object_store.contains_key(u) && vm.session.moved_away.contains_key(u) {
                if let Some(c) = vm.session.region.as_mut() { c.sent.push((Dest::Object(u.to_string()), item.clone())); }
                continue;
            }
        }
        match item {
            Item::Arrive(_) => {}
            Item::MoveTo { uuid, to } => {
                // Gone meanwhile (erased): nothing to move.
                if vm.session.object_store.contains_key(uuid) { send_away(vm, uuid, *to)?; }
            }
            Item::Change { uuid, field, change } => apply_change(vm, uuid, field, change, &mut fp),
            Item::Erase { uuid } => {
                if vm.session.object_store.contains_key(uuid) {
                    let var = vm.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_default();
                    crate::builtins::erase_object(&mut vm.session, &var, uuid);
                }
            }
            Item::ClearOverlays { host } => {
                let var = vm.session.object_names.get(host).map(|(n, _)| n.clone()).unwrap_or_default();
                crate::builtins::drop_overlays_for(&mut vm.session, &var, host);
            }
            Item::Detach { host, overlay } => {
                vm.session.overlay_instances.retain(|i| !(i.host_uuid == *host && i.overlay_name == *overlay));
            }
            Item::Overlay(o) => {
                if vm.session.object_store.contains_key(o.host()) {
                    let inst = overlay_from_p(o, &mut fp, &mut vm.session);
                    crate::tick::overlay_push(vm, inst);
                }
            }
            Item::Reinforce { host, overlay, amount } => {
                if let Some(p) = vm.session.overlay_instances.find(host, overlay) {
                    let i = &mut vm.session.overlay_instances[p];
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
    // Last tick's copy of an object is brought up to date field by field;
    // only an object that is new here (or changed shape) is built whole.
    // `before` keeps each copy's fields as they arrived: a change this tick
    // makes the object its own fields, so the two compare by pointer.
    let mut copies = std::mem::take(&mut vm.session.remote_copies);
    let mut before: Vec<(String, std::rc::Rc<IndexMap<String, Value>>)> = Vec::new();
    for snap in &job.remote {
        for (uuid, p) in &snap.objects {
            if vm.session.object_store.contains_key(uuid) { continue; }
            let v = match copies.remove(uuid) {
                Some(mut v) => if refresh_copy(&mut v, p, &mut fp, &mut vm.session) { v } else { fp.value(p, &mut vm.session) },
                None => fp.value(p, &mut vm.session),
            };
            if let Value::Object(o) = &v { before.push((uuid.clone(), o.fields.clone())); }
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
            if std::rc::Rc::ptr_eq(&o.fields, old) { continue; }
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
        // Keep the copies, with the fields they arrived with, for next tick.
        let mut kept = std::collections::HashMap::with_capacity(before.len());
        for (uuid, old) in before {
            if let Some(slot) = vm.session.object_store.get_mut(&uuid) {
                let mut v = std::mem::replace(slot, Value::Nil);
                if let Value::Object(o) = &mut v { o.fields = old; }
                kept.insert(uuid, v);
            }
        }
        vm.session.object_store.retain(|k, _| !remote.contains(k));
        vm.session.overlay_instances.retain(|i| !remote.contains(&i.host_uuid));
        vm.session.remote_copies = kept;
    }
    Ok(())
}

/// Brings `v`, a copy built from an earlier snapshot, up to `p`: rebuilds
/// only the fields whose value changed. False when it differs in shape
/// (class, field names or order, readonly or trait fields): build it anew.
fn refresh_copy(v: &mut Value, p: &PValue, fp: &mut FromPortable, session: &mut crate::session::Session) -> bool {
    let (Value::Object(o), PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid }) = (v, p) else { return false };
    if o.uuid != *uuid || o.class_name != *class_name || o.fields.len() != fields.len()
        || *o.readonly_fields != *readonly_fields || *o.trait_fields != *trait_fields
        || o.fields.keys().zip(fields).any(|(k, (pk, _))| k != pk) {
        return false;
    }
    let mut stale: smallvec::SmallVec<[usize; 8]> = smallvec::SmallVec::new();
    for (i, ((_, now), (_, next))) in o.fields.iter().zip(fields).enumerate() {
        if !same_plain(next, now) { stale.push(i); }
    }
    if stale.is_empty() { return true; }
    let m = std::rc::Rc::make_mut(&mut o.fields);
    for i in stale {
        let nv = fp.value(&fields[i].1, session);
        if let Some((_, slot)) = m.get_index_mut(i) { *slot = nv; }
    }
    true
}

/// Whether a plain value (nil, bool, number, string) is the same in both.
/// Anything else counts as changed and is rebuilt.
fn same_plain(p: &PValue, v: &Value) -> bool {
    match (p, v) {
        (PValue::Nil, Value::Nil) => true,
        (PValue::Bool(a), Value::Bool(b)) => a == b,
        (PValue::Int(a), Value::Int(b)) => a == b,
        (PValue::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
        (PValue::Str(a), Value::Str(b)) => **a == *b.as_str(),
        _ => false,
    }
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
    if field == crate::store::OWNER_FIELD { vm.session.object_store.refile_owner(uuid); }
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
    // The program that runs the world, on a region's object: the region
    // applies it next tick; the program sees it until then.
    if vm.session.region.is_none() && in_view(&vm.session, &inst.host_uuid) {
        let Ok(p) = overlay_to_p(inst, &mut ToPortable::new(&vm.session)) else { return false };
        if send_to_owner(&mut vm.session, &inst.host_uuid, Item::Overlay(p.clone())) {
            if let Some(w) = vm.session.world.as_mut() { w.pending_overlays.push(p); }
            return true;
        }
        return false;
    }
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

// ── The program that runs the world, writing to the regions' objects ───────

/// Which region holds `uuid` (or will, for an object on its way there).
fn owner_index(w: &World, uuid: &str) -> Option<usize> {
    w.snaps.iter().position(|s| s.objects.iter().any(|(u, _)| u == uuid))
        .or_else(|| w.in_flight.iter().find(|(u, _, _)| u == uuid).map(|(_, _, j)| *j))
}

/// Whether the program that runs the world sees `uuid` as a region's object.
pub(crate) fn in_view(s: &crate::session::Session, uuid: &str) -> bool {
    s.region.is_none() && s.world.as_ref().is_some_and(|w| w.view.contains(uuid))
}

/// Sends `item` to the region that holds `uuid`, applied at the start of
/// the next tick. False when `uuid` is not a region's object here.
fn send_to_owner(s: &mut crate::session::Session, uuid: &str, item: Item) -> bool {
    // In a region: another region's object, in view this tick.
    if let Some(c) = s.region.as_mut() {
        if !c.remote.contains(uuid) { return false; }
        c.sent.push((Dest::Object(uuid.to_string()), item));
        return true;
    }
    let Some(w) = s.world.as_mut() else { return false };
    if !w.view.contains(uuid) { return false; }
    let Some(i) = owner_index(w, uuid) else { return false };
    w.inbox[i].push(item);
    true
}

/// After the program wrote `field` of a region's object: the field's new
/// value goes to the region, which sets it at the start of the next tick.
/// The program's copy already has it, so it reads back what it wrote.
pub(crate) fn after_view_write(s: &mut crate::session::Session, uuid: &str, field: &str) -> Result<(), GoblinError> {
    if !in_view(s, uuid) { return Ok(()); }
    let Some(Value::Object(o)) = s.object_store.get(uuid) else { return Ok(()) };
    let Some(v) = o.fields.get(field) else { return Ok(()) };
    let p = ToPortable::new(s).value(v)?;
    send_to_owner(s, uuid, Item::Change { uuid: uuid.to_string(), field: field.to_string(), change: Change::Set(p) });
    Ok(())
}

/// `delete_object!` on a region's object: its region erases it at the start
/// of the next tick; the program no longer sees it. False when `uuid` is
/// not a region's object here.
pub(crate) fn erase_in_region(s: &mut crate::session::Session, uuid: &str) -> bool {
    if !send_to_owner(s, uuid, Item::Erase { uuid: uuid.to_string() }) { return false; }
    s.object_store.shift_remove(uuid);
    let places = s.overlay_instances.on_host(uuid).to_vec();
    s.overlay_instances.take_at(&places);
    if let Some(w) = s.world.as_mut() {
        w.view.remove(uuid);
        w.pending_overlays.retain(|o| o.host() != uuid);
    }
    true
}

/// `delete_overlays_on!` on a region's object: its region takes them off at
/// the start of the next tick. False when `uuid` is not a region's object here.
pub(crate) fn clear_overlays_in_region(s: &mut crate::session::Session, uuid: &str) -> bool {
    if !send_to_owner(s, uuid, Item::ClearOverlays { host: uuid.to_string() }) { return false; }
    let places = s.overlay_instances.on_host(uuid).to_vec();
    s.overlay_instances.take_at(&places);
    if let Some(w) = s.world.as_mut() {
        w.pending_overlays.retain(|o| o.host() != uuid);
        w.dropped_overlays.push((uuid.to_string(), None));
    }
    true
}

/// `detach X from obj` on a region's object: its region takes it off at the
/// start of the next tick. False when `uuid` is not a region's object here.
pub(crate) fn detach_in_region(s: &mut crate::session::Session, uuid: &str, overlay: &str) -> bool {
    if !send_to_owner(s, uuid, Item::Detach { host: uuid.to_string(), overlay: overlay.to_string() }) { return false; }
    s.overlay_instances.retain(|i| !(i.host_uuid == uuid && i.overlay_name == overlay));
    if let Some(w) = s.world.as_mut() {
        w.pending_overlays.retain(|o| !(o.host() == uuid && o.name() == overlay));
        w.dropped_overlays.push((uuid.to_string(), Some(overlay.to_string())));
    }
    true
}

/// Whether this object belongs to another region (in view during a tick).
pub(crate) fn is_remote(vm: &Vm, uuid: &str) -> bool {
    vm.session.region.as_ref().is_some_and(|c| c.remote.contains(uuid))
        || vm.session.world.as_ref().is_some_and(|w| w.view.contains(uuid))
}

// ── Moving objects between regions ──────────────────────────────────────────

/// `:move!(obj, region)`. In a region: the object leaves now and lives in
/// `region` from the start of the next tick, with its overlays, links and
/// decision. In the program that runs `tick(regions)`: its region moves it
/// at the start of the next tick.
pub(crate) fn move_object(vm: &mut Vm, obj: &Value, region: &Value) -> Result<(), GoblinError> {
    let uuid = match obj {
        Value::Ref(u) => u.clone(),
        Value::Object(o) => o.uuid.clone(),
        other => return Err(err(format!("C0125: move-object: :move!(obj, region) moves an object; got {}", other.type_name()))),
    };
    let Some(to) = crate::actor::actor_of(region).map(|a| crate::actor::actor_id(&a)) else {
        return Err(err(format!("C0125: move-object: :move!(obj, region) moves an object to a region actor; got {}", region.type_name())));
    };
    if let Some(c) = vm.session.region.as_ref() {
        if !c.regions.contains(&to) {
            return Err(err("C0125: move-object: that actor is not one of the regions in this tick(regions)"));
        }
        if c.remote.contains(&uuid) {
            return Err(err("C0125: move-object: this object belongs to another region; only the region that holds it can move it"));
        }
        if !vm.session.object_store.contains_key(&uuid) {
            return Err(crate::actor::missing_object(&vm.session, &uuid));
        }
        if to == c.me { return Ok(()); }
        return send_away(vm, &uuid, to);
    }
    if let Some(w) = vm.session.world.as_mut() {
        let Some(j) = w.ids.iter().position(|id| *id == to) else {
            return Err(err("C0125: move-object: that actor is not one of the regions in the last tick(regions)"));
        };
        let owner = w.snaps.iter().position(|s| s.objects.iter().any(|(u, _)| *u == uuid))
            .or_else(|| w.in_flight.iter().find(|(u, _, _)| *u == uuid).map(|(_, _, j)| *j));
        let Some(i) = owner else {
            return Err(err("C0125: move-object: this object is in none of the regions"));
        };
        if i != j { w.inbox[i].push(Item::MoveTo { uuid, to }); }
        return Ok(());
    }
    Err(err("C0125: move-object: :move!(obj, region) moves an object between the regions of tick(regions); there are none here"))
}

/// Takes an object of this region, and what goes along with it, out of the
/// session and sends it to region `to`.
fn send_away(vm: &mut Vm, uuid: &str, to: u64) -> Result<(), GoblinError> {
    let s = &vm.session;
    let Some(v) = s.object_store.get(uuid) else { return Ok(()) };
    let mut tp = ToPortable::new(s);
    let object = tp.value(v)?;
    let mut overlays = Vec::new();
    for &p in s.overlay_instances.on_host(uuid) { overlays.push(overlay_to_p(&s.overlay_instances[p], &mut tp)?); }
    let name = s.object_names.get(uuid).cloned();
    // Its own links go with it (links go one way); links other objects
    // have to it stay with them.
    let mut links: Vec<_> = s.link_offsets.iter()
        .filter(|((from, _, _), _)| from == uuid)
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    let mut link_defs: Vec<_> = s.object_link_defs.iter()
        .filter(|((obj, _), _)| obj == uuid)
        .map(|(k, v)| (k.clone(), (**v).clone()))
        .collect();
    link_defs.sort_by(|a, b| a.0.cmp(&b.0));
    links.sort_by(|a, b| a.0.cmp(&b.0));
    let decision = s.object_decisions.get(uuid).map(|d| (**d).clone());
    drop(tp);
    let s = &mut vm.session;
    s.object_store.shift_remove(uuid);
    let places = s.overlay_instances.on_host(uuid).to_vec();
    s.overlay_instances.take_at(&places);
    for (k, _) in &links { s.link_offsets.remove(k); }
    for (k, _) in &link_defs { s.object_link_defs.remove(k); }
    s.object_decisions.remove(uuid);
    let var = name.as_ref().map(|(n, _)| n.clone()).unwrap_or_else(|| "this object".to_string());
    s.moved_away.insert(uuid.to_string(), var);
    let a = Arrival { uuid: uuid.to_string(), object, overlays, name, links, link_defs, decision };
    if let Some(c) = s.region.as_mut() { c.sent.push((Dest::Actor(to), Item::Arrive(Box::new(a)))); }
    Ok(())
}

/// An object moving here: it lives in this region from now on.
fn arrive(vm: &mut Vm, a: &Arrival, fp: &mut FromPortable) {
    let v = fp.value(&a.object, &mut vm.session);
    let s = &mut vm.session;
    s.object_store.insert(a.uuid.clone(), v);
    s.moved_away.remove(&a.uuid);
    if let Some(n) = &a.name { s.object_names.insert(a.uuid.clone(), n.clone()); }
    for (k, v) in &a.links { s.link_offsets.insert(k.clone(), v.clone()); }
    for (k, v) in &a.link_defs { s.object_link_defs.insert(k.clone(), std::rc::Rc::new(v.clone())); }
    if let Some(d) = &a.decision { s.object_decisions.insert(a.uuid.clone(), std::rc::Rc::new(d.clone())); }
    for o in &a.overlays {
        let inst = overlay_from_p(o, fp, &mut vm.session);
        vm.session.overlay_instances.push(inst);
    }
}
