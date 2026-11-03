//! `:tick()` with regions: one tick of a world split into regions.
//!
//! A region is an actor that holds DES objects (`crate::actor::region_actors`).
//! `:tick()` in the program that started them runs
//! one DES tick in every region at the same time and returns when all have
//! finished. During a tick a region's rules see the other regions' objects
//! as they were at the end of the previous tick. What a region does to an
//! object of another region (a field a decision's action changes, an
//! overlay that spreads onto it) and what it posts to another region are
//! sent to the owner and applied at the start of the next tick, in a fixed
//! order: by sending region, then in the order sent. So a world gives the
//! same result on any number of threads.

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::{Arc, Mutex};

use indexmap::IndexMap;

use crate::des_image::{overlay_from_p, overlay_to_p, POverlay};
use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::value::Value;
use crate::vm::Vm;

/// A region's objects and overlays at the end of a tick.
#[derive(Debug, Default, Clone)]
pub struct Snapshot {
    /// Shared: an object unchanged since last tick is the same `Arc` as in
    /// last tick's snapshot (see `snapshot_of`), so who reads it can tell.
    objects: Vec<(String, Arc<PValue>)>,
    overlays: Vec<POverlay>,
}

/// A region's own object as it went into its last snapshot: reused while
/// the object is unchanged.
pub(crate) struct Sent {
    p: Arc<PValue>,
    class: std::rc::Rc<str>,
    readonly: std::rc::Rc<std::collections::BTreeSet<String>>,
    traits: std::rc::Rc<std::collections::BTreeSet<String>>,
}

/// A copy of another region's object, and the snapshot entry it was built
/// from: when the next snapshot has the same entry, the copy is current.
pub(crate) struct Copy {
    pub(crate) v: Value,
    from: Arc<PValue>,
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
    /// An overlay to put on; for the event log, the overlay it spread
    /// from (`flu on Town a`), or None when put on.
    Overlay(POverlay, Option<String>),
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
    /// To every region: this object was erased; what it owned there moves
    /// up to `up`, the object that owned it (none: nil). A region sees the
    /// others as of the end of last tick, so the erasing region cannot know
    /// everything that names the object as its owner.
    /// `owner`: the erased object's own owner, for an object it owned that
    /// is `up` itself (an absorber its target owned): that one goes to it.
    Erased { uuid: String, up: Option<String>, owner: Option<String> },
    /// To the program that runs the world: an absorb or merge of another
    /// region's object (see `transition::claim`).
    Claim(Claim),
    /// A claim the world granted, to the claimer's region: the target as it
    /// was at the end of the tick it was claimed, and its overlays.
    Take { claim: Claim, object: PValue, overlays: Vec<POverlay> },
    /// A claim the world granted, to the target's region: the target leaves.
    Retire { uuid: String, heir: String, links: bool },
    /// A retired object's own link offsets and link formulas, now `to`'s.
    Links { to: String, links: Links },
}

/// An absorb (`absorb`) or merge of `target`, another region's object, by
/// `by`, with transition `index` of class `class`. A merge's successor gets
/// `new_id`. `links`: the transition says `links: inherit`.
#[derive(Debug, Clone)]
pub struct Claim {
    pub absorb: bool,
    pub by: String,
    pub target: String,
    pub class: String,
    pub index: usize,
    pub new_id: Option<String>,
    pub links: bool,
}

/// Link offsets and an object's own link formulas, on their way to where
/// the object they now belong to runs.
#[derive(Debug, Clone, Default)]
pub struct Links {
    pub offsets: Vec<((String, String, String), Vec<crate::session::LinkOffset>)>,
    pub defs: Vec<((String, String), crate::session::LinkDef)>,
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
pub enum Dest { Object(String), Actor(u64), All, World }

/// What a region gets for one tick.
pub struct WorldIn {
    tick: i64,
    /// The region this is sent to.
    me: u64,
    /// Only report the objects (the first call, before any tick).
    pub(crate) prime: bool,
    /// One phase of the tick (a line of a `tick()` block), not its end.
    pub(crate) phase: Option<Arc<str>>,
    /// The event log is on: the region hands back its lines.
    log: bool,
    inbox: Vec<Item>,
    remote: Vec<Arc<Snapshot>>,
    /// The region each of `remote` belongs to.
    remote_ids: Vec<u64>,
    regions: Vec<u64>,
    out: Mutex<Option<WorldOut>>,
}

impl std::fmt::Debug for WorldIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<world tick {}>", self.tick)
    }
}

/// A set of object ids. Ids hash by their place, so checking an id already
/// in hand is cheap; a name in text is looked up first.
#[derive(Default, Clone)]
pub(crate) struct IdSet(HashSet<crate::value::ObjId>);

impl IdSet {
    pub(crate) fn contains(&self, u: &(impl crate::value::IdKey + ?Sized)) -> bool {
        !self.0.is_empty() && u.id().is_some_and(|id| self.0.contains(&*id))
    }
    pub(crate) fn insert(&mut self, u: crate::value::ObjId) -> bool { self.0.insert(u) }
    pub(crate) fn remove(&mut self, u: &(impl crate::value::IdKey + ?Sized)) -> bool {
        u.id().is_some_and(|id| self.0.remove(&*id))
    }
    pub(crate) fn is_empty(&self) -> bool { self.0.is_empty() }
}

/// What a region hands back after its tick.
pub struct WorldOut {
    snapshot: Arc<Snapshot>,
    sent: Vec<(Dest, Item)>,
    /// The region's numbers for `:metrics()`.
    counts: crate::metrics::Counts,
    /// Its objects that transitioned this tick: no claim can take them.
    transitioned: Vec<String>,
    /// Its event log lines.
    events: Vec<String>,
}

/// While a region runs a world tick: who else is in the world, which of
/// the objects it holds belong to other regions, and what it sends.
pub struct RegionCtx {
    pub(crate) regions: Vec<u64>,
    pub(crate) me: u64,
    pub(crate) remote: IdSet,
    /// The overlays those objects have: (overlay, host).
    pub(crate) remote_overlays: HashSet<(String, String)>,
    pub(crate) sent: Vec<(Dest, Item)>,
    /// The other regions' snapshots this tick and their regions, to find
    /// where an object runs (`:move!(x, y)`).
    pub(crate) homes: Vec<(u64, Arc<Snapshot>)>,
}

/// The main program's side: the regions of the current world, their last
/// snapshots, and what each gets at the start of the next tick.
#[derive(Default)]
pub struct World {
    /// The regions, in the order they joined.
    actors: Vec<Arc<crate::actor::ActorShared>>,
    ids: Vec<u64>,
    tick: i64,
    snaps: Vec<Arc<Snapshot>>,
    inbox: Vec<Vec<Item>>,
    /// The regions' objects as the main program sees them (copies in its
    /// object store, replaced after every tick; what the program writes to
    /// them goes to their regions, see `send_to_owner`).
    view: IdSet,
    /// What each of those copies was built from, with the fields it had
    /// then: a copy the program has not written to, whose entry is the same
    /// in the next snapshot, is current.
    view_from: HashMap<crate::value::ObjId, (Arc<PValue>, std::rc::Rc<crate::value::FieldMap>)>,
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
    /// The program's own objects `:move!` sends into a region: they go at
    /// the start of the next tick (object, index of the region).
    moving: Vec<(String, usize)>,
    /// Each region's numbers after its last tick, for `:metrics()`.
    counts: Vec<crate::metrics::Counts>,
}

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

// ── Main program ─────────────────────────────────────────────────────────────

/// `:tick()` in a program that runs regions: one tick of every region
/// (`crate::actor::region_actors`), together. None when it runs none.
///
/// The regions keep their order from tick to tick: one that starts holding
/// DES objects joins at the end (before a tick, or during one when a region
/// moves or posts an object to it), one that ended leaves.
///
/// With a phase: that phase of the tick in every region, together; the
/// tick goes on (its count moves when it ends).
pub(crate) fn tick_world(vm: &mut Vm, phase: Option<&str>) -> Result<Option<Value>, GoblinError> {
    let live = crate::actor::region_actors(&vm.session);
    if live.is_empty() {
        // No region left: the world is over, and the program ticks its own
        // objects again.
        if vm.session.world.is_some() { leave_world(vm)?; }
        return Ok(None);
    }
    let kept: Vec<Arc<crate::actor::ActorShared>> = match vm.session.world.as_ref() {
        Some(w) => w.actors.iter().filter(|a| live.iter().any(|b| Arc::ptr_eq(a, b))).cloned().collect(),
        None => Vec::new(),
    };
    let newcomers: Vec<_> = live.iter().filter(|a| !kept.iter().any(|b| Arc::ptr_eq(a, b))).cloned().collect();
    let ended = vm.session.world.as_ref().is_none_or(|w| w.actors.len() != kept.len());
    if ended {
        let actors: Vec<_> = kept.into_iter().chain(newcomers).collect();
        rebuild(vm, actors)?;
    } else if !newcomers.is_empty() {
        let outs = join(vm, newcomers)?;
        let w = vm.session.world.as_mut().unwrap();
        w.counts.extend(outs.iter().map(|o| o.counts.clone()));
        w.snaps.extend(outs.into_iter().map(|o| o.snapshot));
    }
    tick_regions(vm, phase.map(Arc::from)).map(Some)
}

/// Regions that hold objects now and are not in the world yet join it at
/// the end; each reports its objects first. Their reports are returned.
fn join(vm: &mut Vm, newcomers: Vec<Arc<crate::actor::ActorShared>>) -> Result<Vec<WorldOut>, GoblinError> {
    let ids: Vec<u64> = newcomers.iter().map(|a| crate::actor::actor_id(a)).collect();
    let outs = round(vm, &newcomers, &ids, true, None)?;
    let w = vm.session.world.as_mut().unwrap();
    for (a, id) in newcomers.into_iter().zip(ids) {
        w.actors.push(a);
        w.ids.push(id);
        w.inbox.push(Vec::new());
        w.flying.push(Arc::new(Snapshot::default()));
    }
    Ok(outs)
}

/// The last region ended: nothing may be on its way to one, and the
/// program's copies of the regions' objects go.
fn leave_world(vm: &mut Vm) -> Result<(), GoblinError> {
    rebuild_check(vm, &[])?;
    if let Some(old) = vm.session.world.take() {
        vm.session.object_store.retain(|k, _| !old.view.contains(k));
    }
    Ok(())
}

/// What is on its way to a region not in `ids` would be lost: an error.
fn rebuild_check(vm: &Vm, ids: &[u64]) -> Result<(), GoblinError> {
    let at = |id: &u64| ids.iter().position(|x| x == id);
    if let Some(old) = vm.session.world.as_ref() {
        for (k, id) in old.ids.iter().enumerate() {
            if at(id).is_none() && (old.inbox[k].iter().any(|it| matches!(it, Item::Arrive(_))) || old.moving.iter().any(|(_, j)| *j == k)) {
                return Err(err("C0123: tick-regions: an object is on its way to a region that has ended"));
            }
            if at(id).is_none() && !old.inbox[k].is_empty() {
                return Err(err("C0123: tick-regions: a change is on its way to a region that has ended"));
            }
        }
    }
    Ok(())
}

/// A new world of these regions (the first tick, or one ended): what is on
/// its way to a region still running goes there. An object on its way to a
/// region that ended would be lost, so that is an error.
fn rebuild(vm: &mut Vm, actors: Vec<Arc<crate::actor::ActorShared>>) -> Result<(), GoblinError> {
    let ids: Vec<u64> = actors.iter().map(|a| crate::actor::actor_id(a)).collect();
    rebuild_check(vm, &ids)?;
    let at = |id: &u64| ids.iter().position(|x| x == id);
    let mut inbox = vec![Vec::new(); ids.len()];
    let mut flying = vec![Arc::new(Snapshot::default()); ids.len()];
    let (mut in_flight, mut in_flight_overlays, mut moving, mut tick) = (Vec::new(), Vec::new(), Vec::new(), 0);
    if let Some(old) = vm.session.world.take() {
        tick = old.tick;
        for (u, k) in old.moving {
            if let Some(j) = at(&old.ids[k]) { moving.push((u, j)); }
        }
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
    vm.session.world = Some(World { actors: actors.clone(), ids: ids.clone(), tick, snaps: Vec::new(), inbox, view: IdSet::default(), view_from: HashMap::default(), in_flight, in_flight_overlays, flying, pending_overlays: Vec::new(), dropped_overlays: Vec::new(), moving, counts: Vec::new() });
    // Before the first tick, every region reports its objects, so the
    // first tick already sees the whole world.
    let outs = round(vm, &actors, &ids, true, None)?;
    let w = vm.session.world.as_mut().unwrap();
    w.counts = outs.iter().map(|o| o.counts.clone()).collect();
    w.snaps = outs.into_iter().map(|o| o.snapshot).collect();
    Ok(())
}

fn tick_regions(vm: &mut Vm, phase: Option<Arc<str>>) -> Result<Value, GoblinError> {
    let (actors, ids) = { let w = vm.session.world.as_ref().unwrap(); (w.actors.clone(), w.ids.clone()) };
    send_moving(vm)?;
    let mut outs = round(vm, &actors, &ids, false, phase.clone())?;
    // Moved or posted an object during the tick: it joins now, so what is
    // sent to it gets there.
    let late: Vec<_> = crate::actor::region_actors(&vm.session).into_iter().filter(|a| !actors.iter().any(|b| Arc::ptr_eq(a, b))).collect();
    let ids = if late.is_empty() { ids } else {
        outs.extend(join(vm, late)?);
        vm.session.world.as_ref().unwrap().ids.clone()
    };
    let w = vm.session.world.as_mut().unwrap();
    if phase.is_none() { w.tick += 1; }
    // Who owns what, after this tick.
    let mut owner: HashMap<&str, usize> = HashMap::default();
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
                    owner.insert(&*x.uuid, j);
                    in_flight.push((x.uuid.clone(), x.object.clone(), j));
                    in_flight_overlays.extend(x.overlays.iter().cloned());
                    flying[j].objects.push((x.uuid.clone(), Arc::new(x.object.clone())));
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
                Dest::All => {
                    for b in inbox.iter_mut() { b.push(item.clone()); }
                    None
                }
                Dest::World => None,
            };
            // An object that is gone by now (erased this tick) gets nothing.
            if let Some(i) = to { inbox[i].push(item.clone()); }
        }
    }
    // Absorbs and merges across regions, settled in order: an object is
    // taken (or takes) once a tick, and both must still be in the world.
    // An object a transition already changed in its own region this tick
    // can't be taken as well (one transition a tick).
    let mut taken: HashSet<&str> = outs.iter().flat_map(|o| o.transitioned.iter().map(String::as_str)).collect();
    let mut retired: Vec<(usize, String)> = Vec::new();
    // Objects already taken by a granted claim: they can't claim.
    let mut claimed_by: HashSet<&str> = HashSet::default();
    for o in &outs {
        for (dest, item) in &o.sent {
            let (Dest::World, Item::Claim(c)) = (dest, item) else { continue };
            let (Some(&ri), Some(&ti)) = (owner.get(c.by.as_str()), owner.get(c.target.as_str())) else { continue };
            // The claimer is among its region's transitioned (it claimed).
            if taken.contains(c.target.as_str()) || claimed_by.contains(c.by.as_str()) { continue; }
            let Some(object) = outs[ti].snapshot.objects.iter().find(|(u, _)| *u == c.target).map(|(_, p)| (**p).clone()) else { continue };
            taken.insert(c.by.as_str());
            taken.insert(c.target.as_str());
            claimed_by.insert(c.target.as_str());
            let overlays: Vec<POverlay> = outs[ti].snapshot.overlays.iter().filter(|x| x.host() == c.target).cloned().collect();
            let heir = if c.absorb { c.by.clone() } else { c.new_id.clone().unwrap_or_default() };
            let owner = match &object {
                PValue::Object { fields, .. } => fields.iter().find(|(k, _)| k == crate::store::OWNER_FIELD).and_then(|(_, v)| match v {
                    PValue::ObjRef(u) if *u != c.target => Some(u.clone()),
                    _ => None,
                }),
                _ => None,
            };
            inbox[ri].push(Item::Take { claim: c.clone(), object, overlays });
            inbox[ti].push(Item::Retire { uuid: c.target.clone(), heir: heir.clone(), links: c.links });
            for b in inbox.iter_mut() { b.push(Item::Erased { uuid: c.target.clone(), up: Some(heir.clone()), owner: owner.clone() }); }
            retired.push((ti, c.target.clone()));
        }
    }
    w.inbox = inbox;
    w.in_flight = in_flight;
    w.in_flight_overlays = in_flight_overlays;
    w.flying = flying.into_iter().map(Arc::new).collect();
    w.counts = outs.iter().map(|o| o.counts.clone()).collect();
    w.snaps = outs.into_iter().map(|o| o.snapshot).collect();
    // A retired target is out of the world from now: no region sees it
    // next tick, nor does the program.
    for (ti, uuid) in &retired {
        let s = &w.snaps[*ti];
        let objects = s.objects.iter().filter(|(u, _)| u != uuid).cloned().collect();
        let overlays = s.overlays.iter().filter(|x| x.host() != uuid).cloned().collect();
        w.snaps[*ti] = Arc::new(Snapshot { objects, overlays });
    }
    w.pending_overlays.clear();
    w.dropped_overlays.clear();
    let n = w.tick;
    refresh_view(vm);
    // What was erased this tick: the program sees its holdings moved up
    // now, as their regions will set them at the start of the next tick.
    let erased: Vec<(String, Option<String>, Option<String>)> = vm.session.world.as_ref().and_then(|w| w.inbox.first()).map(|b| b.iter().filter_map(|it| match it {
        Item::Erased { uuid, up, owner } => Some((uuid.clone(), up.clone(), owner.clone())),
        _ => None,
    }).collect()).unwrap_or_default();
    if !erased.is_empty() {
        let e: Vec<(&str, Option<&str>, Option<&str>)> = erased.iter().map(|(u, up, o)| (u.as_str(), up.as_deref(), o.as_deref())).collect();
        move_up_erased(&mut vm.session, &e);
    }
    Ok(Value::Int(n))
}

/// The main program's copies of the regions' objects, as of this tick, so
/// `:objects()`, `say` and reports read the whole world.
fn refresh_view(vm: &mut Vm) {
    let Some(w) = vm.session.world.as_mut() else { return };
    let old = std::mem::take(&mut w.view);
    let snaps = w.snaps.clone();
    let mut from = std::mem::take(&mut w.view_from);
    let mut built = HashMap::with_capacity_and_hasher(from.len(), Default::default());
    let flying: Vec<(String, PValue)> = w.in_flight.iter().map(|(u, p, _)| (u.clone(), p.clone())).collect();
    // Last tick's copies are brought up to date field by field (see
    // `refresh_copy`); they go back in snapshot order, as before.
    let mut copies: HashMap<crate::value::ObjId, Value> = HashMap::with_capacity_and_hasher(old.0.len(), Default::default());
    for u in &old.0 {
        if let Some(slot) = vm.session.object_store.get_mut(u) {
            copies.insert(u.clone(), std::mem::replace(slot, Value::Nil));
        }
    }
    vm.session.object_store.retain(|k, _| !old.contains(k));
    let mut fp = FromPortable::new();
    let mut view = IdSet::default();
    for s in &snaps {
        for (u, p) in &s.objects {
            let u = crate::value::ObjId::from(u.as_str());
            let last = from.remove(&u);
            let v = match copies.remove(&u) {
                Some(v) if last.as_ref().is_some_and(|(lp, lf)| Arc::ptr_eq(lp, p)
                    && matches!(&v, Value::Object(o) if std::rc::Rc::ptr_eq(&o.fields, lf))) => v,
                Some(mut v) => { drop(last); if refresh_copy(&mut v, p, &mut fp, &mut vm.session) { v } else { fp.value(p, &mut vm.session) } }
                None => fp.value(p, &mut vm.session),
            };
            if let Value::Object(o) = &v { built.insert(u.clone(), (p.clone(), o.fields.clone())); }
            vm.session.object_store.insert(u.clone(), v);
            view.insert(u);
        }
    }
    for (u, p) in &flying {
        let u = crate::value::ObjId::from(u.as_str());
        let v = match copies.remove(&u) {
            Some(mut v) => if refresh_copy(&mut v, p, &mut fp, &mut vm.session) { v } else { fp.value(p, &mut vm.session) },
            None => fp.value(p, &mut vm.session),
        };
        vm.session.object_store.insert(u.clone(), v);
        view.insert(u);
    }
    if let Some(w) = vm.session.world.as_mut() { w.view = view; w.view_from = built; }
}

/// Sends every region its part of one tick and waits for all of them.
fn round(vm: &mut Vm, actors: &[Arc<crate::actor::ActorShared>], ids: &[u64], prime: bool, phase: Option<Arc<str>>) -> Result<Vec<WorldOut>, GoblinError> {
    let w = vm.session.world.as_mut().unwrap();
    let tick = w.tick + 1;
    let log = vm.session.event_log.is_some();
    let w = vm.session.world.as_mut().unwrap();
    let mut sent = Vec::with_capacity(actors.len());
    for (i, a) in actors.iter().enumerate() {
        // The other regions' objects, and those on their way to them.
        let (remote, remote_ids): (Vec<Arc<Snapshot>>, Vec<u64>) = if prime { (Vec::new(), Vec::new()) } else {
            w.snaps.iter().enumerate().filter(|(j, _)| *j != i).map(|(j, s)| (s.clone(), ids[j]))
                .chain(w.flying.iter().enumerate()
                    .filter(|(j, f)| *j != i && !f.objects.is_empty()).map(|(j, f)| (f.clone(), ids[j])))
                .unzip()
        };
        let inbox = if prime { Vec::new() } else { std::mem::take(&mut w.inbox[i]) };
        let job = Arc::new(WorldIn { tick, me: ids[i], prime, phase: phase.clone(), log, inbox, remote, remote_ids, regions: ids.to_vec(), out: Mutex::new(None) });
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
        None => {
            // The regions' event lines, in region order.
            for o in outs.iter_mut() {
                let lines = std::mem::take(&mut o.events);
                crate::events::put_lines(&mut vm.session, lines)?;
            }
            Ok(outs)
        }
    }
}

/// The regions' overlays, for the main program's `:overlays()`.
pub(crate) fn view_overlays(vm: &mut Vm) -> Vec<crate::session::OverlayInstance> {
    view_overlays_in(&mut vm.session)
}

/// `view_overlays`, given the session.
pub(crate) fn view_overlays_in(s: &mut crate::session::Session) -> Vec<crate::session::OverlayInstance> {
    let Some(w) = s.world.as_ref() else { return Vec::new() };
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
    for snap in &snaps {
        for o in &snap.overlays { if !gone(o) { out.push(overlay_from_p(o, &mut fp, s)); } }
    }
    for o in &flying { if !gone(o) { out.push(overlay_from_p(o, &mut fp, s)); } }
    for o in &pending { if view.contains(o.host()) { out.push(overlay_from_p(o, &mut fp, s)); } }
    out
}

// ── Inside a region ──────────────────────────────────────────────────────────

/// One world tick inside a region: what other regions sent, then the DES
/// tick with the other regions' objects in view, then the region's handler
/// with `{"tick": n}`. Returns its snapshot and what it sends.
pub(crate) fn region_tick(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn) -> Result<(), GoblinError> {
    vm.session.region = Some(RegionCtx { regions: job.regions.clone(), me: job.me, remote: IdSet::default(), remote_overlays: HashSet::default(), sent: Vec::new(), homes: job.remote_ids.iter().copied().zip(job.remote.iter().cloned()).collect() });
    // The region's own event log, if one was started in it, stays its own.
    let own_log = if job.log { vm.session.event_log.replace(crate::events::Sink::Lines(Vec::new())) } else { None };
    let r = region_steps(vm, state, handler, job);
    let events = match job.log {
        true => match std::mem::replace(&mut vm.session.event_log, own_log) {
            Some(crate::events::Sink::Lines(v)) => v,
            _ => Vec::new(),
        },
        false => Vec::new(),
    };
    let ctx = vm.session.region.take();
    // Whatever happened, no other region's object stays here.
    if let Some(c) = &ctx {
        if !c.remote.is_empty() {
            vm.session.object_store.retain(|k, _| !c.remote.contains(k));
            vm.session.overlay_instances.retain(|i| !c.remote.contains(&i.host_uuid));
        }
    }
    r?;
    let sent = ctx.map(|c| c.sent).unwrap_or_default();
    let snapshot = snapshot_of(vm)?;
    let counts = crate::metrics::own_counts(&vm.session, false);
    let transitioned = std::mem::take(&mut vm.session.transitioned);
    *job.out.lock().unwrap_or_else(|e| e.into_inner()) = Some(WorldOut { snapshot: Arc::new(snapshot), sent, counts, transitioned, events });
    Ok(())
}

fn call_handler(vm: &mut Vm, state: &mut Value, handler: &Value, msg: Value) -> Result<(), GoblinError> {
    // `:actor(meadow)` has no handler: the region only runs what it owns.
    if matches!(handler, Value::Nil) { return Ok(()); }
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
    r
}

fn region_tick_steps(vm: &mut Vm, state: &mut Value, handler: &Value, job: &WorldIn, root: usize) -> Result<(), GoblinError> {
    let mut fp = FromPortable::new();
    // 1. What the other regions sent last tick: first the objects that move
    // here, then the rest in order.
    for item in &job.inbox {
        if let Item::Arrive(a) = item { arrive(vm, a, &mut fp); }
    }
    // What objects erased elsewhere owned here moves up before anything is
    // sent on, so an object moved this tick leaves with its new owner.
    let erased: Vec<(&str, Option<&str>, Option<&str>)> = job.inbox.iter().filter_map(|it| match it {
        Item::Erased { uuid, up, owner } => Some((uuid.as_str(), up.as_deref(), owner.as_deref())),
        _ => None,
    }).collect();
    if !erased.is_empty() { move_up_erased(&mut vm.session, &erased); }
    let mut takes: Vec<&Item> = Vec::new();
    let mut posts: Vec<&Item> = Vec::new();
    for item in &job.inbox {
        // An object that moved away earlier in this list (a message's
        // handler, or the main program, moved it): what was sent to it
        // follows it to its new region.
        let to_object = match item {
            Item::Change { uuid, .. } | Item::Erase { uuid } | Item::Retire { uuid, .. } => Some(uuid.as_str()),
            Item::Take { claim, .. } => Some(claim.by.as_str()),
            Item::Links { to, .. } => Some(to.as_str()),
            Item::Overlay(o, _) => Some(o.host()),
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
            Item::Arrive(_) | Item::Erased { .. } | Item::Claim(_) => {}
            // Once the other regions' objects are here (below).
            Item::Take { .. } => takes.push(item),
            Item::Retire { uuid, heir, links } => {
                let l = crate::transition::retire_claimed(vm, uuid, heir, *links);
                if !l.offsets.is_empty() || !l.defs.is_empty() {
                    if let Some(c) = vm.session.region.as_mut() {
                        c.sent.push((Dest::Object(heir.clone()), Item::Links { to: heir.clone(), links: l }));
                    }
                }
            }
            Item::Links { links, .. } => {
                for (k, v) in &links.offsets { vm.session.link_offsets.entry(k.clone()).or_default().extend(v.iter().cloned()); }
                for (k, d) in &links.defs { vm.session.object_link_defs.entry(k.clone()).or_insert_with(|| std::rc::Rc::new(d.clone())); }
            }
            Item::MoveTo { uuid, to } => {
                // Gone meanwhile (erased): nothing to move.
                if vm.session.object_store.contains_key(uuid) { send_one(&mut vm.session, uuid, *to)?; }
            }
            Item::Change { uuid, field, change } => apply_change(vm, uuid, field, change, &mut fp),
            Item::Erase { uuid } => {
                // Its `on erase:` runs here, where the object lives.
                if vm.session.object_store.contains_key(uuid) { crate::tick::run_on_erase(vm, uuid)?; }
                if vm.session.object_store.contains_key(uuid) {
                    let var = vm.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_default();
                    crate::builtins::erase_object(&mut vm.session, &var, uuid);
                }
            }
            Item::ClearOverlays { host } => {
                let var = vm.session.object_names.get(host).map(|(n, _)| n.clone()).unwrap_or_default();
                crate::builtins::drop_overlays_for(&mut vm.session, &var, host);
                crate::tick::settle_films(&mut vm.session);
            }
            Item::Detach { host, overlay } => {
                if crate::events::on(vm) {
                    let gone: Vec<String> = vm.session.overlay_instances.iter()
                        .filter(|i| i.host_uuid == *host && i.overlay_name == *overlay)
                        .map(|i| crate::events::overlay(vm, i)).collect();
                    for who in gone { crate::events::log(vm, who, "taken off", &[])?; }
                }
                vm.session.overlay_instances.retain(|i| !(i.host_uuid == *host && i.overlay_name == *overlay));
                crate::tick::settle_films(&mut vm.session);
            }
            Item::Overlay(o, from) => {
                if vm.session.object_store.contains_key(o.host()) {
                    let inst = overlay_from_p(o, &mut fp, &mut vm.session);
                    let how = match from { Some(f) => crate::tick::Put::Spread(f.clone()), None => crate::tick::Put::On };
                    crate::tick::overlay_put_as(vm, inst, true, how)?;
                }
            }
            Item::Reinforce { host, overlay, amount } => {
                if let Some(p) = vm.session.overlay_instances.find(host, overlay) {
                    let i = &mut vm.session.overlay_instances[p];
                    i.strength = (i.strength + amount).min(1.0);
                }
            }
            // Once the other regions' objects are here (below), so the
            // handler sees the whole world.
            Item::Post(_) => posts.push(item),
        }
    }
    // 2. The other regions' objects, as they were at the end of last tick.
    // Last tick's copy of an object is brought up to date field by field;
    // only an object that is new here (or changed shape) is built whole.
    // `before` keeps each copy's fields as they arrived: a change this tick
    // makes the object its own fields, so the two compare by pointer.
    let mut copies = std::mem::take(&mut vm.session.remote_copies);
    let mut before: Vec<(crate::value::ObjId, std::rc::Rc<crate::value::FieldMap>, Arc<PValue>)> = Vec::new();
    for snap in &job.remote {
        for (uuid, p) in &snap.objects {
            let uuid = crate::value::ObjId::from(uuid.as_str());
            if vm.session.object_store.contains_key(&uuid) { continue; }
            let v = match copies.remove(&uuid) {
                // The same entry as last tick: the copy is current.
                Some(c) if Arc::ptr_eq(&c.from, p) => c.v,
                Some(mut c) => if refresh_copy(&mut c.v, p, &mut fp, &mut vm.session) { c.v } else { fp.value(p, &mut vm.session) },
                None => fp.value(p, &mut vm.session),
            };
            if let Value::Object(o) = &v { before.push((uuid.clone(), o.fields.clone(), p.clone())); }
            vm.session.object_store.insert(uuid.clone(), v);
            if let Some(c) = vm.session.region.as_mut() { c.remote.insert(uuid); }
        }
        if let Some(c) = vm.session.region.as_mut() {
            for o in &snap.overlays {
                if c.remote.contains(o.host()) { c.remote_overlays.insert((o.name().to_string(), o.host().to_string())); }
            }
        }
    }
    // The copies are from the end of last tick: what they say an object
    // erased since owned has moved up in its region already, so the copies
    // say so too (their regions changed them; nothing to send back).
    let sent_owners = std::mem::take(&mut vm.session.owners_sent);
    if !erased.is_empty() || !sent_owners.is_empty() {
        let mut moved: Vec<String> = Vec::new();
        // First what this region set, as their regions did first.
        for (uuid, v) in &sent_owners {
            let is_copy = vm.session.region.as_ref().is_some_and(|c| c.remote.contains(uuid));
            if !is_copy { continue; }
            if let Some(Value::Object(o)) = vm.session.object_store.get_mut(uuid) {
                std::rc::Rc::make_mut(&mut o.fields).insert(crate::store::OWNER_FIELD.to_string(), v.clone());
            }
            vm.session.object_store.refile_owner(uuid);
            moved.push(uuid.to_string());
        }
        if !erased.is_empty() { moved.extend(move_up_erased(&mut vm.session, &erased)); }
        for (uuid, fields, _) in before.iter_mut() {
            if !moved.iter().any(|m| *m == *uuid.as_str()) { continue; }
            if let Some(Value::Object(o)) = vm.session.object_store.get(&*uuid) { *fields = o.fields.clone(); }
        }
    }
    check_owner_loops(&vm.session)?;
    // Granted claims: the claimer takes its target, which is here as it
    // was when claimed, for this alone.
    for item in takes {
        let Item::Take { claim, object, overlays } = item else { continue };
        if !vm.session.object_store.contains_key(claim.by.as_str()) { continue; }
        let target = crate::value::ObjId::from(claim.target.as_str());
        let v = fp.value(object, &mut vm.session);
        vm.session.object_store.insert(target.clone(), v);
        let list: Vec<crate::session::OverlayInstance> = overlays.iter().map(|o| overlay_from_p(o, &mut fp, &mut vm.session)).collect();
        let r = crate::transition::take_claimed(vm, claim, list);
        vm.session.object_store.shift_remove(&target);
        r?;
    }
    // What other regions posted, in the order sent. Its objects get new
    // ids, as in any message: the copy is independent of the sender's.
    for item in posts {
        let Item::Post(p) = item else { continue };
        let m = crate::actor::snapshot(p, &mut vm.session);
        let m = crate::actor::adopt_value(m, &mut vm.session);
        call_handler(vm, state, handler, m)?;
        vm.stack[root] = crate::vm::Operand::Val(state.clone());
    }
    // 3. The DES tick, numbered as the world's.
    vm.session.des_ticks = job.tick - 1;
    if let Some(p) = &job.phase {
        crate::tick::run_phase(vm, p)?;
    } else {
        crate::tick::run_tick(vm)?;
        // The region's own code for this tick, with the other regions in
        // view as its rules had them: what it does to their objects goes to
        // them.
        let mut m = IndexMap::new();
        m.insert("tick".to_string(), Value::Int(job.tick));
        call_handler(vm, state, handler, Value::MapOrd(Box::new(m)))?;
        vm.stack[root] = crate::vm::Operand::Val(state.clone());
    }
    // 4. What this tick did to other regions' objects goes to their owners.
    let mut changes = Vec::new();
    let mut owners = Vec::new();
    {
        let mut tp = ToPortable::new(&vm.session).objects_by_id();
        for (uuid, old, _) in &before {
            let Some(Value::Object(o)) = vm.session.object_store.get(uuid) else { continue };
            let uuid = uuid.to_string();
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
                if k == crate::store::OWNER_FIELD { owners.push((crate::value::ObjId::from(uuid.as_str()), new.clone())); }
                changes.push((Dest::Object(uuid.clone()), Item::Change { uuid: uuid.clone(), field: k.clone(), change }));
            }
        }
    }
    if let Some(c) = vm.session.region.as_mut() {
        c.sent.extend(changes);
        vm.session.owners_sent = owners;
        let remote = std::mem::take(&mut c.remote);
        // Keep the copies, with the fields they arrived with, for next tick.
        let mut kept = HashMap::with_capacity_and_hasher(before.len(), Default::default());
        for (uuid, old, from) in before {
            if let Some(slot) = vm.session.object_store.get_mut(&uuid) {
                let mut v = std::mem::replace(slot, Value::Nil);
                if let Value::Object(o) = &mut v { o.fields = old; }
                kept.insert(uuid, Copy { v, from });
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
    let (Value::Object(o), PValue::Object { class_name, fields, readonly_fields, trait_fields, uuid, .. }) = (v, p) else { return false };
    if *o.uuid != **uuid || *o.class_name != **class_name || o.fields.len() != fields.len()
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
        (PValue::ObjRef(a), Value::Ref(b)) => a == b,
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
fn snapshot_of(vm: &mut Vm) -> Result<Snapshot, GoblinError> {
    let mut last = std::mem::take(&mut vm.session.sent_objects);
    let mut now = HashMap::with_capacity_and_hasher(last.len(), Default::default());
    let s = &vm.session;
    let mut tp = ToPortable::new(s).objects_by_id();
    let mut objects = Vec::with_capacity(s.object_store.len());
    for (u, v) in &s.object_store {
        let Value::Object(o) = v else { continue };
        let p = match last.remove(u) {
            Some(b) if unchanged(&b, o, s) => b,
            _ => Sent { p: Arc::new(tp.value(v)?), class: o.class_name.clone(), readonly: o.readonly_fields.clone(), traits: o.trait_fields.clone() },
        };
        objects.push((u.to_string(), p.p.clone()));
        now.insert(u.clone(), p);
    }
    let mut overlays = Vec::with_capacity(s.overlay_instances.len());
    for i in &s.overlay_instances { overlays.push(overlay_to_p(i, &mut tp)?); }
    drop(tp);
    vm.session.sent_objects = now;
    Ok(Snapshot { objects, overlays })
}

/// Whether `o` would go into a snapshot as it did last time (`b`): the same
/// class and field names, and fields that are plain values or objects named
/// by id, all as they were. Anything else is converted again.
fn unchanged(b: &Sent, o: &crate::value::ObjectVal, s: &crate::session::Session) -> bool {
    use std::rc::Rc;
    let PValue::Object { fields, .. } = &*b.p else { return false };
    if !Rc::ptr_eq(&b.class, &o.class_name) || !Rc::ptr_eq(&b.readonly, &o.readonly_fields)
        || !Rc::ptr_eq(&b.traits, &o.trait_fields) || fields.len() != o.fields.len() {
        return false;
    }
    o.fields.iter().zip(fields).all(|((k, v), (pk, p))| k == pk && match (p, v) {
        (PValue::ObjRef(a), Value::Object(x)) => **a == *x.uuid,
        // Named by id while it is not handed off or moved away.
        (PValue::ObjRef(a), Value::Ref(u)) => a == u
            && !s.handed_off.contains_key(u.as_str()) && !s.moved_away.contains_key(u.as_str()),
        (_, Value::Ref(_)) => false,
        _ => same_plain(p, v),
    })
}

/// An overlay that lands on another region's object: sent to its owner
/// instead of applied here. Returns true when it was sent.
pub(crate) fn send_overlay(vm: &mut Vm, inst: &crate::session::OverlayInstance, from: Option<String>) -> bool {
    // The program that runs the world, on a region's object: the region
    // applies it next tick; the program sees it until then.
    if vm.session.region.is_none() && in_view(&vm.session, &inst.host_uuid) {
        let Ok(p) = overlay_to_p(inst, &mut ToPortable::new(&vm.session)) else { return false };
        if send_to_owner(&mut vm.session, &inst.host_uuid, Item::Overlay(p.clone(), from)) {
            if let Some(w) = vm.session.world.as_mut() { w.pending_overlays.push(p); }
            return true;
        }
        return false;
    }
    let remote = vm.session.region.as_ref().is_some_and(|c| c.remote.contains(&inst.host_uuid));
    if !remote { return false; }
    let p = { let mut tp = ToPortable::new(&vm.session); overlay_to_p(inst, &mut tp) };
    if let (Ok(p), Some(c)) = (p, vm.session.region.as_mut()) {
        c.sent.push((Dest::Object(inst.host_uuid.clone()), Item::Overlay(p, from)));
    }
    true
}

/// Whether another region's object, in view this tick, has this overlay.
/// In a region: other regions' overlays, as they were at the end of last
/// tick, on `host` (every one with None).
pub(crate) fn remote_overlays(s: &mut crate::session::Session, host: Option<&str>) -> Vec<crate::session::OverlayInstance> {
    let Some(c) = s.region.as_ref() else { return Vec::new() };
    let found: Vec<POverlay> = c.homes.iter()
        .flat_map(|(_, snap)| snap.overlays.iter())
        .filter(|o| host.is_none_or(|h| h == o.host()) && c.remote.contains(o.host()))
        .cloned().collect();
    if found.is_empty() { return Vec::new(); }
    let mut fp = FromPortable::new();
    found.iter().map(|o| overlay_from_p(o, &mut fp, s)).collect()
}

pub(crate) fn remote_has_overlay(vm: &Vm, overlay: &str, host: &str) -> bool {
    vm.session.region.as_ref().is_some_and(|c| c.remote_overlays.contains(&(overlay.to_string(), host.to_string())))
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
    let p = ToPortable::new(s).objects_by_id().value(v)?;
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

/// Sends an absorb or merge of another region's object to be settled when
/// the tick ends. False outside a region.
pub(crate) fn send_claim(s: &mut crate::session::Session, c: Claim) -> bool {
    let Some(r) = s.region.as_mut() else { return false };
    r.sent.push((Dest::World, Item::Claim(c)));
    true
}

/// For `:metrics()`: each region's numbers after its last tick, and the
/// objects and overlays on their way between regions. None outside a world.
pub(crate) fn region_counts(s: &crate::session::Session) -> Option<(&[crate::metrics::Counts], u64, u64)> {
    let w = s.world.as_ref()?;
    Some((&w.counts, w.in_flight.len() as u64, w.in_flight_overlays.len() as u64))
}

/// The world's tick count.
pub(crate) fn world_tick(s: &crate::session::Session) -> i64 {
    s.world.as_ref().map(|w| w.tick).unwrap_or(0)
}

/// Whether this object belongs to another region (in view during a tick).
/// How many regions the world this program runs has, when it runs one.
pub(crate) fn world_size(s: &crate::session::Session) -> Option<usize> {
    s.world.as_ref().map(|w| w.ids.len())
}

pub(crate) fn is_remote(vm: &Vm, uuid: &(impl crate::value::IdKey + ?Sized)) -> bool {
    vm.session.region.as_ref().is_some_and(|c| c.remote.contains(uuid))
        || vm.session.world.as_ref().is_some_and(|w| w.view.contains(uuid))
}

// ── Ownership across regions ─────────────────────────────────────────────────

/// Before `owner` of `uuid` is set to `v`: ownership is a tree, so an
/// object can't come to own itself.
pub(crate) fn check_owner_write(s: &crate::session::Session, uuid: &str, v: &Value) -> Result<(), GoblinError> {
    if !s.object_store.would_loop(uuid, v) { return Ok(()); }
    let name = |u: &str| match s.object_store.get(u) {
        Some(Value::Object(o)) => match o.fields.get("name") {
            Some(Value::Str(n)) if !n.is_empty() => format!("{} `{}`", o.class_name, n),
            _ => format!("a {}", o.class_name),
        },
        _ => "an object".to_string(),
    };
    Err(err(format!("R1246: owner-loop: {} can't be owned by that object: it already owns it, at some depth, and ownership is a tree", name(uuid))))
}

/// Before an object is erased: every region hears of it (`Item::Erased`),
/// so what it owns anywhere moves up to its owner. In a region, for its own
/// objects; in the program that runs the world, for the program's own.
pub(crate) fn note_erased(s: &mut crate::session::Session, uuid: &str) {
    note_removed(s, uuid, None)
}

/// As `note_erased`, for an object a transition retires: what it owned
/// goes to `heir` (a split's first, a merge's successor, the absorber)
/// rather than to its owner.
pub(crate) fn note_removed(s: &mut crate::session::Session, uuid: &str, heir: Option<&str>) {
    let up = match s.object_store.get(uuid) {
        Some(Value::Object(_)) if heir.is_some() => heir.map(|h| h.to_string()),
        Some(Value::Object(o)) => match o.fields.get(crate::store::OWNER_FIELD) {
            Some(Value::Ref(u)) if *u.as_str() != *uuid => Some(u.to_string()),
            Some(Value::Object(x)) if &*x.uuid != uuid => Some(x.uuid.to_string()),
            _ => None,
        },
        _ => return,
    };
    if let Some(c) = s.region.as_mut() {
        if c.remote.contains(uuid) { return; }
        c.sent.push((Dest::All, Item::Erased { uuid: uuid.to_string(), up, owner: None }));
        return;
    }
    if let Some(w) = s.world.as_mut() {
        if w.view.contains(uuid) { return; }
        for b in w.inbox.iter_mut() { b.push(Item::Erased { uuid: uuid.to_string(), up: up.clone(), owner: None }); }
    }
}

/// A region checks every owner write it makes, but two regions writing in
/// the same tick each see the other as it was: between them they can make
/// two objects own each other. This region's objects are checked once the
/// others' copies are here; a loop is an error.
fn check_owner_loops(s: &crate::session::Session) -> Result<(), GoblinError> {
    let remote = s.region.as_ref().map(|c| &c.remote);
    // 0 not seen, 1 on the current path, 2 done.
    let mut mark: HashMap<&str, u8> = HashMap::default();
    for start in s.object_store.keys() {
        if remote.is_some_and(|r| r.contains(start.as_str())) || mark.contains_key(start.as_str()) { continue; }
        let mut path: Vec<&str> = Vec::new();
        let mut at: &str = start.as_str();
        loop {
            match mark.get(at) {
                Some(1) => {
                    let i = path.iter().position(|p| *p == at).unwrap_or(0);
                    let loop_ids = &path[i..];
                    let name = |u: &str| match s.object_store.get(u) {
                        Some(Value::Object(o)) => match o.fields.get("name") {
                            Some(Value::Str(n)) if !n.is_empty() => format!("{} `{}`", o.class_name, n),
                            _ => format!("a {}", o.class_name),
                        },
                        _ => "an object".to_string(),
                    };
                    let names: Vec<String> = loop_ids.iter().map(|u| name(u)).collect();
                    return Err(err(format!(
                        "R1247: owner-loop-across-regions: {} own each other: regions changed their `owner`s in the same tick, each seeing the others as they were at the end of the last one", names.join(", "))));
                }
                Some(_) => break,
                None => {}
            }
            mark.insert(at, 1);
            path.push(at);
            match s.object_store.holder_of(at) {
                Some(h) => at = h.as_str(),
                None => break,
            }
        }
        for p in path { mark.insert(p, 2); }
    }
    Ok(())
}

/// This region's objects owned by an object erased elsewhere get that
/// object's owner (following the chain when its owner was erased too).
/// An object that is `up` itself (an absorber owned by its target) gets the
/// erased object's own owner instead.
fn move_up_erased(s: &mut crate::session::Session, erased: &[(&str, Option<&str>, Option<&str>)]) -> Vec<String> {
    let gone: HashMap<&str, Option<&str>> = erased.iter().map(|(u, up, _)| (*u, *up)).collect();
    let mut moved = Vec::new();
    for (uuid, _, own) in erased {
        let kids: Vec<String> = s.object_store.contents_of(*uuid).map(|k| k.to_string()).collect();
        if kids.is_empty() { continue; }
        let mut up = gone.get(uuid).copied().flatten();
        let mut steps = 0;
        while let Some(u) = up {
            match gone.get(u) {
                Some(next) if steps < gone.len() => { up = *next; steps += 1; }
                _ => break,
            }
        }
        let v = match up { Some(u) => Value::Ref(u.into()), None => Value::Nil };
        for k in kids {
            let v = if up == Some(k.as_str()) {
                match own { Some(o) => Value::Ref((*o).into()), None => Value::Nil }
            } else { v.clone() };
            if let Some(Value::Object(o)) = s.object_store.get_mut(k.as_str()) {
                std::rc::Rc::make_mut(&mut o.fields).insert(crate::store::OWNER_FIELD.to_string(), v);
            }
            s.object_store.refile_owner(k.as_str());
            s.touch(k.as_str());
            moved.push(k);
        }
    }
    moved
}

// ── Moving objects between regions ──────────────────────────────────────────

/// `:move!(obj, to)`: `obj` runs in another region from the start of the
/// next tick, with its overlays, links and decision. `to` is a region
/// actor, or an object, meaning the region that runs that object. Only the
/// object moves: what it owns, and what owns it, stay where they run
/// (`owner` is a relationship, not a place).
pub(crate) fn move_object(vm: &mut Vm, obj: &Value, to: &Value) -> Result<(), GoblinError> {
    let uuid = match obj {
        Value::Ref(u) => u.clone(),
        Value::Object(o) => o.uuid.clone(),
        other => return Err(err(format!("C0125: move-object: :move!(obj, to) moves an object; got {}", other.type_name()))),
    };
    let to_obj = match to {
        Value::Ref(u) => Some(u.to_string()),
        Value::Object(o) => Some(o.uuid.to_string()),
        _ => None,
    };
    let to_actor = crate::actor::actor_of(to).map(|a| crate::actor::actor_id(&a));
    if to_obj.is_none() && to_actor.is_none() {
        return Err(err(format!("C0125: move-object: :move!(obj, to) moves an object to a region actor, or to where another object runs; got {}", to.type_name())));
    }
    if let Some(c) = vm.session.region.as_ref() {
        let dest = match (&to_actor, &to_obj) {
            // Not a region yet: it is one now, and joins the world at the
            // end of this tick, in time for the object to arrive.
            (Some(a), _) => {
                if !c.regions.contains(a) {
                    let Some(x) = crate::actor::actor_of(to) else { unreachable!() };
                    if crate::actor::has_ended(&x) {
                        return Err(err("C0125: move-object: that actor has ended, so it cannot be a region"));
                    }
                    crate::actor::mark_region(&x);
                }
                *a
            }
            (None, Some(t)) => {
                if c.remote.contains(t.as_str()) {
                    match c.homes.iter().find(|(_, snap)| snap.objects.iter().any(|(u, _)| u == t)) {
                        Some((id, _)) => *id,
                        None => return Err(err("C0125: move-object: the object to move next to is in none of the regions")),
                    }
                } else if vm.session.object_store.contains_key(t.as_str()) {
                    c.me
                } else {
                    return Err(crate::actor::missing_object(&vm.session, t));
                }
            }
            _ => unreachable!(),
        };
        if c.remote.contains(uuid.as_str()) {
            return Err(err("C0125: move-object: this object belongs to another region; only the region that runs it can move it"));
        }
        if !vm.session.object_store.contains_key(&uuid) {
            return Err(crate::actor::missing_object(&vm.session, &uuid));
        }
        if dest == c.me { return Ok(()); }
        return send_one(&mut vm.session, &uuid, dest);
    }
    if vm.session.world.is_none() {
        return Err(err("C0125: move-object: :move!(obj, to) moves an object into a region; no :tick() has run regions here yet"));
    }
    // An actor that is not a region yet becomes one now and joins the world.
    if let Some(a) = crate::actor::actor_of(to) {
        if !vm.session.world.as_ref().unwrap().ids.contains(&crate::actor::actor_id(&a)) {
            if crate::actor::has_ended(&a) {
                return Err(err("C0125: move-object: that actor has ended, so it cannot be a region"));
            }
            crate::actor::mark_region(&a);
            let outs = join(vm, vec![a])?;
            let w = vm.session.world.as_mut().unwrap();
            w.counts.extend(outs.iter().map(|o| o.counts.clone()));
            w.snaps.extend(outs.into_iter().map(|o| o.snapshot));
        }
    }
    let w = vm.session.world.as_ref().unwrap();
    let j = match (&to_actor, &to_obj) {
        (Some(a), _) => match w.ids.iter().position(|id| id == a) {
            Some(j) => j,
            None => return Err(err("C0125: move-object: that actor is not a region of the last :tick() (it holds no DES objects)")),
        },
        (None, Some(t)) => match owner_index(w, t).or_else(|| w.moving.iter().find(|(u, _)| u == t).map(|(_, j)| *j)) {
            Some(j) => j,
            None => return Err(err("C0125: move-object: the object to move next to runs in none of the regions")),
        },
        _ => unreachable!(),
    };
    let to_id = w.ids[j];
    if w.view.contains(uuid.as_str()) {
        // A region's object: its region sends it at the start of the next tick.
        if let Some(i) = owner_index(w, &uuid) {
            if i != j { vm.session.world.as_mut().unwrap().inbox[i].push(Item::MoveTo { uuid: uuid.to_string(), to: to_id }); }
        }
        return Ok(());
    }
    // The program's own object: it goes to the region at the start of the
    // next tick; until then the program still has it.
    if !vm.session.object_store.contains_key(&uuid) {
        return Err(crate::actor::missing_object(&vm.session, &uuid));
    }
    let w = vm.session.world.as_mut().unwrap();
    w.moving.retain(|(u, _)| *u != *uuid);
    w.moving.push((uuid.to_string(), j));
    Ok(())
}

/// The program's own objects `:move!` sent into regions go there now; the
/// other regions see them there on the tick they arrive.
fn send_moving(vm: &mut Vm) -> Result<(), GoblinError> {
    let moving = match vm.session.world.as_mut() { Some(w) => std::mem::take(&mut w.moving), None => return Ok(()) };
    for (uuid, j) in moving {
        // Erased meanwhile: nothing to send.
        let Some(item) = take_for_region(vm, &uuid)? else { continue };
        let w = vm.session.world.as_mut().unwrap();
        if j >= w.inbox.len() { continue; }
        if let Item::Arrive(a) = &item {
            let f = Arc::make_mut(&mut w.flying[j]);
            f.objects.push((a.uuid.clone(), Arc::new(a.object.clone())));
            f.overlays.extend(a.overlays.iter().cloned());
        }
        w.inbox[j].push(item);
    }
    Ok(())
}

/// Takes an object of this region, and what goes along with it (its
/// overlays, links and decision), out of the session and sends it to
/// region `to`.
fn send_one(ss: &mut crate::session::Session, uuid: &str, to: u64) -> Result<(), GoblinError> {
    let s = &*ss;
    let Some(v) = s.object_store.get(uuid) else { return Ok(()) };
    let mut tp = ToPortable::new(s).objects_by_id();
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
    let s = ss;
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
    // Its fields read through its overlays' film already.
    if !a.overlays.is_empty() { crate::tick::adopt_films(&mut vm.session, &[a.uuid.to_string()], fp); }
}

/// Takes one of the program's own objects out of its session, with its
/// overlays, links and decision, as an arrival for a region.
fn take_for_region(vm: &mut Vm, uuid: &str) -> Result<Option<Item>, GoblinError> {
    let s = &vm.session;
    let Some(v) = s.object_store.get(uuid) else { return Ok(None) };
    let mut tp = ToPortable::new(s).objects_by_id();
    let object = tp.value(v)?;
    let mut overlays = Vec::new();
    for &p in s.overlay_instances.on_host(uuid) { overlays.push(overlay_to_p(&s.overlay_instances[p], &mut tp)?); }
    let name = s.object_names.get(uuid).cloned();
    let mut links: Vec<_> = s.link_offsets.iter().filter(|((from, _, _), _)| from == uuid).map(|(k, v)| (k.clone(), v.clone())).collect();
    links.sort_by(|a, b| a.0.cmp(&b.0));
    let mut link_defs: Vec<_> = s.object_link_defs.iter().filter(|((obj, _), _)| obj == uuid).map(|(k, v)| (k.clone(), (**v).clone())).collect();
    link_defs.sort_by(|a, b| a.0.cmp(&b.0));
    let decision = s.object_decisions.get(uuid).map(|d| (**d).clone());
    drop(tp);
    let s = &mut vm.session;
    s.object_store.shift_remove(uuid);
    let at = s.overlay_instances.on_host(uuid).to_vec();
    s.overlay_instances.take_at(&at);
    for (k, _) in &links { s.link_offsets.remove(k); }
    for (k, _) in &link_defs { s.object_link_defs.remove(k); }
    s.object_decisions.remove(uuid);
    Ok(Some(Item::Arrive(Box::new(Arrival { uuid: uuid.to_string(), object, overlays, name, links, link_defs, decision }))))
}
