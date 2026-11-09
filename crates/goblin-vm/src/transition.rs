//! The eight DES transitions, for class objects and for overlays.
//!
//! One transition, in order: its condition is checked; its successors are
//! built whole (fields copied, then its field lists); what the old object
//! owns, its overlays and its links move; removed objects leave the world;
//! its hook (`on split => ...`) runs, with the removed object still
//! readable and its successors in place.

use goblin_ast::{LinkContinuity, OverlayContinuity, TransitionDef, TransitionKind as K};
use rustc_hash::FxHashSet;
use std::rc::Rc;

use crate::error::GoblinError;
use crate::session::{OverlayDef, OverlayInstance, OverlayInstanceId};
use crate::value::{FieldMap, ObjId, ObjectVal, Value};
use crate::vm::Vm;

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

fn alone(k: &K) -> bool { !matches!(k, K::Merge | K::Absorb | K::Subjugate) }

fn word(k: &K) -> &'static str {
    match k {
        K::Spawn => "spawn", K::Erase => "erase", K::Split => "split", K::Fracture => "fracture",
        K::Merge => "merge", K::Absorb => "absorb", K::Subjugate => "subjugate", K::Mutate => "mutate",
    }
}

fn fields_of<'a>(t: &'a TransitionDef, label: &str) -> &'a [(String, goblin_ast::Expr)] {
    t.successors.iter().find(|s| s.label == label).map(|s| s.fields.as_slice()).unwrap_or(&[])
}

/// A value as a field stores it: an object in the store by reference.
fn entry(vm: &Vm, v: Value) -> Value {
    match &v {
        Value::Object(o) if !o.uuid.starts_with(crate::vm::OVERLAY_RECORD) && vm.session.object_store.contains_key(&*o.uuid) =>
            Value::Ref(o.uuid.clone()),
        _ => v,
    }
}

// ── Class objects ───────────────────────────────────────────────────────────

/// Every object's transitions for this tick. An object fires at most one;
/// the ones on an object alone go first, then the ones with a target. A
/// target has not transitioned yet and was here when the transitions
/// began: an object of this region, or for subjugate any region's.
pub(crate) fn object_transitions(vm: &mut Vm) -> Result<(), GoblinError> {
    let classes = vm.session.classes.clone();
    let candidates: Vec<(ObjId, String)> = {
        let store = &vm.session.object_store;
        store.places_of(|c| classes.get(c).is_some_and(|c| !c.transitions.is_empty()))
            .into_iter()
            .filter_map(|i| match store.get_index(i) {
                Some((uuid, Value::Object(o))) if !crate::world::is_remote(vm, uuid) => Some((uuid.clone(), o.class_name[..].to_string())),
                _ => None,
            }).collect()
    };
    if candidates.is_empty() { return Ok(()); }
    let binary = candidates.iter().any(|(_, c)| classes.get(c).is_some_and(|d| d.transitions.iter().any(|t| !alone(&t.kind))));
    let started: FxHashSet<String> = if binary {
        vm.session.object_store.keys().filter(|k| !crate::world::is_remote(vm, *k)).map(|k| k.to_string()).collect()
    } else { Default::default() };
    let mut done: FxHashSet<String> = Default::default();
    let mut clock = crate::metrics::LayerClock::default();
    for pass in 0..2 {
        for (uuid, class) in &candidates {
            if done.contains(uuid.as_str()) || !vm.session.object_store.contains_key(uuid) { continue; }
            let Some(decl) = classes.get(class) else { continue };
            clock.at(&mut vm.session.des_metrics, class);
            for t in &decl.transitions {
                if alone(&t.kind) != (pass == 0) { continue; }
                let target = if alone(&t.kind) {
                    if !vm.eval_tick_expr_bool(&t.trigger, vec![("self", Value::Ref(uuid.clone()))]) { continue; }
                    None
                } else {
                    match find_target(vm, uuid, t, &started, &done) { Some(x) => Some(x), None => continue }
                };
                done.insert(uuid.to_string());
                if let Some(x) = &target { done.insert(x.clone()); }
                fire(vm, uuid.as_str(), class, t, target.as_deref())?;
                break;
            }
        }
    }
    clock.stop(&mut vm.session.des_metrics);
    // In a region: its objects that transitioned (or were taken) this
    // tick, so another region's claim on one of them is turned down.
    if vm.session.region.is_some() {
        let mine: Vec<String> = done.into_iter().filter(|u| !crate::world::is_remote(vm, u.as_str())).collect();
        vm.session.transitioned = mine;
    }
    Ok(())
}

fn find_target(vm: &mut Vm, me: &ObjId, t: &TransitionDef, started: &FxHashSet<String>, done: &FxHashSet<String>) -> Option<String> {
    let class = t.target_class.as_deref()?;
    let ids: Vec<ObjId> = if class.starts_with("uses ") {
        let store = &vm.session.object_store;
        vm.session.places_of_class(class).into_iter().filter_map(|i| store.get_index(i).map(|(u, _)| u.clone())).collect()
    } else {
        vm.session.object_store.of_class_ids(class).cloned().collect()
    };
    // `targets: Kind where (target >> realm) == @realm and ...`: only the
    // objects that can pass the first test on `target` are tried, in the
    // same order (see `tick::narrow`).
    let ids: Vec<ObjId> = match vm.session.object_store.get_index_of(me.as_str()) {
        Some(me_at) if ids.len() > 1 && vm.session.region.is_none() => {
            let all: Vec<(Value, usize)> = ids.iter()
                .filter_map(|u| vm.session.object_store.get_index_of(u.as_str()).map(|i| (Value::Ref(u.clone()), i)))
                .collect();
            match crate::tick::narrow(vm, &t.trigger, false, class, &Value::Ref(me.clone()), me_at, &all) {
                Some(fewer) => {
                    let keep: FxHashSet<usize> = fewer.iter().map(|(_, i)| *i).collect();
                    all.into_iter().filter(|(_, i)| keep.contains(i))
                        .filter_map(|(v, _)| if let Value::Ref(u) = v { Some(u) } else { None }).collect()
                }
                None => ids,
            }
        }
        _ => ids,
    };
    // A target with a partner may run in another region: subjugate's change
    // goes there; an absorb or merge of it is a claim the world settles
    // (see `claim`).
    let anywhere = matches!(t.kind, K::Subjugate | K::Absorb | K::Merge);
    for id in ids {
        if id == *me || done.contains(id.as_str()) { continue; }
        if crate::world::is_remote(vm, &id) {
            if !anywhere { continue; }
        } else if !started.contains(id.as_str()) { continue; }
        if vm.eval_tick_expr_bool(&t.trigger, vec![("self", Value::Ref(me.clone())), ("target", Value::Ref(id.clone()))]) {
            return Some(id.to_string());
        }
    }
    None
}

fn fire(vm: &mut Vm, me: &str, class: &str, t: &TransitionDef, target: Option<&str>) -> Result<(), GoblinError> {
    let into = |i: usize| -> String {
        t.into_classes.get(i).or(t.into_classes.first()).cloned().unwrap_or_else(|| class.to_string())
    };
    let r = |u: &str| Value::Ref(u.into());
    match t.kind {
        K::Erase => {
            let on_erase = vm.session.classes.get(class).and_then(|c| c.on_erase.clone());
            crate::tick::erase_object(vm, me, on_erase.as_deref())
        }
        K::Spawn => {
            let child_class = into(0);
            let mut fields = defaults(vm, &child_class)?;
            let set = overrides(vm, fields_of(t, "child"), me, target)?;
            for (k, v) in set { fields.insert(k, v); }
            let child = make_object(vm, &child_class, fields)?;
            hook(vm, class, K::Spawn, r(me), vec![r(&child)])
        }
        K::Mutate => {
            let new_class = into(0);
            let set = overrides(vm, fields_of(t, "carries"), me, target)?;
            if new_class != class {
                let mut fields = copy_fields(vm, me, &new_class)?;
                for (k, v) in set { fields.insert(k, v); }
                recast(vm, me, &new_class, fields)?;
            } else {
                for (k, v) in set { write_field(vm, me, &k, v)?; }
            }
            hook(vm, class, K::Mutate, r(me), Vec::new())
        }
        K::Split => {
            let (ca, cb) = (into(0), into(1));
            let mut fa = copy_fields(vm, me, &ca)?;
            for (k, v) in overrides(vm, fields_of(t, "first"), me, target)? { fa.insert(k, v); }
            let mut fb = copy_fields(vm, me, &cb)?;
            for (k, v) in overrides(vm, fields_of(t, "second"), me, target)? { fb.insert(k, v); }
            let first = make_object(vm, &ca, fa)?;
            let second = make_object(vm, &cb, fb)?;
            move_contents(vm, me, &first)?;
            match t.overlay_rule {
                OverlayContinuity::Split => { copy_overlays(vm, me, &first)?; copy_overlays(vm, me, &second)?; }
                OverlayContinuity::Transfer => copy_overlays(vm, me, &first)?,
                OverlayContinuity::Drop => {}
            }
            if matches!(t.link_rule, LinkContinuity::Inherit) { copy_links(vm, me, &first); copy_links(vm, me, &second); }
            copy_decision(vm, me, &first);
            copy_decision(vm, me, &second);
            let old = remove_object(vm, me, &first);
            hook(vm, class, K::Split, old, vec![r(&first), r(&second)])
        }
        K::Fracture => {
            let cf = into(0);
            let mut ff = copy_fields(vm, me, &cf)?;
            for (k, v) in overrides(vm, fields_of(t, "fragment"), me, target)? { ff.insert(k, v); }
            let carries = overrides(vm, fields_of(t, "carries"), me, target)?;
            let fragment = make_object(vm, &cf, ff)?;
            for (k, v) in carries { write_field(vm, me, &k, v)?; }
            match t.overlay_rule {
                OverlayContinuity::Split => copy_overlays(vm, me, &fragment)?,
                OverlayContinuity::Transfer => move_overlays(vm, me, &fragment)?,
                OverlayContinuity::Drop => {}
            }
            if matches!(t.link_rule, LinkContinuity::Inherit) { copy_links(vm, me, &fragment); }
            copy_decision(vm, me, &fragment);
            hook(vm, class, K::Fracture, r(me), vec![r(&fragment)])
        }
        K::Merge => {
            let other = target.ok_or_else(|| err("merge without a target"))?;
            if crate::world::is_remote(vm, other) { return claim(vm, me, class, t, other); }
            let cm = into(0);
            let mut fm = copy_fields(vm, me, &cm)?;
            for (k, v) in overrides(vm, fields_of(t, "carries"), me, target)? { fm.insert(k, v); }
            let merged = make_object(vm, &cm, fm)?;
            move_contents(vm, me, &merged)?;
            move_contents(vm, other, &merged)?;
            if !matches!(t.overlay_rule, OverlayContinuity::Drop) {
                copy_overlays(vm, me, &merged)?;
                copy_overlays(vm, other, &merged)?;
            }
            if matches!(t.link_rule, LinkContinuity::Inherit) { copy_links(vm, me, &merged); copy_links(vm, other, &merged); }
            copy_decision(vm, me, &merged);
            let old = remove_object(vm, me, &merged);
            let old_target = remove_object(vm, other, &merged);
            hook(vm, class, K::Merge, old, vec![old_target, r(&merged)])
        }
        K::Absorb => {
            let other = target.ok_or_else(|| err("absorb without a target"))?;
            if crate::world::is_remote(vm, other) { return claim(vm, me, class, t, other); }
            let carries = overrides(vm, fields_of(t, "carries"), me, target)?;
            for (k, v) in carries { write_field(vm, me, &k, v)?; }
            move_contents(vm, other, me)?;
            if !matches!(t.overlay_rule, OverlayContinuity::Drop) { copy_overlays(vm, other, me)?; }
            if matches!(t.link_rule, LinkContinuity::Inherit) { copy_links(vm, other, me); }
            let old_target = remove_object(vm, other, me);
            hook(vm, class, K::Absorb, r(me), vec![old_target])
        }
        K::Subjugate => {
            let other = target.ok_or_else(|| err("subjugate without a target"))?;
            // Owning one's own owner would make a loop of owners.
            let mut up = Some(me.to_string());
            while let Some(u) = up {
                if u == other {
                    return Err(err(format!(
                        "R1242: subjugate-loop: {class} {} can't subjugate {}: it is held by it, and an object can't own what owns it",
                        crate::tick::host_name(vm, me), crate::tick::host_name(vm, other))));
                }
                up = vm.session.object_store.holder_of(u.as_str()).map(|h| h.to_string());
            }
            write_field(vm, other, crate::store::OWNER_FIELD, r(me))?;
            hook(vm, class, K::Subjugate, r(me), vec![r(other)])
        }
    }
}

// ── Across regions ──────────────────────────────────────────────────────────

/// An absorb or merge whose target runs in another region: this region
/// claims it. Every region's claims are settled when the tick ends, in a
/// fixed order (by region, then as made), so an object is taken once; at
/// the start of the next tick the claimer's region carries it out
/// (`take_claimed`) and the target's region retires the target
/// (`retire_claimed`).
fn claim(vm: &mut Vm, me: &str, class: &str, t: &TransitionDef, other: &str) -> Result<(), GoblinError> {
    let classes = vm.session.classes.clone();
    let index = classes.get(class).and_then(|d| d.transitions.iter().position(|x| std::ptr::eq(x, t)))
        .ok_or_else(|| err("transition: claim without its transition"))?;
    let absorb = matches!(t.kind, K::Absorb);
    let new_id = (!absorb).then(|| uuid::Uuid::new_v4().to_string());
    let links = matches!(t.link_rule, LinkContinuity::Inherit);
    if !crate::world::send_claim(&mut vm.session, crate::world::Claim {
        absorb, by: me.to_string(), target: other.to_string(), class: class.to_string(), index, new_id, links,
    }) {
        return Err(err(format!(
            "R1248: transition-across-world: the program's {class} can't {} a region's object; give the {class} to a region",
            if absorb { "absorb" } else { "merge with" })));
    }
    Ok(())
}

/// The claimer's side of a claim the world granted. `other` is in the
/// store as it was when the claim was made (a copy, gone once this is
/// done); `overlays` are its overlays then.
pub(crate) fn take_claimed(vm: &mut Vm, c: &crate::world::Claim, overlays: Vec<OverlayInstance>) -> Result<(), GoblinError> {
    let (me, other) = (c.by.as_str(), c.target.as_str());
    let classes = vm.session.classes.clone();
    let Some(t) = classes.get(&c.class).and_then(|d| d.transitions.get(c.index)) else { return Ok(()) };
    let r = |u: &str| Value::Ref(u.into());
    let keep_overlays = !matches!(t.overlay_rule, OverlayContinuity::Drop);
    let inherit = matches!(t.link_rule, LinkContinuity::Inherit);
    let put = |vm: &mut Vm, to: &str, list: &[OverlayInstance]| -> Result<(), GoblinError> {
        let host_var = crate::tick::host_name(vm, to);
        for inst in list {
            let des_id = next_overlay_id(vm);
            let copy = OverlayInstance { host_uuid: to.to_string(), host_var: host_var.clone(), applied: Vec::new(), des_id, ..inst.clone() };
            crate::tick::overlay_put(vm, copy, false)?;
        }
        Ok(())
    };
    let old_target = vm.session.object_store.get(other).cloned().unwrap_or(Value::Nil);
    if c.absorb {
        let carries = overrides(vm, fields_of(t, "carries"), me, Some(other))?;
        for (k, v) in carries { write_field(vm, me, &k, v)?; }
        move_contents(vm, other, me)?;
        if keep_overlays { put(vm, me, &overlays)?; }
        if inherit { copy_links(vm, other, me); }
        hook(vm, &c.class, K::Absorb, r(me), vec![old_target])
    } else {
        let cm = t.into_classes.first().cloned().unwrap_or_else(|| c.class.clone());
        let mut fm = copy_fields(vm, me, &cm)?;
        for (k, v) in overrides(vm, fields_of(t, "carries"), me, Some(other))? { fm.insert(k, v); }
        let merged = make_object_as(vm, &cm, fm, c.new_id.as_deref())?;
        move_contents(vm, me, &merged)?;
        move_contents(vm, other, &merged)?;
        if keep_overlays {
            copy_overlays(vm, me, &merged)?;
            put(vm, &merged, &overlays)?;
        }
        if inherit { copy_links(vm, me, &merged); copy_links(vm, other, &merged); }
        copy_decision(vm, me, &merged);
        let old = remove_object(vm, me, &merged);
        hook(vm, &c.class, K::Merge, old, vec![old_target, r(&merged)])
    }
}

/// The target's side of a granted claim: it leaves the world (what it owns
/// already went to `heir`, which every region heard of). With `links`, its
/// link offsets go to `heir`: those of other objects to it stay here, its
/// own (and its own link formulas) are given back to go where `heir` runs.
pub(crate) fn retire_claimed(vm: &mut Vm, uuid: &str, heir: &str, links: bool) -> crate::world::Links {
    let mut out = crate::world::Links::default();
    if !vm.session.object_store.contains_key(uuid) { return out; }
    if links {
        let swap = |u: &String| if u == uuid { heir.to_string() } else { u.clone() };
        let moved: Vec<((String, String, String), Vec<crate::session::LinkOffset>)> = vm.session.link_offsets.iter()
            .filter(|((a, b, _), _)| a == uuid || b == uuid)
            .map(|((a, b, c), v)| ((swap(a), swap(b), c.clone()), v.clone()))
            .filter(|((a, b, _), _)| a != b)
            .collect();
        for (k, v) in moved {
            if k.0 == heir { out.offsets.push((k, v)); } else { vm.session.link_offsets.entry(k).or_default().extend(v); }
        }
        out.defs = vm.session.object_link_defs.iter()
            .filter(|((a, _), _)| a == uuid).map(|((_, c), d)| ((heir.to_string(), c.clone()), (**d).clone())).collect();
    }
    take_overlays_off(vm, uuid);
    vm.session.link_offsets.retain(|(a, b, _), _| a != uuid && b != uuid);
    vm.session.object_link_defs.retain(|(a, _), _| a != uuid);
    vm.session.object_decisions.remove(uuid);
    vm.session.object_store.move_contents_up(uuid);
    vm.session.object_store.shift_remove(uuid);
    out
}

/// The new fields of an object of `class`: its declared defaults.
fn defaults(vm: &mut Vm, class: &str) -> Result<FieldMap, GoblinError> {
    let classes = vm.session.classes.clone();
    let decl = classes.get(class).ok_or_else(|| err(format!("R1241: unknown-class: a transition makes a {class}, and there is no class {class}")))?;
    let mut fields = FieldMap::default();
    for fd in &decl.fields {
        if let Some(e) = &fd.default {
            let v = vm.eval_tick_expr(e, vec![])?;
            fields.insert(fd.name.clone(), v);
        }
    }
    Ok(fields)
}

/// A copy of `from`'s fields with their current stored values (overlays'
/// `modify` left out: the successor gets its own copies of the overlays).
/// For another class: the fields both have, and that class's defaults for
/// the rest.
fn copy_fields(vm: &mut Vm, from: &str, class: &str) -> Result<FieldMap, GoblinError> {
    let Some(Value::Object(o)) = vm.session.object_store.get(from) else {
        return Err(err(format!("transition: object {from} is gone")));
    };
    let traits = o.trait_fields.clone();
    let mut base = (*o.fields).clone();
    if base.has_film() { base.set_film(&[], &|n| traits.contains(n)); }
    if &*o.class_name == class { return Ok(base); }
    let classes = vm.session.classes.clone();
    let decl = classes.get(class).ok_or_else(|| err(format!("R1241: unknown-class: a transition makes a {class}, and there is no class {class}")))?;
    let mut fields = FieldMap::default();
    for fd in &decl.fields {
        match base.get(&fd.name) {
            Some(v) => { fields.insert(fd.name.clone(), v.clone()); }
            None => if let Some(e) = &fd.default {
                let v = vm.eval_tick_expr(e, vec![])?;
                fields.insert(fd.name.clone(), v);
            },
        }
    }
    Ok(fields)
}

/// A field list (`first:`, `carries:` ...), worked out with `self` (and
/// `target`) as they are now.
fn overrides(vm: &mut Vm, list: &[(String, goblin_ast::Expr)], me: &str, target: Option<&str>) -> Result<Vec<(String, Value)>, GoblinError> {
    let mut out = Vec::with_capacity(list.len());
    for (k, e) in list {
        let mut locals = vec![("self", Value::Ref(me.into()))];
        if let Some(x) = target { locals.push(("target", Value::Ref(x.into()))); }
        let v = vm.eval_tick_expr(e, locals)?;
        let v = entry(vm, v);
        out.push((k.clone(), v));
    }
    Ok(out)
}

/// The fields laid out as a new object's are: its own `id` first (read
/// only), the class's fields, its `uuid` last.
fn with_identity(fields: FieldMap, id: &str, decl: &goblin_ast::ClassDecl) -> (FieldMap, std::collections::BTreeSet<String>) {
    let mut out = FieldMap::default();
    let mut readonly = std::collections::BTreeSet::new();
    out.insert("id".to_string(), Value::Str(id.into()));
    readonly.insert("id".to_string());
    for (k, v) in fields.iter() {
        if k == "id" || k == "uuid" { continue; }
        out.insert(k.clone(), v.clone());
    }
    for f in &decl.fields { if f.readonly { readonly.insert(f.name.clone()); } }
    out.insert("uuid".to_string(), Value::Str(id.into()));
    (out, readonly)
}

/// A new object of `class` with these fields (`~` fields kept in 0..1).
fn make_object(vm: &mut Vm, class: &str, fields: FieldMap) -> Result<String, GoblinError> {
    make_object_as(vm, class, fields, None)
}

/// `make_object`, with the id given (a merge across regions picks it when
/// it claims).
fn make_object_as(vm: &mut Vm, class: &str, mut fields: FieldMap, id: Option<&str>) -> Result<String, GoblinError> {
    let classes = vm.session.classes.clone();
    let decl = classes.get(class).ok_or_else(|| err(format!("R1241: unknown-class: a transition makes a {class}, and there is no class {class}")))?;
    let id = id.map(str::to_string).unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    let (mut fields, readonly) = with_identity(fields, &id, decl);
    let traits = crate::vm::trait_fields_of(&mut fields, &decl.fields);
    let obj = Value::Object(Box::new(ObjectVal {
        class_name: crate::value::class_rc(class),
        fields: Rc::new(fields),
        readonly_fields: Rc::new(readonly),
        trait_fields: Rc::new(traits),
        uuid: id.as_str().into(),
    }));
    vm.session.object_store.insert(id.clone(), obj);
    vm.session.touch(id.as_str());
    vm.session.des_metrics.objects_made += 1;
    Ok(id)
}

/// `mutate` into another class: the same object (same id, same place),
/// now of `class` with these fields; its overlays stay on.
fn recast(vm: &mut Vm, me: &str, class: &str, mut fields: FieldMap) -> Result<(), GoblinError> {
    let classes = vm.session.classes.clone();
    let decl = classes.get(class).ok_or_else(|| err(format!("R1241: unknown-class: a transition makes a {class}, and there is no class {class}")))?;
    let (mut fields, readonly) = with_identity(fields, me, decl);
    let traits = crate::vm::trait_fields_of(&mut fields, &decl.fields);
    let obj = Value::Object(Box::new(ObjectVal {
        class_name: crate::value::class_rc(class),
        fields: Rc::new(fields),
        readonly_fields: Rc::new(readonly),
        trait_fields: Rc::new(traits),
        uuid: me.into(),
    }));
    vm.session.object_store.insert(me.to_string(), obj);
    vm.session.touch(me);
    vm.session.overlay_instances.mark(me);
    crate::tick::settle_films(&mut vm.session);
    Ok(())
}

/// Writes one field as `x >> field |= v` does: a `~` field stays in 0..1,
/// `owner` refiles the object, another region's object gets the change.
fn write_field(vm: &mut Vm, uuid: &str, field: &str, v: Value) -> Result<(), GoblinError> {
    let v = entry(vm, v);
    if field == crate::store::OWNER_FIELD { crate::world::check_owner_write(&vm.session, uuid, &v)?; }
    vm.session.touch(uuid);
    let Some(Value::Object(o)) = vm.session.object_store.get_mut(uuid) else {
        return Err(err(format!("transition: object {uuid} is gone")));
    };
    let v = if o.trait_fields.contains(field) {
        match v {
            Value::Float(f) => Value::Float(f.clamp(0.0, 1.0)),
            Value::Int(i) => Value::Float((i as f64).clamp(0.0, 1.0)),
            other => other,
        }
    } else { v };
    Rc::make_mut(&mut o.fields).insert(field.to_string(), v);
    if field == crate::store::OWNER_FIELD { vm.session.object_store.refile_owner(uuid); }
    crate::world::after_view_write(&mut vm.session, uuid, field)
}

/// Everything `from` owns goes to `to`.
fn move_contents(vm: &mut Vm, from: &str, to: &str) -> Result<(), GoblinError> {
    let held: Vec<String> = vm.session.object_store.contents_of(from).map(|s| s.to_string()).collect();
    for h in held {
        if h == to { continue; }
        write_field(vm, &h, crate::store::OWNER_FIELD, Value::Ref(to.into()))?;
    }
    Ok(())
}

/// Copies of `from`'s overlays on `to`, with their intensity, age, time
/// left and fields (their `on start` does not run again).
fn copy_overlays(vm: &mut Vm, from: &str, to: &str) -> Result<(), GoblinError> {
    let list: Vec<OverlayInstance> = vm.session.overlay_instances.on_host(from).iter()
        .map(|&p| vm.session.overlay_instances[p].clone()).collect();
    let host_var = crate::tick::host_name(vm, to);
    for inst in list {
        let des_id = next_overlay_id(vm);
        let copy = OverlayInstance { host_uuid: to.to_string(), host_var: host_var.clone(), applied: Vec::new(), des_id, ..inst };
        crate::tick::overlay_put(vm, copy, false)?;
    }
    Ok(())
}

fn move_overlays(vm: &mut Vm, from: &str, to: &str) -> Result<(), GoblinError> {
    copy_overlays(vm, from, to)?;
    take_overlays_off(vm, from);
    Ok(())
}

fn take_overlays_off(vm: &mut Vm, host: &str) {
    let ps: Vec<usize> = vm.session.overlay_instances.on_host(host).to_vec();
    if ps.is_empty() { return; }
    for gone in vm.session.overlay_instances.take_at(&ps) { crate::tick::forget_overlay(vm, &gone); }
    crate::tick::settle_films(&mut vm.session);
}

fn next_overlay_id(vm: &mut Vm) -> OverlayInstanceId {
    let id = OverlayInstanceId(vm.session.des_overlay_id_counter);
    vm.session.des_overlay_id_counter += 1;
    id
}

/// `to` gets `from`'s link offsets and its own link formulas.
fn copy_links(vm: &mut Vm, from: &str, to: &str) {
    let swap = |u: &String| if u == from { to.to_string() } else { u.clone() };
    let offsets: Vec<((String, String, String), Vec<crate::session::LinkOffset>)> = vm.session.link_offsets.iter()
        .filter(|((a, b, _), _)| a == from || b == from)
        .map(|((a, b, c), v)| ((swap(a), swap(b), c.clone()), v.clone()))
        .filter(|((a, b, _), _)| a != b)
        .collect();
    for (k, v) in offsets { vm.session.link_offsets.entry(k).or_default().extend(v); }
    let defs: Vec<((String, String), Rc<crate::session::LinkDef>)> = vm.session.object_link_defs.iter()
        .filter(|((a, _), _)| a == from).map(|((_, c), d)| ((to.to_string(), c.clone()), d.clone())).collect();
    for (k, d) in defs { vm.session.object_link_defs.entry(k).or_insert(d); }
}

/// A successor keeps a decision given to its parent alone.
fn copy_decision(vm: &mut Vm, from: &str, to: &str) {
    if let Some(d) = vm.session.object_decisions.get(from).cloned() {
        vm.session.object_decisions.insert(to.to_string(), d);
    }
}

/// Takes an object out of the world (its overlays and link offsets with
/// it) and gives what it was, still readable, for its hook.
/// Takes `uuid` out of the world; what it owns in other regions goes to
/// `heir` (this region's contents were moved already).
fn remove_object(vm: &mut Vm, uuid: &str, heir: &str) -> Value {
    let old = vm.session.object_store.get(uuid).cloned().unwrap_or(Value::Nil);
    take_overlays_off(vm, uuid);
    vm.session.link_offsets.retain(|(a, b, _), _| a != uuid && b != uuid);
    vm.session.object_link_defs.retain(|(a, _), _| a != uuid);
    vm.session.object_decisions.remove(uuid);
    crate::world::note_removed(&mut vm.session, uuid, Some(heir));
    vm.session.object_store.move_contents_up(uuid);
    vm.session.object_store.shift_remove(uuid);
    old
}

/// Runs the class's `on split => ...` (or the hook for `kind`): `self`
/// first, then the transition's other names.
fn hook(vm: &mut Vm, class: &str, kind: K, me: Value, args: Vec<Value>) -> Result<(), GoblinError> {
    if crate::events::on(vm) {
        let who = crate::events::who_value(vm, &me).unwrap_or_default();
        let whom: Vec<String> = args.iter().filter_map(|a| crate::events::who_value(vm, a)).collect();
        crate::events::log(vm, who, word(&kind), &whom)?;
    }
    let key = (class.to_string(), format!("on {}", word(&kind)));
    let Some(f) = vm.session.compiled_methods.get(&key).cloned() else { return Ok(()) };
    let mut a = Vec::with_capacity(1 + args.len());
    a.push(me);
    a.extend(args);
    crate::tick::with_erase_check(vm, |vm| vm.call_callable(Value::Function(f), a).map(|_| ()))
}

// ── Overlays ────────────────────────────────────────────────────────────────

/// Every overlay's transitions for this tick, in the order the overlays
/// were put on: at most one each, the ones on an overlay alone first, then
/// the ones with a target (another overlay on the same host).
pub(crate) fn overlay_transitions(vm: &mut Vm) -> Result<(), GoblinError> {
    let phase = vm.session.phase.clone();
    let phase = phase.as_deref();
    if vm.session.overlay_defs.values().all(|d| d.transitions_in(phase).is_empty()) { return Ok(()); }
    let snap: Vec<(OverlayInstanceId, String)> = vm.session.overlay_instances.iter()
        .filter(|i| vm.session.overlay_defs.get(&i.overlay_name).is_some_and(|d| !d.transitions_in(phase).is_empty()))
        .filter(|i| !crate::world::is_remote(vm, &i.host_uuid))
        .map(|i| (i.des_id, i.host_uuid.clone()))
        .collect();
    let mut done: FxHashSet<u32> = Default::default();
    let mut clock = crate::metrics::LayerClock::default();
    for pass in 0..2 {
        for (id, host) in &snap {
            if done.contains(&id.0) { continue; }
            let Some(p) = find_overlay(vm, host, *id) else { continue };
            let inst = vm.session.overlay_instances[p].clone();
            let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name).cloned() else { continue };
            clock.at(&mut vm.session.des_metrics, def.class.as_deref().unwrap_or(&inst.overlay_name));
            for t in def.transitions_in(phase) {
                if alone(&t.kind) != (pass == 0) { continue; }
                let me = self_record(&inst);
                let target = if alone(&t.kind) {
                    if !vm.eval_tick_expr_bool(&t.trigger, overlay_locals(&me, &inst, None, &def.maker)) { continue; }
                    None
                } else {
                    match find_overlay_target(vm, &inst, &me, t, &done) { Some(x) => Some(x), None => continue }
                };
                done.insert(id.0);
                if let Some(x) = &target { done.insert(x.des_id.0); }
                fire_overlay(vm, &def, t, &inst, &me, target.as_ref())?;
                break;
            }
        }
    }
    clock.stop(&mut vm.session.des_metrics);
    Ok(())
}

fn find_overlay(vm: &Vm, host: &str, id: OverlayInstanceId) -> Option<usize> {
    let list = &vm.session.overlay_instances;
    list.on_host(host).iter().copied().find(|&q| list[q].des_id == id)
}

/// What an overlay's transition reads as `self`: its kind, host, intensity,
/// age and fields.
fn self_record(inst: &OverlayInstance) -> Value {
    let mut f = FieldMap::default();
    f.insert("strength".to_string(), Value::Float(inst.strength));
    f.insert("intensity".to_string(), Value::Float(inst.strength));
    f.insert("age".to_string(), Value::Int(inst.age as i64));
    f.insert("overlay_name".to_string(), Value::Str(inst.overlay_name.as_str().into()));
    f.insert("host_uuid".to_string(), Value::Str(inst.host_uuid.as_str().into()));
    f.insert("host_var".to_string(), Value::Str(inst.host_var.as_str().into()));
    for (k, v) in &inst.extra_fields { f.insert(k.clone(), v.clone()); }
    Value::Object(Box::new(ObjectVal {
        class_name: inst.overlay_name.as_str().into(),
        fields: Rc::new(f),
        readonly_fields: Default::default(),
        trait_fields: Default::default(),
        // Its own id, as `:overlays` records have: binding it or passing it
        // to a builtin must not turn it into its host.
        uuid: format!("{}{}:{}", crate::vm::OVERLAY_RECORD, inst.host_uuid, inst.des_id.0).into(),
    }))
}

fn overlay_locals<'a>(me: &Value, inst: &OverlayInstance, target: Option<&Value>, maker: &'a [(String, Value)]) -> Vec<(&'a str, Value)> {
    let mut l = vec![
        ("self", me.clone()),
        ("strength", Value::Float(inst.strength)),
        ("intensity", Value::Float(inst.strength)),
        ("age", Value::Int(inst.age as i64)),
    ];
    if let Some(t) = target { l.push(("target", t.clone())); }
    for (n, v) in maker { l.push((n.as_str(), v.clone())); }
    l
}

/// The variables of the action that made this copy's kind (see `OverlayDef::maker`).
fn maker_of(vm: &Vm, inst: &OverlayInstance) -> Vec<(String, Value)> {
    vm.session.overlay_defs.get(&inst.overlay_name).map(|d| d.maker.clone()).unwrap_or_default()
}

/// Another overlay on the same host, of the overlay (or overlay class)
/// `targets:` names, that the condition holds for.
fn find_overlay_target(vm: &mut Vm, inst: &OverlayInstance, me: &Value, t: &TransitionDef, done: &FxHashSet<u32>) -> Option<OverlayInstance> {
    let wanted = t.target_class.as_deref()?;
    let others: Vec<OverlayInstance> = {
        let list = &vm.session.overlay_instances;
        let defs = &vm.session.overlay_defs;
        list.on_host(&inst.host_uuid).iter().map(|&q| &list[q])
            .filter(|o| o.des_id != inst.des_id && !done.contains(&o.des_id.0))
            .filter(|o| o.overlay_name == wanted || defs.get(&o.overlay_name).is_some_and(|d| d.class.as_deref() == Some(wanted)))
            .cloned().collect()
    };
    let maker = maker_of(vm, inst);
    for o in others {
        let rec = vm.overlay_record(&o);
        if vm.eval_tick_expr_bool(&t.trigger, overlay_locals(me, inst, Some(&rec), &maker)) { return Some(o); }
    }
    None
}

/// An overlay's field list worked out: its intensity, and its other fields.
fn overlay_overrides(vm: &mut Vm, list: &[(String, goblin_ast::Expr)], me: &Value, inst: &OverlayInstance, target: Option<&OverlayInstance>)
    -> Result<(Option<f64>, Vec<(String, Value)>), GoblinError> {
    let rec = target.map(|t| vm.overlay_record(t));
    let maker = maker_of(vm, inst);
    let mut intensity = None;
    let mut out = Vec::new();
    for (k, e) in list {
        let v = vm.eval_tick_expr(e, overlay_locals(me, inst, rec.as_ref(), &maker))?;
        if k == "intensity" {
            let f = match v {
                Value::Float(f) => f,
                Value::Int(i) => i as f64,
                other => return Err(err(format!("R1244: overlay-intensity: an overlay's intensity is a number from 0 to 1, got {}", other.type_name()))),
            };
            intensity = Some(f.clamp(0.0, 1.0));
        } else {
            out.push((k.clone(), v));
        }
    }
    Ok((intensity, out))
}

/// A successor of `inst` of kind `name`: a copy of it (its fields, time
/// left), new, with these changes.
fn successor(vm: &mut Vm, inst: &OverlayInstance, name: String, intensity: Option<f64>, fields: Vec<(String, Value)>) -> OverlayInstance {
    let mut extra = inst.extra_fields.clone();
    for (k, v) in fields { extra.insert(k, v); }
    OverlayInstance {
        overlay_name: name,
        host_uuid: inst.host_uuid.clone(),
        host_var: inst.host_var.clone(),
        strength: intensity.unwrap_or(inst.strength),
        age: 0,
        ticks_remaining: inst.ticks_remaining,
        count: 1,
        applied: Vec::new(),
        extra_fields: extra,
        des_id: next_overlay_id(vm),
    }
}

/// The kind a successor has: the overlay `into:` names, or a new kind made
/// from `parent` now (true: made now).
fn successor_kind(vm: &mut Vm, t: &TransitionDef, i: usize, parent: &str) -> Result<(String, bool), GoblinError> {
    match t.into_classes.get(i) {
        Some(n) => {
            if !vm.session.overlay_defs.contains_key(n) {
                return Err(err(format!("R1243: unknown-overlay: a transition of overlay {parent} goes into {n}, and there is no overlay {n}")));
            }
            Ok((n.clone(), false))
        }
        None if i > 0 && !t.into_classes.is_empty() => successor_kind(vm, t, 0, parent),
        None => Ok((new_kind(vm, parent)?, true)),
    }
}

/// A kind made now starts from the overlay it was made for: a copy that
/// spreads from it carries its fields and intensity, not its parent's.
fn seed_kind(vm: &mut Vm, made: bool, inst: &OverlayInstance) {
    if !made { return; }
    let Some(d) = vm.session.overlay_defs.get(&inst.overlay_name) else { return };
    let mut seeded = OverlayDef { extra_fields: inst.extra_fields.clone(), intensity: inst.strength, ..(**d).clone() };
    // Its blocks take one argument per field, so a field the parent lacked
    // means compiling them again.
    if !inst.extra_fields.keys().eq(d.extra_fields.keys()) {
        let fields = &seeded.extra_fields;
        seeded.maker.retain(|(n, _)| !fields.contains_key(n));
        seeded.methods = crate::tick::compile_overlay_methods(&vm.session, &seeded.name, &seeded.actions, &seeded.own_names()).unwrap_or_default();
    }
    vm.session.overlay_defs.insert(inst.overlay_name.clone(), Rc::new(seeded));
}

/// A new overlay kind, a copy of `parent` (every line, its overlay
/// class), named as the parent's next child. A declared overlay's
/// children are numbered after its family (`language_1`, `language_2`);
/// a made kind's children add a letter after a number and a number after
/// a letter (`language_1` → `language_1A` → `language_1A.1` →
/// `language_1A.1A`). The parent is recorded. Each count is shared out so
/// no other region gives the same name: region k of n hands out k+1,
/// k+1+n, k+1+2n, ... (the main program is 0), so a region's names may
/// skip some.
fn new_kind(vm: &mut Vm, parent: &str) -> Result<String, GoblinError> {
    let def = vm.session.overlay_defs.get(parent).cloned()
        .ok_or_else(|| err(format!("R1243: unknown-overlay: no overlay named '{parent}'")))?;
    let (k, n) = match vm.session.region.as_ref() {
        Some(c) => (c.regions.iter().position(|r| *r == c.me).map_or(0, |p| p + 1) as u64, c.regions.len() as u64 + 1),
        None => (0, crate::world::world_size(&vm.session).map_or(1, |r| r as u64 + 1)),
    };
    let key = if def.generated { def.name.clone() } else { def.family.clone() };
    let name = loop {
        let c = vm.session.kind_counts.entry(key.clone()).or_insert(0);
        let num = *c * n + k + 1;
        *c += 1;
        let name = if !def.generated {
            format!("{}_{}", def.family, num)
        } else if def.name.ends_with(|c: char| c.is_ascii_digit()) {
            format!("{}{}", def.name, letters(num))
        } else {
            format!("{}.{}", def.name, num)
        };
        if !vm.session.overlay_defs.contains_key(&name) { break name; }
    };
    let made = OverlayDef { name: name.clone(), generated: true, parents: vec![parent.to_string()], ..(*def).clone() };
    vm.session.kind_order.push(name.clone());
    vm.session.overlay_defs.insert(name.clone(), Rc::new(made));
    Ok(name)
}

/// The name of a new kind with no parent, made by a run of an `overlay`
/// line (see the `OverlayDef` opcode): its family, numbered as a declared
/// overlay's children are (`faith_1`, `faith_2`), from the same count, so
/// no two kinds share a name.
pub(crate) fn new_root_name(vm: &mut Vm, family: &str) -> String {
    let (k, n) = match vm.session.region.as_ref() {
        Some(c) => (c.regions.iter().position(|r| *r == c.me).map_or(0, |p| p + 1) as u64, c.regions.len() as u64 + 1),
        None => (0, crate::world::world_size(&vm.session).map_or(1, |r| r as u64 + 1)),
    };
    loop {
        let c = vm.session.kind_counts.entry(family.to_string()).or_insert(0);
        let num = *c * n + k + 1;
        *c += 1;
        let name = format!("{family}_{num}");
        if !vm.session.overlay_defs.contains_key(&name) { return name; }
    }
}

/// The kind a merge with no `into:` makes: named after both, as merged
/// companies are (`language_1A` and `language_1B` make `language_1A-1B`;
/// kinds of different families, `latin-greek`), with both as parents. Two
/// overlays of one kind merge into that kind. The same two kinds merging
/// again (anywhere) make the same kind (false: it was there).
fn merged_kind(vm: &mut Vm, a: &str, b: &str) -> Result<(String, bool), GoblinError> {
    if a == b { return Ok((a.to_string(), false)); }
    let def = vm.session.overlay_defs.get(a).cloned()
        .ok_or_else(|| err(format!("R1243: unknown-overlay: no overlay named '{a}'")))?;
    let prefix = format!("{}_", def.family);
    let name = match (a.strip_prefix(&prefix), b.strip_prefix(&prefix)) {
        (Some(x), Some(y)) => format!("{prefix}{x}-{y}"),
        _ => format!("{a}-{b}"),
    };
    if vm.session.overlay_defs.contains_key(&name) { return Ok((name, false)); }
    let made = OverlayDef { name: name.clone(), generated: true, parents: vec![a.to_string(), b.to_string()], ..(*def).clone() };
    vm.session.kind_order.push(name.clone());
    vm.session.overlay_defs.insert(name.clone(), Rc::new(made));
    Ok((name, true))
}

/// 1 → A, 26 → Z, 27 → AA.
fn letters(mut n: u64) -> String {
    let mut out = Vec::new();
    while n > 0 {
        n -= 1;
        out.push(b'A' + (n % 26) as u8);
        n /= 26;
    }
    out.reverse();
    String::from_utf8(out).unwrap_or_default()
}

fn take_overlay(vm: &mut Vm, inst: &OverlayInstance) {
    let Some(p) = find_overlay(vm, &inst.host_uuid, inst.des_id) else { return };
    for gone in vm.session.overlay_instances.take_at(&[p]) { crate::tick::forget_overlay(vm, &gone); }
    crate::tick::settle_films(&mut vm.session);
}

/// Gives the overlay at `p` a new intensity and fields, and what its
/// `modify` adds to the host follows.
fn change_overlay(vm: &mut Vm, p: usize, intensity: Option<f64>, fields: Vec<(String, Value)>) -> Result<(), GoblinError> {
    if let Some(f) = intensity { vm.session.overlay_instances[p].strength = f; }
    for (k, v) in fields { vm.session.overlay_instances[p].extra_fields.insert(k, v); }
    let amounts = crate::tick::modify_amounts(vm, p)?;
    vm.session.overlay_instances[p].applied = amounts;
    let host = vm.session.overlay_instances[p].host_uuid.clone();
    vm.session.overlay_instances.mark(&host);
    crate::tick::settle_films(&mut vm.session);
    Ok(())
}

fn fire_overlay(vm: &mut Vm, def: &Rc<OverlayDef>, t: &TransitionDef, inst: &OverlayInstance, me: &Value, target: Option<&OverlayInstance>) -> Result<(), GoblinError> {
    let name = inst.overlay_name.clone();
    match t.kind {
        K::Erase => { olog(vm, inst, K::Erase, Vec::new())?; take_overlay(vm, inst); Ok(()) }
        K::Mutate => {
            let (kind, made) = successor_kind(vm, t, 0, &name)?;
            let (intensity, fields) = overlay_overrides(vm, fields_of(t, "carries"), me, inst, target)?;
            let Some(p) = find_overlay(vm, &inst.host_uuid, inst.des_id) else { return Ok(()) };
            if kind != name {
                vm.session.overlay_instances.rename(p, kind.clone());
                vm.session.overlay_instances[p].age = 0;
            }
            change_overlay(vm, p, intensity, fields)?;
            let now = vm.session.overlay_instances[p].clone();
            seed_kind(vm, made, &now);
            olog(vm, inst, K::Mutate, if kind != name { vec![kind] } else { Vec::new() })?;
            overlay_hook(vm, def, K::Mutate, inst, Vec::new())
        }
        K::Split => {
            let ((ka, ma), (kb, mb)) = (successor_kind(vm, t, 0, &name)?, successor_kind(vm, t, 1, &name)?);
            let (ia, fa) = overlay_overrides(vm, fields_of(t, "first"), me, inst, target)?;
            let (ib, fb) = overlay_overrides(vm, fields_of(t, "second"), me, inst, target)?;
            let a = successor(vm, inst, ka, ia, fa);
            let b = successor(vm, inst, kb, ib, fb);
            seed_kind(vm, ma, &a);
            seed_kind(vm, mb, &b);
            take_overlay(vm, inst);
            crate::tick::overlay_put(vm, a.clone(), false)?;
            crate::tick::overlay_put(vm, b.clone(), false)?;
            olog(vm, inst, K::Split, vec![a.overlay_name.clone(), b.overlay_name.clone()])?;
            let recs = vec![vm.overlay_record(&a), vm.overlay_record(&b)];
            overlay_hook(vm, def, K::Split, inst, recs)
        }
        K::Fracture => {
            let (kf, mf) = successor_kind(vm, t, 0, &name)?;
            let (i_f, ff) = overlay_overrides(vm, fields_of(t, "fragment"), me, inst, target)?;
            let (ic, fc) = overlay_overrides(vm, fields_of(t, "carries"), me, inst, target)?;
            let frag = successor(vm, inst, kf, i_f, ff);
            seed_kind(vm, mf, &frag);
            if let Some(p) = find_overlay(vm, &inst.host_uuid, inst.des_id) { change_overlay(vm, p, ic, fc)?; }
            crate::tick::overlay_put(vm, frag.clone(), false)?;
            olog(vm, inst, K::Fracture, vec![frag.overlay_name.clone()])?;
            let recs = vec![vm.overlay_record(&frag)];
            overlay_hook(vm, def, K::Fracture, inst, recs)
        }
        K::Spawn => {
            let kind = match t.into_classes.first() { Some(_) => successor_kind(vm, t, 0, &name)?.0, None => name.clone() };
            let kdef = vm.session.overlay_defs.get(&kind).cloned().ok_or_else(|| err(format!("R1243: unknown-overlay: no overlay named '{kind}'")))?;
            let (ic, fc) = overlay_overrides(vm, fields_of(t, "child"), me, inst, target)?;
            let mut extra = kdef.extra_fields.clone();
            for (k, v) in fc { extra.insert(k, v); }
            let child = OverlayInstance {
                overlay_name: kind,
                host_uuid: inst.host_uuid.clone(),
                host_var: inst.host_var.clone(),
                strength: ic.unwrap_or(kdef.intensity),
                age: 0,
                ticks_remaining: kdef.default_duration,
                count: 1,
                applied: Vec::new(),
                extra_fields: extra,
                des_id: next_overlay_id(vm),
            };
            olog(vm, inst, K::Spawn, vec![child.overlay_name.clone()])?;
            crate::tick::overlay_put_as(vm, child.clone(), true, crate::tick::Put::Quiet)?;
            let recs = vec![vm.overlay_record(&child)];
            overlay_hook(vm, def, K::Spawn, inst, recs)
        }
        K::Merge => {
            let Some(other) = target else { return Err(err("merge without a target")) };
            let (km, mm) = if t.into_classes.is_empty() { merged_kind(vm, &name, &other.overlay_name)? } else { successor_kind(vm, t, 0, &name)? };
            let (im, fm) = overlay_overrides(vm, fields_of(t, "carries"), me, inst, target)?;
            let merged = successor(vm, inst, km, im, fm);
            seed_kind(vm, mm, &merged);
            let old_target = vm.overlay_record(other);
            olog(vm, inst, K::Merge, vec![other.overlay_name.clone(), merged.overlay_name.clone()])?;
            take_overlay(vm, inst);
            take_overlay(vm, other);
            crate::tick::overlay_put(vm, merged.clone(), false)?;
            let recs = vec![old_target, vm.overlay_record(&merged)];
            overlay_hook(vm, def, K::Merge, inst, recs)
        }
        K::Absorb => {
            let Some(other) = target else { return Err(err("absorb without a target")) };
            let (ic, fc) = overlay_overrides(vm, fields_of(t, "carries"), me, inst, target)?;
            let old_target = vm.overlay_record(other);
            olog(vm, inst, K::Absorb, vec![other.overlay_name.clone()])?;
            take_overlay(vm, other);
            if let Some(p) = find_overlay(vm, &inst.host_uuid, inst.des_id) { change_overlay(vm, p, ic, fc)?; }
            overlay_hook(vm, def, K::Absorb, inst, vec![old_target])
        }
        K::Subjugate => Err(err("R1245: overlay-subjugate: an overlay can't subjugate")),
    }
}

/// The event log's line for an overlay's transition: the overlays it
/// made or took (on the same host).
fn olog(vm: &mut Vm, inst: &OverlayInstance, kind: K, whom: Vec<String>) -> Result<(), GoblinError> {
    if !crate::events::on(vm) { return Ok(()); }
    let who = crate::events::overlay(vm, inst);
    crate::events::log(vm, who, word(&kind), &whom)
}

/// Runs an overlay's `on split => ...` (or the hook for `kind`) as its other
/// blocks run: `self` is the host, `intensity` and the fields are the old
/// overlay's, then the successors (or target) as records.
fn overlay_hook(vm: &mut Vm, def: &Rc<OverlayDef>, kind: K, inst: &OverlayInstance, recs: Vec<Value>) -> Result<(), GoblinError> {
    let Some(f) = def.methods.get(&format!("on {}", word(&kind))).cloned() else { return Ok(()) };
    if !vm.session.object_store.contains_key(&inst.host_uuid) { return Ok(()); }
    let mut args = Vec::with_capacity(2 + recs.len() + inst.extra_fields.len() + def.maker.len());
    args.push(Value::Ref(inst.host_uuid.clone().into()));
    args.push(Value::Float(inst.strength));
    args.extend(recs);
    args.extend(def.own_values(&inst.extra_fields));
    crate::tick::with_erase_check(vm, |vm| vm.call_callable(Value::Function(f), args).map(|_| ()))
}

