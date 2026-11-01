/// DES tick runner for the Goblin VM.
///
/// Mirrors the interpreter's `tick`/`tick_db` builtin:
///   1. begin_tick on TickRunner
///   2. all_grids tick_db_begin
///   3. overlay_tick (8 passes)
///   4. all_grids tick_db_commit
///   5. des_flush_pending
///   6. end_tick on TickRunner
use rustc_hash::FxHashMap as HashMap;
use indexmap::IndexMap;

use crate::error::GoblinError;
use crate::session::{OverlayInstance, OverlayInstanceId};
use crate::value::Value;
use crate::vm::Vm;

pub fn run_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    vm.session.des_ticks += 1;
    vm.session.des_tick_runner.begin_tick();
    grids_tick_begin(vm);
    overlay_tick(vm)?;
    grids_tick_commit(vm);
    des_flush_pending(vm);
    let store = &mut vm.session.des_store;
    let index = &mut vm.session.des_index;
    vm.session.des_tick_runner.end_tick(store, index);
    Ok(())
}

fn grids_tick_begin(vm: &mut Vm) {
    let names: Vec<String> = vm.session.grid_store.names();
    for name in names {
        if let Some(world) = vm.session.grid_store.get_mut(&name) {
            world.tick_db_begin();
        }
    }
}

fn grids_tick_commit(vm: &mut Vm) {
    let names: Vec<String> = vm.session.grid_store.names();
    for name in names {
        if let Some(world) = vm.session.grid_store.get_mut(&name) {
            world.tick_db_commit();
        }
    }
}

fn des_flush_pending(vm: &mut Vm) {
    use goblin_des::entity::FieldValue;
    let handles: Vec<goblin_des::store::EntityHandle> =
        vm.session.des_store.live_handles().collect();
    for handle in handles {
        let changes: Option<std::collections::HashMap<String, FieldValue>> = vm.session.des_store.get_mut(handle).and_then(|e| {
            if e.has_pending() { Some(e.drain_pending()) } else { None }
        });
        if let Some(changes) = changes {
            let interp_uuid = vm.session.des_store.get(handle)
                .map(|e| e.interp_uuid.clone())
                .unwrap_or_default();
            if let Some(obj) = vm.session.object_store.get_mut(&interp_uuid) {
                if let Value::Object(ref mut o) = obj {
                    let fields = std::rc::Rc::make_mut(&mut o.fields);
                    for (field, fv) in changes {
                        let val = match fv {
                            FieldValue::Float(f) => Value::Float(f),
                            FieldValue::Int(i)   => Value::Int(i),
                            FieldValue::Bool(b)  => Value::Bool(b),
                            FieldValue::Str(s)   => Value::Str(s.into()),
                            FieldValue::Nil      => Value::Nil,
                        };
                        fields.insert(field, val);
                    }
                }
            }
        }
    }
}

fn overlay_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    // ── Pass 0: Decay temporary link offsets ─────────────────────────────────
    for offsets in vm.session.link_offsets.values_mut() {
        for o in offsets.iter_mut() {
            if let Some(ref mut t) = o.ticks_remaining {
                if *t > 0 { *t -= 1; }
            }
        }
        offsets.retain(|o| o.ticks_remaining.map(|t| t > 0).unwrap_or(true));
    }
    vm.session.link_offsets.retain(|_, v| !v.is_empty());
    settle_films(&mut vm.session);

    // ── Pass 1: Spread ────────────────────────────────────────────────────────
    spread_pass(vm)?;

    // ── Pass 2: Intensity decays, age and clock move on ──────────────────────
    {
        let defs = &vm.session.overlay_defs;
        for inst in vm.session.overlay_instances.iter_mut() {
            if let Some(def) = defs.get(&inst.overlay_name) {
                if def.decay_rate != 0.0 {
                    let s = inst.strength - def.decay_rate;
                    inst.strength = if s < 1e-10 { 0.0 } else { s.min(1.0) };
                }
            }
            inst.age += 1;
            if let Some(ref mut remaining) = inst.ticks_remaining {
                if *remaining > 0 { *remaining -= 1; }
            }
        }
    }
    // The film for this tick: `modify` lines that read the intensity or
    // the host are worked out again.
    refresh_films(vm)?;

    // ── Pass 3: The overlays' `every N ticks` blocks ─────────────────────────
    overlay_every(vm)?;

    // ── Pass 4: Transitions ───────────────────────────────────────────────────
    transition_pass(vm)?;

    // ── Pass 5: Overlays that ran out come off, then their `on end` runs ─────
    {
        let gone: Vec<usize> = vm.session.overlay_instances.iter().enumerate()
            .filter(|(_, i)| i.strength <= 0.0 || i.ticks_remaining == Some(0))
            .map(|(p, _)| p)
            .collect();
        let taken = vm.session.overlay_instances.take_at(&gone);
        for inst in &taken { forget_overlay(vm, inst); }
        settle_films(&mut vm.session);
        // In the order the overlays were put on. An error stops the tick.
        for inst in &taken {
            run_overlay_block(vm, inst, "on end")?;
        }
    }

    // ── Pass 6: Remove orphaned overlays ─────────────────────────────────────
    {
        let orphans: Vec<(String, String, OverlayInstanceId)> = vm.session.overlay_instances.iter()
            .filter(|inst| !vm.session.object_store.contains_key(&inst.host_uuid))
            .map(|inst| (inst.host_var.clone(), inst.overlay_name.clone(), inst.des_id))
            .collect();
        for (host_var, overlay_name, des_id) in &orphans {
            if let Some(hh) = vm.session.des_store.handle_for_name(host_var) {
                vm.session.des_index.remove_overlay(hh, overlay_name, *des_id);
            }
        }
        if !orphans.is_empty() {
            let store = &vm.session.object_store;
            vm.session.overlay_instances.retain(|inst| store.contains_key(&inst.host_uuid));
            settle_films(&mut vm.session);
        }
    }

    // ── Pass 7: `every N ticks` blocks, then decisions ────────────────────────
    every_tick(vm)?;
    decision_tick(vm)?;

    // ── Pass 8: Object transition tick ────────────────────────────────────────
    object_transition_tick(vm)?;

    Ok(())
}

/// Takes an overlay that came off out of the DES index.
fn forget_overlay(vm: &mut Vm, inst: &OverlayInstance) {
    if let Some(hh) = vm.session.des_store.handle_for_name(&inst.host_var) {
        vm.session.des_index.remove_overlay(hh, &inst.overlay_name, inst.des_id);
    }
}

/// Runs one of an overlay's blocks (`on start`, `on end`, `every N ticks
/// #k`) for this copy: `self` is the host, then `intensity`, then the
/// overlay's own fields. A host that is gone runs nothing.
fn run_overlay_block(vm: &mut Vm, inst: &OverlayInstance, block: &str) -> Result<(), GoblinError> {
    let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name) else { return Ok(()) };
    let Some(f) = def.methods.get(block).cloned() else { return Ok(()) };
    if !vm.session.object_store.contains_key(&inst.host_uuid) { return Ok(()); }
    let mut args = Vec::with_capacity(2 + inst.extra_fields.len());
    args.push(Value::Ref(inst.host_uuid.clone().into()));
    args.push(Value::Float(inst.strength));
    for k in def.extra_fields.keys() {
        args.push(inst.extra_fields.get(k).cloned().unwrap_or(Value::Nil));
    }
    with_erase_check(vm, |vm| vm.call_callable(Value::Function(f), args).map(|_| ()))
}

/// Each overlay's `every N ticks` blocks, on the ticks of its own age that
/// N divides, in the order the overlays were put on. Another region's
/// host runs them in its own region.
fn overlay_every(vm: &mut Vm) -> Result<(), GoblinError> {
    if vm.session.overlay_defs.values().all(|d| d.every.is_empty()) { return Ok(()); }
    let mut due: Vec<(usize, crate::session::OverlayInstanceId)> = Vec::new();
    {
        let defs = &vm.session.overlay_defs;
        for (p, i) in vm.session.overlay_instances.iter().enumerate() {
            if i.strength <= 0.0 { continue; }
            let Some(d) = defs.get(&i.overlay_name) else { continue };
            if d.every.iter().any(|(n, _)| i.age as u64 % *n == 0) && !crate::world::is_remote(vm, &i.host_uuid) {
                due.push((p, i.des_id));
            }
        }
    }
    for (p, id) in due {
        // The list may have changed since: find the copy again by its id.
        let list = &vm.session.overlay_instances;
        let p = if p < list.len() && list[p].des_id == id { p } else {
            match list.iter().position(|i| i.des_id == id) { Some(p) => p, None => continue }
        };
        let inst = list[p].clone();
        let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name).cloned() else { continue };
        for (n, block) in &def.every {
            if inst.age as u64 % *n != 0 { continue; }
            // Taken off by an earlier block this tick: nothing more to run.
            if !vm.session.overlay_instances.on_host(&inst.host_uuid).iter()
                .any(|&q| vm.session.overlay_instances[q].des_id == inst.des_id) { break; }
            run_overlay_block(vm, &inst, block)?;
        }
    }
    Ok(())
}

/// A `modify` amount written as a plain number (`3`, `-2.5`), read here
/// without running it.
fn literal_amount(e: &goblin_ast::Expr) -> Option<f64> {
    use goblin_ast::Expr as E;
    match e {
        E::Number(t, _) if t.bytes().all(|b| b.is_ascii_digit() || b == b'.' || b == b'_') => t.replace('_', "").parse().ok(),
        E::Prefix(op, inner, _) if op == "-" => literal_amount(inner).map(|n| -n),
        _ => None,
    }
}

/// Whether a `modify` line's amount never changes (a number).
fn constant_amount(e: &goblin_ast::Expr) -> bool {
    use goblin_ast::Expr as E;
    match e {
        E::Number(..) => true,
        E::Prefix(op, inner, _) if op == "-" => constant_amount(inner),
        _ => false,
    }
}

/// What the `modify` lines of the overlay at `at` add to each field of its
/// host now. They see `self` (the host, as stored: the film is off while
/// they are worked out), `intensity` and the overlay's own fields.
fn modify_amounts(vm: &mut Vm, at: usize) -> Result<Vec<(String, f64)>, GoblinError> {
    let inst = &vm.session.overlay_instances[at];
    let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name).cloned() else { return Ok(Vec::new()) };
    if def.modifiers.is_empty() { return Ok(Vec::new()); }
    let host = inst.host_uuid.clone();
    let strength = inst.strength;
    let extra = inst.extra_fields.clone();
    let name = &def.name;
    let mut out = Vec::with_capacity(def.modifiers.len());
    for (field, expr) in &def.modifiers {
        let fail = |what: String| GoblinError::Runtime(format!(
            "R1221: overlay-modify: overlay {name}, `@{field}`: {what}"));
        match vm.session.object_store.get(&host) {
            Some(Value::Object(o)) => match o.fields.get(field) {
                Some(Value::Int(_) | Value::Float(_)) => {}
                Some(other) => return Err(fail(format!("{} holds {}, not a number", o.class_name, other.type_name()))),
                None => return Err(fail(format!("{} has no field named {field}", o.class_name))),
            },
            _ => return Ok(Vec::new()),
        }
        let amount = if let Some(n) = literal_amount(expr) {
            Ok(Value::Float(n))
        } else if constant_amount(expr) {
            vm.eval_tick_expr(expr, Vec::new())
        } else {
            let mut bind: Vec<(&str, Value)> = vec![
                ("self", Value::Ref(host.clone().into())),
                ("intensity", Value::Float(strength)),
            ];
            for (k, v) in &extra { bind.push((k.as_str(), v.clone())); }
            vm.eval_tick_expr(expr, bind)
        };
        match amount {
            Ok(Value::Int(i)) => out.push((field.clone(), i as f64)),
            Ok(Value::Float(f)) => out.push((field.clone(), f)),
            Ok(other) => return Err(fail(format!("gave {}, not a number", other.type_name()))),
            Err(e) => return Err(fail(format!("failed: {e}"))),
        }
    }
    Ok(out)
}

/// Works out again the `modify` amounts that read the intensity or the
/// host (the film is taken off the host while they are), then puts the
/// films back on.
fn refresh_films(vm: &mut Vm) -> Result<(), GoblinError> {
    let live: Vec<usize> = {
        let defs = &vm.session.overlay_defs;
        vm.session.overlay_instances.iter().enumerate()
            .filter(|(_, i)| defs.get(&i.overlay_name).is_some_and(|d| d.modifiers.iter().any(|(_, e)| !constant_amount(e))))
            .filter(|(_, i)| !crate::world::is_remote(vm, &i.host_uuid))
            .map(|(p, _)| p)
            .collect()
    };
    if live.is_empty() { return Ok(()); }
    let mut hosts: Vec<String> = live.iter().map(|&p| vm.session.overlay_instances[p].host_uuid.clone()).collect();
    hosts.sort_unstable();
    hosts.dedup();
    for h in &hosts { film_off(&mut vm.session, h); }
    for p in live {
        let amounts = modify_amounts(vm, p)?;
        let host = vm.session.overlay_instances[p].host_uuid.clone();
        vm.session.overlay_instances[p].applied = amounts;
        vm.session.overlay_instances.mark(&host);
    }
    settle_films(&mut vm.session);
    Ok(())
}

/// Takes the film off a host for now (its fields read as stored).
fn film_off(s: &mut crate::session::Session, host: &str) {
    let Some(Value::Object(o)) = s.object_store.get_mut(host) else { return };
    if !o.fields.has_film() { return; }
    let traits = o.trait_fields.clone();
    std::rc::Rc::make_mut(&mut o.fields).set_film(&[], &|n| traits.contains(n));
    s.overlay_instances.mark(host);
}

/// Puts on each host whose overlays changed the film they make together:
/// reading a field gives what it stores plus what its overlays' `modify`
/// lines add. The stored values never change.
pub(crate) fn settle_films(s: &mut crate::session::Session) {
    if !s.overlay_instances.has_dirty() { return; }
    for host in s.overlay_instances.take_dirty() {
        put_film(s, &host, false);
    }
}

/// Hosts that came from another session with their overlays: their fields
/// already read through the film, so the stored values are worked out
/// from them.
pub(crate) fn adopt_films(s: &mut crate::session::Session, hosts: &[String]) {
    for h in hosts {
        s.overlay_instances.unmark(h);
        put_film(s, h, true);
    }
}

fn put_film(s: &mut crate::session::Session, host: &str, adopt: bool) {
    let mut sum: Vec<(&str, f64)> = Vec::new();
    for &p in s.overlay_instances.on_host(host) {
        for (f, d) in &s.overlay_instances[p].applied {
            match sum.iter_mut().find(|(n, _)| *n == f.as_str()) {
                Some((_, t)) => *t += d,
                None => sum.push((f.as_str(), *d)),
            }
        }
    }
    let Some(Value::Object(o)) = s.object_store.get(host) else { return };
    if sum.is_empty() && !o.fields.has_film() { return; }
    let adds: Vec<(usize, f64)> = sum.iter().filter_map(|(f, d)| o.fields.get_index_of(f).map(|i| (i, *d))).collect();
    if !adopt { s.touch(host); }
    let Some(Value::Object(o)) = s.object_store.get_mut(host) else { return };
    let traits = o.trait_fields.clone();
    let fields = std::rc::Rc::make_mut(&mut o.fields);
    if adopt { fields.adopt_film(&adds, &|n| traits.contains(n)) } else { fields.set_film(&adds, &|n| traits.contains(n)) }
}

fn catches(vm: &mut Vm, chance: f64) -> bool {
    if chance >= 1.0 { return true; }
    if chance <= 0.0 { return false; }
    crate::builtins::rng_u01(&mut vm.session) < chance
}

/// `spread: chance where condition`: each tick, every object of the
/// overlay's `hosts:` classes for which the condition holds (it reads `@`,
/// the carrier, and `target`) catches the overlay with that chance, drawn
/// from the program's seeded random numbers. The chance may be a formula
/// of `@`, `target`, `intensity` and the overlay's own fields. A target
/// that already has the overlay is passed over unless it stacks. The new
/// copy starts at the overlay's `~intensity`.
fn spread_pass(vm: &mut Vm) -> Result<(), GoblinError> {
    if vm.session.overlay_defs.values().all(|d| d.spreads.is_empty()) { return Ok(()); }
    let carriers: Vec<OverlayInstance> = vm.session.overlay_instances.iter()
        .filter(|i| vm.session.overlay_defs.get(&i.overlay_name).is_some_and(|d| !d.spreads.is_empty()))
        .cloned()
        .collect();
    let mut caught: Vec<(String, String)> = Vec::new(); // (overlay, host)
    let mut candidate_lists: HashMap<Vec<String>, std::rc::Rc<Vec<(Value, usize)>>> = HashMap::default();
    let mut field_groups: HashMap<(Vec<String>, String), FieldGroups> = HashMap::default();

    for inst in &carriers {
        let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name).cloned() else { continue };
        let Some(self_at) = vm.session.object_store.get_index_of(&inst.host_uuid) else { continue };
        let self_ref = Value::Ref(inst.host_uuid.clone().into());
        let intensity = Value::Float(inst.strength);
        let stacks = matches!(def.apply_behavior, crate::session::OverlayApplyBehavior::Stacks { .. });
        let mut names: Vec<&str> = vec!["self", "target", "intensity"];
        names.extend(def.extra_fields.keys().map(String::as_str));
        let extras: Vec<Value> = def.extra_fields.keys().map(|k| inst.extra_fields.get(k).cloned().unwrap_or(Value::Nil)).collect();
        for rule in &def.spreads {
            let name = &def.name;
            let cond = vm.tick_formula(&rule.condition, &names)?;
            let fixed_chance = match &rule.chance {
                e if constant_amount(e) => match vm.eval_tick_expr(e, Vec::new())? {
                    Value::Int(i) => Some(i as f64),
                    Value::Float(f) => Some(f),
                    other => return Err(GoblinError::Runtime(format!(
                        "R1232: overlay-spread: overlay {name}: the spread chance gave {}, not a number", other.type_name()))),
                },
                _ => None,
            };
            if fixed_chance.is_some_and(|c| c <= 0.0) { continue; }
            let chance_f = match fixed_chance { None => Some(vm.tick_formula(&rule.chance, &names)?), Some(_) => None };
            let cond_parts = { let mut v = Vec::new(); conjuncts(&rule.condition, &mut v); v };
            // Only objects the overlay may go on (`hosts`), in the order
            // they were made. Nothing is added or erased while overlays
            // spread, so the list is made once per pass.
            let candidates = candidate_lists.entry(def.host_types.clone()).or_insert_with(|| {
                let store = &vm.session.object_store;
                let at: Vec<usize> = if def.host_types.is_empty() { store.places().collect() }
                    else { store.places_of(|c| def.host_types.iter().any(|h| h == c)) };
                std::rc::Rc::new(at.into_iter()
                    .filter_map(|i| store.get_index(i).map(|(u, _)| (Value::Ref(u.clone()), i)))
                    .collect())
            }).clone();
            // `(target >> town) == @town`: only the objects whose field
            // matches can pass (see `same_field`).
            let candidates = match same_field(vm, &rule.condition, &cond_parts, &def.host_types, &candidates, &mut field_groups, &self_ref, self_at) {
                Some(fewer) => fewer,
                None => candidates,
            };
            for (target_ref, target_at) in candidates.iter() {
                let target_at = *target_at;
                if target_at == self_at { continue; }
                let Value::Ref(target_uuid) = target_ref else { continue };
                if !stacks && (vm.session.overlay_instances.find(target_uuid, &inst.overlay_name).is_some()
                    || crate::world::remote_has_overlay(vm, &inst.overlay_name, target_uuid)) { continue; }
                let mut args: Vec<(&str, &Value)> = vec![("self", &self_ref), ("target", target_ref), ("intensity", &intensity)];
                for (k, v) in names[3..].iter().zip(&extras) { args.push((k, v)); }
                let fires = match vm.run_tick_formula_ref(&cond, &args, &[self_at, target_at])? {
                    Value::Bool(b) => b,
                    other => return Err(GoblinError::Runtime(format!(
                        "R1232: overlay-spread: overlay {name}: the spread condition gave {}, not true or false", other.type_name()))),
                };
                if !fires { continue; }
                let chance = match (&fixed_chance, &chance_f) {
                    (Some(c), _) => *c,
                    (None, Some(f)) => match vm.run_tick_formula_ref(f, &args, &[self_at, target_at])? {
                        Value::Int(i) => i as f64,
                        Value::Float(x) => x,
                        other => return Err(GoblinError::Runtime(format!(
                            "R1232: overlay-spread: overlay {name}: the spread chance gave {}, not a number", other.type_name()))),
                    },
                    _ => 0.0,
                };
                if catches(vm, chance) {
                    caught.push((inst.overlay_name.clone(), target_uuid.to_string()));
                }
            }
        }
    }

    // The new copies go on once every carrier has spread.
    for (name, host) in caught {
        let Some(def) = vm.session.overlay_defs.get(&name).cloned() else { continue };
        let host_var = host_name(vm, &host);
        let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
        vm.session.des_overlay_id_counter += 1;
        overlay_push(vm, OverlayInstance {
            overlay_name: name,
            host_var,
            host_uuid: host,
            strength: def.intensity,
            age: 0,
            ticks_remaining: def.default_duration,
            count: 1,
            applied: Vec::new(),
            extra_fields: def.extra_fields.clone(),
            des_id,
        })?;
    }
    Ok(())
}

use crate::store::{field_key, FieldKey};

/// The candidates grouped by one field's value, made once per spread pass
/// (nothing changes a field while overlays spread). `other`: those whose
/// value has no key (a list, an object), always checked.
struct FieldGroups {
    by: HashMap<FieldKey, std::rc::Rc<Vec<(Value, usize)>>>,
    other: Vec<(Value, usize)>,
}

/// For a `spreads where` condition that only reads and has a part
/// `target >> F == E` (E not naming `target`): the candidates whose `F`
/// can equal E for this spreader, in the order they were made. The others
/// fail that part (an object without `F` fails the condition too), so the
/// condition needn't run on them; nothing random is skipped, as the
/// condition only reads. None: every candidate.
#[allow(clippy::too_many_arguments)]
fn same_field(
    vm: &mut Vm,
    condition: &goblin_ast::Expr,
    parts: &[&goblin_ast::Expr],
    hosts: &[String],
    all: &[(Value, usize)],
    groups: &mut HashMap<(Vec<String>, String), FieldGroups>,
    self_ref: &Value,
    self_at: usize,
) -> Option<std::rc::Rc<Vec<(Value, usize)>>> {
    use goblin_ast::Expr as E;
    if !reads(vm, condition) { return None; }
    // `target >> is_nil` calls a builtin, it doesn't read a field.
    let field_of_target = |e: &E| match e {
        E::Member(o, f, _) if matches!(&**o, E::Ident(n, _) if n == "target")
            && crate::vm::member_builtin(f).is_none() => Some(f.clone()),
        _ => None,
    };
    let (field, other_side) = parts.iter().find_map(|c| {
        let E::Binary(l, op, r, _) = c else { return None };
        if op != "==" { return None; }
        match (field_of_target(l), field_of_target(r)) {
            (Some(f), _) if !names(r, "target") => Some((f, &**r)),
            (_, Some(f)) if !names(l, "target") => Some((f, &**l)),
            _ => None,
        }
    })?;
    let e = vm.tick_formula(other_side, &["self", "target"]).ok()?;
    // It is worked out once for every target, so it must give the same
    // answer each time (`self >> town >> shuffle` doesn't).
    if !e.reads_fields_only() { return None; }
    let want = vm.run_tick_formula_ref(&e, &[("self", self_ref), ("target", &Value::Nil)], &[self_at, usize::MAX]).ok()?;
    let key = field_key(&want)?;
    let g = groups.entry((hosts.to_vec(), field.clone())).or_insert_with(|| {
        let store = &vm.session.object_store;
        let mut by: HashMap<FieldKey, Vec<(Value, usize)>> = HashMap::default();
        let mut other = Vec::new();
        for (r, i) in all {
            let Some((_, Value::Object(o))) = store.get_index(*i) else { other.push((r.clone(), *i)); continue };
            match o.fields.get(field.as_str()) {
                // No such field: the condition fails (an error there is false).
                None => {}
                Some(v) => match field_key(v) {
                    Some(k) => by.entry(k).or_default().push((r.clone(), *i)),
                    None => other.push((r.clone(), *i)),
                },
            }
        }
        FieldGroups { by: by.into_iter().map(|(k, v)| (k, std::rc::Rc::new(v))).collect(), other }
    });
    let same = g.by.get(&key).cloned().unwrap_or_default();
    if g.other.is_empty() { return Some(same); }
    let mut both: Vec<(Value, usize)> = same.iter().chain(g.other.iter()).cloned().collect();
    both.sort_by_key(|(_, i)| *i);
    Some(std::rc::Rc::new(both))
}

// ── Spawn pass ────────────────────────────────────────────────────────────────

// ── Overlay transition pass ───────────────────────────────────────────────────

fn transition_pass(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::TransitionKind;
    let defs = vm.session.overlay_defs.clone();
    let instances_snap: Vec<OverlayInstance> = vm.session.overlay_instances.to_vec();

    let mut to_erase: Vec<usize> = Vec::new();
    let mut to_mutate: Vec<(usize, String, IndexMap<String, Value>)> = Vec::new();
    let mut to_spawn_new: Vec<OverlayInstance> = Vec::new();

    for (idx, inst) in instances_snap.iter().enumerate() {
        let def = match defs.get(&inst.overlay_name) { Some(d) => d, None => continue };
        if def.transitions.is_empty() { continue; }

        // Build self object for trigger evaluation
        let mut self_fields = crate::value::FieldMap::default();
        self_fields.insert("strength".to_string(), Value::Float(inst.strength));
        self_fields.insert("age".to_string(), Value::Int(inst.age as i64));
        self_fields.insert("overlay_name".to_string(), Value::Str(inst.overlay_name.clone().into()));
        self_fields.insert("host_uuid".to_string(), Value::Str(inst.host_uuid.clone().into()));
        self_fields.insert("host_var".to_string(), Value::Str(inst.host_var.clone().into()));
        for (k, v) in &inst.extra_fields {
            self_fields.insert(k.clone(), v.clone());
        }
        use std::collections::BTreeSet;
        let self_val = Value::Object(Box::new(crate::value::ObjectVal {
            class_name: inst.overlay_name.as_str().into(),
            fields: std::rc::Rc::new(self_fields),
            readonly_fields: Default::default(),
            trait_fields: Default::default(),
            uuid: inst.host_uuid.as_str().into(),
        }));

        for tdef in &def.transitions {
            let trigger_fired = vm.eval_tick_expr_bool(
                &tdef.trigger,
                vec![
                    ("self", self_val.clone()),
                    ("strength", Value::Float(inst.strength)),
                    ("age", Value::Int(inst.age as i64)),
                ],
            );
            if !trigger_fired { continue; }

            match tdef.kind {
                TransitionKind::Erase => {
                    to_erase.push(idx);
                    break;
                }
                TransitionKind::Mutate => {
                    let new_name = tdef.into_classes.first().cloned().unwrap_or(inst.overlay_name.clone());
                    let mut carries = IndexMap::new();
                    if let Some(succ) = tdef.successors.iter().find(|s| s.label == "carries") {
                        for (field, expr) in &succ.fields {
                            if let Ok(v) = vm.eval_tick_expr(expr, vec![("strength", Value::Float(inst.strength))]) {
                                carries.insert(field.clone(), v);
                            }
                        }
                    }
                    to_mutate.push((idx, new_name, carries));
                    break;
                }
                TransitionKind::Spawn => {
                    let child_name = tdef.into_classes.first().cloned().unwrap_or(inst.overlay_name.clone());
                    let mut child_strength = inst.strength * 0.4;
                    if let Some(succ) = tdef.successors.iter().find(|s| s.label == "child") {
                        for (field, expr) in &succ.fields {
                            if field == "strength" {
                                if let Ok(Value::Float(f)) = vm.eval_tick_expr(expr, vec![("strength", Value::Float(inst.strength))]) {
                                    child_strength = f;
                                }
                            }
                        }
                    }
                    if let Some(cdef) = defs.get(&child_name) {
                        let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
                        vm.session.des_overlay_id_counter += 1;
                        to_spawn_new.push(OverlayInstance {
                            overlay_name: child_name,
                            host_uuid: inst.host_uuid.clone(),
                            host_var: inst.host_var.clone(),
                            strength: child_strength.clamp(0.0, 1.0),
                            age: 0,
                            ticks_remaining: cdef.default_duration,
                            count: 1,
                            applied: Vec::new(),
                            extra_fields: cdef.extra_fields.clone(),
                            des_id,
                        });
                    }
                    break;
                }
                TransitionKind::Fracture => {
                    let frag_name = tdef.into_classes.first().cloned().unwrap_or(inst.overlay_name.clone());
                    let mut frag_strength = inst.strength * 0.2;
                    if let Some(succ) = tdef.successors.iter().find(|s| s.label == "fragment") {
                        for (field, expr) in &succ.fields {
                            if field == "strength" {
                                if let Ok(Value::Float(f)) = vm.eval_tick_expr(expr, vec![("strength", Value::Float(inst.strength))]) {
                                    frag_strength = f;
                                }
                            }
                        }
                    }
                    if let Some(fdef) = defs.get(&frag_name) {
                        let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
                        vm.session.des_overlay_id_counter += 1;
                        to_spawn_new.push(OverlayInstance {
                            overlay_name: frag_name,
                            host_uuid: inst.host_uuid.clone(),
                            host_var: inst.host_var.clone(),
                            strength: frag_strength.clamp(0.0, 1.0),
                            age: 0,
                            ticks_remaining: fdef.default_duration,
                            count: 1,
                            applied: Vec::new(),
                            extra_fields: fdef.extra_fields.clone(),
                            des_id,
                        });
                    }
                    break;
                }
                TransitionKind::Split => {
                    let name_a = tdef.into_classes.first().cloned().unwrap_or(inst.overlay_name.clone());
                    let name_b = tdef.into_classes.get(1).cloned().unwrap_or(inst.overlay_name.clone());
                    let mut str_a = inst.strength * 0.55;
                    let mut str_b = inst.strength * 0.45;
                    if let Some(succ) = tdef.successors.iter().find(|s| s.label == "first") {
                        for (f, e) in &succ.fields {
                            if f == "strength" {
                                if let Ok(Value::Float(v)) = vm.eval_tick_expr(e, vec![("strength", Value::Float(inst.strength))]) { str_a = v; }
                            }
                        }
                    }
                    if let Some(succ) = tdef.successors.iter().find(|s| s.label == "second") {
                        for (f, e) in &succ.fields {
                            if f == "strength" {
                                if let Ok(Value::Float(v)) = vm.eval_tick_expr(e, vec![("strength", Value::Float(inst.strength))]) { str_b = v; }
                            }
                        }
                    }
                    for (name, strength) in [(&name_a, str_a), (&name_b, str_b)] {
                        if let Some(sdef) = defs.get(name) {
                            let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
                            vm.session.des_overlay_id_counter += 1;
                            to_spawn_new.push(OverlayInstance {
                                overlay_name: name.clone(),
                                host_uuid: inst.host_uuid.clone(),
                                host_var: inst.host_var.clone(),
                                strength: strength.clamp(0.0, 1.0),
                                age: 0,
                                ticks_remaining: sdef.default_duration,
                                count: 1,
                                applied: Vec::new(),
                                extra_fields: sdef.extra_fields.clone(),
                                des_id,
                            });
                        }
                    }
                    to_erase.push(idx);
                    break;
                }
                TransitionKind::Absorb => {
                    if let Some(ref tc) = tdef.target_class {
                        let target_idx = instances_snap.iter().enumerate()
                            .find(|(tidx, ti)| *tidx != idx && &ti.overlay_name == tc && ti.host_uuid != inst.host_uuid)
                            .map(|(tidx, _)| tidx);
                        if let Some(tidx) = target_idx {
                            let new_strength = (inst.strength + instances_snap[tidx].strength * 0.6).min(1.0);
                            let mut m = inst.extra_fields.clone();
                            m.insert("strength".to_string(), Value::Float(new_strength));
                            to_mutate.push((idx, inst.overlay_name.clone(), m));
                            to_erase.push(tidx);
                            break;
                        }
                    }
                }
                TransitionKind::Merge => {
                    if let Some(ref tc) = tdef.target_class {
                        let target_idx = instances_snap.iter().enumerate()
                            .find(|(tidx, ti)| *tidx != idx && &ti.overlay_name == tc && ti.host_uuid != inst.host_uuid)
                            .map(|(tidx, _)| tidx);
                        if let Some(tidx) = target_idx {
                            let new_name = tdef.into_classes.first().cloned().unwrap_or(inst.overlay_name.clone());
                            let new_strength = (inst.strength + instances_snap[tidx].strength * 0.8).min(1.0);
                            if let Some(mdef) = defs.get(&new_name) {
                                let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
                                vm.session.des_overlay_id_counter += 1;
                                to_spawn_new.push(OverlayInstance {
                                    overlay_name: new_name,
                                    host_uuid: inst.host_uuid.clone(),
                                    host_var: inst.host_var.clone(),
                                    strength: new_strength,
                                    age: 0,
                                    ticks_remaining: mdef.default_duration,
                                    count: 1,
                                    applied: Vec::new(),
                                    extra_fields: mdef.extra_fields.clone(),
                                    des_id,
                                });
                            }
                            to_erase.push(idx);
                            to_erase.push(tidx);
                            break;
                        }
                    }
                }
                TransitionKind::Subjugate => {
                    if let Some(ref tc) = tdef.target_class {
                        let target_idx = instances_snap.iter().enumerate()
                            .find(|(tidx, ti)| *tidx != idx && &ti.overlay_name == tc && ti.host_uuid != inst.host_uuid)
                            .map(|(tidx, _)| tidx);
                        if let Some(tidx) = target_idx {
                            if tidx < vm.session.overlay_instances.len() {
                                vm.session.overlay_instances[tidx].strength =
                                    (vm.session.overlay_instances[tidx].strength * 0.5).max(0.0);
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    // Apply mutations: the overlay becomes another, and its film is that one's.
    for (idx, new_name, carries) in to_mutate {
        if idx < vm.session.overlay_instances.len() {
            vm.session.overlay_instances.rename(idx, new_name);
            for (k, v) in carries {
                if k == "strength" || k == "intensity" {
                    if let Value::Float(f) = v {
                        vm.session.overlay_instances[idx].strength = f.clamp(0.0, 1.0);
                    }
                } else {
                    vm.session.overlay_instances[idx].extra_fields.insert(k, v);
                }
            }
            let amounts = modify_amounts(vm, idx)?;
            vm.session.overlay_instances[idx].applied = amounts;
        }
    }
    let mut to_erase_dedup = to_erase;
    to_erase_dedup.sort_unstable();
    to_erase_dedup.dedup();
    for gone in vm.session.overlay_instances.take_at(&to_erase_dedup) { forget_overlay(vm, &gone); }
    settle_films(&mut vm.session);
    for new_inst in to_spawn_new {
        overlay_push(vm, new_inst)?;
    }

    Ok(())
}

// ── Decision tick ─────────────────────────────────────────────────────────────

/// Runs each class's `every N ticks` blocks on its objects, in the order
/// the objects were made, on the ticks N divides. Another region's object
/// runs them in its own region. An error in a block stops the tick.
fn every_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    let tick = vm.session.des_ticks;
    let classes = vm.session.classes.clone();
    if classes.values().all(|c| c.every.is_empty()) { return Ok(()); }
    let store = &vm.session.object_store;
    let selves: Vec<(crate::value::ObjId, String)> = store.places_of(|c| classes.get(c).is_some_and(|c| !c.every.is_empty()))
        .into_iter()
        .filter_map(|i| match store.get_index(i) {
            Some((u, Value::Object(o))) => Some((u.clone(), o.class_name[..].to_string())),
            _ => None,
        })
        .filter(|(u, _)| !crate::world::is_remote(vm, u))
        .collect();
    for (uuid, class_name) in selves {
        let Some(class) = classes.get(&class_name) else { continue };
        for e in &class.every {
            if tick % e.ticks as i64 != 0 { continue; }
            // Erased by an earlier block this tick: nothing to run.
            if !vm.session.object_store.contains_key(&uuid) { break; }
            let Some(f) = vm.session.compiled_methods.get(&(class_name.clone(), e.action.clone())).cloned() else { continue };
            let me = Value::Ref(uuid.clone().into());
            with_erase_check(vm, |vm| vm.call_callable(Value::Function(f), vec![me]).map(|_| ()))?;
        }
    }
    Ok(())
}

fn decision_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    // Find all objects with a decision formula
    let candidates: Vec<(crate::value::ObjId, String)> = {
        // The objects of classes with a decision, and objects given one of
        // their own, from the store's class lists, in creation order.
        let store = &vm.session.object_store;
        let mut at = store.places_of(|c| vm.session.classes.get(c).is_some_and(|c| c.decision.is_some()));
        for uuid in vm.session.object_decisions.keys() {
            if let Some(i) = store.get_index_of(uuid) { at.push(i); }
        }
        at.sort_unstable();
        at.dedup();
        let mut out = Vec::with_capacity(at.len());
        for i in at {
            let Some((uuid, Value::Object(o))) = store.get_index(i) else { continue };
            // Another region's object decides in its own region.
            if !crate::world::is_remote(vm, uuid) {
                out.push((uuid.clone(), o.class_name[..].to_string()));
            }
        }
        out
    };
    let candidates = order_candidates(vm, candidates)?;

    let mut targets = Targets::default();
    vm.memo_kept = None;
    let out = decide_all(vm, &mut targets, candidates);
    vm.memo_kept = None;
    out
}

fn decide_all(vm: &mut Vm, targets: &mut Targets, candidates: Vec<(crate::value::ObjId, String)>) -> Result<(), GoblinError> {
    // The judge method of the class decided last (most objects in a row
    // are of one class), so it is not looked up again for each object.
    let mut judge_fn: Option<(String, Option<std::rc::Rc<crate::value::FunctionObject>>)> = None;
    for (self_uuid, class_name) in candidates {
        // Borrow the definitions (an Rc copy, not a deep one), so each
        // formula keeps its address for the tick-expr cache. Taken per
        // object, so a class an earlier action installed is seen here.
        let classes = vm.session.classes.clone();
        let Some(class) = classes.get(&class_name) else { continue };
        // `judge` over block decisions: the class's judge (a method) runs,
        // and each decision picks the first time the judge reads it.
        if let Some(action) = &class.decide {
            let prev = vm.deciding.replace(Box::new(Deciding {
                self_uuid: self_uuid.clone(),
                class: classes.clone(),
                class_name: class_name.clone(),
                picks: vec![None; class.decisions.len()],
                targets: std::mem::take(targets),
            }));
            if !judge_fn.as_ref().is_some_and(|(c, _)| *c == class_name) {
                let f = vm.session.compiled_methods.get(&(class_name.clone(), action.clone())).cloned();
                judge_fn = Some((class_name.clone(), f));
            }
            let out = match judge_fn.as_ref().and_then(|(_, f)| f.clone()) {
                // Erased earlier this tick: it decides nothing.
                _ if !vm.session.object_store.contains_key(&self_uuid) => Ok(Value::Nil),
                Some(f) => {
                    let me = vec![Value::Ref(self_uuid.clone())];
                    with_erase_check(vm, |vm| vm.call_callable(Value::Function(f), me).map(|_| ())).map(|_| Value::Nil)
                }
                None => action_result(vm, &self_uuid, action, Vec::new()),
            };
            if let Some(d) = std::mem::replace(&mut vm.deciding, prev) { *targets = d.targets; }
            // What the decisions worked out stays good for the next object
            // unless a deed ran (it clears it) or a judge condition may
            // change things.
            if !judge_reads_only(vm, class, action) { vm.memo_kept = None; }
            out?;
            continue;
        }
        let obj_decision = object_decision(&vm.session, &self_uuid);
        // The object's decisions in order, each with its judge and the name
        // its score goes by. An object-level decision replaces the first.
        let mut list: Vec<(&str, &goblin_ast::DecisionDef, &goblin_ast::JudgeStmt)> = Vec::new();
        if class.decisions.is_empty() {
            let decision = match (&obj_decision, &class.decision) {
                (Some(d), _) => d,
                (None, Some(d)) => d,
                (None, None) => continue,
            };
            let Some(judge) = &class.judge else { continue };
            list.push(("score", decision, judge));
        } else {
            for (i, nd) in class.decisions.iter().enumerate() {
                let Some(judge) = &nd.judge else { continue };
                let d = match (&obj_decision, i) { (Some(d), 0) => d, _ => &nd.decision };
                list.push((nd.name.as_str(), d, judge));
            }
        }
        // `one of heal, attack`: those decisions are tried in that order
        // and the first whose judge runs an action ends the group, so the
        // others in it don't run this tick. An action that returns false
        // didn't act, and the next one is tried.
        let group_of = |n: &str| class.one_of.iter().position(|g| g.iter().any(|m| m == n));
        let mut done_groups: Vec<usize> = Vec::new();
        for &(name, decision, judge) in &list {
            // Erased by an earlier decision's action: nothing more to decide.
            if !vm.session.object_store.contains_key(&self_uuid) { break; }
            let Some(g) = group_of(name) else {
                decide(vm, targets, &self_uuid, name, decision, judge)?;
                continue;
            };
            if done_groups.contains(&g) { continue; }
            done_groups.push(g);
            for member in &class.one_of[g] {
                if !vm.session.object_store.contains_key(&self_uuid) { break; }
                let Some(&(n, d, j)) = list.iter().find(|(n, _, _)| n == member) else { continue };
                if decide(vm, targets, &self_uuid, n, d, j)? { break; }
            }
        }
    }

    Ok(())
}

/// `order by [ expr ] desc`: the objects of such a class decide in that
/// order (ascending unless `desc`; ties keep the order they were made in).
/// They take the places the class's objects had in the pass, so other
/// classes keep theirs. Keys are worked out once, at the start of the pass.
fn order_candidates(vm: &mut Vm, mut candidates: Vec<(crate::value::ObjId, String)>) -> Result<Vec<(crate::value::ObjId, String)>, GoblinError> {
    let classes = vm.session.classes.clone();
    let mut ordered: Vec<String> = Vec::new();
    for (_, c) in &candidates {
        if !ordered.contains(c) && classes.get(c).is_some_and(|d| d.order_by.is_some()) { ordered.push(c.clone()); }
    }
    for class_name in &ordered {
        let Some((key, desc)) = classes.get(class_name).and_then(|d| d.order_by.as_ref()) else { continue };
        let slots: Vec<usize> = candidates.iter().enumerate()
            .filter(|(_, (_, c))| c == class_name).map(|(i, _)| i).collect();
        let fail = |what: String| GoblinError::Runtime(format!("R1222: decision-order: `order by` of {class_name}: {what}"));
        let mut keyed: Vec<(Value, crate::value::ObjId)> = Vec::with_capacity(slots.len());
        for &i in &slots {
            let uuid = candidates[i].0.clone();
            let k = vm.eval_tick_expr(key, vec![("self", Value::Ref(uuid.clone()))])
                .map_err(|e| fail(format!("failed: {e}")))?;
            match k {
                Value::Int(_) | Value::Float(_) | Value::Str(_) => keyed.push((k, uuid)),
                other => return Err(fail(format!("gave {}, not a number or a string", other.type_name()))),
            }
        }
        let num = |v: &Value| match v { Value::Int(i) => Some(*i as f64), Value::Float(f) => Some(*f), _ => None };
        if keyed.iter().any(|(k, _)| num(k).is_some()) && keyed.iter().any(|(k, _)| num(k).is_none()) {
            return Err(fail("gave numbers for some objects and strings for others".into()));
        }
        // A stable sort, so equal keys keep the order the objects were made in.
        keyed.sort_by(|(a, _), (b, _)| {
            let o = match (num(a), num(b), a, b) {
                // Whole numbers compare exactly (past 2^53 a float can't tell them apart).
                (_, _, Value::Int(x), Value::Int(y)) => x.cmp(y),
                (Some(x), Some(y), _, _) => x.partial_cmp(&y).unwrap_or(std::cmp::Ordering::Equal),
                (_, _, Value::Str(x), Value::Str(y)) => AsRef::<str>::as_ref(x).cmp(AsRef::<str>::as_ref(y)),
                _ => std::cmp::Ordering::Equal,
            };
            if *desc { o.reverse() } else { o }
        });
        for (slot, (_, uuid)) in slots.into_iter().zip(keyed) {
            candidates[slot].0 = uuid;
        }
    }
    Ok(candidates)
}

/// What a decision needs that its definition alone settles: its formula and
/// `where` condition compiled, and, for the formula, the condition and the
/// judge's conditions, the actions they call when they otherwise only read
/// (None: they don't only read). Made once per decision while the class
/// table stays; whether those actions only read is looked at each time
/// (`calls_read`), since a variable can be given another action.
pub(crate) struct DecidePlan {
    formula: std::rc::Rc<crate::vm::TickFormula>,
    filter: Option<std::rc::Rc<crate::vm::TickFormula>>,
    formula_calls: Option<Vec<String>>,
    filter_calls: Option<Vec<String>>,
    judge_calls: Option<Vec<String>>,
}

fn decide_plan(vm: &mut Vm, decision: &goblin_ast::DecisionDef, judge: Option<&goblin_ast::JudgeStmt>) -> Result<std::rc::Rc<DecidePlan>, String> {
    // Kept while the class table is the one they were made from; the
    // table is shared with this cache, so its decisions keep their
    // addresses. A decision given to one object is told apart by where
    // its formula is written.
    let classes = vm.session.classes.clone();
    if !vm.decide_classes.as_ref().is_some_and(|c| std::rc::Rc::ptr_eq(c, &classes)) {
        vm.decide_plans.clear();
        vm.decide_classes = Some(classes);
    }
    let sp = decision.formula.span();
    let key = (decision as *const _ as usize, judge.map_or(0, |j| j as *const _ as usize), sp.start, sp.end, sp.file.as_ptr() as usize);
    if let Some(p) = vm.decide_plans.get(&key) { return Ok(p.clone()); }
    let formula = vm.tick_formula(&decision.formula, &["self", "target"])
        .map_err(|e| format!("the formula can't be compiled: {e}"))?;
    let filter = match &decision.filter {
        Some(f) => Some(vm.tick_formula(f, &["self", "target"])
            .map_err(|e| format!("the `where` condition can't be compiled: {e}"))?),
        None => None,
    };
    let filter_calls = match &decision.filter { Some(f) => crate::compiler::read_calls(f), None => Some(Vec::new()) };
    let mut judge_calls = Some(Vec::new());
    for c in judge.iter().flat_map(|j| j.arms.iter()).filter_map(|a| a.condition.as_ref()) {
        judge_calls = match (judge_calls, crate::compiler::read_calls(c)) {
            (Some(mut a), Some(b)) => { a.extend(b); Some(a) }
            _ => None,
        };
    }
    let plan = std::rc::Rc::new(DecidePlan {
        formula, filter, formula_calls: crate::compiler::read_calls(&decision.formula), filter_calls, judge_calls,
    });
    if vm.decide_plans.len() >= 4096 { vm.decide_plans.clear(); }
    vm.decide_plans.insert(key, plan.clone());
    Ok(plan)
}

/// Whether an expression only reads, from its `DecidePlan` entry: it does
/// apart from these actions, and each of them only reads.
fn calls_read(vm: &mut Vm, calls: &Option<Vec<String>>) -> bool {
    calls.as_ref().is_some_and(|c| c.iter().all(|n| vm.action_reads_only(n)))
}

/// One decision of one object: score the targets, pick the best (within
/// the object's `margin`), and run the action its judge picks.
/// The objects of each target class, in the order they were made, kept
/// across one decision pass while no object is added or removed. A store
/// that only grows at its end and keeps its order on removal changes its
/// length or its last key whenever its set of objects changes.
#[derive(Default)]
struct Targets {
    seen: Option<(usize, crate::value::ObjId)>,
    /// A `Value::Ref` to each object, with where it sits in the store.
    by_class: HashMap<String, std::rc::Rc<Vec<(Value, usize)>>>,
}

impl Targets {
    fn of(&mut self, vm: &Vm, class: &str) -> std::rc::Rc<Vec<(Value, usize)>> {
        let store = &vm.session.object_store;
        let now = (store.len(), store.last().map(|(k, _)| k.clone()).unwrap_or_default());
        if self.seen.as_ref() != Some(&now) {
            self.by_class.clear();
            self.seen = Some(now);
        }
        self.by_class.entry(class.to_string()).or_insert_with(|| std::rc::Rc::new(
            store.of_class_ids(class)
                .filter_map(|u| store.get_index_of(u).map(|i| (Value::Ref(u.clone()), i)))
                .collect()
        )).clone()
    }
}

/// The decision written for one object (`x score | decision against ...`),
/// kept under the id of the object `x` held when it was declared.
fn object_decision(session: &crate::session::Session, uuid: &str) -> Option<std::rc::Rc<goblin_ast::DecisionDef>> {
    session.object_decisions.get(uuid).cloned()
}

/// The conjuncts of `a and b and c`, in order.
fn conjuncts<'a>(e: &'a goblin_ast::Expr, out: &mut Vec<&'a goblin_ast::Expr>) {
    match e {
        goblin_ast::Expr::Binary(l, op, r, _) if op == "and" || op == "&&" => { conjuncts(l, out); conjuncts(r, out); }
        other => out.push(other),
    }
}

/// Whether a read-only expression names `who`.
fn names(e: &goblin_ast::Expr, who: &str) -> bool {
    use goblin_ast::Expr as E;
    match e {
        E::Ident(n, _) => n == who,
        E::Binary(l, _, r, _) | E::Index(l, r, _) | E::IndexMap(l, r, _) => names(l, who) || names(r, who),
        E::Prefix(_, x, _) | E::Member(x, _, _) | E::OptMember(x, _, _) => names(x, who),
        E::Array(xs, _) | E::FreeCall(_, xs, _) => xs.iter().any(|x| names(x, who)),
        _ => false,
    }
}

/// Whether `e` only reads: what `only_reads` allows, and calls to the
/// program's actions that only read (memo.rs).
fn reads(vm: &mut Vm, e: &goblin_ast::Expr) -> bool {
    crate::compiler::only_reads_with(e, &mut |name| vm.action_reads_only(name))
}

/// In a `where` condition, the candidates that can pass, found without
/// running the condition on every object of the target class. None: all of
/// them. It looks at the conditions joined by `and`, from the first, while
/// they only read (nothing random, calls only to actions that only read):
/// - one that doesn't name `target` (`self >> gold < 0`) is the same for
///   every target: run once; false leaves no candidates.
/// - `target >> owner == x` with `x` not naming `target`: only what `x`
///   holds (its contents list) can pass; `target >> owner >> owner == x`:
///   what the things `x` holds hold, and so on.
/// - `target >> f == x` with `x` not naming `target` and `f` a field the
///   class declares: only the objects whose `f` can equal `x` (the store's
///   index of `f`) can pass.
/// The targets left out would have failed the condition at or before that
/// point, and what came before changes nothing, so the result is the same.
/// It stops at the first condition that names `target` and isn't one of
/// those tests. The caller uses it only when scoring changes nothing
/// either (so who holds what stays as it was while the targets are scored).
fn narrow(vm: &mut Vm, filter: &goblin_ast::Expr, filter_reads: bool, class: &str, self_ref: &Value, self_at: usize, all: &[(Value, usize)]) -> Option<Vec<(Value, usize)>> {
    use goblin_ast::Expr as E;
    if !all.iter().any(|(t, _)| t != self_ref) { return None; }
    let mut parts = Vec::new();
    conjuncts(filter, &mut parts);
    let epoch = vm.memo.as_ref().map(|m| m.epoch);
    let changed = |vm: &Vm, e: Option<u64>| vm.memo.as_ref().map(|m| m.epoch) != e;
    for c in parts {
        // A part of a condition that only reads only reads.
        if !filter_reads && !reads(vm, c) { return None; }
        if !names(c, "target") {
            let f = vm.tick_formula(c, &["self", "target"]).ok()?;
            let r = vm.run_tick_formula_ref(&f, &[("self", self_ref), ("target", &Value::Nil)], &[self_at, usize::MAX]);
            // Something it called turned out to change things: check every target.
            if changed(vm, epoch) { return None; }
            match r {
                Ok(Value::Bool(true)) => continue,
                Ok(Value::Bool(false)) => return Some(Vec::new()),
                _ => return None,
            }
        }
        // The first condition that names `target` must be the ownership
        // test: a target it leaves out would have stopped there, before any
        // later condition could run (or fail with an error).
        let E::Binary(l, op, r, _) = c else { return None };
        if op != "==" { return None; }
        // `target >> owner` is 1, `target >> owner >> owner` 2, ...
        fn owner_depth(e: &E) -> usize {
            match e {
                E::Member(o, f, _) if f == crate::store::OWNER_FIELD => match &**o {
                    E::Ident(n, _) if n == "target" => 1,
                    inner => match owner_depth(inner) { 0 => 0, d => d + 1 },
                },
                _ => 0,
            }
        }
        let owner_of_target = |e: &E| owner_depth(e) > 0;
        // `target >> meadow == self >> meadow`: the objects whose field
        // can match, from the store's index of that field. The field must be
        // one the class declares, so no object left out lacks it (reading a
        // missing field is an error, which must still stop the tick).
        let field_of_target = |e: &E| match e {
            E::Member(o, f, _) if f != crate::store::OWNER_FIELD && crate::vm::member_builtin(f).is_none()
                && matches!(&**o, E::Ident(n, _) if n == "target") => Some(f.clone()),
            _ => None,
        };
        let same_field = match (field_of_target(l), field_of_target(r)) {
            (Some(f), _) if !names(r, "target") => Some((f, &**r)),
            (_, Some(f)) if !names(l, "target") => Some((f, &**l)),
            _ => None,
        };
        if let Some((field, other)) = same_field {
            let declared = vm.session.classes.get(class)
                .is_some_and(|c| c.fields.iter().any(|f| f.name == field));
            if !declared || all.len() < crate::store::ObjectStore::INDEX_FROM { return None; }
            let f = vm.tick_formula(other, &["self", "target"]).ok()?;
            // Worked out once for every target: no `>> shuffle` and the like.
            if !f.reads_fields_only() { return None; }
            let want = vm.run_tick_formula_ref(&f, &[("self", self_ref), ("target", &Value::Nil)], &[self_at, usize::MAX]);
            if changed(vm, epoch) { return None; }
            let key = crate::store::field_key(&want.ok()?)?;
            let store = &mut vm.session.object_store;
            let at = store.with_field(class, &field, &key);
            return Some(at.into_iter()
                .filter_map(|i| store.get_index(i).map(|(u, _)| (Value::Ref(u.clone()), i)))
                .collect());
        }
        let (holder, depth) = if owner_of_target(l) && !names(r, "target") { (r, owner_depth(l)) }
            else if owner_of_target(r) && !names(l, "target") { (l, owner_depth(r)) }
            else { return None };
        // `target >> owner >> owner == x`: reading through a holder that is
        // nil, erased or has no `owner` field is an error, which must still
        // stop the tick: only when every target's chain is whole.
        if depth > 1 {
            let store = &vm.session.object_store;
            for (t, _) in all {
                if t == self_ref { continue; }
                let Value::Ref(u) = t else { return None };
                let mut cur: &crate::value::ObjId = u;
                for _ in 0..depth - 1 {
                    let h = store.holder_of_present(cur)?;
                    match store.get_full(h) {
                        Some((_, k, Value::Object(o))) if o.fields.contains_key(crate::store::OWNER_FIELD) => cur = k,
                        _ => return None,
                    }
                }
            }
        }
        let has_owner = vm.session.classes.get(class)
            .is_some_and(|c| c.fields.iter().any(|f| f.name == crate::store::OWNER_FIELD));
        if !has_owner { return None; }
        let f = vm.tick_formula(holder, &["self", "target"]).ok()?;
        let h = vm.run_tick_formula_ref(&f, &[("self", self_ref), ("target", &Value::Nil)], &[self_at, usize::MAX]);
        if changed(vm, epoch) { return None; }
        let h = h.ok()?;
        let uuid = match &h {
            Value::Ref(u) => u.clone(),
            Value::Object(o) => o.uuid.clone(),
            _ => return None,
        };
        // `target >> owner == x` when x holds more than the class has (a
        // meadow and its one patch of grass) or much of it (a meadow and its
        // rabbits): checking every target, or the class's index of
        // `owner`, beats going through everything x holds.
        let store = &mut vm.session.object_store;
        if depth == 1 && store.contents_len(&uuid) * 4 > store.count_of(class) {
            if all.len() < crate::store::ObjectStore::INDEX_FROM { return None; }
            let at = store.with_field(class, crate::store::OWNER_FIELD, &crate::store::FieldKey::Obj(uuid.clone().into()));
            return Some(at.into_iter()
                .filter_map(|i| store.get_index(i).map(|(u, _)| (Value::Ref(u.clone()), i)))
                .collect());
        }
        let store = &vm.session.object_store;
        // What `x` holds, what those hold, ...: `depth` levels down.
        let mut level: Vec<crate::value::ObjId> = vec![uuid.clone()];
        for _ in 0..depth - 1 {
            level = level.iter().flat_map(|h| store.contents_ids(h).cloned()).collect();
        }
        let mut at: Vec<usize> = level.iter().flat_map(|h| store.contents_at(h, Some(class))).collect();
        at.sort_unstable();
        let fewer: Vec<(Value, usize)> = at.into_iter()
            .filter_map(|i| store.get_index(i).map(|(u, _)| (Value::Ref(u.clone()), i)))
            .collect();
        return Some(fewer);
    }
    None
}

/// Every target that passes the `where` condition, with its score, by
/// position in the returned candidate list.
#[allow(clippy::too_many_arguments)]
fn score_targets(
    vm: &mut Vm,
    decision: &goblin_ast::DecisionDef,
    formula: &crate::vm::TickFormula,
    filter: Option<&crate::vm::TickFormula>,
    self_ref: &Value,
    self_at: usize,
    target_candidates: std::rc::Rc<Vec<(Value, usize)>>,
    is_self: &dyn Fn(&Value) -> bool,
    formula_error: &dyn Fn(&Vm, String) -> GoblinError,
    (filter_reads, formula_reads): (bool, bool),
) -> Result<(std::rc::Rc<Vec<(Value, usize)>>, Vec<(usize, f64)>), GoblinError> {
    // Fewer candidates, same result: see `narrow`.
    let target_candidates = match &decision.filter {
        Some(f) if formula_reads => match narrow(vm, f, filter_reads, &decision.target_class, self_ref, self_at, &target_candidates) {
            Some(fewer) => std::rc::Rc::new(fewer),
            None => target_candidates,
        },
        _ => target_candidates,
    };
    let mut scored: Vec<(usize, f64)> = Vec::with_capacity(target_candidates.len());
    for (k, (target, target_at)) in target_candidates.iter().enumerate() {
        if is_self(target) { continue; }
        if let Some(c) = filter {
            match vm.run_tick_formula_ref(c, &[("self", self_ref), ("target", target)], &[self_at, *target_at]) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => continue,
                Ok(other) => return Err(formula_error(vm, format!(
                    "the `where` condition gave {}, not true or false", other.type_name()))),
                Err(e) => return Err(formula_error(vm, format!("the `where` condition failed: {e}"))),
            }
        }
        let raw = match vm.run_tick_formula_ref(formula, &[("self", self_ref), ("target", target)], &[self_at, *target_at]) {
            Ok(Value::Float(f)) => f,
            Ok(Value::Int(i)) => i as f64,
            Ok(other) => return Err(formula_error(vm, format!(
                "the formula gave {} {}, not a number; a score must be a number (a comparison gives a bool: use `where [ ... ]` to leave targets out)",
                if other.type_name().starts_with(['a','e','i','o','u']) { "an" } else { "a" }, other.type_name()))),
            Err(e) => return Err(formula_error(vm, format!("the formula failed: {e}"))),
        };
        // A score that overflowed (inf) or is not a number can't be ranked.
        if !raw.is_finite() { continue; }
        scored.push((k, raw));
    }
    Ok((target_candidates, scored))
}

/// An overlay's blocks compiled as methods: `self` (the host) first, then
/// `intensity`, then the overlay's own data fields by name.
pub(crate) fn compile_overlay_methods(name: &str, actions: &[goblin_ast::ActionDecl], extra: &[String])
    -> Result<std::collections::HashMap<String, std::rc::Rc<crate::value::FunctionObject>>, GoblinError> {
    let mut m = std::collections::HashMap::new();
    for action in actions {
        let span = action.span.clone();
        let p = |n: &str| goblin_ast::Param { name: n.to_string(), type_name: None, default: None, span: span.clone() };
        let mut params = vec![p("self")];
        params.extend(action.params.clone());
        params.extend(extra.iter().map(|k| p(k)));
        let pseudo = goblin_ast::ActionDecl { params, ..action.clone() };
        let mut c = crate::compiler::Compiler::new();
        c.is_class_method = true;
        let f = c.compile_action(&pseudo).map_err(|e| GoblinError::Runtime(format!("overlay {name}, `{}`: {e}", action.name)))?;
        m.insert(action.name.clone(), std::rc::Rc::new(f));
    }
    Ok(m)
}

/// Whether the conditions of a class's judge over its decisions only read.
fn judge_reads_only(vm: &mut Vm, class: &goblin_ast::ClassDecl, action: &str) -> bool {
    // Kept while the class table is the one they were worked out from.
    if !vm.judge_classes.as_ref().is_some_and(|c| std::rc::Rc::ptr_eq(c, &vm.session.classes)) {
        vm.judge_calls.clear();
        vm.judge_classes = Some(vm.session.classes.clone());
    }
    let key = class as *const _ as usize;
    let calls = match vm.judge_calls.get(&key) {
        Some(c) => c.clone(),
        None => {
            let Some(a) = class.actions.iter().find(|a| a.name == action) else { return false };
            let mut calls = Some(Vec::new());
            let body: &[goblin_ast::Stmt] = match &a.body { goblin_ast::ActionBody::Block(b) => b, _ => &[] };
            for st in body {
                let arms = match st {
                    goblin_ast::Stmt::Judge(j) => &j.arms,
                    goblin_ast::Stmt::JudgeAll(j) => &j.arms,
                    _ => continue,
                };
                for c in arms.iter().filter_map(|a| a.condition.as_ref()) {
                    calls = match (calls, crate::compiler::read_calls_rolling(c)) {
                        (Some(mut a), Some(b)) => { a.extend(b); Some(a) }
                        _ => None,
                    };
                }
            }
            let calls: Option<std::rc::Rc<[String]>> = calls.map(|c| c.into());
            vm.judge_calls.insert(key, calls.clone());
            calls
        }
    };
    match calls {
        None => false,
        Some(c) => c.iter().all(|n| vm.action_reads_only(n)),
    }
}

/// The object whose class judge runs, and its decisions' picks so far.
pub(crate) struct Deciding {
    self_uuid: crate::value::ObjId,
    class: std::rc::Rc<std::collections::HashMap<String, goblin_ast::ClassDecl, rustc_hash::FxBuildHasher>>,
    class_name: String,
    picks: Vec<Option<Value>>,
    targets: Targets,
}

/// A decision's name read in its class's judge: its pick (`{target,
/// score}`), or nil when no target qualifies. Made once, when first read.
pub(crate) fn decision_pick(vm: &mut Vm, at: Option<usize>) -> Result<Value, GoblinError> {
    // No place: a judge arm's deed starts; nothing kept for the decisions
    // stays good.
    let Some(i) = at else { vm.memo_kept = None; return Ok(Value::Nil) };
    let Some(mut d) = vm.deciding.take() else {
        return Err(GoblinError::Runtime("R1223: decision-outside-judge: a decision's pick is read in its class's judge".into()));
    };
    if let Some(Some(v)) = d.picks.get(i) {
        let v = v.clone();
        vm.deciding = Some(d);
        return Ok(v);
    }
    let out = (|| {
        let class = d.class.get(&d.class_name).ok_or_else(|| GoblinError::Runtime("decision class is gone".into()))?;
        let Some(nd) = class.decisions.get(i) else { return Ok(Value::Nil) };
        let v = match pick(vm, &mut d.targets, &d.self_uuid, &nd.name, &nd.decision, None)? {
            Some(Pick { target, score }) => pick_record(target, score),
            None => Value::Nil,
        };
        d.picks[i] = Some(v.clone());
        Ok(v)
    })();
    vm.deciding = Some(d);
    out
}

/// A decision's pick: the target it chose and that target's score.
struct Pick { target: crate::value::ObjId, score: f64 }

thread_local! {
    static PICK_SHAPE: std::cell::RefCell<(Option<std::rc::Rc<crate::fields::Layout>>, Option<crate::value::ObjId>, Option<std::rc::Rc<str>>)> = const { std::cell::RefCell::new((None, None, None)) };
}

/// A pick as the judge reads it: a record with `target` and `score`. It is
/// not a stored object (its id has the record prefix), and every pick
/// shares one layout, id and class name, so making one allocates little.
fn pick_record(target: crate::value::ObjId, score: f64) -> Value {
    PICK_SHAPE.with(|c| {
        let mut c = c.borrow_mut();
        let fields = crate::fields::FieldMap::from_names(["target", "score"].into_iter(), vec![Value::Ref(target), Value::Float(score)], &mut c.0);
        let uuid = c.1.get_or_insert_with(|| format!("{}pick", crate::vm::OVERLAY_RECORD).into()).clone();
        let class_name = c.2.get_or_insert_with(|| "pick".into()).clone();
        Value::Object(Box::new(crate::value::ObjectVal {
            class_name,
            fields: std::rc::Rc::new(fields),
            readonly_fields: Default::default(),
            trait_fields: Default::default(),
            uuid,
        }))
    })
}


/// One decision of one object: score the targets and pick the best (within
/// the object's `margin`). None: no target qualifies.
fn pick(vm: &mut Vm, targets: &mut Targets, self_uuid: &crate::value::ObjId, name: &str, decision: &goblin_ast::DecisionDef, judge: Option<&goblin_ast::JudgeStmt>) -> Result<Option<Pick>, GoblinError> {
    let self_uuid = self_uuid.clone();
    let margin = match vm.session.object_store.get(&self_uuid) {
        Some(Value::Object(_)) if decision.margin.is_some() => decision.margin.unwrap_or(0.0),
        Some(Value::Object(o)) => o.fields.get("margin").and_then(|v| match v {
            Value::Float(f) => Some(*f),
            Value::Int(i) => Some(*i as f64),
            _ => None,
        }).unwrap_or(0.0),
        Some(_) => 0.0,
        None => return Ok(None),
    };
    // Formulas see `self` and `target` by reference: they read fields
    // where the objects live instead of copying every candidate.
    let self_ref = Value::Ref(self_uuid.clone());
    let self_at = vm.session.object_store.get_index_of(&self_uuid).unwrap_or(usize::MAX);

    let target_candidates = targets.of(vm, &decision.target_class);
    let is_self = |v: &Value| matches!(v, Value::Ref(u) if *u == self_uuid);

    // Score all targets (by position in the list), the formula compiled once.
    // A formula that fails, or gives something other than a number, stops
    // the tick with an error: a decision never does nothing silently.
    let formula_error = |vm: &Vm, what: String| {
        let class = match vm.session.object_store.get(&self_uuid) {
            Some(Value::Object(o)) => o.class_name[..].to_string(),
            _ => String::new(),
        };
        GoblinError::Runtime(format!("R1220: decision-formula: decision `{name}` of {class} (against {}): {what}", decision.target_class))
    };
    let plan = decide_plan(vm, decision, judge).map_err(|e| formula_error(vm, e))?;
    let formula = plan.formula.clone();
    // `where [ ... ]`: the targets that qualify; the others are not scored.
    let filter = plan.filter.clone();
    // While the targets are scored nothing changes, so an action the
    // condition or the formula calls that only reads gives the same answer
    // for the same arguments: it runs once (memo.rs).
    // The answers stay good for the next decision (this object's or the
    // next one's) as long as nothing changes in between: they are kept
    // until a judge runs an action, or a decision's own steps may change
    // things.
    // Nothing runs between here and the scoring that could change what an
    // action does, so these hold for both.
    let filter_reads = calls_read(vm, &plan.filter_calls);
    let formula_reads = calls_read(vm, &plan.formula_calls);
    let memo = filter_reads && formula_reads;
    if memo { vm.memo = Some(vm.memo_kept.take().unwrap_or_default()); } else { vm.memo_kept = None; }
    let epoch = vm.memo.as_ref().map(|m| m.epoch);
    let scored = score_targets(vm, decision, &formula, filter.as_deref(), &self_ref, self_at, target_candidates, &is_self, &formula_error, (filter_reads, formula_reads));
    if let Some(m) = vm.memo.take() {
        // A call that may have changed things ran: nothing is kept.
        if m.changing == 0 && Some(m.epoch) == epoch && scored.is_ok() { vm.memo_kept = Some(m); }
    }
    let (target_candidates, scored) = scored?;
    if scored.is_empty() { return Ok(None); }
    // A judge condition that may change things, or an action, ends what
    // was kept.
    if !calls_read(vm, &plan.judge_calls) { vm.memo_kept = None; }
    // A score is exactly the number the formula gives: nothing rescales or
    // clamps it. `margin` is in the formula's own units, and the judge sees
    // the same number.
    let best = scored.iter().map(|(_, s)| *s).fold(f64::NEG_INFINITY, f64::max);

    let pool: Vec<(usize, f64)> = scored.into_iter()
        .filter(|(_, s)| *s >= best - margin)
        .collect();

    if pool.is_empty() { return Ok(None); }
    let chosen_idx = if pool.len() == 1 { 0 } else {
        let r = vm.session.next_u128() as f64 / u128::MAX as f64;
        (r * pool.len() as f64).floor() as usize % pool.len()
    };
    let Some((best_k, final_score)) = pool.into_iter().nth(chosen_idx) else { return Ok(None) };
    let Value::Ref(best_target_uuid) = &target_candidates[best_k].0 else { return Ok(None) };
    Ok(Some(Pick { target: best_target_uuid.clone(), score: final_score }))
}

/// One decision of one object with its own judge (`judge using name`):
/// pick a target, then run the action the judge picks.
fn decide(vm: &mut Vm, targets: &mut Targets, self_uuid: &crate::value::ObjId, name: &str, decision: &goblin_ast::DecisionDef, judge: &goblin_ast::JudgeStmt) -> Result<bool, GoblinError> {
    let Some(Pick { target: best_target_uuid, score: final_score }) = pick(vm, targets, self_uuid, name, decision, Some(judge))? else { return Ok(false) };
    let self_uuid = self_uuid.clone();
    let self_ref = Value::Ref(self_uuid.clone());
    let target_ref = Value::Ref(best_target_uuid.clone());

    // Run judge to find selected action
    let mut selected_action: Option<(String, &[goblin_ast::Expr])> = None;
    let mut else_action: Option<(String, &[goblin_ast::Expr])> = None;

    for arm in &judge.arms {
        match &arm.condition {
            None => {
                if let Some(call) = extract_action_call(&arm.body) {
                    else_action = Some(call);
                }
            }
            Some(cond) => {
                let mut bind = vec![
                    ("score", Value::Float(final_score)),
                    ("self", self_ref.clone()),
                    ("target", target_ref.clone()),
                ];
                if name != "score" { bind.push((name, Value::Float(final_score))); }
                let fires = vm.eval_tick_expr_bool(cond, bind);
                if fires {
                    if let Some(call) = extract_action_call(&arm.body) {
                        selected_action = Some(call);
                        break;
                    }
                }
            }
        }
    }

    let (action_name, arg_exprs) = selected_action.or(else_action).unwrap_or((String::new(), &[]));
    if !action_name.is_empty() {
        // The action gets the objects as they are now (nothing above
        // changes them), the same values it was given before.
        let (Some(self_val), Some(_)) = (
            vm.session.object_store.get(&self_uuid).cloned(),
            vm.session.object_store.get(&best_target_uuid).cloned(),
        ) else { return Ok(false) };
        // The class's own action of that name (`act govern(target)` in the
        // class, run with `self`), else a program action taking both.
        // An error in the action stops the tick, as one in an `every`
        // block does: a mistake there must not pass as "nothing happened".
        // Either way it gets `self` first, then the arguments the arm wrote
        // (`purchase(target, 5)`), worked out with `score`, `self` and
        // `target` in view.
        let mut args = Vec::with_capacity(arg_exprs.len() + 1);
        for e in arg_exprs {
            args.push(match e {
                // Objects go by reference, as in an ordinary call: an action
                // written `strike(target, target)` changes one object.
                goblin_ast::Expr::Ident(n, _) if n == "target" => target_ref.clone(),
                other => {
                    let mut bind = vec![
                        ("score", Value::Float(final_score)),
                        ("self", self_ref.clone()),
                        ("target", target_ref.clone()),
                    ];
                    if name != "score" { bind.push((name, Value::Float(final_score))); }
                    let v = vm.eval_tick_expr(other, bind)?;
                    vm.arg_ref(v)
                }
            });
        }
        let _ = self_val;
        // An action that returns false didn't act: in a `one of` group the
        // next decision gets its turn.
        vm.memo_kept = None;
        let out = action_result(vm, &self_uuid, &action_name, args)?;
        return Ok(!matches!(out, Value::Bool(false)));
    }
    Ok(false)
}

// ── Object transition tick ────────────────────────────────────────────────────

fn object_transition_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::TransitionKind;

    let store = &vm.session.object_store;
    let candidates: Vec<(crate::value::ObjId, String)> = store
        .places_of(|c| vm.session.classes.get(c).is_some_and(|c| !c.transitions.is_empty()))
        .into_iter()
        .filter_map(|i| match store.get_index(i) {
            Some((uuid, Value::Object(o))) if !crate::world::is_remote(vm, uuid) =>
                Some((uuid.clone(), o.class_name[..].to_string())),
            _ => None,
        }).collect();

    let mut erased: rustc_hash::FxHashSet<String> = Default::default();

    for (self_uuid, class_name) in &candidates {
        if erased.contains(self_uuid.as_str()) { continue; }

        let classes = vm.session.classes.clone();
        let transitions = match classes.get(class_name) {
            Some(c) => &c.transitions,
            None => continue,
        };
        let self_val = match vm.session.object_store.get(self_uuid).cloned() { Some(v) => v, None => continue };

        for tdef in transitions {
            let trigger_fired = vm.eval_tick_expr_bool(
                &tdef.trigger,
                vec![("self", self_val.clone())],
            );
            if !trigger_fired { continue; }

            match tdef.kind {
                TransitionKind::Erase => {
                    // `on erase: action` runs first, while the object and its
                    // fields are still there. An error in it stops the tick.
                    let on_erase = classes.get(class_name).and_then(|c| c.on_erase.clone());
                    erase_object(vm, self_uuid, on_erase.as_deref())?;
                    erased.insert(self_uuid.to_string());
                    break;
                }
                TransitionKind::Spawn => {
                    let child_class = tdef.into_classes.first().cloned().unwrap_or(class_name.clone());
                    // Borrow the class through the shared table instead of
                    // copying it: the formula cache knows a formula by its
                    // address, which a dropped copy could hand to another.
                    let classes = vm.session.classes.clone();
                    if let Some(child_decl) = classes.get(&child_class) {
                        let new_uuid = uuid::Uuid::new_v4().to_string();
                        let mut fields = crate::value::FieldMap::default();
                        for fd in &child_decl.fields {
                            if let Some(ref default_expr) = fd.default {
                                if let Ok(v) = vm.eval_tick_expr(default_expr, vec![]) {
                                    fields.insert(fd.name.clone(), v);
                                }
                            }
                        }
                        let traits = crate::vm::trait_fields_of(&mut fields, &child_decl.fields);
                        let child_obj = Value::Object(Box::new(crate::value::ObjectVal {
                            class_name: crate::value::class_rc(&child_class),
                            fields: std::rc::Rc::new(fields),
                            readonly_fields: Default::default(),
                            trait_fields: std::rc::Rc::new(traits),
                            uuid: new_uuid.as_str().into(),
                        }));
                        vm.session.object_store.insert(new_uuid, child_obj);
                    }
                    break;
                }
                TransitionKind::Mutate => {
                    let new_class = tdef.into_classes.first().cloned().unwrap_or(class_name.clone());
                    if let Some(Value::Object(fields)) = vm.session.object_store.get_mut(self_uuid) {
                        // Keep fields, just change the class (interpreter behavior)
                        let _ = new_class; // class change not modeled in simple field map
                    }
                    break;
                }
                _ => { break; } // Other kinds not modeled in VM object transitions
            }
        }
    }

    Ok(())
}

// ── Helpers ───────────────────────────────────────────────────────────────────

/// The range a link/decision formula can take, used to normalise its score to
/// 0..1. Ported from the interpreter's `derive_link_formula_range`.
pub(crate) fn derive_link_formula_range(expr: &goblin_ast::Expr) -> (f64, f64) {
    use goblin_ast::Expr;
    match expr {
        Expr::Number(s, _) => {
            let v = s.parse::<f64>().unwrap_or(0.0);
            (v, v)
        }
        // self >> field or target >> field — trait value, range 0..1
        Expr::Binary(_, op, _, _) if op == ">>" => (0.0, 1.0),
        Expr::Binary(lhs, op, rhs, _) => {
            let (lmin, lmax) = derive_link_formula_range(lhs);
            let (rmin, rmax) = derive_link_formula_range(rhs);
            match op.as_str() {
                "+" => (lmin + rmin, lmax + rmax),
                "-" => (lmin - rmax, lmax - rmin),
                "*" => {
                    let products = [lmin * rmin, lmin * rmax, lmax * rmin, lmax * rmax];
                    (products.iter().cloned().fold(f64::INFINITY, f64::min),
                     products.iter().cloned().fold(f64::NEG_INFINITY, f64::max))
                }
                _ => (0.0, 1.0),
            }
        }
        Expr::Prefix(op, inner, _) if op == "-" => {
            let (imin, imax) = derive_link_formula_range(inner);
            (-imax, -imin)
        }
        Expr::FreeCall(name, args, _) if name == "abs" => {
            if let Some(arg) = args.first() {
                let (amin, amax) = derive_link_formula_range(arg);
                let abs_min = if amin <= 0.0 && amax >= 0.0 { 0.0 } else { amin.abs().min(amax.abs()) };
                let abs_max = amin.abs().max(amax.abs());
                (abs_min, abs_max)
            } else {
                (0.0, 1.0)
            }
        }
        _ => (0.0, 1.0),
    }
}

/// The class declaration of A's link on `channel` (`link Nation trade 0...1`).
fn link_decl(vm: &Vm, from: &str, channel: &str) -> Result<std::rc::Rc<crate::session::LinkDef>, GoblinError> {
    let class = match vm.session.object_store.get(from) {
        Some(Value::Object(o)) => o.class_name[..].to_string(),
        _ => return Err(GoblinError::Runtime("R1225: link-not-object: a link end is not an object that exists".into())),
    };
    vm.session.link_defs.get(&(class.clone(), channel.to_string())).cloned().ok_or_else(|| GoblinError::Runtime(format!(
        "R1228: link-undeclared: {class} has no `{channel}` link; declare it with its range first: `link {class} {channel} 0...1` or `link {class} {channel} bool`")))
}

/// The two ends of A's link range on `channel` (`bool` is 0 to 1).
pub(crate) fn link_bounds(vm: &Vm, from: &str, channel: &str) -> Result<(f64, f64), GoblinError> {
    Ok(match link_decl(vm, from, channel)?.range {
        Some(goblin_ast::LinkRange::Between(lo, hi)) => (lo, hi),
        _ => (0.0, 1.0),
    })
}

/// Where A's link to B starts before any change: its formula (the
/// object's own, else its class's), else 0 (kept inside the range).
pub(crate) fn link_base(vm: &mut Vm, from: &str, to: &str, channel: &str) -> Result<f64, GoblinError> {
    let decl = link_decl(vm, from, channel)?;
    let formula = vm.session.object_link_defs.get(&(from.to_string(), channel.to_string())).map(|d| d.formula.clone())
        .or_else(|| decl.has_formula.then(|| decl.formula.clone()));
    // No formula: 0, or the nearer end of the range when 0 is outside it,
    // so a first `offset` gives the number written.
    let Some(formula) = formula else { let (lo, hi) = link_bounds(vm, from, channel)?; return Ok(0f64.clamp(lo, hi)) };
    match vm.eval_tick_expr(&formula, vec![("self", Value::Ref(from.into())), ("target", Value::Ref(to.into()))]) {
        Ok(Value::Int(i)) => Ok(i as f64),
        Ok(Value::Float(f)) => Ok(f),
        Ok(Value::Bool(b)) => Ok(if b { 1.0 } else { 0.0 }),
        Ok(other) => Err(GoblinError::Runtime(format!(
            "R1226: link-formula: the {channel} link formula gave {}, not a number", other.type_name()))),
        Err(e) => Err(GoblinError::Runtime(format!("R1226: link-formula: the {channel} link formula failed: {e}"))),
    }
}

/// A's link to B on `channel`: where it starts plus every change made to
/// the pair, kept inside the declared range. A `bool` link gives true or
/// false. Links go one way: B's link to A is its own.
pub(crate) fn link_value(vm: &mut Vm, from: &str, to: &str, channel: &str) -> Result<Value, GoblinError> {
    let decl = link_decl(vm, from, channel)?;
    let (lo, hi) = link_bounds(vm, from, channel)?;
    let base = link_base(vm, from, to, channel)?;
    let changes: f64 = vm.session.link_offsets.get(&(from.to_string(), to.to_string(), channel.to_string()))
        .map(|v| v.iter().map(|o| o.value).sum()).unwrap_or(0.0);
    let v = (base + changes).clamp(lo, hi);
    Ok(match decl.range {
        Some(goblin_ast::LinkRange::Bool) => Value::Bool(v >= 1.0),
        _ => Value::Float(v),
    })
}

/// A raw formula value scaled to 0..1 over the formula's range, as
/// `eval_link_score` does.
fn normalize_score(raw: f64, formula_min: f64, formula_max: f64) -> f64 {
    let range = formula_max - formula_min;
    if range.abs() < 1e-12 { 0.5 } else { ((raw - formula_min) / range).clamp(0.0, 1.0) }
}

/// Puts an overlay copy on its host. `dup:` decides what a second copy
/// does: `ignore` keeps the one there, `replace` takes it off first (its
/// `on end` doesn't run: it didn't run out), `stack` adds a copy with its
/// own clock. An overlay can't share a host with one it `conflicts:` with
/// (either side naming the other): the one with the lower intensity comes
/// off, and on a tie the one already there stays. Then the copy's `modify`
/// film goes on and its `on start` runs.
pub(crate) fn overlay_push(vm: &mut Vm, inst: OverlayInstance) -> Result<(), GoblinError> {
    // On another region's object: its owner applies it, next tick.
    if crate::world::send_overlay(vm, &inst) { return Ok(()); }
    let Some(def) = vm.session.overlay_defs.get(&inst.overlay_name).cloned() else {
        return Err(GoblinError::Runtime(format!("unknown overlay '{}'", inst.overlay_name)));
    };
    let host = inst.host_uuid.clone();
    use crate::session::OverlayApplyBehavior as Dup;
    let existing = vm.session.overlay_instances.find(&host, &inst.overlay_name);
    let mut off: Vec<usize> = Vec::new();
    match def.apply_behavior {
        Dup::Caps => if existing.is_some() { return Ok(()); },
        Dup::Replaces => if let Some(p) = existing { off.push(p); },
        Dup::Stacks { .. } => {}
    }
    {
        let defs = &vm.session.overlay_defs;
        let list = &vm.session.overlay_instances;
        for &p in list.on_host(&host) {
            let other = &list[p];
            let rival = def.conflicts.iter().any(|c| *c == other.overlay_name)
                || defs.get(&other.overlay_name).is_some_and(|d| d.conflicts.iter().any(|c| *c == inst.overlay_name));
            if !rival { continue; }
            if other.strength >= inst.strength { return Ok(()); }
            off.push(p);
        }
    }
    off.sort_unstable();
    off.dedup();
    for gone in vm.session.overlay_instances.take_at(&off) { forget_overlay(vm, &gone); }
    let at = vm.session.overlay_instances.len();
    vm.session.overlay_instances.push(inst);
    let amounts = modify_amounts(vm, at)?;
    vm.session.overlay_instances[at].applied = amounts;
    settle_films(&mut vm.session);
    if !def.methods.contains_key("on start") { return Ok(()); }
    let inst = vm.session.overlay_instances[at].clone();
    run_overlay_block(vm, &inst, "on start")
}

/// Runs `action` for the object `uuid`: the class's own action of that name
/// (`act give_birth()` in the class, run with `self`), else a program
/// action, given the object first. `args` follow.
fn call_object_action(vm: &mut Vm, uuid: &str, action: &str, args: Vec<Value>) -> Result<(), GoblinError> {
    action_result(vm, uuid, action, args).map(|_| ())
}

/// `call_object_action`, giving what the action returned (nil when the
/// object is gone).
fn action_result(vm: &mut Vm, uuid: &str, action: &str, mut args: Vec<Value>) -> Result<Value, GoblinError> {
    let class_name = match vm.session.object_store.get(uuid) {
        Some(Value::Object(o)) => o.class_name[..].to_string(),
        Some(_) => String::new(),
        None => return Ok(Value::Nil),
    };
    let method = vm.session.compiled_methods.get(&(class_name, action.to_string())).cloned();
    args.insert(0, Value::Ref(uuid.to_string().into()));
    let mut out = Value::Nil;
    with_erase_check(vm, |vm| {
        out = match method {
            Some(f) => vm.call_callable(Value::Function(f), args)?,
            // A reference, as a class action gets: if the action moves or
            // erases the object, reading it afterwards fails instead of
            // reading a stale copy.
            None => vm.call_named(action, args)?,
        };
        Ok(())
    })?;
    Ok(out)
}

/// Runs an action, then the erase transitions of the objects it changed:
/// an object whose erase condition now holds goes right away (after its
/// `on erase:` action), not at the end of the tick, so it neither acts nor
/// is chosen again this tick. Never in the middle of an action.
fn with_erase_check(vm: &mut Vm, f: impl FnOnce(&mut Vm) -> Result<(), GoblinError>) -> Result<(), GoblinError> {
    let outer = vm.session.des_touched.replace(Vec::new());
    let r = f(vm);
    let touched = std::mem::replace(&mut vm.session.des_touched, outer);
    r?;
    match touched {
        Some(t) if !t.is_empty() => erase_changed(vm, t),
        _ => Ok(()),
    }
}

/// A class's `erase when` conditions that don't roll dice, compiled, and
/// its `on erase:` action.
pub(crate) struct EraseConds {
    conds: Vec<std::rc::Rc<crate::vm::TickFormula>>,
    on_erase: Option<String>,
}

fn erase_conds(vm: &mut Vm, class_name: &std::rc::Rc<str>) -> std::rc::Rc<EraseConds> {
    // Kept while the class table is the one they were made from (a change
    // to it copies it first: it is shared with this cache).
    let classes = vm.session.classes.clone();
    if !vm.erase_classes.as_ref().is_some_and(|c| std::rc::Rc::ptr_eq(c, &classes)) {
        vm.erase_conds.clear();
        vm.erase_classes = Some(classes.clone());
    }
    if let Some(c) = vm.erase_conds.get(&**class_name) { return c.clone(); }
    use goblin_ast::TransitionKind;
    let mut conds = Vec::new();
    let mut on_erase = None;
    if let Some(class) = classes.get(&**class_name) {
        // A condition that rolls dice (`pick 1 from 1...200000`) is a chance
        // per tick: it is checked once, at the end of the tick, as before.
        // One that can't be compiled counts as false, as it did.
        for t in class.transitions.iter().filter(|t| matches!(t.kind, TransitionKind::Erase) && !uses_chance(&t.trigger)) {
            if let Ok(f) = vm.tick_formula(&t.trigger, &["self"]) { conds.push(f); }
        }
        on_erase = class.on_erase.clone();
    }
    let c = std::rc::Rc::new(EraseConds { conds, on_erase });
    vm.erase_conds.insert(class_name.clone(), c.clone());
    c
}

fn erase_changed(vm: &mut Vm, touched: Vec<crate::value::ObjId>) -> Result<(), GoblinError> {
    let mut seen = rustc_hash::FxHashSet::with_capacity_and_hasher(touched.len(), Default::default());
    for uuid in touched {
        if !seen.insert(uuid.clone()) { continue; }
        let Some((at, _, Value::Object(o))) = vm.session.object_store.get_full(&uuid) else { continue };
        let class_name = o.class_name.clone();
        let c = erase_conds(vm, &class_name);
        if c.conds.is_empty() { continue; }
        if vm.session.des_erasing.iter().any(|e| uuid == *e) || crate::world::is_remote(vm, &uuid) { continue; }
        let me = Value::Ref(uuid.clone());
        let fires = c.conds.iter().any(|f| match vm.run_tick_formula_ref(f, &[("self", &me)], &[at]) {
            Ok(Value::Bool(b)) => b,
            Ok(Value::Int(i)) => i != 0,
            Ok(Value::Float(f)) => f != 0.0,
            _ => false,
        });
        if fires { erase_object(vm, &uuid, c.on_erase.as_deref())?; }
    }
    Ok(())
}

/// Whether an expression draws a random number (`pick`, `:roll`, ...).
fn uses_chance(e: &goblin_ast::Expr) -> bool {
    use goblin_ast::Expr as E;
    let random_name = |n: &str| {
        let n = n.trim_end_matches('!');
        matches!(n, "pick" | "reap" | "roll" | "roll_detail" | "roll_str" | "roll_detail_str"
            | "shuffle" | "sample_weighted" | "uuid_v4" | "uuid_v7")
            || n.starts_with("secure_") || n.ends_with("_random") || n.ends_with("_sample")
    };
    // A call to the program's own code (an action, a method, `:invoke`)
    // may roll dice inside, so it counts as chance too: checked once a tick.
    let own_code = |n: &str| {
        let n = n.trim_end_matches('!');
        matches!(n, "invoke" | "summon" | "provoke") || crate::compiler::builtin_by_name(n).is_none()
    };
    match e {
        E::FreeCall(n, args, _) => random_name(n) || own_code(n) || args.iter().any(uses_chance),
        E::NsCall(_, n, args, _) => random_name(n) || own_code(n) || args.iter().any(uses_chance),
        E::Call(r, n, args, _) | E::OptCall(r, n, args, _) => random_name(n) || own_code(n) || uses_chance(r) || args.iter().any(uses_chance),
        E::Binary(l, _, r, _) => uses_chance(l) || uses_chance(r),
        E::Prefix(_, x, _) | E::Postfix(x, _, _) => uses_chance(x),
        E::Member(x, _, _) | E::OptMember(x, _, _) => uses_chance(x),
        E::Index(a, b, _) | E::IndexMap(a, b, _) => uses_chance(a) || uses_chance(b),
        E::Array(xs, _) => xs.iter().any(uses_chance),
        E::Object(kv, _) => kv.iter().any(|(_, v)| uses_chance(v)),
        _ => false,
    }
}

/// Erases an object for its erase transition, after its `on erase:`
/// action (which sees the object and its fields; an error in it stops the tick).
fn erase_object(vm: &mut Vm, uuid: &str, on_erase: Option<&str>) -> Result<(), GoblinError> {
    if let Some(action) = on_erase {
        vm.session.des_erasing.push(uuid.to_string());
        let r = call_object_action(vm, uuid, action, Vec::new());
        vm.session.des_erasing.retain(|u| u != uuid);
        r?;
    }
    vm.session.object_store.move_contents_up(uuid);
    vm.session.object_store.shift_remove(uuid);
    Ok(())
}

/// `delete_object!(x)`: x's class's `on erase:` action runs first, as it
/// does for an erase transition (not again while it is already running).
pub(crate) fn run_on_erase(vm: &mut Vm, uuid: &str) -> Result<(), GoblinError> {
    if vm.session.des_erasing.iter().any(|u| u == uuid) { return Ok(()); }
    let class = match vm.session.object_store.get(uuid) {
        Some(Value::Object(o)) => o.class_name[..].to_string(),
        _ => return Ok(()),
    };
    let Some(action) = vm.session.classes.get(&class).and_then(|c| c.on_erase.clone()) else { return Ok(()) };
    vm.session.des_erasing.push(uuid.to_string());
    let r = call_object_action(vm, uuid, &action, Vec::new());
    vm.session.des_erasing.retain(|u| u != uuid);
    r
}

/// The action a judge arm calls, and the arguments it writes (none for a
/// bare name).
fn extract_action_call(body: &goblin_ast::JudgeArmBody) -> Option<(String, &[goblin_ast::Expr])> {
    use goblin_ast::{JudgeArmBody, Expr};
    fn call(e: &Expr) -> Option<(String, &[Expr])> {
        match e {
            Expr::FreeCall(name, args, _) => Some((name.clone(), args.as_slice())),
            Expr::Ident(name, _) => Some((name.clone(), &[])),
            _ => None,
        }
    }
    match body {
        JudgeArmBody::Expr(e) => call(e),
        JudgeArmBody::Stmts(stmts) => stmts.iter().find_map(|s| match s {
            goblin_ast::Stmt::Expr(e) => call(e),
            _ => None,
        }),
        #[allow(unreachable_patterns)]
        _ => None,
    }
}

/// The variable name an object's overlays report as `host_var`.
pub(crate) fn host_name(vm: &Vm, uuid: &str) -> String {
    vm.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_else(|| uuid.to_string())
}
