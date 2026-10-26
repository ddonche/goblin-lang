/// DES tick runner for the Goblin VM.
///
/// Mirrors the interpreter's `tick`/`tick_db` builtin:
///   1. begin_tick on TickRunner
///   2. all_grids tick_db_begin
///   3. overlay_tick (8 passes)
///   4. all_grids tick_db_commit
///   5. des_flush_pending
///   6. end_tick on TickRunner
use std::collections::HashMap;
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

    // ── Pass 1: Spread ────────────────────────────────────────────────────────
    spread_pass(vm)?;

    // ── Pass 2: Decay strength + age + duration ───────────────────────────────
    {
        let defs = vm.session.overlay_defs.clone();
        let count = vm.session.overlay_instances.len();
        for idx in 0..count {
            let overlay_name = vm.session.overlay_instances[idx].overlay_name.clone();
            if let Some(def) = defs.get(&overlay_name) {
                let s = vm.session.overlay_instances[idx].strength;
                let new_s = s - def.decay_rate;
                vm.session.overlay_instances[idx].strength = if new_s <= def.decay_rate { 0.0 } else { new_s.max(0.0) };
            }
            vm.session.overlay_instances[idx].age += 1;
            if let Some(ref mut remaining) = vm.session.overlay_instances[idx].ticks_remaining {
                if *remaining > 0 { *remaining -= 1; }
            }
        }
    }

    // ── Pass 3: Conflict suppression/boost ───────────────────────────────────
    {
        let defs = vm.session.overlay_defs.clone();
        let mut host_index: HashMap<String, Vec<usize>> = HashMap::new();
        for (idx, inst) in vm.session.overlay_instances.iter().enumerate() {
            host_index.entry(inst.host_var.clone()).or_default().push(idx);
        }
        let mut overlay_host_idx: HashMap<(String, String), usize> = HashMap::new();
        for (idx, inst) in vm.session.overlay_instances.iter().enumerate() {
            overlay_host_idx.insert((inst.overlay_name.clone(), inst.host_var.clone()), idx);
        }

        let mut suppress: Vec<(usize, f64)> = Vec::new();
        let mut boost:    Vec<(usize, f64)> = Vec::new();

        for indices in host_index.values() {
            for &i in indices {
                let (ov_a, str_a) = {
                    let inst = &vm.session.overlay_instances[i];
                    (inst.overlay_name.clone(), inst.strength)
                };
                if let Some(def_a) = defs.get(&ov_a) {
                    let decay_a = def_a.decay_rate;
                    for rule in &def_a.conflict_rules {
                        let host_var = vm.session.overlay_instances[i].host_var.clone();
                        if let Some(&j) = overlay_host_idx.get(&(rule.0.clone(), host_var)) {
                            if i == j { continue; }
                            let str_b = vm.session.overlay_instances[j].strength;
                            let gap = (str_a - str_b).abs();
                            if str_a >= str_b {
                                suppress.push((j, decay_a * rule.1 * (1.0 + gap)));
                                boost.push((i, decay_a * 0.1 * gap));
                            } else {
                                suppress.push((i, decay_a * rule.1 * (1.0 + gap)));
                                boost.push((j, decay_a * 0.1 * gap));
                            }
                        }
                    }
                }
            }
        }
        for (idx, extra) in suppress {
            if idx < vm.session.overlay_instances.len() {
                vm.session.overlay_instances[idx].strength =
                    (vm.session.overlay_instances[idx].strength - extra).max(0.0);
            }
        }
        for (idx, b) in boost {
            if idx < vm.session.overlay_instances.len() {
                vm.session.overlay_instances[idx].strength =
                    (vm.session.overlay_instances[idx].strength + b).min(1.0);
            }
        }
    }

    // ── Pass 4: Spawn ─────────────────────────────────────────────────────────
    spawn_pass(vm)?;

    // ── Pass 5: Transitions ───────────────────────────────────────────────────
    transition_pass(vm)?;

    // ── Pass 6: Remove dead / expired overlays ────────────────────────────────
    {
        let defs = vm.session.overlay_defs.clone();
        let mut to_remove: Vec<usize> = Vec::new();
        for (idx, inst) in vm.session.overlay_instances.iter().enumerate() {
            if inst.strength <= 0.0 || inst.ticks_remaining == Some(0) {
                to_remove.push(idx);
            }
        }
        for idx in to_remove.iter().rev() {
            if *idx < vm.session.overlay_instances.len() {
                let inst = vm.session.overlay_instances.remove(*idx);
                if !inst.original_values.is_empty() {
                    restore_originals(vm, &inst.host_uuid, &inst.original_values);
                }
                if let Some(hh) = vm.session.des_store.handle_for_name(&inst.host_var) {
                    vm.session.des_index.remove_overlay(hh, &inst.overlay_name, inst.des_id);
                }
            }
        }
        let _ = defs; // suppress unused warning
    }

    // ── Pass 7: Remove orphaned overlays ─────────────────────────────────────
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
        let live: std::collections::HashSet<String> = vm.session.object_store.keys().cloned().collect();
        vm.session.overlay_instances.retain(|inst| live.contains(&inst.host_uuid));
    }

    // ── Pass 8: `every N ticks` blocks, then decisions ────────────────────────
    every_tick(vm)?;
    decision_tick(vm)?;

    // ── Pass 9: Object transition tick ────────────────────────────────────────
    object_transition_tick(vm)?;

    Ok(())
}

// ── Spread pass ───────────────────────────────────────────────────────────────

fn spread_pass(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::SpreadRule;
    let defs = vm.session.overlay_defs.clone();
    let instances_snap: Vec<OverlayInstance> = vm.session.overlay_instances.clone();

    // Build (overlay_name, host_uuid) → idx
    let mut ov_host_idx: HashMap<(String, String), usize> = HashMap::new();
    for (idx, inst) in vm.session.overlay_instances.iter().enumerate() {
        ov_host_idx.insert((inst.overlay_name.clone(), inst.host_uuid.clone()), idx);
    }

    let mut spread_buf: Vec<(String, String, f64)> = Vec::new(); // (overlay_name, host_uuid, strength)

    for inst in &instances_snap {
        let def = match defs.get(&inst.overlay_name) { Some(d) => d, None => continue };

        for rule in &def.spread_rules {
            match rule {
                SpreadRule::Channel { channel, rate } => {
                    let host_class = match vm.session.object_store.get(&inst.host_uuid) {
                        Some(Value::Object(o)) => o.class_name.clone(),
                        _ => continue,
                    };
                    let link_def = vm.session.link_defs.get(&(host_class.clone(), channel.clone())).cloned()
                        .or_else(|| vm.session.link_defs.get(&(host_class.clone(), "default".to_string())).cloned());
                    let link_def = match link_def { Some(d) => d, None => continue };
                    let self_val = match vm.session.object_store.get(&inst.host_uuid).cloned() {
                        Some(v) => v, None => continue,
                    };
                    let candidates: Vec<(String, Value)> = vm.session.object_store.iter()
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object(o) if o.class_name == host_class))
                        .map(|(uuid, v)| (uuid.clone(), v.clone()))
                        .collect();
                    for (target_uuid, target_val) in candidates {
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
                        let link_score = eval_link_score(vm, self_val.clone(), target_val, &link_def.formula, link_def.formula_min, link_def.formula_max)?;
                        if link_score < 0.3 { continue; }
                        let amount = inst.strength * inst.strength * rate * link_score;
                        if amount <= 0.0 { continue; }
                        if !already && crate::world::reinforce_remote(vm, &inst.overlay_name, &target_uuid, amount * 0.5) { continue; }
                        if already {
                            if let Some(&eidx) = ov_host_idx.get(&(inst.overlay_name.clone(), target_uuid)) {
                                if eidx < vm.session.overlay_instances.len() {
                                    vm.session.overlay_instances[eidx].strength =
                                        (vm.session.overlay_instances[eidx].strength + amount * 0.5).min(1.0);
                                }
                            }
                        } else if amount > 0.15 {
                            spread_buf.push((inst.overlay_name.clone(), target_uuid, amount));
                        }
                    }
                }

                SpreadRule::All { class_name, rate } => {
                    let candidates: Vec<String> = vm.session.object_store.iter()
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object(o) if &o.class_name == class_name))
                        .map(|(uuid, _)| uuid.clone())
                        .collect();
                    for target_uuid in candidates {
                        let amount = inst.strength * inst.strength * rate;
                        if amount <= 0.0 { continue; }
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
                        if !already && crate::world::reinforce_remote(vm, &inst.overlay_name, &target_uuid, amount * 0.5) { continue; }
                        if already {
                            if let Some(&eidx) = ov_host_idx.get(&(inst.overlay_name.clone(), target_uuid)) {
                                if eidx < vm.session.overlay_instances.len() {
                                    vm.session.overlay_instances[eidx].strength =
                                        (vm.session.overlay_instances[eidx].strength + amount * 0.5).min(1.0);
                                }
                            }
                        } else if amount > 0.15 {
                            spread_buf.push((inst.overlay_name.clone(), target_uuid, amount));
                        }
                    }
                }

                SpreadRule::Ownership { rate } => {
                    let host_uuid = match vm.session.object_store.get(&inst.host_uuid) {
                        Some(Value::Object(o)) => o.uuid.clone(),
                        _ => continue,
                    };
                    let candidates: Vec<String> = vm.session.object_store.iter()
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object(o) if
                            o.fields.get("owner_id").map(|o| matches!(o, Value::Str(s) if s == &host_uuid)).unwrap_or(false)
                        ))
                        .map(|(uuid, _)| uuid.clone())
                        .collect();
                    for target_uuid in candidates {
                        let amount = inst.strength * rate;
                        if amount <= 0.0 { continue; }
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
                        if !already && crate::world::reinforce_remote(vm, &inst.overlay_name, &target_uuid, amount) { continue; }
                        if already {
                            if let Some(&eidx) = ov_host_idx.get(&(inst.overlay_name.clone(), target_uuid)) {
                                if eidx < vm.session.overlay_instances.len() {
                                    vm.session.overlay_instances[eidx].strength =
                                        (vm.session.overlay_instances[eidx].strength + amount).min(1.0);
                                }
                            }
                        } else if amount > 0.15 {
                            spread_buf.push((inst.overlay_name.clone(), target_uuid, amount));
                        }
                    }
                }

                SpreadRule::Predicate { condition, rate } => {
                    let self_val = match vm.session.object_store.get(&inst.host_uuid).cloned() {
                        Some(v) => v, None => continue,
                    };
                    let candidates: Vec<String> = vm.session.object_store.keys()
                        .filter(|uuid| *uuid != &inst.host_uuid)
                        .cloned().collect();
                    for target_uuid in candidates {
                        let target_val = match vm.session.object_store.get(&target_uuid).cloned() {
                            Some(v) => v, None => continue,
                        };
                        let fires = vm.eval_tick_expr_bool(
                            condition,
                            vec![("self", self_val.clone()), ("target", target_val)],
                        );
                        if !fires { continue; }
                        let amount = inst.strength * rate;
                        if amount <= 0.0 { continue; }
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
                        if !already && crate::world::reinforce_remote(vm, &inst.overlay_name, &target_uuid, amount) { continue; }
                        if already {
                            if let Some(&eidx) = ov_host_idx.get(&(inst.overlay_name.clone(), target_uuid)) {
                                if eidx < vm.session.overlay_instances.len() {
                                    vm.session.overlay_instances[eidx].strength =
                                        (vm.session.overlay_instances[eidx].strength + amount).min(1.0);
                                }
                            }
                        } else if amount > 0.15 {
                            spread_buf.push((inst.overlay_name.clone(), target_uuid, amount));
                        }
                    }
                }

                SpreadRule::Nearby { .. } => {} // requires map feature — not yet implemented
            }
        }
    }

    // Apply buffered spread additions
    for (ov_name, host_uuid, strength) in spread_buf {
        if let Some(def) = defs.get(&ov_name).cloned() {
            let host_var = host_name(vm, &host_uuid);
            let duration = def.default_duration;
            let is_temporary = duration.is_some();
            let clamped = strength.clamp(0.0, 1.0);
            let original_values: Vec<(String, Value)> = if is_temporary {
                if let Some(Value::Object(o)) = vm.session.object_store.get(&host_uuid) {
                    def.modifiers.iter().filter_map(|(fname, _)| {
                        o.fields.get(fname).map(|v| (fname.clone(), v.clone()))
                    }).collect()
                } else { Vec::new() }
            } else { Vec::new() };
            let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
            vm.session.des_overlay_id_counter += 1;
            overlay_push(vm, OverlayInstance {
                overlay_name: ov_name,
                host_var,
                host_uuid,
                strength: clamped,
                age: 0,
                ticks_remaining: duration,
                count: 1,
                original_values,
                extra_fields: def.extra_fields.clone(),
                des_id,
            });
        }
    }

    Ok(())
}

// ── Spawn pass ────────────────────────────────────────────────────────────────

fn spawn_pass(vm: &mut Vm) -> Result<(), GoblinError> {
    let defs = vm.session.overlay_defs.clone();
    let instances_snap: Vec<OverlayInstance> = vm.session.overlay_instances.clone();
    let mut to_spawn: Vec<(String, String, String, f64)> = Vec::new(); // (overlay_name, host_uuid, host_var, strength)

    for inst in &instances_snap {
        let def = match defs.get(&inst.overlay_name) { Some(d) => d, None => continue };
        for rule in &def.spawn_rules {
            let host_val = match vm.session.object_store.get(&inst.host_uuid).cloned() {
                Some(v) => v, None => continue,
            };
            let fires = vm.eval_tick_expr_bool(
                &rule.condition,
                vec![
                    ("strength", Value::Float(inst.strength)),
                    ("self", host_val),
                ],
            );
            if fires {
                let already = vm.session.overlay_instances.iter().any(|i| {
                    i.overlay_name == rule.spawn_overlay && i.host_uuid == inst.host_uuid
                });
                if !already {
                    to_spawn.push((rule.spawn_overlay.clone(), inst.host_uuid.clone(), inst.host_var.clone(), rule.spawn_strength));
                }
            }
        }
    }

    for (ov_name, host_uuid, host_var, strength) in to_spawn {
        if let Some(def) = defs.get(&ov_name).cloned() {
            let is_temporary = def.default_duration.is_some();
            let clamped = strength.clamp(0.0, 1.0);
            let original_values: Vec<(String, Value)> = if is_temporary {
                if let Some(Value::Object(o)) = vm.session.object_store.get(&host_uuid) {
                    def.modifiers.iter().filter_map(|(fname, _)| {
                        o.fields.get(fname).map(|v| (fname.clone(), v.clone()))
                    }).collect()
                } else { Vec::new() }
            } else { Vec::new() };
            let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
            vm.session.des_overlay_id_counter += 1;
            overlay_push(vm, OverlayInstance {
                overlay_name: ov_name,
                host_var,
                host_uuid,
                strength: clamped,
                age: 0,
                ticks_remaining: def.default_duration,
                count: 1,
                original_values,
                extra_fields: def.extra_fields.clone(),
                des_id,
            });
        }
    }
    Ok(())
}

// ── Overlay transition pass ───────────────────────────────────────────────────

fn transition_pass(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::TransitionKind;
    let defs = vm.session.overlay_defs.clone();
    let instances_snap: Vec<OverlayInstance> = vm.session.overlay_instances.clone();

    let mut to_erase: Vec<usize> = Vec::new();
    let mut to_mutate: Vec<(usize, String, IndexMap<String, Value>)> = Vec::new();
    let mut to_spawn_new: Vec<OverlayInstance> = Vec::new();

    for (idx, inst) in instances_snap.iter().enumerate() {
        let def = match defs.get(&inst.overlay_name) { Some(d) => d, None => continue };
        if def.transitions.is_empty() { continue; }

        // Build self object for trigger evaluation
        let mut self_fields = IndexMap::new();
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
            class_name: inst.overlay_name.clone(),
            fields: std::rc::Rc::new(self_fields),
            readonly_fields: BTreeSet::new(),
            trait_fields: BTreeSet::new(),
            uuid: inst.host_uuid.clone(),
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
                            original_values: Vec::new(),
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
                            original_values: Vec::new(),
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
                                original_values: Vec::new(),
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
                                    original_values: Vec::new(),
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

    // Apply mutations
    for (idx, new_name, carries) in to_mutate {
        if idx < vm.session.overlay_instances.len() {
            vm.session.overlay_instances[idx].overlay_name = new_name;
            for (k, v) in carries {
                if k == "strength" {
                    if let Value::Float(f) = v {
                        vm.session.overlay_instances[idx].strength = f.clamp(0.0, 1.0);
                    }
                } else {
                    vm.session.overlay_instances[idx].extra_fields.insert(k, v);
                }
            }
        }
    }
    for new_inst in to_spawn_new {
        overlay_push(vm, new_inst);
    }
    let mut to_erase_dedup = to_erase;
    to_erase_dedup.sort_unstable();
    to_erase_dedup.dedup();
    for idx in to_erase_dedup.iter().rev() {
        if *idx < vm.session.overlay_instances.len() {
            vm.session.overlay_instances.remove(*idx);
        }
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
    let selves: Vec<(String, String)> = vm.session.object_store.iter()
        .filter_map(|(u, v)| match v {
            Value::Object(o) if classes.get(&o.class_name).is_some_and(|c| !c.every.is_empty()) =>
                Some((u.clone(), o.class_name.clone())),
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
            vm.call_callable(Value::Function(f), vec![Value::Ref(uuid.clone())])?;
        }
    }
    Ok(())
}

fn decision_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    // Find all objects with a decision formula
    let candidates: Vec<(String, String)> = {
        let mut out = Vec::new();
        for (uuid, val) in &vm.session.object_store {
            if let Value::Object(o) = val {
                let class_name = &o.class_name;
                let has_decision = vm.session.classes.get(class_name)
                    .map(|c| c.decision.is_some())
                    .unwrap_or(false)
                    || vm.session.object_decisions.contains_key(uuid.as_str());
                // Another region's object decides in its own region.
                if has_decision && !crate::world::is_remote(vm, uuid) {
                    out.push((uuid.clone(), class_name.clone()));
                }
            }
        }
        out
    };

    let mut targets = Targets::default();
    for (self_uuid, class_name) in candidates {
        // Borrow the definitions (an Rc copy, not a deep one), so each
        // formula keeps its address for the tick-expr cache. Taken per
        // object, so a class an earlier action installed is seen here.
        let classes = vm.session.classes.clone();
        let Some(class) = classes.get(&class_name) else { continue };
        let obj_decision = vm.session.object_decisions.get(&self_uuid).cloned();
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
        for (name, decision, judge) in list {
            // Erased by an earlier decision's action: nothing more to decide.
            if !vm.session.object_store.contains_key(&self_uuid) { break; }
            decide(vm, &mut targets, &self_uuid, name, decision, judge)?;
        }
    }

    Ok(())
}

/// One decision of one object: score the targets, pick the best (within
/// the object's `margin`), and run the action its judge picks.
/// The objects of each target class, in the order they were made, kept
/// across one decision pass while no object is added or removed. A store
/// that only grows at its end and keeps its order on removal changes its
/// length or its last key whenever its set of objects changes.
#[derive(Default)]
struct Targets {
    seen: Option<(usize, String)>,
    /// A `Value::Ref` to each object.
    by_class: std::collections::HashMap<String, std::rc::Rc<Vec<Value>>>,
    /// Per decision (by address): whether its formula reads a `~` field.
    real: std::collections::HashMap<usize, bool>,
}

impl Targets {
    fn of(&mut self, vm: &Vm, class: &str) -> std::rc::Rc<Vec<Value>> {
        let store = &vm.session.object_store;
        let now = (store.len(), store.last().map(|(k, _)| k.clone()).unwrap_or_default());
        if self.seen.as_ref() != Some(&now) {
            self.by_class.clear();
            self.seen = Some(now);
        }
        self.by_class.entry(class.to_string()).or_insert_with(|| std::rc::Rc::new(
            store.iter()
                .filter(|(_, v)| matches!(v, Value::Object(o) if o.class_name == class))
                .map(|(u, _)| Value::Ref(u.clone()))
                .collect()
        )).clone()
    }

    /// Whether `decision`'s formula reads a field declared with `~` (an
    /// actual value, not a 0..1 trait) on `self` or `target`.
    fn reads_real(&mut self, vm: &Vm, self_class: &str, decision: &goblin_ast::DecisionDef) -> bool {
        let key = decision as *const _ as usize;
        if let Some(&r) = self.real.get(&key) { return r; }
        let is_real = |class: &str, field: &str| vm.session.classes.get(class)
            .map(|c| c.fields.iter().any(|f| f.name == field && f.raw))
            .unwrap_or(false);
        let mut fields = Vec::new();
        member_reads(&decision.formula, &mut fields);
        let r = fields.iter().any(|(who, field)| match who.as_str() {
            "self" => is_real(self_class, field),
            _ => is_real(&decision.target_class, field),
        });
        self.real.insert(key, r);
        r
    }
}

/// The `self >> field` / `target >> field` reads in a formula.
fn member_reads(e: &goblin_ast::Expr, out: &mut Vec<(String, String)>) {
    use goblin_ast::Expr as E;
    match e {
        E::Member(obj, field, _) | E::OptMember(obj, field, _) => match obj.as_ref() {
            E::Ident(who, _) if who == "self" || who == "target" => out.push((who.clone(), field.clone())),
            other => member_reads(other, out),
        },
        E::Binary(l, _, r, _) => { member_reads(l, out); member_reads(r, out); }
        E::Prefix(_, x, _) | E::Postfix(x, _, _) => member_reads(x, out),
        E::FreeCall(_, args, _) | E::NsCall(_, _, args, _) | E::Array(args, _) => args.iter().for_each(|a| member_reads(a, out)),
        E::Call(recv, _, args, _) | E::OptCall(recv, _, args, _) => { member_reads(recv, out); args.iter().for_each(|a| member_reads(a, out)) }
        E::Index(a, b, _) | E::IndexMap(a, b, _) => { member_reads(a, out); member_reads(b, out) }
        E::Object(kv, _) => kv.iter().for_each(|(_, v)| member_reads(v, out)),
        _ => {}
    }
}

fn decide(vm: &mut Vm, targets: &mut Targets, self_uuid: &str, name: &str, decision: &goblin_ast::DecisionDef, judge: &goblin_ast::JudgeStmt) -> Result<(), GoblinError> {
    let self_uuid = self_uuid.to_string();
    let margin = match vm.session.object_store.get(&self_uuid) {
        Some(Value::Object(o)) => o.fields.get("margin").and_then(|v| match v {
            Value::Float(f) => Some(*f),
            Value::Int(i) => Some(*i as f64),
            _ => None,
        }).unwrap_or(0.0),
        Some(_) => 0.0,
        None => return Ok(()),
    };
    // Formulas see `self` and `target` by reference: they read fields
    // where the objects live instead of copying every candidate.
    let self_ref = Value::Ref(self_uuid.clone());

    let target_candidates = targets.of(vm, &decision.target_class);
    let is_self = |v: &Value| matches!(v, Value::Ref(u) if *u == self_uuid);

    // Score all targets (by position in the list), the formula compiled once.
    // (A formula that does not compile scores no target, as before.)
    let Ok(formula) = vm.tick_formula(&decision.formula, &["self", "target"]) else { return Ok(()) };
    let mut scored: Vec<(usize, f64)> = Vec::with_capacity(target_candidates.len());
    for (k, target) in target_candidates.iter().enumerate() {
        if is_self(target) { continue; }
        let raw = match vm.run_tick_formula_ref(&formula, &[("self", &self_ref), ("target", target)]) {
            Ok(Value::Float(f)) => f,
            Ok(Value::Int(i)) => i as f64,
            Ok(_) => 0.5,
            Err(_) => continue,
        };
        // A score that overflowed (inf) or is not a number can't be ranked.
        if !raw.is_finite() { continue; }
        scored.push((k, raw));
    }
    if scored.is_empty() { return Ok(()); }
    // A formula on `~` fields (prices, money, ...) has no fixed range, so
    // its scores are measured against the candidates: the best scores 1
    // and the others in proportion to it (0 stays 0). When some score is
    // below 0 they are spread from the worst (0) to the best (1) instead.
    // A formula on 0..1 traits keeps its own range.
    let self_class = match vm.session.object_store.get(&self_uuid) {
        Some(Value::Object(o)) => o.class_name.clone(),
        _ => String::new(),
    };
    if targets.reads_real(vm, &self_class, decision) {
        let lo = scored.iter().map(|(_, r)| *r).fold(f64::INFINITY, f64::min);
        let hi = scored.iter().map(|(_, r)| *r).fold(f64::NEG_INFINITY, f64::max);
        for (_, r) in scored.iter_mut() {
            *r = if lo >= 0.0 {
                if hi <= 0.0 { 0.0 } else { *r / hi }
            } else if hi - lo < 1e-12 { 1.0 } else { (*r - lo) / (hi - lo) };
        }
    } else {
        for (_, r) in scored.iter_mut() { *r = normalize_score(*r, decision.formula_min, decision.formula_max); }
    }

    let best = scored.iter().map(|(_, s)| *s).fold(f64::NEG_INFINITY, f64::max);

    let pool: Vec<(usize, f64)> = scored.into_iter()
        .filter(|(_, s)| *s >= best - margin)
        .collect();

    if pool.is_empty() { return Ok(()); }
    let chosen_idx = if pool.len() == 1 { 0 } else {
        let r = vm.session.next_u128() as f64 / u128::MAX as f64;
        (r * pool.len() as f64).floor() as usize % pool.len()
    };
    let Some((best_k, final_score)) = pool.into_iter().nth(chosen_idx) else { return Ok(()) };
    let Value::Ref(best_target_uuid) = &target_candidates[best_k] else { return Ok(()) };
    let best_target_uuid = best_target_uuid.clone();
    let final_score = final_score.clamp(0.0, 1.0);
    let target_ref = Value::Ref(best_target_uuid.clone());

    // Run judge to find selected action
    let mut selected_action: Option<String> = None;
    let mut else_action: Option<String> = None;

    for arm in &judge.arms {
        match &arm.condition {
            None => {
                if let Some(name) = extract_action_call_name(&arm.body) {
                    else_action = Some(name);
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
                    if let Some(name) = extract_action_call_name(&arm.body) {
                        selected_action = Some(name);
                        break;
                    }
                }
            }
        }
    }

    let action_name = selected_action.or(else_action).unwrap_or_default();
    if !action_name.is_empty() {
        // The action gets the objects as they are now (nothing above
        // changes them), the same values it was given before.
        let (Some(self_val), Some(target_val)) = (
            vm.session.object_store.get(&self_uuid).cloned(),
            vm.session.object_store.get(&best_target_uuid).cloned(),
        ) else { return Ok(()) };
        let _ = vm.call_named(&action_name, vec![self_val, target_val]);
        // Ignore errors — decisions are best-effort
    }
    Ok(())
}

// ── Object transition tick ────────────────────────────────────────────────────

fn object_transition_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::TransitionKind;

    let candidates: Vec<(String, String)> = vm.session.object_store.iter()
        .filter_map(|(uuid, v)| {
            if let Value::Object(o) = v {
                let class_name = &o.class_name;
                let has_transitions = vm.session.classes.get(class_name)
                    .map(|c| !c.transitions.is_empty())
                    .unwrap_or(false);
                if has_transitions && !crate::world::is_remote(vm, uuid) { Some((uuid.clone(), class_name.clone())) }
                else { None }
            } else { None }
        }).collect();

    let mut erased: std::collections::HashSet<String> = std::collections::HashSet::new();

    for (self_uuid, class_name) in &candidates {
        if erased.contains(self_uuid) { continue; }

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
                    vm.session.object_store.shift_remove(self_uuid);
                    erased.insert(self_uuid.clone());
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
                        let mut fields = IndexMap::new();
                        for fd in &child_decl.fields {
                            if let Some(ref default_expr) = fd.default {
                                if let Ok(v) = vm.eval_tick_expr(default_expr, vec![]) {
                                    fields.insert(fd.name.clone(), v);
                                }
                            }
                        }
                        use std::collections::BTreeSet;
                        let child_obj = Value::Object(Box::new(crate::value::ObjectVal {
                            class_name: child_class.clone(),
                            fields: std::rc::Rc::new(fields),
                            readonly_fields: BTreeSet::new(),
                            trait_fields: BTreeSet::new(),
                            uuid: new_uuid.clone(),
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

pub(crate) fn eval_link_score(
    vm: &mut Vm,
    self_val: Value,
    target_val: Value,
    formula: &goblin_ast::Expr,
    formula_min: f64,
    formula_max: f64,
) -> Result<f64, GoblinError> {
    let raw = match vm.eval_tick_expr(formula, vec![("self", self_val), ("target", target_val)])? {
        Value::Float(f) => f,
        Value::Int(i) => i as f64,
        _ => return Ok(0.5),
    };
    let range = formula_max - formula_min;
    let normalized = if range.abs() < 1e-12 { 0.5 } else {
        ((raw - formula_min) / range).clamp(0.0, 1.0)
    };
    Ok(normalized)
}

/// A raw formula value scaled to 0..1 over the formula's range, as
/// `eval_link_score` does.
fn normalize_score(raw: f64, formula_min: f64, formula_max: f64) -> f64 {
    let range = formula_max - formula_min;
    if range.abs() < 1e-12 { 0.5 } else { ((raw - formula_min) / range).clamp(0.0, 1.0) }
}

pub(crate) fn overlay_push(vm: &mut Vm, inst: OverlayInstance) {
    // On another region's object: its owner applies it, next tick.
    if crate::world::send_overlay(vm, &inst) { return; }
    use crate::session::OverlayApplyBehavior;
    let behavior = vm.session.overlay_defs.get(&inst.overlay_name)
        .map(|d| d.apply_behavior.clone())
        .unwrap_or(OverlayApplyBehavior::Caps);

    match behavior {
        OverlayApplyBehavior::Caps => {
            let exists = vm.session.overlay_instances.iter()
                .any(|i| i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid);
            if !exists {
                apply_overlay_modifiers(vm, &inst.host_uuid, &inst.overlay_name, inst.strength, inst.count, &inst.extra_fields.clone());
                vm.session.overlay_instances.push(inst);
            }
        }
        OverlayApplyBehavior::Replaces => {
            let existing_pos = vm.session.overlay_instances.iter().position(|i| {
                i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid
            });
            if let Some(pos) = existing_pos {
                let old_originals = vm.session.overlay_instances[pos].original_values.clone();
                let host_uuid = vm.session.overlay_instances[pos].host_uuid.clone();
                if !old_originals.is_empty() {
                    restore_originals(vm, &host_uuid, &old_originals);
                }
                vm.session.overlay_instances[pos].strength = inst.strength;
                vm.session.overlay_instances[pos].age = inst.age;
                vm.session.overlay_instances[pos].count = 1;
                vm.session.overlay_instances[pos].ticks_remaining = inst.ticks_remaining;
                vm.session.overlay_instances[pos].original_values = inst.original_values;
                vm.session.overlay_instances[pos].extra_fields = inst.extra_fields.clone();
                apply_overlay_modifiers(vm, &host_uuid, &inst.overlay_name, inst.strength, 1, &inst.extra_fields);
            } else {
                apply_overlay_modifiers(vm, &inst.host_uuid, &inst.overlay_name, inst.strength, 1, &inst.extra_fields.clone());
                vm.session.overlay_instances.push(inst);
            }
        }
        OverlayApplyBehavior::Stacks { .. } => {
            let existing_pos = vm.session.overlay_instances.iter().position(|i| {
                i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid
            });
            if let Some(pos) = existing_pos {
                vm.session.overlay_instances[pos].strength = (vm.session.overlay_instances[pos].strength + inst.strength).min(1.0);
                vm.session.overlay_instances[pos].count += 1;
                if inst.ticks_remaining.is_some() {
                    vm.session.overlay_instances[pos].ticks_remaining = inst.ticks_remaining;
                }
                let host_uuid = vm.session.overlay_instances[pos].host_uuid.clone();
                let extra = vm.session.overlay_instances[pos].extra_fields.clone();
                apply_overlay_modifiers(vm, &host_uuid, &inst.overlay_name, inst.strength, 1, &extra);
            } else {
                apply_overlay_modifiers(vm, &inst.host_uuid, &inst.overlay_name, inst.strength, 1, &inst.extra_fields.clone());
                vm.session.overlay_instances.push(inst);
            }
        }
    }
}

pub fn apply_overlay_modifiers(
    vm: &mut Vm,
    host_uuid: &str,
    overlay_name: &str,
    strength: f64,
    count: u64,
    extra_fields: &indexmap::IndexMap<String, Value>,
) {
    let def = match vm.session.overlay_defs.get(overlay_name) {
        Some(d) if !d.modifiers.is_empty() => d.clone(),
        _ => return,
    };
    let modifiers = &def.modifiers;

    // A `stacks as <label>` overlay also sees its count under that label.
    let count_label: Option<String> = vm.session.overlay_defs.get(overlay_name).and_then(|d| {
        use crate::session::OverlayApplyBehavior;
        if let OverlayApplyBehavior::Stacks { label: Some(ref l) } = d.apply_behavior { Some(l.clone()) } else { None }
    });
    let mut deltas: Vec<(String, f64)> = Vec::new();
    for (fname, expr) in modifiers {
        let mut bindings = vec![
            ("strength", Value::Float(strength)),
            ("count", Value::Int(count as i64)),
        ];
        if let Some(ref l) = count_label { bindings.push((l.as_str(), Value::Int(count as i64))); }
        for (k, v) in extra_fields {
            bindings.push((k.as_str(), v.clone()));
        }
        // We need owned bindings for eval_tick_expr
        let owned_bindings: Vec<(&str, Value)> = bindings;
        let delta = match vm.eval_tick_expr(expr, owned_bindings) {
            Ok(Value::Float(f)) => f,
            Ok(Value::Int(i)) => i as f64,
            _ => continue,
        };
        deltas.push((fname.clone(), delta));
    }

    if deltas.is_empty() { return; }

    if let Some(obj) = vm.session.object_store.get_mut(host_uuid) {
        if let Value::Object(ref mut o) = obj {
            let trait_fields = o.trait_fields.clone();
            let fields = std::rc::Rc::make_mut(&mut o.fields);
            for (field_name, delta) in deltas {
                if let Some(field_val) = fields.get_mut(&field_name) {
                    let current = match field_val {
                        Value::Float(f) => *f,
                        Value::Int(i) => *i as f64,
                        _ => continue,
                    };
                    let new_val = current + delta;
                    let new_val = if trait_fields.contains(&field_name) {
                        let clamped = new_val.clamp(0.0, 1.0);
                        if clamped < 1e-10 { 0.0 } else if clamped > 1.0 - 1e-10 { 1.0 } else { clamped }
                    } else {
                        new_val
                    };
                    *field_val = Value::Float(new_val);
                }
            }
        }
    }
}

fn restore_originals(vm: &mut Vm, host_uuid: &str, originals: &[(String, Value)]) {
    if let Some(Value::Object(ref mut o)) = vm.session.object_store.get_mut(host_uuid) {
        let fields = std::rc::Rc::make_mut(&mut o.fields);
        for (fname, fval) in originals {
            fields.insert(fname.clone(), fval.clone());
        }
    }
}

fn extract_action_call_name(body: &goblin_ast::JudgeArmBody) -> Option<String> {
    use goblin_ast::{JudgeArmBody, Expr};
    match body {
        JudgeArmBody::Expr(Expr::FreeCall(name, _, _)) => Some(name.clone()),
        JudgeArmBody::Expr(Expr::Ident(name, _)) => Some(name.clone()),
        JudgeArmBody::Stmts(stmts) => {
            for s in stmts {
                if let goblin_ast::Stmt::Expr(Expr::FreeCall(name, _, _)) = s {
                    return Some(name.clone());
                }
                if let goblin_ast::Stmt::Expr(Expr::Ident(name, _)) = s {
                    return Some(name.clone());
                }
            }
            None
        }
        _ => None,
    }
}

/// The variable name an object's overlays report as `host_var`.
pub(crate) fn host_name(vm: &Vm, uuid: &str) -> String {
    vm.session.object_names.get(uuid).map(|(n, _)| n.clone()).unwrap_or_else(|| uuid.to_string())
}
