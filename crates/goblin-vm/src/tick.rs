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
                if let Value::Object { ref mut fields, .. } = obj {
                    let fields = std::rc::Rc::make_mut(fields);
                    for (field, fv) in changes {
                        let val = match fv {
                            FieldValue::Float(f) => Value::Float(f),
                            FieldValue::Int(i)   => Value::Int(i),
                            FieldValue::Bool(b)  => Value::Bool(b),
                            FieldValue::Str(s)   => Value::Str(s),
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

    // ── Pass 8: Decision tick ─────────────────────────────────────────────────
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
        ov_host_idx.insert((inst.overlay_name.clone(), inst.host_var.clone()), idx);
    }

    let mut spread_buf: Vec<(String, String, f64)> = Vec::new(); // (overlay_name, host_uuid, strength)

    for inst in &instances_snap {
        let def = match defs.get(&inst.overlay_name) { Some(d) => d, None => continue };

        for rule in &def.spread_rules {
            match rule {
                SpreadRule::Channel { channel, rate } => {
                    let host_class = match vm.session.object_store.get(&inst.host_uuid) {
                        Some(Value::Object { class_name, .. }) => class_name.clone(),
                        _ => continue,
                    };
                    let link_def = vm.session.link_defs.get(&(host_class.clone(), channel.clone())).cloned()
                        .or_else(|| vm.session.link_defs.get(&(host_class.clone(), "default".to_string())).cloned());
                    let link_def = match link_def { Some(d) => d, None => continue };
                    let self_val = match vm.session.object_store.get(&inst.host_uuid).cloned() {
                        Some(v) => v, None => continue,
                    };
                    let candidates: Vec<(String, Value)> = vm.session.object_store.iter()
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object { class_name, .. } if class_name == &host_class))
                        .map(|(uuid, v)| (uuid.clone(), v.clone()))
                        .collect();
                    for (target_uuid, target_val) in candidates {
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
                        let link_score = eval_link_score(vm, self_val.clone(), target_val, &link_def.formula, link_def.formula_min, link_def.formula_max)?;
                        if link_score < 0.3 { continue; }
                        let amount = inst.strength * inst.strength * rate * link_score;
                        if amount <= 0.0 { continue; }
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
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object { class_name: cn, .. } if cn == class_name))
                        .map(|(uuid, _)| uuid.clone())
                        .collect();
                    for target_uuid in candidates {
                        let amount = inst.strength * inst.strength * rate;
                        if amount <= 0.0 { continue; }
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
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
                        Some(Value::Object { uuid, .. }) => uuid.clone(),
                        _ => continue,
                    };
                    let candidates: Vec<String> = vm.session.object_store.iter()
                        .filter(|(uuid, v)| *uuid != &inst.host_uuid && matches!(v, Value::Object { fields, .. } if
                            fields.get("owner_id").map(|o| matches!(o, Value::Str(s) if s == &host_uuid)).unwrap_or(false)
                        ))
                        .map(|(uuid, _)| uuid.clone())
                        .collect();
                    for target_uuid in candidates {
                        let amount = inst.strength * rate;
                        if amount <= 0.0 { continue; }
                        let already = ov_host_idx.contains_key(&(inst.overlay_name.clone(), target_uuid.clone()));
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
            let duration = def.default_duration;
            let is_temporary = duration.is_some();
            let clamped = strength.clamp(0.0, 1.0);
            let original_values: Vec<(String, Value)> = if is_temporary {
                if let Some(Value::Object { fields, .. }) = vm.session.object_store.get(&host_uuid) {
                    def.modifiers.iter().filter_map(|(fname, _)| {
                        fields.get(fname).map(|v| (fname.clone(), v.clone()))
                    }).collect()
                } else { Vec::new() }
            } else { Vec::new() };
            let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
            vm.session.des_overlay_id_counter += 1;
            overlay_push(vm, OverlayInstance {
                overlay_name: ov_name,
                host_var: host_uuid.clone(),
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
    let mut to_spawn: Vec<(String, String, f64)> = Vec::new(); // (overlay_name, host_uuid, strength)

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
                    to_spawn.push((rule.spawn_overlay.clone(), inst.host_uuid.clone(), rule.spawn_strength));
                }
            }
        }
    }

    for (ov_name, host_uuid, strength) in to_spawn {
        if let Some(def) = defs.get(&ov_name).cloned() {
            let is_temporary = def.default_duration.is_some();
            let clamped = strength.clamp(0.0, 1.0);
            let original_values: Vec<(String, Value)> = if is_temporary {
                if let Some(Value::Object { fields, .. }) = vm.session.object_store.get(&host_uuid) {
                    def.modifiers.iter().filter_map(|(fname, _)| {
                        fields.get(fname).map(|v| (fname.clone(), v.clone()))
                    }).collect()
                } else { Vec::new() }
            } else { Vec::new() };
            let des_id = OverlayInstanceId(vm.session.des_overlay_id_counter);
            vm.session.des_overlay_id_counter += 1;
            overlay_push(vm, OverlayInstance {
                overlay_name: ov_name,
                host_var: host_uuid.clone(),
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
        self_fields.insert("overlay_name".to_string(), Value::Str(inst.overlay_name.clone()));
        self_fields.insert("host_uuid".to_string(), Value::Str(inst.host_uuid.clone()));
        self_fields.insert("host_var".to_string(), Value::Str(inst.host_var.clone()));
        for (k, v) in &inst.extra_fields {
            self_fields.insert(k.clone(), v.clone());
        }
        use std::collections::BTreeSet;
        let self_val = Value::Object {
            class_name: inst.overlay_name.clone(),
            fields: std::rc::Rc::new(self_fields),
            readonly_fields: BTreeSet::new(),
            trait_fields: BTreeSet::new(),
            uuid: inst.host_uuid.clone(),
        };

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

fn decision_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    // Find all objects with a decision formula
    let candidates: Vec<(String, String)> = {
        let mut out = Vec::new();
        for (uuid, val) in &vm.session.object_store {
            if let Value::Object { class_name, .. } = val {
                let has_decision = vm.session.classes.get(class_name)
                    .map(|c| c.decision.is_some())
                    .unwrap_or(false)
                    || vm.session.object_decisions.contains_key(uuid.as_str());
                if has_decision {
                    out.push((uuid.clone(), class_name.clone()));
                }
            }
        }
        out
    };

    for (self_uuid, class_name) in candidates {
        let (decision, judge) = {
            if let Some(obj_decision) = vm.session.object_decisions.get(&self_uuid).cloned() {
                let class = match vm.session.classes.get(&class_name).cloned() { Some(c) => c, None => continue };
                let judge = match class.judge.clone() { Some(j) => j, None => continue };
                (obj_decision, judge)
            } else {
                let class = match vm.session.classes.get(&class_name).cloned() { Some(c) => c, None => continue };
                match (class.decision.clone(), class.judge.clone()) {
                    (Some(d), Some(j)) => (d, j),
                    _ => continue,
                }
            }
        };

        let self_val = match vm.session.object_store.get(&self_uuid).cloned() { Some(v) => v, None => continue };

        let target_candidates: Vec<(String, Value)> = vm.session.object_store.iter()
            .filter(|(uuid, v)| *uuid != &self_uuid && matches!(v, Value::Object { class_name: cn, .. } if cn == &decision.target_class))
            .map(|(uuid, v)| (uuid.clone(), v.clone()))
            .collect();

        if target_candidates.is_empty() { continue; }

        // Score all targets
        let mut scored: Vec<(String, Value, f64)> = Vec::new();
        for (target_uuid, target_val) in &target_candidates {
            let raw_score = match eval_link_score(vm, self_val.clone(), target_val.clone(), &decision.formula, decision.formula_min, decision.formula_max) {
                Ok(s) => s,
                Err(_) => continue,
            };
            scored.push((target_uuid.clone(), target_val.clone(), raw_score));
        }
        if scored.is_empty() { continue; }

        let best = scored.iter().map(|(_, _, s)| *s).fold(f64::NEG_INFINITY, f64::max);
        let margin = if let Value::Object { ref fields, .. } = self_val {
            fields.get("margin").and_then(|v| match v {
                Value::Float(f) => Some(*f),
                Value::Int(i) => Some(*i as f64),
                _ => None,
            }).unwrap_or(0.0)
        } else { 0.0 };

        let pool: Vec<(String, Value, f64)> = scored.into_iter()
            .filter(|(_, _, s)| *s >= best - margin)
            .collect();

        let chosen_idx = if pool.len() == 1 { 0 } else {
            let r = vm.session.next_u128() as f64 / u128::MAX as f64;
            (r * pool.len() as f64).floor() as usize % pool.len()
        };
        let (best_target_uuid, target_val_owned, final_score) = pool.into_iter().nth(chosen_idx).unwrap();
        let final_score = final_score.clamp(0.0, 1.0);

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
                    let fires = vm.eval_tick_expr_bool(
                        cond,
                        vec![
                            ("score", Value::Float(final_score)),
                            ("self", self_val.clone()),
                            ("target", target_val_owned.clone()),
                        ],
                    );
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
            let _ = vm.call_named(&action_name, vec![self_val, target_val_owned]);
            // Ignore errors — decisions are best-effort
        }
    }

    Ok(())
}

// ── Object transition tick ────────────────────────────────────────────────────

fn object_transition_tick(vm: &mut Vm) -> Result<(), GoblinError> {
    use goblin_ast::TransitionKind;

    let candidates: Vec<(String, String)> = vm.session.object_store.iter()
        .filter_map(|(uuid, v)| {
            if let Value::Object { class_name, .. } = v {
                let has_transitions = vm.session.classes.get(class_name)
                    .map(|c| !c.transitions.is_empty())
                    .unwrap_or(false);
                if has_transitions { Some((uuid.clone(), class_name.clone())) }
                else { None }
            } else { None }
        }).collect();

    let mut erased: std::collections::HashSet<String> = std::collections::HashSet::new();

    for (self_uuid, class_name) in &candidates {
        if erased.contains(self_uuid) { continue; }

        let transitions = match vm.session.classes.get(class_name) {
            Some(c) => c.transitions.clone(),
            None => continue,
        };
        let self_val = match vm.session.object_store.get(self_uuid).cloned() { Some(v) => v, None => continue };

        for tdef in &transitions {
            let trigger_fired = vm.eval_tick_expr_bool(
                &tdef.trigger,
                vec![("self", self_val.clone())],
            );
            if !trigger_fired { continue; }

            match tdef.kind {
                TransitionKind::Erase => {
                    vm.session.object_store.remove(self_uuid);
                    erased.insert(self_uuid.clone());
                    break;
                }
                TransitionKind::Spawn => {
                    let child_class = tdef.into_classes.first().cloned().unwrap_or(class_name.clone());
                    if let Some(child_decl) = vm.session.classes.get(&child_class).cloned() {
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
                        let child_obj = Value::Object {
                            class_name: child_class.clone(),
                            fields: std::rc::Rc::new(fields),
                            readonly_fields: BTreeSet::new(),
                            trait_fields: BTreeSet::new(),
                            uuid: new_uuid.clone(),
                        };
                        vm.session.object_store.insert(new_uuid, child_obj);
                    }
                    break;
                }
                TransitionKind::Mutate => {
                    let new_class = tdef.into_classes.first().cloned().unwrap_or(class_name.clone());
                    if let Some(Value::Object { fields, .. }) = vm.session.object_store.get_mut(self_uuid) {
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

fn eval_link_score(
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

fn overlay_push(vm: &mut Vm, inst: OverlayInstance) {
    use crate::session::OverlayApplyBehavior;
    let behavior = vm.session.overlay_defs.get(&inst.overlay_name)
        .map(|d| d.apply_behavior.clone())
        .unwrap_or(OverlayApplyBehavior::Caps);

    match behavior {
        OverlayApplyBehavior::Caps => {
            let exists = vm.session.overlay_instances.iter()
                .any(|i| i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid);
            if !exists {
                vm.session.overlay_instances.push(inst);
            }
        }
        OverlayApplyBehavior::Replaces => {
            vm.session.overlay_instances.retain(|i| {
                !(i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid)
            });
            vm.session.overlay_instances.push(inst);
        }
        OverlayApplyBehavior::Stacks { .. } => {
            if let Some(existing) = vm.session.overlay_instances.iter_mut()
                .find(|i| i.overlay_name == inst.overlay_name && i.host_uuid == inst.host_uuid)
            {
                existing.strength = (existing.strength + inst.strength).min(1.0);
                existing.count += 1;
            } else {
                vm.session.overlay_instances.push(inst);
            }
        }
    }
}

fn restore_originals(vm: &mut Vm, host_uuid: &str, originals: &[(String, Value)]) {
    if let Some(Value::Object { ref mut fields, .. }) = vm.session.object_store.get_mut(host_uuid) {
        let fields = std::rc::Rc::make_mut(fields);
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
