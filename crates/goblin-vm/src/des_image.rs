//! The DES part of a session, in a form that can cross to another thread.
//!
//! An actor (a region) needs the program's DES definitions: overlay, link
//! and per-object decision definitions, as it needs its classes. It also
//! takes the overlays that sit on the objects handed to it, so they move
//! with their host instead of staying behind.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::session::{LinkDef, LinkOffset, OverlayApplyBehavior, OverlayDef, OverlayInstance, OverlayInstanceId, Session};

pub(crate) struct POverlayDef {
    name: String,
    host_types: Vec<String>,
    decay_rate: f64,
    default_duration: Option<u32>,
    apply_behavior: OverlayApplyBehavior,
    modifiers: Vec<(String, goblin_ast::Expr)>,
    conflict_rules: Vec<(String, f64)>,
    spread_rules: Vec<goblin_ast::SpreadRule>,
    spawn_rules: Vec<goblin_ast::OverlaySpawnRule>,
    transitions: Vec<goblin_ast::TransitionDef>,
    extra_fields: Vec<(String, PValue)>,
}

pub(crate) struct POverlay {
    overlay_name: String,
    host_var: String,
    host_uuid: String,
    strength: f64,
    age: u32,
    ticks_remaining: Option<u32>,
    count: u64,
    original_values: Vec<(String, PValue)>,
    extra_fields: Vec<(String, PValue)>,
    des_id: u32,
}

/// DES definitions plus the overlays (and host names) that go along.
#[derive(Default)]
pub(crate) struct DesImage {
    overlay_defs: Vec<POverlayDef>,
    link_defs: Vec<((String, String), LinkDef)>,
    object_link_defs: Vec<((String, String), LinkDef)>,
    object_decisions: Vec<(String, goblin_ast::DecisionDef)>,
    overlays: Vec<POverlay>,
    object_names: Vec<(String, (String, bool))>,
    /// Link offsets (`a link to b on ch offset ...`) from or to the
    /// objects handed over, with the ticks they have left.
    link_offsets: Vec<((String, String, String), Vec<LinkOffset>)>,
    overlay_ids: u32,
}

/// The names the handed objects were stored under.
pub(crate) fn handed_names(session: &Session, objs: &HashSet<String>) -> HashSet<String> {
    objs.iter().filter_map(|u| session.object_names.get(u).map(|(n, _)| n.clone())).collect()
}

fn pfields(tp: &mut ToPortable, m: &indexmap::IndexMap<String, crate::value::Value>) -> Result<Vec<(String, PValue)>, GoblinError> {
    m.iter().map(|(k, v)| Ok((k.clone(), tp.value(v)?))).collect()
}

/// The session's DES definitions. With `objects`, also the overlays on those
/// objects and the names they were stored under.
pub(crate) fn des_image_of(session: &Session, tp: &mut ToPortable, objects: Option<&HashSet<String>>) -> Result<DesImage, GoblinError> {
    let mut img = DesImage { overlay_ids: session.des_overlay_id_counter, ..Default::default() };
    // In name order, so two runs build the same tables.
    let mut names: Vec<&String> = session.overlay_defs.keys().collect();
    names.sort();
    for n in names {
        let d = &session.overlay_defs[n];
        img.overlay_defs.push(POverlayDef {
            name: d.name.clone(),
            host_types: d.host_types.clone(),
            decay_rate: d.decay_rate,
            default_duration: d.default_duration,
            apply_behavior: d.apply_behavior.clone(),
            modifiers: d.modifiers.clone(),
            conflict_rules: d.conflict_rules.clone(),
            spread_rules: d.spread_rules.clone(),
            spawn_rules: d.spawn_rules.clone(),
            transitions: d.transitions.clone(),
            extra_fields: pfields(tp, &d.extra_fields)?,
        });
    }
    img.link_defs = session.link_defs.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    img.object_link_defs = session.object_link_defs.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    img.object_decisions = session.object_decisions.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    if let Some(objs) = objects {
        for inst in session.overlay_instances.iter().filter(|i| objs.contains(&i.host_uuid)) {
            img.overlays.push(POverlay {
                overlay_name: inst.overlay_name.clone(),
                host_var: inst.host_var.clone(),
                host_uuid: inst.host_uuid.clone(),
                strength: inst.strength,
                age: inst.age,
                ticks_remaining: inst.ticks_remaining,
                count: inst.count,
                original_values: inst.original_values.iter()
                    .map(|(k, v)| Ok((k.clone(), tp.value(v)?)))
                    .collect::<Result<_, GoblinError>>()?,
                extra_fields: pfields(tp, &inst.extra_fields)?,
                des_id: inst.des_id.0,
            });
        }
        for u in objs {
            if let Some(n) = session.object_names.get(u) {
                img.object_names.push((u.clone(), n.clone()));
            }
        }
        img.object_names.sort();
        if !session.link_offsets.is_empty() {
            let names = handed_names(session, objs);
            img.link_offsets = session.link_offsets.iter()
                .filter(|((from, to, _), _)| names.contains(from) || names.contains(to))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            img.link_offsets.sort_by(|a, b| a.0.cmp(&b.0));
        }
    }
    Ok(img)
}

/// Installs the definitions and overlays in a freshly built session.
pub(crate) fn install(img: &DesImage, fp: &mut FromPortable, s: &mut Session) {
    let mut defs = HashMap::with_capacity(img.overlay_defs.len());
    for d in &img.overlay_defs {
        let extra_fields = d.extra_fields.iter().map(|(k, v)| (k.clone(), fp.value(v, s))).collect();
        defs.insert(d.name.clone(), Rc::new(OverlayDef {
            name: d.name.clone(),
            host_types: d.host_types.clone(),
            decay_rate: d.decay_rate,
            default_duration: d.default_duration,
            apply_behavior: d.apply_behavior.clone(),
            modifiers: d.modifiers.clone(),
            conflict_rules: d.conflict_rules.clone(),
            spread_rules: d.spread_rules.clone(),
            spawn_rules: d.spawn_rules.clone(),
            transitions: d.transitions.clone(),
            extra_fields,
        }));
    }
    s.overlay_defs = defs;
    s.link_defs = img.link_defs.iter().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
    s.object_link_defs = img.object_link_defs.iter().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
    s.object_decisions = img.object_decisions.iter().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
    s.des_overlay_id_counter = img.overlay_ids;
    for (u, n) in &img.object_names {
        s.object_names.insert(u.clone(), n.clone());
    }
    for (k, v) in &img.link_offsets {
        s.link_offsets.insert(k.clone(), v.clone());
    }
    for o in &img.overlays {
        let original_values = o.original_values.iter().map(|(k, v)| (k.clone(), fp.value(v, s))).collect();
        let extra_fields = o.extra_fields.iter().map(|(k, v)| (k.clone(), fp.value(v, s))).collect();
        s.overlay_instances.push(OverlayInstance {
            overlay_name: o.overlay_name.clone(),
            host_var: o.host_var.clone(),
            host_uuid: o.host_uuid.clone(),
            strength: o.strength,
            age: o.age,
            ticks_remaining: o.ticks_remaining,
            count: o.count,
            original_values,
            extra_fields,
            des_id: OverlayInstanceId(o.des_id),
        });
    }
}
