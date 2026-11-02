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

#[derive(Debug, Clone)]
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
    on_end: Option<String>,
    each_tick: Vec<(String, goblin_ast::Expr)>,
    intensity: f64,
    spreads: Vec<goblin_ast::OverlaySpread>,
    conflicts: Vec<String>,
    class: Option<String>,
    class_dup: OverlayApplyBehavior,
    actions: Vec<goblin_ast::ActionDecl>,
    on_start: Option<String>,
    every: Vec<(u64, String)>,
    family: String,
    generated: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct POverlay {
    overlay_name: String,
    host_var: String,
    host_uuid: String,
    strength: f64,
    age: u32,
    ticks_remaining: Option<u32>,
    count: u64,
    applied: Vec<(String, f64)>,
    extra_fields: Vec<(String, PValue)>,
    des_id: u32,
    /// The overlay's kind, when a transition made it while the program
    /// ran: the session it goes to may not know it.
    kind: Option<std::sync::Arc<POverlayDef>>,
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

/// An overlay definition, ready to cross to another thread.
pub(crate) fn def_to_p(d: &OverlayDef, tp: &mut ToPortable) -> Result<POverlayDef, GoblinError> {
    Ok(POverlayDef {
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
        on_end: d.on_end.clone(),
        each_tick: d.each_tick.clone(),
        intensity: d.intensity,
        spreads: d.spreads.clone(),
        conflicts: d.conflicts.clone(),
        class: d.class.clone(),
        class_dup: d.class_dup.clone(),
        actions: d.actions.clone(),
        on_start: d.on_start.clone(),
        every: d.every.clone(),
        family: d.family.clone(),
        generated: d.generated,
    })
}

/// An overlay definition rebuilt in this session (its blocks compiled again).
pub(crate) fn def_from_p(d: &POverlayDef, fp: &mut FromPortable, s: &mut Session) -> OverlayDef {
    let extra_fields: indexmap::IndexMap<String, crate::value::Value> = d.extra_fields.iter().map(|(k, v)| (k.clone(), fp.value(v, s))).collect();
    let extra_names: Vec<String> = extra_fields.keys().cloned().collect();
    OverlayDef {
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
        on_end: d.on_end.clone(),
        each_tick: d.each_tick.clone(),
        intensity: d.intensity,
        spreads: d.spreads.clone(),
        conflicts: d.conflicts.clone(),
        class: d.class.clone(),
        class_dup: d.class_dup.clone(),
        methods: crate::tick::compile_overlay_methods(&d.name, &d.actions, &extra_names).unwrap_or_default(),
        actions: d.actions.clone(),
        on_start: d.on_start.clone(),
        every: d.every.clone(),
        family: d.family.clone(),
        generated: d.generated,
    }
}

/// The names the handed objects were stored under.
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
        img.overlay_defs.push(def_to_p(&session.overlay_defs[n], tp)?);
    }
    img.link_defs = session.link_defs.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    img.object_link_defs = session.object_link_defs.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    img.object_decisions = session.object_decisions.iter().map(|(k, v)| (k.clone(), (**v).clone())).collect();
    if let Some(objs) = objects {
        for inst in session.overlay_instances.iter().filter(|i| objs.contains(&i.host_uuid)) {
            img.overlays.push(overlay_to_p(inst, tp)?);
        }
        for u in objs {
            if let Some(n) = session.object_names.get(u) {
                img.object_names.push((u.clone(), n.clone()));
            }
        }
        img.object_names.sort();
        if !session.link_offsets.is_empty() {
            img.link_offsets = session.link_offsets.iter()
                .filter(|((from, _, _), _)| objs.contains(from))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            img.link_offsets.sort_by(|a, b| a.0.cmp(&b.0));
        }
    }
    Ok(img)
}

/// Installs the definitions and overlays in a freshly built session.
pub(crate) fn install(img: &DesImage, fp: &mut FromPortable, s: &mut Session) {
    let mut defs: rustc_hash::FxHashMap<String, Rc<crate::session::OverlayDef>> = rustc_hash::FxHashMap::with_capacity_and_hasher(img.overlay_defs.len(), Default::default());
    for d in &img.overlay_defs {
        let def = def_from_p(d, fp, s);
        defs.insert(d.name.clone(), Rc::new(def));
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
        let inst = overlay_from_p(o, fp, s);
        s.overlay_instances.push(inst);
    }
    // The objects' fields read through their overlays' film already.
    let hosts: Vec<String> = s.overlay_instances.iter().map(|i| i.host_uuid.clone()).collect();
    if !hosts.is_empty() {
        let mut hosts = hosts; hosts.sort_unstable(); hosts.dedup();
        crate::tick::adopt_films(s, &hosts, fp);
    }
}

impl POverlay {
    pub(crate) fn host(&self) -> &str { &self.host_uuid }
    pub(crate) fn name(&self) -> &str { &self.overlay_name }
}

/// One overlay, ready to cross to another thread.
pub(crate) fn overlay_to_p(inst: &OverlayInstance, tp: &mut ToPortable) -> Result<POverlay, GoblinError> {
    Ok(POverlay {
        overlay_name: inst.overlay_name.clone(),
        host_var: inst.host_var.clone(),
        host_uuid: inst.host_uuid.clone(),
        strength: inst.strength,
        age: inst.age,
        ticks_remaining: inst.ticks_remaining,
        count: inst.count,
        applied: inst.applied.clone(),
        extra_fields: pfields(tp, &inst.extra_fields)?,
        des_id: inst.des_id.0,
        kind: match tp.session.overlay_defs.get(&inst.overlay_name) {
            Some(d) if d.generated => {
                let cached = tp.session.portable_kinds.borrow().get(&d.name).cloned();
                match cached {
                    Some(p) => Some(p),
                    None => {
                        let d = d.clone();
                        let p = std::sync::Arc::new(def_to_p(&d, tp)?);
                        tp.session.portable_kinds.borrow_mut().insert(d.name.clone(), p.clone());
                        Some(p)
                    }
                }
            }
            _ => None,
        },
    })
}

/// An overlay rebuilt in this session.
pub(crate) fn overlay_from_p(o: &POverlay, fp: &mut FromPortable, s: &mut Session) -> OverlayInstance {
    if let Some(d) = &o.kind {
        if !s.overlay_defs.contains_key(&d.name) {
            let def = def_from_p(d, fp, s);
            s.overlay_defs.insert(d.name.clone(), Rc::new(def));
        }
    }
    OverlayInstance {
        overlay_name: o.overlay_name.clone(),
        host_var: o.host_var.clone(),
        host_uuid: o.host_uuid.clone(),
        strength: o.strength,
        age: o.age,
        ticks_remaining: o.ticks_remaining,
        count: o.count,
        applied: o.applied.clone(),
        extra_fields: o.extra_fields.iter().map(|(k, v)| (k.clone(), fp.value(v, s))).collect(),
        des_id: OverlayInstanceId(o.des_id),
    }
}
