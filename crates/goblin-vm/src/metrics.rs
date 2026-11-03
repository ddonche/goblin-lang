//! `:metrics()`: what a running program costs. The process's memory, how
//! much of it the DES state takes, how many objects and overlays are live
//! and have been made, and where tick time goes, by pass and by layer.
//!
//! A layer is a class (its objects' `every` blocks, decisions and
//! transitions) or an overlay kind (its spread, `every` blocks, transitions
//! and `on end`; a member of an overlay class counts under the class).
//! Times add up from the start of the program. In a world split into
//! regions they are summed over the regions, so they are thread time.

use std::time::Instant;

use crate::value::{CollectionLayout, Value};

/// The tick's passes, in the order they run.
pub const PASSES: [&str; 9] = [
    "links", "spread", "decay", "overlay_every", "overlay_transitions",
    "overlay_end", "every", "decisions", "transitions",
];

pub const LINKS: usize = 0;
pub const SPREAD: usize = 1;
pub const DECAY: usize = 2;
pub const OVERLAY_EVERY: usize = 3;
pub const OVERLAY_TRANSITIONS: usize = 4;
pub const OVERLAY_END: usize = 5;
pub const EVERY: usize = 6;
pub const DECISIONS: usize = 7;
pub const TRANSITIONS: usize = 8;

/// What a session has counted.
#[derive(Debug, Default, Clone)]
pub struct DesMetrics {
    pub objects_made: u64,
    pub overlays_made: u64,
    pub pass_ns: [u64; PASSES.len()],
    /// Few layers, in the order first met.
    pub layer_ns: Vec<(String, u64)>,
}

impl DesMetrics {
    pub fn add_layer(&mut self, layer: &str, ns: u64) {
        match self.layer_ns.iter_mut().find(|(n, _)| n == layer) {
            Some(e) => e.1 += ns,
            None => self.layer_ns.push((layer.to_string(), ns)),
        }
    }

    pub fn add(&mut self, o: &DesMetrics) {
        self.objects_made += o.objects_made;
        self.overlays_made += o.overlays_made;
        for (a, b) in self.pass_ns.iter_mut().zip(o.pass_ns.iter()) { *a += b; }
        for (n, t) in &o.layer_ns { self.add_layer(n, *t); }
    }
}

/// A region's numbers after its tick, sent to the program that runs the world.
#[derive(Debug, Default, Clone)]
pub struct Counts {
    pub metrics: DesMetrics,
    pub objects: u64,
    pub overlays: u64,
    pub des_bytes: u64,
}

/// Times a run of objects by layer. Objects of one layer mostly come in a
/// row, so the clock is read only where the layer changes.
#[derive(Default)]
pub struct LayerClock {
    /// The layer's place in `layer_ns`, and when it started.
    cur: Option<(usize, Instant)>,
}

impl LayerClock {
    pub fn at(&mut self, m: &mut DesMetrics, layer: &str) {
        if let Some((i, _)) = self.cur { if m.layer_ns[i].0 == layer { return; } }
        let now = Instant::now();
        if let Some((i, t)) = self.cur.take() { m.layer_ns[i].1 += (now - t).as_nanos() as u64; }
        let i = match m.layer_ns.iter().position(|(n, _)| n == layer) {
            Some(i) => i,
            None => { m.layer_ns.push((layer.to_string(), 0)); m.layer_ns.len() - 1 }
        };
        self.cur = Some((i, now));
    }

    pub fn stop(&mut self, m: &mut DesMetrics) {
        if let Some((i, t)) = self.cur.take() { m.layer_ns[i].1 += t.elapsed().as_nanos() as u64; }
    }
}

/// Roughly how many bytes a value takes, its own slot and what it points to.
/// Shared parts are counted for each holder.
pub fn value_bytes(v: &Value) -> usize {
    std::mem::size_of::<Value>() + match v {
        Value::Str(s) => s.len(),
        Value::Object(o) => std::mem::size_of::<crate::value::ObjectVal>()
            + o.uuid.len()
            + o.fields.iter().map(|(k, f)| k.len() + 16 + value_bytes(f)).sum::<usize>(),
        Value::Array(a) => a.iter().map(value_bytes).sum(),
        Value::Map(m) => m.iter().map(|(k, x)| k.len() + 32 + value_bytes(x)).sum(),
        Value::MapOrd(m) => m.iter().map(|(k, x)| k.len() + 24 + value_bytes(x)).sum(),
        Value::Pair(a, b) => value_bytes(a) + value_bytes(b),
        Value::Ref(u) => u.len(),
        Value::Seq(q) => q.items.iter().map(value_bytes).sum(),
        Value::Frozen(f) => value_bytes(&f.value),
        Value::Formatted(v, _) => value_bytes(v),
        Value::Collection(c) => match &c.layout {
            CollectionLayout::FlatArray(xs) => xs.iter().map(value_bytes).sum(),
            CollectionLayout::SmallMap(xs) => xs.iter().map(|(k, x)| value_bytes(k) + value_bytes(x)).sum(),
            CollectionLayout::HashMapBackend(m) => m.iter().map(|(k, x)| 16 + value_bytes(k) + value_bytes(x)).sum(),
            _ => crate::collections::to_vec(c).iter().map(value_bytes).sum(),
        },
        _ => 0,
    }
}

/// Peak resident memory of the process, in bytes (0 where unknown).
#[cfg(target_os = "linux")]
pub fn peak_rss_bytes() -> usize {
    let s = std::fs::read_to_string("/proc/self/status").unwrap_or_default();
    s.lines().find_map(|l| l.strip_prefix("VmHWM:"))
        .and_then(|r| r.split_whitespace().next()?.parse::<usize>().ok())
        .map(|kb| kb * 1024).unwrap_or(0)
}

#[cfg(target_os = "macos")]
pub fn peak_rss_bytes() -> usize {
    use std::ffi::c_void;
    #[repr(C)] struct Info { virtual_size: usize, resident_size: usize, resident_size_max: usize, user_time: [i32;2], system_time: [i32;2], policy: i32, suspend_count: i32 }
    unsafe extern "C" { fn mach_task_self() -> u32; fn task_info(t: u32, f: i32, out: *mut c_void, cnt: *mut u32) -> i32; }
    unsafe {
        let mut info = std::mem::zeroed::<Info>();
        let mut count = (std::mem::size_of::<Info>() / 4) as u32;
        if task_info(mach_task_self(), 20, &mut info as *mut _ as *mut c_void, &mut count) != 0 { 0 } else { info.resident_size_max }
    }
}

#[cfg(target_os = "windows")]
pub fn peak_rss_bytes() -> usize {
    #[repr(C)]
    struct PROCESS_MEMORY_COUNTERS { cb: u32, page_fault_count: u32, peak_working_set_size: usize, working_set_size: usize, quota_peak_paged_pool_usage: usize, quota_paged_pool_usage: usize, quota_peak_non_paged_pool_usage: usize, quota_non_paged_pool_usage: usize, pagefile_usage: usize, peak_pagefile_usage: usize }
    extern "system" { fn GetCurrentProcess() -> *mut std::ffi::c_void; fn K32GetProcessMemoryInfo(h: *mut std::ffi::c_void, p: *mut PROCESS_MEMORY_COUNTERS, cb: u32) -> i32; }
    unsafe {
        let mut pmc: PROCESS_MEMORY_COUNTERS = std::mem::zeroed();
        pmc.cb = std::mem::size_of::<PROCESS_MEMORY_COUNTERS>() as u32;
        if K32GetProcessMemoryInfo(GetCurrentProcess(), &mut pmc, pmc.cb) != 0 { pmc.peak_working_set_size } else { 0 }
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
pub fn peak_rss_bytes() -> usize { 0 }

/// Whether `uuid` is a copy of another region's object (in view during a
/// region's tick, or in the program that runs the world).
fn is_copy(s: &crate::session::Session, uuid: &str) -> bool {
    s.region.as_ref().is_some_and(|c| c.remote.contains(uuid)) || crate::world::in_view(s, uuid)
}

/// This session's own live objects and overlays, and the bytes they take.
/// `objects`: count the objects' bytes too (a region leaves them out: the
/// program that runs the world has copies of them and counts those).
pub fn own_counts(s: &crate::session::Session, object_bytes: bool) -> Counts {
    let mut c = Counts { metrics: s.des_metrics.clone(), ..Default::default() };
    for (u, v) in s.object_store.iter() {
        let Value::Object(o) = v else { continue };
        if o.uuid.starts_with(crate::vm::OVERLAY_RECORD) || is_copy(s, u) { continue; }
        c.objects += 1;
        if object_bytes { c.des_bytes += (u.len() + value_bytes(v)) as u64; }
    }
    for i in s.overlay_instances.iter() {
        if is_copy(s, &i.host_uuid) { continue; }
        c.overlays += 1;
        c.des_bytes += (std::mem::size_of::<crate::session::OverlayInstance>()
            + i.overlay_name.len() + i.host_var.len() + i.host_uuid.len()
            + i.applied.iter().map(|(k, _)| k.len() + 32).sum::<usize>()
            + i.extra_fields.iter().map(|(k, v)| k.len() + 16 + value_bytes(v)).sum::<usize>()) as u64;
    }
    for ((a, b, n), offs) in s.link_offsets.iter() {
        c.des_bytes += (a.len() + b.len() + n.len() + 96
            + offs.len() * std::mem::size_of::<crate::session::LinkOffset>()) as u64;
    }
    c
}

/// `:metrics()`: this program's numbers, and in a world split into regions
/// the regions' numbers as of their last tick.
pub fn report(s: &crate::session::Session) -> Value {
    let mut c = own_counts(s, true);
    for (u, v) in s.object_store.iter() {
        if crate::world::in_view(s, u) { c.des_bytes += (u.len() + value_bytes(v)) as u64; }
    }
    if let Some((regions, moving_objects, moving_overlays)) = crate::world::region_counts(s) {
        for r in regions {
            c.metrics.add(&r.metrics);
            c.objects += r.objects;
            c.overlays += r.overlays;
            c.des_bytes += r.des_bytes;
        }
        c.objects += moving_objects;
        c.overlays += moving_overlays;
    }
    let mb = |b: usize| Value::Float((b as f64 / 1048576.0 * 1000.0).round() / 1000.0);
    let ms = |ns: u64| Value::Float((ns as f64 / 1e6 * 1000.0).round() / 1000.0);
    let int = |n: u64| Value::Int(n as i64);
    let mut m = indexmap::IndexMap::new();
    m.insert("rss_mb".to_string(), mb(crate::builtins::process_memory_bytes()));
    m.insert("peak_rss_mb".to_string(), mb(peak_rss_bytes()));
    m.insert("des_mb".to_string(), mb(c.des_bytes as usize));
    m.insert("ticks".to_string(), Value::Int(s.world.as_ref().map(|_| crate::world::world_tick(s)).unwrap_or(s.des_ticks)));
    m.insert("objects".to_string(), int(c.objects));
    m.insert("objects_made".to_string(), int(c.metrics.objects_made));
    m.insert("objects_gone".to_string(), int(c.metrics.objects_made.saturating_sub(c.objects)));
    m.insert("overlays".to_string(), int(c.overlays));
    m.insert("overlays_made".to_string(), int(c.metrics.overlays_made));
    m.insert("overlays_gone".to_string(), int(c.metrics.overlays_made.saturating_sub(c.overlays)));
    let mut passes = indexmap::IndexMap::new();
    for (name, ns) in PASSES.iter().zip(c.metrics.pass_ns.iter()) { passes.insert(name.to_string(), ms(*ns)); }
    m.insert("tick_ms".to_string(), Value::MapOrd(Box::new(passes)));
    let mut layers = indexmap::IndexMap::new();
    for (name, ns) in &c.metrics.layer_ns { layers.insert(name.clone(), ms(*ns)); }
    m.insert("layer_ms".to_string(), Value::MapOrd(Box::new(layers)));
    Value::MapOrd(Box::new(m))
}
