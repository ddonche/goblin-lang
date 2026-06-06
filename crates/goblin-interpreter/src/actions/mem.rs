//! Memory introspection builtins for Goblin.
//!
//! Exposed to Goblin as:
//!   :mem_addr(x)   -> "0x..."
//!   :mem_total()   -> total process memory bytes currently in use

use crate::{Diag, Session, Span, Value};

use indexmap::IndexMap;
use std::collections::BTreeMap;

/// Return a pointer to "where this value's data lives" in memory.
///
/// - For heap-backed types, we try to return the backing buffer / map / fields.
/// - For everything else, we fall back to the address of the `Value` itself.
fn ptr_for_value(v: &Value) -> *const u8 {
    match v {
        Value::Str(s) => s.as_ptr() as *const u8,

        Value::Formatted(inner, _) => {
            let inner_ref: &Value = inner.as_ref();
            inner_ref as *const Value as *const u8
        }

        Value::Array(xs) => xs.as_ptr() as *const u8,

        Value::Seq(seq) => (seq as *const _) as *const u8,

        Value::Map(m) => (m as *const BTreeMap<String, Value>) as *const u8,

        Value::MapOrd(m) => (m as *const IndexMap<String, Value>) as *const u8,

        Value::Pair(a, _) => {
            let a_ref: &Value = a.as_ref();
            a_ref as *const Value as *const u8
        }

        Value::Object { fields, .. } => {
            (fields as *const IndexMap<String, Value>) as *const u8
        }

        Value::Enum { fields: Some(f), .. } => {
            (f as *const IndexMap<String, Value>) as *const u8
        }

        Value::Enum { fields: None, .. } => v as *const Value as *const u8,

        _ => v as *const Value as *const u8,
    }
}

/// :mem_addr(x)
///
/// Returns a hex string representing the memory address associated with `x`.
pub fn mem_addr(
    _sess: &mut Session,
    args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    let v = match args.get(0) {
        Some(v) => v,
        None => return Ok(Value::Str(String::new())),
    };

    let ptr = ptr_for_value(v);
    Ok(Value::Str(format!("{:#p}", ptr)))
}

/// :mem_total()
///
/// Returns total process memory currently in use, in bytes.
///
/// This is intentionally process-level for now:
/// it answers "is this Goblin run growing memory over time?"
pub fn mem_total(
    _sess: &mut Session,
    _args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    Ok(Value::Int(process_memory_bytes() as i64))
}

#[cfg(target_os = "windows")]
fn process_memory_bytes() -> usize {
    use std::ffi::c_void;

    #[repr(C)]
    struct PROCESS_MEMORY_COUNTERS {
        cb: u32,
        page_fault_count: u32,
        peak_working_set_size: usize,
        working_set_size: usize,
        quota_peak_paged_pool_usage: usize,
        quota_paged_pool_usage: usize,
        quota_peak_non_paged_pool_usage: usize,
        quota_non_paged_pool_usage: usize,
        pagefile_usage: usize,
        peak_pagefile_usage: usize,
    }

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetCurrentProcess() -> *mut c_void;
    }

    #[link(name = "psapi")]
    unsafe extern "system" {
        fn GetProcessMemoryInfo(
            process: *mut c_void,
            counters: *mut PROCESS_MEMORY_COUNTERS,
            size: u32,
        ) -> i32;
    }

    unsafe {
        let process = GetCurrentProcess();

        let mut counters = PROCESS_MEMORY_COUNTERS {
            cb: std::mem::size_of::<PROCESS_MEMORY_COUNTERS>() as u32,
            page_fault_count: 0,
            peak_working_set_size: 0,
            working_set_size: 0,
            quota_peak_paged_pool_usage: 0,
            quota_paged_pool_usage: 0,
            quota_peak_non_paged_pool_usage: 0,
            quota_non_paged_pool_usage: 0,
            pagefile_usage: 0,
            peak_pagefile_usage: 0,
        };

        let ok = GetProcessMemoryInfo(
            process,
            &mut counters,
            std::mem::size_of::<PROCESS_MEMORY_COUNTERS>() as u32,
        );

        if ok == 0 {
            0
        } else {
            counters.working_set_size
        }
    }
}

fn human_bytes(bytes: usize) -> String {
    let units = ["B", "KB", "MB", "GB", "TB"];
    let mut size = bytes as f64;
    let mut unit = 0;

    while size >= 1024.0 && unit < units.len() - 1 {
        size /= 1024.0;
        unit += 1;
    }

    if unit == 0 {
        format!("{} {}", bytes, units[unit])
    } else {
        format!("{:.2} {}", size, units[unit])
    }
}

pub fn mem_human(
    _sess: &mut Session,
    _args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    Ok(Value::Str(human_bytes(process_memory_bytes())))
}

#[cfg(target_os = "linux")]
fn process_memory_bytes() -> usize {
    let contents = match std::fs::read_to_string("/proc/self/statm") {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let resident_pages = match contents.split_whitespace().nth(1) {
        Some(s) => match s.parse::<usize>() {
            Ok(n) => n,
            Err(_) => return 0,
        },
        None => return 0,
    };

    resident_pages * page_size()
}

#[cfg(target_os = "linux")]
fn page_size() -> usize {
    unsafe { libc_sysconf_page_size() }
}

#[cfg(target_os = "linux")]
unsafe fn libc_sysconf_page_size() -> usize {
    unsafe extern "C" {
        fn sysconf(name: i32) -> isize;
    }

    const _SC_PAGESIZE: i32 = 30;
    let size = unsafe { sysconf(_SC_PAGESIZE) };

    if size <= 0 {
        4096
    } else {
        size as usize
    }
}

#[cfg(target_os = "macos")]
fn process_memory_bytes() -> usize {
    use std::ffi::c_void;

    #[repr(C)]
    struct MachTaskBasicInfo {
        virtual_size: usize,
        resident_size: usize,
        resident_size_max: usize,
        user_time: [i32; 2],
        system_time: [i32; 2],
        policy: i32,
        suspend_count: i32,
    }

    unsafe extern "C" {
        fn mach_task_self() -> u32;
        fn task_info(
            target_task: u32,
            flavor: i32,
            task_info_out: *mut c_void,
            task_info_out_cnt: *mut u32,
        ) -> i32;
    }

    const MACH_TASK_BASIC_INFO: i32 = 20;

    unsafe {
        let mut info = std::mem::zeroed::<MachTaskBasicInfo>();
        let mut count = (std::mem::size_of::<MachTaskBasicInfo>() / std::mem::size_of::<u32>()) as u32;

        let result = task_info(
            mach_task_self(),
            MACH_TASK_BASIC_INFO,
            &mut info as *mut _ as *mut c_void,
            &mut count,
        );

        if result != 0 {
            0
        } else {
            info.resident_size
        }
    }
}

#[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
fn process_memory_bytes() -> usize {
    0
}