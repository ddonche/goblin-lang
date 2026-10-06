//! Per-request environment for hosted scripts.
//!
//! goblin-host runs many requests at once, each on its own blocking thread.
//! It used to pass each request's method, path, headers and body through
//! process environment variables, which forced it to run one VM at a time
//! behind a global lock. Instead the host installs the request's variables
//! here for the duration of the request; every lookup a script can make
//! (`env`, the request builtins, the API-mode check, child processes) sees
//! them ahead of the process environment.

use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static OVERLAY: RefCell<Option<HashMap<String, String>>> = RefCell::new(None);
}

/// Look up `name` in this thread's request overlay, then the process environment.
pub fn var(name: &str) -> Option<String> {
    let hit = OVERLAY.with(|o| o.borrow().as_ref().and_then(|m| m.get(name).cloned()));
    hit.or_else(|| std::env::var(name).ok())
}

/// The overlay's variables, for child processes.
pub fn overlay_vars() -> Vec<(String, String)> {
    OVERLAY.with(|o| o.borrow().as_ref().map(|m| m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()).unwrap_or_default())
}

/// Installs `vars` as this thread's overlay until the guard is dropped.
pub fn install(vars: HashMap<String, String>) -> OverlayGuard {
    let prev = OVERLAY.with(|o| o.replace(Some(vars)));
    OverlayGuard { prev }
}

pub struct OverlayGuard {
    prev: Option<HashMap<String, String>>,
}

impl Drop for OverlayGuard {
    fn drop(&mut self) {
        let prev = self.prev.take();
        OVERLAY.with(|o| *o.borrow_mut() = prev);
    }
}
