//! Persistent actors and bounded channels (concurrency spec, stage 3).
//!
//! `:actor(object, handler, policy)` hands an object to a new actor and
//! returns its handle. The actor owns the object from then on: the caller's
//! variable is dead, and creating the actor fails, changing nothing, while
//! anything else still refers to the object (spec IV.4). The actor takes the
//! messages posted to it one at a time and runs `handler(state, message)`;
//! whatever the handler returns is the actor's next state.
//!
//! `:post(handle, message)` copies the message into the actor's bounded
//! mailbox. `:request(handle, message)` does the same and returns a future;
//! `:await(future)` gives a copy of the actor's state right after it handled
//! that message, or raises the handler's error. `:await(handle)` waits for the
//! actor to end and gives its final state. `:halt(handle)` stops new messages,
//! lets the actor finish the ones it already accepted, then ends it.
//! `:cancel(handle)` ends it after the current message and fails the rest.
//!
//! `:channel(capacity)` is a bounded queue: `:post(channel, value)` waits
//! while it is full, `:receive(channel)` waits while it is empty, and
//! `:halt(channel)` closes it, after which receivers drain what is left and
//! then get `nil`.
//!
//! This is the interim implementation the spec allows: every actor has its
//! own OS thread, and the number of live actors is capped
//! (`GOBLIN_MAX_ACTORS`, 256 by default). Lightweight actors that share
//! threads need a VM that can pause an action, which is stage 4.

use std::collections::{HashSet, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::session::Session;
use crate::value::Value;
use crate::vm::{Operand, Vm};

static NEXT_ID: AtomicU64 = AtomicU64::new(1);
static LIVE_ACTORS: AtomicUsize = AtomicUsize::new(0);

const ACTOR_STACK: usize = 64 * 1024 * 1024;
const DEFAULT_MAILBOX: usize = 1000;

fn next_id() -> u64 { NEXT_ID.fetch_add(1, Ordering::Relaxed) }

fn max_actors() -> usize {
    std::env::var("GOBLIN_MAX_ACTORS").ok()
        .and_then(|s| s.trim().parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(256)
}

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

thread_local! {
    /// The actor this thread runs, if any.
    static CURRENT_ACTOR: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

// ── Bounded queue ────────────────────────────────────────────────────────────

pub struct Queue<T> {
    state: Mutex<QState<T>>,
    not_empty: Condvar,
    not_full: Condvar,
    cap: usize,
}

struct QState<T> {
    items: VecDeque<T>,
    closed: bool,
}

pub enum Push<T> { Done, Full(T), Closed(T) }
pub enum Pop<T> { Item(T), Closed, Idle }

impl<T> Queue<T> {
    pub fn new(cap: usize) -> Self {
        Queue {
            state: Mutex::new(QState { items: VecDeque::new(), closed: false }),
            not_empty: Condvar::new(),
            not_full: Condvar::new(),
            cap,
        }
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, QState<T>> {
        self.state.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Add without waiting: a full or closed queue says so.
    pub fn try_push(&self, v: T) -> Push<T> {
        let mut st = self.lock();
        if st.closed { return Push::Closed(v); }
        if st.items.len() >= self.cap { return Push::Full(v); }
        st.items.push_back(v);
        self.not_empty.notify_one();
        Push::Done
    }

    /// Add, waiting while the queue is full.
    pub fn push_wait(&self, v: T) -> Push<T> {
        let mut st = self.lock();
        loop {
            if st.closed { return Push::Closed(v); }
            if st.items.len() < self.cap {
                st.items.push_back(v);
                self.not_empty.notify_one();
                return Push::Done;
            }
            st = self.not_full.wait(st).unwrap_or_else(|e| e.into_inner());
        }
    }

    /// Take the next item, waiting while the queue is empty. With `idle`, a
    /// queue that stays empty that long is closed (nothing can be accepted
    /// and then lost: the close happens under the same lock as the check).
    pub fn pop(&self, idle: Option<Duration>) -> Pop<T> {
        self.pop_until(idle, true)
    }

    /// Take the next item, waiting at most `limit` (forever with None). An
    /// empty queue stays open when the time runs out.
    pub fn pop_within(&self, limit: Option<Duration>) -> Pop<T> {
        self.pop_until(limit, false)
    }

    fn pop_until(&self, wait: Option<Duration>, close_when_idle: bool) -> Pop<T> {
        let deadline = wait.map(|d| Instant::now() + d);
        let mut st = self.lock();
        loop {
            if let Some(v) = st.items.pop_front() {
                self.not_full.notify_one();
                return Pop::Item(v);
            }
            if st.closed { return Pop::Closed; }
            match deadline {
                None => { st = self.not_empty.wait(st).unwrap_or_else(|e| e.into_inner()); }
                Some(d) => {
                    let now = Instant::now();
                    if now >= d {
                        if close_when_idle {
                            st.closed = true;
                            self.not_full.notify_all();
                        }
                        return Pop::Idle;
                    }
                    st = self.not_empty.wait_timeout(st, d - now).unwrap_or_else(|e| e.into_inner()).0;
                }
            }
        }
    }

    /// Accept nothing more. Items already queued stay until taken.
    pub fn close(&self) {
        let mut st = self.lock();
        st.closed = true;
        self.not_empty.notify_all();
        self.not_full.notify_all();
    }

    /// Take everything still queued.
    pub fn drain(&self) -> Vec<T> {
        let mut st = self.lock();
        self.not_full.notify_all();
        st.items.drain(..).collect()
    }
}

// ── Handles ──────────────────────────────────────────────────────────────────

/// What a variable holds for an actor, a channel or a reply to come.
#[derive(Clone)]
pub enum Handle {
    Actor(Arc<ActorShared>),
    Channel(Arc<Channel>),
    Future(Arc<Reply>),
}

impl Handle {
    pub fn kind(&self) -> &'static str {
        match self { Handle::Actor(_) => "actor", Handle::Channel(_) => "channel", Handle::Future(_) => "future" }
    }
    pub fn id(&self) -> u64 {
        match self { Handle::Actor(a) => a.id, Handle::Channel(c) => c.id, Handle::Future(f) => f.id }
    }
    pub fn same(&self, other: &Handle) -> bool {
        match (self, other) {
            (Handle::Actor(a), Handle::Actor(b)) => Arc::ptr_eq(a, b),
            (Handle::Channel(a), Handle::Channel(b)) => Arc::ptr_eq(a, b),
            (Handle::Future(a), Handle::Future(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
    /// How `say` shows it: `Actor(#3)`.
    pub fn display(&self) -> String {
        let name = match self { Handle::Actor(_) => "Actor", Handle::Channel(_) => "Channel", Handle::Future(_) => "Future" };
        format!("{name}(#{})", self.id())
    }
}

impl std::fmt::Debug for Handle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.display()) }
}

pub struct Channel {
    id: u64,
    queue: Queue<PValue>,
}

/// The answer to one `:request`.
pub struct Reply {
    id: u64,
    slot: Mutex<Option<Result<Arc<PValue>, String>>>,
    ready: Condvar,
    /// The actor that will answer, so it cannot wait for itself.
    actor: u64,
}

impl Reply {
    fn set(&self, r: Result<Arc<PValue>, String>) {
        let mut s = self.slot.lock().unwrap_or_else(|e| e.into_inner());
        if s.is_none() { *s = Some(r); }
        self.ready.notify_all();
    }
    fn wait(&self) -> Result<Arc<PValue>, String> {
        let mut s = self.slot.lock().unwrap_or_else(|e| e.into_inner());
        loop {
            if let Some(r) = s.as_ref() { return r.clone(); }
            s = self.ready.wait(s).unwrap_or_else(|e| e.into_inner());
        }
    }
}

enum Msg {
    Post(PValue),
    Request(PValue, Arc<Reply>),
}

#[derive(Clone)]
enum End {
    Done(Arc<PValue>),
    Failed(String),
    Cancelled,
}

pub struct ActorShared {
    id: u64,
    mailbox: Queue<Msg>,
    cancelled: AtomicBool,
    /// Set before the mailbox closes when the handler fails.
    failure: Mutex<Option<String>>,
    end: Mutex<Option<End>>,
    ended: Condvar,
    /// The object handed over, if the state is an object (its uuid).
    origin: Option<String>,
}

impl ActorShared {
    fn finish(&self, e: End) {
        let mut end = self.end.lock().unwrap_or_else(|x| x.into_inner());
        *end = Some(e);
        self.ended.notify_all();
    }
    fn wait_end(&self) -> End {
        let mut end = self.end.lock().unwrap_or_else(|x| x.into_inner());
        loop {
            if let Some(e) = end.as_ref() { return e.clone(); }
            end = self.ended.wait(end).unwrap_or_else(|x| x.into_inner());
        }
    }
    fn stopped_error(&self) -> GoblinError {
        if let Some(f) = self.failure.lock().unwrap_or_else(|x| x.into_inner()).as_ref() {
            return err(format!("C0112: actor-failed: the actor stopped because its handler failed: {f}"));
        }
        if self.cancelled.load(Ordering::SeqCst) {
            return err("C0116: actor-cancelled: the actor was cancelled and takes no more messages");
        }
        err("C0111: actor-stopped: the actor has stopped and takes no more messages")
    }
    /// Stop taking messages; the actor finishes the ones it accepted.
    pub fn halt(&self) { self.mailbox.close(); }
}

/// Ends every actor a session started: each finishes the messages it already
/// accepted, then ends (structured ownership, spec VI). Called when the
/// session is dropped, so a program's actors never outlive it silently.
pub fn finish_owned(actors: &mut Vec<Arc<ActorShared>>) {
    for a in actors.iter() { a.halt(); }
    for a in actors.drain(..) { let _ = a.wait_end(); }
}

// ── :actor ───────────────────────────────────────────────────────────────────

struct Policy {
    mailbox: usize,
    idle: Option<Duration>,
    stop_when: Option<Value>,
}

fn policy_of(v: Option<Value>) -> Result<Policy, GoblinError> {
    let mut p = Policy { mailbox: DEFAULT_MAILBOX, idle: None, stop_when: None };
    let Some(v) = v else { return Ok(p) };
    if matches!(v, Value::Nil) { return Ok(p); }
    let Some(entries) = v.map_entries() else {
        return Err(err(format!("C0108: actor-policy: the policy must be a map, got {}", v.type_name())));
    };
    for (k, val) in entries {
        match k.as_str() {
            "mailbox" => match val {
                Value::Int(n) if n >= 1 => p.mailbox = n as usize,
                other => return Err(err(format!(
                    "C0108: actor-policy: mailbox must be a positive whole number, got {}", crate::builtins::value_to_str(&other)))),
            },
            "idle" => match val {
                Value::Duration(d) if d.ns > 0 => p.idle = Some(Duration::from_nanos(d.ns.min(u64::MAX as i128) as u64)),
                Value::Nil => {}
                other => return Err(err(format!(
                    "C0108: actor-policy: idle must be a duration longer than 0s, like 10m, got {}", crate::builtins::value_to_str(&other)))),
            },
            "stop_when" => match val {
                Value::Function(_) | Value::Closure(_) | Value::Str(_) => p.stop_when = Some(val),
                Value::Nil => {}
                other => return Err(err(format!(
                    "C0108: actor-policy: stop_when must be an action, got {}", other.type_name()))),
            },
            "restart" if matches!(&val, Value::Str(s) if s.as_str() == "never") => {}
            "restart" | "max_restarts" => return Err(err(
                "C0109: actor-restart-unsupported: restart policies are not built yet; an actor whose handler fails stops, and its error is reported (leave restart out, or use restart: \"never\")")),
            other => return Err(err(format!(
                "C0108: actor-policy: unknown policy key `{other}` (known: mailbox, idle, stop_when, restart)"))),
        }
    }
    Ok(p)
}

/// Every object reachable from `v` through fields and collections.
fn graph_of(v: &Value, session: &Session, out: &mut HashSet<String>) {
    let mut stack: Vec<Value> = vec![v.clone()];
    while let Some(v) = stack.pop() {
        match &v {
            Value::Ref(u) => {
                if out.insert(u.clone()) {
                    if let Some(o) = session.object_store.get(u.as_str()) { stack.push(o.clone()); }
                }
            }
            Value::Object(o) => {
                // The stored object is the current one; an inline copy may be older.
                let first = out.insert(o.uuid.clone());
                if let Some(Value::Object(stored)) = session.object_store.get(o.uuid.as_str()) {
                    if first { stack.extend(stored.fields.values().cloned()); }
                }
                stack.extend(o.fields.values().cloned());
            }
            _ => children(&v, &mut |c| stack.push(c.clone())),
        }
    }
}

/// The values directly inside a container (not through objects).
fn children(v: &Value, f: &mut dyn FnMut(&Value)) {
    use crate::value::CollectionLayout as L;
    match v {
        Value::Array(a) => a.iter().for_each(|x| f(x)),
        Value::Seq(s) => s.items.iter().for_each(|x| f(x)),
        Value::Map(m) => m.values().for_each(|x| f(x)),
        Value::MapOrd(m) => m.values().for_each(|x| f(x)),
        Value::Pair(a, b) => { f(a); f(b) }
        Value::Formatted(x, _) | Value::CtrlReturn(x) => f(x),
        Value::Enum(e) => if let Some(fs) = &e.fields { fs.values().for_each(|x| f(x)) },
        Value::Collection(c) => match &c.layout {
            L::FlatArray(v) => v.iter().for_each(|x| f(x)),
            L::RingBuf(r) => r.buf.iter().for_each(|x| f(x)),
            L::ChunkedSeq(s) => s.chunks.iter().flatten().for_each(|x| f(x)),
            L::SmallMap(p) => p.iter().for_each(|(k, x)| { f(k); f(x) }),
            L::HashMapBackend(m) => m.iter().for_each(|(k, x)| { f(k); f(x) }),
        },
        _ => {}
    }
}

/// How many references to objects in `graph` `v` holds (a frozen snapshot is
/// a separate copy, so it holds none).
fn count_refs(v: &Value, graph: &HashSet<String>, n: &mut usize) {
    match v {
        Value::Ref(u) => if graph.contains(u.as_str()) { *n += 1 },
        Value::Object(o) => {
            if graph.contains(o.uuid.as_str()) { *n += 1; return; }
            for x in o.fields.values() { count_refs(x, graph, n) }
        }
        Value::Frozen(_) => {}
        _ => children(v, &mut |c| count_refs(c, graph, n)),
    }
}

impl Vm {
    /// `:actor(object, handler, policy)`. `from_var` says the first argument
    /// came straight from a variable, which is the one reference allowed.
    pub(crate) fn actor_create(&mut self, args: Vec<Value>, from_var: bool) -> Result<Value, GoblinError> {
        if !(2..=3).contains(&args.len()) {
            return Err(err(format!(
                "C0107: actor-args: :actor expects (object, handler) or (object, handler, policy), got {} arguments", args.len())));
        }
        let mut it = args.into_iter();
        let state = it.next().unwrap();
        let handler = it.next().unwrap();
        let policy = policy_of(it.next())?;
        if !matches!(handler, Value::Function(_) | Value::Closure(_) | Value::Str(_)) {
            return Err(err(format!("C0107: actor-args: the handler must be an action, got {}", handler.type_name())));
        }
        if matches!(state, Value::Nil) {
            return Err(err("C0107: actor-args: an actor needs a state to own, got nil"));
        }
        if let Value::Handle(Handle::Future(_)) = state {
            return Err(err("C0107: actor-args: a future cannot be an actor's state"));
        }

        // Objects are shared by reference, so nothing else may still refer to
        // the ones being handed over (spec IV.4). Check before changing anything.
        let mut graph = HashSet::new();
        graph_of(&state, &self.session, &mut graph);
        if !graph.is_empty() {
            let live = self.collect_live_slots();
            let mut n = 0usize;
            for (slot, stash) in self.session.arena.iter() {
                if live.get(slot).copied().unwrap_or(false) { count_refs(&stash.value, &graph, &mut n); }
            }
            for (uuid, obj) in &self.session.object_store {
                if graph.contains(uuid.as_str()) { continue; }
                if let Value::Object(o) = obj { for x in o.fields.values() { count_refs(x, &graph, &mut n); } }
            }
            for m in self.session.token_store.values() { for x in m.values() { count_refs(x, &graph, &mut n); } }
            for x in self.session.box_store.values() { count_refs(x, &graph, &mut n); }
            self.session.grid_store.for_each_value(&mut |x| count_refs(x, &graph, &mut n));
            // `named_values` registers a declared object under its own
            // variable's name; that entry goes with the object.
            for op in &self.stack { if let Operand::Val(x) = op { count_refs(x, &graph, &mut n); } }
            let allowed = if from_var { 1 } else { 0 };
            if n > allowed {
                return Err(err(
                    "C0113: actor-shared-state: this object is also held somewhere else (another variable, a collection or another object), so it cannot be handed to an actor; drop the other reference first, or hand over a copy made with :freeze"));
            }
        }

        if LIVE_ACTORS.fetch_add(1, Ordering::SeqCst) >= max_actors() {
            LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst);
            return Err(err(format!(
                "C0110: actor-limit: {} actors are already running, the most this interim version allows (set GOBLIN_MAX_ACTORS to change it)", max_actors())));
        }

        let converted = (|| {
            let mut tp = ToPortable::new(&self.session);
            let image = crate::swarm::image_of(&self.session, &mut tp)?;
            let p_state = tp.value(&state)?;
            let p_handler = tp.value(&handler)?;
            let p_stop = match &policy.stop_when { Some(f) => Some(tp.value(f)?), None => None };
            Ok::<_, GoblinError>((image, p_state, p_handler, p_stop))
        })();
        let (image, p_state, p_handler, p_stop) = match converted {
            Ok(x) => x,
            Err(e) => { LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst); return Err(e); }
        };

        let origin = match &state { Value::Object(o) => Some(o.uuid.clone()), Value::Ref(u) => Some(u.clone()), _ => None };
        let shared = Arc::new(ActorShared {
            id: next_id(),
            mailbox: Queue::new(policy.mailbox),
            cancelled: AtomicBool::new(false),
            failure: Mutex::new(None),
            end: Mutex::new(None),
            ended: Condvar::new(),
            origin,
        });
        let seed = self.session.next_u128();
        let worker_id = self.session.worker_id;
        let idle = policy.idle;
        let sh = shared.clone();
        let spawned = std::thread::Builder::new()
            .name(format!("goblin-actor-{}", shared.id))
            .stack_size(ACTOR_STACK)
            .spawn(move || {
                run_actor(&sh, image, p_state, p_handler, p_stop, idle, seed, worker_id);
                LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst);
            });
        if let Err(e) = spawned {
            LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst);
            return Err(err(format!("C0110: actor-limit: could not start the actor's thread: {e}")));
        }

        // Handed over: the objects now belong to the actor.
        self.session.named_values.retain(|_, v| {
            let mut k = 0; count_refs(v, &graph, &mut k); k == 0
        });
        for u in &graph {
            self.session.object_store.remove(u.as_str());
            self.session.handed_off.entry(u.clone()).or_insert_with(|| "this object".to_string());
        }
        // Actors that already ended need no shutdown; forget them, so a
        // session that starts many short-lived actors does not keep them all.
        self.session.owned_actors.retain(|a| a.end.lock().unwrap_or_else(|x| x.into_inner()).is_none());
        self.session.owned_actors.push(shared.clone());
        Ok(Value::Handle(Handle::Actor(shared)))
    }
}

/// After `room_worker | :actor(room, ...)`: what `room` holds from now on.
/// Reading it is an error that says where the value went.
pub fn hand_off_marker(handle: &Value, var: &str, session: &mut Session) -> Value {
    let uuid = match handle {
        Value::Handle(Handle::Actor(a)) => a.origin.clone().unwrap_or_else(|| format!("handed-off:{}", a.id)),
        _ => format!("handed-off:{var}"),
    };
    session.handed_off.insert(uuid.clone(), var.to_string());
    Value::Ref(uuid)
}

/// The error for reading an object that is gone.
pub fn missing_object(session: &Session, uuid: &str) -> GoblinError {
    match session.handed_off.get(uuid) {
        Some(var) if var == "this object" => err(
            "C0115: actor-handed-off: this object was handed to an actor and can no longer be used here"),
        Some(var) => err(format!(
            "C0115: actor-handed-off: `{var}` was handed to an actor and can no longer be used here; send the actor a message instead")),
        None => err(format!("dangling ref: uuid {uuid} not in object_store")),
    }
}

/// Objects inside the actor's session live in its object store, as they do
/// after `StoreLocal`, so field writes find them.
fn adopt(v: Value, s: &mut Session) -> Value {
    match v {
        Value::Object(mut o) => {
            let fields: indexmap::IndexMap<String, Value> = o.fields.iter()
                .map(|(k, x)| (k.clone(), adopt(x.clone(), s))).collect();
            o.fields = std::rc::Rc::new(fields);
            let uuid = o.uuid.clone();
            s.object_store.insert(uuid.clone(), Value::Object(o));
            Value::Ref(uuid)
        }
        other => other,
    }
}

fn deref(v: Value, s: &Session) -> Value {
    match v {
        Value::Ref(u) => s.object_store.get(u.as_str()).cloned().unwrap_or(Value::Ref(u)),
        other => other,
    }
}

#[allow(clippy::too_many_arguments)]
fn run_actor(
    shared: &Arc<ActorShared>,
    image: crate::swarm::Image,
    p_state: PValue,
    p_handler: PValue,
    p_stop: Option<PValue>,
    idle: Option<Duration>,
    seed: u128,
    worker_id: usize,
) {
    CURRENT_ACTOR.with(|c| c.set(shared.id));
    let mut fp = FromPortable::new();
    let mut vm = Vm::new(crate::swarm::build_session(&image, &mut fp, worker_id));
    drop(image);
    vm.session.rng_state = seed | 1;
    let handler = fp.value(&p_handler, &mut vm.session);
    let stop_when = p_stop.as_ref().map(|p| fp.value(p, &mut vm.session));
    let first = fp.value(&p_state, &mut vm.session);
    let mut state = adopt(first, &mut vm.session);
    drop(fp);

    let fail = |shared: &ActorShared, msg: String, reply: Option<Arc<Reply>>| {
        *shared.failure.lock().unwrap_or_else(|x| x.into_inner()) = Some(msg.clone());
        shared.mailbox.close();
        if let Some(r) = reply { r.set(Err(msg.clone())); }
        let rest = format!("C0112: actor-failed: the actor stopped because its handler failed: {msg}");
        for m in shared.mailbox.drain() {
            if let Msg::Request(_, r) = m { r.set(Err(rest.clone())); }
        }
    };
    // After a failure: the actors this one started finish first, then it ends,
    // so whoever waits for it never returns while its children still run.
    let fail_end = |shared: &ActorShared, vm: &mut Vm| {
        vm.session.finish_actors();
        let msg = shared.failure.lock().unwrap_or_else(|x| x.into_inner()).clone().unwrap_or_default();
        shared.finish(End::Failed(msg));
    };

    loop {
        if shared.cancelled.load(Ordering::SeqCst) { break; }
        let msg = match shared.mailbox.pop(idle) {
            Pop::Item(m) => m,
            Pop::Closed | Pop::Idle => break,
        };
        // A :cancel that landed while this message was being taken: it is
        // not handled, like the rest of the queue.
        if shared.cancelled.load(Ordering::SeqCst) {
            if let Msg::Request(_, r) = msg {
                r.set(Err("C0116: actor-cancelled: the actor was cancelled before it handled this request".into()));
            }
            break;
        }
        let (p_msg, reply) = match msg { Msg::Post(p) => (p, None), Msg::Request(p, r) => (p, Some(r)) };
        // Converted afresh per message, so nothing from old messages is kept.
        let message = {
            let m = snapshot(&p_msg, &mut vm.session);
            adopt(m, &mut vm.session)
        };
        drop(p_msg);
        let call = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.call_callable(handler.clone(), vec![state.clone(), message])
        }));
        let next = match call {
            Ok(Ok(v)) => v,
            Ok(Err(e)) => { fail(shared, e.to_string(), reply); fail_end(shared, &mut vm); return; }
            Err(_) => { fail(shared, "the handler crashed (internal error)".into(), reply); fail_end(shared, &mut vm); return; }
        };
        if matches!(next, Value::Nil) {
            fail(shared, "the handler returned nil; it must return the actor's next state".into(), reply);
            fail_end(shared, &mut vm);
            return;
        }
        state = adopt(next, &mut vm.session);
        if let Some(r) = reply {
            let current = deref(state.clone(), &vm.session);
            r.set(ToPortable::new(&vm.session).value(&current).map(Arc::new).map_err(|e| e.to_string()));
        }
        if let Some(sw) = &stop_when {
            let current = deref(state.clone(), &vm.session);
            match vm.call_callable(sw.clone(), vec![current]) {
                Ok(v) => if v.is_truthy() { shared.mailbox.close(); },
                Err(e) => { fail(shared, format!("stop_when failed: {e}"), None); fail_end(shared, &mut vm); return; }
            }
        }
    }

    if shared.cancelled.load(Ordering::SeqCst) {
        shared.mailbox.close();
        for m in shared.mailbox.drain() {
            if let Msg::Request(_, r) = m {
                r.set(Err("C0116: actor-cancelled: the actor was cancelled before it handled this request".into()));
            }
        }
        vm.session.finish_actors();
        shared.finish(End::Cancelled);
        return;
    }
    let current = deref(state, &vm.session);
    let end = match ToPortable::new(&vm.session).value(&current) {
        Ok(p) => End::Done(Arc::new(p)),
        Err(e) => End::Failed(format!("its final state cannot be copied out: {e}")),
    };
    vm.session.finish_actors();
    shared.finish(end);
}

// ── Messages ─────────────────────────────────────────────────────────────────

/// A copy of an actor's state, as the asker receives it. Objects in it get
/// new ids: they are independent copies, so they must never be mistaken for
/// the originals (the ones handed over, say). References inside the copy
/// keep pointing at each other.
fn snapshot(p: &PValue, session: &mut Session) -> Value {
    fn walk(p: &mut PValue, ids: &mut std::collections::HashMap<String, String>) {
        use crate::portable::PLayout as L;
        match p {
            PValue::Object { fields, uuid, .. } => {
                let new = ids.entry(uuid.clone()).or_insert_with(|| uuid::Uuid::new_v4().to_string()).clone();
                *uuid = new.clone();
                for (k, v) in fields.iter_mut() {
                    if k == "uuid" { *v = PValue::Str(new.clone()); } else { walk(v, ids); }
                }
            }
            PValue::Formatted(v, _) | PValue::CtrlReturn(v) => walk(v, ids),
            PValue::Array(a) | PValue::Seq(a) => a.iter_mut().for_each(|v| walk(v, ids)),
            PValue::Map(m) => m.values_mut().for_each(|v| walk(v, ids)),
            PValue::MapOrd(m) => m.iter_mut().for_each(|(_, v)| walk(v, ids)),
            PValue::Pair(a, b) => { walk(a, ids); walk(b, ids) }
            PValue::Enum { fields: Some(f), .. } => f.iter_mut().for_each(|(_, v)| walk(v, ids)),
            PValue::Collection(layout, _) => match layout {
                L::FlatArray(v) | L::RingBuf { buf: v, .. } => v.iter_mut().for_each(|x| walk(x, ids)),
                L::ChunkedSeq { chunks, .. } => chunks.iter_mut().flatten().for_each(|x| walk(x, ids)),
                L::SmallMap(kv) | L::HashMapBackend(kv) => kv.iter_mut().for_each(|(k, x)| { walk(k, ids); walk(x, ids) }),
            },
            _ => {}
        }
    }
    let mut copy = p.clone();
    walk(&mut copy, &mut std::collections::HashMap::new());
    FromPortable::new().value(&copy, session)
}

fn handle_arg<'a>(v: &'a Value, what: &str) -> Result<&'a Handle, GoblinError> {
    match v {
        Value::Handle(h) => Ok(h),
        other => Err(err(format!("C0118: not-a-handle: {what} expects an actor or a channel, got {}", other.type_name()))),
    }
}

fn expect_args(args: &[Value], n: usize, name: &str) -> Result<(), GoblinError> {
    if args.len() != n {
        return Err(GoblinError::ArityMismatch { expected: n, got: args.len(), name: name.into() });
    }
    Ok(())
}

/// `:post(handle, message)`.
pub fn post(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    expect_args(&args, 2, "post")?;
    let h = handle_arg(&args[0], ":post")?.clone();
    let msg = &args[1];
    match h {
        Handle::Actor(a) => {
            let p = ToPortable::new(session).value(msg)?;
            match a.mailbox.try_push(Msg::Post(p)) {
                Push::Done => Ok(Value::Bool(true)),
                Push::Full(_) => Err(err(format!(
                    "C0114: actor-mailbox-full: the actor's mailbox is full ({} messages); it is not keeping up", a.mailbox.cap))),
                Push::Closed(_) => Err(a.stopped_error()),
            }
        }
        Handle::Channel(c) => {
            if matches!(msg, Value::Nil) {
                return Err(err("C0117: channel-nil: nil cannot be posted to a channel; :receive gives nil once a channel is closed"));
            }
            let p = ToPortable::new(session).value(msg)?;
            match c.queue.push_wait(p) {
                Push::Done => Ok(Value::Bool(true)),
                _ => Err(err("C0117: channel-closed: the channel is closed and takes no more values")),
            }
        }
        Handle::Future(_) => Err(err("C0118: not-a-handle: :post expects an actor or a channel, got a future")),
    }
}

/// `:request(handle, message)`: post, and get a future for the answer.
pub fn request(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    expect_args(&args, 2, "request")?;
    let Handle::Actor(a) = handle_arg(&args[0], ":request")?.clone() else {
        return Err(err("C0118: not-a-handle: :request expects an actor"));
    };
    let p = ToPortable::new(session).value(&args[1])?;
    let reply = Arc::new(Reply { id: next_id(), slot: Mutex::new(None), ready: Condvar::new(), actor: a.id });
    match a.mailbox.try_push(Msg::Request(p, reply.clone())) {
        Push::Done => Ok(Value::Handle(Handle::Future(reply))),
        Push::Full(_) => Err(err(format!(
            "C0114: actor-mailbox-full: the actor's mailbox is full ({} messages); it is not keeping up", a.mailbox.cap))),
        Push::Closed(_) => Err(a.stopped_error()),
    }
}

/// `:await(future)` or `:await(actor)`.
pub fn await_(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    expect_args(&args, 1, "await")?;
    let me = CURRENT_ACTOR.with(|c| c.get());
    match handle_arg(&args[0], ":await")?.clone() {
        Handle::Future(r) => {
            if me != 0 && r.actor == me && r.slot.lock().unwrap_or_else(|x| x.into_inner()).is_none() {
                return Err(err("C0119: actor-self-wait: an actor cannot wait for its own answer; it would wait forever"));
            }
            match r.wait() {
                Ok(p) => Ok(snapshot(&p, session)),
                Err(e) => Err(err(e)),
            }
        }
        Handle::Actor(a) => {
            if me == a.id {
                return Err(err("C0119: actor-self-wait: an actor cannot wait for itself to end; it would wait forever"));
            }
            match a.wait_end() {
                End::Done(p) => Ok(snapshot(&p, session)),
                End::Failed(e) => Err(err(format!("C0112: actor-failed: the actor stopped because its handler failed: {e}"))),
                End::Cancelled => Err(err("C0116: actor-cancelled: the actor was cancelled")),
            }
        }
        Handle::Channel(_) => Err(err("C0118: not-a-handle: :await expects a future or an actor, got a channel; use :receive")),
    }
}

/// `:receive(channel)`: the next value, or nil once the channel is closed and empty.
pub fn receive(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    if args.is_empty() {
        return Err(err(
            "C0120: receive-unsupported: :receive() is not built yet; an actor gets its messages through its handler (lightweight actors, stage 4)"));
    }
    if args.len() > 2 {
        return Err(err(format!("C0107: actor-args: :receive takes a channel and an optional wait limit, got {} arguments", args.len())));
    }
    // `:receive(ch, 20s)` gives up after 20s and returns nil.
    let limit = match args.get(1) {
        None => None,
        Some(Value::Duration(d)) if d.ns >= 0 => Some(Duration::from_nanos(d.ns.min(u64::MAX as i128) as u64)),
        Some(other) => return Err(err(format!(
            "C0107: actor-args: the wait limit for :receive must be a duration like 20s, got {}",
            crate::builtins::value_to_str(other)))),
    };
    match handle_arg(&args[0], ":receive")?.clone() {
        Handle::Channel(c) => match c.queue.pop_within(limit) {
            Pop::Item(p) => Ok(snapshot(&p, session)),
            _ => Ok(Value::Nil),
        },
        other => Err(err(format!("C0118: not-a-handle: :receive expects a channel, got {}", other.kind()))),
    }
}

/// `:halt(handle)`: an actor finishes what it accepted and ends; a channel closes.
pub fn halt(args: Vec<Value>) -> Result<Value, GoblinError> {
    expect_args(&args, 1, "halt")?;
    match handle_arg(&args[0], ":halt")? {
        Handle::Actor(a) => a.halt(),
        Handle::Channel(c) => c.queue.close(),
        Handle::Future(_) => return Err(err("C0118: not-a-handle: :halt expects an actor or a channel, got a future")),
    }
    Ok(Value::Nil)
}

/// `:cancel(actor)`: end after the current message; the rest are failed.
pub fn cancel(args: Vec<Value>) -> Result<Value, GoblinError> {
    expect_args(&args, 1, "cancel")?;
    match handle_arg(&args[0], ":cancel")? {
        Handle::Actor(a) => {
            a.cancelled.store(true, Ordering::SeqCst);
            a.mailbox.close();
            Ok(Value::Nil)
        }
        other => Err(err(format!("C0118: not-a-handle: :cancel expects an actor, got {}", other.kind()))),
    }
}

/// `:channel(capacity)`.
pub fn channel(args: Vec<Value>) -> Result<Value, GoblinError> {
    expect_args(&args, 1, "channel")?;
    match &args[0] {
        Value::Int(n) if *n >= 1 => Ok(Value::Handle(Handle::Channel(Arc::new(Channel {
            id: next_id(),
            queue: Queue::new(*n as usize),
        })))),
        other => Err(err(format!(
            "C0117: channel-capacity: a channel's capacity must be a positive whole number, got {}", crate::builtins::value_to_str(other)))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_queue_refuses_or_waits() {
        let q = Arc::new(Queue::new(2));
        assert!(matches!(q.try_push(1), Push::Done));
        assert!(matches!(q.try_push(2), Push::Done));
        assert!(matches!(q.try_push(3), Push::Full(3)));
        // A waiting push goes through once a slot frees up.
        let q2 = q.clone();
        let t = std::thread::spawn(move || matches!(q2.push_wait(3), Push::Done));
        std::thread::sleep(Duration::from_millis(20));
        assert!(matches!(q.pop(None), Pop::Item(1)));
        assert!(t.join().unwrap());
        assert!(matches!(q.pop(None), Pop::Item(2)));
        assert!(matches!(q.pop(None), Pop::Item(3)));
    }

    #[test]
    fn closed_queue_drains_then_ends() {
        let q = Queue::new(4);
        let _ = q.try_push("a");
        q.close();
        assert!(matches!(q.try_push("b"), Push::Closed("b")));
        assert!(matches!(q.pop(None), Pop::Item("a")));
        assert!(matches!(q.pop(None), Pop::Closed));
    }

    #[test]
    fn idle_close_never_loses_an_accepted_item() {
        // Many short idle waits racing with pushes: every push that was
        // accepted is popped, and a push after the idle close is refused.
        for _ in 0..200 {
            let q = Arc::new(Queue::new(8));
            let q2 = q.clone();
            let producer = std::thread::spawn(move || {
                let mut accepted = 0;
                for i in 0..50 {
                    if let Push::Done = q2.try_push(i) { accepted += 1; }
                    if i % 7 == 0 { std::thread::yield_now(); }
                }
                accepted
            });
            let mut taken = 0;
            loop {
                match q.pop(Some(Duration::from_micros(200))) {
                    Pop::Item(_) => taken += 1,
                    Pop::Closed | Pop::Idle => break,
                }
            }
            let accepted = producer.join().unwrap();
            taken += q.drain().len();
            assert_eq!(taken, accepted);
        }
    }
}
