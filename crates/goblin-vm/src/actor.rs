//! Persistent actors and bounded channels (concurrency spec, stages 3 and 4).
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
//! Scheduling (stage 4): actors are lightweight. A fixed pool of worker
//! threads (`GOBLIN_ACTOR_THREADS`, one per core by default) runs them all.
//! Each actor lives on one worker for its whole life: its Session and VM are
//! built on that thread and never leave it, so no `Rc` crosses threads
//! (spec XIV). An actor with nothing to do holds no thread. When a handler
//! waits (`:await`, `:receive`, `:post` to a full channel), the VM saves the
//! handler where it stopped and the worker runs other actors; the handler
//! continues once the value it waits for is there. A handler that runs long
//! gives its worker up every so often, so one busy actor cannot starve the
//! others on its worker.
//!
//! A wait inside a builtin that calls back into Goblin (`:map_fn` and the
//! like) cannot be saved that way, because part of the handler is then Rust
//! code. Such a wait keeps its worker, which runs its other actors meanwhile.

use std::cell::{Cell, RefCell};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::time::{Duration, Instant};

use crate::error::GoblinError;
use crate::portable::{FromPortable, PValue, ToPortable};
use crate::session::Session;
use crate::value::{BuiltinId, Value};
use crate::vm::{Operand, Slice, Vm};

static NEXT_ID: AtomicU64 = AtomicU64::new(1);
static LIVE_ACTORS: AtomicUsize = AtomicUsize::new(0);

const WORKER_STACK: usize = 64 * 1024 * 1024;
const DEFAULT_MAILBOX: usize = 1000;
/// Messages one actor handles before its worker moves on to the next actor.
const MESSAGE_BUDGET: usize = 64;
/// VM instructions a handler runs before it lets the other actors on its
/// worker have a turn.
pub(crate) const OP_BUDGET: u32 = 20_000;
/// How long an idle worker keeps looking for work before it sleeps.
const SPIN: Duration = Duration::from_micros(50);
/// How deep a worker that waits inside a callback goes running other actors.
const MAX_HELP_DEPTH: u32 = 8;

fn next_id() -> u64 { NEXT_ID.fetch_add(1, Ordering::Relaxed) }

fn max_actors() -> usize {
    std::env::var("GOBLIN_MAX_ACTORS").ok()
        .and_then(|s| s.trim().parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(1_000_000)
}

fn err(msg: impl Into<String>) -> GoblinError { GoblinError::Runtime(msg.into()) }

fn lock<T>(m: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|e| e.into_inner())
}

thread_local! {
    /// The actor this thread is running right now, if any.
    static CURRENT_ACTOR: RefCell<Option<Arc<ActorShared>>> = const { RefCell::new(None) };
}

fn current_actor() -> Option<Arc<ActorShared>> { CURRENT_ACTOR.with(|c| c.borrow().clone()) }
fn current_actor_id() -> u64 { CURRENT_ACTOR.with(|c| c.borrow().as_ref().map(|a| a.id).unwrap_or(0)) }

// ── Waking ───────────────────────────────────────────────────────────────────

/// Lets a plain thread (the main program, a request) sleep until woken.
pub struct Parker {
    woken: Mutex<bool>,
    cv: Condvar,
}

impl Parker {
    fn new() -> Self { Parker { woken: Mutex::new(false), cv: Condvar::new() } }
    fn unpark(&self) {
        *lock(&self.woken) = true;
        self.cv.notify_one();
    }
    fn park(&self, until: Option<Instant>) {
        // Answers often come back within microseconds: look a few times
        // before sleeping, like an idle worker does.
        let spin_until = Instant::now() + SPIN;
        while Instant::now() < spin_until {
            let mut w = lock(&self.woken);
            if *w { *w = false; return; }
            drop(w);
            std::thread::yield_now();
        }
        let mut w = lock(&self.woken);
        while !*w {
            match until {
                None => w = self.cv.wait(w).unwrap_or_else(|e| e.into_inner()),
                Some(d) => {
                    let now = Instant::now();
                    if now >= d { break; }
                    w = self.cv.wait_timeout(w, d - now).unwrap_or_else(|e| e.into_inner()).0;
                }
            }
        }
        *w = false;
    }
}

thread_local! {
    static PARKER: Arc<Parker> = Arc::new(Parker::new());
}

/// Who to wake when something waited for happens.
#[derive(Clone)]
pub enum Waker {
    /// A suspended or idle actor: schedule it on its worker.
    Actor(Arc<ActorShared>),
    /// A plain thread blocked in a wait.
    Thread(Arc<Parker>),
    /// A worker blocked in a wait inside a callback.
    Worker(Arc<WorkerQ>),
}

impl Waker {
    fn same(&self, o: &Waker) -> bool {
        match (self, o) {
            (Waker::Actor(a), Waker::Actor(b)) => Arc::ptr_eq(a, b),
            (Waker::Thread(a), Waker::Thread(b)) => Arc::ptr_eq(a, b),
            (Waker::Worker(a), Waker::Worker(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
    fn wake(self) {
        match self {
            Waker::Actor(a) => schedule(&a),
            Waker::Thread(p) => p.unpark(),
            Waker::Worker(q) => q.poke(),
        }
    }
}

/// Add a waiter once: a waiter that checks again re-registers, so a list
/// never holds the same waiter twice.
fn add_waiter(list: &mut Vec<Waker>, w: &Waker) {
    if !list.iter().any(|x| x.same(w)) { list.push(w.clone()); }
}

fn wake_all(list: Vec<Waker>) { for w in list { w.wake(); } }

/// Wait until `check` gives a value, or until `deadline` (None when it
/// passes). `check` looks, and when there is nothing yet, registers the
/// waker it is given under the same lock, so no wake-up is lost. A worker
/// thread runs its other actors while it waits.
fn block_on<R>(deadline: Option<Instant>, mut check: impl FnMut(&Waker) -> Option<R>) -> Option<R> {
    if let Some(q) = WORKER.with(|w| w.borrow().clone()) {
        let waker = Waker::Worker(q.clone());
        let mut timer_set = false;
        loop {
            let seen = q.epoch();
            if let Some(r) = check(&waker) { return Some(r); }
            if let Some(d) = deadline {
                if Instant::now() >= d { return None; }
                if !timer_set { timer_at(d, waker.clone()); timer_set = true; }
            }
            help_or_wait(&q, seen);
        }
    }
    let p = PARKER.with(|p| p.clone());
    let waker = Waker::Thread(p.clone());
    loop {
        if let Some(r) = check(&waker) { return Some(r); }
        if deadline.is_some_and(|d| Instant::now() >= d) { return None; }
        p.park(deadline);
    }
}

// ── Bounded queue ────────────────────────────────────────────────────────────

pub struct Queue<T> {
    state: Mutex<QState<T>>,
    cap: usize,
}

struct QState<T> {
    items: VecDeque<T>,
    closed: bool,
    /// Waiting for an item (or the close).
    takers: Vec<Waker>,
    /// Waiting for room.
    givers: Vec<Waker>,
}

pub enum Push<T> { Done, Full(T), Closed(T) }
pub enum Pop<T> { Item(T), Closed, Empty }

impl<T> Queue<T> {
    pub fn new(cap: usize) -> Self {
        Queue {
            state: Mutex::new(QState { items: VecDeque::new(), closed: false, takers: Vec::new(), givers: Vec::new() }),
            cap,
        }
    }

    /// Add without waiting; when full, `w` (if any) is woken once there is room.
    fn push_or_register(&self, v: T, w: Option<&Waker>) -> Push<T> {
        let mut st = lock(&self.state);
        if st.closed { return Push::Closed(v); }
        if st.items.len() >= self.cap {
            if let Some(w) = w { add_waiter(&mut st.givers, w); }
            return Push::Full(v);
        }
        st.items.push_back(v);
        let takers = std::mem::take(&mut st.takers);
        drop(st);
        wake_all(takers);
        Push::Done
    }

    /// Take without waiting; when empty, `w` (if any) is woken once there is
    /// an item or the queue closes.
    fn pop_or_register(&self, w: Option<&Waker>) -> Pop<T> {
        let mut st = lock(&self.state);
        if let Some(v) = st.items.pop_front() {
            let givers = std::mem::take(&mut st.givers);
            drop(st);
            wake_all(givers);
            return Pop::Item(v);
        }
        if st.closed { return Pop::Closed; }
        if let Some(w) = w { add_waiter(&mut st.takers, w); }
        Pop::Empty
    }

    /// Add without waiting: a full or closed queue says so.
    pub fn try_push(&self, v: T) -> Push<T> { self.push_or_register(v, None) }

    /// Add, waiting while the queue is full.
    pub fn push_wait(&self, v: T) -> Push<T> {
        let mut v = Some(v);
        block_on(None, |w| match self.push_or_register(v.take().unwrap(), Some(w)) {
            Push::Full(x) => { v = Some(x); None }
            other => Some(other),
        }).unwrap()
    }

    /// Take the next item, waiting at most `limit` (forever with None).
    /// `Empty` means the time ran out; the queue stays open.
    pub fn pop_within(&self, limit: Option<Duration>) -> Pop<T> {
        let deadline = limit.map(|d| Instant::now() + d);
        block_on(deadline, |w| match self.pop_or_register(Some(w)) {
            Pop::Empty => None,
            other => Some(other),
        }).unwrap_or(Pop::Empty)
    }

    /// Close the queue if it is empty, under the same lock as the check, so
    /// nothing can be accepted and then lost. Says whether it closed.
    fn close_if_empty(&self) -> bool {
        let mut st = lock(&self.state);
        if !st.items.is_empty() { return false; }
        st.closed = true;
        let (t, g) = (std::mem::take(&mut st.takers), std::mem::take(&mut st.givers));
        drop(st);
        wake_all(t);
        wake_all(g);
        true
    }

    /// Accept nothing more. Items already queued stay until taken.
    pub fn close(&self) {
        let mut st = lock(&self.state);
        st.closed = true;
        let (t, g) = (std::mem::take(&mut st.takers), std::mem::take(&mut st.givers));
        drop(st);
        wake_all(t);
        wake_all(g);
    }

    /// Take everything still queued.
    pub fn drain(&self) -> Vec<T> {
        let mut st = lock(&self.state);
        let items: Vec<T> = st.items.drain(..).collect();
        let g = std::mem::take(&mut st.givers);
        drop(st);
        wake_all(g);
        items
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
    slot: Mutex<ReplySlot>,
    /// The actor that will answer, so it cannot wait for itself.
    actor: u64,
}

struct ReplySlot {
    answer: Option<Result<Arc<PValue>, String>>,
    waiters: Vec<Waker>,
}

impl Reply {
    fn set(&self, r: Result<Arc<PValue>, String>) {
        let mut s = lock(&self.slot);
        if s.answer.is_none() { s.answer = Some(r); }
        let w = std::mem::take(&mut s.waiters);
        drop(s);
        wake_all(w);
    }
    fn answered(&self) -> bool { lock(&self.slot).answer.is_some() }
    /// The answer, or None after registering `w`.
    fn poll(&self, w: Option<&Waker>) -> Option<Result<Arc<PValue>, String>> {
        let mut s = lock(&self.slot);
        if let Some(r) = s.answer.as_ref() { return Some(r.clone()); }
        if let Some(w) = w { add_waiter(&mut s.waiters, w); }
        None
    }
    fn wait(&self) -> Result<Arc<PValue>, String> {
        block_on(None, |w| self.poll(Some(w))).unwrap()
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

struct EndSlot {
    end: Option<End>,
    waiters: Vec<Waker>,
}

pub struct ActorShared {
    id: u64,
    mailbox: Queue<Msg>,
    cancelled: AtomicBool,
    /// Set before the mailbox closes when the handler fails.
    failure: Mutex<Option<String>>,
    end: Mutex<EndSlot>,
    /// The object handed over, if the state is an object (its uuid).
    origin: Option<String>,
    /// The worker this actor lives on.
    worker: usize,
    /// A run of this actor is queued on its worker.
    scheduled: AtomicBool,
    /// Requests the running handler took with `:receive()`: answered with
    /// the state the handler returns, like the request that started it.
    pulled: Mutex<Vec<Arc<Reply>>>,
    /// The `idle` policy, and when the actor last took a message: a handler
    /// waiting in `:receive()` counts as idle too.
    idle: Option<Duration>,
    last_active: Mutex<Instant>,
}

impl ActorShared {
    fn finish(&self, e: End) {
        let mut s = lock(&self.end);
        if s.end.is_some() { return; }
        s.end = Some(e);
        let w = std::mem::take(&mut s.waiters);
        drop(s);
        LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst);
        wake_all(w);
    }
    fn ended(&self) -> bool { lock(&self.end).end.is_some() }
    fn poll_end(&self, w: Option<&Waker>) -> Option<End> {
        let mut s = lock(&self.end);
        if let Some(e) = s.end.as_ref() { return Some(e.clone()); }
        if let Some(w) = w { add_waiter(&mut s.waiters, w); }
        None
    }
    fn wait_end(&self) -> End {
        block_on(None, |w| self.poll_end(Some(w))).unwrap()
    }
    fn stopped_error(&self) -> GoblinError {
        if let Some(f) = lock(&self.failure).as_ref() {
            return err(format!("C0112: actor-failed: the actor stopped because its handler failed: {f}"));
        }
        if self.cancelled.load(Ordering::SeqCst) {
            return err("C0116: actor-cancelled: the actor was cancelled and takes no more messages");
        }
        err("C0111: actor-stopped: the actor has stopped and takes no more messages")
    }
    /// Stop taking messages; the actor finishes the ones it accepted.
    pub fn halt(&self) { self.mailbox.close(); }
    /// Stop taking messages and stop the handler at its next checkpoint:
    /// a wait (`:await`, `:receive`, a database or HTTP call, ...) or its
    /// next turn after running a while. Queued requests fail with C0116.
    pub fn cancel(self: &Arc<Self>) {
        self.cancelled.store(true, Ordering::SeqCst);
        self.mailbox.close();
        // Wake it, wherever it waits.
        schedule(self);
    }
}

const CANCELLED_MIDWAY: &str = "C0116: actor-cancelled: the actor was cancelled while it handled this request";

/// Ends every actor a session started: each finishes the messages it already
/// accepted, then ends (structured ownership, spec VI). Called when the
/// session is dropped, so a program's actors never outlive it silently.
/// If the actor doing the waiting is cancelled meanwhile, the ones not yet
/// ended are cancelled instead of waited for.
pub fn finish_owned(actors: &mut Vec<Arc<ActorShared>>) {
    for a in actors.iter() { a.halt(); }
    let me = current_actor();
    let cancelled = || me.as_ref().is_some_and(|m| m.cancelled.load(Ordering::SeqCst));
    for i in 0..actors.len() {
        let a = &actors[i];
        let ended = block_on(None, |w| {
            if cancelled() { return Some(false); }
            a.poll_end(Some(w)).map(|_| true)
        });
        if ended == Some(false) {
            for b in &actors[i..] { b.cancel(); }
            break;
        }
    }
    actors.clear();
}

/// Cancels every actor a session started. Does not wait: each ends at its
/// next checkpoint, so a long chain of owners never nests waits on a worker.
pub fn cancel_owned(actors: &mut Vec<Arc<ActorShared>>) {
    for a in actors.drain(..) { a.cancel(); }
}

/// Shutdown: halt the actors, give them `grace` to finish what they
/// accepted, then cancel the ones still running. Gives how many were
/// cancelled.
pub fn finish_owned_within(actors: &mut Vec<Arc<ActorShared>>, grace: Duration) -> usize {
    for a in actors.iter() { a.halt(); }
    // A grace too long to add to now: wait without a deadline.
    let deadline = Instant::now().checked_add(grace);
    for a in actors.iter() {
        let _ = block_on(deadline, |w| a.poll_end(Some(w)));
    }
    let left: Vec<Arc<ActorShared>> = actors.drain(..).filter(|a| !a.ended()).collect();
    for a in &left { a.cancel(); }
    for a in &left { let _ = a.wait_end(); }
    left.len()
}

// ── Scheduler ────────────────────────────────────────────────────────────────

/// One worker's queue of things to run.
pub struct WorkerQ {
    st: Mutex<WState>,
    cv: Condvar,
}

struct WState {
    jobs: VecDeque<Job>,
    /// Threads asleep on `cv` (the worker, or a wait inside it).
    sleepers: usize,
    /// Bumped on every new job and every wake-up, so a waiting worker can
    /// tell that something happened since it last looked.
    epoch: u64,
}

enum Job {
    Start(Box<Start>),
    Run(Arc<ActorShared>),
}

/// Everything a worker needs to build a new actor. All of it is portable.
struct Start {
    shared: Arc<ActorShared>,
    image: crate::swarm::Image,
    state: PValue,
    handler: PValue,
    stop_when: Option<PValue>,
    idle: Option<Duration>,
    seed: u128,
    worker_id: usize,
    /// Restarts allowed within a minute (`restart: "on_failure"`).
    restart: Option<usize>,
}

impl WorkerQ {
    fn push(&self, j: Job) {
        let mut st = lock(&self.st);
        st.jobs.push_back(j);
        st.epoch += 1;
        let sleeping = st.sleepers > 0;
        drop(st);
        if sleeping { self.cv.notify_all(); }
    }
    fn poke(&self) {
        let mut st = lock(&self.st);
        st.epoch += 1;
        let sleeping = st.sleepers > 0;
        drop(st);
        if sleeping { self.cv.notify_all(); }
    }
    fn epoch(&self) -> u64 { lock(&self.st).epoch }
    fn try_take(&self) -> Option<Job> { lock(&self.st).jobs.pop_front() }
    fn take(&self) -> Job {
        // A message often arrives a few microseconds after the last one was
        // handled; looking again for a moment is far cheaper than going to
        // sleep and being woken by another thread.
        let spin_until = Instant::now() + SPIN;
        loop {
            if let Some(j) = lock(&self.st).jobs.pop_front() { return j; }
            if Instant::now() >= spin_until { break; }
            std::thread::yield_now();
        }
        let mut st = lock(&self.st);
        loop {
            if let Some(j) = st.jobs.pop_front() { return j; }
            st.sleepers += 1;
            st = self.cv.wait(st).unwrap_or_else(|e| e.into_inner());
            st.sleepers -= 1;
        }
    }
    /// Sleep until something new happens after `seen`.
    fn wait_past(&self, seen: u64, want_jobs: bool) {
        let mut st = lock(&self.st);
        while st.epoch == seen && !(want_jobs && !st.jobs.is_empty()) {
            st.sleepers += 1;
            st = self.cv.wait(st).unwrap_or_else(|e| e.into_inner());
            st.sleepers -= 1;
        }
    }
}

struct Sched {
    workers: Vec<Arc<WorkerQ>>,
    next: AtomicUsize,
}

static SCHED: OnceLock<Sched> = OnceLock::new();

fn worker_count() -> usize {
    std::env::var("GOBLIN_ACTOR_THREADS").ok()
        .and_then(|s| s.trim().parse::<usize>().ok())
        .filter(|n| *n > 0)
        .unwrap_or_else(|| std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4))
}

fn sched() -> &'static Sched {
    SCHED.get_or_init(|| {
        let n = worker_count();
        let workers: Vec<Arc<WorkerQ>> = (0..n)
            .map(|_| Arc::new(WorkerQ { st: Mutex::new(WState { jobs: VecDeque::new(), sleepers: 0, epoch: 0 }), cv: Condvar::new() }))
            .collect();
        for (i, q) in workers.iter().enumerate() {
            let q = q.clone();
            std::thread::Builder::new()
                .name(format!("goblin-actors-{i}"))
                .stack_size(WORKER_STACK)
                .spawn(move || worker_main(q))
                .expect("could not start an actor worker thread");
        }
        Sched { workers, next: AtomicUsize::new(0) }
    })
}

/// Queue a run of `a` on its worker, unless one is already queued.
fn schedule(a: &Arc<ActorShared>) {
    if !a.scheduled.swap(true, Ordering::SeqCst) {
        sched().workers[a.worker].push(Job::Run(a.clone()));
    }
}

thread_local! {
    /// Set on worker threads: this worker's queue.
    static WORKER: RefCell<Option<Arc<WorkerQ>>> = const { RefCell::new(None) };
    /// The actors living on this worker that are not running right now.
    static SLOTS: RefCell<HashMap<u64, Box<Slot>>> = RefCell::new(HashMap::new());
    /// Actors running right now on this worker (one, or more when a wait
    /// inside a callback runs others).
    static BUSY: RefCell<HashSet<u64>> = RefCell::new(HashSet::new());
    /// Busy actors that were asked to run again meanwhile.
    static RERUN: RefCell<HashSet<u64>> = RefCell::new(HashSet::new());
    static HELP_DEPTH: Cell<u32> = const { Cell::new(0) };
}

fn worker_main(q: Arc<WorkerQ>) {
    WORKER.with(|w| *w.borrow_mut() = Some(q.clone()));
    loop {
        let job = q.take();
        run_job(job);
    }
}

/// Inside a wait on a worker: run another job if there is one, else sleep
/// until something happens.
fn help_or_wait(q: &Arc<WorkerQ>, seen: u64) {
    let depth = HELP_DEPTH.with(|d| d.get());
    if depth < MAX_HELP_DEPTH {
        if let Some(job) = q.try_take() {
            HELP_DEPTH.with(|d| d.set(depth + 1));
            run_job(job);
            HELP_DEPTH.with(|d| d.set(depth));
            return;
        }
    }
    q.wait_past(seen, depth < MAX_HELP_DEPTH);
}

fn run_job(job: Job) {
    match job {
        Job::Start(s) => {
            let shared = s.shared.clone();
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| Slot::build(*s))) {
                Ok(slot) => turn(shared, Box::new(slot)),
                Err(_) => crash(&shared, "the actor could not be started (internal error)"),
            }
        }
        Job::Run(a) => {
            a.scheduled.store(false, Ordering::SeqCst);
            if let Some(slot) = SLOTS.with(|m| m.borrow_mut().remove(&a.id)) {
                turn(a, slot);
            } else if BUSY.with(|b| b.borrow().contains(&a.id)) {
                RERUN.with(|r| r.borrow_mut().insert(a.id));
            }
            // Otherwise the actor has ended: nothing to do.
        }
    }
}

/// Last resort after a Rust panic: end the actor as failed, so nothing
/// waits for it forever.
fn crash(shared: &ActorShared, msg: &str) {
    lock(&shared.failure).get_or_insert_with(|| msg.to_string());
    shared.mailbox.close();
    for r in std::mem::take(&mut *lock(&shared.pulled)) { r.set(Err(msg.to_string())); }
    for m in shared.mailbox.drain() {
        if let Msg::Request(_, r) = m { r.set(Err(msg.to_string())); }
    }
    shared.finish(End::Failed(msg.to_string()));
}

/// Give an actor a turn on this worker; keep it if it is still alive.
fn turn(shared: Arc<ActorShared>, mut slot: Box<Slot>) {
    let id = shared.id;
    BUSY.with(|b| b.borrow_mut().insert(id));
    let prev = CURRENT_ACTOR.with(|c| c.replace(Some(shared.clone())));
    let alive = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| slot.turn())) {
        Ok(alive) => alive,
        Err(_) => {
            let msg = "the handler crashed (internal error)";
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| slot.fail(msg.into()))) {
                // Restarted: go on with the queued messages.
                Ok(true) => { schedule(&shared); true }
                Ok(false) => false,
                Err(_) => { crash(&shared, msg); false }
            }
        }
    };
    CURRENT_ACTOR.with(|c| *c.borrow_mut() = prev);
    BUSY.with(|b| b.borrow_mut().remove(&id));
    if alive {
        SLOTS.with(|m| m.borrow_mut().insert(id, slot));
        if RERUN.with(|r| r.borrow_mut().remove(&id)) { schedule(&shared); }
    } else {
        RERUN.with(|r| r.borrow_mut().remove(&id));
        drop(slot);
    }
}

// ── Timers ───────────────────────────────────────────────────────────────────

struct Timers {
    st: Mutex<TState>,
    cv: Condvar,
}

struct TState {
    due: BinaryHeap<std::cmp::Reverse<(Instant, u64)>>,
    wakers: HashMap<u64, Waker>,
    seq: u64,
}

static TIMERS: OnceLock<Arc<Timers>> = OnceLock::new();

/// Wake `w` at `at`.
fn timer_at(at: Instant, w: Waker) {
    let t = TIMERS.get_or_init(|| {
        let t = Arc::new(Timers {
            st: Mutex::new(TState { due: BinaryHeap::new(), wakers: HashMap::new(), seq: 0 }),
            cv: Condvar::new(),
        });
        let t2 = t.clone();
        std::thread::Builder::new()
            .name("goblin-timers".into())
            .spawn(move || timer_main(&t2))
            .expect("could not start the timer thread");
        t
    });
    let mut st = lock(&t.st);
    st.seq += 1;
    let n = st.seq;
    st.due.push(std::cmp::Reverse((at, n)));
    st.wakers.insert(n, w);
    drop(st);
    t.cv.notify_one();
}

fn timer_main(t: &Timers) {
    let mut st = lock(&t.st);
    loop {
        let now = Instant::now();
        let mut fire = Vec::new();
        while let Some(std::cmp::Reverse((at, n))) = st.due.peek().copied() {
            if at > now { break; }
            st.due.pop();
            if let Some(w) = st.wakers.remove(&n) { fire.push(w); }
        }
        if !fire.is_empty() {
            drop(st);
            wake_all(fire);
            st = lock(&t.st);
            continue;
        }
        st = match st.due.peek() {
            Some(std::cmp::Reverse((at, _))) => {
                let wait = at.saturating_duration_since(now);
                t.cv.wait_timeout(st, wait).unwrap_or_else(|e| e.into_inner()).0
            }
            None => t.cv.wait(st).unwrap_or_else(|e| e.into_inner()),
        };
    }
}

// ── :actor ───────────────────────────────────────────────────────────────────

struct Policy {
    mailbox: usize,
    idle: Option<Duration>,
    stop_when: Option<Value>,
    /// `restart: "on_failure"`: restarts allowed within a minute.
    restart: Option<usize>,
    max_restarts: Option<usize>,
}

/// Restarts an actor may have within this long before it gives up.
const RESTART_WINDOW: Duration = Duration::from_secs(60);
const DEFAULT_MAX_RESTARTS: usize = 5;
fn policy_of(v: Option<Value>) -> Result<Policy, GoblinError> {
    let mut p = Policy { mailbox: DEFAULT_MAILBOX, idle: None, stop_when: None, restart: None, max_restarts: None };
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
            "restart" => match &val {
                Value::Str(s) if s.as_str() == "never" => p.restart = None,
                Value::Str(s) if s.as_str() == "on_failure" => p.restart = Some(DEFAULT_MAX_RESTARTS),
                other => return Err(err(format!(
                    "C0108: actor-policy: restart must be \"on_failure\" or \"never\", got {}", crate::builtins::value_to_str(other)))),
            },
            "max_restarts" => match val {
                Value::Int(n) if n >= 1 => p.max_restarts = Some(n as usize),
                other => return Err(err(format!(
                    "C0108: actor-policy: max_restarts must be a positive whole number, got {}", crate::builtins::value_to_str(&other)))),
            },
            other => return Err(err(format!(
                "C0108: actor-policy: unknown policy key `{other}` (known: mailbox, idle, stop_when, restart, max_restarts)"))),
        }
    }
    if let (Some(m), Some(_)) = (p.max_restarts, p.restart) { p.restart = Some(m); }
    if p.max_restarts.is_some() && p.restart.is_none() {
        return Err(err("C0109: actor-restart-policy: max_restarts only applies with restart: \"on_failure\""));
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
            // The variable handed over holds the state itself: every
            // reference inside the state is that one holder, not another.
            let allowed = if from_var {
                let mut own = 0usize;
                count_refs(&state, &graph, &mut own);
                own
            } else { 0 };
            if n > allowed {
                return Err(err(
                    "C0113: actor-shared-state: this object is also held somewhere else (another variable, a collection or another object), so it cannot be handed to an actor; drop the other reference first, or hand over a copy made with :freeze"));
            }
        }

        if LIVE_ACTORS.fetch_add(1, Ordering::SeqCst) >= max_actors() {
            LIVE_ACTORS.fetch_sub(1, Ordering::SeqCst);
            return Err(err(format!(
                "C0110: actor-limit: {} actors are already running, the most allowed (set GOBLIN_MAX_ACTORS to change it)", max_actors())));
        }

        let converted = (|| {
            let mut tp = ToPortable::new(&self.session).sharing_values().only_objects(graph.clone());
            let mut roots = vec![&state, &handler];
            if let Some(f) = &policy.stop_when { roots.push(f); }
            let image = crate::swarm::image_for(&self.session, &mut tp, Some(&roots), Some(&graph))?;
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
        let sched = sched();
        let worker = sched.next.fetch_add(1, Ordering::Relaxed) % sched.workers.len();
        let shared = Arc::new(ActorShared {
            id: next_id(),
            mailbox: Queue::new(policy.mailbox),
            cancelled: AtomicBool::new(false),
            failure: Mutex::new(None),
            end: Mutex::new(EndSlot { end: None, waiters: Vec::new() }),
            origin,
            worker,
            scheduled: AtomicBool::new(false),
            pulled: Mutex::new(Vec::new()),
            idle: policy.idle,
            last_active: Mutex::new(Instant::now()),
        });
        // The worker builds the actor's session on its own thread.
        sched.workers[worker].push(Job::Start(Box::new(Start {
            shared: shared.clone(),
            image,
            state: p_state,
            handler: p_handler,
            stop_when: p_stop,
            idle: policy.idle,
            seed: self.session.next_u128(),
            worker_id: self.session.worker_id,
            restart: policy.restart,
        })));

        // Handed over: the objects now belong to the actor.
        // (Left alone when nothing was handed over: a change copies the
        // table, which the next actor's program would then not share.)
        if !graph.is_empty() {
            std::rc::Rc::make_mut(&mut self.session.named_values).retain(|_, v| {
                let mut k = 0; count_refs(v, &graph, &mut k); k == 0
            });
        }
        // Their overlays went with them.
        if !graph.is_empty() {
            self.session.overlay_instances.retain(|i| !graph.contains(&i.host_uuid));
            if !self.session.link_offsets.is_empty() {
                let names = crate::des_image::handed_names(&self.session, &graph);
                self.session.link_offsets.retain(|(from, to, _), _| !names.contains(from) && !names.contains(to));
            }
        }
        for u in &graph {
            self.session.object_store.shift_remove(u.as_str());
            self.session.handed_off.entry(u.clone()).or_insert_with(|| "this object".to_string());
        }
        // Actors that already ended need no shutdown; forget them, so a
        // session that starts many short-lived actors does not keep them all.
        // Pruned when the list doubles, so starting n actors costs O(n).
        if self.session.owned_actors.len().is_power_of_two() {
            self.session.owned_actors.retain(|a| !a.ended());
        }
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
    if let Some(var) = session.kept_home.get(uuid) {
        let what = if var == "this object" { var.clone() } else { format!("`{var}`") };
        return err(format!(
            "C0122: actor-not-owned: {what} belongs to the program that started this actor; an actor only has the objects handed to it in its state. Hand it over in the state, or send the actor what it needs in a message"));
    }
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
// ── Running an actor ─────────────────────────────────────────────────────────

/// What a handler that stopped part-way is waiting for.
pub enum Wait {
    Future(Arc<Reply>),
    End(Arc<ActorShared>),
    /// A channel value; the deadline from `:receive(ch, limit)`, and whether
    /// a timer for it is already set.
    Recv(Arc<Channel>, Option<Instant>, bool),
    /// Room in a full channel for this value.
    Send(Arc<Channel>, PValue),
    /// A database or HTTP call started elsewhere (host.rs).
    Io(Arc<IoSlot>),
    /// The next message in this actor's own mailbox (`:receive()` in a
    /// handler); the deadline, and whether a timer for it is set.
    Mailbox(Arc<ActorShared>, Option<Instant>, bool),
}

/// Where a host call started by an actor leaves its outcome.
pub struct IoSlot {
    slot: Mutex<(Option<crate::host::Finish>, Vec<Waker>)>,
}

impl IoSlot {
    fn finish(&self, f: crate::host::Finish) {
        let mut s = lock(&self.slot);
        s.0 = Some(f);
        let w = std::mem::take(&mut s.1);
        drop(s);
        wake_all(w);
    }
    /// The outcome, or None after registering `w`.
    fn poll(&self, w: Option<&Waker>) -> Option<crate::host::Finish> {
        let mut s = lock(&self.slot);
        if let Some(f) = s.0.take() { return Some(f); }
        if let Some(w) = w { add_waiter(&mut s.1, w); }
        None
    }
}

/// True on an actor worker thread.
pub fn on_worker() -> bool { WORKER.with(|w| w.borrow().is_some()) }

fn start_host_call(call: crate::host::HostCall) -> Wait {
    let slot = Arc::new(IoSlot { slot: Mutex::new((None, Vec::new())) });
    let s2 = slot.clone();
    call.start(move |f| s2.finish(f));
    Wait::Io(slot)
}

/// A database or HTTP call from an actor's handler where the VM can stop
/// it part-way: started elsewhere, with the actor woken once it is done.
pub fn start_io(call: crate::host::HostCall, session: &mut Session) -> Result<Started, GoblinError> {
    let w = start_host_call(call);
    let me = current_actor().map(Waker::Actor);
    match poll_wait(w, me.as_ref(), session) {
        Ok(r) => Ok(Started::Now(r?)),
        Err(w) => Ok(Started::Later(w)),
    }
}

/// A database or HTTP call on a worker where the handler cannot stop
/// (inside a callback): the worker runs its other actors while it waits.
pub fn io_here(call: crate::host::HostCall, session: &mut Session) -> Result<Value, GoblinError> {
    wait_here(start_host_call(call), session)
}

type BlockingJob = Box<dyn FnOnce() + Send>;

struct BlockingPool {
    tx: Mutex<std::sync::mpsc::Sender<BlockingJob>>,
    rx: Arc<Mutex<std::sync::mpsc::Receiver<BlockingJob>>>,
    /// Jobs sent and not yet finished.
    outstanding: AtomicUsize,
    /// Threads started.
    live: AtomicUsize,
    max: usize,
}

/// Run `job` on a thread kept for calls that block (outbound HTTP). A new
/// thread starts whenever more jobs are waiting or running than there are
/// threads, up to `GOBLIN_IO_THREADS` (default 64); past that, jobs wait
/// their turn.
pub fn run_blocking(job: BlockingJob) {
    static POOL: OnceLock<BlockingPool> = OnceLock::new();
    let p = POOL.get_or_init(|| {
        let (tx, rx) = std::sync::mpsc::channel();
        let max = std::env::var("GOBLIN_IO_THREADS").ok().and_then(|v| v.parse().ok()).filter(|n| *n > 0).unwrap_or(64);
        BlockingPool { tx: Mutex::new(tx), rx: Arc::new(Mutex::new(rx)), outstanding: AtomicUsize::new(0), live: AtomicUsize::new(0), max }
    });
    let wanted = p.outstanding.fetch_add(1, Ordering::SeqCst) + 1;
    let mut live = p.live.load(Ordering::SeqCst);
    while wanted > live && live < p.max {
        match p.live.compare_exchange(live, live + 1, Ordering::SeqCst, Ordering::SeqCst) {
            Ok(_) => {
                let rx = p.rx.clone();
                let outstanding: &'static AtomicUsize = &p.outstanding;
                let spawned = std::thread::Builder::new().name("goblin-io".into()).spawn(move || loop {
                    let job = lock(&rx).recv();
                    match job {
                        Ok(j) => { j(); outstanding.fetch_sub(1, Ordering::SeqCst); }
                        Err(_) => return,
                    }
                });
                if spawned.is_err() { p.live.fetch_sub(1, Ordering::SeqCst); }
                break;
            }
            Err(now) => live = now,
        }
    }
    let _ = lock(&p.tx).send(job);
}

/// What a waiting builtin gives inside an actor's handler.
pub enum Started { Now(Value), Later(Wait) }

/// An actor as its worker keeps it. Never leaves its worker thread.
struct Slot {
    shared: Arc<ActorShared>,
    vm: Vm,
    handler: Value,
    stop_when: Option<Value>,
    state: Value,
    idle: Option<Duration>,
    /// When the idle timer set last will fire.
    idle_timer: Option<Instant>,
    /// The message being handled, while the handler is stopped part-way.
    current: Option<Current>,
    /// What a restart starts from, when the actor restarts on failure.
    restart: Option<Box<Restart>>,
}

/// An actor that restarts keeps what it was started from, so a restart
/// builds it again from scratch: its starting state, its own copy of the
/// program's variables, and nothing the failed run left behind.
struct Restart {
    image: crate::swarm::Image,
    state: PValue,
    handler: PValue,
    stop_when: Option<PValue>,
    seed: u128,
    worker_id: usize,
    max: usize,
    /// When it restarted, within the last minute.
    times: VecDeque<Instant>,
}

/// A fresh session for an actor, with its handler, stop condition and state.
fn incarnate(image: &crate::swarm::Image, handler: &PValue, stop_when: Option<&PValue>, state: &PValue, seed: u128, worker_id: usize) -> (Vm, Value, Option<Value>, Value) {
    let mut fp = FromPortable::new().sharing_functions();
    let mut vm = Vm::new(crate::swarm::build_session(image, &mut fp, worker_id));
    vm.session.rng_state = seed | 1;
    let handler = fp.value(handler, &mut vm.session);
    let stop_when = stop_when.map(|p| fp.value(p, &mut vm.session));
    // Every object in the state, however deep, lives in the actor's object
    // store, so its ticks and field writes reach it.
    fp.adopting(true);
    let first = fp.value(state, &mut vm.session);
    fp.adopting(false);
    let state = adopt(first, &mut vm.session);
    (vm, handler, stop_when, state)
}

struct Current {
    reply: Option<Arc<Reply>>,
    /// Call depth the handler runs above.
    base: usize,
    /// None: ready to go on (it gave its worker up after running a while).
    wait: Option<Wait>,
}

impl Slot {
    fn build(s: Start) -> Slot {
        let (vm, handler, stop_when, state) = incarnate(&s.image, &s.handler, s.stop_when.as_ref(), &s.state, s.seed, s.worker_id);
        let restart = s.restart.map(|max| Box::new(Restart {
            image: s.image, state: s.state, handler: s.handler, stop_when: s.stop_when,
            seed: s.seed, worker_id: s.worker_id, max, times: VecDeque::new(),
        }));
        Slot {
            shared: s.shared,
            vm,
            handler,
            stop_when,
            state,
            idle: s.idle,
            idle_timer: None,
            current: None,
            restart,
        }
    }

    /// Run until the actor has nothing to do, waits, or has had its share
    /// of the worker. Says whether the actor is still alive.
    fn turn(&mut self) -> bool {
        let me = Waker::Actor(self.shared.clone());
        let mut handled = 0usize;
        loop {
            if self.current.is_some() && self.shared.cancelled.load(Ordering::SeqCst) {
                self.cancel_midway();
                return false;
            }
            if let Some(cur) = self.current.as_mut() {
                let base = cur.base;
                let step = match cur.wait.take() {
                    None => self.vm.continue_actor(base),
                    Some(w) => match poll_wait(w, Some(&me), &mut self.vm.session) {
                        Ok(r) => self.vm.resume_actor(r, base),
                        Err(w) => { cur.wait = Some(w); return true; }
                    },
                };
                match self.after_step(step) {
                    Some(true) => continue,
                    Some(false) => return true,
                    None => return false,
                }
            }

            if self.shared.cancelled.load(Ordering::SeqCst) {
                self.end_cancelled();
                return false;
            }
            if handled >= MESSAGE_BUDGET {
                schedule(&self.shared);
                return true;
            }
            match self.shared.mailbox.pop_or_register(Some(&me)) {
                Pop::Item(msg) => {
                    handled += 1;
                    *lock(&self.shared.last_active) = Instant::now();
                    // A :cancel that landed while this message was being
                    // taken: it is not handled, like the rest of the queue.
                    if self.shared.cancelled.load(Ordering::SeqCst) {
                        if let Msg::Request(_, r) = msg {
                            r.set(Err("C0116: actor-cancelled: the actor was cancelled before it handled this request".into()));
                        }
                        self.end_cancelled();
                        return false;
                    }
                    let (p_msg, reply) = match msg { Msg::Post(p) => (p, None), Msg::Request(p, r) => (p, Some(r)) };
                    // Converted afresh per message, so nothing from old messages is kept.
                    let message = {
                        let m = snapshot(&p_msg, &mut self.vm.session);
                        adopt(m, &mut self.vm.session)
                    };
                    drop(p_msg);
                    let base = self.vm.call_stack.len();
                    self.current = Some(Current { reply, base, wait: None });
                    // The state is handed to the handler, not copied: the
                    // handler's result replaces it, and if the handler fails
                    // the actor restarts from its starting state or ends.
                    // A copy kept here would make every in-place change the
                    // handler makes copy the state first.
                    let state = std::mem::replace(&mut self.state, Value::Nil);
                    let step = self.vm.start_actor_call(self.handler.clone(), vec![state, message]);
                    match self.after_step(step) {
                        Some(true) => continue,
                        Some(false) => return true,
                        None => return false,
                    }
                }
                Pop::Closed => {
                    self.end_done();
                    return false;
                }
                Pop::Empty => {
                    if let Some(idle) = self.idle {
                        let due = *lock(&self.shared.last_active) + idle;
                        if Instant::now() >= due {
                            // Closes only if still empty; either way, look again.
                            self.shared.mailbox.close_if_empty();
                            continue;
                        }
                        if self.idle_timer != Some(due) {
                            timer_at(due, me.clone());
                            self.idle_timer = Some(due);
                        }
                    }
                    return true;
                }
            }
        }
    }

    /// After the handler ran for a while. Some(true): the message is done,
    /// take the next. Some(false): the handler waits or gave the worker up.
    /// None: the actor has ended.
    fn after_step(&mut self, step: Result<Slice, GoblinError>) -> Option<bool> {
        // Cancelled during this step: the request fails as cancelled, even
        // if the handler caught the failed wait or finished anyway.
        if self.shared.cancelled.load(Ordering::SeqCst) {
            self.cancel_midway();
            return None;
        }
        match step {
            Ok(Slice::Done(v)) => {
                let reply = self.current.take().and_then(|c| c.reply);
                if self.finish_message(v, reply) { Some(true) } else { None }
            }
            Ok(Slice::Waiting(w)) => {
                if let Some(c) = self.current.as_mut() { c.wait = Some(w); }
                Some(false)
            }
            Ok(Slice::Yielded) => {
                schedule(&self.shared);
                Some(false)
            }
            Err(e) => {
                if self.fail(e.to_string()) { Some(true) } else { None }
            }
        }
    }

    /// The handler returned `next`. Says whether the actor goes on.
    fn finish_message(&mut self, next: Value, reply: Option<Arc<Reply>>) -> bool {
        if matches!(next, Value::Nil) {
            return self.fail_with("the handler returned nil; it must return the actor's next state".into(), reply);
        }
        self.state = adopt(next, &mut self.vm.session);
        let pulled = std::mem::take(&mut *lock(&self.shared.pulled));
        if reply.is_some() || !pulled.is_empty() {
            let current = deref(self.state.clone(), &self.vm.session);
            let answer = ToPortable::new(&self.vm.session).value(&current).map(Arc::new).map_err(|e| e.to_string());
            for r in reply.into_iter().chain(pulled) { r.set(answer.clone()); }
        }
        if let Some(sw) = self.stop_when.clone() {
            let current = deref(self.state.clone(), &self.vm.session);
            let result = self.vm.call_callable(sw, vec![current]);
            // Requests the stop condition took with `:receive()` get the state.
            let pulled = std::mem::take(&mut *lock(&self.shared.pulled));
            if !pulled.is_empty() && result.is_ok() {
                let current = deref(self.state.clone(), &self.vm.session);
                let answer = ToPortable::new(&self.vm.session).value(&current).map(Arc::new).map_err(|e| e.to_string());
                for r in pulled { r.set(answer.clone()); }
            } else if !pulled.is_empty() {
                *lock(&self.shared.pulled) = pulled;
            }
            match result {
                Ok(v) => if v.is_truthy() { self.shared.mailbox.close(); },
                // Cancelled while the stop condition waited: that is a
                // cancellation, not a failure.
                Err(_) if self.shared.cancelled.load(Ordering::SeqCst) => {
                    self.end_cancelled();
                    return false;
                }
                Err(e) => {
                    return self.fail_with(format!("stop_when failed: {e}"), None);
                }
            }
        }
        true
    }

    /// Says whether the actor goes on (it restarted).
    fn fail(&mut self, msg: String) -> bool {
        let reply = self.current.take().and_then(|c| c.reply);
        self.fail_with(msg, reply)
    }

    fn name(&self) -> String {
        match &self.handler {
            Value::Function(f) => f.name.clone(),
            Value::Closure(c) => c.func.name.clone(),
            other => crate::builtins::fmt_value_raw(other),
        }
    }

    /// `restart: "on_failure"`: build the actor again from what it was
    /// started from. False when it may not restart (not asked for, being
    /// cancelled, or out of restarts for this minute).
    fn restart(&mut self, msg: &str) -> bool {
        if self.shared.cancelled.load(Ordering::SeqCst) { return false; }
        let name = self.name();
        let Some(r) = self.restart.as_mut() else { return false };
        let now = Instant::now();
        while r.times.front().is_some_and(|t| now.duration_since(*t) >= RESTART_WINDOW) { r.times.pop_front(); }
        if r.times.len() >= r.max {
            eprintln!("goblin: actor `{name}` failed: {msg}; it already restarted {} times within a minute, so it stays stopped", r.max);
            return false;
        }
        r.times.push_back(now);
        let n = r.times.len();
        let max = r.max;
        let (vm, handler, stop_when, state) = incarnate(&r.image, &r.handler, r.stop_when.as_ref(), &r.state, r.seed, r.worker_id);
        // The actors the failed run started go with it.
        self.vm.session.cancel_actors();
        self.vm = vm;
        self.handler = handler;
        self.stop_when = stop_when;
        self.state = state;
        *lock(&self.shared.last_active) = now;
        eprintln!("goblin: actor `{name}` failed: {msg}; restarted with its starting state ({n} of {max} restarts this minute)");
        true
    }

    /// The handler failed: refuse new messages, fail the queued requests,
    /// let the actors this one started finish, then end.
    /// The handler failed. Its requests get the error; with `restart:
    /// "on_failure"` the actor starts again and goes on with its queued
    /// messages (true). Otherwise it refuses new messages, fails the queued
    /// requests, lets the actors it started finish, and ends (false).
    fn fail_with(&mut self, msg: String, reply: Option<Arc<Reply>>) -> bool {
        let pulled = std::mem::take(&mut *lock(&self.shared.pulled));
        let told = reply.is_some() || !pulled.is_empty();
        for r in pulled { r.set(Err(msg.clone())); }
        if let Some(r) = &reply { r.set(Err(msg.clone())); }
        if self.restart.is_some() && self.restart(&msg) { return true; }
        let shared = &self.shared;
        *lock(&shared.failure) = Some(msg.clone());
        shared.mailbox.close();
        // No request carries this failure back (a posted message, or the
        // stop condition): say so, so it is never lost silently. (A
        // restarting actor that gave up has said so already.)
        if !told && self.restart.is_none() {
            eprintln!("goblin: actor `{}` failed: {msg}", self.name());
        }
        let rest = format!("C0112: actor-failed: the actor stopped because its handler failed: {msg}");
        for m in shared.mailbox.drain() {
            if let Msg::Request(_, r) = m { r.set(Err(rest.clone())); }
        }
        self.vm.session.finish_actors();
        self.shared.finish(End::Failed(msg));
        false
    }

    /// Cancelled with a message in progress: its request fails, then the
    /// actor ends as cancelled.
    fn cancel_midway(&mut self) {
        if let Some(r) = self.current.take().and_then(|c| c.reply) {
            r.set(Err(CANCELLED_MIDWAY.into()));
        }
        for r in std::mem::take(&mut *lock(&self.shared.pulled)) {
            r.set(Err(CANCELLED_MIDWAY.into()));
        }
        self.end_cancelled();
    }

    fn end_cancelled(&mut self) {
        self.shared.mailbox.close();
        for m in self.shared.mailbox.drain() {
            if let Msg::Request(_, r) = m {
                r.set(Err("C0116: actor-cancelled: the actor was cancelled before it handled this request".into()));
            }
        }
        for r in std::mem::take(&mut *lock(&self.shared.pulled)) {
            r.set(Err(CANCELLED_MIDWAY.into()));
        }
        self.vm.session.cancel_actors();
        self.shared.finish(End::Cancelled);
    }

    fn end_done(&mut self) {
        let current = deref(self.state.clone(), &self.vm.session);
        let end = match ToPortable::new(&self.vm.session).value(&current) {
            Ok(p) => End::Done(Arc::new(p)),
            Err(e) => End::Failed(format!("its final state cannot be copied out: {e}")),
        };
        // Requests the stop condition took with `:receive()` get the final state.
        for r in std::mem::take(&mut *lock(&self.shared.pulled)) {
            r.set(match &end { End::Done(p) => Ok(p.clone()), End::Failed(e) => Err(e.clone()), End::Cancelled => Err(CANCELLED_MIDWAY.into()) });
        }
        self.vm.session.finish_actors();
        // Cancelled while the actors it started were finishing.
        let end = if self.shared.cancelled.load(Ordering::SeqCst) { End::Cancelled } else { end };
        self.shared.finish(end);
    }
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
                    if k == "uuid" { *v = PValue::Str(new.as_str().into()); } else { walk(v, ids); }
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

/// Look once at what `w` waits for. `Ok` with the outcome when it is there;
/// otherwise `waker` is registered and the wait comes back.
fn poll_wait(w: Wait, waker: Option<&Waker>, session: &mut Session) -> Result<Result<Value, GoblinError>, Wait> {
    match w {
        Wait::Future(r) => match r.poll(waker) {
            Some(Ok(p)) => Ok(Ok(snapshot(&p, session))),
            Some(Err(e)) => Ok(Err(err(e))),
            None => Err(Wait::Future(r)),
        },
        Wait::End(a) => match a.poll_end(waker) {
            Some(End::Done(p)) => Ok(Ok(snapshot(&p, session))),
            Some(End::Failed(e)) => Ok(Err(err(format!("C0112: actor-failed: the actor stopped because its handler failed: {e}")))),
            Some(End::Cancelled) => Ok(Err(err("C0116: actor-cancelled: the actor was cancelled"))),
            None => Err(Wait::End(a)),
        },
        Wait::Recv(c, deadline, timer_set) => match c.queue.pop_or_register(waker) {
            Pop::Item(p) => Ok(Ok(snapshot(&p, session))),
            Pop::Closed => Ok(Ok(Value::Nil)),
            Pop::Empty => {
                let mut timer_set = timer_set;
                if let Some(d) = deadline {
                    if Instant::now() >= d { return Ok(Ok(Value::Nil)); }
                    if !timer_set {
                        if let Some(w) = waker { timer_at(d, w.clone()); }
                        timer_set = true;
                    }
                }
                Err(Wait::Recv(c, deadline, timer_set))
            }
        },
        Wait::Send(c, p) => match c.queue.push_or_register(p, waker) {
            Push::Done => Ok(Ok(Value::Bool(true))),
            Push::Closed(_) => Ok(Err(channel_closed())),
            Push::Full(p) => Err(Wait::Send(c, p)),
        },
        Wait::Io(s) => match s.poll(waker) {
            Some(f) => Ok(f()),
            None => Err(Wait::Io(s)),
        },
        Wait::Mailbox(a, deadline, timer_set) => loop {
            match a.mailbox.pop_or_register(waker) {
                Pop::Item(msg) => {
                    *lock(&a.last_active) = Instant::now();
                    let p = match msg {
                        Msg::Post(p) => p,
                        Msg::Request(p, r) => { lock(&a.pulled).push(r); p }
                    };
                    return Ok(Ok(snapshot(&p, session)));
                }
                Pop::Closed => return Ok(Ok(Value::Nil)),
                Pop::Empty => {
                    let now = Instant::now();
                    // Idle for the `idle` policy's time: the mailbox closes,
                    // `:receive()` gives nil, and the actor ends once the
                    // handler returns. Closes only if still empty; look again.
                    let idle_due = a.idle.map(|i| *lock(&a.last_active) + i);
                    if idle_due.is_some_and(|d| now >= d) {
                        a.mailbox.close_if_empty();
                        continue;
                    }
                    if deadline.is_some_and(|d| now >= d) { return Ok(Ok(Value::Nil)); }
                    let mut timer_set = timer_set;
                    // Neither changes while this wait lasts: last_active
                    // moves only when a message is taken, which ends it.
                    let wake = match (deadline, idle_due) {
                        (Some(a), Some(b)) => Some(a.min(b)),
                        (d, i) => d.or(i),
                    };
                    if let (Some(d), false) = (wake, timer_set) {
                        if let Some(w) = waker { timer_at(d, w.clone()); }
                        timer_set = true;
                    }
                    return Err(Wait::Mailbox(a, deadline, timer_set));
                }
            }
        },
    }
}

/// Wait on this thread until `w` is there. On a worker, other actors run meanwhile.
fn wait_here(w: Wait, session: &mut Session) -> Result<Value, GoblinError> {
    let me = current_actor();
    let mut pending = Some(w);
    block_on(None, |waker| {
        // A wait inside a callback cannot be saved; once the actor is
        // cancelled it fails instead, and the handler ends as cancelled.
        if me.as_ref().is_some_and(|a| a.cancelled.load(Ordering::SeqCst)) {
            return Some(Err(err(CANCELLED_MIDWAY)));
        }
        match poll_wait(pending.take().unwrap(), Some(waker), session) {
            Ok(r) => Some(r),
            Err(w) => { pending = Some(w); None }
        }
    }).unwrap()
}

/// A waiting builtin called from an actor's handler where the VM can stop
/// it part-way: the value now, or what it waits for (with the actor
/// registered to be woken).
pub fn start_wait(id: BuiltinId, args: Vec<Value>, session: &mut Session) -> Result<Started, GoblinError> {
    let w = match id {
        BuiltinId::Await => await_wait(&args)?,
        BuiltinId::Receive => receive_wait(&args)?,
        BuiltinId::Post => match post_step(args, session)? {
            PostStep::Done(v) => return Ok(Started::Now(v)),
            PostStep::Wait(w) => w,
        },
        _ => return Err(err("internal: not a waiting builtin")),
    };
    let me = current_actor().map(Waker::Actor);
    match poll_wait(w, me.as_ref(), session) {
        Ok(r) => Ok(Started::Now(r?)),
        Err(w) => Ok(Started::Later(w)),
    }
}

fn channel_closed() -> GoblinError {
    err("C0117: channel-closed: the channel is closed and takes no more values")
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

enum PostStep { Done(Value), Wait(Wait) }

fn post_step(args: Vec<Value>, session: &mut Session) -> Result<PostStep, GoblinError> {
    expect_args(&args, 2, "post")?;
    let h = handle_arg(&args[0], ":post")?.clone();
    let msg = &args[1];
    match h {
        Handle::Actor(a) => {
            let p = ToPortable::new(session).value(msg)?;
            match a.mailbox.try_push(Msg::Post(p)) {
                Push::Done => Ok(PostStep::Done(Value::Bool(true))),
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
            Ok(PostStep::Wait(Wait::Send(c, p)))
        }
        Handle::Future(_) => Err(err("C0118: not-a-handle: :post expects an actor or a channel, got a future")),
    }
}

/// `:post(handle, message)`.
pub fn post(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    match post_step(args, session)? {
        PostStep::Done(v) => Ok(v),
        PostStep::Wait(w) => wait_here(w, session),
    }
}

/// `:request(handle, message)`: post, and get a future for the answer.
pub fn request(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    expect_args(&args, 2, "request")?;
    let Handle::Actor(a) = handle_arg(&args[0], ":request")?.clone() else {
        return Err(err("C0118: not-a-handle: :request expects an actor"));
    };
    let p = ToPortable::new(session).value(&args[1])?;
    let reply = Arc::new(Reply {
        id: next_id(),
        slot: Mutex::new(ReplySlot { answer: None, waiters: Vec::new() }),
        actor: a.id,
    });
    match a.mailbox.try_push(Msg::Request(p, reply.clone())) {
        Push::Done => Ok(Value::Handle(Handle::Future(reply))),
        Push::Full(_) => Err(err(format!(
            "C0114: actor-mailbox-full: the actor's mailbox is full ({} messages); it is not keeping up", a.mailbox.cap))),
        Push::Closed(_) => Err(a.stopped_error()),
    }
}

fn await_wait(args: &[Value]) -> Result<Wait, GoblinError> {
    expect_args(args, 1, "await")?;
    let me = current_actor_id();
    match handle_arg(&args[0], ":await")?.clone() {
        Handle::Future(r) => {
            if me != 0 && r.actor == me && !r.answered() {
                return Err(err("C0119: actor-self-wait: an actor cannot wait for its own answer; it would wait forever"));
            }
            Ok(Wait::Future(r))
        }
        Handle::Actor(a) => {
            if me == a.id {
                return Err(err("C0119: actor-self-wait: an actor cannot wait for itself to end; it would wait forever"));
            }
            Ok(Wait::End(a))
        }
        Handle::Channel(_) => Err(err("C0118: not-a-handle: :await expects a future or an actor, got a channel; use :receive")),
    }
}

/// `:await(future)` or `:await(actor)`.
pub fn await_(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    let w = await_wait(&args)?;
    wait_here(w, session)
}

fn wait_limit(v: &Value) -> Result<Duration, GoblinError> {
    match v {
        Value::Duration(d) if d.ns >= 0 => Ok(Duration::from_nanos(d.ns.min(u64::MAX as i128) as u64)),
        other => Err(err(format!(
            "C0107: actor-args: the wait limit for :receive must be a duration like 20s, got {}",
            crate::builtins::value_to_str(other)))),
    }
}

fn receive_wait(args: &[Value]) -> Result<Wait, GoblinError> {
    // `:receive()` / `:receive(limit)`: the next message in this actor's own
    // mailbox. A request taken this way is answered with the state the
    // handler returns.
    if args.is_empty() || matches!(args[0], Value::Duration(_)) {
        if args.len() > 1 {
            return Err(err(format!("C0107: actor-args: :receive(limit) takes one wait limit, got {} arguments", args.len())));
        }
        let Some(me) = current_actor() else {
            return Err(err(
                "C0120: receive-outside-actor: only an actor's handler has a mailbox to :receive() from; outside one, give :receive a channel"));
        };
        let limit = args.first().map(wait_limit).transpose()?;
        return Ok(Wait::Mailbox(me, limit.map(|d| Instant::now() + d), false));
    }
    if args.len() > 2 {
        return Err(err(format!("C0107: actor-args: :receive takes a channel and an optional wait limit, got {} arguments", args.len())));
    }
    // `:receive(ch, 20s)` gives up after 20s and returns nil.
    let limit = args.get(1).map(wait_limit).transpose()?;
    match handle_arg(&args[0], ":receive")?.clone() {
        Handle::Channel(c) => Ok(Wait::Recv(c, limit.map(|d| Instant::now() + d), false)),
        other => Err(err(format!("C0118: not-a-handle: :receive expects a channel, got {}", other.kind()))),
    }
}

/// `:receive(channel)`: the next value, or nil once the channel is closed and empty.
pub fn receive(args: Vec<Value>, session: &mut Session) -> Result<Value, GoblinError> {
    let w = receive_wait(&args)?;
    wait_here(w, session)
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
            a.cancel();
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
        assert!(matches!(q.pop_within(None), Pop::Item(1)));
        assert!(t.join().unwrap());
        assert!(matches!(q.pop_within(None), Pop::Item(2)));
        assert!(matches!(q.pop_within(None), Pop::Item(3)));
    }

    #[test]
    fn closed_queue_drains_then_ends() {
        let q = Queue::new(4);
        let _ = q.try_push("a");
        q.close();
        assert!(matches!(q.try_push("b"), Push::Closed("b")));
        assert!(matches!(q.pop_within(None), Pop::Item("a")));
        assert!(matches!(q.pop_within(None), Pop::Closed));
    }

    #[test]
    fn wait_limit_gives_up_and_leaves_the_queue_open() {
        let q: Queue<i32> = Queue::new(4);
        let t = Instant::now();
        assert!(matches!(q.pop_within(Some(Duration::from_millis(30))), Pop::Empty));
        assert!(t.elapsed() >= Duration::from_millis(30));
        assert!(matches!(q.try_push(1), Push::Done));
    }

    #[test]
    fn idle_close_never_loses_an_accepted_item() {
        // Idle closes racing with pushes: every push that was accepted is
        // taken, and a push after the close is refused.
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
                match q.pop_or_register(None) {
                    Pop::Item(_) => taken += 1,
                    Pop::Closed => break,
                    Pop::Empty => { if q.close_if_empty() { break; } }
                }
            }
            let accepted = producer.join().unwrap();
            taken += q.drain().len();
            assert_eq!(taken, accepted);
        }
    }

    #[test]
    fn a_waiter_is_woken_once_per_registration() {
        // Registering twice keeps one entry; a push wakes it.
        let q: Queue<i32> = Queue::new(1);
        let p = Arc::new(Parker::new());
        let w = Waker::Thread(p.clone());
        assert!(matches!(q.pop_or_register(Some(&w)), Pop::Empty));
        assert!(matches!(q.pop_or_register(Some(&w)), Pop::Empty));
        assert_eq!(lock(&q.state).takers.len(), 1);
        assert!(matches!(q.try_push(5), Push::Done));
        assert_eq!(lock(&q.state).takers.len(), 0);
        p.park(Some(Instant::now() + Duration::from_secs(5)));
        assert!(matches!(q.pop_or_register(None), Pop::Item(5)));
    }

    #[test]
    fn timers_fire_in_order() {
        let p = Arc::new(Parker::new());
        let t = Instant::now();
        timer_at(t + Duration::from_millis(40), Waker::Thread(p.clone()));
        p.park(Some(t + Duration::from_secs(5)));
        assert!(t.elapsed() >= Duration::from_millis(40));
        assert!(t.elapsed() < Duration::from_secs(5));
    }
}
