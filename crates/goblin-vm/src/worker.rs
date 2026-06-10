/// Swarm concurrency: isolated workers with message-passing.
///
/// Each worker has its own Session (heap) and Vm. Workers communicate only
/// through messages — no shared mutable memory.
///
/// v1 threading note: Goblin's Value type uses Rc<> for internal sharing,
/// which is not Send. Workers therefore run in the same OS thread but on an
/// isolated heap, communicating via a channel of Send-safe TransferValues.
/// True multi-thread workers (using Arc<> throughout) are a planned upgrade.
use std::sync::mpsc;

use crate::error::GoblinError;
use crate::session::{GcMode, Session};
use crate::value::{FunctionObject, Value};
use crate::vm::Vm;

// ── TransferValue ─────────────────────────────────────────────────────────────

/// A Send-safe representation of a Goblin value for cross-worker transfer.
/// All Rc<> internals are replaced with owned data.
#[derive(Debug, Clone)]
pub enum TransferValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Array(Vec<TransferValue>),
    Map(Vec<(TransferValue, TransferValue)>),
    // Functions and closures cannot currently be transferred between workers.
}

impl TransferValue {
    /// Convert a VM Value into a transferable form (deep copy, no Rc).
    pub fn from_value(v: &Value) -> Result<TransferValue, GoblinError> {
        match v {
            Value::Nil        => Ok(TransferValue::Nil),
            Value::Bool(b)    => Ok(TransferValue::Bool(*b)),
            Value::Int(n)     => Ok(TransferValue::Int(*n)),
            Value::Float(f)   => Ok(TransferValue::Float(*f)),
            Value::Str(s)     => Ok(TransferValue::Str(s.clone())),
            Value::Collection(c) => {
                use crate::collections;
                if matches!(&c.layout, crate::value::CollectionLayout::SmallMap(_)
                    | crate::value::CollectionLayout::HashMapBackend(_))
                {
                    let pairs = collections::to_pairs(c);
                    let mut tv_pairs = Vec::with_capacity(pairs.len());
                    for (k, v) in &pairs {
                        tv_pairs.push((
                            TransferValue::from_value(k)?,
                            TransferValue::from_value(v)?,
                        ));
                    }
                    Ok(TransferValue::Map(tv_pairs))
                } else {
                    let items = collections::to_vec(c);
                    let mut tv_items = Vec::with_capacity(items.len());
                    for item in &items {
                        tv_items.push(TransferValue::from_value(item)?);
                    }
                    Ok(TransferValue::Array(tv_items))
                }
            }
            Value::Function(_) | Value::Closure(_) | Value::Builtin(_) => {
                Err(GoblinError::NotImplemented {
                    feature: "functions/closures cannot be transferred between workers in v1"
                })
            }
        }
    }

    /// Materialise a TransferValue into a live Value in the given session.
    pub fn into_value(self, session: &mut Session) -> Value {
        match self {
            TransferValue::Nil        => Value::Nil,
            TransferValue::Bool(b)    => Value::Bool(b),
            TransferValue::Int(n)     => Value::Int(n),
            TransferValue::Float(f)   => Value::Float(f),
            TransferValue::Str(s)     => Value::Str(s),
            TransferValue::Array(items) => {
                let vals: Vec<Value> = items.into_iter()
                    .map(|tv| tv.into_value(session))
                    .collect();
                Value::Collection(std::rc::Rc::new(crate::value::CollectionValue::from_flat(vals)))
            }
            TransferValue::Map(pairs) => {
                let kv: Vec<(Value, Value)> = pairs.into_iter()
                    .map(|(k, v)| (k.into_value(session), v.into_value(session)))
                    .collect();
                Value::Collection(std::rc::Rc::new(crate::value::CollectionValue::from_map(kv)))
            }
        }
    }
}

// ── Message ───────────────────────────────────────────────────────────────────

/// A message sent between workers.
#[derive(Debug)]
pub enum Message {
    Value(TransferValue),
    Stop,
    Error(String),
}

// ── Worker ────────────────────────────────────────────────────────────────────

/// An isolated execution context: its own Session and Vm.
///
/// v1: Workers are executed by calling `run()` on the calling thread.
/// Future: Workers will be scheduled on an async executor or separate thread
/// once Value is updated to use Arc<> instead of Rc<>.
pub struct Worker {
    pub id: usize,
    vm: Vm,
    /// Inbox for messages sent to this worker.
    pub inbox: mpsc::Receiver<Message>,
    /// Sender end that this worker uses to reply to its spawner.
    pub outbox: mpsc::Sender<Message>,
}

impl Worker {
    fn new(
        id: usize,
        inbox: mpsc::Receiver<Message>,
        outbox: mpsc::Sender<Message>,
    ) -> Self {
        let session = Session::new(GcMode::Auto).with_worker_id(id);
        Worker { id, vm: Vm::new(session), inbox, outbox }
    }

    /// Execute a function and send the result back to the spawner.
    pub fn run(&mut self, func: FunctionObject) {
        match self.vm.execute(func) {
            Ok(result) => {
                match TransferValue::from_value(&result) {
                    Ok(tv) => { let _ = self.outbox.send(Message::Value(tv)); }
                    Err(e) => { let _ = self.outbox.send(Message::Error(e.to_string())); }
                }
            }
            Err(e) => {
                let _ = self.outbox.send(Message::Error(e.to_string()));
            }
        }
    }

    /// Receive a message sent to this worker (blocking).
    pub fn recv_message(&self) -> Option<Message> {
        self.inbox.recv().ok()
    }

    /// Try to receive without blocking.
    pub fn try_recv_message(&self) -> Option<Message> {
        self.inbox.try_recv().ok()
    }
}

// ── WorkerHandle ─────────────────────────────────────────────────────────────

/// A handle to a worker, held by the spawner.
/// In v1, the spawner calls `execute_sync` to run the worker's function.
pub struct WorkerHandle {
    pub id: usize,
    /// Send messages into the worker's inbox.
    sender: mpsc::Sender<Message>,
    /// Receive results from the worker.
    receiver: mpsc::Receiver<Message>,
    /// The worker itself (present until `execute_sync` is called).
    worker: Option<Worker>,
}

impl WorkerHandle {
    /// Run the worker's function synchronously on the calling thread.
    /// This consumes the worker and sends the result to the receiver.
    pub fn execute_sync(&mut self, func: FunctionObject) {
        if let Some(mut w) = self.worker.take() {
            w.run(func);
        }
    }

    /// Send a value to this worker (deep-copy semantics).
    pub fn send_value(&self, v: &Value) -> Result<(), GoblinError> {
        let tv = TransferValue::from_value(v)?;
        self.sender.send(Message::Value(tv))
            .map_err(|e| GoblinError::Runtime(format!("send failed: {}", e)))
    }

    /// Send a stop signal.
    pub fn stop(&self) {
        let _ = self.sender.send(Message::Stop);
    }

    /// Receive the next message from this worker (blocking).
    pub fn recv(&self) -> Result<Message, GoblinError> {
        self.receiver.recv()
            .map_err(|e| GoblinError::Runtime(format!("recv failed: {}", e)))
    }

    /// Try to receive without blocking.
    pub fn try_recv(&self) -> Option<Message> {
        self.receiver.try_recv().ok()
    }
}

// ── Worker spawn ──────────────────────────────────────────────────────────────

static NEXT_WORKER_ID: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(1);

/// Create a new worker. Call `handle.execute_sync(func)` to run it.
pub fn spawn_worker() -> WorkerHandle {
    let id = NEXT_WORKER_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // spawner→worker channel (for sending messages to the worker)
    let (spawner_tx, worker_rx) = mpsc::channel::<Message>();
    // worker→spawner channel (for results)
    let (worker_tx, spawner_rx) = mpsc::channel::<Message>();

    let worker = Worker::new(id, worker_rx, worker_tx);

    WorkerHandle {
        id,
        sender: spawner_tx,
        receiver: spawner_rx,
        worker: Some(worker),
    }
}

// ── WorkerPool ────────────────────────────────────────────────────────────────

/// A pool of worker handles.
pub struct WorkerPool {
    workers: Vec<WorkerHandle>,
}

impl WorkerPool {
    pub fn new() -> Self {
        WorkerPool { workers: Vec::new() }
    }

    /// Spawn a worker, run `func` synchronously, return the worker ID.
    pub fn run_sync(&mut self, func: FunctionObject) -> usize {
        let mut handle = spawn_worker();
        let id = handle.id;
        handle.execute_sync(func);
        self.workers.push(handle);
        id
    }

    pub fn get(&self, id: usize) -> Option<&WorkerHandle> {
        self.workers.iter().find(|w| w.id == id)
    }

    pub fn stop_all(&self) {
        for w in &self.workers {
            w.stop();
        }
    }
}

impl Default for WorkerPool {
    fn default() -> Self { WorkerPool::new() }
}
