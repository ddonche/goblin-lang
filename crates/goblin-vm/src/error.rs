use std::fmt;

/// Runtime and compile-time errors produced by the Goblin VM.
#[derive(Debug, Clone)]
pub enum GoblinError {
    /// A stash address was used after the stash was garbage collected.
    StaleAddress { slot: u32, stored_gen: u32, addr_gen: u32 },

    /// A tether was used but points to an empty arena slot.
    EmptySlot { slot: u32 },

    /// overwrite! was attempted on a stash owned by a different worker.
    CrossWorkerMutation { stash_worker: usize, current_worker: usize },

    /// A stash from one worker was accessed from another.
    CrossWorkerAccess { stash_worker: usize, current_worker: usize },

    /// Type mismatch during an operation.
    TypeError { expected: &'static str, got: &'static str, op: &'static str },

    /// Division or modulo by zero.
    DivisionByZero,

    /// Index out of bounds on a collection.
    IndexOutOfBounds { index: i64, len: usize },

    /// Key not found in a map.
    KeyNotFound,

    /// A call was made with the wrong number of arguments.
    ArityMismatch { expected: usize, got: usize, name: String },

    /// A value that isn't callable was called.
    NotCallable { got: &'static str },

    /// Stack overflow (call depth exceeded).
    StackOverflow,

    /// An undefined variable was referenced.
    UndefinedVariable { name: String },

    /// Compile-time error.
    CompileError { message: String, span_debug: String },

    /// Feature not yet implemented in the VM.
    NotImplemented { feature: &'static str },

    /// A generic runtime error with a message.
    Runtime(String),

    /// An error annotated with a source location.
    WithLocation { inner: Box<GoblinError>, line: u32, file: String },
}

impl GoblinError {
    pub fn type_error(expected: &'static str, got: &'static str, op: &'static str) -> Self {
        GoblinError::TypeError { expected, got, op }
    }
}

impl fmt::Display for GoblinError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GoblinError::StaleAddress { slot, stored_gen, addr_gen } =>
                write!(f, "stale address: slot {slot} has generation {stored_gen}, but address has generation {addr_gen}"),
            GoblinError::EmptySlot { slot } =>
                write!(f, "empty slot: no stash at slot {slot}"),
            GoblinError::CrossWorkerMutation { stash_worker, current_worker } =>
                write!(f, "cross-worker mutation: stash belongs to worker {stash_worker}, current worker is {current_worker}"),
            GoblinError::CrossWorkerAccess { stash_worker, current_worker } =>
                write!(f, "cross-worker access: stash belongs to worker {stash_worker}, current worker is {current_worker}"),
            GoblinError::TypeError { expected, got, op } =>
                write!(f, "type error in {op}: expected {expected}, got {got}"),
            GoblinError::DivisionByZero =>
                write!(f, "division by zero"),
            GoblinError::IndexOutOfBounds { index, len } =>
                write!(f, "index out of bounds: index {index} on collection of length {len}"),
            GoblinError::KeyNotFound =>
                write!(f, "key not found"),
            GoblinError::ArityMismatch { expected, got, name } =>
                write!(f, "arity mismatch calling '{name}': expected {expected} args, got {got}"),
            GoblinError::NotCallable { got } =>
                write!(f, "value of type '{got}' is not callable"),
            GoblinError::StackOverflow =>
                write!(f, "stack overflow: call depth exceeded"),
            GoblinError::UndefinedVariable { name } =>
                write!(f, "undefined variable: '{name}'"),
            GoblinError::CompileError { message, span_debug } =>
                write!(f, "compile error at {span_debug}: {message}"),
            GoblinError::NotImplemented { feature } =>
                write!(f, "not yet implemented: {feature}"),
            GoblinError::Runtime(msg) =>
                write!(f, "runtime error: {msg}"),
            GoblinError::WithLocation { inner, line, file } => {
                if file.is_empty() {
                    write!(f, "[line {line}] {inner}")
                } else {
                    // Show only the filename portion, not the full path
                    let name = std::path::Path::new(file)
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or(file.as_str());
                    write!(f, "[{name}:{line}] {inner}")
                }
            }
        }
    }
}

impl std::error::Error for GoblinError {}
