use std::fmt;

/// A single Y’all error with context (file + line + message).
#[derive(Debug, Clone)]
pub struct YallError {
    pub label: String,   // filename or logical "inline-label"
    pub line: usize,     // line number (1-based)
    pub message: String, // friendly error message
}

impl YallError {
    pub fn new<L: Into<String>, M: Into<String>>(label: L, line: usize, message: M) -> Self {
        Self {
            label: label.into(),
            line,
            message: message.into(),
        }
    }

    /// Convenience: "internal error" where line = 0.
    pub fn internal<M: Into<String>>(label: &str, msg: M) -> Self {
        Self {
            label: label.into(),
            line: 0,
            message: msg.into(),
        }
    }
}

impl fmt::Display for YallError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line == 0 {
            // Used for non-line-specific errors
            write!(f, "Y'all: {}: {}", self.label, self.message)
        } else {
            write!(
                f,
                "Y'all: {}, line {}: {}",
                self.label, self.line, self.message
            )
        }
    }
}

impl std::error::Error for YallError {}
