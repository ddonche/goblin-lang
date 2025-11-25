//! Y’ALL v1.0 — Strict, deterministic config language for Goblin.
//!
//! Public API:
//!   - yall_parse         → parse Y’all string into YallValue
//!   - yall_parse_file    → parse Y’all file into YallValue
//!   - yall_write         → serialize YallValue into valid Y’all text
//!   - yall_write_file    → write Y’all text to a file
//!   - yall_pretty        → pretty-print (same as write)
//!   - yall_minify        → compact inline form
//!
//! No YAML. No serde. Pure native Y’all.

mod error;
mod lexer;
mod parser;
mod value;
mod writer;

pub use error::YallError;
pub use value::YallValue;

use parser::Parser;
use std::fs;
use std::io::{self, Write};

// Writer API
pub use writer::yall_write;

/// Parse a Y’all string into a YallValue.
/// Old API compatibility preserved.
pub fn yall_parse(text: &str, label: &str) -> Result<YallValue, YallError> {
    let p = Parser::new(text, label);
    p.parse()
}

/// Read & parse a Y’all file.
pub fn yall_parse_file(path: &str) -> Result<YallValue, YallError> {
    let text = fs::read_to_string(path)
        .map_err(|e| YallError::new(path, 0, format!("cannot read file: {e}")))?;
    let p = Parser::new(&text, path);
    p.parse()
}

/// Serialize a YallValue back to Y’all text (public canonical name).
pub fn yall_stringify(value: &YallValue) -> String {
    yall_write(value)
}

/// Write Y’all to a file.
pub fn yall_write_file(path: &str, value: &YallValue) -> Result<(), YallError> {
    let text = yall_write(value);
    fs::write(path, text)
        .map_err(|e| YallError::new(path, 0, format!("write failed: {e}")))
}

/// Pretty-print: same as yall_write.
pub fn yall_pretty(value: &YallValue) -> String {
    yall_write(value)
}

/// Minify: inline map if possible, remove indentation when able.
pub fn yall_minify(value: &YallValue) -> String {
    writer::minify(value)
}
