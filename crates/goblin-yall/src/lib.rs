//! Y'all — Goblin’s strict YAML-lite syntax + parser.
//!
//! This crate does ALL the Y’all work:
//!   - normalize line endings
//!   - strip comments (respect quotes)
//!   - enforce Y’all syntax rules
//!   - parse into serde_yaml::Value
//!
//! The interpreter just calls `yall_parse` / `yall_parse_file` and
//! converts serde_yaml::Value into Goblin `Value`.

use std::fmt;
use std::fs;

use serde_yaml;

/// A single Y’all error with context.
#[derive(Debug, Clone)]
pub struct YallError {
    pub label: String,
    pub line: usize,
    pub message: String,
}

impl YallError {
    pub fn new<L: Into<String>, M: Into<String>>(label: L, line: usize, message: M) -> Self {
        Self {
            label: label.into(),
            line,
            message: message.into(),
        }
    }
}

impl fmt::Display for YallError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Y'all: {}, line {}: {}",
            self.label, self.line, self.message
        )
    }
}

impl std::error::Error for YallError {}

/// Normalize line endings and trim trailing spaces.
pub fn yall_normalize(text: &str) -> String {
    let normalized = text.replace("\r\n", "\n").replace('\r', "\n");
    normalized
        .lines()
        .map(|line| line.trim_end().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Count leading spaces (indent).
fn count_leading_spaces(line: &str) -> usize {
    line.chars().take_while(|c| *c == ' ').count()
}

/// True if blank or pure comment.
fn is_ignorable_line(line: &str) -> bool {
    let t = line.trim();
    t.is_empty() || t.starts_with('#')
}

/// Crude URL detector so we don’t complain about `http://`.
fn looks_like_url(line: &str) -> bool {
    line.contains("://")
}

/// Strip inline comments while respecting double-quoted strings.
pub fn yall_strip_comments(text: &str) -> String {
    let mut out_lines = Vec::new();

    for line in text.lines() {
        let mut buf = String::new();
        let mut in_string = false;
        let mut escape = false;

        for ch in line.chars() {
            if escape {
                buf.push(ch);
                escape = false;
                continue;
            }

            match ch {
                '\\' if in_string => {
                    escape = true;
                    buf.push(ch);
                }
                '"' => {
                    in_string = !in_string;
                    buf.push(ch);
                }
                '#' if !in_string => {
                    // start of comment
                    break;
                }
                _ => buf.push(ch),
            }
        }

        out_lines.push(buf.trim_end().to_string());
    }

    out_lines.join("\n")
}

/// Validate Y’all text:
/// - no tabs
/// - indent = multiple of 2 spaces
/// - indent cannot jump more than one level (+2 spaces) at a time
/// - colon must be followed by space for inline values,
///   but allow pure keys like `key:` for nested maps.
pub fn yall_validate(text: &str, label: &str) -> Result<(), YallError> {
    let mut last_indent = 0usize;

    for (idx, line) in text.lines().enumerate() {
        let line_no = idx + 1;

        if is_ignorable_line(line) {
            continue;
        }

        // Tabs are forbidden.
        if line.contains('\t') {
            return Err(YallError::new(
                label,
                line_no,
                "tabs are not allowed; use spaces only",
            ));
        }

        let indent = count_leading_spaces(line);

        if indent % 2 != 0 {
            return Err(YallError::new(
                label,
                line_no,
                "indent must be a multiple of 2 spaces",
            ));
        }

        if indent > last_indent && indent - last_indent > 2 {
            return Err(YallError::new(
                label,
                line_no,
                "indent jumped more than one nesting level",
            ));
        }

        last_indent = indent;

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        if looks_like_url(trimmed) {
            continue;
        }

        // skip fully quoted lines
        if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
            continue;
        }

        if let Some(pos) = trimmed.find(':') {
            // Everything after the colon
            let rest = &trimmed[pos + 1..];

            // Allow "key:" and "key:   " (pure key with nested block)
            if rest.trim().is_empty() {
                continue;
            }

            // Otherwise, require "key: value" (space after colon)
            let after = rest.chars().next().unwrap();
            if after != ' ' {
                return Err(YallError::new(
                    label,
                    line_no,
                    "need space after colon (use 'key: value')",
                ));
            }
        }
    }

    Ok(())
}

/// Full pipeline:
///   normalize → optionally strip comments → validate → return cleaned text.
pub fn yall_enforce(text: &str, label: &str, strip_comments: bool) -> Result<String, YallError> {
    let normalized = yall_normalize(text);
    let cleaned = if strip_comments {
        yall_strip_comments(&normalized)
    } else {
        normalized
    };

    yall_validate(&cleaned, label)?;
    Ok(cleaned)
}

/// High-level: enforce Y’all, then parse with serde_yaml.
/// Returns structured data (serde_yaml::Value).
pub fn yall_parse(text: &str, label: &str) -> Result<serde_yaml::Value, YallError> {
    let cleaned = yall_enforce(text, label, true)?;
    match serde_yaml::from_str::<serde_yaml::Value>(&cleaned) {
        Ok(v) => Ok(v),
        Err(e) => Err(YallError::new(
            label,
            0,
            format!("backend YAML parse failed: {e}"),
        )),
    }
}

/// High-level: read file, enforce Y’all, parse.
pub fn yall_parse_file(path: &str) -> Result<serde_yaml::Value, YallError> {
    let text = fs::read_to_string(path).map_err(|e| {
        YallError::new(
            path,
            0,
            format!("cannot read file: {e}"),
        )
    })?;

    yall_parse(&text, path)
}
