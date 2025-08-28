//! Goblin Diagnostics (v1)
//! Matches docs/diagnostics.md

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Note => write!(f, "note"),
            Severity::Help => write!(f, "help"),
        }
    }
}

/// Source span using both byte offsets and 1-based line/col for display.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize, // half-open
    pub line_start: u32,
    pub col_start: u32,
    pub line_end: u32,
    pub col_end: u32,
}

impl Span {
    pub fn new(
        file: impl Into<String>,
        start: usize,
        end: usize,
        line_start: u32,
        col_start: u32,
        line_end: u32,
        col_end: u32,
    ) -> Self {
        Self {
            file: file.into(),
            start,
            end,
            line_start,
            col_start,
            line_end,
            col_end,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LabeledSpan {
    pub span: Span,
    pub label: Option<String>,
}

impl LabeledSpan {
    pub fn new(span: Span) -> Self {
        Self { span, label: None }
    }
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub category: &'static str, // stable slug, e.g. "bad-escape"
    pub severity: Severity,
    pub message: String, // one-line summary, no trailing period
    pub primary_span: Span,
    pub secondary_spans: Vec<LabeledSpan>,
    pub notes: Vec<String>,
    pub help: Vec<String>,
}

impl Diagnostic {
    pub fn new(
        severity: Severity,
        category: &'static str,
        message: impl Into<String>,
        primary_span: Span,
    ) -> Self {
        Self {
            category,
            severity,
            message: message.into(),
            primary_span,
            secondary_spans: Vec::new(),
            notes: Vec::new(),
            help: Vec::new(),
        }
    }

    pub fn error(category: &'static str, message: impl Into<String>, primary_span: Span) -> Self {
        Self::new(Severity::Error, category, message, primary_span)
    }

    pub fn warning(category: &'static str, message: impl Into<String>, primary_span: Span) -> Self {
        Self::new(Severity::Warning, category, message, primary_span)
    }

    pub fn with_secondary(mut self, sec: LabeledSpan) -> Self {
        self.secondary_spans.push(sec);
        self
    }
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help.push(help.into());
        self
    }
}

/// Minimal pretty printer that follows the guide's first line format.
impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}: {}: {}", self.severity, self.category, self.message)?;
        writeln!(
            f,
            "  ┌─ {}:{}:{}",
            self.primary_span.file, self.primary_span.line_start, self.primary_span.col_start
        )?;
        // We don't render the source line here (needs a source map); leave placeholders.
        // Downstream crates (CLI) can enrich with file content.
        for h in &self.help {
            writeln!(f, "  = help: {}", h)?;
        }
        for n in &self.notes {
            writeln!(f, "  = note: {}", n)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_and_format() {
        let d = Diagnostic::error(
            "bad-escape",
            "invalid escape sequence \\q",
            Span::new("foo.gbln", 10, 11, 1, 12, 1, 13),
        )
        .with_help("use \\n, \\t, \\u{...}, or remove the backslash");
        let s = format!("{}", d);
        assert!(s.contains("error: bad-escape: invalid escape sequence \\q"));
    }
}
