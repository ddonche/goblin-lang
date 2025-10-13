//! Goblin Diagnostics (v1)
//! Matches docs/diagnostics.md

use std::fmt;
pub const DEFAULT_ERROR_DOCS_BASE: &str = "https://goblinlang.org/docs/errors";

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
    /// Stable slug, e.g. "bad-escape"
    pub category: &'static str,
    pub severity: Severity,
    /// One-line summary, no trailing period
    pub message: String,
    pub primary_span: Span,
    pub secondary_spans: Vec<LabeledSpan>,
    pub notes: Vec<String>,
    pub help: Vec<String>,
    pub link: Option<String>,

    /// NEW: machine code, e.g. "R0301", "P0904", etc.
    pub code: &'static str,
    /// Optional pretty-print snippet line
    pub source_line: Option<String>,
    /// 1-based columns for highlight (inclusive range; renderer may treat end as inclusive)
    pub highlight_start: Option<u32>,
    pub highlight_end: Option<u32>,
}

impl Diagnostic {
    /// Backward-compatible constructor (no code/snippet).
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
            link: None,
            code: "UNKNOWN",
            source_line: None,
            highlight_start: None,
            highlight_end: None,
        }
    }

    /// Constructor that includes an error code.
    pub fn new_with_code(
        severity: Severity,
        code: &'static str,
        category: &'static str,
        message: impl Into<String>,
        primary_span: Span,
    ) -> Self {
        let mut d = Self::new(severity, category, message, primary_span);
        d.code = code;
        d
    }

    pub fn error(category: &'static str, message: impl Into<String>, primary_span: Span) -> Self {
        Self::new(Severity::Error, category, message, primary_span)
    }

    pub fn warning(category: &'static str, message: impl Into<String>, primary_span: Span) -> Self {
        Self::new(Severity::Warning, category, message, primary_span)
    }

    /// Convenience for runtime errors with code.
    pub fn runtime(
        code: &'static str,
        category: &'static str,
        message: impl Into<String>,
        primary_span: Span,
    ) -> Self {
        Self::new_with_code(Severity::Error, code, category, message, primary_span)
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

    pub fn with_link(mut self, url: impl Into<String>) -> Self {
        self.link = Some(url.into());
        self
    }

    /// Set/override the code later if needed.
    pub fn with_code(mut self, code: &'static str) -> Self {
        self.code = code;
        self
    }

    /// Attach a source snippet for pretty printing.
    pub fn with_snippet(
        mut self,
        line: impl Into<String>,
        start_col_1based: u32,
        end_col_1based: u32,
    ) -> Self {
        self.source_line = Some(line.into());
        self.highlight_start = Some(start_col_1based);
        self.highlight_end = Some(end_col_1based);
        self
    }

    /// Build a docs link like: https://goblinlang.org/docs/errors#R0301
    pub fn docs_link(&self, base: &str) -> String {
        if self.code == "UNKNOWN" {
            base.to_string()
        } else {
            format!("{base}#{}", self.code)
        }
    }
}

/// Minimal pretty printer that follows the guide's first line format.
impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // First line
        if self.code == "UNKNOWN" {
            writeln!(f, "{}: {}: {}", self.severity, self.category, self.message)?;
        } else {
            writeln!(
                f,
                "{}: {}: {}: {}",
                self.severity, self.code, self.category, self.message
            )?;
        }

        // Location header
        writeln!(
            f,
            "  ┌─ {}:{}:{}",
            self.primary_span.file, self.primary_span.line_start, self.primary_span.col_start
        )?;

        // === Fallback: load source line from disk if no snippet was attached ===
        let mut line_for_print: Option<String> = self.source_line.clone();
        if line_for_print.is_none() {
            if let Ok(src) = std::fs::read_to_string(&self.primary_span.file) {
                let idx = self.primary_span.line_start.saturating_sub(1) as usize;
                if let Some(line) = src.lines().nth(idx) {
                    line_for_print = Some(line.to_string());
                }
            }
        }

        if let Some(ref line) = line_for_print {
            // Try to derive start/end columns from byte offsets relative to this line.
            // This fixes cases where col_start/col_end were set to global offsets.
            let (s_col, e_col) = {
                // Prefer explicit highlight range if provided
                if let (Some(s), Some(e)) = (self.highlight_start, self.highlight_end) {
                    (s, e)
                } else if let Ok(src_again) = std::fs::read_to_string(&self.primary_span.file) {
                    // Find the byte offset where this line begins
                    let mut cursor: usize = 0;
                    let target = self.primary_span.line_start.saturating_sub(1) as usize;
                    let mut line_start_byte: Option<usize> = None;

                    for (i, ln) in src_again.split_inclusive('\n').enumerate() {
                        if i == target {
                            line_start_byte = Some(cursor);
                            break;
                        }
                        cursor += ln.as_bytes().len();
                    }

                    if let Some(ls) = line_start_byte {
                        // Clamp the span to the current line and convert to 1-based columns
                        let start = self.primary_span.start.max(ls);
                        let end   = self.primary_span.end.max(start);
                        let col_s = (start - ls) as u32 + 1;
                        let col_e = (end.saturating_sub(1).saturating_sub(ls)) as u32 + 1;
                        (col_s, col_e)
                    } else {
                        (self.primary_span.col_start, self.primary_span.col_end)
                    }
                } else {
                    (self.primary_span.col_start, self.primary_span.col_end)
                }
            };

            // Print the source line
            writeln!(f, "{:>4} | {}", self.primary_span.line_start, line)?;

            // Draw carets with tab expansion so alignment is correct (tab stop = 4)
            fn visual_width_up_to(s: &str, tabw: usize) -> usize {
                let mut w = 0usize;
                for ch in s.chars() {
                    if ch == '\t' {
                        let next = ((w / tabw) + 1) * tabw;
                        w = next;
                    } else {
                        w += 1; // treat other chars as width 1
                    }
                }
                w
            }

            // Compute prefix (chars before the highlight start, 1-based columns)
            let start_idx = s_col.saturating_sub(1) as usize;
            let end_idx   = if e_col >= s_col { (e_col - 1) as usize } else { start_idx };

            let mut it = line.chars();
            let prefix: String = it.by_ref().take(start_idx).collect();

            let pad_spaces = visual_width_up_to(&prefix, 4);
            let pad = " ".repeat(pad_spaces);

            let caret_len = end_idx.saturating_sub(start_idx).saturating_add(1).max(1);
            let carets = "^".repeat(caret_len);

            writeln!(f, "     | {}{}", pad, carets)?;
        }

        // Secondary spans
        for sec in &self.secondary_spans {
            let where_ = format!(
                "{}:{}:{}",
                sec.span.file, sec.span.line_start, sec.span.col_start
            );
            if let Some(lbl) = &sec.label {
                writeln!(f, "  = note: {} → {}", where_, lbl)?;
            } else {
                writeln!(f, "  = note: {}", where_)?;
            }
        }

        // Help & notes
        for h in &self.help {
            writeln!(f, "  = help: {}", h)?;
        }
        for n in &self.notes {
            writeln!(f, "  = note: {}", n)?;
        }

        // Docs link (only if we have a real code)
        if self.code != "UNKNOWN" {
            writeln!(f, "  = link: {}", self.docs_link(DEFAULT_ERROR_DOCS_BASE))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_and_format_basic() {
        let d = Diagnostic::error(
            "bad-escape",
            "invalid escape sequence \\q",
            Span::new("foo.gbln", 10, 11, 1, 12, 1, 13),
        )
        .with_help("use \\n, \\t, \\u{...}, or remove the backslash");
        let s = format!("{}", d);
        assert!(s.contains("error: bad-escape: invalid escape sequence \\q"));
        assert!(s.contains("┌─ foo.gbln:1:12"));
    }

    #[test]
    fn build_and_format_with_code_and_snippet() {
        let d = Diagnostic::runtime(
            "R0301",
            "wrong-arity",
            "Wrong number of arguments (expected 1, got 3)",
            Span::new("api/all_tests.gbln", 0, 0, 41, 17, 41, 23),
        )
        .with_snippet("say(1, 2, 3)", 1, 12)
        .with_help("‘say’ takes a single value: say(value)");

        let s = format!("{}", d);
        assert!(s.contains("error: R0301: wrong-arity: Wrong number of arguments"));
        assert!(s.contains("┌─ api/all_tests.gbln:41:17"));
        assert!(s.contains("say(1, 2, 3)"));
        assert!(s.contains("^"));
    }

    #[test]
    fn docs_link_builder() {
        let d = Diagnostic::runtime(
            "R0301",
            "wrong-arity",
            "Wrong number of arguments",
            Span::new("x", 0, 0, 1, 1, 1, 1),
        );
        assert_eq!(
            d.docs_link("https://goblinlang.org/docs/errors"),
            "https://goblinlang.org/docs/errors#R0301"
        );
    }
}