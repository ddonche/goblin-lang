use goblin_diagnostics::{Diagnostic, Span};

/// String-returning errors (Result<T, String>)
#[inline]
pub fn s(code: &str, msg: &str) -> String {
    format!("{code}: {msg}")
}

#[inline]
pub fn s_help(code: &str, msg: &str, help: &str) -> String {
    // Keep help on the next paragraph (your CLI prints it as a second line)
    format!("{code}: {msg}\n\n{help}")
}

/// Diagnostic-returning errors (Result<T, Vec<Diagnostic>>)
#[inline]
pub fn derr(code: &'static str, msg: &str, primary: Span) -> Vec<Diagnostic> {
    // Prefix the code in the message so the CLI shows it.
    vec![Diagnostic::error(code, &format!("{code}: {msg}"), primary)]
}

#[inline]
pub fn derr_help(code: &'static str, msg: &str, help: &str, primary: Span) -> Vec<Diagnostic> {
    vec![Diagnostic::error(code, &format!("{code}: {msg}"), primary)
         .with_help(help)]
}

#[inline]
pub fn derr_expected_found(
    code: &'static str,
    expected: &str,
    found: &str,
    primary: Span,
    help: &str,
) -> Vec<Diagnostic> {
    vec![Diagnostic::error(
        code,
        &format!("{code}: expected {expected}, found {found}.\n\n{help}"),
        primary,
    )]
}