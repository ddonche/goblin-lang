//! Goblin Lexer (proto)
//! Adds: punctuation, pipe ops, optional chain/nullish, ASCII identifiers (!/?), basic numbers (INT/FLOAT),
//! base-prefixed integers `0x 0o 0b` (underscores allowed),
//! line comments `///`, block comments `//// … ////` (emits NEWLINEs inside), bracket-aware newline suppression,
//! basic math operators `+ - * ** / % ^ ~ << >>`, their compound-assign versions `+= -= *= /= %= &= |= ^= <<= >>=`,
//! range operators `..` and `...` (longest-match),
//! string literals: `"..."` / `'...'`, triple `"""..."""` / `'''...'''` with escapes,
//! raw strings: `r"..."` / `r'...'` and triple-raw `r"""..."""` / `r'''...'''` (no escaping),
//! hex `\xNN` and Unicode `\u{...}` escapes in non-raw strings,
//! comparison operators `== != <= >= < > !`,
//! `@` / `#`-prefixed identifiers, `:` / `;` (with longest-match for `::`),
//! logical AND `&&` (and single `&`), and arrows `->` / `=>`.
//! Still a placeholder: many features are TODO; ERR reporting not wired yet.

use goblin_diagnostics::{Diagnostic, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    AtIdent,
    HashIdent,
    Int,
    Float,
    String,
    Newline,
    Indent,
    Dedent,
    /// Exact operator/punct as a string (e.g., "+", "**", "?.", "(")
    Op(String),
    Money,
    Eof,
}

impl TokenKind {
    /// Constructor alias for operator tokens.
    /// Lets us write `TokenKind::Operator("+")` while the enum variant remains `Op`.
    #[allow(non_snake_case)]
    #[inline]
    pub fn Operator<S: Into<String>>(s: S) -> Self {
        TokenKind::Op(s.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    /// Optional decoded/canonical value (for identifiers/literals)
    pub value: Option<String>,
}

#[inline]
#[allow(dead_code)] // Needed for upcoming indentation-aware lexing to skip blank/comment-only lines
fn is_blank_or_comment_line(bytes: &[u8], mut k: usize) -> bool {
    // skip horizontal whitespace
    while k < bytes.len() {
        match bytes[k] {
            b' ' | b'\t' => k += 1,
            _ => break,
        }
    }
    // blank (physical) line?
    if k >= bytes.len() || bytes[k] == b'\r' || bytes[k] == b'\n' {
        return true;
    }
    // line comment "///"
    if k + 2 < bytes.len() && bytes[k] == b'/' && bytes[k + 1] == b'/' && bytes[k + 2] == b'/' {
        return true;
    }
    // block comment opener "////"
    if k + 3 < bytes.len()
        && bytes[k] == b'/'
        && bytes[k + 1] == b'/'
        && bytes[k + 2] == b'/'
        && bytes[k + 3] == b'/'
    {
        return true;
    }
    false
}

#[inline]
fn is_block_comment_open(bytes: &[u8], i: usize) -> bool {
    i + 3 < bytes.len()
        && bytes[i] == b'/'
        && bytes[i + 1] == b'/'
        && bytes[i + 2] == b'/'
        && bytes[i + 3] == b'/'
}

/// Consumes a `//// ... ////` block comment starting at `i`,
/// emitting NEWLINE tokens for physical line breaks.
/// On success, advances `i/line/col` and returns Ok(()).
/// On EOF without a closer, returns a diagnostic anchored at the opener.
fn consume_block_comment(
    bytes: &[u8],
    file: &str,
    i: &mut usize,
    line: &mut u32,
    col: &mut u32,
    tokens: &mut Vec<Token>,
) -> Result<(), Vec<Diagnostic>> {
    let open_i = *i;
    let open_line = *line;
    let open_col = *col;

    // consume opening "////"
    *i += 4;
    *col += 4;

    loop {
        // found closer?
        if is_block_comment_open(bytes, *i) {
            *i += 4;
            *col += 4;
            break;
        }

        // EOF → error at opener
        if *i >= bytes.len() {
            let sp = Span::new(file, open_i, (open_i + 4).min(bytes.len()), open_line, open_col, open_line, open_col + 4);
            return Err(vec![Diagnostic::error("LexError", "Unterminated block comment (//// ... ////).", sp)]);
        }

        // advance, tracking newlines (i/line/col are &mut)
        match bytes[*i] {
            b'\r' => {
                // Treat CRLF as a single newline; lone CR as newline too. No indent scanning.
                let start = *i;
                if *i + 1 < bytes.len() && bytes[*i + 1] == b'\n' {
                    *i += 2; // consume \r\n
                } else {
                    *i += 1; // consume \r
                }
                let sp = Span::new(file, start, *i, *line, *col, *line + 1, 1);
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    span: sp,
                    value: None,
                });
                *line += 1;
                *col = 1;
                continue;
            }
            b'\n' => {
                // Emit a Newline token and advance one byte. No indent scanning.
                let sp = Span::new(file, *i, *i + 1, *line, *col, *line, *col + 1);
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    span: sp,
                    value: None,
                });
                *i += 1;
                *line += 1;
                *col = 1;
                continue;
            }
            _ => {
                *i += 1;
                *col += 1;
            }
        }
    }

    Ok(())
}

#[inline]
#[allow(dead_code)]
fn is_dec_digit(b: u8) -> bool {
    b.is_ascii_digit()
}

#[inline]
fn eat_hex_digits_us(bytes: &[u8], i: &mut usize, col: &mut u32) -> bool {
    let mut saw_digit = false;
    let mut last_us = false;
    let mut consumed = 0usize;

    while *i < bytes.len() {
        let b = bytes[*i];
        if b.is_ascii_hexdigit() {
            saw_digit = true;
            last_us = false;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else if b == b'_' && saw_digit && !last_us {
            last_us = true;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else {
            break;
        }
    }

    saw_digit && !last_us && consumed > 0
}

#[inline]
fn eat_oct_digits_us(bytes: &[u8], i: &mut usize, col: &mut u32) -> bool {
    let mut saw_digit = false;
    let mut last_us = false;
    let mut consumed = 0usize;

    while *i < bytes.len() {
        let b = bytes[*i];
        if (b'0'..=b'7').contains(&b) {
            saw_digit = true;
            last_us = false;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else if b == b'_' && saw_digit && !last_us {
            last_us = true;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else {
            break;
        }
    }

    saw_digit && !last_us && consumed > 0
}

#[inline]
fn eat_bin_digits_us(bytes: &[u8], i: &mut usize, col: &mut u32) -> bool {
    let mut saw_digit = false;
    let mut last_us = false;
    let mut consumed = 0usize;

    while *i < bytes.len() {
        let b = bytes[*i];
        if b == b'0' || b == b'1' {
            saw_digit = true;
            last_us = false;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else if b == b'_' && saw_digit && !last_us {
            last_us = true;
            *i += 1;
            *col += 1;
            consumed += 1;
        } else {
            break;
        }
    }

    saw_digit && !last_us && consumed > 0
}

/// Eats digits with optional single underscores BETWEEN digits (e.g., 1_234),
/// returning (new_index, had_separator_misuse)
#[allow(dead_code)]
fn eat_digits_with_seps<F: Fn(u8) -> bool>(
    bytes: &[u8],
    mut j: usize,
    is_digit: F,
) -> (usize, bool) {
    let mut prev_was_underscore = false;
    let mut saw_digit = false;
    let mut misuse = false;

    while j < bytes.len() {
        let b = bytes[j];
        if is_digit(b) {
            saw_digit = true;
            prev_was_underscore = false;
            j += 1;
        } else if b == b'_' {
            // underscore must be between two digits: require previous was a digit and next is a digit
            if !saw_digit || j + 1 >= bytes.len() || !is_digit(bytes[j + 1]) || prev_was_underscore
            {
                misuse = true;
            }
            prev_was_underscore = true;
            j += 1;
        } else {
            break;
        }
    }

    // trailing underscore is misuse
    if prev_was_underscore {
        misuse = true;
    }
    (j, misuse)
}

#[inline]
#[allow(dead_code)]
fn emit_newline(tokens: &mut Vec<Token>, file: &str, start_i: usize, end_i: usize, line: u32, col: u32) {
    let span = Span::new(file, start_i, end_i, line, col, line, col);
    tokens.push(Token { kind: TokenKind::Newline, span, value: None });
}

#[inline]
#[allow(dead_code)]
fn consume_linebreak(bytes: &[u8], i: usize) -> (usize, bool) {
    // returns (new_i, was_crlf)
    if i + 1 < bytes.len() && bytes[i] == b'\r' && bytes[i + 1] == b'\n' {
        (i + 2, true)
    } else if i < bytes.len() && (bytes[i] == b'\r' || bytes[i] == b'\n') {
        (i + 1, false)
    } else {
        (i, false)
    }
}

fn handle_indent_at_line_start(
    bytes: &[u8],
    _file: &str,
    _line: u32,
    line_start: usize,
    _indent_stack: &mut Vec<u32>,
    _tokens: &mut Vec<Token>,
) -> Result<usize, Vec<Diagnostic>> {
    // Indentation-agnostic:
    // - Skip any leading spaces/tabs on the physical line after a newline
    // - Do NOT emit Indent/Dedent
    // - Do NOT enforce multiples of 4 or ban tabs
    let mut j = line_start;
    while j < bytes.len() {
        match bytes[j] {
            b' ' | b'\t' => { j += 1; }
            // stop before actual line terminators; the caller handles newline emission already
            b'\r' | b'\n' => break,
            _ => break,
        }
    }
    Ok(j) // caller updates i/line/col
}

/// Minimal pass so far:
/// - recognize physical newlines (CRLF or LF)
/// - emit a final NEWLINE + EOF
/// - recognize single-char punct/ops: `()[]{},;` (dot handled with longest-match below)
/// - recognize pipe family with longest match: then `||`, else `|`
/// - recognize optional chain/nullish with preference `?.` then `??`, else `?`
/// - recognize ASCII identifiers /[A-Za-z_][A-Za-z0-9_]*[!|?]?/
/// - also recognize `@`- and `#`-prefixed identifiers with the same trailing `!|?` rule
/// - recognize numbers:
/// - INT/FLOAT decimal with `_` separators and optional exponent
/// - base-prefixed INT: 0x/0X (hex), 0o/0O (octal), 0b/0B (binary) with `_` separators
/// - strings: normal `'...'` / `"..."` and triple `'''...'''` / `"""..."""`
/// - escapes in non-raw: `\\ \" \' \n \r \t \xNN \u{H+}`
/// - raw strings: `r'...'`, `r"..."`, and triple-raw; no escapes
/// - comments: line `///` and block `//// … ////` (block emits NEWLINEs inside)
/// - suppress NEWLINE while inside any open ()[]{} delimiter
/// - math ops `+ - * ** / % ^ ~ << >>` and compound assignment `+= -= *= /= %= &= |= ^= <<= >>=`
/// - comparison ops `== != <= >= < > !`
/// - longest-match for `::` (otherwise single `:`), and for `...`/`..`/`.`
/// - logical AND `&&` (otherwise single `&`)
/// - arrows `->` and `=>`
pub fn lex(source: &str, file: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let mut tokens: Vec<Token> = Vec::new();

    let bytes = source.as_bytes();
    let mut i: usize = 0;
    let mut line: u32 = 1;
    let mut col: u32 = 1;

    // Delimiter nesting depth: increment on '(', '[', '{' and decrement on ')', ']', '}'.
    // Any depth > 0 suppresses NEWLINE emission (implicit continuation).
    let mut nest: i32 = 0;

    // layout/indent tracking
    let mut indent_stack: Vec<u32> = vec![0]; // levels in spaces (0,4,8,...)

    let is_alpha = |b: u8| b.is_ascii_alphabetic();
    let is_digit = |b: u8| b.is_ascii_digit();
    let is_ident_start = |b: u8| is_alpha(b) || b == b'_';
    let is_ident_continue = |b: u8| is_alpha(b) || is_digit(b) || b == b'_';

    // Hex helpers
    let hex_val = |b: u8| -> Option<u8> {
        match b {
            b'0'..=b'9' => Some(b - b'0'),
            b'a'..=b'f' => Some(10 + (b - b'a')),
            b'A'..=b'F' => Some(10 + (b - b'A')),
            _ => None,
        }
    };
    #[allow(unused_variables)]
    let _is_hex = |b: u8| hex_val(b).is_some();

    // String prefix modifiers that apply to the *next* literal only.
    let mut pending_raw: bool = false;
    let mut pending_trim_lead: bool = false;

    // Helper: remove common leading spaces across non-empty lines.
    let _trim_common_indent = |s: &str| -> String {
        let mut min: Option<usize> = None;
        for line in s.lines() {
            if line.trim().is_empty() {
                continue;
            }
            let n = line.chars().take_while(|&ch| ch == ' ').count();
            min = Some(match min {
                Some(m) if n < m => n,
                Some(m) => m,
                None => n,
            });
        }
        if let Some(n) = min {
            s.lines()
                .map(|line| {
                    let mut cut = 0usize;
                    for ch in line.chars() {
                        if ch == ' ' && cut < n {
                            cut += 1;
                        } else {
                            break;
                        }
                    }
                    line.get(cut..).unwrap_or("").to_string()
                })
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            s.to_string()
        }
    };

    // Returns Some(pos_of_pipe) if the line (from j) is spaces then "|>".
    // We only check the opener; we’ll skip the rest of the physical line in the NL arms.
    fn starts_with_pipe_carry(bytes: &[u8], mut j: usize) -> Option<usize> {
        while j < bytes.len() {
            match bytes[j] {
                b' ' => j += 1,
                b'\t' => return None, // tabs aren’t allowed for pipeline carry indentation
                b'|' if j + 1 < bytes.len() && bytes[j + 1] == b'>' => return Some(j),
                _ => return None,
            }
        }
        None
    }

    // Returns Some(pos_of_dot) if the line (from j) is spaces then "." followed by an identifier start.
    // Tabs are not allowed for continuation indentation.
    fn starts_with_dot_carry(bytes: &[u8], mut j: usize) -> Option<usize> {
        while j < bytes.len() {
            match bytes[j] {
                b' ' => j += 1,
                b'\t' => return None, // tabs disallowed for carry indentation
                b'.' => {
                    // Require a single dot followed by identifier start: [A-Za-z_]
                    if j + 1 < bytes.len() {
                        let n = bytes[j + 1];
                        if n.is_ascii_alphabetic() || n == b'_' {
                            return Some(j);
                        }
                    }
                    return None;
                }
                _ => return None,
            }
        }
        None
    }

    // --- Initial line indent (ignored; no INDENT/DEDENT) ---
    if i < bytes.len() && bytes[i] != b'\r' && bytes[i] != b'\n' {
        let mut j = 0usize;
        let mut jcol = 1u32;

        // Skip any leading spaces/tabs at the start of the file.
        while j < bytes.len() {
            match bytes[j] {
                b' ' | b'\t' => { j += 1; jcol += 1; }
                b'\r' | b'\n' => break,
                _ => break,
            }
        }

        // Start lexing at the first non-ws column. No Indent/Dedent. No 4-space rule.
        i = j;
        col = jcol;
    }

    while i < bytes.len() {
        // Orphan pipeline-carry line (non-indented):
        // If previous token is layout (or none) and this physical line starts
        // at column 1 with "|>", ignore the whole line so it lexes as a blank line.
        // We do NOT consume the newline here; the NL arm will emit NEWLINE.
        if nest == 0 {
            let prev_is_layout = match tokens.last() {
                None => true,
                Some(t) => matches!(
                    t.kind,
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                ),
            };

            if prev_is_layout {
                // Only treat as orphan if `|>` is at column 1 (no leading spaces).
                let mut j = i;
                let mut saw_space = false;
                while j < bytes.len() && bytes[j] == b' ' {
                    saw_space = true;
                    j += 1;
                }
                if !saw_space && j + 1 < bytes.len() && bytes[j] == b'|' && bytes[j + 1] == b'>' {
                    // Skip to end-of-line (but leave the newline to be handled normally).
                    j += 2;
                    while j < bytes.len() && bytes[j] != b'\n' && bytes[j] != b'\r' {
                        j += 1;
                    }
                    col += (j - i) as u32;
                    i = j;
                    continue;
                }
            }
        }

        match bytes[i] {
            b' ' | b'\t' => {
                i += 1;
                col += 1;
            }

            // --- CR or LF: NEWLINE unless next line starts with optional spaces then "|>" or "."
            // Also computes INDENT/DEDENT for the *next* logical line when not carried.
            b'\r' | b'\n' => {
                let nl_start = i;
                let nl_col = col;

                // how many bytes to consume for the physical newline
                let consumed = if bytes[i] == b'\r' && i + 1 < bytes.len() && bytes[i + 1] == b'\n' { 2 } else { 1 };
                i += consumed;
                let line_start = i; // first byte of next physical line

                if nest == 0 {
                    // Only allow carry if there is a prior non-layout token
                    let can_carry = tokens
                        .iter()
                        .rev()
                        .any(|t| !matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent));

                    if can_carry {
                        if let Some(pipe_pos) = starts_with_pipe_carry(bytes, line_start) {
                            // suppress NEWLINE + layout, and continue lexing from the '|>'
                            line += 1;
                            col = 1 + (pipe_pos - line_start) as u32;
                            i = pipe_pos;
                            continue;
                        }
                        if let Some(dot_pos) = starts_with_dot_carry(bytes, line_start) {
                            line += 1;
                            col = 1 + (dot_pos - line_start) as u32;
                            i = dot_pos;
                            continue;
                        }
                    }

                    // Normal NEWLINE + layout handling
                    let span_nl = Span::new(file, nl_start, line_start, line, nl_col, line, nl_col);
                    tokens.push(Token { kind: TokenKind::Newline, span: span_nl, value: None });

                    // one call replaces the whole duplicated indentation block
                    let j = handle_indent_at_line_start(bytes, file, line, line_start, &mut indent_stack, &mut tokens)?;
                    line += 1;
                    col = 1 + (j - line_start) as u32;
                    i = j;
                    continue;
                } else {
                    // Inside (), [], {}: implicit continuation – just emit NEWLINE
                    let span = Span::new(file, nl_start, line_start, line, nl_col, line, nl_col);
                    tokens.push(Token { kind: TokenKind::Newline, span, value: None });
                    line += 1;
                    col = 1;
                }
            }

            // Slash: comments or operators
            b'/' => {
                // Block comment "//// ... ////"
                if is_block_comment_open(bytes, i) {
                    consume_block_comment(bytes, file, &mut i, &mut line, &mut col, &mut tokens)?;
                    continue;
                }

                // Otherwise it's an operator: "/=" or "/"
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("/=".to_string()), span, value: None });
                    i += 2; col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("/".to_string()), span, value: None });
                    i += 1; col += 1;
                }
            }

            // Numbers (decimal int/float with exponent, or base-prefixed integers)
            // Numbers (decimal, float, base-prefixed, with percent/duration/money)
            b'0'..=b'9' => {
                let start_i = i;
                let start_col = col;
                let mut is_float = false;

                // Base-prefixed integers: 0x / 0o / 0b
                if i + 1 < bytes.len() && bytes[i] == b'0' {
                    match bytes[i + 1] {
                        b'x' | b'X' => {
                            let mut j = i + 2;
                            let mut c = col + 2;
                            if eat_hex_digits_us(bytes, &mut j, &mut c) {
                                let lit = String::from_utf8_lossy(&bytes[i..j]).into_owned();
                                let span = Span::new(file, i, j, line, col, line, c);
                                tokens.push(Token { kind: TokenKind::Int, span, value: Some(lit) });
                                i = j; col = c;
                                continue; // <-- don't fall through to the generic advance
                            }
                        }
                        b'o' | b'O' => {
                            let mut j = i + 2;
                            let mut c = col + 2;
                            if eat_oct_digits_us(bytes, &mut j, &mut c) {
                                let lit = String::from_utf8_lossy(&bytes[i..j]).into_owned();
                                let span = Span::new(file, i, j, line, col, line, c);
                                tokens.push(Token { kind: TokenKind::Int, span, value: Some(lit) });
                                i = j; col = c;
                                continue; // <-- here too
                            }
                        }
                        b'b' | b'B' => {
                            let mut j = i + 2;
                            let mut c = col + 2;
                            if eat_bin_digits_us(bytes, &mut j, &mut c) {
                                let lit = String::from_utf8_lossy(&bytes[i..j]).into_owned();
                                let span = Span::new(file, i, j, line, col, line, c);
                                tokens.push(Token { kind: TokenKind::Int, span, value: Some(lit) });
                                i = j; col = c;
                                continue; // <-- and here
                            }
                        }
                        _ => {}
                    }
                }

                let mut saw_digit = false;
                let mut last_was_underscore = false;

                while i < bytes.len() {
                    let b = bytes[i];
                    match b {
                        b'0'..=b'9' => {
                            saw_digit = true;
                            last_was_underscore = false;
                            i += 1; col += 1;
                        }
                        b'_' => {
                            if !saw_digit || last_was_underscore {
                                let sp = Span::new(file, i, i, line, col, line, col);
                                return Err(vec![Diagnostic::error(
                                    "lexer",
                                    "invalid underscore placement",
                                    sp,
                                )]);
                            }
                            last_was_underscore = true;
                            i += 1; col += 1;
                        }
                        _ => break,
                    }
                }

                if last_was_underscore {
                    let sp = Span::new(file, i, i, line, col, line, col);
                    return Err(vec![Diagnostic::error(
                        "lexer",
                        "underscore cannot trail a number",
                        sp,
                    )]);
                }

                // Fractional part
                if i + 1 < bytes.len() && bytes[i] == b'.' && bytes[i + 1].is_ascii_digit() {
                    is_float = true;
                    i += 1; col += 1;
                    let mut saw_digit = false;
                    let mut last_was_underscore = false;
                    while i < bytes.len() {
                        let b = bytes[i];
                        match b {
                            b'0'..=b'9' => {
                                saw_digit = true;
                                last_was_underscore = false;
                                i += 1; col += 1;
                            }
                            b'_' => {
                                if !saw_digit || last_was_underscore {
                                    let sp = Span::new(file, i, i, line, col, line, col);
                                    return Err(vec![Diagnostic::error(
                                        "lexer",
                                        "invalid underscore in fractional part",
                                        sp,
                                    )]);
                                }
                                last_was_underscore = true;
                                i += 1; col += 1;
                            }
                            _ => break,
                        }
                    }
                    if last_was_underscore {
                        let sp = Span::new(file, i, i, line, col, line, col);
                        return Err(vec![Diagnostic::error(
                            "lexer",
                            "underscore cannot trail fractional part",
                            sp,
                        )]);
                    }
                }

                // Exponent
                if i < bytes.len() && (bytes[i] == b'e' || bytes[i] == b'E') {
                    let save_i = i;
                    let save_col = col;
                    i += 1; col += 1;
                    if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
                        i += 1; col += 1;
                    }

                    let mut saw_digit = false;
                    let mut last_was_underscore = false;
                    while i < bytes.len() {
                        let b = bytes[i];
                        match b {
                            b'0'..=b'9' => {
                                saw_digit = true;
                                last_was_underscore = false;
                                i += 1; col += 1;
                            }
                            b'_' => {
                                if !saw_digit || last_was_underscore {
                                    let sp = Span::new(file, i, i, line, col, line, col);
                                    return Err(vec![Diagnostic::error(
                                        "lexer",
                                        "invalid underscore in exponent",
                                        sp,
                                    )]);
                                }
                                last_was_underscore = true;
                                i += 1; col += 1;
                            }
                            _ => break,
                        }
                    }

                    if !saw_digit {
                        i = save_i;
                        col = save_col;
                    } else {
                        is_float = true;
                    }

                    if last_was_underscore {
                        let sp = Span::new(file, i, i, line, col, line, col);
                        return Err(vec![Diagnostic::error(
                            "lexer",
                            "underscore cannot trail exponent",
                            sp,
                        )]);
                    }
                }

                // Emit number
                let text = &source.as_bytes()[start_i..i];
                let lit = String::from_utf8_lossy(text).into_owned();
                let span = Span::new(file, start_i, i, line, start_col, line, col);
                let kind = if is_float { TokenKind::Float } else { TokenKind::Int };
                tokens.push(Token { kind, span, value: Some(lit) });
            }

            // Raw strings starting with r" / r' / r""" / r''' (case-insensitive 'r')
            // If not followed by a quote, treat as a normal identifier starting at this 'r'.
            b'r' | b'R' => {
                if i + 1 < bytes.len() && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'') {
                    let start_i = i;
                    let start_col = col;
                    let quote = bytes[i + 1];
                    // triple raw?
                    if i + 3 < bytes.len() && bytes[i + 2] == quote && bytes[i + 3] == quote {
                        i += 4;
                        col += 4; // r + """ / '''
                        let mut out = String::new();
                        while i < bytes.len() {
                            if i + 2 < bytes.len()
                                && bytes[i] == quote
                                && bytes[i + 1] == quote
                                && bytes[i + 2] == quote
                            {
                                i += 3;
                                col += 3;
                                break;
                            }
                            let b = bytes[i];
                            if b == b'\r' {
                                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                                    i += 2;
                                } else {
                                    i += 1;
                                }
                                out.push('\n');
                                line += 1;
                                col = 1;
                                continue;
                            } else if b == b'\n' {
                                i += 1;
                                out.push('\n');
                                line += 1;
                                col = 1;
                                continue;
                            }
                            out.push(b as char);
                            i += 1;
                            col += 1;
                        }
                        let span = Span::new(file, start_i, i, line, start_col, line, col);
                        tokens.push(Token {
                            kind: TokenKind::String,
                            span,
                            value: Some(out),
                        });
                    } else {
                        // single-line raw
                        i += 2;
                        col += 2; // r + quote
                        let mut out = String::new();
                        // single-line RAW: newline/EOF before closing quote is an error
                        let mut closed = false;
                        while i < bytes.len() {
                            let b = bytes[i];
                            if b == quote {
                                i += 1;
                                col += 1;
                                closed = true;
                                break;
                            }
                            if b == b'\n' || b == b'\r' {
                                let sp = Span::new(
                                    file,
                                    start_i,
                                    start_i + 1,
                                    line,
                                    start_col,
                                    line,
                                    start_col + 1,
                                );
                                return Err(vec![Diagnostic::error(
                                    "LexError",
                                    "Unterminated string (basic).",
                                    sp,
                                )]);
                            }
                            out.push(b as char);
                            i += 1;
                            col += 1;
                        }
                        if !closed {
                            // hit EOF without a closing quote
                            let sp = Span::new(
                                file,
                                start_i,
                                start_i + 1,
                                line,
                                start_col,
                                line,
                                start_col + 1,
                            );
                            return Err(vec![Diagnostic::error(
                                "LexError",
                                "Unterminated string (basic).",
                                sp,
                            )]);
                        }
                        let span = Span::new(file, start_i, i, line, start_col, line, col);
                        tokens.push(Token {
                            kind: TokenKind::String,
                            span,
                            value: Some(out),
                        });
                    }
                    continue;
                } else {
                    // Not a raw string: lex as identifier starting at 'r'
                    let start_i = i;
                    let start_col = col;
                    i += 1;
                    col += 1;
                    while i < bytes.len() && is_ident_continue(bytes[i]) {
                        i += 1;
                        col += 1;
                    }
                    if i < bytes.len()
                        && (bytes[i] == b'!' || bytes[i] == b'?')
                        && !(bytes[i] == b'?'
                            && i + 1 < bytes.len()
                            && (bytes[i + 1] == b'.' || bytes[i + 1] == b'?'))
                    {
                        i += 1;
                        col += 1;
                    }
                    let text = &bytes[start_i..i];
                    let name = String::from_utf8_lossy(text).into_owned();
                    let span = Span::new(file, start_i, i, line, start_col, line, col);
                    tokens.push(Token {
                        kind: TokenKind::Ident,
                        span,
                        value: Some(name),
                    });
                    continue;
                }
            }

            b'"' | b'\'' => {
                // Capture start & quote kind
                let quote = bytes[i];
                let start_i = i;
                let start_line = line;
                let start_col = col;

                // Per-literal flags (consume and reset the pending ones)
                let this_raw  = pending_raw;
                let this_trim = pending_trim_lead;
                pending_raw = false;
                pending_trim_lead = false;

                // Detect triple
                let is_triple = i + 2 < bytes.len()
                    && bytes[i + 1] == quote
                    && bytes[i + 2] == quote;

                // Advance past opening quotes
                if is_triple {
                    i += 3; col += 3;
                } else {
                    i += 1; col += 1;
                }

                let mut out = String::new();

                // Scan body
                loop {
                    // EOF before closing
                    if i >= bytes.len() {
                        let sp = Span::new(file, start_i, (start_i + 1).min(bytes.len()),
                                           start_line, start_col, start_line, start_col + 1);
                        return Err(vec![Diagnostic::error(
                            "LexError",
                            "Unterminated string literal.",
                            sp,
                        )]);
                    }

                    // Closing?
                    if is_triple {
                        if i + 2 < bytes.len()
                            && bytes[i] == quote
                            && bytes[i + 1] == quote
                            && bytes[i + 2] == quote
                        {
                            i += 3; col += 3;
                            break;
                        }
                    } else if bytes[i] == quote {
                        i += 1; col += 1;
                        break;
                    }

                    let b = bytes[i];

                    // Newlines only allowed in triple strings
                    if b == b'\r' || b == b'\n' {
                        if !is_triple {
                            // newline in single-line string -> unterminated
                            let sp = Span::new(file, start_i, (start_i + 1).min(bytes.len()),
                                               start_line, start_col, start_line, start_col + 1);
                            return Err(vec![Diagnostic::error(
                                "LexError",
                                "Unterminated string literal (newline before closing quote).",
                                sp,
                            )]);
                        }
                        // triple: normalize CRLF / LF to '\n'
                        if b == b'\r' && i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                            out.push('\n');
                            i += 2;
                        } else {
                            out.push('\n');
                            i += 1;
                        }
                        line += 1;
                        col = 1;
                        continue;
                    }

                    // Escapes (only if not raw)
                    if !this_raw && b == b'\\' {
                        if i + 1 >= bytes.len() {
                            let sp = Span::new(file, start_i, (start_i + 1).min(bytes.len()),
                                               start_line, start_col, start_line, start_col + 1);
                            return Err(vec![Diagnostic::error(
                                "LexError",
                                "Unterminated string escape.",
                                sp,
                            )]);
                        }
                        let esc = bytes[i + 1];
                        match esc {
                            b'n'  => out.push('\n'),
                            b'r'  => out.push('\r'),
                            b't'  => out.push('\t'),
                            b'\\' => out.push('\\'),
                            b'"'  => out.push('"'),
                            b'\'' => out.push('\''),
                            b'0'  => out.push('\0'),
                            invalid => {
                                let sp = Span::new(file, i, i + 2, line, col, line, col + 2);
                                return Err(vec![Diagnostic::error(
                                    "LexError",
                                    &format!("Invalid escape sequence: \\{}", invalid as char),
                                    sp,
                                )]);
                            }
                        }
                        i += 2;
                        col += 2;
                        continue;
                    }

                    // Ordinary byte
                    out.push(b as char);
                    i += 1;
                    col += 1;
                }

                // If triple + trim_lead: remove common leading spaces of non-empty lines
                if is_triple && this_trim {
                    let lines_vec: Vec<&str> = out.split('\n').collect();
                    let mut min_indent: Option<usize> = None;
                    for &ln in &lines_vec {
                        if ln.trim().is_empty() { continue; }
                        let n = ln.chars().take_while(|&c| c == ' ').count();
                        min_indent = Some(match min_indent { Some(m) => m.min(n), None => n });
                    }
                    if let Some(n) = min_indent {
                        let mut rebuilt = String::new();
                        for (idx, &ln) in lines_vec.iter().enumerate() {
                            if ln.trim().is_empty() {
                                rebuilt.push_str(ln);
                            } else {
                                let mut dropped = 0usize;
                                for ch in ln.chars() {
                                    if dropped < n && ch == ' ' { dropped += 1; continue; }
                                    rebuilt.push(ch);
                                }
                            }
                            if idx + 1 < lines_vec.len() { rebuilt.push('\n'); }
                        }
                        out = rebuilt;
                    }
                }

                // Emit token
                let span = Span::new(file, start_i, i, start_line, start_col, line, col);
                tokens.push(Token { kind: TokenKind::String, span, value: Some(out) });

                continue;
            }

            // Dot / range family and leading-dot floats: "...", "..", ".", and ".5", ".25e-2"
            b'.' => {
                // 1) Longest-match ranges first
                if i + 2 < bytes.len() && bytes[i + 1] == b'.' && bytes[i + 2] == b'.' {
                    // "..." (exclusive range)
                    let span = Span::new(file, i, i + 3, line, col, line, col + 3);
                    tokens.push(Token { kind: TokenKind::Operator("...".to_string()), span, value: None });
                    i += 3;
                    col += 3;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'.' {
                    // ".." (inclusive range)
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("..".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                }
                // 2) Leading-dot float (e.g., .5 or .25e-2)
                else if i + 1 < bytes.len() && (bytes[i + 1] as char).is_ascii_digit() {
                    let start_i = i;
                    let start_col = col;

                    // consume '.' then digits
                    i += 1;
                    col += 1;
                    while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                        i += 1;
                        col += 1;
                    }

                    // optional exponent: e|E [+-]? digits (must have at least one digit)
                    if i < bytes.len() && (bytes[i] == b'e' || bytes[i] == b'E') {
                        let save_i = i;
                        let save_col = col;
                        i += 1;
                        col += 1; // e/E
                        if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
                            i += 1;
                            col += 1;
                        }
                        let exp_start = i;
                        while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                            i += 1;
                            col += 1;
                        }
                        if i == exp_start {
                            // no exponent digits; roll back
                            i = save_i;
                            col = save_col;
                        }
                    }

                    let lit = String::from_utf8_lossy(&source.as_bytes()[start_i..i]).into_owned();
                    let span = Span::new(file, start_i, i, line, start_col, line, col);
                    tokens.push(Token {
                        kind: TokenKind::Float,
                        span,
                        value: Some(lit),
                    });
                }
                // 3) Plain dot
                else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator(".".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            b':' => {
                // Longest match: '::' then ':'
                if i + 1 < bytes.len() && bytes[i + 1] == b':' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("::".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator(":".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Unicode symbol-leading money: £ (C2 A3) or ¥ (C2 A5)
            0xC2 => {
                // Only handle if it's actually £/¥; otherwise consume one byte and move on.
                if i + 1 < bytes.len() && (bytes[i + 1] == 0xA3 || bytes[i + 1] == 0xA5) {
                    let start_i = i;
                    let start_col = col;
                    // advance past the symbol (2 bytes)
                    i += 2;
                    col += 2;

                    // optional sign after symbol
                    if i < bytes.len() && bytes[i] == b'-' {
                        i += 1;
                        col += 1;
                    }

                    // integer digits with underscores
                    let mut j = i;
                    let mut jcol = col;
                    let mut have_digit = false;
                    while j < bytes.len() {
                        let b2 = bytes[j];
                        if b2.is_ascii_digit() {
                            have_digit = true;
                            j += 1;
                            jcol += 1;
                        } else if b2 == b'_' {
                            j += 1;
                            jcol += 1;
                        } else {
                            break;
                        }
                    }

                    // optional fractional part: '.' then at least one digit (underscores allowed)
                    if j + 1 < bytes.len() && bytes[j] == b'.' && bytes[j + 1].is_ascii_digit() {
                        j += 1;
                        jcol += 1; // consume '.'
                        while j < bytes.len() {
                            let b2 = bytes[j];
                            if b2.is_ascii_digit() {
                                have_digit = true;
                                j += 1;
                                jcol += 1;
                            } else if b2 == b'_' {
                                j += 1;
                                jcol += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    if have_digit {
                        let span = Span::new(file, start_i, j, line, start_col, line, jcol);
                        let canon = String::from_utf8_lossy(&bytes[start_i..j]).into_owned();
                        tokens.push(Token {
                            kind: TokenKind::Money,
                            span,
                            value: Some(canon),
                        });
                        i = j;
                        col = jcol;
                        continue;
                    } else {
                        // Not actually a money literal; back off to avoid an infinite loop,
                        // but still make forward progress.
                        i = start_i + 1;
                        col = start_col + 1;
                        continue;
                    }
                } else {
                    // Not a currency symbol we handle; consume one byte and continue.
                    i += 1;
                    col += 1;
                    continue;
                }
            }

            // Unicode symbol-leading money: € (E2 82 AC)
            0xE2 => {
                if i + 2 < bytes.len() && bytes[i + 1] == 0x82 && bytes[i + 2] == 0xAC {
                    let start_i = i;
                    let start_col = col;
                    // advance past the symbol (3 bytes)
                    i += 3;
                    col += 3;

                    // optional sign after symbol
                    if i < bytes.len() && bytes[i] == b'-' {
                        i += 1;
                        col += 1;
                    }

                    // integer digits with underscores
                    let mut j = i;
                    let mut jcol = col;
                    let mut have_digit = false;
                    while j < bytes.len() {
                        let b2 = bytes[j];
                        if b2.is_ascii_digit() {
                            have_digit = true;
                            j += 1;
                            jcol += 1;
                        } else if b2 == b'_' {
                            j += 1;
                            jcol += 1;
                        } else {
                            break;
                        }
                    }

                    // optional fractional part
                    if j + 1 < bytes.len() && bytes[j] == b'.' && bytes[j + 1].is_ascii_digit() {
                        j += 1;
                        jcol += 1;
                        while j < bytes.len() {
                            let b2 = bytes[j];
                            if b2.is_ascii_digit() {
                                have_digit = true;
                                j += 1;
                                jcol += 1;
                            } else if b2 == b'_' {
                                j += 1;
                                jcol += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    if have_digit {
                        let span = Span::new(file, start_i, j, line, start_col, line, jcol);
                        let canon = String::from_utf8_lossy(&bytes[start_i..j]).into_owned();
                        tokens.push(Token {
                            kind: TokenKind::Money,
                            span,
                            value: Some(canon),
                        });
                        i = j;
                        col = jcol;
                        continue;
                    } else {
                        // Not money; advance one byte to make progress
                        i = start_i + 1;
                        col = start_col + 1;
                        continue;
                    }
                } else {
                    // Not €; consume one byte and continue
                    i += 1;
                    col += 1;
                    continue;
                }
            }

            b'$' => {
                // Symbol-leading money literal: $12, $12.34, $-12.34
                let start_i = i;
                let start_col = col;

                // consume '$'
                i += 1;
                col += 1;

                let mut j = i;
                let mut jcol = col;

                // optional sign AFTER the symbol (e.g., $-12.34)
                if j < bytes.len() && bytes[j] == b'-' {
                    j += 1;
                    jcol += 1;
                }

                // integer digits (allow underscores as typed)
                let mut has_digit = false;
                while j < bytes.len() {
                    let b = bytes[j];
                    if b.is_ascii_digit() {
                        has_digit = true;
                        j += 1;
                        jcol += 1;
                    } else if b == b'_' {
                        j += 1;
                        jcol += 1;
                    } else {
                        break;
                    }
                }

                // optional fractional part: '.' then at least one digit
                if j + 1 < bytes.len() && bytes[j] == b'.' && bytes[j + 1].is_ascii_digit() {
                    j += 1;
                    jcol += 1; // consume '.'
                    while j < bytes.len() {
                        let b = bytes[j];
                        if b.is_ascii_digit() {
                            has_digit = true;
                            j += 1;
                            jcol += 1;
                        } else if b == b'_' {
                            j += 1;
                            jcol += 1;
                        } else {
                            break;
                        }
                    }
                }

                if has_digit {
                    // Commit as a single MONEY token including the '$' prefix
                    let text = &source.as_bytes()[start_i..j];
                    let canon = String::from_utf8_lossy(text).into_owned(); // e.g., "$12.34" or "$-1_000"
                    let span = Span::new(file, start_i, j, line, start_col, line, jcol);
                    tokens.push(Token {
                        kind: TokenKind::Money,
                        span,
                        value: Some(canon),
                    });
                    i = j;
                    col = jcol;
                } else {
                    // Not a money literal — treat '$' as a standalone operator
                    let span = Span::new(
                        file,
                        start_i,
                        start_i + 1,
                        line,
                        start_col,
                        line,
                        start_col + 1,
                    );
                    tokens.push(Token { kind: TokenKind::Operator("$".to_string()), span, value: None });
                    // i/col already advanced by 1 above
                }
            }

            // Operators / punctuators (adjust nesting for delimiters)
            b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' => {
                let ch = bytes[i] as char;
                match ch {
                    '(' | '[' | '{' => {
                        nest += 1;
                    }
                    ')' | ']' | '}' => {
                        if nest > 0 {
                            nest -= 1;
                        }
                    }
                    _ => {}
                }
                let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                tokens.push(Token {
                    kind: TokenKind::Operator(ch.to_string()),
                    span,
                    value: None,
                });
                i += 1;
                col += 1;
            }

            // Equals family: ===, ==, =>, =
            b'=' => {
                // Longest-match first
                if i + 2 < bytes.len() && bytes[i + 1] == b'=' && bytes[i + 2] == b'=' {
                    let span = Span::new(file, i, i + 3, line, col, line, col + 3);
                    tokens.push(Token { kind: TokenKind::Operator("===".to_string()), span, value: None });
                    i += 3;
                    col += 3;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("==".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    // fat arrow =>
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("=>".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    // assignment =
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("=".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Not family: !===, !==, !=, !
            b'!' => {
                // Longest-match first
                if i + 3 < bytes.len()
                    && bytes[i + 1] == b'='
                    && bytes[i + 2] == b'='
                    && bytes[i + 3] == b'='
                {
                    let span = Span::new(file, i, i + 4, line, col, line, col + 4);
                    tokens.push(Token { kind: TokenKind::Operator("!===".to_string()), span, value: None });
                    i += 4;
                    col += 4;
                } else if i + 2 < bytes.len() && bytes[i + 1] == b'=' && bytes[i + 2] == b'=' {
                    let span = Span::new(file, i, i + 3, line, col, line, col + 3);
                    tokens.push(Token { kind: TokenKind::Operator("!==".to_string()), span, value: None });
                    i += 3;
                    col += 3;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("!=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("!".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Less-than family: <<=, <<, <=, <>, <
            b'<' => {
                // Longest-match first
                if i + 1 < bytes.len() && bytes[i + 1] == b'<' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("<<".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("<=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("<>".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("<".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Greater-than family: >>=, >>, >=, >
            b'>' => {
                // Longest-match first
                if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator(">>".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator(">=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'<' {
                    // NEW: divmod
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("><".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator(">".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Ampersand family: && (and-alias), &= (compound assign), &
            b'&' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'&' {
                    // '&&' (logical-and alias)
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("&&".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    // '&=' — explicitly not supported
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);

                    // Return a diagnostics vector directly (matches your lexer Result error type)
                    return Err(vec![Diagnostic::error(
                        "lex",
                        "Unsupported operator '&=' (reserved '&' cannot be combined with '=')",
                        span,
                    )]);
                } else {
                    // Single '&' — reserved token (parser will decide semantics later)
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("&".to_string()), span, value: None });
                }
            }

            // Asterisk family: **=, **, *=, *
            b'*' => {
                // Longest-match first
                if i + 2 < bytes.len() && bytes[i + 1] == b'*' && bytes[i + 2] == b'=' {
                    let span = Span::new(file, i, i + 3, line, col, line, col + 3);
                    tokens.push(Token { kind: TokenKind::Operator("**=".to_string()), span, value: None });
                    i += 3;
                    col += 3;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    // Power operator (parser decides postfix vs infix)
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("**".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("*=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("*".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Plus family: ++ (postfix money inc), +=, +
            b'+' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'+' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("++".to_string()), span, value: None, });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("+=".to_string()), span, value: None, });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("+".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Minus family: -> (arrow), -- (postfix money dec), -=, -
            b'-' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'-' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("--".to_string()), span, value: None, });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("-=".to_string()), span, value: None, });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("-".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // '%' family: '%=', '%'
            b'%' => {
                // Compound assign wins anywhere
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("%=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                }
                // Percent-literal family ONLY when immediately after a digit (no whitespace)
                else if i > 0 && (bytes[i - 1] as char).is_ascii_digit() {
                    if i + 1 < bytes.len() && bytes[i + 1] == b's' {
                        // %s (percent-of-self sugar)
                        let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                        tokens.push(Token { kind: TokenKind::Operator("%s".to_string()), span, value: None });
                        i += 2;
                        col += 2;
                    } else if i + 1 < bytes.len() && bytes[i + 1] == b'o' {
                        // %o (percent-of-other sugar)
                        let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                        tokens.push(Token { kind: TokenKind::Operator("%o".to_string()), span, value: None });
                        i += 2;
                        col += 2;
                    } else {
                        // Bare '%' immediately after a number → percent literal
                        let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                        tokens.push(Token { kind: TokenKind::Operator("%".to_string()), span, value: None });
                        i += 1;
                        col += 1;
                    }
                }
                // Otherwise it's modulo
                else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("%".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Caret family: "^^" (explain-power), "^=" (compound), "^"
            b'^' => {
                // Longest-match first
                if i + 1 < bytes.len() && bytes[i + 1] == b'^' {
                    // "^^" explain-power operator (binary)
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("^^".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    // "^=" compound assign
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("^=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    // bare "^"
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("^".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Tilde (unary bitwise-not / custom use)
            b'~' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("~=".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("~".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // '|' family: ||', '|=', '|'
            b'|' => {
                // Longest-match among |>  ||  |=  |
                if i + 1 < bytes.len() && bytes[i + 1] == b'|' {
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token { kind: TokenKind::Operator("||".to_string()), span, value: None });
                    i += 2;
                    col += 2;
                } else {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token { kind: TokenKind::Operator("|".to_string()), span, value: None });
                    i += 1;
                    col += 1;
                }
            }

            // Nullish / optional member
            b'?' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'?' {
                    // '??' nullish coalescing
                    let span = Span::new(file, i, i + 2, line, col, line, col + 2);
                    tokens.push(Token {
                        kind: TokenKind::Operator("??".to_string()),
                        span,
                        value: None,
                    });
                    i += 2;
                    col += 2;
                } else if i + 2 < bytes.len() && bytes[i + 1] == b'>' && bytes[i + 2] == b'>' {
                    // '?>>' optional chaining (safe navigation)
                    let span = Span::new(file, i, i + 3, line, col, line, col + 3);
                    tokens.push(Token {
                        kind: TokenKind::Operator("?>>".to_string()),
                        span,
                        value: None,
                    });
                    i += 3;
                    col += 3;
                } else {
                    // lone '?': reserved operator or error
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    tokens.push(Token {
                        kind: TokenKind::Operator("?".to_string()),
                        span,
                        value: None,
                    });
                    i += 1;
                    col += 1;
                }
            }

            // --- Combined AT/HASH identifiers (keep this; delete the later standalone `b'#'` arm) ---
            b'@' | b'#' => {
                let prefix = bytes[i];
                let start_i = i;
                let start_col = col;

                // '@' must be followed by an identifier start (class name)
                if i + 1 >= bytes.len() || !is_ident_start(bytes[i + 1]) {
                    let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                    return Err(vec![Diagnostic::error("LexError", "Expected identifier after '@'.", span)]);
                }

                // Version chars used when we see @ followed by a digit
                let is_ver_char = |b: u8| b.is_ascii_alphanumeric();

                // @VERSION form: @ followed by a digit → consume a version atom (kept as Ident for now)
                if prefix == b'@' && i + 1 < bytes.len() && (bytes[i + 1] as char).is_ascii_digit()
                {
                    i += 2;
                    col += 2; // '@' and first digit
                    while i < bytes.len() && is_ver_char(bytes[i]) {
                        i += 1;
                        col += 1;
                    }
                    let text = &source.as_bytes()[start_i..i]; // keep "@1.2.3" shape for now
                    let name = String::from_utf8_lossy(text).into_owned();
                    let span = Span::new(file, start_i, i, line, start_col, line, col);
                    tokens.push(Token {
                        kind: TokenKind::Ident,
                        span,
                        value: Some(name),
                    });
                    continue;
                }

                // @ident / #ident (with optional trailing !/?), value excludes the prefix
                let is_alpha = |b: u8| b.is_ascii_alphabetic();
                let is_digit = |b: u8| b.is_ascii_digit();
                let is_ident_start = |b: u8| is_alpha(b) || b == b'_';
                let is_ident_continue = |b: u8| is_alpha(b) || is_digit(b) || b == b'_';

                if i + 1 < bytes.len() && is_ident_start(bytes[i + 1]) {
                    i += 1;
                    col += 1; // consume prefix
                    let ident_start = i; // start AFTER the prefix
                    let _ident_col = col;

                    i += 1;
                    col += 1; // first char already valid
                    while i < bytes.len() && is_ident_continue(bytes[i]) {
                        i += 1;
                        col += 1;
                    }
                    if i < bytes.len() && (bytes[i] == b'!' || bytes[i] == b'?') {
                        i += 1;
                        col += 1;
                    }

                    let name = String::from_utf8_lossy(&bytes[ident_start..i]).into_owned();
                    let span = Span::new(file, start_i, i, line, start_col, line, col);

                    if prefix == b'@' {
                        tokens.push(Token {
                            kind: TokenKind::AtIdent,
                            span,
                            value: Some(name),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::HashIdent,
                            span,
                            value: Some(name),
                        });
                    }
                    continue;
                }

                // Lone '@' or '#'
                let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                tokens.push(Token {
                    kind: TokenKind::Op((prefix as char).to_string()),
                    span,
                    value: None,
                });
                i += 1;
                col += 1;
            }

            b if is_ident_start(b) => {
                let start_i = i;
                let start_col = col;

                // Special-case: standalone '_' acts as a postfix operator (e.g., 3.14_)
                if bytes[i] == b'_' {
                    let next = bytes.get(i + 1).copied();
                    let continues_ident = matches!(next, Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_'));
                    if !continues_ident {
                        let span = Span::new(file, i, i + 1, line, col, line, col + 1);
                        tokens.push(Token { kind: TokenKind::Operator("_".to_string()), span, value: None });
                        i += 1;
                        col += 1;
                        continue; // don't run the normal ident scanner
                    }
                }

                // consume first ident char
                i += 1;
                col += 1;
                // continue ident
                while i < bytes.len() && is_ident_continue(bytes[i]) {
                    i += 1;
                    col += 1;
                }
                // optional single trailing ! or ?
                if i < bytes.len() && (bytes[i] == b'!' || bytes[i] == b'?') {
                    // avoid stealing `?.` and `??` (handled in the '?' arm)
                    if !(bytes[i] == b'?'
                        && i + 1 < bytes.len()
                        && (bytes[i + 1] == b'.' || bytes[i + 1] == b'?'))
                    {
                        i += 1;
                        col += 1;
                    }
                }

                // ---- STRING PREFIXES: raw / trim_lead (stackable) ----
                // If this ident is a string prefix and is immediately followed (same line)
                // by another prefix and/or a string literal, set flags and *do not* emit an IDENT.
                {
                    let ident_text = String::from_utf8_lossy(&bytes[start_i..i]).into_owned();

                    if ident_text == "raw" || ident_text == "trim_lead" {
                        // Look ahead on the same physical line:
                        // skip spaces/tabs, allow additional prefixes, and require that we
                        // eventually see a quote-started string (or r/R + quote) on this line.
                        let mut k = i;

                        // local helper: scan one ident at k and return (end, text), or None
                        let scan_ident_at = |k0: usize| -> Option<(usize, String)> {
                            if k0 < bytes.len() && is_ident_start(bytes[k0]) {
                                let mut e = k0 + 1;
                                while e < bytes.len() && is_ident_continue(bytes[e]) {
                                    e += 1;
                                }
                                let txt = String::from_utf8_lossy(&bytes[k0..e]).into_owned();
                                Some((e, txt))
                            } else {
                                None
                            }
                        };

                        let leads_to_string = loop {
                            // stop on EOL
                            if k >= bytes.len() || bytes[k] == b'\n' || bytes[k] == b'\r' {
                                break false;
                            }

                            // skip spaces/tabs
                            if bytes[k] == b' ' || bytes[k] == b'\t' {
                                k += 1;
                                continue;
                            }

                            // a quote starts a string
                            if bytes[k] == b'"' || bytes[k] == b'\'' {
                                break true;
                            }

                            // r/R + quote is also a string opener
                            if (bytes[k] == b'r' || bytes[k] == b'R')
                                && k + 1 < bytes.len()
                                && (bytes[k + 1] == b'"' || bytes[k + 1] == b'\'')
                            {
                                break true;
                            }

                            // another prefix ident? (raw / trim_lead)
                            if let Some((kend, txt)) = scan_ident_at(k) {
                                if txt == "raw" || txt == "trim_lead" {
                                    k = kend; // accept and keep walking
                                    continue;
                                }
                            }

                            // anything else → not a prefix context
                            break false;
                        };

                        if leads_to_string {
                            if ident_text == "raw" {
                                pending_raw = true;
                            }
                            if ident_text == "trim_lead" {
                                pending_trim_lead = true;
                            }
                            // swallow this ident as a prefix; next loop iteration will lex the string.
                            continue;
                        }
                    }
                }
                // -------------------------------------------------------

                // ---- Country+symbol money literal: <CC>$<number> (e.g., US$30, JP$-12.34) ----
                // Trigger ONLY when the ident we just scanned is exactly two uppercase ASCII letters,
                // followed immediately by '$' and a number. If it matches, emit a single Money token.
                {
                    let ident_bytes = &bytes[start_i..i];
                    let is_two_upper = ident_bytes.len() == 2
                        && ident_bytes[0].is_ascii_uppercase()
                        && ident_bytes[1].is_ascii_uppercase();

                    if is_two_upper && i < bytes.len() && bytes[i] == b'$' {
                        let mut j = i + 1; // start after '$'
                        let mut jcol = col + 1;

                        // optional sign after the symbol (US$-12.34)
                        if j < bytes.len() && bytes[j] == b'-' {
                            j += 1;
                            jcol += 1;
                        }

                        // integer digits (underscores allowed)
                        let mut have_digit = false;
                        while j < bytes.len() {
                            let b2 = bytes[j];
                            if b2.is_ascii_digit() {
                                have_digit = true;
                                j += 1;
                                jcol += 1;
                            } else if b2 == b'_' {
                                j += 1;
                                jcol += 1;
                            } else {
                                break;
                            }
                        }

                        // optional fractional part: '.' then at least one digit
                        if j + 1 < bytes.len() && bytes[j] == b'.' && bytes[j + 1].is_ascii_digit()
                        {
                            j += 1;
                            jcol += 1; // consume '.'
                            while j < bytes.len() {
                                let b2 = bytes[j];
                                if b2.is_ascii_digit() {
                                    have_digit = true;
                                    j += 1;
                                    jcol += 1;
                                } else if b2 == b'_' {
                                    j += 1;
                                    jcol += 1;
                                } else {
                                    break;
                                }
                            }
                        }

                        if have_digit {
                            // Emit Money token covering CC + '$' + number
                            let span = Span::new(file, start_i, j, line, start_col, line, jcol);
                            let canon = String::from_utf8_lossy(&bytes[start_i..j]).into_owned();
                            tokens.push(Token {
                                kind: TokenKind::Money,
                                span,
                                value: Some(canon),
                            });
                            i = j;
                            col = jcol;
                            continue;
                        }
                        // else: fall through to normal IDENT
                    }
                }

                // Normal IDENT token
                let name = String::from_utf8_lossy(&bytes[start_i..i]).into_owned();
                let span = Span::new(file, start_i, i, line, start_col, line, col);
                tokens.push(Token {
                    kind: TokenKind::Ident,
                    span,
                    value: Some(name),
                });
            }

            _ => {
                i += 1;
                col += 1;
            } // unknown byte for now
        }
    }

    // --- Flush pending DEDENTs before final NEWLINE + EOF ---
    let eof_span = Span::new(file, bytes.len(), bytes.len(), line, col, line, col);
    while indent_stack.len() > 1 {
        indent_stack.pop();
        let sp = eof_span.clone();
        tokens.push(Token {
            kind: TokenKind::Dedent,
            span: sp,
            value: None,
        });
    }

    // Final NEWLINE then EOF (per v1.18)
    tokens.push(Token {
        kind: TokenKind::Newline,
        span: eof_span.clone(),
        value: None,
    });
    tokens.push(Token {
        kind: TokenKind::Eof,
        span: eof_span,
        value: None,
    });

    Ok(tokens)
}

#[test]
fn emits_selected_ops_idents_numbers_and_eof() {
    use crate::*;

    let src = ".PCT! foo? 12 3.4 .5 .25e-2 1_000 0x2A 0XFF 0o755 0O7 0b1010_0110 0B1 \
               x?>>y ?? z .. ... | h t || u && v & w a => b c => d . , : :: ; ( ) [ ] { } \
               = == === => ! != !== !=== < <= > >= << >> <<= >>= += -= *= /= %= ^= ^ ~ \
               \"hi\\n\" 'ok\\'' \"A=\\x41\" \"smile=\\u{1F600}\" @user #topic @ping! #wow?\n\
               \"\"\"hello\\nworld\"\"\" '''x\ny'''\n\
               r\"C:\\Users\\bob\\file.txt\" r'raw\\no\\esc' \
               r\"\"\"keep\\nbackslash\nand real newline\"\"\" r'''x\\ny'''\n";

    let toks = lex(src, "test.gbln").unwrap();

    let has_op = |s: &str| {
        toks.iter()
            .any(|t| matches!(&t.kind, TokenKind::Op(op) if op == s))
    };
    let count_op = |s: &str| {
        toks.iter()
            .filter(|t| matches!(&t.kind, TokenKind::Op(op) if op == s))
            .count()
    };
    let has_ident = |s: &str| {
        toks.iter()
            .any(|t| matches!(t, Token { kind: TokenKind::Ident, value: Some(v), .. } if v == s))
    };
    let has_int = |s: &str| {
        toks.iter()
            .any(|t| matches!(t, Token { kind: TokenKind::Int, value: Some(v), .. } if v == s))
    };
    let has_float = |s: &str| {
        toks.iter()
            .any(|t| matches!(t, Token { kind: TokenKind::Float, value: Some(v), .. } if v == s))
    };
    let has_string = |s: &str| {
        toks.iter()
            .any(|t| matches!(t, Token { kind: TokenKind::String, value: Some(v), .. } if v == s))
    };

    assert!(has_ident("PCT!"));
    assert!(has_ident("foo?"));
    assert!(has_int("12"));
    assert!(has_float("3.4"));
    assert!(has_float(".5"));
    assert!(has_float(".25e-2"));
    assert!(has_int("1_000"));
    assert!(has_int("0x2A"));
    assert!(has_int("0XFF"));
    assert!(has_int("0o755"));
    assert!(has_int("0O7"));
    assert!(has_int("0b1010_0110"));
    assert!(has_int("0B1"));

    assert!(has_op("?>>"));
    assert!(has_op("??"));
    assert!(has_op(".."));
    assert!(has_op("..."));
    assert!(has_op("||"));
    assert!(has_op("|"));
    assert!(has_op("&&"));
    assert!(has_op("&"));
    assert!(has_op("=>"));
    assert!(has_op("."));
    assert!(has_op(","));
    assert!(has_op(":"));
    assert!(has_op("::"));
    assert!(has_op(";"));
    assert!(has_op("("));
    assert!(has_op(")"));
    assert!(has_op("["));
    assert!(has_op("]"));
    assert!(has_op("{"));
    assert!(has_op("}"));

    // Goblin operator families
    assert!(has_op("="));
    assert!(has_op("=="));
    assert!(has_op("!="));
    assert!(has_op("==="));
    assert!(has_op("!=="));
    assert!(has_op("!==="));
    assert!(has_op("!")); // standalone not

    // Guard against false splits: each should appear exactly once in this soup.
    assert_eq!(count_op("!==="), 1);
    assert_eq!(count_op("!=="), 1);
    assert_eq!(count_op("!="), 1);
    assert_eq!(count_op("==="), 1);
    assert_eq!(count_op("=="), 1);
    assert_eq!(count_op("="), 1);

    assert!(has_string("hi\n"));
    assert!(has_string("ok'"));
    assert!(has_string("A=A")); // \x41 -> 'A'
    assert!(has_string("smile=😀")); // \u{1F600}
    assert!(has_string("hello\\nworld")); // triple normal keeps escaped backslash then n
    assert!(has_string("x\ny")); // triple single with real newline
    assert!(has_string(r"C:\Users\bob\file.txt")); // raw keeps backslashes
    assert!(has_string(r"raw\no\esc"));
    assert!(has_string("keep\\nbackslash\nand real newline")); // raw triple: \n kept, real NL added
    assert!(has_string("x\\ny")); // raw triple: backslash-n stays literal

    assert!(matches!(toks.last().unwrap().kind, TokenKind::Eof));
}
