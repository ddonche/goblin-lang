//! Goblin Lexer (refactored)
//! 
//! Complete rewrite with improved organization while preserving all functionality.

#![allow(dead_code)]

use goblin_diagnostics::{Diagnostic, Span};

// ============================================================================
// Token Types
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    AtIdent,
    HashIdent,
    Act,
    Action,
    Int,
    Float,
    String,
    Char,
    Newline,
    Indent,
    Dedent,
    Duration,
    Blob,
    Date,
    Time,
    DateTime,
    Op(String),
    Money,
    Eof,
}

impl TokenKind {
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
    pub value: Option<String>,
}

impl Token {
    fn new(kind: TokenKind, span: Span, value: Option<String>) -> Self {
        Self { kind, span, value }
    }

    fn operator(op: String, span: Span) -> Self {
        Self::new(TokenKind::Op(op.clone()), span, Some(op))
    }

    fn simple_op(op: String, span: Span) -> Self {
        Self::new(TokenKind::Op(op), span, None)
    }
}

// ============================================================================
// Lexer State
// ============================================================================

struct LexerState<'a> {
    bytes: &'a [u8],
    file: &'a str,
    source: &'a str,
    i: usize,
    line: u32,
    col: u32,
    nest: i32,
    indent_stack: Vec<u32>,
    tokens: Vec<Token>,
    pending_raw: bool,
    pending_trim_lead: bool,
}

impl<'a> LexerState<'a> {
    fn new(source: &'a str, file: &'a str) -> Self {
        Self {
            bytes: source.as_bytes(),
            file,
            source,
            i: 0,
            line: 1,
            col: 1,
            nest: 0,
            indent_stack: vec![0],
            tokens: Vec::new(),
            pending_raw: false,
            pending_trim_lead: false,
        }
    }

    #[inline]
    fn bump_char(&mut self) -> Option<char> {
        let s = std::str::from_utf8(&self.bytes[self.i..]).ok()?;
        let mut it = s.chars();
        let ch = it.next()?;
        let adv = ch.len_utf8();
        self.i += adv;
        self.col += adv as u32;
        Some(ch)
    }

    #[inline]
    fn peek_char(&self) -> Option<char> {
        std::str::from_utf8(&self.bytes[self.i..]).ok()?.chars().next()
    }

    #[inline]
    fn current(&self) -> Option<u8> {
        self.bytes.get(self.i).copied()
    }

    #[inline]
    fn peek(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.i + offset).copied()
    }

    #[inline]
    fn advance(&mut self) {
        self.i += 1;
        self.col += 1;
    }

    #[inline]
    fn advance_by(&mut self, n: usize) {
        self.i += n;
        self.col += n as u32;
    }

    fn span(&self, start_i: usize, start_col: u32) -> Span {
        Span::new(self.file, start_i, self.i, self.line, start_col, self.line, self.col)
    }

    fn push_token(&mut self, kind: TokenKind, start_i: usize, start_col: u32, value: Option<String>) {
        let span = self.span(start_i, start_col);
        self.tokens.push(Token::new(kind, span, value));
    }
}

// ============================================================================
// Character Classification
// ============================================================================

#[inline]
fn is_alpha(b: u8) -> bool {
    b.is_ascii_alphabetic()
}

#[inline]
fn is_digit(b: u8) -> bool {
    b.is_ascii_digit()
}

#[inline]
fn is_ident_start(b: u8) -> bool {
    is_alpha(b) || b == b'_'
}

#[inline]
fn is_ident_continue(b: u8) -> bool {
    is_alpha(b) || is_digit(b) || b == b'_'
}

#[inline]
fn is_upper(b: u8) -> bool {
    (b'A'..=b'Z').contains(&b)
}

// ============================================================================
// Comment Handling
// ============================================================================

fn is_block_comment_open(bytes: &[u8], i: usize) -> bool {
    i + 3 < bytes.len()
        && bytes[i] == b'/'
        && bytes[i + 1] == b'/'
        && bytes[i + 2] == b'/'
        && bytes[i + 3] == b'/'
}

fn consume_block_comment(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let open_i = state.i;
    let open_line = state.line;
    let open_col = state.col;

    state.advance_by(4); // consume "////"

    loop {
        if is_block_comment_open(state.bytes, state.i) {
            state.advance_by(4);
            break;
        }

        if state.i >= state.bytes.len() {
            let sp = Span::new(
                state.file,
                open_i,
                (open_i + 4).min(state.bytes.len()),
                open_line,
                open_col,
                open_line,
                open_col + 4,
            );
            return Err(vec![Diagnostic::error(
                "L0206",
                "I found a block comment that never closes\n\nhelp: Close it with //// on a later line",
                sp,
            )]);
        }

        match state.current().unwrap() {
            b'\r' => {
                let start = state.i;
                if state.peek(1) == Some(b'\n') {
                    state.i += 2;
                } else {
                    state.i += 1;
                }
                let sp = Span::new(state.file, start, state.i, state.line, state.col, state.line + 1, 1);
                state.tokens.push(Token::new(TokenKind::Newline, sp, None));
                state.line += 1;
                state.col = 1;
            }
            b'\n' => {
                let sp = Span::new(state.file, state.i, state.i + 1, state.line, state.col, state.line, state.col + 1);
                state.tokens.push(Token::new(TokenKind::Newline, sp, None));
                state.advance();
                state.line += 1;
                state.col = 1;
            }
            _ => state.advance(),
        }
    }

    Ok(())
}

fn consume_line_comment(state: &mut LexerState) {
    state.advance_by(3); // consume "///"
    while state.i < state.bytes.len() && state.current() != Some(b'\r') && state.current() != Some(b'\n') {
        state.advance();
    }
}

fn consume_inline_comment(state: &mut LexerState, dashes: usize) -> Result<(), Vec<Diagnostic>> {
    // Check if there's code before this comment on the same line
    let mut has_code_on_line = false;
    for t in state.tokens.iter().rev() {
        if t.span.line_start < state.line {
            break;
        }
        match t.kind {
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => {}
            _ => {
                has_code_on_line = true;
                break;
            }
        }
    }

    if !has_code_on_line {
        let start_i = state.i;
        let start_col = state.col;
        let end_i = state.i + 1 + dashes;
        let end_col = start_col + (1 + dashes as u32);
        let span = Span::new(state.file, start_i, end_i, state.line, start_col, state.line, end_col);

        return Err(vec![Diagnostic::error(
            "L0207",
            "I found an inline comment marker at the start of this line\n\n\
             help: Inline comments (`<---` or `<----`) are only allowed \
             after code on the same line. If you meant a comment by itself, \
             use `///` for a line comment or `//// ... ////` for a block comment.",
            span,
        )]);
    }

    // Consume comment
    state.advance_by(1 + dashes);
    while state.i < state.bytes.len() && state.current() != Some(b'\r') && state.current() != Some(b'\n') {
        state.advance();
    }

    Ok(())
}

// ============================================================================
// Number Parsing
// ============================================================================

fn match_type_suffix(bytes: &[u8], i: usize) -> Option<(usize, &'static str)> {
    if i >= bytes.len() {
        return None;
    }
    match bytes[i] {
        b'i' => {
            if i + 5 <= bytes.len() && &bytes[i..i + 5] == b"isize" {
                return Some((5, "isize"));
            }
            if i + 2 <= bytes.len() && &bytes[i..i + 2] == b"i8" {
                return Some((2, "i8"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"i16" {
                return Some((3, "i16"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"i32" {
                return Some((3, "i32"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"i64" {
                return Some((3, "i64"));
            }
            if i + 4 <= bytes.len() && &bytes[i..i + 4] == b"i128" {
                return Some((4, "i128"));
            }
            None
        }
        b'u' => {
            if i + 5 <= bytes.len() && &bytes[i..i + 5] == b"usize" {
                return Some((5, "usize"));
            }
            if i + 2 <= bytes.len() && &bytes[i..i + 2] == b"u8" {
                return Some((2, "u8"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"u16" {
                return Some((3, "u16"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"u32" {
                return Some((3, "u32"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"u64" {
                return Some((3, "u64"));
            }
            if i + 4 <= bytes.len() && &bytes[i..i + 4] == b"u128" {
                return Some((4, "u128"));
            }
            None
        }
        b'f' => {
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"f32" {
                return Some((3, "f32"));
            }
            if i + 3 <= bytes.len() && &bytes[i..i + 3] == b"f64" {
                return Some((3, "f64"));
            }
            None
        }
        b'b' => Some((1, "b")),
        _ => None,
    }
}

fn lex_hex_integer(state: &mut LexerState, start_i: usize, start_col: u32) -> Result<(), Vec<Diagnostic>> {
    state.advance_by(2); // consume "0x" or "0X"

    if state.i >= state.bytes.len() || !state.current().unwrap().is_ascii_hexdigit() {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0302",
            "Invalid digit for this base\n\nhelp: After `0x`, use hexadecimal digits (0-9, A-F). Example: `0xDEAD_BEEF`",
            sp,
        )]);
    }

    let mut last_us = false;
    while state.i < state.bytes.len() {
        match state.current().unwrap() {
            b if b.is_ascii_hexdigit() => {
                last_us = false;
                state.advance();
            }
            b'_' => {
                if last_us {
                    let sp = state.span(state.i, state.col);
                    return Err(vec![Diagnostic::error(
                        "L0303",
                        "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits (e.g., `0xDEAD_BEEF`), not doubled",
                        sp,
                    )]);
                }
                if state.peek(1).map_or(false, |b| b.is_ascii_hexdigit()) {
                    last_us = true;
                    state.advance();
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    if last_us {
        let sp = Span::new(state.file, state.i - 1, state.i, state.line, state.col - 1, state.line, state.col);
        return Err(vec![Diagnostic::error(
            "L0309",
            "This number ends with an underscore\n\nhelp: Put underscores only between digits (e.g., `0xDEAD_BEEF`), not at the end",
            sp,
        )]);
    }

    let mut lit = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();
    if let Some((adv, suf)) = match_type_suffix(state.bytes, state.i) {
        lit.push_str(suf);
        state.advance_by(adv);
    }

    state.push_token(TokenKind::Int, start_i, start_col, Some(lit));
    Ok(())
}

fn lex_octal_integer(state: &mut LexerState, start_i: usize, start_col: u32) -> Result<(), Vec<Diagnostic>> {
    state.advance_by(2); // consume "0o" or "0O"

    if state.i >= state.bytes.len() {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0302",
            "Invalid digit for this base\n\nhelp: After `0o`, use octal digits (0-7). Example: `0o755`",
            sp,
        )]);
    }

    let b = state.current().unwrap();
    if b == b'_' {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0303",
            "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits (e.g., `0o7_55`), not at the start",
            sp,
        )]);
    }
    if !(b'0'..=b'7').contains(&b) {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0302",
            "Invalid digit for this base\n\nhelp: After `0o`, use octal digits (0-7). Example: `0o755`",
            sp,
        )]);
    }

    let mut last_us = false;
    let mut saw_digit = false;
    while state.i < state.bytes.len() {
        let b = state.current().unwrap();
        if (b'0'..=b'7').contains(&b) {
            saw_digit = true;
            last_us = false;
            state.advance();
        } else if b == b'_' {
            if !saw_digit || last_us {
                let sp = state.span(state.i, state.col);
                return Err(vec![Diagnostic::error(
                    "L0303",
                    "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits (e.g., `0o7_55`), not at the start, end, or doubled",
                    sp,
                )]);
            }
            if state.peek(1).map_or(false, |b| (b'0'..=b'7').contains(&b)) {
                last_us = true;
                state.advance();
            } else {
                break;
            }
        } else {
            let sp = state.span(state.i, state.col);
            return Err(vec![Diagnostic::error(
                "L0302",
                "Invalid digit for this base\n\nhelp: After `0o`, use octal digits (0-7). Example: `0o755`",
                sp,
            )]);
        }
    }

    if last_us {
        let sp = Span::new(state.file, state.i - 1, state.i, state.line, state.col - 1, state.line, state.col);
        return Err(vec![Diagnostic::error(
            "L0309",
            "This number ends with an underscore\n\nhelp: Put underscores only between digits (e.g., `0o7_55`), not at the end",
            sp,
        )]);
    }

    let mut lit = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();
    if let Some((adv, suf)) = match_type_suffix(state.bytes, state.i) {
        lit.push_str(suf);
        state.advance_by(adv);
    }

    state.push_token(TokenKind::Int, start_i, start_col, Some(lit));
    Ok(())
}

fn lex_binary_integer(state: &mut LexerState, start_i: usize, start_col: u32) -> Result<(), Vec<Diagnostic>> {
    state.advance_by(2); // consume "0b" or "0B"

    if state.i >= state.bytes.len() {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0302",
            "Invalid digit for this base\n\nhelp: After `0b`, use binary digits (0 or 1). Example: `0b1010`",
            sp,
        )]);
    }

    let b = state.current().unwrap();
    if b == b'_' {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0303",
            "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits (e.g., `0b1010_0101`), not at the start",
            sp,
        )]);
    }
    if !(b'0'..=b'1').contains(&b) {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0302",
            "Invalid digit for this base\n\nhelp: After `0b`, use binary digits (0 or 1). Example: `0b1010`",
            sp,
        )]);
    }

    let mut last_us = false;
    let mut saw_digit = false;
    while state.i < state.bytes.len() {
        let b = state.current().unwrap();
        if (b'0'..=b'1').contains(&b) {
            saw_digit = true;
            last_us = false;
            state.advance();
        } else if b == b'_' {
            if !saw_digit || last_us {
                let sp = state.span(state.i, state.col);
                return Err(vec![Diagnostic::error(
                    "L0303",
                    "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits (e.g., `0b1010_0101`), not at the start, end, or doubled",
                    sp,
                )]);
            }
            if state.peek(1).map_or(false, |b| (b'0'..=b'1').contains(&b)) {
                last_us = true;
                state.advance();
            } else {
                break;
            }
        } else {
            let sp = state.span(state.i, state.col);
            return Err(vec![Diagnostic::error(
                "L0302",
                "Invalid digit for this base\n\nhelp: After `0b`, use binary digits (0 or 1). Example: `0b1010`",
                sp,
            )]);
        }
    }

    if last_us {
        let sp = Span::new(state.file, state.i - 1, state.i, state.line, state.col - 1, state.line, state.col);
        return Err(vec![Diagnostic::error(
            "L0309",
            "This number ends with an underscore\n\nhelp: Put underscores only between digits (e.g., `0b1010_0101`), not at the end",
            sp,
        )]);
    }

    let mut lit = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();
    if let Some((adv, suf)) = match_type_suffix(state.bytes, state.i) {
        lit.push_str(suf);
        state.advance_by(adv);
    }

    state.push_token(TokenKind::Int, start_i, start_col, Some(lit));
    Ok(())
}

fn lex_decimal_number(state: &mut LexerState, start_i: usize, start_col: u32) -> Result<(), Vec<Diagnostic>> {
    let mut is_float = false;
    let mut saw_digit = false;
    let mut last_was_underscore = false;

    // Integer part
    while state.i < state.bytes.len() {
        match state.current().unwrap() {
            b'0'..=b'9' => {
                saw_digit = true;
                last_was_underscore = false;
                state.advance();
            }
            b'_' => {
                if !saw_digit || last_was_underscore {
                    let sp = state.span(state.i, state.col);
                    return Err(vec![Diagnostic::error(
                        "L0303",
                        "I found an underscore in this number where it can't go\n\nhelp: Use underscores only between digits: 1_234, not at the start or doubled",
                        sp,
                    )]);
                }
                if state.peek(1).map_or(false, |b| b.is_ascii_digit()) {
                    last_was_underscore = true;
                    state.advance();
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    if last_was_underscore {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0309",
            "This number ends with an underscore\n\nhelp: Put underscores only between digits: 1_234, not at the end",
            sp,
        )]);
    }

    // Fractional part
    if state.current() == Some(b'.') {
        if state.peek(1) != Some(b'.') {
            if state.peek(1).map_or(false, |b| b.is_ascii_digit()) {
                is_float = true;
                state.advance(); // consume '.'
                let mut saw_frac_digit = false;
                let mut last_us = false;
                while state.i < state.bytes.len() {
                    match state.current().unwrap() {
                        b'0'..=b'9' => {
                            saw_frac_digit = true;
                            last_us = false;
                            state.advance();
                        }
                        b'_' => {
                            if !saw_frac_digit || last_us {
                                let sp = state.span(state.i, state.col);
                                return Err(vec![Diagnostic::error(
                                    "L0311",
                                    "I found an underscore here in the fractional part\n\nhelp: In the fractional part, put underscores only between digits: 3.141_592, not right after the dot or doubled",
                                    sp,
                                )]);
                            }
                            if state.peek(1).map_or(false, |b| b.is_ascii_digit()) {
                                last_us = true;
                                state.advance();
                            } else {
                                break;
                            }
                        }
                        _ => break,
                    }
                }
                if last_us {
                    let sp = state.span(state.i, state.col);
                    return Err(vec![Diagnostic::error(
                        "L0312",
                        "The fractional part ends with an underscore\n\nhelp: Put underscores only between digits: 3.141_592, not at the end",
                        sp,
                    )]);
                }
            }
        }
    }

    // Exponent
    if state.current() == Some(b'e') || state.current() == Some(b'E') {
        let e_pos = state.i;
        let e_col = state.col;
        state.advance();

        if state.current() == Some(b'+') || state.current() == Some(b'-') {
            state.advance();
        }

        if !state.current().map_or(false, |b| b.is_ascii_digit()) {
            let sp = Span::new(state.file, e_pos, e_pos + 1, state.line, e_col, state.line, e_col + 1);
            return Err(vec![Diagnostic::error(
                "L0304",
                "Exponent must be followed by digits\n\nhelp: Write `1e9` or `1e+9`, not `1e`",
                sp,
            )]);
        }

        while state.i < state.bytes.len() {
            match state.current().unwrap() {
                b if b.is_ascii_digit() => state.advance(),
                b'_' => {
                    let sp = state.span(state.i, state.col);
                    return Err(vec![Diagnostic::error(
                        "L0313",
                        "I found an underscore in this exponent where it can't go\n\nhelp: Write the exponent with digits only: 1e9, 1e+9, 1e-9; underscores aren't allowed",
                        sp,
                    )]);
                }
                _ => break,
            }
        }

        is_float = true;
    }

    let mut lit = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();
    let mut base_kind = if is_float { TokenKind::Float } else { TokenKind::Int };

    // Check for postfix
    if let Some((adv, suf)) = match_type_suffix(state.bytes, state.i) {
        lit.push_str(suf);
        state.advance_by(adv);
    } else {
        // Duration units
        if state.i + 1 < state.bytes.len() && state.current() == Some(b'm') && state.peek(1) == Some(b'o') {
            lit.push_str("mo");
            state.advance_by(2);
            base_kind = TokenKind::Duration;
        } else if state.i < state.bytes.len() {
            match state.current().unwrap() {
                b's' | b'm' | b'h' | b'd' | b'w' | b'y' => {
                    lit.push(state.current().unwrap() as char);
                    state.advance();
                    base_kind = TokenKind::Duration;
                }
                _ => {}
            }
        }
    }

    state.push_token(base_kind, start_i, start_col, Some(lit));
    Ok(())
}

fn lex_number(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    // Base-prefixed integers
    if state.current() == Some(b'0') && state.i + 1 < state.bytes.len() {
        match state.peek(1).unwrap() {
            b'x' | b'X' => return lex_hex_integer(state, start_i, start_col),
            b'o' | b'O' => return lex_octal_integer(state, start_i, start_col),
            b'b' | b'B' => return lex_binary_integer(state, start_i, start_col),
            _ => {}
        }
    }

    lex_decimal_number(state, start_i, start_col)
}

fn lex_leading_dot_float(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    state.advance(); // consume '.'

    while state.current().map_or(false, |b| b.is_ascii_digit()) {
        state.advance();
    }

    // Optional exponent
    if state.current() == Some(b'e') || state.current() == Some(b'E') {
        let save_i = state.i;
        let save_col = state.col;
        state.advance();
        if state.current() == Some(b'+') || state.current() == Some(b'-') {
            state.advance();
        }
        let exp_start = state.i;
        while state.current().map_or(false, |b| b.is_ascii_digit()) {
            state.advance();
        }
        if state.i == exp_start {
            state.i = save_i;
            state.col = save_col;
        }
    }

    let mut lit = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();
    if let Some((adv, suf)) = match_type_suffix(state.bytes, state.i) {
        lit.push_str(suf);
        state.advance_by(adv);
    }

    state.push_token(TokenKind::Float, start_i, start_col, Some(lit));
    Ok(())
}

// ============================================================================
// String Parsing
// ============================================================================

fn lex_escape_sequence(state: &mut LexerState) -> Result<char, Diagnostic> {
    let esc_start = state.i;
    let esc_col = state.col;

    state.advance(); // consume '\'

    if state.i >= state.bytes.len() {
        let sp = Span::new(state.file, esc_start, esc_start + 1, state.line, esc_col, state.line, esc_col + 1);
        return Err(Diagnostic::error(
            "L0205",
            "This escape sequence never finishes\n\nhelp: Complete the escape: \\n, \\t, \\\" or \\u{1F600}, or remove the trailing '\\\\'",
            sp,
        ));
    }

    let b = state.current().unwrap();
    let simple = match b {
        b'n' => Some('\n'),
        b'r' => Some('\r'),
        b't' => Some('\t'),
        b'\\' => Some('\\'),
        b'"' => Some('"'),
        b'\'' => Some('\''),
        b'0' => Some('\0'),
        _ => None,
    };
    if let Some(ch) = simple {
        state.advance();
        return Ok(ch);
    }

    // \xNN (exactly two hex digits)
    if b == b'x' {
        let _h = |c: u8| -> Option<u8> {
            Some(match c {
                b'0'..=b'9' => c - b'0',
                b'a'..=b'f' => c - b'a' + 10,
                b'A'..=b'F' => c - b'A' + 10,
                _ => return None,
            })
        };
        let x_pos = state.i;
        state.advance(); // consume 'x'
        let _h1 = match state.current() {
            Some(c) => c,
            None => {
                let sp = Span::new(state.file, x_pos, x_pos + 2, state.line, state.col, state.line, state.col + 2);
                return Err(Diagnostic::error(
                    "L0211",
                    "Unexpected end of input in hex escape (need two hex digits)",
                    sp,
                ));
            }
        };

        let _h2 = match state.current() {
            Some(c) => c,
            None => {
                let sp = Span::new(state.file, x_pos, x_pos + 3, state.line, state.col, state.line, state.col + 3);
                return Err(Diagnostic::error(
                    "L0211",
                    "Unexpected end of input in hex escape (need two hex digits)",
                    sp,
                ));
            }
        };
    }

    // \u{HEX...}
    if b == b'u' {
        let u_pos = state.i;
        state.advance(); // 'u'
        if state.current() != Some(b'{') {
            let sp = Span::new(state.file, u_pos, state.i, state.line, state.col, state.line, state.col);
            return Err(Diagnostic::error("L0202", "Use \\u{...} for Unicode escapes", sp));
        }
        state.advance(); // '{'
        let mut val: u32 = 0;
        let mut digits = 0usize;
        while let Some(c) = state.current() {
            if c == b'}' {
                state.advance();
                break;
            }
            let d = match c {
                b'0'..=b'9' => (c - b'0') as u32,
                b'a'..=b'f' => (c - b'a' + 10) as u32,
                b'A'..=b'F' => (c - b'A' + 10) as u32,
                _ => {
                    let sp = Span::new(state.file, u_pos, state.i + 1, state.line, state.col, state.line, state.col + 1);
                    return Err(Diagnostic::error("L0202", "Invalid hex in \\u{...}", sp));
                }
            };
            val = (val << 4) | d;
            digits += 1;
            if digits > 6 {
                let sp = Span::new(state.file, u_pos, state.i + 1, state.line, state.col, state.line, state.col + 1);
                return Err(Diagnostic::error("L0202", "Too many hex digits in \\u{...}", sp));
            }
            state.advance();
        }
        match std::char::from_u32(val) {
            Some(ch) => return Ok(ch),
            None => {
                let sp = Span::new(state.file, u_pos, state.i, state.line, state.col, state.line, state.col);
                return Err(Diagnostic::error("L0202", "Invalid Unicode scalar in \\u{...}", sp));
            }
        }
    }

    let sp = Span::new(state.file, esc_start, state.i + 1, state.line, esc_col, state.line, state.col + 1);
    Err(Diagnostic::error(
        "L0202",
        "Invalid escape sequence\n\nhelp: Use one of: \\n, \\r, \\t, \\\\, \\\", \\\', \\xNN, or \\u{...}",
        sp,
    ))
}

fn lex_string_literal(state: &mut LexerState, is_raw: bool, is_trim: bool) -> Result<(), Vec<Diagnostic>> {
    let quote = state.current().unwrap();
    let start_i = state.i;
    let start_line = state.line;
    let start_col = state.col;

    let is_triple = state.peek(1) == Some(quote) && state.peek(2) == Some(quote);

    if is_triple {
        state.advance_by(3);
    } else {
        state.advance();
    }

    let mut out = String::new();

    loop {
        if state.i >= state.bytes.len() {
            let sp = Span::new(state.file, start_i, (start_i + 1).min(state.bytes.len()),
                             start_line, start_col, start_line, start_col + 1);
            return Err(vec![Diagnostic::error(
                "L0201",
                "This string never closes\n\nhelp: Add the closing '\"'",
                sp,
            )]);
        }

        // Check for closing quote(s)
        if is_triple {
            if state.current() == Some(quote) && state.peek(1) == Some(quote) && state.peek(2) == Some(quote) {
                state.advance_by(3);
                break;
            }
        } else if state.current() == Some(quote) {
            state.advance();
            break;
        }

        let b = state.current().unwrap();

        // Handle newlines
        if b == b'\r' || b == b'\n' {
            if !is_triple {
                let sp = Span::new(state.file, start_i, (start_i + 1).min(state.bytes.len()),
                                 start_line, start_col, start_line, start_col + 1);
                return Err(vec![Diagnostic::error(
                    "L0204",
                    "Strings can't contain an unescaped newline\n\nhelp: Close the quote before the newline or escape it",
                    sp,
                )]);
            }
            if b == b'\r' && state.peek(1) == Some(b'\n') {
                out.push('\n');
                state.i += 2;
            } else {
                out.push('\n');
                state.i += 1;
            }
            state.line += 1;
            state.col = 1;
            continue;
        }

        // Handle escapes (only if not raw)
        if !is_raw && b == b'\\' {
            match lex_escape_sequence(state) {
                Ok(ch) => out.push(ch),
                Err(e) => return Err(vec![e]),
            }
            continue;
        }

        let ch = state.bump_char().ok_or_else(|| {
            vec![Diagnostic::error("L0201", "Invalid UTF-8 in string", state.span(start_i, start_col))]
        })?;
        out.push(ch);
    }

    // Apply trim_lead for triple strings
    if is_triple && is_trim {
        let lines_vec: Vec<&str> = out.split('\n').collect();
        let mut min_indent: Option<usize> = None;
        for &ln in &lines_vec {
            if ln.trim().is_empty() {
                continue;
            }
            let n = ln.chars().take_while(|&c| c == ' ').count();
            min_indent = Some(match min_indent {
                Some(m) => m.min(n),
                None => n,
            });
        }
        if let Some(n) = min_indent {
            let mut rebuilt = String::new();
            for (idx, &ln) in lines_vec.iter().enumerate() {
                if ln.trim().is_empty() {
                    rebuilt.push_str(ln);
                } else {
                    let mut dropped = 0usize;
                    for ch in ln.chars() {
                        if dropped < n && ch == ' ' {
                            dropped += 1;
                            continue;
                        }
                        rebuilt.push(ch);
                    }
                }
                if idx + 1 < lines_vec.len() {
                    rebuilt.push('\n');
                }
            }
            out = rebuilt;
        }
    }

    let span = Span::new(state.file, start_i, state.i, start_line, start_col, state.line, state.col);
    state.tokens.push(Token::new(TokenKind::String, span, Some(out)));
    Ok(())
}

fn lex_char_literal(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_line = state.line;
    let start_col = state.col;

    state.advance(); // consume opening '\''

    if state.i >= state.bytes.len() {
        let sp = Span::new(state.file, start_i, start_i + 1, start_line, start_col, start_line, start_col + 1);
        return Err(vec![Diagnostic::error(
            "L0210",
            "This char literal never closes\n\nhelp: Add the closing ' after the character",
            sp,
        )]);
    }

    // Check for empty char literal
    if state.current() == Some(b'\'') {
        let sp = Span::new(state.file, start_i, state.i + 1, start_line, start_col, state.line, state.col + 1);
        return Err(vec![Diagnostic::error(
            "L0211",
            "Empty char literal\n\nhelp: Char literals must contain exactly one character: 'a', '\\n', or '\\u{1F600}'",
            sp,
        )]);
    }

    let ch = if state.current() == Some(b'\\') {
        // Escape sequence
        match lex_escape_sequence(state) {
            Ok(ch) => ch,
            Err(e) => return Err(vec![e]),
        }
    } else {
        // Regular character
        let ch = match state.peek_char() {
            Some(c) => c,
            None => {
                let sp = Span::new(
                    state.file, start_i, state.i + 1,
                    start_line, start_col, state.line, state.col + 1
                );
                return Err(vec![Diagnostic::error(
                    "L0212",
                    "Char literals can't contain an unescaped newline\n\nhelp: Close the ' before the newline or use \\n",
                    sp,
                )]);
            }
        };
        state.bump_char().unwrap(); // consume the scalar
        ch
    };

    // Must close with '
    if state.current() != Some(b'\'') {
        let sp = Span::new(state.file, start_i, state.i, start_line, start_col, state.line, state.col);
        return Err(vec![Diagnostic::error(
            "L0213",
            "Char literal must contain exactly one character\n\nhelp: Use a string \"...\" for multiple characters, or close with ' after one character",
            sp,
        )]);
    }

    state.advance(); // consume closing '\''

    let span = Span::new(state.file, start_i, state.i, start_line, start_col, state.line, state.col);
    state.tokens.push(Token::new(TokenKind::Char, span, Some(ch.to_string())));
    Ok(())
}

// ============================================================================
// Identifier and Special Tokens
// ============================================================================

fn lex_identifier(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    // Special case: "xx" operator
    if state.current() == Some(b'x') && state.peek(1) == Some(b'x') {
        let continues_ident = state.peek(2).map_or(false, |b| is_ident_continue(b));
        if !continues_ident {
            state.advance_by(2);
            state.tokens.push(Token::operator("xx".to_string(), state.span(start_i, start_col)));
            return Ok(());
        }
    }

    // Special case: standalone "_" as operator
    if state.current() == Some(b'_') {
        let continues_ident = state.peek(1).map_or(false, |b| is_ident_continue(b));
        if !continues_ident {
            state.advance();
            state.tokens.push(Token::simple_op("_".to_string(), state.span(start_i, start_col)));
            return Ok(());
        }
    }

    // Consume identifier
    state.advance();
    while state.current().map_or(false, is_ident_continue) {
        state.advance();
    }

    // Optional trailing ! or ?
    if state.current() == Some(b'!') || state.current() == Some(b'?') {
        if !(state.current() == Some(b'?') && 
             (state.peek(1) == Some(b'.') || state.peek(1) == Some(b'?'))) {
            state.advance();
        }
    }

    let name = String::from_utf8_lossy(&state.bytes[start_i..state.i]).into_owned();

    // Check for string prefixes
    if name == "raw" || name == "trim_lead" {
        if looks_ahead_to_string(state) {
            if name == "raw"       { state.pending_raw = true;       return Ok(()); }
            if name == "trim_lead" { state.pending_trim_lead = true; return Ok(()); }
        }
    }
    
    // Check for country-code money literal (e.g., US$30)
    if state.i - start_i == 2
       && state.bytes[start_i].is_ascii_uppercase()
       && state.bytes[start_i + 1].is_ascii_uppercase()
       && state.current() == Some(b'$')
    {
        // parse "CC$<number>" as a single Money token
        try_lex_money_suffix(state, start_i, start_col);
        return Ok(());
    }

    // Check for special keywords
    match name.as_str() {
        "blob" => {
            state.push_token(TokenKind::Blob, start_i, start_col, Some("blob".to_string()));
        }
        "date" => {
            state.push_token(TokenKind::Date, start_i, start_col, Some(name));
        }
        "time" => {
            state.push_token(TokenKind::Time, start_i, start_col, Some(name));
        }
        "datetime" => {
            state.push_token(TokenKind::DateTime, start_i, start_col, Some(name));
        }
        "act" => {
            state.push_token(TokenKind::Act, start_i, start_col, None);
        }
        "action" => {
            state.push_token(TokenKind::Action, start_i, start_col, None);
        }
        _ => {
            state.push_token(TokenKind::Ident, start_i, start_col, Some(name));
        }
    }

    Ok(())
}

fn looks_ahead_to_string(state: &LexerState) -> bool {
    let mut k = state.i;
    
    loop {
        if k >= state.bytes.len() || state.bytes[k] == b'\n' || state.bytes[k] == b'\r' {
            return false;
        }

        if state.bytes[k] == b' ' || state.bytes[k] == b'\t' {
            k += 1;
            continue;
        }

        if state.bytes[k] == b'"' || state.bytes[k] == b'\'' {
            return true;
        }

        if (state.bytes[k] == b'r' || state.bytes[k] == b'R') &&
           k + 1 < state.bytes.len() &&
           (state.bytes[k + 1] == b'"' || state.bytes[k + 1] == b'\'') {
            return true;
        }

        // Check for another prefix
        let mut end = k;
        if is_ident_start(state.bytes[k]) {
            end += 1;
            while end < state.bytes.len() && is_ident_continue(state.bytes[end]) {
                end += 1;
            }
            let txt = String::from_utf8_lossy(&state.bytes[k..end]);
            if txt == "raw" || txt == "trim_lead" {
                k = end;
                continue;
            }
        }

        return false;
    }
}

fn try_lex_money_suffix(state: &mut LexerState, start_i: usize, start_col: u32) -> Option<()> {
    // precondition: `state.i` points at '$'
    let mut j = state.i + 1;
    let mut jcol = state.col + 1;

    if j < state.bytes.len() && state.bytes[j] == b'-' { j += 1; jcol += 1; }

    let mut have_digit = false;
    while j < state.bytes.len() {
        match state.bytes[j] {
            b'0'..=b'9' => { have_digit = true; j += 1; jcol += 1; }
            b'_'        => { j += 1; jcol += 1; }
            _ => break,
        }
    }
    if j + 1 < state.bytes.len() && state.bytes[j] == b'.' && state.bytes[j + 1].is_ascii_digit() {
        j += 1; jcol += 1;
        while j < state.bytes.len() {
            match state.bytes[j] {
                b'0'..=b'9' => { have_digit = true; j += 1; jcol += 1; }
                b'_'        => { j += 1; jcol += 1; }
                _ => break,
            }
        }
    }
    if !have_digit { return None; }

    let span = Span::new(state.file, start_i, j, state.line, start_col, state.line, jcol);
    let canon = String::from_utf8_lossy(&state.bytes[start_i..j]).into_owned();
    state.tokens.push(Token::new(TokenKind::Money, span, Some(canon)));
    state.i = j;
    state.col = jcol;
    Some(())
}

fn lex_at_identifier(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    state.advance(); // consume '@'

    if state.i >= state.bytes.len() {
        let span = state.span(start_i, start_col);
        return Err(vec![Diagnostic::error(
            "L0402",
            "Expected a class name after `@`\n\nhelp: Start with an uppercase name: `@Player = username: \"john\" :: health: 100`",
            span,
        )]);
    }

    if !is_upper(state.current().unwrap()) {
        let span = Span::new(state.file, start_i, state.i + 1, state.line, start_col, state.line, state.col + 1);
        return Err(vec![Diagnostic::error(
            "L0402",
            "Expected a class name after `@`\n\nhelp: Class names start with uppercase: `@Player`, not `@player` or `@123`",
            span,
        )]);
    }

    state.advance(); // first uppercase letter
    while state.current().map_or(false, is_ident_continue) {
        state.advance();
    }

    let name = String::from_utf8_lossy(&state.bytes[(start_i + 1)..state.i]).into_owned();
    let span_name = state.span(start_i, start_col);

    if state.current() == Some(b'!') {
        state.tokens.push(Token::new(TokenKind::AtIdent, span_name, Some(name)));
        let bang_span = Span::new(state.file, state.i, state.i + 1, state.line, state.col, state.line, state.col + 1);
        state.tokens.push(Token::new(TokenKind::Op("!".to_string()), bang_span, None));
        state.advance();
        return Ok(());
    }

    if state.current() == Some(b'?') {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0403",
            "Names can't end with `?`\n\nhelp: Remove the suffix: write `@Player`",
            sp,
        )]);
    }

    state.tokens.push(Token::new(TokenKind::AtIdent, span_name, Some(name)));
    Ok(())
}

fn lex_hash_identifier(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    state.advance(); // consume '#'

    if !state.current().map_or(false, |b| is_alpha(b) || b == b'_') {
        let span = state.span(start_i, start_col);
        return Err(vec![Diagnostic::error(
            "L0401",
            "Expected a name after `#`\n\nhelp: Write `#tag` or remove the `#`; if you meant to write a comment, use `///` instead",
            span,
        )]);
    }

    state.advance();
    while state.current().map_or(false, is_ident_continue) {
        state.advance();
    }

    if state.current() == Some(b'!') || state.current() == Some(b'?') {
        let sp = state.span(state.i, state.col);
        return Err(vec![Diagnostic::error(
            "L0403",
            "Names can't end with `!` or `?`\n\nhelp: Remove the suffix: write `#tag`",
            sp,
        )]);
    }

    let name = String::from_utf8_lossy(&state.bytes[(start_i + 1)..state.i]).into_owned();
    state.push_token(TokenKind::HashIdent, start_i, start_col, Some(name));
    Ok(())
}

// ============================================================================
// Money Literals
// ============================================================================

fn lex_money_dollar(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    state.advance(); // consume '$'

    let mut j = state.i;
    let mut jcol = state.col;

    if j < state.bytes.len() && state.bytes[j] == b'-' {
        j += 1;
        jcol += 1;
    }

    let mut has_digit = false;
    while j < state.bytes.len() {
        match state.bytes[j] {
            b'0'..=b'9' => {
                has_digit = true;
                j += 1;
                jcol += 1;
            }
            b'_' => {
                j += 1;
                jcol += 1;
            }
            _ => break,
        }
    }

    if j + 1 < state.bytes.len() && state.bytes[j] == b'.' && state.bytes[j + 1].is_ascii_digit() {
        j += 1;
        jcol += 1;
        while j < state.bytes.len() {
            match state.bytes[j] {
                b'0'..=b'9' => {
                    has_digit = true;
                    j += 1;
                    jcol += 1;
                }
                b'_' => {
                    j += 1;
                    jcol += 1;
                }
                _ => break,
            }
        }
    }

    if has_digit {
        let canon = String::from_utf8_lossy(&state.bytes[start_i..j]).into_owned();
        let span = Span::new(state.file, start_i, j, state.line, start_col, state.line, jcol);
        state.tokens.push(Token::new(TokenKind::Money, span, Some(canon)));
        state.i = j;
        state.col = jcol;
    } else {
        let span = state.span(start_i, start_col);
        state.tokens.push(Token::simple_op("$".to_string(), span));
    }

    Ok(())
}

fn lex_money_unicode(state: &mut LexerState, symbol_len: usize) -> Result<(), Vec<Diagnostic>> {
    let start_i = state.i;
    let start_col = state.col;

    state.i += symbol_len;
    state.col += symbol_len as u32;

    if state.current() == Some(b'-') {
        state.advance();
    }

    let mut j = state.i;
    let mut jcol = state.col;
    let mut have_digit = false;

    while j < state.bytes.len() {
        match state.bytes[j] {
            b'0'..=b'9' => {
                have_digit = true;
                j += 1;
                jcol += 1;
            }
            b'_' => {
                j += 1;
                jcol += 1;
            }
            _ => break,
        }
    }

    if j + 1 < state.bytes.len() && state.bytes[j] == b'.' && state.bytes[j + 1].is_ascii_digit() {
        j += 1;
        jcol += 1;
        while j < state.bytes.len() {
            match state.bytes[j] {
                b'0'..=b'9' => {
                    have_digit = true;
                    j += 1;
                    jcol += 1;
                }
                b'_' => {
                    j += 1;
                    jcol += 1;
                }
                _ => break,
            }
        }
    }

    if have_digit {
        let span = Span::new(state.file, start_i, j, state.line, start_col, state.line, jcol);
        let canon = String::from_utf8_lossy(&state.bytes[start_i..j]).into_owned();
        state.tokens.push(Token::new(TokenKind::Money, span, Some(canon)));
        state.i = j;
        state.col = jcol;
    } else {
        state.i = start_i + 1;
        state.col = start_col + 1;
    }

    Ok(())
}

// ============================================================================
// Operator Parsing
// ============================================================================

fn try_match_operator<'a>(bytes: &[u8], i: usize, candidates: &'a [&'a str]) -> Option<&'a str> {
    candidates
        .iter()
        .filter(|op| i + op.len() <= bytes.len())
        .filter(|op| &bytes[i..i + op.len()] == op.as_bytes())
        .max_by_key(|op| op.len())
        .copied()
}

// ============================================================================
// Newline and Layout Handling
// ============================================================================

fn handle_newline(state: &mut LexerState) -> Result<(), Vec<Diagnostic>> {
    let nl_start = state.i;
    let nl_col = state.col;

    let consumed = if state.current() == Some(b'\r') && state.peek(1) == Some(b'\n') {
        2
    } else {
        1
    };
    state.i += consumed;
    let next_line_byte = state.i;

    if state.nest == 0 {
        // Check for dot-carry continuation
        let last_non_layout = state.tokens.iter().rfind(|t| {
            !matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent)
        });

        let can_carry = match last_non_layout {
            Some(t) => t.span.line_start == state.line,
            None => false,
        };

        if can_carry {
            let mut j = next_line_byte;
            while j < state.bytes.len() && (state.bytes[j] == b' ' || state.bytes[j] == b'\t') {
                j += 1;
            }

            if j < state.bytes.len() &&
               state.bytes[j] == b'.' &&
               j + 1 < state.bytes.len() &&
               (state.bytes[j + 1].is_ascii_alphabetic() || state.bytes[j + 1] == b'_') {
                state.line += 1;
                state.col = 1 + (j - next_line_byte) as u32;
                state.i = j;
                return Ok(());
            }
        }

        // Normal newline
        let span_nl = Span::new(state.file, nl_start, next_line_byte, state.line, nl_col, state.line, nl_col);
        state.tokens.push(Token::new(TokenKind::Newline, span_nl, None));

        let j = handle_indent_at_line_start(state, next_line_byte)?;
        state.line += 1;
        state.col = 1 + (j - next_line_byte) as u32;
        state.i = j;
    } else {
        // Inside brackets - implicit continuation
        state.line += 1;
        state.col = 1;
        while state.current() == Some(b' ') || state.current() == Some(b'\t') {
            state.advance();
        }
    }

    Ok(())
}

fn handle_indent_at_line_start(state: &mut LexerState, line_start: usize) -> Result<usize, Vec<Diagnostic>> {
    let mut j = line_start;
    let mut width: u32 = 0;
    // Tabs expand to 4 spaces (adjust if your old rule was different)
    while j < state.bytes.len() {
        match state.bytes[j] {
            b' ' => { j += 1; width += 1; }
            b'\t' => { j += 1; width = ((width / 4) + 1) * 4; }
            b'\r' | b'\n' => break,
            _ => break,
        }
    }

    let cur = *state.indent_stack.last().unwrap_or(&0);
    if width > cur {
        state.indent_stack.push(width);
        let sp = Span::new(state.file, line_start, j, state.line + 0, 1, state.line + 0, (j - line_start + 1) as u32);
        state.tokens.push(Token::new(TokenKind::Indent, sp, None));
    } else if width < cur {
        while let Some(top) = state.indent_stack.last().copied() {
            if top <= width { break; }
            state.indent_stack.pop();
            let sp = Span::new(state.file, line_start, j, state.line + 0, 1, state.line + 0, (j - line_start + 1) as u32);
            state.tokens.push(Token::new(TokenKind::Dedent, sp, None));
        }
    }
    Ok(j)
}

// ============================================================================
// Main Lexer Entry Point
// ============================================================================

pub fn lex(source: &str, file: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let mut state = LexerState::new(source, file);

    // Skip initial indentation
    if state.i < state.bytes.len() && state.current() != Some(b'\r') && state.current() != Some(b'\n') {
        let mut j = 0usize;
        let mut jcol = 1u32;

        while j < state.bytes.len() {
            match state.bytes[j] {
                b' ' | b'\t' => {
                    j += 1;
                    jcol += 1;
                }
                b'\r' | b'\n' => break,
                _ => break,
            }
        }

        state.i = j;
        state.col = jcol;
    }

    while state.i < state.bytes.len() {
        match state.current().unwrap() {
            b' ' | b'\t' => state.advance(),

            b'\r' | b'\n' => handle_newline(&mut state)?,

            // Comments
            b'/' if is_block_comment_open(state.bytes, state.i) => {
                consume_block_comment(&mut state)?;
            }
            b'/' if state.i + 2 < state.bytes.len() && state.peek(1) == Some(b'/') && state.peek(2) == Some(b'/') => {
                consume_line_comment(&mut state);
            }
            b'/' if state.i + 2 < state.bytes.len() && state.peek(1) == Some(b'/') && state.peek(2) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::operator("//=".to_string(), state.span(start_i, start_col)));
            }
            b'/' if state.peek(1) == Some(b'/') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::operator("//".to_string(), state.span(start_i, start_col)));
            }
            b'/' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::operator("/=".to_string(), state.span(start_i, start_col)));
            }
            b'/' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::operator("/".to_string(), state.span(start_i, start_col)));
            }

            // Numbers
            b'0'..=b'9' => lex_number(&mut state)?,

            // Leading dot float
            b'.' if state.peek(1).map_or(false, |b| b.is_ascii_digit()) => {
                lex_leading_dot_float(&mut state)?;
            }

            // Range operators and dot
            b'.' if state.peek(1) == Some(b'.') && state.peek(2) == Some(b'.') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::simple_op("...".to_string(), state.span(start_i, start_col)));
            }
            b'.' if state.peek(1) == Some(b'.') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("..".to_string(), state.span(start_i, start_col)));
            }
            b'.' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op(".".to_string(), state.span(start_i, start_col)));
            }

            // Colon operators
            b':' if state.peek(1) == Some(b':') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("::".to_string(), state.span(start_i, start_col)));
            }
            b':' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op(":".to_string(), state.span(start_i, start_col)));
            }

            // Raw strings and regular identifiers starting with 'r'
            b'r' | b'R' if state.peek(1) == Some(b'"') || state.peek(1) == Some(b'\'') => {
                state.pending_raw = true;
                state.advance(); // consume 'r'
                let this_raw = state.pending_raw;
                let this_trim = state.pending_trim_lead;
                state.pending_raw = false;
                state.pending_trim_lead = false;
                lex_string_literal(&mut state, this_raw, this_trim)?;
            }

            // String literals
            b'"' | b'\'' => {
                // Distinguish between char literals ('x') and string literals ("x" or '''..''')
                if state.current() == Some(b'\'') {
                    // Check if it's a triple-quoted string
                    if state.peek(1) == Some(b'\'') && state.peek(2) == Some(b'\'') {
                        // Triple-quoted string
                        let this_raw = state.pending_raw;
                        let this_trim = state.pending_trim_lead;
                        state.pending_raw = false;
                        state.pending_trim_lead = false;
                        lex_string_literal(&mut state, this_raw, this_trim)?;
                    } else {
                        // Single char literal 'x'
                        lex_char_literal(&mut state)?;
                    }
                } else {
                    // Double-quoted string
                    let this_raw = state.pending_raw;
                    let this_trim = state.pending_trim_lead;
                    state.pending_raw = false;
                    state.pending_trim_lead = false;
                    lex_string_literal(&mut state, this_raw, this_trim)?;
                }
            }

            // Money literals - Unicode symbols
            0xC2 if state.peek(1) == Some(0xA3) || state.peek(1) == Some(0xA5) => {
                lex_money_unicode(&mut state, 2)?;
            }
            0xE2 if state.peek(1) == Some(0x82) && state.peek(2) == Some(0xAC) => {
                lex_money_unicode(&mut state, 3)?;
            }
            b'$' => {
                lex_money_dollar(&mut state)?;
            }

            // Delimiters
            b'[' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::operator("[=".to_string(), state.span(start_i, start_col)));
            }
            b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' => {
                let start_i = state.i;
                let start_col = state.col;
                let ch = state.current().unwrap() as char;
                match ch {
                    '(' | '[' | '{' => state.nest += 1,
                    ')' | ']' | '}' => {
                        if state.nest > 0 {
                            state.nest -= 1;
                        }
                    }
                    _ => {}
                }
                state.advance();
                state.tokens.push(Token::simple_op(ch.to_string(), state.span(start_i, start_col)));
            }

            // Equals family
            b'=' if state.peek(1) == Some(b'=') && state.peek(2) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::simple_op("===".to_string(), state.span(start_i, start_col)));
            }
            b'=' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("==".to_string(), state.span(start_i, start_col)));
            }
            b'=' if state.peek(1) == Some(b'>') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("=>".to_string(), state.span(start_i, start_col)));
            }
            b'=' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("=".to_string(), state.span(start_i, start_col)));
            }

            // Not family
            b'!' if state.peek(1) == Some(b'=') && state.peek(2) == Some(b'=') && state.peek(3) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(4);
                state.tokens.push(Token::simple_op("!===".to_string(), state.span(start_i, start_col)));
            }
            b'!' if state.peek(1) == Some(b'=') && state.peek(2) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::simple_op("!==".to_string(), state.span(start_i, start_col)));
            }
            b'!' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("!=".to_string(), state.span(start_i, start_col)));
            }
            b'!' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("!".to_string(), state.span(start_i, start_col)));
            }

            // Less-than family and inline comments
            b'<' => {
                // Check for inline comments <--- or <----
                let is_four = state.i + 5 <= state.bytes.len() &&
                              &state.bytes[state.i + 1..state.i + 5] == b"----" &&
                              (state.i + 6 > state.bytes.len() || state.bytes[state.i + 5] != b'-');
                let is_three = !is_four &&
                               state.i + 4 <= state.bytes.len() &&
                               &state.bytes[state.i + 1..state.i + 4] == b"---" &&
                               (state.i + 5 > state.bytes.len() || state.bytes[state.i + 4] != b'-');

                if is_three || is_four {
                    let dashes = if is_four { 4 } else { 3 };
                    consume_inline_comment(&mut state, dashes)?;
                } else if state.peek(1) == Some(b'<') {
                    let start_i = state.i;
                    let start_col = state.col;
                    state.advance_by(2);
                    state.tokens.push(Token::simple_op("<<".to_string(), state.span(start_i, start_col)));
                } else if state.peek(1) == Some(b'=') {
                    let start_i = state.i;
                    let start_col = state.col;
                    state.advance_by(2);
                    state.tokens.push(Token::simple_op("<=".to_string(), state.span(start_i, start_col)));
                } else if state.peek(1) == Some(b'>') {
                    let start_i = state.i;
                    let start_col = state.col;
                    state.advance_by(2);
                    state.tokens.push(Token::simple_op("<>".to_string(), state.span(start_i, start_col)));
                } else {
                    let start_i = state.i;
                    let start_col = state.col;
                    state.advance();
                    state.tokens.push(Token::simple_op("<".to_string(), state.span(start_i, start_col)));
                }
            }

            // Greater-than family
            b'>' if state.peek(1) == Some(b'>') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op(">>".to_string(), state.span(start_i, start_col)));
            }
            b'>' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op(">=".to_string(), state.span(start_i, start_col)));
            }
            b'>' if state.peek(1) == Some(b'<') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("><".to_string(), state.span(start_i, start_col)));
            }
            b'>' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op(">".to_string(), state.span(start_i, start_col)));
            }

            // Ampersand family
            b'&' if state.peek(1) == Some(b'&') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::operator("&&".to_string(), state.span(start_i, start_col)));
            }
            b'&' if state.peek(1) == Some(b'=') => {
                let span = Span::new(state.file, state.i, state.i + 2, state.line, state.col, state.line, state.col + 2);
                return Err(vec![Diagnostic::error(
                    "L0112",
                    "There is no `&=` operator in Goblin\n\nhelp: For boolean-and assignment, write it explicitly: `x = x and y` or `x = x && y`. To test definedness, use unary `&`: `flag = &user>>name`",
                    span,
                )]);
            }
            b'&' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::operator("&".to_string(), state.span(start_i, start_col)));
            }

            // Asterisk family
            b'*' if state.peek(1) == Some(b'*') && state.peek(2) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::simple_op("**=".to_string(), state.span(start_i, start_col)));
            }
            b'*' if state.peek(1) == Some(b'*') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("**".to_string(), state.span(start_i, start_col)));
            }
            b'*' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("*=".to_string(), state.span(start_i, start_col)));
            }
            b'*' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("*".to_string(), state.span(start_i, start_col)));
            }

            // Plus family
            b'+' if state.peek(1) == Some(b'+') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("++".to_string(), state.span(start_i, start_col)));
            }
            b'+' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("+=".to_string(), state.span(start_i, start_col)));
            }
            b'+' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("+".to_string(), state.span(start_i, start_col)));
            }

            // Minus family
            b'-' if state.peek(1) == Some(b'-') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("--".to_string(), state.span(start_i, start_col)));
            }
            b'-' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("-=".to_string(), state.span(start_i, start_col)));
            }
            b'-' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("-".to_string(), state.span(start_i, start_col)));
            }

            // Percent family
            b'%' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("%=".to_string(), state.span(start_i, start_col)));
            }
            b'%' if state.i > 0 && state.bytes[state.i - 1].is_ascii_digit() => {
                let start_i = state.i;
                let start_col = state.col;
                if state.peek(1) == Some(b's') {
                    state.advance_by(2);
                    state.tokens.push(Token::simple_op("%s".to_string(), state.span(start_i, start_col)));
                } else if state.peek(1) == Some(b'o') {
                    state.advance_by(2);
                    state.tokens.push(Token::simple_op("%o".to_string(), state.span(start_i, start_col)));
                } else {
                    state.advance();
                    state.tokens.push(Token::simple_op("%".to_string(), state.span(start_i, start_col)));
                }
            }
            b'%' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("%".to_string(), state.span(start_i, start_col)));
            }

            // Caret family
            b'^' if state.peek(1) == Some(b'^') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("^^".to_string(), state.span(start_i, start_col)));
            }
            b'^' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("^=".to_string(), state.span(start_i, start_col)));
            }
            b'^' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("^".to_string(), state.span(start_i, start_col)));
            }

            // Tilde
            b'~' if state.peek(1) == Some(b'=') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("~=".to_string(), state.span(start_i, start_col)));
            }
            b'~' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("~".to_string(), state.span(start_i, start_col)));
            }

            // Pipe family
            b'|' if state.peek(1) == Some(b'|') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("||".to_string(), state.span(start_i, start_col)));
            }
            b'|' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("|".to_string(), state.span(start_i, start_col)));
            }

            // Question mark family
            b'?' if state.peek(1) == Some(b'?') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::simple_op("??".to_string(), state.span(start_i, start_col)));
            }
            b'?' if state.peek(1) == Some(b'>') && state.peek(2) == Some(b'>') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(3);
                state.tokens.push(Token::simple_op("?>>".to_string(), state.span(start_i, start_col)));
            }
            b'?' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op("?".to_string(), state.span(start_i, start_col)));
            }

            // Semicolon family
            b';' if state.peek(1) == Some(b';') => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance_by(2);
                state.tokens.push(Token::operator(";;".to_string(), state.span(start_i, start_col)));
            }
            b';' => {
                let start_i = state.i;
                let start_col = state.col;
                state.advance();
                state.tokens.push(Token::simple_op(";".to_string(), state.span(start_i, start_col)));
            }

            // @ and # identifiers
            b'@' => lex_at_identifier(&mut state)?,
            b'#' => lex_hash_identifier(&mut state)?,

            // Regular identifiers
            b if is_ident_start(b) => lex_identifier(&mut state)?,

            // Unknown byte
            _ => state.advance(),
        }
    }

    // Flush pending dedents
    let eof_span = Span::new(state.file, state.bytes.len(), state.bytes.len(), state.line, state.col, state.line, state.col);
    while state.indent_stack.len() > 1 {
        state.indent_stack.pop();
        state.tokens.push(Token::new(TokenKind::Dedent, eof_span.clone(), None));
    }

    // Final NEWLINE and EOF
    state.tokens.push(Token::new(TokenKind::Newline, eof_span.clone(), None));
    state.tokens.push(Token::new(TokenKind::Eof, eof_span, None));

    Ok(state.tokens)
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn emits_selected_ops_idents_numbers_and_eof() {
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

    assert!(has_op("="));
    assert!(has_op("=="));
    assert!(has_op("!="));
    assert!(has_op("==="));
    assert!(has_op("!=="));
    assert!(has_op("!==="));
    assert!(has_op("!"));

    assert_eq!(count_op("!==="), 1);
    assert_eq!(count_op("!=="), 1);
    assert_eq!(count_op("!="), 1);
    assert_eq!(count_op("==="), 1);
    assert_eq!(count_op("=="), 1);
    assert_eq!(count_op("="), 1);

    assert!(has_string("hi\n"));
    assert!(has_string("ok'"));
    assert!(has_string("A=A"));
    assert!(has_string("smile=😀"));
    assert!(has_string("hello\\nworld"));
    assert!(has_string("x\ny"));
    assert!(has_string(r"C:\Users\bob\file.txt"));
    assert!(has_string(r"raw\no\esc"));
    assert!(has_string("keep\\nbackslash\nand real newline"));
    assert!(has_string("x\\ny"));

    assert!(matches!(toks.last().unwrap().kind, TokenKind::Eof));
}

#[test]
fn lex_char_literals() {
    let src = "'a' '\\n' '\\t' '\\'' '\\\"' '\\\\' '😀'";
    let toks = lex(src, "test.gbln").unwrap();

    let has_char = |s: &str| {
        toks.iter()
            .any(|t| matches!(t, Token { kind: TokenKind::Char, value: Some(v), .. } if v == s))
    };

    assert!(has_char("a"));
    assert!(has_char("\n"));
    assert!(has_char("\t"));
    assert!(has_char("'"));
    assert!(has_char("\""));
    assert!(has_char("\\"));
    assert!(has_char("😀"));
}

#[test]
fn char_vs_string_disambiguation() {
    let src = "'a' \"a\" '''a''' \"\"\"a\"\"\"";
    let toks = lex(src, "test.gbln").unwrap();

    let char_count = toks.iter().filter(|t| matches!(t.kind, TokenKind::Char)).count();
    let string_count = toks.iter().filter(|t| matches!(t.kind, TokenKind::String)).count();

    assert_eq!(char_count, 1); // 'a'
    assert_eq!(string_count, 3); // "a", '''a''', """a"""
}