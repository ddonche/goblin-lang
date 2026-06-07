// ---- version = "0.20.1"

#![allow(dead_code)]
#![allow(unused_assignments)]
#![allow(unused_variables)]

/// Type-lock keywords recognized in `name.TYPE | value` declarations.
const TYPE_LOCK_KEYWORDS: &[&str] = &[
    "str", "bool",
    "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "f32", "f64",
    "big", "money", "pct",
    "date", "time", "datetime", "duration",
    "int", "uint", "float",
];

use goblin_ast::{self as ast, RelationDef};
use goblin_diagnostics::{Diagnostic, Span};
use goblin_lexer::{Token, TokenKind};
mod diagnostics_ext;
pub use diagnostics_ext::{s, derr, derr_help, derr_expected_found};
use goblin_ast::{SweepArm, SweepArmKind, SweepArmRepeat /*, ...*/};

/// Like `s_help_site!`, but appends the Rust source site: `path/file.rs:LINE`.
macro_rules! s_help_site {
    ($code:expr, $msg:expr, $help:expr $(,)?) => {{
        // Call s_help with an absolute path so call sites don't need an import.
        let base = crate::diagnostics_ext::s_help($code, $msg, $help);
        // Append parser source file/line for pinpointing where the error came from.
        format!("{base} [site {}:{}]", file!(), line!())
    }};
}

#[derive(Debug, Clone)]
enum PExpr {
    Array(Vec<PExpr>),
    Assign(Box<PExpr>, Box<PExpr>),
    Binary(Box<PExpr>, String, Box<PExpr>),
    BlobStr(String), 
    BlobNum(String), 
    Bool(bool),
    Call(Box<PExpr>, String, Vec<PExpr>),
    Char(char),
    Date(String),
    DateTime { value: String, tz: Option<String> },
    Float(String),
    FloatWithUnit(String, String),
    FreeCall(String, Vec<PExpr>),
    Ident(String),
    Index(Box<PExpr>, Box<PExpr>),
    IndexMap(Box<PExpr>, Box<PExpr>),             // map{key}
    Index2(Box<PExpr>, Box<PExpr>, Box<PExpr>),  // grid[x, y]
    Int(String),
    IntWithUnit(String, String),
    IsBound(Box<PExpr>),
    Member(Box<PExpr>, String),
    Money(String),
    MutateAssign(Box<PExpr>, Box<PExpr>),
    Nil,
    NsCall(String, String, Vec<PExpr>),
    Object(Vec<(String, PExpr)>),
    OptCall(Box<PExpr>, String, Vec<PExpr>),
    OptMember(Box<PExpr>, String),
    Postfix(Box<PExpr>, String),
    Prefix(String, Box<PExpr>),
    Slice(Box<PExpr>, Option<Box<PExpr>>, Option<Box<PExpr>>),          
    Slice3(Box<PExpr>, Option<Box<PExpr>>, Option<Box<PExpr>>, Option<Box<PExpr>>),
    Str(String),
    StrInterp(Vec<StrPart>),
    Time(String),
    TupleAssign(Vec<String>, Box<PExpr>, Span),
    ClassDecl {
        name: String,
        fields: Vec<(String, PExpr, bool, bool, bool, Option<RelationDef>)>,
        actions: Vec<PAction>,
        decision: Option<Box<PDecisionDef>>,
        judge: Option<ast::JudgeStmt>,
        transitions: Vec<ast::TransitionDef>,
        capacity: Option<ast::CapacityDecl>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        fields: Option<Vec<(String, PExpr)>>,
    },
    Judge {
        using: Option<Box<PExpr>>,
        using_enum: Option<String>,
        header: Option<Box<PExpr>>,         
        pairs: Vec<(PExpr, Option<PExpr>)>,
    },
    JudgeAll {
        using: Option<Box<PExpr>>,
        using_enum: Option<String>,
        header: Option<Box<PExpr>>,         // NEW
        pairs: Vec<(PExpr, Option<PExpr>)>, // NEW
    },
    Block(Vec<ast::Expr>),
    TemplateApply {
        type_name: String,
        pairs: Vec<(String, PExpr)>,
        span: goblin_diagnostics::Span,
    },
    Dump {
        expr: Box<PExpr>,
        show_ids: bool,   // keep the flag; we’ll always false for now
    },
    Collect {
        count: Box<PExpr>,
        body:  Box<PExpr>,
    },
    LiteralToken {
        module: String,
        ident: String,
        span: Span,
    },
    BoxVar { namespace: String, name: String },
    ObjectMatrix {
        type_name: String,
        /// Column identifiers (the object names): USA, France, Russia
        columns: Vec<String>,
        /// One entry per row: (field_name, default_cell, per_column_cells)
        rows: Vec<MatrixRow>,
        span: Span,
    },
}

/// One row in an object matrix.
#[derive(Debug, Clone)]
struct MatrixRow {
    field: String,
    default: MatrixCell,
    cells: Vec<MatrixCell>,
}

/// A single cell value in a matrix.
#[derive(Debug, Clone)]
enum MatrixCell {
    /// `nc` or `::` — inherit the row default
    Nc,
    /// An actual expression
    Expr(PExpr),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumSuffix {
    I8, I16, I32, I64, I128, Isize,
    U8, U16, U32, U64, U128, Usize,
    F32, F64, Big, // Big = 'b' (big int/dec); only relevant to reject in int-range preview
}

#[inline]
fn split_numeric_suffix<'a>(s: &'a str) -> (&'a str, Option<NumSuffix>) {
    // Order matters: longest first
    if let Some(rest) = s.strip_suffix("isize") { return (rest, Some(NumSuffix::Isize)); }
    if let Some(rest) = s.strip_suffix("usize") { return (rest, Some(NumSuffix::Usize)); }
    if let Some(rest) = s.strip_suffix("i128")  { return (rest, Some(NumSuffix::I128)); }
    if let Some(rest) = s.strip_suffix("u128")  { return (rest, Some(NumSuffix::U128)); }
    if let Some(rest) = s.strip_suffix("i64")   { return (rest, Some(NumSuffix::I64)); }
    if let Some(rest) = s.strip_suffix("u64")   { return (rest, Some(NumSuffix::U64)); }
    if let Some(rest) = s.strip_suffix("i32")   { return (rest, Some(NumSuffix::I32)); }
    if let Some(rest) = s.strip_suffix("u32")   { return (rest, Some(NumSuffix::U32)); }
    if let Some(rest) = s.strip_suffix("i16")   { return (rest, Some(NumSuffix::I16)); }
    if let Some(rest) = s.strip_suffix("u16")   { return (rest, Some(NumSuffix::U16)); }
    if let Some(rest) = s.strip_suffix("i8")    { return (rest, Some(NumSuffix::I8));  }
    if let Some(rest) = s.strip_suffix("u8")    { return (rest, Some(NumSuffix::U8));  }
    if let Some(rest) = s.strip_suffix("f32")   { return (rest, Some(NumSuffix::F32)); }
    if let Some(rest) = s.strip_suffix("f64")   { return (rest, Some(NumSuffix::F64)); }
    if let Some(rest) = s.strip_suffix('b')     { return (rest, Some(NumSuffix::Big)); }
    (s, None)
}

#[inline]
fn strip_numeric_underscores(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        if ch != '_' { out.push(ch); }
    }
    out
}

/// Parse a *non-negative* integer literal string (possibly with base prefix and underscores)
/// into i128, **after** removing any numeric type suffix. Returns None if:
/// - it's marked Big (`...b`) or float-suffixed (`f32`/`f64`)
/// - value doesn't fit in i128 (including large unsigned)
#[inline]
fn parse_int_literal_to_i128(s: &str) -> Option<i128> {
    let (base, suf) = split_numeric_suffix(s);

    // Reject float or big suffixes in "integer" context
    match suf {
        Some(NumSuffix::F32) | Some(NumSuffix::F64) | Some(NumSuffix::Big) => return None,
        _ => {}
    }

    let base = strip_numeric_underscores(base);

    // Detect radix by prefix
    if let Some(hex) = base.strip_prefix("0x").or_else(|| base.strip_prefix("0X")) {
        let u = u128::from_str_radix(hex, 16).ok()?;
        i128::try_from(u).ok()
    } else if let Some(bin) = base.strip_prefix("0b").or_else(|| base.strip_prefix("0B")) {
        let u = u128::from_str_radix(bin, 2).ok()?;
        i128::try_from(u).ok()
    } else if let Some(oct) = base.strip_prefix("0o").or_else(|| base.strip_prefix("0O")) {
        let u = u128::from_str_radix(oct, 8).ok()?;
        i128::try_from(u).ok()
    } else {
        // Decimal
        // Try unsigned first (to permit large u* that still fit i128)
        if let Ok(u) = base.parse::<u128>() {
            return i128::try_from(u).ok();
        }
        // Fall back to signed (still non-negative text)
        base.parse::<i128>().ok()
    }
}

#[derive(Debug, Clone)]
struct PAction {
    name: String,
    params: Vec<(String, Option<PExpr>)>,
    body: Vec<ast::Stmt>,  // <-- Changed from Vec<PExpr>
    is_single: bool,
}

/// Parser-internal representation of a decision formula inside a class.
#[derive(Debug, Clone)]
struct PDecisionDef {
    target_class: String,
    formula: Box<PExpr>,
}

#[derive(Debug, Clone)]
struct PEnumDecl {
    name: String,
    variants: Vec<PEnumVariant>,
}

#[derive(Debug, Clone)]
struct PEnumVariant {
    name: String,
    fields: Vec<(String, Option<PExpr>)>,  // field name, optional default
}

#[derive(Debug, Clone)]
enum PDecl {
    Expr(PExpr),
    Action(PAction),
    Class { name: String, fields: Vec<(String, PExpr, bool, bool, bool, Option<RelationDef>)>, actions: Vec<PAction>, decision: Option<Box<PDecisionDef>>, judge: Option<ast::JudgeStmt>, transitions: Vec<ast::TransitionDef>, capacity: Option<ast::CapacityDecl> },
    Enum(PEnumDecl),
    /// Expands into multiple object instantiations at lowering time.
    Matrix(PExpr),
}

#[derive(Clone, Debug)]
pub enum StrPart {
    Text(String),
    LValue {
        root: String,
        segments: Vec<LvSeg>,
        default_str: Option<String>,
    },
}

#[derive(Clone, Debug)]
pub enum LvSeg {
    Member { name: String, optional: bool }, // >>name or ?>>name
    IndexNumber(String),                     // [123]  (kept as string; validate later)
    IndexIdent(String),                      // [ident]
}

pub type ParseResult<T> = Result<T, Vec<Diagnostic>>;

pub struct Parser<'t> {
    toks: &'t [Token],
    i: usize,
    in_stmt: bool,
    block_closed_hard: bool,
    rec_depth: usize,
    suspend_colon_call: usize,
    in_object_construction: bool,
}

fn is_block_starter_name(name: &str) -> bool {
    matches!(name, "if" | "for" | "while" | "repeat" | "judge" | "judge_all")
}

impl<'t> Parser<'t> {
    pub fn new(toks: &'t [goblin_lexer::Token]) -> Self {
        Self {
            toks,
            i: 0,
            in_stmt: false,
            block_closed_hard: false,
            rec_depth: 0,
            suspend_colon_call: 0,
            in_object_construction: false,
        }
    } 

    const MAX_RECURSION: usize = 256;

    #[inline]
    fn with_depth<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, String>
    ) -> Result<T, String> {
        self.rec_depth += 1;
        if self.rec_depth > Self::MAX_RECURSION {
            self.rec_depth -= 1;
            return Err(s_help_site!(
                "P1001",
                "This expression is too deeply nested and can't be parsed clearly.",
                "Split it into smaller sub-expressions on separate lines, then combine the results (use fewer layers of parentheses).",
            ));
        }

        let out = f(self);

        self.rec_depth -= 1;

        out
    }

    fn parse_literal_token(&mut self) -> Result<PExpr, String> {
        #[inline]
        fn is_op(tk: &Token, s: &str) -> bool {
            matches!(&tk.kind, TokenKind::Op(op) if op.as_str() == s)
        }

        // '{{{'
        if !(self.i < self.toks.len() && self.toks[self.i].kind == TokenKind::TripleBraceOpen) {
            return Err(s(
                "P0710",
                "bad-token-shape: expected '{{{' at start of token\nhelp: Write tokens like: {{{BRINDLE::OBSIDIAN}}}",
            ));
        }
        self.i += 1;

        // MODULE
        if self.i >= self.toks.len() || self.toks[self.i].kind != TokenKind::Ident {
            return Err(s(
                "P0710",
                "bad-token-shape: expected MODULE identifier\nhelp: Write tokens like: {{{BRINDLE::OBSIDIAN}}}",
            ));
        }
        let module = self.toks[self.i].value.clone().unwrap_or_default();
        if module.is_empty() {
            return Err(s("P0710", "bad-token-shape: empty MODULE name"));
        }
        self.i += 1;

        // '::' — accept either Op("::") or two Op(":")
        if self.i < self.toks.len() && is_op(&self.toks[self.i], "::") {
            self.i += 1;
        } else {
            for _ in 0..2 {
                if !(self.i < self.toks.len() && is_op(&self.toks[self.i], ":")) {
                    return Err(s(
                        "P0710",
                        "bad-token-shape: expected '::' after MODULE\nhelp: Write tokens like: {{{BRINDLE::OBSIDIAN}}}",
                    ));
                }
                self.i += 1;
            }
        }

        // IDENT
        if self.i >= self.toks.len() || self.toks[self.i].kind != TokenKind::Ident {
            return Err(s(
                "P0710",
                "bad-token-shape: expected IDENT after '::'\nhelp: Write tokens like: {{{BRINDLE::OBSIDIAN}}}",
            ));
        }
        let ident = self.toks[self.i].value.clone().unwrap_or_default();
        if ident.is_empty() {
            return Err(s("P0710", "bad-token-shape: empty IDENT in token"));
        }
        self.i += 1;

        // '}}}'
        if self.i >= self.toks.len() || self.toks[self.i].kind != TokenKind::TripleBraceClose {
            return Err(s(
                "P0711",
                "unclosed-triple-brace: missing '}}}'\nhelp: Close tokens like: {{{MODULE::IDENT}}}",
            ));
        }
        let span = self.toks[self.i].span.clone(); // clone to avoid moving out
        self.i += 1;

        Ok(PExpr::LiteralToken { module, ident, span })
    }

    fn validate_interpolation_braces(&self, s: &str) -> Result<(), String> {
        let bytes = s.as_bytes();
        let n = bytes.len();
        let mut i: usize = 0;

        while i < n {
            // Treat backslash escapes as opaque (skip next char)
            if bytes[i] == b'\\' {
                if i + 1 < n {
                    // handle \u{...} / \xNN superficially so we don't misread braces inside
                    match bytes[i + 1] {
                        b'u' => {
                            // skip \u{ ... } if present
                            let mut k = i + 2;
                            if k < n && bytes[k] == b'{' {
                                k += 1;
                                while k < n && bytes[k] != b'}' { k += 1; }
                                if k < n && bytes[k] == b'}' {
                                    i = k + 1;
                                    continue;
                                }
                            }
                            i += 2;
                            continue;
                        }
                        b'x' => {
                            i = (i + 4).min(n);
                            continue;
                        }
                        _ => {
                            i += 2;
                            continue;
                        }
                    }
                } else {
                    i += 1;
                    continue;
                }
            }

            // ONLY validate triple-brace tokens {{{ ... }}}
            if i + 2 < n && &bytes[i..i + 3] == b"{{{" {
                // find next }}} (not escaped)
                let mut j = i + 3;
                let mut found = false;
                while j + 2 < n {
                    // skip escapes inside token body
                    if bytes[j] == b'\\' {
                        j = (j + 2).min(n);
                        continue;
                    }
                    if &bytes[j..j + 3] == b"}}}" {
                        found = true;
                        break;
                    }
                    j += 1;
                }
                if !found {
                    return Err(s_help_site!(
                        "P0605",
                        "Unclosed module/glam token '{{{ ... }}}' in string",
                        "Close the token with '}}}' like '{{{BRINDLE::OBSIDIAN}}}'",
                    ));
                }
                i = j + 3;
                continue;
            }

            // Everything else (including single braces) is parser-OK; runtime decides.
            i += 1;
        }

        Ok(())
    }

    fn parse_local_bind(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        // Record span start before consuming tokens
        let start_i = self.i;

        // 1) consume 'local' (we already peeked it)
        let local_text = self.eat_ident().ok_or_else(|| s_help_site!(
            "P0300",
            "Expected the keyword 'local'",
            "Write: local name | value"
        ))?;
        debug_assert_eq!(local_text.as_str(), "local");

        // 2) expect variable name (identifier)
        let name_span = match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::Ident) => t.span.clone(),
            _ => {
                return Err(s_help_site!(
                    "P0301",
                    "Expected an identifier after 'local'",
                    "Write: local name | value"
                ));
            }
        };
        let name_text = self.eat_ident().unwrap(); // safe after the match
        let name_ident: ast::Ident = (name_text, name_span);

        // 3) expect '|'
        match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::Op(ref s) if s == "|") => { self.i += 1; }
            _ => {
                return Err(s_help_site!(
                    "P0302",
                    "Expected '|' after the local name",
                    "Write: local name | value"
                ));
            }
        }

        // 4) parse initializer expression
        let init_pexpr = self.parse_assign()?;
        let init_expr = self.lower_expr(init_pexpr);

        // 5) statement span and node
        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::Bind(ast::BindStmt {
            name: name_ident,
            expr: init_expr,
            is_imm: false,
            is_local: true,
            mode: ast::BindMode::Tether,
            span,
            class_name: None,
            lock_type: None,
        }))
    }

    // Forbid `{` immediately after a header on the SAME line.
    // `{` remains allowed only in expressions (maps/interpolation).
    fn forbid_brace_after_header(&self, header_line: u32) -> Result<(), String> {
        if let Some(t) = self.toks.get(self.i) {
            if t.span.line_start == header_line && t.value.as_deref() == Some("{") {
                return Err(s_help_site!(
                    "P0205",
                    "Blocks use layout, not braces.",
                    "Start the block on the next line and close with 'end' or 'xx' (crossbones).",
                ));
            }
        }
        Ok(())
    }

    // --- SHIMS: keep old call sites working ---
    fn parse_primary(&mut self) -> Result<PExpr, String> {
        self.parse_primary_impl()
    }

    fn expand_condition(subject: &PExpr, cond: &PExpr, using_enum: &Option<String>) -> PExpr {
        match cond {
            // Binary with empty LHS placeholder: "" >= 90  =>  subject >= 90
            PExpr::Binary(lhs, op, rhs) if matches!(lhs.as_ref(), PExpr::Ident(s) if s.is_empty()) => {
                PExpr::Binary(Box::new(subject.clone()), op.clone(), rhs.clone())
            }
            
            // Bare identifier with enum context: idle  =>  subject == Enum::idle
            PExpr::Ident(variant_name) if using_enum.is_some() && variant_name != "else" => {
                let enum_name = using_enum.as_ref().unwrap();
                // Create: subject == Enum::variant
                let enum_variant = PExpr::NsCall(
                    enum_name.clone(),
                    variant_name.clone(),
                    vec![]
                );
                PExpr::Binary(
                    Box::new(subject.clone()),
                    "==".to_string(),
                    Box::new(enum_variant)
                )
            }
            
            // Already has LHS or else: just return as-is
            _ => cond.clone()
        }
    }

    fn parse_assign(&mut self) -> Result<PExpr, String> {
        // Assignment expressions are removed from the AST.
        // Keep parse_assign as a compatibility entry-point for callers,
        // but do not parse '=', '|=', '|!', tuple-assign, etc.
        self.parse_coalesce()
    }

    fn is_const_ident(name: &str) -> bool {
        !name.is_empty()
        && name.chars().all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
        && name.chars().next().unwrap().is_ascii_uppercase()
    }

    fn parse_template_pairs_flex(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut pairs: Vec<(String, PExpr)> = Vec::new();

        // At least one unit (pair or placeholder) expected
        self.parse_one_pair_or_skip_into(&mut pairs)?;

        loop {
            // Accept either '::' or ',' as a separator (mix-and-match allowed)
            let sep = if self.eat_op("::") {
                Some("::")
            } else if self.eat_op(",") {
                Some(",")
            } else {
                None
            };
            if sep.is_none() {
                break; // no more separators → done
            }

            // Optional skip units: allow empty '::' (already handled by just continuing)
            // and allow explicit 'nc' between separators.
            // If the next token clearly starts a pair/skip, parse it; otherwise allow trailing sep.
            if self.peek_starts_pair_or_skip() {
                self.parse_one_pair_or_skip_into(&mut pairs)?;
            } else {
                // trailing separator -> treat as an empty skip (OK), keep going
                continue;
            }
        }

        Ok(pairs)
    }

    fn peek_starts_pair_or_skip(&mut self) -> bool {
        use goblin_lexer::TokenKind;
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Ident))
    }

    fn parse_one_pair_or_skip_into(&mut self, pairs: &mut Vec<(String, PExpr)>) -> Result<(), String> {
        // ident expected: either a field name or 'nc' (case-insensitive)
        let Some(head) = self.eat_ident() else {
            return Err(s_help_site!("P0911", "Expected a field name or 'nc' here", "Write: name: value, or use 'nc' to skip"));
        };

        if head.eq_ignore_ascii_case("nc") {
            // placeholder skip; must not be followed by ':'
            if self.peek_op(":") {
                return Err(s_help_site!(
                    "P0916",
                    "'nc' is a placeholder; don't write 'nc:'.",
                    "Use 'nc' by itself between separators, e.g., :: nc :: .",
                ));
            }
            return Ok(());
        }

        if !self.eat_op(":") {
            return Err(s_help_site!("P0911", "Expected ':' after field name", "Write: name: value"));
        }

        let val = self.parse_assign()?; // full expr allowed on RHS
        pairs.push((head, val));
        Ok(())
    }

    fn parse_coalesce(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_or())?;
        while self.eat_op("??") {
            let rhs = self.with_depth(|p| p.parse_or())?;
            lhs = PExpr::Binary(Box::new(lhs), "??".into(), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn ensure_progress(&mut self, start_i: usize, context: &str) -> Result<(), String> {
        if self.i <= start_i {
            return Err(s_help_site!(
                "P0101",
                &format!(
                    "The parser got stuck and can't continue near token {} in {}.",
                    start_i, context
                ),
                "Check for a missing 'end' or 'xx' (crossbones) above, or an unfinished string. Close the block with 'end' or 'xx', or finish the string: \"name\".",
            ));
        }
        Ok(())
    }

    // Forbid a '{' immediately after a block header on the SAME line.
    // Braces are only allowed in expressions (maps/interpolation), not to open statement blocks.
    fn enforce_inline_brace_policy(&self, header_line: u32, _kw: &str) -> Result<(), String> {
        if let Some(t) = self.toks.get(self.i) {
            if t.span.line_start == header_line {
                match &t.kind {
                    goblin_lexer::TokenKind::Op(op) if op == "{" => {
                        return Err(s_help_site!(
                            "P0205",
                            "Blocks use layout, not braces.",
                            "Start the block on the next line and close with 'end' or 'xx' (crossbones).",
                        ));
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    // After finishing the module parse, complain if anything except NEWLINE/EOF remains.
    fn ensure_no_trailing_tokens_after_parse(
        &mut self,
    ) -> Result<(), Vec<goblin_diagnostics::Diagnostic>> {
        // Consume any trailing newlines your parser leaves
        while let Some(t) = self.peek() {
            if matches!(t.kind, goblin_lexer::TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        match self.peek() {
            None => Ok(()),
            Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Eof) => Ok(()),
            Some(tok) => {
                let sp = tok.span.clone();
                let what = match &tok.kind {
                    goblin_lexer::TokenKind::Op(s)    => format!("operator '{}'", s),
                    goblin_lexer::TokenKind::Ident    => tok.value.clone().unwrap_or_else(|| "identifier".to_string()),
                    goblin_lexer::TokenKind::Int      => "integer literal".to_string(),
                    goblin_lexer::TokenKind::Float    => "float literal".to_string(),
                    goblin_lexer::TokenKind::String   => "string literal".to_string(),
                    goblin_lexer::TokenKind::Duration => "duration literal".to_string(),
                    goblin_lexer::TokenKind::Money    => "money literal".to_string(),
                    goblin_lexer::TokenKind::Date     => "date literal".to_string(),
                    goblin_lexer::TokenKind::Time     => "time literal".to_string(),
                    goblin_lexer::TokenKind::DateTime => "datetime literal".to_string(),
                    _                                  => format!("{:?}", tok.kind),
                };
                return Err(derr_help(
                    "P0102",
                    &format!("Found unexpected {} at the top level of your file", what),
                    "Remove it, or add the missing 'end' or 'xx' (crossbones) above to close the previous block",
                    sp,
                ));
            }
        }
    }

    fn span_from_tokens(toks: &[goblin_lexer::Token], start_i: usize, end_i: usize) -> Span {
        if toks.is_empty() {
            return Span::new("<empty>", 0, 0, 0, 0, 0, 0);
        }
        let start_tok = toks.get(start_i.min(toks.len() - 1));
        let end_tok = toks.get(end_i.min(toks.len() - 1));
        
        match (start_tok, end_tok) {
            (Some(s), Some(e)) => Span::new(
                s.span.file.clone(),
                s.span.start,
                e.span.end,
                s.span.line_start,
                e.span.line_end,
                s.span.col_start,
                e.span.col_end,
            ),
            (Some(s), None) => s.span.clone(),
            _ => Span::new("<unknown>", 0, 0, 0, 0, 0, 0),
        }
    }

    #[inline]
    /// Parse the restricted lvalue grammar used by unary `&` definedness checks.
    /// Accepts:  Ident
    ///         | LValue `>>` Ident
    ///         | LValue `>>` "string-key"
    ///         | LValue `?>>` Ident          (optional chaining)
    ///         | LValue `[` expr `]`         (indexing; allows nesting)
    fn parse_definedness_lvalue(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind as K;

        // allow newline(s) before the root after '&'
        self.skip_newlines();

        // Root must be an identifier
        let mut expr = if let Some(name) = self.eat_ident() {
            PExpr::Ident(name)
        } else {
            return Err(s_help_site!(
                "P0401",
                "You need a variable, field access, or array index after '&'",
                "Examples: &user, &user>>name, &items[0]",
            ));
        };

        // zero or more of: >>name | >>"key" | ?>>name | [expr]
        loop {
            // tolerate newlines between segments
            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }

            if self.eat_op(">>") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }

                if let Some(name) = self.eat_ident() {
                    expr = PExpr::Member(Box::new(expr), name);
                    continue;
                } else if let Some(key) = self.eat_string_lit() {
                    expr = PExpr::Member(Box::new(expr), key);
                    continue;
                } else {
                    return Err(s_help_site!(
                        "P0402",
                        "You need a field name or a quoted string after '>>'",
                        "Example: user >> name or config >> \"api-key\"",
                    ));
                }
            }

            if self.eat_op("?>>") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }

                let Some(name) = self.eat_ident() else {
                    return Err(s_help_site!(
                        "P0403",
                        "You need a field name after '?>>'",
                        "Example: user ?>> email",
                    ));
                };
                expr = PExpr::OptMember(Box::new(expr), name);
                continue;
            }

            if self.eat_op("[") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }

                if self.peek_op("]") {
                    return Err(s_help_site!(
                        "P0701",
                        "Brackets need an index or slice expression",
                        "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]",
                    ));
                }

                let idx = self.parse_coalesce()?;

                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }
                if !self.eat_op("]") {
                    return Err(s_help_site!(
                        "P0702",
                        "This index or slice is missing a closing ']'",
                        "Add ']' to close it: items[0] or data[1:5]",
                    ));
                }

                expr = PExpr::Index(Box::new(expr), Box::new(idx));
                continue;
            }

            break;
        }

        Ok(expr)
    }

    #[inline]
    fn key_expr_is_side_effect_free(e: &PExpr) -> bool {
        use PExpr::*;
        match e {
            // Disallow anything that could invoke user code or assign
            Call(..) | FreeCall(..) | NsCall(..) | OptCall(..) | Assign(..) | MutateAssign(..) | TupleAssign(..) => false,

            // Simple literals
            Money(_) | Ident(_) | Int(_) | Float(_) | IntWithUnit(_,_) | FloatWithUnit(_,_)
            | Bool(_) | Nil | Str(_) | Char(_) | BlobStr(_) | BlobNum(_) | Date(_) | Time(_)
            | DateTime { .. } | EnumVariant { .. } 
            | LiteralToken { .. }                                  // ← add this line
                => true,

            // Binary operations - recurse both sides
            Binary(l, _, r) => {
                Self::key_expr_is_side_effect_free(l.as_ref())
                    && Self::key_expr_is_side_effect_free(r.as_ref())
            }

            // Unary operations - recurse
            Prefix(_, x) | Postfix(x, _) | IsBound(x) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
            }

            Dump { expr, .. } => Self::key_expr_is_side_effect_free(expr.as_ref()),

            // Index and member access - recurse
            Index(x, y) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
                    && Self::key_expr_is_side_effect_free(y.as_ref())
            }
            IndexMap(x, y) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
                    && Self::key_expr_is_side_effect_free(y.as_ref())
            }
            Index2(x, a, b) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
                    && Self::key_expr_is_side_effect_free(a.as_ref())
                    && Self::key_expr_is_side_effect_free(b.as_ref())
            }
            Member(x, _) | OptMember(x, _) => Self::key_expr_is_side_effect_free(x.as_ref()),

            // Slice operations - check all parts
            Slice(x, a, b) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
                    && a.as_ref().map_or(true, |bx| Self::key_expr_is_side_effect_free(bx.as_ref()))
                    && b.as_ref().map_or(true, |bx| Self::key_expr_is_side_effect_free(bx.as_ref()))
            }
            Slice3(x, a, b, c) => {
                Self::key_expr_is_side_effect_free(x.as_ref())
                    && a.as_ref().map_or(true, |bx| Self::key_expr_is_side_effect_free(bx.as_ref()))
                    && b.as_ref().map_or(true, |bx| Self::key_expr_is_side_effect_free(bx.as_ref()))
                    && c.as_ref().map_or(true, |bx| Self::key_expr_is_side_effect_free(bx.as_ref()))
            }

            // Collections - check all elements
            Array(xs) => xs.iter().all(Self::key_expr_is_side_effect_free),
            Object(kvs) => kvs.iter().all(|(_, v)| Self::key_expr_is_side_effect_free(v)),

            // String interpolation - only safe if no expressions
            StrInterp(ps) => ps.iter().all(|p| matches!(p, StrPart::Text(_) | StrPart::LValue{..})),

            // Conservative defaults for complex constructs
            ClassDecl { .. } | TemplateApply { .. } | Judge { .. } | JudgeAll { .. } | Block(_) | ObjectMatrix { .. } | BoxVar { .. } | Collect { .. } => false,
        }
    }

    // Non-consuming version - checks if we'd hit a close, handles Dedent/Indent pairs
    fn peek_layout_until_close(&self, _opening_indent: u32) -> bool {
        let mut j = self.i;
        let mut saw_dedent = false;
        
        while let Some(t) = self.toks.get(j) {
            match t.kind {
                goblin_lexer::TokenKind::Newline => { 
                    j += 1; 
                    continue; 
                }
                goblin_lexer::TokenKind::Dedent => {
                    saw_dedent = true;
                    j += 1;
                    continue;
                }
                goblin_lexer::TokenKind::Indent => { 
                    // If we saw a dedent, this indent cancels it
                    if saw_dedent {
                        saw_dedent = false;
                    }
                    j += 1;
                    continue; 
                }
                _ => {
                    // Reached actual content - return whether we have unmatched dedent
                    return saw_dedent;
                }
            }
        }
        // EOF - return whether we saw dedent
        saw_dedent
    }
    // Consuming version - actually advances position
    fn eat_layout_until_close(&mut self, opening_indent: u32) -> bool {
        let mut closed = false;
        while let Some(t) = self.peek() {
            match t.kind {
                goblin_lexer::TokenKind::Newline => { self.i += 1; continue; }
                goblin_lexer::TokenKind::Dedent => {
                    self.i += 1;
                    closed = true;
                    break;
                }
                goblin_lexer::TokenKind::Indent => { self.i += 1; continue; }
                _ => break,
            }
        }
        closed
    }

    fn parse_indented_block(&mut self, start_col: u32, stop_keywords: &[&str]) -> Result<Vec<ast::Stmt>, String> {
        use goblin_lexer::TokenKind;
                
        let result = self.parse_stmt_block_until(|p: &mut Parser<'_>| {
            
            let mut j = p.i;
            while let Some(t) = p.toks.get(j) {
                if matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    j += 1;
                } else {
                    break;
                }
            }
            
            if let Some(t) = p.toks.get(j) {
                let tok_col = t.span.col_start;
                
                if tok_col == start_col {
                    match &t.kind {
                        TokenKind::Ident => stop_keywords.iter().any(|kw| t.value.as_deref() == Some(kw)),
                        TokenKind::Op(op) if op == "xx" => true,
                        _ => false,
                    }
                } else {
                    false
                }
            } else {
                true
            }
        });
        
        result
    }

    fn parse_format_args_pexpr_after_lparen(&mut self) -> Result<Vec<PExpr>, String> {
        // 1) DEC: must be an integer literal token
        let Some(tok) = self.peek() else {
            return Err(s_help_site!(
                "P05F1",
                "Expected a decimal count as the first argument to format(..)",
                "Example: num.format(2 , .)"
            ));
        };

        let dec_lit = if tok.kind == TokenKind::Int {
            let lit = tok.value.clone().unwrap_or_default();
            self.i += 1; // consume int
            PExpr::Int(lit)
        } else {
            return Err(s_help_site!(
                "P05F2",
                "format(..) requires an integer for the number of decimal places",
                "Use something like: format(2 , .)"
            ));
        };

        // If immediately ')', it's the 1-arg form
        if self.eat_op(")") {
            return Ok(vec![dec_lit]);
        }

        // Accept: ',', '.', '_', apostrophe ('''), or the identifier/string "none"
        let consume_thousands = |this: &mut Parser| -> Result<PExpr, String> {
            // First, handle operator separators without borrowing a token
            if this.peek_op(",") { this.i += 1; return Ok(PExpr::Char(',')); }
            if this.peek_op(".") { this.i += 1; return Ok(PExpr::Char('.')); }
            if this.peek_op("_") { this.i += 1; return Ok(PExpr::Char('_')); }
            // If your lexer emits a bare apostrophe as an op, uncomment:
            // if this.peek_op("'") { this.i += 1; return Ok(PExpr::Char('\'')); }

            // Identifier: `none`
            if let Some(t) = this.peek() {
                let is_none_ident = matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("none");
                if is_none_ident {
                    this.i += 1;
                    return Ok(PExpr::Str("none".into()));
                }
            }

            // String literals: ",", ".", "_", "'", "none"
            if let Some(t) = this.peek() {
                let is_string = matches!(t.kind, TokenKind::String);
                if is_string {
                    // clone the string BEFORE advancing, so we don't hold a borrow
                    let s = t.value.as_deref().unwrap_or_default().to_string();
                    this.i += 1;
                    return match s.as_str() {
                        ","    => Ok(PExpr::Char(',')),
                        "."    => Ok(PExpr::Char('.')),
                        "_"    => Ok(PExpr::Char('_')),
                        "'"    => Ok(PExpr::Char('\'')),
                        "none" => Ok(PExpr::Str("none".into())),
                        _ => Err(s_help_site!("P05F3",
                                        "format: unknown thousands separator",
                                        "Use ',', '.', '_', \"'\", or 'none'")),
                    };
                }
            }

            Err(s_help_site!(
                "P05F4",
                "Expected a thousands separator after the decimals in format(..)",
                "Use ',', '.', '_', \"'\", or 'none': format(2 , .)"
            ))
        };

        // Decimal marker: must be '.' or ','
        let consume_decimal = |this: &mut Parser| -> Result<PExpr, String> {
            // Operators first
            if this.peek_op(".") { this.i += 1; return Ok(PExpr::Char('.')); }
            if this.peek_op(",") { this.i += 1; return Ok(PExpr::Char(',')); }

            // String literals: "." or ","
            if let Some(t) = this.peek() {
                if matches!(t.kind, TokenKind::String) {
                    let s = t.value.as_deref().unwrap_or_default().to_string();
                    this.i += 1;
                    return match s.as_str() {
                        "." => Ok(PExpr::Char('.')),
                        "," => Ok(PExpr::Char(',')),
                        _ => Err(s_help_site!("P05F5",
                                        "format: decimal marker must be '.' or ','",
                                        "Example: format(2 , .)")),
                    };
                }
            }

            Err(s_help_site!(
                "P05F6",
                "Expected a decimal marker '.' or ',' after the thousands separator",
                "Example: format(2 , .)"
            ))
        };

        let sep_th = consume_thousands(self)?;
        let sep_dec = consume_decimal(self)?;

        if !self.eat_op(")") {
            return Err(s_help_site!(
                "P05F7",
                "Expected ')' to close format(..)",
                "Close the call: num.format(2 , .)"
            ));
        }

        Ok(vec![dec_lit, sep_th, sep_dec])
    }

    fn apply_postfix_ops(&mut self, mut expr: PExpr) -> PExpr {
        // decide if token after an op starts an expression; keeps "**" and "//" postfix
        // from stealing binary uses like `a ** 2` or `a // 2`
        let lookahead_starts_expr = |from: usize| -> bool {
            use goblin_lexer::TokenKind as K;
            let mut j = from;
            while matches!(self.toks.get(j), Some(t) if matches!(t.kind, K::Newline)) { j += 1; }
            match self.toks.get(j).map(|t| &t.kind) {
                Some(K::Ident)
                | Some(K::Int) | Some(K::Float) | Some(K::String)
                | Some(K::Blob) | Some(K::Date) | Some(K::Time) | Some(K::DateTime) => true,
                Some(K::Op(s)) if s == "(" || s == "[" || s == "{" || s == "+" || s == "-" || s == "!" || s == "&" => true,
                _ => false,
            }
        };

        let mut iterations = 0;
        loop {
            iterations += 1;
            if iterations > 1000 {
                panic!(
                    "P1002: I hit my safety limit while parsing this really long chain of operations at token {}.\n\nhelp: Break the chain into steps: temp = obj()[0]; result = temp.call()",
                    self.i
                );
            }

            let start_i = self.i;

            // postfix ** and // (disambiguate from binary by peeking next token)
            if self.peek_op("**") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("**"); expr = PExpr::Postfix(Box::new(expr), "**".into()); continue; }
            if self.peek_op("//") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("//"); expr = PExpr::Postfix(Box::new(expr), "//".into()); continue; }

            // other postfix ops
            if self.peek_op("++") {
                // postfix iff ++ is glued to the previous token (no whitespace)
                let is_postfix = if self.i > 0 {
                    let prev = &self.toks[self.i - 1]; // token that ended `expr`
                    let plus = &self.toks[self.i];     // the '++' token
                    prev.span.end == plus.span.start   // no space → postfix
                } else {
                    false
                };

                if is_postfix {
                    // consume as postfix increment and keep scanning for more postfix ops
                    self.i += 1; // eat '++'
                    expr = PExpr::Postfix(Box::new(expr), "++".into());
                    continue;
                } else {
                    // not postfix → it's binary concat; stop postfix parsing here
                    break; // let the infix level see and consume '++'
                }
            }
            if self.eat_op("--") { expr = PExpr::Postfix(Box::new(expr), "--".into()); continue; }
            if self.eat_op("?") {
                match expr {
                    PExpr::Member(obj, prop) => {
                        let new_prop = format!("is_{}", prop);
                        expr = PExpr::Member(obj, new_prop);
                    }
                    _ => {
                        panic!(
                            "P0XXX: The '?' postfix only works on member access like .string?\n\
                             help: Use '&variable' to check if a variable is defined, or '.property?' to check type"
                        );
                    }
                }
                continue;
            }
            if self.eat_op("!")  { expr = PExpr::Postfix(Box::new(expr), "!".into());   continue; }
            if self.eat_op("^")  { expr = PExpr::Postfix(Box::new(expr), "^".into());   continue; }
            if self.eat_op("_")  { expr = PExpr::Postfix(Box::new(expr), "_".into());   continue; }

            if self.i == start_i { break; }
        }
        expr
    }

    fn parse_interpolation_lvalue(&self, src: &str) -> Result<StrPart, String> {
        // LVALUE := ident ( (">>" | "?>>") ident | "[" INDEX "]" )* [ "??" STRING ]?
        // INDEX  := digits | ident
        let bytes = src.as_bytes();
        let mut i: usize = 0;
        let n = bytes.len();

        #[inline]
        fn skip_ws(bytes: &[u8], i: &mut usize) {
            while *i < bytes.len() {
                match bytes[*i] {
                    b' ' | b'\t' | b'\r' | b'\n' => *i += 1,
                    _ => break,
                }
            }
        }
        #[inline]
        fn peek(bytes: &[u8], i: usize, k: usize) -> Option<u8> {
            let idx = i + k;
            if idx < bytes.len() { Some(bytes[idx]) } else { None }
        }
        #[inline]
        fn eat(bytes: &[u8], i: &mut usize, c: u8) -> bool {
            if *i < bytes.len() && bytes[*i] == c { *i += 1; true } else { false }
        }
        #[inline]
        fn is_alpha(c: u8) -> bool {
            matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_')
        }
        #[inline]
        fn is_alnum(c: u8) -> bool {
            matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
        }
        fn parse_ident(bytes: &[u8], i: &mut usize) -> Result<String, String> {
            if *i >= bytes.len() || !is_alpha(bytes[*i]) {
                return Err(s_help_site!(
                    "P0103",
                    "Expected a name (identifier) here",
                    "Use a simple name starting with a letter: username or count",
                ));
            }
            let start = *i;
            *i += 1;
            while *i < bytes.len() && is_alnum(bytes[*i]) { *i += 1; }
            Ok(std::str::from_utf8(&bytes[start..*i]).unwrap().to_string())
        }

        skip_ws(bytes, &mut i);
        let root = parse_ident(bytes, &mut i)?;

        let mut segments: Vec<LvSeg> = Vec::new();
        let mut default_str: Option<String> = None;

        loop {
            skip_ws(bytes, &mut i);

            // default tail: ?? 'string' or ?? "string"
            if peek(bytes, i, 0) == Some(b'?') && peek(bytes, i, 1) == Some(b'?') {
                i += 2;
                skip_ws(bytes, &mut i);

                // accept either quote
                let quote = match peek(bytes, i, 0) {
                    Some(b'\'') | Some(b'"') => { let q = bytes[i]; i += 1; q }
                    _ => return Err(s_help_site!(
                        "P0601",
                        "After '??', start the default with a quote (' or \")",
                        "Example: name ?? \"Anonymous\"",
                    )),
                };

                let start = i;
                while i < n && bytes[i] != quote { i += 1; }
                if i >= n {
                    return Err(s_help_site!(
                        "P0602",
                        "The default string after '??' isn't closed",
                        "Add a matching quote to end it: name ?? \"guest\"",
                    ));
                }
                let value = std::str::from_utf8(&bytes[start..i]).unwrap().to_string();
                i += 1; // closing quote
                skip_ws(bytes, &mut i);
                if i != n {
                    return Err(s_help_site!(
                        "P0603",
                        "Only a single string is allowed after '??'",
                        "Remove any extra characters after the closing quote: name ?? \"guest\"",
                    ));
                }
                default_str = Some(value);
                break;
            }

            // optional member: ?>>name
            if peek(bytes, i, 0) == Some(b'?') && peek(bytes, i, 1) == Some(b'>') && peek(bytes, i, 2) == Some(b'>') {
                i += 3;
                skip_ws(bytes, &mut i);
                let name = parse_ident(bytes, &mut i)?;
                segments.push(LvSeg::Member { name, optional: true });
                continue;
            }

            // member: >>name
            if peek(bytes, i, 0) == Some(b'>') && peek(bytes, i, 1) == Some(b'>') {
                i += 2;
                skip_ws(bytes, &mut i);
                let name = parse_ident(bytes, &mut i)?;
                segments.push(LvSeg::Member { name, optional: false });
                continue;
            }

            // index: [number] or [ident]
            if eat(bytes, &mut i, b'[') {
                skip_ws(bytes, &mut i);
                if i >= n { return Err(s_help_site!("P0703", "The '[' starts an index but it isn't closed", "Add a matching ']' to complete the index: items[0]")); }

                // number?
                if i < n && bytes[i].is_ascii_digit() {
                    let start = i;
                    i += 1;
                    while i < n && (bytes[i].is_ascii_digit() || bytes[i] == b'_') { i += 1; }
                    let num = std::str::from_utf8(&bytes[start..i]).unwrap().to_string();
                    skip_ws(bytes, &mut i);
                    if !eat(bytes, &mut i, b']') { return Err(s_help_site!("P0704", "This numeric index is missing a closing ']'", "Add ']' to close it: items[0]")); }
                    segments.push(LvSeg::IndexNumber(num));
                    continue;
                }

                // ident?
                if i < n && is_alpha(bytes[i]) {
                    let name = parse_ident(bytes, &mut i)?;
                    skip_ws(bytes, &mut i);
                    if !eat(bytes, &mut i, b']') { return Err(s_help_site!("P0705", "This identifier index is missing a closing ']'", "Add ']' to close it: items[id]")); }
                    segments.push(LvSeg::IndexIdent(name));
                    continue;
                }

                return Err(s_help_site!(
                    "P0706",
                    "You need a number or a name inside '[ ]'",
                    "Examples: items[0] or data[key]",
                ));
            }

            // nothing more to consume
            break;
        }

        Ok(StrPart::LValue { root, segments, default_str })
    }

    fn fmt_tok(tok: &goblin_lexer::Token) -> String {
        use goblin_lexer::TokenKind as K;
        match &tok.kind {
            K::Ident    => format!("Ident({})", tok.value.as_deref().unwrap_or("")),
            K::Int      => format!("Int({})", tok.value.as_deref().unwrap_or("")),
            K::Float    => format!("Float({})", tok.value.as_deref().unwrap_or("")),
            K::String   => "String(...)".into(),
            K::Op(s)    => format!("Op({})", s),
            K::Newline  => "Newline".into(),
            K::Eof      => "EOF".into(),
            other       => format!("{:?}", other),
        }
    }

    fn got_here(&self) -> String {
        match self.toks.get(self.i) {
            Some(t) => Self::fmt_tok(t),
            None => "EOF".into(),
        }
    }

    fn err_expected_expr(&self, ctx: &str) -> String {
        let here = self.got_here();
        let prev = if self.i > 0 {
            Self::fmt_tok(&self.toks[self.i - 1])
        } else {
            "BOF".into()
        };
        if ctx.is_empty() {
            s_help_site!(
                "P1003",
                &format!("Expected an expression, but found {} after {}", here, prev),
                "Use a value, variable, or call: total = price * qty",
            )
        } else {
            s_help_site!(
                "P1003",
                &format!("Expected an expression {} but found {} after {}", ctx, here, prev),
                "Check for an end keyword inline somewhere like in an if statement",
            )
        }
    }

    #[inline]
    fn token_is_op(&self, tok: &goblin_lexer::Token, s: &str) -> bool {
        // In this codebase, operator tokens carry their exact lexeme in `value`.
        // Comparing the string is sufficient and avoids enum pattern issues.
        tok.value.as_deref() == Some(s)
    }

    #[inline]
    fn peek_is_object_key_start(&self) -> bool {
        if let Some(tok) = self.toks.get(self.i) {
            use goblin_lexer::TokenKind;
            matches!(
                tok.kind,
                TokenKind::Ident
                // If you allow quoted keys in class headers, include the string token too:
                // | TokenKind::Str
            )
        } else {
            false
        }
    }

    fn split_duration_lexeme(s: &str) -> Result<(String, String), String> {
        // Accept units: mo, s, m, h, d, w, y  (longest-match for "mo")
        if s.len() < 2 {
            return Err(s_help_site!(
                "P0901",
                "This duration format isn't valid",
                "Use formats like 5s, 10m, 2h, or 3d",
            ));
        }
        let (base, unit) = if s.ends_with("mo") {
            (&s[..s.len()-2], "mo")
        } else {
            // last char as unit
            let u = &s[s.len()-1..];
            (&s[..s.len()-1], u)
        };
        if base.is_empty() {
            return Err(s_help_site!(
                "P0901",
                "This duration format isn't valid",
                "Use formats like 5s, 10m, 2h, or 3d",
            ));
        }
        // Minimal unit validation
        match unit {
            "s" | "m" | "h" | "d" | "w" | "y" | "mo" => Ok((base.to_string(), unit.to_string())),
            _ => Err(s_help_site!(
                "P0902",
                &format!("'{}' isn't a valid time unit", unit),
                "Use s, m, h, d, w, y, or mo: 5m or 2h",
            )),
        }
    }

    /// Preview a literal list `[ ... ]` at the current index.
    /// Returns Some(len) if it is a well-formed list literal (0 allowed),
    /// or None if it's not a literal list here. Parser position is restored.
    fn preview_list_len(&mut self) -> Option<usize> {
        let save = self.i;

        self.skip_newlines();
        if !self.eat_op("[") {
            self.i = save;
            return None;
        }
        self.skip_newlines();

        // Empty list: []
        if self.eat_op("]") {
            self.i = save;
            return Some(0);
        }

        // Count elements separated by commas at top level
        let mut count = 0usize;
        loop {
            // Parse one element; if this fails, it's not a literal list → bail
            match self.parse_coalesce() {
                Ok(_) => {}
                Err(_) => {
                    self.i = save;
                    return None;
                }
            }
            count += 1;

            self.skip_newlines();
            if self.eat_op("]") {
                break;
            }
            if self.eat_op(",") {
                self.skip_newlines();
                continue;
            }

            // malformed list; not a clean literal
            self.i = save;
            return None;
        }

        self.i = save;
        Some(count)
    }

    fn preview_int_range(&mut self) -> Option<(bool, i128, i128)> {
        let save = self.i;

        // low endpoint
        self.skip_newlines();
        let lo = match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::Int) => {
                if let Some(ref s) = t.value {
                    if let Some(v) = parse_int_literal_to_i128(s) {
                        self.i += 1; v
                    } else { self.i = save; return None; }
                } else { self.i = save; return None; }
            }
            _ => { self.i = save; return None; }
        };

        // dots: prefer "..." over ".."
        let exclusive = if self.eat_op("...") {
            // CHANGED: "..." => inclusive range, so `exclusive = false`
            false
        } else if self.eat_op("..") {
            // CHANGED: ".."  => exclusive range, so `exclusive = true`
            true
        } else {
            self.i = save;
            return None;
        };

        // high endpoint
        self.skip_newlines();
        let hi = match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::Int) => {
                if let Some(ref s) = t.value {
                    if let Some(v) = parse_int_literal_to_i128(s) {
                        self.i += 1; v
                    } else { self.i = save; return None; }
                } else { self.i = save; return None; }
            }
            _ => { self.i = save; return None; }
        };

        self.i = save;
        Some((exclusive, lo, hi))
    }

    fn preview_char_range(&mut self) -> Option<(bool, char, char)> {
        let save = self.i;
        
        // low endpoint - single char string like "a"
        let lo = match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::String) => {
                if let Some(ref s) = t.value {
                    if s.len() == 1 {
                        let c = s.chars().next().unwrap();
                        self.i += 1;
                        c
                    } else {
                        self.i = save;
                        return None;
                    }
                } else {
                    self.i = save;
                    return None;
                }
            }
            _ => { self.i = save; return None; }
        };
        
        // dots: "..." or ".."
        let exclusive = if self.eat_op("...") {
            false
        } else if self.eat_op("..") {
            true
        } else {
            self.i = save;
            return None;
        };
        
        // high endpoint
        let hi = match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::String) => {
                if let Some(ref s) = t.value {
                    if s.len() == 1 {
                        let c = s.chars().next().unwrap();
                        self.i += 1;
                        c
                    } else {
                        self.i = save;
                        return None;
                    }
                } else {
                    self.i = save;
                    return None;
                }
            }
            _ => { self.i = save; return None; }
        };
        
        self.i = save;
        Some((exclusive, lo, hi))
    }

    fn lower_expr_preview(pe: PExpr, sp: Span) -> ast::Expr {
        match pe {
            // Basic literals and identifiers
            PExpr::Ident(name) => ast::Expr::Ident(name, sp),
            PExpr::Int(s)
            | PExpr::Float(s)
            | PExpr::IntWithUnit(s, _)
            | PExpr::FloatWithUnit(s, _) => ast::Expr::Number(s, sp),
            PExpr::Bool(b) => ast::Expr::Bool(b, sp),
            PExpr::Str(s) => ast::Expr::Str(s, sp),
            PExpr::Char(c) => ast::Expr::Char(c, sp),
            PExpr::Nil => ast::Expr::Nil(sp),
            PExpr::IsBound(inner) => {
                // The postfix '?' is only produced after an identifier in this grammar,
                // but we’ll be tolerant here.
                match *inner {
                    PExpr::Ident(name) => {
                        // emit: FreeCall("is_bound_name", [Str(name)])
                        ast::Expr::FreeCall(
                            "is_bound_name".into(),
                            vec![ast::Expr::Str(name, sp.clone())],
                            sp,
                        )
                    }
                    other => {
                        // If ever produced on a non-ident (shouldn’t happen), stringify as a fallback.
                        let text = format!("{:?}", other);
                        ast::Expr::FreeCall(
                            "is_bound_name".into(),
                            vec![ast::Expr::Str(text, sp.clone())],
                            sp,
                        )
                    }
                }
            }
            PExpr::LiteralToken { module, ident, span } => {
                ast::Expr::LiteralToken { module, ident, span }
            }
            PExpr::BoxVar { namespace, name } => ast::Expr::BoxVar { namespace, name, span: sp },
            // Collections
            PExpr::Array(items) => {
                let elems = items
                    .into_iter()
                    .map(|e| Self::lower_expr_preview(e, sp.clone()))
                    .collect();
                ast::Expr::Array(elems, sp)
            }
            PExpr::Object(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(k, v)| (k, Self::lower_expr_preview(v, sp.clone())))
                    .collect();
                ast::Expr::Object(fields, sp)
            }
            PExpr::EnumVariant { enum_name, variant_name, fields } => {
                ast::Expr::EnumVariant {
                    enum_name,
                    variant_name,
                    fields: fields.map(|flds| 
                        flds.into_iter()
                            .map(|(k, v)| (k, Self::lower_expr_preview(v, sp.clone())))
                            .collect()
                    ),
                    span: sp,
                }
            }

            PExpr::Judge { using, using_enum, header, pairs } => {
                let header_expr = header
                    .map(|h| Box::new(Self::lower_expr_preview(*h, sp.clone())));

                let arms = pairs
                    .into_iter()
                    .map(|(cond_expr, val_expr_opt)| {
                        let condition = if matches!(&cond_expr, PExpr::Ident(s) if s == "else") {
                            None
                        } else {
                            let expanded = if let Some(ref subj) = using {
                                Self::expand_condition(subj, &cond_expr, &using_enum)
                            } else {
                                cond_expr
                            };
                            Some(Box::new(Self::lower_expr_preview(expanded, sp.clone())))
                        };

                        let value = val_expr_opt.map(|v| {
                            Box::new(Self::lower_expr_preview(v, sp.clone()))
                        });

                        ast::JudgeArm {
                            condition,
                            value,
                            span: sp.clone(),
                        }
                    })
                    .collect();

                ast::Expr::Judge {
                    using: using.map(|u| Box::new(Self::lower_expr_preview(*u, sp.clone()))),
                    header: header_expr,
                    arms,
                    all: false,
                    span: sp,
                }
            }

            PExpr::JudgeAll { using, using_enum, header, pairs } => {
                let header_expr = header
                    .map(|h| Box::new(Self::lower_expr_preview(*h, sp.clone())));

                let arms = pairs
                    .into_iter()
                    .map(|(cond_expr, val_expr_opt)| {
                        let condition = if matches!(&cond_expr, PExpr::Ident(s) if s == "else") {
                            None
                        } else {
                            let expanded = if let Some(ref subj) = using {
                                Self::expand_condition(subj, &cond_expr, &using_enum)
                            } else {
                                cond_expr
                            };
                            Some(Box::new(Self::lower_expr_preview(expanded, sp.clone())))
                        };

                        let value = val_expr_opt.map(|v| {
                            Box::new(Self::lower_expr_preview(v, sp.clone()))
                        });

                        ast::JudgeArm {
                            condition,
                            value,
                            span: sp.clone(),
                        }
                    })
                    .collect();

                ast::Expr::Judge {
                    using: using.map(|u| Box::new(Self::lower_expr_preview(*u, sp.clone()))),
                    header: header_expr,
                    arms,
                    all: true,
                    span: sp,
                }
            }

            PExpr::Block(exprs) => {
                // exprs: Vec<ast::Expr>  →  stmts: Vec<ast::Stmt>
                let stmts: Vec<ast::Stmt> = exprs.into_iter()
                    .map(ast::Stmt::Expr)
                    .collect();

                ast::Expr::Block {
                    stmts,
                    span: sp,
                }
            }

            // Calls
            PExpr::Call(expr, name, args) => {
                let recv = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::Call(recv, name, args, sp)
            }
            PExpr::FreeCall(name, args) => {
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::FreeCall(name, args, sp)
            }
            PExpr::NsCall(ns, name, args) => {
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::NsCall(ns, name, args, sp)
            }
            PExpr::OptCall(expr, name, args) => {
                let recv = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let args = args
                    .into_iter()
                    .map(|a| Self::lower_expr_preview(a, sp.clone()))
                    .collect();
                ast::Expr::OptCall(recv, name, args, sp)
            }

            // Member and indexing
            PExpr::Member(expr, name) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Member(obj, name, sp)
            }
            PExpr::OptMember(expr, name) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::OptMember(obj, name, sp)
            }
            PExpr::Index(expr, idx) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let idx = Box::new(Self::lower_expr_preview(*idx, sp.clone()));
                ast::Expr::Index(obj, idx, sp)
            }
            PExpr::IndexMap(expr, key) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let key = Box::new(Self::lower_expr_preview(*key, sp.clone()));
                ast::Expr::IndexMap(obj, key, sp)
            }
            PExpr::Index2(expr, x, y) => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let x   = Box::new(Self::lower_expr_preview(*x, sp.clone()));
                let y   = Box::new(Self::lower_expr_preview(*y, sp.clone()));
                ast::Expr::Index2(obj, x, y, sp)
            }

            // Unary / postfix / binary / assignment
            PExpr::Prefix(op, expr) => {
                let expr = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Prefix(op, expr, sp)
            }
            PExpr::Dump { expr, show_ids } => {
                let obj = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                let op = if show_ids { "*>>:show_ids".to_string() } else { "*>>".to_string() };
                ast::Expr::Postfix(obj, op, sp)
            }
            PExpr::Postfix(expr, op) => {
                let expr = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Postfix(expr, op, sp)
            }
            PExpr::Binary(lhs, op, rhs) => {
                let lhs = Box::new(Self::lower_expr_preview(*lhs, sp.clone()));
                let rhs = Box::new(Self::lower_expr_preview(*rhs, sp.clone()));
                ast::Expr::Binary(lhs, op, rhs, sp)
            }

            // Template-style object construction: FreeCall("Type", [Object(pairs)], span)
            PExpr::TemplateApply { type_name, pairs, span } => {
                let pairs_ast: Vec<(String, ast::Expr)> = pairs
                    .into_iter()
                    .map(|(k, v)| (k, Self::lower_expr_preview(v, span.clone())))
                    .collect();

                ast::Expr::FreeCall(
                    type_name,
                    vec![ast::Expr::Object(pairs_ast, span.clone())],
                    span,
                )
            }

            PExpr::Slice(recv, start, end) => {
                let recv_e  = Box::new(Self::lower_expr_preview((*recv).clone(), sp.clone()));
                let start_e = start.as_ref()
                    .map(|x| Box::new(Self::lower_expr_preview((**x).clone(), sp.clone())));
                let end_e   = end.as_ref()
                    .map(|x| Box::new(Self::lower_expr_preview((**x).clone(), sp.clone())));
                ast::Expr::Slice(recv_e, start_e, end_e, sp.clone())
            }

            PExpr::Slice3(recv, start, end, step) => {
                let recv_e  = Box::new(Self::lower_expr_preview((*recv).clone(), sp.clone()));
                let start_e = start.as_ref()
                    .map(|x| Box::new(Self::lower_expr_preview((**x).clone(), sp.clone())));
                let end_e   = end.as_ref()
                    .map(|x| Box::new(Self::lower_expr_preview((**x).clone(), sp.clone())));
                let step_e  = step.as_ref()
                    .map(|x| Box::new(Self::lower_expr_preview((**x).clone(), sp.clone())));
                ast::Expr::Slice3(recv_e, start_e, end_e, step_e, sp.clone())
            }

            // Fallback for unimplemented constructs
            other => {
                let txt = format!("{:?}", other);
                ast::Expr::Ident(txt, sp)
            }
        }
    }

    fn lower_expr(&mut self, pe: PExpr) -> ast::Expr {
        let sp = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i);
        Self::lower_expr_preview(pe, sp)
    }

    fn try_parse_class_decl(&mut self) -> Option<Result<PExpr, String>> {
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::ClassIdent)) {
            return None;
        }
        Some(self.parse_class_decl())
    }

    fn skip_action_block(&mut self) -> Result<(), String> {
        let mut depth = 1;
        loop {
            if self.is_eof() {
                return Err(s_help_site!(
                    "P0204",
                    "I reached the end of the file, but this action block is still open",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
            let tok = self.peek().unwrap();
            // ...
            match &tok.kind {
                TokenKind::Ident if tok.value.as_deref() == Some("end") => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 { break; }
                    continue;
                }
                TokenKind::Op(op) if op == "xx" => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 { break; }
                    continue;
                }
                TokenKind::Op(op) if op == "{" => {
                    self.i += 1;
                    depth += 1;
                    continue;
                }
                TokenKind::Op(op) if op == "}" => {
                    self.i += 1;
                    depth -= 1;
                    if depth == 0 { break; }
                    continue;
                }
                _ => { self.i += 1; }
            }
        }
        Ok(())
    }

    fn parse_return_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        let start_i = self.i;
        // expect literal 'return'
        match self.peek() {
            Some(t) if matches!(t.kind, TokenKind::Ident) && self.peek_is_return() => {
                self.i += 1; // consume 'return'
            }
            _ => {
                return Err(s_help_site!(
                    "P0600",
                    "Internal parser error: parse_return_stmt called when next token is not 'return' or 'send'",
                    "Parser bug.",
                ));
            }
        }
        let mut values: Vec<ast::Expr> = Vec::new();
        // what counts as a stmt terminator
        let is_terminator = |tok: &goblin_lexer::Token| match &tok.kind {
            TokenKind::Newline | TokenKind::Eof | TokenKind::Dedent => true,
            TokenKind::Op(s) if s == "xx" => true,
            TokenKind::Ident => {
                if let Some(ref s) = tok.value {
                    matches!(s.as_str(), "else" | "end" | "elif")
                } else {
                    false
                }
            }
            _ => false,
        };
        // bare `return` is valid
        if let Some(tok) = self.peek() {
            if is_terminator(tok) {
                let span = Parser::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                return Ok(ast::Stmt::Return(ast::ReturnStmt { values, span }));
            }
        }
        // `return` expr (',' expr)*
        let first_pe = self.parse_assign().map_err(|_| s_help_site!(
            "P0603",
            "Invalid expression after 'return'",
            "Use `return expr` or `return a, b`.",
        ))?;
        let first = self.lower_expr(first_pe);
        values.push(first);
        
        while self.eat_op(",") {
            let expr_pe = self.parse_assign().map_err(|_| s_help_site!(
                "P0603",
                "Invalid expression in return list",
                "Separate expressions with commas, e.g., `return a+b, lower(name)`.",
            ))?;
            let expr = self.lower_expr(expr_pe);
            values.push(expr);
        }
        
        // after values, only terminators allowed; don't consume them here
        if let Some(tok) = self.peek() {
            if !is_terminator(tok) {
                return Err(s_help_site!(
                    "P0601",
                    "Invalid tokens after return values",
                    "End the line after `return expr` or `return a, b`.",
                ));
            }
        }
        
        let span = Parser::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::Return(ast::ReturnStmt { values, span }))
    }

    fn parse_field_chain_line_class(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut out = Vec::new();

        loop {
            // 1) Field key (class-strict: forbid 'nc')
            let Some(key) = self.eat_object_key() else {
                if out.is_empty() {
                    return Err(s_help_site!(
                        "P0903",
                        "Expected a field name in this class header",
                        "Add a field in the header: @Player = username: \"john\" :: health: 100",
                    ));
                } else {
                    break;
                }
            };

            if key.eq_ignore_ascii_case("nc") {
                return Err(s_help_site!(
                    "P1910",
                    "'nc' is not allowed in class headers.",
                    "Declare a real field: name: \"...\"",
                ));
            }

            // 2) Colon
            if !self.eat_op(":") {
                return Err(s_help_site!(
                    "P0904",
                    "You need a ':' after the field name",
                    "Write it like username: \"john\"",
                ));
            }

            // Allow layout after ':'
            while let Some(t) = self.peek() {
                match t.kind {
                    goblin_lexer::TokenKind::Newline
                    | goblin_lexer::TokenKind::Indent
                    | goblin_lexer::TokenKind::Dedent => { self.i += 1; }
                    _ => break,
                }
            }

            // 3) Scan this line to find end of this value
            let value_start = self.i;
            let value_line = match self.toks.get(value_start) {
                Some(t) => t.span.line_start,
                None => {
                    return Err(s_help_site!(
                        "P0906",
                        "Expected a value after ':'",
                        "Write it like username: \"john\"",
                    ));
                }
            };

            let mut depth_paren = 0i32;
            let mut depth_brack = 0i32;
            let mut depth_brace = 0i32;
            let mut sep_i: Option<usize> = None;

            let mut k = value_start;
            while let Some(tok) = self.toks.get(k) {
                if tok.span.line_start != value_line { break; } // stop at EOL

                use goblin_lexer::TokenKind;
                match &tok.kind {
                    // track nesting so commas inside ()/[]/{} are ignored
                    TokenKind::Op(op) if op == "(" => { depth_paren += 1; k += 1; continue; }
                    TokenKind::Op(op) if op == ")" => { depth_paren -= 1; k += 1; continue; }
                    TokenKind::Op(op) if op == "[" => { depth_brack += 1; k += 1; continue; }
                    TokenKind::Op(op) if op == "]" => { depth_brack -= 1; k += 1; continue; }
                    TokenKind::Op(op) if op == "{" => { depth_brace += 1; k += 1; continue; }
                    TokenKind::Op(op) if op == "}" => { depth_brace -= 1; k += 1; continue; }

                    // legal field separators at top level
                    TokenKind::Op(op)
                        if (op == "::" || op == ",")
                            && depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
                    {
                        sep_i = Some(k);
                        break;
                    }

                    // Missing separator: ident ':' later on same line
                    TokenKind::Ident
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 && k > value_start =>
                    {
                        if let Some(next) = self.toks.get(k + 1) {
                            if next.span.line_start == value_line
                                && matches!(next.kind, TokenKind::Op(ref c) if c == ":")
                            {
                                return Err(s_help_site!(
                                    "P0905",
                                    "Missing field separator between fields",
                                    "Separate fields with '::' or ',' e.g. username: \"john\" :: health: 100",
                                ));
                            }
                        }
                        k += 1;
                        continue;
                    }

                    _ => { k += 1; continue; }
                }
            }

            // 4) Value slice = [value_start .. end_i)
            let end_i = sep_i.unwrap_or(k);

            // 5) Parse only that slice to avoid swallowing separators
            let val = parse_expr_preview(&self.toks[value_start..end_i])?;
            out.push((key, val));

            // 6) Separator handling (strict: must have another field on SAME LINE)
            if let Some(si) = sep_i {
                self.i = si;

                // consume exactly one separator
                let _ = if self.peek_op("::") { self.eat_op("::") } else { self.eat_op(",") };

                // If another separator immediately follows on the same line → error
                if let Some(tok2) = self.toks.get(self.i) {
                    if tok2.span.line_start == value_line {
                        if let goblin_lexer::TokenKind::Op(op) = &tok2.kind {
                            if op == "::" || op == "," {
                                return Err(s_help_site!(
                                    "P0907",
                                    "Unexpected extra separator",
                                    "Use a single '::' or ',' between fields: name: \"Rook\" :: health: 100",
                                ));
                            }
                        }
                    }
                }

                // STRICT: require next token on same line to be Ident, not 'nc',
                // and ensure a ':' follows (also same line). Do NOT consume them here.
                let next = self.toks.get(self.i);
                match next {
                    Some(t) if t.span.line_start == value_line => {
                        if !matches!(t.kind, goblin_lexer::TokenKind::Ident) {
                            return Err(s_help_site!(
                                "P0914",
                                "Expected a field name after the separator in this class header.",
                                "Write: name: value, age: 0  (no empty '::' and no placeholders)",
                            ));
                        }
                        if t.value.as_deref().map(|s| s.eq_ignore_ascii_case("nc")).unwrap_or(false) {
                            return Err(s_help_site!(
                                "P1910",
                                "'nc' is not allowed in class headers.",
                                "Declare an explicit field: name: \"...\"",
                            ));
                        }
                        // look ahead for ':' on same line
                        let next2 = self.toks.get(self.i + 1);
                        if !(matches!(next2, Some(tt)
                            if tt.span.line_start == value_line
                            && matches!(tt.kind, goblin_lexer::TokenKind::Op(ref c) if c == ":")))
                        {
                            return Err(s_help_site!(
                                "P0904",
                                "You need a ':' after the field name",
                                "Write it like username: \"john\"",
                            ));
                        }
                    }
                    _ => {
                        // newline/EOF after separator → trailing separator is illegal in class headers
                        return Err(s_help_site!(
                            "P0915",
                            "Expected another field after the separator in this class header.",
                            "Write: name: value, age: 0  (no trailing '::' or ',')",
                        ));
                    }
                }

                continue;
            } else {
                self.i = end_i; // EOL/EOF ends header row
                break;
            }
        }

        Ok(out)
    }

    fn parse_object_field_chain_line(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        use goblin_lexer::TokenKind;

        let mut out = Vec::new();

        // If nothing follows (EOF), object provides no overrides
        let line = match self.toks.get(self.i) {
            Some(t) => t.span.line_start,
            None => return Ok(out),
        };

        loop {
            // 0) Consume any number of separators at the start (each empty slot = skip)
            while let Some(t) = self.toks.get(self.i) {
                if t.span.line_start != line { break; }
                if let TokenKind::Op(ref op) = t.kind {
                    if op == "::" || op == "," { self.i += 1; continue; }
                }
                break;
            }

            // End if we reached EOL/EOF
            let Some(t0) = self.toks.get(self.i) else { break; };
            if t0.span.line_start != line { break; }

            // 1) Bare 'nc' placeholder → skip (unless it's a real field 'nc:')
            if matches!(t0.kind, TokenKind::Ident)
                && t0.value.as_deref().map(|s| s.eq_ignore_ascii_case("nc")).unwrap_or(false)
            {
                let is_real_field = match self.toks.get(self.i + 1) {
                    Some(t1)
                        if t1.span.line_start == line
                        && matches!(t1.kind, TokenKind::Op(ref c) if c == ":") => true,
                    _ => false,
                };
                if !is_real_field {
                    self.i += 1; // consume bare 'nc' as skip
                    continue;
                }
            }

            // 2) Try a named field: key ':' expr
            let save_i = self.i;
            let Some(key) = self.eat_object_key() else {
                return Err(s_help_site!(
                    "O0901",
                    "Expected 'name: value', 'nc', or a separator in this object.",
                    "Use 'nc' or '::' to skip, or write a field like name: \"Fluffy\"",
                ));
            };

            if !self.eat_op(":") {
                // Not actually a pair; rewind and error
                self.i = save_i;
                return Err(s_help_site!(
                    "O0902",
                    "Expected ':' after the field name in this object.",
                    "Write: name: \"Fluffy\" or use 'nc' to skip",
                ));
            }

            // Allow layout after ':'
            while let Some(t) = self.peek() {
                match t.kind {
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => { self.i += 1; }
                    _ => break,
                }
            }

            // 3) Scan until next top-level ',' or '::' on the SAME line
            let value_start = self.i;
            let mut depth_paren = 0i32;
            let mut depth_brack = 0i32;
            let mut depth_brace = 0i32;
            let mut sep_i: Option<usize> = None;

            let mut k = value_start;
            while let Some(tok) = self.toks.get(k) {
                if tok.span.line_start != line { break; }

                match &tok.kind {
                    TokenKind::Op(op) if op == "(" => { depth_paren += 1; }
                    TokenKind::Op(op) if op == ")" => { depth_paren -= 1; }
                    TokenKind::Op(op) if op == "[" => { depth_brack += 1; }
                    TokenKind::Op(op) if op == "]" => { depth_brack -= 1; }
                    TokenKind::Op(op) if op == "{" => { depth_brace += 1; }
                    TokenKind::Op(op) if op == "}" => { depth_brace -= 1; }

                    // Unexpected junk token at top-level between fields (e.g., '$$$')
                    TokenKind::Op(op)
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 && k > value_start
                           && op != "::" && op != "," =>
                    {
                        return Err(s_help_site!(
                            "O0904",
                            &format!("Unexpected token '{}' between fields in this object.", op),
                            "Remove it or separate fields with '::' or ',': name: \"n\" :: species: \"cat\"",
                        ));
                    }

                    // NEW: starting a NEW FIELD without a separator → error
                    TokenKind::Ident
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 && k > value_start =>
                    {
                        if let Some(next) = self.toks.get(k + 1) {
                            if next.span.line_start == line
                                && matches!(next.kind, TokenKind::Op(ref c) if c == ":")
                            {
                                return Err(s_help_site!(
                                    "O0903",
                                    "Missing field separator between fields in this object.",
                                    "Separate fields with '::' or ',': name: \"n\" :: species: \"cat\"",
                                ));
                            }
                        }
                    }

                    // legal separators at top level
                    TokenKind::Op(op)
                        if (op == "::" || op == ",")
                            && depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
                    {
                        sep_i = Some(k);
                        break;
                    }

                    _ => {}
                }
                k += 1;
            }

            let end_i = sep_i.unwrap_or(k);

            // 4) Parse just that slice
            let val = parse_expr_preview(&self.toks[value_start..end_i])?;
            out.push((key, val));

            // 5) Advance to separator (if any) or to EOL and continue
            self.i = end_i;
            if sep_i.is_some() { continue; } else { break; }
        }

        Ok(out)
    }

    fn parse_action_after_keyword(&mut self, kw: &str) -> Result<PAction, String> {
        use goblin_lexer::TokenKind;

        // name
        let Some(name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P0501",
                &format!("You need to give your {} a name", kw),
                &format!("Write it like: {} Save", kw),
            ));
        };

        // (p1, p2 = expr, ...)
        let mut params: Vec<(String, Option<PExpr>)> = Vec::new();
        if self.eat_op("(") {
            if !self.peek_op(")") {
                loop {
                    let Some(pname) = self.eat_ident() else {
                        return Err(s_help_site!(
                            "P0502",
                            "Expected a parameter name",
                            "Use a simple identifier like username or count",
                        ));
                    };
                    // optional default: = <expr>
                    let default = if self.eat_op("|") {
                        Some(self.parse_coalesce()?)
                    } else {
                        None
                    };
                    params.push((pname, default));

                    if self.eat_op(",") {
                        if self.peek_op(")") { break; }
                        continue;
                    }
                    break;
                }
            }
            if !self.eat_op(")") {
                return Err(s_help_site!(
                    "P0503",
                    "Expected ')' to close the parameter list",
                    "Add the closing ')': action Save(username, level)",
                ));
            }
        }

        // single-line form (LEGACY):  act foo = expr
        // IMPORTANT: only fire when the *current* token is exactly "=" (NOT "=>")
        if let Some(tok) = self.toks.get(self.i) {
            if matches!(&tok.kind, TokenKind::Op(op) if op == "=") {
                // consume '=' exactly (do not use eat_op here to avoid swallowing '=>')
                self.i += 1;
                let expr = self.parse_coalesce()?;
                return Ok(PAction {
                    name,
                    params,
                    body: vec![ast::Stmt::Expr(self.lower_expr(expr))],
                    is_single: true,
                });
            }
        }

        // multi-line form: body parsed later by parse_free_action
        Ok(PAction { name, params, body: Vec::new(), is_single: false })
    }

    fn parse_free_action(&mut self, kw: &str) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        // ----- header location -----
        let hdr_tok_i = self.i.saturating_sub(1);
        let hdr_start = self.toks[hdr_tok_i].clone();
        let hdr_line  = hdr_start.span.line_start;
        let mut base_col: u32 = hdr_start.span.col_start;

        let mut k = hdr_tok_i;
        while k > 0 && self.toks[k - 1].span.line_start == hdr_line {
            k -= 1;
            base_col = base_col.min(self.toks[k].span.col_start);
        }

        // ----- parse header tail -----
        let action_start = self.i;
        let pa = self.parse_action_after_keyword(kw)?;
        let action_span = Self::span_from_tokens(self.toks, action_start, self.i.saturating_sub(1));

        // params
        let params: Vec<ast::Param> = pa.params.into_iter()
            .map(|(pname, def_pe)| ast::Param {
                name: pname,
                type_name: None,
                default: def_pe.map(|pe| self.lower_expr(pe)),
                span: action_span.clone(),
            })
            .collect();

        // ----- single-line  => expr -----
        let mut j = self.i;
        while let Some(tok) = self.toks.get(j) {
            match &tok.kind {
                TokenKind::Newline => { j += 1; continue; }
                TokenKind::Op(s) if s == ";" => { j += 1; continue; }
                _ => break,
            }
        }
        if let Some(tok) = self.toks.get(j) {
            if matches!(tok.kind, TokenKind::Op(ref s) if s == "=>") {
                self.i = j + 1;
                
                // Check if this is a return statement
                if let Some(ret_tok) = self.toks.get(self.i) {
                    if matches!(ret_tok.kind, TokenKind::Ident) && matches!(ret_tok.value.as_deref(), Some("return") | Some("send")) {
                        self.i += 1; // consume 'return'
                        let pexpr = self.parse_coalesce()?;
                        let expr = self.lower_expr(pexpr);
                        
                        // Create ReturnStmt properly
                        let ret_stmt = ast::Stmt::Return(ast::ReturnStmt {
                            values: vec![expr],
                            span: action_span.clone(),
                        });
                        let act = ast::ActionDecl {
                            name: pa.name,
                            params,
                            body: ast::ActionBody::Block(vec![ret_stmt]),
                            span: action_span,
                            ret: None,
                        };
                        return Ok(ast::Stmt::Action(act));
                    }
                }
                
                // Normal expression case
                let pexpr = self.parse_coalesce()?;
                let expr  = self.lower_expr(pexpr);
                let act = ast::ActionDecl {
                    name: pa.name,
                    params,
                    body: ast::ActionBody::Expr(expr),
                    span: action_span,
                    ret: None,
                };
                return Ok(ast::Stmt::Action(act));
            }
        }

        // ----- legacy single-line  = expr -----
        if !pa.body.is_empty() {
            let body_stmts: Vec<ast::Stmt> = pa.body.into_iter().collect();
            let act = ast::ActionDecl {
                name: pa.name,
                params,
                body: ast::ActionBody::Block(body_stmts),
                span: action_span,
                ret: None,
            };
            return Ok(ast::Stmt::Action(act));
        }

        // ----- block form (layout, no braces) -----
        if let Some(prev_tok) = self.toks.get(self.i.saturating_sub(1)) {
            let mut j = self.i;
            while let Some(tok) = self.toks.get(j) {
                match &tok.kind {
                    TokenKind::Newline => { j += 1; continue; }
                    TokenKind::Op(s) if s == ";" => { j += 1; continue; }
                    _ => break,
                }
            }
            if let Some(next) = self.toks.get(j) {
                if matches!(next.kind, TokenKind::Op(ref s) if s == "{")
                    && next.span.line_start > prev_tok.span.line_end
                {
                    return Err(s_help_site!(
                        "P0205",
                        "A '{' on a new line starts an object, not an action block",
                        "Blocks use layout, not braces. Start the block on the next line and close with 'end' or 'xx' (crossbones).",
                    ));
                }
            }
        }

        self.enforce_inline_brace_policy(hdr_line, kw)?;

        // NOTE: accept both closers
        let body_stmts = self.parse_indented_block(base_col, &["end", "xx"])?;

        self.skip_stmt_separators();

        if self.block_closed_hard {
            self.block_closed_hard = false;
        } else if self.peek_block_close() {
            // Only consume if aligned to (or left of) base_col to be robust to off-by-1
            let col: u32 = self
                .toks
                .get(self.i)
                .map(|t| t.span.col_start)
                .unwrap_or(u32::MAX);
            if col <= base_col {
                self.expect_block_close("action")?;
            } else {
                // fallthrough to dedent rule
            }
        } else if !self.eat_layout_until_close(base_col) {
            // ---- ALWAYS-ON DEBUG IN ERROR TEXT (no helpers) ----
            let next = self.toks.get(self.i);
            let prev = self.toks.get(self.i.saturating_sub(1));
            let fmt_tok = |t: Option<&goblin_lexer::Token>| -> String {
                match t {
                    Some(t) => format!(
                        "{:?}@{}:{}..{}:{}",
                        t.kind, t.span.line_start, t.span.col_start, t.span.line_end, t.span.col_end
                    ),
                    None => "<EOF>".to_string(),
                }
            };
            let dbg = format!(
                " [layout hdr_line={}, base_col={}, next={}, prev={}]",
                hdr_line, base_col, fmt_tok(next), fmt_tok(prev)
            );

            return Err(s_help_site!(
                "P0212",
                "This action block is missing its closing 'end' or 'xx' (crossbones).",
                "Close the block with 'end' or 'xx' (crossbones). [parse free action]"
            ));
        }

        let act = ast::ActionDecl {
            name: pa.name,
            params,
            body: ast::ActionBody::Block(body_stmts),
            span: action_span,
            ret: None,
        };
        Ok(ast::Stmt::Action(act))
    }

    #[inline]
    fn peek(&self) -> Option<&Token> { self.toks.get(self.i) }

    #[inline]
    fn peek_op(&self, s: &str) -> bool {
        if let Some(t) = self.peek() {
            if let goblin_lexer::TokenKind::Op(ref op) = t.kind {
                return op == s;
            }
        }
        false
    }

    #[inline]
    fn eat_op(&mut self, s: &str) -> bool {
        if self.peek_op(s) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    // === BEGIN: parse_bind_stmt (tuple targets + |= + fallback-to-expr for comma expressions) ===
    fn parse_bind_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        // 1) Optional 'imm'
        let is_const = if self.peek_word("imm") {
            let _ = self.eat_word("imm");
            true
        } else {
            false
        };

        // For fallback-to-expr, rewind to "start of expression" AFTER optional imm.
        // (imm is bind-only syntax; we do NOT want to treat "imm a, b + c" as an expr.)
        let start_i_expr = self.i;

        // 2) Parse one-or-more identifiers: a, b, c
        let mut names: Vec<ast::Ident> = Vec::new();

        // Tracks a type-lock suffix peeled from the first name (name.TYPE | value).
        let mut pending_lock_type: Option<String> = None;

        // first name
        {
            let Some(t) = self.peek().cloned() else {
                return Err(s_help_site!(
                    "P0401",
                    "Expected a name here",
                    "Write: name | expr, or: imm name | expr"
                ));
            };
            match t.kind {
                TokenKind::Ident => {
                    let text = t.value.clone().unwrap_or_default();
                    let sp = t.span.clone();
                    self.i += 1; // consume ident

                    // Lookahead: name.TYPE | value  →  typed tether
                    // Only peel if: next is '.', then a type keyword, then '|' (not '|=').
                    if self.peek_op(".") {
                        let save = self.i;
                        self.i += 1; // tentatively consume '.'
                        let type_word = self.peek()
                            .filter(|t| matches!(t.kind, TokenKind::Ident))
                            .and_then(|t| t.value.clone());
                        if let Some(ref tw) = type_word {
                            if TYPE_LOCK_KEYWORDS.contains(&tw.as_str()) {
                                self.i += 1; // consume the type keyword
                                // Next must be '|' (plain tether) — typed retether is not supported.
                                if self.peek_op("|") {
                                    pending_lock_type = Some(tw.clone());
                                    // Leave '|' for the mode-detection step below.
                                } else {
                                    self.i = save; // rewind: not a typed tether
                                }
                            } else {
                                self.i = save; // rewind: not a type keyword
                            }
                        } else {
                            self.i = save; // rewind: no ident after '.'
                        }
                    }

                    names.push((text, sp));
                }
                _ => {
                    return Err(s_help_site!(
                        "P0401",
                        "Expected a name here",
                        "Write: name | expr, or: imm name | expr"
                    ));
                }
            }
        }

        // additional names: ", name"
        self.skip_newlines();
        while self.eat_op(",") {
            self.skip_newlines();
            let Some(t) = self.peek().cloned() else {
                return Err(s_help_site!(
                    "P0404",
                    "Expected a name after ','",
                    "Write: a, b | expr"
                ));
            };
            match t.kind {
                TokenKind::Ident => {
                    let text = t.value.clone().unwrap_or_default();
                    let sp = t.span.clone();
                    self.i += 1; // consume ident
                    names.push((text, sp));
                }
                _ => {
                    return Err(s_help_site!(
                        "P0404",
                        "Expected a name after ','",
                        "Write: a, b | expr"
                    ));
                }
            }
            self.skip_newlines();
        }

        // Policy: imm + tuple bind is disallowed
        if is_const && names.len() > 1 {
            return Err(s_help_site!(
                "P0405",
                "‘imm’ is not allowed with multi-target binding",
                "Write: imm x | expr (single target), or: a, b | expr (without imm)"
            ));
        }

        // 3) Operator:
        //    - '|'  => Normal
        //    - '|=' => Retether
        //    - '[=' => Shadow
        //
        // CRITICAL FIX:
        // If we parsed multiple names AND the next token is NOT a bind op,
        // then this was NOT a bind statement (e.g. "g, h + i").
        // Rewind and parse as an expression statement instead.
        self.skip_newlines();

        let next_is_shadow = matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Shadow));
        let next_is_class_ident = matches!(self.peek().map(|t| &t.kind), Some(TokenKind::ClassIdent));
        let next_is_bind_op = self.peek_op("|") || self.peek_op("|=") || self.peek_op("<>") || next_is_shadow || next_is_class_ident;

        if !is_const && names.len() > 1 && !next_is_bind_op {
            // rewind to start of expression (after optional imm)
            self.i = start_i_expr;
            let pe = self.parse_coalesce()?;
            let e = self.lower_expr(pe);
            return Ok(ast::Stmt::Expr(e));
        }

        // Now actually consume the operator (or error)
        let (mode, op_span, is_object_construct) = if self.peek_op("|") {
            let sp = self.peek().unwrap().span.clone();
            let _ = self.eat_op("|");
            (ast::BindMode::Tether, sp, false)
        } else if self.peek_op("<>") {
            let sp = self.peek().unwrap().span.clone();
            let _ = self.eat_op("<>");
            (ast::BindMode::Tether, sp, true)
        } else if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::ClassIdent)) {
            let sp = self.peek().unwrap().span.clone();
            (ast::BindMode::Tether, sp, true)
        } else if self.peek_op("|=") {
            let sp = self.peek().unwrap().span.clone();
            let _ = self.eat_op("|=");
            (ast::BindMode::Retether, sp, false)
        } else if next_is_shadow {
            let sp = self.peek().unwrap().span.clone();
            self.i += 1;
            (ast::BindMode::Shadow, sp, false)
        } else {
            let name0 = &names[0].0;
            return Err(s_help_site!(
                "P0402",
                &format!("Expected '|', '<>', '|=', or '[=' after '{}'", name0),
                "Use '|' for a normal declaration, '<>' for object construction, '|=' to retether, or '[=' to shadow.",
            ));
        };

        // ---- Multi-target tuple bind ----
        if names.len() > 1 {
            // reject duplicates inside "a, a | expr"
            {
                use std::collections::HashSet;
                let mut seen = HashSet::new();
                for (n, _sp) in &names {
                    if !seen.insert(n.clone()) {
                        return Err(s_help_site!(
                            "P0407",
                            &format!("Duplicate name '{}' in multi-target binding", n),
                            "Each name in a multi-target binding must be unique.",
                        ));
                    }
                }
            }

            let rhs_pe = self.parse_coalesce()?;
            let rhs = self.lower_expr(rhs_pe);

            return Ok(ast::Stmt::TupleBind(ast::TupleBindStmt {
                names,
                expr: rhs,
                is_imm: is_const,     // rename variable later; for now keep it compiling
                is_local: false,      // because `local` is handled by parse_local_bind today
                mode,
                span: op_span,
            }));
        }

        // ---- EXISTING: Single-target path (preserve current behavior) ----
        let (name_text, name_span) = names.remove(0);

        // 3.5) Optional class constructor lookahead:
        //      name <> ClassName | fields
        // After consuming the bind operator, check if current token is a ClassIdent.
        let class_name = if is_object_construct {
            if let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::ClassIdent | TokenKind::Ident) {
                    let name_str = tok.value.clone().unwrap_or_default();
                    self.i += 1;

                    if !self.eat_op("|") {
                        return Err(s_help_site!(
                            "P0412",
                            "Expected '|' after class name in object construction",
                            r#"Write: rome <> Kingdom | name: "Rome", aggression: .7"#,
                        ));
                    }

                    Some(name_str)
                } else {
                    return Err(s_help_site!(
                        "P0411",
                        "Expected class name after '<>' in object construction",
                        r#"Write: rome <> Kingdom | name: "Rome", aggression: .7"#,
                    ));
                }
            } else {
                return Err(s_help_site!(
                    "P0411",
                    "Expected class name after '<>' in object construction",
                    r#"Write: rome <> Kingdom | name: "Rome", aggression: .7"#,
                ));
            }
        } else {
            None
        };

        // 4) RHS expression
        //
        // Support a bind-guard on the RHS:
        //   name | <expr> ?? => <stmt>
        // which should behave like:
        //   name | <expr>
        //   if name.nix? => <stmt>
        //
        // Key constraint: `parse_coalesce()` cannot parse `?? =>` because it expects
        // an expression on the RHS of `??`. So the bind parser must *not* feed `?? =>`
        // into `parse_coalesce()`.
        let mut rhs_guard: Option<ast::Stmt> = None;

        let rhs = if class_name.is_some() {
            // --- Object construction: name <> ClassName | field: val, field: val ---
            // Single-line: fields are comma-separated, terminated by newline/eof/end.
            // Multi-line: fields one per line, terminated by end/xx.
            let mut named: Vec<(String, ast::Expr)> = Vec::new();
            let is_multiline = matches!(
                self.peek().map(|t| &t.kind),
                Some(TokenKind::Newline) | Some(TokenKind::Indent) | Some(TokenKind::Dedent)
            );
            loop {
                if is_multiline {
                    self.skip_newlines();
                } else {
                    self.skip_layout_inline();
                }
                // terminators
                if self.is_eof() { break; }
                if self.peek_word("end") || self.peek_op("xx") { break; }
                if !is_multiline && matches!(self.peek().map(|t| &t.kind),
                    Some(TokenKind::Newline) | Some(TokenKind::Eof)) { break; }

                let Some(fname) = self.eat_ident() else { break; };
                self.skip_layout_inline();
                if !self.eat_op(":") { break; }
                self.skip_layout_inline();
                let fval_pe = self.parse_coalesce()?;
                let fval = self.lower_expr(fval_pe);
                named.push((fname, fval));

                if is_multiline {
                    // newline terminates this field; loop continues
                } else {
                    self.skip_layout_inline();
                    if !self.eat_op(",") { break; }
                }
            }
            if is_multiline {
                if self.peek_word("end") { let _ = self.eat_ident(); }
                else if self.peek_op("xx") { self.i += 1; }
            }
            ast::Expr::Object(named, op_span.clone())
        } else {
            // Parse RHS like `parse_coalesce()`, but STOP if the next `??` is
            // actually the guard introducer `?? =>`.
            let mut lhs_pe = self.with_depth(|p| p.parse_or())?;
            while self.peek_op("??") {
                // Look ahead: `??` followed by optional trivia then `=>` means guard.
                let save_i = self.i;
                self.i += 1; // consume '??' for lookahead

                // Skip trivia between ?? and =>/rhs-expr (newlines, layout, semicolons, comments)
                let mut j = self.i;
                while let Some(tok) = self.toks.get(j) {
                    match &tok.kind {
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => { j += 1; continue; }
                        TokenKind::Op(s) if s == ";" => { j += 1; continue; }
                        TokenKind::Op(s) if s.starts_with("///") || s.starts_with("////") || s.starts_with("<---") => {
                            j += 1; continue;
                        }
                        _ => break,
                    }
                }

                // If the next significant token is `=>`, this `??` belongs to the guard.
                if matches!(self.toks.get(j), Some(t) if matches!(t.kind, TokenKind::Op(ref s) if s == "=>")) {
                    self.i = save_i; // rewind so guard parsing can consume `?? => ...`
                    break;
                }

                // Not a guard: continue normal nullish coalescing.
                self.i = j;
                let rhs_pe = self.with_depth(|p| p.parse_or())?;
                lhs_pe = PExpr::Binary(Box::new(lhs_pe), "??".into(), Box::new(rhs_pe));
            }
            self.lower_expr(lhs_pe)
        };

        // Optional RHS guard: `?? => <stmt>`
        // (consume only if the full pattern is present; otherwise rewind)
        let guard_start_i = self.i;
        {
            let save_i = self.i;

            if self.eat_op("??") {
                // Skip trivia between ?? and =>
                let mut j = self.i;
                while let Some(tok) = self.toks.get(j) {
                    match &tok.kind {
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => { j += 1; continue; }
                        TokenKind::Op(s) if s == ";" => { j += 1; continue; }
                        TokenKind::Op(s) if s.starts_with("///") || s.starts_with("////") || s.starts_with("<---") => {
                            j += 1; continue;
                        }
                        _ => break,
                    }
                }

                if matches!(self.toks.get(j), Some(t) if matches!(t.kind, TokenKind::Op(ref s) if s == "=>")) {
                    self.i = j + 1; // consume `=>`

                    // Skip optional whitespace/layout after => (match parse_if_stmt inline style)
                    while let Some(t) = self.peek() {
                        if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                            self.i += 1;
                        } else {
                            break;
                        }
                    }

                    rhs_guard = Some(self.parse_stmt()?);
                } else {
                    // Not a guard; rewind to before '??'
                    self.i = save_i;
                }
            }
        }

        // 5) Build Stmt::Bind (single target)
        let name_for_cond = name_text.clone();
        let span_for_cond = name_span.clone();

        let bind_stmt = ast::Stmt::Bind(ast::BindStmt {
            name: (name_text, name_span),
            expr: rhs,
            is_imm: is_const,
            is_local: false,
            mode,
            span: op_span.clone(),
            class_name,
            lock_type: pending_lock_type,
        });

        // If a guard was present, return a sequence equivalent to:
        //   <bind>
        //   if <name>.nix? => <guard_stmt>
        if let Some(guard_stmt) = rhs_guard {
            // `x.nix?` is parsed elsewhere as member `is_nix` (because `.prop?` => `.is_prop`).
            let cond = ast::Expr::Member(
                Box::new(ast::Expr::Ident(name_for_cond, span_for_cond.clone())),
                "is_nix".to_string(),
                span_for_cond.clone(),
            );

            let if_span = Self::span_from_tokens(
                self.toks,
                guard_start_i.min(self.toks.len().saturating_sub(1)),
                self.i.saturating_sub(1),
            );

            let then_block = ast::Expr::Block {
                stmts: vec![guard_stmt],
                span: if_span.clone(),
            };

            let if_stmt = ast::Stmt::Expr(ast::Expr::FreeCall(
                "if".to_string(),
                vec![cond, then_block],
                if_span,
            ));

            // Wrap both statements into a single returned statement (existing pattern used elsewhere)
            let block_span = Self::span_from_tokens(self.toks, start_i_expr, self.i.saturating_sub(1));
            return Ok(ast::Stmt::Block {
                stmts: vec![bind_stmt, if_stmt],
                span: block_span,
            });
        }

        Ok(bind_stmt)
    }
    // === END: parse_bind_stmt ===

    #[inline]
    fn peek_word(&self, want: &str) -> bool {
        use goblin_lexer::TokenKind;
        match self.toks.get(self.i) {
            Some(t) if matches!(t.kind, TokenKind::Ident) => t.value.as_deref() == Some(want),
            _ => false,
        }
    }

    #[inline]
    fn eat_word(&mut self, want: &str) -> bool {
        use goblin_lexer::TokenKind;
        match self.toks.get(self.i) {
            Some(t) if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some(want) => {
                self.i += 1;
                true
            }
            _ => false,
        }
    }

    #[inline]
    fn peek_ident(&self) -> Option<&str> {
        self.peek().and_then(|t| {
            if let goblin_lexer::TokenKind::Ident = t.kind {
                t.value.as_deref()
            } else {
                None
            }
        })
    }

    fn peek_is_return(&self) -> bool {
        matches!(self.peek_ident(), Some("return") | Some("send"))
    }

    #[inline]
    fn eat_ident(&mut self) -> Option<String> {
        let val = {
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::Ident = t.kind {
                    t.value.clone()
                } else {
                    None
                }
            } else {
                None
            }
        };
        if val.is_some() {
            self.i += 1;
        }
        val
    }

    // Consume consecutive NEWLINE tokens.
    #[inline]
    fn skip_newlines(&mut self) {
        use goblin_lexer::TokenKind as K;

        let start_i = self.i;
        let mut count: usize = 0;

        while let Some(tok) = self.toks.get(self.i) {
            if !matches!(tok.kind, K::Newline) {
                break;
            }
            let before = self.i;
            self.i += 1;
            count += 1;

            // Progress guard
            if self.i <= before {
                panic!(
                    "P9001: Internal error: I got stuck trying to skip newlines at token {} ({:?}).\n\nhelp: This shouldn't happen - please report this bug with your source code.",
                    before, tok
                );
            }

            // Hard cap to prevent pathological spins
            if count > 100_000 {
                // CLONE the span to avoid moving out of &Token
                let first_span = self.toks.get(start_i).map(|t| t.span.clone());
                panic!(
                    "P9002: Internal guard tripped: I tried to skip way too many newlines (>100k) starting at token {}, span={:?}.\n\nhelp: This shouldn't happen in normal code - please report this bug with your source code near that location.",
                    start_i, first_span
                );
            }
        }
    }

    fn skip_layout(&mut self) {
        use goblin_lexer::TokenKind;
        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => self.i += 1,
                _ => break,
            }
        }
    }

    // Do we see a block closer at the current position?  (allowed: 'end' or 'xx')
    fn peek_block_close(&mut self) -> bool {
        let save_i = self.i;
        
        // Skip newlines and matched Dedent/Indent pairs
        while let Some(tok) = self.toks.get(self.i) {
            match tok.kind {
                ::goblin_lexer::TokenKind::Newline => {
                    self.i += 1;
                }
                ::goblin_lexer::TokenKind::Dedent => {
                    // Look ahead to see if there's a matching Indent
                    let mut j = self.i + 1;
                    // Skip newlines between Dedent and potential Indent
                    while let Some(t) = self.toks.get(j) {
                        if matches!(t.kind, ::goblin_lexer::TokenKind::Newline) {
                            j += 1;
                        } else {
                            break;
                        }
                    }
                    // If we find an Indent, skip both the Dedent and Indent
                    if let Some(t) = self.toks.get(j) {
                        if matches!(t.kind, ::goblin_lexer::TokenKind::Indent) {
                            self.i = j + 1; // Skip past the Indent
                            continue;
                        }
                    }
                    // Unmatched Dedent - this IS a closer
                    self.i = save_i;
                    return true;
                }
                _ => break,
            }
        }
        
        // Now check if we see a closer
        let is_close = match self.toks.get(self.i) {
            Some(t) => {
                t.value.as_deref() == Some("end")
                || t.value.as_deref() == Some("xx")
                || matches!(t.kind, TokenKind::Op(ref s) if s == "xx")
            }
            None => true,
        };
        
        // Restore position
        self.i = save_i;
        is_close
    }

    // If present, consume a block closer. Returns true if one was consumed. (end or xx)
    fn eat_block_close(&mut self) -> bool {
        if self.peek_ident() == Some("end") {
            let _ = self.eat_ident(); // 'end'
            return true;
        }
        if self.peek_op("xx") {
            self.i += 1; // consume 'xx'
            return true;
        }
        false
    }

    // Require a block closer right here (after optional newlines). Helpful error if missing.
    fn expect_block_close(&mut self, block_type: &str) -> Result<(), String> {
        self.skip_newlines();
        if self.is_eof() {
            return Err(s_help_site!(
                "P0212",
                "This action block is missing its closing 'end' or 'xx' (crossbones).",
                "Close the block with 'end' or 'xx' (crossbones). [expect block close]"
            ));
        }
        let tok = self.toks.get(self.i).ok_or_else(|| {
            s_help_site!(
                "P0212",
                &format!("This {} block is missing its closing 'end' or 'xx' (crossbones).", block_type),
                "Close the block with 'end' or 'xx' (crossbones). [expect block close]",
            )
        })?;
        if tok.value.as_deref() != Some("end") && tok.value.as_deref() != Some("xx") {
            return Err(s_help_site!(
                "P0212",
                &format!("Expected 'end' or 'xx' to close this {} block.", block_type),
                "Use 'end' or 'xx' to close the block. [expect block close]",
            ));
        }
        self.i += 1;
        self.eat_layout_until_close(0);
        Ok(())
    }

    fn is_capitalized(name: &str) -> bool {
        name.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false)
    }

    /// Parse one `name: expr` pair. Caller has NOT consumed `name` yet.
    fn parse_name_colon_expr_pair(&mut self) -> Result<(String, PExpr), String> {
        let Some(name) = self.eat_ident() else {
            return Err(s_help_site!("P0911", "Expected a field name before ':'", "Write: name: value"));
        };
        if !self.eat_op(":") {
            return Err(s_help_site!("P0911", "Expected ':' after field name", "Write: name: value"));
        }
        let val = self.parse_assign()?; // full expr on the right
        Ok((name, val))
    }

    /// Parse `name: expr` pairs separated by `::`, allowing empty `::` segments (skips)
    fn parse_template_pairs(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        use goblin_lexer::TokenKind;

        let mut pairs: Vec<(String, PExpr)> = Vec::new();

        // First pair (required)
        pairs.push(self.parse_name_colon_expr_pair()?);

        loop {
            // Stop on newline / block closers / expression terminators
            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Newline => break,
                    TokenKind::Op(op) if op == ")" || op == "]" || op == "}" || op == "," => break,
                    // We're inside an expr; don't eat 'end'/'else' here — higher-level parsers own them.
                    TokenKind::Ident if matches!(t.value.as_deref(), Some("end") | Some("else")) => break,
                    _ => {}
                }
            } else {
                break; // EOF
            }

            // Expect `::` or stop
            if !self.eat_op("::") {
                break;
            }

            // Allow empty segment to mean "skip (use default)"
            // e.g., `Pet: name: "Max" :: :: species: "cat"`
            // If next token starts a new pair, parse it; if it's another `::` or newline/terminator, just continue.
            let should_parse_pair = if let Some(t) = self.peek() {
                matches!(t.kind, TokenKind::Ident)
            } else {
                false
            };

            if should_parse_pair {
                pairs.push(self.parse_name_colon_expr_pair()?);
            } else {
                // empty `::` — do nothing (skip)
            }
        }

        Ok(pairs)
    }

    /// Try to parse `TypeName: name: expr (:: name: expr)*` after having just seen an Ident.
    /// `type_name` is the already-consumed identifier text. If not a template apply, returns None and does not consume.
    fn try_parse_template_apply_after_ident(
        &mut self,
        type_name: String,
        start_i: usize,
    ) -> Result<Option<PExpr>, String> {
        // Must see a colon immediately after the Ident
        if !self.peek_op(":") {
            return Ok(None);
        }

        // Enforce capitalized Type name
        if !Self::is_capitalized(&type_name) {
            return Err(s_help_site!(
                "P0910",
                "Types used in object construction must begin with a capital letter.",
                &format!("Write '{}: name: \"...\"' with a capitalized type, like 'Pet: ...'.", Self::capitalize_like(&type_name)),
            ));
        }

        let _ = self.eat_op(":"); // consume the first ':'

        // Parse pairs: name: expr (:: name: expr)*
        let pairs = self.parse_template_pairs_flex()?;

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(Some(PExpr::TemplateApply {
            type_name,
            pairs,
            span,
        }))
    }

    /// Simple helper to suggest a capitalized form (e.g., "pet" -> "Pet")
    fn capitalize_like(s: &str) -> String {
        let mut it = s.chars();
        match it.next() {
            Some(first) => first.to_ascii_uppercase().to_string() + it.as_str(),
            None => String::new(),
        }
    }

    // Optional: also treat semicolons as statement separators when scanning blocks.
    fn skip_stmt_separators(&mut self) {
        use goblin_lexer::TokenKind as K;

        loop {
            match self.toks.get(self.i) {
                // Layout trivia
                Some(tok) if matches!(tok.kind, K::Newline | K::Indent | K::Dedent) => {
                    self.i += 1;
                }

                // Inline/block comments:
                //  - works whether your lexer has comment kinds OR emits Op("///") / Op("////")
                Some(tok) if matches!(tok.kind, K::Op(ref s)
                    if s.starts_with("///") || s.starts_with("////") || s.starts_with("<---")) => {
                    self.i += 1;
                }

                // If your lexer DOES have explicit kinds, you can add them too (optional):
                // Some(tok) if matches!(tok.kind, K::LineComment | K::BlockComment) => { self.i += 1; }

                _ => break,
            }
        }
    }

    fn eat_semi_separators(&mut self) {
        use goblin_lexer::TokenKind as K;

        loop {
            // Goblin has no ';' statement separators — do NOT consume ';'

            match self.toks.get(self.i) {
                // Treat layout as separators
                Some(tok) if matches!(tok.kind, K::Newline | K::Indent | K::Dedent) => {
                    self.i += 1;
                    continue;
                }

                // Treat comments as separators as well (see note above)
                Some(tok) if matches!(tok.kind, K::Op(ref s)
                    if s.starts_with("///") || s.starts_with("////") || s.starts_with("<---")) => {
                    self.i += 1;
                    continue;
                }

                // Optional explicit comment kinds:
                // Some(tok) if matches!(tok.kind, K::LineComment | K::BlockComment) => {
                //     self.i += 1;
                //     continue;
                // }

                _ => {}
            }

            break;
        }
    }

    // Generic "read statements until a closer". Use this for bodies of fn/if/while/class/etc.
    // Braces are NOT used for statement blocks. Blocks are layout + closed by 'end' or 'xx'.
    fn parse_stmt_block(&mut self) -> Result<Vec<ast::Stmt>, String> {
        // LAYOUT MODE: require newline, optionally followed by Indent
        let mut saw_nl = false;
        while let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Newline) {
                self.i += 1;
                saw_nl = true;
            } else {
                break;
            }
        }

        // Fallback: accept physical line break if lexer didn’t emit Newline
        if !saw_nl {
            if let (Some(prev), Some(curr)) = (self.toks.get(self.i.saturating_sub(1)), self.toks.get(self.i)) {
                if curr.span.line_start > prev.span.line_end {
                    saw_nl = true;
                }
            }
        }

        // Optionally consume a single Indent
        let mut depth: usize = 0;
        if let Some(tok) = self.toks.get(self.i) {
            if matches!(tok.kind, TokenKind::Indent) {
                self.i += 1;
                depth = 1;
            }
        }

        if !saw_nl {
            return Err(s_help_site!(
                "P0210",
                "Expected indentation after this header",
                "Start the block on the next line, indent the body, and close with 'end' or 'xx' (crossbones).",
            ));
        }

        let mut out = Vec::new();
        loop {
            match self.toks.get(self.i) {
                Some(tok) if matches!(tok.kind, TokenKind::Indent) => {
                    self.i += 1;
                    depth += 1;
                    continue;
                }
                Some(tok) if matches!(tok.kind, TokenKind::Dedent) => {
                    self.i += 1;
                    if depth == 0 {
                        return Err(s_help_site!(
                            "P0211",
                            "The indentation went back too far for this block",
                            "Indent the line to stay inside the block, or close it first with 'end' or 'xx' (crossbones).",
                        ));
                    }
                    depth -= 1;
                    if depth == 0 {
                        // Require an explicit closer after layout ends
                        self.skip_newlines();
                        if self.peek_block_close() {
                            let _ = self.eat_block_close();
                            break;
                        }
                        return Err(s_help_site!(
                            "P0212",
                            "This block is missing its closing 'end' or 'xx' (crossbones)",
                            "Close the block with 'end' or 'xx' (crossbones). [parse stmt block]",
                        ));
                    }
                    continue;
                }
                Some(_) => { /* normal statement path */ }
                None => {
                    return Err(s_help_site!(
                        "P0201",
                        "I reached the end of the file, but this block is still open",
                        "Close the block with 'end' or 'xx' (crossbones). [parse stmt block]",
                    ));
                }
            }

            // Also allow explicit closers at header depth
            if depth == 0 && self.peek_block_close() {
                let _ = self.eat_block_close();
                break;
            }

            // Parse one statement in the block
            out.push(self.parse_stmt()?);
            self.skip_stmt_separators();
        }

        Ok(out)
    }

    fn peek_newline_or_eof(&self) -> bool {
        match self.toks.get(self.i) {
            Some(tok) => matches!(tok.kind, TokenKind::Newline | TokenKind::Eof),
            None => true,
        }
    }

    fn peek_ident_at_col(&self, kw: &str, col: u32) -> bool {
        if let Some(tok) = self.toks.get(self.i) {
            match tok.kind {
                TokenKind::Ident => tok.value.as_deref() == Some(kw) && tok.span.col_start == col,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Consume optional `between` sugar after a subject expression:
    ///   <expr> between <lo> ..  <hi>
    ///   <expr> between <lo> ... <hi>
    ///   <expr> !between <lo> ..  <hi>
    /// Returns Ok(true) if consumed; Ok(false) if not present.
    fn eat_between_tail(&mut self) -> Result<bool, String> {
        let save = self.i;

        // Optional '!between' (the '!' must be immediately before 'between')
        let mut neg = false;
        if self.peek_op("!") {
            self.i += 1; // eat '!'
            if self.peek_ident() == Some("between") {
                let _ = self.eat_ident(); // 'between'
                neg = true;
            } else {
                // Not actually '!between' → restore and bail
                self.i = save;
                return Ok(false);
            }
        } else {
            // Plain 'between'
            if self.peek_ident() == Some("between") {
                let _ = self.eat_ident(); // 'between'
            } else {
                return Ok(false);
            }
        }

        // lower bound
        let _low = self.parse_coalesce()?;

        // dots: prefer "..." over ".."
        let exclusive_end = if self.eat_op("...") {
            // CHANGED: "..." => inclusive upper bound
            false
        } else if self.eat_op("..") {
            // CHANGED: ".."  => exclusive upper bound
            true
        } else {
            return Err(s_help_site!(
                "P1004",
                "Expected '..' or '...' after the lower bound in 'between'",
                "Write it like: between 1..5 or between 10...20",
            ));
        };

        // upper bound
        let _high = self.parse_coalesce()?;

        // (Desugaring to comparisons comes later; for now just consume.)
        let _ = (neg, exclusive_end);
        Ok(true)
    }

    fn parse_kv_bind_list(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut fields = Vec::new();
        loop {
            self.skip_newlines();

            let Some(key) = self.eat_object_key() else {
                break;
            };

            if !self.eat_op(":") {
                return Err(s_help_site!(
                    "P0904",
                    "You need a ':' after the field name in the class header",
                    "Write it like: username: \"john\"",
                ));
            }

            let value = self.parse_coalesce()?;
            fields.push((key, value));

            self.skip_newlines();
            if self.eat_op("::") {
                continue;
            }
            break;
        }
        Ok(fields)
    }

    fn parse_matrix_decl(&mut self, type_name: String, start_i: usize) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;

        // consume 'matrix'
        let _ = self.eat_ident();

        // skip into indented block — columns are inferred from the id row
        self.skip_layout();

        let mut rows: Vec<MatrixRow> = Vec::new();
        let mut col_count: Option<usize> = None;

        loop {
            self.skip_layout();

            // stop at 'end' / 'xx' / EOF
            if self.is_eof() { break; }
            if let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Ident)
                    && matches!(tok.value.as_deref(), Some("end") | Some("xx"))
                {
                    break;
                }
            }

            // field name
            let Some(field) = self.eat_ident() else { break; };

            if !self.eat_op(":") {
                return Err(s_help_site!(
                    "P1101",
                    "Expected ':' after field name in matrix row",
                    "Write: power: 100, nc, 70, 90",
                ));
            }

            // skip optional layout after ':'
            self.skip_layout();

            // default value (required)
            let default = self.parse_matrix_cell()?;

            // per-column cells
            let mut cells: Vec<MatrixCell> = Vec::new();
            loop {
                // allow newlines inside a row (multiline cell values)
                self.skip_layout_inline();
                if let Some(tok) = self.peek() {
                    match tok.kind {
                        TokenKind::Newline => {
                            // peek ahead past newline: if next non-ws is a comma, it's continuation
                            let saved = self.i;
                            self.skip_newlines();
                            if self.eat_op(",") {
                                self.skip_layout_inline();
                                self.skip_newlines();
                                cells.push(self.parse_matrix_cell()?);
                                continue;
                            } else {
                                self.i = saved;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                if !self.eat_op(",") { break; }
                self.skip_layout_inline();
                self.skip_newlines();
                cells.push(self.parse_matrix_cell()?);
            }

            // Lock in column count from first row; validate subsequent rows
            if let Some(expected) = col_count {
                if cells.len() != expected {
                    return Err(s_help_site!(
                        "P1102",
                        &format!(
                            "Matrix row '{}' has {} value(s) but expected {} (from first row)",
                            field, cells.len(), expected
                        ),
                        "Each row must have one value per column (use 'nc' to inherit the default)",
                    ));
                }
            } else {
                if cells.is_empty() {
                    return Err(s_help_site!(
                        "P1100",
                        "Object matrix needs at least one column",
                        "Write: name: \"default\", Col1, Col2, Col3",
                    ));
                }
                col_count = Some(cells.len());
            }

            rows.push(MatrixRow { field, default, cells });
        }

        // consume closing 'end' / 'xx'
        if let Some(tok) = self.peek() {
            if matches!(tok.kind, TokenKind::Ident)
                && matches!(tok.value.as_deref(), Some("end") | Some("xx"))
            {
                self.i += 1;
            }
        }
        if self.eat_op("xx") { /* already consumed above, no-op */ }

        if rows.is_empty() {
            return Err(s_help_site!(
                "P1100",
                "Object matrix is empty",
                "Add at least one row: id: \"{id}\", USA, France",
            ));
        }

        // Extract column names from the id row cells.
        // Each cell should be a string literal whose value is the column name.
        let col_count = col_count.unwrap_or(0);
        let mut columns: Vec<String> = Vec::with_capacity(col_count);

        if let Some(first_row) = rows.first() {
            for cell in &first_row.cells {
                match cell {
                    MatrixCell::Expr(PExpr::Str(s)) => columns.push(s.clone()),
                    MatrixCell::Expr(PExpr::Ident(s)) => columns.push(s.clone()),
                    _ => columns.push(format!("col_{}", columns.len())),
                }
            }
        } else {
            // No rows — use positional names col_0, col_1, ...
            for i in 0..col_count {
                columns.push(format!("col_{}", i));
            }
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        Ok(PExpr::ObjectMatrix { type_name, columns, rows, span })
    }

    /// Parse a single matrix cell: `nc`, `::`, or an expression.
    fn parse_matrix_cell(&mut self) -> Result<MatrixCell, String> {
        // '::' means nc
        if self.eat_op("::") {
            return Ok(MatrixCell::Nc);
        }
        // 'nc' ident means nc
        if self.peek_ident() == Some("nc") {
            let _ = self.eat_ident();
            return Ok(MatrixCell::Nc);
        }
        let expr = self.parse_coalesce()?;
        Ok(MatrixCell::Expr(expr))
    }

    /// Skip spaces/tabs but NOT newlines (used inside a matrix row).
    fn skip_layout_inline(&mut self) {
        // The lexer emits Indent/Dedent for significant whitespace; ordinary
        // horizontal whitespace is already consumed.  We just need to skip any
        // Indent tokens that may appear mid-line on some lexer configurations.
        use goblin_lexer::TokenKind;
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Indent => { self.i += 1; }
                _ => break,
            }
        }
    }

    fn parse_class_decl(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;

        // Detect REPL input by filename
        let is_repl = self
            .toks
            .get(0)
            .map(|t| t.span.file.as_str() == "<repl>")
            .unwrap_or(false);

        // ── Header: <>Name …
        let (name, hdr_col, hdr_line) = if let Some(tok0) = self.peek().cloned() {
            match tok0.kind {
                TokenKind::ClassIdent => {
                    let nm = tok0.value.clone().unwrap_or_default();
                    if nm.is_empty() {
                        return Err(s_help_site!(
                            "P0906",
                            "Expected a class name after '<>'",
                            "Start with a capitalized name: <>Player |",
                        ));
                    }
                    self.i += 1;
                    (nm, tok0.span.col_start, tok0.span.line_start)
                }
                _ => {
                    return Err(s_help_site!(
                        "P0905",
                        "Expected '<>' to start a class declaration",
                        "Start the class like: <>Player |",
                    ));
                }
            }
        } else {
            return Err(s_help_site!(
                "P0905",
                "Expected '@' to start a class declaration",
                "Start the class like: @Player = username: \"john\", health: 100",
            ));
        };

        if !name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            return Err(s_help_site!(
                "P0907",
                &format!("Class names must start with a capital letter (found '{}')", name),
                "Rename it to start with uppercase: Player",
            ));
        }

        // Optionally consume a '!' modifier after the class name (@Entity! | ...)
        self.eat_op("!");


        if self.eat_op("(") {
            if self.peek_op("(") {
                return Err(s_help_site!(
                    "P0710",
                    "Parentheses are not allowed after a class name.",
                    "Put fields after '=': @Player | username: \"john\", health: 100",
                ));
            }
        }

        if !self.eat_op("|") {
            return Err(s_help_site!(
                "P0908",
                &format!("You need '|' after the class name '{}'", name),
                "Write it like: @Player | username: \"john\", health: 100",
            ));
        }

        // ── Fields (single-line OR multi-line). Commas and newlines are both allowed.
        let mut fields: Vec<(String, PExpr, bool, bool, bool, Option<RelationDef>)> = Vec::new();

        loop {
            // Skip layout
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Stop if a closer/action/EOF is coming
            if self.peek_block_close() || self.is_eof() {
                break;
            }
            if let Some(tok) = self.peek() {
                // Keywords that start action blocks or explicit closers
                if matches!(tok.kind, TokenKind::Ident)
                    && matches!(
                        tok.value.as_deref(),
                        Some("act") | Some("action") | Some("end") | Some("xx")
                        | Some("score") | Some("judge") | Some("transition") | Some("capacity")
                    )
                {
                    break;
                }
                if matches!(tok.kind, TokenKind::Act | TokenKind::Action) {
                    break;
                }
            }

            // Check if this is a relation keyword first (BEFORE eating ident)
            let first_ident = self.peek_ident();
            
            if first_ident == Some("with") || first_ident == Some("of") || first_ident == Some("re") {
                // This is a relation declaration, not a regular field
                let rel_keyword = self.eat_ident().unwrap();
                
                let Some(class_name) = self.eat_ident() else {
                    return Err(s_help_site!("P0920", 
                        &format!("Expected class name after '{}'", rel_keyword),
                        &format!("Write: {} ClassName", rel_keyword)));
                };
                
                let (relation, field_name) = if rel_keyword == "of" {
                    if self.peek_ident() != Some("as") {
                        return Err(s_help_site!("P0921", "Expected 'as' after class name", "Write: of User as author"));
                    }
                    self.i += 1; // eat 'as'
                    let Some(as_name) = self.eat_ident() else {
                        return Err(s_help_site!("P0922", "Expected relation name after 'as'", "Write: of User as author"));
                    };
                    (Some(RelationDef::Of { class_name: class_name.clone(), as_name: as_name.clone() }), as_name)
                } else if rel_keyword == "with" {
                    (Some(RelationDef::With { class_name: class_name.clone() }), class_name.to_lowercase())
                } else { // "re"
                    (Some(RelationDef::Re { class_name: class_name.clone() }), class_name.to_lowercase())
                };
                
                let field_expr = PExpr::Nil;
                fields.push((field_name, field_expr, false, false, false, relation));
                
                // Skip layout/separators
                while let Some(tok) = self.peek() {
                    if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                self.eat_op("::");
                self.eat_op(",");
                
                continue;
            }

            // Parse regular field: [~]name[!][?] : expr
            // Check for ~ raw sigil before the field name
            let raw = self.eat_op("~");

            let Some(mut fname) = self.eat_ident() else { break; };

            // Modifiers: ? (nullable), ! (readonly) as suffixes
            let mut readonly = false;
            let mut nullable = false;
            if fname.ends_with('?') {
                nullable = true;
                fname.truncate(fname.len() - 1);
            }
            if fname.ends_with('!') {
                readonly = true;
                fname.truncate(fname.len() - 1);
            }

            // Normal field: expect ':' and value
            if !self.eat_op(":") {
                return Err(s_help_site!(
                    "P0904",
                    "You need a ':' after the field name (and any modifiers)",
                    "Write it like username: \"john\" or email?: \"\" or id!: 0",
                ));
            }
            // Skip layout before the value expression
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            let fexpr = self.parse_assign()?;
            fields.push((fname, fexpr, readonly, nullable, raw, None));

            // Skip layout after the value
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Optional separator (:: or ,)
            self.eat_op("::");
            self.eat_op(",");
        }

        // ── SINGLE-LINE / REPL AUTO-CLOSE:
        if is_repl && !fields.is_empty() && self.is_eof() {
            return Ok(PExpr::ClassDecl { name, fields, actions: Vec::new(), decision: None, judge: None, transitions: Vec::new(), capacity: None });
        }

        // ── Actions / explicit closer (multi-line file mode continues)
        let mut actions: Vec<PAction> = Vec::new();
        let mut decision: Option<PDecisionDef> = None;
        let mut judge: Option<ast::JudgeStmt> = None;
        let mut transitions: Vec<ast::TransitionDef> = Vec::new();
        let mut capacity: Option<ast::CapacityDecl> = None;

        loop {
            // Skip layout
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Allow inline closer on the same line as header (optional)
            if let Some(tok) = self.peek().cloned() {
                let is_inline_ident = matches!(tok.kind, TokenKind::Ident)
                    && matches!(tok.value.as_deref(), Some("end") | Some("xx"));
                if is_inline_ident && tok.span.line_start == hdr_line {
                    self.i += 1; // consume 'end'/'xx'
                    return Ok(PExpr::ClassDecl { name, fields, actions, decision: decision.map(Box::new), judge, transitions, capacity });
                }
            }

            // Proper block closer (on its own line), enforce alignment
            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != hdr_col {
                    let closer = self.peek_ident().unwrap_or("xx");
                    return Err(s_help_site!(
                        "P0222",
                        &format!(
                            "This '{}' closer is misaligned: expected column {}, found column {}",
                            closer, hdr_col, col
                        ),
                        "Align the closer with its header (same column): place 'end' or 'xx' (crossbones) directly under the start of the class header.",
                    ));
                }
                self.expect_block_close("class")?;
                break;
            }

            // In REPL, if we ever reach EoF here, also auto-close (for multi-line REPL entries)
            if is_repl && self.is_eof() {
                return Ok(PExpr::ClassDecl { name, fields, actions, decision: decision.map(Box::new), judge, transitions, capacity });
            }

            if self.is_eof() {
                return Err(s_help_site!(
                    "P0909",
                    &format!(
                        "I reached the end of the file, but the class '{}' is still open (missing 'end' or 'xx')",
                        name
                    ),
                    "Close the class with 'end' or 'xx' (crossbones): @Player = username: \"john\", health: 100 end",
                ));
            }

            // Next token must start an action
            let act_tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };
            let is_action_kw = matches!(act_tok.kind, TokenKind::Act | TokenKind::Action)
                || (matches!(act_tok.kind, TokenKind::Ident)
                    && matches!(act_tok.value.as_deref(), Some("act") | Some("action")));

            // Handle decision formula: `score | decision against TargetClass by [ formula ]`
            let is_decision = matches!(act_tok.kind, TokenKind::Ident)
                && act_tok.value.as_deref() == Some("score");

            // Handle judge block inside class
            let is_judge = matches!(act_tok.kind, TokenKind::Ident)
                && act_tok.value.as_deref() == Some("judge");

            if is_decision {
                self.i += 1; // consume 'score'
                if !self.eat_op("|") {
                    return Err(s_help_site!(
                        "P1400",
                        "Expected '|' after 'score' in decision declaration",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                }
                if self.peek_ident() != Some("decision") {
                    return Err(s_help_site!(
                        "P1401",
                        "Expected 'decision' after 'score |'",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                }
                let _ = self.eat_ident(); // consume 'decision'
                self.skip_layout();
                if self.peek_ident() != Some("against") {
                    return Err(s_help_site!(
                        "P1402",
                        "Expected 'against' after 'decision'",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                }
                let _ = self.eat_ident(); // consume 'against'
                self.skip_layout();
                let Some(target_class) = self.eat_ident() else {
                    return Err(s_help_site!(
                        "P1403",
                        "Expected target class name after 'against'",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                };
                self.skip_layout();
                if self.peek_ident() != Some("by") {
                    return Err(s_help_site!(
                        "P1404",
                        "Expected 'by' after target class name",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                }
                let _ = self.eat_ident(); // consume 'by'
                self.skip_layout();
                if !self.eat_op("[") {
                    return Err(s_help_site!(
                        "P1405",
                        "Expected '[' to open decision formula",
                        "Write: score | decision against Fighter by [ formula ]",
                    ));
                }
                self.skip_layout();
                let formula = self.parse_coalesce()?;
                self.skip_layout();
                if !self.eat_op("]") {
                    return Err(s_help_site!(
                        "P1406",
                        "Expected ']' to close decision formula",
                        "Write: score | decision against Fighter by [ self >> aggression ]",
                    ));
                }
                decision = Some(PDecisionDef { target_class, formula: Box::new(formula) });
                continue;
            }

            if is_judge {
                // Parse the judge block using the existing judge parser
                let stmt = self.parse_judge_stmt()?;
                if let ast::Stmt::Judge(js) = stmt {
                    judge = Some(js);
                }
                continue;
            }

            // Handle transition declaration inside class body
            let is_transition = matches!(act_tok.kind, TokenKind::Ident)
                && act_tok.value.as_deref() == Some("transition");

            if is_transition {
                let td = self.parse_transition_def()?;
                transitions.push(td);
                continue;
            }

            // Handle capacity declaration inside class body
            // `capacity: N` or `capacity from field_name: N`
            let is_capacity = matches!(act_tok.kind, TokenKind::Ident)
                && act_tok.value.as_deref() == Some("capacity");

            if is_capacity {
                let _ = self.eat_ident(); // consume 'capacity'
                self.skip_layout();
                if self.peek_ident() == Some("from") {
                    let _ = self.eat_ident(); // consume 'from'
                    self.skip_layout();
                    let Some(field_name) = self.eat_ident() else {
                        return Err(s_help_site!(
                            "P1550",
                            "Expected field name after 'capacity from'",
                            "Write: capacity from weight: 100",
                        ));
                    };
                    self.skip_layout();
                    if !self.eat_op(":") {
                        return Err(s_help_site!(
                            "P1551",
                            "Expected ':' after field name in capacity declaration",
                            "Write: capacity from weight: 100",
                        ));
                    }
                    self.skip_layout();
                    let limit_expr = self.parse_primary()?;
                    let limit = match &limit_expr {
                        PExpr::Float(n) | PExpr::Int(n) => n.parse::<f64>().unwrap_or(0.0),
                        _ => return Err(s_help_site!(
                            "P1552",
                            "Expected a numeric limit in capacity declaration",
                            "Write: capacity from weight: 100",
                        )),
                    };
                    capacity = Some(ast::CapacityDecl::Field { field_name, limit });
                } else {
                    if !self.eat_op(":") {
                        return Err(s_help_site!(
                            "P1553",
                            "Expected ':' after 'capacity'",
                            "Write: capacity: 20",
                        ));
                    }
                    self.skip_layout();
                    let count_expr = self.parse_primary()?;
                    let count = match &count_expr {
                        PExpr::Int(n) => n.parse::<u64>().unwrap_or(0),
                        PExpr::Float(n) => n.parse::<f64>().unwrap_or(0.0) as u64,
                        _ => return Err(s_help_site!(
                            "P1554",
                            "Expected a numeric count in capacity declaration",
                            "Write: capacity: 20",
                        )),
                    };
                    capacity = Some(ast::CapacityDecl::Count(count));
                }
                continue;
            }

            if !is_action_kw {
                // A new top-level declaration (<>Name) on its own line closes a single-line class.
                if matches!(act_tok.kind, TokenKind::ClassIdent) {
                    // Don't consume — let parse_module handle it
                    return Ok(PExpr::ClassDecl { name, fields, actions, decision: decision.map(Box::new), judge, transitions, capacity });
                }
                return Err(s_help_site!(
                    "P0910",
                    "Inside a class, only 'act', 'action', or a closing 'end'/'xx' are allowed here",
                    "Add an action or close the class: act run(a) ... end, or end",
                ));
            }

            // Parse the action
            let kw = match &act_tok.kind {
                TokenKind::Act => {
                    self.i += 1;
                    "act"
                }
                TokenKind::Action => {
                    self.i += 1;
                    "action"
                }
                TokenKind::Ident if act_tok.value.as_deref() == Some("act") => {
                    let _ = self.eat_ident();
                    "act"
                }
                TokenKind::Ident if act_tok.value.as_deref() == Some("action") => {
                    let _ = self.eat_ident();
                    "action"
                }
                _ => unreachable!(),
            };

            let act_line = act_tok.span.line_start;
            let act_col = act_tok.span.col_start;
            let action = self.parse_action_after_keyword(kw)?;
            self.enforce_inline_brace_policy(act_line, "action")?;

            if !action.body.is_empty() {
                actions.push(action);
                continue;
            }

            // Action body in block form
            let body = self.parse_indented_block(act_col, &["end"])?;

            self.skip_stmt_separators();

            if self.block_closed_hard {
                self.block_closed_hard = false;
            } else if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != act_col {
                    return Err(s_help_site!(
                        "P0222",
                        &format!(
                            "This closer is misaligned: expected column {}, found column {}",
                            act_col, col
                        ),
                        "Align the closer with its header (same column), placing 'end' or 'xx' (crossbones) directly under the action header.",
                    ));
                }
                self.expect_block_close("action")?;
            } else if !self.eat_layout_until_close(act_col) {
                return Err(s_help_site!(
                    "P0212",
                    "This action block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }

            actions.push(PAction {
                name: action.name,
                params: action.params,
                body,
                is_single: false,
            });
        }

        Ok(PExpr::ClassDecl { name, fields, actions, decision: decision.map(Box::new), judge, transitions, capacity })
    }

    fn parse_enum_decl(&mut self) -> Result<PEnumDecl, String> {
        
        // Get enum name
        let Some(name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1001",
                "You need to give your enum a name",
                "Write it like: enum Status",
            ));
        };
        
        // Enum names should be capitalized (optional check, matching your class style)
        if !name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            return Err(s_help_site!(
                "P1002",
                &format!("Enum names must start with a capital letter (found '{}')", name),
                "Rename it to start with uppercase: Status",
            ));
        }
        
        let hdr_col = self.toks.get(self.i.saturating_sub(2))
            .map(|t| t.span.col_start)
            .unwrap_or(0);
        
        // Expect newline + indent for variants
        self.skip_newlines();
        
        let mut variants = Vec::new();
        
        loop {
            self.skip_newlines();
            
            // Check for end of enum block
            if self.eat_layout_until_close(hdr_col) || self.peek_block_close() {
                if self.peek_block_close() {
                    let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                    if col != hdr_col {
                        return Err(s_help_site!(
                            "P1003",
                            &format!("This closer is misaligned: expected column {}, found column {}", hdr_col, col),
                            "Align 'end' or 'xx' with the enum header",
                        ));
                    }
                    self.expect_block_close("enum")?;
                }
                break;
            }
            
            if self.is_eof() {
                return Err(s_help_site!(
                    "P1004",
                    &format!("The enum '{}' is missing its closing 'end' or 'xx'", name),
                    "Close the enum with 'end' or 'xx' (crossbones)",
                ));
            }
            
            // Parse variant name
            let Some(variant_name) = self.eat_ident() else {
                return Err(s_help_site!(
                    "P1005",
                    "Expected a variant name",
                    "Use a simple identifier like: idle or loading",
                ));
            };
            
            // Check for variant fields: { x: int, y: int }
            let mut fields = Vec::new();
            if self.eat_op("{") {
                loop {
                    self.skip_newlines();
                    
                    if self.eat_op("}") {
                        break;
                    }
                    
                    let Some(field_name) = self.eat_ident() else {
                        return Err(s_help_site!(
                            "P1006",
                            "Expected a field name",
                            "Write it like: x: int",
                        ));
                    };
                    
                    if !self.eat_op(":") {
                        return Err(s_help_site!(
                            "P1007",
                            "Expected ':' after field name",
                            "Write it like: x: int",
                        ));
                    }
                    
                    // For now, just parse the type as an identifier/expression
                    // (we'll ignore types in the interpreter for Phase 1)
                    let type_expr = self.parse_coalesce()?;
                    
                    fields.push((field_name, Some(type_expr)));
                    
                    self.skip_newlines();
                    if !self.eat_op(",") {
                        if !self.peek_op("}") {
                            return Err(s_help_site!(
                                "P1008",
                                "Expected ',' or '}' after field",
                                "Separate fields with commas: { x: int, y: int }",
                            ));
                        }
                    }
                }
            }
            
            variants.push(PEnumVariant {
                name: variant_name,
                fields,
            });
            
            self.skip_stmt_separators();
        }
        
        if variants.is_empty() {
            return Err(s_help_site!(
                "P1009",
                &format!("Enum '{}' has no variants", name),
                "Add at least one variant: idle or loading",
            ));
        }
        
        Ok(PEnumDecl { name, variants })
    }

    fn parse_decl(&mut self) -> Result<PDecl, String> {
        if let Some(t) = self.peek() {
            let kind = t.kind.clone();
            let val = t.value.clone();
            match kind {
                // <>ClassName token — class declaration or matrix
                TokenKind::ClassIdent => {
                    let start_i = self.i;
                    let name = self.toks[self.i].value.clone().unwrap_or_default();
                    self.i += 1;
                    if self.peek_ident() == Some("matrix") {
                        let matrix = self.parse_matrix_decl(name, start_i)?;
                        return Ok(PDecl::Matrix(matrix));
                    }
                    // not a matrix — backtrack and parse as class decl
                    self.i = start_i;
                    let class = self.parse_class_decl()?;
                    if let PExpr::ClassDecl { name, fields, actions, decision, judge, transitions, capacity } = class {
                        return Ok(PDecl::Class { name, fields, actions, decision, judge, transitions, capacity });
                    }
                }

                TokenKind::Ident => {
                    let kw = val.as_deref().unwrap_or("");
                    match kw {
                        "enum" => {
                            let _ = self.eat_ident();
                            let enum_decl = self.parse_enum_decl()?;
                            return Ok(PDecl::Enum(enum_decl));
                        }
                        "act" | "action" => {
                            let _ = self.eat_ident();
                            let act = self.parse_action_after_keyword(kw)?;
                            return Ok(PDecl::Action(act));
                        }
                        _ => {}
                    }
                }

                _ => {}
            }
        }

        let expr = self.parse_assign()?;
        Ok(PDecl::Expr(expr))
    }

    fn parse_class_decl_keyword(&mut self) -> Result<PExpr, String> {
        Err(s_help_site!(
            "P0912",
            "The 'class' keyword isn't used here",
            "Declare a class with '@' instead: @Player = username: \"john\" :: health: 100",
        ))
    }

    fn eat_object_key(&mut self) -> Option<String> {
        if let Some(t) = self.peek() {
            match t.kind {
                goblin_lexer::TokenKind::Ident | goblin_lexer::TokenKind::String => {
                    let val = t.value.clone().unwrap_or_default();
                    self.i += 1;
                    Some(val)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    #[inline]
    fn eat_string_lit(&mut self) -> Option<String> {
        let val = {
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::String = t.kind {
                    t.value.clone()
                } else {
                    None
                }
            } else {
                None
            }
        };
        if val.is_some() {
            self.i += 1;
        }
        val
    }

    #[inline]
    fn bump(&mut self) -> Option<&Token> {
        let idx = self.i;
        self.i += 1;
        self.toks.get(idx)
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.i >= self.toks.len() || matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof))
    }

    fn eat_layout(&mut self) {
        while let Some(t) = self.peek() {
            match t.kind {
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => self.i += 1,
                _ => break,
            }
        }
    }

    #[inline]
    fn is_expr_start(k: &goblin_lexer::TokenKind) -> bool {
        use goblin_lexer::TokenKind;

        match k {
            // identifiers (including capitalized) and <>Ident can begin expressions
            TokenKind::Ident | TokenKind::ClassIdent => true,

            // grouping / collection literals
            TokenKind::Op(op) if op == "(" || op == "[" || op == "{" => true,

            // never starts an expression
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent | TokenKind::Eof => false,
            TokenKind::Op(op) if op == "}" || op == "xx" || op == ";" => false,

            // let the expression parser decide the rest (numbers/strings/etc. as your lexer emits them)
            _ => true,
        }
    }

    fn forbid_next_line_brace(&mut self, header_line: u32, _header_col: u32, who: &str) -> Result<(), String> {

        let saved_i = self.i;

        // Skip ONLY newline tokens after the header.
        let mut j = self.i;
        while j < self.toks.len() && matches!(self.toks[j].kind, TokenKind::Newline) {
            j += 1;
        }

        // If the next significant token is '{' on a later line, that's invalid.
        if j < self.toks.len() {
            self.i = j;
            if self.peek_op("{") {
                let brace_line = self.toks[j].span.line_start;
                if brace_line > header_line {
                    self.i = saved_i;
                    return Err(s_help_site!(
                        "P0203",
                        &format!("Don't put '{{' after the {} header on a new line", who),
                        "Put '{{' on the same line as the header, or use indentation and close with 'end': if ok {{ run() }}",
                    ));
                }
            }
        }

        self.i = saved_i;
        Ok(())
    }

    fn parse_if_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let if_col = self.toks[start_i].span.col_start;

        // 'if'
        debug_assert_eq!(self.peek_ident().as_deref(), Some("if"));
        let _ = self.eat_ident();

        // condition
        let cond_pe = self.parse_assign()?;
        let cond = self.lower_expr(cond_pe);

        // ========== INLINE IF CHECK - NEW CODE STARTS HERE ==========
        // Check for inline if syntax: if condition => statement
        // First skip any newlines/semicolons to find =>
        let mut j = self.i;
        while let Some(tok) = self.toks.get(j) {
            match &tok.kind {
                TokenKind::Newline => { j += 1; continue; }
                TokenKind::Op(s) if s == ";" => { j += 1; continue; }
                _ => break,
            }
        }
        if let Some(tok) = self.toks.get(j) {
            if matches!(tok.kind, TokenKind::Op(ref s) if s == "=>") {
                self.i = j + 1; // skip to after =>
                
                // Skip optional whitespace/newlines after =>
                while let Some(t) = self.peek() {
                    if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                
                // Parse single statement
                let stmt = self.parse_stmt()?;
                
                // Calculate span for the entire inline if
                let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                
                // Create single-statement block
                let then_block = ast::Expr::Block { 
                    stmts: vec![stmt], 
                    span: span.clone() 
                };
                
                // Return inline if (no elif, no else)
                return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![cond, then_block],
                    span
                )));
            }
        }
        // ========== INLINE IF CHECK - NEW CODE ENDS HERE ==========

        // Skip the newline and indent after the condition
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // THEN block: stop at elif/else/end
        let then_stmts = self.parse_indented_block(if_col, &["elif", "else", "end"])?;

        // Consume the Dedent that ended the block
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Vector to store all elif conditions and blocks
        let mut elif_clauses = Vec::new();
        
        // Handle ELIF blocks
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("elif") {
                let _ = self.eat_ident(); // 'elif'
                
                // Parse elif condition
                let elif_cond_pe = self.parse_assign()?;
                let elif_cond = self.lower_expr(elif_cond_pe);
                
                // Skip newline and indent after elif condition
                while let Some(t) = self.peek() {
                    if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                
                // Parse elif block
                let elif_stmts = self.parse_indented_block(if_col, &["elif", "else", "end"])?;
                
                // Skip dedents/newlines after elif block
                while let Some(t) = self.peek() {
                    if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                
                // Create elif block expression
                let elif_block = ast::Expr::Block { stmts: elif_stmts, span: self.toks[self.i].span.clone() };
                
                // Add condition and block to elif_clauses
                elif_clauses.push((elif_cond, elif_block));
            } else {
                break;
            }
        }

        // Optional ELSE block
        let else_stmts = if let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("else") {
                let _ = self.eat_ident(); // 'else'
                
                // Skip newline and indent after 'else'
                while let Some(t) = self.peek() {
                    if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                
                Some(self.parse_indented_block(if_col, &["end"])?)
            } else {
                None
            }
        } else {
            None
        };

        // Skip dedents/newlines after else block
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Consume the closer
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => {
                    let _ = self.eat_ident();
                }
                TokenKind::Op(op) if op == "xx" => {
                    let _ = self.eat_op("xx");
                }
                _ => {
                    return Err(s_help_site!("P0320", "Expected 'end' or 'xx' (crossbones) to close if block", "Add 'end' or 'xx' at the same indentation as 'if'"));
                }
            }
        } else {
            return Err(s_help_site!("P0321", "Expected 'end' or 'xx' (crossbones) to close if block", "Add 'end' or 'xx' before end of file"));
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        // Bodies are statement blocks now
        let then_block = ast::Expr::Block { stmts: then_stmts, span: span.clone() };

        // Create nested if-then-else structure to handle elif clauses
        let result = if let Some(else_stmts) = else_stmts {
            let else_block = ast::Expr::Block { stmts: else_stmts, span: span.clone() };
            
            if elif_clauses.is_empty() {
                // No elif clauses, just the main if-else
                ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![cond, then_block, else_block],
                    span.clone()
                )
            } else {
                // Start with the main if-then and build the chain
                let mut elif_iter = elif_clauses.into_iter().collect::<Vec<_>>();
                
                // Process the last elif clause (connects to else block)
                let (last_elif_cond, last_elif_block) = elif_iter.pop().unwrap();
                let mut current = ast::Expr::FreeCall(
                    "if".to_string(), 
                    vec![last_elif_cond, last_elif_block, else_block],
                    span.clone()
                );
                
                // Build the chain from back to front
                while let Some((elif_cond, elif_block)) = elif_iter.pop() {
                    let elif_expr = ast::Expr::FreeCall(
                        "if".to_string(),
                        vec![
                            elif_cond,
                            elif_block,
                            ast::Expr::Block {
                                stmts: vec![ast::Stmt::Expr(current)],
                                span: span.clone(),
                            },
                        ],
                        span.clone(),
                    );
                    current = elif_expr;
                }
                
                // Add the main if-then at the beginning
                ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![
                        cond,
                        then_block,
                        ast::Expr::Block {
                            stmts: vec![ast::Stmt::Expr(current)],
                            span: span.clone(),
                        },
                    ],
                    span.clone(),
                )
            }
        } else {
            // No else block
            if elif_clauses.is_empty() {
                // No elif clauses, just the main if
                ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![cond, then_block],
                    span.clone()
                )
            } else {
                // Start with the main if-then and build the chain
                let mut elif_iter = elif_clauses.into_iter().collect::<Vec<_>>();
                
                // Process the last elif clause (no else block)
                let (last_elif_cond, last_elif_block) = elif_iter.pop().unwrap();
                let mut current = ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![last_elif_cond, last_elif_block],
                    span.clone()
                );
                
                // Build the chain from back to front
                while let Some((elif_cond, elif_block)) = elif_iter.pop() {
                    let elif_expr = ast::Expr::FreeCall(
                        "if".to_string(),
                        vec![
                            elif_cond,
                            elif_block,
                            ast::Expr::Block {
                                stmts: vec![ast::Stmt::Expr(current)],
                                span: span.clone(),
                            },
                        ],
                        span.clone(),
                    );
                    current = elif_expr;
                }
                
                // Add the main if-then at the beginning
                ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![
                        cond,
                        then_block,
                        ast::Expr::Block {
                            stmts: vec![ast::Stmt::Expr(current)],
                            span: span.clone(),
                        },
                    ],
                    span,
                )
            }
        };

        Ok(ast::Stmt::Expr(result))
    }

    fn parse_unless_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let unless_col = self.toks[start_i].span.col_start;

        // 'unless'
        debug_assert_eq!(self.peek_ident().as_deref(), Some("unless"));
        let _ = self.eat_ident();

        // condition (negated): unless <cond>  =>  if !<cond>
        let cond_pe = self.parse_assign()?;
        let cond_pe = PExpr::Prefix("!".into(), Box::new(cond_pe));
        let cond = self.lower_expr(cond_pe);

        // Skip the newline and indent after the condition
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // THEN block: stop at else/end
        let then_stmts = self.parse_indented_block(unless_col, &["else", "end"])?;

        // Optional ELSE block
        let else_stmts = if let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("else") {
                let _ = self.eat_ident(); // 'else'
                
                // Skip newline and indent after 'else'
                while let Some(t) = self.peek() {
                    if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                
                Some(self.parse_indented_block(unless_col, &["end"])?)
            } else {
                None
            }
        } else {
            None
        };

        // Skip dedents/newlines after else block
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Consume the closer
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => {
                    let _ = self.eat_ident();
                }
                TokenKind::Op(op) if op == "xx" => {
                    let _ = self.eat_op("xx");
                }
                _ => {
                    return Err(s_help_site!("P0320", "Expected 'end' or 'xx' (crossbones) to close unless block", "Add 'end' or 'xx' at the same indentation as 'unless'"));
                }
            }
        } else {
            return Err(s_help_site!("P0321", "Expected 'end' or 'xx' (crossbones) to close unless block", "Add 'end' or 'xx' before end of file"));
        }

        // Convert stmt blocks -> arrays of exprs (same contract as 'if')
        let to_exprs = |stmts: Vec<ast::Stmt>| -> Result<Vec<ast::Expr>, String> {
            stmts
                .into_iter()
                .map(|s| match s {
                    ast::Stmt::Expr(e) => Ok(e),
                    ast::Stmt::Bind(_) | ast::Stmt::TupleBind(_) => {
                        Err("Bind statements (`|`, `|=`, `[=`) are not expressions.".to_string())
                    }
                    ast::Stmt::Return(ret_stmt) => {
                        let values: Vec<ast::Expr> = ret_stmt.values.clone();
                        Ok(ast::Expr::FreeCall("return".to_string(), values, ret_stmt.span.clone()))
                    }
                    _ => Err(s_help_site!(
                        "P0311",
                        "Only expressions and variable assignments are allowed inside control flow blocks.",
                        "Move class/action/enum declarations outside the if/while/unless block.",
                    )),
                })
                .collect()
        };

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        // Bodies are statement blocks now; no P0311 conversion.
        let then_block = ast::Expr::Block { stmts: then_stmts, span: span.clone() };

        let mut args = vec![cond, then_block];
        if let Some(else_stmts) = else_stmts {
            let else_block = ast::Expr::Block { stmts: else_stmts, span: span.clone() };
            args.push(else_block);
        }

        Ok(ast::Stmt::Expr(ast::Expr::FreeCall("if".to_string(), args, span)))
    }

    fn parse_for_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        
        let start_i = self.i;
        let for_col = self.toks[start_i].span.col_start;
        
        debug_assert_eq!(self.peek_ident().as_deref(), Some("for"));
        let _ = self.eat_ident();
        
        // Parse loop variable: for name in ...
        let Some(var_name) = self.eat_ident() else {
            return Err(s_help_site!("P0330", "Expected variable name after 'for'", "Write: for item in items"));
        };
        
        // Expect 'in' keyword
        if self.peek_ident() != Some("in") {
            return Err(s_help_site!("P0331", "Expected 'in' after loop variable", "Write: for item in items"));
        }
        let _ = self.eat_ident();
        
        // Parse iterable expression
        let iterable_pe = self.parse_assign()?;
        let iterable = self.lower_expr(iterable_pe);
        
        // Skip newline/indent
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }
        
        // Parse body
        let body_stmts = self.parse_indented_block(for_col, &["end"])?;
        
        // Skip dedents
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }
        
        // Consume closer
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => {
                    let _ = self.eat_ident();
                }
                TokenKind::Op(op) if op == "xx" => {
                    let _ = self.eat_op("xx");
                }
                _ => {
                    return Err(s_help_site!("P0332", "Expected 'end' or 'xx' (crossbones) to close for loop", "Add 'end' or 'xx'"));
                }
            }
        } else {
            return Err(s_help_site!("P0332", "Expected 'end' or 'xx' (crossbones) to close for loop", "Add 'end' or 'xx' before end of file"));
        }
        
        // Convert body
        let to_exprs = |stmts: Vec<ast::Stmt>| -> Result<Vec<ast::Expr>, String> {
            stmts.into_iter().map(|s| match s {
                ast::Stmt::Expr(e) => Ok(e),
                ast::Stmt::Bind(_) | ast::Stmt::TupleBind(_) => {
                    Err("Bind statements (`|`, `|=`, `[=`) are not expressions.".to_string())
                }
                ast::Stmt::Return(ret_stmt) => {
                    let values: Vec<ast::Expr> = ret_stmt.values.clone();
                    Ok(ast::Expr::FreeCall("return".to_string(), values, ret_stmt.span.clone()))
                }
                _ => Err(s_help_site!("P0333", "Only expressions allowed in for loop", "Move declarations outside")),
            }).collect()
        };
        
        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        // Body is a statement block now.
        let body_block = ast::Expr::Block { stmts: body_stmts, span: span.clone() };

        // FreeCall("for", [var_name_str, iterable, body_block])
        let args = vec![
            ast::Expr::Str(var_name, span.clone()),
            iterable,
            body_block,
        ];
        Ok(ast::Stmt::Expr(ast::Expr::FreeCall("for".to_string(), args, span)))
    }

    fn parse_while_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let while_col = self.toks[start_i].span.col_start;

        // 'while'
        debug_assert_eq!(self.peek_ident().as_deref(), Some("while"));
        let _ = self.eat_ident();

        // condition
        let cond_pe = self.parse_assign()?;
        let cond = self.lower_expr(cond_pe);

        // Skip the newline and indent after the condition
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // BODY block: stop at end/xx
        let body_stmts = self.parse_indented_block(while_col, &["end"])?;

        // Skip dedents/newlines after body
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Consume closer
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => { 
                    let _ = self.eat_ident(); 
                }
                TokenKind::Op(op) if op == "xx" => { 
                    let _ = self.eat_op("xx"); 
                }
                _ => {
                    return Err(s_help_site!("P0340", "Expected 'end' or 'xx' (crossbones) to close while block", "Add 'end' or 'xx'"));
                }
            }
        } else {
            return Err(s_help_site!("P0340", "Expected 'end' or 'xx' (crossbones) to close while block", "Add 'end' or 'xx' before end of file"));
        }

        // Convert stmt list -> expr array
        let to_exprs = |stmts: Vec<ast::Stmt>| -> Result<Vec<ast::Expr>, String> {
            stmts
                .into_iter()
                .map(|s| match s {
                    ast::Stmt::Expr(e) => Ok(e),
                    ast::Stmt::Bind(_) | ast::Stmt::TupleBind(_) => {
                        Err("Bind statements (`|`, `|=`, `[=`) are not expressions.".to_string())
                    }
                    ast::Stmt::Return(ret_stmt) => {
                        let values: Vec<ast::Expr> = ret_stmt.values.clone();
                        Ok(ast::Expr::FreeCall("return".to_string(), values, ret_stmt.span.clone()))
                    }
                    _ => Err(s_help_site!(
                        "P0311",
                        "Only expressions and variable assignments are allowed inside control flow blocks.",
                        "Move class/action/enum declarations outside the if/while/unless block.",
                    )),
                })
                .collect()
        };
        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        // Body is a statement block now; no P0311 conversion.
        let body_block = ast::Expr::Block { stmts: body_stmts, span: span.clone() };

        let args = vec![cond, body_block];
        Ok(ast::Stmt::Expr(ast::Expr::FreeCall("while".to_string(), args, span)))
    }

    fn parse_repeat_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let repeat_col = self.toks[start_i].span.col_start;
        debug_assert_eq!(self.peek_ident().as_deref(), Some("repeat"));
        let _ = self.eat_ident();

        let is_word = |t: &goblin_lexer::Token, word: &str| {
            matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some(word)
        };

        // Bare repeat (infinite loop) — nothing before newline / closer
        let (count, as_name) = if self.peek_newline_or_eof()
            || self.peek_op("xx")
            || matches!(self.peek(), Some(t) if is_word(t, "end"))
        {
            (None, None)
        } else {
            // Parse the repeat target/count/condition
            let expr_pe = self.parse_assign()?;

            // Optional: `as name`
            let alias = match self.peek() {
                Some(t) if is_word(t, "as") => {
                    let _ = self.eat_ident(); // consume 'as'
                    let Some(name) = self.eat_ident() else {
                        return Err(s_help_site!(
                            "P0324",
                            "Expected a name after 'as' in repeat",
                            "Write: repeat items as item"
                        ));
                    };
                    Some(name)
                }
                _ => None,
            };

            (Some(self.lower_expr(expr_pe)), alias)
        };

        // Skip newline/indent after header
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // IMPORTANT: let the block parser stop on BOTH end and xx
        let body_stmts = self.parse_indented_block(repeat_col, &["end", "xx"])?;

        // Skip dedents/newlines before closer
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Consume closer
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => {
                    let _ = self.eat_ident();
                }
                TokenKind::Op(op) if op == "xx" => {
                    let _ = self.eat_op("xx");
                }
                _ => {
                    return Err(s_help_site!(
                        "P0322",
                        "Expected 'end' or 'xx' (crossbones) to close repeat block",
                        "Add 'end' or 'xx'"
                    ));
                }
            }
        } else {
            return Err(s_help_site!(
                "P0322",
                "Expected 'end' or 'xx' (crossbones) to close repeat block",
                "Add 'end' or 'xx' before end of file"
            ));
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        let body_block = ast::Expr::Block {
            stmts: body_stmts,
            span: span.clone(),
        };

        let expr_arg = count.unwrap_or(ast::Expr::Nil(span.clone()));
        let as_arg = match as_name {
            Some(name) => ast::Expr::Str(name, span.clone()),
            None => ast::Expr::Nil(span.clone()),
        };

        Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
            "repeat".to_string(),
            vec![expr_arg, body_block, as_arg],
            span,
        )))
    }

    fn parse_collect_stmt(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        debug_assert_eq!(self.peek_ident().as_deref(), Some("collect"));
        let _ = self.eat_ident(); // consume 'collect'

        self.skip_layout();

        // parse count expr
        let count = self.parse_primary()?;

        self.skip_layout();

        // expect 'of'
        if self.peek_ident().as_deref() != Some("of") {
            return Err(s_help_site!(
                "P0340",
                "Expected 'of' after count in collect",
                "Write: collect 4 of goblin_ipsum_sentence()"
            ));
        }
        let _ = self.eat_ident(); // consume 'of'

        self.skip_layout();

        // parse body expr
        let body = self.parse_coalesce()?;

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
            "collect".to_string(),
            vec![self.lower_expr(count), self.lower_expr(body)],
            span,
        )))
    }

    fn parse_stmt_block_until<F>(&mut self, mut stop: F) -> Result<Vec<ast::Stmt>, String>
    where
        F: FnMut(&mut Parser<'_>) -> bool,
    {
        use goblin_lexer::TokenKind;
        let mut out: Vec<ast::Stmt> = Vec::new();
        loop {
            // Skip any number of blank lines AND layout tokens between statements
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }
            
            // If caller's stopper says "stop now", we stop BEFORE the stopper token.
            if stop(self) {
                break;
            }
            
            // Parse one statement
            let stmt = self.parse_stmt()?;
            out.push(stmt);
        }
        Ok(out)
    }

    fn parse_attempt_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let attempt_col = self.toks[start_i].span.col_start;

        // 'attempt'
        debug_assert_eq!(self.peek_ident().as_deref(), Some("attempt"));
        let _ = self.eat_ident();

        // Skip newline/indent after 'attempt'
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // ATTEMPT block: stop at rescue/ensure/end
        let attempt_stmts = self.parse_indented_block(attempt_col, &["rescue", "ensure", "end"])?;

        // Consume dedents/newlines after attempt body (match other blocks)
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Optional RESCUE blocks (0..*)
        let mut rescue_blocks: Vec<(Option<String>, Vec<ast::Stmt>)> = Vec::new();
        loop {
            if self.peek_ident().as_deref() != Some("rescue") {
                break;
            }
            let _ = self.eat_ident(); // 'rescue'

            // Optional error binding: `rescue err`
            // (Only bind if next ident isn't 'ensure' or 'end')
            let error_var = match self.peek_ident().as_deref() {
                Some("ensure") | Some("end") => None,
                Some(_) => self.eat_ident(), // Some(var_name)
                None => None,
            };

            // Skip newline/indent after 'rescue'
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Rescue body: stop at rescue/ensure/end
            let rescue_stmts = self.parse_indented_block(attempt_col, &["rescue", "ensure", "end"])?;
            rescue_blocks.push((error_var, rescue_stmts));

            // Consume dedents/newlines after each rescue body
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                    self.i += 1;
                } else {
                    break;
                }
            }
        }

        // Optional ENSURE block
        let ensure_stmts = if self.peek_ident().as_deref() == Some("ensure") {
            let _ = self.eat_ident(); // 'ensure'

            // Skip newline/indent after 'ensure'
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Newline | TokenKind::Indent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Ensure body: stop at end
            let body = self.parse_indented_block(attempt_col, &["end"])?;

            // Consume dedents/newlines after ensure body
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            Some(body)
        } else {
            None
        };

        // Consume the closer ('end' or 'xx') — same pattern as others
        if let Some(t) = self.peek() {
            match &t.kind {
                TokenKind::Ident if t.value.as_deref() == Some("end") => { let _ = self.eat_ident(); }
                TokenKind::Op(op) if op == "xx" => { let _ = self.eat_op("xx"); }
                _ => {
                    return Err(s_help_site!(
                        "P0351",
                        "Expected 'end' or 'xx' to close attempt block",
                        "Add 'end' or 'xx' at the same indentation as 'attempt'",
                    ));
                }
            }
        } else {
            return Err(s_help_site!(
                "P0351",
                "Expected 'end' or 'xx' to close attempt block",
                "Add 'end' or 'xx' before end of file",
            ));
        }

        // ------ Lower inner stmt blocks to arrays of exprs (expressions only inside) ------
        let to_exprs = |stmts: Vec<ast::Stmt>| -> Result<Vec<ast::Expr>, String> {
            stmts
                .into_iter()
                .map(|s| match s {
                    ast::Stmt::Expr(e) => Ok(e),
                    ast::Stmt::Bind(_) | ast::Stmt::TupleBind(_) => {
                        Err("Bind statements (`|`, `|=`, `[=`) are statements, not expressions.".to_string())
                    }
                    ast::Stmt::Return(ret_stmt) => {
                        let values: Vec<ast::Expr> = ret_stmt.values.clone();
                        Ok(ast::Expr::FreeCall("return".to_string(), values, ret_stmt.span.clone()))
                    }
                    _ => Err(s_help_site!(
                        "P0352",
                        "Only expressions allowed inside attempt/rescue/ensure blocks.",
                        "Move declarations outside the block.",
                    )),
                })
                .collect()
        };

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

        // attempt body as a statement block
        let attempt_block = ast::Expr::Block { stmts: attempt_stmts, span: span.clone() };

        // rescue blocks -> Array of [var_or_nil, Block]
        let rescues_expr = {
            let mut pairs: Vec<ast::Expr> = Vec::new();
            for (var_opt, stmts) in rescue_blocks {
                // If your var_opt is a String: use it directly.
                // If it's an Ident (String, Span): use the String part.
                let var_expr = match var_opt {
                    Some(name) => ast::Expr::Str(name, span.clone()),
                    None => ast::Expr::Ident("nil".into(), span.clone()),
                };
                let body_block = ast::Expr::Block { stmts, span: span.clone() };
                pairs.push(ast::Expr::Array(vec![var_expr, body_block], span.clone()));
            }
            ast::Expr::Array(pairs, span.clone())
        };

        let mut args = vec![attempt_block, rescues_expr];

        if let Some(ens) = ensure_stmts {
            args.push(ast::Expr::Block { stmts: ens, span: span.clone() });
        }

        Ok(ast::Stmt::Expr(ast::Expr::FreeCall("attempt".to_string(), args, span)))
    }

    pub fn parse_module(mut self) -> ParseResult<ast::Module> {
        // --- front-gate: disallow '=' or ':' as the first non-newline token ---
        {
            // Look from the beginning; do not advance self.i.
            let mut j = 0usize;
            while let Some(t) = self.toks.get(j) {
                if matches!(t.kind, TokenKind::Newline) { j += 1; } else { break; }
            }
            if let Some(first) = self.toks.get(j) {
                if let TokenKind::Op(ref op) = first.kind {
                    if op == "=" {
                        return Err(derr_help(
                            "P0104",
                            &format!("A statement can't start with '{}'", op),
                            "Start with a keyword or a name: x = 1 or if ok … end",
                            first.span.clone(),
                        ));
                    }
                }
            }
        }

        // Top-level guard: "<>Class" must be followed by '|' or 'matrix' on the same head line.
        if let Some(tok0) = self.peek() {
            if matches!(tok0.kind, TokenKind::ClassIdent) {
                let mut k = self.i + 1;
                while let Some(t) = self.toks.get(k) {
                    match &t.kind {
                        TokenKind::Op(s) if s == "|" => { break; }
                        TokenKind::Ident => {
                            if t.value.as_deref() == Some("matrix") { break; }
                            k += 1;
                        }
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent | TokenKind::Eof => break,
                        _ => { k += 1; }
                    }
                }
            }
        }

        let mut items = Vec::new();
        self.eat_layout();

        while !self.is_eof() {
            // Skip layout; error on orphan closers here instead of silently eating them
            loop {
                match self.peek().map(|t| &t.kind) {
                    Some(k) if Self::is_expr_start(k) => break,
                    Some(TokenKind::Eof) | None => break,

                    // let layout flow
                    Some(TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) => {
                        self.i += 1;
                    }

                    // orphan '}' at top level
                    Some(TokenKind::Op(op)) if op == "}" => {
                        return Err(derr_help(
                            "P0224",
                            "Found '}' without a matching '{'",
                            "Remove this '}' or add the opening brace above to match it.",
                            self.toks[self.i].span.clone(),
                        ));
                    }

                    // orphan 'end' at top level
                    Some(TokenKind::Ident) if self.peek_ident() == Some("end") => {
                        return Err(derr_help(
                            "P0223",
                            "Found 'end' without a matching block start",
                            "Remove this 'end' or add the missing header above (e.g., `if ok` … `end`).",
                            self.toks[self.i].span.clone(),
                        ));
                    }

                    // orphan 'xx' at top level
                    Some(TokenKind::Op(op)) if op == "xx" => {
                        return Err(derr_help(
                            "P0223",
                            "Found 'xx' (crossbones) without a matching block start",
                            "Remove this 'xx' or add the missing header above (e.g., `if ok` … `xx`).",
                            self.toks[self.i].span.clone(),
                        ));
                    }

                    // anything else: advance (comments, stray symbols, etc.)
                    _ => {
                        self.i += 1;
                    }
                }
            }

            if self.is_eof() { break; }

            // -------- progress guard: bail if parse_stmt() consumes nothing --------
            let before_i = self.i;

            match self.parse_stmt() {
                Ok(stmt) => {
                    if self.i == before_i {
                        // No forward progress => hard error to avoid infinite loop
                        let sp = if let Some(tok) = self.peek() {
                            tok.span.clone()
                        } else if let Some(last) = self.toks.last() {
                            last.span.clone()
                        } else {
                            goblin_diagnostics::Span::new("<eof>", 0, 0, 0, 0, 0, 0)
                        };
                        return Err(derr_help(
                            "P0101",
                            "The parser got stuck and didn't consume any tokens.",
                            "Check for invalid or unsupported syntax near here, or remove the stray token.",
                            sp,
                        ));
                    }
                    // Flatten matrix blocks into top-level stmts so class decl and binds aren't scoped away
                    match stmt {
                        ast::Stmt::Block { stmts: block_stmts, .. }
                            if block_stmts.iter().all(|s| matches!(s, ast::Stmt::Bind(_) | ast::Stmt::Class(_))) =>
                        {
                            items.extend(block_stmts);
                        }
                        other => items.push(other),
                    }
                }
                Err(msg) => {
                    // Promote the String error EXACTLY as produced by s/s_help_site! into a Diagnostic.
                    // Your CLI expects the code to appear in the first line of the message (if present),
                    // and finds the help line by "help:" on subsequent lines.
                    let sp = if let Some(tok) = self.peek() {
                        tok.span.clone()
                    } else if let Some(last) = self.toks.last() {
                        last.span.clone()
                    } else {
                        goblin_diagnostics::Span::new("<eof>", 0, 0, 0, 0, 0, 0)
                    };
                    let msg_static: &'static str = Box::leak(msg.into_boxed_str());
                    return Err(vec![goblin_diagnostics::Diagnostic::error(
                        "", // category is ignored by your formatter; message carries "P####: ..." when present
                        msg_static,
                        sp,
                    )]);
                }
            }

            self.eat_layout();
        }

        // Ensure nothing but NEWLINE/EOF remains after the last statement
        self.ensure_no_trailing_tokens_after_parse()?;

        Ok(ast::Module { items })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        // Strip leading : from builtin calls at statement level
        if self.peek_op(":") {
            if let Some(next) = self.toks.get(self.i + 1) {
                if matches!(next.kind, TokenKind::Ident) {
                    self.i += 1; // consume the colon
                }
            }
        }

        // --- block-local bind: local <name> = <expr>
        if self.peek_ident() == Some("local") {
            return self.parse_local_bind();
        }

        if self.peek_is_return() {
            return self.parse_return_stmt();
        }

        if self.peek_ident() == Some("imm") {
            return self.parse_bind_stmt();
        }

        // Box variable write: #namespace::name | value
        if let Some(tok) = self.peek() {
            if matches!(tok.kind, TokenKind::HashIdent) {
                let tok = self.toks[self.i].clone();
                let raw = tok.value.clone().unwrap_or_default();
                if let Some(pos) = raw.find("::") {
                    let namespace = raw[..pos].to_string();
                    let name = raw[pos + 2..].to_string();
                    let span = tok.span.clone();
                    self.i += 1;

                    let mode = if self.eat_op("|=") {
                        ast::BindMode::Retether
                    } else if self.eat_op("|") {
                        ast::BindMode::Tether
                    } else {
                        return Err(s_help_site!(
                            "P0420",
                            "Expected '|' or '|=' after Box variable",
                            "Write: #namespace::name | value",
                        ));
                    };

                    let rhs_pe = self.parse_coalesce()?;
                    let rhs = self.lower_expr(rhs_pe);

                    return Ok(ast::Stmt::BoxBind { namespace, name, expr: rhs, mode, span });
                }
            }
        }

        // ---- Friendly guard: looks like a class header but missing '@'
        // Pattern: Capitalized Ident '=' Ident ':'  (e.g., A = n: 1)
        if let Some(t0) = self.peek() {
            if matches!(t0.kind, TokenKind::Ident) {
                if let Some(t1) = self.toks.get(self.i + 1) {
                    // Check for IDENT | or IDENT |=
                    if matches!(t1.kind, TokenKind::Op(ref s) if s == "|" || s == "|=" || s == "<>")
                        || matches!(t1.kind, TokenKind::ClassIdent)
                        || matches!(t1.kind, TokenKind::Shadow)
                        || matches!(t1.kind, TokenKind::Op(ref s) if s == ",")
                    {
                        return self.parse_bind_stmt();
                    }
                    // Check for typed tether: IDENT . TYPE_KEYWORD |
                    if matches!(t1.kind, TokenKind::Op(ref s) if s == ".") {
                        if let Some(t2) = self.toks.get(self.i + 2) {
                            let is_type_kw = matches!(&t2.kind, TokenKind::Ident)
                                && t2.value.as_deref().map(|w| TYPE_LOCK_KEYWORDS.contains(&w)).unwrap_or(false);
                            if is_type_kw {
                                if let Some(t3) = self.toks.get(self.i + 3) {
                                    if matches!(t3.kind, TokenKind::Op(ref s) if s == "|") {
                                        return self.parse_bind_stmt();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if self.peek_ident() == Some("stop") {
            let start_i = self.i;
            let _ = self.eat_ident(); // consume stop

            if self.peek_ident() == Some("if") {
                let _ = self.eat_ident(); // consume if

                if !self.eat_op(":") {
                    return Err(s_help_site!(
                        "P0325",
                        "Expected ':' after 'stop if'",
                        "Write: stop if: condition"
                    ));
                }

                let cond_pe = self.parse_coalesce()?;
                let cond = self.lower_expr(cond_pe);

                let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

                let stop_stmt = ast::Stmt::Expr(ast::Expr::FreeCall(
                    "stop".to_string(),
                    vec![],
                    span.clone(),
                ));

                let then_block = ast::Expr::Block {
                    stmts: vec![stop_stmt],
                    span: span.clone(),
                };

                return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![cond, then_block],
                    span,
                )));
            }

            // plain stop
            let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
            return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                "stop".to_string(),
                vec![],
                span,
            )));
        }

        if self.peek_ident() == Some("skip") {
            let start_i = self.i;
            let _ = self.eat_ident(); // consume skip

            if self.peek_ident() == Some("if") {
                let _ = self.eat_ident(); // consume if

                if !self.eat_op(":") {
                    return Err(s_help_site!(
                        "P0326",
                        "Expected ':' after 'skip if'",
                        "Write: skip if: condition"
                    ));
                }

                let cond_pe = self.parse_coalesce()?;
                let cond = self.lower_expr(cond_pe);

                let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

                let skip_stmt = ast::Stmt::Expr(ast::Expr::FreeCall(
                    "skip".to_string(),
                    vec![],
                    span.clone(),
                ));

                let then_block = ast::Expr::Block {
                    stmts: vec![skip_stmt],
                    span: span.clone(),
                };

                return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                    "if".to_string(),
                    vec![cond, then_block],
                    span,
                )));
            }

            // plain skip
            let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
            return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                "skip".to_string(),
                vec![],
                span,
            )));
        }

        // -------- grid(...) with optional hierarchy sub-block --------
        // Handles:
        //   world | grid("world", 1024, 1024)
        //   world | grid("world", 1024, 1024, 8)
        //       tile 32 by 32
        //       regions 16
        if self.peek_word("grid") {
            let start_i = self.i;
            self.i += 1; // consume 'grid'

            if !self.eat_op("(") {
                // Not a call — reset and fall through
                self.i = start_i;
            } else {
                // Parse the argument list manually
                let mut call_args: Vec<PExpr> = Vec::new();
                loop {
                    self.skip_newlines();
                    if self.eat_op(")") { break; }
                    if !call_args.is_empty() {
                        if !self.eat_op(",") {
                            return Err(s_help_site!(
                                "P0801",
                                "Expected ',' or ')' in grid_new argument list",
                                "Write: grid_new(\"world\", 1024, 1024) or grid_new(\"world\", 1024, 1024, 8)",
                            ));
                        }
                        self.skip_newlines();
                    }
                    let arg = self.parse_coalesce()?;
                    call_args.push(arg);
                }

                // Check for optional sub-block (newline + indent)
                let saved = self.i;
                self.skip_newlines();
                let has_block = matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, TokenKind::Indent));

                let mut tile_w:   i64 = -1;
                let mut tile_h:   i64 = -1;
                let mut regions:  i64 = -1;

                if has_block {
                    self.i += 1; // consume Indent

                    loop {
                        self.skip_newlines();
                        match self.toks.get(self.i) {
                            Some(t) if matches!(t.kind, TokenKind::Dedent) => {
                                self.i += 1;
                                break;
                            }
                            None => break,
                            _ => {}
                        }

                        if self.peek_word("tile") {
                            self.i += 1; // consume 'tile'
                            let tw_pe = self.parse_coalesce()?;
                            if !self.eat_word("by") {
                                return Err(s_help_site!(
                                    "P0802",
                                    "Expected 'by' in tile declaration",
                                    "Write: tile 32 by 32",
                                ));
                            }
                            let th_pe = self.parse_coalesce()?;
                            // Extract literal ints from PExpr before lowering
                            if let PExpr::Int(s) = &tw_pe { tile_w = s.parse::<i64>().unwrap_or(-1); }
                            if let PExpr::Int(s) = &th_pe { tile_h = s.parse::<i64>().unwrap_or(-1); }
                            if tile_w <= 0 || tile_h <= 0 {
                                return Err(s_help_site!(
                                    "P0803",
                                    "Tile dimensions must be positive integers",
                                    "Write: tile 32 by 32",
                                ));
                            }
                        } else if self.peek_word("regions") {
                            self.i += 1; // consume 'regions'
                            let r_pe = self.parse_coalesce()?;
                            if let PExpr::Int(s) = &r_pe { regions = s.parse::<i64>().unwrap_or(-1); }
                            if regions <= 0 {
                                return Err(s_help_site!(
                                    "P0804",
                                    "Region count must be a positive integer",
                                    "Write: regions 16",
                                ));
                            }
                        } else {
                            return Err(s_help_site!(
                                "P0805",
                                "Unexpected declaration in grid_new block",
                                "Only 'tile N by N' and 'regions N' are valid here",
                            ));
                        }

                        self.skip_newlines();
                    }
                } else {
                    // No block — reset to after the call args
                    self.i = saved;
                }

                // Build the lowered call args
                let sp = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                let mut lowered: Vec<ast::Expr> = call_args.into_iter()
                    .map(|a| self.lower_expr(a))
                    .collect();

                // Pad to at least 4 args (name, w, h, mode) with -1 sentinels
                while lowered.len() < 4 {
                    lowered.push(self.lower_expr(PExpr::Int("-1".to_string())));
                }

                // Append tile_w, tile_h, regions
                lowered.push(self.lower_expr(PExpr::Int(tile_w.to_string())));
                lowered.push(self.lower_expr(PExpr::Int(tile_h.to_string())));
                lowered.push(self.lower_expr(PExpr::Int(regions.to_string())));

                return Ok(ast::Stmt::Expr(ast::Expr::FreeCall(
                    "grid".to_string(),
                    lowered,
                    sp,
                )));
            }
        }

        // -------- keyword-first dispatch (keeps style consistent) --------
        if self.peek_ident() == Some("if") {
            return self.parse_if_stmt();
        }

        if self.peek_ident() == Some("unless") {
            return self.parse_unless_stmt();
        }

        if self.peek_ident() == Some("while") {
            return self.parse_while_stmt();
        }

        if let Some("for") = self.peek_ident() {
            return self.parse_for_stmt();
        }

        if let Some("repeat") = self.peek_ident() {
            return self.parse_repeat_stmt();
        }

        if self.peek_ident() == Some("collect") {
            return self.parse_collect_stmt();
        }

        if self.peek_ident() == Some("attempt") {
            return self.parse_attempt_stmt();
        }

        if self.peek_ident() == Some("provoke") {
            return self.parse_provoke_stmt();
        }

        if self.peek_ident() == Some("judge") {
            return self.parse_judge_stmt();
        }

        if self.peek_ident() == Some("judge_all") {
            return self.parse_judge_all_stmt();
        }

        if self.peek_ident() == Some("sweep") {
            return self.parse_sweep_stmt(ast::SweepMode::Match);
        }

        if self.peek_ident() == Some("sweep_all") {
            return self.parse_sweep_stmt(ast::SweepMode::All);
        }

        // Check for import statements
        if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Import)) {
            return self.parse_import();
        }

        if self.peek_ident() == Some("act") {
            let _ = self.eat_ident(); // 'act'
            return self.parse_free_action("act");
        }

        if self.peek_ident() == Some("action") {
            let _ = self.eat_ident(); // 'action'
            return self.parse_free_action("action");
        }

        // -------- enum declarations --------
        if self.peek_ident() == Some("enum") {
            let _ = self.eat_ident(); // consume 'enum'
            let start_i = self.i;
            let enum_decl = self.parse_enum_decl()?;
            let sp = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
            return self.lower_enum_stmt(enum_decl, sp);
        }

        // -------- overlay declarations and applications --------
        if self.peek_ident() == Some("overlay") {
            return self.parse_overlay_stmt();
        }

        // -------- detach overlay from host --------
        if self.peek_ident() == Some("detach") {
            return self.parse_overlay_detach();
        }

        // -------- link definitions --------
        // class-level: link ClassName by [ formula ]
        if self.peek_ident() == Some("link") {
            return self.parse_link_def();
        }

        // -------- unit declarations --------
        if self.peek_ident() == Some("unit") {
            return self.parse_unit_decl();
        }

        // object-level: ObjectName link by [ formula ]
        // object-level: ObjectName score | decision against Class by [ formula ]
        if let Some(t0) = self.peek() {
            if matches!(t0.kind, TokenKind::Ident) {
                let next_tok = self.toks.get(self.i + 1)
                    .and_then(|t| t.value.as_deref().map(str::to_owned));
                if next_tok.as_deref() == Some("link") {
                    return self.parse_object_link_def();
                }
                if next_tok.as_deref() == Some("score") {
                    return self.parse_object_decision();
                }
            }
        }

        // clear link VarA to VarB on channel
        if self.peek_ident() == Some("clear") {
            if self.toks.get(self.i + 1)
                .and_then(|t| t.value.as_deref())
                == Some("link")
            {
                return self.parse_clear_link();
            }
        }

        // -------- free action: dedicated token from the lexer --------
        if let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Act) {
                self.i += 1;                 // eat the Act token
                return self.parse_free_action("act");
            }
        }

        // action (keyword token form)
        if let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Action) {
                self.i += 1; // consume the keyword token
                return self.parse_free_action("action");
            }
        }

        // -------- class declarations / matrices: <>ClassName --------
        if let Some(tok0) = self.peek().cloned() {
            if matches!(tok0.kind, TokenKind::ClassIdent) {
                let start_i = self.i;
                let name = tok0.value.clone().unwrap_or_default();
                let next_is_matrix = self.toks.get(self.i + 1)
                    .and_then(|t| t.value.as_deref().map(str::to_owned))
                    .as_deref() == Some("matrix");
                if next_is_matrix {
                    self.i += 1; // consume <>Name
                    let pe = self.parse_matrix_decl(name, start_i)?;
                    let sp = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                    return self.lower_matrix_stmt(pe, sp);
                }
                let pe = self.parse_class_decl()?;
                let sp = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                return self.lower_class_stmt_from_pexpr(pe, sp);
            }
        }

        // -------- explicit bind/shadow/imm dispatch --------
        // Forms:
        //   imm IDENT (=|[=) expr
        //   IDENT (=|[=) expr
        if self.peek_word("imm") {
            // Lookahead: imm IDENT (=|[=)
            if let (Some(t1), Some(t2)) = (self.toks.get(self.i + 1), self.toks.get(self.i + 2)) {
                let is_ident = matches!(t1.kind, TokenKind::Ident);
                let is_bind_start =
                    matches!(t2.kind, TokenKind::Op(ref s) if s == "|")
                    || matches!(t2.kind, TokenKind::Shadow)
                    || matches!(t2.kind, TokenKind::Op(ref s) if s == ",");
                if is_ident && is_bind_start {
                    return self.parse_bind_stmt();
                }
            }
        }
        if let Some(t0) = self.peek() {
            if matches!(t0.kind, TokenKind::Ident) {
                if let Some(t1) = self.toks.get(self.i + 1) {
                    if matches!(t1.kind, TokenKind::Op(ref s) if s == "|")
                        || matches!(t1.kind, TokenKind::Shadow)
                        || matches!(t1.kind, TokenKind::Op(ref s) if s == ",") // NEW
                    {
                        return self.parse_bind_stmt();
                    }
                }
            }
        }
        // -------- grid[x, y] | value  and  grid[x, y] |= value --------
        // Detects IDENT [ ... , ... ] followed by | or |=
        // and lowers to Expr::Binary(Index2(...), "|" or "|=", rhs).
        if let Some(t0) = self.peek().cloned() {
            if matches!(t0.kind, TokenKind::Ident) {
                if let Some(t1) = self.toks.get(self.i + 1) {
                    if matches!(t1.kind, TokenKind::Op(ref s) if s == "[") {
                        let mut k = self.i + 2;
                        let mut depth = 1usize;
                        let mut found_comma = false;
                        let mut found_bind = false;
                        while let Some(t) = self.toks.get(k) {
                            match &t.kind {
                                TokenKind::Op(s) if s == "[" => { depth += 1; k += 1; }
                                TokenKind::Op(s) if s == "]" => {
                                    depth -= 1;
                                    k += 1;
                                    if depth == 0 {
                                        while matches!(self.toks.get(k), Some(t) if matches!(t.kind, TokenKind::Newline)) { k += 1; }
                                        if let Some(tnext) = self.toks.get(k) {
                                            if matches!(tnext.kind, TokenKind::Op(ref s) if s == "|" || s == "|=") {
                                                found_bind = true;
                                            }
                                        }
                                        break;
                                    }
                                }
                                TokenKind::Op(s) if s == "," && depth == 1 => {
                                    found_comma = true;
                                    k += 1;
                                }
                                _ => { k += 1; }
                            }
                        }
                        if found_comma && found_bind {
                            let start_i = self.i;
                            let lhs_pe = self.parse_coalesce()?;
                            let lhs = self.lower_expr(lhs_pe);
                            self.skip_newlines();
                            let op_sp = self.peek().map(|t| t.span.clone()).unwrap_or_else(|| t0.span.clone());
                            let op = if self.eat_op("|=") {
                                "|="
                            } else if self.eat_op("|") {
                                "|"
                            } else {
                                self.i = start_i;
                                ""
                            };
                            if !op.is_empty() {
                                self.skip_newlines();
                                let rhs_pe = self.parse_coalesce()?;
                                let rhs = self.lower_expr(rhs_pe);
                                return Ok(ast::Stmt::Expr(ast::Expr::Binary(
                                    Box::new(lhs),
                                    op.to_string(),
                                    Box::new(rhs),
                                    op_sp,
                                )));
                            }
                        }
                    }
                }
            }
        }

        // -------- member path assignment: Object >> field |= value --------
        // Detects IDENT >> ... |= pattern and lowers to a member retether.
        if let Some(t0) = self.peek().cloned() {
            if matches!(t0.kind, TokenKind::Ident) {
                if let Some(t1) = self.toks.get(self.i + 1) {
                    if matches!(t1.kind, TokenKind::Op(ref s) if s == ">>") {
                        // Scan ahead to find |= anywhere in this member chain
                        let mut k = self.i + 2;
                        let mut found_retether = false;
                        while let Some(t) = self.toks.get(k) {
                            match &t.kind {
                                TokenKind::Op(s) if s == "|=" => { found_retether = true; break; }
                                TokenKind::Op(s) if s == ">>" => { k += 2; } // skip >> and field name
                                TokenKind::Ident => { k += 1; }
                                _ => break,
                            }
                        }
                        if found_retether {
                            let start_i = self.i;
                            // Parse the full LHS expression (member chain)
                            let lhs_pe = self.parse_coalesce()?;
                            let lhs = self.lower_expr(lhs_pe);
                            // Consume |=
                            let op_sp = self.peek().map(|t| t.span.clone()).unwrap_or_else(|| t0.span.clone());
                            if !self.eat_op("|=") {
                                self.i = start_i;
                            } else {
                                self.skip_newlines();
                                let rhs_pe = self.parse_coalesce()?;
                                let rhs = self.lower_expr(rhs_pe);
                                // Lower to: lhs |= rhs as an Expr assignment
                                // We use a FreeCall to set_field or handle via eval
                                // Actually lower as TupleAssign using lhs path
                                // Simplest: emit as Expr(Binary(lhs, "|=", rhs)) and handle in interpreter
                                return Ok(ast::Stmt::Expr(ast::Expr::Binary(
                                    Box::new(lhs),
                                    "|=".to_string(),
                                    Box::new(rhs),
                                    op_sp,
                                )));
                            }
                        }
                    }
                }
            }
        }

        // ---- indexed |! update sugar: array[1] |! value ----
        {
            let save_i = self.i;

            if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Ident)) {
                // Parse a full LHS expression: array[1], map["key"], grid[x,y], etc.
                let lhs_pe = self.parse_coalesce()?;

                self.skip_newlines();

                if self.peek_op("|!") {
                    let op_sp = self.peek().unwrap().span.clone();
                    let _ = self.eat_op("|!");

                    self.skip_newlines();

                    let rhs_pe = self.parse_coalesce()?;
                    let rhs = self.lower_expr(rhs_pe);

                    let lhs = self.lower_expr(lhs_pe);

                    match lhs {
                        ast::Expr::Index(base, index, _) => {
                            let call = ast::Expr::FreeCall(
                                "update_at!".to_string(),
                                vec![*base, *index, rhs],
                                op_sp.clone(),
                            );
                            return Ok(ast::Stmt::Expr(call));
                        }

                        ast::Expr::Index2(base, x, y, _) => {
                            let call = ast::Expr::FreeCall(
                                "update_at2!".to_string(),
                                vec![*base, *x, *y, rhs],
                                op_sp.clone(),
                            );
                            return Ok(ast::Stmt::Expr(call));
                        }

                        ast::Expr::Ident(_, _) => {
                            let call = ast::Expr::FreeCall(
                                "update!".to_string(),
                                vec![lhs, rhs],
                                op_sp.clone(),
                            );
                            return Ok(ast::Stmt::Expr(call));
                        }

                        _ => {
                            return Err(s_help_site!(
                                "P04U1",
                                "Invalid target for '|!' update",
                                "Use '|!' with a name or indexed collection target: array |! value or array[0] |! value."
                            ));
                        }
                    }
                }

                // Not actually a |! statement; rewind and let normal parsing handle it.
                self.i = save_i;
            }
        }

        // -------- aug-assign statement sugar (legacy set only) --------
        // Supports exactly what old parse_assign supported:
        // ??= //= += -= *= /= %= **= |!  (|= already handled by parse_bind_stmt)
        //
        // Statement-only. LHS is IDENT only (matches Sheriff usage and avoids weirdness).
        if let Some(t0) = self.peek().cloned() {
            if matches!(t0.kind, TokenKind::Ident) {
                if let Some(t1) = self.toks.get(self.i + 1) {
                    // match only the legacy operators
                    let op: Option<&str> = match &t1.kind {
                        TokenKind::Op(s) if s == "??=" => Some("??="),
                        TokenKind::Op(s) if s == "//=" => Some("//="),
                        TokenKind::Op(s) if s == "+="  => Some("+="),
                        TokenKind::Op(s) if s == "-="  => Some("-="),
                        TokenKind::Op(s) if s == "*="  => Some("*="),
                        TokenKind::Op(s) if s == "/="  => Some("/="),
                        TokenKind::Op(s) if s == "%="  => Some("%="),
                        TokenKind::Op(s) if s == "**=" => Some("**="),
                        TokenKind::Op(s) if s == "|!"  => Some("|!"),
                        _ => None,
                    };

                    if let Some(op) = op {
                        // Consume IDENT
                        let name = t0.value.clone().unwrap_or_default();
                        let name_sp = t0.span.clone();
                        self.i += 1;

                        // Consume operator
                        let op_sp = t1.span.clone();
                        self.i += 1;

                        self.skip_newlines();

                        // Parse RHS as a normal expression
                        let rhs_pe = self.parse_coalesce()?;
                        let rhs = self.lower_expr(rhs_pe);

                        // Build LHS Ident expr for lowering
                        let lhs_expr = ast::Expr::Ident(name.clone(), name_sp.clone());

                        // |! is statement sugar for update!(name, rhs)
                        if op == "|!" {
                            match lhs_expr {
                                ast::Expr::Index(base, index, _) => {
                                    let call = ast::Expr::FreeCall(
                                        "update_at!".to_string(),
                                        vec![*base, *index, rhs],
                                        op_sp.clone(),
                                    );
                                    return Ok(ast::Stmt::Expr(call));
                                }

                                _ => {
                                    let call = ast::Expr::FreeCall(
                                        "update!".to_string(),
                                        vec![lhs_expr, rhs],
                                        op_sp.clone(),
                                    );
                                    return Ok(ast::Stmt::Expr(call));
                                }
                            }
                        }

                        // Otherwise lower to: name |= (name <baseop> rhs)
                        let base_op = match op {
                            "+="  => "+",
                            "-="  => "-",
                            "*="  => "*",
                            "/="  => "/",
                            "%="  => "%",
                            "**=" => "**",
                            "??=" => "??",
                            "//=" => "//",
                            _ => unreachable!(),
                        };

                        let combined = ast::Expr::Binary(
                            Box::new(lhs_expr),
                            base_op.to_string(),
                            Box::new(rhs),
                            op_sp.clone(),
                        );

                        return Ok(ast::Stmt::Bind(ast::BindStmt {
                            name: (name, name_sp),
                            expr: combined,
                            is_imm: false,
                            is_local: false,
                            mode: ast::BindMode::Retether,
                            span: op_sp,
                            class_name: None,
                            lock_type: None,
                        }));
                    }
                }
            }
        }

        // -------- default: expression/assignment statement --------
        let was = self.in_stmt;
        self.in_stmt = true;
        let expr_pe = self.parse_assign()?;
        self.in_stmt = was;
        Ok(ast::Stmt::Expr(self.lower_expr(expr_pe)))
    }

    fn lower_enum_stmt(
        &mut self,
        pe: PEnumDecl,
        sp: goblin_diagnostics::Span,
    ) -> Result<ast::Stmt, String> {
        let variants_ast: Vec<ast::EnumVariant> = pe.variants
            .into_iter()
            .map(|pv| {
                let fields_ast = if pv.fields.is_empty() {
                    None
                } else {
                    Some(
                        pv.fields
                            .into_iter()
                            .map(|(fname, _type_expr)| ast::FieldDecl {
                                name: fname,
                                private: false,
                                nullable: false,  
                                readonly: false,
                                raw: false,
                                relation: None,
                                default: None, // enum fields don't have defaults in Phase 1
                                span: sp.clone(),
                            })
                            .collect()
                    )
                };
                
                ast::EnumVariant {
                    name: pv.name,
                    fields: fields_ast,
                    span: sp.clone(),
                }
            })
            .collect();
        
        Ok(ast::Stmt::Enum(ast::EnumDecl {
            name: pe.name,
            variants: variants_ast,
            span: sp,
        }))
    }

    // Helper: lower a parsed class PExpr into Stmt::Class (fields + actions)
    fn lower_matrix_stmt(&mut self, pe: PExpr, sp: goblin_diagnostics::Span) -> Result<ast::Stmt, String> {
        let (type_name, columns, rows, _span) = match pe {
            PExpr::ObjectMatrix { type_name, columns, rows, span } => (type_name, columns, rows, span),
            _ => return Err(s_help_site!("P1110", "Internal: expected ObjectMatrix", "Report this as a Goblin bug")),
        };

        let mut stmts: Vec<ast::Stmt> = Vec::new();

        // Synthesize a class declaration from the matrix rows so no separate
        // @Type | field: default ... declaration is needed.
        let fields_ast: Vec<ast::FieldDecl> = rows.iter().map(|row| {
            let default_pe = match &row.default {
                MatrixCell::Nc => PExpr::Nil,
                MatrixCell::Expr(e) => e.clone(),
            };
            ast::FieldDecl {
                name: row.field.clone(),
                private: false,
                nullable: false,
                readonly: false,
                raw: false,
                relation: None,
                default: Some(self.lower_expr(default_pe)),
                span: sp.clone(),
            }
        }).collect();

        stmts.push(ast::Stmt::Class(ast::ClassDecl {
            name: type_name.clone(),
            fields: fields_ast,
            actions: vec![],
            decision: None,
            judge: None,
            transitions: vec![],
            capacity: None,
            span: sp.clone(),
        }));

        for (col_idx, col_name) in columns.iter().enumerate() {
            // Build the field map for this object: field -> resolved ast::Expr
            let mut field_exprs: Vec<(String, ast::Expr)> = Vec::new();

            // First pass: collect all field default exprs so field references resolve correctly.
            // We build a small env of already-resolved PExpr values per column as we go.
            let mut resolved_pexprs: std::collections::HashMap<String, PExpr> = std::collections::HashMap::new();

            for row in &rows {
                // Pick the cell for this column
                let cell = &row.cells[col_idx];

                let cell_pe: PExpr = match cell {
                    MatrixCell::Nc => match &row.default {
                        MatrixCell::Nc => PExpr::Nil,
                        MatrixCell::Expr(e) => e.clone(),
                    },
                    MatrixCell::Expr(e) => e.clone(),
                };

                // Substitute field references: any Ident that matches a previously resolved field
                // gets replaced with its resolved PExpr value.
                let cell_pe = Self::substitute_matrix_field_refs(cell_pe, &resolved_pexprs);

                // Handle first row: any Ident cell (e.g. USA, France) is a column name
                // used as a string value. The first row defines variable names.
                let first_row_field = rows.first().map(|r| r.field.as_str()).unwrap_or("");
                let cell_pe = if row.field == first_row_field {
                    match cell_pe {
                        PExpr::Ident(ref s) => PExpr::Str(s.clone()),
                        other => other,
                    }
                } else {
                    cell_pe
                };

                resolved_pexprs.insert(row.field.clone(), cell_pe.clone());
                let ast_expr = self.lower_expr(cell_pe);
                field_exprs.push((row.field.clone(), ast_expr));
            }

            // Build: col_name|TypeName = { field: val, ... }
            let obj_expr = ast::Expr::Object(field_exprs, sp.clone());

            let bind = ast::BindStmt {
                name: (col_name.clone(), sp.clone()),
                expr: obj_expr,
                is_imm: false,
                is_local: false,
                mode: ast::BindMode::Tether,
                span: sp.clone(),
                class_name: Some(type_name.clone()),
                lock_type: None,
            };

            stmts.push(ast::Stmt::Bind(bind));
        }

        Ok(ast::Stmt::Block { stmts, span: sp })
    }

    /// Walk a PExpr and replace any bare Ident that matches a resolved field name
    /// with its resolved PExpr. This handles `threat: 0, strength + dexterity` style
    /// field references within a matrix row.
    fn substitute_matrix_field_refs(
        pe: PExpr,
        resolved: &std::collections::HashMap<String, PExpr>,
    ) -> PExpr {
        match pe {
            PExpr::Ident(ref name) => {
                if let Some(replacement) = resolved.get(name) {
                    replacement.clone()
                } else {
                    pe
                }
            }
            PExpr::Binary(lhs, op, rhs) => {
                let lhs = Self::substitute_matrix_field_refs(*lhs, resolved);
                let rhs = Self::substitute_matrix_field_refs(*rhs, resolved);
                PExpr::Binary(Box::new(lhs), op, Box::new(rhs))
            }
            PExpr::Prefix(op, expr) => {
                let expr = Self::substitute_matrix_field_refs(*expr, resolved);
                PExpr::Prefix(op, Box::new(expr))
            }
            PExpr::Postfix(expr, op) => {
                let expr = Self::substitute_matrix_field_refs(*expr, resolved);
                PExpr::Postfix(Box::new(expr), op)
            }
            // All other variants pass through unchanged
            other => other,
        }
    }


    /// Derive theoretical (min, max) from a link/decision formula for normalization.
    /// This is the parser-side version operating on ast::Expr.
    fn derive_link_formula_range_parser(expr: &ast::Expr) -> (f64, f64) {
        match expr {
            ast::Expr::Number(s, _) => {
                let v = s.parse::<f64>().unwrap_or(0.0);
                (v, v)
            }
            ast::Expr::Binary(lhs, op, rhs, _) if op == ">>" => {
                match lhs.as_ref() {
                    ast::Expr::Ident(name, _) if name == "self" || name == "target" => (0.0, 1.0),
                    _ => (0.0, 1.0),
                }
            }
            ast::Expr::Binary(lhs, op, rhs, _) => {
                let (lmin, lmax) = Self::derive_link_formula_range_parser(lhs);
                let (rmin, rmax) = Self::derive_link_formula_range_parser(rhs);
                match op.as_str() {
                    "+" => (lmin + rmin, lmax + rmax),
                    "-" => (lmin - rmax, lmax - rmin),
                    "*" => {
                        let products = [lmin*rmin, lmin*rmax, lmax*rmin, lmax*rmax];
                        (products.iter().cloned().fold(f64::INFINITY, f64::min),
                         products.iter().cloned().fold(f64::NEG_INFINITY, f64::max))
                    }
                    _ => (0.0, 1.0),
                }
            }
            ast::Expr::Prefix(op, inner, _) if op == "-" => {
                let (imin, imax) = Self::derive_link_formula_range_parser(inner);
                (-imax, -imin)
            }
            _ => (0.0, 1.0),
        }
    }

    fn lower_class_stmt_from_pexpr(
        &mut self,
        pe: PExpr,
        sp: goblin_diagnostics::Span,
    ) -> Result<ast::Stmt, String> {
        match pe {
            PExpr::ClassDecl { name, fields, actions, decision, judge, transitions, capacity } => {
                // Fields
                let fields_ast: Vec<ast::FieldDecl> = fields
                    .into_iter()
                    .map(|(fname, fexpr, readonly, nullable, raw, relation)| ast::FieldDecl {
                        name: fname,
                        private: false,
                        nullable,
                        readonly,
                        raw,
                        relation,
                        default: Some(self.lower_expr(fexpr)),
                        span: sp.clone(),
                    })
                    .collect();

                let actions_ast: Vec<ast::ActionDecl> = actions
                    .into_iter()
                    .map(|pa| {
                        let params: Vec<ast::Param> = pa.params.into_iter()
                            .map(|(pname, def_pe)| ast::Param {
                                name: pname,
                                type_name: None,
                                default: def_pe.map(|pe| self.lower_expr(pe)),
                                span: sp.clone(),
                            })
                            .collect();

                        ast::ActionDecl {
                            name: pa.name,
                            params,
                            body: ast::ActionBody::Block(pa.body),
                            span: sp.clone(),
                            ret: None,
                        }
                    })
                    .collect();

                // Lower decision formula if present
                let decision_ast = decision.map(|d| {
                    let formula = self.lower_expr(*d.formula);
                    let (formula_min, formula_max) = Self::derive_link_formula_range_parser(&formula);
                    ast::DecisionDef {
                        target_class: d.target_class,
                        formula,
                        formula_min,
                        formula_max,
                        span: sp.clone(),
                    }
                });

                Ok(ast::Stmt::Class(ast::ClassDecl {
                    name,
                    fields: fields_ast,
                    actions: actions_ast,
                    decision: decision_ast,
                    judge,
                    transitions,
                    capacity,
                    span: sp,
                }))
            }
            _ => Err(s_help_site!(
                "P0905",
                "Expected a class declaration after '@'",
                "Start the class like: @Player | username: \"john\" :: health: 100",
            )),
        }
    }

    /// Dispatch: `overlay Name | ... end` vs `overlay Name on target`
    fn parse_overlay_stmt(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'overlay'

        let Some(name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1200",
                "Expected overlay name after 'overlay'",
                "Write: overlay plague | ... end",
            ));
        };

        self.skip_layout();

        // Definition: overlay Name |
        if self.eat_op("|") {
            return self.parse_overlay_def(name, start_i);
        }

        // Application: overlay Name on target
        if self.peek_ident() == Some("on") {
            let _ = self.eat_ident(); // consume 'on'
            return self.parse_overlay_apply(name, start_i);
        }

        Err(s_help_site!(
            "P1201",
            "Expected '|' (definition) or 'on' (application) after overlay name",
            "Write: overlay plague | ... end  OR  overlay plague on city at .6",
        ))
    }

    /// Parse overlay definition body after `overlay Name |`
    fn parse_overlay_def(&mut self, name: String, start_i: usize) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;

        let mut host_types: Vec<String> = Vec::new();
        let mut spread_rules: Vec<ast::SpreadRule> = Vec::new();
        let mut decay_rate: f64 = 0.0;
        let mut modifiers: Vec<(String, ast::Expr)> = Vec::new();
        let mut conflict_rules: Vec<ast::OverlayConflictRule> = Vec::new();
        let mut spawn_rules: Vec<ast::OverlaySpawnRule> = Vec::new();
        let mut transitions: Vec<ast::TransitionDef> = Vec::new();
        let mut default_duration: Option<u32> = None;
        let mut apply_behavior: ast::OverlayApplyBehavior = ast::OverlayApplyBehavior::Caps;
        let mut extra_fields: Vec<(String, ast::Expr)> = Vec::new();

        self.skip_layout();

        loop {
            self.skip_layout();

            if self.is_eof() { break; }
            if let Some(tok) = self.peek() {
                let is_closer = matches!(tok.kind, TokenKind::Ident if tok.value.as_deref() == Some("end"))
                    || matches!(tok.kind, TokenKind::Op(ref s) if s == "xx");
                if is_closer {
                    self.i += 1;
                    break;
                }
            }

            // Eat either an Ident keyword or skip Op("xx") as closer
            let kw = if let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Op(ref s) if s == "xx") {
                    self.i += 1;
                    break;
                }
                match self.eat_ident() {
                    Some(k) => k,
                    None => break,
                }
            } else { break };

            match kw.as_str() {
                "hosts" => {
                    // hosts Nation, City, ...
                    loop {
                        self.skip_layout_inline();
                        let Some(ht) = self.eat_ident() else { break; };
                        host_types.push(ht);
                        self.skip_layout_inline();
                        if !self.eat_op(",") { break; }
                    }
                }
                "spreads" => {
                    // spreads through channel at rate per tick
                    // spreads to all ClassName at rate per tick
                    // spreads to nearby at rate per tick
                    // spreads through ownership at rate per tick
                    // spreads where condition at rate per tick
                    self.skip_layout_inline();
                    let mode_kw = self.peek_ident();
                    let rule = if mode_kw == Some("through") {
                        let _ = self.eat_ident(); // consume 'through'
                        self.skip_layout_inline();
                        let Some(ch) = self.eat_ident() else { break; };
                        let rate = self.parse_spread_rate();
                        if ch == "ownership" {
                            ast::SpreadRule::Ownership { rate }
                        } else {
                            ast::SpreadRule::Channel { channel: ch, rate }
                        }
                    } else if mode_kw == Some("to") {
                        let _ = self.eat_ident(); // consume 'to'
                        self.skip_layout_inline();
                        let next = self.peek_ident();
                        if next == Some("nearby") {
                            let _ = self.eat_ident();
                            let rate = self.parse_spread_rate();
                            ast::SpreadRule::Nearby { rate }
                        } else if next == Some("all") {
                            let _ = self.eat_ident(); // consume 'all'
                            self.skip_layout_inline();
                            let class_name = self.eat_ident().unwrap_or_else(|| "Object".to_string());
                            let rate = self.parse_spread_rate();
                            ast::SpreadRule::All { class_name, rate }
                        } else {
                            // fallback: treat as class name
                            let class_name = self.eat_ident().unwrap_or_else(|| "Object".to_string());
                            let rate = self.parse_spread_rate();
                            ast::SpreadRule::All { class_name, rate }
                        }
                    } else if mode_kw == Some("where") {
                        let _ = self.eat_ident(); // consume 'where'
                        self.skip_layout_inline();
                        let cond_pe = self.parse_coalesce().unwrap_or(PExpr::Bool(true));
                        let condition = self.lower_expr(cond_pe);
                        let rate = self.parse_spread_rate();
                        ast::SpreadRule::Predicate { condition, rate }
                    } else {
                        // bare channel name (legacy: spreads culture at .1)
                        let ch = self.eat_ident().unwrap_or_default();
                        let rate = self.parse_spread_rate();
                        ast::SpreadRule::Channel { channel: ch, rate }
                    };
                    spread_rules.push(rule);
                }
                "decays" => {
                    // decays rate per tick
                    self.skip_layout_inline();
                    if let Some(n) = self.eat_number_f64() {
                        decay_rate = n;
                    }
                    self.skip_layout_inline();
                    if self.peek_ident() == Some("per") {
                        let _ = self.eat_ident();
                        self.skip_layout_inline();
                        if self.peek_ident() == Some("tick") { let _ = self.eat_ident(); }
                    }
                }
                "lasts" => {
                    // lasts N ticks
                    self.skip_layout_inline();
                    if let Some(n) = self.eat_number_u32() {
                        default_duration = Some(n);
                    }
                    self.skip_layout_inline();
                    if self.peek_ident() == Some("ticks") || self.peek_ident() == Some("tick") {
                        let _ = self.eat_ident();
                    }
                }
                "modifies" => {
                    // modifies: field +/- expr ... end
                    self.skip_layout_inline();
                    self.eat_op(":");
                    self.skip_layout();
                    loop {
                        self.skip_layout();
                        if self.is_eof() { break; }
                        if let Some(tok) = self.peek() {
                            if matches!(tok.kind, TokenKind::Ident)
                                && matches!(tok.value.as_deref(), Some("end") | Some("xx")
                                    | Some("conflicts") | Some("spawns") | Some("hosts")
                                    | Some("spreads") | Some("decays") | Some("lasts")
                                    | Some("transition") | Some("stacks") | Some("replaces")
                                    | Some("caps"))
                            {
                                break;
                            }
                        }
                        let Some(field_name) = self.eat_ident() else { break; };
                        self.skip_layout_inline();
                        // expect + or -
                        let negative = if self.eat_op("+") {
                            false
                        } else if self.eat_op("-") {
                            true
                        } else {
                            break;
                        };
                        self.skip_layout_inline();
                        // parse full expression (supports self >> count * .0000003 etc)
                        let val_pe = self.parse_coalesce().unwrap_or(PExpr::Int("0".to_string()));
                        let val_expr = self.lower_expr(val_pe);
                        let modifier_expr = if negative {
                            ast::Expr::Prefix(
                                "-".to_string(),
                                Box::new(val_expr),
                                goblin_diagnostics::Span::new("<synthetic>", 0, 0, 0, 0, 0, 0),
                            )
                        } else {
                            val_expr
                        };
                        modifiers.push((field_name, modifier_expr));
                    }
                    // consume optional 'end' closing modifies block
                    if let Some(tok) = self.peek() {
                        if matches!(tok.kind, TokenKind::Ident)
                            && matches!(tok.value.as_deref(), Some("end") | Some("xx"))
                        {
                            let saved = self.i;
                            self.i += 1;
                            self.skip_layout();
                            if let Some(next) = self.peek() {
                                if matches!(next.kind, TokenKind::Ident)
                                    && matches!(next.value.as_deref(),
                                        Some("conflicts") | Some("spawns") | Some("hosts")
                                        | Some("spreads") | Some("decays") | Some("lasts")
                                        | Some("transition") | Some("stacks") | Some("replaces")
                                        | Some("caps")
                                        | Some("end") | Some("xx"))
                                {
                                    // consumed ok
                                } else {
                                    self.i = saved; // restore — this end closes the overlay
                                }
                            } else {
                                self.i = saved;
                            }
                        }
                    }
                }
                "stacks" => {
                    // stacks  OR  stacks as label
                    self.skip_layout_inline();
                    let label = if self.peek_ident() == Some("as") {
                        let _ = self.eat_ident(); // consume 'as'
                        self.skip_layout_inline();
                        self.eat_ident()
                    } else {
                        None
                    };
                    apply_behavior = ast::OverlayApplyBehavior::Stacks { label };
                }
                "replaces" => {
                    apply_behavior = ast::OverlayApplyBehavior::Replaces;
                }
                "caps" => {
                    apply_behavior = ast::OverlayApplyBehavior::Caps;
                }
                "conflicts" => {
                    // conflicts opponent_name  OR  conflicts opponent_name: suppress rate end
                    self.skip_layout_inline();
                    let Some(opponent) = self.eat_ident() else { continue; };
                    let mut suppress_rate = 2.0f64;
                    self.skip_layout_inline();
                    if self.eat_op(":") {
                        self.skip_layout();
                        loop {
                            self.skip_layout();
                            if self.is_eof() { break; }
                            if let Some(tok) = self.peek() {
                                if matches!(tok.kind, TokenKind::Ident)
                                    && matches!(tok.value.as_deref(), Some("end") | Some("xx"))
                                {
                                    self.i += 1;
                                    break;
                                }
                            }
                            let Some(prop) = self.eat_ident() else { break; };
                            self.skip_layout_inline();
                            match prop.as_str() {
                                "suppress" => {
                                    if let Some(n) = self.eat_number_f64() {
                                        suppress_rate = n;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    conflict_rules.push(ast::OverlayConflictRule { opponent, suppress_rate });
                }
                "transition" => {
                    let td = self.parse_transition_def()?;
                    transitions.push(td);
                }
                "spawns" => {
                    self.skip_layout_inline();

                    let Some(spawn_name) = self.eat_ident() else {
                        return Err(s_help_site!(
                            "P1230",
                            "Expected overlay name after 'spawns'",
                            "Write: spawns Revolution when strength > .75:"
                        ));
                    };

                    self.skip_layout_inline();

                    if self.peek_ident() != Some("when") {
                        return Err(s_help_site!(
                            "P1231",
                            "Expected 'when' after spawned overlay name",
                            "Write: spawns Revolution when strength > .75:"
                        ));
                    }
                    let _ = self.eat_ident();

                    self.skip_layout_inline();

                    self.suspend_colon_call += 1;
                    let condition_pe = self.parse_coalesce()?;
                    self.suspend_colon_call -= 1;

                    self.skip_layout_inline();

                    if !self.eat_op(":") {
                        return Err(s_help_site!(
                            "P1232",
                            "Expected ':' after overlay spawn condition",
                            "Write: spawns Revolution when strength > .75:"
                        ));
                    }

                    let condition = self.lower_expr(condition_pe);

                    self.skip_layout();

                    let mut spawn_strength = 0.5f64;

                    loop {
                        self.skip_layout();

                        if self.is_eof() {
                            break;
                        }

                        if let Some(tok) = self.peek() {
                            if matches!(tok.kind, TokenKind::Ident)
                                && matches!(tok.value.as_deref(), Some("end") | Some("xx"))
                            {
                                self.i += 1;
                                break;
                            }
                        }

                        let Some(prop) = self.eat_ident() else {
                            break;
                        };

                        self.skip_layout_inline();

                        match prop.as_str() {
                            "strength" => {
                                if let Some(n) = self.eat_number_f64() {
                                    spawn_strength = n;
                                }
                            }
                            _ => {}
                        }
                    }

                    spawn_rules.push(ast::OverlaySpawnRule {
                        condition,
                        spawn_overlay: spawn_name,
                        spawn_strength,
                    });
                }
                _ => {
                    // Check if this is a field: value declaration (e.g. kind: "language")
                    self.skip_layout_inline();
                    if self.eat_op(":") {
                        self.skip_layout_inline();
                        if let Ok(val_pe) = self.parse_primary() {
                            let val_expr = self.lower_expr(val_pe);
                            extra_fields.push((kw, val_expr));
                        }
                    }
                    // Skip to end of line
                    while let Some(tok) = self.peek() {
                        if matches!(tok.kind, TokenKind::Newline | TokenKind::Eof) { break; }
                        self.i += 1;
                    }
                }
            }
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::OverlayDef(ast::OverlayDefStmt {
            name,
            host_types,
            spread_rules,
            decay_rate,
            modifiers,
            conflict_rules,
            spawn_rules,
            transitions,
            default_duration,
            apply_behavior,
            extra_fields,
            span,
        }))
    }

    /// Parse `overlay Name on target [at strength] [for N ticks]`
    fn parse_overlay_apply(&mut self, overlay_name: String, start_i: usize) -> Result<ast::Stmt, String> {
        // target expression
        let host_pe = self.parse_coalesce()?;
        let host_expr = self.lower_expr(host_pe);

        self.skip_layout_inline();

        // optional: at strength
        let mut strength = 1.0f64;
        if self.peek_ident() == Some("at") {
            let _ = self.eat_ident();
            self.skip_layout_inline();
            if let Some(n) = self.eat_number_f64() {
                strength = n;
            }
        }

        self.skip_layout_inline();

        // optional: for N ticks
        let mut duration_override: Option<u32> = None;
        if self.peek_ident() == Some("for") {
            let _ = self.eat_ident();
            self.skip_layout_inline();
            if let Some(n) = self.eat_number_u32() {
                duration_override = Some(n);
            }
            self.skip_layout_inline();
            // consume 'ticks' / 'tick'
            if self.peek_ident() == Some("ticks") || self.peek_ident() == Some("tick") {
                let _ = self.eat_ident();
            }
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::OverlayApply(ast::OverlayApplyStmt {
            overlay_name,
            host_expr,
            strength,
            duration_override,
            span,
        }))
    }

    /// Parse `detach Name from target`
    fn parse_overlay_detach(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'detach'

        let Some(overlay_name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1210",
                "Expected overlay name after 'detach'",
                "Write: detach plague from city",
            ));
        };

        self.skip_layout_inline();

        if self.peek_ident() != Some("from") {
            return Err(s_help_site!(
                "P1211",
                "Expected 'from' after overlay name in detach",
                "Write: detach plague from city",
            ));
        }
        let _ = self.eat_ident(); // consume 'from'

        self.skip_layout_inline();

        let host_pe = self.parse_coalesce()?;
        let host_expr = self.lower_expr(host_pe);

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::OverlayDetach(ast::OverlayDetachStmt {
            overlay_name,
            host_expr,
            span,
        }))
    }

    /// Eat a numeric token and return it as f64. Returns None if next token is not a number.
    fn eat_number_f64(&mut self) -> Option<f64> {
        use goblin_lexer::TokenKind;
        if let Some(tok) = self.peek() {
            if matches!(tok.kind, TokenKind::Int | TokenKind::Float) {
                let s = tok.value.clone().unwrap_or_default();
                self.i += 1;
                return s.parse::<f64>().ok();
            }
        }
        None
    }

    /// Eat a numeric token and return it as u32.
    fn eat_number_u32(&mut self) -> Option<u32> {
        self.eat_number_f64().map(|f| f as u32)
    }

    /// Parse `link ClassName [channel] by [ formula ]`
    fn parse_link_def(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'link'

        let Some(class_name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1300",
                "Expected class name after 'link'",
                "Write: link Nation border by [ formula ]",
            ));
        };

        self.skip_layout_inline();

        // Optional channel name: link Nation border by [...]
        //                        link Nation by [...]  (no channel)
        let channel = if self.peek_ident() != Some("by") {
            self.eat_ident()
        } else {
            None
        };

        self.skip_layout();

        if self.peek_ident() != Some("by") {
            return Err(s_help_site!(
                "P1301",
                "Expected 'by' after class name in link declaration",
                "Write: link Nation border by [ formula ]",
            ));
        }
        let _ = self.eat_ident(); // consume 'by'

        self.skip_layout();

        if !self.eat_op("[") {
            return Err(s_help_site!(
                "P1302",
                "Expected '[' to open link formula",
                "Write: link Nation border by [ formula ]",
            ));
        }

        let formula_pe = self.parse_link_formula()?;
        let formula = self.lower_expr(formula_pe);

        self.skip_layout();
        if !self.eat_op("]") {
            return Err(s_help_site!(
                "P1303",
                "Expected ']' to close link formula",
                "Write: link Nation border by [ formula ]",
            ));
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::LinkDef(ast::LinkDefStmt { class_name, channel, formula, span }))
    }

    /// Parse `ObjectName link [channel] by [ formula ]`
    /// OR   `VarA link to VarB on channel offset value [for N ticks]`
    fn parse_object_link_def(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;

        let Some(object_var) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1310",
                "Expected object variable name before 'link'",
                "Write: Russia link border by [ formula ]",
            ));
        };

        let _ = self.eat_ident(); // consume 'link'

        self.skip_layout_inline();

        // Check for offset syntax: VarA link to VarB on channel offset value [for N ticks]
        if self.peek_ident() == Some("to") {
            let _ = self.eat_ident(); // consume 'to'
            self.skip_layout_inline();
            let Some(to_var) = self.eat_ident() else {
                return Err(s_help_site!("P1320", "Expected target variable after 'to'",
                    "Write: Rome link to Carthage on trust offset -.6"));
            };
            self.skip_layout_inline();
            if self.peek_ident() != Some("on") {
                return Err(s_help_site!("P1321", "Expected 'on' after target variable",
                    "Write: Rome link to Carthage on trust offset -.6"));
            }
            let _ = self.eat_ident(); // consume 'on'
            self.skip_layout_inline();
            let Some(channel) = self.eat_ident() else {
                return Err(s_help_site!("P1322", "Expected channel name after 'on'",
                    "Write: Rome link to Carthage on trust offset -.6"));
            };
            self.skip_layout_inline();
            if self.peek_ident() != Some("offset") {
                return Err(s_help_site!("P1323", "Expected 'offset' keyword",
                    "Write: Rome link to Carthage on trust offset -.6"));
            }
            let _ = self.eat_ident(); // consume 'offset'
            self.skip_layout_inline();
            // Parse offset value (may have + or - prefix)
            let negative = self.eat_op("-");
            let positive = if !negative { self.eat_op("+") } else { false };
            let Some(offset_abs) = self.eat_number_f64() else {
                return Err(s_help_site!("P1324", "Expected numeric offset value",
                    "Write: Rome link to Carthage on trust offset -.6"));
            };
            let offset: f64 = if negative { -offset_abs } else { offset_abs };
            self.skip_layout_inline();
            // Optional: for N ticks
            let ticks = if self.peek_ident() == Some("for") {
                let _ = self.eat_ident(); // consume 'for'
                self.skip_layout_inline();
                if let Some(n) = self.eat_number_u32() {
                    let _ = self.eat_ident(); // consume 'ticks' (optional label)
                    Some(n)
                } else { None }
            } else { None };
            let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
            return Ok(ast::Stmt::LinkOffset(ast::LinkOffsetStmt {
                from_var: object_var,
                to_var,
                channel,
                offset,
                ticks,
                span,
            }));
        }

        // Optional channel name before 'by'
        let channel = if self.peek_ident() != Some("by") {
            self.eat_ident()
        } else {
            None
        };

        self.skip_layout();

        if self.peek_ident() != Some("by") {
            return Err(s_help_site!(
                "P1311",
                "Expected 'by' after 'link' in object link declaration",
                "Write: Russia link border by [ formula ]",
            ));
        }
        let _ = self.eat_ident(); // consume 'by'

        self.skip_layout();

        if !self.eat_op("[") {
            return Err(s_help_site!(
                "P1312",
                "Expected '[' to open link formula",
                "Write: Russia link border by [ self >> power - target >> aggression ]",
            ));
        }

        let formula_pe = self.parse_link_formula()?;
        let formula = self.lower_expr(formula_pe);

        self.skip_layout();
        if !self.eat_op("]") {
            return Err(s_help_site!(
                "P1313",
                "Expected ']' to close link formula",
                "Write: Russia link border by [ self >> power - target >> aggression ]",
            ));
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::ObjectLinkDef(ast::ObjectLinkDefStmt { object_var, channel, formula, span }))
    }

    /// Parse `clear link VarA to VarB on channel`
    /// Parse `VarName score | decision against Class by [ formula ]`
    fn parse_object_decision(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        let Some(var_name) = self.eat_ident() else {
            return Err("expected var name".to_string());
        };
        let _ = self.eat_ident(); // consume 'score'
        if !self.eat_op("|") {
            return Err(s_help_site!("P1410", "Expected '|' after 'score'",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        if self.peek_ident() != Some("decision") {
            return Err(s_help_site!("P1411", "Expected 'decision' after 'score |'",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        let _ = self.eat_ident(); // consume 'decision'
        self.skip_layout();
        if self.peek_ident() != Some("against") {
            return Err(s_help_site!("P1412", "Expected 'against'",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        let _ = self.eat_ident(); // consume 'against'
        self.skip_layout();
        let Some(target_class) = self.eat_ident() else {
            return Err(s_help_site!("P1413", "Expected target class name",
                "Write: Germany score | decision against Nation by [ formula ]"));
        };
        self.skip_layout();
        if self.peek_ident() != Some("by") {
            return Err(s_help_site!("P1414", "Expected 'by'",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        let _ = self.eat_ident(); // consume 'by'
        self.skip_layout();
        if !self.eat_op("[") {
            return Err(s_help_site!("P1415", "Expected '[' to open formula",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        self.skip_layout();
        let formula_pe = self.parse_coalesce()?;
        let formula = self.lower_expr(formula_pe);
        self.skip_layout();
        if !self.eat_op("]") {
            return Err(s_help_site!("P1416", "Expected ']' to close formula",
                "Write: Germany score | decision against Nation by [ formula ]"));
        }
        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        let def = ast::DecisionDef {
            target_class,
            formula,
            formula_min: 0.0,
            formula_max: 1.0,
            span: span.clone(),
        };
        Ok(ast::Stmt::ObjectDecision(var_name, def))
    }

    fn parse_clear_link(&mut self) -> Result<ast::Stmt, String> {
        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'clear'
        let _ = self.eat_ident(); // consume 'link'
        self.skip_layout_inline();
        let Some(from_var) = self.eat_ident() else {
            return Err(s_help_site!("P1330", "Expected source variable after 'clear link'",
                "Write: clear link Rome to Carthage on trust"));
        };
        self.skip_layout_inline();
        if self.peek_ident() != Some("to") {
            return Err(s_help_site!("P1331", "Expected 'to' after source variable",
                "Write: clear link Rome to Carthage on trust"));
        }
        let _ = self.eat_ident(); // consume 'to'
        self.skip_layout_inline();
        let Some(to_var) = self.eat_ident() else {
            return Err(s_help_site!("P1332", "Expected target variable",
                "Write: clear link Rome to Carthage on trust"));
        };
        self.skip_layout_inline();
        if self.peek_ident() != Some("on") {
            return Err(s_help_site!("P1333", "Expected 'on' after target variable",
                "Write: clear link Rome to Carthage on trust"));
        }
        let _ = self.eat_ident(); // consume 'on'
        self.skip_layout_inline();
        let Some(channel) = self.eat_ident() else {
            return Err(s_help_site!("P1334", "Expected channel name after 'on'",
                "Write: clear link Rome to Carthage on trust"));
        };
        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::ClearLink(ast::ClearLinkStmt { from_var, to_var, channel, span }))
    }

    /// Parse `unit name | types: a, b; N a = M b end/xx`
    fn parse_unit_decl(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'unit'
        self.skip_layout();

        let Some(name) = self.eat_ident() else {
            return Err(s_help_site!(
                "P1600",
                "Expected unit name after 'unit'",
                "Write: unit weight | types: kg, g; 1 kg = 1000 g end",
            ));
        };

        self.skip_layout();
        if !self.eat_op("|") {
            return Err(s_help_site!(
                "P1601",
                "Expected '|' after unit name",
                "Write: unit weight | types: kg, g end",
            ));
        }

        let mut types: Vec<String> = Vec::new();
        let mut conversions: Vec<(String, f64, String, f64)> = Vec::new();

        loop {
            self.skip_layout();
            if self.peek_block_close() || self.is_eof() { break; }

            let Some(kw) = self.peek_ident() else { break; };

            if kw == "types" {
                let _ = self.eat_ident(); // consume 'types'
                self.skip_layout();
                if !self.eat_op(":") {
                    return Err(s_help_site!("P1602", "Expected ':' after 'types'", "Write: types: kg, g"));
                }
                loop {
                    self.skip_layout();
                    let Some(type_name) = self.eat_ident() else { break; };
                    types.push(type_name);
                    self.skip_layout();
                    if !self.eat_op(",") { break; }
                }
                continue;
            }

            // Conversion rule: N type_a = M type_b
            if let Some(from_count) = self.eat_number_f64() {
                self.skip_layout();
                let Some(from_type) = self.eat_ident() else {
                    return Err(s_help_site!("P1603", "Expected type name in conversion rule", "Write: 1 kg = 1000 g"));
                };
                self.skip_layout();
                if !self.eat_op("=") {
                    return Err(s_help_site!("P1604", "Expected '=' in conversion rule", "Write: 1 kg = 1000 g"));
                }
                self.skip_layout();
                let Some(to_count) = self.eat_number_f64() else {
                    return Err(s_help_site!("P1605", "Expected number after '=' in conversion rule", "Write: 1 kg = 1000 g"));
                };
                self.skip_layout();
                let Some(to_type) = self.eat_ident() else {
                    return Err(s_help_site!("P1606", "Expected type name after count in conversion rule", "Write: 1 kg = 1000 g"));
                };
                conversions.push((from_type, from_count, to_type, to_count));
                // optional separator
                self.eat_op(";");
                self.eat_op(",");
                continue;
            }

            break;
        }

        // consume end/xx
        self.eat_block_close();

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::Stmt::UnitDecl(ast::UnitDecl { name, types, conversions, span }))
    }

    /// Parse the expression inside a link formula block `[ ... ]`.
    /// Allows multi-line expressions with `self` and `target` as identifiers.
    /// Newlines inside the brackets are treated as whitespace.
    fn parse_link_formula(&mut self) -> Result<PExpr, String> {
        self.skip_layout();

        // Collect all tokens until the matching ']', substituting newlines with spaces
        // so the expression parser sees a flat expression.
        // We parse it as a normal expression — `self` and `target` are just idents.
        let expr = self.parse_coalesce()?;
        Ok(expr)
    }

    /// Parse a `transition KIND [(TargetClass)] when CONDITION ... end` inside a class body.
    fn parse_transition_def(&mut self) -> Result<ast::TransitionDef, String> {
        use goblin_lexer::TokenKind;

        let start_i = self.i;
        let _ = self.eat_ident(); // consume 'transition'

        self.skip_layout_inline();

        // Parse transition kind
        let Some(kind_str) = self.eat_ident() else {
            return Err(s_help_site!("P1500", "Expected transition kind after 'transition'",
                "Write: transition split when ... end"));
        };
        let kind = match kind_str.as_str() {
            "spawn"      => ast::TransitionKind::Spawn,
            "erase"      => ast::TransitionKind::Erase,
            "split"      => ast::TransitionKind::Split,
            "fracture"   => ast::TransitionKind::Fracture,
            "merge"      => ast::TransitionKind::Merge,
            "absorb"     => ast::TransitionKind::Absorb,
            "subjugate"  => ast::TransitionKind::Subjugate,
            "mutate"     => ast::TransitionKind::Mutate,
            other => return Err(s_help_site!("P1501",
                &format!("Unknown transition kind '{}'. Valid kinds: spawn erase split fracture merge absorb subjugate mutate", other),
                "Write: transition split when ... end")),
        };

        self.skip_layout_inline();

        // Optional target class for binary transitions: absorb(Faction)
        let target_class = if self.eat_op("(") {
            let tc = self.eat_ident();
            self.eat_op(")");
            tc
        } else {
            None
        };

        self.skip_layout_inline();

        // Consume 'when'
        if self.peek_ident() != Some("when") {
            return Err(s_help_site!("P1502", "Expected 'when' after transition kind",
                "Write: transition split when self >> stability < .2"));
        }
        let _ = self.eat_ident();

        self.skip_layout_inline();

        // Parse trigger condition expression
        let trigger_pe = self.parse_coalesce()?;
        let trigger = self.lower_expr(trigger_pe);

        self.skip_layout();

        // Optional: into ClassName, ClassName, ...
        let mut into_classes: Vec<String> = Vec::new();
        if self.peek_ident() == Some("into") {
            let _ = self.eat_ident();
            self.skip_layout_inline();
            loop {
                let Some(cls) = self.eat_ident() else { break; };
                into_classes.push(cls);
                self.skip_layout_inline();
                if !self.eat_op(",") { break; }
                self.skip_layout_inline();
            }
            self.skip_layout();
        }

        // Parse successor blocks and continuity rules until end/xx
        let mut successors: Vec<ast::SuccessorDef> = Vec::new();
        let mut overlay_rule = ast::OverlayContinuity::Drop;
        let mut link_rule = ast::LinkContinuity::Reset;

        loop {
            self.skip_layout();
            if self.is_eof() { break; }
            // Check for end/xx closer (end=Ident, xx=Op)
            if let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Ident if tok.value.as_deref() == Some("end"))
                    || matches!(tok.kind, TokenKind::Op(ref s) if s == "xx")
                {
                    self.i += 1;
                    break;
                }
            }

            // xx as Op won't be eaten by eat_ident — handle separately
            if self.eat_op("xx") { break; }

            let Some(kw) = self.eat_ident() else { break; };

            match kw.as_str() {
                "overlays" => {
                    self.skip_layout_inline();
                    self.eat_op(":");
                    self.skip_layout_inline();
                    let Some(rule) = self.eat_ident() else { continue; };
                    overlay_rule = match rule.as_str() {
                        "split"    => ast::OverlayContinuity::Split,
                        "transfer" => ast::OverlayContinuity::Transfer,
                        "drop"     => ast::OverlayContinuity::Drop,
                        _ => ast::OverlayContinuity::Drop,
                    };
                }
                "links" => {
                    self.skip_layout_inline();
                    self.eat_op(":");
                    self.skip_layout_inline();
                    let Some(rule) = self.eat_ident() else { continue; };
                    link_rule = match rule.as_str() {
                        "inherit" => ast::LinkContinuity::Inherit,
                        "reset"   => ast::LinkContinuity::Reset,
                        _ => ast::LinkContinuity::Reset,
                    };
                }
                // Successor state blocks: first, second, child, fragment, carries
                label @ ("first" | "second" | "child" | "fragment" | "carries") => {
                    self.skip_layout_inline();
                    self.eat_op(":");
                    self.skip_layout();
                    let mut fields: Vec<(String, ast::Expr)> = Vec::new();
                    loop {
                        self.skip_layout();
                        if self.is_eof() { break; }
                        if let Some(tok) = self.peek() {
                            let is_end = matches!(tok.kind, TokenKind::Ident if tok.value.as_deref() == Some("end"))
                                || matches!(tok.kind, TokenKind::Op(ref s) if s == "xx");
                            let is_section_kw = matches!(tok.kind, TokenKind::Ident)
                                && matches!(tok.value.as_deref(), Some("first") | Some("second")
                                    | Some("child") | Some("fragment") | Some("carries")
                                    | Some("overlays") | Some("links") | Some("into"));
                            if is_end {
                                self.i += 1;
                                break;
                            }
                            if is_section_kw {
                                break;
                            }
                        }
                        // Also handle xx as Op directly
                        if self.eat_op("xx") { break; }
                        // Handle ~ prefix for raw fields (e.g. ~gold: ...)
                        let raw_prefix = self.eat_op("~");
                        let Some(base_name) = self.eat_ident() else { break; };
                        let fname = if raw_prefix { format!("{}", base_name) } else { base_name };
                        self.skip_layout_inline();
                        if !self.eat_op(":") { break; }
                        self.skip_layout_inline();
                        let fval_pe = self.parse_coalesce()?;
                        let fval = self.lower_expr(fval_pe);
                        fields.push((fname, fval));
                    }
                    let sp = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
                    successors.push(ast::SuccessorDef {
                        label: label.to_string(),
                        fields,
                        span: sp,
                    });
                }
                "end" | "xx" => {
                    // Consumed the closer — done
                    break;
                }
                _ => {
                    // Unknown keyword — skip to newline
                    while let Some(tok) = self.peek() {
                        if matches!(tok.kind, TokenKind::Newline | TokenKind::Eof) { break; }
                        self.i += 1;
                    }
                }
            }
        }

        let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));
        Ok(ast::TransitionDef {
            kind,
            trigger,
            target_class,
            into_classes,
            successors,
            overlay_rule,
            link_rule,
            span,
        })
    }

    /// Parse optional `at rate [per tick]` after a spread mode keyword.
    fn parse_spread_rate(&mut self) -> f64 {
        self.skip_layout_inline();
        let mut rate = 0.1f64;
        if self.peek_ident() == Some("at") {
            let _ = self.eat_ident();
            self.skip_layout_inline();
            if let Some(n) = self.eat_number_f64() {
                rate = n;
            }
            self.skip_layout_inline();
            if self.peek_ident() == Some("per") {
                let _ = self.eat_ident();
                self.skip_layout_inline();
                if self.peek_ident() == Some("tick") {
                    let _ = self.eat_ident();
                }
            }
        }
        rate
    }

    fn parse_import(&mut self) -> Result<ast::Stmt, String> {
        let start_span = self
            .peek()
            .map(|t| t.span.clone())
            .unwrap_or_else(|| goblin_diagnostics::Span::new("<unknown>", 0, 0, 0, 0, 0, 0));

        // ---------------------------------------------------------------------
        // import
        // ---------------------------------------------------------------------
        if !matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Import)) {
            return Err(s_help_site!("P1001", "Expected 'import'", "import game/hero"));
        }
        self.i += 1;
        self.skip_newlines();

        // ---------------------------------------------------------------------
        // GROUPED IMPORTS: import { a, b as c } from game
        // ---------------------------------------------------------------------
        if self.eat_op("{") {
            self.skip_newlines();

            let mut items = Vec::new();

            loop {
                let Some(name) = self.eat_ident() else {
                    return Err(s_help_site!(
                        "P1010",
                        "Expected item name",
                        "import { hero, Combat } from game"
                    ));
                };

                let alias = if self.peek_ident() == Some("as") {
                    self.i += 1;
                    self.eat_ident()
                } else {
                    None
                };

                items.push(ast::ImportItem { name, alias });
                self.skip_newlines();

                if self.eat_op(",") {
                    self.skip_newlines();
                    if self.peek_op("}") {
                        break;
                    }
                    continue;
                }
                break;
            }

            self.skip_newlines();

            if !self.eat_op("}") {
                return Err(s_help_site!(
                    "P1011",
                    "Expected '}' to close import list",
                    "import { hero, Combat } from game"
                ));
            }

            self.skip_newlines();

            if self.peek_ident() != Some("from") {
                return Err(s_help_site!(
                    "P1012",
                    "Expected 'from' after import list",
                    "import { hero, Combat } from game"
                ));
            }
            self.i += 1;
            self.skip_newlines();

            let Some(source) = self.eat_ident() else {
                return Err(s_help_site!(
                    "P1013",
                    "Expected source path after 'from'",
                    "import { hero } from game"
                ));
            };

            return Ok(ast::Stmt::Import(ast::ImportStmt {
                items: ast::ImportItems::Named { items, source },
                alias: None,
                span: start_span,
            }));
        }

        // ---------------------------------------------------------------------
        // SINGLE IMPORT
        // ---------------------------------------------------------------------

        // 1) String literal import
        if let Some(raw) = self.eat_string_lit() {
            self.skip_newlines();

            let alias = if self.peek_ident() == Some("as") {
                self.i += 1;
                self.eat_ident()
            } else {
                None
            };

            // 🔑 CLASSIFICATION RULE:
            // If the string contains '{', it is dynamic → Expr
            if raw.contains('{') {
                return Ok(ast::Stmt::Import(ast::ImportStmt {
                    items: ast::ImportItems::Expr(
                        ast::Expr::Str(raw, start_span.clone())
                    ),
                    alias,
                    span: start_span,
                }));
            }

            // Otherwise: static path
            return Ok(ast::Stmt::Import(ast::ImportStmt {
                items: ast::ImportItems::Path(raw),
                alias,
                span: start_span,
            }));
        }

        // 2) IDENT / path-style import (game/hero)
        let mut path_parts = Vec::new();
        loop {
            let Some(part) = self.eat_ident() else {
                return Err(s_help_site!(
                    "P1002",
                    "Expected module path after 'import'",
                    "import game/hero"
                ));
            };
            path_parts.push(part);

            if !self.eat_op("/") {
                break;
            }
        }

        let path = path_parts.join("/");

        let alias = if self.peek_ident() == Some("as") {
            self.i += 1;
            self.eat_ident()
        } else {
            None
        };

        Ok(ast::Stmt::Import(ast::ImportStmt {
            items: ast::ImportItems::Path(path),
            alias,
            span: start_span,
        }))
    }

    fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
        let t = match self.bump() {
            Some(t) => t,
            None => {
                let sp = self.toks
                    .get(self.i)
                    .or_else(|| self.toks.last())
                    .map(|t| t.span.clone())
                    .unwrap_or_else(|| Span::new("<eof>", 0, 0, 0, 0, 0, 0));
                return Err(derr_help(
                    "P0106",
                    "I reached the end of the file unexpectedly.",
                    "Close the open construct or add the missing token.",
                    sp,
                ));
            }
        };
        let sp = t.span.clone();
        match &t.kind {
            TokenKind::Ident => {
                let text = t.value.clone().unwrap_or_default();
                Ok(ast::Expr::Ident(text, sp))
            }
            TokenKind::Int | TokenKind::Float | TokenKind::Money => {
                let text = t.value.clone().unwrap_or_default();
                Ok(ast::Expr::Number(text, sp))
            }
            TokenKind::Act | TokenKind::Action => {
                let kw = if matches!(t.kind, TokenKind::Act) { "act" } else { "action" };
                Err(derr_help(
                    "P1005",
                    &format!("The keyword '{}' can't be used here", kw),
                    "Move it to a valid place or remove it: use 'if' to start a block, not inside an expression",
                    sp,
                ))
            }
            _ => Err(derr_help(
                "P1003",
                "Expected an expression here",
                "Use a value, variable, or call: total = price * qty",
                sp,
            )),
        }
    }

    fn parse_primary_impl(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;

        if self.i < self.toks.len() && self.toks[self.i].kind == TokenKind::TripleBraceOpen {
            return self.parse_literal_token();
        }

        // Skip layout tokens before parsing primary expression
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                self.i += 1;
            } else {
                break;
            }
        }

        // Box variable read: #namespace::name
        if let Some(tok) = self.peek() {
            if matches!(tok.kind, TokenKind::HashIdent) {
                let tok = self.toks[self.i].clone();
                self.i += 1;
                let raw = tok.value.unwrap_or_default();
                return if let Some(pos) = raw.find("::") {
                    Ok(PExpr::BoxVar {
                        namespace: raw[..pos].to_string(),
                        name: raw[pos + 2..].to_string(),
                    })
                } else {
                    // plain #tag — preserve existing behaviour
                    Ok(PExpr::Ident(format!("#{}", raw)))
                };
            }
        }

        // Sugar: standalone `..` → empty string ""
        if self.peek_op("..") {
            self.i += 1;
            return Ok(PExpr::Str(String::new()));
        }

        // Check for :builtin pattern
        if self.peek_op(":") {
            if let Some(next_tok) = self.toks.get(self.i + 1) {
                if matches!(next_tok.kind, TokenKind::Ident) {
                    self.i += 1; // consume ':'
                    let name = self.eat_ident().unwrap();
                    let builtin_name = format!(":{}", name);
                    // If followed by '(', parse as FreeCall with args
                    if self.peek_op("(") {
                        self.i += 1; // consume '('
                        let mut args = Vec::new();
                        self.skip_layout();
                        while !self.peek_op(")") {
                            if self.i >= self.toks.len() { break; }
                            args.push(self.parse_compare()?);
                            self.skip_layout_inline();
                            if !self.eat_op(",") { break; }
                            self.skip_layout();
                        }
                        self.eat_op(")");
                        return Ok(PExpr::FreeCall(builtin_name, args));
                    }
                    return Ok(PExpr::Ident(builtin_name));
                }
            }
        }

        // expression-form: judge …
        if let Some("judge") = self.peek_ident() {
            let header_tok_i = self.i;
            let header_line = self.toks[header_tok_i].span.line_start;
            let header_col  = self.toks[header_tok_i].span.col_start;
            let _ = self.eat_ident();

            self.suspend_colon_call += 1;

            // Header: [<subject>] [using <EnumOrExpr>] [return <expr>]
            let mut using_expr: Option<Box<PExpr>> = None;
            let mut using_enum: Option<String> = None;
            let mut header_expr: Option<Box<PExpr>> = None;

            // Optional subject (but not if next is 'using' or 'return')
            if self.peek_ident() != Some("using")
                && !self.peek_is_return()
                && !self.peek_newline_or_eof()
                && !self.peek_op("{")
            {
                let subject = self.parse_compare()?;
                using_expr = Some(Box::new(subject));
            }

            // Optional 'using <Name>'
            if self.peek_ident() == Some("using") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let Some(name) = self.eat_ident() else {
                    self.suspend_colon_call -= 1;
                    return Err(s_help_site!(
                        "P0814",
                        "Expected a name after 'using'",
                        "Write: judge using score or judge status using Status",
                    ));
                };
                let is_cap = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                if is_cap { using_enum = Some(name); } else { using_expr = Some(Box::new(PExpr::Ident(name))); }
            }

            // NEW: optional header 'return <expr>'
            if self.peek_is_return() {
                let _ = self.eat_ident();
                self.skip_newlines();
                let pe = self.parse_assign()?;
                header_expr = Some(Box::new(pe));
            }

            if self.peek_op("{") {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!(
                    "P0812",
                    "Don't put '{' after 'judge'",
                    "Use indentation and close with 'end' or 'xx' (crossbones): judge x > 5: \"big\" end",
                ));
            }
            self.forbid_next_line_brace(header_line, header_col, "judge")?;

            // Enter the block
            self.skip_newlines();
            while let Some(t) = self.peek() {
                if matches!(t.kind, goblin_lexer::TokenKind::Indent) { self.i += 1; } else { break; }
            }

            // NOTE: pass allow_empty = header_expr.is_some()
            let pairs = self.parse_kv_bind_list_judge(header_col, header_expr.is_some())?;
            self.suspend_colon_call -= 1;

            // Consume pending Dedent/Newline before closer
            while let Some(t) = self.peek() {
                use goblin_lexer::TokenKind::*;
                if matches!(t.kind, Dedent | Newline) { self.i += 1; } else { break; }
            }

            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != header_col {
                    let closer = self.peek_ident().unwrap_or("}");
                    return Err(s_help_site!(
                        "P0222",
                        &format!(
                            "This '{}' closer is misaligned: expected column {}, found column {}",
                            closer, header_col, col
                        ),
                        "Align the closer with its header (same column): place 'end' or 'xx' (crossbones) directly under the start of the judge header.",
                    ));
                }
                self.expect_block_close("judge")?;
            } else if !self.eat_layout_until_close(header_col) {
                return Err(s_help_site!(
                    "P0212",
                    "This judge block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones). [parse primary impl]",
                ));
            }

            return Ok(PExpr::Judge {
                using: using_expr,
                using_enum,
                header: header_expr,
                pairs,
            });
        }

        // expression-form: judge_all …
        if let Some("judge_all") = self.peek_ident() {
            let header_tok_i = self.i;
            let header_line = self.toks[header_tok_i].span.line_start;
            let header_col  = self.toks[header_tok_i].span.col_start;
            let _ = self.eat_ident();

            self.suspend_colon_call += 1;

            let mut using_expr: Option<Box<PExpr>> = None;
            let mut using_enum: Option<String> = None;
            let mut header_expr: Option<Box<PExpr>> = None;

            // Optional subject (same rule: not 'using'/'return')
            if self.peek_ident() != Some("using")
                && !self.peek_is_return()
                && !self.peek_newline_or_eof()
                && !self.peek_op("{")
            {
                let subject = self.parse_compare()?;
                using_expr = Some(Box::new(subject));
            }

            if self.peek_ident() == Some("using") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let Some(name) = self.eat_ident() else {
                    self.suspend_colon_call -= 1;
                    return Err(s_help_site!(
                        "P0814",
                        "Expected a name after 'using'",
                        "Write: judge_all using score or judge_all status using Status",
                    ));
                };
                let is_cap = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                if is_cap { using_enum = Some(name); } else { using_expr = Some(Box::new(PExpr::Ident(name))); }
            }

            // Optional header 'return <expr>'
            if self.peek_is_return() {
                let _ = self.eat_ident();
                self.skip_newlines();
                let pe = self.parse_assign()?;
                header_expr = Some(Box::new(pe));
            }

            if self.peek_op("{") {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!(
                    "P0813",
                    "Don't put '{' after 'judge_all'",
                    "Use indentation and close with 'end' or 'xx' (crossbones): judge_all x > 5: \"big\" end",
                ));
            }
            self.forbid_next_line_brace(header_line, header_col, "judge_all")?;

            self.skip_newlines();
            while let Some(t) = self.peek() {
                if matches!(t.kind, goblin_lexer::TokenKind::Indent) { self.i += 1; } else { break; }
            }

            let pairs = self.parse_kv_bind_list_judge(header_col, header_expr.is_some())?;
            self.suspend_colon_call -= 1;

            while let Some(t) = self.peek() {
                use goblin_lexer::TokenKind::*;
                if matches!(t.kind, Dedent | Newline) { self.i += 1; } else { break; }
            }

            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != header_col {
                    let closer = self.peek_ident().unwrap_or("}");
                    return Err(s_help_site!(
                        "P0222",
                        &format!(
                            "This '{}' closer is misaligned: expected column {}, found column {}",
                            closer, header_col, col
                        ),
                        "Align the closer with its header...",
                    ));
                }
                self.expect_block_close("judge_all")?;
            } else if !self.eat_layout_until_close(header_col) {
                return Err(s_help_site!(
                    "P0212",
                    "This judge_all block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones). [parse primary impl]",
                ));
            }

            return Ok(PExpr::JudgeAll {
                using: using_expr,
                using_enum,
                header: header_expr,
                pairs,
            });
        }

        // --- blob literal special-case: blob "..." | blob 0xDEAD... ---
        if let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Blob) {
                self.i += 1;
                self.skip_newlines();

                let (k2, v2) = match self.peek() {
                    Some(t2) => (t2.kind.clone(), t2.value.clone()),
                    None => {
                        return Err(s_help_site!(
                            "P1101",
                            "You need a string or an integer after 'blob'",
                            "Examples: blob \"text\" or blob 0xFF",
                        ))
                    }
                };

                let expr = match k2 {
                    TokenKind::String => {
                        let lit = v2.unwrap_or_default();
                        self.i += 1;
                        PExpr::BlobStr(lit)
                    }
                    TokenKind::Int => {
                        let lit = v2.unwrap_or_default();
                        self.i += 1;
                        PExpr::BlobNum(lit)
                    }
                    _ => {
                        return Err(s_help_site!(
                            "P1101",
                            "You need a string or an integer after 'blob'.",
                            "Examples: blob \"text\" or blob 0xFF.",
                        ))
                    }
                };

                return Ok(self.apply_postfix_ops(expr));
            }
        }

        // --- Parenthesized ---
        if self.peek_op("(") {
            let _ = self.eat_op("(");
            self.skip_newlines();

            let expr = self.with_depth(|p| p.parse_coalesce())?;
            self.skip_newlines();

            // NEW: if there is a comma before the ')', this isn't a plain parenthesized expr.
            // We are not adding tuple/target syntax here—just guiding the user clearly.
            if self.peek_op(",") {
                return Err(s_help_site!(
                    "P0715",
                    "Comma inside parentheses isn’t allowed in a single parenthesized expression.",
                    "Goblin doesn’t support tuple targets or tuple literals in parentheses yet. \
        If you meant destructuring on the left side, write it without parentheses: x, y = one(). \
        If you meant a function call, use foo(x, y). \
        If you meant a list, use [x, y].",
                ));
            }

            if !self.eat_op(")") {
                return Err(s_help_site!(
                    "P0707",
                    "Expected ')' to close this parenthesized expression",
                    "Add the closing ')': (a + b)",
                ));
            }
            return Ok(self.apply_postfix_ops(expr));
        }

        // --- Array literal ---
        if self.eat_op("[") {
            // skip newlines after '['
            while let Some(tok) = self.toks.get(self.i) {
                if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
            }

            let mut elems = Vec::new();
            if !self.peek_op("]") {
                loop {
                    let elem = self.with_depth(|p| p.parse_coalesce())?;
                    elems.push(elem);

                    if self.eat_op(",") {
                        while let Some(tok) = self.toks.get(self.i) {
                            if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
                        }
                        if self.peek_op("]") { break; }
                        continue;
                    }
                    break;
                }
            }

            while let Some(tok) = self.toks.get(self.i) {
                if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
            }

            if !self.eat_op("]") {
                return Err(s_help_site!(
                    "P0708",
                    "Expected ']' to close this array",
                    "Add the closing ']': [1, 2, 3]",
                ));
            }
            return Ok(self.apply_postfix_ops(PExpr::Array(elems)));
        }

        // --- Identifier (name, keywords; forbid legacy `Type: ...`) ---
        if let Some(tok0) = self.peek().cloned() {
            if matches!(tok0.kind, TokenKind::Ident) {
                let name = tok0.value.clone().unwrap_or_default();
                self.i += 1; // eat the ident

                // DEPRECATE old `Type: ...`
                if self.peek_op(":") && self.suspend_colon_call == 0 && !self.in_object_construction {
                    return Err(s_help_site!(
                        "P0998",
                        "Object construction has moved to 'name | Type = ...'.",
                        &format!("Write: myVar | {} = name: \"...\"", Self::capitalize_like(&name)),
                    ));
                }

                if name == "skip" {
                    // no args
                    let span = self.toks[self.i - 1].span.clone();
                    return Ok(self.apply_postfix_ops(PExpr::FreeCall("skip".to_string(), vec![])));
                }
                if name == "stop" {
                    // no args
                    let span = self.toks[self.i - 1].span.clone();
                    return Ok(self.apply_postfix_ops(PExpr::FreeCall("stop".to_string(), vec![])));
                }

                // ---- Special no-parens grammar: "reap [count] from <src>" and "reap! [count] from <ident>" ----
                // --- destructive reap! (mirrors "reap N from xs") ---
                if name == "reap!" {
                    self.skip_newlines();

                    // Optional count before 'from' (default 1 if omitted)
                    // If the next token is literally the identifier "from", skip parsing count.
                    let count_opt = {
                        let is_from_kw = match self.peek() {
                            Some(t) if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("from") => true,
                            _ => false,
                        };
                        if is_from_kw {
                            None
                        } else {
                            // Parse one expression as count (e.g., 3)
                            // (We keep it general Expr so you can later allow names/exprs if desired.)
                            Some(self.parse_coalesce()?)
                        }
                    };

                    self.skip_newlines();

                    // Expect the keyword 'from'
                    match self.peek().cloned() {
                        Some(t) if matches!(t.kind, TokenKind::Ident) && t.value.as_deref() == Some("from") => {
                            self.i += 1; // consume 'from'
                        }
                        _ => {
                            return Err(s_help_site!(
                                "P1410",
                                "Expected 'from' here",
                                "Write it like: reap! 3 from xs",
                            ));
                        }
                    }

                    self.skip_newlines();

                    // Expect a variable name (the target to mutate)
                    let src_ident = match self.peek().cloned() {
                        Some(t) if matches!(t.kind, TokenKind::Ident) => {
                            self.i += 1; // safe: cloned above
                            t.value.unwrap_or_default()
                        }
                        _ => {
                            return Err(s_help_site!(
                                "P1411",
                                "reap! expects a variable name after 'from'",
                                "Example: reap! 3 from xs",
                            ));
                        }
                    };

                    // Build args for the destructive lowering:
                    //   FreeCall("reap!", [Ident(src), (count)?])
                    let mut args = Vec::<PExpr>::new();
                    args.push(PExpr::Ident(src_ident));
                    if let Some(c) = count_opt { args.push(c); }

                    return Ok(self.apply_postfix_ops(PExpr::FreeCall("reap!".to_string(), args)));
                }

                // ---- NO-PARENS FREE-CALL WHITELIST (one-arg) ----
                // Allow: upper "x", lower "x", title "x", slug "x", mixed "x"
                fn is_no_parens_freecall(name: &str) -> bool {
                    matches!(
                        name,
                        "say" | "sweep" | "sweep_all" |
                        "upper" | "lower" | "title" | "slug" | "mixed" | "raw" |
                        "trim" | "trim_lead" | "trim_trail" | "minimize" |
                        "reverse" | "reverse_chars" | "lines" | "words" | "chars" |
                        "keys" | "values" | "items" |
                        "count" | "len" |
                        "parse_bool" | "json_parse" | "json_stringify" | "json_stringify_pretty" |
                        "is_even" | "is_odd" | "is_positive" | "is_negative" |
                        "is_alpha" | "is_digit" | "is_alnum" | "is_whitespace"
                    )
                }

                let bare = name.strip_prefix(':').unwrap_or(&name);
                if is_no_parens_freecall(bare) {
                    self.skip_newlines();

                    // Inline: does the next token start an expression?
                    let starts_expr = match self.peek() {
                        Some(t) => match &t.kind {
                            // literals
                            TokenKind::String
                            | TokenKind::Int
                            | TokenKind::Float
                            | TokenKind::Money
                            | TokenKind::Duration
                            | TokenKind::Date
                            | TokenKind::Time
                            | TokenKind::DateTime
                            | TokenKind::Blob => true,

                            // identifiers, but do NOT swallow repeat aliases
                            TokenKind::Ident => {
                                !matches!(t.value.as_deref(), Some("as"))
                            }

                            // grouping / collection / prefix ops that begin an expr
                            TokenKind::Op(s) if s == "(" || s == "[" || s == "{" || s == "-" || s == "+" || s == "!" => true,

                            _ => false,
                        },
                        None => false,
                    };

                    if starts_expr {
                        // Special-case: `say` should consume a FULL expression as its single arg,
                        // and must NOT allow postfix chaining on the call itself.
                        if name == "say" {
                            let arg = self.parse_additive()?;   // <-- full expr (handles ++, +, >>, etc.)
                            return Ok(PExpr::FreeCall(bare.to_string(), vec![arg])); // <-- no apply_postfix_ops here
                        }

                        // All other bare calls keep the existing behavior.
                        let arg = self.parse_coalesce()?;
                        return Ok(self.apply_postfix_ops(PExpr::FreeCall(bare.to_string(), vec![arg])));
                    }
                    // else: fall through to ident handling below
                }

                // true/false/nil or plain identifier
                let base = match name.as_str() {
                    "true" => PExpr::Bool(true),
                    "false" => PExpr::Bool(false),
                    "nil" => PExpr::Nil,
                    _ => PExpr::Ident(name),
                };

                return Ok(self.apply_postfix_ops(base));
            }
        }

        // --- Object literal ---
        if self.eat_op("{") {
            // skip newlines after '{'
            while let Some(tok) = self.toks.get(self.i) {
                if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
            }

            let mut props: Vec<(String, PExpr)> = Vec::new();
            if !self.peek_op("}") {
                loop {
                    let Some(key) = self.eat_object_key() else {
                        return Err(s_help_site!(
                            "P1201",
                            "Expected an object key here",
                            "Add a property name before ':': name: \"John\"",
                        ));
                    };

                    while let Some(tok) = self.toks.get(self.i) {
                        if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
                    }

                    if !self.eat_op(":") {
                        return Err(s_help_site!(
                            "P1202",
                            "You need a ':' after the object key",
                            "Write it like: name: \"John\"",
                        ));
                    }

                    while let Some(tok) = self.toks.get(self.i) {
                        if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
                    }

                    let value = self.with_depth(|p| p.parse_assign())?;
                    props.push((key, value));

                    if self.eat_op(",") {
                        while let Some(tok) = self.toks.get(self.i) {
                            if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
                        }
                        if self.peek_op("}") { break; }
                        continue;
                    }
                    break;
                }
            }

            while let Some(tok) = self.toks.get(self.i) {
                if matches!(tok.kind, TokenKind::Newline) { self.i += 1; } else { break; }
            }

            if !self.eat_op("}") {
                return Err(s_help_site!(
                    "P1203",
                    "Expected '}' to close this object",
                    "Add the closing '}': { name: \"John\" }",
                ));
            }

            return Ok(self.apply_postfix_ops(PExpr::Object(props)));
        }

        // --- Lit / other dispatch (ident handled above) ---
        let (kind, val_opt) = match self.peek() {
            Some(t) => (t.kind.clone(), t.value.clone()),
            None => {
                return Err(s_help_site!(
                    "P1003",
                    "Expected an expression here",
                    "Use a value, variable, or call: total = price * qty",
                ))
            }
        };

        let expr = match kind {
            TokenKind::Char => {
                // The lexer put exactly one Unicode scalar in `val_opt` (e.g., "a", "\n", "😀")
                let s = val_opt.as_deref().unwrap_or("");
                let mut it = s.chars();
                let ch = if let (Some(c), None) = (it.next(), it.next()) {
                    c
                } else {
                    return Err(s_help_site!(
                        "P0214",
                        "Invalid char literal payload from lexer",
                        "This should be exactly one Unicode character like 'a', '\\n', or '😀'.",
                    ));
                };
                self.i += 1;
                PExpr::Char(ch)
            }

            // MONEY
            TokenKind::Money => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                PExpr::Money(lit)
            }

            TokenKind::Int => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                if let Some(t) = self.peek() {
                    if t.kind == TokenKind::Op("unit".into()) {
                        let unit = t.value.clone().unwrap_or_default();
                        self.i += 1;
                        PExpr::IntWithUnit(lit, unit)
                    } else {
                        PExpr::Int(lit)
                    }
                } else {
                    PExpr::Int(lit)
                }
            }

            TokenKind::Float => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                if let Some(t) = self.peek() {
                    if t.kind == TokenKind::Op("unit".into()) {
                        let unit = t.value.clone().unwrap_or_default();
                        self.i += 1;
                        PExpr::FloatWithUnit(lit, unit)
                    } else {
                        PExpr::Float(lit)
                    }
                } else {
                    PExpr::Float(lit)
                }
            }

            TokenKind::Duration => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                let (base, unit) = Self::split_duration_lexeme(&lit)?;
                let is_floaty = base.contains('.') || base.contains('e') || base.contains('E');
                if is_floaty {
                    PExpr::FloatWithUnit(base, unit)
                } else {
                    PExpr::IntWithUnit(base, unit)
                }
            }

            // --- String literal: ALWAYS plain string (no StrInterp here) ---
            TokenKind::String => {
                let lit = self.toks[self.i].value.clone().unwrap_or_default();
                self.i += 1;
                PExpr::Str(lit)
            }

            TokenKind::Date => {
                self.i += 1;
                self.skip_newlines();
                let (k2, v2) = match self.peek() {
                    Some(t2) => (t2.kind.clone(), t2.value.clone()),
                    None => {
                        return Err(s_help_site!(
                            "P1301",
                            "Expected a string after 'date'.",
                            "Write it like: date \"2023-12-25\".",
                        ))
                    }
                };
                if !matches!(k2, TokenKind::String) {
                    return Err(s_help_site!(
                        "P1301",
                        "Expected a string after 'date'",
                        "Write it like: date \"2023-12-25\"",
                    ));
                }
                let lit = v2.unwrap_or_default();
                self.i += 1;
                PExpr::Date(lit)
            }

            TokenKind::Time => {
                self.i += 1;
                self.skip_newlines();
                let (k2, v2) = match self.peek() {
                    Some(t2) => (t2.kind.clone(), t2.value.clone()),
                    None => {
                        return Err(s_help_site!(
                            "P1302",
                            "Expected a string after 'time'",
                            "Write it like: time \"14:30:00\"",
                        ))
                    }
                };
                if !matches!(k2, TokenKind::String) {
                    return Err(s_help_site!(
                        "P1302",
                        "Expected a string after 'time'",
                        "Write it like: time \"14:30:00\"",
                    ));
                }
                let lit = v2.unwrap_or_default();
                self.i += 1;
                PExpr::Time(lit)
            }

            TokenKind::DateTime => {
                self.i += 1;
                self.skip_newlines();
                let (k2, v2) = match self.peek() {
                    Some(t2) => (t2.kind.clone(), t2.value.clone()),
                    None => {
                        return Err(s_help_site!(
                            "P1303",
                            "Expected a string after 'datetime'.",
                            "Write it like: datetime \"2023-12-25T14:30:00\".",
                        ))
                    }
                };
                if !matches!(k2, TokenKind::String) {
                    return Err(s_help_site!(
                        "P1303",
                        "Expected a string after 'datetime'",
                        "Write it like: datetime \"2023-12-25T14:30:00\"",
                    ));
                }
                let value = v2.unwrap_or_default();
                self.i += 1;
                self.skip_newlines();

                let mut tz: Option<String> = None;
                if let Some(tz_tok) = self.peek() {
                    if matches!(tz_tok.kind, TokenKind::Ident) && tz_tok.value.as_deref() == Some("tz") {
                        self.i += 1;
                        self.skip_newlines();
                        if !self.eat_op(":") {
                            return Err(s_help_site!(
                                "P1304",
                                "You need a ':' after tz",
                                "Write it like: tz: \"UTC\"",
                            ));
                        }
                        self.skip_newlines();
                        let (k3, v3) = match self.peek() {
                            Some(t3) => (t3.kind.clone(), t3.value.clone()),
                            None => {
                                return Err(s_help_site!(
                                    "P1305",
                                    "Expected a string after tz:",
                                    "Write it like: tz: \"UTC\"",
                                ))
                            }
                        };
                        if !matches!(k3, TokenKind::String) {
                            return Err(s_help_site!(
                                "P1305",
                                "Expected a string after tz:",
                                "Write it like: tz: \"UTC\"",
                            ));
                        }
                        tz = Some(v3.unwrap_or_default());
                        self.i += 1;
                    }
                }
                PExpr::DateTime { value, tz }
            }

            // Catch-all for orphaned operators at primary position: consume + error (prevents loops).
            TokenKind::Op(ref s) => {
                let sp = self.toks[self.i].span.clone();
                self.i += 1;
                return Err(s_help_site!(
                    "P1006",
                    &format!(
                        "Expected an expression, but found operator '{}' at line {}, col {}",
                        s, sp.line_start, sp.col_start
                    ),
                    "Add a value or name before the operator: total | price * qty (not * qty)",
                ));
            }

            _ => return Err(self.err_expected_expr("at start of primary")),
        };

        Ok(self.apply_postfix_ops(expr))
    }

    #[inline]
    fn lhs_ends_with_dot_type_at(&self, eq_i: usize) -> bool {
        if eq_i == 0 { return false; }

        // Walk left from the '=' position, skipping layout tokens
        let mut j = eq_i;
        while j > 0 {
            match self.toks[j - 1].kind {
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => j -= 1,
                _ => break,
            }
        }
        if j == 0 { return false; }

        // Must be Ident("type")
        let t_ident = &self.toks[j - 1];
        if !matches!(t_ident.kind, TokenKind::Ident) || t_ident.value.as_deref() != Some("type") {
            return false;
        }

        // Skip layout again to find preceding '.'
        let mut k = j - 1;
        while k > 0 {
            match self.toks[k - 1].kind {
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => k -= 1,
                _ => break,
            }
        }
        if k == 0 { return false; }

        let t_dot = &self.toks[k - 1];
        matches!(t_dot.kind, TokenKind::Op(ref op) if op == ".")
    }

    fn parse_coalesce_impl(&mut self) -> Result<PExpr, String> {

        // Nullish coalescing layer (above OR)
        let mut lhs = self.with_depth(|p| p.parse_or())?;

        while self.eat_op("??") {
            self.skip_newlines();
            let rhs = self.with_depth(|p| p.parse_or())?;
            lhs = PExpr::Binary(Box::new(lhs), "??".into(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_or(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_and())?;
        loop {
            if self.peek_ident() == Some("or") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_and())?;
                lhs = PExpr::Binary(Box::new(lhs), "or".into(), Box::new(rhs));
                continue;
            }
            if self.eat_op("<>") {
                let rhs = self.with_depth(|p| p.parse_and())?;
                lhs = PExpr::Binary(Box::new(lhs), "<>".into(), Box::new(rhs));
                continue;
            }
            break;
        }
        Ok(lhs)
    }
    
    fn parse_and(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_compare())?;
        loop {
            if self.peek_ident() == Some("and") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_compare())?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            // support && alias if your lexer emits it as Op("&&")
            if self.eat_op("&&") {
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_compare())?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn parse_compare(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_range())?;

        loop {
            // textual: is / is not
            if self.peek_ident() == Some("is") {
                let _ = self.eat_ident();
                let neg = self.peek_ident() == Some("not");
                if neg { let _ = self.eat_ident(); }
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_additive())?;
                let op = if neg { "is not" } else { "is" };
                lhs = PExpr::Binary(Box::new(lhs), op.into(), Box::new(rhs));
                continue;
            }

            // predicate:  <lhs> between <lo> .. <hi>    or    <lhs> !between <lo> .. <hi>
            if self.peek_ident() == Some("between")
                || (self.peek_op("!") && self.toks.get(self.i + 1).and_then(|t| t.value.as_deref()) == Some("between"))
            {
                // consume '!between' or 'between'
                let mut neg = false;
                if self.peek_op("!") {
                    self.i += 1; // '!'
                    debug_assert_eq!(self.peek_ident().as_deref(), Some("between"));
                    let _ = self.eat_ident(); // 'between'
                    neg = true;
                } else {
                    let _ = self.eat_ident(); // 'between'
                }

                self.skip_newlines();
                // lower bound
                let lo = self.with_depth(|p| p.parse_additive())?;

                self.skip_newlines();
                // dots: "..." (inclusive upper) preferred over ".." (exclusive upper)
                let inclusive_upper = if self.eat_op("...") {
                    true
                } else if self.eat_op("..") {
                    false
                } else {
                    return Err(s_help_site!(
                        "P1004",
                        "Expected '..' or '...' after the lower bound in 'between'",
                        "Write it like:  x between 1..5   or   x between 10...20",
                    ));
                };

                self.skip_newlines();
                // upper bound
                let hi = self.with_depth(|p| p.parse_additive())?;

                // Desugar:
                //   between: (lo <= lhs) and (lhs <|<= hi)
                //   !between: (lhs < lo) or (lhs > hi)
                if neg {
                    let a = PExpr::Binary(Box::new(lhs.clone()), "<".into(), Box::new(lo));
                    let b = PExpr::Binary(Box::new(lhs.clone()), ">".into(), Box::new(hi));
                    lhs = PExpr::Binary(Box::new(a), "or".into(), Box::new(b));
                } else {
                    let left = PExpr::Binary(Box::new(lo), "<=".into(), Box::new(lhs.clone()));
                    let right_op = if inclusive_upper { "<=" } else { "<" };
                    let right = PExpr::Binary(Box::new(lhs.clone()), right_op.into(), Box::new(hi));
                    lhs = PExpr::Binary(Box::new(left), "and".into(), Box::new(right));
                }

                continue;
            }

            if self.eat_op("===")  { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "===".into(), Box::new(rhs)); continue; }
            if self.eat_op("!==")  { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "!==".into(), Box::new(rhs)); continue; }
            if self.eat_op("==")   { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "==".into(),  Box::new(rhs)); continue; }
            if self.eat_op("!=")   { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "!=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<=")   { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "<=".into(),  Box::new(rhs)); continue; }
            if self.eat_op(">=")   { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), ">=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<")    { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), "<".into(),   Box::new(rhs)); continue; }
            if self.eat_op(">")    { self.skip_newlines(); let rhs = self.with_depth(|p| p.parse_additive())?; lhs = PExpr::Binary(Box::new(lhs), ">".into(),   Box::new(rhs)); continue; }

            break;
        }

        Ok(lhs)
    }

    fn parse_range(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_additive()?; // was parse_join()

        loop {
            let op = if self.eat_op("...") { "..." }
            else if self.eat_op("..") { ".." }
            else { break };

            let rhs = self.parse_additive()?; // was parse_join()
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_additive(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_multiplicative())?;

        loop {
            // allow chaining inside say() expressions

            if self.eat_op("++") {
                self.skip_newlines();
                let mut rhs = self.with_depth(|p| p.parse_multiplicative())?;
                if let PExpr::Postfix(inner, op) = &rhs {
                    if op == "%s" {
                        let n_pct = PExpr::Postfix(Box::new((**inner).clone()), "%".to_string());
                        rhs = PExpr::Binary(Box::new(n_pct), "of".to_string(), Box::new(lhs.clone()));
                    }
                }
                lhs = PExpr::Binary(Box::new(lhs), "++".to_string(), Box::new(rhs));
                continue;
            } else if self.eat_op("+") {
                self.skip_newlines();
                let mut rhs = self.with_depth(|p| p.parse_multiplicative())?;
                if let PExpr::Postfix(inner, op) = &rhs {
                    if op == "%s" {
                        let n_pct = PExpr::Postfix(Box::new((**inner).clone()), "%".to_string());
                        rhs = PExpr::Binary(Box::new(n_pct), "of".to_string(), Box::new(lhs.clone()));
                    }
                }
                lhs = PExpr::Binary(Box::new(lhs), "+".to_string(), Box::new(rhs));
                continue;
            } else if self.eat_op("-") {
                self.skip_newlines();
                let mut rhs = self.with_depth(|p| p.parse_multiplicative())?;
                if let PExpr::Postfix(inner, op) = &rhs {
                    if op == "%s" {
                        let n_pct = PExpr::Postfix(Box::new((**inner).clone()), "%".to_string());
                        rhs = PExpr::Binary(Box::new(n_pct), "of".to_string(), Box::new(lhs.clone()));
                    }
                }
                lhs = PExpr::Binary(Box::new(lhs), "-".to_string(), Box::new(rhs));
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    fn parse_multiplicative(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_power())?;

        loop {
            let op = if self.eat_op("><") { "><" }       // divmod
                     else if self.eat_op("//") { "//" }  // floor division
                     else if self.eat_op("*")  { "*"  }
                     else if self.eat_op("/")  { "/"  }
                     else if self.eat_op("%")  { "%"  }  // modulo (NOT percent-literal)
                     else { break };

            self.skip_newlines();
            let mut rhs = self.with_depth(|p| p.parse_power())?;

            // Desugar RHS `N%s` => `(N% of lhs)`
            if let PExpr::Postfix(inner, op_pct) = &rhs {
                if op_pct == "%s" {
                    let n_pct = PExpr::Postfix(Box::new((**inner).clone()), "%".to_string());
                    rhs = PExpr::Binary(Box::new(n_pct), "of".to_string(), Box::new(lhs.clone()));
                }
            }

            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_power(&mut self) -> Result<PExpr, String> {
        let lhs = self.with_depth(|p| p.parse_unary())?;

        if self.eat_op("**") {
            self.skip_newlines();
            let rhs = self.with_depth(|p| p.parse_power())?;
            return Ok(PExpr::Binary(Box::new(lhs), "**".into(), Box::new(rhs)));
        }
        if self.eat_op("^^") {
            self.skip_newlines();
            let rhs = self.with_depth(|p| p.parse_power())?;
            return Ok(PExpr::Binary(Box::new(lhs), "^^".into(), Box::new(rhs)));
        }
        Ok(lhs)
    }

    /// Parse a block of **statements** for a judge arm until we dedent back to `arm_col`
    /// or we see the next arm/closer. Returns Vec<ast::Stmt>.
    fn parse_stmt_block_until_dedent_or_next_case(&mut self, arm_col: u32) -> Result<Vec<ast::Stmt>, String> {
        use goblin_lexer::TokenKind;

        let mut out: Vec<ast::Stmt> = Vec::new();

        loop {
            // Skip blank lines AND layout tokens between statements
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // Stop if we dedent back to the arm's column (or less)
            let current_col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if current_col <= arm_col {
                break;
            }

            // Stop at a block closer ('end' / 'xx')
            if self.peek_block_close() {
                break;
            }

            if self.is_eof() {
                break;
            }

            // Parse one statement
            let stmt = self.parse_stmt()?;
            out.push(stmt);

            // Optional separators
            self.eat_semi_separators();
        }

        Ok(out)
    }

    fn parse_provoke_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        
        let header_tok_i = self.i;
        let header_line  = self.toks[header_tok_i].span.line_start;
        let header_col   = self.toks[header_tok_i].span.col_start;
        
        debug_assert_eq!(self.peek_ident().as_deref(), Some("provoke"));
        let _ = self.eat_ident();
        
        // Check for inline form: provoke => condition
        self.skip_newlines();
        if self.peek_op("=>") {
            let _ = self.eat_op("=>");
            self.skip_newlines();
            
            // Parse the condition expression
            let cond_expr = self.parse_assign()?;
            let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
            let condition = Self::lower_expr_preview(cond_expr, span.clone());
            
            // Generate: :provoke(condition)
            let provoke_call = ast::Expr::FreeCall(
                "provoke".to_string(),
                vec![condition],
                span.clone()
            );
            
            return Ok(ast::Stmt::Expr(provoke_call));
        }
        
        // Block form: provoke \n conditions... \n xx
        if self.peek_op("{") {
            return Err(s_help_site!(
                "P0812",
                "Don't put '{' after 'provoke'",
                "Use indentation and close with 'end' or 'xx'."
            ));
        }
        
        self.forbid_next_line_brace(header_line, header_col, "provoke")?;
        self.skip_newlines();
        
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Indent) {
                self.i += 1;
            } else {
                break;
            }
        }
        
        // Parse conditions (one per line)
        let mut conditions: Vec<ast::Expr> = Vec::new();
        
        loop {
            self.skip_newlines();
            
            // Check for block closer
            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Ident if t.value.as_deref() == Some("end") && t.span.col_start == header_col => break,
                    TokenKind::Op(op) if op == "xx" && t.span.col_start == header_col => break,
                    _ => {}
                }
            } else {
                return Err(s_help_site!(
                    "P0212",
                    "This provoke block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block."
                ));
            }
            
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Indent) {
                    self.i += 1;
                } else {
                    break;
                }
            }
            
            if let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Dedent) {
                    break;
                }
            }
            
            if self.is_eof() {
                return Err(s_help_site!(
                    "P0212",
                    "This provoke block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block."
                ));
            }
            
            // Parse one condition
            let cond_expr = self.parse_assign()?;
            let cond_span = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i.saturating_sub(1));
            conditions.push(Self::lower_expr_preview(cond_expr, cond_span));
            
            self.skip_newlines();
        }
        
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Dedent | TokenKind::Newline) {
                self.i += 1;
            } else {
                break;
            }
        }
        
        if self.peek_block_close() {
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if col != header_col {
                let closer = self.peek_ident().unwrap_or("}");
                return Err(s_help_site!(
                    "P0222",
                    &format!("This '{}' closer is misaligned: expected column {}, found {}", closer, header_col, col),
                    "Align the closer with its header."
                ));
            }
            self.expect_block_close("provoke")?;
        } else if !self.eat_layout_until_close(header_col) {
            return Err(s_help_site!(
                "P0212",
                "This provoke block is missing its closing 'end' or 'xx' (crossbones).",
                "Close the block."
            ));
        }
        
        let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
        
        // Generate multiple :provoke() calls wrapped in a block
        let mut stmts = Vec::new();
        for condition in conditions {
            let provoke_call = ast::Expr::FreeCall(
                "provoke".to_string(),
                vec![condition],
                span.clone()
            );
            stmts.push(ast::Stmt::Expr(provoke_call));
        }
        
        // Wrap in a Block expression
        let block_expr = ast::Expr::Block {
            stmts,
            span: span.clone()
        };
        
        Ok(ast::Stmt::Expr(block_expr))
    }

    fn parse_judge_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        use ast::{JudgeArmBody, JudgeArmStmt, JudgeStmt, Stmt};

        let header_tok_i = self.i;
        let header_line  = self.toks[header_tok_i].span.line_start;
        let header_col   = self.toks[header_tok_i].span.col_start;

        debug_assert_eq!(self.peek_ident().as_deref(), Some("judge"));
        let _ = self.eat_ident();

        self.suspend_colon_call += 1;

        // Header: [<subject>] [using <EnumOrExpr>] [return <expr>]
        let mut using_expr: Option<Box<PExpr>> = None;
        let mut using_enum: Option<String> = None;
        let mut default_body: Option<ast::JudgeArmBody> = None;

        if self.peek_ident() != Some("using")
            && !self.peek_is_return()
            && !self.peek_newline_or_eof()
            && !self.peek_op("{")
        {
            let subject = self.parse_compare()?;
            using_expr = Some(Box::new(subject));
        }

        if self.peek_ident() == Some("using") {
            let _ = self.eat_ident();
            self.skip_newlines();
            let Some(name) = self.eat_ident() else {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0814","Expected a name after 'using'","Write: judge status using Status  or  judge using score"));
            };
            let is_cap = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
            if is_cap { using_enum = Some(name); } else { using_expr = Some(Box::new(PExpr::Ident(name))); }
        }

        // Single keyword: 'return' <expr>  (default body for empty arms)
        if self.peek_is_return() {
            let _ = self.eat_ident();
            self.skip_newlines();
            let pe = self.parse_assign()?;
            let sp = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
            let expr = Self::lower_expr_preview(pe, sp.clone());
            let ret  = ast::Stmt::Return(ast::ReturnStmt { values: vec![expr], span: sp.clone() });
            default_body = Some(ast::JudgeArmBody::Stmts(vec![ret]));
        }

        if self.peek_op("{") {
            self.suspend_colon_call -= 1;
            return Err(s_help_site!("P0812","Don't put '{' after 'judge'","Use indentation and close with 'end' or 'xx'."));
        }
        self.forbid_next_line_brace(header_line, header_col, "judge")?;

        self.skip_newlines();
        while let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Indent) { self.i += 1; } else { break; } }

        let mut arms: Vec<ast::JudgeArmStmt> = Vec::new();

        loop {
            self.skip_newlines();

            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Ident if t.value.as_deref() == Some("end") && t.span.col_start == header_col => break,
                    TokenKind::Op(op) if op == "xx" && t.span.col_start == header_col => break,
                    _ => {}
                }
            } else {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212","This judge block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge stmt]"));
            }

            while let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Indent) { self.i += 1; } else { break; } }
            if let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Dedent) { break; } }
            if self.is_eof() {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212","This judge block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge stmt]"));
            }

            let arm_col: u32 = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(header_col);

            let is_else = if let Some(tok) = self.peek() {
                matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("else")
            } else { false };

            let cond_opt = if is_else {
                let _ = self.eat_ident();
                None
            } else {
                let raw = self.parse_judge_condition()?;
                let expanded = if let Some(ref subj) = using_expr { Self::expand_condition(subj, &raw, &using_enum) } else { raw };
                let cond_span = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i.saturating_sub(1));
                Some(Box::new(Self::lower_expr_preview(expanded, cond_span)))
            };

            if !self.eat_op(":") {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0811","You need ':' after the condition in 'judge'","Write it like: judge x > 5: say \"big\""));
            }

            let body = if self.peek_newline_or_eof() {
                self.skip_newlines();
                match self.peek() {
                    Some(t) if matches!(t.kind, TokenKind::Indent) => {
                        self.i += 1;
                        let stmts = self.parse_stmt_block_until_dedent_or_next_case(arm_col)?;
                        JudgeArmBody::Stmts(stmts)
                    }
                    _ => {
                        if let Some(db) = default_body.clone() { db } else {
                            return Err(s_help_site!("P0816","Expected an indented block after ':' in judge arm","Start the arm body on the next line and indent it."));
                        }
                    }
                }
            } else {
                let stmt = self.parse_stmt()?;
                self.eat_semi_separators();
                JudgeArmBody::Stmts(vec![stmt])
            };

            let arm_span = self.toks.get(self.i.saturating_sub(1)).map(|t| t.span.clone())
                .unwrap_or_else(|| self.toks[header_tok_i].span.clone());

            arms.push(JudgeArmStmt { condition: cond_opt, body, span: arm_span });
            self.skip_newlines();
        }

        self.suspend_colon_call -= 1;

        while let Some(t) = self.peek() {
            use goblin_lexer::TokenKind::*;
            if matches!(t.kind, Dedent | Newline) { self.i += 1; } else { break; }
        }

        if self.peek_block_close() {
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if col != header_col {
                let closer = self.peek_ident().unwrap_or("}");
                return Err(s_help_site!("P0222",&format!("This '{}' closer is misaligned: expected column {}, found {}", closer, header_col, col),"Align the closer with its header."));
            }
            self.expect_block_close("judge")?;
        } else if !self.eat_layout_until_close(header_col) {
            return Err(s_help_site!("P0212","This judge block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge stmt]"));
        }

        let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
        Ok(Stmt::Judge(JudgeStmt { arms, span }))
    }

    fn parse_judge_all_stmt(&mut self) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        use ast::{JudgeArmBody, JudgeArmStmt, JudgeAllStmt, Stmt};

        let header_tok_i = self.i;
        let header_line  = self.toks[header_tok_i].span.line_start;
        let header_col   = self.toks[header_tok_i].span.col_start;

        debug_assert_eq!(self.peek_ident().as_deref(), Some("judge_all"));
        let _ = self.eat_ident();

        self.suspend_colon_call += 1;

        // Header: [<subject>] [using <EnumOrExpr>] [return <expr>]
        let mut using_expr: Option<Box<PExpr>> = None;
        let mut using_enum: Option<String> = None;
        let mut default_body: Option<ast::JudgeArmBody> = None;

        if self.peek_ident() != Some("using")
            && !self.peek_is_return()
            && !self.peek_newline_or_eof()
            && !self.peek_op("{")
        {
            let subject = self.parse_compare()?;
            using_expr = Some(Box::new(subject));
        }

        if self.peek_ident() == Some("using") {
            let _ = self.eat_ident();
            self.skip_newlines();
            let Some(name) = self.eat_ident() else {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0814","Expected a name after 'using'","Write: judge_all status using Status  or  judge_all using score"));
            };
            let is_cap = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
            if is_cap { using_enum = Some(name); } else { using_expr = Some(Box::new(PExpr::Ident(name))); }
        }

        if self.peek_is_return() {
            let _ = self.eat_ident();
            self.skip_newlines();
            let pe = self.parse_assign()?;
            let sp = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
            let expr = Self::lower_expr_preview(pe, sp.clone());
            let ret  = ast::Stmt::Return(ast::ReturnStmt { values: vec![expr], span: sp.clone() });
            default_body = Some(ast::JudgeArmBody::Stmts(vec![ret]));
        }

        if self.peek_op("{") {
            self.suspend_colon_call -= 1;
            return Err(s_help_site!("P0813","Don't put '{' after 'judge_all'","Use indentation and close with 'end' or 'xx'."));
        }
        self.forbid_next_line_brace(header_line, header_col, "judge_all")?;

        self.skip_newlines();
        while let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Indent) { self.i += 1; } else { break; } }

        let mut arms: Vec<ast::JudgeArmStmt> = Vec::new();

        loop {
            self.skip_newlines();

            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Ident if t.value.as_deref() == Some("end") && t.span.col_start == header_col => break,
                    TokenKind::Op(op) if op == "xx" && t.span.col_start == header_col => break,
                    _ => {}
                }
            } else {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212","This judge_all block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge all stmt]"));
            }

            while let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Indent) { self.i += 1; } else { break; } }
            if let Some(t) = self.peek() { if matches!(t.kind, TokenKind::Dedent) { break; } }
            if self.is_eof() {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212","This judge_all block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge all stmt]"));
            }

            let arm_col: u32 = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(header_col);

            let is_else = if let Some(tok) = self.peek() {
                matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("else")
            } else { false };

            let cond_opt = if is_else {
                let _ = self.eat_ident();
                None
            } else {
                let raw = self.parse_judge_condition()?;
                let expanded = if let Some(ref subj) = using_expr { Self::expand_condition(subj, &raw, &using_enum) } else { raw };
                let cond_span = Self::span_from_tokens(self.toks, self.i.saturating_sub(1), self.i.saturating_sub(1));
                Some(Box::new(Self::lower_expr_preview(expanded, cond_span)))
            };

            if !self.eat_op(":") {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0811","You need ':' after the condition in 'judge_all'","Write it like: judge_all x > 5: say \"big\""));
            }

            let body = if self.peek_newline_or_eof() {
                self.skip_newlines();
                match self.peek() {
                    Some(t) if matches!(t.kind, TokenKind::Indent) => {
                        self.i += 1;
                        let stmts = self.parse_stmt_block_until_dedent_or_next_case(arm_col)?;
                        JudgeArmBody::Stmts(stmts)
                    }
                    _ => {
                        if let Some(db) = default_body.clone() { db } else {
                            return Err(s_help_site!("P0816","Expected an indented block after ':' in judge_all arm","Start the arm body on the next line and indent it."));
                        }
                    }
                }
            } else {
                let stmt = self.parse_stmt()?;
                self.eat_semi_separators();
                JudgeArmBody::Stmts(vec![stmt])
            };

            let arm_span = self.toks.get(self.i.saturating_sub(1)).map(|t| t.span.clone())
                .unwrap_or_else(|| self.toks[header_tok_i].span.clone());

            arms.push(JudgeArmStmt { condition: cond_opt, body, span: arm_span });
            self.skip_newlines();
        }

        self.suspend_colon_call -= 1;

        while let Some(t) = self.peek() {
            use goblin_lexer::TokenKind::*;
            if matches!(t.kind, Dedent | Newline) { self.i += 1; } else { break; }
        }

        if self.peek_block_close() {
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if col != header_col {
                let closer = self.peek_ident().unwrap_or("}");
                return Err(s_help_site!("P0222",&format!("This '{}' closer is misaligned: expected column {}, found {}", closer, header_col, col),"Align the closer with its header."));
            }
            self.expect_block_close("judge_all")?;
        } else if !self.eat_layout_until_close(header_col) {
            return Err(s_help_site!("P0212","This judge_all block is missing its closing 'end' or 'xx' (crossbones).","Close the block. [parse judge all stmt]"));
        }

        let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
        Ok(Stmt::JudgeAll(JudgeAllStmt { arms, span }))
    }

    fn parse_kv_bind_list_judge(
        &mut self,
        hdr_col: u32,
        allow_empty: bool,
    ) -> Result<Vec<(PExpr, Option<PExpr>)>, String> {
        use goblin_lexer::TokenKind;
        let mut out: Vec<(PExpr, Option<PExpr>)> = Vec::new();

        loop {
            // Only skip newlines here; do NOT eat layout-to-close.
            self.skip_newlines();

            // If we are at an aligned closer for the whole judge block, leave it to the caller.
            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Ident
                        if t.value.as_deref() == Some("end") && t.span.col_start == hdr_col => break,
                    TokenKind::Op(op) if op == "xx" && t.span.col_start == hdr_col => break,
                    _ => {}
                }
            } else {
                return Err(s_help_site!(
                    "P0212",
                    "This judge block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones). [parse kv bind list judge]",
                ));
            }

            // Consume any Indent tokens to get to the arm start ('==', 'else', or explicit expr).
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Indent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // If we see a dedent that took us back to the header column, the block is done.
            if let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Dedent) {
                    // Do not consume it here; outer caller handles layout/closer.
                    break;
                }
            }

            if self.is_eof() {
                return Err(s_help_site!(
                    "P0212",
                    "This judge block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones). [parse kv bind list judge]",
                ));
            }

            // Remember the column of THIS arm so we can stop its body on dedent.
            let arm_col: u32 = self
                .toks
                .get(self.i)
                .map(|t| t.span.col_start)
                .unwrap_or(hdr_col);

            // Check for "else:" special case
            let is_else = if let Some(tok) = self.peek() {
                matches!(tok.kind, TokenKind::Ident) && tok.value.as_deref() == Some("else")
            } else {
                false
            };

            let condition = if is_else {
                let _ = self.eat_ident();
                None
            } else {
                let cond = self.parse_judge_condition()?;
                Some(cond)
            };

            if !self.eat_op(":") {
                return Err(s_help_site!(
                    "P0811",
                    "You need ':' after the condition in 'judge'",
                    "Write it like: judge x > 5: \"big\"",
                ));
            }

            // Multiline arm body? (newline + indent) → parse statements until dedent back to arm_col.
            let val_opt: Option<PExpr> = if self.peek_newline_or_eof() {
                self.skip_newlines();

                match self.peek() {
                    Some(t) if matches!(t.kind, TokenKind::Indent) => {
                        self.i += 1;
                        let val = self.parse_stmt_sequence_until_dedent_or_next_case(arm_col)?;
                        Some(val)
                    }
                    _ => {
                        if allow_empty {
                            // Header 'judge return ...' is present; bare ':' means "empty arm"
                            None
                        } else {
                            return Err(s_help_site!(
                                "P0816",
                                "Expected an indented block after ':' in judge arm",
                                "Start the arm body on the next line and indent it.",
                            ));
                        }
                    }
                }
            } else {
                // Single-line arm value (expression until newline)
                let val = self.parse_assign()?;
                Some(val)
            };

            let cond_expr = condition.unwrap_or_else(|| PExpr::Ident("else".to_string()));
            out.push((cond_expr, val_opt));

            // Prepare for next arm or the closing token; do NOT consume closers here.
            self.skip_newlines();
        }

        Ok(out)
    }

    // NEW helper method
    fn parse_stmt_sequence_until_dedent_or_next_case(&mut self, hdr_col: u32) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;
        
        let mut exprs: Vec<ast::Expr> = Vec::new();  // Changed type
        
        loop {
            // Skip newlines AND layout tokens (Indent/Dedent)
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }
            
            let current_col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            
            // Stop if we dedent back to header level or beyond
            if current_col <= hdr_col {
                break;
            }
            
            // Stop if we hit a block closer
            if self.peek_block_close() {
                break;
            }
            
            // Stop if EOF
            if self.is_eof() {
                break;
            }
            
            // Parse one statement (handles repeat, if, for, while, etc.)
            let stmt = self.parse_stmt()?;
            
            // Extract expression from statement
            let expr = match stmt {
                ast::Stmt::Expr(e) => e,
                ast::Stmt::Bind(b) => {
                    return Err(s_help_site!(
                        "P0BIND",
                        "Bind statements can't be used as expressions in judge cases",
                        "Use the statement-form judge arm (stmts body), or make the case body a real expression"
                    ));
                }
                ast::Stmt::TupleBind(tb) => {
                    return Err(s_help_site!(
                        "P0TBND",
                        "Tuple bind statements can't be used as expressions in judge cases",
                        "Use the statement-form judge arm (stmts body), or make the case body a real expression"
                    ));
                }
                ast::Stmt::Return(r) => {
                    // Convert return to FreeCall
                    let values: Vec<ast::Expr> = r.values.clone();
                    ast::Expr::FreeCall("return".to_string(), values, r.span)
                }
                _ => {
                    return Err(s_help_site!(
                        "P0815",
                        "Can't use class/action/enum declarations inside judge cases",
                        "Move declarations outside the judge block"
                    ));
                }
            };
            
            exprs.push(expr);
            self.eat_semi_separators();
        }
        
        // Wrap in Block
        Ok(PExpr::Block(exprs))
    }
    fn parse_judge_condition(&mut self) -> Result<PExpr, String> {
        
        // Check if this is a shorthand comparison operator without LHS
        if self.peek_op(">=") || self.peek_op("<=") || self.peek_op(">") || 
           self.peek_op("<") || self.peek_op("==") || self.peek_op("!=") {
            let op = if self.eat_op(">=") { ">=" }
                    else if self.eat_op("<=") { "<=" }
                    else if self.eat_op(">") { ">" }
                    else if self.eat_op("<") { "<" }
                    else if self.eat_op("==") { "==" }
                    else if self.eat_op("!=") { "!=" }
                    else { unreachable!() };
            
            self.skip_newlines();
            let rhs = self.parse_additive()?;
            
            return Ok(PExpr::Binary(
                Box::new(PExpr::Ident(String::new())),
                op.to_string(),
                Box::new(rhs)
            ));
        }
        
        // For enum matching, bare identifiers are valid (will be expanded later)
        // Just parse a normal expression
        let expr = self.parse_or()?;
        
        Ok(expr)
    }

    fn parse_sweep_stmt(&mut self, mode: ast::SweepMode) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        use ast::{SweepStmt, SweepArm, SweepArmKind, Stmt};

        let header_tok_i = self.i;
        let header_line  = self.toks[header_tok_i].span.line_start;
        let header_col   = self.toks[header_tok_i].span.col_start;

        // eat 'sweep' or 'sweep_all'
        match mode {
            ast::SweepMode::Match => { let _ = self.eat_ident(); }
            ast::SweepMode::All   => { let _ = self.eat_ident(); }
        }

        self.suspend_colon_call += 1;

        // ==== NEW: accept ANY expr list as sweep targets ====
        let targets = self.parse_sweep_target_exprs()?; 
        // =====================================================

        // no '{' after header
        if self.peek_op("{") {
            self.suspend_colon_call -= 1;
            let kw = match mode { ast::SweepMode::Match => "sweep", _ => "sweep_all" };
            return Err(s_help_site!("P09S1",
                &format!("Don't put '{{' after '{}'", kw),
                "Use indentation and close with 'end' or 'xx'." ));
        }
        self.forbid_next_line_brace(
            header_line,
            header_col,
            match mode { ast::SweepMode::Match => "sweep", _ => "sweep_all" }
        )?;

        // ===== sweep_all special case =====
        if let ast::SweepMode::All = mode {
            self.skip_newlines();
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Indent) { self.i += 1; }
                else { break; }
            }

            let body_stmts = self.parse_stmt_block_until_dedent_or_close(header_col)?;

            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != header_col {
                    let closer = self.peek_ident().unwrap_or("}");
                    return Err(s_help_site!("P0222",
                        &format!("This '{}' closer is misaligned: expected column {}, found {}",
                        closer, header_col, col),
                        "Align the closer with its header." ));
                }
                self.expect_block_close("sweep_all")?;
            } else if !self.eat_layout_until_close(header_col) {
                return Err(s_help_site!("P0212",
                    "This sweep block is missing its closing 'end' or 'xx'.",
                    "Close the block. [parse sweep_all]" ));
            }

            self.suspend_colon_call -= 1;

            let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
            let arm = SweepArm {
                kind:   SweepArmKind::AllBody,
                repeat: SweepArmRepeat::All,   // default behavior
                body:   body_stmts,
                span:   span.clone(),
            };
            return Ok(Stmt::Sweep(SweepStmt { mode, targets, arms: vec![arm], span }));
        }

        // ===== Normal sweep with arms =====
        self.skip_newlines();
        while let Some(t) = self.peek() {
            if matches!(t.kind, TokenKind::Indent) { self.i += 1; }
            else { break; }
        }

        let mut arms: Vec<SweepArm> = Vec::new();

        loop {
            self.skip_newlines();

            // end / xx closes block
            if let Some(t) = self.peek() {
                match &t.kind {
                    TokenKind::Ident if t.value.as_deref() == Some("end")
                        && t.span.col_start == header_col => break,
                    TokenKind::Op(op) if op == "xx"
                        && t.span.col_start == header_col => break,
                    _ => {}
                }
            } else {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212",
                    "This sweep block is missing its closing 'end' or 'xx'.",
                    "Close the block. [parse sweep stmt]" ));
            }

            // Indent handling
            while let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Indent) { self.i += 1; }
                else { break; }
            }
            if let Some(t) = self.peek() {
                if matches!(t.kind, TokenKind::Dedent) { break; }
            }
            if self.is_eof() {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P0212",
                    "This sweep block is missing its closing 'end' or 'xx'.",
                    "Close the block. [parse sweep stmt]" ));
            }

            let arm_col =
                self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(header_col);

            let (kind, repeat) = self.parse_sweep_arm_header()?;
            if !self.eat_op(":") {
                self.suspend_colon_call -= 1;
                return Err(s_help_site!("P09S2",
                    "You need ':' after a sweep arm header",
                    "Examples: \"<h1>\" ... \"</h1>\" :  or  \"needle\" :" ));
            }

            let body_stmts =
                if self.peek_newline_or_eof() {
                    self.skip_newlines();
                    match self.peek() {
                        Some(t) if matches!(t.kind, TokenKind::Indent) => {
                            self.i += 1;
                            self.parse_stmt_block_until_dedent_or_next_arm(arm_col)?
                        }
                        _ => return Err(s_help_site!("P09S3",
                            "Expected an indented block after ':' in sweep arm",
                            "Indent the arm body on the next line." )),
                    }
                } else {
                    let stmt = self.parse_stmt()?;
                    self.eat_semi_separators();
                    vec![stmt]
                };

            let arm_span =
                self.toks.get(self.i.saturating_sub(1))
                .map(|t| t.span.clone())
                .unwrap_or_else(|| self.toks[header_tok_i].span.clone());

            arms.push(SweepArm {
                kind,
                repeat,
                body:   body_stmts,
                span:   arm_span,
            });

            self.skip_newlines();
        }

        self.suspend_colon_call -= 1;

        while let Some(t) = self.peek() {
            use goblin_lexer::TokenKind::*;
            if matches!(t.kind, Dedent | Newline) { self.i += 1; }
            else { break; }
        }

        if self.peek_block_close() {
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if col != header_col {
                let closer = self.peek_ident().unwrap_or("}");
                return Err(s_help_site!("P0222",
                    &format!("This '{}' closer is misaligned: expected column {}, found {}",
                    closer, header_col, col),
                    "Align the closer with its header." ));
            }
            self.expect_block_close("sweep")?;
        } else if !self.eat_layout_until_close(header_col) {
            return Err(s_help_site!("P0212",
                "This sweep block is missing its closing 'end' or 'xx'.",
                "Close the block. [parse sweep stmt]" ));
        }

        let span = Self::span_from_tokens(self.toks, header_tok_i, self.i.saturating_sub(1));
        Ok(Stmt::Sweep(SweepStmt { mode, targets, arms, span }))
    }

    /// NEW: allow arbitrary expressions as sweep targets
    fn parse_sweep_target_exprs(&mut self) -> Result<Vec<ast::Expr>, String> {
        let mut out = Vec::new();

        loop {
            // -------- FIX: NO `?` HERE --------
            let expr = match self.parse_expr() {
                Ok(e) => e,
                Err(diags) => {
                    // Collapse diagnostics into a single parser error String
                    let msg = diags.iter()
                        .map(|d| d.to_string())
                        .collect::<Vec<_>>()
                        .join("\n");
                    return Err(msg);
                }
            };
            // ----------------------------------

            out.push(expr);

            self.skip_newlines();
            if self.eat_op(",") {
                self.skip_newlines();
                continue;
            }
            break;
        }

        Ok(out)
    }

    // "<str>" ":"   |   "<str>" "..." "<str>" ":"
    // [first|last] "<str>" ":"   |   [first|last] "<str>" "..." "<str>" ":"
    fn parse_sweep_arm_header(&mut self) -> Result<(ast::SweepArmKind, ast::SweepArmRepeat), String> {
        use goblin_lexer::TokenKind;
        use ast::{SweepArmKind, SweepArmRepeat};

        // Optional leading repeat modifier: first / last
        let mut repeat = SweepArmRepeat::All;

        if let Some(t) = self.peek() {
            if let TokenKind::Ident = t.kind {
                if let Some(name) = t.value.as_deref() {
                    if name == "first" {
                        repeat = SweepArmRepeat::First;
                        self.i += 1;        // consume 'first'
                        self.skip_newlines();
                    } else if name == "last" {
                        repeat = SweepArmRepeat::Last;
                        self.i += 1;        // consume 'last'
                        self.skip_newlines();
                    }
                }
            }
        }

        // Now we require a string literal
        let start = self.eat_string_lit()
            .ok_or_else(|| s_help_site!(
                "P09A1",
                "Expected a string literal at the start of a sweep arm",
                "Examples: \"<h1>\" ... \"</h1>\" :  or  \"needle\" :"
            ))?;

        if self.peek_op("...") {
            let _ = self.eat_op("...");
            let end = self.eat_string_lit()
                .ok_or_else(|| s_help_site!(
                    "P09A2",
                    "Expected a string literal after '...' in sweep arm",
                    "Write: \"<a>\" ... \"</a>\" :"
                ))?;
            Ok((
                SweepArmKind::Range { start, end },
                repeat,
            ))
        } else {
            Ok((
                SweepArmKind::Pattern(start),
                repeat,
            ))
        }
    }

    // Arm body until dedent to arm_col OR aligned closer/next arm
    fn parse_stmt_block_until_dedent_or_next_arm(&mut self, arm_col: u32) -> Result<Vec<ast::Stmt>, String> {
        use goblin_lexer::TokenKind;
        let mut stmts = Vec::new();

        loop {
            // swallow stray layout between statements
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // stop if dedented back to arm_col or we hit a block closer
            let current_col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if current_col <= arm_col || self.peek_block_close() || self.is_eof() {
                break;
            }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.eat_semi_separators();
        }

        Ok(stmts)
    }

    fn parse_stmt_block_until_dedent_or_close(&mut self, header_col: u32) -> Result<Vec<ast::Stmt>, String> {
        use goblin_lexer::TokenKind;
        let mut stmts = Vec::new();

        loop {
            // swallow stray layout
            while let Some(tok) = self.peek() {
                if matches!(tok.kind, TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent) {
                    self.i += 1;
                } else {
                    break;
                }
            }

            // stop if dedented to or before header_col (i.e., out of the block) or aligned closer/end-of-file
            let current_col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if current_col <= header_col || self.peek_block_close() || self.is_eof() {
                break;
            }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.eat_semi_separators();
        }

        Ok(stmts)
    }

    fn parse_unary(&mut self) -> Result<PExpr, String> {
            // use goblin_lexer::TokenKind;

            // unary definedness: &LValue (right-assoc, same tier as other prefix ops)
            if self.eat_op("&") {
                // allow newline(s) after '&'
                self.skip_newlines();
                let i0 = self.i;
                match self.parse_definedness_lvalue() {
                    Ok(lv) => return Ok(PExpr::IsBound(Box::new(lv))),
                    Err(_) => {
                        // Don't loop forever if nothing consumed after '&'
                        if self.i == i0 { /* we already ate '&' */ }
                        return Err(s_help_site!(
                            "P0401",
                            "You need a variable, field access, or array index after '&'",
                            "Examples: &user, &user>>name, &items[0]"
                        ));
                    }
                }
            }

            // collect <count> of <expr>
            if self.peek_ident() == Some("collect") {
                let start_i = self.i;
                let _ = self.eat_ident(); // consume 'collect'
                self.skip_layout();

                let count = self.parse_primary()?;
                self.skip_layout();

                if self.peek_ident().as_deref() != Some("of") {
                    return Err(s_help_site!(
                        "P0340",
                        "Expected 'of' after count in collect",
                        "Write: collect 4 of goblin_ipsum_sentence()"
                    ));
                }
                let _ = self.eat_ident(); // consume 'of'
                self.skip_layout();

                let body = self.parse_coalesce()?;
                let span = Self::span_from_tokens(self.toks, start_i, self.i.saturating_sub(1));

                return Ok(PExpr::FreeCall(
                    "collect".to_string(),
                    vec![count, body],
                ));
            }

            // PICK / REAP (expression form)
            // Syntax:
            //   pick <count:int | {var} | var> [_ <digits:int>]   // also supports packed shorthand: pick 100_8
            //        [from <expr or range>]
            //        [with dups | without dups | wo dups]
            //        [unique]   // for digit shorthand (x_y): each number’s digits must be distinct
            //
            //   reap <count:int | {var} | var> from <expr or range>
            //   // Note: reap does NOT support digit shorthand or dups modifiers.
            if self.peek_ident() == Some("pick") || self.peek_ident() == Some("reap") || self.peek_ident() == Some("secure_pick") {
                let verb = self.peek_ident().unwrap().to_string(); // "pick" or "reap"
                let _ = self.eat_ident(); // consume verb
                self.skip_layout();

                // <count>: integer literal (lexer may give "1_6" as a single Int token)
                // OR default to 1 if 'from' immediately follows (sugar: `pick from xs`)
                // OR dynamic count via `{var}` or bare identifier `var`
                let mut count_txt = String::new();
                let mut count: i128;
                // <count>: int | {var} | var | defaults to 1 if 'from' follows
                let mut count_expr: Option<PExpr> = None;

                // braced dynamic: pick {var} ...
                if self.eat_op("{") {
                    self.skip_layout();

                    // accept a bare identifier inside braces
                    if let Some(name_owned) = self.peek_ident().map(|s| s.to_string()) {
                        if name_owned == "from" {
                            return Err(s_help_site!("P1407","Expected a variable name inside '{}'","Use: pick {count} from items"));
                        }
                        let _ = self.eat_ident(); // consume the ident
                        count_expr = Some(PExpr::Ident(name_owned));
                    } else {
                        return Err(s_help_site!("P1407","Expected a variable name inside '{}'","Use: pick {count} from items"));
                    }

                    self.skip_layout();
                    // require closing '}'
                    if !self.eat_op("}") {
                        return Err(s_help_site!(
                            "P1402",
                            "Unclosed '{' in variable reference",
                            "Close the variable reference with '}'"
                        ));
                    }

                    // harmless default for any static-only checks later
                    count = 1;
                    self.skip_layout();

                } else if let Some(name_owned) = self
                    .peek_ident()
                    .filter(|&n| n != "from")
                    .map(|s| s.to_string())
                {
                    let _ = self.eat_ident(); // consume it
                    count_expr = Some(PExpr::Ident(name_owned));
                    count = 1;
                    self.skip_layout();

                } else {
                    // static numeric / sugar / error
                    match self.peek() {
                        // Normal numeric form: pick 3 from items
                        Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Int) => {
                            count_txt = t.value.clone().unwrap_or_default();
                            if let Some(v) = parse_int_literal_to_i128(&count_txt) {
                                self.i += 1;
                                count = v;
                            } else {
                                return Err(s_help_site!(
                                    "P1401",
                                    &format!("You need a number after '{}'", verb),
                                    &format!("Write it like: {} 5 from items", verb),
                                ));
                            }
                            self.skip_layout();
                        }

                        // sugar: pick from xs -> defaults to 1
                        _ if self.peek_ident() == Some("from") => {
                            count = 1;
                        }

                        // Otherwise, still error
                        _ => {
                            return Err(s_help_site!(
                                "P1401",
                                &format!("You need a number after '{}'", verb),
                                &format!("Write it like: {} 5 from items", verb),
                            ));
                        }
                    }
                }

                // No negatives allowed (only meaningful for static counts)
                if count_expr.is_none() && count < 0 {
                    return Err(s_help_site!(
                        "P1402",
                        &format!("You can't {} a negative number of items", verb),
                        &format!("Use a positive number: {} 3 from items", verb),
                    ));
                }

                self.skip_layout();

                // Digit shorthand is ONLY for 'pick', never for 'reap'
                // And ONLY when count is static (not dynamic)
                let mut digits_expr: Option<PExpr> = None;
                if (verb == "pick" || verb == "secure_pick") && count_expr.is_none() {
                    // (a) packed detection — only when the raw count text contains exactly one underscore
                    if let Some(udx) = count_txt.find('_') {
                        let (lhs, rhs) = (&count_txt[..udx], &count_txt[udx + 1..]);
                        let is_digits = |s: &str| !s.is_empty() && s.chars().all(|c| c.is_ascii_digit());
                        if is_digits(lhs) && is_digits(rhs) {
                            if let Ok(rhs_val) = rhs.parse::<i32>() {
                                if (1..=18).contains(&rhs_val) {
                                    if let Ok(lhs_val) = lhs.parse::<i128>() {
                                        count = lhs_val;
                                        digits_expr = Some(PExpr::Int(rhs.to_string()));
                                    }
                                }
                            }
                        }
                    }

                    // (b) separate form: pick N _ D
                    if digits_expr.is_none() && self.eat_op("_") {
                        match self.peek() {
                            Some(t2) if matches!(t2.kind, goblin_lexer::TokenKind::Int) => {
                                let s2 = t2.value.clone().unwrap_or_default();
                                self.i += 1;
                                digits_expr = Some(PExpr::Int(s2));
                            }
                            _ => return Err(s_help_site!("P1401","Expected digits after '_'","Example: pick 5_4")),
                        }
                    }
                    self.skip_layout();
                }

                // Source: required for both verbs (if digits were parsed, 'pick' makes it optional)
                let mut src_expr: Option<PExpr> = None;
                if digits_expr.is_none() {
                    if self.peek_ident() != Some("from") {
                        return Err(s_help_site!(
                            "P1403",
                            &format!("Expected 'from' after the {} count", verb),
                            &format!("Write it like: {} 3 from items", verb),
                        ));
                    }
                    let _ = self.eat_ident(); // 'from'
                    self.skip_layout();                    // <-- CHANGED (was skip_newlines)
                    let parsed_src = self.parse_range()?; // handles ".." and "..."
                    src_expr = Some(parsed_src);
                    self.skip_layout();                    // <-- CHANGED (was skip_newlines)
                } else if self.peek_ident() == Some("from") {
                    // optional range filter for digits (pick only)
                    let _ = self.eat_ident();
                    self.skip_layout();                    // <-- CHANGED (was skip_newlines)
                    let parsed_src = self.parse_range()?;
                    src_expr = Some(parsed_src);
                    self.skip_layout();                    // <-- CHANGED (was skip_newlines)
                }

                // Modifiers: ONLY allowed for 'pick'
                let mut allow_dups: Option<bool> = None;
                let mut unique_digits = false;

                if verb == "pick" || verb == "secure_pick" {
                    loop {
                        self.skip_layout();                // <-- CHANGED (was skip_newlines)

                        // with dups
                        if self.peek_ident() == Some("with") {
                            let _ = self.eat_ident();
                            self.skip_layout();            // <-- CHANGED (was skip_newlines)
                            if self.peek_ident() == Some("dups") {
                                let _ = self.eat_ident();
                                allow_dups = Some(true);
                                continue;
                            } else {
                                return Err(s_help_site!("P1404","Expected 'dups' after 'with'","Use: with dups"));
                            }
                        }

                        // without dups / wo dups
                        if self.peek_ident() == Some("without") || self.peek_ident() == Some("wo") {
                            let _ = self.eat_ident();
                            self.skip_layout();            // <-- CHANGED (was skip_newlines)
                            if self.peek_ident() == Some("dups") {
                                let _ = self.eat_ident();
                                allow_dups = Some(false);
                                continue;
                            } else {
                                return Err(s_help_site!("P1406","Expected 'dups' after 'without'/'wo'","Use: without dups"));
                            }
                        }

                        // unique (digit-uniqueness for x_y)
                        if self.peek_ident() == Some("unique") {
                            let _ = self.eat_ident();
                            unique_digits = true;
                            continue;
                        }

                        break;
                    }
                }

                // Compile-time sanity check (same as before) — only for 'pick' with static count
                if (verb == "pick" || verb == "secure_pick") && allow_dups != Some(true) && count_expr.is_none() {
                    if let Some(PExpr::Array(ref elems)) = src_expr {
                        use std::collections::HashSet;
                        let mut set: HashSet<String> = HashSet::new();
                        let mut all_simple = true;
                        for e in elems {
                            match e {
                                PExpr::Int(s) | PExpr::Float(s) | PExpr::Str(s) => {
                                    let mut norm = String::with_capacity(s.len());
                                    for ch in s.chars() { if ch != '_' { norm.push(ch); } }
                                    set.insert(norm);
                                }
                                PExpr::Bool(b) => { set.insert(format!("b{}", b)); }
                                _ => { all_simple = false; break; }
                            }
                        }
                        if all_simple {
                            let distinct = set.len() as i128;
                            if count > distinct {
                                return Err(s_help_site!(
                                    "P1405",
                                    &format!("You're trying to {} {} items, but there are only {} distinct items available", verb, count, distinct),
                                    "Add 'with dups' to allow repeats, or pick fewer items",
                                ));
                            }
                        }
                    }
                }

                // ---- Lower to FreeCall(verb, [ {config} ]) ----
                let mut props: Vec<(String, PExpr)> = Vec::new();

                // choose dynamic or static count
                if let Some(expr) = count_expr {
                    props.push(("count_expr".into(), expr));
                } else {
                    props.push(("count".into(),  PExpr::Int(count.to_string())));
                }

                if let Some(s) = src_expr {
                    match &s {
                        PExpr::Binary(lhs, op, rhs) if op == ".." || op == "..." => {
                            props.push(("range_start".into(), (*lhs.clone())));
                            props.push(("range_end".into(),   (*rhs.clone())));
                            props.push(("range_inclusive".into(), PExpr::Bool(op == "...")));
                        }
                        _ => { props.push(("src".into(), s)); }
                    }
                }
                if verb == "pick" || verb == "secure_pick" {
                    if let Some(b) = allow_dups { props.push(("allow_dups".into(), PExpr::Bool(b))); }
                    if unique_digits { props.push(("unique".into(), PExpr::Bool(true))); }
                    if let Some(d) = digits_expr { props.push(("digits".into(), d)); }
                }

                let cfg = PExpr::Object(props);
                return Ok(self.apply_postfix_ops(PExpr::FreeCall(verb, vec![cfg])));
            }

            // ---- roll / roll_detail (syntax-only; contiguous dice + extras) ----
            if let Some(id0) = self.peek_ident() {
                if id0 == "roll" || id0 == "roll_detail" {
                    use goblin_lexer::TokenKind;

                    let is_detail = id0 == "roll_detail";
                    let _ = self.eat_ident(); // 'roll' | 'roll_detail'
                    self.skip_newlines();

                    #[inline]
                    fn contiguous(a: &Span, b: &Span) -> bool {
                        b.line_start == a.line_end && b.col_start == a.col_end
                    }

                    // collected pieces
                    let mut count_str = String::new();
                    let mut sides_str = String::new();
                    let mut mod_str   = String::from("0");
                    let mut last_span: Option<Span> = None;

                    // extras
                    let mut keep_high: Option<String> = None; // kN
                    let mut drop_low:  Option<String> = None; // dN
                    let mut reroll_eq: Option<String> = None; // rN
                    let mut explode:   bool = false;          // !
                    let mut adv:       bool = false;          // +adv
                    let mut dis:       bool = false;          // +dis
                    let mut clamp_min: Option<PExpr> = None;  // clamp A..B / A...B
                    let mut clamp_max: Option<PExpr> = None;

                    // helper to parse compact suffix trail inside a single token, e.g. "k3r1!d2"
                    let parse_trail = |raw: &str,
                                           keep_high: &mut Option<String>,
                                           drop_low:  &mut Option<String>,
                                           reroll_eq: &mut Option<String>,
                                           explode:   &mut bool| -> Result<(), String> {
                        let bytes = raw.as_bytes();
                        let mut i = 0usize;
                        while i < bytes.len() {
                            let ch = bytes[i] as char;
                            match ch {
                                '!' => { *explode = true; i += 1; }
                                'k' | 'x' | 'r' => {
                                    i += 1;
                                    let start = i;
                                    while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
                                    if start == i {
                                        return Err(s_help_site!("P1505","Expected digits after suffix","Examples: k3, x1, r1"));
                                    }
                                    let num = &raw[start..i];
                                    match ch {
                                        'k' => *keep_high = Some(num.to_string()),
                                        'x' => *drop_low  = Some(num.to_string()),
                                        'r' => *reroll_eq = Some(num.to_string()),
                                        _ => {}
                                    }
                                }
                                _ => {
                                    return Err(s_help_site!("P1505","Bad dice suffix","Use kN / dN / rN / !"));
                                }
                            }
                        }
                        Ok(())
                    };

                    // ---- first token after 'roll'
                    let t0 = match self.peek().cloned() {
                        Some(t) => t,
                        None => {
                            return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                        }
                    };

                    // ---------- Case A: Int("N") then contiguous die part ----------
                    if matches!(t0.kind, TokenKind::Int) {
                        let nstr = t0.value.clone().unwrap_or_default();
                        self.i += 1; // eat Int("N")
                        count_str = nstr;
                        last_span = Some(t0.span.clone());

                        let t1 = self.peek().cloned().ok_or_else(|| {
                            s_help_site!("P1502","Dice notation must be contiguous","Write it like 2d6, not 2 d 6")
                        })?;
                        if !contiguous(last_span.as_ref().unwrap(), &t1.span) {
                            return Err(s_help_site!("P1502","Dice notation must be contiguous","Write it like 2d6, not 2 d 6"));
                        }

                        match (t1.kind, t1.value.clone()) {
                            // Int + Ident("dM...")  (e.g. d6, d6k3, d6r1!, etc)
                            (TokenKind::Ident, Some(s)) => {
                                if s.len() > 1 && s.starts_with('d') {
                                    let mut j = 1;
                                    while j < s.len() && s.as_bytes()[j].is_ascii_digit() { j += 1; }
                                    sides_str = s[1..j].to_string();
                                    last_span = Some(t1.span.clone());
                                    let trail = &s[j..];
                                    if !trail.is_empty() {
                                        parse_trail(trail, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                    }
                                    self.i += 1; // consume t1
                                } else {
                                    return Err(s_help_site!("P1502","Dice notation must be contiguous.","Write it like 2d6, not 2 d 6."));
                                }
                            }
                            // Int + Op(unit,"dM...")  (same, but unit token)
                            (TokenKind::Op(op), Some(val)) => {
                                let op_s = op.as_str();
                                if op_s == "unit" && val.len() > 1 && val.starts_with('d') {
                                    let mut j = 1;
                                    while j < val.len() && val.as_bytes()[j].is_ascii_digit() { j += 1; }
                                    sides_str = val[1..j].to_string();
                                    last_span = Some(t1.span.clone());
                                    let trail = &val[j..];
                                    if !trail.is_empty() {
                                        parse_trail(trail, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                    }
                                    self.i += 1; // consume t1
                                } else if op_s == "unit" && val == "d" {
                                    // Int + Op(unit,"d") + contiguous Int("M")
                                    let d_span = t1.span.clone();
                                    self.i += 1; // eat unit("d")
                                    let t2 = self.peek().cloned().ok_or_else(|| {
                                        s_help_site!("P1502","Dice notation must be contiguous.","Write it like 2d6, not 2 d 6.")
                                    })?;
                                    if !matches!(t2.kind, TokenKind::Int) || !contiguous(&d_span, &t2.span) {
                                        return Err(s_help_site!("P1502","Dice notation must be contiguous.","Write it like 2d6, not 2 d 6."));
                                    }
                                    self.i += 1; // eat Int("M")
                                    sides_str = t2.value.unwrap_or_default();
                                    last_span = Some(t2.span.clone());
                                } else {
                                    return Err(s_help_site!("P1502","Dice notation must be contiguous.","Write it like 2d6, not 2 d 6."));
                                }
                            }
                            _ => {
                                return Err(s_help_site!("P1502","Dice notation must be contiguous.","Write it like 2d6, not 2 d 6."));
                            }
                        }
                    }
                    // ---------- Case B: Duration("Nd") + contiguous Int("M") ----------
                    else if matches!(t0.kind, TokenKind::Duration) {
                        let dur = t0.value.clone().unwrap_or_default(); // "Nd"
                        let (base, unit) = Self::split_duration_lexeme(&dur).map_err(|_| {
                            s_help_site!("P1501","Bad duration token where dice were expected","Example: roll 2d6")
                        })?;
                        if unit != "d" {
                            return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                        }
                        self.i += 1; // eat Duration("Nd")
                        count_str = base;
                        last_span = Some(t0.span.clone());

                        let t1 = self.peek().cloned().ok_or_else(|| {
                            s_help_site!("P1502","Dice notation must be contiguous","Write it like 2d6, not 2 d 6")
                        })?;
                        if !matches!(t1.kind, TokenKind::Int) || !contiguous(last_span.as_ref().unwrap(), &t1.span) {
                            return Err(s_help_site!("P1502","Dice notation must be contiguous","Write it like 2d6, not 2 d 6"));
                        }
                        self.i += 1; // Int("M")
                        sides_str = t1.value.unwrap_or_default();
                        last_span = Some(t1.span.clone());
                    }
                    // ---------- Case C: fused Ident/Op(unit) "NdM..." ----------
                    else {
                        match (t0.kind, t0.value.clone()) {
                            (TokenKind::Ident, Some(raw)) => {
                                if let Some(dpos) = raw.find('d') {
                                    let (lhs, rhs) = raw.split_at(dpos);
                                    if !lhs.is_empty()
                                        && lhs.chars().all(|c| c.is_ascii_digit())
                                        && rhs.len() > 1
                                        && rhs[1..].chars().all(|c| c.is_ascii_digit() || c=='k' || c=='x' || c=='r' || c=='!')
                                    {
                                        self.i += 1;
                                        count_str = lhs.to_string();
                                        let mut j = 1;
                                        while j < rhs.len() && rhs.as_bytes()[j].is_ascii_digit() { j += 1; }
                                        sides_str = rhs[1..j].to_string();
                                        let trail = &rhs[j..];
                                        last_span = Some(t0.span.clone());
                                        if !trail.is_empty() {
                                            parse_trail(trail, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                        }
                                    } else {
                                        return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                                    }
                                } else {
                                    return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                                }
                            }
                            (TokenKind::Op(op), Some(raw)) => {
                                if op.as_str() == "unit" {
                                    if let Some(dpos) = raw.find('d') {
                                        let (lhs, rhs) = raw.split_at(dpos);
                                        if !lhs.is_empty()
                                            && lhs.chars().all(|c| c.is_ascii_digit())
                                            && rhs.len() > 1
                                            && rhs[1..].chars().all(|c| c.is_ascii_digit() || c=='k' || c=='x' || c=='r' || c=='!')
                                        {
                                            self.i += 1;
                                            count_str = lhs.to_string();
                                            let mut j = 1;
                                            while j < rhs.len() && rhs.as_bytes()[j].is_ascii_digit() { j += 1; }
                                            sides_str = rhs[1..j].to_string();
                                            let trail = &rhs[j..];
                                            last_span = Some(t0.span.clone());
                                            if !trail.is_empty() {
                                                parse_trail(trail, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                            }
                                        } else {
                                            return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                                        }
                                    } else {
                                        return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                                    }
                                } else {
                                    return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                                }
                            }
                            _ => {
                                return Err(s_help_site!("P1501","You need a number before 'd' in a roll","Example: roll 2d6"));
                            }
                        }
                    }

                    // ---- Optional contiguous numeric +Z / -Z
                    if let Some(op_tok) = self.peek().cloned() {
                        if let TokenKind::Op(s) = op_tok.kind {
                            if (s == "+" || s == "-") && contiguous(last_span.as_ref().unwrap(), &op_tok.span) {
                                let sign = if s == "-" { -1i32 } else { 1i32 };
                                self.i += 1; // '+'|'-'
                                let n_tok = self.peek().cloned().ok_or_else(|| {
                                    s_help_site!("P1504","You need a number after '+' or '-' in a dice notation roll","Examples: 2d6+3 or 1d20-1")
                                })?;
                                if !matches!(n_tok.kind, TokenKind::Int) || !contiguous(&op_tok.span, &n_tok.span) {
                                    return Err(s_help_site!("P1504","You need a number after '+' or '-' in a dice notation roll","Examples: 2d6+3 or 1d20-1"));
                                }
                                self.i += 1; // Int
                                let n_raw = n_tok.value.unwrap_or_default();
                                mod_str = if sign < 0 { format!("-{}", n_raw) } else { n_raw };
                                last_span = Some(n_tok.span.clone());
                            }
                        }
                    }

                    // ---- Optional +adv / +dis (NOT required to be contiguous)
                    self.skip_newlines();
                    if self.peek_op("+") {
                        let _ = self.eat_op("+");
                        self.skip_newlines();
                        match self.peek_ident() {
                            Some("adv") => { let _ = self.eat_ident(); adv = true; }
                            Some("dis") => { let _ = self.eat_ident(); dis = true; }
                            _ => return Err(s_help_site!("P1506","Expected 'adv' or 'dis' after '+'","Use: roll 1d20 +adv")),
                        }
                    }

                    // ---- Optional: consume further contiguous suffix tokens ('!' or compact "k3r1")
                    loop {
                        let t = match self.peek().cloned() { Some(t) => t, None => break };
                        if !contiguous(last_span.as_ref().unwrap(), &t.span) { break; }
                        match (t.kind, t.value.clone()) {
                            // match only the bang op here
                            (TokenKind::Op(s), _) if s == "!" => {
                                self.i += 1;
                                explode = true;
                                last_span = Some(t.span.clone());
                                continue;
                            },
                            // keep your existing 'unit' trailer arm as-is
                            (TokenKind::Op(op), Some(raw)) => {
                                if op.as_str() == "unit" {
                                    parse_trail(&raw, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                    self.i += 1;
                                    last_span = Some(t.span.clone());
                                    continue;
                                } else {
                                    break;
                                }
                            },
                            (TokenKind::Ident, Some(raw)) => {
                                parse_trail(&raw, &mut keep_high, &mut drop_low, &mut reroll_eq, &mut explode)?;
                                self.i += 1;
                                last_span = Some(t.span.clone());
                                continue;
                            }
                            _ => break,
                        }
                    }

                    // ---- Optional: clamp A..B / A...B (not required to be contiguous)
                    self.skip_newlines();
                    if self.peek_ident() == Some("clamp") {
                        let _ = self.eat_ident(); // 'clamp'
                        self.skip_newlines();
                        let rng = self.parse_range()?;
                        if let PExpr::Binary(lhs, op, rhs) = rng {
                            if op == ".." || op == "..." {
                                clamp_min = Some(*lhs);
                                clamp_max = Some(*rhs);
                            } else {
                                return Err(s_help_site!("P1507","Expected a range after 'clamp'","Use: clamp 3..18"));
                            }
                        } else {
                            return Err(s_help_site!("P1507","Expected a range after 'clamp'","Use: clamp 3..18"));
                        }
                    }

                    // ---- Build config object -> FreeCall
                    if adv && dis {
                        return Err(s_help_site!("P1508","You can't use both adv and dis","Use only one of +adv or +dis"));
                    }
                    if keep_high.is_some() && drop_low.is_some() {
                        return Err(s_help_site!("P1509","You can't combine kN and dN","Use only one of kN or dN"));
                    }

                    let mut props: Vec<(String, PExpr)> = vec![
                        ("count".into(),    PExpr::Int(count_str)),
                        ("sides".into(),    PExpr::Int(sides_str)),
                        ("modifier".into(), PExpr::Int(mod_str)),
                    ];
                    if let Some(k) = keep_high { props.push(("keep_high".into(), PExpr::Int(k))); }
                    if let Some(d) = drop_low  { props.push(("drop_low".into(),  PExpr::Int(d))); }
                    if let Some(r) = reroll_eq { props.push(("reroll_eq".into(), PExpr::Int(r))); }
                    if explode { props.push(("explode".into(), PExpr::Bool(true))); }
                    if adv { props.push(("adv".into(), PExpr::Bool(true))); }
                    if dis { props.push(("dis".into(), PExpr::Bool(true))); }
                    if let (Some(lo), Some(hi)) = (clamp_min, clamp_max) {
                        props.push(("clamp_min".into(), lo));
                        props.push(("clamp_max".into(), hi));
                    }

                    let cfg = PExpr::Object(props);
                    let call = PExpr::FreeCall(
                        if is_detail { "roll_detail".to_string() } else { "roll".to_string() },
                        vec![cfg],
                    );
                    return Ok(self.apply_postfix_ops(call));
                }
            }

            // logical-not
            if self.eat_op("!") {
                let rhs = self.with_depth(|p| p.parse_unary())?;
                return Ok(PExpr::Prefix("!".into(), Box::new(rhs)));
            }

            // alias: "not" keyword for logical-not
            if self.peek_ident() == Some("not") {
                let _ = self.eat_ident(); // consumed "not"
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_unary())?;
                return Ok(PExpr::Prefix("!".into(), Box::new(rhs))); // normalize to "!"
            }

            // unary +/-
            if self.eat_op("+") {
                let rhs = self.with_depth(|p| p.parse_unary())?;
                return Ok(PExpr::Prefix("+".into(), Box::new(rhs)));
            }
            if self.eat_op("-") {
                let rhs = self.with_depth(|p| p.parse_unary())?;
                return Ok(PExpr::Prefix("-".into(), Box::new(rhs)));
            }

            // hand off
            Ok(self.with_depth(|p| p.parse_postfix())?)
    }

    fn parse_postfix(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_member())?;

        // helper: does the token *after* an operator look like an expression head?
        // used to disambiguate postfix "**" / "//" from binary power/int-div.
        let lookahead_starts_expr = |from: usize| -> bool {
            use goblin_lexer::TokenKind as K;
            let mut j = from;
            while matches!(self.toks.get(j), Some(t) if matches!(t.kind, K::Newline)) { j += 1; }
            match self.toks.get(j).map(|t| &t.kind) {
                Some(K::Ident)
                | Some(K::Int) | Some(K::Float) | Some(K::String)
                | Some(K::Blob) | Some(K::Date) | Some(K::Time) | Some(K::DateTime) => true,
                Some(K::Op(s)) if s == "(" || s == "[" || s == "{" || s == "+" || s == "-" || s == "!" || s == "&" => true,
                _ => false,
            }
        };

        loop {
            let start_i = self.i; // progress snapshot

            // --- allow call continuation across newline(s) ---
            {
                let mut k = self.i;
                while matches!(self.toks.get(k), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { k += 1; }
                if k != self.i {
                    if let Some(tok) = self.toks.get(k) {
                        if tok.kind == goblin_lexer::TokenKind::Op("(".into()) {
                            self.i = k;
                        }
                    }
                }
            }

            // --- function call: lhs(args...) ---
            if self.eat_op("(") {
                // Parse comma-separated arguments; allow newlines freely.
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                let mut args: Vec<PExpr> = Vec::new();
                if !self.peek_op(")") {
                    loop {
                        let arg = self.parse_coalesce()?;
                        args.push(arg);
                        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if self.eat_op(",") {
                            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                            if self.peek_op(")") { break; } // allow trailing comma
                            continue;
                        }
                        break;
                    }
                }
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if !self.eat_op(")") { return Err(s_help_site!("P0505", "Expected ')' to close this action call", "Add the closing ')': calculate(price, tax)")); }

                lhs = PExpr::Call(Box::new(lhs), "()".to_string(), args);
                continue;
            }

            // --- indexing / slicing ---
            if self.eat_op("[") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if self.eat_op("]") { return Err(s_help_site!("P0701", "Brackets need an index or slice expression", "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]")); }

                let mut start: Option<PExpr> = None;
                if !self.peek_op(":") {
                    start = Some(self.parse_coalesce()?);
                    while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                }

                // slice?
                if self.eat_op(":") {
                    while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    let mut end: Option<PExpr> = None;
                    if !self.peek_op("]") && !self.peek_op(":") {
                        end = Some(self.parse_coalesce()?);
                        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    }

                    let mut step: Option<PExpr> = None;
                    if self.eat_op(":") {
                        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if !self.peek_op("]") {
                            step = Some(self.parse_assign()?);
                            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        }
                    }

                    if !self.eat_op("]") { return Err(s_help_site!("P0709", "Expected ']' to close this slice", "Add the closing ']': items[1:4]")); }

                    lhs = if step.is_some() {
                        PExpr::Slice3(Box::new(lhs), start.map(Box::new), end.map(Box::new), step.map(Box::new))
                    } else {
                        PExpr::Slice(Box::new(lhs), start.map(Box::new), end.map(Box::new))
                    };
                    continue;
                }

                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if !self.eat_op("]") { return Err(s_help_site!("P0702", "Expected ']' to close this index", "Add the closing ']': items[0]")); }
                let idx = start.expect("index expression parsed");
                lhs = PExpr::Index(Box::new(lhs), Box::new(idx));
                continue;
            }

            // --- postfix ops ---
            // Disambiguate "**" and "//": treat as *postfix* only when they are not followed by an expression head,
            // so "a ** b" / "a // b" remain binary at higher precedence levels.
            if self.peek_op("**") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("**"); lhs = PExpr::Postfix(Box::new(lhs), "**".to_string()); continue; }
            if self.peek_op("//") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("//"); lhs = PExpr::Postfix(Box::new(lhs), "//".to_string()); continue; }

            if self.peek_op("++") && !lookahead_starts_expr(self.i + 1) {
                self.i += 1;
                lhs = PExpr::Postfix(Box::new(lhs), "++".to_string());
                continue;
            }
            if self.eat_op("--") { lhs = PExpr::Postfix(Box::new(lhs), "--".to_string()); continue; }
            if self.eat_op("?") {
                // ? is ONLY for member access sugar: .property? -> .is_property
                match lhs {
                    PExpr::Member(obj, prop) => {
                        let new_prop = format!("is_{}", prop);
                        lhs = PExpr::Member(obj, new_prop);
                    }
                    _ => {
                        return Err(s_help_site!(
                            "P0XXX",
                            "The '?' postfix only works on member access like .string?",
                            "Use '&variable' to check if a variable is defined, or '.property?' to check type"
                        ));
                    }
                }
                continue;
            }
            if self.eat_op("!")  { lhs = PExpr::Postfix(Box::new(lhs), "!".to_string());  continue; }
            if self.eat_op("^")  { lhs = PExpr::Postfix(Box::new(lhs), "^".to_string());  continue; }
            if self.eat_op("_")  { lhs = PExpr::Postfix(Box::new(lhs), "_".to_string());  continue; }

            // NEW: dump postfix `*>>` (no disambiguation needed; it's never binary)
            if self.eat_op("*>>") {
                // Optional: consume an inline flag `show_ids`
                let mut show_ids = false;
                if let Some(tok) = self.toks.get(self.i) {
                    if let goblin_lexer::TokenKind::Ident = tok.kind {
                        if tok.value.as_deref() == Some("show_ids") {
                            self.i += 1;
                            show_ids = true;
                        }
                    }
                }
                // Represent as a generic postfix so it fits your existing evaluator infra.
                // In eval, treat op=="*>>" (and maybe with `show_ids`) specially.
                if show_ids {
                    // Encode the flag by appending suffix; evaluator can check for it.
                    lhs = PExpr::Postfix(Box::new(lhs), "*>>:show_ids".to_string());
                } else {
                    lhs = PExpr::Postfix(Box::new(lhs), "*>>".to_string());
                }
                continue;
            }

            // ---- percent family (tight) --------------------------------------------

            // `%o` — tight binary: (lhs %o rhs)
            if self.eat_op("%o") {
                self.skip_newlines();
                let rhs = self.with_depth(|p| p.parse_postfix())?; // tight binding
                lhs = PExpr::Binary(Box::new(lhs), "%o".to_string(), Box::new(rhs));
                continue;
            }

            // Try `%`
            let save_i = self.i;
            if self.eat_op("%") {
                self.skip_newlines();

                // If next token is ident 'of', parse:  ( (lhs%) of <rhs> )
                let is_of = match self.toks.get(self.i) {
                    Some(t) => match &t.kind {
                        goblin_lexer::TokenKind::Ident => t.value.as_deref() == Some("of"),
                        _ => false,
                    },
                    None => false,
                };
                if is_of {
                    let _ = self.eat_ident(); // consume 'of'
                    self.skip_newlines();
                    let rhs = self.with_depth(|p| p.parse_postfix())?; // tight
                    let pct = PExpr::Postfix(Box::new(lhs), "%".to_string());
                    lhs = PExpr::Binary(Box::new(pct), "of".to_string(), Box::new(rhs));
                    continue;
                }

                // Not `of`: decide literal vs modulo by lookahead, without holding borrows.
                let mut j = self.i;
                while matches!(self.toks.get(j).map(|t| &t.kind), Some(goblin_lexer::TokenKind::Newline)) {
                    j += 1;
                }
                let starts_expr = match self.toks.get(j) {
                    Some(t) => {
                        use goblin_lexer::TokenKind as K;
                        match &t.kind {
                            K::Int | K::Float | K::Money | K::String | K::Duration
                            | K::Date | K::Time | K::DateTime
                            | K::Ident | K::ClassIdent | K::HashIdent => true,
                            K::Op(s) if s == "(" => true,
                            _ => false,
                        }
                    }
                    None => false,
                };

                if starts_expr {
                    // treat as modulo at multiplicative tier: roll back to before '%'
                    self.i = save_i;
                } else {
                    // postfix percent literal: N%
                    lhs = PExpr::Postfix(Box::new(lhs), "%".to_string());
                    continue;
                }
            }

            // `%s` — percent-of-self postfix; desugared when used as RHS
            if self.eat_op("%s") {
                lhs = PExpr::Postfix(Box::new(lhs), "%s".to_string());
                continue;
            }

            // nothing matched; ensure progress or bail
            if self.i == start_i { break; }
        }

        Ok(lhs)
    }

    fn parse_member(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.with_depth(|p| p.parse_primary())?;

        loop {
            let start_i = self.i; // progress guard

            // ---------- Indexing / Slicing ----------
            if self.eat_op("[") {
                // suspend colon-call parsing while inside brackets (so "1:5" isn't a colon-call)
                self.suspend_colon_call += 1;

                // allow newlines
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }

                // Disallow empty brackets: a[]
                if self.peek_op("]") {
                    self.suspend_colon_call -= 1;
                    return Err(s_help_site!("P0701", "Brackets need an index or slice expression", "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]"));
                }

                // Parse start / end / step using ":" separators
                let mut start: Option<PExpr> = None;
                let mut end:   Option<PExpr> = None;
                let mut step:  Option<PExpr> = None;

                // start is present iff the next token isn't ":" or ".." or "..." (or "]" which we rejected above)
                if !self.peek_op(":") && !self.peek_op("..") && !self.peek_op("...") {
                    start = Some(self.parse_coalesce()?);
                }

                // If we see a ':' or '..' or '...', we're in slice mode; otherwise it's an index
                let is_slice = if self.eat_op("...") {
                    true
                } else if self.eat_op("..") {
                    true
                } else {
                    self.eat_op(":")
                };

                if is_slice {
                    // optional end
                    while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    if !self.peek_op(":") && !self.peek_op("]") {
                        end = Some(self.parse_coalesce()?);
                    }

                    // optional step after second ':'
                    if self.eat_op(":") {
                        while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if !self.peek_op("]") {
                            step = Some(self.parse_coalesce()?);
                        }
                    }
                }

                // Close bracket — or 2D index: grid[x, y]
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }

                // Check for comma before the close bracket: grid[x, y]
                if !is_slice && self.peek_op(",") {
                    let idx = start.ok_or_else(|| s_help_site!(
                        "P0701",
                        "Brackets need an index or slice expression",
                        "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]",
                    ))?;
                    let _ = self.eat_op(",");
                    while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    let idx2 = self.parse_coalesce()?;
                    while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    if !self.eat_op("]") {
                        self.suspend_colon_call -= 1;
                        return Err(s_help_site!(
                            "P0702",
                            "Expected ']' to close this 2D index",
                            "Write: grid[x, y] with exactly two coordinates",
                        ));
                    }
                    self.suspend_colon_call -= 1;
                    lhs = PExpr::Index2(Box::new(lhs), Box::new(idx), Box::new(idx2));
                    continue;
                }

                if !self.eat_op("]") {
                    self.suspend_colon_call -= 1;
                    return Err(s_help_site!(
                        "P0702",
                        "Expected ']' to close this index or slice",
                        "Add the closing ']': items[0] or data[1:4]",
                    ));
                }

                // leave bracket mode
                self.suspend_colon_call -= 1;

                // Build node
                lhs = if is_slice {
                    // Slice or Slice3
                    let s = start.map(Box::new);
                    let e = end.map(Box::new);
                    let p = step.map(Box::new);
                    if p.is_some() {
                        PExpr::Slice3(Box::new(lhs), s, e, p)
                    } else {
                        PExpr::Slice(Box::new(lhs), s, e)
                    }
                } else {
                    // Plain index: require an index expr (i.e., start must exist)
                    let idx = start.ok_or_else(|| s_help_site!(
                        "P0701",
                        "Brackets need an index or slice expression",
                        "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]",
                    ))?;
                    PExpr::Index(Box::new(lhs), Box::new(idx))
                };
                continue;
            }

            // ---------- map keyed lookup: map{key} ----------
            if self.eat_op("{") {
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }

                if self.peek_op("}") {
                    return Err(s_help_site!(
                        "P0710",
                        "Braces need a key expression",
                        "Write the key inside the braces: ctx{\"body\"} or ages{2}",
                    ));
                }

                let key = self.parse_coalesce()?;

                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }

                if !self.eat_op("}") {
                    return Err(s_help_site!(
                        "P0711",
                        "Expected '}' to close this map lookup",
                        "Add the closing '}': ctx{\"body\"}",
                    ));
                }

                lhs = PExpr::IndexMap(Box::new(lhs), Box::new(key));
                continue;
            }

            // ---------- member: >> name | >> "string" ----------
            if self.eat_op(">>") {
                if let Some(name) = self.eat_ident()      { lhs = PExpr::Member(Box::new(lhs), name); continue; }
                if let Some(key)  = self.eat_string_lit() { lhs = PExpr::Member(Box::new(lhs), key ); continue; }
                return Err(s_help_site!(
                    "P0402",
                    "You need a field name or a quoted string after '>>'",
                    "Example: user >> email or config >> \"api-key\"",
                ));
            }

            // ---------- optional member / optional call: ?>> name | ?>>(args...) ----------
            if self.eat_op("?>>") {
                if self.eat_op("(") {
                    // ?>>( ... )  — optional call on the LHS (name may be empty if not a prior member)
                    let mut args = Vec::new();
                    while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    if !self.peek_op(")") {
                        loop {
                            args.push(self.parse_coalesce()?);
                            if self.eat_op(",") {
                                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                                if self.peek_op(")") { break; }
                                continue;
                            }
                            break;
                        }
                    }
                    if !self.eat_op(")") { return Err(s_help_site!("P0506", "Expected ')' to close this optional call", "Add the closing ')': user?>>getName()")); }
                    lhs = match lhs {
                        PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                        other                       => PExpr::OptCall(Box::new(other), String::new(), args),
                    };
                    continue;
                } else {
                    // ?>> name  — optional member
                    let Some(name) = self.eat_ident() else { return Err(s_help_site!(
                        "P0403",
                        "You need a field name after '?>>'",
                        "Example: user ?>> email",
                    )); };
                    lhs = PExpr::OptMember(Box::new(lhs), name);
                    continue;
                }
            }

            // ---------- dot-call: .name or .name(args...) (call-only) ----------
            if self.eat_op(".") {
                // require identifier after '.'
                let name_tok = self
                    .eat_ident()
                    .ok_or_else(|| s_help_site!("P.DOTID", "Expected identifier after '.'", "Example: obj.method(...)"))?;
                let opname = name_tok.as_str().to_string();

                if self.eat_op("(") {
                    // Special-case: .format(...)
                    if opname == "format" {
                        // Parses: format(DEC)  or  format(DEC THOUSANDS DECIMAL)
                        // Example: x.format(2 , .)   // US  -> thousands ','  decimal '.'
                        //          x.format(2 . ,)   // EU  -> thousands '.'  decimal ','
                        //          x.format(2 "none" .) // no thousands sep, decimal '.'
                        let args = self.parse_format_args_pexpr_after_lparen()?;

                        lhs = match lhs {
                            PExpr::IsBound(inner) => PExpr::OptCall(inner, opname, args),
                            other                  => PExpr::Call(Box::new(other),  opname, args),
                        };
                    } else {
                        // Fallback: normal comma-separated args (may be empty)
                        let args = if self.eat_op(")") { vec![] } else { self.parse_args_paren()? };

                        lhs = match lhs {
                            PExpr::IsBound(inner) => PExpr::OptCall(inner, opname, args),
                            other                  => PExpr::Call(Box::new(other),  opname, args),
                        };
                    }
                } else {
                    let is_method_reference = match self.toks.get(self.i) {
                        Some(tok) => matches!(&tok.kind,
                            goblin_lexer::TokenKind::Newline |
                            goblin_lexer::TokenKind::Eof
                        ) || matches!(&tok.kind, goblin_lexer::TokenKind::Op(s) if s == "," || s == ")"),
                        None => true,
                    };
                    
                    if is_method_reference {
                        lhs = PExpr::Member(Box::new(lhs), opname);
                    } else {
                        lhs = match lhs {
                            PExpr::IsBound(inner) => PExpr::OptCall(inner, opname, vec![]),
                            other                  => PExpr::Call(Box::new(other),  opname, vec![]),
                        };
                    }
                }

                // Check for ? AFTER the if/else
                if self.peek_op("?") {
                    self.eat_op("?");
                    match lhs {
                        PExpr::Member(obj, prop) => {
                            let new_prop = format!("is_{}", prop);
                            lhs = PExpr::Member(obj, new_prop);
                        }
                        PExpr::Call(obj, prop, args) if args.is_empty() => {
                            // Transform .method?() into .is_method as a Member
                            let new_prop = format!("is_{}", prop);
                            lhs = PExpr::Member(obj, new_prop);
                        }
                        _ => {
                            panic!(
                                "P0XXX: The '?' postfix only works on member access like .string?\n\
                                 help: Current lhs: {:?}", lhs
                            );
                        }
                    }
                }

                continue;
            }

            // ---------- colon-call: target : arg, arg, ... ----------
            if self.suspend_colon_call == 0 && self.eat_op(":") {
                let mut args = Vec::new();
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if self.peek_newline_or_eof() { return Err(s_help_site!("P0507", "Expected an argument after ':'", "Add at least one argument after ':': calculate: price, tax")); }

                loop {
                    args.push(self.parse_coalesce()?);
                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if self.peek_newline_or_eof() { break; }
                        continue;
                    }
                    break;
                }

                lhs = match lhs {
                    PExpr::Member(obj, name)    => PExpr::Call(obj, name, args),
                    PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                    PExpr::Ident(name)          => PExpr::FreeCall(name, args),
                    other                       => return Err(s_help_site!(
                        "P0508",
                        &format!("You can't use ':' to call this: {:?}", other),
                        "Use ':' with a free action or member action target: calculate: price, tax",
                    )),
                };
                continue;
            }

            // ---------- paren-call: target(args...) ----------
            if self.eat_op("(") {
                let mut args = Vec::new();
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if !self.peek_op(")") {
                    loop {
                        args.push(self.parse_coalesce()?);
                        if self.eat_op(",") {
                            while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                            if self.peek_op(")") { break; }
                            continue;
                        }
                        break;
                    }
                }
                if !self.eat_op(")") { return Err(s_help_site!("P0509", "Expected ')' after the argument list", "Add the closing ')': calculate(price, tax)")); }

                lhs = match lhs {
                    PExpr::Member(obj, name)    => PExpr::Call(obj, name, args),
                    PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                    PExpr::Ident(name)          => PExpr::FreeCall(name, args),
                    other                       => PExpr::Call(Box::new(other), String::new(), args),
                };
                continue;
            }

            // ---------- namespaced free call OR enum variant: Ns::func(...) or Enum::variant ----------
            if matches!(&lhs, PExpr::Ident(_)) && self.peek_op("::") {
                let ns = if let PExpr::Ident(ref s) = lhs { s.clone() } else { unreachable!() };
                let _ = self.eat_op("::");
                let Some(name) = self.eat_ident() else { 
                    return Err(s_help_site!(
                        "P0510",
                        "Expected a name after '::'",
                        "Write it like: Status::idle or Module::action()",
                    )); 
                };
                
                // Check if this is a call (has parentheses or colon args)
                if self.eat_op("(") {
                    let args = if self.eat_op(")") { vec![] } else { self.parse_args_paren()? };
                    lhs = PExpr::NsCall(ns, name, args);
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::NsCall(ns, name, args);
                } else if self.eat_op("{") {
                    // Enum variant with fields: Message::move { x: 10, y: 20 }
                    let mut fields = Vec::new();
                    
                    loop {
                        self.skip_newlines();
                        
                        if self.eat_op("}") {
                            break;
                        }
                        
                        let Some(field_name) = self.eat_ident() else {
                            return Err(s_help_site!(
                                "P1010",
                                "Expected a field name",
                                "Write it like: { x: 10, y: 20 }",
                            ));
                        };
                        
                        if !self.eat_op(":") {
                            return Err(s_help_site!(
                                "P1011",
                                "Expected ':' after field name",
                                "Write it like: { x: 10, y: 20 }",
                            ));
                        }
                        
                        let value = self.parse_coalesce()?;
                        fields.push((field_name, value));
                        
                        self.skip_newlines();
                        if !self.eat_op(",") {
                            if !self.peek_op("}") {
                                return Err(s_help_site!(
                                    "P1012",
                                    "Expected ',' or '}' after field value",
                                    "Separate fields with commas: { x: 10, y: 20 }",
                                ));
                            }
                        }
                    }
                    
                    lhs = PExpr::EnumVariant {
                        enum_name: ns,
                        variant_name: name,
                        fields: Some(fields),
                    };
                } else {
                    // No call syntax, no fields - simple enum variant
                    lhs = PExpr::EnumVariant {
                        enum_name: ns,
                        variant_name: name,
                        fields: None,
                    };
                }
                continue;
            }

            // ---------- nothing matched; ensure progress or exit ----------
            if self.i == start_i { break; }
        }

        Ok(lhs)
    }

    fn parse_args_paren(&mut self) -> Result<Vec<PExpr>, String> {
        let mut args = Vec::new();

        loop {
            let expr = self.parse_coalesce()?;
            args.push(expr);

            if self.eat_op(",") {
                continue;
            } else if self.eat_op(")") {
                break;
            } else {
                return Err(s_help_site!(
                    "P0511",
                    "Expected ',' or ')' in the argument list",
                    "Use ',' to separate and ')' to close: calculate(price, tax)",
                ));
            }
        }

        Ok(args)
    }

    fn parse_args_colon(&mut self) -> Result<Vec<PExpr>, String> {
        let mut args = Vec::new();
        args.push(self.parse_coalesce()?);
        while self.eat_op(",") {
            args.push(self.parse_coalesce()?);
        }
        Ok(args)
    }
}

pub(crate) fn parse_expr_preview(tokens: &[goblin_lexer::Token]) -> Result<PExpr, String> {
    let mut p = Parser::new(tokens);
    p.parse_coalesce()
}

pub(crate) fn parse_program(tokens: &[goblin_lexer::Token]) -> Result<Vec<PDecl>, String> {
    let mut p = Parser::new(tokens);
    let mut out = Vec::new();

    while p.i < p.toks.len() {
        p.eat_semi_separators();
        p.skip_newlines();
        if p.i >= p.toks.len() {
            break;
        }

        let decl = p.parse_decl()?;
        out.push(decl);

        p.eat_semi_separators();
        p.skip_newlines();
    }

    Ok(out)
}

pub(crate) fn parse_program_preview(tokens: &[goblin_lexer::Token]) -> Result<Vec<PExpr>, String> {
    let mut p = Parser::new(tokens);
    let mut out = Vec::new();

    while p.i < p.toks.len() {
        p.eat_semi_separators();
        p.skip_newlines();
        if p.i >= p.toks.len() { break; }

        if matches!(p.peek().map(|t| &t.kind), Some(TokenKind::ClassIdent)) {
            if let Some(res) = p.try_parse_class_decl() {
                let node = res?;
                out.push(node);
                p.eat_semi_separators();
                p.skip_newlines();
                continue;
            }
        }

        let expr = p.parse_coalesce()?;
        out.push(expr);

        p.eat_semi_separators();
        p.skip_newlines();
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    // 1) Provide an empty static token slice to satisfy Parser::new(&[Token])
    static EMPTY_TOKENS: [goblin_lexer::Token; 0] = [];

    // 2) Return a Parser with a concrete lifetime
    fn parser() -> Parser<'static> {
        Parser::new(&EMPTY_TOKENS)
    }

    #[test]
    fn ok_plain_interpolation() {
        assert!(parser().validate_interpolation_braces("Hello {name}").is_ok());
    }

    #[test]
    fn ok_triple_brace_token() {
        assert!(parser().validate_interpolation_braces("X {{{BRINDLE::OBSIDIAN}}} Y").is_ok());
    }

    #[test]
    fn err_unclosed_triple() {
        let err = parser()
            .validate_interpolation_braces("X {{{BRINDLE::OBSIDIAN")
            .unwrap_err();
        assert!(err.contains("Unclosed"));
    }

    #[test]
    fn ok_literal_close_brace_escape() {
        assert!(parser().validate_interpolation_braces("This prints a brace: {{/}}").is_ok());
    }

    #[test]
    fn err_unclosed_single_brace() {
        assert!(parser().validate_interpolation_braces("{ not closed").is_err());
    }
}