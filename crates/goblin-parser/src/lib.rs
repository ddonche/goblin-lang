#![allow(dead_code)]
#![allow(unused_assignments)]
#![allow(unused_variables)]

use goblin_ast as ast;
use goblin_diagnostics::{Diagnostic, Span};
use goblin_lexer::{Token, TokenKind};

mod diagnostics_ext;
pub use diagnostics_ext::{s, s_help, derr, derr_help, derr_expected_found};

#[derive(Debug, Clone)]
enum PExpr {
    Array(Vec<PExpr>),
    Assign(Box<PExpr>, Box<PExpr>),
    Binary(Box<PExpr>, String, Box<PExpr>),
    Bool(bool),
    Call(Box<PExpr>, String, Vec<PExpr>),
    Float(String),
    FloatWithUnit(String, String),
    FreeCall(String, Vec<PExpr>),
    Ident(String),
    Index(Box<PExpr>, Box<PExpr>),
    Int(String),
    IntWithUnit(String, String),
    IsBound(Box<PExpr>),
    Member(Box<PExpr>, String),
    Money(String),
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
    BlobStr(String), 
    BlobNum(String),  
    Date(String),
    Time(String),
    DateTime { value: String, tz: Option<String> },
    StrInterp(Vec<StrPart>),

    ClassDecl {
        name: String,
        fields: Vec<(String, PExpr)>,
        actions: Vec<PAction>,
    },
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
    params: Vec<String>,
    body: Vec<PExpr>,
    is_single: bool,
}


#[derive(Debug, Clone)]
enum PDecl {
    Expr(PExpr),
    Action(PAction),
    Class {
        name: String,
        fields: Vec<(String, PExpr)>,
        actions: Vec<PAction>,
    },
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
    last_progress_check: usize,
    suspend_colon_call: usize,
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
            last_progress_check: 0,
            suspend_colon_call: 0,
        }
    } 

    const MAX_RECURSION: usize = 64;

    #[inline]
    fn with_depth<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, String>
    ) -> Result<T, String> {
        let start_i = self.i;

        // Detect "stuck" progress to avoid infinite loops.
        if start_i == self.last_progress_check {
            return Err(s_help(
                "P0101",
                &format!(
                    "The parser got stuck and can't continue near token {} (token: {:?}).",
                    start_i,
                    self.toks.get(start_i).map(|t| &t.kind)
                ),
                "Check for a missing 'end' or 'xx' (crossbones) above, or an unfinished string nearby. Close the block with 'end' or 'xx', or finish the string.",
            ));
        }

        self.rec_depth += 1;
        if self.rec_depth > Self::MAX_RECURSION {
            self.rec_depth -= 1;
            return Err(s_help(
                "P1001",
                "This expression is too deeply nested and can't be parsed clearly.",
                "Split it into smaller sub-expressions on separate lines, then combine the results (use fewer layers of parentheses).",
            ));
        }

        let out = f(self);

        self.rec_depth -= 1;

        // Update progress tracking
        if self.i > start_i {
            self.last_progress_check = self.i;
        }

        out
    }

    // Forbid `{` immediately after a header on the SAME line.
    // `{` remains allowed only in expressions (maps/interpolation).
    fn forbid_brace_after_header(&self, header_line: u32) -> Result<(), String> {
        if let Some(t) = self.toks.get(self.i) {
            if t.span.line_start == header_line && t.value.as_deref() == Some("{") {
                return Err(s_help(
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

    fn parse_assign(&mut self) -> Result<PExpr, String> {
        // Parse potential LHS first
        let lhs = self.parse_coalesce()?;

        // Detect assignment / compound-assign
        let op: Option<&'static str> =
            if self.peek_op("//=") { Some("//=") }
            else if self.peek_op("+=") { Some("+=") }
            else if self.peek_op("-=") { Some("-=") }
            else if self.peek_op("*=") { Some("*=") }
            else if self.peek_op("/=") { Some("/=") }
            else if self.peek_op("%=") { Some("%=") }
            else if self.peek_op("=")  { Some("=")  }
            else { None };

        if op.is_none() {
            return Ok(lhs);
        }

        // Assignments only at statement level
        if !self.in_stmt {
            return Err(s_help(
                "P0301",
                "You can't use assignment (=) inside an expression.",
                "Put the assignment on its own line, then use the variable: result = calculate() then total = result * 2.",
            ));
        }

        // Forbid assigning to meta .type
        if self.lhs_ends_with_dot_type_at(self.i) {
            return Err(s_help(
                "P0302",
                "You can't assign to `.type`; it's a special system property.",
                "If you want a field named 'type', write: person >> type = \"admin\".",
            ));
        }

        let op = op.unwrap();
        let _ = self.eat_op(op);

        // Allow newline(s) before RHS
        self.skip_newlines();

        // NOTE: We no longer support “brace-block RHS”. Braces after '=' belong to expressions
        // (maps/interpolation) and are handled by the primary/object-literal parser.
        // Fall through to normal RHS expression (disable nested assignment).
        let prev_in_stmt = self.in_stmt;
        self.in_stmt = false;
        let rhs = self.parse_coalesce()?;
        self.in_stmt = prev_in_stmt;

        if op == "=" {
            Ok(PExpr::Assign(Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs)))
        }
    }

    fn parse_coalesce(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_or()?;
        while self.eat_op("??") {
            let rhs = self.parse_or()?;
            lhs = PExpr::Binary(Box::new(lhs), "??".into(), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn ensure_progress(&mut self, start_i: usize, context: &str) -> Result<(), String> {
        if self.i <= start_i {
            return Err(s_help(
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
                        return Err(s_help(
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
            return Err(s_help(
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
                    return Err(s_help(
                        "P0402",
                        "You need a field name or a quoted string after '>>'",
                        "Example: user >> name or config >> \"api-key\"",
                    ));
                }
            }

            if self.eat_op("?>>") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }

                let Some(name) = self.eat_ident() else {
                    return Err(s_help(
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
                    return Err(s_help(
                        "P0701",
                        "Brackets need an index or slice expression",
                        "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]",
                    ));
                }

                let idx = self.parse_coalesce()?;

                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, K::Newline)) { self.i += 1; }
                if !self.eat_op("]") {
                    return Err(s_help(
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
            // disallow anything that could invoke user code or assign
            Call(..) | FreeCall(..) | NsCall(..) | OptCall(..) | Assign(..) => false,

            & PExpr::Money(_) => true,

            // recurse through composites (note the .as_ref() since we have &PExpr)
            Binary(l,_,r) => Self::key_expr_is_side_effect_free(l.as_ref()) && Self::key_expr_is_side_effect_free(r.as_ref()),
            Prefix(_,x) | Postfix(x,_) | IsBound(x) => Self::key_expr_is_side_effect_free(x.as_ref()),
            Index(x,y) => Self::key_expr_is_side_effect_free(x.as_ref()) && Self::key_expr_is_side_effect_free(y.as_ref()),
            Member(x,_) | OptMember(x,_) => Self::key_expr_is_side_effect_free(x.as_ref()),

            Slice(x,a,b) => {
                if !Self::key_expr_is_side_effect_free(x.as_ref()) { return false; }
                if let Some(bx) = a.as_ref() { if !Self::key_expr_is_side_effect_free(bx.as_ref()) { return false; } }
                if let Some(bx) = b.as_ref() { if !Self::key_expr_is_side_effect_free(bx.as_ref()) { return false; } }
                true
            }
            Slice3(x,a,b,c) => {
                if !Self::key_expr_is_side_effect_free(x.as_ref()) { return false; }
                if let Some(bx) = a.as_ref() { if !Self::key_expr_is_side_effect_free(bx.as_ref()) { return false; } }
                if let Some(bx) = b.as_ref() { if !Self::key_expr_is_side_effect_free(bx.as_ref()) { return false; } }
                if let Some(bx) = c.as_ref() { if !Self::key_expr_is_side_effect_free(bx.as_ref()) { return false; } }
                true
            }

            Array(xs)   => xs.iter().all(Self::key_expr_is_side_effect_free),
            Object(kvs) => kvs.iter().all(|(_,v)| Self::key_expr_is_side_effect_free(v)),
            StrInterp(ps) => ps.iter().all(|p| matches!(p, StrPart::Text(_) | StrPart::LValue{..})),

            // harmless leaves in your PExpr
            Ident(_) | Int(_) | Float(_) | IntWithUnit(_,_) | FloatWithUnit(_,_)
            | Bool(_) | Nil | Str(_) | BlobStr(_) | BlobNum(_) | Date(_) | Time(_) | DateTime { .. } => true,

            // conservative default for constructs that shouldn't appear in keys
            ClassDecl { .. } => false,
        }
    }

    // Add helper to eat layout and detect closes (non-consuming peek variant for peek_block_close)
    fn eat_layout_until_close(&mut self, opening_indent: u32) -> bool {
        let mut closed = false;
        while let Some(t) = self.peek() {
            match t.kind {
                goblin_lexer::TokenKind::Newline => { self.i += 1; continue; }
                goblin_lexer::TokenKind::Dedent => {
                    self.i += 1;
                    // Optional: Check if current indent <= opening_indent (need lexer to expose indent level in Dedent)
                    // For now, any Dedent signals potential close
                    closed = true;
                    break;
                }
                goblin_lexer::TokenKind::Indent => { self.i += 1; continue; } // Eat but don't close
                _ => break,
            }
        }
        closed
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
            if self.eat_op("++") { expr = PExpr::Postfix(Box::new(expr), "++".into()); continue; }
            if self.eat_op("--") { expr = PExpr::Postfix(Box::new(expr), "--".into()); continue; }
            if self.eat_op("?")  { expr = PExpr::IsBound(Box::new(expr));               continue; }
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
                return Err(s_help(
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
                    _ => return Err(s_help(
                        "P0601",
                        "After '??', start the default with a quote (' or \")",
                        "Example: name ?? \"Anonymous\"",
                    )),
                };

                let start = i;
                while i < n && bytes[i] != quote { i += 1; }
                if i >= n {
                    return Err(s_help(
                        "P0602",
                        "The default string after '??' isn't closed",
                        "Add a matching quote to end it: name ?? \"guest\"",
                    ));
                }
                let value = std::str::from_utf8(&bytes[start..i]).unwrap().to_string();
                i += 1; // closing quote
                skip_ws(bytes, &mut i);
                if i != n {
                    return Err(s_help(
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
                if i >= n { return Err(s_help("P0703", "The '[' starts an index but it isn't closed", "Add a matching ']' to complete the index: items[0]")); }

                // number?
                if i < n && bytes[i].is_ascii_digit() {
                    let start = i;
                    i += 1;
                    while i < n && (bytes[i].is_ascii_digit() || bytes[i] == b'_') { i += 1; }
                    let num = std::str::from_utf8(&bytes[start..i]).unwrap().to_string();
                    skip_ws(bytes, &mut i);
                    if !eat(bytes, &mut i, b']') { return Err(s_help("P0704", "This numeric index is missing a closing ']'", "Add ']' to close it: items[0]")); }
                    segments.push(LvSeg::IndexNumber(num));
                    continue;
                }

                // ident?
                if i < n && is_alpha(bytes[i]) {
                    let name = parse_ident(bytes, &mut i)?;
                    skip_ws(bytes, &mut i);
                    if !eat(bytes, &mut i, b']') { return Err(s_help("P0705", "This identifier index is missing a closing ']'", "Add ']' to close it: items[id]")); }
                    segments.push(LvSeg::IndexIdent(name));
                    continue;
                }

                return Err(s_help(
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

    fn validate_interpolation_braces(&self, s: &str) -> Result<(), String> {
        // Error if there's any unmatched '{' after accounting for the literal sequence "{{/}}"
        let bytes = s.as_bytes();
        let mut i = 0usize;
        let n = bytes.len();
        let mut depth: usize = 0;

        while i < n {
            // Treat "{{/}}" as a literal '}' in TEXT context
            if i + 4 < n && &bytes[i..i + 5] == b"{{/}}" {
                i += 5;
                continue;
            }
            match bytes[i] {
                b'{' => { depth += 1; i += 1; }
                b'}' => { if depth > 0 { depth -= 1; } i += 1; }
                _ => { i += 1; }
            }
        }

        if depth != 0 {
            Err(s_help(
                "P0604",
                "There's an unclosed '{' in this string",
                "Add a matching '}' to close it: \"Hello {name}\"",
            ))
        } else {
            Ok(())
        }
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
            s_help(
                "P1003",
                &format!("Expected an expression, but found {} after {}", here, prev),
                "Use a value, variable, or call: total = price * qty",
            )
        } else {
            s_help(
                "P1003",
                &format!("Expected an expression {} but found {} after {}", ctx, here, prev),
                "Use a value, variable, or call: total = price * qty",
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
            return Err(s_help(
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
            return Err(s_help(
                "P0901",
                "This duration format isn't valid",
                "Use formats like 5s, 10m, 2h, or 3d",
            ));
        }
        // Minimal unit validation
        match unit {
            "s" | "m" | "h" | "d" | "w" | "y" | "mo" => Ok((base.to_string(), unit.to_string())),
            _ => Err(s_help(
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
            PExpr::Nil => ast::Expr::Nil(sp),

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

            // Unary / postfix / binary / assignment
            PExpr::Prefix(op, expr) => {
                let expr = Box::new(Self::lower_expr_preview(*expr, sp.clone()));
                ast::Expr::Prefix(op, expr, sp)
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
            PExpr::Assign(lhs, rhs) => {
                let lhs = Box::new(Self::lower_expr_preview(*lhs, sp.clone()));
                let rhs = Box::new(Self::lower_expr_preview(*rhs, sp.clone()));
                ast::Expr::Assign(lhs, rhs, sp)
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
        if !self.peek_op("@") {
            return None;
        }
        
        let save = self.i;
        if self.eat_op("@") {
            if let Some(_) = self.peek_ident() {
                self.i = save;
                Some(self.parse_class_decl())
            } else {
                self.i = save;
                None
            }
        } else {
            None
        }
    }

    fn skip_action_block(&mut self) -> Result<(), String> {
        let mut depth = 1;
        loop {
            if self.is_eof() {
                return Err(s_help(
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

    fn parse_field_chain_line(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        let mut out = Vec::new();

        loop {
            // 1) Field key
            let Some(key) = self.eat_object_key() else {
                if out.is_empty() {
                    return Err(s_help(
                        "P0903",
                        "Expected a field name in this class header",
                        "Add a field in the header: @Player = username: \"john\" :: health: 100",
                    ));
                } else {
                    break;
                }
            };

            // 2) Colon
            if !self.eat_op(":") {
                return Err(s_help(
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

            // 3) Scan the rest of THIS LINE (top-level only) to find end of this value
            let value_start = self.i;
            let value_line = match self.toks.get(value_start) {
                Some(t) => t.span.line_start,
                None => {
                    return Err(s_help(
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

                    // NEW FIELD starting without a separator: ident ':' at top level, later on same line
                    TokenKind::Ident
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 && k > value_start =>
                    {
                        if let Some(next) = self.toks.get(k + 1) {
                            if next.span.line_start == value_line
                                && matches!(next.kind, TokenKind::Op(ref c) if c == ":")
                            {
                                return Err(s_help(
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

            // 6) Separator handling (consume exactly ONE; disallow doubles)
            if let Some(si) = sep_i {
                self.i = si;

                // consume exactly one separator
                let _ = if self.peek_op("::") { self.eat_op("::") } else { self.eat_op(",") };

                // If another separator immediately follows on the same line → error
                if let Some(tok2) = self.toks.get(self.i) {
                    if tok2.span.line_start == value_line {
                        if let goblin_lexer::TokenKind::Op(op) = &tok2.kind {
                            if op == "::" || op == "," {
                                return Err(s_help(
                                    "P0907",
                                    "Unexpected extra separator",
                                    "Use a single '::' or ',' between fields: name: \"Rook\" :: health: 100",
                                ));
                            }
                        }
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

    fn parse_action_after_keyword(&mut self, kw: &str) -> Result<PAction, String> {

        let Some(name) = self.eat_ident() else {
            return Err(s_help(
                "P0501",
                &format!("You need to give your {} a name", kw),
                &format!("Write it like: {} Save", kw),
            ));
        };

        // optional params: (p1, p2, ...)
        let mut params = Vec::new();
        if self.eat_op("(") {
            if !self.peek_op(")") {
                loop {
                    let Some(p) = self.eat_ident() else {
                        return Err(s_help(
                            "P0502",
                            "Expected a parameter name",
                            "Use a simple identifier like username or count",
                        ));
                    };
                    params.push(p);
                    if self.eat_op(",") {
                        if self.peek_op(")") { break; }
                        continue;
                    }
                    break;
                }
            }
            if !self.eat_op(")") {
                return Err(s_help(
                    "P0503",
                    "Expected ')' to close the parameter list",
                    "Add the closing ')': action Save(username, level)",
                ));
            }
        }

        // single-line form: `act foo = expr`
        if self.eat_op("=") {
            let expr = self.parse_coalesce()?;
            return Ok(PAction {
                name,
                params,
                body: vec![expr],
                is_single: true,
            });
        }

        // IMPORTANT: do NOT parse a block body here.
        Ok(PAction {
            name,
            params,
            body: Vec::new(),
            is_single: false,
        })
    }

    fn parse_free_action(&mut self, kw: &str) -> Result<ast::Stmt, String> {
        use goblin_lexer::TokenKind;
        let hdr_tok_i = self.i.saturating_sub(1);
        let hdr_start = self.toks[hdr_tok_i].clone();
        let hdr_line = hdr_start.span.line_start;
        let hdr_col = hdr_start.span.col_start;
        let action_start = self.i;
        let pa = self.parse_action_after_keyword(kw)?;
        let action_span = Self::span_from_tokens(self.toks, action_start, self.i);
        let params: Vec<ast::Param> = pa
            .params
            .into_iter()
            .map(|pname| ast::Param {
                name: pname,
                type_name: None,
                default: None,
                span: action_span.clone(),
            })
            .collect();
        if !pa.body.is_empty() {
            let body_stmts: Vec<ast::Stmt> = pa
                .body
                .into_iter()
                .map(|pe| ast::Stmt::Expr(self.lower_expr(pe)))
                .collect();
            let act = ast::ActionDecl {
                name: pa.name,
                params,
                body: ast::ActionBody::Block(body_stmts),
                span: action_span,
                ret: None,
            };
            return Ok(ast::Stmt::Action(act));
        }
        if let Some(prev_tok) = self.toks.get(self.i.saturating_sub(1)) {
            let mut j = self.i;
            while let Some(tok) = self.toks.get(j) {
                match &tok.kind {
                    TokenKind::Newline => {
                        j += 1;
                        continue;
                    }
                    TokenKind::Op(s) if s == ";" => {
                        j += 1;
                        continue;
                    }
                    _ => break,
                }
            }
            if let Some(next) = self.toks.get(j) {
                if matches!(next.kind, TokenKind::Op(ref s) if s == "{")
                    && next.span.line_start > prev_tok.span.line_end
                {
                    return Err(s_help(
                        "P0205",
                        "A '{' on a new line starts an object, not an action block",
                        "Blocks use layout, not braces. Start the block on the next line and close with 'end' or 'xx' (crossbones).",
                    ));
                }
            }
        }
        self.enforce_inline_brace_policy(hdr_line, kw)?;
        let body_stmts = self.parse_stmt_block_until(|p: &mut Parser<'_>| {
            p.skip_newlines();
            p.eat_layout_until_close(hdr_col) || (p.peek_block_close() && p.toks[p.i].span.col_start == hdr_col)
        })?;
        self.skip_stmt_separators();
        if self.block_closed_hard {
            self.block_closed_hard = false;
        } else if self.peek_block_close() {
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if col != hdr_col {
                return Err(s_help(
                    "P0222",
                    &format!(
                        "This closer is misaligned: expected column {}, found column {}",
                        hdr_col, col
                    ),
                    "Align the closer with its header (same column), placing 'end' or 'xx' directly under the action header.",
                ));
            }
            self.expect_block_close("action")?;
        } else if !self.eat_layout_until_close(hdr_col) {
            return Err(s_help(
                "P0212",
                "This action block is missing its closing 'end' or 'xx' (crossbones).",
                "Close the block with 'end' or 'xx' (crossbones).",
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

    // Do we see a block closer at the current position?  (allowed: 'end' or 'xx')
    fn peek_block_close(&mut self) -> bool {
        let save_i = self.i;
        self.skip_newlines();
        let j = self.i;
        self.eat_layout_until_close(0); // Temp eat for peek
        let is_close = match self.toks.get(j) {
            Some(t) => {
                t.value.as_deref() == Some("end") || t.value.as_deref() == Some("xx") || matches!(t.kind, ::goblin_lexer::TokenKind::Dedent)
            }
            None => true, // EOF closes
        };
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
            return Err(s_help(
                "P0212",
                &format!("This {} block is missing its closing 'end' or 'xx' (crossbones).", block_type),
                "Close the block with 'end' or 'xx' (crossbones).",
            ));
        }
        let tok = self.toks.get(self.i).ok_or_else(|| {
            s_help(
                "P0212",
                &format!("This {} block is missing its closing 'end' or 'xx' (crossbones).", block_type),
                "Close the block with 'end' or 'xx' (crossbones).",
            )
        })?;
        if tok.value.as_deref() != Some("end") && tok.value.as_deref() != Some("xx") {
            return Err(s_help(
                "P0212",
                &format!("Expected 'end' or 'xx' to close this {} block.", block_type),
                "Use 'end' or 'xx' to close the block.",
            ));
        }
        self.i += 1;
        self.eat_layout_until_close(0);
        Ok(())
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
                Some(tok) if matches!(tok.kind, K::Op(ref s) if s.starts_with("///") || s.starts_with("////")) => {
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
                Some(tok) if matches!(tok.kind, K::Op(ref s) if s.starts_with("///") || s.starts_with("////")) => {
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
            return Err(s_help(
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
                        return Err(s_help(
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
                        return Err(s_help(
                            "P0212",
                            "This block is missing its closing 'end' or 'xx' (crossbones)",
                            "Close the block with 'end' or 'xx' (crossbones).",
                        ));
                    }
                    continue;
                }
                Some(_) => { /* normal statement path */ }
                None => {
                    return Err(s_help(
                        "P0201",
                        "I reached the end of the file, but this block is still open",
                        "Close the block with 'end' or 'xx' (crossbones).",
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
            return Err(s_help(
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
                return Err(s_help(
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

    fn parse_class_decl(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;
        let (name, hdr_col) = if let Some(tok0) = self.peek().cloned() {
            match tok0.kind {
                TokenKind::AtIdent => {
                    let nm = tok0.value.clone().unwrap_or_default();
                    if nm.is_empty() {
                        return Err(s_help(
                            "P0906",
                            "Expected a class name after '@'",
                            "Start with a capitalized name: @Player = username: \"john\" :: health: 100",
                        ));
                    }
                    self.i += 1; // eat AtIdent
                    (nm, tok0.span.col_start)
                }
                TokenKind::Op(ref op) if op == "@" => {
                    let at_col = tok0.span.col_start;
                    self.i += 1; // eat '@'
                    let Some(nm) = self.eat_ident() else {
                        return Err(s_help(
                            "P0906",
                            "Expected a class name after '@'",
                            "Start with a capitalized name: @Player = username: \"john\" :: health: 100",
                        ));
                    };
                    (nm, at_col)
                }
                _ => {
                    return Err(s_help(
                        "P0905",
                        "Expected '@' to start a class declaration",
                        "Start the class like: @Player = username: \"john\" :: health: 100",
                    ));
                }
            }
        } else {
            return Err(s_help(
                "P0905",
                "Expected '@' to start a class declaration",
                "Start the class like: @Player = username: \"john\" :: health: 100",
            ));
        };
        if !name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            return Err(s_help(
                "P0907",
                &format!("Class names must start with a capital letter (found '{}')", name),
                "Rename it to start with uppercase: Player",
            ));
        }
        if self.peek_op("!") {
            self.i += 1;
        }
        if self.eat_op("(") {
            if !self.peek_op(")") {
                loop {
                    let _ = self.parse_coalesce()?;
                    if self.eat_op(",") {
                        continue;
                    }
                    break;
                }
            }
            if !self.eat_op(")") {
                return Err(s_help(
                    "P0703",
                    "Unclosed parameter list after class header",
                    "Close it with ')': @Class(Name)",
                ));
            }
        }
        if !self.eat_op("=") {
            return Err(s_help(
                "P0908",
                &format!("You need '=' after the class name '{}'", name),
                "Write it like: @Player = username: \"john\" :: health: 100",
            ));
        }
        let fields = self.parse_field_chain_line()?;
        self.eat_semi_separators();
        let mut actions: Vec<PAction> = Vec::new();
        loop {
            self.skip_newlines();
            if self.eat_layout_until_close(hdr_col) || self.peek_block_close() {
                if self.peek_block_close() {
                    let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                    if col != hdr_col {
                        let closer = self.peek_ident().unwrap_or("}");
                        return Err(s_help(
                            "P0222",
                            &format!(
                                "This '{}' closer is misaligned: expected column {}, found column {}",
                                closer, hdr_col, col
                            ),
                            "Align the closer with its header (same column): place 'end' or 'xx' (crossbones) directly under the start of the class header.",
                        ));
                    }
                    self.expect_block_close("class")?;
                }
                break;
            }
            if self.is_eof() {
                return Err(s_help(
                    "P0909",
                    &format!(
                        "I reached the end of the file, but the class '{}' is still open (missing 'end' or 'xx')",
                        name
                    ),
                    "Close the class with 'end' or 'xx' (crossbones): @Player = username: \"john\" :: health: 100 end",
                ));
            }
            if let Some(tok) = self.peek() {
                if tok.span.col_start == hdr_col {
                    let is_act_kw = matches!(&tok.kind, TokenKind::Act | TokenKind::Action)
                        || (matches!(&tok.kind, TokenKind::Ident)
                            && (tok.value.as_deref() == Some("act") || tok.value.as_deref() == Some("action")));
                    if !is_act_kw {
                        break;
                    }
                }
            }
            let act_tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };
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
                _ => {
                    return Err(s_help(
                        "P0910",
                        "Inside a class, only 'act', 'action', or a closing 'end'/'xx' are allowed here",
                        "Add an action or close the class: act run(a) ... end, or end",
                    ));
                }
            };
            let act_line = act_tok.span.line_start;
            let act_col = act_tok.span.col_start;
            let action = self.parse_action_after_keyword(kw)?;
            self.enforce_inline_brace_policy(act_line, "action")?;
            if !action.body.is_empty() {
                actions.push(action);
                continue;
            }
            let body = self.parse_stmt_block_until(|p: &mut Parser<'_>| {
                p.skip_newlines();
                p.eat_layout_until_close(act_col)
                    || (p.peek_block_close() && p.toks[p.i].span.col_start == act_col)
            })?;
            self.skip_stmt_separators();
            if self.block_closed_hard {
                self.block_closed_hard = false;
            } else if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != act_col {
                    return Err(s_help(
                        "P0222",
                        &format!(
                            "This closer is misaligned: expected column {}, found column {}",
                            act_col, col
                        ),
                        "Align the closer with its header (same column), placing 'end' or 'xx' directly under the action header.",
                    ));
                }
                self.expect_block_close("action")?;
            } else if !self.eat_layout_until_close(act_col) {
                return Err(s_help(
                    "P0212",
                    "This action block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
            // Convert ast::Stmt to PExpr for PAction::body
            actions.push(PAction {
                name: action.name,
                params: action.params,
                body: body
                    .into_iter()
                    .map(|stmt| match stmt {
                        ast::Stmt::Expr(expr) => match expr {
                            ast::Expr::Ident(name, _) => PExpr::Ident(name), // Ignore Span
                            _ => PExpr::Ident("expr".into()), // Fallback for other ast::Expr variants
                        },
                        _ => PExpr::Ident("stmt".into()), // Fallback for non-expression statements
                    })
                    .collect(),
                is_single: false,
            });
        }
        Ok(PExpr::ClassDecl { name, fields, actions })
    }

    fn parse_decl(&mut self) -> Result<PDecl, String> {
        if let Some(t) = self.peek() {
            let kind = t.kind.clone();
            let val = t.value.clone();
            match kind {
                TokenKind::Op(op) if op == "@" => {
                    let class = self.parse_class_decl()?;
                    if let PExpr::ClassDecl { name, fields, actions } = class {
                        return Ok(PDecl::Class { name, fields, actions });
                    }
                }
                TokenKind::Ident => {
                    let kw = val.as_deref().unwrap_or("");
                    match kw {
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
        Err(s_help(
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
            // identifiers (including capitalized) and @Ident can begin expressions
            TokenKind::Ident | TokenKind::AtIdent => true,

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
                    return Err(s_help(
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
        // We are at 'if <cond> <newline or {>'
        let if_tok_i = self.i; // index of 'if'
        let if_line  = self.toks[if_tok_i].span.line_start;
        let if_col   = self.toks[if_tok_i].span.col_start;

        let _ = self.eat_ident();        // 'if'
        let _cond = self.parse_assign()?; // parse condition

        // Friendly diagnostic: next-line '{' is object literal, not a block
        {
            let save = self.i;
            // Look ahead (don't consume anything that matters to block reader)
            while let Some(tok) = self.toks.get(self.i) {
                match tok.kind {
                    TokenKind::Newline => { self.i += 1; continue; }
                    TokenKind::Op(ref s) if s == ";" => { self.i += 1; continue; }
                    _ => break,
                }
            }
            if let Some(t) = self.peek() {
                if let goblin_lexer::TokenKind::Op(op) = &t.kind {
                    if op == "{" && t.span.line_start > if_line {
                        self.i = save;
                        return Err(s_help(
                            "P0205",
                            "A '{' on a new line starts an object, not an 'if' block",
                            "Blocks use layout, not braces. Start the block on the next line and close with 'end' or 'xx' (crossbones).",
                        ));
                    }
                }
            }
            self.i = save;
        }

        // THEN body — hand control straight to the block reader
        // FIXED: Also check for aligned 'end' or 'xx' as stop conditions
        let _then_body = self.parse_stmt_block_until(|p| {
            p.peek_ident_at_col("elif", if_col) || 
            p.peek_ident_at_col("else", if_col) ||
            (p.peek_block_close() && p.toks.get(p.i).map_or(false, |t| t.span.col_start == if_col))
        })?;

        // zero or more ELIF blocks
        let mut elif_count = 0usize;
        loop {
            self.eat_semi_separators();
            if self.peek_ident_at_col("elif", if_col) {
                let _ = self.eat_ident();       // 'elif'
                let _elif_cond = self.parse_assign()?;
                let _elif_body = self.parse_stmt_block_until(|p| {
                    p.peek_ident_at_col("elif", if_col) || 
                    p.peek_ident_at_col("else", if_col) ||
                    (p.peek_block_close() && p.toks.get(p.i).map_or(false, |t| t.span.col_start == if_col))
                })?;
                elif_count += 1;
                continue;
            }
            break;
        }

        // optional ELSE block
        let mut has_else = false;
        self.eat_semi_separators();
        if self.peek_ident_at_col("else", if_col) {
            let _ = self.eat_ident(); // 'else'
            // For the final else block, use parse_stmt_block which expects to consume the closer
            let _else_body = self.parse_stmt_block()?; // to 'end' / '}' / dedent
            has_else = true;
        } else {
            // No else block, so we need to consume the aligned closer ourselves
            self.skip_stmt_separators();
            if self.block_closed_hard {
                // The then/elif block already consumed the closer
                self.block_closed_hard = false;
            } else if self.peek_ident_at_col("end", if_col) || 
                      (self.peek_op("xx") && self.toks.get(self.i).map_or(false, |t| t.span.col_start == if_col)) {
                let _ = self.eat_block_close();
            } else {
                return Err(s_help(
                    "P0212",
                    "This 'if' block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
        }

        // Placeholder node so parse completes without dedicated If AST (your existing pattern)
        let sp  = Self::span_from_tokens(self.toks, if_tok_i, self.i.saturating_sub(1));
        let tag = if has_else {
            format!("if(+{} elif, else)", elif_count)
        } else {
            format!("if(+{} elif)", elif_count)
        };
        Ok(ast::Stmt::Expr(ast::Expr::Ident(tag, sp)))
    }

    // Replace your entire `parse_stmt_block_until` with this version.
    // It removes inline `{ ... }` mode, **does not consume** the aligned closer,
    // and sets `block_closed_hard` so the caller knows a hard closer is present.

    // REPLACE the existing function with this one in lib.rs
    fn parse_stmt_block_until<F>(&mut self, mut stop: F) -> Result<Vec<ast::Stmt>, String>
    where
        F: FnMut(&mut Parser<'_>) -> bool, {
        let ended_hard = false; // Fixed unused_mut warning
        let mut stmts: Vec<ast::Stmt> = Vec::new();
        let mut soft_cols: Vec<u32> = Vec::new();
        let mut soft_mode = false;
        let mut depth = 0;

        loop {
            self.skip_newlines();
            if stop(self) {
                if !soft_cols.is_empty() && !self.eat_layout_until_close(*soft_cols.last().unwrap_or(&0)) {
                    return Err(s_help(
                        "P0212",
                        "This block is missing its closing 'end' or 'xx' (crossbones).",
                        "Close the block with 'end' or 'xx' (crossbones).",
                    ));
                }
                break;
            }
            if self.is_eof() {
                if depth > 0 || !soft_cols.is_empty() {
                    return Err(s_help(
                        "P0212",
                        "This block is missing its closing 'end' or 'xx' (crossbones).",
                        "Close the block with 'end' or 'xx' (crossbones).",
                    ));
                }
                break;
            }
            let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
            if let Some(&last_col) = soft_cols.last() {
                if self.peek_block_close() {
                    if !soft_cols.contains(&col) {
                        return Err(s_help(
                            "P0222",
                            "This closer is misaligned with its block",
                            &format!("Expected 'end' or 'xx' at column {}, found column {}.", last_col, col),
                        ));
                    }
                    self.expect_block_close("block")?;
                    soft_cols.pop(); // Close the current block
                    break;
                }
                if col < last_col {
                    return Err(s_help(
                        "P0211",
                        "The indentation went back too far for this block",
                        &format!("Indent the line to match column {}, found column {}.", last_col, col),
                    ));
                } else if col > last_col {
                    soft_cols.push(col);
                    soft_mode = true;
                }
            } else {
                soft_cols.push(col);
                soft_mode = true;
            }
            if let Some(_t) = self.peek() {
                if matches!(_t.kind, ::goblin_lexer::TokenKind::Indent) {
                    depth += 1;
                    self.i += 1;
                }
            }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.skip_stmt_separators();
        }
        if !soft_cols.is_empty() && !self.eat_layout_until_close(*soft_cols.last().unwrap_or(&0)) {
            return Err(s_help(
                "P0212",
                "This block is missing its closing 'end' or 'xx' (crossbones).",
                "Close the block with 'end' or 'xx' (crossbones).",
            ));
        }
        self.block_closed_hard = ended_hard;
        Ok(stmts)
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
                    if op == "=" || op == ":" {
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

        // Top-level guard: "@Class" (optionally "@Class!") must have an '=' on the same head line.
        // We scan forward a few tokens (skipping layout and an optional '!') to find '='; if not found
        // before a newline/indent/dedent/eof, report P0411.
        if let Some(tok0) = self.peek() {
            if matches!(tok0.kind, TokenKind::AtIdent) {
                // skip layout
                let mut j = self.i + 1;
                while let Some(t) = self.toks.get(j) {
                    match t.kind {
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => j += 1,
                        _ => break,
                    }
                }
                // optionally skip a separate '!' token if your lexer produces one
                if let Some(t) = self.toks.get(j) {
                    if matches!(t.kind, TokenKind::Op(ref s) if s == "!") {
                        j += 1;
                        while let Some(t2) = self.toks.get(j) {
                            match t2.kind {
                                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => j += 1,
                                _ => break,
                            }
                        }
                    }
                }

                // scan ahead until layout/eof; succeed if we see '=' anywhere in that head segment
                let mut k = j;
                let mut saw_eq = false;
                while let Some(t) = self.toks.get(k) {
                    match &t.kind {
                        TokenKind::Op(s) if s == "=" => { saw_eq = true; break; }
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent | TokenKind::Eof => break,
                        _ => { k += 1; }
                    }
                }

                if !saw_eq {
                    return Err(derr_help(
                        "P0411",
                        "This looks like a class declaration but it’s missing '='",
                        "Write `@Card! = field: …` to declare a class, or remove the leading `@` if you meant a value",
                        tok0.span.clone(),
                    ));
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
                    items.push(stmt);
                }
                Err(msg) => {
                    // Promote the String error EXACTLY as produced by s/s_help into a Diagnostic.
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
        let start_i = self.i;
        let expr = self.parse_coalesce()?;
        if let Some(assign_op) = ["=", "+=", "-="].iter().find(|&&op| self.peek_op(op)).copied() {
            self.i += 1;
            let value = self.parse_coalesce()?;
            self.skip_stmt_separators();
            if !self.peek_block_close() && !self.eat_layout_until_close(0) {
                if self.i == start_i {
                    return Err(s_help(
                        "P0204",
                        "Expected a statement, found something else",
                        "Use a valid statement like an expression or assignment.",
                    ));
                }
            }
            match assign_op {
                "=" => Ok(ast::Stmt::Assign {
                    target: expr.clone().try_into().map_err(|_| {
                        s_help(
                            "P0302",
                            "Invalid assignment target",
                            "Use a variable, attribute, or index expression as the target.",
                        )
                    })?,
                    value: value.try_into().map_err(|_| {
                        s_help(
                            "P0303",
                            "Invalid assignment value",
                            "Use a valid expression as the assignment value.",
                        )
                    })?,
                }),
                "+=" => Ok(ast::Stmt::Append {
                    target: expr.clone().try_into().map_err(|_| {
                        s_help(
                            "P0304",
                            "Invalid append target",
                            "Use a list or other appendable expression as the target.",
                        )
                    })?,
                    value: value.try_into().map_err(|_| {
                        s_help(
                            "P0305",
                            "Invalid append value",
                            "Use a valid expression as the append value.",
                        )
                    })?,
                }),
                "-=" => Ok(ast::Stmt::Delete {
                    target: expr.clone().try_into().map_err(|_| {
                        s_help(
                            "P0306",
                            "Invalid delete target",
                            "Use a dictionary or other expression with keys as the target.",
                        )
                    })?,
                    key: value.try_into().map_err(|_| {
                        s_help(
                            "P0307",
                            "Invalid delete key",
                            "Use a valid key expression for deletion.",
                        )
                    })?,
                }),
                _ => Err(s_help(
                    "P0301",
                    &format!("Unknown assignment operator '{}'", assign_op),
                    "Use '=', '+=', or '-=' for assignments.",
                )),
            }
        } else {
            // Handle 'if' as PExpr::If
            if let PExpr::If(branches) = &expr {
                let if_col = self.toks.get(start_i).map(|t| t.span.col_start).unwrap_or(0);
                if !self.block_closed_hard {
                    self.skip_stmt_separators();
                    if self.peek_block_close() {
                        let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                        if col != if_col {
                            let closer = self.peek_ident().unwrap_or("}");
                            return Err(s_help(
                                "P0222",
                                &format!(
                                    "This '{}' closer is misaligned: expected column {}, found column {}",
                                    closer, if_col, col
                                ),
                                "Align the closer with its header (same column): place 'end' or 'xx' (crossbones) directly under the 'if' header.",
                            ));
                        }
                        self.expect_block_close("if")?;
                    } else if !self.eat_layout_until_close(if_col) {
                        return Err(s_help(
                            "P0212",
                            "This 'if' block is missing its closing 'end' or 'xx' (crossbones).",
                            "Close the block with 'end' or 'xx' (crossbones).",
                        ));
                    }
                }
            }
            self.skip_stmt_separators();
            if !self.peek_block_close() && !self.eat_layout_until_close(0) {
                if self.i == start_i {
                    return Err(s_help(
                        "P0204",
                        "Expected a statement, found something else",
                        "Use a valid statement like an expression or assignment.",
                    ));
                }
            }
            Ok(ast::Stmt::Expr(expr.try_into().map_err(|_| {
                s_help(
                    "P0308",
                    "Invalid statement expression",
                    "Use a valid expression for the statement.",
                )
            })?))
        }
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
        // --- blob literal special-case: blob "..." | blob 0xDEAD... ---
        if let Some(t) = self.peek() {
            if matches!(t.kind, goblin_lexer::TokenKind::Blob) {
                self.i += 1; self.skip_newlines();
                let (k2, v2) = match self.peek() { Some(t2) => (t2.kind.clone(), t2.value.clone()), None => return Err(s_help("P1101", "You need a string or an integer after 'blob'", "Examples: blob \"text\" or blob 0xFF")) };
                let expr = match k2 {
                    goblin_lexer::TokenKind::String => { let lit = v2.unwrap_or_default(); self.i += 1; PExpr::BlobStr(lit) }
                    goblin_lexer::TokenKind::Int    => { let lit = v2.unwrap_or_default(); self.i += 1; PExpr::BlobNum(lit) }
                    _ => return Err(s_help(
                        "P1101",
                        "You need a string or an integer after 'blob'.",
                        "Examples: blob \"text\" or blob 0xFF.",
                    )),
                };
                return Ok(self.apply_postfix_ops(expr));
            }
        }

        // --- Parenthesized ---
        if self.peek_op("(") {
            let _ = self.eat_op("(");

            // Guard deep recursion while parsing the inner expression
            let expr = self.with_depth(|p| p.parse_coalesce())?;

            if !self.eat_op(")") {
                return Err(s_help(
                    "P0707",
                    "Expected ')' to close this parenthesized expression",
                    "Add the closing ')': (a + b)",
                ));
            }
            return Ok(self.apply_postfix_ops(expr));
        }

        // --- Array literal ---
        if self.eat_op("[") {
            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
            let mut elems = Vec::new();
            if !self.peek_op("]") {
                loop {
                    let elem = self.parse_coalesce()?; elems.push(elem);
                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if self.peek_op("]") { break; }
                        continue;
                    }
                    break;
                }
            }
            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
            if !self.eat_op("]") { return Err(s_help("P0708", "Expected ']' to close this array", "Add the closing ']': [1, 2, 3]")); }
            return Ok(self.apply_postfix_ops(PExpr::Array(elems)));
        }

        // --- Object literal ---
        if self.eat_op("{") {
            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
            let mut props: Vec<(String, PExpr)> = Vec::new();
            if !self.peek_op("}") {
                loop {
                    let Some(key) = self.eat_object_key() else { return Err(s_help(
                        "P1201",
                        "Expected an object key here",
                        "Add a property name before ':': name: \"John\"",
                    )); };
                    while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    if !self.eat_op(":") { return Err(s_help("P1202", "You need a ':' after the object key", "Write it like: name: \"John\"")); }
                    while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                    let value = self.parse_assign()?; props.push((key, value));
                    if self.eat_op(",") {
                        while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                        if self.peek_op("}") { break; }
                        continue;
                    }
                    break;
                }
            }
            while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
            if !self.eat_op("}") { return Err(s_help("P1203", "Expected '}' to close this object", "Add the closing '}': { name: \"John\" }")); }
            return Ok(self.apply_postfix_ops(PExpr::Object(props)));
        }

        // --- Lit / Ident dispatch ---
        let (kind, val_opt) = match self.peek() { Some(t) => (t.kind.clone(), t.value.clone()), None => return Err(s_help("P1003", "Expected an expression here", "Use a value, variable, or call: total = price * qty")) };
        let expr = match kind {
            // MONEY
            goblin_lexer::TokenKind::Money => {
                let lit = val_opt.unwrap_or_default();
                self.i += 1;
                PExpr::Money(lit)
            }

            goblin_lexer::TokenKind::Int => {
                let lit = val_opt.unwrap_or_default(); self.i += 1;
                if let Some(t) = self.peek() {
                    if t.kind == goblin_lexer::TokenKind::Op("unit".into()) { let unit = t.value.clone().unwrap_or_default(); self.i += 1; PExpr::IntWithUnit(lit, unit) }
                    else { PExpr::Int(lit) }
                } else { PExpr::Int(lit) }
            }
            goblin_lexer::TokenKind::Float => {
                let lit = val_opt.unwrap_or_default(); self.i += 1;
                if let Some(t) = self.peek() {
                    if t.kind == goblin_lexer::TokenKind::Op("unit".into()) { let unit = t.value.clone().unwrap_or_default(); self.i += 1; PExpr::FloatWithUnit(lit, unit) }
                    else { PExpr::Float(lit) }
                } else { PExpr::Float(lit) }
            }
            goblin_lexer::TokenKind::Duration => {
                let lit = val_opt.unwrap_or_default(); self.i += 1;
                let (base, unit) = Self::split_duration_lexeme(&lit)?;
                let is_floaty = base.contains('.') || base.contains('e') || base.contains('E');
                if is_floaty { PExpr::FloatWithUnit(base, unit) } else { PExpr::IntWithUnit(base, unit) }
            }
            goblin_lexer::TokenKind::String => {
                let lit = self.toks[self.i].value.clone().unwrap_or_default(); self.i += 1;
                if !lit.as_bytes().contains(&b'{') { PExpr::Str(lit) } else {
                    let b = lit.as_bytes(); let mut parts: Vec<StrPart> = Vec::new(); let mut text = String::new(); let mut i: usize = 0; let n = b.len();
                    while i < n {
                        if i + 4 < n && &b[i..i + 5] == b"{{/}}" { text.push('}'); i += 5; continue; }
                        if b[i] == b'{' {
                            if !text.is_empty() { parts.push(StrPart::Text(std::mem::take(&mut text))); }
                            let mut depth: usize = 1; let start = i + 1; i += 1;
                            while i < n && depth > 0 { match b[i] { b'{' => depth += 1, b'}' => depth -= 1, _ => {} } i += 1; }
                            if depth != 0 { return Err(s_help("P0604", "There's an unclosed '{' in this interpolated string.", "Add a matching '}' to close it, e.g., \"Hello {name}\".")); }
                            let end = i - 1; let inner = &lit[start..end]; let part = self.parse_interpolation_lvalue(inner)?; parts.push(part); continue;
                        }
                        text.push(b[i] as char); i += 1;
                    }
                    if !text.is_empty() { parts.push(StrPart::Text(text)); }
                    PExpr::StrInterp(parts)
                }
            }
            goblin_lexer::TokenKind::Date => {
                self.i += 1; self.skip_newlines();
                let (k2, v2) = match self.peek() { Some(t2) => (t2.kind.clone(), t2.value.clone()), None => return Err(s_help("P1301", "Expected a string after 'date'.", "Write it like: date \"2023-12-25\".")) };
                if !matches!(k2, goblin_lexer::TokenKind::String) { return Err(s_help("P1301", "Expected a string after 'date'", "Write it like: date \"2023-12-25\"")); }
                let lit = v2.unwrap_or_default(); self.i += 1; PExpr::Date(lit)
            }
            goblin_lexer::TokenKind::Time => {
                self.i += 1; self.skip_newlines();
                let (k2, v2) = match self.peek() { Some(t2) => (t2.kind.clone(), t2.value.clone()), None => return Err(s_help("P1302", "Expected a string after 'time'", "Write it like: time \"14:30:00\"")) };
                if !matches!(k2, goblin_lexer::TokenKind::String) { return Err(s_help("P1302", "Expected a string after 'time'", "Write it like: time \"14:30:00\"")); }
                let lit = v2.unwrap_or_default(); self.i += 1; PExpr::Time(lit)
            }
            goblin_lexer::TokenKind::DateTime => {
                self.i += 1; self.skip_newlines();
                let (k2, v2) = match self.peek() { Some(t2) => (t2.kind.clone(), t2.value.clone()), None => return Err(s_help("P1303", "Expected a string after 'datetime'.", "Write it like: datetime \"2023-12-25T14:30:00\".")) };
                if !matches!(k2, goblin_lexer::TokenKind::String) { return Err(s_help("P1303", "Expected a string after 'datetime'", "Write it like: datetime \"2023-12-25T14:30:00\"")); }
                let value = v2.unwrap_or_default(); self.i += 1; self.skip_newlines();
                let mut tz: Option<String> = None;
                if let Some(tz_tok) = self.peek() {
                    if matches!(tz_tok.kind, goblin_lexer::TokenKind::Ident) && tz_tok.value.as_deref() == Some("tz") {
                        self.i += 1; self.skip_newlines();
                        if !self.eat_op(":") { return Err(s_help("P1304", "You need a ':' after tz", "Write it like: tz: \"UTC\"")); }
                        self.skip_newlines();
                        let (k3, v3) = match self.peek() { Some(t3) => (t3.kind.clone(), t3.value.clone()), None => return Err(s_help("P1305", "Expected a string after tz:", "Write it like: tz: \"UTC\"")) };
                        if !matches!(k3, goblin_lexer::TokenKind::String) { return Err(s_help("P1305", "Expected a string after tz:", "Write it like: tz: \"UTC\"")); }
                        tz = Some(v3.unwrap_or_default()); self.i += 1;
                    }
                }
                PExpr::DateTime { value, tz }
            }
            goblin_lexer::TokenKind::Ident => {
                let name = val_opt.unwrap_or_default(); self.i += 1;
                match name.as_str() { "true" => PExpr::Bool(true), "false" => PExpr::Bool(false), "nil" => PExpr::Nil, _ => PExpr::Ident(name) }
            }
            // Catch-all for orphaned operators at primary position: consume + error (prevents loops).
            goblin_lexer::TokenKind::Op(ref s) => {
                let sp = self.toks[self.i].span.clone();
                self.i += 1;
                return Err(s_help(
                    "P1006",
                    &format!("Expected an expression, but found operator '{}' at line {}, col {}", s, sp.line_start, sp.col_start),
                    "Add a value or name before the operator: total = price * qty (not * qty)",
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
        let mut lhs = self.parse_or()?;

        while self.eat_op("??") {
            self.skip_newlines();
            let rhs = self.parse_or()?;
            lhs = PExpr::Binary(Box::new(lhs), "??".into(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_or(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_and()?;
        loop {
            if self.peek_ident() == Some("or") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let rhs = self.parse_and()?;
                lhs = PExpr::Binary(Box::new(lhs), "or".into(), Box::new(rhs));
                continue;
            }
            break;
        }
        Ok(lhs)
    }
    
    fn parse_and(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_compare()?;
        loop {
            if self.peek_ident() == Some("and") {
                let _ = self.eat_ident();
                self.skip_newlines();
                let rhs = self.parse_compare()?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            // support && alias if your lexer emits it as Op("&&")
            if self.eat_op("&&") {
                self.skip_newlines();
                let rhs = self.parse_compare()?;
                lhs = PExpr::Binary(Box::new(lhs), "and".into(), Box::new(rhs));
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn parse_compare(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_additive()?;

        loop {
            // textual: is / is not
            if self.peek_ident() == Some("is") {
                let _ = self.eat_ident();
                let neg = self.peek_ident() == Some("not");
                if neg { let _ = self.eat_ident(); }
                self.skip_newlines();
                let rhs = self.parse_additive()?;
                let op = if neg { "is not" } else { "is" };
                lhs = PExpr::Binary(Box::new(lhs), op.into(), Box::new(rhs));
                continue;
            }

            if self.eat_op("===")  { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "===".into(), Box::new(rhs)); continue; }
            if self.eat_op("!==")  { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "!==".into(), Box::new(rhs)); continue; }
            if self.eat_op("==")   { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "==".into(),  Box::new(rhs)); continue; }
            if self.eat_op("!=")   { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "!=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<=")   { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "<=".into(),  Box::new(rhs)); continue; }
            if self.eat_op(">=")   { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), ">=".into(),  Box::new(rhs)); continue; }
            if self.eat_op("<")    { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), "<".into(),   Box::new(rhs)); continue; }
            if self.eat_op(">")    { self.skip_newlines(); let rhs = self.parse_additive()?; lhs = PExpr::Binary(Box::new(lhs), ">".into(),   Box::new(rhs)); continue; }

            break;
        }

        Ok(lhs)
    }

    fn parse_range(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_join()?;

        loop {
            let op = if self.eat_op("...") { "..." }
            else if self.eat_op("..") { ".." }
            else { break };

            let rhs = self.parse_join()?;
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_join(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_additive()?;

        loop {
            if self.eat_op("||") {
                let rhs = self.parse_additive()?;
                lhs = PExpr::Binary(Box::new(lhs), "||".to_string(), Box::new(rhs));
                continue;
            } else if self.eat_op("|") {
                let rhs = self.parse_additive()?;
                lhs = PExpr::Binary(Box::new(lhs), "|".to_string(), Box::new(rhs));
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    fn parse_additive(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_multiplicative()?;

        loop {
            if self.eat_op("+") {
                self.skip_newlines();
                let rhs = self.parse_multiplicative()?;
                lhs = PExpr::Binary(Box::new(lhs), "+".into(), Box::new(rhs));
            } else if self.eat_op("-") {
                self.skip_newlines();
                let rhs = self.parse_multiplicative()?;
                lhs = PExpr::Binary(Box::new(lhs), "-".into(), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_multiplicative(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_power()?;

        loop {
            let op = if self.eat_op("><") { "><" }      // divmod
                     else if self.eat_op("//") { "//" } // integer division
                     else if self.eat_op("*")  { "*"  }
                     else if self.eat_op("/")  { "/"  }
                     else if self.eat_op("%")  { "%"  }
                     else { break };

            self.skip_newlines();
            let rhs = self.parse_power()?;
            lhs = PExpr::Binary(Box::new(lhs), op.to_string(), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_power(&mut self) -> Result<PExpr, String> {
        let lhs = self.parse_unary()?;

        if self.eat_op("**") {
            self.skip_newlines();
            let rhs = self.parse_power()?;
            return Ok(PExpr::Binary(Box::new(lhs), "**".into(), Box::new(rhs)));
        }
        if self.eat_op("^^") {
            self.skip_newlines();
            let rhs = self.parse_power()?;
            return Ok(PExpr::Binary(Box::new(lhs), "^^".into(), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn parse_kv_bind_list_judge(&mut self) -> Result<Vec<(String, PExpr)>, String> {
        use goblin_lexer::TokenKind;
        // Capture the header column for alignment (assume called after 'judge'/'judge_all')
        let hdr_tok_i = self.i.saturating_sub(1);
        let hdr_col = self.toks[hdr_tok_i].span.col_start;
        let mut out: Vec<(String, PExpr)> = Vec::new();
        loop {
            // Stop when we see a block closer 'end' or '}' (caller will consume it)
            self.skip_newlines();
            if self.eat_layout_until_close(hdr_col) || self.peek_block_close() {
                break;
            }
            // Check for EOF
            if self.is_eof() {
                return Err(s_help(
                    "P0212",
                    "This judge block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
            // Capture everything from here up to the first ':' at nesting level 0
            let start_i = self.i;
            let mut depth_paren = 0i32;
            let mut depth_brack = 0i32;
            let mut depth_brace = 0i32;
            let mut end_i: Option<usize> = None;
            while let Some(tok) = self.toks.get(self.i) {
                match &tok.kind {
                    TokenKind::Op(op) if op == ":" => {
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 {
                            end_i = Some(self.i);
                            break;
                        }
                        // ':' inside nested ()/[]/{} – part of the expression
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == "(" => {
                        depth_paren += 1;
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == ")" => {
                        depth_paren -= 1;
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == "[" => {
                        depth_brack += 1;
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == "]" => {
                        depth_brack -= 1;
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == "{" => {
                        depth_brace += 1;
                        self.i += 1;
                    }
                    TokenKind::Op(op) if op == "}" => {
                        depth_brace -= 1;
                        self.i += 1;
                    }
                    // Treat layout as whitespace while searching for the first ':' at top level
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                        if depth_paren == 0 && depth_brack == 0 && depth_brace == 0 =>
                    {
                        self.i += 1;
                    }
                    // Any other token – keep scanning
                    _ => {
                        self.i += 1;
                    }
                }
            }
            let Some(colon_i) = end_i else {
                return Err(s_help(
                    "P0811",
                    "You need ':' after the condition in 'judge'",
                    "Write it like: judge x > 5: \"big\"",
                ));
            };
            // Build a readable key string from tokens [start_i .. colon_i)
            let mut parts: Vec<String> = Vec::new();
            for t in &self.toks[start_i..colon_i] {
                match &t.kind {
                    TokenKind::Op(op) => parts.push(op.clone()),
                    _ => parts.push(t.value.clone().unwrap_or_default()),
                }
            }
            // Simple join with spaces; keeps operators recognizable
            let key_text = parts.join(" ").trim().to_string();
            // Consume the ':'
            self.i = colon_i;
            let _ = self.eat_op(":");
            // Optional layout after ':'
            self.skip_newlines();
            self.eat_layout_until_close(0); // Handle Dedent after ':'
            // Parse value expression
            let val = self.parse_assign()?;
            out.push((key_text, val));
            // Allow trailing commas or line breaks between pairs
            self.eat_semi_separators();
            if self.peek_block_close() {
                break;
            }
        }
        Ok(out)
    }

    fn parse_unary(&mut self) -> Result<PExpr, String> {
        use goblin_lexer::TokenKind;
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
                    return Err(s_help(
                        "P0401",
                        "You need a variable, field access, or array index after '&'",
                        "Examples: &user, &user>>name, &items[0]",
                    ));
                }
            }
        }
        // expression-form: judge …
        if let Some("judge") = self.peek_ident() {
            let header_tok_i = self.i;
            let header_line = self.toks[header_tok_i].span.line_start;
            let header_col = self.toks[header_tok_i].span.col_start;
            let _ = self.eat_ident(); // 'judge'
            if self.peek_op("{") {
                return Err(s_help(
                    "P0812",
                    "Don't put '{' after 'judge'",
                    "Use indentation and close with 'end' or 'xx' (crossbones): judge x > 5: \"big\" end",
                ));
            }
            self.forbid_next_line_brace(header_line, header_col, "judge")?;
            self.skip_newlines();
            self.eat_layout_until_close(header_col); // Handle Dedent before parsing pairs
            let pairs = self.parse_kv_bind_list_judge()?;
            self.skip_newlines();
            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != header_col {
                    let closer = self.peek_ident().unwrap_or("}");
                    return Err(s_help(
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
                return Err(s_help(
                    "P0212",
                    "This judge block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
            return Ok(PExpr::Object(pairs));
        }
        // expression-form: judge_all …
        if let Some("judge_all") = self.peek_ident() {
            let header_tok_i = self.i;
            let header_line = self.toks[header_tok_i].span.line_start;
            let header_col = self.toks[header_tok_i].span.col_start;
            let _ = self.eat_ident(); // 'judge_all'
            if self.peek_op("{") {
                return Err(s_help(
                    "P0813",
                    "Don't put '{' after 'judge_all'",
                    "Use indentation and close with 'end' or 'xx' (crossbones): judge_all x > 5: \"big\" end",
                ));
            }
            self.forbid_next_line_brace(header_line, header_col, "judge_all")?;
            self.skip_newlines();
            self.eat_layout_until_close(header_col); // Handle Dedent before parsing pairs
            let pairs = self.parse_kv_bind_list_judge()?;
            self.skip_newlines();
            if self.peek_block_close() {
                let col = self.toks.get(self.i).map(|t| t.span.col_start).unwrap_or(0);
                if col != header_col {
                    let closer = self.peek_ident().unwrap_or("}");
                    return Err(s_help(
                        "P0222",
                        &format!(
                            "This '{}' closer is misaligned: expected column {}, found column {}",
                            closer, header_col, col
                        ),
                        "Align the closer with its header (same column): place 'end' or 'xx' (crossbones) directly under the start of the judge_all header.",
                    ));
                }
                self.expect_block_close("judge_all")?;
            } else if !self.eat_layout_until_close(header_col) {
                return Err(s_help(
                    "P0212",
                    "This judge_all block is missing its closing 'end' or 'xx' (crossbones).",
                    "Close the block with 'end' or 'xx' (crossbones).",
                ));
            }
            return Ok(PExpr::Object(pairs));
        }
        // PICK (expression form)
        // Syntax: pick <count:int> from <expr> [with dups]
        if self.peek_ident() == Some("pick") {
            let pick_tok_i = self.i; // kept for potential future diagnostics
            let _ = self.eat_ident(); // 'pick'
            self.skip_newlines();
            // <count>: integer literal (supports suffixes)
            let count = match self.peek() {
                Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Int) => {
                    let s = t.value.clone().unwrap_or_default();
                    if let Some(v) = parse_int_literal_to_i128(&s) {
                        self.i += 1;
                        v
                    } else {
                        return Err(s_help(
                            "P1401",
                            "You need a number after 'pick'",
                            "Write it like: pick 5 from items",
                        ));
                    }
                }
                _ => {
                    return Err(s_help(
                        "P1401",
                        "You need a number after 'pick'",
                        "Write it like: pick 5 from items",
                    ))
                }
            };
            if count < 0 {
                return Err(s_help(
                    "P1402",
                    "You can't pick a negative number of items",
                    "Use a positive number: pick 3 from items",
                ));
            }
            self.skip_newlines();
            // 'from'
            if self.peek_ident() != Some("from") {
                return Err(s_help(
                    "P1403",
                    "Expected 'from' after the pick count",
                    "Write it like: pick 3 from items",
                ));
            }
            let _ = self.eat_ident(); // 'from'
            self.skip_newlines();
            // <expr> (iterable/source expression)
            let src_expr = self.parse_coalesce()?;
            self.skip_newlines();
            // Optional: 'with dups'
            let mut with_dups = false;
            if self.peek_ident() == Some("with") {
                let _ = self.eat_ident(); // 'with'
                self.skip_newlines();
                if self.peek_ident() == Some("dups") {
                    let _ = self.eat_ident(); // 'dups'
                    with_dups = true;
                    self.skip_newlines();
                } else {
                    return Err(s_help(
                        "P1404",
                        "Expected 'dups' after 'with'",
                        "Write it like: pick 5 from items with dups",
                    ));
                }
            }
            // Compile-time sanity check: if src is a literal array and no 'with dups',
            // ensure count <= number of DISTINCT literal elements.
            if !with_dups {
                if let PExpr::Array(ref elems) = src_expr {
                    use std::collections::HashSet;
                    let mut set: HashSet<String> = HashSet::new();
                    let mut all_simple = true;
                    for e in elems {
                        match e {
                            PExpr::Int(s) | PExpr::Float(s) | PExpr::Str(s) => {
                                // Normalize underscores so 1_0 == 10
                                let mut norm = String::with_capacity(s.len());
                                for ch in s.chars() {
                                    if ch != '_' {
                                        norm.push(ch);
                                    }
                                }
                                set.insert(norm);
                            }
                            PExpr::Bool(b) => {
                                set.insert(format!("b{}", b));
                            }
                            _ => {
                                all_simple = false;
                                break;
                            }
                        }
                    }
                    if all_simple {
                        let distinct = set.len() as i128;
                        if count > distinct {
                            return Err(s_help(
                                "P1405",
                                &format!(
                                    "You're trying to pick {} items, but there are only {} distinct items available",
                                    count, distinct
                                ),
                                "Add 'with dups' to allow repeats: pick 5 from items with dups, or pick fewer items",
                            ));
                        }
                    }
                }
            }
            // Placeholder expression node; matches how 'judge' returns directly from parse_unary.
            return Ok(PExpr::Ident("pick_expr".into()));
        }
        // roll / roll_detail (syntax-only; contiguous dice)
        if let Some(id) = self.peek_ident() {
            if id == "roll" || id == "roll_detail" {
                let is_detail = id == "roll_detail";
                let _ = self.eat_ident();
                let count_tok_i = match self.peek() {
                    Some(t) if matches!(t.kind, TokenKind::Int) => {
                        let idx = self.i;
                        self.i += 1;
                        idx
                    }
                    _ => {
                        return Err(s_help(
                            "P1501",
                            "You need a number before 'd' in a roll",
                            "Example: roll 2d6",
                        ))
                    }
                };
                // contiguous dNNN
                let die_tok_i = match self.peek_ident() {
                    Some(s) if s.len() > 1
                        && s.starts_with('d')
                        && s[1..].chars().all(|c| c.is_ascii_digit()) =>
                    {
                        let prev = &self.toks[count_tok_i].span;
                        let cur = &self.toks[self.i].span;
                        if !(cur.line_start == prev.line_end && cur.col_start == prev.col_end) {
                            return Err(s_help(
                                "P1502",
                                "Dice notation must be contiguous",
                                "Write it like 2d6, not 2 d 6",
                            ));
                        }
                        let idx = self.i;
                        self.i += 1;
                        idx
                    }
                    _ => {
                        return Err(s_help(
                            "P1502",
                            "Dice notation must be contiguous.",
                            "Write it like 2d6, not 2 d 6.",
                        ))
                    }
                };
                // optional contiguous +Z / -Z
                let mut has_mod = false;
                let mut mod_sign_i = 0usize;
                if self.peek_op("+") || self.peek_op("-") {
                    let prev = &self.toks[die_tok_i].span;
                    let cur = &self.toks[self.i].span;
                    if !(cur.line_start == prev.line_end && cur.col_start == prev.col_end) {
                        return Err(s_help(
                            "P1503",
                            "Don't put spaces in dice notation modifiers",
                            "Write it like 2d6+3, not 2d6 + 3",
                        ));
                    }
                    mod_sign_i = self.i;
                    self.i += 1;
                    has_mod = true;
                    match self.peek() {
                        Some(t) if matches!(t.kind, TokenKind::Int) => {
                            let cur2 = &t.span;
                            let prev2 = &self.toks[mod_sign_i].span;
                            if !(cur2.line_start == prev2.line_end && cur2.col_start == prev2.col_end) {
                                return Err(s_help(
                                    "P1503",
                                    "Don't put spaces in dice notation modifiers",
                                    "Write it like 2d6+3, not 2d6 + 3",
                                ));
                            }
                            self.i += 1;
                        }
                        _ => {
                            return Err(s_help(
                                "P1504",
                                "You need a number after '+' or '-' in a dice notation roll",
                                "Examples: 2d6+3 or 1d20-1",
                            ))
                        }
                    }
                }
                if !has_mod && self.peek_op("+") {
                    return Err(s_help(
                        "P1504",
                        "You need a number after '+' in a dice notation roll",
                        "Write it like 2d6+3",
                    ));
                }
                if !has_mod && self.peek_op("-") {
                    return Err(s_help(
                        "P1504",
                        "You need a number after '-' in a dice notation roll",
                        "Write it like 1d20-1",
                    ));
                }
                let name = if is_detail { "roll_detail_expr" } else { "roll_expr" };
                return Ok(PExpr::Ident(name.into()));
            }
        }
        // logical-not
        if self.eat_op("!") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("!".into(), Box::new(rhs)));
        }
        // unary +/-
        if self.eat_op("+") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("+".into(), Box::new(rhs)));
        }
        if self.eat_op("-") {
            let rhs = self.parse_unary()?;
            return Ok(PExpr::Prefix("-".into(), Box::new(rhs)));
        }
        // hand off
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_member()?;

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
                if !self.eat_op(")") { return Err(s_help("P0505", "Expected ')' to close this action call", "Add the closing ')': calculate(price, tax)")); }

                lhs = PExpr::Call(Box::new(lhs), "()".to_string(), args);
                continue;
            }

            // --- indexing / slicing ---
            if self.eat_op("[") {
                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if self.eat_op("]") { return Err(s_help("P0701", "Brackets need an index or slice expression", "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]")); }

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

                    if !self.eat_op("]") { return Err(s_help("P0709", "Expected ']' to close this slice", "Add the closing ']': items[1:4]")); }

                    lhs = if step.is_some() {
                        PExpr::Slice3(Box::new(lhs), start.map(Box::new), end.map(Box::new), step.map(Box::new))
                    } else {
                        PExpr::Slice(Box::new(lhs), start.map(Box::new), end.map(Box::new))
                    };
                    continue;
                }

                while matches!(self.toks.get(self.i), Some(tok) if matches!(tok.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if !self.eat_op("]") { return Err(s_help("P0702", "Expected ']' to close this index", "Add the closing ']': items[0]")); }
                let idx = start.expect("index expression parsed");
                lhs = PExpr::Index(Box::new(lhs), Box::new(idx));
                continue;
            }

            // --- postfix ops ---
            // Disambiguate "**" and "//": treat as *postfix* only when they are not followed by an expression head,
            // so "a ** b" / "a // b" remain binary at higher precedence levels.
            if self.peek_op("**") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("**"); lhs = PExpr::Postfix(Box::new(lhs), "**".to_string()); continue; }
            if self.peek_op("//") && !lookahead_starts_expr(self.i + 1) { let _ = self.eat_op("//"); lhs = PExpr::Postfix(Box::new(lhs), "//".to_string()); continue; }

            if self.eat_op("++") { lhs = PExpr::Postfix(Box::new(lhs), "++".to_string()); continue; }
            if self.eat_op("--") { lhs = PExpr::Postfix(Box::new(lhs), "--".to_string()); continue; }
            if self.eat_op("?")  { lhs = PExpr::IsBound(Box::new(lhs));                     continue; }
            if self.eat_op("!")  { lhs = PExpr::Postfix(Box::new(lhs), "!".to_string());   continue; }
            if self.eat_op("^")  { lhs = PExpr::Postfix(Box::new(lhs), "^".to_string());   continue; }
            if self.eat_op("_")  { lhs = PExpr::Postfix(Box::new(lhs), "_".to_string());   continue; }

            // nothing matched; ensure progress or bail
            if self.i == start_i { break; }
        }

        Ok(lhs)
    }

    fn parse_member(&mut self) -> Result<PExpr, String> {
        let mut lhs = self.parse_primary()?;

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
                    return Err(s_help("P0701", "Brackets need an index or slice expression", "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]"));
                }

                // Parse start / end / step using ":" separators
                let mut start: Option<PExpr> = None;
                let mut end:   Option<PExpr> = None;
                let mut step:  Option<PExpr> = None;

                // start is present iff the next token isn't ":" (or "]" which we rejected above)
                if !self.peek_op(":") {
                    start = Some(self.parse_coalesce()?);
                }

                // If we see a ':', we're in slice mode; otherwise it's an index
                let is_slice = self.eat_op(":");
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

                // Close bracket
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if !self.eat_op("]") {
                    self.suspend_colon_call -= 1;
                    return Err(s_help(
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
                    let idx = start.ok_or_else(|| s_help(
                        "P0701",
                        "Brackets need an index or slice expression",
                        "Write something inside the brackets: items[0], data[1:5], or list[2:8:2]",
                    ))?;
                    PExpr::Index(Box::new(lhs), Box::new(idx))
                };
                continue;
            }

            // ---------- member: >> name | >> "string" ----------
            if self.eat_op(">>") {
                if let Some(name) = self.eat_ident()      { lhs = PExpr::Member(Box::new(lhs), name); continue; }
                if let Some(key)  = self.eat_string_lit() { lhs = PExpr::Member(Box::new(lhs), key ); continue; }
                return Err(s_help(
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
                    if !self.eat_op(")") { return Err(s_help("P0506", "Expected ')' to close this optional call", "Add the closing ')': user?>>getName()")); }
                    lhs = match lhs {
                        PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                        other                       => PExpr::OptCall(Box::new(other), String::new(), args),
                    };
                    continue;
                } else {
                    // ?>> name  — optional member
                    let Some(name) = self.eat_ident() else { return Err(s_help(
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
                // Dot is for calling only. It must be followed by an identifier.
                let Some(opname) = self.eat_ident() else {
                    return Err(s_help(
                        "P0504",
                        "Expected a callable name after '.'",
                        "Use '.' only to call: Player.take_damage(30) or Player.status",
                    ));
                };

                // If '(' follows, parse a normal argument list; otherwise it's a zero-arg call.
                if self.eat_op("(") {
                    let args = if self.eat_op(")") { vec![] } else { self.parse_args_paren()? };
                    lhs = PExpr::Call(Box::new(lhs), opname, args);
                } else {
                    // Zero-arg sugar: obj.foo  ==  obj.foo()
                    lhs = PExpr::Call(Box::new(lhs), opname, vec![]);
                }
                continue;
            }

            // ---------- colon-call: target : arg, arg, ... ----------
            if self.suspend_colon_call == 0 && self.eat_op(":") {
                let mut args = Vec::new();
                while matches!(self.toks.get(self.i), Some(t) if matches!(t.kind, goblin_lexer::TokenKind::Newline)) { self.i += 1; }
                if self.peek_newline_or_eof() { return Err(s_help("P0507", "Expected an argument after ':'", "Add at least one argument after ':': calculate: price, tax")); }

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
                    other                       => return Err(s_help(
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
                if !self.eat_op(")") { return Err(s_help("P0509", "Expected ')' after the argument list", "Add the closing ')': calculate(price, tax)")); }

                lhs = match lhs {
                    PExpr::Member(obj, name)    => PExpr::Call(obj, name, args),
                    PExpr::OptMember(obj, name) => PExpr::OptCall(obj, name, args),
                    PExpr::Ident(name)          => PExpr::FreeCall(name, args),
                    other                       => PExpr::Call(Box::new(other), String::new(), args),
                };
                continue;
            }

            // ---------- namespaced free call: Ns::func(...) ----------
            if matches!(&lhs, PExpr::Ident(_)) && self.peek_op("::") {
                let ns = if let PExpr::Ident(ref s) = lhs { s.clone() } else { unreachable!() };
                let _ = self.eat_op("::");
                let Some(opname) = self.eat_ident() else { return Err(s_help(
                    "P0510",
                    "Expected a name after '::'",
                    "Write it like: module_name::Action",
                )); };

                if self.eat_op("(") {
                    let args = if self.eat_op(")") { vec![] } else { self.parse_args_paren()? };
                    lhs = PExpr::NsCall(ns, opname, args);
                } else if self.eat_op(":") {
                    let args = self.parse_args_colon()?;
                    lhs = PExpr::NsCall(ns, opname, args);
                } else {
                    lhs = PExpr::NsCall(ns, opname, vec![]);
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
                return Err(s_help(
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

        if p.peek_op("@") {
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
