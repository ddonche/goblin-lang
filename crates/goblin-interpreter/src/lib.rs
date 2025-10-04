// goblin-interpreter/src/lib.rs
//! Stage 1–3 interpreter on the real Goblin AST.
//!
//! Implemented (per your repo so far):
//! - literals: numbers/strings/bools/nil  (escapes handled by lexer)
//! - unary +/- and logical not: "!" and "not"
//! - postfix: %, !, ^ (ceil), _ (floor), ** (square), // (sqrt)
//! - infix math: + - * / // % ** ><
//! - percent family: N% literal, `N %o X` == (N/100)*X, `N% of X` == (N/100)*X (parser lowers "of")
//! - comparisons: == != < <= > >= (num/num or str/str)
//! - logic: and/or (aliases &&, <>), coalesce ??
//! - strings: "a" + "b", and "a" ++ "b" (space-join, stringifies rhs/lhs)
//! - string interpolation at runtime: "Hello {name}" with {{ and }} escapes
//! - vars: `name = expr`, compound: += -= *= /= //= %= **=
//! - arrays & maps: [..], {k: v}; indexing: arr[i], map["key"]
//! - member access (from parser): Member/OptMember work on maps (name key); OptMember nil-propagates
//! - free calls: v(n), say(x)
//! - control flow (parser lowers to free calls):
//!     if(cond) { then[] [, else[]] } -> FreeCall("if", [cond, then[], else?[]])
//!     while(cond) { body[] }         -> FreeCall("while", [cond, body[]])
//!
//! Not implemented yet:
//! - namespaced/receiver calls (Call/OptCall/NsCall) -> clear "not implemented" diag
//! - classes/actions execution semantics beyond registering free actions (Stage 4)

use std::collections::BTreeMap;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use goblin_ast as ast;
use goblin_diagnostics::Span;
use serde_json as sj;
use goblin_ast::BindMode;
use rust_decimal::Decimal;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use std::str::FromStr;

const F64_SAFE_INT_MAX: i64 = 9_007_199_254_740_992; // for reference 

// ===================== Public API =====================

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeqKind { Auto, Array, List, Chunked }

#[derive(Clone, Debug, Default, PartialEq)]   // <-- add PartialEq
pub struct SeqMetrics {
    pub len: usize,
    pub ops_push_back: u64,
    pub ops_push_front: u64,
    pub ops_insert_idx: u64,
    pub ops_remove_idx: u64,
    pub ops_random_access: u64,
    pub transitions: u64,
}

#[derive(Clone, Debug, PartialEq)]            // <-- add PartialEq
enum SeqBackend {
    Array(Vec<Value>),
    // List(ListCore),
    // Chunked(ChunkCore),
}

#[derive(Clone, Debug, PartialEq)]            // <-- add PartialEq
pub struct Seq {
    backend: SeqBackend,
    kind_hint: SeqKind,
    metrics: SeqMetrics,
}

impl Seq {
    pub fn get(&self, i: usize) -> Option<&Value> {
        match &self.backend {
            SeqBackend::Array(vs) => {
                // metrics
                // (optional) self.metrics.ops_random_access += 1;  // if metrics is &mut; otherwise omit
                vs.get(i)
            }
        }
    }

    pub fn set(&mut self, i: usize, v: Value) -> Result<(), ()> {
        match &mut self.backend {
            SeqBackend::Array(vs) => {
                if i < vs.len() {
                    vs[i] = v;
                    // metrics
                    self.metrics.ops_random_access += 1;
                    Ok(())
                } else {
                    Err(())
                }
            }
        }
    }

    pub fn from_vec(v: Vec<Value>) -> Self {
        Seq {
            metrics: SeqMetrics { len: v.len(), ..Default::default() },
            backend: SeqBackend::Array(v),
            kind_hint: SeqKind::Auto,
        }
    }

    #[inline] pub fn len(&self) -> usize { self.metrics.len }

    // Read-only snapshot view used by existing code paths (sum, avg, etc.).
    pub fn as_slice(&self) -> Option<&[Value]> {
        match &self.backend {
            SeqBackend::Array(v) => Some(v.as_slice()),
            //_ => None, // later variants
        }
    }

    // Materialize: used by sort/print until we migrate all call sites.
    pub fn to_vec(&self) -> Vec<Value> {
        match &self.backend {
            SeqBackend::Array(v) => v.clone(),
        }
    }

    // Mutating ops (used by future “!” forms; safe no-ops for now):
    pub fn push_back(&mut self, v: Value) {
        self.metrics.ops_push_back += 1;
        match &mut self.backend {
            SeqBackend::Array(vs) => { vs.push(v); self.metrics.len = vs.len(); }
        }
        self.maybe_rebucket();
    }

    pub fn insert(&mut self, i: usize, v: Value) -> Result<(), ()> {
        self.metrics.ops_insert_idx += 1;
        match &mut self.backend {
            SeqBackend::Array(vs) => {
                if i > vs.len() { return Err(()); }
                vs.insert(i, v);
                self.metrics.len = vs.len();
            }
        }
        self.maybe_rebucket();
        Ok(())
    }

    pub fn remove(&mut self, i: usize) -> Option<Value> {
        self.metrics.ops_remove_idx += 1;
        let out = match &mut self.backend {
            SeqBackend::Array(vs) => {
                if i < vs.len() { Some(vs.remove(i)) } else { None }
            }
        };
        if out.is_some() {
            self.metrics.len = self.metrics.len.saturating_sub(1);
        }
        self.maybe_rebucket();
        out
    }

    // Heuristics hook; no-op in Phase 0
    pub fn maybe_rebucket(&mut self) {
        // later: switch Array <-> List <-> Chunked + hysteresis
    }

    pub fn backend_name(&self) -> &'static str {
        match &self.backend {
            SeqBackend::Array(_) => "array",
        }
    }

    pub fn metrics_map(&self) -> BTreeMap<String, Value> {
        let mut m = BTreeMap::new();
        m.insert("len".into(), Value::Int(self.metrics.len as i64));
        m.insert("ops_push_back".into(), Value::Int(self.metrics.ops_push_back as i64));
        m.insert("ops_push_front".into(), Value::Int(self.metrics.ops_push_front as i64));
        m.insert("ops_insert_idx".into(), Value::Int(self.metrics.ops_insert_idx as i64));
        m.insert("ops_remove_idx".into(), Value::Int(self.metrics.ops_remove_idx as i64));
        m.insert("ops_random_access".into(), Value::Int(self.metrics.ops_random_access as i64));
        m.insert("transitions".into(), Value::Int(self.metrics.transitions as i64));
        m.insert("backend".into(), Value::Str(self.backend_name().into()));
        m
    }
}

// A tiny read-only view to bridge old call sites:
pub enum SeqView<'a> {
    Slice(&'a [Value]),
    // Iter(Box<dyn Iterator<Item=&'a Value> + 'a>), // later
}
impl<'a> SeqView<'a> {
    pub fn iter(&'a self) -> Box<dyn Iterator<Item = &'a Value> + 'a> {
        match self {
            SeqView::Slice(s) => Box::new(s.iter()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Big(Decimal),
    Str(String),
    Char(char),
    Bool(bool),
    Pct(f64),
    Formatted(Box<Value>, FormatSpec),
    Array(Vec<Value>),
    Map(BTreeMap<String, Value>),
    Pair(Box<Value>, Box<Value>), // for >< (divmod)
    Seq(Seq),
    Nil,
    Unit,
    CtrlSkip,  
    CtrlStop,
    Object {
        class_name: String,
        fields: BTreeMap<String, Value>,
    },
    Enum {                              // <- ADD THIS
        enum_name: String,
        variant_name: String,
        fields: Option<BTreeMap<String, Value>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormatSpec {
    pub decimals: u32,                  // how many decimals to display
    pub sep_thousands: Option<char>,    // ',', '.', '_', '\'' or None
    pub sep_decimal: char,              // '.' or ','
}

fn fmt_string_visible(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"'  => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => out.push_str(&format!("\\u{{{:x}}}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn fmt_char_visible(c: char) -> String {
    match c {
        '\\' => "'\\\\'".into(),
        '\'' => "'\\''".into(),
        '\n' => "'\\n'".into(),
        '\r' => "'\\r'".into(),
        '\t' => "'\\t'".into(),
        c if c.is_control() => format!("'\\u{{{:x}}}'", c as u32),
        c => {
            let mut s = String::with_capacity(3);
            s.push('\''); s.push(c); s.push('\''); s
        }
    }
}

#[inline]
fn synth_span() -> Span {
    // file, start, end, line_start, line_end, col_start, col_end
    Span::new("<internal>", 0, 0, 0, 0, 0, 0)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Render formatted numbers first (display metadata lives on Value::Formatted)
        if let Value::Formatted(inner, spec) = self {
            match &**inner {
                Value::Int(x) => {
                    let canon = x.to_string();
                    let out   = render_with_spec(&canon, spec);
                    return write!(f, "{out}");
                }
                Value::Float(x) => {
                    if !x.is_finite() { return write!(f, "{x}"); }
                    let rounded = round_to(*x, spec.decimals);
                    let canon   = fmt_num_trim(rounded);
                    let out     = render_with_spec(&canon, spec);
                    return write!(f, "{out}");
                }
                Value::Pct(p) => {
                    if !p.is_finite() { return write!(f, "{p}"); }
                    let rounded = round_to(*p, spec.decimals);
                    let canon   = fmt_num_trim(rounded);
                    let out     = render_with_spec(&canon, spec);
                    return write!(f, "{out}");
                }
                Value::Big(d) => {
                    let rounded = d.round_dp(spec.decimals);
                    let canon   = rounded.to_string();
                    let out     = render_with_spec(&canon, spec);
                    return write!(f, "{out}");
                }
                other => {
                    return write!(f, "{other}");
                }
            }
        }

        // Unformatted values (normal rendering)
        match self {
            Value::Int(x)   => write!(f, "{x}"),
            Value::Float(x) => write!(f, "{}", fmt_num_trim(*x)),
            Value::Pct(p)   => write!(f, "{}", fmt_num_trim(*p)),
            Value::Str(s)   => f.write_str(&fmt_string_visible(s)),
            Value::Char(c)  => f.write_str(&fmt_char_visible(*c)),
            Value::Bool(b)  => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Nil      => write!(f, "nil"),
            Value::Big(d)   => write!(f, "{}", d),
            Value::Object { class_name, fields } => {
                write!(f, "{}{{", class_name)?;
                let mut first = true;
                for (k, v) in fields.iter() {
                    if !first { write!(f, ", ")?; }
                    first = false;
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }

            // ---- Array (single arm) ----
            Value::Array(xs) => {
                write!(f, "[")?;
                for (i, v) in xs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }

            Value::Enum { enum_name, variant_name, fields } => {
                write!(f, "{}::{}", enum_name, variant_name)?;
                if let Some(field_map) = fields {
                    write!(f, " {{")?;
                    let mut first = true;
                    for (k, v) in field_map {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, " {}: {}", k, v)?;
                        first = false;
                    }
                    write!(f, " }}")?;
                }
                Ok(())
            }

            // ---- Map ----
            Value::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map.iter() {
                    if !first { write!(f, ", ")?; }
                    first = false;
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }

            // ---- Pair / Unit / control ----
            Value::Pair(a, b) => write!(f, "({}, {})", a, b),
            Value::Unit | Value::CtrlSkip | Value::CtrlStop => Ok(()),

            // ---- Seq ----
            Value::Seq(xs) => {
                write!(f, "[")?;
                if let Some(s) = xs.as_slice() {
                    for (i, v) in s.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{v}")?;
                    }
                } else {
                    // future backends: materialize for printing
                    let vecd = xs.to_vec();
                    for (i, v) in vecd.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{v}")?;
                    }
                }
                write!(f, "]")
            }

            // This case is unreachable because we return early above for Formatted,
            // but we keep it to satisfy exhaustiveness.
            Value::Formatted(_, _) => unreachable!("Value::Formatted should have been rendered in the early return"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Diag {
    pub code: String,
    pub message: String,
    pub span: Span,
}
impl fmt::Display for Diag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {} at {:?}", self.code, self.message, self.span)
    }
}
impl std::error::Error for Diag {}

#[derive(Default)]
pub struct Session {
    history: Vec<Value>,                               // v(n)
    pub env: Vec<BTreeMap<String, Value>>,             // scope stack (globals at [0])
    pub actions: BTreeMap<String, ast::ActionDecl>,    // free actions by name
    pub classes: BTreeMap<String, ast::ClassDecl>,
    pub enums: BTreeMap<String, ast::EnumDecl>,
    pub loop_depth: i32,
    rng_state: u128,
    pub consts: Vec<BTreeMap<String, bool>>, // true = immutable binding 
}

impl Session {
    pub fn new() -> Self {
        let t = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default();
        let seed = t.as_nanos() ^ 0xA24B_AED4_963E_E407u128;
        Self {
            history: Vec::new(),
            env: vec![BTreeMap::new()],
            actions: BTreeMap::new(),
            classes: BTreeMap::new(), 
            enums: BTreeMap::new(),
            loop_depth: 0,
            rng_state: seed,
            consts: vec![BTreeMap::new()],
        }
    }

    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut Value> {
        for frame in self.env.iter_mut().rev() {
            if let Some(v) = frame.get_mut(name) { return Some(v); }
        }
        None
    }

    #[inline]
    pub fn reseed(&mut self, seed: u128) {
        self.rng_state = seed;
    }

    // Fast 128-bit LCG
    #[inline]
    pub fn next_u128(&mut self) -> u128 {
        // Numerical Recipes LCG 64x2 widened
        self.rng_state = self
            .rng_state
            .wrapping_mul(6364136223846793005u128)
            .wrapping_add(1u128);
        self.rng_state
    }

    // --- scope helpers ---
    fn push_frame(&mut self) {
        self.env.push(BTreeMap::new());
        self.consts.push(BTreeMap::new()); // mirror
    }
    fn pop_frame(&mut self) {
        let _ = self.env.pop();
        let _ = self.consts.pop(); // mirror
    }

    pub fn get_var(&self, name: &str) -> Option<&Value> {
        for frame in self.env.iter().rev() {
            if let Some(v) = frame.get(name) { return Some(v); }
        }
        None
    }
    pub fn set_var(&mut self, name: String, val: Value) {
        if let Some(top) = self.env.last_mut() {
            top.insert(name, val);
        }
    }

    // Define a local in the current frame
    fn define_local(&mut self, name: String, val: Value, is_const: bool) {
        let top = self.env.last_mut().expect("has frame");
        top.insert(name.clone(), val);
        let tc = self.consts.last_mut().expect("has frame");
        tc.insert(name, is_const);
    }

    // Find the nearest frame index containing `name` (0 = globals, len-1 = current)
    fn find_name_frame(&self, name: &str) -> Option<usize> {
        for (i, frame) in self.env.iter().enumerate().rev() {
            if frame.contains_key(name) { return Some(i); }
        }
        None
    }

    // Is binding immutable in frame i?
    fn is_const_in_frame(&self, frame_ix: usize, name: &str) -> bool {
        self.consts
            .get(frame_ix)
            .and_then(|m| m.get(name).copied())
            .unwrap_or(false)
    }

    pub fn eval_stmt(&mut self, s: &ast::Stmt) -> Result<Option<Value>, Diag> {
        eval_stmt(s, self)
    }

    // Evaluate a whole module (returns last expression value if any).
    pub fn eval_module(&mut self, m: &ast::Module) -> Result<Option<Value>, Diag> {
        let mut last = None;
        for stmt in &m.items {
            if let Some(v) = eval_stmt(stmt, self)? {
                last = Some(v);
            }
        }
        Ok(last)
    }

    // Parse + eval a single REPL form using the real parser.
    pub fn eval_line(&mut self, src: &str) -> Result<Value, Diag> {
        // 1) Lex
        let toks = match goblin_lexer::lex(src, "<repl>") {
            Ok(t) => t,
            Err(diags) => {
                let d = diags.into_iter().next().expect("nonempty diags");
                return Err(Diag {
                    code: if d.category.is_empty() { "LEX".to_string() } else { d.category.to_string() },
                    message: d.message.clone(),
                    span: d.primary_span.clone(),
                });
            }
        };

        // 2) Parse
        let parser = goblin_parser::Parser::new(&toks);
        let module = match parser.parse_module() {
            Ok(m) => m,
            Err(diags) => {
                let d = diags.into_iter().next().expect("nonempty diags");
                return Err(Diag {
                    code: if d.category.is_empty() { "PARSE".to_string() } else { d.category.to_string() },
                    message: d.message.clone(),
                    span: d.primary_span.clone(),
                });
            }
        };

        // 3) Eval (return the last expression value)
        match self.eval_module(&module)? {
            Some(v) => {
                self.history.push(v.clone());
                Ok(v)
            }
            None => Err(Diag {
                code: "R0005".to_string(),
                message: "no expression to evaluate".into(),
                span: Span::new("<repl>", 0, 0, 1, 1, 1, 1),
            }),
        }
    }

    // Evaluate a single expression node and push it to history.
    pub fn eval_expr(&mut self, e: &ast::Expr) -> Result<Value, Diag> {
        let v = eval_expr(e, self)?;
        self.history.push(v.clone());
        Ok(v)
    }

    pub fn history_len(&self) -> usize { self.history.len() }
    pub fn get(&self, idx_1_based: usize) -> Option<&Value> { self.history.get(idx_1_based.saturating_sub(1)) }
}

// ===================== Helpers =====================

fn strip_format(v: &Value) -> (&Value, Option<&FormatSpec>) {
    match v {
        Value::Formatted(inner, spec) => (&*inner, Some(spec)),
        _ => (v, None),
    }
}

fn take_owned_unformatted(v: Value) -> (Value, Option<FormatSpec>) {
    match v {
        Value::Formatted(inner, spec) => (*inner, Some(spec)),
        other => (other, None),
    }
}

// carry formatting forward for math results: prefer left-hand spec if present, else right, else none
fn reapply_format(result: Value, left_spec: Option<FormatSpec>, right_spec: Option<FormatSpec>) -> Value {
    if let Some(spec) = left_spec.or(right_spec) {
        Value::Formatted(Box::new(result), spec)
    } else {
        result
    }
}
#[allow(dead_code)]
fn want_str(v: &Value, label: &str, sp: Span) -> Result<String, Diag> {
    match v {
        Value::Str(s)  => Ok(s.clone()),
        Value::Char(c) => Ok(c.to_string()),        // NEW: char -> 1-len string
        _ => Err(rt("T0205", format!("{label} expects a string"), sp)),
    }
}
#[allow(dead_code)]
fn want_char(v: &Value, label: &str, sp: Span) -> Result<char, Diag> {
    match v {
        Value::Char(c) => Ok(*c),
        Value::Str(s) if s.chars().count() == 1 => Ok(s.chars().next().unwrap()),
        _ => Err(rt("T0205", format!("{label} expects a char"), sp)),
    }
}

// ---------- Value <-> JSON ----------
fn to_json(v: &Value) -> sj::Value {
    let v = if let Value::Formatted(inner, _) = v { &**inner } else { v };
    match v {
        Value::Int(i)      => sj::Value::Number(serde_json::Number::from(*i)),
        Value::Float(n)    => sj::Value::Number(serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from_f64(0.0).unwrap())),
        Value::Pct(p)      => sj::Value::Number(serde_json::Number::from_f64(*p).unwrap_or_else(|| serde_json::Number::from_f64(0.0).unwrap())),
        Value::Big(d)      => sj::Value::String(d.to_string()),
        Value::Str(s)      => sj::Value::String(s.clone()),
        Value::Char(c)     => sj::Value::String(c.to_string()),
        Value::Bool(b)     => sj::Value::Bool(*b),
        Value::Array(xs)   => sj::Value::Array(xs.iter().map(to_json).collect()),
        Value::Map(m)      => {
            let mut obj = serde_json::Map::new();
            for (k, v) in m { obj.insert(k.clone(), to_json(v)); }
            sj::Value::Object(obj)
        }
        Value::Pair(a, b)  => sj::Value::Array(vec![to_json(a), to_json(b)]),
        Value::Seq(_s)     => sj::Value::String("<seq>".to_string()),
        Value::Nil | Value::Unit => sj::Value::Null,
        Value::CtrlSkip | Value::CtrlStop => sj::Value::String("control".to_string()),
        Value::Formatted(_, _) => unreachable!("peeled above"),
        Value::Object { class_name, fields } => {
            let mut obj = serde_json::Map::new();
            obj.insert("__class".to_string(), sj::Value::String(class_name.clone()));
            for (k, v) in fields {
                obj.insert(k.clone(), to_json(v));
            }
            sj::Value::Object(obj)
        }
        Value::Enum { enum_name, variant_name, .. } => {
            sj::Value::String(format!("{}::{}", enum_name, variant_name))
        }
    }
}

fn from_json(v: &sj::Value) -> Value {
    match v {
        sj::Value::Null        => Value::Nil,
        sj::Value::Bool(b)     => Value::Bool(*b),
        sj::Value::Number(n)   => {
            if let Some(i) = n.as_i64() { Value::Int(i) }
            else if let Some(f) = n.as_f64() { Value::Float(f) }
            else { Value::Float(0.0) }
        }
        sj::Value::String(s)   => Value::Str(s.clone()),
        sj::Value::Array(xs)   => Value::Array(xs.iter().map(from_json).collect()),
        sj::Value::Object(obj) => {
            let mut m = BTreeMap::new();
            for (k, v) in obj { m.insert(k.clone(), from_json(v)); }
            Value::Map(m)
        }
    }
}

// --- Numbers Casting ---
fn cast_to_big(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Big(d) => Ok(Value::Big(d)),
        Value::Int(i) => Ok(Value::Big(Decimal::from(i))),
        Value::Float(f) | Value::Pct(f) => {
            let d = Decimal::from_f64(f).ok_or_else(|| rt("T0312","big(): cannot represent float as decimal", synth_span()))?;
            Ok(Value::Big(d))
        }
        Value::Str(s) => {
            let trimmed = s.trim();
            if trimmed.ends_with('%') { return Err(rt("T0323","big(): use pct() for percent strings", synth_span())); }
            let cleaned: String = trimmed.chars().filter(|&c| c != '_').collect();
            let d = Decimal::from_str(&cleaned)
                .map_err(|_| rt("T0324","big(): invalid numeric string".to_string(), synth_span()))?;
            Ok(Value::Big(d))
        }
        Value::Formatted(inner, _) => cast_to_big(*inner),
        _ => Err(rt("T0312","big(): cannot cast value to big".to_string(), synth_span())),
    }
}

fn cast_to_float(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Float(f) => Ok(Value::Float(f)),
        Value::Pct(p)   => Ok(Value::Float(p)),
        Value::Int(i)   => {
            if (i64::MIN..i64::MAX).contains(&i) && (i.abs() as i128) < (F64_SAFE_INT_MAX as i128) {
                Ok(Value::Float(i as f64))
            } else {
                Err(rt("T0313","float(): cannot cast int ≥ 2^53 without precision loss; keep it as big", synth_span()))
            }
        }
        Value::Big(d) => {
            let t = d.trunc();
            let is_integer = d == t;
            if is_integer {
                let mag_ok = d.abs().to_f64().map(|x| x < (F64_SAFE_INT_MAX as f64)).unwrap_or(false);
                if !mag_ok {
                    return Err(rt("T0313","float(): cannot cast big integer ≥ 2^53 without precision loss; keep it as big", synth_span()));
                }
                let f = t.to_f64().ok_or_else(|| rt("T0313","float(): overflow casting big->float", synth_span()))?;
                Ok(Value::Float(f))
            } else {
                if let Some(f) = d.to_f64() { Ok(Value::Float(f)) }
                else { Err(rt("T0313","float(): overflow casting big->float", synth_span())) }
            }
        }
        Value::Str(s) => {
            let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
            if cleaned.ends_with('%') {
                return Err(rt("T0314","float(): use pct() for percent strings", synth_span()));
            }
            if let Ok(d) = Decimal::from_str(&cleaned) {
                let t = d.trunc();
                let is_integer = d == t;
                if is_integer {
                    let mag_ok = d.abs().to_f64().map(|x| x < (F64_SAFE_INT_MAX as f64)).unwrap_or(false);
                    if !mag_ok { return Err(rt("T0313","float(): cannot cast big integer ≥ 2^53 without precision loss; keep it as big", synth_span())); }
                    let f = t.to_f64().ok_or_else(|| rt("T0313","float(): overflow casting string->float", synth_span()))?;
                    Ok(Value::Float(f))
                } else {
                    if let Some(f) = d.to_f64() { Ok(Value::Float(f)) }
                    else { Err(rt("T0313","float(): overflow casting string->float", synth_span())) }
                }
            } else if let Ok(f) = cleaned.parse::<f64>() {
                if f.is_finite() { Ok(Value::Float(f)) }
                else { Err(rt("T0313","float(): overflow casting string->float", synth_span())) }
            } else {
                Err(rt("T0314", "float(): cannot cast string to float".to_string(), synth_span()))
            }
        }
        other => Err(rt("T0314", format!("float(): cannot cast {} to float", value_kind_str(&other)), synth_span())),
    }
}

fn cast_to_int_like(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Int(i) => Ok(Value::Int(i)),
        Value::Float(f) | Value::Pct(f) => {
            // truncate toward zero
            let t = f.trunc();
            if t.is_finite() && t.abs() < (i64::MAX as f64) + 1.0 {
                Ok(Value::Int(t as i64))
            } else {
                // represent as Big truncated
                let d = Decimal::from_f64(f).ok_or_else(|| rt("T0315","int(): cannot represent float as decimal", synth_span()))?;
                Ok(Value::Big(d.trunc()))
            }
        }
        Value::Big(d) => {
            let t = d.trunc();
            if let Some(i) = t.to_i64() {
                Ok(Value::Int(i))
            } else {
                Ok(Value::Big(t))
            }
        }
        Value::Str(s) => {
            let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
            let d = Decimal::from_str(&cleaned)
                .map_err(|_| rt("T0316", "cannot cast string to int".to_string(), synth_span()))?;
            let t = d.trunc();
            if let Some(i) = t.to_i64() { Ok(Value::Int(i)) } else { Ok(Value::Big(t)) }
        }
        Value::Formatted(inner, _) => cast_to_int_like(*inner),
        _ => Err(rt("T0316","cannot cast value to int".to_string(), synth_span())),
    }
}

fn cast_to_str(v: Value) -> Result<Value, Diag> {
    let s = match v {
        Value::Str(s)                 => s,
        Value::Char(c)                => c.to_string(),
        Value::Float(f)               => fmt_num_trim(f),
        Value::Pct(p)                 => fmt_num_trim(p),
        Value::Big(d)                 => d.to_string(),
        Value::Formatted(inner, _)    => return cast_to_str(*inner),
        Value::Nil                    => "nil".to_string(),
        Value::Bool(b)                => if b { "true".to_string() } else { "false".to_string() },
        other                         => format!("{other}"),
    };
    Ok(Value::Str(s))
}

fn cast_to_pct(v: Value) -> Result<Value, Diag> {
    let to_pct = |f: f64| -> Value { Value::Pct(f) };
    match v {
        Value::Pct(p)               => Ok(Value::Pct(p)),
        Value::Float(f)             => Ok(to_pct(f)),
        Value::Big(d)               => {
            if let Some(f) = d.to_f64() { Ok(to_pct(f)) }
            else { Err(rt("T0320","pct(): cannot cast big to pct (out of range)", synth_span())) }
        }
        Value::Str(s)               => {
            let trimmed = s.trim();
            let cleaned: String = trimmed.chars().filter(|&c| c != '_').collect();
            if cleaned.ends_with('%') {
                let num = cleaned[..cleaned.len()-1].trim();
                if let Ok(d) = Decimal::from_str(num) {
                    if let Some(f) = d.to_f64() { return Ok(to_pct(f / 100.0)); }
                }
                Err(rt("T0321", format!("pct(): invalid percent string '{}'", s), synth_span()))
            } else {
                if let Ok(d) = Decimal::from_str(&cleaned) {
                    if let Some(f) = d.to_f64() { return Ok(to_pct(f)); }
                }
                Err(rt("T0321", format!("pct(): invalid numeric string '{}'", s), synth_span()))
            }
        }
        Value::Formatted(inner, _)  => cast_to_pct(*inner),
        _                           => Err(rt("T0322", "pct(): cannot cast value to pct".to_string(), synth_span())),
    }
}

#[inline] fn int_checked_add(a: i64, b: i64) -> Option<i64> { a.checked_add(b) }
#[inline] fn int_checked_sub(a: i64, b: i64) -> Option<i64> { a.checked_sub(b) }
#[inline] fn int_checked_mul(a: i64, b: i64) -> Option<i64> { a.checked_mul(b) }

#[inline]
fn to_decimal(v: &Value) -> Result<rust_decimal::Decimal, Diag> {
    use rust_decimal::Decimal;
    match v {
        Value::Int(i)     => Ok(Decimal::from(*i)),
        Value::Float(f)   => Decimal::from_f64(*f).ok_or_else(|| rt("R0298","float->decimal failed", synth_span())),
        Value::Pct(f)     => Decimal::from_f64(*f).ok_or_else(|| rt("R0298","pct->decimal failed", synth_span())),
        Value::Big(d)     => Ok(d.clone()),
        _ => Err(rt("R0200","numeric expected", synth_span())),
    }
}

fn to_big_for_math(v: &Value, at: Span, label: &str) -> Result<Decimal, Diag> {
    match v {
        Value::Big(d) => Ok(*d),
        Value::Float(f) | Value::Pct(f) => Decimal::from_f64(*f)
            .ok_or_else(|| rt("T0320", format!("{label}: nan/inf not representable as big"), at)),
        _ => Err(need_number(label, at)), // your existing numeric-type error
    }
}

fn to_f64_for_math(v: &Value, at: Span, label: &str) -> Result<f64, Diag> {
    match v {
        Value::Int(i)   => Ok(*i as f64),
        Value::Float(f) => Ok(*f),
        Value::Pct(f)   => Ok(*f),
        Value::Big(d)   => d.to_f64().ok_or_else(|| rt("T0321", format!("{label}: big overflow to float"), at)),
        _ => Err(need_number(label, at)),
    }
}

fn either_is_big(a: &Value, b: &Value) -> bool {
    matches!(a, Value::Big(_)) || matches!(b, Value::Big(_))
}

#[inline]
fn decimal_powi(base: Decimal, mut exp: i64) -> Result<Decimal, Diag> {
    use rust_decimal::Decimal;
    if exp == 0 { return Ok(Decimal::ONE); }
    let neg = exp < 0;
    if neg { exp = -exp; }

    // fast exponentiation by squaring
    let mut acc = Decimal::ONE;
    let mut b = base;
    let mut e = exp as u64;
    while e > 0 {
        if (e & 1) == 1 { acc = acc * b; }
        b = b * b;
        e >>= 1;
    }

    if neg {
        if acc.is_zero() {
            return Err(rt("R0206", "power: division by zero (negative exponent on zero)", synth_span()));
        }
        Ok(Decimal::ONE / acc)
    } else {
        Ok(acc)
    }
}

// --- String-as-collection helpers (Unicode scalar semantics) ---
fn char_len(s: &str) -> usize { s.chars().count() }

fn byte_ix_at_char(s: &str, i: usize) -> Option<usize> {
    if i == char_len(s) { return Some(s.len()); }
    s.char_indices().nth(i).map(|(b, _)| b)
}

fn slice_char(s: &str, i: usize) -> Option<String> {
    let start = byte_ix_at_char(s, i)?;
    let end   = byte_ix_at_char(s, i + 1)?;
    Some(s[start..end].to_string())
}

fn str_insert_at(s: &str, i: usize, sub: &str) -> Option<String> {
    let pos = byte_ix_at_char(s, i)?;
    let mut out = String::with_capacity(s.len() + sub.len());
    out.push_str(&s[..pos]);
    out.push_str(sub);
    out.push_str(&s[pos..]);
    Some(out)
}

fn str_update_at(s: &str, i: usize, with: &str) -> Option<String> {
    let start = byte_ix_at_char(s, i)?;
    let end   = byte_ix_at_char(s, i + 1)?;
    let mut out = String::with_capacity(s.len() - (end - start) + with.len());
    out.push_str(&s[..start]);
    out.push_str(with);
    out.push_str(&s[end..]);
    Some(out)
}

fn str_delete_at(s: &str, i: usize) -> Option<String> {
    let start = byte_ix_at_char(s, i)?;
    let end   = byte_ix_at_char(s, i + 1)?;
    let mut out = String::with_capacity(s.len() - (end - start));
    out.push_str(&s[..start]);
    out.push_str(&s[end..]);
    Some(out)
}

#[inline]
fn rng_u64(sess: &mut Session) -> u64 {
    // use the high 64 bits; LCG low bits are the problem
    (sess.next_u128() >> 64) as u64
}

// Lemire's unbiased bounded integer (uniform in [0, bound))
#[inline]
fn rng_bounded(sess: &mut Session, bound: u64) -> u64 {
    if bound == 0 { return 0; }
    loop {
        let x = rng_u64(sess);
        let m = (x as u128).wrapping_mul(bound as u128);
        let l = m as u64;
        let t = bound.wrapping_neg() % bound; // threshold to avoid bias
        if l >= t {
            return (m >> 64) as u64;
        }
        // else retry
    }
}

fn rng_index(sess: &mut Session, len: usize) -> usize {
    if len == 0 { return 0; }
    rng_bounded(sess, len as u64) as usize
}

fn rng_roll_1_to_s(sess: &mut Session, sides: i64) -> i64 {
    debug_assert!(sides > 0);
    (rng_bounded(sess, sides as u64) as i64) + 1
}

// 53-bit uniform in [0,1)
#[inline]
fn rng_u01(sess: &mut Session) -> f64 {
    let bits53 = (sess.next_u128() >> 75) as u64; // keep top 53 random bits
    (bits53 as f64) / ((1u64 << 53) as f64)
}

fn rt(code: &str, msg: impl Into<String>, span: Span) -> Diag {
    Diag { code: code.to_string(), message: msg.into(), span }
}
#[allow(dead_code)]
fn not_impl(stage: &str, what: &str, span: Span) -> Diag {
    rt("R0000", format!("{what} is not implemented in {stage}"), span)
}

fn need_number(what: &str, span: Span) -> Diag {
    rt("T0201", format!("{what} expects a number"), span)
}

fn parse_number_value(text: &str, sp: Span) -> Result<Value, Diag> {
    // allow underscores in literals
    let cleaned: String = text.chars().filter(|&c| c != '_').collect();
    let has_float_syntax = cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E');

    if has_float_syntax {
        let f = cleaned.parse::<f64>()
            .map_err(|_| rt("P0301", format!("invalid number literal '{text}'"), sp.clone()))?;
        Ok(Value::Float(f))
    } else {
        // integer literal
        if let Ok(i) = cleaned.parse::<i64>() {
            Ok(Value::Int(i))
        } else {
            // too large for i64 → Big(Decimal)
            let d = Decimal::from_str(&cleaned)
                .map_err(|_| rt("P0301", format!("invalid number literal '{text}'"), sp.clone()))?;
            Ok(Value::Big(d))
        }
    }
}

fn span_of_expr(e: &ast::Expr) -> Span {
    match e {
        ast::Expr::Nil(sp)
        | ast::Expr::Bool(_, sp)
        | ast::Expr::Number(_, sp)
        | ast::Expr::Str(_, sp)
        | ast::Expr::Char(_, sp)
        | ast::Expr::Ident(_, sp)
        | ast::Expr::Array(_, sp)
        | ast::Expr::Object(_, sp)
        | ast::Expr::Member(_, _, sp)
        | ast::Expr::OptMember(_, _, sp)
        | ast::Expr::Index(_, _, sp)
        | ast::Expr::Slice(_, _, _, sp)                
        | ast::Expr::Slice3(_, _, _, _, sp)
        | ast::Expr::Call(_, _, _, sp)
        | ast::Expr::OptCall(_, _, _, sp)
        | ast::Expr::FreeCall(_, _, sp)
        | ast::Expr::NsCall(_, _, _, sp)
        | ast::Expr::Prefix(_, _, sp)
        | ast::Expr::Postfix(_, _, sp)
        | ast::Expr::Binary(_, _, _, sp)
        | ast::Expr::Assign(_, _, sp)
        | ast::Expr::EnumVariant { span: sp, .. } => sp.clone(),
    }
}

fn as_bool(v: Value, at: Span, label: &str) -> Result<bool, Diag> {
    match v {
        Value::Bool(b) => Ok(b),
        _ => Err(rt("T0303", format!("{label} requires a boolean"), at)),
    }
}
#[allow(dead_code)]
fn eval_args_to_values(args: &[ast::Expr], sess: &mut Session) -> Result<Vec<Value>, Diag> {
    let mut out = Vec::with_capacity(args.len());
    for a in args {
        out.push(eval_expr(a, sess)?);
    }
    Ok(out)
}

fn fmt_num_trim(n: f64) -> String {
    if n.is_finite() && n.fract() == 0.0 {
        format!("{}", n as i64)
    } else {
        let s = format!("{}", n);
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    }
}

fn round_to(n: f64, places: u32) -> f64 {
    if !n.is_finite() { return n; }
    if places == 0 { return n.round(); }
    let p = places.min(308);
    let f = 10f64.powi(p as i32);
    (n * f).round() / f
}

// Take canonical "1234567.89" ('.' decimal, no grouping) and render with separators.
fn render_with_spec(canon: &str, spec: &FormatSpec) -> String {
    let (sign, digits) = if canon.starts_with('-') { ("-", &canon[1..]) } else { ("", canon) };
    let mut parts = digits.split('.');
    let int_part = parts.next().unwrap_or("");
    let mut frac = parts.next().unwrap_or("").to_string();

    let need = spec.decimals as usize;
    if need == 0 {
        frac.clear();
    } else {
        if frac.len() < need { while frac.len() < need { frac.push('0'); } }
        else if frac.len() > need { frac.truncate(need); }
    }

    let grouped = if let Some(sep) = spec.sep_thousands {
        let mut out = String::with_capacity(int_part.len() + int_part.len() / 3 + 1);
        let bytes = int_part.as_bytes();
        let len = bytes.len();
        for i in 0..len {
            out.push(bytes[i] as char);
            let left = len - 1 - i;
            if left > 0 && left % 3 == 0 { out.push(sep); }
        }
        out
    } else {
        int_part.to_string()
    };

    if need == 0 {
        format!("{sign}{grouped}")
    } else {
        format!("{sign}{grouped}{}{}", spec.sep_decimal, frac)
    }
}

fn fmt_value_raw(v: &Value) -> String {
    match v {
        Value::Formatted(inner, spec) => {
            match &**inner {
                Value::Int(x) => {
                    let canon = x.to_string();
                    let out   = render_with_spec(&canon, spec);
                    return out;
                }
                Value::Float(x) => {
                    if !x.is_finite() { return x.to_string(); }
                    let rounded = round_to(*x, spec.decimals);
                    let canon   = fmt_num_trim(rounded);
                    let out     = render_with_spec(&canon, spec);
                    return out;
                }
                Value::Pct(p) => {
                    if !p.is_finite() { format!("{p}") } else {
                        let rounded = round_to(*p, spec.decimals);
                        let canon = fmt_num_trim(rounded);
                        render_with_spec(&canon, spec)
                    }
                }

                Value::Big(d) => {
                    let rounded = d.round_dp(spec.decimals);
                    let canon   = rounded.to_string();       // canonical "1234567.89"
                    render_with_spec(&canon, spec)           // <-- return it (no semicolon)
                }
                // If you later wrap Big/Int/etc., stringify canonically then render.
                other => fmt_value_raw(other),
            }
        }

        Value::Str(s)  => s.clone(),
        Value::Char(c) => c.to_string(),
        Value::Float(n)  => fmt_num_trim(*n),
        Value::Int(i) => i.to_string(),
        Value::Bool(b) => if *b { "true".into() } else { "false".into() },
        Value::Nil     => "nil".into(),
        _ => format!("{}", v), // Array/Map/Pair/Seq -> use Display
    }
}

fn value_kind_str(v: &Value) -> &'static str {
    match v {
        Value::Nil                 => "nil",
        Value::Bool(_)             => "bool",
        Value::Int(_)              => "int",
        Value::Float(_)            => "float",
        Value::Pct(_)              => "pct",
        Value::Big(_)              => "big",
        Value::Str(_)              => "str",
        Value::Char(_)             => "char",
        Value::Array(_)            => "array",
        Value::Map(_)              => "map",
        Value::Pair(_, _)          => "pair",
        Value::Seq(_)              => "seq",
        Value::Unit                => "unit",
        Value::CtrlSkip | Value::CtrlStop => "control",
        Value::Formatted(_, _)     => "formatted",
        Value::Object { .. } => "object",
        Value::Enum { .. } => "enum",
    }
}

fn as_num(v: Value, at: Span, label: &str) -> Result<f64, Diag> {
    match v {
        Value::Int(i)   => Ok(i as f64),
        Value::Float(n) => Ok(n),
        Value::Pct(p)   => Ok(p),
        Value::Big(d)   => d.to_f64().ok_or_else(|| rt("T0201", format!("{label} expects a number"), at)),
        _ => Err(rt("T0201", format!("{label} expects a number"), at)),
    }
}
#[allow(dead_code)]
fn bin_nums(lhs: &ast::Expr, rhs: &ast::Expr, sess: &mut Session, label: &str) -> Result<(f64, f64), Diag> {
    let lv = eval_expr(lhs, sess)?;
    let rv = eval_expr(rhs, sess)?;
    let ln = as_num(lv, span_of_expr(lhs), &format!("{label}: left operand"))?;
    let rn = as_num(rv, span_of_expr(rhs), &format!("{label}: right operand"))?;
    Ok((ln, rn))
}

fn is_ident(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

// Convert a Value to a non-negative usize (for slice indices)
fn want_usize_index(v: Value, label: &str, sp: Span) -> Result<usize, Diag> {
    match v {
        Value::Float(n) if n.is_finite() && n.fract() == 0.0 && n >= 0.0 => Ok(n as usize),
        _ => Err(rt("T0201", format!("{label} must be a non-negative integer"), sp)),
    }
}

// Clamp (start, end) against a known length; make end-exclusive and non-negative
fn clamp_range(mut start: isize, mut end: isize, len: usize) -> (usize, usize) {
    let l = len as isize;
    if start < 0 { start = 0; }
    if end   < 0 { end = 0; }
    if start > l { start = l; }
    if end   > l { end   = l; }
    (start as usize, end as usize)
}

// Render "Hello {name}" by looking identifiers up in the Session env.
// Supports "{{" -> "{" and "}}" -> "}".
fn render_interpolated(s: &str, sess: &Session, sp: &Span) -> Result<String, Diag> {
    let b = s.as_bytes();
    let mut i = 0usize;
    let mut out = String::new();

    while i < b.len() {
        match b[i] {
            b'{' => {
                // "{{" -> "{"
                if i + 1 < b.len() && b[i + 1] == b'{' {
                    out.push('{');
                    i += 2;
                    continue;
                }
                // find closing '}'
                let start = i + 1;
                let mut j = start;
                while j < b.len() && b[j] != b'}' { j += 1; }
                if j >= b.len() {
                    return Err(rt("P0604", "There's an unclosed '{' in this string.", sp.clone()));
                }
                let inner = s[start..j].trim();
                if !is_ident(inner) {
                    return Err(rt("P0103", "Expected a name (identifier) inside { ... }", sp.clone()));
                }
                match sess.get_var(inner) {
                    Some(v) => out.push_str(&fmt_value_raw(v)),
                    None => {
                        // Fallback: if there's a `self` map in scope, allow `{field}` to read `self[field]`
                        if let Some(Value::Map(m)) = sess.get_var("self") {
                            if let Some(v) = m.get(inner) {
                                out.push_str(&fmt_value_raw(v));
                                // continue
                            } else {
                                return Err(rt("R0110", format!("unknown identifier '{}'", inner), sp.clone()))
                            }
                        } else {
                            return Err(rt("R0110", format!("unknown identifier '{}'", inner), sp.clone()))
                        }
                    }
                }
                i = j + 1;
            }
            b'}' => {
                // "}}" -> "}"
                if i + 1 < b.len() && b[i + 1] == b'}' {
                    out.push('}');
                    i += 2;
                } else {
                    // Bare '}' — treat as literal to be permissive
                    out.push('}');
                    i += 1;
                }
            }
            _ => {
                out.push(b[i] as char);
                i += 1;
            }
        }
    }

    Ok(out)
}

fn eval_stmt(s: &ast::Stmt, sess: &mut Session) -> Result<Option<Value>, Diag> {
    match s {
        ast::Stmt::Expr(e) => Ok(Some(eval_expr(e, sess)?)),

        ast::Stmt::Action(decl) => {
            sess.actions.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        ast::Stmt::Class(decl) => {
            sess.classes.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        ast::Stmt::Enum(decl) => {
            // Store the enum definition in the session
            sess.enums.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        ast::Stmt::Bind(b) => {
            // name + span
            let (name, name_span) = (&b.name.0, b.name.1.clone());

            // Check if this is object instantiation
            if let Some(class_name) = &b.class_name {
                return instantiate_object(sess, name, class_name, &b.expr, name_span, b.is_const);
            }

            // evaluate RHS once
            let rhs = eval_expr(&b.expr, sess)?;

            match b.mode {
                BindMode::Shadow => {
                    // Always create a new local in the *current* frame.
                    // Error if this frame already has the name.
                    let cur = sess.env.len() - 1;
                    if sess.env[cur].contains_key(name) {
                        return Err(rt(
                            "R0111",
                            format!("'{}' is already declared in this scope; use '=' to reassign", name),
                            name_span,
                        ));
                    }
                    // record constness
                    sess.define_local(name.clone(), rhs, b.is_const);
                    Ok(None)
                }

                BindMode::Normal => {
                    match sess.find_name_frame(name) {
                        // Name exists *in current frame* → mutate (unless immutable)
                        Some(ix) if ix == sess.env.len() - 1 => {
                            if sess.is_const_in_frame(ix, name) {
                                return Err(rt(
                                    "R0113",
                                    format!("cannot reassign immutable '{}'", name),
                                    name_span,
                                ));
                            }
                            if let Some(slot) = sess.env[ix].get_mut(name) {
                                *slot = rhs;
                                Ok(None)
                            } else {
                                Err(rt("R0009", "internal: slot missing during assign", name_span))
                            }
                        }

                        // Name exists only in an *outer* frame → ERROR (no accidental shadowing)
                        Some(_outer_ix) => Err(rt(
                            "R0114",
                            format!("'{}' exists in an outer scope; use '[=' to shadow", name),
                            name_span,
                        )),

                        // Name not found anywhere → smart-declare local (respect imm)
                        None => {
                            sess.define_local(name.clone(), rhs, b.is_const);
                            Ok(None)
                        }
                    }
                }
            }
        }
    }
}

fn eval_expr_list(exprs: &[ast::Expr], sess: &mut Session) -> Result<Option<Value>, Diag> {
    let mut last: Option<Value> = None;
    for e in exprs {
        last = Some(eval_expr(e, sess)?);
    }
    Ok(last)
}

#[inline]
fn as_array_like<'a>(v: &'a Value) -> Option<&'a [Value]> {
    match v {
        Value::Array(xs) => Some(xs.as_slice()),
        Value::Seq(xs)   => xs.as_slice(),   // uses your Seq::as_slice()
        _ => None,
    }
}

fn expect_array<'a>(e: &'a ast::Expr, label: &str) -> Result<&'a [ast::Expr], Diag> {
    if let ast::Expr::Array(items, _) = e {
        Ok(items.as_slice())
    } else {
        Err(rt("P0314", format!("{label} must be an array of expressions"), span_of_expr(e)))
    }
}

// Try a shadowable builtin. Returns Ok(Some(Value)) if handled, Ok(None) if unknown.
#[allow(dead_code)]
fn eval_builtin(
    name: &str,
    args: &[Value],
    sess: &mut Session,
    sp: &Span,
) -> Result<Option<Value>, Diag> {
    // local helpers
    let arity = |wanted: usize| -> Result<(), Diag> {
        if args.len() != wanted {
            Err(rt("A0402", format!("wrong number of arguments: expected {}, got {}", wanted, args.len()), sp.clone()))
        } else { Ok(()) }
    };
    let _want_num = |v: &Value, label: &str| -> Result<f64, Diag> {
        match v {
            Value::Float(n) => Ok(*n),
            _ => Err(need_number(label, sp.clone())),
        }
    };

    let want_str = |v: &Value, label: &str| -> Result<String, Diag> {
        match v {
            Value::Str(s) => Ok(s.clone()),
            other => Err(rt("J0000", format!("{label} expects a string, got {}", fmt_value_raw(other)), sp.clone())),
        }
    };

    let out = match name {
        "int" => {
            arity(1)?;
            return Ok(Some(cast_to_int_like(args[0].clone())?));
        }
        "float" => {
            arity(1)?;
            return Ok(Some(cast_to_float(args[0].clone())?));
        }
        "big" => {
            arity(1)?;
            return Ok(Some(cast_to_big(args[0].clone())?));
        }
        // ---------- Numeric ----------
        "round" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.round()),
                Value::Pct(p)    => Value::Float(p.round()),
                Value::Big(d)    => Value::Big(d.round_dp(0)),
                _ => return Err(rt("T0402", "round requires a number", sp.clone())),
            }
        }
        "floor" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.floor()),
                Value::Pct(p)    => Value::Float(p.floor()),
                Value::Big(d)    => Value::Big(d.floor()),
                _ => return Err(rt("T0402", "floor requires a number", sp.clone())),
            }
        }
        "ceil" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),
                Value::Float(f)  => Value::Float(f.ceil()),
                Value::Pct(p)    => Value::Float(p.ceil()),
                Value::Big(d)    => Value::Big(d.ceil()),
                _ => return Err(rt("T0402", "ceil requires a number", sp.clone())),
            }
        }
        "abs" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(i.abs()),
                Value::Float(f)  => Value::Float(f.abs()),
                Value::Pct(p)    => Value::Float(p.abs()),
                Value::Big(d)    => Value::Big(d.abs()),
                _ => return Err(rt("T0402", "abs requires a number", sp.clone())),
            }
        }
        "pow" => {
            arity(2)?;
            // If any arg is Big, use Decimal math for integer exponents; else fall back to float powf
            let any_big = matches!(args[0], Value::Big(_)) || matches!(args[1], Value::Big(_));
            if any_big {
                let base = to_big_for_math(&args[0], sp.clone(), "pow (base)")?;
                match &args[1] {
                    Value::Int(ei) => {
                        Value::Big(decimal_powi(base, *ei)?)
                    }
                    Value::Big(e) => {
                        let et = e.trunc();
                        if *e == et {
                            let n = et.to_i64().ok_or_else(|| rt("R0203", "big exponent out of i64 range", sp.clone()))?;
                            Value::Big(decimal_powi(base, n)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                            let ef = e.to_f64().ok_or_else(|| rt("R0205", "big exponent overflow to float", sp.clone()))?;
                            Value::Float(bf.powf(ef))
                        }
                    }
                    Value::Float(f) | Value::Pct(f) => {
                        if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                            Value::Big(decimal_powi(base, *f as i64)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                            Value::Float(bf.powf(*f))
                        }
                    }
                    _ => return Err(rt("T0402", "pow requires numeric exponent", sp.clone())),
                }
            } else {
                let a = to_f64_for_math(&args[0], sp.clone(), "pow (base)")?;
                let b = to_f64_for_math(&args[1], sp.clone(), "pow (exponent)")?;
                Value::Float(a.powf(b))
            }
        }
        "sqrt" => {
            arity(1)?;
            match &args[0] {
                Value::Big(d) => {
                    if d.is_sign_negative() {
                        return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone()));
                    }
                    let f = d.to_f64().ok_or_else(|| rt("R0204", "big overflow to float for sqrt", sp.clone()))?;
                    Value::Float(f.sqrt())
                }
                _ => {
                    let n = to_f64_for_math(&args[0], sp.clone(), "sqrt")?;
                    if n < 0.0 { return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone())); }
                    Value::Float(n.sqrt())
                }
            }
        }

        // ---------- Collection (array<number>) ----------
        "sum" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    let any_big = xs.iter().any(|v| matches!(v, Value::Big(_)));
                    if any_big {
                        let mut acc = Decimal::ZERO;
                        for v in xs { acc += to_big_for_math(v, sp.clone(), "sum")?; }
                        Value::Big(acc)
                    } else {
                        let mut acc = 0.0;
                        for v in xs { acc += to_f64_for_math(v, sp.clone(), "sum")?; }
                        Value::Float(acc)
                    }
                }
                _ => return Err(rt("T0401", "sum expects an array of numbers", sp.clone())),
            }
        }

        "avg" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    if xs.is_empty() { Value::Float(0.0) } // maintain your old behavior
                    else {
                        let any_big = xs.iter().any(|v| matches!(v, Value::Big(_)));
                        if any_big {
                            let mut acc = Decimal::ZERO;
                            for v in xs { acc += to_big_for_math(v, sp.clone(), "avg")?; }
                            let n = Decimal::from(xs.len() as i64);
                            Value::Big(acc / n)
                        } else {
                            let mut acc = 0.0;
                            for v in xs { acc += to_f64_for_math(v, sp.clone(), "avg")?; }
                            Value::Float(acc / (xs.len() as f64))
                        }
                    }
                }
                _ => return Err(rt("T0401", "avg expects an array of numbers", sp.clone())),
            }
        }

        "min" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    let mut it = xs.iter();
                    let first = it.next().ok_or_else(|| rt("R0404", "min of empty array", sp.clone()))?;
                    // Big-aware selection
                    let any_big = xs.iter().any(|v| matches!(v, Value::Big(_)));
                    if any_big {
                        let mut m = to_big_for_math(first, sp.clone(), "min")?;
                        for v in it {
                            let dv = to_big_for_math(v, sp.clone(), "min")?;
                            if dv < m { m = dv; }
                        }
                        Value::Big(m)
                    } else {
                        let mut m = to_f64_for_math(first, sp.clone(), "min")?;
                        for v in it {
                            let fv = to_f64_for_math(v, sp.clone(), "min")?;
                            if fv < m { m = fv; }
                        }
                        Value::Float(m)
                    }
                }
                _ => return Err(rt("T0401", "min expects an array of numbers", sp.clone())),
            }
        }

        "max" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    let mut it = xs.iter();
                    let first = it.next().ok_or_else(|| rt("R0404", "max of empty array", sp.clone()))?;
                    let any_big = xs.iter().any(|v| matches!(v, Value::Big(_)));
                    if any_big {
                        let mut m = to_big_for_math(first, sp.clone(), "max")?;
                        for v in it {
                            let dv = to_big_for_math(v, sp.clone(), "max")?;
                            if dv > m { m = dv; }
                        }
                        Value::Big(m)
                    } else {
                        let mut m = to_f64_for_math(first, sp.clone(), "max")?;
                        for v in it {
                            let fv = to_f64_for_math(v, sp.clone(), "max")?;
                            if fv > m { m = fv; }
                        }
                        Value::Float(m)
                    }
                }
                _ => return Err(rt("T0401", "max expects an array of numbers", sp.clone())),
            }
        }

        // ---------- String case & transforms ----------
        "upper" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "upper")?;
            Value::Str(s.to_uppercase())
        }
        "lower" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "lower")?;
            Value::Str(s.to_lowercase())
        }
        "title" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "title")?;
            let mut out = String::with_capacity(s.len());
            for (i, w) in s.split_whitespace().enumerate() {
                if i > 0 { out.push(' '); }
                let mut chs = w.chars();
                if let Some(first) = chs.next() {
                    out.extend(first.to_uppercase());
                    let rest: String = chs.collect();
                    out.push_str(&rest.to_lowercase());
                }
            }
            Value::Str(out)
        }
        "slug" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "slug")?;
            let mut out = String::with_capacity(s.len());
            let mut last_dash = false;
            for ch in s.chars() {
                if ch.is_ascii_alphanumeric() {
                    out.push(ch.to_ascii_lowercase());
                    last_dash = false;
                } else if !last_dash {
                    out.push('-');
                    last_dash = true;
                }
            }
            Value::Str(out.trim_matches('-').to_string())
        }
        "mixed" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "mixed")?;
            let mut out = String::with_capacity(s.len());
            for ch in s.chars() {
                if ch.is_alphabetic() {
                    let upper = ((sess.next_u128() >> 127) & 1) == 1;
                    if upper { out.extend(ch.to_uppercase()); } else { out.extend(ch.to_lowercase()); }
                } else {
                    out.push(ch);
                }
            }
            Value::Str(out)
        }

        // Unknown builtin → tell caller to fall back to A0401
        _ => return Ok(None),
    };

    Ok(Some(out))
}

fn call_action_by_name(
    sess: &mut Session,
    name: &str,
    args: Vec<Value>,
    sp: Span,
) -> Result<Value, Diag> {
    // Prefer user-defined actions (shadowable)
    if let Some(decl) = sess.actions.get(name).cloned() {
        let params = &decl.params;
        if args.len() > params.len() {
            return Err(rt("A0402",
                format!("wrong number of arguments: expected {}, got {}", params.len(), args.len()),
                sp.clone()));
        }

        let mut bound: Vec<(String, Value)> = Vec::with_capacity(params.len());
        for (i, p) in params.iter().enumerate() {
            if i < args.len() {
                bound.push((p.name.clone(), args[i].clone()));
            } else if let Some(def_e) = &p.default {
                let v = eval_expr(def_e, sess)?;
                bound.push((p.name.clone(), v));
            } else {
                return Err(rt("A0402", format!("missing required argument '{}'", p.name), sp.clone()));
            }
        }

        sess.push_frame();
        for (k, v) in bound { sess.set_var(k, v); }

        let ret = {
            let ast::ActionBody::Block(stmts) = &decl.body;
            let mut last = Value::Unit;
            for st in stmts {
                if let Some(v) = eval_stmt(st, sess)? {
                    match v {
                        Value::CtrlSkip | Value::CtrlStop => { /* ignore outside loops */ }
                        other => last = other,
                    }
                }
            }
            last
        };

        sess.pop_frame();
        return Ok(ret);
    }

    // Shared helpers

    let want_bool = |v: &Value, label: &str| -> Result<bool, Diag> {
        match v {
            Value::Bool(b) => Ok(*b),
            _ => Err(rt("T0203", format!("{label} expects a boolean"), sp.clone())),
        }
    };

    // ---------- Built-ins (shadowable) ----------
    let arity = |wanted: usize| -> Result<(), Diag> {
        if args.len() != wanted {
            Err(rt("A0402",
                format!("wrong number of arguments: expected {}, got {}", wanted, args.len()),
                sp.clone()))
        } else { Ok(()) }
    };
    let want_num = |v: &Value, label: &str| -> Result<f64, Diag> {
        match v {
            Value::Float(n) => Ok(*n),
            _ => Err(need_number(label, sp.clone())),
        }
    };
    let want_str = |v: &Value, label: &str| -> Result<String, Diag> {
        match v {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(rt("T0205", format!("{label} expects a string"), sp.clone())),
        }
    };

    // Map a (&str -> String) transform over string or array<string>
    let map_str_1 = |v: &Value, label: &str, f: &dyn Fn(&str) -> String| -> Result<Value, Diag> {
        match v {
            Value::Str(s) => Ok(Value::Str(f(s))),
            _ => {
                if let Some(xs) = as_array_like(v) {   // <-- now handles Array or Seq
                    let mut out = Vec::with_capacity(xs.len());
                    for it in xs {
                        match it {
                            Value::Str(s) => out.push(Value::Str(f(s))),
                            _ => return Err(rt("T0205",
                                format!("{label} expects a string (or array/seq of strings)"),
                                sp.clone())),
                        }
                    }
                    Ok(Value::Array(out))              // keep legacy Array output for now
                    // If/when producers should return Seq:
                    // Ok(Value::Seq(Seq::from_vec(out)))
                } else {
                    Err(rt("T0205",
                        format!("{label} expects a string (or array/seq of strings)"),
                        sp.clone()))
                }
            }
        }
    };

    let out: Value = match name {
        "is_bound_name" => {
            // is_bound_name(name: string) -> bool
            if args.len() != 1 {
                return Err(rt("A0402",
                    format!("wrong number of arguments: expected 1, got {}", args.len()),
                    sp.clone()));
            }
            let v0 = args[0].clone();
            let name = match v0 {
                Value::Str(s) => s,
                _ => return Err(rt("T0205", "is_bound_name expects a string (variable name)", sp.clone())),
            };
            let bound = sess.find_name_frame(&name).is_some();
            Value::Bool(bound)
        }

        // ----- Introspection -----
        "type" => {
            arity(1)?;
            let recv = match &args[0] {
                Value::Formatted(inner, _) => &**inner,
                other => other,
            };

            let kind = match recv {
                Value::Nil                         => "nil",
                Value::Bool(_)                     => "bool",
                Value::Float(n) if n.is_finite()
                                  && n.fract() == 0.0 => "int",
                Value::Big(_)                      => "big",
                Value::Float(_)                    => "float",
                Value::Int(_)                      => "int",
                Value::Pct(_)                      => "pct",
                Value::Str(_)                      => "str",
                Value::Char(_)                     => "char",
                Value::Array(_)                    => "array",
                Value::Map(_)                      => "map",
                Value::Pair(_, _)                  => "pair",
                Value::Seq(_)                      => "seq",
                Value::Unit                        => "unit",
                Value::CtrlSkip | Value::CtrlStop  => "control",
            _ => "unknown",
            };
            Value::Str(kind.to_string())
        }

        "format" => {
            // Usage:
            //   n.format(dec)                     // decimals only
            //   n.format(dec, sep_th, sep_dec)    // full spec
            // dec: integer >= 0
            // sep_th: ',', '.', '_', '\'', 'none'
            // sep_dec: '.', ','
            if args.len() != 2 && args.len() != 4 {
                return Err(rt("P0901", "format expects 1 or 3 args: (dec) or (dec, sep_th, sep_dec)", sp.clone()));
            }

            // receiver: unwrap if already formatted
            let inner = match &args[0] {
                Value::Formatted(inner, _) => *inner.clone(),
                v => v.clone(),
            };

            // arg1: decimals
            let dec: u32 = match &args[1] {
                Value::Float(x) => {
                    if !x.is_finite() { 0 } else {
                        let n = *x as i64;
                        if (*x - n as f64).abs() > 0.0 || n < 0 {
                            return Err(rt("P0901", "format decimals must be integer >= 0", sp.clone()));
                        }
                        n as u32
                    }
                }
                Value::Str(s) => s.parse::<u32>().map_err(|_| rt("P0901", "format decimals must be integer >= 0", sp.clone()))?,
                Value::Char(c) if c.is_ascii_digit() => c.to_string().parse::<u32>().map_err(|_| rt("P0901", "format decimals must be integer >= 0", sp.clone()))?,
                _ => return Err(rt("T0205", "format decimals must be integer >= 0", sp.clone())),
            };

            let mut spec = FormatSpec { decimals: dec, sep_thousands: None, sep_decimal: '.' };

            if args.len() == 4 {
                // thousands sep (arg2)
                spec.sep_thousands = match &args[2] {
                    Value::Str(s) => match s.as_str() {
                        "," => Some(','),
                        "." => Some('.'),
                        "_" => Some('_'),
                        "'" => Some('\''),
                        "none" => None,
                        other => return Err(rt("P0901", format!("unknown thousands separator: {other}"), sp.clone())),
                    },
                    Value::Char(c) => match *c {
                        ',' => Some(','),
                        '.' => Some('.'),
                        '_' => Some('_'),
                        '\'' => Some('\''),
                        _ => return Err(rt("P0901", "sep_th must be ',', '.', '_', '\\'', or 'none'", sp.clone())),
                    },
                    _ => return Err(rt("T0205", "sep_th must be ',', '.', '_', '\\'', or 'none'", sp.clone())),
                };

                // decimal marker (arg3)
                spec.sep_decimal = match &args[3] {
                    Value::Str(s) => match s.as_str() {
                        "." => '.',
                        "," => ',',
                        other => return Err(rt("P0901", format!("unknown decimal marker: {other}"), sp.clone())),
                    },
                    Value::Char(c) => match *c {
                        '.' => '.',
                        ',' => ',',
                        _ => return Err(rt("P0901", "sep_dec must be '.' or ','", sp.clone())),
                    },
                    _ => return Err(rt("T0205", "sep_dec must be '.' or ','", sp.clone())),
                };
            }

            match inner {
                Value::Float(x) => Value::Formatted(Box::new(Value::Float(x)), spec),
                Value::Int(i)   => Value::Formatted(Box::new(Value::Int(i)),   spec),
                Value::Pct(p) => Value::Formatted(Box::new(Value::Pct(p)), spec),
                Value::Big(d) => Value::Formatted(Box::new(Value::Big(d)), spec),
                _ => return Err(rt("T0201", "format receiver must be a number", sp.clone())),
            }
        }

        "clear_format" => {
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            match &args[0] {
                Value::Formatted(inner, _) => *inner.clone(),
                v => v.clone(),
            }
        }

        "format_info" => {
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            match &args[0] {
                Value::Formatted(_, spec) => {
                    let mut m = BTreeMap::new();
                    m.insert("dec".to_string(), Value::Int(spec.decimals as i64));
                    let th = match spec.sep_thousands {
                        Some(',') => ",".to_string(),
                        Some('.') => ".".to_string(),
                        Some('_') => "_".to_string(),
                        Some('\'') => "'".to_string(),
                        None => "none".to_string(),
                        _ => "?".to_string(),
                    };
                    let dm = match spec.sep_decimal { '.' => ".", ',' => ",", _ => "?" }.to_string();
                    m.insert("th".to_string(), Value::Str(th));
                    m.insert("decmark".to_string(), Value::Str(dm));
                    Value::Map(m)
                }
                _ => Value::Nil,
            }
        }

        // ----- Numeric (methods) -----
        "int" | "i" => {
            arity(1)?;
            cast_to_int_like(args[0].clone())?
        }
        "float" | "f" => {
            arity(1)?;
            cast_to_float(args[0].clone())?
        }
        "big" | "b" => {
            arity(1)?;
            cast_to_big(args[0].clone())?
        }
        "str" | "string" => {
            arity(1)?;
            cast_to_str(args[0].clone())?
        }
        "pct" | "percent" => {
            arity(1)?;
            cast_to_pct(args[0].clone())?
        }
        "round" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.round()),
                Value::Pct(p)    => Value::Float(p.round()),
                Value::Big(d)    => Value::Big(d.round_dp(0)),
                _ => return Err(rt("T0402", "round requires a number", sp.clone())),
            }
        }
        "floor" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.floor()),
                Value::Pct(p)    => Value::Float(p.floor()),
                Value::Big(d)    => Value::Big(d.floor()),
                _ => return Err(rt("T0402", "floor requires a number", sp.clone())),
            }
        }
        "ceil" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.ceil()),
                Value::Pct(p)    => Value::Float(p.ceil()),
                Value::Big(d)    => Value::Big(d.ceil()),
                _ => return Err(rt("T0402", "ceil requires a number", sp.clone())),
            }
        }
        "abs" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(i.abs()),
                Value::Float(f)  => Value::Float(f.abs()),
                Value::Pct(p)    => Value::Float(p.abs()),
                Value::Big(d)    => Value::Big(d.abs()),
                _ => return Err(rt("T0402", "abs requires a number", sp.clone())),
            }
        }
        "pow" => {
            arity(2)?;
            // If any arg is Big, try decimal pow for integer exponent
            let any_big = matches!(args[0], Value::Big(_)) || matches!(args[1], Value::Big(_));
            if any_big {
                let base = to_big_for_math(&args[0], sp.clone(), "pow (base)")?;
                match &args[1] {
                    Value::Big(e) => {
                        let et = e.trunc();
                        if *e == et {
                            let n = et.to_i64().ok_or_else(|| rt("R0203", "big exponent out of i64 range", sp.clone()))?;
                            Value::Big(decimal_powi(base, n)?)
                        } else {
                            // fractional exponent -> float fallback
                            let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                            let ef = e.to_f64().ok_or_else(|| rt("R0205", "big exponent overflow to float", sp.clone()))?;
                            Value::Float(bf.powf(ef))
                        }
                    }
                    Value::Float(f) | Value::Pct(f) => {
                        if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                            Value::Big(decimal_powi(base, *f as i64)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                            Value::Float(bf.powf(*f))
                        }
                    }
                    _ => return Err(rt("T0402", "pow requires numeric exponent", sp.clone())),
                }
            } else {
                let a = to_f64_for_math(&args[0], sp.clone(), "pow (base)")?;
                let b = to_f64_for_math(&args[1], sp.clone(), "pow (exponent)")?;
                Value::Float(a.powf(b))
            }
        }
        "sqrt" => {
            arity(1)?;
            match &args[0] {
                Value::Big(d) => {
                    if d.is_sign_negative() {
                        return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone()));
                    }
                    let f = d.to_f64().ok_or_else(|| rt("R0204", "big overflow to float for sqrt", sp.clone()))?;
                    Value::Float(f.sqrt())    // <-- no trailing semicolon
                }
                _ => {
                    let n = to_f64_for_math(&args[0], sp.clone(), "sqrt")?;
                    if n < 0.0 {
                        return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone()));
                    }
                    Value::Float(n.sqrt())     // <-- no trailing semicolon
                }
            }
        }

        // ----- Array<number> stats -----
        "sum" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                let mut acc = 0.0;
                for v in xs { acc += want_num(v, "sum")?; }
                Value::Float(acc)
            } else {
                return Err(rt("T0401", "sum expects an array of numbers", sp.clone()));
            }
        }
        "avg" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                if xs.is_empty() { Value::Float(0.0) } else {
                    let mut acc = 0.0;
                    for v in xs { acc += want_num(v, "avg")?; }
                    Value::Float(acc / xs.len() as f64)
                }
            } else {
                return Err(rt("T0401", "avg expects an array of numbers", sp.clone()));
            }
        }
        "min" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                let mut it = xs.iter();
                let first = it.next().ok_or_else(|| rt("R0404", "min of empty array", sp.clone()))?;
                let mut m = want_num(first, "min")?;
                for v in it { m = m.min(want_num(v, "min")?); }
                Value::Float(m)
            } else {
                return Err(rt("T0401", "min expects an array of numbers", sp.clone()));
            }
        }
        "max" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                let mut it = xs.iter();
                let first = it.next().ok_or_else(|| rt("R0404", "max of empty array", sp.clone()))?;
                let mut m = want_num(first, "max")?;
                for v in it { m = m.max(want_num(v, "max")?); }
                Value::Float(m)
            } else {
                return Err(rt("T0401", "max expects an array of numbers", sp.clone()));
            }
        }

        // ----- Collections -----
        "pick" => {
            use std::collections::{BTreeMap, BTreeSet};

            // ---- validate arg ----
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            let cfg = match &args[0] {
                Value::Map(m) => m.clone(),
                _ => return Err(rt("T0401", "pick expects a config object", sp.clone())),
            };

            // ---- small getters ----
            let get_bool = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b) = v { Some(*b) } else { None })
            };
            let get_num = |m: &BTreeMap<String, Value>, k: &str| -> Option<f64> {
                m.get(k).and_then(|v| if let Value::Float(n) = v { Some(*n) } else { None })
            };
            let get_arr = |m: &BTreeMap<String, Value>, k: &str| -> Option<Vec<Value>> {
                m.get(k).and_then(|v| if let Value::Array(xs) = v { Some(xs.clone()) } else { None })
            };

            // ---- base config ----
            let count = get_num(&cfg, "count").unwrap_or(1.0);
            if count < 1.0 || count.fract() != 0.0 {
                return Err(rt("P1406", "pick count must be a positive integer", sp.clone()));
            }
            let n_out = count as usize;

            let digits_opt = get_num(&cfg, "digits").map(|d| d as i64);
            if let Some(d) = digits_opt { if d < 1 { return Err(rt("P1407", "digits must be >= 1", sp.clone())); } }
            let unique_digits = get_bool(&cfg, "unique").unwrap_or(false);

            let src_array = get_arr(&cfg, "src");
            let has_range  = cfg.contains_key("range_start") && cfg.contains_key("range_end");

            // default allow_dups: collections => false; numeric (range/digits) => true
            let allow_dups = match get_bool(&cfg, "allow_dups") {
                Some(b) => b,
                None => if src_array.is_some() { false } else { true },
            };

            // ---- RNG: fast 128-bit LCG ----

            let finish = |mut items: Vec<Value>| -> Value {
                if n_out == 1 { items.pop().unwrap_or(Value::Nil) } else { Value::Array(items) }
            };

            // ================== Collections ==================
            if let Some(arr) = src_array {
                if arr.is_empty() { return Err(rt("R0701", "cannot pick from an empty collection", sp.clone())); }
                if !allow_dups && n_out > arr.len() {
                    return Err(rt("R0701", format!("cannot pick {} distinct items from {}", n_out, arr.len()), sp.clone()));
                }

                let out = if allow_dups {
                    let mut out = Vec::with_capacity(n_out);
                    for _ in 0..n_out {
                        out.push(arr[rng_index(sess, arr.len())].clone());
                    }
                    out
                } else {
                    // without replacement: partial Fisher–Yates over indices
                    let mut idxs: Vec<usize> = (0..arr.len()).collect();
                    let mut out = Vec::with_capacity(n_out);
                    for i in 0..n_out {
                        let j = i + rng_index(sess, arr.len() - i);
                        idxs.swap(i, j);
                        out.push(arr[idxs[i]].clone());
                    }
                    out
                };
                return Ok(finish(out));
            }

            // ---------- helpers for numeric domains ----------
            let _within_digits = |v: i64, d: i64| -> bool {
                if d <= 0 { return true; }
                let min = 10_i64.pow((d - 1) as u32);
                let max = 10_i64.pow(d as u32) - 1;
                v >= min && v <= max
            };
            let _has_unique_digits = |mut v: i64, d: i64| -> bool {
                if !unique_digits { return true; }
                if d > 0 && v < 10_i64.pow((d - 1) as u32) { return false; } // no leading-zero width
                let mut seen = [false; 10];
                if v == 0 { return false; }
                while v > 0 {
                    let dd = (v % 10) as usize;
                    if seen[dd] { return false; }
                    seen[dd] = true;
                    v /= 10;
                }
                true
            };

            // ================== Numeric Range (optional digits) ==================
            // IMPORTANT: handle range BEFORE pure-digits to avoid any retry loops.
            if has_range {
                let a = get_num(&cfg, "range_start").ok_or_else(|| rt("T0201", "range bounds must be numbers", sp.clone()))?;
                let b = get_num(&cfg, "range_end").ok_or_else(|| rt("T0201", "range bounds must be numbers", sp.clone()))?;
                if a.fract() != 0.0 || b.fract() != 0.0 {
                    return Err(rt("T0201", "range bounds must be integers", sp.clone()));
                }

                let mut lo = a as i64;
                let mut hi = b as i64;
                if lo > hi { std::mem::swap(&mut lo, &mut hi); }
                let inc = get_bool(&cfg, "range_inclusive").unwrap_or(false);

                let d = digits_opt.unwrap_or(0);

                // filters
                let within_digits = |v: i64, d: i64| -> bool {
                    if d <= 0 { return true; }
                    let min = 10_i64.pow((d - 1) as u32);
                    let max = 10_i64.pow(d as u32) - 1;
                    v >= min && v <= max
                };
                let has_unique_digits = |mut v: i64, d: i64| -> bool {
                    if !unique_digits { return true; }
                    if d > 0 && v < 10_i64.pow((d - 1) as u32) { return false; } // no leading-zero width
                    let mut seen = [false; 10];
                    if v == 0 { return false; }
                    while v > 0 {
                        let dd = (v % 10) as usize;
                        if seen[dd] { return false; }
                        seen[dd] = true;
                        v /= 10;
                    }
                    true
                };

                // Build finite pool.
                let mut pool: Vec<i64> = Vec::new();
                if inc {
                    for v in lo..=hi {
                        if within_digits(v, d) && has_unique_digits(v, d) { pool.push(v); }
                    }
                } else {
                    for v in lo..hi {
                        if within_digits(v, d) && has_unique_digits(v, d) { pool.push(v); }
                    }
                }

                if pool.is_empty() {
                    return Err(rt("R0702", "invalid range (no values after filters)", sp.clone()));
                }
                if !allow_dups && n_out > pool.len() {
                    return Err(rt("R0701", "not enough values in range for !dups", sp.clone()));
                }

                // RNG

                let out_vals: Vec<Value> = if allow_dups {
                    let mut out = Vec::with_capacity(n_out);
                    for _ in 0..n_out {
                        out.push(Value::Int(pool[rng_index(sess, pool.len())] as i64));
                    }
                    out
                } else {
                    // Without replacement: partial Fisher–Yates on the pool itself.
                    for i in 0..n_out {
                        let j = i + rng_index(sess, pool.len() - i);
                        pool.swap(i, j);
                    }
                    (0..n_out).map(|i| Value::Int(pool[i] as i64)).collect()
                };

                return Ok(if n_out == 1 {
                    out_vals.into_iter().next().unwrap()
                } else {
                    Value::Array(out_vals)
                });
            }

            // ================== Pure Digits (no range) ==================
            if let Some(d) = digits_opt {
                // Feasibility for !dups
                let domain_size = if unique_digits {
                    // 1st digit 1..9, then P(9, d-1)
                    let mut total: i64 = 9;
                    let mut avail: i64 = 9;
                    for _ in 1..d { total *= avail; avail -= 1; }
                    total.max(0) as usize
                } else {
                    // d-digit numbers: 10^(d-1) .. 10^d - 1
                    (10_i64.pow(d as u32) - 10_i64.pow((d - 1) as u32)) as usize
                };
                if !allow_dups && n_out > domain_size {
                    return Err(rt("R0701", "cannot satisfy !dups for the requested digit space", sp.clone()));
                }

                // generator of one d-digit number
                let mut gen_one = || -> i64 {
                    if !unique_digits {
                        let min = 10_i64.pow((d - 1) as u32);
                        let width = (9_i64 * 10_i64.pow((d - 1) as u32)) as usize;
                        let j = rng_index(sess, width);
                        min + j as i64
                    } else {
                        // first digit 1..9, remaining without repetition
                        let mut digits: [i64; 10] = [0,1,2,3,4,5,6,7,8,9];
                        let first_idx = 1 + rng_index(sess, 9);
                        let first = digits[first_idx];
                        digits[first_idx] = digits[9]; // remove chosen
                        let mut val: i64 = first;
                        let mut size = 9; // remaining usable positions
                        for _ in 1..d {
                            let idx = rng_index(sess, size + 1);
                            let chosen = digits[idx];
                            digits[idx] = digits[size];
                            if size > 0 { size -= 1; }
                            val = val * 10 + chosen;
                        }
                        val
                    }
                };

                // allow_dups:
                let out = if allow_dups {
                    let mut out = Vec::with_capacity(n_out);
                    for _ in 0..n_out {
                        out.push(Value::Int(gen_one() as i64));
                    }
                    out
                } else {
                    // without replacement:
                    let mut set = BTreeSet::<i64>::new();
                    let mut attempts_left: usize = domain_size.saturating_mul(3).max(n_out * 10);
                    while set.len() < n_out {
                        if attempts_left == 0 {
                            return Err(rt("R0701", "could not generate enough distinct values", sp.clone()));
                        }
                        attempts_left -= 1;
                        set.insert(gen_one());
                    }
                    set.into_iter().map(|v| Value::Int(v as i64)).collect()
                };

                return Ok(finish(out));
            }

            // nothing recognized
            return Err(rt("P1408", "pick needs a source: `from <collection>` or a numeric form", sp.clone()));
        }

        //===== SEEDS =====
        "rand_seed" => {
            arity(1)?;
            let n = want_num(&args[0], "rand_seed")?;
            // Make negative / float inputs deterministic too
            let bits = (n.to_bits() as u128) ^ 0x9E37_79B9_7F4A_7C15u128;
            sess.reseed(bits);
            Value::Unit
        }

        // ====== roll (numeric result) ======
        "roll" => {
            use std::collections::BTreeMap;
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => return Err(rt("T0401", "roll expects a config object", sp.clone())),
            };

            // --- helpers ---
            let get_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Result<i64, Diag> {
                match m.get(k) {
                    Some(Value::Float(n)) if n.fract() == 0.0 => Ok(*n as i64),
                    Some(_) => Err(rt("T0201", &format!("roll '{k}' must be an integer"), sp.clone())),
                    None => Err(rt("T0201", &format!("missing roll field '{k}'"), sp.clone())),
                }
            };
            let get_i64_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| if let Value::Float(n)=v { if n.fract()==0.0 { Some(*n as i64) } else { None } } else { None })
            };
            let get_bool_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b)=v { Some(*b) } else { None })
            };

            let count    = get_i64(cfg, "count")?;
            let sides    = get_i64(cfg, "sides")?;
            let modifier = get_i64_opt(cfg, "modifier").or_else(|| get_i64_opt(cfg, "mod")).unwrap_or(0);

            if count <= 0 || sides <= 0 {
                return Err(rt("T0201", "roll count and sides must be > 0", sp.clone()));
            }

            // extras
            let keep_high = get_i64_opt(cfg, "keep_high").unwrap_or(0);
            let drop_low  = get_i64_opt(cfg, "drop_low").unwrap_or(0);
            let reroll_eq = get_i64_opt(cfg, "reroll_eq");
            let explode   = get_bool_opt(cfg, "explode").unwrap_or(false);
            let adv       = get_bool_opt(cfg, "adv").unwrap_or(false);
            let dis       = get_bool_opt(cfg, "dis").unwrap_or(false);
            let clamp_lo  = get_i64_opt(cfg, "clamp_min");
            let clamp_hi  = get_i64_opt(cfg, "clamp_max");

            if adv || dis && count != 1 {
                return Err(rt("T0201","adv/dis requires a single die (count=1)", sp.clone()));
            }
            if keep_high > 0 && drop_low > 0 {
                return Err(rt("T0201","cannot combine keep_high and drop_low", sp.clone()));
            }
            if let Some(x) = reroll_eq {
                if x < 1 || x > sides {
                    return Err(rt("T0201","reroll_eq must be between 1 and sides", sp.clone()));
                }
            }

            // local RNG (LCG)
            let mut roll_one = |s: i64| -> i64 {
                let mut r = rng_roll_1_to_s(sess, s);
                if let Some(face) = reroll_eq {
                    if r == face {
                        r = rng_roll_1_to_s(sess, s); // reroll once
                    }
                }
                if explode {
                    let mut total = r;
                    let mut last = r;
                    let mut guard = 0usize;
                    while last == s && guard < 1024 {
                        let extra = rng_roll_1_to_s(sess, s);
                        total += extra;
                        last = extra;
                        guard += 1;
                    }
                    total
                } else {
                    r
                }
            };

            // === produce a plain Value (no Ok, no return) ===
            let result_num: i64 = if adv || dis {
                let a = roll_one(sides);
                let b = roll_one(sides);
                let mut total = if adv { a.max(b) } else { a.min(b) } + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) {
                    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
                    if total < lo { total = lo; }
                    if total > hi { total = hi; }
                }
                total
            } else {
                // regular N dice
                let mut vals: Vec<i64> = Vec::with_capacity(count as usize);
                for _ in 0..count { vals.push(roll_one(sides)); }

                let kept_sum: i64 = if keep_high > 0 {
                    let mut xs = vals.clone();
                    xs.sort_unstable_by(|a,b| b.cmp(a)); // desc
                    let k = keep_high.max(0) as usize;
                    xs.into_iter().take(k.min(vals.len())).sum()
                } else if drop_low > 0 {
                    let mut xs = vals.clone();
                    xs.sort_unstable(); // asc
                    let d = drop_low.max(0) as usize;
                    xs.into_iter().skip(d.min(vals.len())).sum()
                } else {
                    vals.iter().sum()
                };

                let mut total = kept_sum + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) {
                    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
                    if total < lo { total = lo; }
                    if total > hi { total = hi; }
                }
                total
            };

            Value::Float(result_num as f64)
        },

        // ====== roll_detail (map with values / kept / dropped / sum / total) ======
        "roll_detail" => {
            use std::collections::BTreeMap;
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => return Err(rt("T0401", "roll_detail expects a config object", sp.clone())),
            };

            // --- helpers ---
            let get_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Result<i64, Diag> {
                match m.get(k) {
                    Some(Value::Float(n)) if n.fract() == 0.0 => Ok(*n as i64),
                    Some(_) => Err(rt("T0201", &format!("roll '{k}' must be an integer"), sp.clone())),
                    None => Err(rt("T0201", &format!("missing roll field '{k}'"), sp.clone())),
                }
            };
            let get_i64_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| if let Value::Float(n)=v { if n.fract()==0.0 { Some(*n as i64) } else { None } } else { None })
            };
            let get_bool_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b)=v { Some(*b) } else { None })
            };

            let count    = get_i64(cfg, "count")?;
            let sides    = get_i64(cfg, "sides")?;
            let modifier = get_i64_opt(cfg, "modifier").or_else(|| get_i64_opt(cfg, "mod")).unwrap_or(0);

            if count <= 0 || sides <= 0 {
                return Err(rt("T0201", "roll count and sides must be > 0", sp.clone()));
            }

            // extras
            let keep_high = get_i64_opt(cfg, "keep_high").unwrap_or(0);
            let drop_low  = get_i64_opt(cfg, "drop_low").unwrap_or(0);
            let reroll_eq = get_i64_opt(cfg, "reroll_eq");
            let explode   = get_bool_opt(cfg, "explode").unwrap_or(false);
            let adv       = get_bool_opt(cfg, "adv").unwrap_or(false);
            let dis       = get_bool_opt(cfg, "dis").unwrap_or(false);
            let clamp_lo  = get_i64_opt(cfg, "clamp_min");
            let clamp_hi  = get_i64_opt(cfg, "clamp_max");

            if (adv || dis) && count != 1 {
                return Err(rt("T0201","adv/dis requires a single die (count=1)", sp.clone()));
            }
            if keep_high > 0 && drop_low > 0 {
                return Err(rt("T0201","cannot combine keep_high and drop_low", sp.clone()));
            }
            if let Some(x) = reroll_eq {
                if x < 1 || x > sides {
                    return Err(rt("T0201","reroll_eq must be between 1 and sides", sp.clone()));
                }
            }

            let mut roll_one = |s: i64| -> i64 {
                let mut r = rng_roll_1_to_s(sess, s);
                if let Some(face) = reroll_eq {
                    if r == face {
                        r = rng_roll_1_to_s(sess, s); // reroll once
                    }
                }
                if explode {
                    let mut total = r;
                    let mut last = r;
                    let mut guard = 0usize;
                    while last == s && guard < 1024 {
                        let extra = rng_roll_1_to_s(sess, s);
                        total += extra;
                        last = extra;
                        guard += 1;
                    }
                    total
                } else {
                    r
                }
            };

            // === produce the detail map ===
            let detail_out: Value = if adv || dis {
                // two rolls, choose one
                let a = roll_one(sides);
                let b = roll_one(sides);
                let chosen = if adv { a.max(b) } else { a.min(b) };
                let dropped_val = if adv { a.min(b) } else { a.max(b) };

                let mut total = chosen + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) {
                    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
                    if total < lo { total = lo; }
                    if total > hi { total = hi; }
                }

                let mut out = BTreeMap::<String, Value>::new();
                out.insert("count".into(),    Value::Int(1));
                out.insert("sides".into(),    Value::Int(sides as i64));
                out.insert("modifier".into(), Value::Int(modifier as i64));
                out.insert("values".into(),   Value::Array(vec![Value::Int(a as i64), Value::Int(b as i64)]));
                out.insert("kept".into(),     Value::Array(vec![Value::Int(chosen as i64)]));
                out.insert("dropped".into(),  Value::Array(vec![Value::Int(dropped_val as i64)]));
                out.insert("sum".into(),      Value::Int(chosen as i64));
                out.insert("total".into(),    Value::Int(total as i64));
                out.insert("adv".into(),      Value::Bool(adv));
                out.insert("dis".into(),      Value::Bool(dis));
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x as i64)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo as i64)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi as i64)); }
                Value::Map(out)
            } else {
                // N dice, mark kept vs dropped explicitly
                let mut vals: Vec<i64> = Vec::with_capacity(count as usize);
                for _ in 0..count { vals.push(roll_one(sides)); }

                // Decide kept/dropped (by index, stable against equal values)
                let mut keep_mask = vec![true; vals.len()];
                if keep_high > 0 {
                    let k = keep_high.max(0) as usize;
                    let mut idxs: Vec<usize> = (0..vals.len()).collect();
                    idxs.sort_unstable_by(|&i, &j| vals[j].cmp(&vals[i])); // desc by value
                    for &i in idxs.iter().skip(k.min(vals.len())) { keep_mask[i] = false; }
                } else if drop_low > 0 {
                    let d = drop_low.max(0) as usize;
                    let mut idxs: Vec<usize> = (0..vals.len()).collect();
                    idxs.sort_unstable_by(|&i, &j| vals[i].cmp(&vals[j])); // asc by value
                    for &i in idxs.iter().take(d.min(vals.len())) { keep_mask[i] = false; }
                }

                let mut kept_vals: Vec<i64> = Vec::new();
                let mut dropped_vals: Vec<i64> = Vec::new();
                for (i, &v) in vals.iter().enumerate() {
                    if keep_mask[i] { kept_vals.push(v); } else { dropped_vals.push(v); }
                }

                let kept_sum: i64 = kept_vals.iter().sum();
                let mut total = kept_sum + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) {
                    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
                    if total < lo { total = lo; }
                    if total > hi { total = hi; }
                }

                let mut out = BTreeMap::<String, Value>::new();
                out.insert("count".into(),    Value::Int(count as i64));
                out.insert("sides".into(),    Value::Int(sides as i64));
                out.insert("modifier".into(), Value::Int(modifier as i64));
                out.insert("values".into(),   Value::Array(vals.iter().copied().map(|v| Value::Int(v as i64)).collect()));
                out.insert("kept".into(),     Value::Array(kept_vals.iter().copied().map(|v| Value::Int(v as i64)).collect()));
                out.insert("dropped".into(),  Value::Array(dropped_vals.iter().copied().map(|v| Value::Int(v as i64)).collect()));
                out.insert("sum".into(),      Value::Int(kept_sum as i64));
                out.insert("total".into(),    Value::Int(total as i64));
                if keep_high > 0 { out.insert("keep_high".into(), Value::Int(keep_high as i64)); }
                if drop_low  > 0 { out.insert("drop_low".into(),  Value::Int(drop_low  as i64)); }
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x as i64)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo as i64)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi as i64)); }
                Value::Map(out)
            };

            detail_out
        },

        // ----- String case & transforms -----
        "upper" => { arity(1)?; map_str_1(&args[0], "upper", &|s| s.to_uppercase())? }
        "lower" => { arity(1)?; map_str_1(&args[0], "lower", &|s| s.to_lowercase())? }
        "title" => {
            arity(1)?;
            let to_title = |s: &str| -> String {
                let mut out = String::with_capacity(s.len());
                for (i, w) in s.split_whitespace().enumerate() {
                    if i > 0 { out.push(' '); }
                    let mut chs = w.chars();
                    if let Some(first) = chs.next() {
                        out.extend(first.to_uppercase());
                        let rest: String = chs.collect();
                        out.push_str(&rest.to_lowercase());
                    }
                }
                out
            };
            map_str_1(&args[0], "title", &to_title)?
        }
        "slug" => {
            arity(1)?;
            let to_slug = |s: &str| -> String {
                let mut out = String::with_capacity(s.len());
                let mut last_dash = false;
                for ch in s.chars() {
                    if ch.is_ascii_alphanumeric() {
                        out.push(ch.to_ascii_lowercase());
                        last_dash = false;
                    } else if !last_dash {
                        out.push('-');
                        last_dash = true;
                    }
                }
                out.trim_matches('-').to_string()
            };
            map_str_1(&args[0], "slug", &to_slug)?
        }
        "mixed" => {
            arity(1)?;
            // draw one seed from the session RNG, then do pure mixing per char
            let seed = sess.next_u128();
            let to_mixed = move |s: &str| -> String {
                let mut out = String::with_capacity(s.len());
                for (i, ch) in s.chars().enumerate() {
                    // SplitMix-style stateless mixing from (seed ^ i)
                    let mut x = seed ^ ((i as u128).wrapping_mul(0x9E37_79B9_7F4A_7C15));
                    x ^= x >> 30; x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
                    x ^= x >> 27; x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
                    x ^= x >> 31;
                    let upper = (x & 1) == 1;
                    if ch.is_alphabetic() {
                        if upper { out.extend(ch.to_uppercase()); }
                        else     { out.extend(ch.to_lowercase()); }
                    } else {
                        out.push(ch);
                    }
                }
                out
            };
            map_str_1(&args[0], "mixed", &to_mixed)?
        }

        // ----- String trim -----
        "trim"       => { arity(1)?; map_str_1(&args[0], "trim",       &|s| s.trim().to_string())? }
        "trim_lead"  => { arity(1)?; map_str_1(&args[0], "trim_lead",  &|s| s.trim_start().to_string())? }
        "trim_trail" => { arity(1)?; map_str_1(&args[0], "trim_trail", &|s| s.trim_end().to_string())? }

        // ===== Search & test =====
        "has" => { // s contains sub?  -> Bool
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s   = want_str(&args[0], "has")?;
            let sub = want_str(&args[1], "has")?;
            Value::Bool(s.contains(&sub))
        }
        "find" => { // first index of sub (0-based), or nil
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s   = want_str(&args[0], "find")?;
            let sub = want_str(&args[1], "find")?;
            match s.find(&sub) { Some(i) => Value::Int(i as i64), None => Value::Nil }
        }
        "find_all" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let s   = want_str(&args[0], "find_all")?;
            let sub = want_str(&args[1], "find_all")?;
            if sub.is_empty() {
                Value::Array(vec![])
            } else {
                let mut out = Vec::new();
                let mut start = 0usize;
                while let Some(pos) = s[start..].find(&sub) {
                    let idx = start + pos;
                    out.push(Value::Int(idx as i64));
                    start = idx + sub.len(); // non-overlapping
                }
                Value::Array(out)
            }
        }
        
        "count" => {
            match args.len() {
                // count(x)  -> length of string/collection/map
                1 => {
                    match &args[0] {
                        Value::Str(s)    => Value::Int(s.chars().count() as i64),
                        Value::Array(xs) => Value::Int(xs.len() as i64),
                        Value::Seq(xs)   => Value::Int(xs.len() as i64),
                        Value::Map(m)    => Value::Int(m.len() as i64),
                        _ => return Err(rt("T0401", "count expects a string or collection", sp.clone())),
                    }
                }
                // count(s, sub) -> substring occurrences (existing behavior)
                2 => {
                    let s   = want_str(&args[0], "count")?;
                    let sub = want_str(&args[1], "count")?;
                    if sub.is_empty() {
                        Value::Float(0.0)
                    } else {
                        let mut n = 0usize;
                        let mut start = 0usize;
                        while let Some(pos) = s[start..].find(&sub) {
                            n += 1;
                            start = start + pos + sub.len();
                        }
                        Value::Float(n as f64)
                    }
                }
                _ => return Err(rt("A0402",
                    format!("wrong number of arguments: expected 1 or 2, got {}", args.len()),
                    sp.clone())),
            }
        }

        // ===== Other Stuff =====

        "shuffle" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    let mut v: Vec<char> = s.chars().collect();
                    for i in 0..v.len() {
                        let j = i + rng_index(sess, v.len()-i);
                        v.swap(i, j);
                    }
                    Value::Str(v.into_iter().collect())
                }
                _ => {
                    let s = as_array_like(&args[0]).ok_or_else(|| rt("T0401", "shuffle expects array/seq/string", sp.clone()))?;
                    let mut v = s.to_vec();
                    for i in 0..v.len() {
                        let j = i + rng_index(sess, v.len()-i);
                        v.swap(i, j);
                    }
                    Value::Array(v)
                }
            }
        }

        "sort" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    let mut v: Vec<char> = s.chars().collect();
                    v.sort_unstable(); // Unicode scalar order
                    Value::Str(v.into_iter().collect())
                }
                _ => {
                    let s = as_array_like(&args[0]).ok_or_else(|| rt("T0401", "sort expects array/seq/string", sp.clone()))?;
                    let mut v = s.to_vec();
                    v.sort_by(|a,b| fmt_value_raw(a).cmp(&fmt_value_raw(b)));
                    Value::Array(v)
                }
            }
        }

        "freq" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    use std::collections::BTreeMap;
                    let mut m = BTreeMap::<String, Value>::new();
                    let mut cnt = BTreeMap::<char, i64>::new();
                    for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
                    for (c, n) in cnt { m.insert(c.to_string(), Value::Int(n as i64)); }
                    Value::Map(m)
                }
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401", "freq expects an array/seq/string", sp.clone()))?;
                    let mut map = std::collections::BTreeMap::<String, i64>::new();
                    for v in xs { *map.entry(fmt_value_raw(v)).or_insert(0) += 1; }
                    let mut out = std::collections::BTreeMap::<String, Value>::new();
                    for (k, n) in map { out.insert(k, Value::Int(n as i64)); }
                    Value::Map(out)
                }
            }
        }

        "mode" => {
            arity(1)?;
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "mode expects an array", sp.clone()))?;
            if xs.is_empty() { return Err(rt("R0404", "mode of empty array", sp.clone())); }
            let mut counts = std::collections::BTreeMap::<String, i64>::new();
            for v in xs {
                let k = fmt_value_raw(v);
                *counts.entry(k).or_insert(0) += 1;
            }
            let mut best_k = String::new();
            let mut best_n = -1i64;
            for (k, n) in counts.iter() {
                if *n > best_n { best_n = *n; best_k = k.clone(); }
            }
            Value::Map(vec![(best_k, Value::Int(best_n as i64))].into_iter().collect())
        },

        "sample_weighted" => {
            // sample_weighted({ src: Array|Seq, weights: Array<num>, count?: int }) -> Array
            arity(1)?;
            let cfg = match &args[0] { Value::Map(m) => m, _ => return Err(rt("T0401","expects config map", sp.clone())) };
            let s = cfg.get("src")
                .ok_or_else(|| rt("T0401","missing 'src'", sp.clone()))?;
            let xs = as_array_like(s)
                .ok_or_else(|| rt("T0401","'src' must be array/seq", sp.clone()))?;
            let wsv = cfg.get("weights")
                .ok_or_else(|| rt("T0401","missing 'weights'", sp.clone()))?;
            let ws = as_array_like(wsv)
                .ok_or_else(|| rt("T0401","'weights' must be array/seq", sp.clone()))?;
            if xs.len() != ws.len() || xs.is_empty() {
                return Err(rt("R0701","weights length must match src and be non-empty", sp.clone()));
            }
            let n_out: usize = match cfg.get("count") {
                None => 1,
                Some(Value::Float(n)) if *n > 0.0 && n.fract()==0.0 => *n as usize,
                Some(_) => return Err(rt("T0201","count must be int > 0", sp.clone())),
            };
            // build cumulative weights
            let mut cum = Vec::with_capacity(ws.len());
            let mut sum = 0.0;
            for w in ws {
                let w = want_num(w, "weights")?;
                if w < 0.0 { return Err(rt("T0201","weights must be >= 0", sp.clone())); }
                sum += w;
                cum.push(sum);
            }
            if sum == 0.0 { return Err(rt("R0701","all weights are zero", sp.clone())); }
            // sample with replacement (weighted). If you want without replacement later, add alias.
            let mut out = Vec::with_capacity(n_out);
            for _ in 0..n_out {
                let r = rng_u01(sess) * sum; // in [0, sum)
                let mut lo = 0usize;
                let mut hi = cum.len();
                while lo < hi {
                    let mid = (lo+hi)/2;
                    if r < cum[mid] { hi = mid; } else { lo = mid+1; }
                }
                out.push(xs[lo].clone());
            }
            Value::Array(out)
        }
        "map" => {
            if args.len() != 2 {
                return Err(rt(
                    "A0402",
                    format!("wrong number of arguments: expected 2, got {}", args.len()),
                    sp.clone(),
                ));
            }

            let action = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "map expects the action name as a string", sp.clone())),
            };

            // --- String case: treat as array of runes/chars ---
            if let Value::Str(s) = &args[0] {
                let mut results: Vec<Value> = Vec::with_capacity(char_len(s));
                let mut _all_text = true;       // all Char or Str
                let mut any_non_text = false;  // any non Char/Str

                for c in s.chars() {
                    let r = call_action_by_name(sess, &action, vec![Value::Char(c)], sp.clone())?;
                    match r {
                        Value::Char(_) | Value::Str(_) => { /* still text-like */ }
                        _ => { any_non_text = true; }
                    }
                    results.push(r);
                }

                // If everything is text-like, collapse to a single string
                if !results.is_empty() && !any_non_text {
                    let mut out = String::new();
                    for r in results {
                        match r {
                            Value::Char(ch) => out.push(ch),
                            Value::Str(ts)  => out.push_str(&ts),
                            _ => unreachable!(),
                        }
                    }
                    return Ok(Value::Str(out));
                }

                // Mixed types -> return array
                return Ok(Value::Array(results));
            }

            // --- Array/Seq case (unchanged logic, but allow seq as well) ---
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "map expects array/seq/string as first argument", sp.clone()))?;

            let mut out = Vec::with_capacity(xs.len());
            for v in xs {
                let r = call_action_by_name(sess, &action, vec![v.clone()], sp.clone())?;
                out.push(r);
            }
            Value::Array(out)
        },

        // ---------- Collections: unique / dups ----------
        "unique" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    use std::collections::BTreeSet;
                    let mut seen = BTreeSet::new();
                    let mut out = String::new();
                    for c in s.chars() {
                        if seen.insert(c) { out.push(c); }
                    }
                    Value::Str(out)
                }
                _ => {
                    let s = as_array_like(&args[0]).ok_or_else(|| rt("T0401","unique expects array/seq/string", sp.clone()))?;
                    use std::collections::BTreeSet;
                    let mut seen = BTreeSet::<String>::new();
                    let mut outv = Vec::with_capacity(s.len());
                    for v in s {
                        let k = fmt_value_raw(v);
                        if seen.insert(k) { outv.push(v.clone()); }
                    }
                    Value::Array(outv)
                }
            }
        }

        "dups" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    use std::collections::BTreeMap;
                    let mut cnt = BTreeMap::<char, usize>::new();
                    for c in s.chars() { *cnt.entry(c).or_insert(0) += 1; }
                    let mut out = String::new();
                    for (c, n) in cnt { if n >= 2 { out.push(c); } }
                    Value::Str(out)
                }
                _ => {
                    let s = as_array_like(&args[0]).ok_or_else(|| rt("T0401","dups expects array/seq/string", sp.clone()))?;
                    use std::collections::BTreeMap;
                    let mut cnt = BTreeMap::<String, (usize, Value)>::new();
                    for v in s {
                        let k = fmt_value_raw(v);
                        cnt.entry(k).and_modify(|e| e.0+=1).or_insert((1, v.clone()));
                    }
                    let mut out = Vec::new();
                    for (_, (n, exemplar)) in cnt { if n >= 2 { out.push(exemplar); } }
                    Value::Array(out)
                }
            }
        }

        // collections CRUD style
        "grab_first" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    if s.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    let c = s.chars().next().unwrap();
                    Value::Char(c)
                }
                _ => {
                    let xs = as_array_like(&args[0])
                        .ok_or_else(|| rt("T0401","grab_first expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    xs[0].clone()
                }
            }
        }

        "grab_last" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    if s.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    let c = s.chars().rev().next().unwrap();
                    Value::Char(c)
                }
                _ => {
                    let xs = as_array_like(&args[0])
                        .ok_or_else(|| rt("T0401","grab_last expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    xs[xs.len()-1].clone()
                }
            }
        }

        "grab_at" => {
            arity(2)?;
            let idx = match &args[1] {
                Value::Float(n) if *n >= 0.0 && n.fract()==0.0 => *n as usize,
                _ => return Err(rt("T0201","index must be a non-negative integer", sp.clone())),
            };
            match &args[0] {
                Value::Str(s) => {
                    let n = s.chars().count();
                    if idx >= n { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    let c = s.chars().nth(idx).unwrap();
                    Value::Char(c)
                }
                _ => {
                    let xs = as_array_like(&args[0])
                        .ok_or_else(|| rt("T0401","grab_at expects array/seq/string", sp.clone()))?;
                    if idx >= xs.len() { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    xs[idx].clone()
                }
            }
        }

        "grab_where" => {
            arity(2)?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "grab_where expects the predicate action name as a string", sp.clone())),
            };

            match &args[0] {
                Value::Str(s) => {
                    let mut out = String::new();
                    for c in s.chars() {
                        let ok_v = call_action_by_name(sess, &pred, vec![Value::Char(c)], sp.clone())?;
                        if matches!(ok_v, Value::Bool(true)) {
                            out.push(c);
                        }
                    }
                    Value::Str(out)
                }
                _ => {
                    let xs = as_array_like(&args[0])
                        .ok_or_else(|| rt("T0401","grab_where expects array/seq/string", sp.clone()))?;
                    let mut out_vec = Vec::new();
                    for v in xs.iter() {
                        let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                        if matches!(ok_v, Value::Bool(true)) { out_vec.push(v.clone()); }
                    }
                    Value::Array(out_vec)
                }
            }
        }

        "grab_all" => {
            arity(1)?;
            match &args[0] {
                Value::Str(_) | Value::Array(_) | Value::Seq(_) | Value::Map(_) => args[0].clone(),
                _ => return Err(rt("T0401", "grab_all expects a collection", sp.clone())),
            }
        }

        "put_first" => {
            arity(2)?;
            match (&args[0], &args[1]) {
                (Value::Str(s), Value::Str(sub)) => Value::Str(format!("{}{}", sub, s)),
                (Value::Str(_), _) => return Err(rt("T0205","put_first(string, ...) expects a string to insert", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","put_first expects array/seq/string", sp.clone()))?;
                    let mut out = Vec::with_capacity(xs.len()+1);
                    out.push(args[1].clone());
                    out.extend(xs.iter().cloned());
                    Value::Array(out)
                }
            }
        }

        "put_last" => {
            arity(2)?;
            match (&args[0], &args[1]) {
                (Value::Str(s), Value::Str(sub)) => Value::Str(format!("{}{}", s, sub)),
                (Value::Str(_), _) => return Err(rt("T0205","put_last(string, ...) expects a string to insert", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","put_last expects array/seq/string", sp.clone()))?;
                    let mut out = xs.to_vec();
                    out.push(args[1].clone());
                    Value::Array(out)
                }
            }
        }

        "put_at" => {
            arity(3)?;
            let idx = match &args[1] {
                Value::Float(n) if *n >= 0.0 && n.fract()==0.0 => *n as usize,
                _ => return Err(rt("T0201","index must be a non-negative integer", sp.clone())),
            };
            match (&args[0], &args[2]) {
                (Value::Str(s), Value::Str(sub)) => {
                    let n = char_len(s);
                    if idx > n { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    Value::Str(str_insert_at(s, idx, sub).unwrap())
                }
                (Value::Str(_), _) => return Err(rt("T0205","put_at(string, ...) expects a string to insert", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","put_at expects array/seq/string", sp.clone()))?;
                    if idx > xs.len() { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    let mut out = Vec::with_capacity(xs.len()+1);
                    out.extend(xs.iter().take(idx).cloned());
                    out.push(args[2].clone());
                    out.extend(xs.iter().skip(idx).cloned());
                    Value::Array(out)
                }
            }
        }

        "update_first" => {
            arity(2)?;
            match (&args[0], &args[1]) {
                (Value::Str(s), Value::Str(with)) => {
                    if s.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Str(str_update_at(s, 0, with).unwrap())
                }
                (Value::Str(_), _) => return Err(rt("T0205","update_first(string, ...) expects a string", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","update_first expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    let mut out = xs.to_vec();
                    out[0] = args[1].clone();
                    Value::Array(out)
                }
            }
        }

        "update_last" => {
            arity(2)?;
            match (&args[0], &args[1]) {
                (Value::Str(s), Value::Str(with)) => {
                    let n = char_len(s);
                    if n == 0 { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Str(str_update_at(s, n-1, with).unwrap())
                }
                (Value::Str(_), _) => return Err(rt("T0205","update_last(string, ...) expects a string", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","update_last expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    let mut out = xs.to_vec();
                    let k = out.len()-1;
                    out[k] = args[1].clone();
                    Value::Array(out)
                }
            }
        }

        "update_at" => {
            arity(3)?;
            let idx = match &args[1] {
                Value::Float(n) if *n >= 0.0 && n.fract()==0.0 => *n as usize,
                _ => return Err(rt("T0201","index must be a non-negative integer", sp.clone())),
            };
            match (&args[0], &args[2]) {
                (Value::Str(s), Value::Str(with)) => {
                    let n = char_len(s);
                    if idx >= n { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    Value::Str(str_update_at(s, idx, with).unwrap())
                }
                (Value::Str(_), _) => return Err(rt("T0205","update_at(string, ...) expects a string", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","update_at expects array/seq/string", sp.clone()))?;
                    if idx >= xs.len() { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    let mut out = xs.to_vec();
                    out[idx] = args[2].clone();
                    Value::Array(out)
                }
            }
        }

        "update_where" => {
            arity(3)?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205","update_where expects a predicate action name (string)", sp.clone())),
            };
            match (&args[0], &args[2]) {
                (Value::Str(s), Value::Str(with)) => {
                    let mut out = String::new();
                    for i in 0..char_len(s) {
                        let ch = slice_char(s, i).unwrap();
                        let ok_v = call_action_by_name(sess, &pred, vec![Value::Str(ch.clone())], sp.clone())?;
                        if want_bool(&ok_v, "update_where predicate")? { out.push_str(with); } else { out.push_str(&ch); }
                    }
                    Value::Str(out)
                }
                (Value::Str(_), _) => return Err(rt("T0205","update_where on string expects a string replacement", sp.clone())),
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","update_where expects array/seq/string", sp.clone()))?;
                    let withv = args[2].clone();
                    let mut out: Vec<Value> = Vec::with_capacity(xs.len());
                    for v in xs.iter() {
                        let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                        out.push(if want_bool(&ok_v, "update_where predicate")? { withv.clone() } else { v.clone() });
                    }
                    Value::Array(out)
                }
            }
        }

        "update_all" => {
            if args.len() != 2 {
                return Err(rt("A0402",
                    format!("wrong number of arguments: expected 2, got {}", args.len()),
                    sp.clone()));
            }
            let withv = args[1].clone();
            match &args[0] {
                Value::Array(xs) => Value::Array(vec![withv; xs.len()]),
                Value::Seq(s)    => Value::Seq(Seq::from_vec(vec![withv; s.len()])),
                Value::Str(s)    => {
                    let sub = match &withv {
                        Value::Str(t) => t.clone(),
                        _ => return Err(rt("T0205", "update_all(string, ...) expects a string replacement", sp.clone())),
                    };
                    let n = char_len(s);
                    Value::Str(sub.repeat(n))
                }
                _ => return Err(rt("T0401", "update_all expects array/seq/string", sp.clone())),
            }
        }

        "delete_first" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    if char_len(s) == 0 { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Str(str_delete_at(s, 0).unwrap())
                }
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","delete_first expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Array(xs.iter().skip(1).cloned().collect())
                }
            }
        }

        "delete_last" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    let n = char_len(s);
                    if n == 0 { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Str(str_delete_at(s, n-1).unwrap())
                }
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","delete_last expects array/seq/string", sp.clone()))?;
                    if xs.is_empty() { return Err(rt("R0701","empty", sp.clone())); }
                    Value::Array(xs.iter().take(xs.len()-1).cloned().collect())
                }
            }
        }

        "delete_at" => {
            arity(2)?;
            let idx = match &args[1] {
                Value::Float(n) if *n >= 0.0 && n.fract()==0.0 => *n as usize,
                _ => return Err(rt("T0201","index must be a non-negative integer", sp.clone())),
            };
            match &args[0] {
                Value::Str(s) => {
                    let n = char_len(s);
                    if idx >= n { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    Value::Str(str_delete_at(s, idx).unwrap())
                }
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","delete_at expects array/seq/string", sp.clone()))?;
                    if idx >= xs.len() { return Err(rt("R0402","index out of bounds", sp.clone())); }
                    let mut out = Vec::with_capacity(xs.len().saturating_sub(1));
                    for (i, v) in xs.iter().enumerate() {
                        if i != idx { out.push(v.clone()); }
                    }
                    Value::Array(out)
                }
            }
        }

        "delete_where" => {
            arity(2)?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205","delete_where expects a predicate action name (string)", sp.clone())),
            };
            match &args[0] {
                Value::Str(s) => {
                    let mut out = String::new();
                    for i in 0..char_len(s) {
                        let ch = slice_char(s, i).unwrap();
                        let ok_v = call_action_by_name(sess, &pred, vec![Value::Str(ch.clone())], sp.clone())?;
                        if !want_bool(&ok_v, "delete_where predicate")? { out.push_str(&ch); }
                    }
                    Value::Str(out)
                }
                _ => {
                    let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401","delete_where expects array/seq/string", sp.clone()))?;
                    let mut kept: Vec<Value> = Vec::with_capacity(xs.len());
                    for v in xs.iter() {
                        let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                        if !want_bool(&ok_v, "delete_where predicate")? { kept.push(v.clone()); }
                    }
                    Value::Array(kept)
                }
            }
        }

        "delete_all" => {
            arity(1)?;
            match &args[0] {
                Value::Str(_)   => Value::Str(String::new()),
                Value::Array(_) => Value::Array(vec![]),
                Value::Seq(_)   => Value::Seq(Seq::from_vec(vec![])),
                Value::Map(_)   => { use std::collections::BTreeMap; Value::Map(BTreeMap::new()) }
                _ => return Err(rt("T0401","delete_all expects a collection", sp.clone())),
            }
        }

        // ===== Replace & remove =====
        "reap" => {
            // Expect exactly one config map: { src: <array|seq>, count?: int }
            if args.len() != 1 {
                return Err(rt(
                    "A0402",
                    format!("wrong number of arguments: expected 1, got {}", args.len()),
                    sp.clone()
                ));
            }
            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => return Err(rt("T0401", "reap expects a config object", sp.clone())),
            };

            // Pull src
            let srcv = cfg.get("src")
                .ok_or_else(|| rt("T0401", "reap: missing 'src'", sp.clone()))?;

            // Accept both Array and Seq via the read-only array-like view
            let s: &[Value] = as_array_like(srcv)
                .ok_or_else(|| rt("T0401", "reap: 'src' must be an array/seq", sp.clone()))?;

            // Resolve count (default 1), must be positive integer
            let n_out: usize = match cfg.get("count") {
                None => 1,
                Some(Value::Float(n)) if *n > 0.0 && n.fract()==0.0 => *n as usize,
                Some(_) => return Err(rt("T0201", "reap 'count' must be a positive integer", sp.clone())),
            };

            if s.is_empty() {
                return Err(rt("R0701", "cannot reap from an empty collection", sp.clone()));
            }
            if n_out > s.len() {
                return Err(rt(
                    "R0701",
                    format!("not enough to sample: requested {n_out}, have {}", s.len()),
                    sp.clone()
                ));
            }

            // Sample n unique indices without replacement (partial Fisher–Yates over indices)
            let len = s.len();
            let mut idxs: Vec<usize> = (0..len).collect();
            for i in 0..n_out {
                let j = i + rng_index(sess, len - i);
                idxs.swap(i, j);
            }

            // Collect items in draw order
            let mut items: Vec<Value> = Vec::with_capacity(n_out);
            for &i in &idxs[..n_out] {
                items.push(s[i].clone());
            }

            // Return: single value if count==1, else array
            if n_out == 1 {
                items.pop().unwrap()
            } else {
                Value::Array(items)
            }
        }

        "replace" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let s    = want_str(&args[0], "replace")?;
            let from = want_str(&args[1], "replace")?;
            let to   = want_str(&args[2], "replace")?;
            if from.is_empty() {
                Value::Str(s)
            } else {
                Value::Str(s.replace(&from, &to))
            }
        }


        // ===== Slice / extract =====
        "before" => {
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "before")?;
            let sep = want_str(&args[1], "before")?;
            match s.find(&sep) { Some(i) => Value::Str(s[..i].to_string()), None => Value::Str(s) }
        }
        "after" => {
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "after")?;
            let sep = want_str(&args[1], "after")?;
            match s.find(&sep) { Some(i) => Value::Str(s[i + sep.len()..].to_string()), None => Value::Str(String::new()) }
        }
        "before_last" => {
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "before_last")?;
            let sep = want_str(&args[1], "before_last")?;
            match s.rfind(&sep) { Some(i) => Value::Str(s[..i].to_string()), None => Value::Str(s) }
        }
        "after_last" => {
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "after_last")?;
            let sep = want_str(&args[1], "after_last")?;
            match s.rfind(&sep) { Some(i) => Value::Str(s[i + sep.len()..].to_string()), None => Value::Str(String::new()) }
        }
        "between" => { // first left … right after that
            if args.len() != 3 { return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "between")?;
            let left  = want_str(&args[1], "between")?;
            let right = want_str(&args[2], "between")?;
            if let Some(i) = s.find(&left) {
                let jstart = i + left.len();
                if let Some(jrel) = s[jstart..].find(&right) {
                    let j = jstart + jrel;
                    Value::Str(s[jstart..j].to_string())
                } else {
                    Value::Str(String::new())
                }
            } else {
                Value::Str(String::new())
            }
        }

        // ===== Split & join =====
        "lines" => { // split on '\n'
            if args.len() != 1 { return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "lines")?;
            Value::Array(s.split('\n').map(|t| Value::Str(t.to_string())).collect())
        }
        "words" => {
            if args.len() != 1 { return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "words")?;
            Value::Array(s.split_whitespace().map(|t| Value::Str(t.to_string())).collect())
        }
        "chars" => {
            // chars(string) -> Array<Char>
            if args.len() != 1 { return Err(rt("A0402",
                format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "chars")?;
            Value::Array(s.chars().map(Value::Char).collect())
        }
        "split" => { // split by separator (string)
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "split")?;
            let sep = want_str(&args[1], "split")?;
            if sep.is_empty() {
                Value::Array(s.chars().map(|c| Value::Str(c.to_string())).collect())
            } else {
                Value::Array(s.split(&sep).map(|t| Value::Str(t.to_string())).collect())
            }
        }
        "join" => {
            arity(2)?;
            let xs = as_array_like(&args[0]).ok_or_else(|| rt("T0401", "join expects an array/seq", sp.clone()))?;
            let sep = want_str(&args[1], "join")?;
            let mut out = String::new();
            for (i, v) in xs.iter().enumerate() {
                let piece = match v {
                    Value::Str(s)  => s.clone(),
                    Value::Char(c) => c.to_string(),           // NEW
                    _ => return Err(rt("T0205", "join expects array of strings/chars", sp.clone())),
                };
                if i > 0 { out.push_str(&sep); }
                out.push_str(&piece);
            }
            Value::Str(out)
        }

        // ===== Other transforms =====
        "reverse" => {
            // reverse the ORDER of a collection (pure)
            arity(1)?;
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "reverse expects array/seq", sp.clone()))?;
            let mut v: Vec<Value> = xs.to_vec();
            v.reverse();
            Value::Array(v)
        }

        "reverse_chars" => {
            // reverse characters (string or array-of-strings)
            arity(1)?;
            map_str_1(&args[0], "reverse_chars", &|s| s.chars().rev().collect())?
        }

        "minimize" => {
            arity(1)?;
            let f = |s: &str| {
                let mut out = String::new();
                let mut in_ws = false;
                for ch in s.chars() {
                    if ch.is_whitespace() {
                        if !in_ws { out.push(' '); in_ws = true; }
                    } else { in_ws = false; out.push(ch); }
                }
                out.trim().to_string()
            };
            map_str_1(&args[0], "minimize", &f)?
        }
        "parse_bool" => { // "true"/"false" (case-insensitive); else error
            if args.len() != 1 { return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "parse_bool")?;
            match s.to_ascii_lowercase().as_str() {
                "true"  => Value::Bool(true),
                "false" => Value::Bool(false),
                _ => return Err(rt("T0205", "parse_bool expects 'true' or 'false'", sp.clone())),
            }
        }

        "len" => {
            if args.len() != 1 {
                return Err(rt("A0402",
                    format!("wrong number of arguments: expected 1, got {}", args.len()),
                    sp.clone()));
            }
            match &args[0] {
                Value::Str(s)    => Value::Int(s.chars().count() as i64), // Unicode scalar count
                Value::Array(xs) => Value::Int(xs.len() as i64),
                Value::Seq(xs)   => Value::Int(xs.len() as i64),
                Value::Map(m)    => Value::Int(m.len() as i64),
                _ => return Err(rt("T0401", "len expects string or collection", sp.clone())),
            }
        }

        "backend" => {
            if args.len() != 1 { return Err(rt("A0402", "backend expects 1 receiver", sp.clone())); }
            match &args[0] {
                Value::Seq(xs)   => Value::Str(xs.backend_name().into()),
                Value::Array(_)  => Value::Str("array(legacy)".into()),
                _ => return Err(rt("T0401", "backend expects a collection", sp.clone())),
            }
        }

        "metrics" => {
            if args.len() != 1 { return Err(rt("A0402", "metrics expects 1 receiver", sp.clone())); }
            match &args[0] {
                Value::Seq(xs)   => Value::Map(xs.metrics_map()),
                Value::Array(xs) => {
                    // legacy metrics for plain arrays
                    let mut m = BTreeMap::new();
                    m.insert("len".into(), Value::Int(xs.len() as i64));
                    m.insert("backend".into(), Value::Str("array(legacy)".into()));
                    Value::Map(m)
                }
                _ => return Err(rt("T0401", "metrics expects a collection", sp.clone())),
            }
        }

        // ----- JSON -----
        "json_parse" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s  = want_str(&v0, "json_parse")?;
            let vj: sj::Value = sj::from_str(&s)
                .map_err(|e| rt("J0001", format!("json parse failed: {e}"), sp.clone()))?;
            from_json(&vj)
        }
        "json_stringify" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s = sj::to_string(&to_json(&v0))
                .map_err(|e| rt("J0002", format!("json stringify failed: {e}"), sp.clone()))?;
            Value::Str(s)
        }
        "json_stringify_pretty" => {
            arity(1)?;
            let v0 = args[0].clone();
            let s = sj::to_string_pretty(&to_json(&v0))
                .map_err(|e| rt("J0002", format!("json stringify failed: {e}"), sp.clone()))?;
            Value::Str(s)
        }
        "read_json" => {
            arity(1)?;
            let vpath = args[0].clone();
            let path  = want_str(&vpath, "read_json path")?;
            let txt   = std::fs::read_to_string(&path)
                .map_err(|e| rt("J0003", format!("read_json: {e}"), sp.clone()))?;
            let vj: sj::Value = sj::from_str(&txt)
                .map_err(|e| rt("J0001", format!("json parse failed: {e}"), sp.clone()))?;
            from_json(&vj)
        }
        "write_json!" => {
            // write_json!(path, value, pretty=false)
            if args.len() < 2 || args.len() > 3 {
                return Err(rt("A0402", format!("write_json! expects 2 or 3 args, got {}", args.len()), sp.clone()));
            }
            let vpath = args[0].clone();
            let vval   = args[1].clone();
            let pretty = if args.len() == 3 {
                let vpretty = args[2].clone();
                as_bool(vpretty, sp.clone(), "write_json! pretty")?
            } else { false };

            let j = to_json(&vval);
            let out = if pretty {
                sj::to_string_pretty(&j)
            } else {
                sj::to_string(&j)
            }.map_err(|e| rt("J0002", format!("json stringify failed: {e}"), sp.clone()))?;

            let path = want_str(&vpath, "write_json! path")?;
            std::fs::write(&path, out)
                .map_err(|e| rt("J0004", format!("write_json!: {e}"), sp.clone()))?;
            Value::Unit
        }

        // ----- Unknown -----
        other => return Err(rt("A0401", format!("unknown action '{}'", other), sp.clone())),
    };

    Ok(out)
}

// ===================== Evaluation =====================

fn eval_expr(e: &ast::Expr, sess: &mut Session) -> Result<Value, Diag> {
    fn mutate_via_call_name(
        sess: &mut Session,
        name: &str,        // "put_at!"
        recv_ident: Option<&str>,
        arg_exprs: &[ast::Expr],
        sp: Span,
    ) -> Result<Value, Diag> {
        let base = name.strip_suffix('!')
            .ok_or_else(|| rt("R0800", "internal: expected bang name", sp.clone()))?;

        // Determine the target variable name and build argv for the pure version
        let (target_name, argv_vals): (String, Vec<Value>) = if let Some(base_ident) = recv_ident {
            // Receiver style: xs.put_at!(...)
            let mut vals = Vec::with_capacity(arg_exprs.len() + 1);
            // receiver value first
            let recv_val = sess.get_var(base_ident)
                .cloned()
                .ok_or_else(|| rt("R0110", format!("unknown identifier '{}'", base_ident), sp.clone()))?;
            vals.push(recv_val);
            // then args
            for a in arg_exprs { vals.push(eval_expr(a, sess)?); }
            (base_ident.to_string(), vals)
        } else {
            // Free-call style: put_at!(xs, ...)
            if arg_exprs.is_empty() {
                return Err(rt("A0402",
                    format!("'{}' requires a target variable as first argument", name), sp.clone()));
            }
            let tgt = match &arg_exprs[0] {
                ast::Expr::Ident(n, _) => n.clone(),
                other => {
                    return Err(rt("P0802",
                        &format!("'{}' requires a variable name (not an expression) as first argument", name),
                        span_of_expr(other)));
                }
            };
            let mut vals = Vec::with_capacity(arg_exprs.len());
            for a in arg_exprs { vals.push(eval_expr(a, sess)?); }
            (tgt, vals)
        };

        // Special-case destructive reap!: mutate target and return removed value(s)
        if base == "reap" {
            // Figure out target variable & optional count expr
            let (target_name, count_expr_opt): (String, Option<&ast::Expr>) = if let Some(base_ident) = recv_ident {
                // xs.reap!() or xs.reap!(count)
                (base_ident.to_string(), arg_exprs.get(0))
            } else {
                // reap!(xs) or reap!(xs, count)
                let Some(first) = arg_exprs.get(0) else {
                    return Err(rt("A0402","reap! expects (xs) or (xs, count)", sp.clone()));
                };
                let ast::Expr::Ident(n, _) = first else {
                    return Err(rt("P0802","reap!: first argument must be a variable name", span_of_expr(first)));
                };
                (n.clone(), arg_exprs.get(1))
            };

            // Parse count (default 1)
            let count: usize = if let Some(e) = count_expr_opt {
                let v = eval_expr(e, sess)?;
                let n = as_num(v, span_of_expr(e), "reap!(..., count)")?;
                if n <= 0.0 || n.fract() != 0.0 {
                    return Err(rt("T0201","reap! count must be a positive integer", sp.clone()));
                }
                n as usize
            } else { 1 };

            // 1) Read-only: figure out length and type *without* mut-borrowing slot
            enum CollKind { Arr(usize), Seq(usize), Str(usize) }
            let kind_len = match sess.get_var(&target_name) {
                Some(Value::Array(xs)) => {
                    if xs.is_empty() { return Err(rt("R0701","cannot reap from an empty collection", sp.clone())); }
                    CollKind::Arr(xs.len())
                }
                Some(Value::Seq(xs)) => {
                    let len = xs.len();
                    if len == 0 { return Err(rt("R0701","cannot reap from an empty collection", sp.clone())); }
                    CollKind::Seq(len)
                }
                Some(Value::Str(s)) => {
                    let n = s.chars().count();
                    if n == 0 { return Err(rt("R0701","cannot reap from an empty string", sp.clone())); }
                    CollKind::Str(n)
                }
                Some(_) => return Err(rt("T0401","reap! expects an array/seq/string variable", sp.clone())),
                None => return Err(rt("R0110", format!("unknown identifier '{}'", target_name), sp.clone())),
            };

            let len = match kind_len { CollKind::Arr(n)|CollKind::Seq(n)|CollKind::Str(n) => n };
            if count > len {
                return Err(rt("R0701", format!("not enough to sample: requested {count}, have {len}"), sp.clone()));
            }

            // 2) RNG picks BEFORE any mutable borrow of the slot
            fn sample_indices(sess: &mut Session, len: usize, k: usize) -> Vec<usize> {
                let mut idxs: Vec<usize> = (0..len).collect();
                for i in 0..k {
                    let j = i + rng_bounded(sess, (len - i) as u64) as usize;
                    idxs.swap(i, j);
                }
                idxs[..k].to_vec()
            }
            let picks = sample_indices(sess, len, count);

            // 3) Now mut-borrow slot and remove at descending indices
            let slot = sess.get_var_mut(&target_name)
                .ok_or_else(|| rt("R0110", format!("unknown identifier '{}'", target_name), sp.clone()))?;

            let finish_vals = |mut items: Vec<Value>| -> Value {
                if items.len() == 1 { items.pop().unwrap() } else { Value::Array(items) }
            };

            match slot {
                // Arrays
                Value::Array(vecd) => {
                    let mut removed: Vec<Value> = Vec::with_capacity(count);
                    let mut sorted = picks.clone();
                    sorted.sort_unstable_by(|a,b| b.cmp(a)); // remove safely
                    for i in sorted { removed.push(vecd.remove(i)); }
                    removed.reverse(); // report in draw order
                    return Ok(finish_vals(removed));
                }
                // Adaptive seq
                Value::Seq(seq) => {
                    let mut removed: Vec<Value> = Vec::with_capacity(count);
                    let mut sorted = picks.clone();
                    sorted.sort_unstable_by(|a,b| b.cmp(a));
                    for i in sorted {
                        if let Some(v) = seq.remove(i) { removed.push(v); }
                    }
                    removed.reverse();
                    return Ok(finish_vals(removed));
                }
                // Strings (Unicode scalar semantics)
                Value::Str(s) => {
                    // Build removed string from the original contents (draw order)
                    let original = s.clone();
                    let mut removed_s = String::new();
                    for idx in &picks {
                        if let Some(ch) = slice_char(&original, *idx) {
                            removed_s.push_str(&ch);
                        }
                    }
                    // Mutate the string by deleting chosen scalars (descending index order)
                    let mut sorted = picks.clone();
                    sorted.sort_unstable_by(|a,b| b.cmp(a));
                    for idx in sorted {
                        if let Some(new_s) = str_delete_at(s, idx) { *s = new_s; }
                    }
                    return Ok(Value::Str(removed_s));
                }
                _ => return Err(rt("T0401","reap! expects an array/seq/string variable", sp.clone())),
            }
        }

        // Call the pure version to compute the updated value
        let updated = call_action_by_name(sess, base, argv_vals, sp.clone())?;

        // Write back to the target binding
        if let Some(slot) = sess.get_var_mut(&target_name) {
            *slot = updated;
            Ok(Value::Unit) // or Ok(slot.clone()) if you want echo
        } else {
            Err(rt("R0110",
                format!("unknown identifier '{}'", target_name), sp.clone()))
        }
    }

    match e {
        // ---- Literals & identifiers ----
        ast::Expr::Nil(_) => Ok(Value::Nil),
        ast::Expr::Bool(b, _) => Ok(Value::Bool(*b)),
        ast::Expr::Number(txt, sp) => {
            // If it's an integer-like literal (no '.' or exponent), keep it exact.
            let raw = txt.as_str();
            let is_integer_like = !raw.contains('.') && !raw.contains('e') && !raw.contains('E');

            if is_integer_like {
                let cleaned: String = raw.chars().filter(|&c| c != '_').collect();

                if let Ok(i) = cleaned.parse::<i64>() {
                    // f64 is exact for |n| <= 2^53
                    let mag: i128 = if i >= 0 { i as i128 } else { -(i as i128) };
                    if mag <= F64_SAFE_INT_MAX as i128 {
                        Ok(Value::Int(i as i64))
                    } else {
                        Ok(Value::Big(Decimal::from_i128_with_scale(i as i128, 0)))
                    }
                } else {
                    // larger than i64 → exact decimal
                    let d = cleaned.parse::<Decimal>()
                        .map_err(|_| rt("P0301", format!("invalid number literal '{raw}'"), sp.clone()))?;
                    Ok(Value::Big(d))
                }
            } else {
                // float-like literal ('.' or exponent) — parse as f64 (underscores already allowed)
                Ok(parse_number_value(raw, sp.clone())?)
            }
        },
        ast::Expr::Str(s, sp) => {
            if s.as_bytes().contains(&b'{') {
                // only simple identifiers are allowed inside { … } at this stage
                let rendered = render_interpolated(s, sess, sp)?;
                Ok(Value::Str(rendered))
            } else {
                Ok(Value::Str(s.clone()))
            }
        }
        ast::Expr::Char(c, _sp) => Ok(Value::Char(*c)),
        ast::Expr::Ident(name, sp) => {
            match sess.get_var(name) {
                Some(v) => Ok(v.clone()),
                None => Err(rt("R0110", format!("unknown identifier '{}'", name), sp.clone())),
            }
        }

        ast::Expr::NsCall(ns, name, _args, sp) => {
            // Keep this explicit until Stage 4 adds namespaced call semantics.
            return Err(rt(
                "R0000",
                format!("namespaced call '{}::{}' not implemented", ns, name),
                sp.clone(),
            ));
        }

        ast::Expr::EnumVariant { enum_name, variant_name, fields, span } => {
            // Look up the enum definition and extract what we need
            let (variant_fields, _enum_exists) = {
                let enum_decl = sess.enums.get(enum_name).ok_or_else(|| Diag {
                    code: "E1002".into(),
                    message: format!("Unknown enum: {}", enum_name),
                    span: span.clone(),
                })?;
                
                // Find the variant
                let variant = enum_decl.variants.iter()
                    .find(|v| &v.name == variant_name)
                    .ok_or_else(|| Diag {
                        code: "E1003".into(),
                        message: format!("Unknown variant '{}' for enum '{}'", variant_name, enum_name),
                        span: span.clone(),
                    })?;
                
                // Clone the field names we need to validate
                (variant.fields.clone(), true)
            }; // enum_decl reference dropped here
            
            // Now we can mutably borrow sess to evaluate fields
            let field_values = if let Some(field_exprs) = fields {
                let mut field_map = BTreeMap::new();
                
                for (field_name, field_expr) in field_exprs {
                    let value = eval_expr(field_expr, sess)?;
                    field_map.insert(field_name.clone(), value);
                }
                
                // Validate that provided fields match the variant's definition
                if let Some(expected_fields) = &variant_fields {
                    for field_decl in expected_fields {
                        if !field_map.contains_key(&field_decl.name) {
                            return Err(Diag {
                                code: "E1004".into(),
                                message: format!("Missing field '{}' for variant '{}'", field_decl.name, variant_name),
                                span: span.clone(),
                            });
                        }
                    }
                }
                
                Some(field_map)
            } else {
                None
            };
            
            Ok(Value::Enum {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: field_values,
            })
        }

        // Plain assignment (ident = expr)
        ast::Expr::Assign(lhs, rhs, _sp) => {
            // Check for field assignment: object >> field = value
            // Could be Binary or Member depending on how parser handles >>
            match &**lhs {
                ast::Expr::Member(obj_expr, field_name, _) => {
                    // Field assignment via Member syntax
                    let var_name = match &**obj_expr {
                        ast::Expr::Ident(n, _) => n.clone(),
                        _ => return Err(rt("P0804", "can only assign to fields of object variables", span_of_expr(lhs))),
                    };
                    
                    let new_value = eval_expr(rhs, sess)?;
                    
                    let obj_slot = sess.get_var_mut(&var_name)
                        .ok_or_else(|| rt("R0110", format!("unknown variable '{}'", var_name), span_of_expr(lhs)))?;
                    
                    match obj_slot {
                        Value::Object { fields, .. } => {
                            fields.insert(field_name.clone(), new_value.clone());
                            return Ok(new_value);
                        }
                        _ => return Err(rt("T0403", "not an object", span_of_expr(lhs))),
                    }
                }
                
                ast::Expr::Binary(obj_expr, op, field_expr, _) if op == ">>" => {
                    // Field assignment via Binary syntax (if parser uses this)
                    let var_name = match &**obj_expr {
                        ast::Expr::Ident(n, _) => n.clone(),
                        _ => return Err(rt("P0804", "can only assign to fields of object variables", span_of_expr(lhs))),
                    };
                    
                    let field_name = match &**field_expr {
                        ast::Expr::Ident(n, _) => n.clone(),
                        _ => return Err(rt("P0804", "field name required after >>", span_of_expr(lhs))),
                    };
                    
                    let new_value = eval_expr(rhs, sess)?;
                    
                    let obj_slot = sess.get_var_mut(&var_name)
                        .ok_or_else(|| rt("R0110", format!("unknown variable '{}'", var_name), span_of_expr(lhs)))?;
                    
                    match obj_slot {
                        Value::Object { fields, .. } => {
                            fields.insert(field_name, new_value.clone());
                            return Ok(new_value);
                        }
                        _ => return Err(rt("T0403", "not an object", span_of_expr(lhs))),
                    }
                }
                
                ast::Expr::Ident(name, _) => {
                    // Simple assignment: name = expr
                    let v = eval_expr(rhs, sess)?;
                    sess.set_var(name.clone(), v.clone());
                    return Ok(v);
                }
                
                _ => {
                    return Err(rt("P0801", "left-hand side of assignment must be a name or field access", span_of_expr(lhs)));
                }
            }
        }

        // ---- Collections ----
        ast::Expr::Array(elems, _sp) => {
            let mut v = Vec::with_capacity(elems.len());
            for e in elems { v.push(eval_expr(e, sess)?); }
            Ok(Value::Array(v))
        }

        ast::Expr::Object(kvs, _sp) => {
            let mut m = BTreeMap::new();
            for (k, vexpr) in kvs {
                let v = eval_expr(vexpr, sess)?;
                m.insert(k.clone(), v);
            }
            Ok(Value::Map(m))
        }

        // ---- Indexing (array / map) ----
        ast::Expr::Index(base, idx, sp) => {
            let b = eval_expr(base, sess)?;
            let i = eval_expr(idx, sess)?;
            match (b, i) {
                (Value::Array(items), Value::Int(n)) => {
                    if n < 0 {
                        return Err(rt("T0201", "index must be a non-negative integer", span_of_expr(idx)));
                    }
                    let k = n as usize;
                    items.get(k).cloned().ok_or_else(|| rt("R0402", "array index out of bounds", sp.clone()))
                }
                (Value::Map(map), Value::Str(key)) => {
                    map.get(&key).cloned().ok_or_else(|| rt("R0403", format!("missing key '{}'", key), sp.clone()))
                }
                (Value::Map(_), _) => Err(rt("T0201", "map index must be a string key", span_of_expr(idx))),
                _ => Err(rt("T0401", "indexing requires an array or map", span_of_expr(base))),
            }
        }

        // arr[start:end]  (strings or arrays)
            ast::Expr::Slice(recv, start_opt, end_opt, sp) => {
                let recv_v = eval_expr(recv, sess)?;
                match recv_v {
                    Value::Array(xs) => {
                        let len = xs.len();
                        let s_ix = if let Some(s) = start_opt {
                            let sv = eval_expr(s, sess)?;
                            want_usize_index(sv, "slice start", sp.clone())? as isize
                        } else { 0 };
                        let e_ix = if let Some(e) = end_opt {
                            let ev = eval_expr(e, sess)?;
                            want_usize_index(ev, "slice end", sp.clone())? as isize
                        } else { len as isize };

                        let (s, e) = clamp_range(s_ix, e_ix, len);
                        if s >= e { return Ok(Value::Array(vec![])); }
                        Ok(Value::Array(xs[s..e].to_vec()))
                    }

                    Value::Str(s) => {
                        let len = char_len(&s);
                        let s_ix = if let Some(st) = start_opt {
                            let sv = eval_expr(st, sess)?;
                            want_usize_index(sv, "slice start", sp.clone())? as isize
                        } else { 0 };
                        let e_ix = if let Some(en) = end_opt {
                            let ev = eval_expr(en, sess)?;
                            want_usize_index(ev, "slice end", sp.clone())? as isize
                        } else { len as isize };

                        let (s_i, e_i) = clamp_range(s_ix, e_ix, len);
                        if s_i >= e_i { return Ok(Value::Str(String::new())); }

                        let b0 = byte_ix_at_char(&s, s_i).unwrap();
                        let b1 = byte_ix_at_char(&s, e_i).unwrap();
                        Ok(Value::Str(s[b0..b1].to_string()))
                    }

                    _ => Err(rt("T0401", "slice expects an array or string", sp.clone())),
                }
            }

            // arr[start:end:step]  (step > 0 only, end-exclusive)
            ast::Expr::Slice3(recv, start_opt, end_opt, step_opt, sp) => {
                let recv_v = eval_expr(recv, sess)?;

                // evaluate indices (defaults)
                let step_v: usize = if let Some(stp) = step_opt {
                    let vv = eval_expr(stp, sess)?;
                    let u = want_usize_index(vv, "slice step", sp.clone())?;
                    if u == 0 { return Err(rt("T0201", "slice step must be a positive integer", sp.clone())); }
                    u
                } else { 1 };

                match recv_v {
                    Value::Array(xs) => {
                        let len = xs.len();
                        let s_ix = if let Some(s) = start_opt {
                            let sv = eval_expr(s, sess)?;
                            want_usize_index(sv, "slice start", sp.clone())? as isize
                        } else { 0 };
                        let e_ix = if let Some(e) = end_opt {
                            let ev = eval_expr(e, sess)?;
                            want_usize_index(ev, "slice end", sp.clone())? as isize
                        } else { len as isize };

                        let (s, e) = clamp_range(s_ix, e_ix, len);
                        if s >= e { return Ok(Value::Array(vec![])); }

                        let mut out = Vec::new();
                        let mut i = s;
                        while i < e {
                            out.push(xs[i].clone());
                            i = i.saturating_add(step_v);
                        }
                        Ok(Value::Array(out))
                    }

                    Value::Str(s) => {
                        let len = char_len(&s);
                        let s_ix = if let Some(st) = start_opt {
                            let sv = eval_expr(st, sess)?;
                            want_usize_index(sv, "slice start", sp.clone())? as isize
                        } else { 0 };
                        let e_ix = if let Some(en) = end_opt {
                            let ev = eval_expr(en, sess)?;
                            want_usize_index(ev, "slice end", sp.clone())? as isize
                        } else { len as isize };

                        let (s_i, e_i) = clamp_range(s_ix, e_ix, len);
                        if s_i >= e_i { return Ok(Value::Str(String::new())); }

                        // Build by chars to honor Unicode scalars and step
                        let mut out = String::new();
                        let mut idx = s_i;
                        while idx < e_i {
                            let ch = slice_char(&s, idx).unwrap();   // one-character string
                            out.push_str(&ch);
                            idx = idx.saturating_add(step_v);
                        }
                        Ok(Value::Str(out))
                    }

                    _ => Err(rt("T0401", "slice expects an array or string", sp.clone())),
                }
            }

       // ---- Member access on maps (syntax from parser; you're not using dot in code, but handle it) ----
       ast::Expr::Member(base, name, sp) => {
           let base_v = eval_expr(base, sess)?;
           match base_v {
               Value::Map(map) => {
                   match map.get(name) {
                       Some(v) => Ok(v.clone()),
                       None => Err(rt("R0403", format!("missing key '{}'", name), sp.clone())),
                   }
               }
               Value::Object { fields, .. } => {
                   fields.get(name)
                       .cloned()
                       .ok_or_else(|| rt("R0403", format!("no field '{}'", name), sp.clone()))
               }
               Value::Enum { fields: Some(field_map), variant_name, .. } => {
                   field_map.get(name)
                       .cloned()
                       .ok_or_else(|| rt("R0404", format!("variant '{}' has no field '{}'", variant_name, name), sp.clone()))
               }
               Value::Enum { fields: None, variant_name, .. } => {
                   Err(rt("E1005", format!("variant '{}' has no fields", variant_name), sp.clone()))
               }
               _ => Err(rt("T0402", "member access requires a map, object, or enum", span_of_expr(base))),
           }
       }

        ast::Expr::OptMember(base, name, _sp) => {
            let base_v = eval_expr(base, sess)?;
            match base_v {
                Value::Nil => Ok(Value::Nil),
                Value::Map(map) => Ok(map.get(name).cloned().unwrap_or(Value::Nil)),
                _ => Err(rt("T0402", "member access requires a map", span_of_expr(base))),
            }
        }

        // ---- Free calls ----
        ast::Expr::FreeCall(name, args, sp) => {
            // Mutating casts for function form when arg is a plain identifier.
            // Examples: float(x), int(x), big(x)
            if matches!(name.as_str(), "float" | "int" | "big" | "str" | "pct" | "f" | "i" | "b" | "string" | "percent")
               && args.len() == 1
            {
                if let ast::Expr::Ident(var_name, _) = &args[0] {
                    let cur = eval_expr(&args[0], sess)?;
                    let out = call_action_by_name(sess, &name, vec![cur], sp.clone())?;
                    sess.set_var(var_name.clone(), out.clone());
                    return Ok(out);
                }
            }
            // ---- bang builtins: put_at!(), delete_all!(), reap!(), ... ----
            if name.ends_with('!') {
                // no receiver here (free call): target must be first argument Ident
                return mutate_via_call_name(sess, name, None, args, sp.clone());
            }

            match name.as_str() {
                // control flow lowered by parser
                "if" => {
                    if args.len() < 2 || args.len() > 3 {
                        return Err(rt("P0315", "if expects [cond, then_block, (else_block)]", sp.clone()));
                    }
                    let cond_v = eval_expr(&args[0], sess)?;
                    if as_bool(cond_v, span_of_expr(&args[0]), "if condition")? {
                        let then_es = expect_array(&args[1], "then_block")?;
                        Ok(eval_expr_list(then_es, sess)?.unwrap_or(Value::Unit))
                    } else if args.len() == 3 {
                        let else_es = expect_array(&args[2], "else_block")?;
                        Ok(eval_expr_list(else_es, sess)?.unwrap_or(Value::Unit))
                    } else {
                        Ok(Value::Unit)
                    }
                }
                "while" => {
                    if args.len() != 2 {
                        return Err(rt("P0316", "while expects [cond, body_block]", sp.clone()));
                    }
                    let body_es = expect_array(&args[1], "body_block")?;
                    sess.loop_depth += 1;
                    'outer: loop {
                        let c = eval_expr(&args[0], sess)?;
                        if !as_bool(c, span_of_expr(&args[0]), "while condition")? { break; }
                        for e in body_es {
                            let v = eval_expr(e, sess)?;
                            match v {
                                Value::CtrlSkip => continue 'outer,
                                Value::CtrlStop => break 'outer,
                                _ => {}
                            }
                        }
                    }
                    sess.loop_depth -= 1;
                    Ok(Value::Unit)
                }

                "skip" => {
                    if sess.loop_depth <= 0 {
                        return Err(rt("R0501", "'skip' used outside of a loop", sp.clone()));
                    }
                    Ok(Value::CtrlSkip)
                }
                "stop" => {
                    if sess.loop_depth <= 0 {
                        return Err(rt("R0502", "'stop' used outside of a loop", sp.clone()));
                    }
                    Ok(Value::CtrlStop)
                }

                // v(n): history lookup (1-based)
                "v" => {
                    if args.len() != 1 {
                        return Err(rt("P0703", "v(n) requires exactly one numeric argument", sp.clone()));
                    }
                    let n_val = eval_expr(&args[0], sess)?;
                    let n = as_num(n_val, span_of_expr(&args[0]), "v(n)")?;
                    if n < 1.0 || n.fract() != 0.0 {
                        return Err(rt("P0703", "v(n) requires a positive integer", sp.clone()));
                    }
                    let idx = n as usize;
                    match sess.get(idx) {
                        Some(v) => Ok(v.clone()),
                        None => Err(rt("R0101", format!("no value at v({}); session has {}", idx, sess.history_len()), sp.clone())),
                    }
                }

                // say: prints value; for strings, render interpolation at print time
                "say" => {
                    let printed = if args.is_empty() { Value::Unit } else { eval_expr(&args[0], sess)? };
                    match printed {
                        Value::Str(s) => {
                            let rendered = render_interpolated(&s, sess, &span_of_expr(&args[0]))?;
                            println!("{}", rendered);
                        }
                        other => println!("{}", fmt_value_raw(&other)),
                    }
                    Ok(Value::Unit)
                }

                // everything else → regular (pure) call
                other_name => {
                    let mut arg_vals = Vec::with_capacity(args.len());
                    for a in args { arg_vals.push(eval_expr(a, sess)?); }
                    call_action_by_name(sess, other_name, arg_vals, sp.clone())
                }
            }
        }

        // ---- Member/optional member calls (receiver becomes first argument) ----
        ast::Expr::Call(base, name, args, sp) => {
            // Mutating casts for function form when arg is a plain identifier.
            if args.is_empty() {
                if matches!(name.as_str(), "float" | "f" | "int" | "i" | "big" | "b" | "str" | "string" | "pct" | "percent") {
                    if let ast::Expr::Ident(var_name, _) = &**base {
                        let recv = eval_expr(base, sess)?;
                        let out  = call_action_by_name(sess, &name, vec![recv], sp.clone())?;
                        sess.set_var(var_name.clone(), out.clone());
                        return Ok(out);
                    }
                }
            }
            
            // Special-case: <expr>.type
            if name == "type" && args.is_empty() {
                let recv = eval_expr(base, sess)?;
                return Ok(Value::Str(value_kind_str(&recv).to_string()));
            }
            
            // Bang methods
            if name.ends_with('!') {
                let recv_ident = match &**base {
                    ast::Expr::Ident(n, _) => Some(n.as_str()),
                    other => {
                        return Err(rt("P0802",
                            &format!("'{}' requires a variable receiver (e.g. xs.{}(...))", name, name),
                            span_of_expr(other)));
                    }
                };
                return mutate_via_call_name(sess, name, recv_ident, args, sp.clone());
            }
            
            // Object method calls - check if receiver is a variable holding an object
            if let ast::Expr::Ident(var_name, _) = &**base {
                let recv = eval_expr(base, sess)?;
                if let Value::Object { class_name, mut fields } = recv {
                    let result = call_object_method(sess, &class_name, &mut fields, name, args, sp.clone())?;
                    
                    // Update the object variable with modified fields
                    let updated_obj = Value::Object { class_name, fields };
                    sess.set_var(var_name.clone(), updated_obj);
                    
                    return Ok(result);
                }
                // Fall through if not an object
            }
            
            // Regular method calls (non-objects or non-variables)
            let recv = eval_expr(base, sess)?;
            let mut argv = Vec::with_capacity(args.len() + 1);
            argv.push(recv);
            for a in args { argv.push(eval_expr(a, sess)?); }
            call_action_by_name(sess, name, argv, sp.clone())
        }

        ast::Expr::OptCall(base, name, args, sp) => {
            // Support ?.type (nil-propagating)
            if name == "type" && args.is_empty() {
                // If syntactically `nil`, short-circuit to nil
                if matches!(&**base, ast::Expr::Nil(_)) { return Ok(Value::Nil); }

                // Otherwise evaluate; nil still short-circuits
                let recv = eval_expr(base, sess)?;
                if matches!(recv, Value::Nil) { return Ok(Value::Nil); }

                // For non-nil, reuse the existing builtin classification
                return call_action_by_name(sess, "type", vec![recv], sp.clone());
            }

            // normal optional-call path
            let recv = eval_expr(base, sess)?;
            if matches!(recv, Value::Nil) { return Ok(Value::Nil); }
            let mut argv: Vec<Value> = Vec::with_capacity(args.len() + 1);
            argv.push(recv);
            for a in args { argv.push(eval_expr(a, sess)?); }
            call_action_by_name(sess, name, argv, sp.clone())
        }

        // ---- Prefix operators ----
        ast::Expr::Prefix(op, expr, sp) => {
            match op.as_str() {
                "-" => {
                    let v = eval_expr(expr, sess)?;
                    let (u, uspec) = take_owned_unformatted(v);

                    let out = match u {
                        Value::Big(d)    => Value::Big(-d),
                        Value::Int(i)    => {
                            if i == i64::MIN { Value::Big(-(rust_decimal::Decimal::from(i))) }
                            else { Value::Int(-i) }
                        }
                        Value::Float(f)  => Value::Float(-f),
                        Value::Pct(p)    => Value::Pct(-p),
                        _other           => return Err(need_number("unary '-'", span_of_expr(expr)).into()),
                    };

                    Ok(reapply_format(out, uspec, None))
                }

                "+" => {
                    let v = eval_expr(expr, sess)?;
                    let (u, uspec) = take_owned_unformatted(v);

                    let out = match u {
                        Value::Big(d)    => Value::Big(d),
                        Value::Int(i)    => Value::Int(i),
                        Value::Float(f)  => Value::Float(f),
                        Value::Pct(p)    => Value::Pct(p),
                        _other            => return Err(need_number("unary '+'", span_of_expr(expr)).into()),
                    };

                    Ok(reapply_format(out, uspec, None))
                }

                "!" | "not" => {
                    let v = eval_expr(expr, sess)?;
                    match v {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err(rt("T0303", "logical 'not' requires a boolean", sp.clone())),
                    }
                }

                _ => Err(rt("R0002", format!("prefix operator '{}' not implemented", op), sp.clone())),
            }
        }

        // ---- Postfix operators ----
        ast::Expr::Postfix(expr, op, sp) => {
            let v = eval_expr(expr, sess)?;
            match op.as_str() {
                "%" => {
                    let n = as_num(v, span_of_expr(expr), "percent literal")?;
                    Ok(Value::Pct(n / 100.0))
                }
                "**" => Ok(Value::Float(as_num(v, span_of_expr(expr), "postfix square")?.powf(2.0))),
                "//" => {
                    let n = as_num(v, span_of_expr(expr), "postfix sqrt")?;
                    if n < 0.0 { return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone())); }
                    Ok(Value::Float(n.sqrt()))
                }
                "!" => {
                    let n = as_num(v, span_of_expr(expr), "factorial")?;
                    if n < 0.0 { return Err(rt("R0202", "factorial requires non-negative integer", sp.clone())); }
                    if n.fract() != 0.0 { return Err(rt("R0203", "factorial requires integer", sp.clone())); }
                    let mut acc: u128 = 1;
                    let k = n as u128;
                    for i in 2..=k { acc = acc.saturating_mul(i); }
                    Ok(Value::Float(acc as f64))
                }
                "^" => Ok(Value::Float(as_num(v, span_of_expr(expr), "ceil")?.ceil())),
                "_" => Ok(Value::Float(as_num(v, span_of_expr(expr), "floor")?.floor())),
                "?" => Ok(Value::Bool(!matches!(v, Value::Nil))),
                _ => Err(rt("R0003", format!("postfix operator '{}' not implemented", op), sp.clone())),
            }
        }

        // ---- Binary & assignment ----
        ast::Expr::Binary(lhs, op, rhs, sp) => {
            match op.as_str() {
                ">>" => {
                    // Field access: object >> field or enum >> field
                    let obj = eval_expr(lhs, sess)?;
                    
                    let field_name = match &**rhs {
                        ast::Expr::Ident(name, _) => name.clone(),
                        _ => return Err(rt("P0803", "right side of >> must be a field name", sp.clone())),
                    };
                    
                    match obj {
                        Value::Object { fields, .. } => {
                            fields.get(&field_name)
                                .cloned()
                                .ok_or_else(|| rt("R0403", format!("no field '{}'", field_name), sp.clone()))
                        }
                        Value::Enum { fields: Some(field_map), variant_name, .. } => {
                            field_map.get(&field_name)
                                .cloned()
                                .ok_or_else(|| rt("R0404", format!("variant '{}' has no field '{}'", variant_name, field_name), sp.clone()))
                        }
                        Value::Enum { fields: None, variant_name, .. } => {
                            Err(rt("E1005", format!("variant '{}' has no fields", variant_name), sp.clone()))
                        }
                        _ => Err(rt("T0403", ">> requires an object or enum on the left side", sp.clone()))
                    }
                }
                // arithmetic
                "+" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = match (&lu, &ru) {
                        (Value::Int(a),   Value::Int(b)) => {
                            if let Some(r) = int_checked_add(*a, *b) { Value::Int(r) }
                            else { Value::Big(rust_decimal::Decimal::from(*a) + rust_decimal::Decimal::from(*b)) }
                        }
                        (Value::Int(a),   Value::Float(b)) | (Value::Float(b), Value::Int(a)) =>
                            Value::Float((*a as f64) + *b),
                        (Value::Float(a), Value::Float(b)) =>
                            Value::Float(*a + *b),
                        (Value::Big(_),   _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; let db = to_decimal(&ru)?;
                            Value::Big(da + db)
                        }
                        (Value::Pct(a),   Value::Pct(b)) => Value::Pct(*a + *b),
                        (Value::Pct(a),   _) => Value::Float(*a + to_f64_for_math(&ru, span_of_expr(rhs), "addition: rhs")?),
                        (_,               Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, span_of_expr(lhs), "addition: lhs")? + *b),
                        _ => return Err(need_number("addition", sp.clone()).into()),
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "++" => {
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    let ls = fmt_value_raw(&lv);
                    let rs = fmt_value_raw(&rv);
                    let out = if ls.is_empty() { rs }
                              else if rs.is_empty() { ls }
                              else { format!("{ls} {rs}") };
                    Ok(Value::Str(out))
                },

                "-" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = match (&lu, &ru) {
                        (Value::Int(a),   Value::Int(b)) => {
                            if let Some(r) = int_checked_sub(*a, *b) { Value::Int(r) }
                            else { Value::Big(rust_decimal::Decimal::from(*a) - rust_decimal::Decimal::from(*b)) }
                        }
                        (Value::Int(a),   Value::Float(b)) => Value::Float((*a as f64) - *b),
                        (Value::Float(a), Value::Int(b))   => Value::Float(*a - (*b as f64)),
                        (Value::Float(a), Value::Float(b)) => Value::Float(*a - *b),
                        (Value::Big(_),   _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; let db = to_decimal(&ru)?;
                            Value::Big(da - db)
                        }
                        (Value::Pct(a),   Value::Pct(b)) => Value::Pct(*a - *b),
                        (Value::Pct(a),   _) => Value::Float(*a - to_f64_for_math(&ru, span_of_expr(rhs), "subtraction: rhs")?),
                        (_,               Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, span_of_expr(lhs), "subtraction: lhs")? - *b),
                        _ => return Err(need_number("subtraction", sp.clone()).into()),
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "*" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = match (&lu, &ru) {
                        (Value::Int(a),   Value::Int(b)) => {
                            if let Some(r) = int_checked_mul(*a, *b) { Value::Int(r) }
                            else { Value::Big(rust_decimal::Decimal::from(*a) * rust_decimal::Decimal::from(*b)) }
                        }
                        (Value::Int(a),   Value::Float(b)) | (Value::Float(b), Value::Int(a)) =>
                            Value::Float((*a as f64) * *b),
                        (Value::Float(a), Value::Float(b)) =>
                            Value::Float(*a * *b),
                        (Value::Big(_),   _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; let db = to_decimal(&ru)?;
                            Value::Big(da * db)
                        }
                        (Value::Pct(a),   Value::Pct(b)) => Value::Pct(*a * *b),
                        (Value::Pct(a),   _) => Value::Float(*a * to_f64_for_math(&ru, span_of_expr(rhs), "multiplication: rhs")?),
                        (_,               Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, span_of_expr(lhs), "multiplication: lhs")? * *b),
                        _ => return Err(need_number("multiplication", sp.clone()).into()),
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "/" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = match (&lu, &ru) {
                        (Value::Int(a),   Value::Int(b)) => {
                            if *b == 0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            if a % b == 0 { Value::Int(a / b) } else { Value::Float((*a as f64) / (*b as f64)) }
                        }
                        (Value::Int(a),   Value::Float(b)) => {
                            if *b == 0.0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Float((*a as f64) / *b)
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            if *b == 0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Float(*a / (*b as f64))
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            if *b == 0.0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Float(*a / *b)
                        }
                        (Value::Big(_),   _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; let db = to_decimal(&ru)?;
                            if db.is_zero() { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Big(da / db)
                        }
                        (Value::Pct(a),   Value::Pct(b)) => {
                            if *b == 0.0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Float(*a / *b)
                        }
                        (Value::Pct(a),   _) => {
                            let denom = to_f64_for_math(&ru, span_of_expr(rhs), "division: rhs")?;
                            if denom == 0.0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            Value::Float(*a / denom)
                        }
                        (_,               Value::Pct(b)) => {
                            if *b == 0.0 { return Err(rt("R0203","division by zero", span_of_expr(rhs))); }
                            let num = to_f64_for_math(&lu, span_of_expr(lhs), "division: lhs")?;
                            Value::Float(num / *b)
                        }
                        _ => return Err(need_number("division", sp.clone()).into()),
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "%" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        // Euclidean-style remainder with Decimal: r = a - floor(a/b) * b
                        let a = to_big_for_math(&lu, span_of_expr(lhs), "modulo: left")?;
                        let b = to_big_for_math(&ru, span_of_expr(rhs), "modulo: right")?;
                        if b.is_zero() { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        let q = (a / b).floor();         // Decimal::floor
                        let r = a - q * b;
                        Value::Big(r)
                    } else {
                        // Keep your existing behavior for f64
                        let a = to_f64_for_math(&lu, span_of_expr(lhs), "modulo: left")?;
                        let b = to_f64_for_math(&ru, span_of_expr(rhs), "modulo: right")?;
                        if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        let q = (a / b).floor();
                        let r = a - q * b;
                        Value::Float(r)
                    };
                    Ok(reapply_format(out, lspec, rspec))
                },
                "//" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        let a = to_big_for_math(&lu, span_of_expr(lhs), "floor division: left")?;
                        let b = to_big_for_math(&ru, span_of_expr(rhs), "floor division: right")?;
                        if b.is_zero() { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        Value::Big((a / b).floor())
                    } else {
                        let a = to_f64_for_math(&lu, span_of_expr(lhs), "floor division: left")?;
                        let b = to_f64_for_math(&ru, span_of_expr(rhs), "floor division: right")?;
                        if (b) == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        Value::Float((a / b).floor())
                    };
                    Ok(reapply_format(out, lspec, rspec))
                }
                "**" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        // Promote base to Decimal
                        let base = to_big_for_math(&lu, span_of_expr(lhs), "power: base")?;

                        // Try to use integer exponent in Decimal space (precise)
                        match &ru {
                            Value::Big(db) => {
                                let e_trunc = db.trunc();
                                if *db == e_trunc {
                                    // integer exponent in Decimal
                                    let n = e_trunc.to_i64().ok_or_else(|| rt("R0203", "big exponent out of i64 range", sp.clone()))?;
                                    Value::Big(decimal_powi(base, n)?)
                                } else {
                                    // fractional exponent -> fall back to f64 powf (precision loss)
                                    let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                                    let ef = db.to_f64().ok_or_else(|| rt("R0205", "big exponent overflow to float", sp.clone()))?;
                                    Value::Float(bf.powf(ef))
                                }
                            }
                            Value::Float(f) | Value::Pct(f) => {
                                // If exponent is an integer (e.g. 2.0), keep Big. Else fall back to float.
                                if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                                    Value::Big(decimal_powi(base, *f as i64)?)
                                } else {
                                    let bf = base.to_f64().ok_or_else(|| rt("R0204", "big base overflow to float", sp.clone()))?;
                                    Value::Float(bf.powf(*f))
                                }
                            }
                            _ => return Err(need_number("power: exponent", span_of_expr(rhs))),
                        }
                    } else {
                        // pure float
                        let a = to_f64_for_math(&lu, span_of_expr(lhs), "power: base")?;
                        let b = to_f64_for_math(&ru, span_of_expr(rhs), "power: exponent")?;
                        Value::Float(a.powf(b))
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }
                "><" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, _lspec) = take_owned_unformatted(lv);
                    let (ru, _rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        let a = to_big_for_math(&lu, span_of_expr(lhs), "divmod: left")?;
                        let b = to_big_for_math(&ru, span_of_expr(rhs), "divmod: right")?;
                        if b.is_zero() { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        let q = (a / b).floor();
                        let r = a - q * b;
                        Value::Pair(Box::new(Value::Big(q)), Box::new(Value::Big(r)))
                    } else {
                        let a = to_f64_for_math(&lu, span_of_expr(lhs), "divmod: left")?;
                        let b = to_f64_for_math(&ru, span_of_expr(rhs), "divmod: right")?;
                        if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                        let q = (a / b).floor();
                        let r = a - q * b;
                        Value::Pair(Box::new(Value::Float(q)), Box::new(Value::Float(r)))
                    };

                    // divmod returns a Pair; formatting wrappers don't apply, so just return it
                    Ok(out)
                }

                // percent-of family
                "of" => {
                    // Back-compat: if left is a pct, do pct * right; otherwise multiply numbers.
                    let lv  = eval_expr(lhs, sess)?;
                    let rv  = eval_expr(rhs, sess)?;
                    match lv {
                        Value::Pct(p) => {
                            let rnum = as_num(rv, span_of_expr(rhs), "'of' right")?;
                            Ok(Value::Float(p * rnum))
                        }
                        _ => {
                            let lnum = as_num(lv,  span_of_expr(lhs),  "'of' left")?;
                            let rnum = as_num(rv,  span_of_expr(rhs),  "'of' right")?;
                            Ok(Value::Float(lnum * rnum))
                        }
                    }
                }
                "%o" => {
                    // Allow either a pct on the left OR a plain number "N" meaning "N%".
                    let lv  = eval_expr(lhs, sess)?;
                    let rv  = eval_expr(rhs, sess)?;
                    let p = match lv {
                        Value::Pct(p) => p,
                        other         => as_num(other, span_of_expr(lhs), "percent-of-other")? / 100.0,
                    };
                    let b = as_num(rv, span_of_expr(rhs), "percent-of-other")?;
                    Ok(Value::Float(p * b))
                }

                // comparisons (bool)
                "==" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (la, _) = strip_format(&lv);
                    let (rb, _) = strip_format(&rv);

                    // numeric equality if both numeric
                    let is_num = |v: &Value| matches!(v, Value::Float(_) | Value::Pct(_) | Value::Big(_) | Value::Int(_));
                    let eqv = if is_num(&la) && is_num(&rb) {
                        if either_is_big(&la, &rb) {
                            let a = to_big_for_math(&la, span_of_expr(lhs), "== left")?;
                            let b = to_big_for_math(&rb, span_of_expr(rhs), "== right")?;
                            a == b
                        } else {
                            let a = to_f64_for_math(&la, span_of_expr(lhs), "== left")?;
                            let b = to_f64_for_math(&rb, span_of_expr(rhs), "== right")?;
                            a == b
                        }
                    } else {
                        // non-numeric: structural equality on Value (after strip_format)
                        la == rb
                    };
                    Ok(Value::Bool(eqv))
                }

                "!=" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (la, _) = strip_format(&lv);
                    let (rb, _) = strip_format(&rv);

                    let is_num = |v: &Value| matches!(v, Value::Float(_) | Value::Pct(_) | Value::Big(_) | Value::Int(_));
                    let neqv = if is_num(&la) && is_num(&rb) {
                        if either_is_big(&la, &rb) {
                            let a = to_big_for_math(&la, span_of_expr(lhs), "!= left")?;
                            let b = to_big_for_math(&rb, span_of_expr(rhs), "!= right")?;
                            a != b
                        } else {
                            let a = to_f64_for_math(&la, span_of_expr(lhs), "!= left")?;
                            let b = to_f64_for_math(&rb, span_of_expr(rhs), "!= right")?;
                            a != b
                        }
                    } else {
                        la != rb
                    };
                    Ok(Value::Bool(neqv))
                }

                "<" | "<=" | ">" | ">=" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (la, _) = strip_format(&lv);
                    let (rb, _) = strip_format(&rv);

                    // numeric?
                    let is_num = |v: &Value| matches!(v, Value::Float(_) | Value::Pct(_) | Value::Big(_) | Value::Int(_));
                    let b = if is_num(&la) && is_num(&rb) {
                        if either_is_big(&la, &rb) {
                            let a = to_big_for_math(&la, span_of_expr(lhs), "compare left")?;
                            let c = to_big_for_math(&rb, span_of_expr(rhs), "compare right")?;
                            match op.as_str() {
                                "<"  => a <  c,
                                "<=" => a <= c,
                                ">"  => a >  c,
                                ">=" => a >= c,
                                _    => unreachable!(),
                            }
                        } else {
                            let a = to_f64_for_math(&la, span_of_expr(lhs), "compare left")?;
                            let c = to_f64_for_math(&rb, span_of_expr(rhs), "compare right")?;
                            match op.as_str() {
                                "<"  => a <  c,
                                "<=" => a <= c,
                                ">"  => a >  c,
                                ">=" => a >= c,
                                _    => unreachable!(),
                            }
                        }
                    } else if matches!((&la, &rb), (Value::Str(_), Value::Str(_))) {
                        // string lexicographic comparisons (your existing behavior)
                        let a = if let Value::Str(s) = la.clone() { s } else { unreachable!() };
                        let c = if let Value::Str(s) = rb.clone() { s } else { unreachable!() };
                        match op.as_str() {
                            "<"  => a <  c,
                            "<=" => a <= c,
                            ">"  => a >  c,
                            ">=" => a >= c,
                            _    => unreachable!(),
                        }
                    } else {
                        return Err(rt("T0302", "comparison requires compatible types", sp.clone()));
                    };

                    Ok(Value::Bool(b))
                }

                // logical ops + coalesce
                "and" | "&&" => {
                    let lv = eval_expr(lhs, sess)?;
                    match lv {
                        Value::Bool(false) => return Ok(Value::Bool(false)), // short-circuit
                        Value::Bool(true)  => { /* evaluate rhs */ }
                        _ => return Err(rt("T0303", "logical 'and' requires booleans", sp.clone())),
                    }
                    let rv = eval_expr(rhs, sess)?;
                    match rv {
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        _ => Err(rt("T0303", "logical 'and' requires booleans", sp.clone())),
                    }
                }
                "or" | "<>" => {
                    let lv = eval_expr(lhs, sess)?;
                    match lv {
                        Value::Bool(true)  => return Ok(Value::Bool(true)), // short-circuit
                        Value::Bool(false) => { /* evaluate rhs */ }
                        _ => return Err(rt("T0303", "logical 'or' requires booleans", sp.clone())),
                    }
                    let rv = eval_expr(rhs, sess)?;
                    match rv {
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        _ => Err(rt("T0303", "logical 'or' requires booleans", sp.clone())),
                    }
                }
                "??" => {
                    let lv = eval_expr(lhs, sess)?;
                    if !matches!(lv, Value::Nil) { return Ok(lv); }
                    let rv = eval_expr(rhs, sess)?;
                    Ok(rv)
                }

                // compound assigns on identifiers only
                "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**=" => {
                    let name = if let ast::Expr::Ident(n, _) = &**lhs {
                        n.clone()
                    } else {
                        return Err(rt("P0801", "left-hand side of compound assign must be a name", span_of_expr(lhs)));
                    };
                    let old = match sess.get_var(&name) {
                        Some(v) => v.clone(),
                        None => return Err(rt("R0110", format!("unknown identifier '{}'", name), span_of_expr(lhs))),
                    };
                    let rv = eval_expr(rhs, sess)?;
                    let a = as_num(old, span_of_expr(lhs), "compound assign (left value)")?;
                    let b = as_num(rv,  span_of_expr(rhs), "compound assign (right value)")?;
                    let new = match op.as_str() {
                        "+=" => a + b,
                        "-=" => a - b,
                        "*=" => a * b,
                        "/=" => { if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); } a / b }
                        "//=" => { if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); } (a / b).floor() }
                        "%=" =>  { if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); } let q = (a / b).floor(); a - q * b }
                        "**=" => a.powf(b),
                        _ => unreachable!(),
                    };
                    let out = Value::Float(new);
                    sess.set_var(name, out.clone());
                    Ok(out)
                }

                _ => Err(rt("R0004", format!("binary operator '{}' not implemented", op), sp.clone())),
            }
        }
    }
}

fn call_object_method(
    sess: &mut Session,
    class_name: &str,
    fields: &mut BTreeMap<String, Value>,
    method_name: &str,
    arg_exprs: &[ast::Expr],
    sp: Span,
) -> Result<Value, Diag> {
    let class = sess.classes.get(class_name)
        .ok_or_else(|| rt("R0116", format!("class '{}' not found", class_name), sp.clone()))?
        .clone();
    
    let action = class.actions.iter()
        .find(|a| a.name == method_name)
        .ok_or_else(|| rt("R0404", format!("no action '{}' in class '{}'", method_name, class_name), sp.clone()))?
        .clone();
    
    let mut arg_vals = Vec::with_capacity(arg_exprs.len());
    for a in arg_exprs {
        arg_vals.push(eval_expr(a, sess)?);
    }
    
    sess.push_frame();
    sess.set_var("self".to_string(), Value::Map(fields.clone()));
    
    // Bind each field as a variable
    for (field_name, field_value) in fields.iter() {
        sess.set_var(field_name.clone(), field_value.clone());
    }
    
    // Bind parameters
    for (i, param) in action.params.iter().enumerate() {
        if i < arg_vals.len() {
            sess.set_var(param.name.clone(), arg_vals[i].clone());
        } else if let Some(def_expr) = &param.default {
            let def_val = eval_expr(def_expr, sess)?;
            sess.set_var(param.name.clone(), def_val);
        } else {
            sess.pop_frame();
            return Err(rt("A0402", format!("missing required parameter '{}'", param.name), sp));
        }
    }
    
    let result = {
        let ast::ActionBody::Block(stmts) = &action.body;
        let mut last = Value::Unit;
        for stmt in stmts {
            if let Some(v) = eval_stmt(stmt, sess)? {
                match v {
                    Value::CtrlSkip | Value::CtrlStop => { /* ignore */ }
                    other => last = other,
                }
            }
        }
        last
    };
    
    // CRITICAL: Copy modified field values back BEFORE popping frame
    let current_frame = sess.env.last().expect("has frame");
    let field_names: Vec<String> = fields.keys().cloned().collect();
    for field_name in field_names {
        if let Some(modified_value) = current_frame.get(&field_name) {
            fields.insert(field_name, modified_value.clone());
        }
    }
    
    sess.pop_frame();
    Ok(result)
}

fn instantiate_object(
    sess: &mut Session,
    var_name: &str,
    class_name: &str,
    expr: &ast::Expr,
    span: Span,
    is_const: bool,
) -> Result<Option<Value>, Diag> {
    // Get class definition
    let class = sess.classes.get(class_name)
        .ok_or_else(|| rt("R0115", format!("unknown class '{}'", class_name), span.clone()))?
        .clone();
    
    // Evaluate RHS expression
    let rhs_val = eval_expr(expr, sess)?;
    
    // Convert to array of values
    let values = match rhs_val {
        Value::Array(vals) => vals,
        single => vec![single],
    };
    
    // Match values to fields positionally
    let mut field_map = BTreeMap::new();
    let mut value_idx = 0;
    
    for field in &class.fields {
        let value = if value_idx < values.len() {
            let val = &values[value_idx];
            value_idx += 1;
            
            // Check for skip marker (Unit represents :: or nc)
            if matches!(val, Value::Unit) {
                eval_expr(field.default.as_ref().unwrap(), sess)?
            } else {
                val.clone()
            }
        } else {
            // No more values - use default
            eval_expr(field.default.as_ref().unwrap(), sess)?
        };
        
        field_map.insert(field.name.clone(), value);
    }
    
    let obj = Value::Object {
        class_name: class_name.to_string(),
        fields: field_map,
    };
    
    sess.define_local(var_name.to_string(), obj, is_const);
    Ok(None)
}