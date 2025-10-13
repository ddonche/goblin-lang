use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use goblin_ast as ast;
use serde_json as sj;
use goblin_ast::BindMode;
use rust_decimal::Decimal;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use std::str::FromStr;
use std::collections::HashMap;
use crate::diagnostics::rtcode;
use goblin_diagnostics::Span;
pub type Diag = goblin_diagnostics::Diagnostic;

pub mod modules;
pub mod diagnostics;

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

#[derive(Debug, Clone)]
pub struct ClassRelations {
    pub of_relations: BTreeMap<String, (String, String)>, // field_name -> (class_name, as_name)
    pub with_relations: Vec<String>,  // class names
    pub re_relations: Vec<String>,    // class names
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
        readonly_fields: BTreeSet<String>,
    },
    Enum {                              
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
            Value::Object { class_name, fields, .. } => {
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

pub struct Session {
    history: Vec<Value>,                               // v(n)
    pub env: Vec<BTreeMap<String, Value>>,             // scope stack (globals at [0])
    pub actions: BTreeMap<String, ast::ActionDecl>,    // free actions by name
    pub classes: BTreeMap<String, ast::ClassDecl>,
    pub enums: BTreeMap<String, ast::EnumDecl>,
    pub loop_depth: i32,
    rng_state: u128,
    pub consts: Vec<BTreeMap<String, bool>>, // true = immutable binding 
    pub relationship_graph: BTreeMap<String, ClassRelations>,
    pub modules: crate::modules::ModuleCache,
    pub current_module: Option<String>, 
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
            relationship_graph: BTreeMap::new(),
            modules: crate::modules::ModuleCache::new(),
            current_module: None,
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
        // First check module environment if we're in a module
        if let Some(ref module_name) = self.current_module {
            if let Some(module_env) = self.modules.get_module_env(module_name) {
                if let Some(v) = module_env.get(name) {
                    return Some(v);
                }
            }
        }
        
        // Then check local frames
        for frame in self.env.iter().rev() {
            if let Some(v) = frame.get(name) {
                return Some(v);
            }
        }
        None
    }

    pub fn set_var(&mut self, name: String, val: Value) {
        // If we're at the top level of a module, store in module env
        if let Some(ref module_name) = self.current_module {
            if self.env.len() == 1 {  // Top level
                self.modules.set_module_var(module_name, name, val);
                return;
            }
        }
        
        // Otherwise store in current frame
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

    pub fn eval_line(&mut self, src: &str) -> Result<Value, Diag> {
        // 1) Lex
        let toks = match goblin_lexer::lex(src, "<repl>") {
            Ok(t) => t,
            Err(diags) => {
                let mut d = diags.into_iter().next().expect("nonempty diags");
                if d.code == "UNKNOWN" {
                    d = d.with_code("L9900");
                }
                return Err(
                    d.with_help("Check for stray characters, unterminated strings, or bad escapes like \\q.")
                     .with_help("If you used quotes, ensure they’re balanced and escapes are valid (\\n, \\t, \\\\, \\\" ).")
                );
            }
        };

        // 2) Parse
        let parser = goblin_parser::Parser::new(&toks);
        let module = match parser.parse_module() {
            Ok(m) => m,
            Err(diags) => {
                let mut d = diags.into_iter().next().expect("nonempty diags");
                if d.code == "UNKNOWN" {
                    d = d.with_code("P9900");
                }
                return Err(
                    d.with_help("Look for missing expressions, unmatched delimiters, or indentation issues.")
                     .with_help("If stuck, inspect tokens (e.g., a REPL :tokens command) to see how the source was lexed.")
                );
            }
        };

        // 3) Eval (return the last expression value)
        match self.eval_module(&module)? {
            Some(v) => {
                self.history.push(v.clone());
                Ok(v)
            }
            None => Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    "R0902",
                    "no-result",
                    "No expression to evaluate",
                    Span::new("<repl>", 0, 0, 1, 1, 1, 1),
                )
                .with_help("Type an expression (e.g., 2+2) so the evaluator has a value to return.")
                .with_help("Tip: assign to a name, then enter the name to echo it (e.g., x = 5 ↵ then x ↵).")
                .with_link("https://goblinlang.org/docs/errors#R0902")
            ),
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
        Value::Char(c) => Ok(c.to_string()),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "T0205",
                "type-mismatch",
                format!("{label} expects a string value"),
                sp.clone(),
            )
            .with_help("Use quotes to define a string (e.g., \"text\"), not single quotes or numbers.")
            .with_help("Example: name = \"Alice\"")
            .with_link("https://goblinlang.org/docs/errors#T0205")
        ),
    }
}


#[allow(dead_code)]
fn want_char(v: &Value, label: &str, sp: Span) -> Result<char, Diag> {
    match v {
        Value::Char(c) => Ok(*c),
        Value::Str(s) if s.chars().count() == 1 => Ok(s.chars().next().unwrap()),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "T0205",
                "type-mismatch",
                format!("{label} expects a single character"),
                sp.clone(),
            )
            .with_help("Use single quotes for characters (e.g., 'A'), not double quotes or multi-character strings.")
            .with_help("Example: grade = 'B'")
            .with_link("https://goblinlang.org/docs/errors#T0205")
        ),
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
        Value::Object { class_name, fields, .. } => {
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
use goblin_diagnostics::{Diagnostic, Severity};

// cast_to_big: convert various Values to Decimal Big
fn cast_to_big(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Big(d) => Ok(Value::Big(d)),

        Value::Int(i) => Ok(Value::Big(Decimal::from(i))),

        Value::Float(f) | Value::Pct(f) => {
            let d = Decimal::from_f64(f).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    "R0212",
                    "invalid-cast",
                    "big(): cannot represent float as decimal",
                    synth_span(),
                )
                .with_help("Use a precise decimal literal like 1.23 (no repeating binary fraction).")
                .with_help("If you need exact math, prefer strings: big(\"1.23\").")
                .with_link("https://goblinlang.org/docs/errors#R0212")
            })?;
            Ok(Value::Big(d))
        }

        Value::Str(s) => {
            let trimmed = s.trim();
            if trimmed.ends_with('%') {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0223",
                        "percent-string",
                        "big(): use pct() for percent strings",
                        synth_span(),
                    )
                    .with_help("Example: pct(\"12.5%\") instead of big(\"12.5%\").")
                    .with_help("Or remove % and scale manually if you really want a decimal.")
                    .with_link("https://goblinlang.org/docs/errors#R0223")
                );
            }
            let cleaned: String = trimmed.chars().filter(|&c| c != '_').collect();
            let d = Decimal::from_str(&cleaned).map_err(|_| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    "R0224",
                    "invalid-numeric-string",
                    "big(): invalid numeric string",
                    synth_span(),
                )
                .with_help("Only digits, optional sign, and one decimal point are allowed (underscores are ignored).")
                .with_help("Example: big(\"1234.50\")")
                .with_link("https://goblinlang.org/docs/errors#R0224")
            })?;
            Ok(Value::Big(d))
        }

        Value::Formatted(inner, _) => cast_to_big(*inner),

        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0212",
                "invalid-cast",
                "big(): cannot cast value to big",
                synth_span(),
            )
            .with_help("Acceptable inputs: Int, Float, Pct, Big, or numeric String.")
            .with_link("https://goblinlang.org/docs/errors#R0212")
        ),
    }
}

fn cast_to_float(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Float(f) => Ok(Value::Float(f)),

        Value::Pct(p) => Ok(Value::Float(p)),

        Value::Int(i) => {
            if (i64::MIN..i64::MAX).contains(&i) && (i.abs() as i128) < (F64_SAFE_INT_MAX as i128) {
                Ok(Value::Float(i as f64))
            } else {
                Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0313",
                        "precision-loss",
                        "float(): cannot cast int ≥ 2^53 without precision loss; keep it as big",
                        synth_span(),
                    )
                    .with_help("Use big() if exact precision is required.")
                    .with_help("Floats cannot represent all 64-bit integers exactly.")
                    .with_link("https://goblinlang.org/docs/errors#R0313")
                )
            }
        }

        Value::Big(d) => {
            let t = d.trunc();
            let is_integer = d == t;
            if is_integer {
                let mag_ok = d.abs().to_f64().map(|x| x < (F64_SAFE_INT_MAX as f64)).unwrap_or(false);
                if !mag_ok {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            "R0313",
                            "precision-loss",
                            "float(): cannot cast big integer ≥ 2^53 without precision loss; keep it as big",
                            synth_span(),
                        )
                        .with_help("Large integers lose precision beyond 2^53 in floating-point form.")
                        .with_help("Keep it as Big or convert to string explicitly.")
                        .with_link("https://goblinlang.org/docs/errors#R0313")
                    );
                }
                let f = t.to_f64().ok_or_else(|| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0313",
                        "overflow",
                        "float(): overflow casting big->float",
                        synth_span(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0313")
                })?;
                Ok(Value::Float(f))
            } else if let Some(f) = d.to_f64() {
                Ok(Value::Float(f))
            } else {
                Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0313",
                        "overflow",
                        "float(): overflow casting big->float",
                        synth_span(),
                    )
                    .with_help("Try rounding or truncating before conversion.")
                    .with_link("https://goblinlang.org/docs/errors#R0313")
                )
            }
        }

        Value::Str(s) => {
            let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
            if cleaned.ends_with('%') {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0314",
                        "percent-string",
                        "float(): use pct() for percent strings",
                        synth_span(),
                    )
                    .with_help("Example: pct(\"50%\") instead of float(\"50%\")")
                    .with_link("https://goblinlang.org/docs/errors#R0314")
                );
            }

            if let Ok(d) = Decimal::from_str(&cleaned) {
                let t = d.trunc();
                let is_integer = d == t;
                if is_integer {
                    let mag_ok = d.abs().to_f64().map(|x| x < (F64_SAFE_INT_MAX as f64)).unwrap_or(false);
                    if !mag_ok {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                "R0313",
                                "precision-loss",
                                "float(): cannot cast big integer ≥ 2^53 without precision loss; keep it as big",
                                synth_span(),
                            )
                            .with_link("https://goblinlang.org/docs/errors#R0313")
                        );
                    }
                    let f = t.to_f64().ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            "R0313",
                            "overflow",
                            "float(): overflow casting string->float",
                            synth_span(),
                        )
                        .with_link("https://goblinlang.org/docs/errors#R0313")
                    })?;
                    Ok(Value::Float(f))
                } else if let Some(f) = d.to_f64() {
                    Ok(Value::Float(f))
                } else {
                    Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            "R0313",
                            "overflow",
                            "float(): overflow casting string->float",
                            synth_span(),
                        )
                        .with_link("https://goblinlang.org/docs/errors#R0313")
                    )
                }
            } else if let Ok(f) = cleaned.parse::<f64>() {
                if f.is_finite() {
                    Ok(Value::Float(f))
                } else {
                    Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            "R0313",
                            "overflow",
                            "float(): overflow casting string->float",
                            synth_span(),
                        )
                        .with_link("https://goblinlang.org/docs/errors#R0313")
                    )
                }
            } else {
                Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0314",
                        "invalid-string",
                        "float(): cannot cast string to float",
                        synth_span(),
                    )
                    .with_help("Provide a numeric string (e.g., \"123.45\").")
                    .with_link("https://goblinlang.org/docs/errors#R0314")
                )
            }
        }

        other => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0314",
                "invalid-cast",
                &format!("float(): cannot cast {} to float", value_kind_str(&other)),
                synth_span(),
            )
            .with_help("Valid conversions: Int, Big, Float, Pct, or numeric String.")
            .with_link("https://goblinlang.org/docs/errors#R0314"),
        ),
    }
}

fn cast_to_int_like(v: Value) -> Result<Value, Diag> {
    match v {
        Value::Int(i) => Ok(Value::Int(i)),

        Value::Float(f) | Value::Pct(f) => {
            let t = f.trunc();
            if t.is_finite() && t.abs() < (i64::MAX as f64) + 1.0 {
                Ok(Value::Int(t as i64))
            } else {
                let d = Decimal::from_f64(f).ok_or_else(|| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0315",
                        "float-to-decimal",
                        "int(): cannot represent float as decimal",
                        synth_span(),
                    )
                    .with_help("Use big() when you need exact precision for large or non-finite floats.")
                    .with_link("https://goblinlang.org/docs/errors#R0315")
                })?;
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
            let d = Decimal::from_str(&cleaned).map_err(|_| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    "R0316",
                    "invalid-int-cast",
                    "int(): cannot cast string to int",
                    synth_span(),
                )
                .with_help("Provide a numeric string like \"123\" (underscores are allowed).")
                .with_link("https://goblinlang.org/docs/errors#R0316")
            })?;
            let t = d.trunc();
            if let Some(i) = t.to_i64() { Ok(Value::Int(i)) } else { Ok(Value::Big(t)) }
        }

        Value::Formatted(inner, _) => cast_to_int_like(*inner),

        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0316",
                "invalid-int-cast",
                "int(): cannot cast value to int",
                synth_span(),
            )
            .with_help("Valid inputs: Int, Float, Pct, Big, or numeric String.")
            .with_link("https://goblinlang.org/docs/errors#R0316")
        ),
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
        Value::Pct(p)   => Ok(Value::Pct(p)),
        Value::Float(f) => Ok(to_pct(f)),
        Value::Big(d)   => {
            if let Some(f) = d.to_f64() {
                Ok(to_pct(f))
            } else {
                Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0320",
                        "invalid-big-to-pct",
                        "pct(): cannot cast big to pct (out of range)",
                        synth_span(),
                    )
                    .with_help("Use a representable numeric value or scale it before converting to pct().")
                    .with_link("https://goblinlang.org/docs/errors#R0320")
                )
            }
        }
        Value::Str(s)   => {
            let trimmed = s.trim();
            let cleaned: String = trimmed.chars().filter(|&c| c != '_').collect();
            if cleaned.ends_with('%') {
                let num = cleaned[..cleaned.len()-1].trim();
                if let Ok(d) = Decimal::from_str(num) {
                    if let Some(f) = d.to_f64() {
                        return Ok(to_pct(f / 100.0));
                    }
                }
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0321",
                        "invalid-pct-string",
                        &format!("pct(): invalid percent string '{}'", s),
                        synth_span(),
                    )
                    .with_help("Use a well-formed percent like \"12.5%\".")
                    .with_link("https://goblinlang.org/docs/errors#R0321")
                );
            } else {
                if let Ok(d) = Decimal::from_str(&cleaned) {
                    if let Some(f) = d.to_f64() {
                        return Ok(to_pct(f));
                    }
                }
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "R0321",
                        "invalid-pct-string",
                        &format!("pct(): invalid numeric string '{}'", s),
                        synth_span(),
                    )
                    .with_help("Provide a numeric string (e.g., \"0.125\" or \"12.5%\" with a percent sign).")
                    .with_link("https://goblinlang.org/docs/errors#R0321")
                );
            }
        }
        Value::Formatted(inner, _) => cast_to_pct(*inner),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0322",
                "invalid-pct-cast",
                "pct(): cannot cast value to pct",
                synth_span(),
            )
            .with_help("Valid inputs: Float, Big, Pct, or numeric/percent String (e.g., \"12.5%\" or \"0.125\").")
            .with_link("https://goblinlang.org/docs/errors#R0322")
        ),
    }
}

#[inline] fn int_checked_add(a: i64, b: i64) -> Option<i64> { a.checked_add(b) }
#[inline] fn int_checked_sub(a: i64, b: i64) -> Option<i64> { a.checked_sub(b) }
#[inline] fn int_checked_mul(a: i64, b: i64) -> Option<i64> { a.checked_mul(b) }

#[inline]
fn to_decimal(v: &Value) -> Result<rust_decimal::Decimal, Diagnostic> {
    use rust_decimal::Decimal;
    match v {
        Value::Int(i) => Ok(Decimal::from(*i)),
        Value::Float(f) => Decimal::from_f64(*f).ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                "R0298",
                "float-to-decimal-failed",
                "float->decimal conversion failed",
                synth_span(),
            )
            .with_help("The float value is too large or invalid for Decimal representation.")
            .with_link("https://goblinlang.org/docs/errors#R0298")
        }),
        Value::Pct(f) => Decimal::from_f64(*f).ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                "R0298",
                "pct-to-decimal-failed",
                "pct->decimal conversion failed",
                synth_span(),
            )
            .with_help("The percent value is too large or invalid for Decimal representation.")
            .with_link("https://goblinlang.org/docs/errors#R0298")
        }),
        Value::Big(d) => Ok(d.clone()),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0200",
                "numeric-expected",
                "numeric value expected",
                synth_span(),
            )
            .with_help("Only Int, Float, Big, or Pct values are valid for this conversion.")
            .with_link("https://goblinlang.org/docs/errors#R0200"),
        ),
    }
}

fn to_big_for_math(v: &Value, at: Span, label: &str) -> Result<Decimal, Diagnostic> {
    match v {
        Value::Big(d) => Ok(*d),
        Value::Float(f) | Value::Pct(f) => Decimal::from_f64(*f).ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                "T0320",
                "non-finite-float",
                &format!("{label}: NaN or Infinity cannot be represented as Big"),
                at.clone(),
            )
            .with_help("Ensure the value is a finite number before performing Big math operations.")
            .with_help("Use is_finite() to check if a value is NaN or Infinity.")
            .with_link("https://goblinlang.org/docs/errors#T0320")
        }),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0200",
                "numeric-expected",
                &format!("{label}: numeric value expected"),
                at.clone(),
            )
            .with_help("Valid numeric types: Int, Float, Pct, or Big.")
            .with_link("https://goblinlang.org/docs/errors#R0200"),
        ),
    }
}

fn to_f64_for_math(v: &Value, at: Span, label: &str) -> Result<f64, Diagnostic> {
    match v {
        Value::Int(i) => Ok(*i as f64),
        Value::Float(f) => Ok(*f),
        Value::Pct(f) => Ok(*f),
        Value::Big(d) => d.to_f64().ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                "T0321",
                "big-overflow",
                &format!("{label}: cannot convert Big to float (overflow)"),
                at.clone(),
            )
            .with_help("The Big number is too large or precise to fit into a 64-bit float.")
            .with_help("Use Big math or reduce precision before converting to float.")
            .with_link("https://goblinlang.org/docs/errors#T0321")
        }),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0200",
                "numeric-expected",
                &format!("{label}: numeric value expected"),
                at.clone(),
            )
            .with_help("Valid numeric types: Int, Float, Pct, or Big.")
            .with_link("https://goblinlang.org/docs/errors#R0200"),
        ),
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
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    "R0206",
                    "division-by-zero",
                    "power(): division by zero — cannot raise zero to a negative exponent",
                    synth_span(),
                )
                .with_help("Zero cannot be used as the base when the exponent is negative.")
                .with_help("Example: 0 ** -1 is invalid because it requires dividing by zero.")
                .with_link("https://goblinlang.org/docs/errors#R0206")
            );
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

fn rt(code: &'static str, message: impl Into<String>, sp: Span) -> Diag {
    Diagnostic::new_with_code(
        Severity::Error,
        code,
        "runtime-error", // generic slug for legacy sites; we’ve been supplying specific slugs elsewhere
        message,
        sp,
    )
}
#[allow(dead_code)]
fn not_impl(stage: &str, what: &str, sp: Span) -> Diagnostic {
    Diagnostic::new_with_code(
        Severity::Error,
        crate::diagnostics::rtcode::OP_NOT_IMPLEMENTED, // R0504
        "op-not-implemented",
        format!("{what} is not implemented in {stage}"),
        sp,
    )
    .with_help("This operation or feature isn’t available yet.")
    .with_help("Check the Goblin release notes or docs for planned support.")
    .with_link("https://goblinlang.org/docs/errors#R0504")
}

fn need_number(what: &str, span: Span) -> Diagnostic {
    Diagnostic::new_with_code(
        Severity::Error,
        "R0201",
        "type-mismatch",
        &format!("{what} expects a numeric value"),
        span.clone(),
    )
    .with_help("Provide an Int, Float, Big, or Pct value.")
    .with_help("Example: x = 42 or x = 3.14")
    .with_link("https://goblinlang.org/docs/errors#R0201")
}


fn parse_number_value(text: &str, sp: Span) -> Result<Value, Diag> {
    use rust_decimal::Decimal;
    use std::str::FromStr;

    // allow underscores in literals
    let cleaned: String = text.chars().filter(|&c| c != '_').collect();
    let has_float_syntax = cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E');

    if has_float_syntax {
        let f = cleaned.parse::<f64>().map_err(|_| {
            Diagnostic::new_with_code(
                Severity::Error,
                "P0330",
                "invalid-number-literal",
                format!("invalid number literal '{text}'"),
                sp.clone(),
            )
            .with_help("Ensure numbers have only one decimal point and valid digits.")
            .with_help("Valid forms: 42, 3.1415, 1_000_000, 6.02e23.")
            .with_link("https://goblinlang.org/docs/errors#P0330")
        })?;
        Ok(Value::Float(f))
    } else {
        if let Ok(i) = cleaned.parse::<i64>() {
            Ok(Value::Int(i))
        } else {
            let d = Decimal::from_str(&cleaned).map_err(|_| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    "P0330",
                    "invalid-number-literal",
                    format!("invalid number literal '{text}'"),
                    sp.clone(),
                )
                .with_help("Ensure numbers contain only digits and optional underscores.")
                .with_help("For very large integers, ensure no invalid symbols or spaces are present.")
                .with_link("https://goblinlang.org/docs/errors#P0330")
            })?;
            Ok(Value::Big(d))
        }
    }
}

#[allow(dead_code)]
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
        | ast::Expr::TupleAssign(_,_, sp)
        | ast::Expr::Call(_, _, _, sp)
        | ast::Expr::OptCall(_, _, _, sp)
        | ast::Expr::FreeCall(_, _, sp)
        | ast::Expr::NsCall(_, _, _, sp)
        | ast::Expr::Prefix(_, _, sp)
        | ast::Expr::Postfix(_, _, sp)
        | ast::Expr::Binary(_, _, _, sp)
        | ast::Expr::Assign(_, _, sp)
        | ast::Expr::EnumVariant { span: sp, .. }
        | ast::Expr::Judge { span: sp, .. } 
        | ast::Expr::Block { span: sp, .. } => sp.clone(),
    }
}

fn as_bool(v: Value, at: Span, label: &str) -> Result<bool, Diag> {
    match v {
        Value::Bool(b) => Ok(b),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0201",
                "type-mismatch",
                &format!("{label} requires a boolean value"),
                at.clone(),
            )
            .with_help("Use true or false here.")
            .with_link("https://goblinlang.org/docs/errors#R0201"),
        ),
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
        Value::Big(d)   => d.to_f64().ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                "R0326",
                "big-overflow",
                &format!("{label}: Big value cannot be represented as a float"),
                at.clone(),
            )
            .with_help("Use big() math or reduce precision before converting to float.")
            .with_link("https://goblinlang.org/docs/errors#R0326")
        }),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0200",
                "numeric-expected",
                &format!("{label} expects a numeric value"),
                at.clone(),
            )
            .with_help("Provide Int, Float, Big, or Pct.")
            .with_link("https://goblinlang.org/docs/errors#R0200")
        ),
    }
}

#[allow(dead_code)]
fn bin_nums(lhs: &ast::Expr, rhs: &ast::Expr, sess: &mut Session, label: &str, sp: Span) -> Result<(f64, f64), Diag> {
    let lv = eval_expr(lhs, sess)?;
    let rv = eval_expr(rhs, sess)?;
    let ln = as_num(lv, sp.clone(), &format!("{label}: left operand"))?;
    let rn = as_num(rv, sp.clone(), &format!("{label}: right operand"))?;
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
        Value::Int(n) if n >= 0 => Ok(n as usize),
        Value::Float(n) if n.is_finite() && n.fract() == 0.0 && n >= 0.0 => Ok(n as usize),
        _ => Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "R0401",
                "invalid-index",
                &format!("{label} must be a non-negative integer index"),
                sp.clone(),
            )
            .with_help("Use 0, 1, 2, … (no negatives, no fractions).")
            .with_link("https://goblinlang.org/docs/errors#R0401")
        ),
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

fn parse_dice_string(s: &str, sp: Span) -> Result<BTreeMap<String, Value>, Diag> {
    use std::collections::BTreeMap;

    let s = s.trim();
    let mut cfg = BTreeMap::new();

    let d_pos = s.find('d').ok_or_else(|| {
        Diagnostic::new_with_code(
            Severity::Error,
            "P0340",
            "invalid-dice-notation",
            "Missing 'd' in dice notation",
            sp.clone(),
        )
        .with_help("Dice notation must include 'd', e.g., '2d6' or '4d10+2'.")
        .with_link("https://goblinlang.org/docs/errors#P0340")
    })?;

    let count: i64 = s[..d_pos].parse().map_err(|_| {
        Diagnostic::new_with_code(
            Severity::Error,
            "P0340",
            "invalid-dice-notation",
            &format!("Invalid dice count in '{}'", s),
            sp.clone(),
        )
        .with_help("Dice notation must start with a valid number, e.g., '2d6'.")
        .with_link("https://goblinlang.org/docs/errors#P0340")
    })?;

    let mut rest = &s[d_pos + 1..];
    let mut sides_end = 0;
    for (i, ch) in rest.char_indices() {
        if ch.is_ascii_digit() {
            sides_end = i + 1;
        } else {
            break;
        }
    }

    if sides_end == 0 {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                "P0340",
                "invalid-dice-notation",
                "Missing number of sides in dice expression",
                sp.clone(),
            )
            .with_help("Use a format like '2d6' or '4d10+1'.")
            .with_link("https://goblinlang.org/docs/errors#P0340"),
        );
    }

    let sides: i64 = rest[..sides_end].parse().map_err(|_| {
        Diagnostic::new_with_code(
            Severity::Error,
            "P0340",
            "invalid-dice-notation",
            &format!("Invalid number of sides in '{}'", s),
            sp.clone(),
        )
        .with_help("Ensure the sides are numeric, e.g., 'd6', 'd20'.")
        .with_link("https://goblinlang.org/docs/errors#P0340")
    })?;

    rest = &rest[sides_end..];
    cfg.insert("count".into(), Value::Int(count));
    cfg.insert("sides".into(), Value::Int(sides));
    cfg.insert("modifier".into(), Value::Int(0));

    let chars: Vec<char> = rest.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            '+' | '-' => {
                let sign = if chars[i] == '-' { -1 } else { 1 };
                i += 1;

                if i < chars.len() && chars[i].is_alphabetic() {
                    let word_start = i;
                    while i < chars.len() && chars[i].is_alphabetic() {
                        i += 1;
                    }
                    let word: String = chars[word_start..i].iter().collect();
                    match word.as_str() {
                        "adv" => cfg.insert("adv".into(), Value::Bool(true)),
                        "dis" => cfg.insert("dis".into(), Value::Bool(true)),
                        _ => {
                            return Err(Diagnostic::new_with_code(
                                Severity::Error,
                                "P0340",
                                "invalid-dice-notation",
                                &format!("Unknown modifier '{}'", word),
                                sp.clone(),
                            )
                            .with_help("Valid textual modifiers: adv, dis")
                            .with_link("https://goblinlang.org/docs/errors#P0340"));
                        }
                    };
                } else {
                    let num_start = i;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        i += 1;
                    }
                    if i == num_start {
                        return Err(Diagnostic::new_with_code(
                            Severity::Error,
                            "P0340",
                            "invalid-dice-notation",
                            "Expected number after '+' or '-'",
                            sp.clone(),
                        )
                        .with_help("Example: 1d20+3 or 2d6-1")
                        .with_link("https://goblinlang.org/docs/errors#P0340"));
                    }
                    let num: String = chars[num_start..i].iter().collect();
                    let modifier: i64 = num.parse().map_err(|_| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            "P0340",
                            "invalid-dice-notation",
                            "Invalid numeric modifier",
                            sp.clone(),
                        )
                        .with_link("https://goblinlang.org/docs/errors#P0340")
                    })?;
                    cfg.insert("modifier".into(), Value::Int(sign * modifier));
                }
            }

            'k' => {
                i += 1;
                let num_start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                if i == num_start {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        "P0340",
                        "invalid-dice-notation",
                        "Expected number after 'k'",
                        sp.clone(),
                    )
                    .with_help("Example: 4d6k3 keeps the 3 highest rolls")
                    .with_link("https://goblinlang.org/docs/errors#P0340"));
                }
                let num: String = chars[num_start..i].iter().collect();
                let n: i64 = num.parse().map_err(|_| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        "P0340",
                        "invalid-dice-notation",
                        "Invalid keep_high number",
                        sp.clone(),
                    )
                    .with_help("Use digits after ‘k’, e.g., 4d6k3")
                    .with_link("https://goblinlang.org/docs/errors#P0340")
                })?;
                cfg.insert("keep_high".into(), Value::Int(n));
            }

            '!' => {
                cfg.insert("explode".into(), Value::Bool(true));
                i += 1;
            }

            ' ' => i += 1,

            _ => {
                return Err(Diagnostic::new_with_code(
                    Severity::Error,
                    "P0340",
                    "invalid-dice-notation",
                    &format!("Unexpected character '{}'", chars[i]),
                    sp.clone(),
                )
                .with_help("Valid dice syntax: XdY[+/-N][!][kN][adv|dis]")
                .with_link("https://goblinlang.org/docs/errors#P0340"));
            }
        }
    }

    Ok(cfg)
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
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            rtcode::UNCLOSED_INTERP_BRACE, // R0500
                            "interpolation",
                            "unclosed '{' in interpolated string",
                            sp.clone(),
                        )
                        .with_help("Use '{{' to render a literal '{', or close the interpolation with '}'.")
                        .with_help("Example: \"{name}\" or \"{{\" for a literal left brace.")
                        .with_link("https://goblinlang.org/docs/errors#R0500")
                    );
                }

                // raw contents between braces (keep whitespace for literal echo)
                let inner_raw = &s[start..j];
                let inner_trim = inner_raw.trim();

                // Only interpolate {ident}. Anything else is emitted literally.
                if !is_ident(inner_trim) {
                    out.push('{');
                    out.push_str(inner_raw);
                    out.push('}');
                    i = j + 1;
                    continue;
                }

                // Interpolate {ident}
                match sess.get_var(inner_trim) {
                    Some(v) => out.push_str(&fmt_value_raw(v)),
                    None => {
                        // Fallback: if there's a `self` map in scope, allow `{field}` to read `self[field]`
                        if let Some(Value::Map(m)) = sess.get_var("self") {
                            if let Some(v) = m.get(inner_trim) {
                                out.push_str(&fmt_value_raw(v));
                            } else {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::UNKNOWN_IDENT, // R0101
                                        "unknown-ident",
                                        &format!("unknown name '{inner_trim}'"),
                                        sp.clone(),
                                    )
                                    .with_help("Declare it, or qualify it (e.g., module::name).")
                                    .with_help("Inside objects, ensure the field exists on 'self'.")
                                    .with_link("https://goblinlang.org/docs/errors#R0101")
                                );
                            }
                        } else {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::UNKNOWN_IDENT, // R0101
                                    "unknown-ident",
                                    &format!("unknown name '{inner_trim}'"),
                                    sp.clone(),
                                )
                                .with_help("Declare it, or qualify it (e.g., module::name).")
                                .with_link("https://goblinlang.org/docs/errors#R0101")
                            );
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
            // Build relationship metadata
            let mut relations = ClassRelations {
                of_relations: BTreeMap::new(),
                with_relations: Vec::new(),
                re_relations: Vec::new(),
            };
            
            for field in &decl.fields {
                if let Some(ref rel) = field.relation {
                    match rel {
                        ast::RelationDef::Of { class_name, as_name } => {
                            relations.of_relations.insert(
                                field.name.clone(),
                                (class_name.clone(), as_name.clone())
                            );
                        }
                        ast::RelationDef::With { class_name } => {
                            relations.with_relations.push(class_name.clone());
                        }
                        ast::RelationDef::Re { class_name } => {
                            relations.re_relations.push(class_name.clone());
                        }
                    }
                }
            }
            
            sess.relationship_graph.insert(decl.name.clone(), relations);
            sess.classes.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        ast::Stmt::Enum(decl) => {
            // Store the enum definition in the session
            sess.enums.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        ast::Stmt::Import(import_stmt) => {
            let base_dir = std::env::current_dir().map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::IMPORT_IO,              // R0501
                    "import-io",
                    format!("cannot get current directory: {}", e),
                    import_stmt.span.clone(),
                )
                .with_help("Ensure the working directory exists and is accessible (permissions, sandbox constraints).")
                .with_help("If running in a container or sandbox, verify the process has a valid CWD.")
                .with_link("https://goblinlang.org/docs/errors#R0501")
            })?;

            // Helper to execute a loaded module
            fn execute_module(sess: &mut Session, namespace: String, module_ast: ast::Module) -> Result<(), Diag> {
                let old_module = sess.current_module.clone();
                sess.current_module = Some(namespace);

                // First pass: imports
                for stmt in &module_ast.items {
                    if matches!(stmt, ast::Stmt::Import(_)) {
                        eval_stmt(stmt, sess)?;
                    }
                }
                // Second pass: everything else
                for stmt in &module_ast.items {
                    if !matches!(stmt, ast::Stmt::Import(_)) {
                        eval_stmt(stmt, sess)?;
                    }
                }

                sess.current_module = old_module;
                Ok(())
            }

            match &import_stmt.items {
                ast::ImportItems::Path(path) => {
                    let (namespace, maybe_ast) = sess
                        .modules
                        .load_module(path, import_stmt.alias.as_deref(), &base_dir)
                        .map_err(|e| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::IMPORT_FAILED,   // R0502
                                "import-failed",
                                e,
                                import_stmt.span.clone(),
                            )
                            .with_help("Check that the path exists and is readable. Use an absolute path or a correct relative path.")
                            .with_help("If this is a module name, ensure the module can be resolved from the current directory.")
                            .with_link("https://goblinlang.org/docs/errors#R0502")
                        })?;

                    if let Some(module_ast) = maybe_ast {
                        execute_module(sess, namespace, module_ast)?;
                    }
                }
                ast::ImportItems::Named { items, source } => {
                    for item in items {
                        let full_path = format!("{}/{}", source, item.name);
                        let namespace_str = item.alias.as_deref().unwrap_or(&item.name);

                        let (namespace, maybe_ast) = sess
                            .modules
                            .load_module(&full_path, Some(namespace_str), &base_dir)
                            .map_err(|e| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::IMPORT_FAILED,   // R0502
                                    "import-failed",
                                    e,
                                    import_stmt.span.clone(),
                                )
                                .with_help("Verify the named import exists at the resolved path and is readable.")
                                .with_help("If you meant to import a symbol, ensure it is exposed by that module.")
                                .with_link("https://goblinlang.org/docs/errors#R0502")
                            })?;

                        if let Some(module_ast) = maybe_ast {
                            execute_module(sess, namespace, module_ast)?;
                        }
                    }
                }
            }

            Ok(None)
        }

        ast::Stmt::Return(rs) => {
            use std::collections::BTreeMap;

            // Evaluate each returned expression and (when possible) capture an identifier label
            let mut vals: Vec<Value> = Vec::new();
            let mut labels: Vec<Option<String>> = Vec::new();

            for e in &rs.values {
                match e {
                    ast::Expr::Ident(name, _) => {
                        labels.push(Some(name.clone()));
                        vals.push(sess.get_var(name).cloned().unwrap_or(Value::Nil));
                    }
                    _ => {
                        labels.push(None);
                        vals.push(eval_expr(e, sess)?);
                    }
                }
            }

            let ret = match vals.len() {
                0 => Value::Nil,
                1 => vals.into_iter().next().unwrap(),
                _ => {
                    // If ALL are labeled (identifiers), return a Map keyed by names (back-compat)
                    if labels.iter().all(|l| l.is_some()) {
                        let mut map = BTreeMap::new();
                        for (lab, v) in labels.into_iter().zip(vals.into_iter()) {
                            map.insert(lab.unwrap(), v);
                        }
                        Value::Map(map)
                    } else {
                        // Mixed/literal returns: build a Map
                        // - Identifiers keep their real names
                        // - Unlabeled expressions get positional keys: "_1", "_2", ...
                        let mut map = BTreeMap::new();
                        let mut idx = 1usize;
                        for (lab, v) in labels.into_iter().zip(vals.into_iter()) {
                            if let Some(name) = lab {
                                map.insert(name, v);
                            } else {
                                let key = format!("_{}", idx);
                                map.insert(key, v);
                                idx += 1;
                            }
                        }
                        Value::Map(map)
                    }
                }
            };

            sess.set_var("__return__".to_string(), ret);
            return Ok(Some(Value::CtrlStop));
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::DUPLICATE_LOCAL, // R0111
                                "duplicate-local",
                                format!("'{}' is already declared in this scope; use '=' to reassign", name),
                                name_span,
                            )
                            .with_help("Use '=' to modify an existing variable instead of redeclaring it.")
                            .with_help("If you intend to create a new local, choose a unique name or shadow using '[=' syntax.")
                        );
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
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::IMMUTABLE_ASSIGN, // R0113
                                        "immutable-assign",
                                        format!("cannot reassign immutable '{}'", name),
                                        name_span,
                                    )
                                    .with_help("Values declared with 'imm' cannot be reassigned.")
                                    .with_help("Remove 'imm' or create a new variable if reassignment is intended.")
                                );
                            }
                            if let Some(slot) = sess.env[ix].get_mut(name) {
                                *slot = rhs;
                                Ok(None)
                            } else {
                                Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INTERNAL_ASSIGN_SLOT, // R0009
                                        "internal-assign-slot",
                                        "internal: slot missing during assign",
                                        name_span,
                                    )
                                    .with_help("This indicates a bug in Goblin’s runtime environment or scope tracking.")
                                )
                            }
                        }

                        // Name exists only in an *outer* frame → ERROR (no accidental shadowing)
                        Some(_outer_ix) => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OUTER_SCOPE_SHADOW, // R0114
                                "outer-scope-shadow",
                                format!("'{}' exists in an outer scope; use '[=' to shadow", name),
                                name_span,
                            )
                            .with_help("Use '[=' if you intend to shadow a variable from an outer scope.")
                            .with_help("Otherwise, reassign it directly with '=' instead of redeclaring.")
                        ),

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

fn expect_array<'a>(e: &'a ast::Expr, label: &str, sp: Span) -> Result<&'a [ast::Expr], Diag> {
    if let ast::Expr::Array(items, _) = e {
        Ok(items.as_slice())
    } else {
        Err(
            Diagnostic::new_with_code(
                Severity::Error,
                rtcode::EXPECTED_ARRAY, // P0314
                "expected-array",
                format!("{label} must be an array of expressions"),
                sp.clone(),
            )
            .with_help("Use square brackets [] to define arrays, e.g., [1, 2, 3].")
            .with_help("If you meant to pass multiple arguments, use commas within an array expression.")
        )
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
            Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::WRONG_ARITY, // R0301
                    "wrong-arity",
                    format!("wrong number of arguments (expected {}, got {})", wanted, args.len()),
                    sp.clone(),
                )
                .with_help("Check the function’s required parameters and provide the correct number of arguments.")
                .with_link("https://goblinlang.org/docs/errors#R0301")
            )
        } else { Ok(()) }
    };

    let want_str = |v: &Value, label: &str| -> Result<String, Diag> {
        match v {
            Value::Str(s) => Ok(s.clone()),
            other => Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    format!("{label} expects a string, got {}", value_kind_str(other)),
                    sp.clone(),
                )
                .with_help("Use quotes to provide a string value, e.g., \"text\".")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            ),
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
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::NUMERIC_EXPECTED, // R0200
                        "numeric-expected",
                        "round requires a numeric value",
                        sp.clone(),
                    )
                    .with_help("Valid numeric types: Int, Float, Pct, or Big.")
                    .with_link("https://goblinlang.org/docs/errors#R0200")
                ),
            }
        }
        "floor" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),                  // already integral
                Value::Float(f)  => Value::Float(f.floor()),
                Value::Pct(p)    => Value::Float(p.floor()),
                Value::Big(d)    => Value::Big(d.floor()),
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::NUMERIC_EXPECTED, // R0200
                        "numeric-expected",
                        "floor requires a numeric value",
                        sp.clone(),
                    )
                    .with_help("Valid numeric types: Int, Float, Pct, or Big.")
                    .with_link("https://goblinlang.org/docs/errors#R0200")
                ),
            }
        }
        "ceil" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(*i),
                Value::Float(f)  => Value::Float(f.ceil()),
                Value::Pct(p)    => Value::Float(p.ceil()),
                Value::Big(d)    => Value::Big(d.ceil()),
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::NUMERIC_EXPECTED, // R0200
                        "numeric-expected",
                        "ceil requires a numeric value",
                        sp.clone(),
                    )
                    .with_help("Valid numeric types: Int, Float, Pct, or Big.")
                    .with_link("https://goblinlang.org/docs/errors#R0200")
                ),
            }
        }
        "abs" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)    => Value::Int(i.abs()),
                Value::Float(f)  => Value::Float(f.abs()),
                Value::Pct(p)    => Value::Float(p.abs()),
                Value::Big(d)    => Value::Big(d.abs()),
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::NUMERIC_EXPECTED, // R0200
                        "numeric-expected",
                        "abs requires a numeric value",
                        sp.clone(),
                    )
                    .with_help("Valid numeric types: Int, Float, Pct, or Big.")
                    .with_link("https://goblinlang.org/docs/errors#R0200")
                ),
            }
        }
        "pow" => {
            arity(2)?;
            // If any arg is Big, use Decimal math for integer exponents; else fall back to float powf
            let any_big = matches!(args[0], Value::Big(_)) || matches!(args[1], Value::Big(_));
            if any_big {
                let base = to_big_for_math(&args[0], sp.clone(), "pow (base)")?;
                match &args[1] {
                    Value::Int(ei) => Value::Big(decimal_powi(base, *ei)?),
                    Value::Big(e) => {
                        let et = e.trunc();
                        if *e == et {
                            let n = et.to_i64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::BIG_EXPONENT_RANGE, // R0203
                                    "big-exponent-range",
                                    "pow(): big exponent out of i64 range",
                                    sp.clone(),
                                )
                                .with_help("Use a smaller integer exponent or switch to float math (non-integer exponent).")
                                .with_link("https://goblinlang.org/docs/errors#R0203")
                            })?;
                            Value::Big(decimal_powi(base, n)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "pow(): cannot convert big base to float (overflow)",
                                    sp.clone(),
                                )
                                .with_help("Reduce magnitude or use Big math with an integer exponent.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            let ef = e.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "pow(): cannot convert big exponent to float (overflow)",
                                    sp.clone(),
                                )
                                .with_help("Reduce magnitude or use an integer exponent to stay in Big math.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            Value::Float(bf.powf(ef))
                        }
                    }
                    Value::Float(f) | Value::Pct(f) => {
                        if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                            Value::Big(decimal_powi(base, *f as i64)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "pow(): cannot convert big base to float (overflow)",
                                    sp.clone(),
                                )
                                .with_help("Reduce magnitude or use Big math with an integer exponent.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            Value::Float(bf.powf(*f))
                        }
                    }
                    _ => return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            rtcode::NUMERIC_EXPECTED, // R0200
                            "numeric-expected",
                            "pow requires a numeric exponent",
                            sp.clone(),
                        )
                        .with_help("Use Int, Float, Pct, or Big as the exponent.")
                        .with_link("https://goblinlang.org/docs/errors#R0200")
                    ),
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::MATH_DOMAIN, // R0207
                                "math-domain",
                                "sqrt(): cannot take square root of a negative value",
                                sp.clone(),
                            )
                            .with_help("Ensure the argument is non-negative, or use abs() first if appropriate.")
                            .with_link("https://goblinlang.org/docs/errors#R0207")
                        );
                    }
                    let f = d.to_f64().ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            rtcode::BIG_OVERFLOW, // R0326
                            "big-overflow",
                            "sqrt(): cannot convert big to float (overflow)",
                            sp.clone(),
                        )
                        .with_help("Reduce magnitude or perform Big math that avoids float conversion.")
                        .with_link("https://goblinlang.org/docs/errors#R0326")
                    })?;
                    Value::Float(f.sqrt())
                }
                _ => {
                    let n = to_f64_for_math(&args[0], sp.clone(), "sqrt")?;
                    if n < 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::MATH_DOMAIN, // R0207
                                "math-domain",
                                "sqrt(): cannot take square root of a negative value",
                                sp.clone(),
                            )
                            .with_help("Ensure the argument is non-negative.")
                            .with_link("https://goblinlang.org/docs/errors#R0207")
                        );
                    }
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
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::ARRAY_EXPECTED, // R0402
                        "array-expected",
                        "sum expects an array of numbers",
                        sp.clone(),
                    )
                    .with_help("Pass a single array argument, e.g., sum([1, 2, 3]).")
                    .with_link("https://goblinlang.org/docs/errors#R0402")
                ),
            }
        }

        "avg" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    if xs.is_empty() { Value::Float(0.0) }
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
                _ => return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::ARRAY_EXPECTED, // R0402
                        "array-expected",
                        "avg expects an array of numbers",
                        sp.clone(),
                    )
                    .with_help("Pass a single array argument, e.g., avg([1, 2, 3]).")
                    .with_link("https://goblinlang.org/docs/errors#R0402")
                ),
            }
        }

        "min" => {
            if args.is_empty() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        "min requires at least 1 argument",
                        sp.clone(),
                    )
                    .with_help("Call as min(x, y, ...) or min([x, y, ...]).")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let result = if args.len() == 1 {
                match &args[0] {
                    Value::Array(xs) => {
                        let mut it = xs.iter();
                        let first = it.next().ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::EMPTY_ARRAY, // R0404
                                "empty-array",
                                "min of empty array",
                                sp.clone(),
                            )
                            .with_help("Provide at least one element.")
                            .with_link("https://goblinlang.org/docs/errors#R0404")
                        })?;
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
                    _ => return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            rtcode::ARRAY_EXPECTED, // R0402
                            "array-expected",
                            "min with 1 argument expects an array",
                            sp.clone(),
                        )
                        .with_help("Call as min([x, y, ...]) for the single-argument form.")
                        .with_link("https://goblinlang.org/docs/errors#R0402")
                    ),
                }
            } else {
                // Multiple args: find min of the args themselves
                let any_big = args.iter().any(|v| matches!(v, Value::Big(_)));
                if any_big {
                    let mut m = to_big_for_math(&args[0], sp.clone(), "min")?;
                    for v in &args[1..] {
                        let dv = to_big_for_math(v, sp.clone(), "min")?;
                        if dv < m { m = dv; }
                    }
                    Value::Big(m)
                } else {
                    let mut m = to_f64_for_math(&args[0], sp.clone(), "min")?;
                    for v in &args[1..] {
                        let fv = to_f64_for_math(v, sp.clone(), "min")?;
                        if fv < m { m = fv; }
                    }
                    Value::Float(m)
                }
            };

            result
        }

        "max" => {
            if args.is_empty() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        "max requires at least 1 argument",
                        sp.clone(),
                    )
                    .with_help("Call as max(x, y, ...) or max([x, y, ...]).")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let result = if args.len() == 1 {
                match &args[0] {
                    Value::Array(xs) => {
                        let mut it = xs.iter();
                        let first = it.next().ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::EMPTY_ARRAY, // R0404
                                "empty-array",
                                "max of empty array",
                                sp.clone(),
                            )
                            .with_help("Provide at least one element.")
                            .with_link("https://goblinlang.org/docs/errors#R0404")
                        })?;
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
                    _ => return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            rtcode::ARRAY_EXPECTED, // R0402
                            "array-expected",
                            "max with 1 argument expects an array",
                            sp.clone(),
                        )
                        .with_help("Call as max([x, y, ...]) for the single-argument form.")
                        .with_link("https://goblinlang.org/docs/errors#R0402")
                    ),
                }
            } else {
                // Multiple args: find max of the args themselves
                let any_big = args.iter().any(|v| matches!(v, Value::Big(_)));
                if any_big {
                    let mut m = to_big_for_math(&args[0], sp.clone(), "max")?;
                    for v in &args[1..] {
                        let dv = to_big_for_math(v, sp.clone(), "max")?;
                        if dv > m { m = dv; }
                    }
                    Value::Big(m)
                } else {
                    let mut m = to_f64_for_math(&args[0], sp.clone(), "max")?;
                    for v in &args[1..] {
                        let fv = to_f64_for_math(v, sp.clone(), "max")?;
                        if fv > m { m = fv; }
                    }
                    Value::Float(m)
                }
            };

            result
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

        // Unknown builtin → tell caller to fall back to R0301 etc. (None signals “not a builtin here”)
        _ => return Ok(None),
    };

    Ok(Some(out))
}

// ===================== Collection Operation Refactoring =====================

#[derive(Debug, Clone)]
enum Position {
    First,
    Last,
    At(Value),
    Where(String),
    All,
}

#[derive(Debug, Clone)]
enum Operation {
    Grab,
    Put(Value),
    Update(Value),
    Delete,
}

fn collection_operation(
    coll: &Value,
    pos: Position,
    op: Operation,
    sp: &Span,
    sess: &mut Session,
) -> Result<Value, Diag> {
    match coll {
        // ==================== MAP ====================
        Value::Map(map) => {
            match pos {
                Position::First => {
                    match &op {
                        Operation::Grab => {
                            map.iter()
                                .next()
                                .map(|(_, v)| v.clone())
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty map",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty map before using this operation.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                })
                        }
                        Operation::Put(v) => {
                            // Maps don't have a natural "first" position.
                            Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::OP_NOT_MEANINGFUL, // R0503
                                    "op-not-meaningful",
                                    "put_first is not meaningful for maps",
                                    sp.clone(),
                                )
                                .with_help("Insert into a map by key instead (use Position::At with a string key).")
                                .with_link("https://goblinlang.org/docs/errors#R0503")
                            )
                        }
                        Operation::Update(v) => {
                            let first_key = map
                                .keys()
                                .next()
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty map",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty map before using update-first.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                })?
                                .clone();
                            let mut out = map.clone();
                            out.insert(first_key, v.clone());
                            Ok(Value::Map(out))
                        }
                        Operation::Delete => {
                            let first_key = map
                                .keys()
                                .next()
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty map",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty map before deleting the first item.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                })?
                                .clone();
                            let mut out = map.clone();
                            out.remove(&first_key);
                            Ok(Value::Map(out))
                        }
                    }
                }

                Position::Last => {
                    match &op {
                        Operation::Grab => {
                            map.iter()
                                .last()
                                .map(|(_, v)| v.clone())
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty map",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty map before using this operation.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                })
                        }
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_MEANINGFUL, // R0503
                                "op-not-meaningful",
                                "operation not meaningful for maps in 'last' position",
                                sp.clone(),
                            )
                            .with_help("Use Position::At with a specific key for map updates/inserts.")
                            .with_link("https://goblinlang.org/docs/errors#R0503")
                        ),
                    }
                }

                Position::At(key_val) => {
                    let key = match key_val {
                        Value::Str(s) => s.clone(),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "map key must be a string",
                                    sp.clone(),
                                )
                                .with_help("Use \"key\" (double quotes) for string keys.")
                                .with_link("https://goblinlang.org/docs/errors#T0205")
                            );
                        }
                    };

                    match &op {
                        Operation::Grab => {
                            map.get(&key)
                                .cloned()
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::NO_SUCH_FIELD, // R0403
                                        "no-such-field",
                                        format!("no key '{}'", key),
                                        sp.clone(),
                                    )
                                    .with_help("Check the key spelling or insert the key before reading it.")
                                    .with_link("https://goblinlang.org/docs/errors#R0403")
                                })
                        }
                        Operation::Put(v) => {
                            let mut out = map.clone();
                            out.insert(key, v.clone());
                            Ok(Value::Map(out))
                        }
                        Operation::Update(v) => {
                            if !map.contains_key(&key) {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::NO_SUCH_FIELD, // R0403
                                        "no-such-field",
                                        format!("no key '{}'", key),
                                        sp.clone(),
                                    )
                                    .with_help("Insert the key with a value before updating it.")
                                    .with_link("https://goblinlang.org/docs/errors#R0403")
                                );
                            }
                            let mut out = map.clone();
                            out.insert(key, v.clone());
                            Ok(Value::Map(out))
                        }
                        Operation::Delete => {
                            let mut out = map.clone();
                            out.remove(&key);
                            Ok(Value::Map(out))
                        }
                    }
                }

                Position::Where(pred) => {
                    match &op {
                        Operation::Grab => {
                            let mut out_map = BTreeMap::new();
                            for (k, v) in map {
                                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                                if matches!(ok_v, Value::Bool(true)) {
                                    out_map.insert(k.clone(), v.clone());
                                }
                            }
                            Ok(Value::Map(out_map))
                        }
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_IMPLEMENTED, // R0504
                                "op-not-implemented",
                                "operation not yet implemented for maps with where",
                                sp.clone(),
                            )
                            .with_help("Currently only 'grab where' is supported for maps.")
                            .with_link("https://goblinlang.org/docs/errors#R0504")
                        ),
                    }
                }

                Position::All => {
                    match &op {
                        Operation::Grab => Ok(Value::Map(map.clone())),
                        Operation::Delete => Ok(Value::Map(BTreeMap::new())),
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_MEANINGFUL, // R0503
                                "op-not-meaningful",
                                "operation not meaningful for maps in 'all' position",
                                sp.clone(),
                            )
                            .with_help("Use Position::At with a key, or 'grab where' for filtering.")
                            .with_link("https://goblinlang.org/docs/errors#R0503")
                        ),
                    }
                }
            }
        }

        // ==================== STRING ====================
        Value::Str(s) => {
            let len = char_len(s);

            match pos {
                Position::First => {
                    match &op {
                        Operation::Grab => {
                            if s.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty string for this operation.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            Ok(Value::Char(s.chars().next().unwrap()))
                        }
                        Operation::Put(v) => {
                            let sub = match v {
                                Value::Str(t) => t.clone(),
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(format!("{}{}", sub, s)))
                        }
                        Operation::Update(v) => {
                            if s.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty string for this operation.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            let with = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(str_update_at(s, 0, with).unwrap()))
                        }
                        Operation::Delete => {
                            if len == 0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("There is no first character to delete.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            Ok(Value::Str(str_delete_at(s, 0).unwrap()))
                        }
                    }
                }

                Position::Last => {
                    match &op {
                        Operation::Grab => {
                            if s.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("Provide a non-empty string for this operation.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            Ok(Value::Char(s.chars().rev().next().unwrap()))
                        }
                        Operation::Put(v) => {
                            let sub = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(format!("{}{}", s, sub)))
                        }
                        Operation::Update(v) => {
                            if len == 0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("There is no last character to update.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            let with = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(str_update_at(s, len - 1, with).unwrap()))
                        }
                        Operation::Delete => {
                            if len == 0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_COLLECTION, // R0701
                                        "empty-collection",
                                        "empty string",
                                        sp.clone(),
                                    )
                                    .with_help("There is no last character to delete.")
                                    .with_link("https://goblinlang.org/docs/errors#R0701")
                                );
                            }
                            Ok(Value::Str(str_delete_at(s, len - 1).unwrap()))
                        }
                    }
                }

                Position::At(idx_val) => {
                    let idx = match idx_val {
                        Value::Int(n) if n >= 0 => n as usize,
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::INVALID_INDEX, // R0401
                                    "invalid-index",
                                    "string index must be a non-negative integer",
                                    sp.clone(),
                                )
                                .with_help("Use 0, 1, 2, … (no negatives, no fractions).")
                                .with_link("https://goblinlang.org/docs/errors#R0401")
                            );
                        }
                    };

                    match &op {
                        Operation::Grab => {
                            if idx >= len {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Use an index within the string’s length.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            Ok(Value::Char(s.chars().nth(idx).unwrap()))
                        }
                        Operation::Put(v) => {
                            if idx > len {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Insert at a valid position from 0..=len.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            let sub = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(str_insert_at(s, idx, sub).unwrap()))
                        }
                        Operation::Update(v) => {
                            if idx >= len {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Update within the string’s valid index range.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            let with = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string operation expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(str_update_at(s, idx, with).unwrap()))
                        }
                        Operation::Delete => {
                            if idx >= len {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Delete within the string’s valid index range.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            Ok(Value::Str(str_delete_at(s, idx).unwrap()))
                        }
                    }
                }

                Position::Where(pred) => {
                    match &op {
                        Operation::Grab | Operation::Delete => {
                            let keep = matches!(&op, Operation::Grab);
                            let mut out = String::new();
                            for c in s.chars() {
                                let ok_v = call_action_by_name(sess, &pred, vec![Value::Char(c)], sp.clone())?;
                                let matches = matches!(ok_v, Value::Bool(true));
                                if matches == keep {
                                    out.push(c);
                                }
                            }
                            Ok(Value::Str(out))
                        }
                        Operation::Update(v) => {
                            let with = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string update expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            let mut out = String::new();
                            for i in 0..len {
                                let ch = slice_char(s, i).unwrap();
                                let ok_v = call_action_by_name(sess, &pred, vec![Value::Str(ch.clone())], sp.clone())?;
                                if matches!(ok_v, Value::Bool(true)) {
                                    out.push_str(with);
                                } else {
                                    out.push_str(&ch);
                                }
                            }
                            Ok(Value::Str(out))
                        }
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_SUPPORTED, // R0505
                                "op-not-supported",
                                "operation not supported",
                                sp.clone(),
                            )
                            .with_help("Use grab/delete/update/put with Position::First/Last/At/Where/All.")
                            .with_link("https://goblinlang.org/docs/errors#R0505")
                        ),
                    }
                }

                Position::All => {
                    match &op {
                        Operation::Grab => Ok(Value::Str(s.clone())),
                        Operation::Delete => Ok(Value::Str(String::new())),
                        Operation::Update(v) => {
                            let sub = match v {
                                Value::Str(t) => t,
                                _ => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            rtcode::TYPE_MISMATCH, // T0205
                                            "type-mismatch",
                                            "string update expects string",
                                            sp.clone(),
                                        )
                                        .with_help("Use a string value for this string operation.")
                                        .with_link("https://goblinlang.org/docs/errors#T0205")
                                    );
                                }
                            };
                            Ok(Value::Str(sub.repeat(len)))
                        }
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_SUPPORTED, // R0505
                                "op-not-supported",
                                "operation not supported",
                                sp.clone(),
                            )
                            .with_help("Use grab/delete/update/put with Position::First/Last/At/Where/All.")
                            .with_link("https://goblinlang.org/docs/errors#R0505")
                        ),
                    }
                }
            }
        }

        // ==================== ARRAY/SEQ ====================
        _ => {
            let xs = as_array_like(coll).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    rtcode::ARRAY_EXPECTED, // R0402
                    "array-expected",
                    "operation expects array/seq/string/map",
                    sp.clone(),
                )
                .with_help("Pass a collection value (array/seq/string/map) to use collection operations.")
                .with_link("https://goblinlang.org/docs/errors#R0402")
            })?;

            match pos {
                Position::First => {
                    match &op {
                        Operation::Grab => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("Provide at least one element.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            Ok(xs[0].clone())
                        }
                        Operation::Put(v) => {
                            let mut out = Vec::with_capacity(xs.len() + 1);
                            out.push(v.clone());
                            out.extend(xs.iter().cloned());
                            Ok(Value::Array(out))
                        }
                        Operation::Update(v) => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("Provide at least one element to update the first position.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            let mut out = xs.to_vec();
                            out[0] = v.clone();
                            Ok(Value::Array(out))
                        }
                        Operation::Delete => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("There is no first element to delete.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            Ok(Value::Array(xs.iter().skip(1).cloned().collect()))
                        }
                    }
                }

                Position::Last => {
                    match &op {
                        Operation::Grab => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("Provide at least one element.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            Ok(xs[xs.len() - 1].clone())
                        }
                        Operation::Put(v) => {
                            let mut out = xs.to_vec();
                            out.push(v.clone());
                            Ok(Value::Array(out))
                        }
                        Operation::Update(v) => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("There is no last element to update.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            let mut out = xs.to_vec();
                            let idx = out.len() - 1;
                            out[idx] = v.clone();
                            Ok(Value::Array(out))
                        }
                        Operation::Delete => {
                            if xs.is_empty() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::EMPTY_ARRAY, // R0404
                                        "empty-array",
                                        "empty array",
                                        sp.clone(),
                                    )
                                    .with_help("There is no last element to delete.")
                                    .with_link("https://goblinlang.org/docs/errors#R0404")
                                );
                            }
                            Ok(Value::Array(xs.iter().take(xs.len() - 1).cloned().collect()))
                        }
                    }
                }

                Position::At(idx_val) => {
                    let idx = match idx_val {
                        Value::Int(n) if n >= 0 => n as usize,
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    rtcode::INVALID_INDEX, // R0401
                                    "invalid-index",
                                    "array index must be a non-negative integer",
                                    sp.clone(),
                                )
                                .with_help("Use 0, 1, 2, … (no negatives, no fractions).")
                                .with_link("https://goblinlang.org/docs/errors#R0401")
                            );
                        }
                    };

                    match &op {
                        Operation::Grab => {
                            if idx >= xs.len() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Use an index within the array’s length.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            Ok(xs[idx].clone())
                        }
                        Operation::Put(v) => {
                            if idx > xs.len() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Insert at a valid position from 0..=len.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            let mut out = Vec::with_capacity(xs.len() + 1);
                            out.extend(xs.iter().take(idx).cloned());
                            out.push(v.clone());
                            out.extend(xs.iter().skip(idx).cloned());
                            Ok(Value::Array(out))
                        }
                        Operation::Update(v) => {
                            if idx >= xs.len() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Update within the array’s valid index range.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            let mut out = xs.to_vec();
                            out[idx] = v.clone();
                            Ok(Value::Array(out))
                        }
                        Operation::Delete => {
                            if idx >= xs.len() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        rtcode::INVALID_INDEX, // R0401
                                        "invalid-index",
                                        "index out of bounds",
                                        sp.clone(),
                                    )
                                    .with_help("Delete within the array’s valid index range.")
                                    .with_link("https://goblinlang.org/docs/errors#R0401")
                                );
                            }
                            let mut out = Vec::with_capacity(xs.len().saturating_sub(1));
                            for (i, v) in xs.iter().enumerate() {
                                if i != idx { out.push(v.clone()); }
                            }
                            Ok(Value::Array(out))
                        }
                    }
                }

                Position::Where(pred) => {
                    match &op {
                        Operation::Grab | Operation::Delete => {
                            let keep = matches!(&op, Operation::Grab);
                            let mut out = Vec::new();
                            for v in xs {
                                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                                let matches = matches!(ok_v, Value::Bool(true));
                                if matches == keep {
                                    out.push(v.clone());
                                }
                            }
                            Ok(Value::Array(out))
                        }
                        Operation::Update(v) => {
                            let mut out = Vec::with_capacity(xs.len());
                            for item in xs {
                                let ok_v = call_action_by_name(sess, &pred, vec![item.clone()], sp.clone())?;
                                if matches!(ok_v, Value::Bool(true)) {
                                    out.push(v.clone());
                                } else {
                                    out.push(item.clone());
                                }
                            }
                            Ok(Value::Array(out))
                        }
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_SUPPORTED, // R0505
                                "op-not-supported",
                                "operation not supported",
                                sp.clone(),
                            )
                            .with_help("Use grab/delete/update/put with Position::First/Last/At/Where/All.")
                            .with_link("https://goblinlang.org/docs/errors#R0505")
                        ),
                    }
                }

                Position::All => {
                    match &op {
                        Operation::Grab => Ok(Value::Array(xs.to_vec())),
                        Operation::Delete => Ok(Value::Array(vec![])),
                        Operation::Update(v) => Ok(Value::Array(vec![v.clone(); xs.len()])),
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                rtcode::OP_NOT_SUPPORTED, // R0505
                                "op-not-supported",
                                "operation not supported",
                                sp.clone(),
                            )
                            .with_help("Use grab/delete/update/put with Position::First/Last/At/Where/All.")
                            .with_link("https://goblinlang.org/docs/errors#R0505")
                        ),
                    }
                }
            }
        }
    }
}

fn call_action_by_name(
    sess: &mut Session,
    name: &str,
    args: Vec<Value>,
    sp: Span,
) -> Result<Value, Diag> {

    // FIRST: Check current module's exports
    if let Some(ref module_name) = sess.current_module.clone() {
        if let Some(crate::modules::ExportedItem::Action(action_decl)) = sess.modules.get_export(&module_name, name) {
            let action_decl = action_decl.clone();
            
            // Execute the action (same code as below)
            let params = &action_decl.params;
            if args.len() > params.len() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected {}, got {})", params.len(), args.len()),
                        sp.clone(),
                    )
                    .with_help(&format!("‘{}’ takes {} argument(s).", name, params.len()))
                    .with_help("Provide all required arguments or remove extras."),
                );
            }

            let mut bound: Vec<(String, Value)> = Vec::with_capacity(params.len());
            for (i, p) in params.iter().enumerate() {
                if i < args.len() {
                    bound.push((p.name.clone(), args[i].clone()));
                } else if let Some(def_e) = &p.default {
                    let v = eval_expr(def_e, sess)?;
                    bound.push((p.name.clone(), v));
                } else {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                            "wrong-arity",
                            &format!(
                                "Wrong number of arguments (expected {}, got {})",
                                params.len(),
                                args.len()
                            ),
                            sp.clone(),
                        )
                        .with_help(&format!("Missing required argument ‘{}’.", p.name))
                        .with_help("Provide all required arguments or define defaults.")
                    );
                }
            }

            sess.push_frame();
            for (k, v) in bound { sess.set_var(k, v); }

            let ret = {
                let ast::ActionBody::Block(stmts) = &action_decl.body;
                let mut last = Value::Unit;
                for st in stmts {
                    if let Some(v) = eval_stmt(st, sess)? {
                        match v {
                            Value::CtrlSkip => { /* keep going */ }
                            Value::CtrlStop => {
                                let rv = sess.get_var("__return__").cloned().unwrap_or(Value::Nil);
                                sess.pop_frame();
                                return Ok(rv);
                            }
                            other => last = other,
                        }
                    }
                }
                last
            };

            sess.pop_frame();
            return Ok(ret);
        }
    }

    // Prefer user-defined actions (shadowable)
    if let Some(decl) = sess.actions.get(name).cloned() {

        let params = &decl.params;
        if args.len() > params.len() {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                    "wrong-arity",
                    "wrong number of arguments",
                    sp.clone(),
                )
                .with_help(&format!("expected {}, got {}", params.len(), args.len()))
                .with_help(&format!("‘{}’ takes {} argument(s)", name, params.len()))
                .with_link("https://goblinlang.org/docs/errors#R0301"),
            );
        }

        let mut bound: Vec<(String, Value)> = Vec::with_capacity(params.len());
        for (i, p) in params.iter().enumerate() {
            if i < args.len() {
                bound.push((p.name.clone(), args[i].clone()));
            } else if let Some(def_e) = &p.default {
                let v = eval_expr(def_e, sess)?;
                bound.push((p.name.clone(), v));
            } else {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                        "missing-argument",
                        "missing required argument",
                        sp.clone(),
                    )
                    .with_help(&format!("argument ‘{}’ is required", p.name))
                    .with_link("https://goblinlang.org/docs/errors#R0302"),
                );
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
                        Value::CtrlSkip => { /* keep going */ }
                        Value::CtrlStop => {
                            let rv = sess.get_var("__return__").cloned().unwrap_or(Value::Nil);
                            sess.pop_frame();  // <-- ADD THIS
                            return Ok(rv);
                        }
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

    let _want_bool = |v: &Value, label: &str| -> Result<bool, Diag> {
        match v {
            Value::Bool(b) => Ok(*b),
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::BOOLEAN_EXPECTED, // T0203
                        "boolean-expected",
                        format!("{label} must be a boolean (true or false)"),
                        sp.clone(),
                    )
                    .with_help("Use `true` or `false`, or an expression that evaluates to a boolean.")
                    .with_link("https://goblinlang.org/docs/errors#T0203"),
                );
            }
        }
    };

    // ---------- Built-ins (shadowable) ----------
    let arity = |wanted: usize| -> Result<(), Diag> {
        if args.len() != wanted {
            Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                    "wrong-arity",
                    "wrong number of arguments",
                    sp.clone(),
                )
                .with_help(&format!("expected {}, got {}", wanted, args.len()))
                .with_help(&format!("‘{}’ takes {} argument(s)", name, wanted))
                .with_link("https://goblinlang.org/docs/errors#R0301"),
            )
        } else {
            Ok(())
        }
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
            _ => Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    &format!("{label} expects a string"),
                    sp.clone(),
                )
                .with_help("Pass a string value.")
                .with_link("https://goblinlang.org/docs/errors#T0205"),
            ),
        }
    };

    // Map a (&str -> String) transform over string or array<string>
    let map_str_1 = |v: &Value, label: &str, f: &dyn Fn(&str) -> String| -> Result<Value, Diag> {
        match v {
            Value::Str(s) => Ok(Value::Str(f(s))),
            _ => {
                if let Some(xs) = as_array_like(v) { // handles Array or Seq
                    let mut out = Vec::with_capacity(xs.len());
                    for it in xs {
                        match it {
                            Value::Str(s) => out.push(Value::Str(f(s))),
                            _ => {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                        "type-mismatch",
                                        &format!("{label} expects a string (or array/seq of strings)"),
                                        sp.clone(),
                                    )
                                    .with_help("Pass a string or an array/seq of strings.")
                                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                                );
                            }
                        }
                    }
                    Ok(Value::Array(out)) // keep legacy Array output for now
                    // If/when producers should return Seq:
                    // Ok(Value::Seq(Seq::from_vec(out)))
                } else {
                    Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            &format!("{label} expects a string (or array/seq of strings)"),
                            sp.clone(),
                        )
                        .with_help("Pass a string or an array/seq of strings.")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            }
        }
    };

    let out: Value = match name {
        "is_bound_name" => {
            // is_bound_name(name: string) -> bool
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘is_bound_name’ takes exactly 1 argument.")
                    .with_help("Provide exactly one string (the variable name).")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let name = match &args[0] {
                Value::Str(s) => s.clone(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "is_bound_name expects a string (variable name)",
                            sp.clone(),
                        )
                        .with_help("Pass a string, e.g. \"x\".")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };

            let bound = sess.find_name_frame(&name).is_some();
            Value::Bool(bound)
        }

        "is_type" => {
            // recv.is_type(typename)
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘is_type’ takes 2 arguments: a value and a string type name (e.g., \"int\").")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let recv = &args[0];
            let type_name = match &args[1] {
                Value::Str(s) => s.as_str(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "is_type expects the second argument to be a string type name",
                            sp.clone(),
                        )
                        .with_help("Pass a string such as \"int\", \"float\", \"bool\", \"str\", \"big\", or \"pct\".")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };

            let can_convert = match (recv, type_name) {
                // String to int conversion check
                (Value::Str(s), "int") => {
                    let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
                    Decimal::from_str(&cleaned).is_ok()
                }
                // String to float conversion check
                (Value::Str(s), "float") => {
                    let cleaned: String = s.trim().chars().filter(|&c| c != '_').collect();
                    Decimal::from_str(&cleaned).is_ok() || cleaned.parse::<f64>().is_ok()
                }

                // Already the correct type
                (Value::Int(_), "int") => true,
                (Value::Float(_), "float") => true,
                (Value::Bool(_), "bool") => true,
                (Value::Str(_), "str") => true,
                (Value::Big(_), "big") => true,
                (Value::Pct(_), "pct") => true,

                // Numeric types can convert between each other
                (Value::Int(_) | Value::Float(_) | Value::Big(_) | Value::Pct(_), "int" | "float" | "big" | "pct") => true,

                _ => false,
            };

            Value::Bool(can_convert)
        }

        "input" | "ask" => {
            // Guard: non-interactive mode
            if std::env::var("GOBLIN_NONINTERACTIVE").ok().as_deref() == Some("1") {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::OP_NOT_SUPPORTED, // R0505
                        "non-interactive",
                        "interactive input is disabled in non-interactive mode",
                        sp.clone(),
                    )
                    .with_help("Run without GOBLIN_NONINTERACTIVE=1 or remove calls to input/ask.")
                    .with_link("https://goblinlang.org/docs/errors#R0505"),
                );
            }

            // Expect 0 or 1 argument (optional prompt)
            let prompt = if args.is_empty() {
                ""
            } else {
                match &args[0] {
                    Value::Str(s) => s.as_str(),
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "input/ask expects the prompt to be a string",
                                sp.clone(),
                            )
                            .with_help("Pass a single string argument as the prompt, e.g., ask(\"Name: \").")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        );
                    }
                }
            };

            // Print prompt if provided
            if !prompt.is_empty() {
                print!("{}", prompt);
                use std::io::Write;
                std::io::stdout().flush().unwrap();
            }

            // Read line from stdin
            let mut buffer = String::new();
            std::io::stdin()
                .read_line(&mut buffer)
                .map_err(|e| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::IMPORT_IO, // R0501 (generic I/O failure here)
                        "io-error",
                        &format!("failed to read from stdin: {}", e),
                        sp.clone(),
                    )
                    .with_help("Ensure stdin is available and readable in this environment.")
                    .with_link("https://goblinlang.org/docs/errors#R0501")
                })?;

            // Trim newline and return as string
            Value::Str(buffer.trim_end().to_string())
        }

        // ----- Introspection -----
        "valtype" | "vt" => {
            arity(1)?;
            let recv = match &args[0] {
                Value::Formatted(inner, _) => &**inner,
                other => other,
            };
            let kind = match recv {
                Value::Nil => "nil",
                Value::Bool(_) => "bool",
                Value::Float(n) if n.is_finite() && n.fract() == 0.0 => "int",
                Value::Big(_) => "big",
                Value::Float(_) => "float",
                Value::Int(_) => "int",
                Value::Pct(_) => "pct",
                Value::Str(_) => "str",
                Value::Char(_) => "char",
                Value::Array(_) => "array",
                Value::Map(_) => "map",
                Value::Pair(_, _) => "pair",
                Value::Seq(_) => "seq",
                Value::Unit => "unit",
                Value::CtrlSkip | Value::CtrlStop => "control",
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
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        "format expects 1 or 3 arguments: (dec) or (dec, sep_th, sep_dec)",
                        sp.clone(),
                    )
                    .with_help("Call as value.format(dec) or value.format(dec, sep_th, sep_dec).")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // receiver: unwrap if already formatted
            let inner = match &args[0] {
                Value::Formatted(inner, _) => *inner.clone(),
                v => v.clone(),
            };

            // arg1: decimals
            let dec: u32 = match &args[1] {
                Value::Float(x) => {
                    if !x.is_finite() {
                        0
                    } else {
                        let n = *x as i64;
                        if (*x - n as f64).abs() > 0.0 || n < 0 {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "format decimals must be an integer ≥ 0",
                                    sp.clone(),
                                )
                                .with_help("Provide a non-negative whole number for the decimals argument.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                        n as u32
                    }
                }
                Value::Str(s) => s.parse::<u32>().map_err(|_| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "format decimals must be an integer ≥ 0",
                        sp.clone(),
                    )
                    .with_help("Example: 0, 2, 4 …")
                    .with_link("https://goblinlang.org/docs/errors#T0205")
                })?,
                Value::Char(c) if c.is_ascii_digit() => c
                    .to_string()
                    .parse::<u32>()
                    .map_err(|_| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "format decimals must be an integer ≥ 0",
                            sp.clone(),
                        )
                        .with_help("Example: 0, 2, 4 …")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "format decimals must be an integer ≥ 0",
                            sp.clone(),
                        )
                        .with_help("Pass a non-negative whole number (e.g., 0, 2, 4).")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            let mut spec = FormatSpec {
                decimals: dec,
                sep_thousands: None,
                sep_decimal: '.',
            };

            if args.len() == 4 {
                // thousands sep (arg2)
                spec.sep_thousands = match &args[2] {
                    Value::Str(s) => match s.as_str() {
                        "," => Some(','),
                        "." => Some('.'),
                        "_" => Some('_'),
                        "'" => Some('\''),
                        "none" => None,
                        other => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    &format!("unknown thousands separator: {other}"),
                                    sp.clone(),
                                )
                                .with_help("Use one of: \",\", \".\", \"_\", \"'\", or \"none\".")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    },
                    Value::Char(c) => match *c {
                        ',' => Some(','),
                        '.' => Some('.'),
                        '_' => Some('_'),
                        '\'' => Some('\''),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "sep_th must be ',', '.', '_', '\\'', or 'none'",
                                    sp.clone(),
                                )
                                .with_help("Use one of: \",\", \".\", \"_\", or \"'\"; for none, pass the string \"none\".")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    },
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "sep_th must be ',', '.', '_', '\\'', or 'none'",
                                sp.clone(),
                            )
                            .with_help("Example: value.format(2, \",\", \".\")")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        )
                    }
                };

                // decimal marker (arg3)
                spec.sep_decimal = match &args[3] {
                    Value::Str(s) => match s.as_str() {
                        "." => '.',
                        "," => ',',
                        other => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    &format!("unknown decimal marker: {other}"),
                                    sp.clone(),
                                )
                                .with_help("Use '.' or ','.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    },
                    Value::Char(c) => match *c {
                        '.' => '.',
                        ',' => ',',
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "sep_dec must be '.' or ','",
                                    sp.clone(),
                                )
                                .with_help("Use '.' or ','.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    },
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "sep_dec must be '.' or ','",
                                sp.clone(),
                            )
                            .with_help("Example: value.format(2, \",\", \".\")")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        )
                    }
                };
            }

            match inner {
                Value::Float(x) => Value::Formatted(Box::new(Value::Float(x)), spec),
                Value::Int(i)   => Value::Formatted(Box::new(Value::Int(i)),   spec),
                Value::Pct(p)   => Value::Formatted(Box::new(Value::Pct(p)),   spec),
                Value::Big(d)   => Value::Formatted(Box::new(Value::Big(d)),   spec),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH_RUNTIME, // R0201
                            "type-mismatch-runtime",
                            "format receiver must be a number",
                            sp.clone(),
                        )
                        .with_help("Call format on int/float/big/pct values.")
                        .with_link("https://goblinlang.org/docs/errors#R0201"),
                    )
                }
            }
        }

        "clear_format" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("Call as clear_format(value) with exactly one argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            match &args[0] {
                Value::Formatted(inner, _) => *inner.clone(),
                v => v.clone(),
            }
        }

        "format_info" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("Call as format_info(value) with exactly one argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
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
                Value::Int(i)   => Value::Int(*i),                // already integral
                Value::Float(f) => Value::Float(f.round()),
                Value::Pct(p)   => Value::Float(p.round()),
                Value::Big(d)   => Value::Big(d.round_dp(0)),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                            "numeric-expected",
                            "round requires a numeric value",
                            sp.clone(),
                        )
                        .with_help("Pass an int, float, big, or pct.")
                        .with_link("https://goblinlang.org/docs/errors#R0200"),
                    );
                }
            }
        }
        "floor" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)   => Value::Int(*i),            // already integral
                Value::Float(f) => Value::Float(f.floor()),
                Value::Pct(p)   => Value::Float(p.floor()),
                Value::Big(d)   => Value::Big(d.floor()),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                            "numeric-expected",
                            "floor requires a numeric value",
                            sp.clone(),
                        )
                        .with_help("Pass an int, float, big, or pct.")
                        .with_link("https://goblinlang.org/docs/errors#R0200"),
                    );
                }
            }
        },
        "ceil" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)   => Value::Int(*i),            // already integral
                Value::Float(f) => Value::Float(f.ceil()),
                Value::Pct(p)   => Value::Float(p.ceil()),
                Value::Big(d)   => Value::Big(d.ceil()),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                            "numeric-expected",
                            "ceil requires a numeric value",
                            sp.clone(),
                        )
                        .with_help("Pass an int, float, big, or pct.")
                        .with_link("https://goblinlang.org/docs/errors#R0200"),
                    );
                }
            }
        },
        "abs" => {
            arity(1)?;
            match &args[0] {
                Value::Int(i)   => Value::Int(i.abs()),
                Value::Float(f) => Value::Float(f.abs()),
                Value::Pct(p)   => Value::Float(p.abs()),
                Value::Big(d)   => Value::Big(d.abs()),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                            "numeric-expected",
                            "abs requires a numeric value",
                            sp.clone(),
                        )
                        .with_help("Pass an int, float, big, or pct.")
                        .with_link("https://goblinlang.org/docs/errors#R0200"),
                    );
                }
            }
        },
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
                            let n = et.to_i64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BIG_EXPONENT_RANGE, // R0203
                                    "big-exponent-range",
                                    "big exponent out of i64 range",
                                    sp.clone(),
                                )
                                .with_help("Use a smaller integer exponent.")
                                .with_link("https://goblinlang.org/docs/errors#R0203")
                            })?;
                            Value::Big(decimal_powi(base, n)?)
                        } else {
                            // fractional exponent -> float fallback
                            let bf = base.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "big base overflow to float",
                                    sp.clone(),
                                )
                                .with_help("Reduce the magnitude of the base or use a different numeric type.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            let ef = e.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "big exponent overflow to float",
                                    sp.clone(),
                                )
                                .with_help("Reduce the magnitude of the exponent or use a different numeric type.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            Value::Float(bf.powf(ef))
                        }
                    }
                    Value::Float(f) | Value::Pct(f) => {
                        if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                            Value::Big(decimal_powi(base, *f as i64)?)
                        } else {
                            let bf = base.to_f64().ok_or_else(|| {
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BIG_OVERFLOW, // R0326
                                    "big-overflow",
                                    "big base overflow to float",
                                    sp.clone(),
                                )
                                .with_help("Reduce the magnitude of the base or use a different numeric type.")
                                .with_link("https://goblinlang.org/docs/errors#R0326")
                            })?;
                            Value::Float(bf.powf(*f))
                        }
                    }
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                "numeric-expected",
                                "pow requires a numeric exponent",
                                sp.clone(),
                            )
                            .with_help("Pass int, float, big, or pct as the exponent.")
                            .with_link("https://goblinlang.org/docs/errors#R0200"),
                        );
                    }
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                                "math-domain",
                                "sqrt domain error: cannot take square root of a negative value",
                                sp.clone(),
                            )
                            .with_help("Ensure the input is ≥ 0.")
                            .with_link("https://goblinlang.org/docs/errors#R0207"),
                        );
                    }
                    let f = d.to_f64().ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::BIG_OVERFLOW, // R0326
                            "big-overflow",
                            "big overflow to float for sqrt",
                            sp.clone(),
                        )
                        .with_help("Reduce the magnitude of the value or use a different numeric type.")
                        .with_link("https://goblinlang.org/docs/errors#R0326")
                    })?;
                    Value::Float(f.sqrt())
                }
                _ => {
                    let n = to_f64_for_math(&args[0], sp.clone(), "sqrt")?;
                    if n < 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                                "math-domain",
                                "sqrt domain error: cannot take square root of a negative value",
                                sp.clone(),
                            )
                            .with_help("Ensure the input is ≥ 0.")
                            .with_link("https://goblinlang.org/docs/errors#R0207"),
                        );
                    }
                    Value::Float(n.sqrt())
                }
            }
        }

        // ----- Collections / Numbers -----
        // Builtin: pick({ count, digits?, unique?, allow_dups?, src? | range_start?, range_end?, range_inclusive? })
        "pick" => {
            use std::collections::{BTreeMap, BTreeSet};

            // ---- validate arg ----
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("Provide exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let cfg = match &args[0] {
                Value::Map(m) => m.clone(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "pick expects a config object (map)",
                            sp.clone(),
                        )
                        .with_help("Pass a map, e.g. { count: 3, src: [...] }")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    );
                }
            };

            // ---- small getters (accept Int or Float; Str optional) ----
            let get_bool = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b) = v { Some(*b) } else { None })
            };
            let get_num = |m: &BTreeMap<String, Value>, k: &str| -> Option<f64> {
                match m.get(k)? {
                    Value::Float(n) => Some(*n),
                    Value::Int(i)   => Some(*i as f64),
                    Value::Str(s)   => s.parse::<f64>().ok(),
                    _ => None,
                }
            };

            // ---- base config ----
            let count = get_num(&cfg, "count").unwrap_or(1.0);
            if count < 1.0 || count.fract() != 0.0 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                        "pick-count-not-integer",
                        "pick ‘count’ must be a positive integer",
                        sp.clone(),
                    )
                    .with_help("Provide an integer ≥ 1, e.g. { count: 3 }")
                    .with_link("https://goblinlang.org/docs/errors#T0202"),
                );
            }
            let n_out = count as usize;

            let digits_opt_i64: Option<i64> = get_num(&cfg, "digits").map(|d| d as i64);
            if let Some(d) = digits_opt_i64 {
                if d < 1 {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                            "digits-positive-int",
                            "‘digits’ must be a positive integer (>= 1)",
                            sp.clone(),
                        )
                        .with_help("Provide an integer ≥ 1, e.g. { digits: 3 }")
                        .with_link("https://goblinlang.org/docs/errors#T0202"),
                    );
                }
            }
            let unique_digits = get_bool(&cfg, "unique").unwrap_or(false);

            let has_range = cfg.contains_key("range_start") && cfg.contains_key("range_end");

            // Check for collection source (array, seq, or map)
            enum CollectionSource {
                Array(Vec<Value>),
                Seq(Vec<Value>),
                Map(BTreeMap<String, Value>),
            }

            let src_collection: Option<CollectionSource> = if let Some(Value::Array(arr)) = cfg.get("src") {
                Some(CollectionSource::Array(arr.clone()))
            } else if let Some(Value::Seq(seq)) = cfg.get("src") {
                Some(CollectionSource::Seq(seq.to_vec()))
            } else if let Some(Value::Map(m)) = cfg.get("src") {
                Some(CollectionSource::Map(m.clone()))
            } else {
                None
            };

            // default allow_dups: collections => false; numeric (range/digits) => true
            let allow_dups = match get_bool(&cfg, "allow_dups") {
                Some(b) => b,
                None => if src_collection.is_some() { false } else { true },
            };

            // ---- handy finisher ----
            let finish = |mut items: Vec<Value>| -> Value {
                if n_out == 1 { items.pop().unwrap_or(Value::Nil) } else { Value::Array(items) }
            };

            // ================== Collections ==================
            if let Some(coll) = src_collection {
                match coll {
                    CollectionSource::Array(arr) | CollectionSource::Seq(arr) => {
                        if arr.is_empty() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                                    "empty-collection",
                                    "cannot pick from an empty collection",
                                    sp.clone(),
                                )
                                .with_help("Provide a non-empty source for ‘pick’, or handle the empty case.")
                                .with_link("https://goblinlang.org/docs/errors#R0701"),
                            );
                        }
                        if !allow_dups && n_out > arr.len() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::INSUFFICIENT_DISTINCT, // R0703 (NEW)
                                    "insufficient-distinct",
                                    &format!(
                                        "cannot pick {} distinct items from {}",
                                        n_out,
                                        arr.len()
                                    ),
                                    sp.clone(),
                                )
                                .with_help("Reduce ‘count’, enable ‘allow_dups’, or supply a larger source.")
                                .with_link("https://goblinlang.org/docs/errors#R0703"),
                            );
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
                    
                    CollectionSource::Map(map) => {
                        if map.is_empty() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                                    "empty-collection",
                                    "cannot pick from an empty map",
                                    sp.clone(),
                                )
                                .with_help("Provide at least one entry in the map.")
                                .with_link("https://goblinlang.org/docs/errors#R0701"),
                            );
                        }
                        
                        let entries: Vec<(String, Value)> = map.iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect();
                        
                        if !allow_dups && n_out > entries.len() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, 
                                    "sample-too-large",
                                    &format!(
                                        "requested {} entries but only {} available",
                                        n_out,
                                        entries.len()
                                    ),
                                    sp.clone(),
                                )
                                .with_help("Reduce ‘count’ or enable allow_dups: true.")
                                .with_link("https://goblinlang.org/docs/errors#R0704"),
                            );
                        }

                        let out = if allow_dups {
                            let mut out = Vec::with_capacity(n_out);
                            for _ in 0..n_out {
                                let idx = rng_index(sess, entries.len());
                                let mut pair = BTreeMap::new();
                                pair.insert(entries[idx].0.clone(), entries[idx].1.clone());
                                out.push(Value::Map(pair));
                            }
                            out
                        } else {
                            let mut idxs: Vec<usize> = (0..entries.len()).collect();
                            let mut out = Vec::with_capacity(n_out);
                            for i in 0..n_out {
                                let j = i + rng_index(sess, entries.len() - i);
                                idxs.swap(i, j);
                                let mut pair = BTreeMap::new();
                                pair.insert(entries[idxs[i]].0.clone(), entries[idxs[i]].1.clone());
                                out.push(Value::Map(pair));
                            }
                            out
                        };
                        
                        return Ok(finish(out));
                    }
                }
            }

            // ---------- helpers for numeric domains ----------
            let within_digits = |v: i64, d: i64| -> bool {
                if d <= 0 { return true; }
                let min = 10_i64.pow((d - 1) as u32);
                let max = 10_i64.pow(d as u32) - 1;
                v >= min && v <= max
            };
            let has_unique_digits = |mut v: i64, d: i64| -> bool {
                if !unique_digits { return true; }
                if d > 0 && v < 10_i64.pow((d - 1) as u32) { return false; }
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
            if has_range {
                let a = get_num(&cfg, "range_start").ok_or_else(|| rt("T0201", "range bounds must be numbers", sp.clone()))?;
                let b = get_num(&cfg, "range_end").ok_or_else(|| rt("T0201", "range bounds must be numbers", sp.clone()))?;
                if a.fract() != 0.0 || b.fract() != 0.0 {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204 (NEW)
                            "integer-expected",
                            "range bounds must be integers",
                            sp.clone(),
                        )
                        .with_help("Use whole numbers for ‘range_start’ and ‘range_end’ (e.g., 1 and 10).")
                        .with_link("https://goblinlang.org/docs/errors#T0204"),
                    );
                }
                let mut lo = a as i64;
                let mut hi = b as i64;
                if lo > hi { std::mem::swap(&mut lo, &mut hi); }
                let inc = get_bool(&cfg, "range_inclusive").unwrap_or(false);

                let d = digits_opt_i64.unwrap_or(0);

                // Build finite pool with filters.
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
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INVALID_RANGE_NO_VALUES, // R0702 (NEW)
                            "invalid-range-no-values",
                            "invalid range (no values after filters)",
                            sp.clone(),
                        )
                        .with_help("Adjust bounds and filters (e.g., digits/unique) so the range yields at least one value.")
                        .with_link("https://goblinlang.org/docs/errors#R0702"),
                    );
                }
                if !allow_dups && n_out > pool.len() {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, // R0704
                            "sample-too-large",
                            &format!(
                                "requested sample of {} exceeds available {} distinct values in range",
                                n_out,
                                pool.len()
                            ),
                            sp.clone(),
                        )
                        .with_help("Reduce ‘count’ or set allow_dups: true.")
                        .with_link("https://goblinlang.org/docs/errors#R0704"),
                    );
                }

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
            if let Some(d) = digits_opt_i64 {
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
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, // R0704
                            "sample-too-large",
                            &format!(
                                "requested sample of {} exceeds available {} distinct values in digit space",
                                n_out,
                                domain_size
                            ),
                            sp.clone(),
                        )
                        .with_help("Reduce ‘count’ or set allow_dups: true.")
                        .with_link("https://goblinlang.org/docs/errors#R0704"),
                    );
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

                let out = if allow_dups {
                    let mut out = Vec::with_capacity(n_out);
                    for _ in 0..n_out {
                        out.push(Value::Int(gen_one()));
                    }
                    out
                } else {
                    // without replacement:
                    let mut set = BTreeSet::<i64>::new();
                    let mut attempts_left: usize = domain_size.saturating_mul(3).max(n_out * 10);
                    while set.len() < n_out {
                        if attempts_left == 0 {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::INSUFFICIENT_DISTINCT, // R0703
                                    "insufficient-distinct",
                                    "could not generate enough distinct values",
                                    sp.clone(),
                                )
                                .with_help("Lower ‘count’, relax uniqueness (unique: false / allow_dups: true), or widen the digit space.")
                                .with_link("https://goblinlang.org/docs/errors#R0703"),
                            );
                        }
                        attempts_left -= 1;
                        set.insert(gen_one());
                    }
                    set.into_iter().map(|v| Value::Int(v)).collect()
                };

                return Ok(finish(out));
            }

            // If we got here, there was neither `src` nor any numeric form.
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::PICK_MISSING_SOURCE, // P1408 (NEW)
                    "pick-missing-source",
                    "pick needs a source: `from <collection>` or a numeric form",
                    sp.clone(),
                )
                .with_help("Provide `src: [...]`/`src: {...}` or numeric options like { range_start, range_end } or { digits }.")
                .with_link("https://goblinlang.org/docs/errors#P1408"),
            );
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

        // ====== roll (numeric result, INT) ======
        "roll" => {
            use std::collections::BTreeMap;

            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("Provide exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "config-expected",
                            "roll expects a config object (map)",
                            sp.clone(),
                        )
                        .with_help("Pass an object like { count: 3, sides: 6 }.")
                        .with_help("Example: roll({ count: 2, sides: 6 })")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            // --- helpers: hard-cast to i64 (truncate for non-integers) ---
            let cast_i64_from_value = |v: &Value| -> Option<i64> {
                match v {
                    Value::Int(i) => Some(*i),
                    Value::Float(f) => Some(f.trunc() as i64),
                    #[allow(unreachable_patterns)]
                    Value::Big(d) => { #[allow(deprecated)] d.trunc().to_i64() },
                    Value::Str(s) => {
                        let cleaned = s.trim().trim_start_matches('+').replace('_', "");
                        cleaned.parse::<i128>().ok().and_then(|x| i64::try_from(x).ok())
                    }
                    _ => None,
                }
            };
            let req_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Result<i64, Diag> {
                match m.get(k) {
                    Some(v) => cast_i64_from_value(v).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0201
                            "integer-expected",
                            &format!("‘roll.{k}’ must be an integer-like value"),
                            sp.clone(),
                        )
                        .with_help("Use an integer, a float that truncates cleanly, a big integer, or a numeric string.")
                        .with_link("https://goblinlang.org/docs/errors#T0201")
                    }),
                    None => Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205 (generic type/config shape error)
                            "missing-field",
                            &format!("Missing required field ‘roll.{k}’."),
                            sp.clone(),
                        )
                        .with_help("Provide this key in the config map, e.g. { count: 3, sides: 6 }.")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    ),
                }
            };
            let opt_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| cast_i64_from_value(v))
            };
            let opt_bool = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b) = v { Some(*b) } else { None })
            };

            // ---- read fields (now always i64 thanks to casting) ----
            let count    = req_i64(cfg, "count")?;
            let sides    = req_i64(cfg, "sides")?;
            let modifier = opt_i64(cfg, "modifier").or_else(|| opt_i64(cfg, "mod")).unwrap_or(0);

            // extras
            let keep_high = opt_i64(cfg, "keep_high").unwrap_or(0);
            let drop_low  = opt_i64(cfg, "drop_low").unwrap_or(0);
            let reroll_eq = opt_i64(cfg, "reroll_eq");
            let explode   = opt_bool(cfg, "explode").unwrap_or(false);
            let adv       = opt_bool(cfg, "adv").unwrap_or(false);
            let dis       = opt_bool(cfg, "dis").unwrap_or(false);
            let clamp_lo  = opt_i64(cfg, "clamp_min");
            let clamp_hi  = opt_i64(cfg, "clamp_max");

            if (adv || dis) && count != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::SINGLE_DIE_REQUIRED, // R0705
                        "single-die-required",
                        "adv/dis requires a single die (count=1)",
                        sp.clone(),
                    )
                    .with_help("Use a single die: { count: 1 } when using adv/dis.")
                    .with_link("https://goblinlang.org/docs/errors#R0705"),
                );
            }
            if keep_high > 0 && drop_low > 0 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::INVALID_OPTION_COMBINATION, // R0706
                        "invalid-option-combination",
                        "cannot combine keep_high and drop_low",
                        sp.clone(),
                    )
                    .with_help("Remove one of the options: keep_high or drop_low.")
                    .with_link("https://goblinlang.org/docs/errors#R0706"),
                );
            }
            if let Some(x) = reroll_eq {
                if x < 1 || x > sides {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::REROLL_EQ_OUT_OF_RANGE, // R0707
                            "reroll-eq-out-of-range",
                            "reroll_eq must be between 1 and sides",
                            sp.clone(),
                        )
                        .with_help("Choose an integer in the inclusive range [1, sides].")
                        .with_link("https://goblinlang.org/docs/errors#R0707"),
                    );
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

            // === produce a plain Value::Int ===
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

            Value::Int(result_num)
        },

        // ====== roll_detail (map with values / kept / dropped / sum / total) ======
        "roll_detail" => {
            use std::collections::BTreeMap;

            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘roll_detail’ takes exactly 1 argument.")
                    .with_help("Example: roll_detail({ count: 2, sides: 6 })")
                );
            }

            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "config-expected",
                            "‘roll_detail’ expects a config object (map).",
                            sp.clone(),
                        )
                        .with_help("Pass a map like { count: 3, sides: 6 }.")
                        .with_help("Example: roll_detail({ count: 2, sides: 6, keep_high: 1 })")
                    );
                }
            };

            // --- helpers: hard-cast to i64 (truncate for non-integers) ---
            let cast_i64_from_value = |v: &Value| -> Option<i64> {
                match v {
                    Value::Int(i) => Some(*i),
                    Value::Float(f) => Some(f.trunc() as i64),
                    #[allow(unreachable_patterns)]
                    Value::Big(d) => { #[allow(deprecated)] d.trunc().to_i64() },
                    Value::Str(s) => {
                        let cleaned = s.trim().trim_start_matches('+').replace('_', "");
                        cleaned.parse::<i128>().ok().and_then(|x| i64::try_from(x).ok())
                    }
                    _ => None,
                }
            };
            let req_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Result<i64, Diag> {
                match m.get(k) {
                    Some(v) => cast_i64_from_value(v).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "integer-like-expected",
                            &format!("roll ‘{k}’ must be an integer-like value"),
                            sp.clone(),
                        )
                        .with_help("Use an integer (e.g., 3), a numeric float that truncates (e.g., 3.0), or a numeric string like \"3\".")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    }),
                    None => Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                            "missing-field",
                            &format!("missing roll field ‘{k}’"),
                            sp.clone(),
                        )
                        .with_help(&format!("Add ‘{k}’ to the config map, e.g., {{ {k}: 6 }}."))
                        .with_link("https://goblinlang.org/docs/errors#R0403")
                    ),
                }
            };
            let opt_i64 = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| cast_i64_from_value(v))
            };
            let opt_bool = |m: &BTreeMap<String, Value>, k: &str| -> Option<bool> {
                m.get(k).and_then(|v| if let Value::Bool(b) = v { Some(*b) } else { None })
            };

            // ---- read fields (now always i64 thanks to casting) ----
            let count    = req_i64(cfg, "count")?;
            let sides    = req_i64(cfg, "sides")?;
            let modifier = opt_i64(cfg, "modifier").or_else(|| opt_i64(cfg, "mod")).unwrap_or(0);

            // extras
            let keep_high = opt_i64(cfg, "keep_high").unwrap_or(0);
            let drop_low  = opt_i64(cfg, "drop_low").unwrap_or(0);
            let reroll_eq = opt_i64(cfg, "reroll_eq");
            let explode   = opt_bool(cfg, "explode").unwrap_or(false);
            let adv       = opt_bool(cfg, "adv").unwrap_or(false);
            let dis       = opt_bool(cfg, "dis").unwrap_or(false);
            let clamp_lo  = opt_i64(cfg, "clamp_min");
            let clamp_hi  = opt_i64(cfg, "clamp_max");

            if (adv || dis) && count != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::SINGLE_DIE_REQUIRED, // R0705
                        "single-die-required",
                        "adv/dis requires a single die (count=1)",
                        sp.clone(),
                    )
                    .with_help("Set ‘count’ to 1 when using advantage (adv) or disadvantage (dis).")
                    .with_link("https://goblinlang.org/docs/errors#R0705"),
                );
            }

            if keep_high > 0 && drop_low > 0 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::INVALID_OPTION_COMBINATION, // R0706
                        "invalid-option-combination",
                        "cannot combine keep_high and drop_low",
                        sp.clone(),
                    )
                    .with_help("Use only one of ‘keep_high’ or ‘drop_low’, not both.")
                    .with_link("https://goblinlang.org/docs/errors#R0706"),
                );
            }

            if let Some(x) = reroll_eq {
                if x < 1 || x > sides {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::REROLL_EQ_OUT_OF_RANGE, // R0707
                            "reroll-eq-out-of-range",
                            "reroll_eq must be between 1 and sides",
                            sp.clone(),
                        )
                        .with_help("Choose a face within the range [1, sides].")
                        .with_link("https://goblinlang.org/docs/errors#R0707"),
                    );
                }
            }

            // RNG (same as roll)
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
                out.insert("sides".into(),    Value::Int(sides));
                out.insert("modifier".into(), Value::Int(modifier));
                out.insert("values".into(),   Value::Array(vec![Value::Int(a), Value::Int(b)]));
                out.insert("kept".into(),     Value::Array(vec![Value::Int(chosen)]));
                out.insert("dropped".into(),  Value::Array(vec![Value::Int(dropped_val)]));
                out.insert("sum".into(),      Value::Int(chosen));
                out.insert("total".into(),    Value::Int(total));
                out.insert("adv".into(),      Value::Bool(adv));
                out.insert("dis".into(),      Value::Bool(dis));
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi)); }
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
                out.insert("count".into(),    Value::Int(count));
                out.insert("sides".into(),    Value::Int(sides));
                out.insert("modifier".into(), Value::Int(modifier));
                out.insert("values".into(),   Value::Array(vals.into_iter().map(Value::Int).collect()));
                out.insert("kept".into(),     Value::Array(kept_vals.into_iter().map(Value::Int).collect()));
                out.insert("dropped".into(),  Value::Array(dropped_vals.into_iter().map(Value::Int).collect()));
                out.insert("sum".into(),      Value::Int(kept_sum));
                out.insert("total".into(),    Value::Int(total));
                if keep_high > 0 { out.insert("keep_high".into(), Value::Int(keep_high)); }
                if drop_low  > 0 { out.insert("drop_low".into(),  Value::Int(drop_low)); }
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Int(x)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Int(lo)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Int(hi)); }
                Value::Map(out)
            };

            detail_out
        },

        "roll_str" | "roll_detail_str" => {
            let is_detail = name == "roll_detail_str";

            // Arity: exactly 1 argument
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help(&format!("‘{}’ takes exactly 1 argument.", name))
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // Expect a string dice expression
            let dice_str = match &args[0] {
                Value::Str(s) => s.as_str(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            &format!("‘{}’ expects a string dice expression.", name),
                            sp.clone(),
                        )
                        .with_help("Pass a dice string like \"2d6+1\", \"4d8kh3\", or \"1d20+5\".")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            // Parse dice notation
            let cfg = parse_dice_string(dice_str, sp.clone()).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::INVALID_DICE_NOTATION, // P0340
                    "invalid-dice-notation",
                    &format!("Invalid dice notation ‘{}’: {}", dice_str, e),
                    sp.clone(),
                )
                .with_help("Use NdM with optional modifiers, e.g. \"3d6\", \"2d20kh1+3\".")
                .with_link("https://goblinlang.org/docs/errors#P0340")
            })?;

            // Dispatch to roll / roll_detail
            let roll_fn = if is_detail { "roll_detail" } else { "roll" };
            return call_action_by_name(sess, roll_fn, vec![Value::Map(cfg)], sp.clone());
        }

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
        "has" => { // s contains sub? -> Bool
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘has’ takes exactly 2 arguments.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let s   = want_str(&args[0], "has")?;
            let sub = want_str(&args[1], "has")?;
            Value::Bool(s.contains(&sub))
        }

        "find" => { // first index of sub (0-based), or nil
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘find’ takes exactly 2 arguments.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let s   = want_str(&args[0], "find")?;
            let sub = want_str(&args[1], "find")?;
            match s.find(&sub) { Some(i) => Value::Int(i as i64), None => Value::Nil }
        }

        "find_all" => {
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘find_all’ takes exactly 2 arguments.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
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
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "‘count’ expects a string or collection",
                                    sp.clone(),
                                )
                                .with_help("Pass a string, array/seq, or map to ‘count’.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                    }
                }
                // count(s, sub) -> substring occurrences
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
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                            "wrong-arity",
                            &format!("Wrong number of arguments (expected 1 or 2, got {})", args.len()),
                            sp.clone(),
                        )
                        .with_help("‘count’ takes 1 or 2 arguments.")
                        .with_link("https://goblinlang.org/docs/errors#R0301"),
                    );
                }
            }
        }

        // ===== Other Stuff =====

        "shuffle" => {
            arity(1)?;
            match &args[0] {
                Value::Str(s) => {
                    let mut v: Vec<char> = s.chars().collect();
                    for i in 0..v.len() {
                        let j = i + rng_index(sess, v.len() - i);
                        v.swap(i, j);
                    }
                    Value::Str(v.into_iter().collect())
                }
                _ => {
                    let s = as_array_like(&args[0]).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘shuffle’ expects a string or collection (array/seq).",
                            sp.clone(),
                        )
                        .with_help("Pass a string to shuffle characters, or an array/seq to shuffle elements.")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;
                    let mut v = s.to_vec();
                    for i in 0..v.len() {
                        let j = i + rng_index(sess, v.len() - i);
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
                    let s = as_array_like(&args[0]).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘sort’ expects a string or collection (array/seq).",
                            sp.clone(),
                        )
                        .with_help("Pass a string to sort characters, or an array/seq to sort elements.")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;
                    let mut v = s.to_vec();
                    v.sort_by(|a, b| fmt_value_raw(a).cmp(&fmt_value_raw(b)));
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
                    let xs = as_array_like(&args[0]).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘freq’ expects a string or collection (array/seq).",
                            sp.clone(),
                        )
                        .with_help("Pass a string to count character frequency, or an array/seq to count element frequency.")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;
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
            let xs = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘mode’ expects an array/seq.",
                    sp.clone(),
                )
                .with_help("Pass an array or seq of values to compute the most frequent element.")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;

            if xs.is_empty() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::EMPTY_ARRAY, // R0404
                        "empty-array",
                        "mode of empty array",
                        sp.clone(),
                    )
                    .with_help("Provide at least one element.")
                    .with_link("https://goblinlang.org/docs/errors#R0404"),
                );
            }

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

            // ---- config map ----
            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "config-expected",
                            "‘sample_weighted’ expects a config object (map).",
                            sp.clone(),
                        )
                        .with_help("Pass a map like { src: [...], weights: [...], count: 3 }.")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            // ---- required: src ----
            let s = cfg.get("src").ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                    "missing-field",
                    "missing required field ‘src’.",
                    sp.clone(),
                )
                .with_help("Provide a collection in ‘src’, e.g. { src: [\"a\",\"b\"], weights: [1,2] }")
                .with_link("https://goblinlang.org/docs/errors#R0403")
            })?;

            let xs = as_array_like(s).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘src’ must be an array or seq.",
                    sp.clone(),
                )
                .with_help("Example: { src: [10, 20, 30], weights: [1, 2, 3] }")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;

            // ---- required: weights ----
            let wsv = cfg.get("weights").ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                    "missing-field",
                    "missing required field ‘weights’.",
                    sp.clone(),
                )
                .with_help("Provide an array/seq of numeric weights matching the length of ‘src’.")
                .with_link("https://goblinlang.org/docs/errors#R0403")
            })?;

            let ws = as_array_like(wsv).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘weights’ must be an array or seq.",
                    sp.clone(),
                )
                .with_help("Example: { src: [\"a\",\"b\"], weights: [0.3, 0.7] }")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;

            // ---- validation: non-empty and length match ----
            if xs.is_empty() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                        "empty-collection",
                        "cannot sample from an empty ‘src’.",
                        sp.clone(),
                    )
                    .with_help("Provide at least one element in ‘src’.")
                    .with_link("https://goblinlang.org/docs/errors#R0701"),
                );
            }
            if xs.len() != ws.len() {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WEIGHTS_LEN_MISMATCH, // R0708 (NEW)
                        "length-mismatch",
                        "‘weights’ length must match ‘src’.",
                        sp.clone(),
                    )
                    .with_help(&format!("src has {}, weights has {}.", xs.len(), ws.len()))
                    .with_help("Make both arrays the same length.")
                    .with_link("https://goblinlang.org/docs/errors#R0708"),
                );
            }

            // ---- optional: count (positive int) ----
            let n_out: usize = match cfg.get("count") {
                None => 1,
                Some(Value::Float(n)) if *n > 0.0 && n.fract() == 0.0 => *n as usize,
                Some(Value::Int(i)) if *i > 0 => *i as usize,
                Some(_) => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                            "positive-int-expected",
                            "‘sample_weighted.count’ must be a positive integer.",
                            sp.clone(),
                        )
                        .with_help("Use an integer ≥ 1, e.g. { count: 3 }")
                        .with_link("https://goblinlang.org/docs/errors#T0202"),
                    )
                }
            };

            // ---- build cumulative weights ----
            let mut cum = Vec::with_capacity(ws.len());
            let mut sum = 0.0;
            for w in ws {
                let w = want_num(w, "weights")?; // emits NUMERIC_EXPECTED (R0200) if not numeric
                if w < 0.0 {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                            "math-domain",
                            "weights must be ≥ 0.",
                            sp.clone(),
                        )
                        .with_help("Remove negative weights or clamp them to zero.")
                        .with_link("https://goblinlang.org/docs/errors#R0207"),
                    );
                }
                sum += w;
                cum.push(sum);
            }

            if sum == 0.0 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                        "math-domain",
                        "all weights are zero.",
                        sp.clone(),
                    )
                    .with_help("At least one weight must be > 0.")
                    .with_link("https://goblinlang.org/docs/errors#R0207"),
                );
            }

            // ---- sample with replacement (weighted) ----
            let mut out = Vec::with_capacity(n_out);
            for _ in 0..n_out {
                let r = rng_u01(sess) * sum; // in [0, sum)
                let mut lo = 0usize;
                let mut hi = cum.len();
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    if r < cum[mid] { hi = mid; } else { lo = mid + 1; }
                }
                out.push(xs[lo].clone());
            }

            Value::Array(out)
        }

        "map" => {
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘map’ takes exactly 2 arguments: (collection, actionName).")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let action = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "action-name-string-expected",
                            "‘map’ expects the action name as a string.",
                            sp.clone(),
                        )
                        .with_help(r#"Example: map(["a","b"], "upper")"#)
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            // --- String case: treat as array of runes/chars ---
            if let Value::Str(s) = &args[0] {
                let mut results: Vec<Value> = Vec::with_capacity(char_len(s));
                let mut any_non_text = false; // any non Char/Str

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

            // --- Array/Seq case (allow seq as well) ---
            let xs = match as_array_like(&args[0]) {
                Some(a) => a,
                None => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "collection-expected",
                            "‘map’ expects array/seq/string as first argument.",
                            sp.clone(),
                        )
                        .with_help(r#"Examples: map(["a","b"], "upper"), map(chars("abc"), "upper")"#)
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

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
                    let s = as_array_like(&args[0]).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘unique’ expects array/seq/string.",
                            sp.clone(),
                        )
                        .with_help("Pass a string or a collection (array/seq).")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;
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
                    let s = as_array_like(&args[0]).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘dups’ expects array/seq/string.",
                            sp.clone(),
                        )
                        .with_help("Pass a string or a collection (array/seq).")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;
                    use std::collections::BTreeMap;
                    let mut cnt = BTreeMap::<String, (usize, Value)>::new();
                    for v in s {
                        let k = fmt_value_raw(v);
                        cnt.entry(k).and_modify(|e| e.0 += 1).or_insert((1, v.clone()));
                    }
                    let mut out = Vec::new();
                    for (_, (n, exemplar)) in cnt {
                        if n >= 2 { out.push(exemplar); }
                    }
                    Value::Array(out)
                }
            }
        }

        // collections CRUD style
        // Inside the match name { ... } block in call_action_by_name, replace all these functions:

        "grab_first" => {
            arity(1)?;
            collection_operation(&args[0], Position::First, Operation::Grab, &sp, sess)?
        }

        "grab_last" => {
            arity(1)?;
            collection_operation(&args[0], Position::Last, Operation::Grab, &sp, sess)?
        }

        "grab_at" => {
            arity(2)?;
            collection_operation(&args[0], Position::At(args[1].clone()), Operation::Grab, &sp, sess)?
        }

        "grab_where" => {
            arity(2)?;
            let pred = want_str(&args[1], "grab_where predicate")?;
            collection_operation(&args[0], Position::Where(pred), Operation::Grab, &sp, sess)?
        }

        "grab_all" => {
            arity(1)?;
            collection_operation(&args[0], Position::All, Operation::Grab, &sp, sess)?
        }

        "put_first" => {
            arity(2)?;
            collection_operation(&args[0], Position::First, Operation::Put(args[1].clone()), &sp, sess)?
        }

        "put_last" => {
            arity(2)?;
            collection_operation(&args[0], Position::Last, Operation::Put(args[1].clone()), &sp, sess)?
        }

        "put_at" => {
            arity(3)?;
            collection_operation(&args[0], Position::At(args[1].clone()), Operation::Put(args[2].clone()), &sp, sess)?
        }

        "update_first" => {
            arity(2)?;
            collection_operation(&args[0], Position::First, Operation::Update(args[1].clone()), &sp, sess)?
        }

        "update_last" => {
            arity(2)?;
            collection_operation(&args[0], Position::Last, Operation::Update(args[1].clone()), &sp, sess)?
        }

        "update_at" => {
            arity(3)?;
            collection_operation(&args[0], Position::At(args[1].clone()), Operation::Update(args[2].clone()), &sp, sess)?
        }

        "update_where" => {
            arity(3)?;
            let pred = want_str(&args[1], "update_where predicate")?;
            collection_operation(&args[0], Position::Where(pred), Operation::Update(args[2].clone()), &sp, sess)?
        }

        "update_all" => {
            arity(2)?;
            collection_operation(&args[0], Position::All, Operation::Update(args[1].clone()), &sp, sess)?
        }

        "delete_first" => {
            arity(1)?;
            collection_operation(&args[0], Position::First, Operation::Delete, &sp, sess)?
        }

        "delete_last" => {
            arity(1)?;
            collection_operation(&args[0], Position::Last, Operation::Delete, &sp, sess)?
        }

        "delete_at" => {
            arity(2)?;
            collection_operation(&args[0], Position::At(args[1].clone()), Operation::Delete, &sp, sess)?
        }

        "delete_where" => {
            arity(2)?;
            let pred = want_str(&args[1], "delete_where predicate")?;
            collection_operation(&args[0], Position::Where(pred), Operation::Delete, &sp, sess)?
        }

        "delete_all" => {
            arity(1)?;
            collection_operation(&args[0], Position::All, Operation::Delete, &sp, sess)?
        }

        // ===== File IO Stuff =====
        "file_exists" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘file_exists’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let path = want_str(&args[0], "file_exists")?;
            Value::Bool(std::path::Path::new(&path).exists())
        }

        // ===== Replace & remove =====
        "reap" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘reap’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let cfg = match &args[0] {
                Value::Map(m) => m,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "config-expected",
                            "‘reap’ expects a config object (map).",
                            sp.clone(),
                        )
                        .with_help("Pass a map like { src: [...], count: 3 }. ")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            };

            let srcv = cfg.get("src").ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                    "missing-field",
                    "missing required field ‘src’.",
                    sp.clone(),
                )
                .with_help("Provide a collection in ‘src’, e.g. { src: [1,2,3], count: 2 }")
                .with_link("https://goblinlang.org/docs/errors#R0403")
            })?;

            let n_out: usize = match cfg.get("count") {
                None => 1,
                Some(Value::Int(n)) if *n > 0 => *n as usize,
                Some(_) => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                            "positive-int-expected",
                            "reap ‘count’ must be a positive integer.",
                            sp.clone(),
                        )
                        .with_help("Use an integer ≥ 1, e.g. { count: 3 }")
                        .with_link("https://goblinlang.org/docs/errors#T0202"),
                    )
                }
            };

            match srcv {
                Value::Array(_) | Value::Seq(_) => {
                    let s: &[Value] = as_array_like(srcv).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "reap: ‘src’ must be an array/seq/map.",
                            sp.clone(),
                        )
                        .with_help("Example: { src: [10,20,30], count: 2 }")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    })?;

                    if s.is_empty() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                                "empty-collection",
                                "cannot reap from an empty collection.",
                                sp.clone(),
                            )
                            .with_help("Provide at least one element in ‘src’.")
                            .with_link("https://goblinlang.org/docs/errors#R0701"),
                        );
                    }
                    if n_out > s.len() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, // R0704
                                "sample-too-large",
                                &format!("requested {n_out}, but ‘src’ has only {} element(s).", s.len()),
                                sp.clone(),
                            )
                            .with_help("Decrease ‘count’ or increase the size of ‘src’.")
                            .with_link("https://goblinlang.org/docs/errors#R0704"),
                        );
                    }

                    let len = s.len();
                    let mut idxs: Vec<usize> = (0..len).collect();
                    for i in 0..n_out {
                        let j = i + rng_index(sess, len - i);
                        idxs.swap(i, j);
                    }

                    let mut items: Vec<Value> = Vec::with_capacity(n_out);
                    for &i in &idxs[..n_out] {
                        items.push(s[i].clone());
                    }

                    if n_out == 1 {
                        items.pop().unwrap()
                    } else {
                        Value::Array(items)
                    }
                }

                Value::Map(map) => {
                    if map.is_empty() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                                "empty-collection",
                                "cannot reap from an empty map.",
                                sp.clone(),
                            )
                            .with_help("Provide at least one entry in ‘src’.")
                            .with_link("https://goblinlang.org/docs/errors#R0701"),
                        );
                    }

                    let entries: Vec<(String, Value)> =
                        map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

                    if n_out > entries.len() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, // R0704
                                "sample-too-large",
                                &format!(
                                    "requested {n_out}, but ‘src’ has only {} entry(ies).",
                                    entries.len()
                                ),
                                sp.clone(),
                            )
                            .with_help("Decrease ‘count’ or increase the number of entries in ‘src’.")
                            .with_link("https://goblinlang.org/docs/errors#R0704"),
                        );
                    }

                    let mut idxs: Vec<usize> = (0..entries.len()).collect();
                    for i in 0..n_out {
                        let j = i + rng_index(sess, entries.len() - i);
                        idxs.swap(i, j);
                    }

                    let mut items: Vec<Value> = Vec::with_capacity(n_out);
                    for &i in &idxs[..n_out] {
                        let mut pair = BTreeMap::new();
                        pair.insert(entries[i].0.clone(), entries[i].1.clone());
                        items.push(Value::Map(pair));
                    }

                    if n_out == 1 {
                        items.pop().unwrap()
                    } else {
                        Value::Array(items)
                    }
                }

                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "reap: ‘src’ must be an array/seq/map.",
                            sp.clone(),
                        )
                        .with_help("Example: { src: [1,2,3], count: 2 } or { src: {a:1,b:2}, count: 1 }")
                        .with_link("https://goblinlang.org/docs/errors#T0205"),
                    )
                }
            }
        }

        "replace" => {
            if args.len() != 3 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 3, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘replace’ takes exactly 3 arguments.")
                    .with_help("Usage: replace(string, from, to)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str(...) already emits T0205 with links if the type is wrong.
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
            // Arity check
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘before’ takes exactly 2 arguments.")
                    .with_help("Usage: before(string, separator)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str(...) emits T0205 with proper messaging/links if type is wrong.
            let s   = want_str(&args[0], "before")?;
            let sep = want_str(&args[1], "before")?;

            match s.find(&sep) {
                Some(i) => Value::Str(s[..i].to_string()),
                None    => Value::Str(s),
            }
        }

        "after" => {
            // Arity check
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘after’ takes exactly 2 arguments.")
                    .with_help("Usage: after(string, separator)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str(...) emits T0205 with proper messaging/links if type is wrong.
            let s   = want_str(&args[0], "after")?;
            let sep = want_str(&args[1], "after")?;

            match s.find(&sep) {
                Some(i) => Value::Str(s[i + sep.len()..].to_string()),
                None    => Value::Str(String::new()),
            }
        }

        "before_last" => {
            // Arity check
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘before_last’ takes exactly 2 arguments.")
                    .with_help("Usage: before_last(string, separator)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str(...) will emit T0205 with proper message/links if types are wrong.
            let s   = want_str(&args[0], "before_last")?;
            let sep = want_str(&args[1], "before_last")?;

            match s.rfind(&sep) {
                Some(i) => Value::Str(s[..i].to_string()),
                None    => Value::Str(s),
            }
        }

        "after_last" => {
            // Arity check
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘after_last’ takes exactly 2 arguments.")
                    .with_help("Usage: after_last(string, separator)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str(...) enforces T0205 type checks with proper messaging/links.
            let s   = want_str(&args[0], "after_last")?;
            let sep = want_str(&args[1], "after_last")?;

            match s.rfind(&sep) {
                Some(i) => Value::Str(s[i + sep.len()..].to_string()),
                None    => Value::Str(String::new()),
            }
        }

        "between" => { // first left … right after that
            // Arity check
            if args.len() != 3 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 3, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘between’ takes exactly 3 arguments.")
                    .with_help("Usage: between(string, left, right)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str enforces T0205 with proper messaging/links
            let s     = want_str(&args[0], "between")?;
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
            // Arity check
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘lines’ takes exactly 1 argument.")
                    .with_help("Usage: lines(string)")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            // want_str enforces T0205 with proper messaging/links
            let s = want_str(&args[0], "lines")?;
            Value::Array(s.split('\n').map(|t| Value::Str(t.to_string())).collect())
        }

        "words" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘words’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let s = want_str(&args[0], "words")?;
            Value::Array(s.split_whitespace().map(|t| Value::Str(t.to_string())).collect())
        }

        "chars" => {
            // chars(string) -> Array<Char>
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘chars’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let s = want_str(&args[0], "chars")?;
            Value::Array(s.chars().map(Value::Char).collect())
        }

        "split" => { // split by separator (string)
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘split’ takes exactly 2 arguments: split(string, separator).")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }
            let s   = want_str(&args[0], "split")?;
            let sep = want_str(&args[1], "split")?;
            if sep.is_empty() {
                Value::Array(s.chars().map(|c| Value::Str(c.to_string())).collect())
            } else {
                Value::Array(s.split(&sep).map(|t| Value::Str(t.to_string())).collect())
            }
        }

        "join" => {
            if args.len() != 2 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘join’ takes exactly 2 arguments: join(array|seq, separator).")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let xs = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘join’ expects the first argument to be an array or seq.",
                    sp.clone(),
                )
                .with_help("Example: join([\"a\",\"b\",\"c\"], \",\")  -> \"a,b,c\"")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;

            let sep = want_str(&args[1], "join")?;
            let mut out = String::new();
            for (i, v) in xs.iter().enumerate() {
                let piece = match v {
                    Value::Str(s)  => s.clone(),
                    Value::Char(c) => c.to_string(),
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "‘join’ expects an array/seq of strings or chars.",
                                sp.clone(),
                            )
                            .with_help("Ensure every element is a string or char.")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        );
                    }
                };
                if i > 0 { out.push_str(&sep); }
                out.push_str(&piece);
            }
            Value::Str(out)
        }

        // ===== Other transforms =====
        "reverse" => {
            // reverse the ORDER of a collection (pure)
            arity(1)?; // emits R0301 on arity mismatch

            let xs = as_array_like(&args[0]).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                    "type-mismatch",
                    "‘reverse’ expects an array or seq.",
                    sp.clone(),
                )
                .with_help("Pass a collection like [1,2,3] or a seq.")
                .with_link("https://goblinlang.org/docs/errors#T0205")
            })?;

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
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘parse_bool’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let s = want_str(&args[0], "parse_bool")?; // emits T0205 on non-string

            match s.to_ascii_lowercase().as_str() {
                "true"  => Value::Bool(true),
                "false" => Value::Bool(false),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘parse_bool’ expects the string \"true\" or \"false\" (case-insensitive).",
                            sp.clone(),
                        )
                        .with_help("Use \"true\" or \"false\".")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    );
                }
            }
        }

        "len" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘len’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            match &args[0] {
                Value::Str(s)    => Value::Int(s.chars().count() as i64), // Unicode scalar count
                Value::Array(xs) => Value::Int(xs.len() as i64),
                Value::Seq(xs)   => Value::Int(xs.len() as i64),
                Value::Map(m)    => Value::Int(m.len() as i64),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘len’ expects a string or collection (array/seq/map).",
                            sp.clone(),
                        )
                        .with_help("Pass a string, array, seq, or map to ‘len’.")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    );
                }
            }
        },

        "backend" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘backend’ takes exactly 1 receiver.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            match &args[0] {
                Value::Seq(xs)  => Value::Str(xs.backend_name().into()),
                Value::Array(_) => Value::Str("array(legacy)".into()),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘backend’ expects a collection (array/seq).",
                            sp.clone(),
                        )
                        .with_help("Call like: backend([1,2,3]) or backend(my_seq).")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    );
                }
            }
        }

        "metrics" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘metrics’ takes exactly 1 receiver.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            match &args[0] {
                Value::Seq(xs) => Value::Map(xs.metrics_map()),
                Value::Array(xs) => {
                    // legacy metrics for plain arrays
                    let mut m = BTreeMap::new();
                    m.insert("len".into(), Value::Int(xs.len() as i64));
                    m.insert("backend".into(), Value::Str("array(legacy)".into()));
                    Value::Map(m)
                }
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                            "type-mismatch",
                            "‘metrics’ expects a collection (array/seq).",
                            sp.clone(),
                        )
                        .with_help("Call like: metrics([1,2,3]) or metrics(my_seq).")
                        .with_link("https://goblinlang.org/docs/errors#T0205")
                    );
                }
            }
        }

        // ----- JSON -----
        "json_parse" => {
            // arity(1)?;  --> expand for uniform diagnostics
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘json_parse’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let v0 = args[0].clone();
            let s  = want_str(&v0, "json_parse")?; // emits T0205 with link

            let vj: sj::Value = sj::from_str(&s).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_PARSE_FAILED, // J0001 (NEW)
                    "json-parse-failed",
                    &format!("JSON parse failed: {e}"),
                    sp.clone(),
                )
                .with_help("Ensure the input is valid JSON text.")
                .with_link("https://goblinlang.org/docs/errors#J0001")
            })?;

            from_json(&vj)
        }

        "json_stringify" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘json_stringify’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let v0 = args[0].clone();
            let s = sj::to_string(&to_json(&v0)).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_STRINGIFY_FAILED, // J0002 (NEW)
                    "json-stringify-failed",
                    &format!("JSON stringify failed: {e}"),
                    sp.clone(),
                )
                .with_help("Remove non-serializable values or convert them to JSON-friendly forms.")
                .with_link("https://goblinlang.org/docs/errors#J0002")
            })?;

            Value::Str(s)
        }

        "json_stringify_pretty" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘json_stringify_pretty’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let v0 = args[0].clone();
            let s = sj::to_string_pretty(&to_json(&v0)).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_STRINGIFY_FAILED, // J0002 (NEW)
                    "json-stringify-failed",
                    &format!("JSON stringify failed: {e}"),
                    sp.clone(),
                )
                .with_help("Remove non-serializable values or convert them to JSON-friendly forms.")
                .with_link("https://goblinlang.org/docs/errors#J0002")
            })?;

            Value::Str(s)
        }

        "read_json" => {
            if args.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                        sp.clone(),
                    )
                    .with_help("‘read_json’ takes exactly 1 argument: a file path.")
                    .with_link("https://goblinlang.org/docs/errors#R0301")
                );
            }

            let vpath = args[0].clone();
            let path  = want_str(&vpath, "read_json path")?; // emits T0205 with link

            let txt = std::fs::read_to_string(&path).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_IO, // J0003 (NEW)
                    "json-io",
                    &format!("read_json: {e}"),
                    sp.clone(),
                )
                .with_help("Verify the file exists and is readable.")
                .with_link("https://goblinlang.org/docs/errors#J0003")
            })?;

            let vj: sj::Value = sj::from_str(&txt).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_PARSE_FAILED, // J0001 (NEW)
                    "json-parse-failed",
                    &format!("JSON parse failed: {e}"),
                    sp.clone(),
                )
                .with_help("Ensure the file contains valid JSON.")
                .with_link("https://goblinlang.org/docs/errors#J0001")
            })?;

            from_json(&vj)
        }

        other => {
            if let Some(v) = eval_builtin(other, &args, sess, &sp)? {
                return Ok(v);
            }
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::UNKNOWN_ACTION, // A0401 (NEW)
                    "unknown-action",
                    &format!("unknown action ‘{}’", other),
                    sp.clone(),
                )
                .with_help("Check the action name or import the module that provides it.")
                .with_link("https://goblinlang.org/docs/errors#A0401")
            );
        },
    };

    Ok(out)
}

// ===================== Evaluation =====================

/// Represents a parsed lvalue path for mutation
#[derive(Debug, Clone)]
enum LValuePath {
    /// Simple variable: `items`
    Var(String),
    /// Field access: `building >> items`
    Field {
        base: Box<LValuePath>,
        field: String,
    },
    /// Index access: `buildings[0]`
    Index {
        base: Box<LValuePath>,
        index: Value,
    },
}

/// Parse an expression into an lvalue path for mutation
fn parse_lvalue(expr: &ast::Expr, sess: &mut Session) -> Result<LValuePath, Diag> {
    match expr {
        // Simple identifier
        ast::Expr::Ident(name, _) => Ok(LValuePath::Var(name.clone())),
        
        // Field access via >> (parsed as Member)
        ast::Expr::Member(obj_expr, field_name, _) => {
            let base = parse_lvalue(obj_expr, sess)?;
            Ok(LValuePath::Field {
                base: Box::new(base),
                field: field_name.clone(),
            })
        }
        
        // Field access: obj >> field (Binary form - keep this for compatibility)
        ast::Expr::Binary(obj_expr, op, field_expr, sp) if op == ">>" => {
            let base = parse_lvalue(obj_expr, sess)?;
            let field_name = match &**field_expr {
                ast::Expr::Ident(n, _) => n.clone(),
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::FIELD_NAME_REQUIRED, // P0804
                            "field-name-required",
                            "field name required after ‘>>’.",
                            sp.clone(),
                        )
                        .with_help("Use an identifier immediately after ‘>>’, e.g. `>> name`.")
                        .with_link("https://goblinlang.org/docs/errors#P0804"),
                    );
                }
            };
            Ok(LValuePath::Field {
                base: Box::new(base),
                field: field_name,
            })
        }
        
        // Index access: obj[index]
        ast::Expr::Index(base_expr, index_expr, _) => {
            let base = parse_lvalue(base_expr, sess)?;
            let index_val = eval_expr(index_expr, sess)?;
            Ok(LValuePath::Index {
                base: Box::new(base),
                index: index_val,
            })
        }
        
        _ => {
            eprintln!("DEBUG: Unhandled expression type in parse_lvalue");
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::LVALUE_EXPECTED, // P0802
                    "lvalue-expected",
                    "expected a variable name or field access",
                    expr.span().clone(),
                )
                .with_help("Use an identifier (e.g., foo) or a field access (foo.bar) as the target.")
                .with_link("https://goblinlang.org/docs/errors#P0802"),
            );
        }
    }
}

/// Get a mutable reference to the value at the end of an lvalue path
fn get_lvalue_mut<'a>(
    path: &LValuePath,
    sess: &'a mut Session,
    sp: &Span,
) -> Result<&'a mut Value, Diag> {
    match path {
        LValuePath::Var(name) => {
            sess.get_var_mut(name).ok_or_else(|| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                    "unknown-ident",
                    &format!("unknown identifier ‘{}’", name),
                    sp.clone(),
                )
                .with_help("Declare/bind the variable before using it, or check for a typo.")
                .with_link("https://goblinlang.org/docs/errors#R0101")
            })
        }
        
        LValuePath::Field { base, field } => {
            let base_val = get_lvalue_mut(base, sess, sp)?;
            
            match base_val {
                Value::Object { fields, readonly_fields, .. } => {
                    if readonly_fields.contains(field) {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::READONLY_FIELD, // P9001
                                "readonly-field",
                                &format!("cannot modify readonly field ‘{}’", field),
                                sp.clone(),
                            )
                            .with_help("This field is immutable. Remove the mutation or write to a different, mutable field.")
                            .with_link("https://goblinlang.org/docs/errors#P9001"),
                        );
                    }

                    fields.get_mut(field).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                            "no-such-field",
                            &format!("no field ‘{}’", field),
                            sp.clone(),
                        )
                        .with_help("Check the field name or add it to the object.")
                        .with_link("https://goblinlang.org/docs/errors#R0403")
                    })
                }
                
                Value::Map(map) => {
                    map.get_mut(field).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                            "missing-key",
                            &format!("missing key ‘{}’", field),
                            sp.clone(),
                        )
                        .with_help("Insert the key first or guard for its absence.")
                        .with_link("https://goblinlang.org/docs/errors#R0403")
                    })
                }
                
                Value::Enum { fields: Some(field_map), variant_name, .. } => {
                    field_map.get_mut(field).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                            "no-such-field",
                            &format!("variant ‘{}’ has no field ‘{}’", variant_name, field),
                            sp.clone(),
                        )
                        .with_help("Use a field that exists on this variant or adjust the variant.")
                        .with_link("https://goblinlang.org/docs/errors#R0403")
                    })
                }
                
                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "member access requires a map, object, or enum",
                        sp.clone(),
                    )
                    .with_help("Use a Map/Object/Enum value before accessing a field.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
            }
        }
        
        LValuePath::Index { base, index } => {
            let base_val = get_lvalue_mut(base, sess, sp)?;
            
            let idx = match index {
                Value::Int(n) if *n >= 0 => *n as usize,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204
                            "integer-expected",
                            "index must be a non-negative integer",
                            sp.clone(),
                        )
                        .with_help("Use an integer ≥ 0 (e.g., 0, 1, 2, ...).")
                        .with_link("https://goblinlang.org/docs/errors#T0204"),
                    )
                }
            };
            
            match base_val {
                Value::Array(arr) => {
                    if idx >= arr.len() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::INVALID_INDEX, // R0401
                                "index-out-of-bounds",
                                &format!("index {} is out of bounds (len = {})", idx, arr.len()),
                                sp.clone(),
                            )
                            .with_help(&format!(
                                "Use an index in the range 0..{}.",
                                arr.len().saturating_sub(1)
                            ))
                            .with_link("https://goblinlang.org/docs/errors#R0401"),
                        );
                    }
                    Ok(&mut arr[idx])
                }

                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "index access requires an array or seq",
                        sp.clone(),
                    )
                    .with_help("Provide an array/seq as the receiver, e.g., xs[0].")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
            }
        }
    }
}

/// Evaluate an lvalue path to get its current value (for building argv)
fn eval_lvalue(path: &LValuePath, sess: &mut Session, sp: &Span) -> Result<Value, Diag> {
    match path {
        LValuePath::Var(name) => {
            sess.get_var(name)
                .cloned()
                .ok_or_else(|| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                        "unknown-ident",
                        &format!("unknown identifier ‘{}’", name),
                        sp.clone(),
                    )
                    .with_help("Declare the variable before using it (e.g., let x = ...).")
                    .with_link("https://goblinlang.org/docs/errors#R0101")
                })
        }
        
        LValuePath::Field { base, field } => {
            let base_val = eval_lvalue(base, sess, sp)?;
            
            match base_val {
                Value::Object { fields, .. } => {
                    fields
                        .get(field)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                "no-such-field",
                                &format!("no field ‘{}’", field),
                                sp.clone(),
                            )
                            .with_help("Check the field name or ensure it exists on this object.")
                            .with_link("https://goblinlang.org/docs/errors#R0403")
                        })
                }
                
                Value::Map(map) => {
                    map.get(field)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                "missing-key",
                                &format!("missing key ‘{}’", field),
                                sp.clone(),
                            )
                            .with_help("Check the key name or ensure it exists in this map.")
                            .with_link("https://goblinlang.org/docs/errors#R0403")
                        })
                }

                Value::Enum { fields: Some(field_map), variant_name, .. } => {
                    field_map.get(field)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                "no-such-field",
                                &format!("variant ‘{}’ has no field ‘{}’", variant_name, field),
                                sp.clone(),
                            )
                            .with_help("Verify the field is declared on this enum variant.")
                            .with_link("https://goblinlang.org/docs/errors#R0403")
                        })
                }
                
                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "member access requires a map, object, or enum",
                        sp.clone(),
                    )
                    .with_help("Use member access ('.' or '[]') only on map/object/enum values.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
            }
        }
        
        LValuePath::Index { base, index } => {
            let base_val = eval_lvalue(base, sess, sp)?;

            // ---- index must be non-negative integer ----
            let idx = match index {
                Value::Int(n) if *n >= 0 => *n as usize,
                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204
                            "integer-expected",
                            "index must be a non-negative integer",
                            sp.clone(),
                        )
                        .with_help("Use an integer ≥ 0 (e.g., 0 for the first element).")
                        .with_link("https://goblinlang.org/docs/errors#T0204"),
                    )
                }
            };

            match base_val {
                Value::Array(arr) => {
                    let len = arr.len();
                    if idx >= len {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::INVALID_INDEX, // R0401
                                "index-out-of-bounds",
                                "index out of bounds",
                                sp.clone(),
                            )
                            .with_help(&format!(
                                "Valid index range for this array is 0..{}.",
                                len.saturating_sub(1)
                            ))
                            .with_link("https://goblinlang.org/docs/errors#R0401"),
                        );
                    }
                    Ok(arr[idx].clone())
                }

                Value::Str(s) => {
                    let chars: Vec<char> = s.chars().collect();
                    let len = chars.len();
                    if idx >= len {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::INVALID_INDEX, // R0401
                                "index-out-of-bounds",
                                "index out of bounds",
                                sp.clone(),
                            )
                            .with_help(&format!(
                                "Valid index range for this string is 0..{} (by Unicode scalar).",
                                len.saturating_sub(1)
                            ))
                            .with_link("https://goblinlang.org/docs/errors#R0401"),
                        );
                    }
                    Ok(Value::Char(chars[idx]))
                }

                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "index access requires an array, seq, or string",
                        sp.clone(),
                    )
                    .with_help("Use indexing only on array/seq/string values.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
            }
        }
    }
}

fn mutate_via_call_name(
    sess: &mut Session,
    name: &str,        // "put_at!"
    recv_ident: Option<&str>,
    arg_exprs: &[ast::Expr],
    sp: Span,
) -> Result<Value, Diag> {
    let base = name
        .strip_suffix('!')
        .ok_or_else(|| {
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::INTERNAL, // R0000
                "internal-assertion",
                "expected bang name (…!)",
                sp.clone(),
            )
            .with_help("This indicates an internal invariant violation; a ‘bang’ (!) suffix was required here.")
            .with_link("https://goblinlang.org/docs/errors#R0000")
        })?;

    // Special cases that don't follow the lvalue pattern
    match base {
        "write_json" => {
            // write_json!(path, value, pretty=false)
            if arg_exprs.len() < 2 || arg_exprs.len() > 3 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!(
                            "Wrong number of arguments (expected 2 or 3, got {})",
                            arg_exprs.len()
                        ),
                        sp.clone(),
                    )
                    .with_help("Usage: write_json!(path, value[, pretty])")
                    .with_help("Provide exactly 2 or 3 arguments.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let vpath  = eval_expr(&arg_exprs[0], sess)?;
            let vval   = eval_expr(&arg_exprs[1], sess)?;
            let pretty = if arg_exprs.len() == 3 {
                let vpretty = eval_expr(&arg_exprs[2], sess)?;
                as_bool(vpretty, sp.clone(), "write_json! pretty")?
            } else {
                false
            };

            // Serialize JSON
            let j = to_json(&vval);
            let out = (if pretty { sj::to_string_pretty(&j) } else { sj::to_string(&j) })
                .map_err(|e| {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::JSON_STRINGIFY_FAILED, // J0002
                        "json-stringify-failed",
                        &format!("JSON stringify failed: {e}"),
                        sp.clone(),
                    )
                    .with_help("Ensure the value can be represented in JSON.")
                    .with_link("https://goblinlang.org/docs/errors#J0002")
                })?;

            // Path must be a string
            let path = want_str(&vpath, "write_json! path", sp.clone())?;

            // Write file
            std::fs::write(&path, out).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::JSON_WRITE_IO, // J0004 (new)
                    "json-write-io",
                    &format!("write_json! failed to write file: {e}"),
                    sp.clone(),
                )
                .with_help("Check that the directory exists and you have write permissions.")
                .with_link("https://goblinlang.org/docs/errors#J0004")
            })?;

            return Ok(Value::Unit)
        }
        
        "create_dir" => {
            // create_dir!(path)
            if arg_exprs.len() != 1 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                        "wrong-arity",
                        &format!("Wrong number of arguments (expected 1, got {})", arg_exprs.len()),
                        sp.clone(),
                    )
                    .with_help("Usage: create_dir!(path)")
                    .with_help("‘create_dir’ takes exactly 1 argument.")
                    .with_link("https://goblinlang.org/docs/errors#R0301"),
                );
            }

            let vpath = eval_expr(&arg_exprs[0], sess)?;
            let path = want_str(&vpath, "create_dir! path", sp.clone())?;

            std::fs::create_dir_all(&path).map_err(|e| {
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::FILESYSTEM_IO, // FS0001 (NEW)
                    "filesystem-io",
                    &format!("failed to create directory: {e}"),
                    sp.clone(),
                )
                .with_help(&format!("Check permissions and that ‘{}’ is not an existing file.", path))
                .with_help("Create parent directories or use an absolute path if needed.")
                .with_link("https://goblinlang.org/docs/errors#FS0001")
            })?;

            return Ok(Value::Unit)
        }
        
        _ => {} // Fall through to normal lvalue-based mutations
    }

    // Determine the target lvalue path and build argv for the pure version
    let (target_path, argv_vals): (LValuePath, Vec<Value>) = if let Some(base_ident) = recv_ident {
        // Receiver style: xs.put_at!(...)
        let mut vals = Vec::with_capacity(arg_exprs.len() + 1);
        
        // Get receiver value
        let recv_val = sess
            .get_var(base_ident)
            .cloned()
            .ok_or_else(|| {
                if base_ident == "from" {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                        "unknown-ident",
                        "unknown identifier ‘from’",
                        sp.clone(),
                    )
                    .with_help("`from` was parsed as an identifier here.")
                    .with_help("After `pick`, either provide a count (e.g., `pick 1 from items`) or enable the sugar so `pick from items` defaults to 1.")
                    .with_help("Declare the variable before use, or reference an in-scope name.")
                    .with_link("https://goblinlang.org/docs/errors#R0101")
                } else {
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                        "unknown-ident",
                        &format!("unknown identifier ‘{}’", base_ident),
                        sp.clone(),
                    )
                    .with_help("Declare the variable before use, or reference an in-scope name.")
                    .with_link("https://goblinlang.org/docs/errors#R0101")
                }
            })?;

        vals.push(recv_val);
        
        // Evaluate remaining arguments
        for a in arg_exprs { 
            vals.push(eval_expr(a, sess)?); 
        }
        
        (LValuePath::Var(base_ident.to_string()), vals)
    } else {
        // Free-call style: put_at!(xs, ...) or put_at!(obj >> field, ...)
        if arg_exprs.is_empty() {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                    "missing-argument",
                    &format!("‘{}’ requires a target variable as the first argument.", name),
                    sp.clone(),
                )
                .with_help("Provide a variable name as the first argument.")
                .with_link("https://goblinlang.org/docs/errors#R0302"),
            );
        }
        
        // Parse the first argument as an lvalue path
        let target_path = parse_lvalue(&arg_exprs[0], sess)?;
        
        // Build argv: first element is the current value at the lvalue
        let mut vals = Vec::with_capacity(arg_exprs.len());
        vals.push(eval_lvalue(&target_path, sess, &sp)?);
        
        // Evaluate remaining arguments
        for a in &arg_exprs[1..] { 
            vals.push(eval_expr(a, sess)?); 
        }
        
        (target_path, vals)
    };

    // Special-case destructive reap!: mutate target and return removed value(s)
    if base == "reap" {
        // Parse count (default 1)
        let count: usize = if argv_vals.len() > 1 {
            let n = as_num(argv_vals[1].clone(), sp.clone(), "reap!(..., count)")?;
            if n <= 0.0 || n.fract() != 0.0 {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                        "positive-int-expected",
                        "‘reap!.count’ must be a positive integer.",
                        sp.clone(),
                    )
                    .with_help("Use an integer ≥ 1, e.g. { count: 3 }.") 
                    .with_link("https://goblinlang.org/docs/errors#T0202"),
                );
            }
            n as usize
        } else { 1 };

        // Get length and type
        enum CollKind { Arr(usize), Seq(usize), Str(usize) }
        let kind_len = match &argv_vals[0] {
            Value::Array(xs) => {
                if xs.is_empty() {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                            "empty-collection",
                            "cannot reap from an empty collection",
                            sp.clone(),
                        )
                        .with_help("Provide at least one element in ‘src’.")
                        .with_link("https://goblinlang.org/docs/errors#R0701"),
                    );
                }
                CollKind::Arr(xs.len())
            }
            Value::Seq(xs) => {
                let len = xs.len();
                if len == 0 {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                            "empty-collection",
                            "cannot reap from an empty collection",
                            sp.clone(),
                        )
                        .with_help("Provide at least one element in ‘src’.")
                        .with_link("https://goblinlang.org/docs/errors#R0701"),
                    );
                }
                CollKind::Seq(len)
            }
            Value::Str(s) => {
                let n = s.chars().count();
                if n == 0 {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::EMPTY_COLLECTION, // R0701
                            "empty-collection",
                            "cannot reap from an empty string",
                            sp.clone(),
                        )
                        .with_help("Provide at least one character in the string.")
                        .with_link("https://goblinlang.org/docs/errors#R0701"),
                    );
                }
                CollKind::Str(n)
            }
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "reap! expects an array/seq/string variable",
                        sp.clone(),
                    )
                    .with_help("Pass a variable bound to an array, seq, or string.")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            }
        };

        let len = match kind_len { CollKind::Arr(n)|CollKind::Seq(n)|CollKind::Str(n) => n };
        if count > len {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::SAMPLE_TOO_LARGE, // R0704
                    "sample-too-large",
                    &format!("not enough to sample: requested {}, have {}", count, len),
                    sp.clone(),
                )
                .with_help("Reduce ‘count’ or provide a larger source collection.")
                .with_link("https://goblinlang.org/docs/errors#R0704"),
            );
        }

        // Sample indices
        fn sample_indices(sess: &mut Session, len: usize, k: usize) -> Vec<usize> {
            let mut idxs: Vec<usize> = (0..len).collect();
            for i in 0..k {
                let j = i + rng_bounded(sess, (len - i) as u64) as usize;
                idxs.swap(i, j);
            }
            idxs[..k].to_vec()
        }
        let picks = sample_indices(sess, len, count);

        // Get mutable reference to the target
        let slot = get_lvalue_mut(&target_path, sess, &sp)?;

        let finish_vals = |mut items: Vec<Value>| -> Value {
            if items.len() == 1 { items.pop().unwrap() } else { Value::Array(items) }
        };

        match slot {
            Value::Array(vecd) => {
                let mut removed: Vec<Value> = Vec::with_capacity(count);
                let mut sorted = picks.clone();
                sorted.sort_unstable_by(|a,b| b.cmp(a));
                for i in sorted { removed.push(vecd.remove(i)); }
                removed.reverse();
                return Ok(finish_vals(removed));
            }
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
            Value::Str(s) => {
                let original = s.clone();
                let mut removed_s = String::new();
                for idx in &picks {
                    if let Some(ch) = slice_char(&original, *idx) {
                        removed_s.push_str(&ch);
                    }
                }
                let mut sorted = picks.clone();
                sorted.sort_unstable_by(|a,b| b.cmp(a));
                for idx in sorted {
                    if let Some(new_s) = str_delete_at(s, idx) { *s = new_s; }
                }
                return Ok(Value::Str(removed_s));
            }
            _ => {
                return Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "‘reap!’ expects an array/seq/string variable.",
                        sp.clone(),
                    )
                    .with_help("Pass a variable bound to an array, seq, or string (e.g., let xs = [1,2,3]; reap!(xs, { count: 2 })).")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                );
            }
        }
    }

    // Call the pure version to compute the updated value
    let updated = call_action_by_name(sess, base, argv_vals, sp.clone())?;

    // Write back to the target lvalue
    let slot = get_lvalue_mut(&target_path, sess, &sp)?;
    *slot = updated;
    
    Ok(Value::Unit)
}

fn eval_expr(e: &ast::Expr, sess: &mut Session) -> Result<Value, Diag> {

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
                    let d = cleaned.parse::<Decimal>().map_err(|_| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INVALID_NUMBER_LITERAL, // P0330
                            "invalid-number-literal",
                            &format!("invalid number literal ‘{}’", raw),
                            sp.clone(),
                        )
                        .with_help("Use a valid numeric literal (e.g., 42, 3.14, 1_000).")
                        .with_link("https://goblinlang.org/docs/errors#P0330")
                    })?;
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
                Some(v) => {
                    Ok(v.clone())
                }
                None => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                            "unknown-ident",
                            &format!("unknown identifier ‘{}’", name),
                            sp.clone(),
                        )
                        .with_help("Declare the variable before use or check the spelling.")
                        .with_link("https://goblinlang.org/docs/errors#R0101"),
                    );
                }
            }
        }
        ast::Expr::Block { stmts, .. } => {
            // Execute each statement in sequence
            let mut last_value = Value::Nil;
            for stmt in stmts {
                last_value = eval_expr(stmt, sess)?;
            }
            // Return the value of the last statement
            Ok(last_value)
        }

        ast::Expr::NsCall(ns, name, args, sp) => {
            // Check if it's a module namespace first
            if let Some(exported) = sess.modules.get_export(ns, name) {
                match exported {
                    crate::modules::ExportedItem::Action(action_decl) => {
                        // Clone the action declaration to avoid holding a borrow
                        let action_decl = action_decl.clone();
                        
                        // Evaluate arguments
                        let mut arg_vals = Vec::with_capacity(args.len());
                        for a in args {
                            arg_vals.push(eval_expr(a, sess)?);
                        }
                        
                        // Push a new scope for the action's parameters
                        sess.env.push(BTreeMap::new());
                        sess.consts.push(BTreeMap::new());
                        
                        // Set current module so the function can access module vars
                        let old_module = sess.current_module.clone();
                        sess.current_module = Some(ns.to_string());
                        
                        // Bind parameters
                        for (i, param) in action_decl.params.iter().enumerate() {
                            let val = if i < arg_vals.len() {
                                arg_vals[i].clone()
                            } else if let Some(ref default_expr) = param.default {
                                eval_expr(default_expr, sess)?
                            } else {
                                sess.current_module = old_module;
                                sess.env.pop();
                                sess.consts.pop();
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                                        "missing-argument",
                                        &format!("missing argument for parameter ‘{}’", param.name),
                                        sp.clone(),
                                    )
                                    .with_help("Provide a value for this parameter or define a default.")
                                    .with_link("https://goblinlang.org/docs/errors#R0302"),
                                );
                            };
                            sess.define_local(param.name.clone(), val, false);
                        }
                        
                        // Execute the action body
                        let result = match &action_decl.body {
                            ast::ActionBody::Block(stmts) => {
                                let mut last_val = Value::Unit;
                                for stmt in stmts {
                                    if let Some(v) = eval_stmt(stmt, sess)? {
                                        match v {
                                            Value::CtrlSkip => { /* keep going */ }
                                            Value::CtrlStop => {
                                                let rv = sess.get_var("__return__").cloned().unwrap_or(Value::Nil);
                                                sess.env.pop();
                                                sess.consts.pop();
                                                sess.current_module = old_module;
                                                return Ok(rv);
                                            }
                                            other => last_val = other,
                                        }
                                    }
                                }
                                last_val
                            }
                        };
                        
                        // Restore module context and pop scope
                        sess.current_module = old_module;
                        sess.env.pop();
                        sess.consts.pop();
                        
                        return Ok(result);
                    }
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NOT_CALLABLE, // M0002
                                "not-callable",
                                &format!("‘{}’ is not callable", name),
                                sp.clone(),
                            )
                            .with_help("Call an action name, or ensure the value is an action.")
                            .with_link("https://goblinlang.org/docs/errors#M0002")
                        );
                    }
                }
            }
            
            // Check if this is an enum variant access
            if let Some(_enum_def) = sess.enums.get(ns) {
                // Return the variant name as a string for now
                // You can expand this to handle enum values properly
                return Ok(Value::Str(format!("{}::{}", ns, name)));
            }
            
            // Not found anywhere
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::NAMESPACE_NOT_FOUND, // R0115 (NEW)
                    "namespace-not-found",
                    &format!("Namespace ‘{}’ not found (not a module or enum).", ns),
                    sp.clone(),
                )
                .with_help("Use a declared module or enum before ‘::’.")
                .with_link("https://goblinlang.org/docs/errors#R0115"),
            );
        }
        ast::Expr::EnumVariant { enum_name, variant_name, fields, span } => {
            // Look up the enum definition and extract what we need
            let (variant_fields, _enum_exists) = {
                let enum_decl = sess
                    .enums
                    .get(enum_name)
                    .ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::UNKNOWN_ENUM, // R0117 (NEW)
                            "unknown-enum",
                            &format!("unknown enum ‘{}’", enum_name),
                            span.clone(),
                        )
                        .with_help("Ensure the enum is declared in scope and imported correctly.")
                        .with_link("https://goblinlang.org/docs/errors#R0117")
                    })?;

                let variant = enum_decl
                    .variants
                    .iter()
                    .find(|v| &v.name == variant_name)
                    .ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::UNKNOWN_ENUM_VARIANT, // R0118 (NEW)
                            "unknown-variant",
                            &format!("unknown variant ‘{}’ for enum ‘{}’", variant_name, enum_name),
                            span.clone(),
                        )
                        .with_help("Check the variant name or add it to the enum definition.")
                        .with_link("https://goblinlang.org/docs/errors#R0118")
                    })?;

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
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                    "missing-variant-field",
                                    &format!("missing field ‘{}’ for variant ‘{}’", field_decl.name, variant_name),
                                    span.clone(),
                                )
                                .with_help("Provide all required fields for this enum variant.")
                                .with_help(&format!("Example: {}{{ {}: <value>, ... }}", variant_name, field_decl.name))
                                .with_link("https://goblinlang.org/docs/errors#R0403"),
                            );
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

        ast::Expr::Judge { using: _, arms, all, .. } => {
            if *all {
                // judge_all: collect ALL matching results
                let mut results = Vec::new();
                for arm in arms {
                    let matches = if let Some(cond) = &arm.condition {
                        let cond_val = eval_expr(cond, sess)?;
                        match cond_val {
                            Value::Bool(b) => b,
                            Value::Nil => false,
                            _ => true,
                        }
                    } else {
                        true  // else arm always matches
                    };
                    
                    if matches {
                        results.push(eval_expr(&arm.value, sess)?);
                    }
                }
                Ok(Value::Array(results))
            } else {
                // judge: return FIRST match only (existing behavior)
                for arm in arms {
                    let matches = if let Some(cond) = &arm.condition {
                        let cond_val = eval_expr(cond, sess)?;
                        match cond_val {
                            Value::Bool(b) => b,
                            Value::Nil => false,
                            _ => true,
                        }
                    } else {
                        true  // else arm
                    };
                    
                    if matches {
                        return eval_expr(&arm.value, sess);
                    }
                }
                
                // No arm matched
                Ok(Value::Nil)
            }
        }

        ast::Expr::TupleAssign(names, rhs, sp) => {
            let rhs_val = eval_expr(rhs, sess)?;
            
            match &rhs_val {
                Value::Map(map) => {
                    // Assign each variable by trying positional key first, then named key
                    for (i, name) in names.iter().enumerate() {
                        let positional_key = format!("_{}", i + 1);
                        
                        // Try positional key first (_1, _2, etc.)
                        let val = if let Some(v) = map.get(&positional_key) {
                            v.clone()
                        } else if let Some(v) = map.get(name) {
                            // Fall back to named key matching variable name
                            v.clone()
                        } else {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NO_RESULT, // R0902
                                    "no-result",
                                    &format!(
                                        "action didn’t return a value for position {} (variable ‘{}’)",
                                        i + 1,
                                        name
                                    ),
                                    sp.clone(),
                                )
                                .with_help("Ensure the action sets a return value or yields one via ‘stop’/return semantics.")
                                .with_link("https://goblinlang.org/docs/errors#R0902"),
                            );
                        };
                        
                        sess.set_var(name.clone(), val);
                    }
                    
                    Ok(rhs_val)
                }
                _ => {
                    if names.len() == 1 {
                        sess.set_var(names[0].clone(), rhs_val.clone());
                        Ok(rhs_val)
                    } else {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::RETURN_ARITY_MISMATCH, // R0903 (NEW)
                                "return-arity-mismatch",
                                &format!(
                                    "expected action to return {} values, but got a single value",
                                    names.len()
                                ),
                                sp.clone(),
                            )
                            .with_help("Return a tuple or list with the required number of values.")
                            .with_help("Example: `stop (a, b)` when two targets are on the left-hand side.")
                            .with_link("https://goblinlang.org/docs/errors#R0903"),
                        );
                    }
                }
            }
        }

        // Plain assignment (ident = expr)
        ast::Expr::Assign(lhs, rhs, sp) => {
            // Check for field assignment: object >> field = value
            // Could be Binary or Member depending on how parser handles >>
            match &**lhs {
                ast::Expr::Member(base, name, sp) => {
                    let base_v = eval_expr(base, sess)?;
                    match base_v {
                        // object field OR method reference
                        Value::Object { class_name, fields, readonly_fields: _ } => {
                            // 1) If it's a method name on this class, return a bound-method wrapper
                            if let Some(class) = sess.classes.get(&class_name) {
                                if class.actions.iter().any(|a| a.name == *name) {
                                    let mut m = BTreeMap::new();
                                    m.insert("__kind__".to_string(), Value::Str("__bound_action__".to_string()));
                                    m.insert("__name__".to_string(), Value::Str(name.to_string()));
                                    // If the receiver is an identifier, capture its var name so mutations persist
                                    if let ast::Expr::Ident(var_name, _) = &**base {
                                        m.insert("__var__".to_string(), Value::Str(var_name.clone()));
                                    } else {
                                        // Otherwise capture the value so it can still be called (mutations won't persist)
                                        m.insert(
                                            "__recv__".to_string(),
                                            Value::Object {
                                                class_name: class_name.clone(),
                                                fields: fields.clone(),
                                                readonly_fields: BTreeSet::new(),
                                            },
                                        );
                                    }
                                    return Ok(Value::Map(m));
                                }
                            }

                            // 2) Otherwise: normal field lookup
                            match fields.get(name) {
                                Some(v) => Ok(v.clone()),
                                None => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                            "no-such-field",
                                            &format!("no field ‘{}’", name),
                                            sp.clone(),
                                        )
                                        .with_help("Ensure the receiver has this field/key, or guard before accessing.")
                                        .with_link("https://goblinlang.org/docs/errors#R0403"),
                                    );
                                }
                            }
                        }

                        // map key
                        Value::Map(map) => {
                            match map.get(name) {
                                Some(v) => Ok(v.clone()),
                                None => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                            "missing-key",
                                            &format!("missing key ‘{}’", name),
                                            sp.clone(),
                                        )
                                        .with_help("Ensure the map contains this key, or guard before accessing.")
                                        .with_link("https://goblinlang.org/docs/errors#R0403"),
                                    );
                                }
                            }
                        }

                        // enum field on a variant-with-fields
                        Value::Enum { fields: Some(field_map), variant_name, .. } => {
                            match field_map.get(name) {
                                Some(v) => Ok(v.clone()),
                                None => {
                                    return Err(
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                            "no-such-field",
                                            &format!("variant ‘{}’ has no field ‘{}’", variant_name, name),
                                            sp.clone(),
                                        )
                                        .with_help("Check the variant’s declared fields or correct the field name.")
                                        .with_link("https://goblinlang.org/docs/errors#R0403"),
                                    );
                                }
                            }
                        }

                        // enum variant with no fields
                        Value::Enum { fields: None, variant_name, .. } => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                    "no-such-field",
                                    &format!("variant ‘{}’ has no fields", variant_name),
                                    sp.clone(),
                                )
                                .with_help("Use a fieldless pattern for this variant, or pick a variant that defines fields.")
                                .with_link("https://goblinlang.org/docs/errors#R0403"),
                            );
                        }

                        // everything else is a type error for member access
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "member-access-type",
                                    "member access requires a map, object, or enum",
                                    sp.clone(),
                                )
                                .with_help("Use ‘obj.field’ only on a map/object, or an enum variant with fields.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                    }
                }
                
                ast::Expr::Binary(obj_expr, op, field_expr, _) if op == ">>" => {
                    // lhs must be an identifier (object var)
                    let var_name = match &**obj_expr {
                        ast::Expr::Ident(n, _) => n.clone(),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::FIELD_NAME_REQUIRED, // P0804
                                    "field-name-required",
                                    "can only assign to fields of object variables (e.g., obj >> field = …)",
                                    sp.clone(),
                                )
                                .with_help("Use an identifier on the left of ‘>>’, e.g. ‘user >> name’.")
                                .with_link("https://goblinlang.org/docs/errors#P0804"),
                            );
                        }
                    };

                    // rhs (after >>) must be a bare field name
                    let field_name = match &**field_expr {
                        ast::Expr::Ident(n, _) => n.clone(),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::FIELD_NAME_REQUIRED, // P0804
                                    "field-name-required",
                                    "field name required after ‘>>’.",
                                    sp.clone(),
                                )
                                .with_help("Write ‘obj >> field’, where ‘field’ is an identifier.")
                                .with_link("https://goblinlang.org/docs/errors#P0804"),
                            );
                        }
                    };

                    let new_value = eval_expr(rhs, sess)?;

                    // ensure the variable exists and is an object; capture its class name
                    let class_name = match sess.get_var(&var_name) {
                        Some(Value::Object { class_name, .. }) => class_name.clone(),
                        Some(_) => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "not an object",
                                    sp.clone(),
                                )
                                .with_help(&format!("‘{}’ must be an object to use ‘>>’.", var_name))
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                        None => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                                    "unknown-ident",
                                    &format!("unknown variable ‘{}’", var_name),
                                    sp.clone(),
                                )
                                .with_help("Declare the variable before assigning its fields.")
                                .with_link("https://goblinlang.org/docs/errors#R0101"),
                            );
                        }
                    };

                    let class = sess
                        .classes
                        .get(&class_name)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NAMESPACE_NOT_FOUND, // R0115
                                "namespace-not-found",
                                &format!("unknown class ‘{}’", class_name),
                                sp.clone(),
                            )
                            .with_help("Ensure the class is defined and imported.")
                            .with_link("https://goblinlang.org/docs/errors#R0115")
                        })?;

                    let field_decl = class
                        .fields
                        .iter()
                        .find(|f| f.name == field_name)
                        .cloned()
                        .ok_or_else(|| {
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                "no-such-field",
                                &format!("no field ‘{}’", field_name),
                                sp.clone(),
                            )
                            .with_help(&format!("‘{}’ is not a field on class ‘{}’.", field_name, class_name))
                            .with_link("https://goblinlang.org/docs/errors#R0403")
                        })?;

                    let obj_slot = sess.get_var_mut(&var_name).ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                            "unknown-ident",
                            &format!("unknown variable ‘{}’", var_name),
                            sp.clone(),
                        )
                        .with_link("https://goblinlang.org/docs/errors#R0101")
                    })?;

                    match obj_slot {
                        Value::Object { fields, readonly_fields, .. } => {
                            if readonly_fields.contains(&field_name) {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::READONLY_FIELD, // P9001
                                        "readonly-field",
                                        &format!("cannot modify readonly field ‘{}’", field_name),
                                        sp.clone(),
                                    )
                                    .with_help("Remove the mutation or write to a different, mutable field.")
                                    .with_link("https://goblinlang.org/docs/errors#P9001"),
                                );
                            }

                            if matches!(new_value, Value::Nil) && !field_decl.nullable {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205 (use generic type mismatch)
                                        "type-mismatch",
                                        &format!("cannot assign nil to non-nullable field ‘{}’", field_name),
                                        sp.clone(),
                                    )
                                    .with_help("Make the field nullable or provide a non-nil value.")
                                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                                );
                            }

                            fields.insert(field_name.clone(), new_value.clone());
                            return Ok(new_value);
                        }
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "not an object",
                                    sp.clone(),
                                )
                                .with_help(&format!("‘{}’ must be an object to use ‘>>’.", var_name))
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                    }
                }
                
                ast::Expr::Ident(name, _) => {
                    // Simple assignment: name = expr
                    let v = eval_expr(rhs, sess)?;
                    sess.set_var(name.clone(), v.clone());
                    return Ok(v);
                }

                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::LVALUE_EXPECTED, // P0802
                            "lvalue-expected",
                            "left-hand side of assignment must be a variable name or field access",
                            sp.clone(),
                        )
                        .with_help("Assign to a variable (e.g., ‘x = ...’) or an object field (e.g., ‘obj >> field = ...’).")
                        .with_link("https://goblinlang.org/docs/errors#P0802"),
                    );
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204
                                "integer-expected",
                                "index must be a non-negative integer",
                                sp.clone(),
                            )
                            .with_help("Use an integer ≥ 0, e.g., arr[0].")
                            .with_link("https://goblinlang.org/docs/errors#T0204"),
                        );
                    }
                    let k = n as usize;
                    items.get(k).cloned().ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::INVALID_INDEX, // R0401
                            "index-out-of-bounds",
                            "array index out of bounds",
                            sp.clone(),
                        )
                        .with_help("Ensure the index is within the array length.")
                        .with_link("https://goblinlang.org/docs/errors#R0401")
                    })
                }

                (Value::Map(map), Value::Str(key)) => {
                    map.get(&key).cloned().ok_or_else(|| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                            "no-such-field",
                            &format!("missing key ‘{}’", key),
                            sp.clone(),
                        )
                        .with_help("Check the key exists in the map.")
                        .with_link("https://goblinlang.org/docs/errors#R0403")
                    })
                }

                (Value::Map(_), other_idx) => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "map index must be a string key",
                        sp.clone(),
                    )
                    .with_help(&format!("Got {:?}. Use a string key: map[\"name\"]", other_idx))
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),

                (other_base, _) => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "type-mismatch",
                        "indexing requires an array or map",
                        sp.clone(),
                    )
                    .with_help(&format!("Got {:?}. Use an array (arr[i]) or map (obj[\"key\"]).", other_base))
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
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

                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "‘slice’ expects an array or string.",
                                sp.clone(),
                            )
                            .with_help("Pass an array or a string as the receiver, e.g. slice([1,2,3], 1, 2) or slice(\"hello\", 1, 3).")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        );
                    }
                }
            }

            // arr[start:end:step]  (step > 0 only, end-exclusive)
            ast::Expr::Slice3(recv, start_opt, end_opt, step_opt, sp) => {
                let recv_v = eval_expr(recv, sess)?;

                // evaluate indices (defaults)
                let step_v: usize = if let Some(stp) = step_opt {
                    let vv = eval_expr(stp, sess)?;
                    let u = want_usize_index(vv, "slice step", sp.clone())?;
                    if u == 0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                                "positive-int-expected",
                                "‘slice.step’ must be a positive integer.",
                                sp.clone(),
                            )
                            .with_help("Use an integer ≥ 1.")
                            .with_link("https://goblinlang.org/docs/errors#T0202"),
                        );
                    }
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
                            let ch = slice_char(&s, idx).unwrap(); // one-character string
                            out.push_str(&ch);
                            idx = idx.saturating_add(step_v);
                        }
                        Ok(Value::Str(out))
                    }

                    _ => {
                        Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "‘slice’ expects an array or string.",
                                sp.clone(),
                            )
                            .with_help("Pass an array or a string as the receiver, e.g. slice([1,2,3], 1, 2) or slice(\"hello\", 1, 3).")
                            .with_link("https://goblinlang.org/docs/errors#T0205"),
                        )
                    }
                }
            }

       // ---- Member access on maps (syntax from parser; you're not using dot in code, but handle it) ----
       ast::Expr::Member(base, name, sp) => {
           let base_v = eval_expr(base, sess)?;
           
           // Check if this is a builtin method on a primitive type
           let is_builtin_method = matches!(name.as_str(), 
               // Collection methods
               "count" | "len" | "length" |
               // String transforms
               "upper" | "lower" | "title" | "slug" | "mixed" |
               "trim" | "trim_lead" | "trim_trail" |
               // Collection operations  
               "reverse" | "reverse_chars" | "shuffle" | "sort" | "unique" | "dups" |
               "freq" | "mode" |
               // String operations
               "split" | "join" | "lines" | "words" | "chars" |
               "has" | "find" | "find_all" |
               "before" | "after" | "before_last" | "after_last" | "between" |
               "replace" |
               // Numeric
               "round" | "floor" | "ceil" | "abs" | "sqrt" |
               // Type operations
               "valtype" | "vt" | "backend" | "metrics"
           );
           
           if is_builtin_method {
               // Check if the value type supports this method
               let valid = match name.as_str() {
                   "count" | "len" | "length" => {
                       matches!(base_v, Value::Str(_) | Value::Array(_) | Value::Seq(_) | Value::Map(_))
                   }
                   "upper" | "lower" | "title" | "slug" | "mixed" | 
                   "trim" | "trim_lead" | "trim_trail" |
                   "reverse_chars" | "lines" | "words" | "chars" |
                   "has" | "find" | "find_all" | "before" | "after" | 
                   "before_last" | "after_last" | "between" | "replace" => {
                       matches!(base_v, Value::Str(_))
                   }
                   "reverse" | "shuffle" | "sort" | "unique" | "dups" | 
                   "freq" | "mode" => {
                       matches!(base_v, Value::Str(_) | Value::Array(_) | Value::Seq(_))
                   }
                   "split" => {
                       matches!(base_v, Value::Str(_))
                   }
                   "join" => {
                       matches!(base_v, Value::Array(_) | Value::Seq(_))
                   }
                   "round" | "floor" | "ceil" | "abs" | "sqrt" => {
                       matches!(base_v, Value::Int(_) | Value::Float(_) | Value::Pct(_) | Value::Big(_))
                   }
                   "valtype" | "vt" | "backend" | "metrics" => true, // Works on any type
                   _ => false,
               };
               
               if valid {
                   // Call the builtin function with base_v as first argument
                   return call_action_by_name(sess, name, vec![base_v], sp.clone());
               }
           }
           match base_v {
               Value::Map(map) => {
                   match map.get(name) {
                       Some(v) => Ok(v.clone()),
                       None => Err(
                           Diagnostic::new_with_code(
                               Severity::Error,
                               crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                               "missing-key",
                               &format!("missing key ‘{}’", name),
                               sp.clone(),
                           )
                           .with_help("Ensure the key exists before accessing it.")
                           .with_link("https://goblinlang.org/docs/errors#R0403"),
                       ),
                   }
               }

               Value::Object { class_name, fields, readonly_fields: _ } => {
                   // 1) If it's a method name on this class, return a bound-method wrapper
                   if let Some(class) = sess.classes.get(&class_name) {
                       if class.actions.iter().any(|a| a.name == *name) {
                           let mut m = BTreeMap::new();
                           m.insert("__kind__".to_string(), Value::Str("__bound_action__".to_string()));
                           m.insert("__name__".to_string(), Value::Str(name.to_string()));
                           // If the receiver is an identifier, capture its var name so mutations persist
                           if let ast::Expr::Ident(var_name, _) = &**base {
                               m.insert("__var__".to_string(), Value::Str(var_name.clone()));
                           } else {
                               // Otherwise capture the value so it can still be called (mutations won't persist)
                               m.insert(
                                   "__recv__".to_string(),
                                   Value::Object {
                                       class_name: class_name.clone(),
                                       fields: fields.clone(),
                                       readonly_fields: BTreeSet::new(),
                                   },
                               );
                           }
                           return Ok(Value::Map(m));
                       }
                   }

                   // 2) Otherwise: normal field lookup
                   match fields.get(name) {
                       Some(v) => Ok(v.clone()),
                       None => Err(
                           Diagnostic::new_with_code(
                               Severity::Error,
                               crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                               "no-such-field",
                               &format!("no field ‘{}’", name),
                               sp.clone(),
                           )
                           .with_help("Check the object’s fields or correct the field name.")
                           .with_link("https://goblinlang.org/docs/errors#R0403"),
                       ),
                   }
               }

               Value::Enum { fields: Some(field_map), variant_name, .. } => {
                   field_map.get(name)
                       .cloned()
                       .ok_or_else(|| 
                           Diagnostic::new_with_code(
                               Severity::Error,
                               crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                               "no-such-field",
                               &format!("variant ‘{}’ has no field ‘{}’", variant_name, name),
                               sp.clone(),
                           )
                           .with_help("Check the variant’s declared fields or correct the field name.")
                           .with_link("https://goblinlang.org/docs/errors#R0403")
                       )
               }

               Value::Enum { fields: None, variant_name, .. } => {
                   Err(
                       Diagnostic::new_with_code(
                           Severity::Error,
                           crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                           "no-fields-on-variant",
                           &format!("variant ‘{}’ has no fields", variant_name),
                           sp.clone(),
                       )
                       .with_help("Use a variant that declares fields, or remove the field access.")
                       .with_link("https://goblinlang.org/docs/errors#R0403")
                   )
               }
               _ => {
                   return Err(
                       Diagnostic::new_with_code(
                           Severity::Error,
                           crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                           "member-access-type",
                           "member access requires a map, object, or enum.",
                           sp.clone(),
                       )
                       .with_help("Use ‘obj.field’ only on a map/object/enum variant.")
                       .with_link("https://goblinlang.org/docs/errors#T0205")
                   );
               }
           }
       }

        ast::Expr::OptMember(base, name, sp) => {
            let base_v = eval_expr(base, sess)?;
            match base_v {
                Value::Nil => Ok(Value::Nil),
                Value::Map(map) => Ok(map.get(name).cloned().unwrap_or(Value::Nil)),
                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                        "opt-member-type",
                        "optional member access requires a map.",
                        sp.clone(),
                    )
                    .with_help("Use ‘m?.key’ only when the receiver is a map (or Nil).")
                    .with_link("https://goblinlang.org/docs/errors#T0205"),
                ),
            }
        }

        // ---- Free calls ----
        ast::Expr::FreeCall(name, args, sp) => {

            // ============ BOUND METHOD DISPATCH ============
            // Check local frames for bound methods (but not module env to avoid recursion)
            let mut found_bound_method = None;
            for frame in sess.env.iter().rev() {
                if let Some(Value::Map(m)) = frame.get(name) {
                    if matches!(m.get("__kind__"), Some(Value::Str(s)) if s == "__bound_action__") {
                        found_bound_method = Some(m.clone());
                        break;
                    }
                }
            }

            if let Some(m) = found_bound_method {
                let _method_name = match m.get("__name__") {
                    Some(Value::Str(s)) => s.clone(),
                    _ => {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::MALFORMED_BOUND_ACTION, // R0410 (NEW)
                                "malformed-bound-action",
                                "bound action map is missing the ‘__name__’ field.",
                                sp.clone(),
                            )
                            .with_help("Ensure bound-action objects include a string ‘__name__’.")
                            .with_link("https://goblinlang.org/docs/errors#R0410"),
                        )
                    }
                };
            }

            // ============ END BOUND METHOD DISPATCH ============

            // ---- Mutating casts for function form when arg is a plain identifier ----
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                &format!("Wrong number of arguments (expected 2 or 3, got {})", args.len()),
                                sp.clone(),
                            )
                            .with_help("Usage: if(cond, then_block[, else_block])")
                            .with_help("‘then_block’/‘else_block’ must be arrays of expressions.")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    let cond_v = eval_expr(&args[0], sess)?;
                    if as_bool(cond_v, sp.clone(), "if condition")? {
                        let then_es = expect_array(&args[1], "then_block", sp.clone())?;
                        Ok(eval_expr_list(then_es, sess)?.unwrap_or(Value::Unit))
                    } else if args.len() == 3 {
                        let else_es = expect_array(&args[2], "else_block", sp.clone())?;
                        Ok(eval_expr_list(else_es, sess)?.unwrap_or(Value::Unit))
                    } else {
                        Ok(Value::Unit)
                    }
                }

                "while" => {
                    if args.len() != 2 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                                sp.clone(),
                            )
                            .with_help("Usage: while(cond, body_block)")
                            .with_help("‘body_block’ must be an array of expressions.")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    let body_es = expect_array(&args[1], "body_block", sp.clone())?;
                    sess.loop_depth += 1;
                    'outer: loop {
                        let c = eval_expr(&args[0], sess)?;
                        if !as_bool(c, sp.clone(), "while condition")? { break; }
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

                "for" => {
                    if args.len() != 3 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                &format!("Wrong number of arguments (expected 3, got {})", args.len()),
                                sp.clone(),
                            )
                            .with_help("Usage: for([var_name, iterable, body])")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    // var_name (must be String)
                    let var_name_expr = eval_expr(&args[0], sess)?;
                    let var_name = match var_name_expr {
                        Value::Str(s) => s,
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "‘for’ var_name must be a string.",
                                    sp.clone(),
                                )
                                .with_help("Example: for([\"x\", [1,2,3], [ say x ]])")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    };

                    // iterable (Array or String)
                    let iterable_val = eval_expr(&args[1], sess)?;
                    let items = match iterable_val {
                        Value::Array(arr) => arr,
                        Value::Str(s) => s.chars().map(Value::Char).collect(),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    "‘for’ can only iterate over arrays or strings.",
                                    sp.clone(),
                                )
                                .with_help("Pass an Array (e.g., [1,2,3]) or a String (iterates characters).")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            )
                        }
                    };

                    // body (expects array/block)
                    let body_es = expect_array(&args[2], "body", sp.clone()).map_err(|_| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::EXPECTED_ARRAY, // P0314
                            "expected-array",
                            "‘for’ body must be an array (block).",
                            sp.clone(),
                        )
                        .with_help("Example: for([\"x\", [1,2,3], [ say x ]])")
                        .with_link("https://goblinlang.org/docs/errors#P0314")
                    })?;

                    sess.loop_depth += 1;

                    'outer: for item in items {
                        sess.set_var(var_name.clone(), item);
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

                "repeat" => {
                    // arity
                    if args.len() != 2 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                &format!("Wrong number of arguments (expected 2, got {})", args.len()),
                                sp.clone(),
                            )
                            .with_help("Usage: repeat([count, body_block])")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    // count
                    let count_val = eval_expr(&args[0], sess)?;
                    let count = match count_val {
                        Value::Int(n) if n >= 0 => n as usize,
                        Value::Int(n) => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                                    "math-domain",
                                    &format!("‘repeat’ count must be ≥ 0 (got {}).", n),
                                    sp.clone(),
                                )
                                .with_help("Use a non-negative integer, e.g. 0, 1, 2, …")
                                .with_link("https://goblinlang.org/docs/errors#R0207"),
                            )
                        }
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204
                                    "integer-expected",
                                    "‘repeat’ count must be an integer.",
                                    sp.clone(),
                                )
                                .with_help("Example: repeat([3, [ say \"hi\" ]])")
                                .with_link("https://goblinlang.org/docs/errors#T0204"),
                            )
                        }
                    };

                    // body
                    let body_es = expect_array(&args[1], "body_block", sp.clone()).map_err(|_| {
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::EXPECTED_ARRAY, // P0314
                            "expected-array",
                            "‘repeat’ body_block must be an array (block).",
                            sp.clone(),
                        )
                        .with_help("Example: repeat([2, [ say \"hi\" ]])")
                        .with_link("https://goblinlang.org/docs/errors#P0314")
                    })?;

                    // execute
                    sess.loop_depth += 1;
                    'outer: for _ in 0..count {
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

                "write_json" => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::MUTATION_OPERATOR_REQUIRED, // M0001
                            "mutation-operator-required",
                            "‘write_json’ requires the bang form: use write_json!(…)",
                            sp.clone(),
                        )
                        .with_help("Append ‘!’ to perform filesystem writes, e.g., write_json!(path, value[, pretty]).")
                        .with_link("https://goblinlang.org/docs/errors#M0001"),
                    );
                }

                "create_dir" => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::MUTATION_OPERATOR_REQUIRED, // M0001
                            "mutation-operator-required",
                            "‘create_dir’ requires the bang form: use create_dir!(…)",
                            sp.clone(),
                        )
                        .with_help("Append ‘!’ to create directories, e.g., create_dir!(path).")
                        .with_link("https://goblinlang.org/docs/errors#M0001"),
                    );
                }

                "attempt" => {
                    // args: [attempt_body_array, rescue_blocks_array, ensure_body_array?]
                    if args.is_empty() {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                "‘attempt’ requires at least 1 argument.",
                                sp.clone(),
                            )
                            .with_help("Usage: attempt([attempt_block], [rescue_blocks][, ensure_block])")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    // Get the attempt body expressions
                    let attempt_es = expect_array(&args[0], "attempt_block", sp.clone())?;

                    let rescue_blocks_es = if args.len() > 1 {
                        expect_array(&args[1], "rescue_blocks", sp.clone())?
                    } else {
                        &[]
                    };

                    // Try to execute attempt block
                    let mut result = Value::Unit;
                    let mut had_error = false;

                    for e in attempt_es {
                        match eval_expr(e, sess) {
                            Ok(v) => result = v,
                            Err(_err) => {
                                had_error = true;
                                break;
                            }
                        }
                    }

                    // If error occurred, execute first rescue block
                    if had_error && rescue_blocks_es.len() > 0 {
                        // Each rescue block is [var_name_or_nil, body_array]
                        let rescue_info_es = expect_array(&rescue_blocks_es[0], "rescue_info", sp.clone())?;

                        if rescue_info_es.len() >= 2 {
                            // rescue_info_es[1] is the rescue body array
                            let rescue_body_es = expect_array(&rescue_info_es[1], "rescue_body", sp.clone())?;

                            // Execute rescue block
                            result = Value::Unit;
                            for e in rescue_body_es {
                                result = eval_expr(e, sess)?;
                            }
                        }
                    }

                    // Execute ensure block if present (should always run)
                    if args.len() > 2 {
                        let ensure_es = expect_array(&args[2], "ensure_block", sp.clone())?;
                        for e in ensure_es {
                            let _ = eval_expr(e, sess)?;
                        }
                    }

                    Ok(result)
                }

                "skip" => {
                    if sess.loop_depth <= 0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::LOOP_CONTROL_OUTSIDE, // R0405
                                "loop-control-outside-loop",
                                "‘skip’ used outside of a loop",
                                sp.clone(),
                            )
                            .with_help("Use ‘skip’ only inside loop constructs like while/for/repeat.")
                            .with_link("https://goblinlang.org/docs/errors#R0405"),
                        );
                    }
                    Ok(Value::CtrlSkip)
                }

                "stop" => {
                    if sess.loop_depth <= 0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::LOOP_CONTROL_OUTSIDE, // R0405
                                "loop-control-outside-loop",
                                "‘stop’ used outside of a loop",
                                sp.clone(),
                            )
                            .with_help("Use ‘stop’ only inside loop constructs like while/for/repeat.")
                            .with_link("https://goblinlang.org/docs/errors#R0405"),
                        );
                    }
                    Ok(Value::CtrlStop)
                }

                "return" => {
                    // return was called as FreeCall from inside an if block
                    let mut vals: Vec<Value> = Vec::with_capacity(args.len());
                    let mut labels: Vec<Option<String>> = Vec::with_capacity(args.len());

                    for e in args {
                        match e {
                            ast::Expr::Ident(name, _) => {
                                labels.push(Some(name.clone()));
                                vals.push(sess.get_var(name).cloned().unwrap_or(Value::Nil));
                            }
                            _ => {
                                labels.push(None);
                                vals.push(eval_expr(e, sess)?);
                            }
                        }
                    }

                    let ret = match vals.len() {
                        0 => Value::Nil,
                        1 => vals.into_iter().next().unwrap(),
                        _ => {
                            if labels.iter().all(|l| l.is_some()) {
                                let mut map = BTreeMap::new();
                                for (lab, v) in labels.into_iter().zip(vals.into_iter()) {
                                    map.insert(lab.unwrap(), v);
                                }
                                Value::Map(map)
                            } else {
                                let mut map = BTreeMap::new();
                                let mut idx = 1usize;
                                for (lab, v) in labels.into_iter().zip(vals.into_iter()) {
                                    if let Some(name) = lab {
                                        map.insert(name, v);
                                    } else {
                                        let key = format!("_{}", idx);
                                        map.insert(key, v);
                                        idx += 1;
                                    }
                                }
                                Value::Map(map)
                            }
                        }
                    };

                    sess.set_var("__return__".to_string(), ret);
                    Ok(Value::CtrlStop)
                }

                // v(n): history lookup (1-based)
                "v" => {
                    if args.len() != 1 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::WRONG_ARITY, // R0301
                                "wrong-arity",
                                &format!("Wrong number of arguments (expected 1, got {})", args.len()),
                                sp.clone(),
                            )
                            .with_help("‘v(n)’ takes exactly 1 argument.")
                            .with_help("Usage: v(1)")
                            .with_link("https://goblinlang.org/docs/errors#R0301"),
                        );
                    }

                    let n_val = eval_expr(&args[0], sess)?;
                    let n = as_num(n_val, sp.clone(), "v(n)")?; // emits R0200 if not numeric

                    if n < 1.0 || n.fract() != 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED, // T0202
                                "positive-int-expected",
                                "‘v(n)’ requires a positive integer.",
                                sp.clone(),
                            )
                            .with_help("Use an integer ≥ 1 (e.g., v(1)).")
                            .with_link("https://goblinlang.org/docs/errors#T0202"),
                        );
                    }

                    let idx = n as usize;
                    match sess.get(idx) {
                        Some(v) => Ok(v.clone()),
                        None => {
                            let len = sess.history_len();
                            Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::INVALID_INDEX, // R0401
                                    "index-out-of-range",
                                    &format!("v({idx}) is out of range (history has {len})."),
                                    sp.clone(),
                                )
                                .with_help(&format!("Valid range is 1..={len}."))
                                .with_link("https://goblinlang.org/docs/errors#R0401"),
                            )
                        }
                    }
                }

                // say: prints value; for strings, render interpolation at print time
                "say" => {
                    let printed = if args.is_empty() { Value::Unit } else { eval_expr(&args[0], sess)? };
                    match printed {
                        Value::Str(s) => {
                            let rendered = render_interpolated(&s, sess, &sp.clone())?;
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
            
            
            // Special-case: <expr>.valtype or <expr>.vt
            if (name == "valtype" || name == "vt") && args.is_empty() {
                let recv = eval_expr(base, sess)?;
                return Ok(Value::Str(value_kind_str(&recv).to_string()));
            }
                        
            // Bang methods
            if name.ends_with('!') {
            }
            
            let base_ref: &ast::Expr = base;  // Explicit conversion
            let recv = eval_expr(base_ref, sess)?;
            
            // Object method calls - check if receiver is an object AND base is a variable
            if let ast::Expr::Ident(var_name, _) = &**base {
                if let Value::Object { class_name, mut fields, readonly_fields } = recv {
                    let result = call_object_method(sess, &class_name, &mut fields, name, args, sp.clone())?;
                    
                    // Update the object variable with modified fields
                    let updated_obj = Value::Object { 
                        class_name, 
                        fields,
                        readonly_fields,
                    };
                    sess.set_var(var_name.clone(), updated_obj);
                    
                    return Ok(result);
                }
            }
            
            // Regular method calls on non-object values (or object non-variables)
            let mut argv = Vec::with_capacity(args.len() + 1);
            argv.push(recv);  // Use the already-evaluated recv
            for a in args { argv.push(eval_expr(a, sess)?); }
            call_action_by_name(sess, name, argv, sp.clone())
        }

        ast::Expr::OptCall(base, name, args, sp) => {
            // Support ?.type (nil-propagating)
            if (name == "valtype" || name == "vt") && args.is_empty() {
                // If syntactically `nil`, short-circuit to nil
                if matches!(&**base, ast::Expr::Nil(_)) { return Ok(Value::Nil); }
                // Otherwise evaluate; nil still short-circuits
                let recv = eval_expr(base, sess)?;
                if matches!(recv, Value::Nil) { return Ok(Value::Nil); }
                // For non-nil, reuse the existing builtin classification
                return call_action_by_name(sess, "valtype", vec![recv], sp.clone());
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
                        _other           => return Err(need_number("unary '-'", sp.clone()).into()),  // Change this line
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
                        _other           => return Err(need_number("unary '+'", sp.clone()).into()),  // Changed here
                    };
                    Ok(reapply_format(out, uspec, None))
                }

                "!" | "not" => {
                    let v = eval_expr(expr, sess)?;
                    match v {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::BOOLEAN_EXPECTED, // T0203
                                "boolean-expected",
                                "logical ‘not’ requires a boolean.",
                                sp.clone(),
                            )
                            .with_help("Use true/false, or an expression that evaluates to a boolean.")
                            .with_link("https://goblinlang.org/docs/errors#T0203"),
                        ),
                    }
                }

                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::OP_NOT_IMPLEMENTED, // R0504
                        "op-not-implemented",
                        &format!("prefix operator ‘{}’ is not implemented.", op),
                        sp.clone(),
                    )
                    .with_help("Use a supported operator or update the implementation.")
                    .with_link("https://goblinlang.org/docs/errors#R0504"),
                ),
            }
        }

        // ---- Postfix operators ----
        ast::Expr::Postfix(expr, op, sp) => {
            let v = eval_expr(expr, sess)?;
            match op.as_str() {
                // ---------- MUTATING POSTFIX OPS ----------
                "++" => {
                    // expr++  ==>  expr = expr + 1
                    let one = ast::Expr::Number("1".to_string(), sp.clone());
                    let rhs = ast::Expr::Binary(expr.clone(), "+".to_string(), Box::new(one), sp.clone());
                    let assign = ast::Expr::Assign(expr.clone(), Box::new(rhs), sp.clone());
                    return eval_expr(&assign, sess); // reuse existing Assign semantics
                }
                "--" => {
                    // expr--  ==>  expr = expr - 1
                    let one = ast::Expr::Number("1".to_string(), sp.clone());
                    let rhs = ast::Expr::Binary(expr.clone(), "-".to_string(), Box::new(one), sp.clone());
                    let assign = ast::Expr::Assign(expr.clone(), Box::new(rhs), sp.clone());
                    return eval_expr(&assign, sess);
                }

                // ---------- NON-MUTATING POSTFIX OPS ----------
                "%" => {
                    let n = as_num(v, sp.clone(), "percent literal")?;
                    Ok(Value::Pct(n / 100.0))
                }
                "**" => Ok(Value::Float(as_num(v, sp.clone(), "postfix square")?.powf(2.0))),
                "//" => {
                    let n = as_num(v, sp.clone(), "postfix sqrt")?;
                    if n < 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::MATH_DOMAIN, // R0207
                                "math-domain",
                                "sqrt domain error: cannot take square root of a negative value.",
                                sp.clone(),
                            )
                            .with_help("Provide a non-negative input (x ≥ 0).")
                            .with_help("Guard the call, e.g. `if x >= 0 { sqrt(x) }`.")
                            .with_link("https://goblinlang.org/docs/errors#R0207"),
                        );
                    }

                    Ok(Value::Float(n.sqrt()))
                }
                "!" => {
                    let n = as_num(v, sp.clone(), "factorial")?;
                    // factorial(n): requires n to be a non-negative integer
                    if n < 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::POSITIVE_INT_EXPECTED,
                                "positive-integer-expected",
                                "factorial requires a non-negative integer (n ≥ 0).",
                                sp.clone(),
                            )
                            .with_help("Use an integer ≥ 0, e.g. 0, 1, 2, …")
                            .with_link("https://goblinlang.org/docs/errors#T0202"),
                        );
                    }
                    if n.fract() != 0.0 {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::INTEGER_EXPECTED, // T0204
                                "integer-expected",
                                "factorial requires an integer value.",
                                sp.clone(),
                            )
                            .with_help("Provide a whole number without a fractional part.")
                            .with_link("https://goblinlang.org/docs/errors#T0204"),
                        );
                    }
                    let mut acc: u128 = 1;
                    let k = n as u128;
                    for i in 2..=k { acc = acc.saturating_mul(i); }
                    Ok(Value::Float(acc as f64))
                }
                "^" => Ok(Value::Float(as_num(v, sp.clone(), "ceil")?.ceil())),
                "_" => Ok(Value::Float(as_num(v, sp.clone(), "floor")?.floor())),
                "?" => Ok(Value::Bool(!matches!(v, Value::Nil))),
                "*>>" | "*>>:show_ids" => {
                    let show_ids = op.ends_with(":show_ids");
                    match v {
                        Value::Object { fields, .. } => {
                            // move fields into a plain map; optionally hide ids
                            let mut out = std::collections::BTreeMap::new();
                            for (k, val) in fields {
                                if !show_ids && (k == "id" || k.ends_with("_id")) { continue; }
                                out.insert(k, val);
                            }
                            Ok(Value::Map(out))
                        }
                        Value::Map(m) => {
                            if show_ids { Ok(Value::Map(m)) } else {
                                let mut out = std::collections::BTreeMap::new();
                                for (k, v) in m {
                                    if k == "id" || k.ends_with("_id") { continue; }
                                    out.insert(k, v);
                                }
                                Ok(Value::Map(out))
                            }
                        }
                        other => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                    "type-mismatch",
                                    &format!("‘*>>’ expects an object or map; got {:?}", other),
                                    sp.clone(),
                                )
                                .with_help("Use an object or map on the left-hand side, e.g. obj *>> { k: v } or map *>> { k: v }.")
                                .with_link("https://goblinlang.org/docs/errors#T0205"),
                            );
                        }
                    }
                }

                _ => {
                    return Err(
                        Diagnostic::new_with_code(
                            Severity::Error,
                            crate::diagnostics::rtcode::OP_NOT_IMPLEMENTED, // R0504
                            "op-not-implemented",
                            &format!("postfix operator ‘{}’ is not implemented", op),
                            sp.clone(),
                        )
                        .with_help("Use a supported postfix operator, or remove it.")
                        .with_link("https://goblinlang.org/docs/errors#R0504"),
                    );
                }
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
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::FIELD_NAME_REQUIRED, // P0804
                                    "field-name-required",
                                    "right side of ‘>>’ must be a field name",
                                    sp.clone(),
                                )
                                .with_help("Use an identifier after ‘>>’, e.g. obj >> field")
                                .with_link("https://goblinlang.org/docs/errors#P0804"),
                            )
                        }
                    };
                    
                    match obj {
                        Value::Object { fields, .. } => {
                            fields.get(&field_name)
                                .cloned()
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                        "no-such-field",
                                        &format!("no field ‘{}’", field_name),
                                        sp.clone(),
                                    )
                                    .with_help("Check the field name or ensure it exists on the object.")
                                    .with_link("https://goblinlang.org/docs/errors#R0403")
                                })
                        }

                        Value::Enum { fields: Some(field_map), variant_name, .. } => {
                            field_map.get(&field_name)
                                .cloned()
                                .ok_or_else(|| {
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                        "no-such-field",
                                        &format!("variant ‘{}’ has no field ‘{}’", variant_name, field_name),
                                        sp.clone(),
                                    )
                                    .with_help("Verify the field exists on this enum variant.")
                                    .with_link("https://goblinlang.org/docs/errors#R0403")
                                })
                        }

                        Value::Enum { fields: None, variant_name, .. } => {
                            Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NO_SUCH_FIELD, // R0403
                                    "no-such-field",
                                    &format!("variant ‘{}’ has no fields", variant_name),
                                    sp.clone(),
                                )
                                .with_help("Use a variant that defines fields, or remove the field access.")
                                .with_link("https://goblinlang.org/docs/errors#R0403")
                            )
                        }

                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::ARRAY_EXPECTED, // R0402 (member/collection access target wrong kind)
                                "member-target-invalid",
                                ">> requires an object or enum on the left side",
                                sp.clone(),
                            )
                            .with_help("Provide an object or enum value before ‘>>’.")
                            .with_link("https://goblinlang.org/docs/errors#R0402")
                        ),
                    }
                }
                // arithmetic
                "+" => {
                    let lhs_clone = lhs.clone();
                    let rhs_clone = rhs.clone();
                    let lv = eval_expr(&lhs_clone, sess)?;
                    let rv = eval_expr(&rhs_clone, sess)?;
                    
                    // If either side is a string, do string concat (after removing formatting wrappers)
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);
                    
                    match (&lu, &ru) {
                        (Value::Str(a), Value::Str(b)) => {
                            return Ok(Value::Str(format!("{a}{b}")));
                        }
                        (Value::Str(a), other) => {
                            return Ok(Value::Str(format!("{a}{}", fmt_value_raw(other))));
                        }
                        (other, Value::Str(b)) => {
                            return Ok(Value::Str(format!("{}{b}", fmt_value_raw(other))));
                        }
                        _ => { /* fall through to numeric ladder below using lu/ru */ }
                    }
                    
                    // ---- NUMERIC ADDITION ----
                    let out = match (&lu, &ru) {
                        (Value::Int(a), Value::Int(b)) => {
                            if let Some(r) = int_checked_add(*a, *b) { Value::Int(r) }
                            else { Value::Big(rust_decimal::Decimal::from(*a) + rust_decimal::Decimal::from(*b)) }
                        }
                        (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) =>
                            Value::Float((*a as f64) + *b),
                        (Value::Float(a), Value::Float(b)) =>
                            Value::Float(*a + *b),
                        (Value::Big(_), _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; 
                            let db = to_decimal(&ru)?;
                            Value::Big(da + db)
                        }
                        (Value::Pct(a), Value::Pct(b)) => Value::Pct(*a + *b),
                        (Value::Pct(a), _) => Value::Float(*a + to_f64_for_math(&ru, sp.clone(), "addition: rhs")?),
                        (_, Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, sp.clone(), "addition: lhs")? + *b),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                    "numeric-expected",
                                    "‘addition’ requires numeric operands.",
                                    sp.clone(),
                                )
                                .with_help("Both the left and right operands must be numbers (int/float/big/pct).")
                                .with_help("Convert or cast non-numeric values before using ‘+’.")
                                .with_link("https://goblinlang.org/docs/errors#R0200")
                            );
                        }
                    };
                    
                    Ok(reapply_format(out, lspec, rspec))
                }

                "++" => {
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;

                    // Remove any formatting wrappers first so Str stays bare (no quotes)
                    let (lu, _) = take_owned_unformatted(lv);
                    let (ru, _) = take_owned_unformatted(rv);

                    // Bare string for Str; fall back to your raw formatter for non-strings
                    let ls = match &lu { Value::Str(s) => s.clone(), _ => fmt_value_raw(&lu) };
                    let rs = match &ru { Value::Str(s) => s.clone(), _ => fmt_value_raw(&ru) };

                    let out = if ls.is_empty() { rs }
                              else if rs.is_empty() { ls }
                              else { format!("{ls} {rs}") };

                    let out = out.trim_matches('"').to_string();
                    Ok(Value::Str(out))
                }

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
                        (Value::Pct(a),   _) => Value::Float(*a - to_f64_for_math(&ru, sp.clone(), "subtraction: rhs")?),
                        (_,               Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, sp.clone(), "subtraction: lhs")? - *b),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                    "numeric-expected",
                                    "'subtraction' requires numeric operands.",
                                    sp.clone(),
                                )
                                .with_help("Both the left and right operands must be numbers (int/float/big/pct).")
                                .with_help("Convert or cast non-numeric values before using ‘-’.")
                                .with_link("https://goblinlang.org/docs/errors#R0200")
                            );
                        }
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
                        (Value::Pct(a),   _) => Value::Float(*a * to_f64_for_math(&ru, sp.clone(), "multiplication: rhs")?),
                        (_,               Value::Pct(b)) => Value::Float(to_f64_for_math(&lu, sp.clone(), "multiplication: lhs")? * *b),
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                    "numeric-expected",
                                    "‘multiplication’ requires numeric operands.",
                                    sp.clone(),
                                )
                                .with_help("Both the left and right operands must be numbers (int/float/big/pct).")
                                .with_help("Convert or cast non-numeric values before using ‘*’.")
                                .with_link("https://goblinlang.org/docs/errors#R0200")
                            );
                        }
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "/" => {
                    let lv = eval_expr(lhs, sess)?; 
                    let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = match (&lu, &ru) {
                        (Value::Int(a), Value::Int(b)) => {
                            if *b == 0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            if a % b == 0 {
                                Value::Int(a / b)
                            } else {
                                Value::Float((*a as f64) / (*b as f64))
                            }
                        }
                        (Value::Int(a), Value::Float(b)) => {
                            if *b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Float((*a as f64) / *b)
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            if *b == 0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Float(*a / (*b as f64))
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            if *b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Float(*a / *b)
                        }
                        (Value::Big(_),   _) | (_, Value::Big(_)) => {
                            let da = to_decimal(&lu)?; 
                            let db = to_decimal(&ru)?;
                            if db.is_zero() {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Big(da / db)
                        }
                        (Value::Pct(a), Value::Pct(b)) => {
                            if *b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Float(*a / *b)
                        }
                        (Value::Pct(a),   _) => {
                            let denom = to_f64_for_math(&ru, sp.clone(), "division: rhs")?;
                            if denom == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            Value::Float(*a / denom)
                        }
                        (_, Value::Pct(b)) => {
                            if *b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before dividing.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            let num = to_f64_for_math(&lu, sp.clone(), "division: lhs")?;
                            Value::Float(num / *b)
                        }
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                    "numeric-expected",
                                    "‘division’ requires numeric operands.",
                                    sp.clone(),
                                )
                                .with_help("Both the left and right operands must be numbers (int/float/big/pct).")
                                .with_help("Convert or cast non-numeric values before using ‘/’.")
                                .with_link("https://goblinlang.org/docs/errors#R0200")
                            );
                        }
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "%" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        // Euclidean-style remainder with Decimal: r = a - floor(a/b) * b
                        let a = to_big_for_math(&lu, sp.clone(), "modulo: left")?;
                        let b = to_big_for_math(&ru, sp.clone(), "modulo: right")?;
                        if b.is_zero() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘%’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
                        let q = (a / b).floor();
                        let r = a - q * b;
                        Value::Big(r)
                    } else {
                        // f64 path
                        let a = to_f64_for_math(&lu, sp.clone(), "modulo: left")?;
                        let b = to_f64_for_math(&ru, sp.clone(), "modulo: right")?;
                        if b == 0.0 {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘%’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
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
                        let a = to_big_for_math(&lu, sp.clone(), "floor division: left")?;
                        let b = to_big_for_math(&ru, sp.clone(), "floor division: right")?;
                        if b.is_zero() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘//’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
                        Value::Big((a / b).floor())
                    } else {
                        let a = to_f64_for_math(&lu, sp.clone(), "floor division: left")?;
                        let b = to_f64_for_math(&ru, sp.clone(), "floor division: right")?;
                        if b == 0.0 {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘//’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
                        Value::Float((a / b).floor())
                    };
                    Ok(reapply_format(out, lspec, rspec))
                },

                "**" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, lspec) = take_owned_unformatted(lv);
                    let (ru, rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        // Promote base to Decimal
                        let base = to_big_for_math(&lu, sp.clone(), "power: base")?;

                        // Try to use integer exponent in Decimal space (precise)
                        match &ru {
                            Value::Big(db) => {
                                let e_trunc = db.trunc();
                                if *db == e_trunc {
                                    // integer exponent in Decimal
                                    let n = e_trunc.to_i64().ok_or_else(|| 
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            crate::diagnostics::rtcode::BIG_EXPONENT_RANGE, // R0203
                                            "big-exponent-range",
                                            "decimal exponent out of range",
                                            sp.clone(),
                                        )
                                        .with_help("When using Decimal pow, the exponent must fit in a 64-bit integer.")
                                        .with_help("Use an integer exponent (e.g., 2) or cast to float for very large exponents.")
                                        .with_link("https://goblinlang.org/docs/errors#R0203")
                                    )?;
                                    Value::Big(decimal_powi(base, n)?)
                                } else {
                                    // fractional exponent -> fall back to f64 powf (precision loss)
                                    let bf = base.to_f64().ok_or_else(||
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            /* NEW */ "R0204", // Reserve this: BIG_BASE_OVERFLOW_FLOAT
                                            "big-base-overflow-to-float",
                                            "big base overflow to float",
                                            sp.clone(),
                                        )
                                        .with_help("The Decimal base cannot be represented as f64 for ‘**’.")
                                        .with_help("Try reducing magnitude, increasing precision, or using a smaller exponent.")
                                        .with_link("https://goblinlang.org/docs/errors#R0204")
                                    )?;
                                    let ef = db.to_f64().ok_or_else(||
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            /* NEW */ "R0205", // Reserve this: BIG_EXPONENT_OVERFLOW_FLOAT
                                            "big-exponent-overflow-to-float",
                                            "big exponent overflow to float",
                                            sp.clone(),
                                        )
                                        .with_help("The Decimal exponent cannot be represented as f64 for ‘**’.")
                                        .with_help("Try reducing magnitude or use an integer exponent to stay in Decimal space.")
                                        .with_link("https://goblinlang.org/docs/errors#R0205")
                                    )?;
                                    Value::Float(bf.powf(ef))
                                }
                            }
                            Value::Float(f) | Value::Pct(f) => {
                                // If exponent is an integer (e.g. 2.0), keep Big. Else fall back to float.
                                if f.fract() == 0.0 && *f >= i64::MIN as f64 && *f <= i64::MAX as f64 {
                                    Value::Big(decimal_powi(base, *f as i64)?)
                                } else {
                                    let bf = base.to_f64().ok_or_else(||
                                        Diagnostic::new_with_code(
                                            Severity::Error,
                                            /* NEW */ "R0204", // BIG_BASE_OVERFLOW_FLOAT
                                            "big-base-overflow-to-float",
                                            "big base overflow to float",
                                            sp.clone(),
                                        )
                                        .with_help("The Decimal base cannot be represented as f64 for ‘**’.")
                                        .with_help("Try reducing magnitude or use an integer exponent to remain in Decimal space.")
                                        .with_link("https://goblinlang.org/docs/errors#R0204")
                                    )?;
                                    Value::Float(bf.powf(*f))
                                }
                            }
                            _ => {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::NUMERIC_EXPECTED, // R0200
                                        "numeric-expected",
                                        "numeric value expected",
                                        sp.clone(),
                                    )
                                    .with_help("The exponent for ‘**’ must be a number (Int, Float, Percent, or Decimal).")
                                    .with_help("Provide a numeric exponent or cast before using ‘**’.")
                                    .with_link("https://goblinlang.org/docs/errors#R0200")
                                )
                            }
                        }
                    } else {
                        // pure float
                        let a = to_f64_for_math(&lu, sp.clone(), "power: base")?;
                        let b = to_f64_for_math(&ru, sp.clone(), "power: exponent")?;
                        Value::Float(a.powf(b))
                    };

                    Ok(reapply_format(out, lspec, rspec))
                }

                "><" => {
                    let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?;
                    let (lu, _lspec) = take_owned_unformatted(lv);
                    let (ru, _rspec) = take_owned_unformatted(rv);

                    let out = if either_is_big(&lu, &ru) {
                        let a = to_big_for_math(&lu, sp.clone(), "divmod: left")?;
                        let b = to_big_for_math(&ru, sp.clone(), "divmod: right")?;
                        if b.is_zero() {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘><’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
                        let q = (a / b).floor();
                        let r = a - q * b;
                        Value::Pair(Box::new(Value::Big(q)), Box::new(Value::Big(r)))
                    } else {
                        let a = to_f64_for_math(&lu, sp.clone(), "divmod: left")?;
                        let b = to_f64_for_math(&ru, sp.clone(), "divmod: right")?;
                        if b == 0.0 {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                    "division-by-zero",
                                    "division by zero",
                                    sp.clone(),
                                )
                                .with_help("The right-hand operand evaluated to zero.")
                                .with_help("Guard against zero or handle it explicitly before using ‘><’.")
                                .with_link("https://goblinlang.org/docs/errors#R0206")
                            );
                        }
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
                            let rnum = as_num(rv, sp.clone(), "'of' right")?;
                            Ok(Value::Float(p * rnum))
                        }
                        _ => {
                            let lnum = as_num(lv,  sp.clone(),  "'of' left")?;
                            let rnum = as_num(rv,  sp.clone(),  "'of' right")?;
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
                        other         => as_num(other, sp.clone(), "percent-of-other")? / 100.0,
                    };
                    let b = as_num(rv, sp.clone(), "percent-of-other")?;
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
                            let a = to_big_for_math(&la, sp.clone(), "== left")?;
                            let b = to_big_for_math(&rb, sp.clone(), "== right")?;
                            a == b
                        } else {
                            let a = to_f64_for_math(&la, sp.clone(), "== left")?;
                            let b = to_f64_for_math(&rb, sp.clone(), "== right")?;
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
                            let a = to_big_for_math(&la, sp.clone(), "!= left")?;
                            let b = to_big_for_math(&rb, sp.clone(), "!= right")?;
                            a != b
                        } else {
                            let a = to_f64_for_math(&la, sp.clone(), "!= left")?;
                            let b = to_f64_for_math(&rb, sp.clone(), "!= right")?;
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
                            let a = to_big_for_math(&la, sp.clone(), "compare left")?;
                            let c = to_big_for_math(&rb, sp.clone(), "compare right")?;
                            match op.as_str() {
                                "<"  => a <  c,
                                "<=" => a <= c,
                                ">"  => a >  c,
                                ">=" => a >= c,
                                _    => unreachable!(),
                            }
                        } else {
                            let a = to_f64_for_math(&la, sp.clone(), "compare left")?;
                            let c = to_f64_for_math(&rb, sp.clone(), "compare right")?;
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::TYPE_MISMATCH, // T0205
                                "type-mismatch",
                                "type mismatch",
                                sp.clone(),
                            )
                            .with_help("Both operands of this comparison must be compatible types.")
                            .with_help("Cast or convert one operand so the types match before comparing.")
                            .with_link("https://goblinlang.org/docs/errors#T0205")
                        );
                    };

                    Ok(Value::Bool(b))
                }

                // logical ops + coalesce
                "and" | "&&" => {
                    let lv = eval_expr(lhs, sess)?;
                    match lv {
                        Value::Bool(false) => return Ok(Value::Bool(false)), // short-circuit
                        Value::Bool(true)  => { /* evaluate rhs */ }
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BOOLEAN_EXPECTED, // T0203
                                    "boolean-expected",
                                    "boolean expected",
                                    sp.clone(),
                                )
                                .with_help("Logical ‘and’ (and/&&) requires a boolean on the left side.")
                                .with_help("Cast or convert the left operand to Bool before using ‘and’/‘&&’.")
                                .with_link("https://goblinlang.org/docs/errors#T0203")
                            );
                        }
                    }
                    let rv = eval_expr(rhs, sess)?;
                    match rv {
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::BOOLEAN_EXPECTED, // T0203
                                "boolean-expected",
                                "boolean expected",
                                sp.clone(),
                            )
                            .with_help("Logical ‘and’ (and/&&) requires a boolean on the right side.")
                            .with_help("Cast or convert the right operand to Bool before using ‘and’/‘&&’.")
                            .with_link("https://goblinlang.org/docs/errors#T0203")
                        ),
                    }
                }
                "or" | "<>" => {
                    let lv = eval_expr(lhs, sess)?;
                    match lv {
                        Value::Bool(true)  => return Ok(Value::Bool(true)), // short-circuit
                        Value::Bool(false) => { /* evaluate rhs */ }
                        _ => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::BOOLEAN_EXPECTED, // T0203
                                    "boolean-expected",
                                    "boolean expected",
                                    sp.clone(),
                                )
                                .with_help("Logical ‘or’ (or/<>) requires a boolean on the left side.")
                                .with_help("Cast or convert the left operand to Bool before using ‘or’/‘<>’.")
                                .with_link("https://goblinlang.org/docs/errors#T0203")
                            );
                        }
                    }
                    let rv = eval_expr(rhs, sess)?;
                    match rv {
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        _ => Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::BOOLEAN_EXPECTED, // T0203
                                "boolean-expected",
                                "boolean expected",
                                sp.clone(),
                            )
                            .with_help("Logical ‘or’ (or/<>) requires a boolean on the right side.")
                            .with_help("Cast or convert the right operand to Bool before using ‘or’/‘<>’.")
                            .with_link("https://goblinlang.org/docs/errors#T0203")
                        ),
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
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                crate::diagnostics::rtcode::LVALUE_EXPECTED, // P0802
                                "lvalue-expected",
                                "lvalue expected",
                                sp.clone(),
                            )
                            .with_help("The left-hand side of a compound assignment must be a variable name.")
                            .with_help("Assign to an identifier (e.g., ‘x += 1’) rather than an expression.")
                            .with_link("https://goblinlang.org/docs/errors#P0802")
                        );
                    };

                    let old = match sess.get_var(&name) {
                        Some(v) => v.clone(),
                        None => {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    crate::diagnostics::rtcode::UNKNOWN_IDENT, // R0101
                                    "unknown-ident",
                                    "unknown identifier",
                                    sp.clone(),
                                )
                                .with_help(&format!("‘{}’ is not defined in this scope.", name))
                                .with_help("Declare it before use, or check for typos.")
                                .with_link("https://goblinlang.org/docs/errors#R0101")
                            );
                        }
                    };

                    let rv = eval_expr(rhs, sess)?;
                    let a = as_num(old, sp.clone(), "compound assign (left value)")?;
                    let b = as_num(rv,  sp.clone(), "compound assign (right value)")?;
                    let new = match op.as_str() {
                        "+=" => a + b,
                        "-=" => a - b,
                        "*=" => a * b,
                        "/=" => {
                            if b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before using ‘/=’.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            a / b
                        }
                        "//=" => {
                            if b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before using ‘//=’.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            (a / b).floor()
                        }
                        "%=" =>  {
                            if b == 0.0 {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        crate::diagnostics::rtcode::DIVISION_BY_ZERO, // R0206
                                        "division-by-zero",
                                        "division by zero",
                                        sp.clone(),
                                    )
                                    .with_help("The right-hand operand evaluated to zero.")
                                    .with_help("Guard against zero or handle it explicitly before using ‘%=’.")
                                    .with_link("https://goblinlang.org/docs/errors#R0206")
                                );
                            }
                            let q = (a / b).floor();
                            a - q * b
                        }
                        "**=" => a.powf(b),
                        _ => unreachable!(),
                    };
                    let out = Value::Float(new);
                    sess.set_var(name, out.clone());
                    Ok(out)
                }

                _ => Err(
                    Diagnostic::new_with_code(
                        Severity::Error,
                        crate::diagnostics::rtcode::OP_NOT_IMPLEMENTED, // R0504
                        "op-not-implemented",
                        "operator not implemented",
                        sp.clone(),
                    )
                    .with_help(&format!("The operator ‘{}’ is not implemented.", op))
                    .with_help("Use a different operator or convert to a supported form.")
                    .with_link("https://goblinlang.org/docs/errors#R0504")
                ),
            }
        }
    }
}

fn call_object_method_with_values(
    sess: &mut Session,
    class_name: &str,
    fields: &mut BTreeMap<String, Value>,
    method_name: &str,
    arg_vals: Vec<Value>,
    sp: Span,
) -> Result<Value, Diag> {
    let class = sess.classes.get(class_name)
        .ok_or_else(|| 
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::UNKNOWN_CLASS, // R0116 (new)
                "unknown-class",
                "unknown class",
                sp.clone(),
            )
            .with_help(&format!("No class named ‘{}’ is defined.", class_name))
            .with_help("Check imports, registration, or spelling.")
            .with_link("https://goblinlang.org/docs/errors#R0116")
        )?
        .clone();

    let action = class.actions.iter()
        .find(|a| a.name == method_name)
        .ok_or_else(|| 
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::UNKNOWN_ACTION, // A0401
                "unknown-action",
                "unknown action",
                sp.clone(),
            )
            .with_help(&format!("Class ‘{}’ has no action named ‘{}’.", class_name, method_name))
            .with_help("Check the method name or define it on the class.")
            .with_link("https://goblinlang.org/docs/errors#A0401")
        )?
        .clone();

    // Check if we have enough arguments (non-defaulted params)
    let required_params: Vec<_> = action.params.iter().filter(|p| p.default.is_none()).collect();
    if arg_vals.len() < required_params.len() {
        return Err(
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                "missing-argument",
                "missing argument",
                sp.clone(),
            )
            .with_help(&format!(
                "Missing required parameter ‘{}’ (got {} args, need at least {}).",
                required_params[arg_vals.len()].name,
                arg_vals.len(),
                required_params.len()
            ))
            .with_help("Provide all required parameters or specify defaults.")
            .with_link("https://goblinlang.org/docs/errors#R0302")
        );
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
            // Should be unreachable due to the check above; emit the same standardized error.
            sess.pop_frame();
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                    "missing-argument",
                    "missing argument",
                    sp.clone(),
                )
                .with_help(&format!("Missing required parameter ‘{}’.", param.name))
                .with_help("Provide all required parameters or specify defaults.")
                .with_link("https://goblinlang.org/docs/errors#R0302")
            );
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

    // Copy modified field values back
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

fn call_object_method(
    sess: &mut Session,
    class_name: &str,
    fields: &mut BTreeMap<String, Value>,
    method_name: &str,
    arg_exprs: &[ast::Expr],
    sp: Span,
) -> Result<Value, Diag> {
    let class = sess.classes.get(class_name)
        .ok_or_else(|| 
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::UNKNOWN_CLASS, // R0116
                "unknown-class",
                "unknown class",
                sp.clone(),
            )
            .with_help(&format!("No class named ‘{}’ is defined.", class_name))
            .with_help("Check imports, registration, or spelling.")
            .with_link("https://goblinlang.org/docs/errors#R0116")
        )?
        .clone();

    let action = class.actions.iter()
        .find(|a| a.name == method_name)
        .ok_or_else(|| 
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::UNKNOWN_ACTION, // A0401
                "unknown-action",
                "unknown action",
                sp.clone(),
            )
            .with_help(&format!("Class ‘{}’ has no action named ‘{}’.", class_name, method_name))
            .with_help("Check the method name or define it on the class.")
            .with_link("https://goblinlang.org/docs/errors#A0401")
        )?
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
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    crate::diagnostics::rtcode::MISSING_ARGUMENT, // R0302
                    "missing-argument",
                    "missing argument",
                    sp.clone(),
                )
                .with_help(&format!("Missing required parameter ‘{}’.", param.name))
                .with_help("Provide all required parameters or specify defaults.")
                .with_link("https://goblinlang.org/docs/errors#R0302")
            );
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
        .ok_or_else(|| 
            Diagnostic::new_with_code(
                Severity::Error,
                crate::diagnostics::rtcode::UNKNOWN_CLASS, // R0116
                "unknown-class",
                "unknown class",
                span.clone(),
            )
            .with_help(&format!("No class named ‘{}’ is defined.", class_name))
            .with_help("Check imports, registration, or spelling.")
            .with_link("https://goblinlang.org/docs/errors#R0116")
        )?
        .clone();

    // === Auto-ID generation (readonly) ===
    let r = sess.next_u128();
    let time_hi_and_version = ((r >> 64) as u16 & 0x0FFF) | 0x4000;      // version 4
    let clock_seq_hi_and_reserved = ((r >> 48) as u16 & 0x3FFF) | 0x8000; // variant 10
    let id_str = format!("{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
        (r >> 96) as u32,
        (r >> 80) as u16,
        time_hi_and_version,
        clock_seq_hi_and_reserved,
        ((r & 0x0000_FFFF_FFFF_FFFFu128) >> 16) as u64
    );
    
    // Evaluate RHS expression
    let rhs_val = eval_expr(expr, sess)?;
    
    let mut field_map = BTreeMap::new();
    let mut readonly_fields = BTreeSet::new();
    
    // Mark readonly fields
    for field in &class.fields {
        if field.readonly {
            readonly_fields.insert(field.name.clone());
        }
    }

    // Inject auto-generated id as readonly field
    field_map.insert("id".to_string(), Value::Str(id_str.clone()));
    readonly_fields.insert("id".to_string());
    
    match rhs_val {
        Value::Map(provided_fields) => {
            // Named field construction: { name: "Alice", author: user_obj }
            for field in &class.fields {
                // Handle relationship fields
                if let Some(ref relation) = field.relation {
                    match relation {
                        ast::RelationDef::Of { class_name: _, as_name } => {
                            // 'of' relation: stores a foreign key
                            let fk_field_name = format!("{}_id", as_name);
                            
                            if let Some(provided_val) = provided_fields.get(&field.name) {
                                // Extract ID if value is an object
                                let id_val = match provided_val {
                                    Value::Object { fields: obj_fields, .. } => {
                                        obj_fields.get("id")
                                            .cloned()
                                            .unwrap_or(Value::Nil)
                                    }
                                    // Assume it's already an ID
                                    other => other.clone(),
                                };
                                
                                // Store both the FK field and optionally the relation field
                                field_map.insert(fk_field_name, id_val.clone());
                                field_map.insert(field.name.clone(), id_val);
                            } else if field.nullable {
                                field_map.insert(fk_field_name, Value::Nil);
                                field_map.insert(field.name.clone(), Value::Nil);
                            } else {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        "R0411", // NEW: relation value required
                                        "relation-value-required",
                                        "relation value required",
                                        span.clone(),
                                    )
                                    .with_help(&format!("‘of’ relation field ‘{}’ requires a value.", field.name))
                                    .with_help("Provide an object with an ‘id’ or a foreign key string.")
                                    .with_link("https://goblinlang.org/docs/errors#R0411")
                                );
                            }
                        }
                        ast::RelationDef::With { .. } | ast::RelationDef::Re { .. } => {
                            // 'with' and 're' are reverse/many-to-many metadata only
                            // No storage needed - skip this field
                            continue;
                        }
                    }
                    continue;
                }
                
                // Normal field handling
                let value = if let Some(val) = provided_fields.get(&field.name) {
                    // User provided this field
                    if matches!(val, Value::Nil) && !field.nullable {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                "T0207", // NEW: non-nullable field assigned nil
                                "non-nullable-field-nil",
                                "cannot assign nil to non-nullable field",
                                span.clone(),
                            )
                            .with_help(&format!("Field ‘{}’ is non-nullable.", field.name))
                            .with_help("Provide a non-nil value or mark the field as nullable.")
                            .with_link("https://goblinlang.org/docs/errors#T0207")
                        );
                    }
                    val.clone()
                } else {
                    // Field not provided - use default or error
                    if let Some(default_expr) = &field.default {
                        eval_expr(default_expr, sess)?
                    } else if field.nullable {
                        Value::Nil
                    } else {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                "T0206", // NEW: non-nullable field requires a value
                                "non-nullable-field-required",
                                "non-nullable field requires a value",
                                span.clone(),
                            )
                            .with_help(&format!("Field ‘{}’ is required and has no default.", field.name))
                            .with_help("Provide a value explicitly or add a default.")
                            .with_link("https://goblinlang.org/docs/errors#T0206")
                        );
                    }
                };
                
                field_map.insert(field.name.clone(), value);
            }
        }
        Value::Array(values) => {
            // Positional construction
            let mut value_idx = 0;
            
            for field in &class.fields {
                // Handle relationship fields
                if let Some(ref relation) = field.relation {
                    match relation {
                        ast::RelationDef::Of { class_name: _, as_name } => {
                            let fk_field_name = format!("{}_id", as_name);
                            
                            if value_idx < values.len() {
                                let val = &values[value_idx];
                                value_idx += 1;
                                
                                let id_val = match val {
                                    Value::Object { fields: obj_fields, .. } => {
                                        obj_fields.get("id")
                                            .cloned()
                                            .unwrap_or(Value::Nil)
                                    }
                                    other => other.clone(),
                                };
                                
                                field_map.insert(fk_field_name, id_val.clone());
                                field_map.insert(field.name.clone(), id_val);
                            } else if field.nullable {
                                field_map.insert(fk_field_name, Value::Nil);
                                field_map.insert(field.name.clone(), Value::Nil);
                            } else {
                                return Err(
                                    Diagnostic::new_with_code(
                                        Severity::Error,
                                        "R0411", // NEW: relation value required
                                        "relation-value-required",
                                        "relation value required",
                                        span.clone(),
                                    )
                                    .with_help(&format!("‘of’ relation field ‘{}’ requires a value.", field.name))
                                    .with_help("Provide an object with an ‘id’ or a foreign key string.")
                                    .with_link("https://goblinlang.org/docs/errors#R0411")
                                );
                            }
                        }
                        ast::RelationDef::With { .. } | ast::RelationDef::Re { .. } => {
                            // Skip - no storage
                            continue;
                        }
                    }
                    continue;
                }
                
                // Normal field handling
                let value = if value_idx < values.len() {
                    let val = &values[value_idx];
                    value_idx += 1;
                    
                    if matches!(val, Value::Unit) {
                        if let Some(default_expr) = &field.default {
                            eval_expr(default_expr, sess)?
                        } else if field.nullable {
                            Value::Nil
                        } else {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    "T0206", // NEW: non-nullable field requires a value
                                    "non-nullable-field-required",
                                    "non-nullable field requires a value",
                                    span.clone(),
                                )
                                .with_help(&format!("Field ‘{}’ is required and has no default.", field.name))
                                .with_help("Provide a value explicitly or add a default.")
                                .with_link("https://goblinlang.org/docs/errors#T0206")
                            );
                        }
                    } else {
                        if matches!(val, Value::Nil) && !field.nullable {
                            return Err(
                                Diagnostic::new_with_code(
                                    Severity::Error,
                                    "T0207", // NEW: non-nullable field assigned nil
                                    "non-nullable-field-nil",
                                    "cannot assign nil to non-nullable field",
                                    span.clone(),
                                )
                                .with_help(&format!("Field ‘{}’ is non-nullable.", field.name))
                                .with_help("Provide a non-nil value or mark the field as nullable.")
                                .with_link("https://goblinlang.org/docs/errors#T0207")
                            );
                        }
                        val.clone()
                    }
                } else {
                    if let Some(default_expr) = &field.default {
                        eval_expr(default_expr, sess)?
                    } else if field.nullable {
                        Value::Nil
                    } else {
                        return Err(
                            Diagnostic::new_with_code(
                                Severity::Error,
                                "T0206", // NEW: non-nullable field requires a value
                                "non-nullable-field-required",
                                "non-nullable field requires a value",
                                span.clone(),
                            )
                            .with_help(&format!("Field ‘{}’ is required and has no default.", field.name))
                            .with_help("Provide a value explicitly or add a default.")
                            .with_link("https://goblinlang.org/docs/errors#T0206")
                        );
                    }
                };
                
                field_map.insert(field.name.clone(), value);
            }
        }
        single => {
            return Err(
                Diagnostic::new_with_code(
                    Severity::Error,
                    "T0208", // NEW: object or array expected for construction
                    "object-or-array-expected",
                    "object or array expected",
                    span,
                )
                .with_help(format!("Expected an object literal (‘{{...}}’) or array (‘[...]’) to construct ‘{}’. Got {:?}", class_name, single))
                .with_help("Use named-field construction with a map or positional construction with an array.")
                .with_link("https://goblinlang.org/docs/errors#T0208")
            );
        }
    }
    
    let obj = Value::Object {
        class_name: class_name.to_string(),
        fields: field_map,
        readonly_fields,
    };
    
    sess.define_local(var_name.to_string(), obj, is_const);
    Ok(None)
}