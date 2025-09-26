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
use std::sync::Arc;

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
            if let SeqBackend::Array(vs) = &self.backend { /* borrow ends */ }
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
        m.insert("len".into(), Value::Num(self.metrics.len as f64));
        m.insert("ops_push_back".into(), Value::Num(self.metrics.ops_push_back as f64));
        m.insert("ops_push_front".into(), Value::Num(self.metrics.ops_push_front as f64));
        m.insert("ops_insert_idx".into(), Value::Num(self.metrics.ops_insert_idx as f64));
        m.insert("ops_remove_idx".into(), Value::Num(self.metrics.ops_remove_idx as f64));
        m.insert("ops_random_access".into(), Value::Num(self.metrics.ops_random_access as f64));
        m.insert("transitions".into(), Value::Num(self.metrics.transitions as f64));
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
    Num(f64),
    Str(String),
    Bool(bool),
    Array(Vec<Value>),
    Map(BTreeMap<String, Value>),
    Pair(Box<Value>, Box<Value>), // for >< (divmod)
    Seq(Seq),
    Nil,
    Unit,
    CtrlSkip,  
    CtrlStop,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(x)  => write!(f, "{}", fmt_num_trim(*x)),
            Value::Str(s)  => write!(f, "{:?}", s),
            Value::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Nil     => write!(f, "nil"),

            // ---- Array (single arm) ----
            Value::Array(xs) => {
                write!(f, "[")?;
                for (i, v) in xs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
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
    pub loop_depth: i32,
    rng_state: u128, 
}

impl Session {
    pub fn new() -> Self {
        let t = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default();
        let seed = t.as_nanos() ^ 0xA24B_AED4_963E_E407u128;
        Self {
            history: Vec::new(),
            env: vec![BTreeMap::new()],
            actions: BTreeMap::new(),
            loop_depth: 0,
            rng_state: seed,
        }

    }

    #[inline]
    pub fn reseed(&mut self, seed: u128) {
        self.rng_state = seed;
    }

    /// Fast 128-bit LCG
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
    fn push_frame(&mut self) { self.env.push(BTreeMap::new()); }
    fn pop_frame(&mut self) { let _ = self.env.pop(); }

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

    pub fn eval_stmt(&mut self, s: &ast::Stmt) -> Result<Option<Value>, Diag> {
        eval_stmt(s, self)
    }

    /// Evaluate a whole module (returns last expression value if any).
    pub fn eval_module(&mut self, m: &ast::Module) -> Result<Option<Value>, Diag> {
        let mut last = None;
        for stmt in &m.items {
            if let Some(v) = eval_stmt(stmt, self)? {
                last = Some(v);
            }
        }
        Ok(last)
    }

    /// Parse + eval a single REPL form using the real parser.
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

    /// Evaluate a single expression node and push it to history.
    pub fn eval_expr(&mut self, e: &ast::Expr) -> Result<Value, Diag> {
        let v = eval_expr(e, self)?;
        self.history.push(v.clone());
        Ok(v)
    }

    pub fn history_len(&self) -> usize { self.history.len() }
    pub fn get(&self, idx_1_based: usize) -> Option<&Value> { self.history.get(idx_1_based.saturating_sub(1)) }
}

// ===================== Helpers =====================

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

fn not_impl(stage: &str, what: &str, span: Span) -> Diag {
    rt("R0000", format!("{what} is not implemented in {stage}"), span)
}

fn need_number(what: &str, span: Span) -> Diag {
    rt("T0201", format!("{what} expects a number"), span)
}

fn parse_num(text: &str, span: Span) -> Result<f64, Diag> {
    text.parse::<f64>().map_err(|_| rt("P0301", format!("invalid number literal '{text}'"), span))
}

fn span_of_expr(e: &ast::Expr) -> Span {
    match e {
        ast::Expr::Nil(sp)
        | ast::Expr::Bool(_, sp)
        | ast::Expr::Number(_, sp)
        | ast::Expr::Str(_, sp)
        | ast::Expr::Ident(_, sp)
        | ast::Expr::Array(_, sp)
        | ast::Expr::Object(_, sp)
        | ast::Expr::Member(_, _, sp)
        | ast::Expr::OptMember(_, _, sp)
        | ast::Expr::Index(_, _, sp)
        | ast::Expr::Call(_, _, _, sp)
        | ast::Expr::OptCall(_, _, _, sp)
        | ast::Expr::FreeCall(_, _, sp)
        | ast::Expr::NsCall(_, _, _, sp)
        | ast::Expr::Prefix(_, _, sp)
        | ast::Expr::Postfix(_, _, sp)
        | ast::Expr::Binary(_, _, _, sp)
        | ast::Expr::Assign(_, _, sp) => sp.clone(),
    }
}

fn as_bool(v: Value, at: Span, label: &str) -> Result<bool, Diag> {
    match v {
        Value::Bool(b) => Ok(b),
        _ => Err(rt("T0303", format!("{label} requires a boolean"), at)),
    }
}

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

fn fmt_value_raw(v: &Value) -> String {
    match v {
        Value::Str(s) => s.clone(),
        Value::Num(n) => fmt_num_trim(*n),
        Value::Bool(b) => if *b { "true".into() } else { "false".into() },
        Value::Nil => "nil".into(),
        _ => format!("{}", v), // Array/Map/Pair -> use Display
    }
}

fn as_num(v: Value, at: Span, label: &str) -> Result<f64, Diag> {
    match v {
        Value::Num(n) => Ok(n),
        _ => Err(need_number(label, at)),
    }
}

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

/// Render "Hello {name}" by looking identifiers up in the Session env.
/// Supports "{{" -> "{" and "}}" -> "}".
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

        // Register free action declarations at load/run time (Stage 4+).
        ast::Stmt::Action(decl) => {
            sess.actions.insert(decl.name.clone(), decl.clone());
            Ok(None)
        }

        // Classes are later; ignore gracefully for now.
        ast::Stmt::Class(_c) => Ok(None),
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

/// Try a shadowable builtin. Returns Ok(Some(Value)) if handled, Ok(None) if unknown.
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
    let want_num = |v: &Value, label: &str| -> Result<f64, Diag> {
        match v {
            Value::Num(n) => Ok(*n),
            _ => Err(need_number(label, sp.clone())),
        }
    };

    let want_str = |v: &Value, label: &str, at: &Span| -> Result<String, Diag> {
        match v {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(rt("T0205", format!("{label} expects a string"), at.clone())),
        }
    };

    let out = match name {
        // ---------- Numeric ----------
        "round" => {
            arity(1)?;
            Value::Num(want_num(&args[0], "round")?.round())
        }
        "floor" => {
            arity(1)?;
            Value::Num(want_num(&args[0], "floor")?.floor())
        }
        "ceil" => {
            arity(1)?;
            Value::Num(want_num(&args[0], "ceil")?.ceil())
        }
        "abs" => {
            arity(1)?;
            Value::Num(want_num(&args[0], "abs")?.abs())
        }
        "pow" => {
            arity(2)?;
            let a = want_num(&args[0], "pow (base)")?;
            let b = want_num(&args[1], "pow (exponent)")?;
            Value::Num(a.powf(b))
        }
        "sqrt" => {
            arity(1)?;
            let n = want_num(&args[0], "sqrt")?;
            if n < 0.0 { return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone())); }
            Value::Num(n.sqrt())
        }

        // ---------- Collection (array<number>) ----------
        "sum" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    let mut acc = 0.0;
                    for v in xs { acc += want_num(v, "sum")?; }
                    Value::Num(acc)
                }
                _ => return Err(rt("T0401", "sum expects an array of numbers", sp.clone())),
            }
        }

        "avg" => {
            arity(1)?;
            match &args[0] {
                Value::Array(xs) => {
                    if xs.is_empty() { Value::Num(0.0) }
                    else {
                        let mut acc = 0.0;
                        for v in xs { acc += want_num(v, "avg")?; }
                        Value::Num(acc / xs.len() as f64)
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
                    let mut m = want_num(first, "min")?;
                    for v in it { m = m.min(want_num(v, "min")?); }
                    Value::Num(m)
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
                    let mut m = want_num(first, "max")?;
                    for v in it { m = m.max(want_num(v, "max")?); }
                    Value::Num(m)
                }
                _ => return Err(rt("T0401", "max expects an array of numbers", sp.clone())),
            }
        }

        // ---------- String case & transforms ----------
        "upper" => {
            arity(1)?;
            let s = want_str(&args[0], "upper", &sp)?;
            Value::Str(s.to_uppercase())
        }
        "lower" => {
            arity(1)?;
            let s = want_str(&args[0], "lower", &sp)?;
            Value::Str(s.to_lowercase())
        }
        "title" => {
            arity(1)?;
            let s = want_str(&args[0], "title", &sp)?;
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
            let s = want_str(&args[0], "slug", &sp)?;
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
            let s = want_str(&args[0], "mixed", &sp)?;
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
    fn get_i64_from(m:&std::collections::BTreeMap<String,Value>, k:&str, sp:&Span)->Result<i64,Diag>{
        match m.get(k) {
            Some(Value::Num(n)) if n.fract()==0.0 => Ok(*n as i64),
            Some(_) => Err(rt("T0201",&format!("roll '{k}' must be an integer"), sp.clone())),
            None => Err(rt("T0201",&format!("missing roll field '{k}'"), sp.clone())),
        }
    }
    fn get_i64_opt(m:&std::collections::BTreeMap<String,Value>, k:&str)->Option<i64>{
        m.get(k).and_then(|v| if let Value::Num(n)=v { if n.fract()==0.0 { Some(*n as i64) } else { None } } else { None })
    }
    fn get_bool_opt(m:&std::collections::BTreeMap<String,Value>, k:&str)->Option<bool>{
        m.get(k).and_then(|v| if let Value::Bool(b)=v { Some(*b) } else { None })
    }

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
            Value::Num(n) => Ok(*n),
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
            Value::Array(xs) => {
                let mut out = Vec::with_capacity(xs.len());
                for it in xs {
                    match it {
                        Value::Str(s) => out.push(Value::Str(f(s))),
                        _ => return Err(rt("T0205", format!("{label} expects a string (or array of strings)"), sp.clone())),
                    }
                }
                Ok(Value::Array(out))
            }
            _ => Err(rt("T0205", format!("{label} expects a string (or array of strings)"), sp.clone())),
        }
    };

    let out: Value = match name {
        // ----- Numeric -----
        "round" => { arity(1)?; Value::Num(want_num(&args[0], "round")?.round()) }
        "floor" => { arity(1)?; Value::Num(want_num(&args[0], "floor")?.floor()) }
        "ceil"  => { arity(1)?; Value::Num(want_num(&args[0], "ceil")?.ceil()) }
        "abs"   => { arity(1)?; Value::Num(want_num(&args[0], "abs")?.abs()) }
        "pow"   => { arity(2)?; Value::Num(want_num(&args[0], "pow (base)")?.powf(want_num(&args[1], "pow (exponent)")?)) }
        "sqrt"  => {
            arity(1)?;
            let n = want_num(&args[0], "sqrt")?;
            if n < 0.0 { return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone())); }
            Value::Num(n.sqrt())
        }

        // ----- Array<number> stats -----
        "sum" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                let mut acc = 0.0;
                for v in xs { acc += want_num(v, "sum")?; }
                Value::Num(acc)
            } else {
                return Err(rt("T0401", "sum expects an array of numbers", sp.clone()));
            }
        }
        "avg" => {
            arity(1)?;
            if let Some(xs) = as_array_like(&args[0]) {
                if xs.is_empty() { Value::Num(0.0) } else {
                    let mut acc = 0.0;
                    for v in xs { acc += want_num(v, "avg")?; }
                    Value::Num(acc / xs.len() as f64)
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
                Value::Num(m)
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
                Value::Num(m)
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
                m.get(k).and_then(|v| if let Value::Num(n) = v { Some(*n) } else { None })
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
                        out.push(Value::Num(pool[rng_index(sess, pool.len())] as f64));
                    }
                    out
                } else {
                    // Without replacement: partial Fisher–Yates on the pool itself.
                    for i in 0..n_out {
                        let j = i + rng_index(sess, pool.len() - i);
                        pool.swap(i, j);
                    }
                    (0..n_out).map(|i| Value::Num(pool[i] as f64)).collect()
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
                        out.push(Value::Num(gen_one() as f64));
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
                    set.into_iter().map(|v| Value::Num(v as f64)).collect()
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
                    Some(Value::Num(n)) if n.fract() == 0.0 => Ok(*n as i64),
                    Some(_) => Err(rt("T0201", &format!("roll '{k}' must be an integer"), sp.clone())),
                    None => Err(rt("T0201", &format!("missing roll field '{k}'"), sp.clone())),
                }
            };
            let get_i64_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| if let Value::Num(n)=v { if n.fract()==0.0 { Some(*n as i64) } else { None } } else { None })
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

            Value::Num(result_num as f64)
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
                    Some(Value::Num(n)) if n.fract() == 0.0 => Ok(*n as i64),
                    Some(_) => Err(rt("T0201", &format!("roll '{k}' must be an integer"), sp.clone())),
                    None => Err(rt("T0201", &format!("missing roll field '{k}'"), sp.clone())),
                }
            };
            let get_i64_opt = |m: &BTreeMap<String, Value>, k: &str| -> Option<i64> {
                m.get(k).and_then(|v| if let Value::Num(n)=v { if n.fract()==0.0 { Some(*n as i64) } else { None } } else { None })
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
            let detail_out: Value = if (adv || dis) {
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
                out.insert("count".into(),    Value::Num(1.0));
                out.insert("sides".into(),    Value::Num(sides as f64));
                out.insert("modifier".into(), Value::Num(modifier as f64));
                out.insert("values".into(),   Value::Array(vec![Value::Num(a as f64), Value::Num(b as f64)]));
                out.insert("kept".into(),     Value::Array(vec![Value::Num(chosen as f64)]));
                out.insert("dropped".into(),  Value::Array(vec![Value::Num(dropped_val as f64)]));
                out.insert("sum".into(),      Value::Num(chosen as f64));
                out.insert("total".into(),    Value::Num(total as f64));
                out.insert("adv".into(),      Value::Bool(adv));
                out.insert("dis".into(),      Value::Bool(dis));
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Num(x as f64)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Num(lo as f64)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Num(hi as f64)); }
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

                let mut kept_sum: i64 = kept_vals.iter().sum();
                let mut total = kept_sum + modifier;
                if let (Some(lo), Some(hi)) = (clamp_lo, clamp_hi) {
                    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
                    if total < lo { total = lo; }
                    if total > hi { total = hi; }
                }

                let mut out = BTreeMap::<String, Value>::new();
                out.insert("count".into(),    Value::Num(count as f64));
                out.insert("sides".into(),    Value::Num(sides as f64));
                out.insert("modifier".into(), Value::Num(modifier as f64));
                out.insert("values".into(),   Value::Array(vals.iter().copied().map(|v| Value::Num(v as f64)).collect()));
                out.insert("kept".into(),     Value::Array(kept_vals.iter().copied().map(|v| Value::Num(v as f64)).collect()));
                out.insert("dropped".into(),  Value::Array(dropped_vals.iter().copied().map(|v| Value::Num(v as f64)).collect()));
                out.insert("sum".into(),      Value::Num(kept_sum as f64));
                out.insert("total".into(),    Value::Num(total as f64));
                if keep_high > 0 { out.insert("keep_high".into(), Value::Num(keep_high as f64)); }
                if drop_low  > 0 { out.insert("drop_low".into(),  Value::Num(drop_low  as f64)); }
                if let Some(x) = reroll_eq { out.insert("reroll_eq".into(), Value::Num(x as f64)); }
                if explode { out.insert("explode".into(), Value::Bool(true)); }
                if let Some(lo) = clamp_lo { out.insert("clamp_min".into(), Value::Num(lo as f64)); }
                if let Some(hi) = clamp_hi { out.insert("clamp_max".into(), Value::Num(hi as f64)); }
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
            match s.find(&sub) { Some(i) => Value::Num(i as f64), None => Value::Nil }
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
                    out.push(Value::Num(idx as f64));
                    start = idx + sub.len(); // non-overlapping
                }
                Value::Array(out)
            }
        }
        "count" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let s   = want_str(&args[0], "count")?;
            let sub = want_str(&args[1], "count")?;
            if sub.is_empty() {
                Value::Num(0.0)
            } else {
                let mut n = 0usize;
                let mut start = 0usize;
                while let Some(pos) = s[start..].find(&sub) {
                    n += 1;
                    start = start + pos + sub.len();
                }
                Value::Num(n as f64)
            }
        }

        // ===== Other Stuff =====
        "add" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs,
                _ => return Err(rt("T0401", "add expects an array as first argument", sp.clone())),
            };
            let mut out = arr.clone();
            out.push(args[1].clone());
            Value::Array(out)
        }

        "insert" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs,
                _ => return Err(rt("T0401", "insert expects an array as first argument", sp.clone())),
            };
            let idx = match &args[1] {
                Value::Num(n) if *n >= 0.0 && n.fract() == 0.0 => *n as usize,
                _ => return Err(rt("T0201", "insert index must be a non-negative integer", sp.clone())),
            };
            let mut out = arr.clone();
            if idx > out.len() {
                return Err(rt("R0402", "insert index out of bounds", sp.clone()));
            }
            out.insert(idx, args[2].clone());
            Value::Array(out)
        }

        "shuffle" => {
            if args.len() != 1 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs.clone(),
                _ => return Err(rt("T0401", "shuffle expects an array", sp.clone())),
            };
            let mut out = arr;
            let len = out.len();
            for i in (1..len).rev() {
                let j = rng_index(sess, i + 1);
                out.swap(i, j);
            }
            Value::Array(out)
        }

        "sort" => {
            if args.len() < 1 || args.len() > 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 1..2, got {}", args.len()), sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs.clone(),
                _ => return Err(rt("T0401", "sort expects an array", sp.clone())),
            };
            let desc = if args.len() == 2 {
                match &args[1] {
                    Value::Bool(b) => *b,
                    _ => return Err(rt("T0303", "sort second argument must be boolean (desc)", sp.clone())),
                }
            } else { false };

            if arr.iter().all(|v| matches!(v, Value::Num(_))) {
                let mut xs: Vec<f64> = arr.iter().map(|v| if let Value::Num(n)=v { *n } else { 0.0 }).collect();
                xs.sort_by(|a,b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                if desc { xs.reverse(); }
                Value::Array(xs.into_iter().map(Value::Num).collect())
            } else if arr.iter().all(|v| matches!(v, Value::Str(_))) {
                let mut xs: Vec<String> = arr.iter().map(|v| if let Value::Str(s)=v { s.clone() } else { String::new() }).collect();
                xs.sort();
                if desc { xs.reverse(); }
                Value::Array(xs.into_iter().map(Value::Str).collect())
            } else {
                return Err(rt("T0401", "sort expects an array of numbers or strings", sp.clone()));
            }
        }

        "freq" => {
            arity(1)?;
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "freq expects an array", sp.clone()))?;
            let mut map = std::collections::BTreeMap::<String, i64>::new();
            for v in xs {
                let k = fmt_value_raw(v);
                *map.entry(k).or_insert(0) += 1;
            }
            let mut out = std::collections::BTreeMap::<String, Value>::new();
            for (k, n) in map { out.insert(k, Value::Num(n as f64)); }
            Value::Map(out)
        },

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
            Value::Map(vec![(best_k, Value::Num(best_n as f64))].into_iter().collect())
        },

        "sample_weighted" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let items = match &args[0] {
                Value::Array(xs) => xs,
                _ => return Err(rt("T0401", "sample_weighted expects an array of items", sp.clone())),
            };
            let weights = match &args[1] {
                Value::Array(ws) => ws,
                _ => return Err(rt("T0401", "sample_weighted expects an array of weights", sp.clone())),
            };
            if items.len() != weights.len() {
                return Err(rt("A0402", "items and weights must have same length", sp.clone()));
            }

            let mut ws: Vec<f64> = Vec::with_capacity(weights.len());
            let mut total = 0.0f64;
            for w in weights {
                let n = match w {
                    Value::Num(n) => *n,
                    _ => return Err(rt("T0201", "weights must be numbers", sp.clone())),
                };
                if n < 0.0 {
                    return Err(rt("T0201", "weights must be non-negative", sp.clone()));
                }
                ws.push(n);
                total += n;
            }
            if total <= 0.0 {
                return Err(rt("T0201", "sum of weights must be > 0", sp.clone()));
            }

            let mut r = rng_u01(sess) * total; // r in [0,total)

            // choose an index without early-returning a Result
            let mut choice: Option<Value> = None;
            for (i, w) in ws.iter().enumerate() {
                if r < *w {
                    choice = Some(items[i].clone());
                    break;
                }
                r -= *w;
            }

            choice.unwrap_or_else(|| items.last().cloned().unwrap_or(Value::Nil))
        }

        "map" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "map expects an array as first argument", sp.clone()))?;
            let action = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "map expects the action name as a string", sp.clone())),
            };
            let mut out = Vec::with_capacity(xs.len());
            for v in xs {
                let res = call_action_by_name(sess, &action, vec![v.clone()], sp.clone())?;
                out.push(res);
            }
            // If you've begun returning Seq for producers, do:
            // Value::Seq(gseq::Seq::from_vec(out))
            // Otherwise, keep legacy Array for now:
            Value::Array(out)
        },

        // ---------- Collections: unique / dups ----------
        "unique" => {
            if args.len() != 1 {
                return Err(rt("A0402",
                    format!("wrong number of arguments: expected 1, got {}", args.len()),
                    sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs,
                _ => return Err(rt("T0401", "unique expects an array", sp.clone())),
            };
            let mut out: Vec<Value> = Vec::with_capacity(arr.len());
            for v in arr {
                if !out.iter().any(|x| x == v) { out.push(v.clone()); }
            }
            Value::Array(out)
        }

        "dups" => {
            if args.len() != 1 {
                return Err(rt("A0402",
                    format!("wrong number of arguments: expected 1, got {}", args.len()),
                    sp.clone()));
            }
            let arr = match &args[0] {
                Value::Array(xs) => xs,
                _ => return Err(rt("T0401", "dups expects an array", sp.clone())),
            };
            let mut seen: Vec<Value> = Vec::new();
            let mut dup_only: Vec<Value> = Vec::new();
            for v in arr {
                if seen.iter().any(|x| x == v) {
                    if !dup_only.iter().any(|x| x == v) { dup_only.push(v.clone()); }
                } else {
                    seen.push(v.clone());
                }
            }
            Value::Array(dup_only)
        }

        "keep_where" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let xs_view = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "keep_where expects an array", sp.clone()))?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "keep_where expects the predicate action name as a string", sp.clone())),
            };

            let mut out: Vec<Value> = Vec::new();
            for v in xs_view.iter() {
                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                if want_bool(&ok_v, "keep_where predicate")? { out.push(v.clone()); }
            }
            Value::Array(out)
        },

        "drop_where" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let xs_view = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "drop_where expects an array", sp.clone()))?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "drop_where expects the predicate action name as a string", sp.clone())),
            };

            let mut out: Vec<Value> = Vec::new();
            for v in xs_view.iter() {
                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                if !want_bool(&ok_v, "drop_where predicate")? { out.push(v.clone()); }
            }
            Value::Array(out)
        },

        // ===== Replace & remove =====
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

        "replace_all" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let xs_view = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "replace_all expects an array", sp.clone()))?;
            let oldv = args[1].clone();
            let newv = args[2].clone();

            let mut out: Vec<Value> = Vec::with_capacity(xs_view.iter().count());
            for v in xs_view.iter() {
                if v == &oldv { out.push(newv.clone()); } else { out.push(v.clone()); }
            }
            Value::Array(out)
        },

        "replace_where" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let xs_view = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "replace_where expects an array", sp.clone()))?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "replace_where expects the predicate action name as a string", sp.clone())),
            };
            let withv = args[2].clone();

            let mut out: Vec<Value> = Vec::with_capacity(xs_view.iter().count());
            for v in xs_view.iter() {
                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                if want_bool(&ok_v, "replace_where predicate")? { out.push(withv.clone()); }
                else { out.push(v.clone()); }
            }
            Value::Array(out)
        },

        "replace_first" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let s    = want_str(&args[0], "replace_first")?;
            let from = want_str(&args[1], "replace_first")?;
            let to   = want_str(&args[2], "replace_first")?;
            if from.is_empty() {
                Value::Str(s)
            } else if let Some(pos) = s.find(&from) {
                let mut out = String::with_capacity(s.len());
                out.push_str(&s[..pos]);
                out.push_str(&to);
                out.push_str(&s[pos + from.len()..]);
                Value::Str(out)
            } else {
                Value::Str(s)
            }
        }

        "remove" => {
            if args.len() != 2 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone()));
            }
            let s   = want_str(&args[0], "remove")?;
            let sub = want_str(&args[1], "remove")?;
            if sub.is_empty() {
                Value::Str(s)
            } else {
                Value::Str(s.replace(&sub, ""))
            }
        }

        "usurp_where" => {
            if args.len() != 3 {
                return Err(rt("A0402", format!("wrong number of arguments: expected 3, got {}", args.len()), sp.clone()));
            }
            let xs_view = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "usurp_where expects an array", sp.clone()))?;
            let pred = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(rt("T0205", "usurp_where expects the predicate action name as a string", sp.clone())),
            };
            let withv = args[2].clone();

            // returns only the (old,new) pairs that actually changed
            let mut audit: Vec<Value> = Vec::new();
            for v in xs_view.iter() {
                let ok_v = call_action_by_name(sess, &pred, vec![v.clone()], sp.clone())?;
                if want_bool(&ok_v, "usurp_where predicate")? {
                    audit.push(Value::Pair(Box::new(v.clone()), Box::new(withv.clone())));
                }
            }
            Value::Array(audit)
        },

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
            if args.len() != 1 { return Err(rt("A0402", format!("wrong number of arguments: expected 1, got {}", args.len()), sp.clone())); }
            let s = want_str(&args[0], "chars")?;
            Value::Array(s.chars().map(|c| Value::Str(c.to_string())).collect())
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
        "join" => { // join array of strings with sep
            if args.len() != 2 { return Err(rt("A0402", format!("wrong number of arguments: expected 2, got {}", args.len()), sp.clone())); }
            let xs = as_array_like(&args[0])
                .ok_or_else(|| rt("T0401", "join expects an array", sp.clone()))?;
            let sep = want_str(&args[1], "join")?;
            let mut out = String::new();
            for (i, v) in xs.iter().enumerate() {
                let s = match v { Value::Str(s) => s.clone(), _ => return Err(rt("T0205", "join expects array of strings", sp.clone())) };
                if i > 0 { out.push_str(&sep); }
                out.push_str(&s);
            }
            Value::Str(out)
        },

        // ===== Other transforms =====
        "reverse" => { arity(1)?; map_str_1(&args[0], "reverse", &|s| s.chars().rev().collect())? }
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
                    m.insert("len".into(), Value::Num(xs.len() as f64));
                    m.insert("backend".into(), Value::Str("array(legacy)".into()));
                    Value::Map(m)
                }
                _ => return Err(rt("T0401", "metrics expects a collection", sp.clone())),
            }
        }

        // ----- Unknown -----
        other => return Err(rt("A0401", format!("unknown action '{}'", other), sp.clone())),
    };

    Ok(out)
}

// ===================== Evaluation =====================

fn eval_expr(e: &ast::Expr, sess: &mut Session) -> Result<Value, Diag> {
    match e {
        // ---- Literals & identifiers ----
        ast::Expr::Nil(_) => Ok(Value::Nil),
        ast::Expr::Bool(b, _) => Ok(Value::Bool(*b)),
        ast::Expr::Number(txt, sp) => Ok(Value::Num(parse_num(txt, sp.clone())?)),
        ast::Expr::Str(s, sp) => {
            if s.as_bytes().contains(&b'{') {
                // only simple identifiers are allowed inside { … } at this stage
                let rendered = render_interpolated(s, sess, sp)?;
                Ok(Value::Str(rendered))
            } else {
                Ok(Value::Str(s.clone()))
            }
        }
        ast::Expr::Ident(name, sp) => {
            match sess.get_var(name) {
                Some(v) => Ok(v.clone()),
                None => Err(rt("R0110", format!("unknown identifier '{}'", name), sp.clone())),
            }
        }

        // Plain assignment (ident = expr)
        ast::Expr::Assign(lhs, rhs, _sp) => {
            let name = if let ast::Expr::Ident(n, _) = &**lhs {
                n.clone()
            } else {
                return Err(rt("P0801", "left-hand side of assignment must be a name", span_of_expr(lhs)));
            };
            let v = eval_expr(rhs, sess)?;
            sess.set_var(name, v.clone());
            Ok(v)
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
                (Value::Array(items), Value::Num(n)) => {
                    if n.fract() != 0.0 || n < 0.0 {
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

        // ---- Member access on maps (syntax from parser; you’re not using dot in code, but handle it) ----
        ast::Expr::Member(base, name, sp) => {
            let base_v = eval_expr(base, sess)?;
            match base_v {
                Value::Map(map) => {
                    match map.get(name) {
                        Some(v) => Ok(v.clone()),
                        None => Err(rt("R0403", format!("missing key '{}'", name), sp.clone())),
                    }
                }
                _ => Err(rt("T0402", "member access requires a map", span_of_expr(base))),
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
                                Value::CtrlSkip => continue 'outer, // next iteration
                                Value::CtrlStop => break 'outer,    // exit loop
                                _ => {} // ignore normal values
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

                other_name => {
                    let mut arg_vals = Vec::with_capacity(args.len());
                    for a in args { arg_vals.push(eval_expr(a, sess)?); }
                    call_action_by_name(sess, other_name, arg_vals, sp.clone())
                }
            }
        }

        // ---- Member/optional member calls (receiver becomes first argument) ----
        ast::Expr::Call(base, name, args, sp) => {
            // evaluate receiver and args
            let recv = eval_expr(base, sess)?;
            let mut argv: Vec<Value> = Vec::with_capacity(args.len() + 1);
            argv.push(recv);
            for a in args { argv.push(eval_expr(a, sess)?); }
            // reuse your existing action call path
            call_action_by_name(sess, name, argv, sp.clone())
        }

        ast::Expr::OptCall(base, name, args, sp) => {
            // nil-propagation on receiver
            let recv = eval_expr(base, sess)?;
            if matches!(recv, Value::Nil) {
                return Ok(Value::Nil);
            }
            let mut argv: Vec<Value> = Vec::with_capacity(args.len() + 1);
            argv.push(recv);
            for a in args { argv.push(eval_expr(a, sess)?); }
            call_action_by_name(sess, name, argv, sp.clone())
        }

        ast::Expr::NsCall(_, _, _, sp) => {
            Err(not_impl("Stage 4", "namespace calls", sp.clone()))
        }

        // ---- Prefix operators ----
        ast::Expr::Prefix(op, expr, sp) => {
            match op.as_str() {
                "-" => {
                    let v = eval_expr(expr, sess)?;
                    let n = as_num(v, span_of_expr(expr), "unary '-'")?;
                    Ok(Value::Num(-n))
                }
                "+" => {
                    let v = eval_expr(expr, sess)?;
                    let n = as_num(v, span_of_expr(expr), "unary '+'")?;
                    Ok(Value::Num(n))
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
                "%"  => Ok(Value::Num(as_num(v, span_of_expr(expr), "percent literal")? / 100.0)),
                "**" => Ok(Value::Num(as_num(v, span_of_expr(expr), "postfix square")?.powf(2.0))),
                "//" => {
                    let n = as_num(v, span_of_expr(expr), "postfix sqrt")?;
                    if n < 0.0 { return Err(rt("R0204", "sqrt domain (cannot sqrt negative)", sp.clone())); }
                    Ok(Value::Num(n.sqrt()))
                }
                "!" => {
                    let n = as_num(v, span_of_expr(expr), "factorial")?;
                    if n < 0.0 { return Err(rt("R0202", "factorial requires non-negative integer", sp.clone())); }
                    if n.fract() != 0.0 { return Err(rt("R0203", "factorial requires integer", sp.clone())); }
                    let mut acc: u128 = 1;
                    let k = n as u128;
                    for i in 2..=k { acc = acc.saturating_mul(i); }
                    Ok(Value::Num(acc as f64))
                }
                "^" => Ok(Value::Num(as_num(v, span_of_expr(expr), "ceil")?.ceil())),
                "_" => Ok(Value::Num(as_num(v, span_of_expr(expr), "floor")?.floor())),
                "?" => Ok(Value::Bool(!matches!(v, Value::Nil))),
                _ => Err(rt("R0003", format!("postfix operator '{}' not implemented", op), sp.clone())),
            }
        }

        // ---- Binary & assignment ----
        ast::Expr::Binary(lhs, op, rhs, sp) => {
            match op.as_str() {
                // arithmetic
                "+" => {
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    match (lv, rv) {
                        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                        (Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{a}{b}"))),
                        (a, b) => Err(rt("T0204", format!("'+' expects two numbers or two strings; got {a:?} and {b:?}"), sp.clone())),
                    }
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
                }
                "-" => { let (a,b) = bin_nums(lhs, rhs, sess, "subtraction")?; Ok(Value::Num(a - b)) }
                "*" => { let (a,b) = bin_nums(lhs, rhs, sess, "multiplication")?; Ok(Value::Num(a * b)) }
                "/" => {
                    let (a,b) = bin_nums(lhs, rhs, sess, "division")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    Ok(Value::Num(a / b))
                }
                "%" => {
                    let (a,b) = bin_nums(lhs, rhs, sess, "modulo")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    let q = (a / b).floor();
                    let r = a - q * b;
                    Ok(Value::Num(r))
                }
                "//" => {
                    let (a,b) = bin_nums(lhs, rhs, sess, "floor division")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    Ok(Value::Num((a / b).floor()))
                }
                "**" => { let (a,b) = bin_nums(lhs, rhs, sess, "power")?; Ok(Value::Num(a.powf(b))) }
                "><" => {
                    let (a,b) = bin_nums(lhs, rhs, sess, "divmod")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    let q = (a / b).floor();
                    let r = a - q * b;
                    Ok(Value::Pair(Box::new(Value::Num(q)), Box::new(Value::Num(r))))
                }

                // percent-of family
                "of" => {
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    let lnum = as_num(lv, span_of_expr(lhs), "'of' left")?;
                    let rnum = as_num(rv, span_of_expr(rhs), "'of' right")?;
                    Ok(Value::Num(lnum * rnum))
                }
                "%o" => {
                    let (a,b) = bin_nums(lhs, rhs, sess, "percent-of-other")?;
                    Ok(Value::Num((a / 100.0) * b))
                }

                // comparisons (bool)
                "==" => { let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?; Ok(Value::Bool(lv == rv)) }
                "!=" => { let lv = eval_expr(lhs, sess)?; let rv = eval_expr(rhs, sess)?; Ok(Value::Bool(lv != rv)) }
                "<" | "<=" | ">" | ">=" => {
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    let b = match (op.as_str(), lv, rv) {
                        ("<",  Value::Num(a), Value::Num(b)) => a <  b,
                        ("<=", Value::Num(a), Value::Num(b)) => a <= b,
                        (">",  Value::Num(a), Value::Num(b)) => a >  b,
                        (">=", Value::Num(a), Value::Num(b)) => a >= b,
                        ("<",  Value::Str(a), Value::Str(b)) => a <  b,
                        ("<=", Value::Str(a), Value::Str(b)) => a <= b,
                        (">",  Value::Str(a), Value::Str(b)) => a >  b,
                        (">=", Value::Str(a), Value::Str(b)) => a >= b,
                        _ => return Err(rt("T0302", "comparison requires compatible types", sp.clone())),
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
                    let out = Value::Num(new);
                    sess.set_var(name, out.clone());
                    Ok(out)
                }

                _ => Err(rt("R0004", format!("binary operator '{}' not implemented", op), sp.clone())),
            }
        }
    }
}
