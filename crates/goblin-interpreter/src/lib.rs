// goblin-interp/src/lib.rs
//! Stage 1 interpreter that works with the *real* Goblin AST.
//! Scope: numbers, strings, basic unary/postfix/binary math, percent family,
//! and `v(n)` / `say(x)` via FreeCall.
//! Anything else (classes, fields, actions, member/index/calls beyond FreeCall)
//! returns a clean "not implemented in Stage 1" diagnostic.

use std::fmt;

use goblin_diagnostics::Span;
use goblin_ast as ast;

// ===================== Public API =====================

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Pair(Box<Value>, Box<Value>), // for >< (divmod) result
    Unit,                         // side-effect result (e.g., say)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(x) => {
                if x.is_finite() && x.fract() == 0.0 {
                    write!(f, "{}", *x as i64)
                } else {
                    let s = format!("{}", x);
                    write!(f, "{}", s.trim_end_matches('0').trim_end_matches('.'))
                }
            }
            // REPL echo uses a repr-ish format (quoted)
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Pair(a, b) => write!(f, "({}, {})", a, b),
            Value::Unit => Ok(()),
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
    history: Vec<Value>, // v(n) lookup; 1-based externally
}

impl Session {
    pub fn new() -> Self { Self::default() }

    /// Evaluate a whole module (returns last expression value if any).
    /// Stage 1: executes only top-level `Stmt::Expr`; skips class/action decls.
    pub fn eval_module(&mut self, m: &ast::Module) -> Result<Option<Value>, Diag> {
        let mut last = None;
        for stmt in &m.items {
            if let ast::Stmt::Expr(e) = stmt {
                let v = eval_expr(e, self)?;
                last = Some(v);
            }
        }
        Ok(last)
    }

    /// Parse a single REPL line using the real parser, evaluate it, and return the value.
    /// Only non-Unit values are recorded into history (so `say` doesn't pollute v(n)).
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

        // 3) Evaluate; record only non-Unit values
        match self.eval_module(&module)? {
            Some(v) => {
                if !matches!(v, Value::Unit) {
                    self.history.push(v.clone());
                }
                Ok(v)
            }
            None => Err(Diag {
                code: "R0005".to_string(),
                message: "no expression to evaluate".into(),
                span: Span::new("<repl>", 0, 0, 1, 1, 1, 1),
            }),
        }
    }

    /// Evaluate a single expression node and record only non-Unit results.
    pub fn eval_expr(&mut self, e: &ast::Expr) -> Result<Value, Diag> {
        let v = eval_expr(e, self)?;
        if !matches!(v, Value::Unit) {
            self.history.push(v.clone());
        }
        Ok(v)
    }

    pub fn history_len(&self) -> usize { self.history.len() }
    pub fn get(&self, idx_1_based: usize) -> Option<&Value> { self.history.get(idx_1_based.saturating_sub(1)) }
}

// ===================== Evaluation =====================

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
        Value::Str(s) => s.clone(), // raw, unquoted
        Value::Num(n) => fmt_num_trim(*n),
        Value::Pair(a, b) => format!("({}, {})", fmt_value_raw(a), fmt_value_raw(b)),
        Value::Unit => String::new(),
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

fn eval_expr(e: &ast::Expr, sess: &mut Session) -> Result<Value, Diag> {
    match e {
        // ---- Literals & identifiers ----
        ast::Expr::Nil(sp) => Err(not_impl("Stage 1", "nil literal", sp.clone())),
        ast::Expr::Bool(b, _) => Ok(Value::Str(if *b { "true" } else { "false" }.to_string())),
        ast::Expr::Number(txt, sp) => Ok(Value::Num(parse_num(txt, sp.clone())?)),
        ast::Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        ast::Expr::Ident(name, sp) => Err(rt("R0101", format!("unbound identifier '{}'", name), sp.clone())),

        // ---- Collections (not in Stage 1) ----
        ast::Expr::Array(_, sp) => Err(not_impl("Stage 1", "arrays", sp.clone())),
        ast::Expr::Object(_, sp) => Err(not_impl("Stage 1", "objects", sp.clone())),

        // ---- Property/index (not in Stage 1) ----
        ast::Expr::Member(_, _, sp) | ast::Expr::OptMember(_, _, sp) => Err(not_impl("Stage 1", "member access", sp.clone())),
        ast::Expr::Index(_, _, sp) => Err(not_impl("Stage 1", "indexing", sp.clone())),

        // ---- Calls ----
        ast::Expr::Call(_, _, _, sp) | ast::Expr::OptCall(_, _, _, sp) | ast::Expr::NsCall(_, _, _, sp) => {
            Err(not_impl("Stage 1", "method/namespace calls", sp.clone()))
        }

        // Free calls: allow v(n) and say(x)
        ast::Expr::FreeCall(name, args, sp) => {
            let nm = name.as_str();
            match nm {
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
                        None => Err(rt(
                            "R0101",
                            format!("no value at v({}); session has {}", idx, sess.history_len()),
                            sp.clone(),
                        )),
                    }
                }
                "say" => {
                    // zero args -> newline; one arg -> print raw form; more -> error (Stage 1)
                    if args.len() > 1 {
                        return Err(rt("R0001", "say(...) takes at most one argument in Stage 1", sp.clone()));
                    }
                    let printed = if let Some(e) = args.get(0) {
                        let v = eval_expr(e, sess)?;
                        let s = fmt_value_raw(&v);   // no quotes for strings; trimmed numbers
                        println!("{}", s);
                        Value::Unit
                    } else {
                        println!();
                        Value::Unit
                    };
                    Ok(printed)
                }
                _ => Err(rt("R0001", format!("free call '{}' is not implemented in Stage 1", nm), sp.clone())),
            }
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
                _ => Err(rt("R0002", format!("prefix operator '{}' not implemented in Stage 1", op), sp.clone())),
            }
        }

        // ---- Postfix operators ----
        ast::Expr::Postfix(expr, op, sp) => {
            let v = eval_expr(expr, sess)?;
            match op.as_str() {
                "%" => Ok(Value::Num(as_num(v, span_of_expr(expr), "percent literal")? / 100.0)),
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
                    for i in 2..=k {
                        acc = acc.saturating_mul(i);
                    }
                    Ok(Value::Num(acc as f64))
                }
                "^" => Ok(Value::Num(as_num(v, span_of_expr(expr), "ceil")?.ceil())),
                "_" => Ok(Value::Num(as_num(v, span_of_expr(expr), "floor")?.floor())),
                _ => Err(rt("R0003", format!("postfix operator '{}' not implemented in Stage 1", op), sp.clone())),
            }
        }

        // ---- Binary operators ----
        ast::Expr::Binary(lhs, op, rhs, sp) => {
            match op.as_str() {
                "+" => {
                    // numeric add OR string concat
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    match (lv, rv) {
                        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                        (Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{a}{b}"))),
                        (a, b) => Err(rt("T0204", format!("'+' expects two numbers or two strings; got {a:?} and {b:?}"), sp.clone())),
                    }
                }
                "-" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "subtraction")?;
                    Ok(Value::Num(a - b))
                }
                "*" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "multiplication")?;
                    Ok(Value::Num(a * b))
                }
                "/" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "division")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    Ok(Value::Num(a / b))
                }
                "%" => {
                    // Python-style modulo paired with floor-div
                    let (a, b) = bin_nums(lhs, rhs, sess, "modulo")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    let q = (a / b).floor();
                    let r = a - q * b;
                    Ok(Value::Num(r))
                }
                "//" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "floor division")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    Ok(Value::Num((a / b).floor()))
                }
                "**" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "power")?;
                    Ok(Value::Num(a.powf(b)))
                }
                "><" => {
                    let (a, b) = bin_nums(lhs, rhs, sess, "divmod")?;
                    if b == 0.0 { return Err(rt("R0201", "divide by zero", sp.clone())); }
                    let q = (a / b).floor();
                    let r = a - q * b;
                    Ok(Value::Pair(Box::new(Value::Num(q)), Box::new(Value::Num(r))))
                }
                // Percent-of family
                "of" => {
                    // left should already be a percent-literal (e.g., 25%) → multiply directly
                    let lv = eval_expr(lhs, sess)?;
                    let rv = eval_expr(rhs, sess)?;
                    let lnum = as_num(lv, span_of_expr(lhs), "'of' left")?;
                    let rnum = as_num(rv, span_of_expr(rhs), "'of' right")?;
                    Ok(Value::Num(lnum * rnum))
                }
                "%o" => {
                    // N %o E  == (N/100) * E
                    let (a, b) = bin_nums(lhs, rhs, sess, "percent-of-other")?;
                    Ok(Value::Num((a / 100.0) * b))
                }

                op => Err(rt("R0004", format!("binary operator '{}' not implemented in Stage 1", op), sp.clone())),
            }
        }

        ast::Expr::Assign(_, _, sp) => Err(not_impl("Stage 1", "assignment", sp.clone())),
    }
}

// ===================== End =====================