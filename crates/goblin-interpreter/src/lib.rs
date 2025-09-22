// goblin-interpreter/src/lib.rs
//! Stage 1+ interpreter on the *real* Goblin AST.
//! Scope implemented now (matches your current repo):
//!   - numbers, strings (lexer provides escapes), booleans, nil
//!   - unary +/-
//!   - postfix: %, !, ^, _, ** (square), // (sqrt)
//!   - infix: + - * / // % ** ><
//!   - percent-of family: N% (literal), N% of E, N%o E
//!   - comparisons: == != < <= > >=
//!   - logic: and/or (&&, <>), coalesce ??
//!   - assignment: name = expr, and compound assigns (+= -= *= /= //= %= **=)
//!   - free calls: v(n), say(expr)
//!
//! Not implemented yet (as per your current AST + smokes):
//!   - string interpolation (AST has no StrInterp)
//!   - arrays/objects/member/index (return clear "not implemented in Stage 2")

use std::collections::BTreeMap;
use std::fmt;

use goblin_ast as ast;
use goblin_diagnostics::Span;

// ===================== Public API =====================

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Nil,
    Pair(Box<Value>, Box<Value>), // for >< (divmod)
    Unit,                         // printed as nothing (used by `say`)
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
            Value::Str(s) => write!(f, "{:?}", s), // repr-style for REPL echo
            Value::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Nil => write!(f, "nil"),
            Value::Pair(a, b) => write!(f, "({}, {})", a, b),
            Value::Unit => Ok(()), // prints nothing if displayed (CLI suppresses extra line)
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
    history: Vec<Value>,          // v(n) lookup; 1-based externally
    env: BTreeMap<String, Value>, // Stage 2: single global env
}
impl Session {
    pub fn new() -> Self { Self::default() }

    /// Evaluate a whole module (returns last expression value if any).
    /// Stage 1/2: executes only top-level `Stmt::Expr`; skips class/action decls.
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

    /// Parse + eval a single REPL line using the real parser.
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
        Value::Str(s) => s.clone(),                           // raw (no quotes)
        Value::Num(n) => fmt_num_trim(*n),
        Value::Bool(b) => if *b { "true".into() } else { "false".into() },
        Value::Nil => "nil".into(),
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
                    return Err(rt(
                        "P0604",
                        "There's an unclosed '{' in this string.",
                        sp.clone(),
                    ));
                }
                let inner = s[start..j].trim();
                if !is_ident(inner) {
                    return Err(rt(
                        "P0103",
                        "Expected a name (identifier) inside { ... }",
                        sp.clone(),
                    ));
                }
                match sess.env.get(inner) {
                    Some(v) => out.push_str(&fmt_value_raw(v)),
                    None => {
                        return Err(rt(
                            "R0110",
                            format!("unknown identifier '{}'", inner),
                            sp.clone(),
                        ))
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

// ===================== Evaluation =====================

fn eval_expr(e: &ast::Expr, sess: &mut Session) -> Result<Value, Diag> {
    match e {
        // ---- Literals & identifiers ----
        ast::Expr::Nil(_) => Ok(Value::Nil),
        ast::Expr::Bool(b, _) => Ok(Value::Bool(*b)),
        ast::Expr::Number(txt, sp) => Ok(Value::Num(parse_num(txt, sp.clone())?)),
        ast::Expr::Str(s, sp) => {
            // If the string has braces, run the lightweight interpolator.
            if s.as_bytes().contains(&b'{') || s.as_bytes().contains(&b'}') {
                let rendered = render_interpolated(s, sess, sp)?;
                Ok(Value::Str(rendered))
            } else {
                Ok(Value::Str(s.clone()))
            }
        }
        ast::Expr::Ident(name, sp) => {
            match sess.env.get(name) {
                Some(v) => Ok(v.clone()),
                None => Err(rt("R0110", format!("unknown identifier '{}'", name), sp.clone())),
            }
        }

        // ---- Collections (not in Stage 2 yet) ----
        ast::Expr::Array(_, sp) => Err(not_impl("Stage 2", "arrays", sp.clone())),
        ast::Expr::Object(_, sp) => Err(not_impl("Stage 2", "objects", sp.clone())),

        // ---- Property/index (not in Stage 2 yet) ----
        ast::Expr::Member(_, _, sp) | ast::Expr::OptMember(_, _, sp) => Err(not_impl("Stage 2", "member access", sp.clone())),
        ast::Expr::Index(_, _, sp) => Err(not_impl("Stage 2", "indexing", sp.clone())),

        // ---- Calls ----
        ast::Expr::Call(_, _, _, sp) | ast::Expr::OptCall(_, _, _, sp) | ast::Expr::NsCall(_, _, _, sp) => {
            Err(not_impl("Stage 2", "method/namespace calls", sp.clone()))
        }

        // Free calls: v(n) (history) and say(expr)
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
                    let printed = if args.is_empty() {
                        Value::Unit
                    } else {
                        eval_expr(&args[0], sess)?
                    };

                    match printed {
                        Value::Str(s) => {
                            // Interpolate at print time, using current env
                            let rendered = render_interpolated(&s, sess, &span_of_expr(&args[0]))?;
                            println!("{}", rendered);
                        }
                        other => {
                            println!("{}", fmt_value_raw(&other));
                        }
                    }
                    Ok(Value::Unit)
                }
                _ => Err(rt("R0001", format!("free call '{}' is not implemented in Stage 2", nm), sp.clone())),
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

                // comparisons
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

                // compound assigns on identifiers only (parser must produce Binary for them)
                "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**=" => {
                    let name = if let ast::Expr::Ident(n, _) = &**lhs {
                        n.clone()
                    } else {
                        return Err(rt("P0801", "left-hand side of compound assign must be a name", span_of_expr(lhs)));
                    };
                    let old = match sess.env.get(&name) {
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
                    sess.env.insert(name, out.clone());
                    Ok(out)
                }

                _ => Err(rt("R0004", format!("binary operator '{}' not implemented", op), sp.clone())),
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
            sess.env.insert(name, v.clone());
            Ok(v)
        }
    }
}
