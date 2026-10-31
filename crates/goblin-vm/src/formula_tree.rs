//! DES formulas as a small tree, for running them many times fast.
//!
//! A decision's formula and `where` condition, a `spreads where` condition
//! and an `:objects` predicate run once per candidate object, so their cost
//! per run decides how fast a simulation goes. `Vm::run_formula_in` runs the
//! bytecode on its own operand stack; most formulas are field reads,
//! numbers, comparisons, arithmetic and `and`/`or`, and as a tree of those
//! they run with no operand stack and no values made or dropped.
//!
//! The tree gives exactly what `run_formula_in` gives. Anything it does not
//! handle, or any case where the result is not certain (an erased object, a
//! value of another kind), stops the run, and the caller runs the formula
//! the usual way from the start; nothing here changes anything.

use std::cell::Cell;
use crate::opcode::Opcode;
use crate::value::{FunctionObject, ObjectVal, Value};
use crate::vm::{FormulaEnv, Vm};

/// Where a formula reads a value: an argument, a captured variable or a
/// global.
#[derive(Clone, Copy)]
enum Slot { Arg(u16), Up(u16), Global(u16) }

#[derive(Clone, Copy)]
enum Arith { Add, Sub, Mul, Div }

#[derive(Clone, Copy)]
enum Cmp { Lt, Le, Gt, Ge }

pub(crate) enum Node {
    Const(Value),
    Load(Slot),
    /// `obj >> name`; `hint`: where the field was found last time (objects
    /// of one class share a layout).
    Member(Box<Node>, String, Cell<u32>),
    /// A bare name in a query predicate: `it`'s field of that name when it
    /// has one, else the variable (the node).
    ItOr(String, Box<Node>),
    Arith(Arith, Box<Node>, Box<Node>),
    Neg(Box<Node>),
    Cmp(Cmp, Box<Node>, Box<Node>),
    /// `==` (true) or `!=` (false).
    Eq(bool, Box<Node>, Box<Node>),
    Not(Box<Node>),
    /// `a and b` (true) / `a or b` (false): the left value when it decides.
    Logic(bool, Box<Node>, Box<Node>),
}

pub(crate) struct FormulaTree { root: Node }

/// A value while the tree runs. Objects are read where they live.
#[derive(Clone, Copy)]
enum Tv<'a> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
    /// Any other value (a string, say), as stored.
    Other(&'a Value),
    /// An object in the store (checked to be there), and whether it came
    /// from an argument (an argument that is an object is never a result).
    Obj(&'a ObjectVal, bool),
}

/// Why a run stopped.
enum Stop {
    /// Run it the usual way.
    Fallback,
    /// A query predicate read a field the object doesn't have: no match.
    False,
}

type R<'a> = Result<Tv<'a>, Stop>;

impl FormulaTree {
    /// A part `x >> f == e` (or `e == x >> f`) of the formula's top `and`
    /// chain, where `x` is argument `arg` and `e` doesn't read it: the
    /// formula is false for any object whose `f` differs from `e`.
    /// `has_field`: whether the objects looked at have a field of that name
    /// (a bare name in a predicate reads it then).
    pub(crate) fn field_equal(&self, arg: u16, has_field: &dyn Fn(&str) -> bool) -> Option<(&str, &Node)> {
        fn parts<'a>(n: &'a Node, out: &mut Vec<&'a Node>) {
            match n {
                Node::Logic(true, a, b) => { parts(a, out); parts(b, out); }
                // `a and b` as a condition comes out as `not not (a and b)`.
                Node::Not(x) => match &**x { Node::Not(y) => parts(y, out), _ => out.push(n) },
                other => out.push(other),
            }
        }
        fn field_of(n: &Node, arg: u16) -> Option<&str> {
            match n {
                Node::Member(o, f, _) if matches!(&**o, Node::Load(Slot::Arg(a)) if *a == arg) => Some(f.as_str()),
                _ => None,
            }
        }
        let mut all = Vec::new();
        parts(&self.root, &mut all);
        all.into_iter().find_map(|p| match p {
            Node::Eq(true, a, b) => match (field_of(a, arg), field_of(b, arg)) {
                (Some(f), _) if !reads_arg(b, arg, has_field) => Some((f, &**b)),
                (_, Some(f)) if !reads_arg(a, arg, has_field) => Some((f, &**a)),
                _ => None,
            },
            _ => None,
        })
    }

    /// The tree for `f`, when every step is one the tree handles.
    pub(crate) fn build(f: &FunctionObject) -> Option<FormulaTree> {
        let code = &f.bytecode;
        let (mut nodes, end) = segment(f, 0, code.len(), true)?;
        if end.is_none() || nodes.len() != 1 { return None; }
        Some(FormulaTree { root: nodes.pop()? })
    }
}

/// Turns `code[start..end]` into the values it leaves on the stack.
/// `top`: the whole formula, which ends with Return (the second result says
/// where; None for a segment, which must not return).
fn segment(f: &FunctionObject, start: usize, end: usize, top: bool) -> Option<(Vec<Node>, Option<usize>)> {
    let code = &f.bytecode;
    let mut st: Vec<Node> = Vec::new();
    let mut i = start;
    let bin = |st: &mut Vec<Node>| -> Option<(Box<Node>, Box<Node>)> {
        let b = st.pop()?;
        let a = st.pop()?;
        Some((Box::new(a), Box::new(b)))
    };
    while i < end {
        let op = code[i];
        match op {
            Opcode::LoadConst(c) => st.push(Node::Const(f.constants.get(c as usize)?.clone_fast())),
            Opcode::LoadTrue => st.push(Node::Const(Value::Bool(true))),
            Opcode::LoadFalse => st.push(Node::Const(Value::Bool(false))),
            Opcode::LoadNil => st.push(Node::Const(Value::Nil)),
            Opcode::LoadLocal(s) => st.push(Node::Load(Slot::Arg(s))),
            Opcode::LoadUpvalue(u) => st.push(Node::Load(Slot::Up(u as u16))),
            Opcode::LoadGlobal(g) => st.push(Node::Load(Slot::Global(g))),
            Opcode::GetMember(c) => {
                let Value::Str(name) = f.constants.get(c as usize)? else { return None };
                let obj = st.pop()?;
                st.push(Node::Member(Box::new(obj), name.to_string(), Cell::new(u32::MAX)));
            }
            // The field, or else the load in the next `off` steps.
            Opcode::LoadItFieldOrJump(c, off) => {
                let Value::Str(name) = f.constants.get(c as usize)? else { return None };
                let end_of = i + 1 + off as usize;
                if off < 1 || end_of > end { return None; }
                let (mut alt, ret) = segment(f, i + 1, end_of, false)?;
                if ret.is_some() || alt.len() != 1 { return None; }
                st.push(Node::ItOr(name.to_string(), Box::new(alt.pop()?)));
                i = end_of;
                continue;
            }
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                let (a, b) = bin(&mut st)?;
                let k = match op { Opcode::Add => Arith::Add, Opcode::Sub => Arith::Sub, Opcode::Mul => Arith::Mul, _ => Arith::Div };
                st.push(Node::Arith(k, a, b));
            }
            Opcode::Neg => { let a = st.pop()?; st.push(Node::Neg(Box::new(a))); }
            Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge => {
                let (a, b) = bin(&mut st)?;
                let k = match op { Opcode::Lt => Cmp::Lt, Opcode::Le => Cmp::Le, Opcode::Gt => Cmp::Gt, _ => Cmp::Ge };
                st.push(Node::Cmp(k, a, b));
            }
            Opcode::Eq | Opcode::Ne => {
                let (a, b) = bin(&mut st)?;
                st.push(Node::Eq(matches!(op, Opcode::Eq), a, b));
            }
            Opcode::Not => { let a = st.pop()?; st.push(Node::Not(Box::new(a))); }
            // `a and b`: Dup, JumpIfFalse past b, Pop, b (`or`: JumpIfTrue).
            Opcode::Dup => {
                let (and, off) = match code.get(i + 1)? {
                    Opcode::JumpIfFalse(off) => (true, *off),
                    Opcode::JumpIfTrue(off) => (false, *off),
                    _ => return None,
                };
                if !matches!(code.get(i + 2)?, Opcode::Pop) { return None; }
                let target = (i as isize + 2 + off as isize) as usize;
                if target <= i + 3 || target > end { return None; }
                let (mut rhs, ret) = segment(f, i + 3, target, false)?;
                if ret.is_some() || rhs.len() != 1 { return None; }
                let lhs = st.pop()?;
                st.push(Node::Logic(and, Box::new(lhs), Box::new(rhs.pop()?)));
                i = target;
                continue;
            }
            Opcode::Return if top => return Some((st, Some(i))),
            _ => return None,
        }
        i += 1;
    }
    if top { None } else { Some((st, None)) }
}

fn reads_arg(n: &Node, arg: u16, has_field: &dyn Fn(&str) -> bool) -> bool {
    let r = |x: &Node| reads_arg(x, arg, has_field);
    match n {
        Node::Const(_) => false,
        Node::Load(Slot::Arg(a)) => *a == arg,
        Node::Load(_) => false,
        // `it` is argument 0 of a query predicate.
        Node::ItOr(name, x) => (arg == 0 && has_field(name)) || r(x),
        Node::Member(x, _, _) | Node::Neg(x) | Node::Not(x) => r(x),
        Node::Arith(_, a, b) | Node::Cmp(_, a, b) | Node::Eq(_, a, b) | Node::Logic(_, a, b) => r(a) || r(b),
    }
}

fn plain(v: Tv<'_>) -> Option<Value> {
    match v {
        Tv::Int(i) => Some(Value::Int(i)),
        Tv::Float(f) => Some(Value::Float(f)),
        Tv::Bool(b) => Some(Value::Bool(b)),
        Tv::Nil => Some(Value::Nil),
        _ => None,
    }
}

/// `==` on two plain values, as `values_equal` gives it (a float equals
/// a float with the same bits; an int equals a float of the same value).
fn eq_plain(a: Tv<'_>, b: Tv<'_>) -> Option<bool> {
    Some(match (a, b) {
        (Tv::Int(x), Tv::Int(y)) => x == y,
        (Tv::Float(x), Tv::Float(y)) => x.to_bits() == y.to_bits(),
        (Tv::Int(x), Tv::Float(y)) | (Tv::Float(y), Tv::Int(x)) => x as f64 == y,
        (Tv::Bool(x), Tv::Bool(y)) => x == y,
        (Tv::Nil, Tv::Nil) => true,
        (Tv::Int(_) | Tv::Float(_) | Tv::Bool(_) | Tv::Nil, Tv::Int(_) | Tv::Float(_) | Tv::Bool(_) | Tv::Nil) => false,
        _ => return None,
    })
}

/// `Value::is_truthy` of a plain value.
fn truthy(v: Tv<'_>) -> Option<bool> {
    Some(match v {
        Tv::Int(n) => n != 0,
        Tv::Float(x) => x != 0.0,
        Tv::Bool(b) => b,
        Tv::Nil => false,
        _ => return None,
    })
}

impl Vm {
    /// Runs `tree` with these arguments, as `run_formula_in` would. None:
    /// run it that way instead.
    /// `at`: per slot (arguments, then captured variables, then globals),
    /// where its object sits in the store, `usize::MAX - 1` when not known.
    pub(crate) fn run_formula_tree(&self, tree: &FormulaTree, args: &[(&str, &Value)], at: &mut [usize], env: &FormulaEnv) -> Option<Value> {
        self.run_formula_part(&tree.root, args, at, env)
    }

    /// Runs one part of a tree (see `FormulaTree::field_equal`).
    pub(crate) fn run_formula_part(&self, n: &Node, args: &[(&str, &Value)], at: &mut [usize], env: &FormulaEnv) -> Option<Value> {
        match self.tree_eval(n, args, at, env) {
            Ok(v) => match v {
                Tv::Obj(..) => None,
                Tv::Other(v) => Some(v.clone_fast()),
                v => plain(v),
            },
            Err(Stop::False) => Some(Value::Bool(false)),
            Err(Stop::Fallback) => None,
        }
    }

    /// The key (see `store::field_key`) of what one part of a tree gives,
    /// an object included (it keys by id). None: work it out the usual way.
    pub(crate) fn run_formula_key(&self, n: &Node, args: &[(&str, &Value)], at: &mut [usize], env: &FormulaEnv) -> Option<crate::store::FieldKey> {
        match self.tree_eval(n, args, at, env) {
            Ok(Tv::Obj(o, _)) => Some(crate::store::FieldKey::Obj(o.uuid.to_string())),
            Ok(Tv::Other(v)) => crate::store::field_key(v),
            Ok(v) => crate::store::field_key(&plain(v)?),
            Err(Stop::False) => crate::store::field_key(&Value::Bool(false)),
            Err(Stop::Fallback) => None,
        }
    }

    /// An object a value refers to, if it is in the store.
    fn tree_obj<'a>(&'a self, v: &'a Value, pos: Option<&mut usize>, arg: bool) -> R<'a> {
        let store = &self.session.object_store;
        match v {
            Value::Object(o) => Ok(Tv::Obj(o, arg)),
            Value::Ref(uuid) => {
                if let Some(pos) = pos {
                    if let Some((k, Value::Object(o))) = store.get_index(*pos) {
                        if k == uuid { return Ok(Tv::Obj(o, arg)); }
                    }
                    let (i, _, o) = store.get_full(uuid.as_str()).ok_or(Stop::Fallback)?;
                    *pos = i;
                    return match o { Value::Object(o) => Ok(Tv::Obj(o, arg)), _ => Err(Stop::Fallback) };
                }
                match store.get(uuid.as_str()) {
                    Some(Value::Object(o)) => Ok(Tv::Obj(o, arg)),
                    _ => Err(Stop::Fallback),
                }
            }
            _ => Err(Stop::Fallback),
        }
    }

    /// The value field `name` of the object `obj` gives holds, as stored.
    fn tree_field<'a>(&'a self, obj: &'a Node, name: &str, hint: &Cell<u32>, args: &[(&str, &'a Value)], at: &mut [usize], env: &'a FormulaEnv) -> Result<&'a Value, Stop> {
        let Tv::Obj(o, _) = self.tree_eval(obj, args, at, env)? else { return Err(Stop::Fallback) };
        Ok(match o.fields.get_index(hint.get() as usize) {
            Some((k, v)) if k == name => v,
            _ => match o.fields.get_full(name) {
                Some((i, _, v)) => { hint.set(i as u32); v }
                None => return Err(if env.missing_field_is_false { Stop::False } else { Stop::Fallback }),
            },
        })
    }

    /// A field's value as `tree_eval` gives it.
    fn tree_field_tv<'a>(&'a self, v: &'a Value) -> R<'a> {
        Ok(match v {
            Value::Int(x) => Tv::Int(*x),
            Value::Float(x) => Tv::Float(*x),
            Value::Bool(x) => Tv::Bool(*x),
            Value::Nil => Tv::Nil,
            Value::Frozen(_) => return Err(Stop::Fallback),
            Value::Ref(u) if u.starts_with(crate::vm::OVERLAY_RECORD) => return Err(Stop::Fallback),
            Value::Ref(_) => return self.tree_obj(v, None, false),
            other => Tv::Other(other),
        })
    }

    fn tree_eval<'a>(&'a self, n: &'a Node, args: &[(&str, &'a Value)], at: &mut [usize], env: &'a FormulaEnv) -> R<'a> {
        let ups_at = args.len();
        let globals_at = ups_at + env.upvalues.len();
        Ok(match n {
            Node::Const(v) => match v {
                Value::Int(i) => Tv::Int(*i),
                Value::Float(f) => Tv::Float(*f),
                Value::Bool(b) => Tv::Bool(*b),
                Value::Nil => Tv::Nil,
                other => Tv::Other(other),
            },
            Node::Load(slot) => {
                let (i, v): (usize, &'a Value) = match *slot {
                    Slot::Arg(s) => { let s = s as usize; (s, args.get(s).ok_or(Stop::Fallback)?.1) }
                    Slot::Up(u) => (ups_at + u as usize, env.upvalues.get(u as usize).ok_or(Stop::Fallback)?),
                    Slot::Global(g) => {
                        let k = env.globals.iter().position(|(n, _)| *n == g).ok_or(Stop::Fallback)?;
                        (globals_at + k, &env.globals[k].1)
                    }
                };
                match v {
                    Value::Int(x) => Tv::Int(*x),
                    Value::Float(x) => Tv::Float(*x),
                    Value::Bool(x) => Tv::Bool(*x),
                    Value::Nil => Tv::Nil,
                    Value::Object(_) | Value::Ref(_) => return self.tree_obj(v, at.get_mut(i), true),
                    _ => return Err(Stop::Fallback),
                }
            }
            Node::Member(obj, name, hint) => return self.tree_field_tv(self.tree_field(obj, name, hint, args, at, env)?),
            Node::ItOr(name, alt) => {
                // As `run_formula_in`: `it` (argument 0) when it is an object
                // that has the field.
                let it = args.first().ok_or(Stop::Fallback)?.1;
                let obj = match it {
                    Value::Object(o) => Some(&**o),
                    Value::Ref(uuid) => match self.session.object_store.get(uuid.as_str()) {
                        Some(Value::Object(o)) => Some(&**o),
                        _ => None,
                    },
                    _ => None,
                };
                match obj.and_then(|o| o.fields.get(name.as_str())) {
                    Some(Value::Frozen(_) | Value::Ref(_)) => return Err(Stop::Fallback),
                    Some(v) => match v {
                        Value::Int(x) => Tv::Int(*x),
                        Value::Float(x) => Tv::Float(*x),
                        Value::Bool(x) => Tv::Bool(*x),
                        Value::Nil => Tv::Nil,
                        other => Tv::Other(other),
                    },
                    None => self.tree_eval(alt, args, at, env)?,
                }
            }
            Node::Arith(k, a, b) => {
                let a = self.tree_eval(a, args, at, env)?;
                let b = self.tree_eval(b, args, at, env)?;
                let num = |v: Tv| match v { Tv::Int(n) => Some(n as f64), Tv::Float(x) => Some(x), _ => None };
                match (k, a, b) {
                    (Arith::Add, Tv::Int(x), Tv::Int(y)) => Tv::Int(x.wrapping_add(y)),
                    (Arith::Add, Tv::Float(x), Tv::Float(y)) => Tv::Float(x + y),
                    (Arith::Add, Tv::Int(x), Tv::Float(y)) => Tv::Float(x as f64 + y),
                    (Arith::Add, Tv::Float(x), Tv::Int(y)) => Tv::Float(x + y as f64),
                    (Arith::Sub, Tv::Int(x), Tv::Int(y)) => Tv::Int(x.checked_sub(y).ok_or(Stop::Fallback)?),
                    (Arith::Mul, Tv::Int(x), Tv::Int(y)) => Tv::Int(x.checked_mul(y).ok_or(Stop::Fallback)?),
                    (Arith::Sub | Arith::Mul, Tv::Int(_) | Tv::Float(_), Tv::Int(_) | Tv::Float(_)) => {
                        let (x, y) = (num(a).ok_or(Stop::Fallback)?, num(b).ok_or(Stop::Fallback)?);
                        Tv::Float(if matches!(k, Arith::Sub) { x - y } else { x * y })
                    }
                    (Arith::Div, Tv::Int(_) | Tv::Float(_), Tv::Int(_) | Tv::Float(_)) => {
                        let (a, b) = (plain(a).ok_or(Stop::Fallback)?, plain(b).ok_or(Stop::Fallback)?);
                        match crate::vm::num_div(&a, &b).map_err(|_| Stop::Fallback)? {
                            Value::Int(n) => Tv::Int(n),
                            Value::Float(x) => Tv::Float(x),
                            _ => return Err(Stop::Fallback),
                        }
                    }
                    _ => return Err(Stop::Fallback),
                }
            }
            Node::Neg(a) => match self.tree_eval(a, args, at, env)? {
                Tv::Int(x) => Tv::Int(x.checked_neg().ok_or(Stop::Fallback)?),
                Tv::Float(x) => Tv::Float(-x),
                _ => return Err(Stop::Fallback),
            },
            Node::Cmp(k, a, b) => {
                let a = self.tree_eval(a, args, at, env)?;
                let b = self.tree_eval(b, args, at, env)?;
                // As `compare_values` orders numbers (an unordered pair is equal).
                use std::cmp::Ordering;
                let o = match (a, b) {
                    (Tv::Int(x), Tv::Int(y)) => x.cmp(&y),
                    (Tv::Float(x), Tv::Float(y)) => x.partial_cmp(&y).unwrap_or(Ordering::Equal),
                    (Tv::Int(x), Tv::Float(y)) => (x as f64).partial_cmp(&y).unwrap_or(Ordering::Equal),
                    (Tv::Float(x), Tv::Int(y)) => x.partial_cmp(&(y as f64)).unwrap_or(Ordering::Equal),
                    _ => return Err(Stop::Fallback),
                };
                Tv::Bool(match k {
                    Cmp::Lt => o == Ordering::Less,
                    Cmp::Le => o != Ordering::Greater,
                    Cmp::Gt => o == Ordering::Greater,
                    Cmp::Ge => o != Ordering::Less,
                })
            }
            Node::Eq(is_eq, a, b) => {
                // Two fields that hold the same object id are equal without
                // looking the object up (`target >> owner == @owner`).
                let (a, b) = match (&**a, &**b) {
                    (Node::Member(ao, an, ah), Node::Member(bo, bn, bh)) => {
                        let x = self.tree_field(ao, an, ah, args, at, env)?;
                        let y = self.tree_field(bo, bn, bh, args, at, env)?;
                        if let (Value::Ref(x), Value::Ref(y)) = (x, y) {
                            if x == y && !x.starts_with(crate::vm::OVERLAY_RECORD) { return Ok(Tv::Bool(*is_eq)); }
                        }
                        (self.tree_field_tv(x)?, self.tree_field_tv(y)?)
                    }
                    _ => (self.tree_eval(a, args, at, env)?, self.tree_eval(b, args, at, env)?),
                };
                let eq = match (a, b) {
                    (Tv::Obj(x, _), Tv::Obj(y, _)) => x.uuid == y.uuid,
                    (Tv::Obj(..), Tv::Nil) | (Tv::Nil, Tv::Obj(..)) => false,
                    (Tv::Obj(..), _) | (_, Tv::Obj(..)) => return Err(Stop::Fallback),
                    (Tv::Other(x), Tv::Other(y)) => match (x, y) {
                        (Value::Str(x), Value::Str(y)) => x == y,
                        _ => return Err(Stop::Fallback),
                    },
                    (Tv::Other(Value::Str(_)), v) | (v, Tv::Other(Value::Str(_))) if plain(v).is_some() => false,
                    (Tv::Other(_), _) | (_, Tv::Other(_)) => return Err(Stop::Fallback),
                    (a, b) => eq_plain(a, b).ok_or(Stop::Fallback)?,
                };
                Tv::Bool(eq == *is_eq)
            }
            Node::Not(a) => {
                Tv::Bool(!truthy(self.tree_eval(a, args, at, env)?).ok_or(Stop::Fallback)?)
            }
            Node::Logic(and, a, b) => {
                let l = self.tree_eval(a, args, at, env)?;
                if truthy(l).ok_or(Stop::Fallback)? != *and { l } else { self.tree_eval(b, args, at, env)? }
            }
        })
    }
}
