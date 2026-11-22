//! The register VM (selected with `GOBLIN_ENGINE=reg` until it replaces
//! the stack VM).
//!
//! A call's registers are its window of `Vm::stack`: register r of a call
//! is `stack[base + r]`, its variables first, then what its expressions
//! work out. Each function's bytecode is turned once into register code:
//! instructions that name the registers they read and write (`ADD x x t`,
//! `LT.JMP i n exit`), so a line like `x |= x + i * 2 - 1` is three steps
//! instead of nine pushes and pops, and nothing goes through the operand
//! stack. Every register instruction covers a run of bytecode steps: when
//! its values are not the plain numbers it is made for (a string, an
//! object, a value in a shared cell, an overflow), those steps run as they
//! always have, so the language behaves exactly the same. Steps that are
//! not turned into register code (rare ones: overlays, imports, DES
//! declarations, ...) run as they are, on the same registers.

use super::*;
use crate::opcode::Opcode as O;

/// Whether the register VM runs (`GOBLIN_ENGINE=reg`).
pub(crate) fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("GOBLIN_ENGINE").is_ok_and(|v| v == "reg"))
}

/// A plain number while register code runs.
#[derive(Clone, Copy, Debug)]
pub(crate) enum N { I(i64), F(f64), B(bool) }

/// Where an instruction reads a value: a variable (register of the
/// call's window), a constant (`Prog::k`), or a scratch register of the
/// current line.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Src { L(u16), K(u16), T(u8) }

/// Where an instruction puts its result.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Dst {
    /// A scratch register, read by a later instruction of the same line.
    T(u8),
    /// Pushed for the bytecode steps after the line.
    Push,
    /// Into variable d, as `GuardRebindLocal(d)` + `StoreLocal(d)` do.
    Rebind(u16),
    /// Into variable d, as `StoreLocal(d)` / `BindLocal(d)` do.
    Store(u16),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Bin { Add, Sub, Mul, Div, Lt, Le, Gt, Ge, Eq, Ne, AddInts }

#[derive(Clone, Copy, Debug)]
pub(crate) enum Ins {
    /// The bytecode step at `at`, as the stack VM does it.
    Old { at: u32 },
    /// `dst = a op b`; `from`/`to`: the bytecode steps it stands for, with
    /// the ones before it in the same line (run from `from` when the values
    /// are not plain numbers).
    Bin { op: Bin, a: Src, b: Src, dst: Dst, from: u32, to: u32, nto: u32 },
    /// `dst = a`, a variable or constant copied whole (`a |= b`).
    Move { a: Src, dst: Dst, from: u32, to: u32, nto: u32 },
    /// `if (a op b) == when goto target` (bytecode positions), else `to`.
    CmpJump { op: Bin, a: Src, b: Src, when: bool, target: u32, ntarget: u32, from: u32, to: u32, nto: u32 },
    /// `d |= d + k` on a whole number (`i |= i + 1`): Bin Add/AddInts of
    /// variable d and an int constant, rebound into d.
    IncK { d: u16, k: i64, from: u32, to: u32, nto: u32 },
    /// CmpJump of two variables.
    CmpLL { op: Bin, a: u16, b: u16, when: bool, target: u32, ntarget: u32, from: u32, to: u32, nto: u32 },
    /// `dst = obj >> name` (constant `name`): a number or bool field of
    /// the object variable `obj` holds. `hint`: `Prog::hints` entry.
    GetF { obj: u16, name: u16, hint: u32, dst: Dst, from: u32, to: u32, nto: u32 },
    /// `obj >> name |= val` with a plain value, then `obj` handed its own
    /// id back (`Dup`, `StoreLocal(obj)`, `Pop`), as `set_plain_field`.
    SetF { obj: u16, name: u16, hint: u32, val: Src, from: u32, to: u32, nto: u32 },
    /// `g(args)` for an action in global g, with `argc` arguments from
    /// `Prog::args[args..]`: a new frame in this loop, as `op_call` makes
    /// it. `at`: the `Call` step; `to`: the step after it.
    CallG { g: u16, args: u32, argc: u8, from: u32, at: u32, to: u32 },
    /// `Return` from an action called by another one.
    Ret { at: u32 },
    /// `goto target` (a bytecode position).
    Jump { target: u32, ntarget: u32, back: bool },
    /// `RepeatNext` of a condition loop (`repeat i < n`): count the pass,
    /// leave at `exit` past the limit. Any other loop: the bytecode step.
    RepNext { id: u16, exit: u32, nexit: u32, at: u32, ncont: u32 },
    /// `RepeatIdx`: the pass number into variable `slot`.
    RepIdx { id: u16, slot: u16, at: u32, ncont: u32 },
}

/// A function's register code.
#[derive(Debug, Clone)]
pub(crate) struct Prog {
    pub(crate) ins: Vec<Ins>,
    /// Per bytecode position: its register instruction, or NONE when the
    /// position is inside a line that one instruction covers.
    pub(crate) at: Vec<u32>,
    /// The number constants instructions read.
    pub(crate) k: Vec<N>,
    /// The arguments of `CallG` instructions.
    pub(crate) args: Vec<Arg>,
    /// Per field instruction: the layout it last saw and the field's place in it.
    pub(crate) hints: Vec<std::cell::Cell<(usize, u32)>>,
}

/// An argument of a call: a variable read, a variable moved out
/// (`TakeLocal`), or a constant of the function.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Arg { L(u16), Take(u16), K(u16) }

const NONE: u32 = u32::MAX;
/// The most scratch registers a line uses.
const TEMPS: usize = 16;

/// The bytecode positions any jump can land on.
fn jump_targets(f: &FunctionObject) -> Vec<bool> {
    let n = f.bytecode.len();
    let mut t = vec![false; n + 1];
    let mut mark = |i: usize, off: isize| {
        let j = i as isize + 1 + off;
        if j >= 0 && (j as usize) <= n { t[j as usize] = true; }
    };
    for (i, op) in f.bytecode.iter().enumerate() {
        match op.plain() {
            O::Jump(o) | O::JumpIfFalse(o) | O::JumpIfTrue(o) => mark(i, o as isize),
            O::RepeatNext(idx) => {
                if let Some(crate::opcode::OpData::Repeat(r)) = f.op_data.get(idx as usize) {
                    mark(i, r.exit as isize);
                }
            }
            _ => {}
        }
    }
    t
}

/// Turns `f`'s bytecode into register code.
pub(crate) fn build(f: &FunctionObject) -> Prog {
    let code = &f.bytecode;
    let targets = jump_targets(f);
    let mut ins: Vec<Ins> = Vec::with_capacity(code.len());
    let mut at = vec![NONE; code.len()];
    let mut k: Vec<N> = Vec::new();
    let mut hints = 0u32;
    let mut args: Vec<Arg> = Vec::new();
    let mut i = 0;
    while i < code.len() {
        if let Some((call, end)) = call_at(f, i, &targets, &mut args) {
            at[i] = ins.len() as u32;
            ins.push(call);
            i = end;
            continue;
        }
        if let Some((line, end)) = line_at(f, i, &targets, &mut k, &mut hints) {
            at[i] = ins.len() as u32;
            ins.extend(line);
            i = end;
            continue;
        }
        at[i] = ins.len() as u32;
        let rep = |idx: u16| match f.op_data.get(idx as usize) {
            Some(crate::opcode::OpData::Repeat(r)) => Some(r),
            _ => None,
        };
        ins.push(match code[i].plain() {
            O::Jump(o) => Ins::Jump { target: (i as isize + 1 + o as isize) as u32, ntarget: NONE, back: o < 0 },
            O::RepeatNext(idx) => match rep(idx) {
                Some(r) => Ins::RepNext { id: r.id, exit: i as u32 + 1 + r.exit, nexit: NONE, at: i as u32, ncont: NONE },
                None => Ins::Old { at: i as u32 },
            },
            O::Return => Ins::Ret { at: i as u32 },
            O::RepeatIdx(idx) => match rep(idx) {
                Some(r) => Ins::RepIdx { id: r.id, slot: r.idx, at: i as u32, ncont: NONE },
                None => Ins::Old { at: i as u32 },
            },
            _ => Ins::Old { at: i as u32 },
        });
        i += 1;
    }
    // The common forms get instructions of their own.
    for x in ins.iter_mut() {
        match *x {
            Ins::Bin { op: Bin::Add | Bin::AddInts, a: Src::L(d), b: Src::K(c), dst: Dst::Rebind(e), from, to, .. } if d == e => {
                if let N::I(k) = k[c as usize] { *x = Ins::IncK { d, k, from, to, nto: NONE }; }
            }
            Ins::CmpJump { op, a: Src::L(a), b: Src::L(b), when, target, from, to, .. } => {
                *x = Ins::CmpLL { op, a, b, when, target, ntarget: NONE, from, to, nto: NONE };
            }
            _ => {}
        }
    }
    // Where each instruction goes on, as instruction numbers.
    let pc_of = |ip: u32| at.get(ip as usize).copied().unwrap_or(NONE);
    for x in ins.iter_mut() {
        match x {
            Ins::Bin { to, nto, dst, .. } => if !matches!(dst, Dst::T(_)) { *nto = pc_of(*to) },
            Ins::Move { to, nto, .. } => *nto = pc_of(*to),
            Ins::CmpJump { target, ntarget, to, nto, .. } | Ins::CmpLL { target, ntarget, to, nto, .. } => { *ntarget = pc_of(*target); *nto = pc_of(*to); }
            Ins::IncK { to, nto, .. } | Ins::SetF { to, nto, .. } => *nto = pc_of(*to),
            Ins::GetF { to, nto, dst, .. } => if !matches!(dst, Dst::T(_)) { *nto = pc_of(*to) },
            Ins::Jump { target, ntarget, .. } => *ntarget = pc_of(*target),
            Ins::RepNext { exit, nexit, at: a, ncont, .. } => { *nexit = pc_of(*exit); *ncont = pc_of(*a + 1); }
            Ins::RepIdx { at: a, ncont, .. } => *ncont = pc_of(*a + 1),
            Ins::Old { .. } | Ins::CallG { .. } | Ins::Ret { .. } => {}
        }
    }
    Prog { ins, at, k, args, hints: (0..hints).map(|_| std::cell::Cell::new((0, 0))).collect() }
}

/// `LoadGlobal(g)`, one load per argument, `Call(n)`, starting at `start`.
fn call_at(f: &FunctionObject, start: usize, targets: &[bool], args: &mut Vec<Arg>) -> Option<(Ins, usize)> {
    let code = &f.bytecode;
    let O::LoadGlobal(g) = code[start].plain() else { return None };
    let first = args.len();
    let mut i = start + 1;
    let done = loop {
        if i >= code.len() || targets[i] { break None; }
        match code[i].plain() {
            O::LoadLocal(s) => args.push(Arg::L(s)),
            O::TakeLocal(s) => args.push(Arg::Take(s)),
            O::LoadConst(c) => args.push(Arg::K(c)),
            O::Call(n) if n as usize == args.len() - first => break Some(n),
            _ => break None,
        }
        i += 1;
    };
    let Some(argc) = done else { args.truncate(first); return None };
    Some((Ins::CallG { g, args: first as u32, argc, from: start as u32, at: i as u32, to: i as u32 + 1 }, i + 1))
}

fn konst(f: &FunctionObject, c: u16) -> Option<N> {
    match f.constants.get(c as usize)? {
        Value::Int(n) => Some(N::I(*n)),
        Value::Float(x) => Some(N::F(*x)),
        _ => None,
    }
}

/// A line of plain number work starting at bytecode position `start`:
/// loads of variables and number constants, `+ - * /` and comparisons,
/// ending in a store into a variable, a conditional jump, or one value
/// left for the steps after. Gives its instructions and where it ends.
/// No jump may land inside it.
fn line_at(f: &FunctionObject, start: usize, targets: &[bool], k: &mut Vec<N>, hints: &mut u32) -> Option<(Vec<Ins>, usize)> {
    let code = &f.bytecode;
    let mut st: Vec<Src> = Vec::new();
    let mut out: Vec<Ins> = Vec::new();
    let mut temps = 0u8;
    let mut i = start;
    let from = start as u32;
    // Leaves the line before step i, with what has been worked out pushed.
    let end_push = |out: &mut Vec<Ins>, st: &[Src], i: usize| -> Option<(Vec<Ins>, usize)> {
        if st.len() != 1 { return None; }
        match (out.last_mut(), st[0]) {
            (Some(Ins::Bin { dst, to, .. } | Ins::GetF { dst, to, .. }), Src::T(_)) => { *dst = Dst::Push; *to = i as u32; }
            _ => return None,
        }
        Some((std::mem::take(out), i))
    };
    let field_name = |c: u16| -> bool {
        matches!(f.constants.get(c as usize), Some(Value::Str(n)) if member_builtin(n).is_none())
    };
    loop {
        if i >= code.len() { return None; }
        if i > start && targets[i] { return end_push(&mut out, &st, i); }
        let op = code[i].plain();
        match op {
            O::LoadLocal(s) => st.push(Src::L(s)),
            O::LoadConst(c) => match konst(f, c) {
                Some(n) => { k.push(n); st.push(Src::K(k.len() as u16 - 1)) }
                None => return end_push(&mut out, &st, i),
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Lt | O::Le | O::Gt | O::Ge | O::Eq | O::Ne | O::AppendLocal(_) => {
                let (a, b) = match op {
                    O::AppendLocal(s) => (Src::L(s), st.pop()?),
                    _ => { let b = st.pop()?; (st.pop()?, b) }
                };
                let bin = match op {
                    O::Add => Bin::Add, O::Sub => Bin::Sub, O::Mul => Bin::Mul, O::Div => Bin::Div,
                    O::Lt => Bin::Lt, O::Le => Bin::Le, O::Gt => Bin::Gt, O::Ge => Bin::Ge,
                    O::Eq => Bin::Eq, O::Ne => Bin::Ne, _ => Bin::AddInts,
                };
                // Scratch registers free up as their values are used.
                let used = [a, b].iter().filter(|s| matches!(s, Src::T(_))).count() as u8;
                temps -= used;
                if temps as usize >= TEMPS { return None; }
                let t = temps;
                temps += 1;
                out.push(Ins::Bin { op: bin, a, b, dst: Dst::T(t), from, to: NONE, nto: NONE });
                st.push(Src::T(t));
            }
            O::GetMember(c) if field_name(c) && matches!(st.last(), Some(Src::L(_))) => {
                let Some(Src::L(obj)) = st.pop() else { unreachable!() };
                if temps as usize >= TEMPS { return None; }
                let t = temps;
                temps += 1;
                out.push(Ins::GetF { obj, name: c, hint: *hints, dst: Dst::T(t), from, to: NONE, nto: NONE });
                *hints += 1;
                st.push(Src::T(t));
            }
            O::SetField(c) if field_name(c) && st.len() == 2 && matches!(st[0], Src::L(_)) => {
                let Src::L(obj) = st[0] else { unreachable!() };
                // Then `obj` gets its own id back: `StoreLocal(obj)`, or
                // `Dup`, `StoreLocal(obj)`, `Pop`.
                let plain = |j: usize| code.get(j).map(|o| o.plain());
                let len = if matches!(plain(i + 1), Some(O::StoreLocal(s)) if s == obj) { 2 }
                    else if matches!(plain(i + 1), Some(O::Dup)) && matches!(plain(i + 2), Some(O::StoreLocal(s)) if s == obj)
                        && matches!(plain(i + 3), Some(O::Pop)) { 4 }
                    else { return None };
                if (i + 1..i + len).any(|j| targets[j]) { return None; }
                let val = st[1];
                out.push(Ins::SetF { obj, name: c, hint: *hints, val, from, to: (i + len) as u32, nto: NONE });
                *hints += 1;
                return Some((out, i + len));
            }
            O::JumpIfFalse(o) | O::JumpIfTrue(o) => {
                let when = matches!(op, O::JumpIfTrue(_));
                let target = (i as isize + 1 + o as isize) as u32;
                if st.len() != 1 { return None; }
                let Some(Ins::Bin { op: bin, a, b, dst: Dst::T(_), .. }) = out.pop() else { return None };
                if !matches!(bin, Bin::Lt | Bin::Le | Bin::Gt | Bin::Ge | Bin::Eq | Bin::Ne) { return None; }
                out.push(Ins::CmpJump { op: bin, a, b, when, target, ntarget: NONE, from, to: i as u32 + 1, nto: NONE });
                return Some((out, i + 1));
            }
            O::GuardRebindLocal(d, _) | O::StoreLocal(d) | O::BindLocal(d) => {
                let (dst, len) = match op {
                    O::GuardRebindLocal(..) => {
                        if !matches!(code.get(i + 1).map(|o| o.plain()), Some(O::StoreLocal(s)) if s == d) { return end_push(&mut out, &st, i); }
                        if targets.get(i + 1) == Some(&true) { return end_push(&mut out, &st, i); }
                        (Dst::Rebind(d), 2)
                    }
                    _ => (Dst::Store(d), 1),
                };
                if st.len() != 1 { return None; }
                let to = (i + len) as u32;
                let empty = out.is_empty();
                match (out.last_mut(), st[0]) {
                    (Some(Ins::Bin { dst: last, to: t, .. } | Ins::GetF { dst: last, to: t, .. }), Src::T(_)) => { *last = dst; *t = to; }
                    (None, a @ (Src::L(_) | Src::K(_))) if empty => out.push(Ins::Move { a, dst, from, to, nto: NONE }),
                    _ => return None,
                }
                return Some((out, i + len));
            }
            _ => return end_push(&mut out, &st, i),
        }
        i += 1;
    }
}

impl Vm {
    /// The register code of `f`, made the first time.
    fn reg_prog<'a>(&self, f: &'a FunctionObject) -> &'a Prog {
        f.reg.get_or_init(|| Box::new(build(f)))
    }

    /// Runs register code from the current frame, with the same contract
    /// as `run_fast`: a step for `execute_op`, or None at the end of the
    /// frame's code, after a call or return, or every 64 steps.
    /// Calls and returns switch frames here, in the same loop, as long as
    /// more than `floor` frames are left.
    pub(super) fn run_reg(&mut self, left: &mut u32, floor: usize) -> Option<(Step, Opcode)> {
        let frame = self.call_stack.last().unwrap();
        let mut func = frame.func.clone();
        // The running function's code. `func` keeps it alive; it is found
        // again whenever a call or return changes the function.
        macro_rules! code_of { ($f:expr) => {{
            let f: &FunctionObject = $f;
            let prog = self.reg_prog(f);
            unsafe { (&*(f.bytecode.as_slice() as *const [Opcode]), &*(prog as *const Prog)) }
        }} }
        let (mut code, mut prog): (&[Opcode], &Prog) = code_of!(&func);
        let mut ins: &[Ins] = &prog.ins;
        let mut ks: &[N] = &prog.k;
        let mut ip = frame.ip;
        let mut tmp = [N::I(0); TEMPS];
        // Bytecode steps to run the old way before register code goes on
        // (a line whose values were not plain numbers).
        let mut slow_to = 0usize;
        macro_rules! out { () => {{ self.call_stack.last_mut().unwrap().ip = ip; return None; }} }
        loop {
            if ip >= code.len() { out!(); }
            let pc = if ip >= slow_to { prog.at[ip] } else { NONE };
            if pc != NONE {
                let mut pc = pc as usize;
                // A call or return made in register code: the frame to go on in.
                let mut switched = false;
                // The call's registers and frame, found once for the run of
                // register instructions (nothing in it moves them).
                let n = self.cur_n;
                let mut regs: *mut Operand = unsafe { self.stack.as_mut_ptr().add(self.cur_base) };
                let fr: *mut CallFrame = self.call_stack.last_mut().unwrap();
                let unlocked = unsafe { (*fr).locks.is_none() };
                // A register as a plain number (a variable holding one, not a cell).
                macro_rules! num { ($s:expr) => {
                    match $s {
                        Src::K(i) => Some(ks[i as usize]),
                        Src::T(t) => Some(tmp[t as usize]),
                        Src::L(r) => if (r as usize) < n {
                            match unsafe { &*regs.add(r as usize) } {
                                Operand::Val(Value::Int(v)) => Some(N::I(*v)),
                                Operand::Val(Value::Float(v)) => Some(N::F(*v)),
                                _ => None,
                            }
                        } else { None },
                    }
                } }
                // A plain value into variable d: `rebind` only over a plain
                // value (as `GuardRebindLocal` lets through), else over any
                // value but a cell's. False: the bytecode steps do it.
                macro_rules! store { ($d:expr, $v:expr, $rebind:expr) => {{
                    let d = $d as usize;
                    if !unlocked || d >= n { false } else {
                        match unsafe { &mut *regs.add(d) } {
                            Operand::Val(old @ (Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Nil)) => {
                                unsafe { std::ptr::write(old, $v) }; true
                            }
                            Operand::Val(old) if !$rebind => { *old = $v; true }
                            _ => false,
                        }
                    }
                }} }
                macro_rules! val { ($r:expr) => { match $r { N::I(v) => Value::Int(v), N::F(x) => Value::Float(x), N::B(b) => Value::Bool(b) } } }
                // Register instructions, until one hands back to a bytecode step.
                // `go!(ip, pc)`: on at bytecode position ip, which is
                // instruction pc (NONE: a bytecode step).
                macro_rules! go { ($ip:expr, $pc:expr) => {{
                    let p = $pc;
                    if p == NONE { ip = $ip as usize; break; }
                    pc = p as usize;
                    continue;
                }} }
                loop {
                    match unsafe { ins.get_unchecked(pc) } {
                        &Ins::IncK { d, k, from, to, nto } => {
                            let d = d as usize;
                            if unlocked && d < n {
                                if let Operand::Val(Value::Int(v)) = unsafe { &mut *regs.add(d) } {
                                    *v = v.wrapping_add(k);
                                    go!(to, nto);
                                }
                            }
                            ip = from as usize; slow_to = to as usize; break;
                        }
                        &Ins::CmpLL { op, a, b, when, target, ntarget, from, to, nto } => {
                            if (a as usize) < n && (b as usize) < n {
                                let (x, y) = unsafe { (&*regs.add(a as usize), &*regs.add(b as usize)) };
                                if let (Operand::Val(Value::Int(x)), Operand::Val(Value::Int(y))) = (x, y) {
                                    let c = match op {
                                        Bin::Lt => x < y, Bin::Le => x <= y, Bin::Gt => x > y,
                                        Bin::Ge => x >= y, Bin::Eq => x == y, _ => x != y,
                                    };
                                    if c == when { go!(target, ntarget) } else { go!(to, nto) }
                                }
                                let r = match (num!(Src::L(a)), num!(Src::L(b))) { (Some(x), Some(y)) => bin(op, x, y), _ => None };
                                if let Some(N::B(c)) = r { if c == when { go!(target, ntarget) } else { go!(to, nto) } }
                            }
                            ip = from as usize; slow_to = to as usize; break;
                        }
                        &Ins::Old { at } => { ip = at as usize; break; }
                        &Ins::CallG { g, args, argc, from, at, to } => {
                            if self.reg_call(g, &prog.args[args as usize..args as usize + argc as usize], to, &func) {
                                switched = true;
                            } else {
                                // The bytecode steps make the call.
                                ip = from as usize; slow_to = at as usize + 1;
                            }
                            break;
                        }
                        &Ins::Ret { at } => {
                            if self.call_stack.len() >= 2 && self.reg_return() {
                                if self.call_stack.len() <= floor { return None; }
                                switched = true;
                            } else { ip = at as usize; }
                            break;
                        }
                        &Ins::GetF { obj, name, hint, dst, from, to, nto } => {
                            let r = if (obj as usize) < n {
                                let held = unsafe { &*regs.add(obj as usize) };
                                self.reg_field(held, name, &prog.hints[hint as usize], &func)
                            } else { None };
                            let Some(r) = r else { ip = from as usize; slow_to = line_end(ins, pc); break };
                            let ok = match dst {
                                Dst::T(t) => { tmp[t as usize] = r; pc += 1; continue; }
                                Dst::Rebind(d) => store!(d, val!(r), true),
                                Dst::Store(d) => store!(d, val!(r), false),
                                Dst::Push => {
                                    // The push may move the stack: find the registers again.
                                    self.stack.push(Operand::Val(val!(r)));
                                    regs = unsafe { self.stack.as_mut_ptr().add(self.cur_base) };
                                    true
                                }
                            };
                            if !ok { ip = from as usize; slow_to = to as usize; break; }
                            go!(to, nto);
                        }
                        &Ins::SetF { obj, name, hint, val, from, to, nto } => {
                            let v = match val { Src::T(t) => Some(tmp[t as usize]), s => num!(s) };
                            let ok = unlocked && (obj as usize) < n && match (v, unsafe { &*regs.add(obj as usize) }) {
                                (Some(v), Operand::Val(Value::Ref(u))) => {
                                    let u = u.clone();
                                    self.reg_set_field(u, name, &prog.hints[hint as usize], &func, val!(v))
                                }
                                _ => false,
                            };
                            if !ok { ip = from as usize; slow_to = to as usize; break; }
                            go!(to, nto);
                        }
                        &Ins::Jump { target, ntarget, back } => {
                            if back {
                                *left = left.wrapping_sub(1);
                                if *left % 64 == 0 { ip = target as usize; out!(); }
                            }
                            go!(target, ntarget);
                        }
                        &Ins::Bin { op, a, b, dst, from, to, nto } => {
                            let r = match (num!(a), num!(b)) { (Some(x), Some(y)) => bin(op, x, y), _ => None };
                            let Some(r) = r else { ip = from as usize; slow_to = line_end(ins, pc); break };
                            let ok = match dst {
                                Dst::T(t) => { tmp[t as usize] = r; pc += 1; continue; }
                                Dst::Rebind(d) => store!(d, val!(r), true),
                                Dst::Store(d) => store!(d, val!(r), false),
                                Dst::Push => {
                                    // The push may move the stack: find the registers again.
                                    self.stack.push(Operand::Val(val!(r)));
                                    regs = unsafe { self.stack.as_mut_ptr().add(self.cur_base) };
                                    true
                                }
                            };
                            if !ok { ip = from as usize; slow_to = to as usize; break; }
                            go!(to, nto);
                        }
                        &Ins::Move { a, dst, from, to, nto } => {
                            let ok = match num!(a) {
                                Some(r) => match dst {
                                    Dst::Rebind(d) => store!(d, val!(r), true),
                                    Dst::Store(d) => store!(d, val!(r), false),
                                    _ => false,
                                },
                                None => false,
                            };
                            if !ok { ip = from as usize; slow_to = to as usize; break; }
                            go!(to, nto);
                        }
                        &Ins::CmpJump { op, a, b, when, target, ntarget, from, to, nto } => {
                            let r = match (num!(a), num!(b)) { (Some(x), Some(y)) => bin(op, x, y), _ => None };
                            let Some(N::B(c)) = r else { ip = from as usize; slow_to = to as usize; break };
                            if c == when { go!(target, ntarget) } else { go!(to, nto) }
                        }
                        &Ins::RepNext { id, exit, nexit, at, ncont } => {
                            let Some(st) = (unsafe { (&mut (*fr).loops).get_mut(id as usize) }) else { ip = at as usize; break };
                            if st.mode != 4 { ip = at as usize; break; }
                            st.counter += 1;
                            if st.counter >= st.limit { go!(exit, nexit) } else { go!(at + 1, ncont) }
                        }
                        &Ins::RepIdx { id, slot, at, ncont } => {
                            let Some(st) = (unsafe { (&(*fr).loops).get(id as usize) }) else { ip = at as usize; break };
                            if !store!(slot, Value::Int(st.counter), false) { ip = at as usize; break; }
                            go!(at + 1, ncont);
                        }
                    }
                }
            if switched {
                let f = self.call_stack.last().unwrap();
                func = f.func.clone();
                (code, prog) = code_of!(&func);
                ins = &prog.ins;
                ks = &prog.k;
                ip = self.call_stack.last().unwrap().ip;
                slow_to = 0;
                *left = left.wrapping_sub(1);
                if *left % 64 == 0 { out!(); }
                continue;
            }
            }
            // One bytecode step, as `run_fast` does it.
            let Some(&op) = code.get(ip) else { out!() };
            ip += 1;
            *left = left.wrapping_sub(1);
            if matches!(op, Opcode::Call(_)) || (matches!(op, Opcode::Return) && self.call_stack.len() >= 2) {
                self.call_stack.last_mut().unwrap().ip = ip;
                let r = match op { Opcode::Call(n) => self.op_call(n), _ => self.op_return() };
                if let Err(e) = r { self.step_err = Some(e); return Some((Step::Failed, op)); }
                if self.call_stack.len() <= floor { return None; }
                // Go on in the frame that runs now, in this loop.
                let Some(f) = self.call_stack.last() else { return None };
                if !Rc::ptr_eq(&f.func, &func) || matches!(op, Opcode::Return) {
                    func = f.func.clone();
                    (code, prog) = code_of!(&func);
                    ins = &prog.ins;
                    ks = &prog.k;
                }
                ip = self.call_stack.last().unwrap().ip;
                slow_to = 0;
                *left = left.wrapping_sub(1);
                if *left % 64 == 0 { return None; }
                continue;
            }
            match self.fast_step(op, &mut ip, code) {
                Step::Done => if *left % 64 == 0 { out!(); },
                step => {
                    self.call_stack.last_mut().unwrap().ip = ip;
                    return Some((step, op));
                }
            }
        }
    }
}

impl Vm {
    /// `CallG`: a call of the action in global `g` that `op_call` would
    /// make without anything special (no decision memo or deed log, every
    /// parameter given, plain or object arguments): the function and its
    /// arguments go on the stack, the caller goes on at `to` once the
    /// callee returns, and the callee's frame is pushed. False (nothing
    /// changed): the bytecode steps make the call.
    fn reg_call(&mut self, g: u16, args: &[Arg], to: u32, caller: &FunctionObject) -> bool {
        if self.memo.is_some() || self.call_stack.len() >= MAX_CALL_DEPTH { return false; }
        if self.deciding.as_deref().is_some_and(|d| d.deed_depth == Some(self.call_stack.len())) { return false; }
        let Some(t) = self.session.get_global(g as usize) else { return false };
        let f = match self.session.get_stash(t) {
            Ok(st) => match &st.value { Value::Function(f) => f.clone(), _ => return false },
            Err(_) => return false,
        };
        if f.params != args.len() || f.required_params != args.len() { return false; }
        let plain = |v: &Value| matches!(v, Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Nil);
        for a in args {
            let ok = match *a {
                Arg::L(s) | Arg::Take(s) => match loc!(self, s) {
                    Some(Operand::Val(v)) if plain(v) => true,
                    Some(Operand::Val(Value::Ref(u))) => !u.starts_with(OVERLAY_RECORD)
                        && matches!(self.session.object_store.get(u), Some(Value::Object(_))),
                    _ => false,
                },
                Arg::K(c) => caller.constants.get(c as usize).is_some_and(plain),
            };
            if !ok { return false; }
        }
        // A moved-out variable keeps nothing (`TakeLocal`): only a plain
        // value is moved; an object stays where it is.
        let unlocked_take = |vm: &Vm, s: u16| vm.call_stack.last().unwrap().get_hard_type_lock(s).is_none();
        let t = t.clone();
        let func_idx = self.stack.len();
        self.stack.push(Operand::Ref(t));
        for a in args {
            let v = match *a {
                Arg::L(s) => match loc!(self, s) { Some(Operand::Val(v)) => v.clone_fast(), _ => unreachable!() },
                Arg::Take(s) => {
                    let take = unlocked_take(self, s);
                    match loc_mut!(self, s) {
                        Some(Operand::Val(v)) if take && plain(v) => std::mem::replace(v, Value::Nil),
                        Some(Operand::Val(v)) => v.clone_fast(),
                        _ => unreachable!(),
                    }
                }
                Arg::K(c) => caller.constants[c as usize].clone_fast(),
            };
            self.stack.push(Operand::Val(v));
        }
        self.call_stack.last_mut().unwrap().ip = to as usize;
        self.push_frame(f, Vec::new(), func_idx, func_idx + 1);
        true
    }

    /// `Return` as `op_return` does it, when there is nothing special: no
    /// decision memo, no variable in a cell, a value (not a variable) to
    /// give back. False (nothing changed): `op_return` does it.
    fn reg_return(&mut self) -> bool {
        let fr = self.call_stack.last().unwrap();
        if fr.memo.is_some() { return false; }
        let top = fr.top();
        if self.stack[fr.base..top].iter().any(|l| matches!(l, Operand::Cell(_))) { return false; }
        let v = if self.stack.len() > top {
            match self.stack.last() {
                Some(Operand::Val(Value::Frozen(_))) | None => return false,
                Some(Operand::Val(_)) => match self.stack.pop() { Some(Operand::Val(v)) => v, _ => unreachable!() },
                _ => return false,
            }
        } else { Value::Nil };
        let frame = self.call_stack.pop().unwrap();
        self.refresh_top();
        self.stack.truncate(frame.stack_base);
        self.stack.push(Operand::Val(v));
        true
    }

    /// A number or bool field read, as `get_plain_field` reads it (None:
    /// the bytecode steps read it).
    #[inline(always)]
    fn reg_field(&self, held: &Operand, name: u16, hint: &std::cell::Cell<(usize, u32)>, func: &FunctionObject) -> Option<N> {
        let obj = match held {
            Operand::Val(Value::Ref(u)) => match self.session.object_store.get(u) { Some(Value::Object(o)) => o, _ => return None },
            Operand::Val(Value::Object(o)) => o,
            _ => return None,
        };
        let layout = obj.fields.layout_id();
        let at = match hint.get() {
            (l, at) if l == layout => at as usize,
            _ => {
                let Some(Value::Str(name)) = func.constants.get(name as usize) else { return None };
                let at = obj.fields.get_index_of(name.as_str())?;
                hint.set((layout, at as u32));
                at
            }
        };
        match *obj.fields.value_at(at) {
            Value::Int(n) => Some(N::I(n)),
            Value::Float(x) => Some(N::F(x)),
            Value::Bool(b) => Some(N::B(b)),
            _ => None,
        }
    }

    /// A plain value written to a stored object's field, as
    /// `set_plain_field` writes it; false (nothing changed) when the
    /// bytecode steps must do it.
    fn reg_set_field(&mut self, uuid: crate::value::ObjId, name: u16, hint: &std::cell::Cell<(usize, u32)>, func: &FunctionObject, v: Value) -> bool {
        // In the program that runs a world, its objects' writes go to their regions.
        if self.session.region.is_none() && self.session.world.is_some() { return false; }
        let Some(Value::Str(name)) = func.constants.get(name as usize) else { return false };
        if AsRef::<str>::as_ref(name) == crate::store::OWNER_FIELD { return false; }
        if uuid.starts_with(OVERLAY_RECORD) { return false; }
        let Some(Value::Object(o)) = self.session.object_store.get_mut(&uuid) else { return false };
        if !o.readonly_fields.is_empty() && o.readonly_fields.contains(name.as_str()) { return false; }
        let layout = o.fields.layout_id();
        let at = match hint.get() {
            (l, at) if l == layout => at as usize,
            _ => {
                let Some(at) = o.fields.get_index_of(name.as_str()) else { return false };
                hint.set((layout, at as u32));
                at
            }
        };
        // A trait field (a 0..1 number at creation) stays in 0..1.
        let v = if !o.trait_fields.is_empty() && o.trait_fields.contains(name.as_str()) {
            match v {
                Value::Float(f) => Value::Float(f.clamp(0.0, 1.0)),
                Value::Int(i) => Value::Float((i as f64).clamp(0.0, 1.0)),
                other => other,
            }
        } else { v };
        match std::rc::Rc::make_mut(&mut o.fields).get_index_mut(at) {
            Some((_, slot)) => *slot = v,
            None => return false,
        }
        self.session.touch(&uuid);
        true
    }
}

/// Where the line holding instruction `pc` ends (its last instruction's `to`).
fn line_end(ins: &[Ins], mut pc: usize) -> usize {
    loop {
        match ins[pc] {
            Ins::Bin { dst: Dst::T(_), .. } | Ins::GetF { dst: Dst::T(_), .. } => pc += 1,
            Ins::Bin { to, .. } | Ins::Move { to, .. } | Ins::CmpJump { to, .. } | Ins::IncK { to, .. } | Ins::CmpLL { to, .. }
            | Ins::GetF { to, .. } | Ins::SetF { to, .. } => return to as usize,
            _ => return 0,
        }
    }
}

/// `x op y` on plain numbers, as the stack VM's fast steps work it out;
/// None to leave it to them.
#[inline(always)]
fn bin(op: Bin, a: N, b: N) -> Option<N> {
    Some(match (a, b) {
        (N::I(x), N::I(y)) => match op {
            Bin::Add | Bin::AddInts => N::I(x.wrapping_add(y)),
            Bin::Sub => N::I(x.checked_sub(y)?),
            Bin::Mul => N::I(x.checked_mul(y)?),
            Bin::Div => match num_div(&Value::Int(x), &Value::Int(y)).ok()? {
                Value::Int(n) => N::I(n),
                Value::Float(f) => N::F(f),
                _ => return None,
            },
            Bin::Lt => N::B(x < y), Bin::Le => N::B(x <= y),
            Bin::Gt => N::B(x > y), Bin::Ge => N::B(x >= y),
            Bin::Eq => N::B(x == y), Bin::Ne => N::B(x != y),
        },
        (N::B(_), _) | (_, N::B(_)) => return None,
        _ if op == Bin::AddInts => return None,
        (a, b) => {
            let f = |v: N| match v { N::I(i) => i as f64, N::F(x) => x, N::B(_) => 0.0 };
            let (x, y) = (f(a), f(b));
            let o = || x.partial_cmp(&y).map_or(0, |o| o as i32);
            match op {
                Bin::Add | Bin::AddInts => N::F(x + y),
                Bin::Sub => N::F(x - y),
                Bin::Mul => N::F(x * y),
                Bin::Div => if y == 0.0 { return None } else { N::F(x / y) },
                Bin::Lt => N::B(o() < 0), Bin::Le => N::B(o() <= 0),
                Bin::Gt => N::B(o() > 0), Bin::Ge => N::B(o() >= 0),
                // `==` with a decimal goes the usual way.
                Bin::Eq | Bin::Ne => return None,
            }
        }
    })
}
