use std::rc::Rc;

use crate::error::GoblinError;
use crate::opcode::Opcode;
use crate::session::Session;
use crate::value::{
    Closure, CollectionValue, FunctionObject, Tether, UpvalueCell,
    UpvalueDescriptor, Value,
};

pub const MAX_CALL_DEPTH: usize = 512;

// ── CallFrame ─────────────────────────────────────────────────────────────────

/// One activation record on the call stack.
pub struct CallFrame {
    /// Local variable slots (0..params are args, rest are locals).
    pub locals: Vec<Option<Tether>>,
    /// Captured upvalues for the current closure (empty if plain function).
    pub upvalues: Vec<UpvalueCell>,
    /// Instruction pointer into func.bytecode.
    pub ip: usize,
    /// The function being executed.
    pub func: Rc<FunctionObject>,
    /// Index into Vm::stack where this frame's operands begin.
    /// On return, stack is truncated to this base.
    pub stack_base: usize,
}

impl CallFrame {
    fn new(func: Rc<FunctionObject>, upvalues: Vec<UpvalueCell>, stack_base: usize) -> Self {
        let locals = vec![None; func.locals];
        CallFrame { locals, upvalues, ip: 0, func, stack_base }
    }

    fn load_local(&self, slot: u8) -> Result<Tether, GoblinError> {
        match self.locals.get(slot as usize) {
            Some(Some(t)) => Ok(t.clone()),
            Some(None) => Err(GoblinError::Runtime(
                format!("local slot {slot} is uninitialized")
            )),
            None => Err(GoblinError::Runtime(
                format!("local slot {slot} out of range (frame has {} slots)", self.locals.len())
            )),
        }
    }

    fn store_local(&mut self, slot: u8, t: Tether) {
        let idx = slot as usize;
        if idx >= self.locals.len() {
            self.locals.resize(idx + 1, None);
        }
        self.locals[idx] = Some(t);
    }
}

struct CatchFrame {
    call_depth: usize,
    stack_depth: usize,
    catch_ip: usize,
    #[allow(dead_code)]
    frame_ip_index: usize, // which frame's ip to set
}

// ── Vm ───────────────────────────────────────────────────────────────────────

/// The Goblin VM: executes bytecode against a Session.
///
/// - `session` owns the arena of stashes (isolated per worker).
/// - `stack` is the operand stack (Tethers).
/// - `call_stack` is the function call stack (no Rust recursion).
pub struct Vm {
    pub session: Session,
    pub stack: Vec<Tether>,
    pub call_stack: Vec<CallFrame>,
    catch_stack: Vec<CatchFrame>,
}

impl Vm {
    pub fn new(session: Session) -> Self {
        Vm { session, stack: Vec::new(), call_stack: Vec::new(), catch_stack: Vec::new() }
    }

    /// Run a top-level function. Returns the final return value.
    pub fn execute(&mut self, func: FunctionObject) -> Result<Value, GoblinError> {
        let func_rc = Rc::new(func);
        let frame = CallFrame::new(func_rc, Vec::new(), 0);
        self.call_stack.push(frame);
        self.run_loop()?;
        // Return value is the top of the stack (or Nil).
        if let Some(t) = self.stack.pop() {
            self.session.read_value(&t)
        } else {
            Ok(Value::Nil)
        }
    }

    // ── Internal execution loop ──────────────────────────────────────────────

    fn run_loop(&mut self) -> Result<(), GoblinError> {
        loop {
            // Fetch next opcode (avoid holding a mutable borrow across the match).
            let op = {
                let frame = match self.call_stack.last_mut() {
                    Some(f) => f,
                    None => break, // all frames returned
                };
                if frame.ip >= frame.func.bytecode.len() {
                    // Implicit return Nil at end of bytecode.
                    let nil_t = self.session.alloc_value(Value::Nil);
                    let stack_base = frame.stack_base;
                    self.call_stack.pop();
                    self.stack.truncate(stack_base);
                    self.stack.push(nil_t);
                    if self.call_stack.is_empty() {
                        break;
                    }
                    continue;
                }
                let op = frame.func.bytecode[frame.ip].clone();
                frame.ip += 1;
                op
            };

            match self.execute_op(op) {
                Ok(()) => {}
                Err(e) => {
                    if let Some(handler) = self.catch_stack.pop() {
                        // Unwind call stack to catch frame's depth
                        while self.call_stack.len() > handler.call_depth {
                            self.call_stack.pop();
                        }
                        // Restore operand stack
                        self.stack.truncate(handler.stack_depth);
                        // Push error message as a string
                        let err_str = e.to_string();
                        let t = self.session.alloc_value(Value::Str(err_str));
                        self.stack.push(t);
                        // Jump to catch block
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.ip = handler.catch_ip;
                        }
                    } else {
                        return Err(e);
                    }
                }
            }

            if self.call_stack.is_empty() {
                break;
            }
        }
        Ok(())
    }

    fn execute_op(&mut self, op: Opcode) -> Result<(), GoblinError> {
        match op {
            // ── Literals ────────────────────────────────────────────────────
            Opcode::LoadConst(idx) => {
                let val = {
                    let frame = self.call_stack.last().unwrap();
                    frame.func.constants[idx as usize].clone()
                };
                let t = self.session.alloc_value(val);
                self.stack.push(t);
            }
            Opcode::LoadNil => {
                let t = self.session.alloc_value(Value::Nil);
                self.stack.push(t);
            }
            Opcode::LoadTrue => {
                let t = self.session.alloc_value(Value::Bool(true));
                self.stack.push(t);
            }
            Opcode::LoadFalse => {
                let t = self.session.alloc_value(Value::Bool(false));
                self.stack.push(t);
            }

            // ── Locals ──────────────────────────────────────────────────────
            Opcode::LoadLocal(slot) => {
                let t = self.call_stack.last().unwrap().load_local(slot)?;
                self.stack.push(t);
            }
            Opcode::StoreLocal(slot) => {
                let t = self.stack_pop()?;
                self.call_stack.last_mut().unwrap().store_local(slot, t);
            }

            // ── Globals ──────────────────────────────────────────────────────
            Opcode::LoadGlobal(idx) => {
                let t = self.session.get_global(idx as usize)
                    .cloned()
                    .ok_or_else(|| GoblinError::Runtime(
                        format!("global slot {idx} is uninitialized")
                    ))?;
                self.stack.push(t);
            }
            Opcode::StoreGlobal(idx) => {
                let t = self.stack_pop()?;
                self.session.set_global(idx as usize, t);
            }

            // ── Upvalues ─────────────────────────────────────────────────────
            Opcode::LoadUpvalue(idx) => {
                let t = self.call_stack.last().unwrap()
                    .upvalues.get(idx as usize)
                    .ok_or_else(|| GoblinError::Runtime(format!("upvalue {idx} out of range")))?
                    .get();
                self.stack.push(t);
            }
            Opcode::StoreUpvalue(idx) => {
                let t = self.stack_pop()?;
                self.call_stack.last_mut().unwrap()
                    .upvalues.get(idx as usize)
                    .ok_or_else(|| GoblinError::Runtime(format!("upvalue {idx} out of range")))?
                    .set(t);
            }

            // ── Stack ────────────────────────────────────────────────────────
            Opcode::Pop => { self.stack_pop()?; }
            Opcode::Dup => {
                let t = self.stack.last()
                    .ok_or(GoblinError::Runtime("dup on empty stack".into()))?
                    .clone();
                self.stack.push(t);
            }

            // ── overwrite! ───────────────────────────────────────────────────
            Opcode::Overwrite => {
                let new_val_tether = self.stack_pop()?;
                let target_tether = self.stack_pop()?;
                let new_val = self.session.read_value(&new_val_tether)?;
                self.session.overwrite(&target_tether, new_val)?;
            }

            // ── Arithmetic ───────────────────────────────────────────────────
            Opcode::Add => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = match (&a, &b) {
                    (Value::Int(x), Value::Int(y))     => Value::Int(x.wrapping_add(*y)),
                    (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                    (Value::Int(x), Value::Float(y))   => Value::Float(*x as f64 + y),
                    (Value::Float(x), Value::Int(y))   => Value::Float(x + *y as f64),
                    (Value::Str(x), Value::Str(y))     => Value::Str(format!("{}{}", x, y)),
                    (Value::Big(x), Value::Big(y))     => Value::Big(x + y),
                    (Value::Big(x), Value::Int(y))     => Value::Big(x + rust_decimal::Decimal::from(*y)),
                    (Value::Int(x), Value::Big(y))     => Value::Big(rust_decimal::Decimal::from(*x) + y),
                    (Value::Pct(x), Value::Pct(y))     => Value::Pct(x + y),
                    (Value::Pct(x), Value::Float(y))   => Value::Pct(x + y),
                    (Value::Float(x), Value::Pct(y))   => Value::Pct(x + y),
                    (Value::Char(c), Value::Int(n))    => {
                        let new_cp = (*c as i64).wrapping_add(*n) as u32;
                        Value::Char(char::from_u32(new_cp).unwrap_or(*c))
                    }
                    _ => return Err(GoblinError::type_error("number or str", b.type_name(), "+")),
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::AddInt => {
                let b = self.pop_int()?;
                let a = self.pop_int()?;
                let t = self.session.alloc_value(Value::Int(a.wrapping_add(b)));
                self.stack.push(t);
            }
            Opcode::AddFloat => {
                let b = self.pop_float()?;
                let a = self.pop_float()?;
                let t = self.session.alloc_value(Value::Float(a + b));
                self.stack.push(t);
            }
            Opcode::Sub => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = self.arith_op(a, b, "sub", |x, y| x - y, |x, y| x - y)?;
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::SubInt => {
                let b = self.pop_int()?; let a = self.pop_int()?;
                let t = self.session.alloc_value(Value::Int(a.wrapping_sub(b)));
                self.stack.push(t);
            }
            Opcode::SubFloat => {
                let b = self.pop_float()?; let a = self.pop_float()?;
                let t = self.session.alloc_value(Value::Float(a - b));
                self.stack.push(t);
            }
            Opcode::Mul => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = self.arith_op(a, b, "mul", |x, y| x * y, |x, y| x * y)?;
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::MulInt => {
                let b = self.pop_int()?; let a = self.pop_int()?;
                let t = self.session.alloc_value(Value::Int(a.wrapping_mul(b)));
                self.stack.push(t);
            }
            Opcode::MulFloat => {
                let b = self.pop_float()?; let a = self.pop_float()?;
                let t = self.session.alloc_value(Value::Float(a * b));
                self.stack.push(t);
            }
            Opcode::Div => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = match (&a, &b) {
                    (Value::Int(_), Value::Int(0)) | (Value::Float(_), Value::Float(_))
                        if matches!(&b, Value::Int(0)) => {
                        return Err(GoblinError::DivisionByZero);
                    }
                    (Value::Int(x), Value::Int(y)) => {
                        if *y == 0 { return Err(GoblinError::DivisionByZero); }
                        Value::Int(x / y)
                    }
                    (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
                    (Value::Int(x), Value::Float(y))   => Value::Float(*x as f64 / y),
                    (Value::Float(x), Value::Int(y))   => Value::Float(x / *y as f64),
                    (Value::Big(x), Value::Big(y))     => {
                        if y.is_zero() { return Err(GoblinError::DivisionByZero); }
                        Value::Big(x / y)
                    }
                    (Value::Big(x), Value::Int(y))     => {
                        if *y == 0 { return Err(GoblinError::DivisionByZero); }
                        Value::Big(x / rust_decimal::Decimal::from(*y))
                    }
                    (Value::Pct(x), Value::Pct(y))     => Value::Pct(x / y),
                    (Value::Pct(x), Value::Float(y))   => Value::Pct(x / y),
                    _ => return Err(GoblinError::type_error("number", b.type_name(), "/")),
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::DivInt => {
                let b = self.pop_int()?; let a = self.pop_int()?;
                if b == 0 { return Err(GoblinError::DivisionByZero); }
                let t = self.session.alloc_value(Value::Int(a / b));
                self.stack.push(t);
            }
            Opcode::DivFloat => {
                let b = self.pop_float()?; let a = self.pop_float()?;
                let t = self.session.alloc_value(Value::Float(a / b));
                self.stack.push(t);
            }
            Opcode::Rem => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = match (&a, &b) {
                    (Value::Int(x), Value::Int(y)) => {
                        if *y == 0 { return Err(GoblinError::DivisionByZero); }
                        Value::Int(x % y)
                    }
                    (Value::Float(x), Value::Float(y)) => Value::Float(x % y),
                    _ => return Err(GoblinError::type_error("number", b.type_name(), "%")),
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::RemInt => {
                let b = self.pop_int()?; let a = self.pop_int()?;
                if b == 0 { return Err(GoblinError::DivisionByZero); }
                let t = self.session.alloc_value(Value::Int(a % b));
                self.stack.push(t);
            }
            Opcode::Neg => {
                let a = self.pop_value()?;
                let result = match &a {
                    Value::Int(x)   => Value::Int(-x),
                    Value::Float(x) => Value::Float(-x),
                    Value::Big(x)   => Value::Big(-x),
                    Value::Pct(x)   => Value::Pct(-x),
                    _ => return Err(GoblinError::type_error("number", a.type_name(), "neg")),
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::NegInt => {
                let a = self.pop_int()?;
                let t = self.session.alloc_value(Value::Int(-a));
                self.stack.push(t);
            }
            Opcode::NegFloat => {
                let a = self.pop_float()?;
                let t = self.session.alloc_value(Value::Float(-a));
                self.stack.push(t);
            }
            Opcode::Concat => {
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let result = match (&a, &b) {
                    (Value::Str(x), Value::Str(y)) => Value::Str(format!("{}{}", x, y)),
                    (Value::Array(x), Value::Array(y)) => {
                        let mut v = x.clone();
                        v.extend_from_slice(y);
                        Value::Array(v)
                    }
                    (Value::Char(x), Value::Char(y)) => Value::Str(format!("{}{}", x, y)),
                    (Value::Char(x), Value::Str(y))  => Value::Str(format!("{}{}", x, y)),
                    (Value::Str(x), Value::Char(y))  => Value::Str(format!("{}{}", x, y)),
                    _ => return Err(GoblinError::type_error("string or array", a.type_name(), "<>")),
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }

            // ── Comparison ───────────────────────────────────────────────────
            Opcode::Eq => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let t = self.session.alloc_value(Value::Bool(a == b));
                self.stack.push(t);
            }
            Opcode::Ne => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let t = self.session.alloc_value(Value::Bool(a != b));
                self.stack.push(t);
            }
            Opcode::Lt => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let result = self.compare_values(&a, &b, "<")?;
                let t = self.session.alloc_value(Value::Bool(result < 0));
                self.stack.push(t);
            }
            Opcode::Le => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let result = self.compare_values(&a, &b, "<=")?;
                let t = self.session.alloc_value(Value::Bool(result <= 0));
                self.stack.push(t);
            }
            Opcode::Gt => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let result = self.compare_values(&a, &b, ">")?;
                let t = self.session.alloc_value(Value::Bool(result > 0));
                self.stack.push(t);
            }
            Opcode::Ge => {
                let b = self.pop_value()?; let a = self.pop_value()?;
                let result = self.compare_values(&a, &b, ">=")?;
                let t = self.session.alloc_value(Value::Bool(result >= 0));
                self.stack.push(t);
            }
            Opcode::Not => {
                let a = self.pop_value()?;
                let t = self.session.alloc_value(Value::Bool(!a.is_truthy()));
                self.stack.push(t);
            }

            // ── Control flow ─────────────────────────────────────────────────
            Opcode::Jump(offset) => {
                let frame = self.call_stack.last_mut().unwrap();
                frame.ip = (frame.ip as isize + offset as isize) as usize;
            }
            Opcode::JumpIfFalse(offset) => {
                let cond = self.pop_value()?;
                if !cond.is_truthy() {
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.ip = (frame.ip as isize + offset as isize) as usize;
                }
            }
            Opcode::JumpIfTrue(offset) => {
                let cond = self.pop_value()?;
                if cond.is_truthy() {
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.ip = (frame.ip as isize + offset as isize) as usize;
                }
            }

            // ── Collections ──────────────────────────────────────────────────
            Opcode::MakeArray(n) => {
                let count = n as usize;
                let start = self.stack.len().saturating_sub(count);
                let mut items = Vec::with_capacity(count);
                for i in start..self.stack.len() {
                    let v = self.session.read_value(&self.stack[i])?;
                    items.push(v);
                }
                self.stack.truncate(start);
                let coll = CollectionValue::from_flat(items);
                let t = self.session.alloc_value(Value::Collection(Rc::new(coll)));
                self.stack.push(t);
            }
            Opcode::MakeMap(n) => {
                let pair_count = n as usize;
                let start = self.stack.len().saturating_sub(pair_count * 2);
                let mut pairs = Vec::with_capacity(pair_count);
                let mut all_str_keys = true;
                for i in (start..self.stack.len()).step_by(2) {
                    if i + 1 < self.stack.len() {
                        let k = self.session.read_value(&self.stack[i])?;
                        let v = self.session.read_value(&self.stack[i + 1])?;
                        if !matches!(k, Value::Str(_)) { all_str_keys = false; }
                        pairs.push((k, v));
                    }
                }
                self.stack.truncate(start);
                let result = if all_str_keys {
                    let m: std::collections::BTreeMap<String, Value> = pairs.into_iter()
                        .map(|(k, v)| (match k { Value::Str(s) => s, _ => unreachable!() }, v))
                        .collect();
                    Value::Map(m)
                } else {
                    Value::Collection(Rc::new(CollectionValue::from_map(pairs)))
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::GetIndex => {
                let key = self.pop_value()?;
                let coll_val = self.pop_value()?;
                let result = crate::collections::get_index(&coll_val, &key)?;
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }
            Opcode::SetIndex => {
                let new_val = self.pop_value()?;
                let key = self.pop_value()?;
                let coll_val = self.pop_value()?;
                let updated = crate::collections::set_index(coll_val, &key, new_val)?;
                let t = self.session.alloc_value(updated);
                self.stack.push(t);
            }
            Opcode::GetMember(idx) => {
                let key = {
                    let frame = self.call_stack.last().unwrap();
                    frame.func.constants[idx as usize].clone()
                };
                let coll_val = self.pop_value()?;
                let result = if let Value::Str(ref name) = key {
                    member_dispatch(&coll_val, name, &mut self.session)?
                } else {
                    crate::collections::get_index(&coll_val, &key)?
                };
                let t = self.session.alloc_value(result);
                self.stack.push(t);
            }

            // ── Function calls ────────────────────────────────────────────────
            Opcode::Call(argc) => {
                let arg_count = argc as usize;
                if self.stack.len() < arg_count + 1 {
                    return Err(GoblinError::Runtime("stack underflow on Call".into()));
                }
                if self.call_stack.len() >= MAX_CALL_DEPTH {
                    return Err(GoblinError::StackOverflow);
                }

                // Stack: [..., func, arg0, ..., arg_{argc-1}]
                let func_idx = self.stack.len() - arg_count - 1;
                let func_tether = self.stack[func_idx].clone();
                let func_val = self.session.read_value(&func_tether)?;

                // If it's a Builtin value, dispatch directly without a call frame.
                if let Value::Builtin(bid) = &func_val {
                    let bid = *bid;
                    let arg_tethers: Vec<_> = self.stack.drain(func_idx + 1..).collect();
                    self.stack.pop(); // pop the func tether
                    let result = crate::builtins::call_builtin(bid, arg_tethers, &mut self.session)?;
                    self.stack.push(result);
                } else {

                let (func_rc, upvalues) = match func_val {
                    Value::Function(f) => (f, Vec::new()),
                    Value::Closure(c) => (c.func.clone(), c.upvalues.clone()),
                    _ => return Err(GoblinError::NotCallable { got: func_val.type_name() }),
                };

                if arg_count != func_rc.params {
                    return Err(GoblinError::ArityMismatch {
                        expected: func_rc.params,
                        got: arg_count,
                        name: func_rc.name.clone(),
                    });
                }

                let stack_base = func_idx; // caller cleans up from here
                let mut new_frame = CallFrame::new(func_rc, upvalues, stack_base);

                // Move args from stack into frame locals.
                for i in (0..arg_count).rev() {
                    let t = self.stack.pop().unwrap();
                    new_frame.locals[i] = Some(t);
                }
                // Pop function tether.
                self.stack.pop();

                self.call_stack.push(new_frame);
                } // end else (not a Builtin)
            }

            Opcode::Return => {
                // Return value is top of stack (or Nil).
                let ret_val = if self.stack.len() > self.call_stack.last().unwrap().stack_base {
                    self.stack.pop().unwrap()
                } else {
                    self.session.alloc_value(Value::Nil)
                };

                let frame = self.call_stack.pop().unwrap();
                self.stack.truncate(frame.stack_base);
                self.stack.push(ret_val);
            }

            // ── Builtins ──────────────────────────────────────────────────────
            Opcode::CallBuiltin(id, argc) => {
                let arg_count = argc as usize;
                if self.stack.len() < arg_count {
                    return Err(GoblinError::Runtime("stack underflow on CallBuiltin".into()));
                }
                let start = self.stack.len() - arg_count;
                let arg_tethers: Vec<Tether> = self.stack.drain(start..).collect();
                let result = crate::builtins::call_builtin(id, arg_tethers, &mut self.session)?;
                self.stack.push(result);
            }

            // ── Closures ──────────────────────────────────────────────────────
            Opcode::MakeClosure(idx) => {
                let func_rc: Rc<FunctionObject> = {
                    let frame = self.call_stack.last().unwrap();
                    match &frame.func.constants[idx as usize] {
                        Value::Function(f) => f.clone(),
                        _ => return Err(GoblinError::Runtime(
                            format!("MakeClosure: constants[{idx}] is not a Function")
                        )),
                    }
                };

                // Populate upvalues according to descriptors.
                let descriptors = func_rc.upvalue_descriptors.clone();
                let mut upvalues = Vec::with_capacity(descriptors.len());
                for desc in &descriptors {
                    let tether = match desc {
                        UpvalueDescriptor::Local(slot) => {
                            self.call_stack.last().unwrap().load_local(*slot)?
                        }
                        UpvalueDescriptor::Upvalue(uv_idx) => {
                            self.call_stack.last().unwrap()
                                .upvalues.get(*uv_idx as usize)
                                .ok_or_else(|| GoblinError::Runtime(
                                    format!("upvalue forward {uv_idx} out of range")
                                ))?
                                .get()
                        }
                    };
                    upvalues.push(UpvalueCell::new(tether));
                }

                let closure = Closure { func: func_rc, upvalues };
                let t = self.session.alloc_value(Value::Closure(Rc::new(closure)));
                self.stack.push(t);
            }

            Opcode::ToPct => {
                let v = self.pop_value()?;
                let f = match v {
                    Value::Int(n)   => n as f64,
                    Value::Float(f) => f,
                    Value::Pct(p)   => p,
                    other => return Err(GoblinError::type_error("number", other.type_name(), "%")),
                };
                let t = self.session.alloc_value(Value::Pct(f / 100.0));
                self.stack.push(t);
            }

            Opcode::MakePair => {
                // >< divmod operator: Pair(floor(a/b), a mod b)
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                let pair = match (&a, &b) {
                    (Value::Int(x), Value::Int(y)) => {
                        if *y == 0 { return Err(GoblinError::DivisionByZero); }
                        let q = x.div_euclid(*y);
                        let r = x.rem_euclid(*y);
                        Value::Pair(Box::new(Value::Int(q)), Box::new(Value::Int(r)))
                    }
                    (Value::Float(x), Value::Float(y)) => {
                        let q = (x / y).floor();
                        let r = x - q * y;
                        Value::Pair(Box::new(Value::Float(q)), Box::new(Value::Float(r)))
                    }
                    (Value::Big(x), Value::Big(y)) => {
                        if y.is_zero() { return Err(GoblinError::DivisionByZero); }
                        let q = (x / y).floor();
                        let r = x - q * y;
                        Value::Pair(Box::new(Value::Big(q)), Box::new(Value::Big(r)))
                    }
                    _ => return Err(GoblinError::type_error("number", b.type_name(), "><")),
                };
                let t = self.session.alloc_value(pair);
                self.stack.push(t);
            }

            Opcode::MakeRange => {
                let end = self.pop_value()?;
                let start = self.pop_value()?;
                let (s, e) = match (&start, &end) {
                    (Value::Int(s), Value::Int(e)) => (*s, *e),
                    _ => return Err(GoblinError::type_error("int", end.type_name(), "..")),
                };
                let v: Vec<Value> = (s..e).map(Value::Int).collect();
                let t = self.session.alloc_value(Value::Array(v));
                self.stack.push(t);
            }

            Opcode::MakeRangeInclusive => {
                let end = self.pop_value()?;
                let start = self.pop_value()?;
                let (s, e) = match (&start, &end) {
                    (Value::Int(s), Value::Int(e)) => (*s, *e),
                    _ => return Err(GoblinError::type_error("int", end.type_name(), "...")),
                };
                let v: Vec<Value> = (s..=e).map(Value::Int).collect();
                let t = self.session.alloc_value(Value::Array(v));
                self.stack.push(t);
            }

            Opcode::Quick(_) => {
                // Placeholder; the quickening pass fills this in at runtime.
                // For now, treat as no-op (quickening is layered on later).
            }

            Opcode::TryBegin(offset) => {
                let frame = self.call_stack.last().unwrap();
                let catch_ip = (frame.ip as i64 + offset as i64) as usize;
                let call_depth = self.call_stack.len();
                let stack_depth = self.stack.len();
                self.catch_stack.push(CatchFrame {
                    call_depth,
                    stack_depth,
                    catch_ip,
                    frame_ip_index: call_depth - 1,
                });
            }
            Opcode::TryEnd => {
                self.catch_stack.pop();
            }

            Opcode::ImportFile(path_idx) => {
                let path_str = {
                    let frame = self.call_stack.last().ok_or_else(|| GoblinError::Runtime("no call frame".into()))?;
                    match &frame.func.constants[path_idx as usize] {
                        Value::Str(s) => s.clone(),
                        _ => return Err(GoblinError::Runtime("ImportFile: path must be a string constant".into())),
                    }
                };

                // Resolve path against base_dir
                let full_path = if std::path::Path::new(&path_str).is_absolute() {
                    std::path::PathBuf::from(&path_str)
                } else {
                    self.session.base_dir.join(&path_str)
                };

                // Skip if already imported
                let canonical = full_path.to_string_lossy().to_string();
                if self.session.imported.contains(&canonical) {
                    // already imported — skip
                } else {
                    self.session.imported.insert(canonical.clone());

                    // Try .gbln fallback if not found
                    let actual_path = if !full_path.exists() && !path_str.ends_with(".gbln") {
                        let p = full_path.with_extension("gbln");
                        if p.exists() { p } else { full_path.clone() }
                    } else {
                        full_path.clone()
                    };

                    let source = std::fs::read_to_string(&actual_path)
                        .map_err(|e| GoblinError::Runtime(format!("import '{}': {}", actual_path.display(), e)))?;

                    let tokens = goblin_lexer::lex(&source, &actual_path.to_string_lossy())
                        .map_err(|diags| GoblinError::Runtime(diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")))?;

                    let module = goblin_parser::Parser::new(&tokens).parse_module()
                        .map_err(|diags| GoblinError::Runtime(diags.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n")))?;

                    let compiled = crate::compiler::Compiler::new().compile_module(&module)
                        .map_err(|e| GoblinError::Runtime(format!("import compile error: {:?}", e)))?;

                    // Pre-register classes/enums from the imported module
                    for decl in compiled.classes { self.session.classes.insert(decl.name.clone(), decl); }
                    for decl in compiled.enums   { self.session.enums.insert(decl.name.clone(), decl); }

                    // Update base_dir to imported file's directory during its execution
                    let prev_base_dir = self.session.base_dir.clone();
                    if let Some(parent) = actual_path.parent() {
                        self.session.base_dir = parent.to_path_buf();
                    }

                    self.execute(compiled.entry)?;

                    // Restore base_dir
                    self.session.base_dir = prev_base_dir;
                }
            }
        }
        Ok(())
    }

    // ── Quickening ───────────────────────────────────────────────────────────

    /// Replace generic Add/Sub/etc. opcodes in the given function with their
    /// specialised forms, based on the types currently on the stack.
    ///
    /// Called after hot paths are identified; mutates the bytecode in place.
    /// This is safe because each worker has its own private bytecode copy.
    pub fn quicken(&self, _func: &mut FunctionObject) {
        // TODO: implement full quickening pass.
        // Inspect each generic op (Add, Sub, …) and check the constant or
        // local type to decide if it can be replaced with AddInt/AddFloat/Concat.
    }

    // ── Helpers ───────────────────────────────────────────────────────────────

    fn stack_pop(&mut self) -> Result<Tether, GoblinError> {
        self.stack.pop().ok_or_else(|| GoblinError::Runtime("stack underflow".into()))
    }

    fn pop_value(&mut self) -> Result<Value, GoblinError> {
        let t = self.stack_pop()?;
        self.session.read_value(&t)
    }

    fn pop_int(&mut self) -> Result<i64, GoblinError> {
        match self.pop_value()? {
            Value::Int(n) => Ok(n),
            other => Err(GoblinError::type_error("int", other.type_name(), "arithmetic")),
        }
    }

    fn pop_float(&mut self) -> Result<f64, GoblinError> {
        match self.pop_value()? {
            Value::Float(f) => Ok(f),
            Value::Int(n)   => Ok(n as f64),
            other => Err(GoblinError::type_error("float", other.type_name(), "arithmetic")),
        }
    }

    fn arith_op(
        &self,
        a: Value,
        b: Value,
        op: &'static str,
        int_fn: fn(i64, i64) -> i64,
        flt_fn: fn(f64, f64) -> f64,
    ) -> Result<Value, GoblinError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y))     => Ok(Value::Int(int_fn(*x, *y))),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(flt_fn(*x, *y))),
            (Value::Int(x), Value::Float(y))   => Ok(Value::Float(flt_fn(*x as f64, *y))),
            (Value::Float(x), Value::Int(y))   => Ok(Value::Float(flt_fn(*x, *y as f64))),
            (Value::Big(x), Value::Big(y))     => {
                let xf = x.to_string().parse::<f64>().unwrap_or(f64::NAN);
                let yf = y.to_string().parse::<f64>().unwrap_or(f64::NAN);
                // Use decimal arithmetic for sub/mul; we use the flt_fn to determine op
                // For sub: int_fn(1,1)=0 vs mul: int_fn(2,3)=6; detect by testing int_fn
                // Simple approach: just use Decimal native ops via a discriminant
                let result_f = flt_fn(xf, yf);
                let d = rust_decimal::Decimal::try_from(result_f)
                    .unwrap_or_else(|_| *x - *y);
                Ok(Value::Big(d))
            }
            (Value::Big(x), Value::Int(y))     => {
                let xf = x.to_string().parse::<f64>().unwrap_or(f64::NAN);
                let d = rust_decimal::Decimal::try_from(flt_fn(xf, *y as f64))
                    .unwrap_or_else(|_| *x);
                Ok(Value::Big(d))
            }
            (Value::Int(x), Value::Big(y))     => {
                let yf = y.to_string().parse::<f64>().unwrap_or(f64::NAN);
                let d = rust_decimal::Decimal::try_from(flt_fn(*x as f64, yf))
                    .unwrap_or_else(|_| *y);
                Ok(Value::Big(d))
            }
            (Value::Pct(x), Value::Pct(y))     => Ok(Value::Pct(flt_fn(*x, *y))),
            (Value::Pct(x), Value::Float(y))   => Ok(Value::Pct(flt_fn(*x, *y))),
            (Value::Float(x), Value::Pct(y))   => Ok(Value::Pct(flt_fn(*x, *y))),
            _ => Err(GoblinError::type_error("number", b.type_name(), op)),
        }
    }

    /// Returns negative, zero, or positive for ordering.
    fn compare_values(&self, a: &Value, b: &Value, op: &'static str) -> Result<i32, GoblinError> {
        use std::cmp::Ordering;
        let ord_to_i32 = |o: Ordering| match o { Ordering::Less => -1, Ordering::Equal => 0, Ordering::Greater => 1 };
        match (a, b) {
            (Value::Int(x), Value::Int(y))     => Ok(ord_to_i32(x.cmp(y))),
            (Value::Float(x), Value::Float(y)) => Ok(x.partial_cmp(y).map(ord_to_i32).unwrap_or(0)),
            (Value::Int(x), Value::Float(y))   => Ok((*x as f64).partial_cmp(y).map(ord_to_i32).unwrap_or(0)),
            (Value::Float(x), Value::Int(y))   => Ok(x.partial_cmp(&(*y as f64)).map(ord_to_i32).unwrap_or(0)),
            (Value::Str(x), Value::Str(y))     => Ok(ord_to_i32(x.cmp(y))),
            (Value::Char(x), Value::Char(y))   => Ok(ord_to_i32(x.cmp(y))),
            (Value::Big(x), Value::Big(y))     => Ok(ord_to_i32(x.cmp(y))),
            (Value::Big(x), Value::Int(y))     => Ok(ord_to_i32(x.cmp(&rust_decimal::Decimal::from(*y)))),
            (Value::Int(x), Value::Big(y))     => Ok(ord_to_i32(rust_decimal::Decimal::from(*x).cmp(y))),
            (Value::Pct(x), Value::Pct(y))     => Ok(x.partial_cmp(y).map(ord_to_i32).unwrap_or(0)),
            _ => Err(GoblinError::type_error("comparable", b.type_name(), op)),
        }
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn value_to_str(v: &Value) -> String {
    crate::builtins::value_to_str(v)
}

/// Handle `.method` postfix member accesses that dispatch to builtins.
fn member_dispatch(v: &Value, name: &str, session: &mut Session) -> Result<Value, GoblinError> {
    use crate::value::BuiltinId;
    use crate::builtins::call_builtin;

    // Try builtin method dispatch first
    let bid = match name {
        "str" | "string"   => Some(BuiltinId::ToStr),
        "int"              => Some(BuiltinId::ToInt),
        "float"            => Some(BuiltinId::ToFloat),
        "bool"             => Some(BuiltinId::ToBool),
        "count" | "len" | "length" => Some(BuiltinId::Count),
        "lower"            => Some(BuiltinId::Lower),
        "upper"            => Some(BuiltinId::Upper),
        "title"            => Some(BuiltinId::Title),
        "slug"             => Some(BuiltinId::Slug),
        "mixed"            => Some(BuiltinId::Mixed),
        "raw"              => Some(BuiltinId::Raw),
        "trim"             => Some(BuiltinId::Trim),
        "trim_lead"        => Some(BuiltinId::TrimLead),
        "trim_trail"       => Some(BuiltinId::TrimTrail),
        "reverse"          => Some(BuiltinId::Reverse),
        "shuffle"          => Some(BuiltinId::Shuffle),
        "sort"             => Some(BuiltinId::Sort),
        "unique"           => Some(BuiltinId::Unique),
        "dups"             => Some(BuiltinId::Dups),
        "pack"             => Some(BuiltinId::Pack),
        "unpack"           => Some(BuiltinId::Unpack),
        "keys"             => Some(BuiltinId::Keys),
        "values"           => Some(BuiltinId::Values),
        "items"            => Some(BuiltinId::Items),
        "lines"            => Some(BuiltinId::Lines),
        "words"            => Some(BuiltinId::Words),
        "chars"            => Some(BuiltinId::Chars),
        "abs"              => Some(BuiltinId::Abs),
        "sqrt"             => Some(BuiltinId::Sqrt),
        "floor"            => Some(BuiltinId::Floor),
        "ceil"             => Some(BuiltinId::Ceil),
        "round"            => Some(BuiltinId::Round),
        "type_of"          => Some(BuiltinId::TypeOf),
        "pct"              => Some(BuiltinId::Pct),
        "is_big"           => Some(BuiltinId::IsBig),
        "is_pct"           => Some(BuiltinId::IsPct),
        "is_num"           => Some(BuiltinId::IsNum),
        "is_char"          => Some(BuiltinId::IsChar),
        "is_pair"          => Some(BuiltinId::IsPair),
        "is_seq"           => Some(BuiltinId::IsSeq),
        "is_unit"          => Some(BuiltinId::IsUnit),
        "is_alnum"         => Some(BuiltinId::IsAlnum),
        "is_alpha"         => Some(BuiltinId::IsAlpha),
        "is_digit"         => Some(BuiltinId::IsDigit),
        "is_whitespace"    => Some(BuiltinId::IsWhitespace),
        "is_even"          => Some(BuiltinId::IsEven),
        "is_odd"           => Some(BuiltinId::IsOdd),
        "is_positive"      => Some(BuiltinId::IsPositive),
        "is_negative"      => Some(BuiltinId::IsNegative),
        "is_nix"           => Some(BuiltinId::IsNix),
        "is_empty"         => Some(BuiltinId::IsEmpty),
        "is_matching"      => None,  // needs arg; handled below
        "before"           => None,  // needs arg; handled below
        "after"            => None,  // needs arg; handled below
        "before_last"      => None,
        "after_last"       => None,
        "reverse_chars"    => Some(BuiltinId::ReverseChars),
        "minimize"         => Some(BuiltinId::Minimize),
        "sanitize_bom"     => Some(BuiltinId::SanitizeBom),
        "normalize_newlines" => Some(BuiltinId::NormalizeNewlines),
        "json_stringify"   => Some(BuiltinId::JsonStringify),
        "json_stringify_pretty" => Some(BuiltinId::JsonStringifyPretty),
        "flatten"          => Some(BuiltinId::Flatten),
        "pairs"            => Some(BuiltinId::Pairs),
        "is_nil"           => Some(BuiltinId::IsNil),
        "is_bool"          => Some(BuiltinId::IsBool),
        "is_int"           => Some(BuiltinId::IsInt),
        "is_float"         => Some(BuiltinId::IsFloat),
        "is_str"           => Some(BuiltinId::IsStr),
        "is_array"         => Some(BuiltinId::IsArray),
        "is_map"           => Some(BuiltinId::IsMap),
        "parse_bool"       => Some(BuiltinId::ParseBool),
        "get_first"        => Some(BuiltinId::GetFirst),
        "get_last"         => Some(BuiltinId::GetLast),
        "get_all"          => Some(BuiltinId::GetAll),
        "get_random"       => Some(BuiltinId::GetRandom),
        "delete_first"     => Some(BuiltinId::DeleteFirst),
        "delete_last"      => Some(BuiltinId::DeleteLast),
        "delete_random"    => Some(BuiltinId::DeleteRandom),
        "delete_all"       => Some(BuiltinId::DeleteAll),
        "reap_first"       => Some(BuiltinId::ReapFirst2),
        "reap_last"        => Some(BuiltinId::ReapLast2),
        "reap_random"      => Some(BuiltinId::ReapRandom2),
        "sum"              => Some(BuiltinId::Sum),
        "avg"              => Some(BuiltinId::Avg),
        "min"              => Some(BuiltinId::Min),
        "max"              => Some(BuiltinId::Max),
        "range"            => Some(BuiltinId::Range),
        "freq"             => Some(BuiltinId::Freq),
        "mode"             => Some(BuiltinId::Mode),
        "big"              => Some(BuiltinId::ToBig),
        "to_map"           => Some(BuiltinId::ToMap),
        "valtype" | "vt"   => Some(BuiltinId::TypeOf),
        "find_all"         => Some(BuiltinId::FindAll),
        "json_parse"       => Some(BuiltinId::JsonParse),
        "yall_write"       => Some(BuiltinId::YallWrite),
        "yall_pretty"      => Some(BuiltinId::YallPretty),
        "yall_minify"      => Some(BuiltinId::YallMinify),
        "is_control"       => Some(BuiltinId::IsControl),
        "is_multiple_of"   => None,  // needs arg
        "is_type"          => None,  // needs arg
        "is_bound_name"    => None,  // needs arg
        "is_matching"      => None,  // needs arg; handled below
        "i8"  => Some(BuiltinId::CastI8),
        "i16" => Some(BuiltinId::CastI16),
        "i32" => Some(BuiltinId::CastI32),
        "i64" => Some(BuiltinId::CastI64),
        "u8"  => Some(BuiltinId::CastU8),
        "u16" => Some(BuiltinId::CastU16),
        "u32" => Some(BuiltinId::CastU32),
        "u64" => Some(BuiltinId::CastU64),
        "f32" => Some(BuiltinId::CastF32),
        "f64" => Some(BuiltinId::CastF64),
        // path/filesystem postfix methods
        "basename"         => Some(BuiltinId::Basename),
        "dirname"          => Some(BuiltinId::Dirname),
        "stem"             => Some(BuiltinId::Stem),
        "ext"              => Some(BuiltinId::Ext),
        "path_split"       => Some(BuiltinId::PathSplit),
        "path_normalize"   => Some(BuiltinId::PathNormalize),
        "file_exists"      => Some(BuiltinId::FileExists),
        "is_file"          => Some(BuiltinId::IsFile),
        "is_dir"           => Some(BuiltinId::IsDir),
        "escape_html"      => Some(BuiltinId::EscapeHtml),
        _ => None,
    };

    if let Some(id) = bid {
        let tether = session.alloc_value(v.clone());
        let result_tether = call_builtin(id, vec![tether], session)?;
        return session.read_value(&result_tether);
    }

    // Fall through to map/collection field lookup
    crate::collections::get_index(v, &Value::Str(name.to_string()))
}


// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::GcMode;

    fn make_vm() -> Vm {
        Vm::new(Session::new(GcMode::Off))
    }

    fn simple_func(bytecode: Vec<Opcode>, constants: Vec<Value>) -> FunctionObject {
        FunctionObject {
            bytecode,
            constants,
            locals: 0,
            params: 0,
            name: "test".into(),
            upvalue_descriptors: Vec::new(),
        }
    }

    #[test]
    fn returns_constant() {
        let mut vm = make_vm();
        let result = vm.execute(simple_func(
            vec![Opcode::LoadConst(0), Opcode::Return],
            vec![Value::Int(42)],
        )).unwrap();
        assert!(matches!(result, Value::Int(42)));
    }

    #[test]
    fn add_ints() {
        let mut vm = make_vm();
        let result = vm.execute(simple_func(
            vec![Opcode::LoadConst(0), Opcode::LoadConst(1), Opcode::AddInt, Opcode::Return],
            vec![Value::Int(3), Value::Int(4)],
        )).unwrap();
        assert!(matches!(result, Value::Int(7)));
    }

    #[test]
    fn store_and_load_local() {
        let mut vm = make_vm();
        let func = FunctionObject {
            bytecode: vec![
                Opcode::LoadConst(0),  // push 10
                Opcode::StoreLocal(0), // store in slot 0
                Opcode::LoadLocal(0),  // load from slot 0
                Opcode::Return,
            ],
            constants: vec![Value::Int(10)],
            locals: 1,
            params: 0,
            name: "test".into(),
            upvalue_descriptors: Vec::new(),
        };
        let result = vm.execute(func).unwrap();
        assert!(matches!(result, Value::Int(10)));
    }

    #[test]
    fn if_else_jump() {
        let mut vm = make_vm();
        // if false { 1 } else { 2 }
        // LoadFalse, JumpIfFalse(+2), LoadConst(0)[=1], Jump(+1), LoadConst(1)[=2], Return
        let func = FunctionObject {
            bytecode: vec![
                Opcode::LoadFalse,
                Opcode::JumpIfFalse(2),  // skip next 2 instructions
                Opcode::LoadConst(0),    // push 1 (skipped)
                Opcode::Jump(1),         // jump past else (skipped)
                Opcode::LoadConst(1),    // push 2
                Opcode::Return,
            ],
            constants: vec![Value::Int(1), Value::Int(2)],
            locals: 0,
            params: 0,
            name: "if_else".into(),
            upvalue_descriptors: Vec::new(),
        };
        let result = vm.execute(func).unwrap();
        assert!(matches!(result, Value::Int(2)));
    }

    #[test]
    fn function_call() {
        let mut vm = make_vm();

        // Inner function: takes one arg (slot 0), returns it + 1
        let inner = FunctionObject {
            bytecode: vec![
                Opcode::LoadLocal(0),   // arg
                Opcode::LoadConst(0),   // 1
                Opcode::AddInt,
                Opcode::Return,
            ],
            constants: vec![Value::Int(1)],
            locals: 1,
            params: 1,
            name: "add1".into(),
            upvalue_descriptors: Vec::new(),
        };

        // Outer: create inner, call with 5, return result
        let outer = FunctionObject {
            bytecode: vec![
                Opcode::LoadConst(0),  // push the function (inner)
                Opcode::LoadConst(1),  // push 5 (arg)
                Opcode::Call(1),       // call with 1 arg
                Opcode::Return,
            ],
            constants: vec![Value::Function(Rc::new(inner)), Value::Int(5)],
            locals: 0,
            params: 0,
            name: "outer".into(),
            upvalue_descriptors: Vec::new(),
        };

        let result = vm.execute(outer).unwrap();
        assert!(matches!(result, Value::Int(6)));
    }

    #[test]
    fn make_array() {
        let mut vm = make_vm();
        let func = simple_func(
            vec![
                Opcode::LoadConst(0), // 1
                Opcode::LoadConst(1), // 2
                Opcode::LoadConst(2), // 3
                Opcode::MakeArray(3),
                Opcode::Return,
            ],
            vec![Value::Int(1), Value::Int(2), Value::Int(3)],
        );
        let result = vm.execute(func).unwrap();
        match result {
            Value::Collection(c) => assert_eq!(c.len(), 3),
            _ => panic!("expected collection"),
        }
    }
}
