use crate::session::Session;
use crate::value::{Tether, Value};
use crate::builtins;

// ──────────────────────────────────────────────
// Opcodes — per blueprint §3
// Stack for expressions; fixed numeric slots for locals.
// ──────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Opcode {
    // Constants / locals
    LoadConst(u16),
    LoadLocal(u16),
    StoreLocal(u16),

    // Int arithmetic (specialised fast paths)
    AddInt, SubInt, MulInt, DivInt, ModInt, NegInt,

    // Float arithmetic
    AddFloat, SubFloat, MulFloat, DivFloat, NegFloat,

    // String
    Concat,

    // Comparison (specialised)
    EqInt, EqFloat, EqStr, EqGeneric,
    LtInt, LtFloat, LtGeneric,
    LeInt, LeFloat, LeGeneric,
    GtInt, GtFloat, GtGeneric,
    GeInt, GeFloat, GeGeneric,
    NotEq,

    // Logic
    Not,

    // Collections
    MakeArray(u16),
    MakeMap(u16),
    GetIndex,
    SetIndex,

    // Control flow
    Jump(i32),
    JumpIfFalse(i32),
    JumpIfTrue(i32),

    // Calls
    Call(u8),
    CallBuiltin { name: &'static str, argc: u8 },

    // Stack management
    Pop,
    Dup,

    // Return
    Return,
}

// ──────────────────────────────────────────────
// FunctionObject
// ──────────────────────────────────────────────

/// A compiled Goblin function.
/// Variable names are resolved to slot indices at compile time — no runtime hashmaps.
#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub bytecode: Vec<Opcode>,
    pub constants: Vec<Value>,
    pub locals: usize,
    pub params: usize,
    pub name: String,
}

// ──────────────────────────────────────────────
// Frame
// ──────────────────────────────────────────────

pub struct Frame {
    pub locals: Vec<Option<Tether>>,
    pub ip: usize,
    pub func: FunctionObject,
}

// ──────────────────────────────────────────────
// VM
// ──────────────────────────────────────────────

pub struct Vm<'a> {
    pub session: &'a mut Session,
    pub stack: Vec<Tether>,
    pub frame: Frame,
}

impl<'a> Vm<'a> {
    pub fn new(session: &'a mut Session, func: FunctionObject) -> Self {
        let locals = vec![None; func.locals];
        Vm { session, stack: Vec::new(), frame: Frame { locals, ip: 0, func } }
    }

    #[inline]
    fn push(&mut self, t: Tether) { self.stack.push(t); }

    #[inline]
    fn pop(&mut self) -> Result<Tether, String> {
        self.stack.pop().ok_or_else(|| "stack underflow".to_string())
    }

    #[inline]
    fn pop_value(&mut self) -> Result<Value, String> {
        let t = self.pop()?;
        Ok(self.session.read_value(&t))
    }

    #[inline]
    fn push_value(&mut self, v: Value) {
        let t = self.session.alloc_value(v);
        self.push(t);
    }

    pub fn run(&mut self) -> Result<(), String> {
        loop {
            if self.frame.ip >= self.frame.func.bytecode.len() { break; }
            let op = self.frame.func.bytecode[self.frame.ip].clone();
            self.frame.ip += 1;

            match op {
                // ── Constants / locals ────────────────────────────────
                Opcode::LoadConst(idx) => {
                    let v = self.frame.func.constants[idx as usize].clone();
                    self.push_value(v);
                }
                Opcode::LoadLocal(slot) => {
                    let t = self.frame.locals[slot as usize]
                        .clone()
                        .ok_or_else(|| format!("uninitialized local slot {slot}"))?;
                    self.push(t);
                }
                Opcode::StoreLocal(slot) => {
                    let t = self.pop()?;
                    let slot = slot as usize;
                    if slot >= self.frame.locals.len() {
                        self.frame.locals.resize(slot + 1, None);
                    }
                    self.frame.locals[slot] = Some(t);
                }

                // ── Int arithmetic ────────────────────────────────────
                Opcode::AddInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Int(a + b));
                }
                Opcode::SubInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Int(a - b));
                }
                Opcode::MulInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Int(a * b));
                }
                Opcode::DivInt => {
                    let (a, b) = self.pop2_int()?;
                    if b == 0 { return Err("division by zero".to_string()); }
                    self.push_value(Value::Int(a / b));
                }
                Opcode::ModInt => {
                    let (a, b) = self.pop2_int()?;
                    if b == 0 { return Err("modulo by zero".to_string()); }
                    self.push_value(Value::Int(a % b));
                }
                Opcode::NegInt => {
                    let a = self.pop_value()?;
                    match a {
                        Value::Int(x) => self.push_value(Value::Int(-x)),
                        _ => return Err("NegInt: expected Int".to_string()),
                    }
                }

                // ── Float arithmetic ──────────────────────────────────
                Opcode::AddFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Float(a + b));
                }
                Opcode::SubFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Float(a - b));
                }
                Opcode::MulFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Float(a * b));
                }
                Opcode::DivFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Float(a / b));
                }
                Opcode::NegFloat => {
                    let a = self.pop_value()?;
                    match a {
                        Value::Float(x) => self.push_value(Value::Float(-x)),
                        _ => return Err("NegFloat: expected Float".to_string()),
                    }
                }

                // ── String ────────────────────────────────────────────
                Opcode::Concat => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    match (a, b) {
                        (Value::Str(x), Value::Str(y)) => self.push_value(Value::Str(x + &y)),
                        _ => return Err("Concat: expected two Str values".to_string()),
                    }
                }

                // ── Comparison ────────────────────────────────────────
                Opcode::EqInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Bool(a == b));
                }
                Opcode::EqFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Bool(a == b));
                }
                Opcode::EqStr => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    match (a, b) {
                        (Value::Str(x), Value::Str(y)) => self.push_value(Value::Bool(x == y)),
                        _ => return Err("EqStr: expected two Str".to_string()),
                    }
                }
                Opcode::EqGeneric => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(values_equal(&a, &b)));
                }
                Opcode::LtInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Bool(a < b));
                }
                Opcode::LtFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Bool(a < b));
                }
                Opcode::LtGeneric => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(value_lt(&a, &b)));
                }
                Opcode::LeInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Bool(a <= b));
                }
                Opcode::LeFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Bool(a <= b));
                }
                Opcode::LeGeneric => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(values_equal(&a, &b) || value_lt(&a, &b)));
                }
                Opcode::GtInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Bool(a > b));
                }
                Opcode::GtFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Bool(a > b));
                }
                Opcode::GtGeneric => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(!values_equal(&a, &b) && !value_lt(&a, &b)));
                }
                Opcode::GeInt => {
                    let (a, b) = self.pop2_int()?;
                    self.push_value(Value::Bool(a >= b));
                }
                Opcode::GeFloat => {
                    let (a, b) = self.pop2_float()?;
                    self.push_value(Value::Bool(a >= b));
                }
                Opcode::GeGeneric => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(!value_lt(&a, &b)));
                }
                Opcode::NotEq => {
                    let b = self.pop_value()?;
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(!values_equal(&a, &b)));
                }

                // ── Logic ─────────────────────────────────────────────
                Opcode::Not => {
                    let a = self.pop_value()?;
                    self.push_value(Value::Bool(!is_truthy(&a)));
                }

                // ── Collections ───────────────────────────────────────
                Opcode::MakeArray(count) => {
                    let count = count as usize;
                    let mut items: Vec<Value> = Vec::with_capacity(count);
                    for _ in 0..count { items.push(self.pop_value()?); }
                    items.reverse();
                    let coll = crate::value::CollectionValue::from_vec(items);
                    self.push_value(Value::Collection(std::rc::Rc::new(coll)));
                }
                Opcode::MakeMap(count) => {
                    let count = count as usize;
                    let mut pairs: Vec<(String, Value)> = Vec::with_capacity(count);
                    for _ in 0..count {
                        let v = self.pop_value()?;
                        let k = match self.pop_value()? {
                            Value::Str(s) => s,
                            _ => return Err("MakeMap: keys must be strings".to_string()),
                        };
                        pairs.push((k, v));
                    }
                    pairs.reverse();
                    let map: indexmap::IndexMap<String, Value> = pairs.into_iter().collect();
                    let coll = crate::value::CollectionValue::from_indexmap(map);
                    self.push_value(Value::Collection(std::rc::Rc::new(coll)));
                }
                Opcode::GetIndex => {
                    let idx = self.pop_value()?;
                    let coll = self.pop_value()?;
                    match (coll, idx) {
                        (Value::Collection(c), Value::Int(i)) => {
                            let v = c.as_seq_slice()
                                .and_then(|s| s.get(i as usize))
                                .cloned()
                                .unwrap_or(Value::Nil);
                            self.push_value(v);
                        }
                        (Value::Collection(c), Value::Str(k)) => {
                            let v = c.map_get(&k).cloned().unwrap_or(Value::Nil);
                            self.push_value(v);
                        }
                        _ => return Err("GetIndex: unsupported type".to_string()),
                    }
                }
                Opcode::SetIndex => {
                    let val = self.pop_value()?;
                    let idx = self.pop_value()?;
                    let coll = self.pop_value()?;
                    match (coll, idx) {
                        (Value::Collection(c), Value::Int(i)) => {
                            let mut v = c.to_vec();
                            let i = i as usize;
                            if i < v.len() { v[i] = val; }
                            else { return Err("SetIndex: out of bounds".to_string()); }
                            let updated = crate::value::CollectionValue::from_vec(v);
                            self.push_value(Value::Collection(std::rc::Rc::new(updated)));
                        }
                        _ => return Err("SetIndex: unsupported".to_string()),
                    }
                }

                // ── Control flow ──────────────────────────────────────
                Opcode::Jump(offset) => {
                    self.frame.ip = (self.frame.ip as i64 + offset as i64) as usize;
                }
                Opcode::JumpIfFalse(offset) => {
                    let cond = self.pop_value()?;
                    if !is_truthy(&cond) {
                        self.frame.ip = (self.frame.ip as i64 + offset as i64) as usize;
                    }
                }
                Opcode::JumpIfTrue(offset) => {
                    let cond = self.pop_value()?;
                    if is_truthy(&cond) {
                        self.frame.ip = (self.frame.ip as i64 + offset as i64) as usize;
                    }
                }

                // ── Builtins ──────────────────────────────────────────
                Opcode::CallBuiltin { name, argc } => {
                    let argc = argc as usize;
                    if self.stack.len() < argc {
                        return Err(format!(
                            "builtin '{name}': need {argc} args, stack has {}",
                            self.stack.len()
                        ));
                    }
                    let tethers: Vec<Tether> = self.stack
                        .drain(self.stack.len() - argc..)
                        .collect();
                    let args: Vec<Value> = tethers.iter()
                        .map(|t| self.session.read_value(t))
                        .collect();
                    let result = builtins::call(name, &args)
                        .map_err(|e| format!("builtin '{name}': {e}"))?;
                    self.push_value(result);
                }

                // ── User function call (future) ────────────────────────
                Opcode::Call(_argc) => {
                    return Err("user function calls not yet implemented".to_string());
                }

                // ── Stack management ──────────────────────────────────
                Opcode::Pop => { self.pop()?; }
                Opcode::Dup => {
                    let t = self.stack.last()
                        .cloned()
                        .ok_or_else(|| "Dup: empty stack".to_string())?;
                    self.push(t);
                }

                // ── Return ────────────────────────────────────────────
                Opcode::Return => break,
            }
        }
        Ok(())
    }

    // ── Typed pop helpers ─────────────────────────────────────────────

    fn pop2_int(&mut self) -> Result<(i64, i64), String> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => Ok((x, y)),
            _ => Err("expected two Int values".to_string()),
        }
    }

    fn pop2_float(&mut self) -> Result<(f64, f64), String> {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        match (a, b) {
            (Value::Float(x), Value::Float(y)) => Ok((x, y)),
            _ => Err("expected two Float values".to_string()),
        }
    }
}

// ──────────────────────────────────────────────
// Value helpers
// ──────────────────────────────────────────────

pub fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b)       => *b,
        Value::Nil           => false,
        Value::Int(n)        => *n != 0,
        Value::Float(f)      => *f != 0.0,
        Value::Str(s)        => !s.is_empty(),
        Value::Collection(c) => !c.is_empty(),
        _                    => true,
    }
}

pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(x),   Value::Int(y))   => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Str(x),   Value::Str(y))   => x == y,
        (Value::Bool(x),  Value::Bool(y))  => x == y,
        (Value::Char(x),  Value::Char(y))  => x == y,
        (Value::Nil,      Value::Nil)      => true,
        (Value::Unit,     Value::Unit)     => true,
        (Value::Big(x),   Value::Big(y))   => x == y,
        (Value::Pct(x),   Value::Pct(y))   => x == y,
        _                                  => false,
    }
}

pub fn value_lt(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(x),   Value::Int(y))   => x < y,
        (Value::Float(x), Value::Float(y)) => x < y,
        (Value::Str(x),   Value::Str(y))   => x < y,
        (Value::Big(x),   Value::Big(y))   => x < y,
        (Value::Pct(x),   Value::Pct(y))   => x < y,
        _                                  => false,
    }
}

// ──────────────────────────────────────────────
// Tests
// ──────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::{Session, GcMode};

    fn run(bytecode: Vec<Opcode>, constants: Vec<Value>) -> Vec<Value> {
        let mut sess = Session::new(GcMode::Off);
        let func = FunctionObject {
            bytecode,
            constants,
            locals: 8,
            params: 0,
            name: "test".to_string(),
        };
        let mut vm = Vm::new(&mut sess, func);
        vm.run().unwrap();
        vm.stack.iter().map(|t| vm.session.read_value(t)).collect()
    }

    #[test]
    fn load_const_return() {
        let result = run(
            vec![Opcode::LoadConst(0), Opcode::Return],
            vec![Value::Int(42)],
        );
        assert!(matches!(result[0], Value::Int(42)));
    }

    #[test]
    fn add_int() {
        let result = run(
            vec![Opcode::LoadConst(0), Opcode::LoadConst(1), Opcode::AddInt, Opcode::Return],
            vec![Value::Int(3), Value::Int(4)],
        );
        assert!(matches!(result[0], Value::Int(7)));
    }

    #[test]
    fn store_and_load_local() {
        let result = run(
            vec![
                Opcode::LoadConst(0),
                Opcode::StoreLocal(0),
                Opcode::LoadLocal(0),
                Opcode::Return,
            ],
            vec![Value::Int(99)],
        );
        assert!(matches!(result[0], Value::Int(99)));
    }

    #[test]
    fn abs_int() {
        let result = run(
            vec![
                Opcode::LoadConst(0),
                Opcode::CallBuiltin { name: "abs", argc: 1 },
                Opcode::Return,
            ],
            vec![Value::Int(-7)],
        );
        assert!(matches!(result[0], Value::Int(7)));
    }

    #[test]
    fn abs_float() {
        let result = run(
            vec![
                Opcode::LoadConst(0),
                Opcode::CallBuiltin { name: "abs", argc: 1 },
                Opcode::Return,
            ],
            vec![Value::Float(-3.14)],
        );
        match &result[0] {
            Value::Float(f) => assert!((*f - 3.14).abs() < 1e-10),
            _ => panic!("expected Float"),
        }
    }
}
