/// vm.rs
use crate::session::Session;
use crate::value::{Tether, Value};

/// A single bytecode instruction for the Goblin VM.
///
/// Minimal starting set; we’ll extend this later.
#[derive(Debug, Clone)]
pub enum Opcode {
    /// Push constants[idx] (as a new stash/tether) onto the stack.
    LoadConst(u16),

    /// Return from the current function.
    /// For now we treat this as "stop execution".
    Return,
}

/// A compiled Goblin function: bytecode + constants + metadata.
///
/// The frontend (compiler) will eventually produce this from AST.
#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub bytecode: Vec<Opcode>,
    pub constants: Vec<Value>, // payloads; Session will stash them when executed
    pub locals: usize,         // number of local slots
    pub params: usize,         // number of parameters
    pub name: String,
}

/// One call frame: locals + instruction pointer + function being executed.
///
/// For this first step we don’t even use locals yet; we’re just structuring.
#[derive(Debug)]
pub struct Frame {
    pub locals: Vec<Tether>,   // slot-based locals (name -> slot -> Tether)
    pub ip: usize,             // instruction pointer into func.bytecode
    pub func: FunctionObject,
}

/// The Goblin VM: runs bytecode against a Session.
///
/// - `session` owns the arena of stashes.
/// - `stack` is the operand stack (stores Tethers).
/// - `frame` holds locals + bytecode + IP.
pub struct Vm<'a> {
    pub session: &'a mut Session,
    pub stack: Vec<Tether>,
    pub frame: Frame,
}

impl<'a> Vm<'a> {
    /// Create a new VM instance for a given function and session.
    ///
    /// This does NOT run the function. It just prepares the frame and stack.
    pub fn new(session: &'a mut Session, func: FunctionObject) -> Self {
        // For this first step we don’t populate locals yet.
        let locals = Vec::with_capacity(func.locals);

        Vm {
            session,
            stack: Vec::new(),
            frame: Frame {
                locals,
                ip: 0,
                func,
            },
        }
    }

    /// Run the current frame until a Return is hit.
    ///
    /// Right now this only supports:
    /// - LoadConst
    /// - Return
    pub fn run(&mut self) {
        loop {
            if self.frame.ip >= self.frame.func.bytecode.len() {
                break;
            }

            let op = self.frame.func.bytecode[self.frame.ip].clone();
            self.frame.ip += 1;

            match op {
                Opcode::LoadConst(idx) => {
                    // 1. Grab the payload from constants
                    let payload = self.frame.func.constants[idx as usize].clone();
                    // 2. Allocate a stash for it in the Session
                    let tether = self.session.alloc_value(payload);
                    // 3. Push the tether onto the stack
                    self.stack.push(tether);
                }

                Opcode::Return => {
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::{Session, GcMode};

    #[test]
    fn vm_can_return_42() {
        // 1. Make a function: constants = [42], bytecode = [LoadConst(0), Return]
        let func = FunctionObject {
            bytecode: vec![Opcode::LoadConst(0), Opcode::Return],
            constants: vec![Value::Int(42)],
            locals: 0,
            params: 0,
            name: "test_return_42".to_string(),
        };

        // 2. Create a session + VM
        let mut sess = Session::new(GcMode::Off);
        let mut vm = Vm::new(&mut sess, func);

        // 3. Run the VM
        vm.run();

        // 4. Assert: stack has one tether, and it points to Int(42)
        assert_eq!(vm.stack.len(), 1);
        let t = &vm.stack[0];
        let v = vm.session.read_value(t);
        match v {
            Value::Int(n) => assert_eq!(n, 42),
            _ => panic!("expected Int(42), got {:?}", v),
        }
    }
}
