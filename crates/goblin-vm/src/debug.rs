/// Debug, introspection, and diagnostic tools for the Goblin VM.
///
/// All debug output goes to stderr. None of this affects execution.
use crate::opcode::Opcode;
use crate::session::Session;
use crate::value::{FunctionObject, Tether, Value};
use crate::vm::Vm;

// ── Disassembler ─────────────────────────────────────────────────────────────

/// Pretty-print the bytecode of a FunctionObject to stderr.
pub fn disassemble(func: &FunctionObject) {
    eprintln!("=== {} (locals: {}, params: {}) ===", func.name, func.locals, func.params);

    if !func.upvalue_descriptors.is_empty() {
        eprintln!("  upvalues: {:?}", func.upvalue_descriptors);
    }

    if !func.constants.is_empty() {
        eprintln!("  constants:");
        for (i, c) in func.constants.iter().enumerate() {
            eprintln!("    [{i}] = {}", format_value(c));
        }
    }

    eprintln!("  bytecode:");
    for (i, op) in func.bytecode.iter().enumerate() {
        eprintln!("    {:04} {}", i, format_op(op, func));
    }

    // Recursively disassemble any nested functions in constants.
    for c in &func.constants {
        if let Value::Function(f) = c {
            eprintln!();
            disassemble(f);
        }
    }
}

/// Format a single opcode as a human-readable string.
pub fn format_op(op: &Opcode, func: &FunctionObject) -> String {
    match op {
        Opcode::LoadConst(idx) => {
            let preview = func.constants.get(*idx as usize)
                .map(|v| format_value(v))
                .unwrap_or_else(|| "?".into());
            format!("LoadConst({idx}) ; {preview}")
        }
        Opcode::LoadLocal(s)    => format!("LoadLocal({s})"),
        Opcode::StoreLocal(s)   => format!("StoreLocal({s})"),
        Opcode::LoadGlobal(i)   => format!("LoadGlobal({i})"),
        Opcode::StoreGlobal(i)  => format!("StoreGlobal({i})"),
        Opcode::LoadUpvalue(i)  => format!("LoadUpvalue({i})"),
        Opcode::StoreUpvalue(i) => format!("StoreUpvalue({i})"),
        Opcode::Jump(off)       => format!("Jump({off:+})"),
        Opcode::JumpIfFalse(off)=> format!("JumpIfFalse({off:+})"),
        Opcode::JumpIfTrue(off) => format!("JumpIfTrue({off:+})"),
        Opcode::Call(n)         => format!("Call({n})"),
        Opcode::CallBuiltin(id, n) => format!("CallBuiltin({id:?}, {n})"),
        Opcode::MakeClosure(i)  => format!("MakeClosure({i})"),
        Opcode::MakeArray(n)    => format!("MakeArray({n})"),
        Opcode::MakeMap(n)      => format!("MakeMap({n})"),
        Opcode::GetMember(i)    => {
            let key = func.constants.get(*i as usize)
                .map(|v| format_value(v))
                .unwrap_or_else(|| "?".into());
            format!("GetMember({i}) ; {key}")
        }
        other => format!("{}", other.name()),
    }
}

// ── Session dump ─────────────────────────────────────────────────────────────

/// Dump all live stashes in the session to stderr.
pub fn dump_session(session: &Session) {
    eprintln!("=== session dump (worker {}) ===", session.worker_id);
    eprintln!("  live stashes: {}", session.stash_count());
    for (slot, stash) in session.arena.iter() {
        eprintln!(
            "  [{slot:04}] gen={} count={} val={}",
            stash.generation,
            stash.tether_count,
            format_value(&stash.value),
        );
    }
    eprintln!("  globals ({}):", session.globals.len());
    for (i, g) in session.globals.iter().enumerate() {
        match g {
            Some(t) => eprintln!("    [{i}] → addr={:?}", t.addr),
            None    => eprintln!("    [{i}] (unset)"),
        }
    }
}

// ── VM stack dump ─────────────────────────────────────────────────────────────

/// Dump the current operand stack and call stack of the VM to stderr.
pub fn dump_vm(vm: &Vm) {
    eprintln!("=== vm dump ===");
    eprintln!("  call depth: {}", vm.call_stack.len());
    for (i, frame) in vm.call_stack.iter().enumerate().rev() {
        eprintln!(
            "  frame {i}: {} ip={} locals={}",
            frame.func.name,
            frame.ip,
            frame.locals.len(),
        );
        for (s, t) in frame.locals.iter().enumerate() {
            match t {
                Some(tt) => eprintln!("    local[{s}] → {:?}", tt.addr),
                None     => eprintln!("    local[{s}] (nil)"),
            }
        }
    }
    eprintln!("  operand stack ({} items):", vm.stack.len());
    for (i, v) in vm.stack.iter().enumerate().rev() {
        let val = format_value(v);
        eprintln!("    [{i}] {val}");
    }
}

// ── Collection layout visualiser ─────────────────────────────────────────────

/// Show the physical layout and meta counters for a collection.
pub fn describe_collection(v: &Value) -> String {
    match v {
        Value::Collection(c) => {
            use crate::value::CollectionLayout;
            let layout_name = match &c.layout {
                CollectionLayout::FlatArray(_)       => "FlatArray",
                CollectionLayout::RingBuf(_)         => "RingBuf",
                CollectionLayout::ChunkedSeq(_)      => "ChunkedSeq",
                CollectionLayout::SmallMap(_)        => "SmallMap",
                CollectionLayout::HashMapBackend(_)  => "HashMapBackend",
            };
            let m = &c.meta;
            format!(
                "Collection({layout_name}, len={}, front={}, back={}, mid={}, rand={}, scan={})",
                m.len, m.front_hits, m.back_hits, m.mid_hits, m.random_hits, m.scan_hits
            )
        }
        other => format!("(not a collection: {})", other.type_name()),
    }
}

/// Dump a Tether's pointed-to value with layout information.
pub fn dump_tether(t: &Tether, session: &Session) {
    match session.read_value(t) {
        Ok(v) => eprintln!(
            "  tether {:?} → {}",
            t.addr,
            describe_collection(&v)
        ),
        Err(e) => eprintln!("  tether {:?} → <err: {}>", t.addr, e),
    }
}

// ── Trace mode ───────────────────────────────────────────────────────────────

/// Trace a single opcode execution to stderr.
/// Call this from the VM loop when trace mode is enabled.
pub fn trace_op(op: &Opcode, func: &FunctionObject, stack_depth: usize, ip: usize) {
    eprintln!(
        "  [trace] {:04} {} (stack={})",
        ip,
        format_op(op, func),
        stack_depth,
    );
}

// ── Format helpers ────────────────────────────────────────────────────────────

fn format_value(v: &Value) -> String {
    format_value_depth(v, 0)
}

fn format_value_depth(v: &Value, depth: usize) -> String {
    if depth > 2 { return "...".into(); }
    match v {
        Value::Nil           => "nil".into(),
        Value::Unit          => "()".into(),
        Value::Bool(b)       => b.to_string(),
        Value::Int(n)        => n.to_string(),
        Value::Float(f)      => f.to_string(),
        Value::Big(d)        => d.to_string(),
        Value::Pct(p)        => format!("{}%", p),
        Value::Char(c)       => format!("'{}'", c),
        Value::Str(s)        => format!("{:?}", s),
        Value::Formatted(v, _) => format_value_depth(v, depth),
        Value::Array(items) if items.len() <= 6 => {
            let parts: Vec<String> = items.iter().map(|x| format_value_depth(x, depth + 1)).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Array(items) => format!("[array len={}]", items.len()),
        Value::Map(m)        => format!("{{map len={}}}", m.len()),
        Value::MapOrd(m)     => format!("{{map_ord len={}}}", m.len()),
        Value::Pair(k, v)    => format!("({}, {})", format_value_depth(k, depth+1), format_value_depth(v, depth+1)),
        Value::Seq(s)        => format!("[seq len={}]", s.len()),
        Value::CtrlSkip      => "<skip>".into(),
        Value::CtrlStop      => "<stop>".into(),
        Value::CtrlReturn(v) => format!("<return {}>", format_value_depth(v, depth+1)),
        Value::Object { class_name, .. } => format!("<{}>", class_name),
        Value::Ref(s)        => format!("<ref {}>", s),
        Value::GridRef { grid_id, x, y } => format!("<gridref {}[{},{}]>", grid_id, x, y),
        Value::Enum { enum_name, variant_name, .. } => format!("{}.{}", enum_name, variant_name),
        Value::Class { name } => format!("<class {}>", name),
        Value::Collection(c) => {
            #[allow(unused_imports)] use crate::collections;
            use crate::value::CollectionLayout;
            match &c.layout {
                CollectionLayout::FlatArray(v) if v.len() <= 6 => {
                    let items: Vec<String> = v.iter()
                        .map(|x| format_value_depth(x, depth + 1))
                        .collect();
                    format!("[{}]", items.join(", "))
                }
                CollectionLayout::SmallMap(pairs) if pairs.len() <= 6 => {
                    let items: Vec<String> = pairs.iter()
                        .map(|(k, v)| format!(
                            "{}: {}",
                            format_value_depth(k, depth + 1),
                            format_value_depth(v, depth + 1)
                        ))
                        .collect();
                    format!("{{{}}}", items.join(", "))
                }
                _ => describe_collection(&Value::Collection(c.clone())),
            }
        }
        Value::DateTime(gdt) => crate::builtins::dt_display(gdt),
        Value::Function(f)  => format!("<fn {}>", f.name),
        Value::Closure(c)   => format!("<closure {}>", c.func.name),
        Value::Builtin(b)   => format!("<builtin {:?}>", b),
    }
}

// Convenience: format a Value stored in a stash.
pub fn format_stash_value(sv: &Value) -> String {
    format_value(sv)
}
