/// Bytecode compiler: walks goblin-ast and emits FunctionObject bytecode.
///
/// Design:
/// - Names are resolved at compile time to numeric slot indices.
/// - Each function body compiles into a fresh FunctionObject.
/// - Closures record upvalue descriptors so the VM can populate them at runtime.
/// - Unsupported AST nodes (ClassDecl, OverlayDef, etc.) produce CompileError.
///   These belong to the DES/object layer and will be handled separately.
use goblin_ast::{
    ActionBody, ActionDecl, BindMode, Expr, JudgeArm, Module, ReturnStmt, Stmt,
};

use crate::error::GoblinError;
use crate::opcode::Opcode;
use crate::value::{BuiltinId, FunctionObject, UpvalueDescriptor, Value};

// ── Scope ─────────────────────────────────────────────────────────────────────

/// A single lexical scope (one function body).
struct FunctionScope {
    /// Maps variable names to local slot indices within this function.
    locals: Vec<(String, u8)>,
    /// Upvalue captures from enclosing scopes.
    upvalues: Vec<(String, UpvalueDescriptor)>,
    /// Bytecode under construction.
    bytecode: Vec<Opcode>,
    /// Constant pool.
    constants: Vec<Value>,
    /// Total local slots allocated so far.
    next_slot: u8,
    /// How many slots are parameters (always slots 0..params-1).
    params: usize,
    /// Function name.
    name: String,
    /// Pending break jump patches (jump offsets that need to be back-patched).
    #[allow(dead_code)]
    break_patches: Vec<usize>,
    #[allow(dead_code)]
    continue_patches: Vec<usize>,
    #[allow(dead_code)]
    loop_start: Option<usize>,
}

impl FunctionScope {
    fn new(name: impl Into<String>, params: usize) -> Self {
        FunctionScope {
            locals: Vec::new(),
            upvalues: Vec::new(),
            bytecode: Vec::new(),
            constants: Vec::new(),
            next_slot: params as u8,
            params,
            name: name.into(),
            break_patches: Vec::new(),
            continue_patches: Vec::new(),
            loop_start: None,
        }
    }

    /// Add a constant to the pool, returning its index.
    fn add_constant(&mut self, v: Value) -> u16 {
        // Deduplicate simple scalars.
        for (i, c) in self.constants.iter().enumerate() {
            if simple_eq(c, &v) { return i as u16; }
        }
        let idx = self.constants.len() as u16;
        self.constants.push(v);
        idx
    }

    /// Declare a new local variable in this scope. Returns its slot index.
    fn declare_local(&mut self, name: &str) -> u8 {
        let slot = self.next_slot;
        self.next_slot = self.next_slot.saturating_add(1);
        self.locals.push((name.to_string(), slot));
        slot
    }

    /// Declare parameters (must be called before any other locals).
    fn declare_params(&mut self, names: &[String]) {
        for (i, n) in names.iter().enumerate() {
            self.locals.push((n.clone(), i as u8));
        }
    }

    /// Look up a local variable by name. Returns its slot, if found.
    fn find_local(&self, name: &str) -> Option<u8> {
        // Search in reverse (innermost binding wins).
        self.locals.iter().rev().find(|(n, _)| n == name).map(|(_, s)| *s)
    }

    /// Find or register an upvalue. Returns its upvalue index.
    fn find_or_add_upvalue(&mut self, name: &str, desc: UpvalueDescriptor) -> u8 {
        if let Some(pos) = self.upvalues.iter().position(|(n, _)| n == name) {
            return pos as u8;
        }
        let idx = self.upvalues.len() as u8;
        self.upvalues.push((name.to_string(), desc));
        idx
    }

    fn emit(&mut self, op: Opcode) -> usize {
        let pos = self.bytecode.len();
        self.bytecode.push(op);
        pos
    }

    /// Emit a placeholder jump and return its index for back-patching.
    fn emit_jump(&mut self, op: fn(i16) -> Opcode) -> usize {
        self.emit(op(0))
    }

    /// Patch a previously emitted jump to the current position.
    fn patch_jump(&mut self, jump_idx: usize) {
        let current = self.bytecode.len();
        // offset is relative to instruction AFTER the jump.
        let offset = (current as isize - jump_idx as isize - 1) as i16;
        match &mut self.bytecode[jump_idx] {
            Opcode::Jump(o) | Opcode::JumpIfFalse(o) | Opcode::JumpIfTrue(o) => *o = offset,
            _ => panic!("patch_jump on non-jump opcode"),
        }
    }

    fn finish(self) -> FunctionObject {
        let upvalue_descriptors: Vec<UpvalueDescriptor> =
            self.upvalues.into_iter().map(|(_, d)| d).collect();
        FunctionObject {
            bytecode: self.bytecode,
            constants: self.constants,
            locals: self.next_slot as usize,
            params: self.params,
            name: self.name,
            upvalue_descriptors,
        }
    }
}

// ── Compiler ─────────────────────────────────────────────────────────────────

/// The stateful bytecode compiler.
/// Maintains a stack of FunctionScopes (one per nested function body).
pub struct Compiler {
    /// Innermost scope is at the back.
    scopes: Vec<FunctionScope>,
    /// Registered global names (name → global slot index).
    globals: Vec<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { scopes: Vec::new(), globals: Vec::new() }
    }

    // ── Public API ────────────────────────────────────────────────────────────

    /// Compile a top-level module into a FunctionObject (the module's "main").
    pub fn compile_module(mut self, module: &Module) -> Result<FunctionObject, GoblinError> {
        self.push_scope("__main__", 0);
        for stmt in &module.items {
            self.compile_stmt(stmt)?;
        }
        // Return nil at end of module.
        let scope = self.scopes.last_mut().unwrap();
        scope.emit(Opcode::LoadNil);
        scope.emit(Opcode::Return);
        Ok(self.pop_scope())
    }

    /// Compile a single action/function declaration into a FunctionObject.
    pub fn compile_action(mut self, action: &ActionDecl) -> Result<FunctionObject, GoblinError> {
        let param_names: Vec<String> = action.params.iter().map(|p| p.name.clone()).collect();
        self.push_scope(&action.name, param_names.len());
        {
            let scope = self.scopes.last_mut().unwrap();
            scope.declare_params(&param_names);
        }
        match &action.body {
            ActionBody::Block(stmts) => {
                for s in stmts {
                    self.compile_stmt(s)?;
                }
                let scope = self.scopes.last_mut().unwrap();
                scope.emit(Opcode::LoadNil);
                scope.emit(Opcode::Return);
            }
            ActionBody::Expr(e) => {
                self.compile_expr(e)?;
                let scope = self.scopes.last_mut().unwrap();
                scope.emit(Opcode::Return);
            }
        }
        Ok(self.pop_scope())
    }

    // ── Scope management ──────────────────────────────────────────────────────

    fn push_scope(&mut self, name: &str, params: usize) {
        self.scopes.push(FunctionScope::new(name, params));
    }

    fn pop_scope(&mut self) -> FunctionObject {
        self.scopes.pop().unwrap().finish()
    }

    fn scope(&self) -> &FunctionScope {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut FunctionScope {
        self.scopes.last_mut().unwrap()
    }

    fn emit(&mut self, op: Opcode) -> usize {
        self.scope_mut().emit(op)
    }

    fn add_constant(&mut self, v: Value) -> u16 {
        self.scope_mut().add_constant(v)
    }

    // ── Variable resolution ───────────────────────────────────────────────────

    /// Resolve a name to a load opcode sequence, searching locals → upvalues → globals.
    fn resolve_load(&mut self, name: &str) -> Result<Opcode, GoblinError> {
        // 1. Check innermost scope's locals first.
        if let Some(slot) = self.scope().find_local(name) {
            return Ok(Opcode::LoadLocal(slot));
        }

        // 2. Walk outer scopes to find the variable; if found, create upvalue chain.
        if self.scopes.len() > 1 {
            if let Some(uv_idx) = self.resolve_upvalue(self.scopes.len() - 1, name) {
                return Ok(Opcode::LoadUpvalue(uv_idx));
            }
        }

        // 3. Check globals.
        if let Some(pos) = self.globals.iter().position(|g| g == name) {
            return Ok(Opcode::LoadGlobal(pos as u16));
        }

        // 4. Check if it's a known builtin name.
        if let Some(bid) = builtin_by_name(name) {
            // Push a Builtin value as a constant.
            let idx = self.add_constant(Value::Builtin(bid));
            return Ok(Opcode::LoadConst(idx));
        }

        Err(GoblinError::UndefinedVariable { name: name.to_string() })
    }

    /// Recursively walk outer scopes to build an upvalue capture chain.
    /// Returns the upvalue index in the innermost scope (self.scopes[inner_idx]).
    fn resolve_upvalue(&mut self, inner_idx: usize, name: &str) -> Option<u8> {
        if inner_idx == 0 { return None; }
        let outer_idx = inner_idx - 1;

        // Is it a local in the directly enclosing scope?
        if let Some(slot) = self.scopes[outer_idx].find_local(name) {
            let uv_idx = self.scopes[inner_idx]
                .find_or_add_upvalue(name, UpvalueDescriptor::Local(slot));
            return Some(uv_idx);
        }

        // Recurse: is it an upvalue in an even outer scope?
        if let Some(outer_uv) = self.resolve_upvalue(outer_idx, name) {
            let uv_idx = self.scopes[inner_idx]
                .find_or_add_upvalue(name, UpvalueDescriptor::Upvalue(outer_uv));
            return Some(uv_idx);
        }

        None
    }

    fn resolve_store(&mut self, name: &str) -> Option<Opcode> {
        if let Some(slot) = self.scope().find_local(name) {
            return Some(Opcode::StoreLocal(slot));
        }
        if self.scopes.len() > 1 {
            if let Some(uv_idx) = self.resolve_upvalue(self.scopes.len() - 1, name) {
                return Some(Opcode::StoreUpvalue(uv_idx));
            }
        }
        if let Some(pos) = self.globals.iter().position(|g| g == name) {
            return Some(Opcode::StoreGlobal(pos as u16));
        }
        None
    }

    // ── Statement compiler ───────────────────────────────────────────────────

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), GoblinError> {
        match stmt {
            Stmt::Expr(e) => {
                self.compile_expr(e)?;
                self.emit(Opcode::Pop); // discard expression result
            }

            Stmt::Bind(bind) => {
                self.compile_expr(&bind.expr)?;

                let name = &bind.name.0;
                match bind.mode {
                    BindMode::Tether => {
                        // x | expr — initial binding; declare new local slot.
                        let slot = self.scope_mut().declare_local(name);
                        self.emit(Opcode::StoreLocal(slot));
                    }
                    BindMode::Retether => {
                        // x |= expr — rebind existing slot.
                        let op = self.resolve_store(name)
                            .ok_or_else(|| GoblinError::UndefinedVariable { name: name.clone() })?;
                        self.emit(op);
                    }
                    BindMode::Shadow => {
                        // x[= expr — shadow: always declare a new slot.
                        let slot = self.scope_mut().declare_local(name);
                        self.emit(Opcode::StoreLocal(slot));
                    }
                }
            }

            Stmt::Return(ReturnStmt { values, .. }) => {
                if values.is_empty() {
                    self.emit(Opcode::LoadNil);
                } else if values.len() == 1 {
                    self.compile_expr(&values[0])?;
                } else {
                    // Multiple return values → wrap in an array.
                    for v in values { self.compile_expr(v)?; }
                    self.emit(Opcode::MakeArray(values.len() as u16));
                }
                self.emit(Opcode::Return);
            }

            Stmt::Action(action) => {
                // Nested action declaration: compile into a FunctionObject constant.
                self.compile_action_decl(action)?;
                // Store the resulting function/closure in a local slot.
                let slot = self.scope_mut().declare_local(&action.name);
                self.emit(Opcode::StoreLocal(slot));
            }

            Stmt::Judge(judge) => {
                self.compile_judge_stmt(&judge.arms)?;
            }

            Stmt::JudgeAll(judge_all) => {
                // Execute every matching arm (no short-circuit).
                for arm in &judge_all.arms {
                    if let Some(cond) = &arm.condition {
                        self.compile_expr(cond)?;
                        let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        self.compile_arm_body(&arm.body)?;
                        self.scope_mut().patch_jump(skip);
                    } else {
                        self.compile_arm_body(&arm.body)?;
                    }
                }
            }

            Stmt::Block { stmts, .. } => {
                for s in stmts { self.compile_stmt(s)?; }
            }

            Stmt::Sweep(sweep) => {
                self.compile_sweep(sweep)?;
            }

            // ── Unhandled in the VM compiler (DES / object system) ────────────
            Stmt::Class(_) | Stmt::Enum(_) | Stmt::Import(_) | Stmt::Use(_)
            | Stmt::OverlayDef(_) | Stmt::OverlayApply(_) | Stmt::OverlayDetach(_)
            | Stmt::LinkDef(_) | Stmt::ObjectLinkDef(_) | Stmt::LinkOffset(_)
            | Stmt::ClearLink(_) | Stmt::ObjectDecision(..) | Stmt::UnitDecl(_)
            | Stmt::BoxBind { .. } | Stmt::TupleBind(_) => {
                return Err(GoblinError::NotImplemented {
                    feature: "DES/object system statements are not compiled by the VM compiler yet",
                });
            }
        }
        Ok(())
    }

    fn compile_arm_body(&mut self, body: &goblin_ast::JudgeArmBody) -> Result<(), GoblinError> {
        match body {
            goblin_ast::JudgeArmBody::Expr(e) => {
                self.compile_expr(e)?;
                self.emit(Opcode::Pop);
            }
            goblin_ast::JudgeArmBody::Stmts(stmts) => {
                for s in stmts { self.compile_stmt(s)?; }
            }
        }
        Ok(())
    }

    fn compile_judge_stmt(&mut self, arms: &[goblin_ast::JudgeArmStmt]) -> Result<(), GoblinError> {
        let mut end_jumps: Vec<usize> = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;
            if let Some(cond) = &arm.condition {
                self.compile_expr(cond)?;
                let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                self.compile_arm_body(&arm.body)?;
                if !is_last {
                    let end = self.scope_mut().emit_jump(Opcode::Jump);
                    end_jumps.push(end);
                }
                self.scope_mut().patch_jump(skip);
            } else {
                // else arm — no condition check
                self.compile_arm_body(&arm.body)?;
            }
        }

        for j in end_jumps {
            self.scope_mut().patch_jump(j);
        }
        Ok(())
    }

    fn compile_sweep(&mut self, sweep: &goblin_ast::SweepStmt) -> Result<(), GoblinError> {
        // sweep compiles each target and runs arm matching.
        // For now: compile each target and apply arms sequentially.
        // Full pattern matching (regex, range) is deferred.
        use goblin_ast::{SweepArmKind, SweepMode};

        for target_expr in &sweep.targets {
            self.compile_expr(target_expr)?;
            let target_slot = self.scope_mut().declare_local("__sweep_target__");
            self.emit(Opcode::StoreLocal(target_slot));

            for arm in &sweep.arms {
                match &arm.kind {
                    SweepArmKind::Pattern(pat) => {
                        // Emit: target == pat → run body
                        self.emit(Opcode::LoadLocal(target_slot));
                        let idx = self.add_constant(Value::Str(pat.clone()));
                        self.emit(Opcode::LoadConst(idx));
                        self.emit(Opcode::Eq);
                        let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        for s in &arm.body { self.compile_stmt(s)?; }
                        self.scope_mut().patch_jump(skip);
                        if matches!(sweep.mode, SweepMode::Match) { break; }
                    }
                    SweepArmKind::AllBody => {
                        for s in &arm.body { self.compile_stmt(s)?; }
                    }
                    SweepArmKind::Range { start, end } => {
                        // target >= start && target <= end
                        self.emit(Opcode::LoadLocal(target_slot));
                        let si = self.add_constant(Value::Str(start.clone()));
                        self.emit(Opcode::LoadConst(si));
                        self.emit(Opcode::Ge);
                        let skip1 = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        self.emit(Opcode::LoadLocal(target_slot));
                        let ei = self.add_constant(Value::Str(end.clone()));
                        self.emit(Opcode::LoadConst(ei));
                        self.emit(Opcode::Le);
                        let skip2 = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        for s in &arm.body { self.compile_stmt(s)?; }
                        self.scope_mut().patch_jump(skip1);
                        self.scope_mut().patch_jump(skip2);
                    }
                }
            }
        }
        Ok(())
    }

    // ── Expression compiler ───────────────────────────────────────────────────

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), GoblinError> {
        match expr {
            // ── Literals ──────────────────────────────────────────────────────
            Expr::Nil(_) => { self.emit(Opcode::LoadNil); }
            Expr::Bool(true, _) => { self.emit(Opcode::LoadTrue); }
            Expr::Bool(false, _) => { self.emit(Opcode::LoadFalse); }

            Expr::Number(raw, _) => {
                let v = parse_number(raw)?;
                let idx = self.add_constant(v);
                self.emit(Opcode::LoadConst(idx));
            }

            Expr::Str(s, _) => {
                let idx = self.add_constant(Value::Str(s.clone()));
                self.emit(Opcode::LoadConst(idx));
            }

            Expr::Char(c, _) => {
                let idx = self.add_constant(Value::Str(c.to_string()));
                self.emit(Opcode::LoadConst(idx));
            }

            // ── Variables ─────────────────────────────────────────────────────
            Expr::Ident(name, _) => {
                let op = self.resolve_load(name)?;
                self.emit(op);
            }

            // ── Collections ───────────────────────────────────────────────────
            Expr::Array(items, _) => {
                for item in items { self.compile_expr(item)?; }
                self.emit(Opcode::MakeArray(items.len() as u16));
            }

            Expr::Object(fields, _) => {
                for (key, val_expr) in fields {
                    let kidx = self.add_constant(Value::Str(key.clone()));
                    self.emit(Opcode::LoadConst(kidx));
                    self.compile_expr(val_expr)?;
                }
                self.emit(Opcode::MakeMap(fields.len() as u16));
            }

            // ── Indexing ──────────────────────────────────────────────────────
            Expr::Index(obj, idx, _) => {
                self.compile_expr(obj)?;
                self.compile_expr(idx)?;
                self.emit(Opcode::GetIndex);
            }

            Expr::IndexMap(obj, key, _) => {
                self.compile_expr(obj)?;
                self.compile_expr(key)?;
                self.emit(Opcode::GetIndex);
            }

            Expr::Member(obj, name, _) => {
                self.compile_expr(obj)?;
                let kidx = self.add_constant(Value::Str(name.clone()));
                self.emit(Opcode::GetMember(kidx));
            }

            // ── Calls ─────────────────────────────────────────────────────────
            Expr::FreeCall(name, args, _) => {
                // Check if it's a known builtin call pattern.
                if let Some(op) = self.try_compile_builtin_call(name, args)? {
                    let _ = op; // op already emitted
                } else {
                    let load_op = self.resolve_load(name)?;
                    self.emit(load_op);
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                }
            }

            Expr::Call(recv, method, args, _) => {
                // recv.method(args) — compile as a free call on the method with recv as first arg.
                // For now: look up method as a global/local function.
                // TODO: when object methods are supported, dispatch differently.
                let load_op = self.resolve_load(method)
                    .unwrap_or_else(|_| {
                        // Emit a placeholder; will fail at runtime if name not found.
                        Opcode::LoadNil
                    });
                self.emit(load_op);
                self.compile_expr(recv)?;
                for arg in args { self.compile_expr(arg)?; }
                self.emit(Opcode::Call((args.len() + 1) as u8));
            }

            Expr::NsCall(ns, name, args, _) => {
                // Namespace call: compile as free call for now.
                let full_name = format!("{}::{}", ns, name);
                let load_op = self.resolve_load(&full_name)
                    .or_else(|_| self.resolve_load(name))?;
                self.emit(load_op);
                for arg in args { self.compile_expr(arg)?; }
                self.emit(Opcode::Call(args.len() as u8));
            }

            // Optional calls: compile guard + call.
            Expr::OptCall(recv, method, args, _span) => {
                // recv?.method(args) — if recv is nil, short-circuit to nil.
                self.compile_expr(recv)?;
                self.emit(Opcode::Dup);
                let is_nil_idx = self.add_constant(Value::Nil);
                self.emit(Opcode::LoadConst(is_nil_idx));
                self.emit(Opcode::Eq);
                let skip = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
                // Not nil: do the call.
                self.emit(Opcode::Pop); // pop duplicated receiver
                // Re-compile the receiver since we popped the dup.
                self.compile_expr(recv)?;
                let load_op = self.resolve_load(method).unwrap_or(Opcode::LoadNil);
                self.emit(load_op);
                for arg in args { self.compile_expr(arg)?; }
                self.emit(Opcode::Call((args.len() + 1) as u8));
                let end = self.scope_mut().emit_jump(Opcode::Jump);
                self.scope_mut().patch_jump(skip);
                // Was nil: stack still has nil from Dup. Pop it and push nil result.
                self.emit(Opcode::Pop);
                self.emit(Opcode::LoadNil);
                self.scope_mut().patch_jump(end);
            }
            Expr::OptMember(obj, name, _) => {
                self.compile_expr(obj)?;
                self.emit(Opcode::Dup);
                let nil_idx = self.add_constant(Value::Nil);
                self.emit(Opcode::LoadConst(nil_idx));
                self.emit(Opcode::Eq);
                let skip = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
                let kidx = self.add_constant(Value::Str(name.clone()));
                self.emit(Opcode::GetMember(kidx));
                let end = self.scope_mut().emit_jump(Opcode::Jump);
                self.scope_mut().patch_jump(skip);
                self.emit(Opcode::Pop);
                self.emit(Opcode::LoadNil);
                self.scope_mut().patch_jump(end);
            }

            // ── Operators ────────────────────────────────────────────────────
            Expr::Prefix(op, operand, _) => {
                self.compile_expr(operand)?;
                match op.as_str() {
                    "-" => { self.emit(Opcode::Neg); }
                    "!" | "not" => { self.emit(Opcode::Not); }
                    _ => return Err(GoblinError::NotImplemented {
                        feature: "unknown prefix operator",
                    }),
                }
            }

            Expr::Binary(lhs, op, rhs, _) => {
                self.compile_binary(lhs, op, rhs)?;
            }

            Expr::Postfix(_, _op, _) => {
                return Err(GoblinError::NotImplemented { feature: "postfix operators" });
            }

            // ── judge expression ──────────────────────────────────────────────
            Expr::Judge { using, header, arms, all, .. } => {
                self.compile_judge_expr(using.as_deref(), header.as_deref(), arms, *all)?;
            }

            // ── Block expression ──────────────────────────────────────────────
            Expr::Block { stmts, .. } => {
                if stmts.is_empty() {
                    self.emit(Opcode::LoadNil);
                    return Ok(());
                }
                let last_idx = stmts.len() - 1;
                for (i, s) in stmts.iter().enumerate() {
                    if i == last_idx {
                        // Last stmt: if it's an expression, leave its value on the stack.
                        if let Stmt::Expr(e) = s {
                            self.compile_expr(e)?;
                        } else {
                            self.compile_stmt(s)?;
                            self.emit(Opcode::LoadNil);
                        }
                    } else {
                        self.compile_stmt(s)?;
                    }
                }
            }

            Expr::Slice(_, _, _, _) | Expr::Slice3(_, _, _, _, _) => {
                return Err(GoblinError::NotImplemented { feature: "slice expressions" });
            }

            Expr::Index2(_, _, _, _) => {
                return Err(GoblinError::NotImplemented { feature: "2D index (grid)" });
            }

            Expr::EnumVariant { .. } => {
                return Err(GoblinError::NotImplemented { feature: "enum variant expressions" });
            }

            Expr::LiteralToken { .. } | Expr::BoxVar { .. } => {
                return Err(GoblinError::NotImplemented { feature: "literal tokens / box vars" });
            }
        }
        Ok(())
    }

    fn compile_binary(&mut self, lhs: &Expr, op: &str, rhs: &Expr) -> Result<(), GoblinError> {
        // Short-circuit operators.
        if op == "&&" || op == "and" {
            self.compile_expr(lhs)?;
            self.emit(Opcode::Dup);
            let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
            self.emit(Opcode::Pop);
            self.compile_expr(rhs)?;
            self.scope_mut().patch_jump(skip);
            return Ok(());
        }
        if op == "||" || op == "or" {
            self.compile_expr(lhs)?;
            self.emit(Opcode::Dup);
            let skip = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
            self.emit(Opcode::Pop);
            self.compile_expr(rhs)?;
            self.scope_mut().patch_jump(skip);
            return Ok(());
        }

        self.compile_expr(lhs)?;
        self.compile_expr(rhs)?;

        let instr = match op {
            "+"  | "++"  => Opcode::Add,
            "-"          => Opcode::Sub,
            "*"          => Opcode::Mul,
            "/"          => Opcode::Div,
            "%"          => Opcode::Rem,
            "=="         => Opcode::Eq,
            "!=" | "/=" => Opcode::Ne,
            "<"          => Opcode::Lt,
            "<="         => Opcode::Le,
            ">"          => Opcode::Gt,
            ">="         => Opcode::Ge,
            "<>"         => Opcode::Concat,
            _ => return Err(GoblinError::NotImplemented { feature: "unknown binary operator" }),
        };
        self.emit(instr);
        Ok(())
    }

    fn compile_judge_expr(
        &mut self,
        _using: Option<&Expr>,
        header: Option<&Expr>,
        arms: &[JudgeArm],
        all: bool,
    ) -> Result<(), GoblinError> {
        // judge expression evaluates to the value of the matching arm.
        let mut end_jumps: Vec<usize> = Vec::new();

        // If there's a header, compile it and store for comparison.
        let header_slot: Option<u8> = if let Some(h) = header {
            self.compile_expr(h)?;
            let s = self.scope_mut().declare_local("__judge_header__");
            self.emit(Opcode::StoreLocal(s));
            Some(s)
        } else {
            None
        };

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            if let Some(cond) = &arm.condition {
                if let Some(hslot) = header_slot {
                    // Pattern match: header == condition
                    self.emit(Opcode::LoadLocal(hslot));
                    self.compile_expr(cond)?;
                    self.emit(Opcode::Eq);
                } else {
                    self.compile_expr(cond)?;
                }
                let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);

                if let Some(v) = &arm.value {
                    self.compile_expr(v)?;
                } else {
                    self.emit(Opcode::LoadNil);
                }

                if !is_last || !all {
                    let end = self.scope_mut().emit_jump(Opcode::Jump);
                    end_jumps.push(end);
                }
                self.scope_mut().patch_jump(skip);
            } else {
                // else arm
                if let Some(v) = &arm.value {
                    self.compile_expr(v)?;
                } else {
                    self.emit(Opcode::LoadNil);
                }
            }
        }

        for j in end_jumps {
            self.scope_mut().patch_jump(j);
        }
        Ok(())
    }

    // ── Nested action / closure compilation ───────────────────────────────────

    fn compile_action_decl(&mut self, action: &ActionDecl) -> Result<(), GoblinError> {
        let param_names: Vec<String> = action.params.iter().map(|p| p.name.clone()).collect();
        let n_params = param_names.len();

        self.push_scope(&action.name, n_params);
        {
            let scope = self.scopes.last_mut().unwrap();
            scope.declare_params(&param_names);
        }

        match &action.body {
            ActionBody::Block(stmts) => {
                for s in stmts { self.compile_stmt(s)?; }
                let scope = self.scopes.last_mut().unwrap();
                scope.emit(Opcode::LoadNil);
                scope.emit(Opcode::Return);
            }
            ActionBody::Expr(e) => {
                self.compile_expr(e)?;
                self.scope_mut().emit(Opcode::Return);
            }
        }

        let func_obj = self.pop_scope();
        let has_upvalues = !func_obj.upvalue_descriptors.is_empty();
        let v = Value::Function(std::rc::Rc::new(func_obj));
        let cidx = self.add_constant(v);

        if has_upvalues {
            self.emit(Opcode::MakeClosure(cidx));
        } else {
            self.emit(Opcode::LoadConst(cidx));
        }
        Ok(())
    }

    // ── Builtin call optimisation ─────────────────────────────────────────────

    /// Try to compile a free call as a direct CallBuiltin opcode.
    /// Returns Ok(Some(())) if emitted, Ok(None) if caller should emit a regular Call.
    fn try_compile_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<()>, GoblinError> {
        let bid = match builtin_by_name(name) {
            Some(b) => b,
            None    => return Ok(None),
        };
        for arg in args { self.compile_expr(arg)?; }
        self.emit(Opcode::CallBuiltin(bid, args.len() as u8));
        Ok(Some(()))
    }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn parse_number(raw: &str) -> Result<Value, GoblinError> {
    // Try int first, then float.
    if let Ok(n) = raw.parse::<i64>() {
        return Ok(Value::Int(n));
    }
    if let Ok(n) = raw.replace('_', "").parse::<i64>() {
        return Ok(Value::Int(n));
    }
    if let Ok(f) = raw.parse::<f64>() {
        return Ok(Value::Float(f));
    }
    // Hex literals (0x...)
    if raw.starts_with("0x") || raw.starts_with("0X") {
        if let Ok(n) = i64::from_str_radix(&raw[2..], 16) {
            return Ok(Value::Int(n));
        }
    }
    Err(GoblinError::CompileError {
        message: format!("cannot parse number literal: {}", raw),
        span_debug: "(unknown)".into(),
    })
}

fn simple_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Nil, Value::Nil)          => true,
        (Value::Bool(x), Value::Bool(y))  => x == y,
        (Value::Int(x), Value::Int(y))    => x == y,
        (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
        (Value::Str(x), Value::Str(y))    => x == y,
        _ => false,
    }
}

fn builtin_by_name(name: &str) -> Option<BuiltinId> {
    Some(match name {
        ":mem_id"      | "mem_id"       => BuiltinId::MemId,
        ":mem_addr"    | "mem_addr"     => BuiltinId::MemAddr,
        ":gc"          | "gc"           => BuiltinId::Gc,
        "abs"                           => BuiltinId::Abs,
        "min"                           => BuiltinId::Min,
        "max"                           => BuiltinId::Max,
        "floor"                         => BuiltinId::Floor,
        "ceil"                          => BuiltinId::Ceil,
        "round"                         => BuiltinId::Round,
        "sqrt"                          => BuiltinId::Sqrt,
        "pow"                           => BuiltinId::Pow,
        "len"          | "count"        => BuiltinId::Len,
        "to_string"    | "str"          => BuiltinId::ToStr,
        "to_upper"                      => BuiltinId::ToUpperCase,
        "to_lower"                      => BuiltinId::ToLowerCase,
        "trim"                          => BuiltinId::Trim,
        "trim_lead"                     => BuiltinId::TrimLead,
        "trim_trail"                    => BuiltinId::TrimTrail,
        "lower"                         => BuiltinId::Lower,
        "upper"                         => BuiltinId::Upper,
        "title"                         => BuiltinId::Title,
        "slug"                          => BuiltinId::Slug,
        "mixed"                         => BuiltinId::Mixed,
        "raw"                           => BuiltinId::Raw,
        "find_all"                      => BuiltinId::FindAll,
        "ord"                           => BuiltinId::Ord,
        "split"                         => BuiltinId::Split,
        "join"                          => BuiltinId::Join,
        "contains"                      => BuiltinId::Contains,
        "starts_with"                   => BuiltinId::StartsWith,
        "ends_with"                     => BuiltinId::EndsWith,
        "replace"                       => BuiltinId::Replace,
        "items"                         => BuiltinId::Items,
        "shuffle"                       => BuiltinId::Shuffle,
        "freq"                          => BuiltinId::Freq,
        "mode"                          => BuiltinId::Mode,
        "sample_weighted"               => BuiltinId::SampleWeighted,
        "dups"                          => BuiltinId::Dups,
        "range"                         => BuiltinId::Range,
        "is_array"                      => BuiltinId::IsArray,
        "is_map"                        => BuiltinId::IsMap,
        "mem_total"     | ":mem_total"  => BuiltinId::MemTotal,
        "mem_human"     | ":mem_human"  => BuiltinId::MemHuman,
        "ipsum"                         => BuiltinId::Ipsum,
        "ipsum_sentences"               => BuiltinId::IpsumSentences,
        "ipsum_paragraphs"              => BuiltinId::IpsumParagraphs,
        "ipsum_full"                    => BuiltinId::IpsumFull,
        "run_cmd"                       => BuiltinId::RunCmd,
        "grab"                          => BuiltinId::Grab,
        "grab_first"                    => BuiltinId::GrabFirst,
        "grab_last"                     => BuiltinId::GrabLast,
        "grab_at"                       => BuiltinId::GrabAt,
        "grab_random"                   => BuiltinId::GrabRandom,
        "grab_where"                    => BuiltinId::GrabWhere,
        "grab_all"                      => BuiltinId::GrabAll,
        "grab_between"                  => BuiltinId::GrabBetween,
        "grab_matching"                 => BuiltinId::GrabMatching,
        "put"                           => BuiltinId::Put,
        "put_first"                     => BuiltinId::PutFirst,
        "put_last"                      => BuiltinId::PutLast,
        "put_at"                        => BuiltinId::PutAt,
        "update"                        => BuiltinId::Update,
        "update_first"                  => BuiltinId::UpdateFirst,
        "update_last"                   => BuiltinId::UpdateLast,
        "update_at"                     => BuiltinId::UpdateAt,
        "delete"                        => BuiltinId::Delete,
        "delete_first"                  => BuiltinId::DeleteFirst,
        "delete_last"                   => BuiltinId::DeleteLast,
        "delete_at"                     => BuiltinId::DeleteAt,
        "delete_where"                  => BuiltinId::DeleteWhere,
        "delete_all"                    => BuiltinId::DeleteAll,
        "reap"                          => BuiltinId::Reap,
        "reap_first"                    => BuiltinId::ReapFirst,
        "reap_last"                     => BuiltinId::ReapLast,
        "reap_at"                       => BuiltinId::ReapAt,
        "reap_random"                   => BuiltinId::ReapRandom,
        "reap_where"                    => BuiltinId::ReapWhere,
        "reap_all"                      => BuiltinId::ReapAll,
        "has"                           => BuiltinId::Has,
        "keys"                          => BuiltinId::Keys,
        "values"                        => BuiltinId::Values,
        "pairs"                         => BuiltinId::Pairs,
        "is_empty"                      => BuiltinId::IsEmpty,
        "reverse"                       => BuiltinId::Reverse,
        "sort"                          => BuiltinId::Sort,
        "sort_by"                       => BuiltinId::SortBy,
        "map"                           => BuiltinId::Map,
        "filter"                        => BuiltinId::Filter,
        "reduce"                        => BuiltinId::Reduce,
        "any"                           => BuiltinId::Any,
        "all"                           => BuiltinId::All,
        "find"                          => BuiltinId::Find,
        "find_index"                    => BuiltinId::FindIndex,
        "zip"                           => BuiltinId::Zip,
        "flatten"                       => BuiltinId::Flatten,
        "unique"                        => BuiltinId::Unique,
        "slice"                         => BuiltinId::Slice,
        "print"                         => BuiltinId::Print,
        "println"                       => BuiltinId::Println,
        "eprint"                        => BuiltinId::Eprint,
        "eprintln"                      => BuiltinId::Eprintln,
        "is_nil"                        => BuiltinId::IsNil,
        "is_bool"                       => BuiltinId::IsBool,
        "is_int"                        => BuiltinId::IsInt,
        "is_float"                      => BuiltinId::IsFloat,
        "is_str"                        => BuiltinId::IsStr,
        "is_collection"                 => BuiltinId::IsCollection,
        "is_function"                   => BuiltinId::IsFunction,
        "to_int"                        => BuiltinId::ToInt,
        "to_float"                      => BuiltinId::ToFloat,
        "to_str"                        => BuiltinId::ToStr,
        "to_bool"                       => BuiltinId::ToBool,
        "type_of"                       => BuiltinId::TypeOf,
        "assert"                        => BuiltinId::Assert,
        "panic"                         => BuiltinId::Panic,
        _ => return None,
    })
}

// ── Convenience entry points ──────────────────────────────────────────────────

/// Compile a Module to a top-level FunctionObject.
pub fn compile_module(module: &Module) -> Result<FunctionObject, GoblinError> {
    Compiler::new().compile_module(module)
}

/// Compile a single ActionDecl to a FunctionObject (for testing / embedding).
pub fn compile_action(action: &ActionDecl) -> Result<FunctionObject, GoblinError> {
    Compiler::new().compile_action(action)
}
