/// Bytecode compiler: walks goblin-ast and emits FunctionObject bytecode.
///
/// Design:
/// - Names are resolved at compile time to numeric slot indices.
/// - Each function body compiles into a fresh FunctionObject.
/// - Closures record upvalue descriptors so the VM can populate them at runtime.
/// - Unsupported AST nodes (ClassDecl, OverlayDef, etc.) produce CompileError.
///   These belong to the DES/object layer and will be handled separately.
use goblin_ast::{
    ActionBody, ActionDecl, BindMode, ClassDecl, EnumDecl, Expr, JudgeArm, JudgeArmBody, Module, ReturnStmt, Stmt,
};
use rust_decimal::Decimal;

use crate::error::GoblinError;
use crate::opcode::Opcode;
use crate::value::{BuiltinId, CompiledModule, FunctionObject, UpvalueDescriptor, Value};

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
    /// Source line number for each emitted opcode (parallel to bytecode).
    line_numbers: Vec<u32>,
    /// Current source line to attach to emitted opcodes.
    current_line: u32,
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
            line_numbers: Vec::new(),
            current_line: 0,
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
        self.line_numbers.push(self.current_line);
        pos
    }

    /// Emit a placeholder jump and return its index for back-patching.
    fn emit_jump(&mut self, op: fn(i16) -> Opcode) -> usize {
        self.emit(op(0))
    }

    /// Patch a previously emitted jump to the current position.
    fn patch_jump(&mut self, jump_idx: usize) {
        let current = self.bytecode.len();
        let offset = (current as isize - jump_idx as isize - 1) as i16;
        match &mut self.bytecode[jump_idx] {
            Opcode::Jump(o) | Opcode::JumpIfFalse(o) | Opcode::JumpIfTrue(o) => *o = offset,
            _ => panic!("patch_jump on non-jump opcode"),
        }
    }

    /// Patch a previously emitted jump to a specific target IP.
    fn patch_jump_to(&mut self, jump_idx: usize, target_ip: usize) {
        let offset = (target_ip as isize - jump_idx as isize - 1) as i16;
        match &mut self.bytecode[jump_idx] {
            Opcode::Jump(o) | Opcode::JumpIfFalse(o) | Opcode::JumpIfTrue(o) => *o = offset,
            _ => panic!("patch_jump_to on non-jump opcode"),
        }
    }

    fn finish(self) -> FunctionObject {
        let upvalue_descriptors: Vec<UpvalueDescriptor> =
            self.upvalues.into_iter().map(|(_, d)| d).collect();
        let total_slots = self.next_slot as usize;
        let mut local_names = vec![String::new(); total_slots];
        for (name, slot) in &self.locals {
            if (*slot as usize) < total_slots {
                local_names[*slot as usize] = name.clone();
            }
        }
        FunctionObject {
            bytecode: self.bytecode,
            constants: self.constants,
            locals: total_slots,
            params: self.params,
            name: self.name,
            upvalue_descriptors,
            line_numbers: self.line_numbers,
            local_names,
        }
    }
}

/// Recursively collect all variable names declared with `bind`/`|` (Tether) or
/// action names at module level so they can be pre-declared before the main
/// compilation pass. Does NOT recurse into Action/Class bodies (own scope).
fn collect_bind_names(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::Bind(b) if matches!(b.mode, BindMode::Tether | BindMode::Shadow) => {
                let name = b.name.0.clone();
                if !out.contains(&name) { out.push(name); }
            }
            Stmt::TupleBind(tb) if matches!(tb.mode, BindMode::Tether | BindMode::Shadow) => {
                for (name, _) in &tb.names {
                    if !out.contains(name) { out.push(name.clone()); }
                }
            }
            Stmt::Action(a) => {
                if !out.contains(&a.name) { out.push(a.name.clone()); }
            }
            Stmt::Block { stmts, .. } => collect_bind_names(stmts, out),
            Stmt::Judge(j) => {
                for arm in &j.arms {
                    if let JudgeArmBody::Stmts(stmts) = &arm.body {
                        collect_bind_names(stmts, out);
                    }
                }
            }
            Stmt::JudgeAll(j) => {
                for arm in &j.arms {
                    if let JudgeArmBody::Stmts(stmts) = &arm.body {
                        collect_bind_names(stmts, out);
                    }
                }
            }
            Stmt::Sweep(s) => {
                for arm in &s.arms {
                    collect_bind_names(&arm.body, out);
                }
            }
            _ => {}
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
    /// Class declarations collected during compilation.
    pub collected_classes: Vec<ClassDecl>,
    /// Enum declarations collected during compilation.
    pub collected_enums: Vec<EnumDecl>,
    /// Current source line (updated before compiling each AST node).
    current_line: u32,
    /// When true, unknown identifiers are compiled as self-field loads (for class methods).
    pub is_class_method: bool,
    /// Stack of loop contexts: (break_patch_indices, continue_ip).
    /// Innermost loop is at the back.
    loop_stack: Vec<LoopCtx>,
}

#[derive(Default)]
struct LoopCtx {
    /// Bytecode indices of Jump(0) placeholders emitted by `stop`.
    break_patches: Vec<usize>,
    /// Bytecode indices of Jump(0) placeholders emitted by `skip`.
    continue_patches: Vec<usize>,
    /// IP of the loop increment/condition check (for `skip`).
    continue_ip: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { scopes: Vec::new(), globals: Vec::new(), collected_classes: Vec::new(), collected_enums: Vec::new(), current_line: 0, is_class_method: false, loop_stack: Vec::new() }
    }

    // ── Public API ────────────────────────────────────────────────────────────

    /// Compile a top-level module into a FunctionObject (the module's "main").
    pub fn compile_module(mut self, module: &Module) -> Result<CompiledModule, GoblinError> {
        self.push_scope("__main__", 0);
        // Pre-declare all module-level variable and action names so forward
        // references resolve correctly (interpreter resolves names at runtime).
        let mut hoisted: Vec<String> = Vec::new();
        collect_bind_names(&module.items, &mut hoisted);
        for name in &hoisted {
            let slot = self.scope_mut().declare_local(name);
            self.emit(Opcode::LoadNil);
            self.emit(Opcode::StoreLocal(slot));
        }
        for stmt in &module.items {
            self.compile_stmt(stmt)?;
        }
        // Return nil at end of module.
        let scope = self.scopes.last_mut().unwrap();
        scope.emit(Opcode::LoadNil);
        scope.emit(Opcode::Return);
        let entry = self.pop_scope();
        Ok(CompiledModule {
            entry,
            classes: self.collected_classes,
            enums: self.collected_enums,
            global_names: self.globals.clone(),
        })
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
                let mut hoisted: Vec<String> = Vec::new();
                collect_bind_names(stmts, &mut hoisted);
                for name in hoisted.iter().filter(|n| !param_names.contains(n)) {
                    let slot = self.scope_mut().declare_local(name);
                    self.emit(Opcode::LoadNil);
                    self.emit(Opcode::StoreLocal(slot));
                }
                // For class methods, the last Stmt::Expr is an implicit return value.
                if self.is_class_method && !stmts.is_empty() {
                    let (body, last) = stmts.split_at(stmts.len() - 1);
                    for s in body { self.compile_stmt(s)?; }
                    match &last[0] {
                        Stmt::Expr(e) => {
                            self.compile_expr(e)?;
                            // Leave value on stack — don't Pop.
                        }
                        other => {
                            self.compile_stmt(other)?;
                            self.emit(Opcode::LoadNil);
                        }
                    }
                } else {
                    for s in stmts { self.compile_stmt(s)?; }
                    self.emit(Opcode::LoadNil);
                }
                let scope = self.scopes.last_mut().unwrap();
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

    fn set_current_line(&mut self, line: u32) {
        self.current_line = line;
        if let Some(scope) = self.scopes.last_mut() {
            scope.current_line = line;
        }
    }

    fn locate_err(&self, e: GoblinError) -> GoblinError {
        if matches!(e, GoblinError::WithLocation { .. }) { return e; }
        GoblinError::WithLocation { inner: Box::new(e), line: self.current_line }
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

        // 4. In a class method, unknown names are self-field accesses (shadow builtins).
        if self.is_class_method {
            let field_idx = self.add_constant(Value::Str(name.to_string()));
            return Ok(Opcode::SelfField(field_idx));
        }

        // 5. Check if it's a known builtin name.
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

                // If this is a typed bind (alice|Person = {...}), instantiate the class.
                if let Some(ref cn) = bind.class_name {
                    let idx = self.add_constant(Value::Str(cn.clone()));
                    self.emit(Opcode::ClassInstantiate(idx));
                }

                let name = &bind.name.0;
                match bind.mode {
                    BindMode::Tether => {
                        // x | expr — initial binding. Reuse pre-declared slot if
                        // present (hoisted from module pre-pass), else declare new.
                        let slot = self.scope_mut().find_local(name)
                            .unwrap_or_else(|| self.scopes.last_mut().unwrap().declare_local(name));
                        self.emit(Opcode::StoreLocal(slot));
                    }
                    BindMode::Retether => {
                        // x |= expr — rebind existing slot.
                        let op = self.resolve_store(name)
                            .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: name.clone() }))?;
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
                    // Multiple return values → named map (matches interpreter behaviour).
                    // Each identifier keeps its name as the key; non-ident exprs get "_1", "_2".
                    let mut pos = 1usize;
                    for v in values.iter() {
                        let key = match v {
                            Expr::Ident(name, _) => name.clone(),
                            _ => { let k = format!("_{}", pos); pos += 1; k }
                        };
                        let key_idx = self.add_constant(Value::Str(key));
                        self.emit(Opcode::LoadConst(key_idx));
                        self.compile_expr(v)?;
                    }
                    self.emit(Opcode::MakeMap(values.len() as u16));
                }
                self.emit(Opcode::Return);
            }

            Stmt::Action(action) => {
                // Nested action declaration: compile into a FunctionObject constant.
                self.compile_action_decl(action)?;
                // At top-level scope, also register by name for invoke().
                if self.scopes.len() == 1 {
                    let name_idx = self.add_constant(Value::Str(action.name.clone()));
                    self.emit(Opcode::RegisterAction(name_idx));
                }
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

            Stmt::TupleBind(tb) => {
                self.compile_expr(&tb.expr)?;
                let n = tb.names.len();
                for (i, name) in tb.names.iter().enumerate() {
                    if i < n - 1 {
                        self.emit(Opcode::Dup);
                    }
                    let idx_val = Value::Int(i as i64);
                    let cidx = self.add_constant(idx_val);
                    self.emit(Opcode::LoadConst(cidx));
                    self.emit(Opcode::GetIndex);
                    let name_str = &name.0;
                    let slot = self.scope_mut().declare_local(name_str);
                    self.emit(Opcode::StoreLocal(slot));
                }
            }

            // ── Class/Enum: collect at compile time, pre-registered before run ─
            Stmt::Class(decl) => {
                self.collected_classes.push(decl.clone());
            }
            Stmt::Enum(decl) => {
                self.collected_enums.push(decl.clone());
            }

            // ── Import / Use ──────────────────────────────────────────────────
            Stmt::Import(import_stmt) => {
                use goblin_ast::ImportItems;
                match &import_stmt.items {
                    ImportItems::Path(path) => {
                        // Resolve extension: try .gbln then .gob
                        let resolved = if path.ends_with(".gbln") || path.ends_with(".gob") || path.ends_with(".imports") {
                            path.replace('/', std::path::MAIN_SEPARATOR_STR)
                        } else {
                            format!("{}.gbln", path.replace('/', std::path::MAIN_SEPARATOR_STR))
                        };
                        let idx = self.add_constant(Value::Str(resolved));
                        self.emit(Opcode::ImportFile(idx));
                    }
                    ImportItems::Named { items, source } => {
                        // import { a, b } from source — import the source file
                        let resolved = format!("{}.gbln", source.replace('/', std::path::MAIN_SEPARATOR_STR));
                        let idx = self.add_constant(Value::Str(resolved));
                        self.emit(Opcode::ImportFile(idx));
                        let _ = items; // named imports — globals are populated by running the file
                    }
                    ImportItems::Expr(_) => {
                        return Err(GoblinError::NotImplemented { feature: "dynamic import paths" });
                    }
                }
            }
            Stmt::Use(use_stmt) => {
                // use namespace [as alias] — load glams/<namespace>/<namespace>.gbln
                let path = format!("glams{sep}{ns}{sep}{ns}.gbln",
                    sep = std::path::MAIN_SEPARATOR_STR,
                    ns = use_stmt.namespace);
                let idx = self.add_constant(Value::Str(path));
                self.emit(Opcode::ImportFile(idx));
            }

            // ── DES / Overlay / Link statements ──────────────────────────────
            Stmt::OverlayDef(def) => {
                self.emit(Opcode::OverlayDef(Box::new(def.clone())));
            }
            Stmt::OverlayApply(apply) => {
                self.compile_expr(&apply.host_expr)?;
                self.emit(Opcode::OverlayApply {
                    overlay_name: apply.overlay_name.clone(),
                    strength: apply.strength,
                    duration_override: apply.duration_override,
                });
            }
            Stmt::OverlayDetach(detach) => {
                self.compile_expr(&detach.host_expr)?;
                self.emit(Opcode::OverlayDetach { overlay_name: detach.overlay_name.clone() });
            }
            Stmt::LinkDef(def) => {
                self.emit(Opcode::LinkDef(Box::new(def.clone())));
            }
            Stmt::ObjectLinkDef(def) => {
                self.emit(Opcode::ObjectLinkDef(Box::new(def.clone())));
            }
            Stmt::LinkOffset(s) => {
                self.emit(Opcode::LinkOffset(Box::new(s.clone())));
            }
            Stmt::ClearLink(s) => {
                self.emit(Opcode::ClearLink(Box::new(s.clone())));
            }
            Stmt::ObjectDecision(var_name, def) => {
                self.emit(Opcode::ObjectDecision {
                    var_name: var_name.clone(),
                    def: Box::new(def.clone()),
                });
            }
            Stmt::UnitDecl(decl) => {
                self.emit(Opcode::UnitDecl(Box::new(decl.clone())));
            }

            // ── Unhandled ─────────────────────────────────────────────────────
            Stmt::BoxBind { .. } => {
                return Err(GoblinError::NotImplemented {
                    feature: "box bind statements are not supported in the VM",
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
        self.set_current_line(expr.span().line_start);
        match expr {
            // ── Literals ──────────────────────────────────────────────────────
            Expr::Nil(_) => { self.emit(Opcode::LoadNil); }
            Expr::Bool(true, _) => { self.emit(Opcode::LoadTrue); }
            Expr::Bool(false, _) => { self.emit(Opcode::LoadFalse); }

            Expr::Number(raw, _) => {
                let raw_str = raw.as_str();
                let is_integer_like = !raw_str.contains('.') && !raw_str.contains('e') && !raw_str.contains('E');
                let v = if is_integer_like {
                    let cleaned: String = raw_str.chars().filter(|&c| c != '_').collect();
                    if let Ok(i) = cleaned.parse::<i64>() {
                        let mag: i128 = if i >= 0 { i as i128 } else { -(i as i128) };
                        if mag <= 9_007_199_254_740_992i128 {
                            Value::Int(i)
                        } else {
                            let d = rust_decimal::Decimal::from_str_exact(&cleaned)
                                .map_err(|_| GoblinError::CompileError {
                                    message: format!("invalid number: {}", raw_str),
                                    span_debug: "(unknown)".into(),
                                })?;
                            Value::Big(d)
                        }
                    } else {
                        // Too large for i64 — parse as Decimal
                        let d = rust_decimal::Decimal::from_str_exact(&cleaned)
                            .map_err(|_| GoblinError::CompileError {
                                message: format!("invalid number: {}", raw_str),
                                span_debug: "(unknown)".into(),
                            })?;
                        Value::Big(d)
                    }
                } else {
                    // float
                    let cleaned: String = raw_str.chars().filter(|&c| c != '_').collect();
                    let f: f64 = cleaned.parse()
                        .map_err(|_| GoblinError::CompileError {
                            message: format!("invalid float: {}", raw_str),
                            span_debug: "(unknown)".into(),
                        })?;
                    Value::Float(f)
                };
                let idx = self.add_constant(v);
                self.emit(Opcode::LoadConst(idx));
            }

            Expr::Str(s, _) => {
                let idx = self.add_constant(Value::Str(s.clone()));
                if s.as_bytes().contains(&b'{') && !s.starts_with('\u{001E}') {
                    self.emit(Opcode::StringInterp(idx));
                } else {
                    self.emit(Opcode::LoadConst(idx));
                }
            }

            Expr::Char(c, _) => {
                let idx = self.add_constant(Value::Char(*c));
                self.emit(Opcode::LoadConst(idx));
            }

            // ── Variables ─────────────────────────────────────────────────────
            Expr::Ident(name, _) => {
                let op = self.resolve_load(name).map_err(|e| self.locate_err(e))?;
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
                // Special forms (look like calls but compile to control flow).
                if self.try_compile_special_form(name, args)? {
                    return Ok(());
                }
                // Check if it's a known builtin call pattern.
                if let Some(op) = self.try_compile_builtin_call(name, args)? {
                    let _ = op; // op already emitted
                } else {
                    let load_op = self.resolve_load(name).map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                }
            }

            Expr::Call(recv, method, args, _) => {
                // recv.method(args) — try free function first, fall back to method dispatch.
                // If method is a known local/global, call it directly with recv as first arg.
                // Otherwise emit CallMethod so the VM can dispatch on the receiver's class.
                if let Ok(load_op) = self.resolve_load(method) {
                    self.emit(load_op);
                    self.compile_expr(recv)?;
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call((args.len() + 1) as u8));
                } else {
                    // Method call: stack will be [recv, arg0, ..., arg_{n-1}]
                    self.compile_expr(recv)?;
                    for arg in args { self.compile_expr(arg)?; }
                    let method_idx = self.add_constant(Value::Str(method.clone()));
                    self.emit(Opcode::CallMethod(method_idx, args.len() as u8));
                }
            }

            Expr::NsCall(ns, name, args, _) => {
                // If the namespace starts with uppercase and there are no args,
                // treat as an enum variant: Status::idle → EnumVariantExpr("Status", "idle")
                let ns_is_enum = ns.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                if ns_is_enum && args.is_empty() {
                    let en_idx = self.add_constant(Value::Str(ns.clone()));
                    let vn_idx = self.add_constant(Value::Str(name.clone()));
                    self.emit(Opcode::LoadConst(en_idx));
                    self.emit(Opcode::LoadConst(vn_idx));
                    self.emit(Opcode::LoadNil);
                    self.emit(Opcode::CallBuiltin(BuiltinId::EnumVariantExpr, 3));
                } else {
                    // Namespace call: try full_name then bare name as a function.
                    let full_name = format!("{}::{}", ns, name);
                    let load_op = self.resolve_load(&full_name)
                        .or_else(|_| self.resolve_load(name))
                        .map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                }
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

            Expr::Postfix(inner, op, _) => {
                self.compile_expr(inner)?;
                match op.as_str() {
                    "%" => { self.emit(Opcode::ToPct); }
                    "**" => {
                        // x ** postfix = x^2 → Pow(x, 2)
                        let two = self.scope_mut().add_constant(Value::Int(2));
                        self.emit(Opcode::LoadConst(two));
                        self.emit(Opcode::CallBuiltin(BuiltinId::Pow, 2));
                    }
                    "//" => { self.emit(Opcode::CallBuiltin(BuiltinId::Sqrt, 1)); }
                    "++" | "--" => {
                        // x++ compiles to x + 1 (non-mutating form; mutation via |! is separate)
                        let one = self.scope_mut().add_constant(Value::Int(1));
                        self.emit(Opcode::LoadConst(one));
                        if op == "++" { self.emit(Opcode::Add); } else { self.emit(Opcode::Sub); }
                    }
                    _ => { /* ! and other postfix ops: compile inner value, no transform */ }
                }
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

            // arr[start:end] — push recv, start_or_nil, end_or_nil → SliceExpr
            Expr::Slice(recv, start_opt, end_opt, _) => {
                self.compile_expr(recv)?;
                if let Some(e) = start_opt { self.compile_expr(e)?; } else { self.emit(Opcode::LoadNil); }
                if let Some(e) = end_opt   { self.compile_expr(e)?; } else { self.emit(Opcode::LoadNil); }
                self.emit(Opcode::CallBuiltin(BuiltinId::SliceExpr, 3));
            }

            // arr[start:end:step] — push recv, start_or_nil, end_or_nil, step_or_nil → Slice3Expr
            Expr::Slice3(recv, start_opt, end_opt, step_opt, _) => {
                self.compile_expr(recv)?;
                if let Some(e) = start_opt { self.compile_expr(e)?; } else { self.emit(Opcode::LoadNil); }
                if let Some(e) = end_opt   { self.compile_expr(e)?; } else { self.emit(Opcode::LoadNil); }
                if let Some(e) = step_opt  { self.compile_expr(e)?; } else { self.emit(Opcode::LoadNil); }
                self.emit(Opcode::CallBuiltin(BuiltinId::Slice3Expr, 4));
            }

            // grid[x, y] — push grid, x, y → Index2Expr → GridRef
            Expr::Index2(base, x_expr, y_expr, _) => {
                self.compile_expr(base)?;
                self.compile_expr(x_expr)?;
                self.compile_expr(y_expr)?;
                self.emit(Opcode::CallBuiltin(BuiltinId::Index2Expr, 3));
            }

            // EnumName::Variant { fields } — push name_str, variant_str, fields_map_or_nil
            Expr::EnumVariant { enum_name, variant_name, fields, .. } => {
                let en_idx = self.add_constant(Value::Str(enum_name.clone()));
                let vn_idx = self.add_constant(Value::Str(variant_name.clone()));
                self.emit(Opcode::LoadConst(en_idx));
                self.emit(Opcode::LoadConst(vn_idx));
                if let Some(field_exprs) = fields {
                    let n = field_exprs.len() as u16;
                    for (k, v) in field_exprs {
                        let kidx = self.add_constant(Value::Str(k.clone()));
                        self.emit(Opcode::LoadConst(kidx));
                        self.compile_expr(v)?;
                    }
                    self.emit(Opcode::MakeMap(n));
                } else {
                    self.emit(Opcode::LoadNil);
                }
                self.emit(Opcode::CallBuiltin(BuiltinId::EnumVariantExpr, 3));
            }

            // Module::Token — push module_str, ident_str → LiteralTokenExpr
            Expr::LiteralToken { module, ident, .. } => {
                let midx = self.add_constant(Value::Str(module.clone()));
                let iidx = self.add_constant(Value::Str(ident.clone()));
                self.emit(Opcode::LoadConst(midx));
                self.emit(Opcode::LoadConst(iidx));
                self.emit(Opcode::CallBuiltin(BuiltinId::LiteralTokenExpr, 2));
            }

            // #namespace::name box var — push ns_str, name_str → BoxVarExpr
            Expr::BoxVar { namespace, name, .. } => {
                let nidx = self.add_constant(Value::Str(namespace.clone()));
                let aidx = self.add_constant(Value::Str(name.clone()));
                self.emit(Opcode::LoadConst(nidx));
                self.emit(Opcode::LoadConst(aidx));
                self.emit(Opcode::CallBuiltin(BuiltinId::BoxVarExpr, 2));
            }
        }
        Ok(())
    }

    fn compile_binary(&mut self, lhs: &Expr, op: &str, rhs: &Expr) -> Result<(), GoblinError> {
        // Pipeline operator: lhs >> rhs_call(args) → rhs_call(lhs, args)
        if op == ">>" {
            match rhs {
                Expr::FreeCall(name, args, _) => {
                    let argc = (args.len() + 1) as u8;
                    if let Some(id) = builtin_by_name(name) {
                        // Builtins: push lhs first, then rest of args
                        self.compile_expr(lhs)?;
                        for arg in args { self.compile_expr(arg)?; }
                        self.emit(Opcode::CallBuiltin(id, argc));
                    } else {
                        // User function: stack layout must be [func, lhs, args...]
                        self.resolve_load(name).map_err(|e| self.locate_err(e))?;
                        self.compile_expr(lhs)?;
                        for arg in args { self.compile_expr(arg)?; }
                        self.emit(Opcode::Call(argc));
                    }
                }
                Expr::Call(recv, method, args, _) => {
                    self.compile_expr(recv)?;
                    self.compile_expr(lhs)?;
                    for arg in args { self.compile_expr(arg)?; }
                    let method_idx = self.scope_mut().add_constant(Value::Str(method.clone()));
                    // Emit as member call: recv.method(lhs, args...)
                    let argc = (args.len() + 1) as u8;
                    self.emit(Opcode::GetMember(method_idx));
                    self.emit(Opcode::Call(argc));
                }
                _ => {
                    // Generic pipeline: evaluate rhs as callable, call with lhs
                    self.compile_expr(rhs)?;
                    self.compile_expr(lhs)?;
                    self.emit(Opcode::Call(1));
                }
            }
            return Ok(());
        }

        // Range operators
        if op == ".." {
            self.compile_expr(lhs)?;
            self.compile_expr(rhs)?;
            self.emit(Opcode::MakeRange);
            return Ok(());
        }
        if op == "..." {
            self.compile_expr(lhs)?;
            self.compile_expr(rhs)?;
            self.emit(Opcode::MakeRangeInclusive);
            return Ok(());
        }

        // Field read: obj >> field
        if op == ">>" {
            self.compile_expr(lhs)?;
            let field_name = match rhs {
                Expr::Ident(name, _) => name.clone(),
                other => return Err(GoblinError::Runtime(format!(">> rhs must be identifier, got {:?}", other))),
            };
            let idx = self.add_constant(Value::Str(field_name));
            self.emit(Opcode::GetMember(idx));
            return Ok(());
        }

        // Field assignment: (obj >> field) |= rhs  OR  obj.field |= rhs
        if op == "|=" {
            // Extract (obj_expr, field_name) from either Expr::Member or Expr::Binary(_, ">>", _)
            let field_target: Option<(&Expr, String)> = match lhs {
                Expr::Member(obj_expr, field_name, _) => Some((obj_expr.as_ref(), field_name.clone())),
                Expr::Binary(obj_expr, inner_op, field_expr, _) if inner_op == ">>" => {
                    let fname = match field_expr.as_ref() {
                        Expr::Ident(n, _) => n.clone(),
                        other => return Err(GoblinError::Runtime(format!(">> field must be identifier, got {:?}", other))),
                    };
                    Some((obj_expr.as_ref(), fname))
                }
                _ => None,
            };

            if let Some((obj_expr, field_name)) = field_target {
                self.compile_expr(obj_expr)?;
                self.compile_expr(rhs)?;
                let idx = self.add_constant(Value::Str(field_name));
                self.emit(Opcode::SetField(idx));
                let var_name = match obj_expr {
                    Expr::Ident(n, _) => n.clone(),
                    _ => return Err(GoblinError::Runtime("field assignment: object must be a simple variable".into())),
                };
                let store_op = self.resolve_store(&var_name)
                    .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: var_name.clone() }))?;
                self.emit(Opcode::Dup);
                self.emit(store_op);
                return Ok(());
            }

            // Simple variable retether as expression
            let var_name = match lhs {
                Expr::Ident(n, _) => n.clone(),
                _ => return Err(GoblinError::Runtime(format!("unsupported |= lhs: {:?}", lhs))),
            };
            self.compile_expr(rhs)?;
            let store_op = self.resolve_store(&var_name)
                .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: var_name.clone() }))?;
            self.emit(Opcode::Dup);
            self.emit(store_op);
            return Ok(());
        }

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

        // Null coalescing must be handled before we compile operands.
        if op == "??" {
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
            "+"          => Opcode::Add,
            "++"         => Opcode::Concat,
            "-"          => Opcode::Sub,
            "*"          => Opcode::Mul,
            "/"          => Opcode::Div,
            "%"          => Opcode::Rem,
            "=="  | "===" => Opcode::Eq,
            "!=" | "/=" | "!==" => Opcode::Ne,
            "<"          => Opcode::Lt,
            "<="         => Opcode::Le,
            ">"          => Opcode::Gt,
            ">="         => Opcode::Ge,
            // <> is class declaration syntax only — not a binary operator
            // Floor division: operands already on stack, emit Div then Floor builtin
            "//" => {
                self.emit(Opcode::Div);
                let floor_id = crate::value::BuiltinId::Floor;
                self.emit(Opcode::CallBuiltin(floor_id, 1));
                return Ok(());
            }
            // Exponentiation: operands already on stack, call Pow(2)
            "**" => {
                let pow_id = crate::value::BuiltinId::Pow;
                self.emit(Opcode::CallBuiltin(pow_id, 2));
                return Ok(());
            }
            // percent-of: pct of value  →  pct * value
            "of" | "%o" => Opcode::Mul,
            "><" => {
                self.emit(Opcode::MakePair);
                return Ok(());
            }
            _ => return Err(GoblinError::NotImplemented { feature: "unknown binary operator" }),
        };
        self.emit(instr);
        Ok(())
    }

    fn compile_judge_expr(
        &mut self,
        using: Option<&Expr>,
        header: Option<&Expr>,
        arms: &[JudgeArm],
        all: bool,
    ) -> Result<(), GoblinError> {
        // Extract enum name from `using` clause (e.g., `judge x using Status` → "Status")
        let using_name: Option<String> = using.and_then(|e| match e {
            Expr::Ident(n, _) => Some(n.clone()),
            _ => None,
        });

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
                    // If using an enum, qualify the condition: Status::idle
                    if let Some(ref en) = using_name {
                        if let Expr::Ident(variant, sp) = cond.as_ref() {
                            let qualified = Expr::NsCall(en.clone(), variant.clone(), vec![], sp.clone());
                            self.compile_expr(&qualified)?;
                        } else {
                            self.compile_expr(cond)?;
                        }
                    } else {
                        self.compile_expr(cond)?;
                    }
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
                // Pre-hoist all bind names in this function body so forward
                // references within the function resolve (same as module-level).
                let mut hoisted: Vec<String> = Vec::new();
                collect_bind_names(stmts, &mut hoisted);
                for name in hoisted.iter().filter(|n| !param_names.contains(n)) {
                    let slot = self.scope_mut().declare_local(name);
                    self.emit(Opcode::LoadNil);
                    self.emit(Opcode::StoreLocal(slot));
                }
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
    /// Compile special-form "function calls" that are actually control flow.
    /// Returns true if handled, false if the name is not a special form.
    fn try_compile_special_form(&mut self, name: &str, args: &[Expr]) -> Result<bool, GoblinError> {
        let bare = name.trim_start_matches(':');
        match bare {
            // if(cond, then [, else])
            "if" | "unless" => {
                let invert = bare == "unless";
                if args.is_empty() {
                    return Err(GoblinError::CompileError {
                        message: format!("'{bare}' requires at least a condition argument"),
                        span_debug: String::new(),
                    });
                }
                self.compile_expr(&args[0])?;
                if invert { self.emit(Opcode::Not); }
                let else_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // then branch
                if args.len() > 1 {
                    self.compile_expr(&args[1])?;
                } else {
                    self.emit(Opcode::LoadNil);
                }
                let end_jump = self.scope_mut().emit_jump(Opcode::Jump);
                self.scope_mut().patch_jump(else_jump);
                // else branch
                if args.len() > 2 {
                    self.compile_expr(&args[2])?;
                } else {
                    self.emit(Opcode::LoadNil);
                }
                self.scope_mut().patch_jump(end_jump);
                Ok(true)
            }

            // while(cond, body)
            "while" => {
                let loop_start = self.scope_mut().bytecode.len();
                if args.len() < 2 {
                    self.emit(Opcode::LoadNil);
                    return Ok(true);
                }
                self.compile_expr(&args[0])?;
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                self.compile_expr(&args[1])?;
                self.emit(Opcode::Pop); // discard body result
                // Jump back to loop start
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                self.scope_mut().patch_jump(exit_jump);
                self.emit(Opcode::LoadNil);
                Ok(true)
            }

            // repeat(n, body [, as_name])
            "repeat" => {
                if args.len() < 2 {
                    self.emit(Opcode::LoadNil);
                    return Ok(true);
                }
                // Compile limit, store in hidden local
                self.compile_expr(&args[0])?;
                let limit_slot = self.scope_mut().declare_local("__repeat_n__");
                self.emit(Opcode::StoreLocal(limit_slot));
                // counter = 0
                let zero = self.scope_mut().add_constant(Value::Int(0));
                self.emit(Opcode::LoadConst(zero));
                let counter_slot = self.scope_mut().declare_local("__repeat_i__");
                self.emit(Opcode::StoreLocal(counter_slot));
                // optional as_name binding
                let as_slot = if args.len() >= 3 {
                    let as_name = match &args[2] {
                        Expr::Str(s, _) => s.clone(),
                        Expr::Ident(s, _) => s.clone(),
                        _ => "__repeat_as__".into(),
                    };
                    let s = self.scope_mut().declare_local(&as_name);
                    self.emit(Opcode::LoadConst(zero));
                    self.emit(Opcode::StoreLocal(s));
                    Some(s)
                } else { None };
                // loop_start: if counter >= limit, exit
                let loop_start = self.scope_mut().bytecode.len();
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::LoadLocal(limit_slot));
                self.emit(Opcode::Lt);
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // update as_name if present
                if let Some(s) = as_slot {
                    self.emit(Opcode::LoadLocal(counter_slot));
                    self.emit(Opcode::StoreLocal(s));
                }
                // push loop context for stop/skip
                self.loop_stack.push(LoopCtx::default());
                // body
                self.compile_expr(&args[1])?;
                self.emit(Opcode::Pop);
                // increment position (skip jumps here)
                let increment_ip = self.scope_mut().bytecode.len();
                let one = self.scope_mut().add_constant(Value::Int(1));
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::LoadConst(one));
                self.emit(Opcode::Add);
                self.emit(Opcode::StoreLocal(counter_slot));
                // back-jump
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                // exit position
                let exit_ip = self.scope_mut().bytecode.len();
                self.scope_mut().patch_jump(exit_jump);
                // patch stop/skip jumps
                let ctx = self.loop_stack.pop().unwrap();
                for idx in ctx.break_patches {
                    self.scope_mut().patch_jump_to(idx, exit_ip);
                }
                for idx in ctx.continue_patches {
                    self.scope_mut().patch_jump_to(idx, increment_ip);
                }
                self.emit(Opcode::LoadNil);
                Ok(true)
            }

            // for(var_name, iterable, body)
            "for" => {
                if args.len() < 3 {
                    self.emit(Opcode::LoadNil);
                    return Ok(true);
                }
                let var_name = match &args[0] {
                    Expr::Str(s, _) => s.clone(),
                    Expr::Ident(s, _) => s.clone(),
                    _ => return Err(GoblinError::CompileError {
                        message: "for: first arg must be a variable name".into(),
                        span_debug: String::new(),
                    }),
                };
                // Compile iterable, store in hidden local
                self.compile_expr(&args[1])?;
                let iter_slot = self.scope_mut().declare_local("__for_iter__");
                self.emit(Opcode::StoreLocal(iter_slot));
                // index = 0
                let zero = self.scope_mut().add_constant(Value::Int(0));
                self.emit(Opcode::LoadConst(zero));
                let idx_slot = self.scope_mut().declare_local("__for_i__");
                self.emit(Opcode::StoreLocal(idx_slot));
                // loop variable slot
                let var_slot = self.scope_mut().declare_local(&var_name);
                self.emit(Opcode::LoadConst(zero));
                self.emit(Opcode::StoreLocal(var_slot));
                // loop_start: idx < count(iter)?
                let loop_start = self.scope_mut().bytecode.len();
                self.emit(Opcode::LoadLocal(idx_slot));
                self.emit(Opcode::LoadLocal(iter_slot));
                self.emit(Opcode::CallBuiltin(BuiltinId::Count, 1));
                self.emit(Opcode::Lt);
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // elem = iter[idx]
                self.emit(Opcode::LoadLocal(iter_slot));
                self.emit(Opcode::LoadLocal(idx_slot));
                self.emit(Opcode::GetIndex);
                self.emit(Opcode::StoreLocal(var_slot));
                // push loop context for stop/skip
                self.loop_stack.push(LoopCtx::default());
                // body
                self.compile_expr(&args[2])?;
                self.emit(Opcode::Pop);
                // increment position (skip jumps here)
                let increment_ip = self.scope_mut().bytecode.len();
                let one = self.scope_mut().add_constant(Value::Int(1));
                self.emit(Opcode::LoadLocal(idx_slot));
                self.emit(Opcode::LoadConst(one));
                self.emit(Opcode::Add);
                self.emit(Opcode::StoreLocal(idx_slot));
                // back-jump
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                let exit_ip = self.scope_mut().bytecode.len();
                self.scope_mut().patch_jump(exit_jump);
                // patch stop/skip jumps
                let ctx = self.loop_stack.pop().unwrap();
                for idx in ctx.break_patches {
                    self.scope_mut().patch_jump_to(idx, exit_ip);
                }
                for idx in ctx.continue_patches {
                    self.scope_mut().patch_jump_to(idx, increment_ip);
                }
                self.emit(Opcode::LoadNil);
                Ok(true)
            }

            // say(val) — print to stdout
            "say" => {
                if args.is_empty() {
                    self.emit(Opcode::CallBuiltin(BuiltinId::Println, 0));
                } else {
                    self.compile_expr(&args[0])?;
                    self.emit(Opcode::CallBuiltin(BuiltinId::Println, 1));
                }
                Ok(true)
            }

            // print(val) — no newline
            "print" => {
                for arg in args { self.compile_expr(arg)?; }
                self.emit(Opcode::CallBuiltin(BuiltinId::Print, args.len() as u8));
                Ok(true)
            }

            // skip / stop — loop control
            "skip" => {
                let patch_idx = self.scope_mut().bytecode.len();
                self.emit(Opcode::Jump(0)); // patched when loop compiles continue_ip
                if let Some(ctx) = self.loop_stack.last_mut() {
                    ctx.continue_patches.push(patch_idx);
                }
                Ok(true)
            }
            "stop" => {
                let patch_idx = self.scope_mut().bytecode.len();
                self.emit(Opcode::Jump(0)); // patched when loop compiles exit
                if let Some(ctx) = self.loop_stack.last_mut() {
                    ctx.break_patches.push(patch_idx);
                }
                Ok(true)
            }

            // return(val) — explicit return
            "return" => {
                if args.is_empty() {
                    self.emit(Opcode::LoadNil);
                } else if args.len() == 1 {
                    self.compile_expr(&args[0])?;
                } else {
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::MakeArray(args.len() as u16));
                }
                self.emit(Opcode::Return);
                Ok(true)
            }

            // collect(count, body) — evaluate body N times, collect results into array
            "collect" => {
                if args.len() < 2 {
                    self.emit(Opcode::LoadNil);
                    return Ok(true);
                }
                // Compile count, store in __collect_n__
                self.compile_expr(&args[0])?;
                let n_slot = self.scope_mut().declare_local("__collect_n__");
                self.emit(Opcode::StoreLocal(n_slot));
                // __collect_i__ = 0
                let zero_idx = self.scope_mut().add_constant(Value::Int(0));
                self.emit(Opcode::LoadConst(zero_idx));
                let i_slot = self.scope_mut().declare_local("__collect_i__");
                self.emit(Opcode::StoreLocal(i_slot));
                // __collect_arr__ = []
                self.emit(Opcode::MakeArray(0));
                let arr_slot = self.scope_mut().declare_local("__collect_arr__");
                self.emit(Opcode::StoreLocal(arr_slot));
                // loop_start:
                let loop_start = self.scope_mut().bytecode.len();
                // if i >= n, exit
                self.emit(Opcode::LoadLocal(i_slot));
                self.emit(Opcode::LoadLocal(n_slot));
                self.emit(Opcode::Lt);
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // evaluate body, then arr = array_push(arr, value)
                self.emit(Opcode::LoadLocal(arr_slot));
                self.compile_expr(&args[1])?;
                self.emit(Opcode::CallBuiltin(BuiltinId::ArrayPush, 2));
                self.emit(Opcode::StoreLocal(arr_slot));
                // i++
                let one_idx = self.scope_mut().add_constant(Value::Int(1));
                self.emit(Opcode::LoadLocal(i_slot));
                self.emit(Opcode::LoadConst(one_idx));
                self.emit(Opcode::Add);
                self.emit(Opcode::StoreLocal(i_slot));
                // back-jump
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                self.scope_mut().patch_jump(exit_jump);
                // push result array
                self.emit(Opcode::LoadLocal(arr_slot));
                Ok(true)
            }

            // attempt(try_block, rescues, [ensure_block]) — try/catch/ensure
            "attempt" => {
                if args.is_empty() {
                    self.emit(Opcode::LoadNil);
                    return Ok(true);
                }
                // Extract first rescue block info if present
                let first_rescue: Option<(Option<String>, usize)> = if args.len() > 1 {
                    if let Expr::Array(rescue_pairs, _) = &args[1] {
                        if let Some(Expr::Array(pair, _)) = rescue_pairs.first() {
                            if pair.len() >= 2 {
                                let var_name = match &pair[0] {
                                    Expr::Str(s, _) => Some(s.clone()),
                                    Expr::Ident(s, _) if s != "nil" => Some(s.clone()),
                                    _ => None,
                                };
                                Some((var_name, 0))
                            } else { None }
                        } else { None }
                    } else { None }
                } else { None };

                // Declare result slot
                let nil_idx = self.scope_mut().add_constant(Value::Nil);
                self.emit(Opcode::LoadConst(nil_idx));
                let result_slot = self.scope_mut().declare_local("__attempt_result__");
                self.emit(Opcode::StoreLocal(result_slot));

                // TryBegin → patch later
                let try_begin_pos = self.scope_mut().bytecode.len();
                self.emit(Opcode::TryBegin(0)); // placeholder

                // Compile try body
                self.compile_expr(&args[0])?;
                self.emit(Opcode::StoreLocal(result_slot));

                // TryEnd (normal completion)
                self.emit(Opcode::TryEnd);

                // Jump to end (skip catch block)
                let skip_catch = self.scope_mut().emit_jump(Opcode::Jump);

                // Catch block starts here — patch TryBegin offset
                let catch_ip = self.scope_mut().bytecode.len();
                {
                    // patch TryBegin: offset = catch_ip - (try_begin_pos + 1)
                    let offset = (catch_ip as i64 - (try_begin_pos as i64 + 1)) as i16;
                    self.scope_mut().bytecode[try_begin_pos] = Opcode::TryBegin(offset);
                }

                // TOS is error string — bind or pop
                if let Some((var_name_opt, rescue_pair_idx)) = first_rescue {
                    if let Expr::Array(rescue_pairs, _) = &args[1] {
                        if let Some(Expr::Array(pair, _)) = rescue_pairs.get(rescue_pair_idx) {
                            if pair.len() >= 2 {
                                if let Some(var_name) = var_name_opt {
                                    let err_slot = self.scope_mut().declare_local(&var_name);
                                    self.emit(Opcode::StoreLocal(err_slot));
                                    // We need to compile pair[1] but can't borrow args while compiling
                                    // Clone the rescue body index
                                    let rescue_body = pair[1].clone();
                                    self.compile_expr(&rescue_body)?;
                                    self.emit(Opcode::StoreLocal(result_slot));
                                } else {
                                    self.emit(Opcode::Pop); // discard error
                                    let rescue_body = pair[1].clone();
                                    self.compile_expr(&rescue_body)?;
                                    self.emit(Opcode::StoreLocal(result_slot));
                                }
                            } else {
                                self.emit(Opcode::Pop);
                            }
                        } else {
                            self.emit(Opcode::Pop);
                        }
                    } else {
                        self.emit(Opcode::Pop);
                    }
                } else {
                    self.emit(Opcode::Pop); // no rescue block, discard error
                }

                // end label
                self.scope_mut().patch_jump(skip_catch);

                // Ensure block (always runs, result discarded)
                if args.len() > 2 {
                    let ensure_body = args[2].clone();
                    self.compile_expr(&ensure_body)?;
                    self.emit(Opcode::Pop);
                }

                // Result
                self.emit(Opcode::LoadLocal(result_slot));
                Ok(true)
            }

            _ => Ok(false),
        }
    }

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

pub fn builtin_by_name(name: &str) -> Option<BuiltinId> {
    Some(match name {
        ":mem_id"      | "mem_id"       => BuiltinId::MemId,
        ":mem_addr"    | "mem_addr"     => BuiltinId::MemAddr,
        ":gc"          | "gc"           => BuiltinId::Gc,
        "abs"                           => BuiltinId::Abs,
        "min"                           => BuiltinId::Min,
        "max"                           => BuiltinId::Max,
        "avg"                           => BuiltinId::Avg,
        "sum"                           => BuiltinId::Sum,
        "floor"                         => BuiltinId::Floor,
        "ceil"                          => BuiltinId::Ceil,
        "round"                         => BuiltinId::Round,
        "sqrt"                          => BuiltinId::Sqrt,
        "clamp"                         => BuiltinId::Clamp,
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
        "before"                        => BuiltinId::Before,
        "after"                         => BuiltinId::After,
        "before_last"                   => BuiltinId::BeforeLast,
        "after_last"                    => BuiltinId::AfterLast,
        "keep_before"                   => BuiltinId::KeepBefore,
        "keep_after"                    => BuiltinId::KeepAfter,
        "keep_between"                  => BuiltinId::KeepBetween,
        "sanitize_bom"                  => BuiltinId::SanitizeBom,
        "normalize_newlines"            => BuiltinId::NormalizeNewlines,
        "ignore_where"                  => BuiltinId::IgnoreWhere,
        "ignore_lines_where"            => BuiltinId::IgnoreLinesWhere,
        "ignore_matching"               => BuiltinId::IgnoreMatching,
        "ignore_lines_matching"         => BuiltinId::IgnoreLinesMatching,
        "is_matching"                   => BuiltinId::IsMatching,
        "count_matching"                => BuiltinId::CountMatching,
        "keep_matching"                 => BuiltinId::KeepMatching,
        "json_parse"                    => BuiltinId::JsonParse,
        "json_stringify"                => BuiltinId::JsonStringify,
        "json_stringify_pretty"         => BuiltinId::JsonStringifyPretty,
        "ignore_between"                => BuiltinId::IgnoreBetween,
        "ignore_blocks"                 => BuiltinId::IgnoreBlocks,
        "env"                           => BuiltinId::Env,
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

        // Request (HTTP context)
        "req_method"                    => BuiltinId::ReqMethod,
        "req_path"                      => BuiltinId::ReqPath,
        "req_query"                     => BuiltinId::ReqQuery,
        "req_body"                      => BuiltinId::ReqBody,
        "req_header"                    => BuiltinId::ReqHeader,
        "cookie"                        => BuiltinId::Cookie,

        // Response
        "set_status"                    => BuiltinId::SetStatus,
        "set_header"                    => BuiltinId::SetHeader,
        "set_cookie"                    => BuiltinId::SetCookie,

        "roll"                          => BuiltinId::Roll,
        "roll_detail"                   => BuiltinId::RollDetail,
        "rand_seed"                     => BuiltinId::RandSeed,
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
        "reap"                          => BuiltinId::ReapSample,
        "reap_first"                    => BuiltinId::ReapFirst,
        "reap_last"                     => BuiltinId::ReapLast,
        "reap_at"                       => BuiltinId::ReapAt,
        "reap_random"                   => BuiltinId::ReapRandom,
        "reap_where"                    => BuiltinId::ReapWhere,
        "reap_all"                      => BuiltinId::ReapAll,
        // New Position×Operation matrix
        "get_first"    | ":get_first"    => BuiltinId::GetFirst,
        "get_last"     | ":get_last"     => BuiltinId::GetLast,
        "get_at"       | ":get_at"       => BuiltinId::GetAt,
        "get_where"    | ":get_where"    => BuiltinId::GetWhere,
        "get_all"      | ":get_all"      => BuiltinId::GetAll,
        "get_matching" | ":get_matching" => BuiltinId::GetMatching,
        "get_between"  | ":get_between"  => BuiltinId::GetBetween,
        "get_random"   | ":get_random"   => BuiltinId::GetRandom,
        "put_where"    | ":put_where"    => BuiltinId::PutWhere,
        "put_matching" | ":put_matching" => BuiltinId::PutMatching,
        "put_between"  | ":put_between"  => BuiltinId::PutBetween,
        "put_random"   | ":put_random"   => BuiltinId::PutRandom,
        "put_all"      | ":put_all"      => BuiltinId::PutAll,
        "update_all"      | ":update_all"      => BuiltinId::UpdateAll,
        "update_where"    | ":update_where"    => BuiltinId::UpdateWhere,
        "update_matching" | ":update_matching" => BuiltinId::UpdateMatching,
        "update_between"  | ":update_between"  => BuiltinId::UpdateBetween,
        "update_random"   | ":update_random"   => BuiltinId::UpdateRandom,
        "delete_matching" | ":delete_matching" => BuiltinId::DeleteMatching,
        "delete_between"  | ":delete_between"  => BuiltinId::DeleteBetween,
        "delete_random"   | ":delete_random"   => BuiltinId::DeleteRandom,
        "reap_matching"   | ":reap_matching"   => BuiltinId::ReapMatching,
        "reap_between"    | ":reap_between"    => BuiltinId::ReapBetween,
        "has"                           => BuiltinId::Has,
        "keys"                          => BuiltinId::Keys,
        "values"                        => BuiltinId::Values,
        "pairs"                         => BuiltinId::Pairs,
        "is_empty"                      => BuiltinId::IsEmpty,
        "reverse"                       => BuiltinId::Reverse,
        "reverse_chars"                 => BuiltinId::ReverseChars,
        "minimize"                      => BuiltinId::Minimize,
        "parse_bool"                    => BuiltinId::ParseBool,
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
        "is_big"                        => BuiltinId::IsBig,
        "is_pct"                        => BuiltinId::IsPct,
        "is_num"                        => BuiltinId::IsNum,
        "is_char"                       => BuiltinId::IsChar,
        "is_pair"                       => BuiltinId::IsPair,
        "is_seq"                        => BuiltinId::IsSeq,
        "is_unit"                       => BuiltinId::IsUnit,
        "is_alnum"                      => BuiltinId::IsAlnum,
        "is_alpha"                      => BuiltinId::IsAlpha,
        "is_digit"                      => BuiltinId::IsDigit,
        "is_whitespace"                 => BuiltinId::IsWhitespace,
        "is_even"                       => BuiltinId::IsEven,
        "is_odd"                        => BuiltinId::IsOdd,
        "is_multiple_of"                => BuiltinId::IsMultipleOf,
        "is_positive"                   => BuiltinId::IsPositive,
        "is_negative"                   => BuiltinId::IsNegative,
        "is_nix"                        => BuiltinId::IsNix,
        "to_int"                        => BuiltinId::ToInt,
        "to_float"                      => BuiltinId::ToFloat,
        "to_str"                        => BuiltinId::ToStr,
        "to_bool"                       => BuiltinId::ToBool,
        "type_of"                       => BuiltinId::TypeOf,
        "assert"                        => BuiltinId::Assert,
        "panic"                         => BuiltinId::Panic,
        "secure_pick"    | ":secure_pick"    => BuiltinId::SecurePick,
        "secure_random"  | ":secure_random"  => BuiltinId::SecureRandom,
        "secure_shuffle" | ":secure_shuffle" => BuiltinId::SecureShuffle,
        "pack"         | ":pack"          => BuiltinId::Pack,
        "unpack"       | ":unpack"        => BuiltinId::Unpack,
        "lines"        | ":lines"         => BuiltinId::Lines,
        "words"        | ":words"         => BuiltinId::Words,
        "chars"        | ":chars"         => BuiltinId::Chars,
        "format"       | ":format"        => BuiltinId::Format,
        "pad"          | ":pad"           => BuiltinId::Pad,
        "pad_left"     | ":pad_left"      => BuiltinId::PadLeft,
        "pad_right"    | ":pad_right"     => BuiltinId::PadRight,
        "repeat_str"   | ":repeat_str"    => BuiltinId::Repeat,
        "pct"                            => BuiltinId::Pct,
        "between"                        => BuiltinId::Between,
        "is_control"                     => BuiltinId::IsControl,
        "ignore_blocks_first"            => BuiltinId::IgnoreBlocksFirst,
        "pick"                           => BuiltinId::Pick,
        "read_json"                      => BuiltinId::ReadJson,
        "write_text"   | "write_text!"   => BuiltinId::WriteText,
        "append_file"  | "append_file!"  => BuiltinId::AppendFile,
        "write_json"   | "write_json!"   => BuiltinId::WriteJson,
        "is_type"                        => BuiltinId::IsType,
        "is_bound_name"                  => BuiltinId::IsBoundName,
        "invoke"                         => BuiltinId::Invoke,
        "summon"                         => BuiltinId::Summon,
        "provoke"                        => BuiltinId::Provoke,
        "yall_parse"                     => BuiltinId::YallParse,
        "yall_parse_file"                => BuiltinId::YallParseFile,
        "yall_write"                     => BuiltinId::YallWrite,
        "yall_write_file"                => BuiltinId::YallWriteFile,
        "yall_pretty"                    => BuiltinId::YallPretty,
        "yall_minify"                    => BuiltinId::YallMinify,
        "create_dir"   | "create_dir!"   => BuiltinId::CreateDir,
        "copy_file"    | "copy_file!"    => BuiltinId::CopyFile,
        "delete_path"  | "delete_path!"  => BuiltinId::DeletePath,
        "md_to_html"                     => BuiltinId::MdToHtml,
        "highlight_code"                 => BuiltinId::HighlightCode,
        "big"  | "b"                     => BuiltinId::ToBig,
        "to_map" | "m"                   => BuiltinId::ToMap,
        "read_text"                      => BuiltinId::ReadText,
        "array_push"                     => BuiltinId::ArrayPush,
        "i8"                             => BuiltinId::CastI8,
        "i16"                            => BuiltinId::CastI16,
        "i32"                            => BuiltinId::CastI32,
        "i64"                            => BuiltinId::CastI64,
        "u8"                             => BuiltinId::CastU8,
        "u16"                            => BuiltinId::CastU16,
        "u32"                            => BuiltinId::CastU32,
        "u64"                            => BuiltinId::CastU64,
        "f32"                            => BuiltinId::CastF32,
        "f64"                            => BuiltinId::CastF64,
        // Type casts as free calls
        "int"  | "i"                     => BuiltinId::ToInt,
        "float" | "f"                    => BuiltinId::ToFloat,
        "bool"                           => BuiltinId::ToBool,
        // Filesystem / path
        "file_exists"                    => BuiltinId::FileExists,
        "is_file"                        => BuiltinId::IsFile,
        "is_dir"                         => BuiltinId::IsDir,
        "basename"                       => BuiltinId::Basename,
        "dirname"                        => BuiltinId::Dirname,
        "stem"                           => BuiltinId::Stem,
        "ext"                            => BuiltinId::Ext,
        "path_join"                      => BuiltinId::PathJoin,
        "path_split"                     => BuiltinId::PathSplit,
        "path_normalize"                 => BuiltinId::PathNormalize,
        "path_relative_to"               => BuiltinId::PathRelativeTo,
        "walk"                           => BuiltinId::Walk,
        "list_dirs"                      => BuiltinId::ListDirs,
        "escape_html"                    => BuiltinId::EscapeHtml,
        "uuid_v4"                        => BuiltinId::UuidV4,
        "uuid_v7"                        => BuiltinId::UuidV7,
        "pathfind"                       => BuiltinId::Pathfind,
        // Interactive input
        "ask" | "input"                  => BuiltinId::AskInput,
        // Dice string
        "roll_str"                       => BuiltinId::RollStr,
        "roll_detail_str"                => BuiltinId::RollDetailStr,

        // Type/format
        "valtype" | "vt"                 => BuiltinId::ValType,
        "format_info"                    => BuiltinId::FormatInfo,
        "clear_format"                   => BuiltinId::ClearFormat,
        "backend"                        => BuiltinId::Backend,
        "metrics"                        => BuiltinId::Metrics,

        // Process
        "zip_dir"                        => BuiltinId::ZipDir,

        // Token store
        "register_token"                 => BuiltinId::RegisterToken,
        "resolve_token"                  => BuiltinId::ResolveToken,
        "clear_token"                    => BuiltinId::ClearToken,
        "clear_tokens"                   => BuiltinId::ClearTokens,
        "clear_all_tokens"               => BuiltinId::ClearAllTokens,
        "list_tokens"                    => BuiltinId::ListTokens,

        // Date/time type locks (not yet implemented)
        "date"                           => BuiltinId::CastDate,
        "time"                           => BuiltinId::CastTime,
        "datetime"                       => BuiltinId::CastDatetime,
        "duration"                       => BuiltinId::CastDuration,

        // DES tick
        "tick" | "tick_db"               => BuiltinId::Tick,

        // DES / overlay
        "decision_debug"                 => BuiltinId::DecisionDebug,
        "overlays_of"                    => BuiltinId::OverlaysOf,
        "overlay_strength"               => BuiltinId::OverlayStrength,
        "link_score"                     => BuiltinId::LinkScore,
        "owned_by"                       => BuiltinId::OwnedBy,
        "owns_tree"                      => BuiltinId::OwnsTree,
        "clone_object"                   => BuiltinId::CloneObject,
        "delete_object"                  => BuiltinId::DeleteObject,
        "delete_overlays_on"             => BuiltinId::DeleteOverlaysOn,

        // Grid
        "grid"                           => BuiltinId::Grid,
        "grid_get"                       => BuiltinId::GridGet,
        "grid_set"                       => BuiltinId::GridSet,
        "grid_void"                      => BuiltinId::GridVoid,
        "grid_tile_get"                  => BuiltinId::GridTileGet,
        "grid_tile_set"                  => BuiltinId::GridTileSet,
        "grid_region_get"                => BuiltinId::GridRegionGet,
        "grid_region_set"                => BuiltinId::GridRegionSet,
        "grid_default_get"               => BuiltinId::GridDefaultGet,
        "grid_default_set"               => BuiltinId::GridDefaultSet,
        "grid_neighbors"                 => BuiltinId::GridNeighbors,
        "grid_occupied"                  => BuiltinId::GridOccupied,
        "grid_unoccupied"                => BuiltinId::GridUnoccupied,
        "grid_occupied_count"            => BuiltinId::GridOccupiedCount,
        "grid_unoccupied_count"          => BuiltinId::GridUnoccupiedCount,
        "grid_count"                     => BuiltinId::GridCount,
        "grid_occupied_by"               => BuiltinId::GridOccupiedBy,
        "grid_has"                       => BuiltinId::GridHas,
        "grid_info"                      => BuiltinId::GridInfo,
        "grid_tile_info"                 => BuiltinId::GridTileInfo,
        "grid_region_info"               => BuiltinId::GridRegionInfo,

        _ => return None,
    })
}

// ── Convenience entry points ──────────────────────────────────────────────────

/// Compile a Module to a top-level FunctionObject.
pub fn compile_module(module: &Module) -> Result<CompiledModule, GoblinError> {
    Compiler::new().compile_module(module)
}

/// Compile a single ActionDecl to a FunctionObject (for testing / embedding).
pub fn compile_action(action: &ActionDecl) -> Result<FunctionObject, GoblinError> {
    Compiler::new().compile_action(action)
}
