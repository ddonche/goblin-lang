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
    /// Only the names visible at the current point: a block's locals are
    /// dropped when it closes (the slots are not reused).
    locals: Vec<(String, u8)>,
    /// Parallel to `locals`.
    local_meta: Vec<LocalMeta>,
    /// `locals.len()` at the start of each open block.
    block_marks: Vec<usize>,
    /// Every local ever declared, for slot names (string interpolation).
    all_locals: Vec<(String, u8)>,
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
    /// Arguments a caller must pass; the rest have defaults.
    required_params: usize,
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
    /// An `:objects(…)` / `:overlays(…)` predicate body: the fields of `it`
    /// are in scope by name, as in the interpreter.
    pred_fields: bool,
}

impl FunctionScope {
    fn new(name: impl Into<String>, params: usize) -> Self {
        FunctionScope {
            locals: Vec::new(),
            local_meta: Vec::new(),
            block_marks: Vec::new(),
            all_locals: Vec::new(),
            upvalues: Vec::new(),
            bytecode: Vec::new(),
            constants: Vec::new(),
            next_slot: params as u8,
            params,
            required_params: params,
            name: name.into(),
            break_patches: Vec::new(),
            continue_patches: Vec::new(),
            loop_start: None,
            line_numbers: Vec::new(),
            current_line: 0,
            pred_fields: false,
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
        self.local_meta.push(LocalMeta { depth: self.block_marks.len(), bound: true, imm: false });
        self.all_locals.push((name.to_string(), slot));
        slot
    }

    /// Declare a hoisted local: it has a slot from the start of the function
    /// (so forward references compile), but counts as bound only once its
    /// `x | v` statement is reached.
    fn declare_hoisted(&mut self, name: &str) -> u8 {
        let slot = self.declare_local(name);
        self.local_meta.last_mut().unwrap().bound = false;
        slot
    }

    /// Declare parameters (must be called before any other locals).
    fn declare_params(&mut self, names: &[String]) {
        for (i, n) in names.iter().enumerate() {
            self.locals.push((n.clone(), i as u8));
            self.local_meta.push(LocalMeta { depth: 0, bound: true, imm: false });
            self.all_locals.push((n.clone(), i as u8));
        }
    }

    /// The innermost visible local with this name: (index into `locals`, slot).
    fn find_local_entry(&self, name: &str) -> Option<(usize, u8)> {
        self.locals.iter().enumerate().rev().find(|(_, (n, _))| n == name).map(|(i, (_, s))| (i, *s))
    }

    fn begin_block(&mut self) {
        self.block_marks.push(self.locals.len());
    }

    /// Stop resolving names to this slot (the slot itself stays allocated).
    fn hide_local(&mut self, slot: u8) {
        if let Some(e) = self.locals.iter_mut().rev().find(|(_, s)| *s == slot) {
            e.0 = String::from("\u{0}hidden");
        }
    }

    fn end_block(&mut self) {
        if let Some(mark) = self.block_marks.pop() {
            self.locals.truncate(mark);
            self.local_meta.truncate(mark);
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
            Opcode::Jump(o) | Opcode::JumpIfFalse(o) | Opcode::JumpIfTrue(o)
            | Opcode::JumpIfLocalSet(_, o) | Opcode::LoadItFieldOrJump(_, o) => *o = offset,
            _ => panic!("patch_jump on non-jump opcode"),
        }
    }

    /// Patch a previously emitted jump to a specific target IP.
    fn patch_jump_to(&mut self, jump_idx: usize, target_ip: usize) {
        let offset = (target_ip as isize - jump_idx as isize - 1) as i16;
        match &mut self.bytecode[jump_idx] {
            Opcode::Jump(o) | Opcode::JumpIfFalse(o) | Opcode::JumpIfTrue(o)
            | Opcode::JumpIfLocalSet(_, o) | Opcode::LoadItFieldOrJump(_, o) => *o = offset,
            _ => panic!("patch_jump_to on non-jump opcode"),
        }
    }

    fn finish(self, source_file: String, global_names: std::rc::Rc<std::cell::OnceCell<Vec<String>>>) -> FunctionObject {
        let upvalue_descriptors: Vec<UpvalueDescriptor> =
            self.upvalues.into_iter().map(|(_, d)| d).collect();
        let total_slots = self.next_slot as usize;
        let mut local_names = vec![String::new(); total_slots];
        for (name, slot) in &self.all_locals {
            if (*slot as usize) < total_slots {
                local_names[*slot as usize] = name.clone();
            }
        }
        FunctionObject {
            bytecode: self.bytecode,
            constants: self.constants,
            locals: total_slots,
            params: self.params,
            required_params: self.required_params,
            name: self.name,
            upvalue_descriptors,
            line_numbers: self.line_numbers,
            local_names,
            owner_glam: None,
            source_file,
            global_names,
        }
    }
}

/// Collect action names declared directly in a statement list (one level deep).
/// Recurses into Block/Judge/JudgeAll/Sweep but NOT into Action bodies (those are
/// handled by compile_action_decl when it processes each body).
fn collect_action_names(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::Action(a) => {
                if !out.contains(&a.name) { out.push(a.name.clone()); }
            }
            Stmt::Block { stmts, .. } => collect_action_names(stmts, out),
            Stmt::Judge(j) => {
                for arm in &j.arms {
                    if let JudgeArmBody::Stmts(stmts) = &arm.body {
                        collect_action_names(stmts, out);
                    }
                }
            }
            Stmt::JudgeAll(j) => {
                for arm in &j.arms {
                    if let JudgeArmBody::Stmts(stmts) = &arm.body {
                        collect_action_names(stmts, out);
                    }
                }
            }
            Stmt::Sweep(s) => {
                for arm in &s.arms {
                    collect_action_names(&arm.body, out);
                }
            }
            _ => {}
        }
    }
}

/// Names bound with `x | v` directly at module top level (not inside blocks,
/// loops or actions): these become module globals.
fn collect_global_bind_names(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        if let Stmt::Bind(b) = stmt {
            if matches!(b.mode, BindMode::Tether) && !out.contains(&b.name.0) {
                out.push(b.name.0.clone());
            }
        }
    }
}

/// Recursively collect bind variable names declared at module level so they can
/// be pre-hoisted as locals in __main__ before the main compilation pass.
/// Does NOT collect action names (those are pre-registered as globals separately)
/// and does NOT recurse into Action/Class bodies (own scope).
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
    unit_globals: std::rc::Rc<std::cell::OnceCell<Vec<String>>>,
    /// Class declarations collected during compilation.
    pub collected_classes: Vec<ClassDecl>,
    /// Enum declarations collected during compilation.
    pub collected_enums: Vec<EnumDecl>,
    /// Current source line (updated before compiling each AST node).
    current_line: u32,
    /// When true, unknown identifiers are compiled as self-field loads (for class methods).
    pub is_class_method: bool,
    /// When true, top-level binds use StoreGlobal/LoadGlobal so state persists across REPL entries.
    pub repl_mode: bool,
    /// Number of globals that existed before this REPL snippet (for duplicate-bind detection).
    repl_known_globals_count: usize,
    /// Stack of loop contexts: (break_patch_indices, continue_ip).
    /// Innermost loop is at the back.
    loop_stack: Vec<LoopCtx>,
    /// `loop_stack.len()` when each open function scope began: loops outside
    /// the current function are not targets for its `skip` / `stop`.
    loop_floors: Vec<usize>,
    /// When compiling a GLAM's entry module (via `use <namespace>`), the namespace
    /// to stamp onto its top-level actions' `owner_glam`, for `:need()` resolution.
    glam_namespace: Option<String>,
    /// Source file being compiled (stamped onto every FunctionObject, for error messages).
    source_file: String,
    /// Names of actions declared in this module. Builtins are shadowable
    /// (docs/language-spec.md §4 "Built-ins (shadowable operations & types)"),
    /// so a free call to one of these names calls the user's action, never the
    /// builtin of the same name.
    user_actions: std::collections::HashSet<String>,
    /// Imported modules get their own global namespace: names they declare are
    /// stored as `<prefix><name>` so they cannot collide with the importer's
    /// (or another module's) globals and actions. None for the entry script.
    global_prefix: Option<String>,
    /// Top-level names whose `x | v` has been compiled (module globals), and
    /// which of them are `imm`.
    bound_globals: std::collections::HashSet<String>,
    imm_globals: std::collections::HashSet<String>,
}

#[derive(Clone, Copy)]
struct LocalMeta {
    /// Block depth the local was declared at (0 = the function body).
    depth: usize,
    /// False for a hoisted local whose binding has not been reached yet.
    bound: bool,
    /// Declared with `imm`.
    imm: bool,
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
        Compiler { scopes: Vec::new(), globals: Vec::new(), unit_globals: Default::default(), collected_classes: Vec::new(), collected_enums: Vec::new(), current_line: 0, is_class_method: false, repl_mode: false, repl_known_globals_count: 0, loop_stack: Vec::new(), loop_floors: Vec::new(), glam_namespace: None, source_file: String::new(), user_actions: Default::default(), global_prefix: None, bound_globals: Default::default(), imm_globals: Default::default() }
    }

    /// Set the source file name stamped onto compiled functions (for error messages).
    pub fn for_file(mut self, path: &str) -> Self {
        self.source_file = path.to_string();
        self
    }

    /// Pre-declare global names so they resolve to LoadGlobal/StoreGlobal at compile time.
    /// Used to inject runtime-provided globals (e.g. CLI `args`) before compiling a module.
    pub fn with_globals(mut self, names: &[&str]) -> Self {
        for name in names {
            if !self.globals.contains(&name.to_string()) {
                self.globals.push(name.to_string());
            }
        }
        self
    }

    /// Seed the compiler with the full list of already-allocated global names from the session.
    /// Imported modules call this so their new globals are appended at non-overlapping indices,
    /// preventing different modules from clobbering each other's slots in session.globals.
    pub fn with_initial_globals(mut self, names: Vec<String>) -> Self {
        self.globals = names;
        self
    }

    /// Give this module its own global namespace (see `global_prefix`).
    pub fn with_global_prefix(mut self, prefix: Option<String>) -> Self {
        self.global_prefix = prefix;
        self
    }

    /// Slot of a global visible from this module: its own name first, then a
    /// shared (unprefixed) one such as `args`.
    fn global_index(&self, name: &str) -> Option<usize> {
        if let Some(p) = &self.global_prefix {
            let own = format!("{p}{name}");
            if let Some(i) = self.globals.iter().position(|g| *g == own) { return Some(i); }
        }
        self.globals.iter().position(|g| g == name)
    }

    /// Declare a global owned by this module (prefixed in imported modules).
    fn declare_global(&mut self, name: &str) -> usize {
        let full = match &self.global_prefix { Some(p) => format!("{p}{name}"), None => name.to_string() };
        if let Some(i) = self.globals.iter().position(|g| *g == full) { return i; }
        self.globals.push(full);
        self.globals.len() - 1
    }

    /// Mark this compilation as a GLAM's entry module, so its top-level actions
    /// get `owner_glam` stamped for `:need()` resolution.
    pub fn with_glam_namespace(mut self, ns: Option<String>) -> Self {
        self.glam_namespace = ns;
        self
    }

    // ── Public API ────────────────────────────────────────────────────────────

    /// Compile a top-level module into a FunctionObject (the module's "main").
    pub fn compile_module(mut self, module: &Module) -> Result<CompiledModule, GoblinError> {
        // First pass: pre-register all module-level action names as globals so that
        // cross-action references inside function bodies compile to LoadGlobal rather
        // than capturing a nil upvalue from __main__'s locals. This matches the
        // interpreter, which looks up action names at call time from sess.actions/modules.
        // collect_action_names recurses into Block/Judge/Sweep so actions inside
        // conditional branches at module level are also pre-registered.
        {
            let mut module_action_names: Vec<String> = Vec::new();
            collect_action_names(&module.items, &mut module_action_names);
            for name in module_action_names {
                self.user_actions.insert(name.clone());
                self.declare_global(&name);
            }
        }
        self.push_scope("__main__", 0);
        // Pre-hoist bind variable names as nil locals so forward bind references work.
        // Action names are NOT hoisted here — they live in globals (pre-registered above).
        // Top-level `x | v` bindings are module globals: every action reads and
        // writes the same slot (as locals captured by upvalue, each action got
        // its own copy and writes were lost). Shadow and tuple binds stay locals.
        let mut global_binds: Vec<String> = Vec::new();
        collect_global_bind_names(&module.items, &mut global_binds);
        for name in &global_binds {
            let pos = self.declare_global(name);
            self.emit(Opcode::LoadNil);
            self.emit(Opcode::StoreGlobal(pos as u16));
        }
        let mut hoisted: Vec<String> = Vec::new();
        collect_bind_names(&module.items, &mut hoisted);
        for name in hoisted.iter().filter(|n| !global_binds.contains(n)) {
            let slot = self.scope_mut().declare_hoisted(name);
            self.emit(Opcode::LoadNil);
            self.emit(Opcode::StoreLocal(slot));
        }
        for stmt in &module.items {
            match stmt {
                // In the entry script, a top-level expression's value is offered
                // to the API response (GOBLIN_NONINTERACTIVE=1), as the
                // interpreter's runner does; imported modules never echo.
                Stmt::Expr(e) if self.global_prefix.is_none() => {
                    self.compile_expr(e)?;
                    self.emit(Opcode::CallBuiltin(BuiltinId::ApiEcho, 1));
                    self.emit(Opcode::Pop);
                }
                _ => self.compile_stmt(stmt)?,
            }
        }
        // Return nil at end of module.
        let scope = self.scopes.last_mut().unwrap();
        scope.emit(Opcode::LoadNil);
        scope.emit(Opcode::Return);
        let entry = self.pop_scope();
        self.seal_globals();
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
        self.compile_param_defaults(&action.params)?;
        match &action.body {
            ActionBody::Block(stmts) => {
                // Two-pass: pre-register nested action names as globals.
                let mut nested_action_names: Vec<String> = Vec::new();
                collect_action_names(stmts, &mut nested_action_names);
                for name in &nested_action_names {
                    self.declare_global(name);
                }

                let mut hoisted: Vec<String> = Vec::new();
                collect_bind_names(stmts, &mut hoisted);
                for name in hoisted.iter().filter(|n| !param_names.contains(n)) {
                    let slot = self.scope_mut().declare_hoisted(name);
                    self.emit(Opcode::LoadNil);
                    self.emit(Opcode::StoreLocal(slot));
                }
                // The last statement's value is the implicit return (matches interpreter).
                if !stmts.is_empty() {
                    let (body, last) = stmts.split_at(stmts.len() - 1);
                    for s in body { self.compile_stmt(s)?; }
                    match &last[0] {
                        Stmt::Expr(e) => {
                            self.compile_expr(e)?;
                            // Leave value on stack — don't Pop.
                        }
                        Stmt::Bind(b) => {
                            // Compile the full bind (stores into local), then reload the rhs value.
                            self.compile_stmt(&last[0])?;
                            self.compile_expr(&b.expr)?;
                        }
                        Stmt::TupleBind(b) => {
                            self.compile_stmt(&last[0])?;
                            self.compile_expr(&b.expr)?;
                        }
                        other => {
                            self.compile_stmt(other)?;
                            self.emit(Opcode::LoadUnit);
                        }
                    }
                } else {
                    self.emit(Opcode::LoadUnit);
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
        let f = self.pop_scope();
        self.seal_globals();
        Ok(f)
    }

    // ── Scope management ──────────────────────────────────────────────────────

    fn push_scope(&mut self, name: &str, params: usize) {
        self.scopes.push(FunctionScope::new(name, params));
        self.loop_floors.push(self.loop_stack.len());
    }

    fn pop_scope(&mut self) -> FunctionObject {
        self.loop_floors.pop();
        let names = self.unit_globals.clone();
        self.scopes.pop().unwrap().finish(self.source_file.clone(), names)
    }

    /// Record the unit's final global names in every function it produced.
    fn seal_globals(&self) {
        let _ = self.unit_globals.set(self.globals.clone());
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
        GoblinError::WithLocation { inner: Box::new(e), line: self.current_line, file: self.source_file.clone() }
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
        if let Some(pos) = self.global_index(name) {
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

    /// D16: the routes the interpreter guards (`|=`, tuple `|=`, `++`/`--`,
    /// bang casts, `x.method!`) raise R0113 on an `imm` name when reached.
    fn emit_imm_guard(&mut self, name: &str) -> Result<(), GoblinError> {
        if self.is_imm_name(name) {
            let msg = format!("cannot update imm binding '{}' (declared with imm)", name);
            self.emit_runtime_error(&msg)?;
        }
        Ok(())
    }

    /// Whether `name`, as resolved from the current scope (local, captured
    /// local of an enclosing action, or module global), was bound with `imm`.
    fn is_imm_name(&self, name: &str) -> bool {
        for sc in self.scopes.iter().rev() {
            if let Some((i, _)) = sc.find_local_entry(name) {
                return sc.local_meta[i].imm;
            }
        }
        self.imm_globals.contains(name)
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
        if let Some(pos) = self.global_index(name) {
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
                        // In REPL mode at top scope: use globals so state persists.
                        if self.repl_mode && self.scopes.len() == 1 {
                            if let Some(pos) = self.global_index(name) {
                                if pos < self.repl_known_globals_count {
                                    return Err(GoblinError::CompileError {
                                        message: format!("duplicate-local: '{}' is already bound", name),
                                        span_debug: format!("{:?}", bind.name.1),
                                    });
                                }
                                let name_idx = self.add_constant(Value::Str(name.clone()));
                                self.emit(Opcode::RegisterAction(name_idx));
                                if let Some(ref lock) = bind.lock_type {
                                    self.emit(Opcode::StoreLockGlobal(pos as u16, lock.clone()));
                                } else {
                                    self.emit(Opcode::StoreGlobal(pos as u16));
                                }
                            } else {
                                let pos = self.globals.len();
                                self.globals.push(name.clone());
                                let name_idx = self.add_constant(Value::Str(name.clone()));
                                self.emit(Opcode::RegisterAction(name_idx));
                                if let Some(ref lock) = bind.lock_type {
                                    self.emit(Opcode::StoreLockGlobal(pos as u16, lock.clone()));
                                } else {
                                    self.emit(Opcode::StoreGlobal(pos as u16));
                                }
                            }
                        } else {
                            self.compile_tether_store(name, bind.is_imm, &bind.lock_type)?;
                        }
                    }
                    BindMode::Retether => {
                        // x |= expr — rebind existing slot. Lock check handled in StoreLocal/StoreGlobal at runtime.
                        if self.is_imm_name(name) {
                            // D16: an `imm` binding cannot be updated (R0113).
                            let msg = format!("cannot update imm binding '{}' (declared with imm)", name);
                            return self.emit_runtime_error(&msg);
                        }
                        self.emit_obj_rebind_guard(name)?;
                        let op = self.resolve_store(name)
                            .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: name.clone() }))?;
                        self.emit(op);
                    }
                    BindMode::Shadow => {
                        self.compile_shadow_store(name, bind.is_imm, &bind.lock_type)?;
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
                self.user_actions.insert(action.name.clone());
                self.compile_action_decl(action)?;
                // All actions — regardless of nesting depth — register in named_values
                // and store to the global slot. The interpreter's sess.actions is a flat
                // global pool: sess.actions.insert() runs for every Stmt::Action whenever
                // current_module is None (which includes all runtime execution, not just
                // module loading). The VM matches this: all actions live in globals and are
                // registered in named_values on execution, enabling string-based dispatch
                // from collection builtins (grab_where, map, etc.) at any nesting level.
                let name_idx = self.add_constant(Value::Str(action.name.clone()));
                self.emit(Opcode::RegisterAction(name_idx));
                let pos = self.declare_global(&action.name);
                self.emit(Opcode::StoreGlobal(pos as u16));
            }

            Stmt::Judge(judge) => {
                self.compile_judge_stmt(&judge.arms)?;
            }

            Stmt::JudgeAll(judge_all) => {
                // Execute every matching arm (no short-circuit); `else` runs only
                // when no arm matched, tracked in a hidden local.
                let has_else = judge_all.arms.iter().any(|a| a.condition.is_none());
                let matched = if has_else {
                    self.emit(Opcode::LoadFalse);
                    let slot = self.scope_mut().declare_local("\u{0}judge_all_matched");
                    self.emit(Opcode::StoreLocal(slot));
                    Some(slot)
                } else { None };
                for arm in &judge_all.arms {
                    if let Some(cond) = &arm.condition {
                        self.compile_expr(cond)?;
                        let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        if let Some(slot) = matched {
                            self.emit(Opcode::LoadTrue);
                            self.emit(Opcode::StoreLocal(slot));
                        }
                        self.compile_arm_body(&arm.body)?;
                        self.scope_mut().patch_jump(skip);
                    } else {
                        let slot = matched.expect("else arm implies a matched flag");
                        self.emit(Opcode::LoadLocal(slot));
                        let skip = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
                        self.compile_arm_body(&arm.body)?;
                        self.scope_mut().patch_jump(skip);
                    }
                }
                if let Some(slot) = matched { self.scope_mut().hide_local(slot); }
            }

            Stmt::Block { stmts, .. } => {
                for s in stmts { self.compile_stmt(s)?; }
            }

            Stmt::Sweep(sweep) => {
                self.compile_sweep(sweep)?;
            }

            Stmt::TupleBind(tb) => {
                self.compile_expr(&tb.expr)?;
                let n = tb.names.len() as u8;
                self.emit(Opcode::TupleSplit(n));
                for name in tb.names.iter() {
                    let name_str = &name.0;
                    if name_str.starts_with('#') && name_str.contains("::") {
                        // Box ref target: parse ns::key and emit StoreBox
                        let key = name_str.trim_start_matches('#');
                        let mut parts = key.splitn(2, "::");
                        let ns_str = parts.next().unwrap_or("").to_string();
                        let nm_str = parts.next().unwrap_or("").to_string();
                        let ns_idx = self.add_constant(Value::Str(ns_str)) as u16;
                        let nm_idx = self.add_constant(Value::Str(nm_str)) as u16;
                        self.emit(Opcode::StoreBox(ns_idx, nm_idx, matches!(tb.mode, BindMode::Tether)));
                    } else {
                        let store_op = match tb.mode {
                            BindMode::Retether => {
                                self.emit_imm_guard(name_str)?;
                                self.emit_obj_rebind_guard(name_str)?;
                                self.resolve_store(name_str)
                                    .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: name_str.to_string() }))?
                            }
                            BindMode::Shadow => {
                                self.compile_shadow_store(&name.0, tb.is_imm, &None)?;
                                continue;
                            }
                            BindMode::Tether if !(self.repl_mode && self.scopes.len() == 1) => {
                                self.compile_tether_store(&name.0, tb.is_imm, &None)?;
                                continue;
                            }
                            _ => {
                                let slot = self.scope_mut().declare_local(name_str);
                                Opcode::StoreLocal(slot)
                            }
                        };
                        self.emit(store_op);
                    }
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
                        let path_idx = self.add_constant(Value::Str(resolved));
                        if let Some(alias) = &import_stmt.alias {
                            // `import path as ns` — register actions under the alias namespace
                            // so that `ns::action(...)` dispatch works (same as GLAM `use`).
                            let ns_idx = self.add_constant(Value::Str(alias.clone()));
                            self.emit(Opcode::ImportFileAs(path_idx, ns_idx));
                        } else {
                            self.emit(Opcode::ImportFile(path_idx));
                        }
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
                // use namespace [as alias] — read glams/<namespace>/glam.toml (for
                // [needs.actions]) then load glams/<namespace>/<namespace>.gbln,
                // stamping owner_glam = Some(namespace) on its top-level actions.
                let idx = self.add_constant(Value::Str(use_stmt.namespace.clone()));
                self.emit(Opcode::UseGlam(idx));
            }

            // ── DES / Overlay / Link statements ──────────────────────────────
            Stmt::OverlayDef(def) => {
                self.emit(Opcode::OverlayDef(Box::new(def.clone())));
            }
            Stmt::OverlayApply(apply) => {
                self.compile_expr(&apply.host_expr)?;
                let host_var_name = match &apply.host_expr {
                    goblin_ast::Expr::Ident(name, _) => name.clone(),
                    _ => String::new(),
                };
                self.emit(Opcode::OverlayApply {
                    overlay_name: apply.overlay_name.clone(),
                    host_var_name,
                    strength: apply.strength,
                    duration_override: apply.duration_override,
                });
            }
            Stmt::OverlayDetach(detach) => {
                self.compile_expr(&detach.host_expr)?;
                let host_var_name = match &detach.host_expr {
                    goblin_ast::Expr::Ident(name, _) => name.clone(),
                    _ => String::new(),
                };
                self.emit(Opcode::OverlayDetach { overlay_name: detach.overlay_name.clone(), host_var_name });
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

            // ── Box store bind: #namespace::name = expr ───────────────────────
            Stmt::BoxBind { namespace, name, expr, mode, .. } => {
                // Push namespace string, name string, value, is-tether → BoxBindExpr
                let ns_idx = self.add_constant(Value::Str(namespace.clone()));
                let nm_idx = self.add_constant(Value::Str(name.clone()));
                self.emit(Opcode::BoxCheck(ns_idx as u16, nm_idx as u16));
                self.emit(Opcode::LoadConst(ns_idx as u16));
                self.emit(Opcode::LoadConst(nm_idx as u16));
                self.compile_expr(expr)?;
                self.emit(if matches!(mode, BindMode::Tether) { Opcode::LoadTrue } else { Opcode::LoadFalse });
                self.emit(Opcode::CallBuiltin(BuiltinId::BoxBindExpr, 4));
                self.emit(Opcode::Pop);
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
        // Only one arm runs, so each arm is compiled as if the others had not
        // bound anything; after the judge a name counts as bound if any arm
        // bound it (arms share the enclosing block, as in the interpreter).
        let before: Vec<bool> = self.scope().local_meta.iter().map(|m| m.bound).collect();
        let mut bound_after: Vec<bool> = before.clone();
        let reset = |c: &mut Self, bound_after: &mut Vec<bool>| {
            let meta = &mut c.scopes.last_mut().unwrap().local_meta;
            for (i, m) in meta.iter_mut().enumerate() {
                if i >= bound_after.len() { bound_after.push(false); }
                bound_after[i] |= m.bound;
                m.bound = before.get(i).copied().unwrap_or(false);
            }
        };

        for (i, arm) in arms.iter().enumerate() {
            if i > 0 { reset(self, &mut bound_after); }
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
        reset(self, &mut bound_after);
        let meta = &mut self.scopes.last_mut().unwrap().local_meta;
        for (i, m) in meta.iter_mut().enumerate() {
            m.bound = bound_after.get(i).copied().unwrap_or(m.bound);
        }
        Ok(())
    }

    fn compile_sweep(&mut self, sweep: &goblin_ast::SweepStmt) -> Result<(), GoblinError> {
        // A sweep is a loop driven by the sweep runtime (sweep.rs): it finds
        // each match, the arm's body runs with `self` bound to the matched
        // text, and the arm's `self` is spliced back. Like the interpreter, the
        // body runs in the enclosing block (its bindings stay visible after
        // the sweep); `self` is visible only inside the sweep. `skip` keeps the
        // text unchanged and moves on, `stop` ends the whole sweep.
        use goblin_ast::{SweepArmKind, SweepArmRepeat, SweepMode};

        let spec: Vec<Value> = sweep.arms.iter().map(|arm| {
            let s = |x: &str| Value::Str(x.to_string());
            let fields = match &arm.kind {
                SweepArmKind::Pattern(p) => vec![s("pattern"), s(p), s(""), s("all")],
                SweepArmKind::Range { start, end } => vec![s("range"), s(start), s(end), s(match arm.repeat {
                    SweepArmRepeat::All => "all", SweepArmRepeat::First => "first", SweepArmRepeat::Last => "last",
                })],
                SweepArmKind::AllBody => vec![s("all"), s(""), s(""), s("all")],
            };
            Value::Array(fields)
        }).collect();
        let spec_idx = self.add_constant(Value::Array(spec));
        self.emit(Opcode::LoadConst(spec_idx));
        self.emit(if matches!(sweep.mode, SweepMode::All) { Opcode::LoadTrue } else { Opcode::LoadFalse });
        for t in &sweep.targets { self.compile_expr(t)?; }
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepBegin, (2 + sweep.targets.len()) as u8));
        let id_slot = self.scope_mut().declare_local("__sweep_id__");
        self.emit(Opcode::StoreLocal(id_slot));
        let arm_slot = self.scope_mut().declare_local("__sweep_arm__");
        let self_slot = self.scope_mut().declare_local("self");
        self.emit(Opcode::LoadNil);
        self.emit(Opcode::StoreLocal(self_slot));

        // loop: arm = next(id); done when -1
        let loop_top = self.scope_mut().bytecode.len();
        self.emit(Opcode::LoadLocal(id_slot));
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepNext, 1));
        self.emit(Opcode::StoreLocal(arm_slot));
        self.emit(Opcode::LoadLocal(arm_slot));
        let minus_one = self.add_constant(Value::Int(-1));
        self.emit(Opcode::LoadConst(minus_one));
        self.emit(Opcode::Eq);
        let done_jump = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
        self.emit(Opcode::LoadLocal(id_slot));
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepSelf, 1));
        self.emit(Opcode::StoreLocal(self_slot));

        let mut to_apply: Vec<usize> = Vec::new();
        let mut to_skip: Vec<usize> = Vec::new();
        let mut to_stop: Vec<usize> = Vec::new();
        for (i, arm) in sweep.arms.iter().enumerate() {
            self.emit(Opcode::LoadLocal(arm_slot));
            let k = self.add_constant(Value::Int(i as i64));
            self.emit(Opcode::LoadConst(k));
            self.emit(Opcode::Eq);
            let next_arm = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
            self.loop_stack.push(LoopCtx::default());
            for st in &arm.body { self.compile_stmt(st)?; }
            let ctx = self.loop_stack.pop().unwrap();
            to_skip.extend(ctx.continue_patches);
            to_stop.extend(ctx.break_patches);
            to_apply.push(self.scope_mut().emit_jump(Opcode::Jump));
            self.scope_mut().patch_jump(next_arm);
        }

        // apply(id, self, false); then the next match
        let apply_ip = self.scope_mut().bytecode.len();
        for j in to_apply { self.scope_mut().patch_jump_to(j, apply_ip); }
        self.emit(Opcode::LoadLocal(id_slot));
        self.emit(Opcode::LoadLocal(self_slot));
        self.emit(Opcode::LoadFalse);
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepApply, 3));
        self.emit(Opcode::Pop);
        let cur = self.scope_mut().bytecode.len();
        self.emit(Opcode::Jump(-(((cur - loop_top) as i16) + 1)));

        // skip: apply(id, self, true)
        let skip_ip = self.scope_mut().bytecode.len();
        for j in to_skip { self.scope_mut().patch_jump_to(j, skip_ip); }
        self.emit(Opcode::LoadLocal(id_slot));
        self.emit(Opcode::LoadLocal(self_slot));
        self.emit(Opcode::LoadTrue);
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepApply, 3));
        self.emit(Opcode::Pop);
        let cur = self.scope_mut().bytecode.len();
        self.emit(Opcode::Jump(-(((cur - loop_top) as i16) + 1)));

        // stop: end(id)
        let stop_ip = self.scope_mut().bytecode.len();
        for j in to_stop { self.scope_mut().patch_jump_to(j, stop_ip); }
        self.emit(Opcode::LoadLocal(id_slot));
        self.emit(Opcode::CallBuiltin(BuiltinId::SweepEnd, 1));
        self.emit(Opcode::Pop);

        self.scope_mut().patch_jump(done_jump);
        // `self` goes out of view after the sweep (an outer `self` is visible again).
        self.scope_mut().hide_local(self_slot);
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
                    // Resolve `{name}` placeholders to the variables in scope here
                    // (locals, captured upvalues, globals) so shadowed and
                    // block-local names interpolate the right binding.
                    let mut names: Vec<String> = Vec::new();
                    for name in interp_placeholder_names(s) {
                        let ok = match self.scope().find_local_entry(&name) {
                            Some((i, _)) => self.scope().local_meta[i].bound,
                            None => true,
                        };
                        if !ok { continue; }
                        match self.resolve_load(&name) {
                            Ok(op @ (Opcode::LoadLocal(_) | Opcode::LoadUpvalue(_) | Opcode::LoadGlobal(_))) => {
                                self.emit(op);
                                names.push(name);
                            }
                            _ => {}
                        }
                    }
                    if names.is_empty() {
                        self.emit(Opcode::StringInterp(idx));
                    } else {
                        let mut parts = vec![Value::Str(s.clone())];
                        parts.extend(names.iter().cloned().map(Value::Str));
                        let tidx = self.add_constant(Value::Array(parts));
                        self.emit(Opcode::StringInterpVals(tidx, names.len() as u8));
                    }
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
                // In a query predicate, a field of `it` shadows the name.
                let field_jump = if self.scope().pred_fields && name != "it"
                    && self.scope().find_local(name).is_none() {
                    let idx = self.add_constant(Value::Str(name.clone()));
                    Some(self.emit(Opcode::LoadItFieldOrJump(idx, 0)))
                } else { None };
                match self.resolve_load(name) {
                    Ok(op) => { self.emit(op); }
                    Err(_) => {
                        // Unknown at compile time — may come from an imported module.
                        // Emit a runtime named-value lookup as fallback.
                        let name_idx = self.add_constant(Value::Str(name.clone()));
                        self.emit(Opcode::LoadNamed(name_idx));
                    }
                }
                if let Some(j) = field_jump { self.scope_mut().patch_jump(j); }
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
            // Arrays and strings take `[]`, maps take `{}` (owner, 2026-10-06).
            Expr::Index(obj, idx, _) => {
                self.compile_expr(obj)?;
                self.compile_expr(idx)?;
                let label = self.add_constant(Value::Str(source_text(obj)));
                self.emit(Opcode::IndexGet(label));
            }

            Expr::IndexMap(obj, key, _) => {
                self.compile_expr(obj)?;
                self.compile_expr(key)?;
                let label = self.add_constant(Value::Str(source_text(obj)));
                self.emit(Opcode::KeyGet(label));
            }

            Expr::Member(obj, name, _) => {
                const CAST_TYPES: &[&str] = &[
                    "str", "string", "bool", "int", "uint", "float", "big", "pct",
                    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
                ];
                // Special case: Ident.vt / Ident.valtype — return type lock if set.
                if name == "vt" || name == "valtype" {
                    if let Expr::Ident(var_name, _) = obj.as_ref() {
                        if let Some(slot) = self.scope().find_local(var_name) {
                            self.emit(Opcode::GetTypeLockLocal(slot));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(var_name) {
                            self.emit(Opcode::GetTypeLockGlobal(pos as u16));
                            return Ok(());
                        }
                    }
                }
                // Special case: Ident.cast_type — check hard lock, then cast.
                if CAST_TYPES.contains(&name.as_str()) {
                    if let Expr::Ident(var_name, _) = obj.as_ref() {
                        if let Some(slot) = self.scope().find_local(var_name) {
                            self.emit(Opcode::CastMemberLocal(slot, name.clone()));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(var_name) {
                            self.emit(Opcode::CastMemberGlobal(pos as u16, name.clone()));
                            return Ok(());
                        }
                    }
                }
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
                // A user-defined action shadows any builtin of the same name.
                let bare_name = name.trim_start_matches(':');
                if self.user_actions.contains(bare_name) {
                    let load_op = self.resolve_load(bare_name).map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                    return Ok(());
                }
                // `raw "…"`: the literal is kept exactly as written, so it is
                // loaded as a constant and never interpolated.
                if bare_name == "raw" && args.len() == 1 {
                    if let Expr::Str(s, _) = &args[0] {
                        let idx = self.add_constant(Value::Str(s.clone()));
                        self.emit(Opcode::LoadConst(idx));
                        return Ok(());
                    }
                }
                // delete_object!(x) / delete_overlays_on!(x) act on the variable
                // itself and write nothing back (as in the interpreter). The
                // builtin gets [x's value, "x", 1]; a wrong arity or a non-variable
                // argument is passed as [nil, nil, argc] so it raises at run time.
                if bare_name == "delete_object!" || bare_name == "delete_overlays_on!" {
                    let bid = if bare_name == "delete_object!" { BuiltinId::DeleteObject } else { BuiltinId::DeleteOverlaysOn };
                    match args.as_slice() {
                        [Expr::Ident(var, _)] => {
                            self.compile_expr(&args[0])?;
                            let idx = self.add_constant(Value::Str(var.clone()));
                            self.emit(Opcode::LoadConst(idx));
                        }
                        _ => {
                            self.emit(Opcode::LoadNil);
                            self.emit(Opcode::LoadNil);
                        }
                    }
                    let n = self.add_constant(Value::Int(args.len() as i64));
                    self.emit(Opcode::LoadConst(n));
                    self.emit(Opcode::CallBuiltin(bid, 3));
                    return Ok(());
                }
                // reap_*!(target, …): the call's value is what was reaped, and
                // the target is left holding the rest (as in the interpreter):
                // value = reap_X(…), rest = delete_X(…).
                if let Some(pos_name) = reap_bang_position(bare_name) {
                    if let Some(target) = args.first().filter(|t| is_lvalue(t)) {
                        let sp = target.span().clone();
                        self.compile_expr(&Expr::FreeCall(format!("reap_{pos_name}"), args.clone(), sp.clone()))?;
                        self.compile_expr(&Expr::FreeCall(format!("delete_{pos_name}"), args.clone(), sp))?;
                        self.compile_store_from_stack(target)?;
                        return Ok(());
                    }
                }
                // name!(x[k]…, args…): change the element at the path, in
                // place when x's collection is not shared.
                if let Some((bid, root, keys)) = bang_path_call(name, args) {
                    let load_op = self.resolve_load(&root).map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    for k in &keys { self.compile_expr(k)?; }
                    self.emit_check_path(&args[0])?;
                    for a in &args[1..] { self.compile_expr(a)?; }
                    self.emit(Opcode::CallBuiltinMutPath(bid, args.len() as u8, keys.len() as u8));
                    let store_op = self.resolve_store(&root)
                        .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: root.clone() }))?;
                    self.emit(store_op);
                    self.emit(Opcode::LoadNil);
                    return Ok(());
                }
                // Bang casts `:int!(k)` recast one plain variable in place,
                // like `k.int!`: R0802 for anything else, R0113 on `imm`,
                // R0215 against a type lock.
                if let Some(base) = bare_name.strip_suffix('!').filter(|b| CAST_BANG_FREE.contains(b)) {
                    let Some(Expr::Ident(var_name, _)) = args.first().filter(|_| args.len() == 1) else {
                        return self.emit_runtime_error(&format!(
                            "R0802: {base}!(name) requires exactly one variable name, not an expression"));
                    };
                    let var_name = var_name.clone();
                    self.emit_imm_guard(&var_name)?;
                    let cast_op = if let Some(slot) = self.scope().find_local(&var_name) {
                        Some(Opcode::CastBangLocal(slot, base.to_string()))
                    } else if self.scopes.len() == 1 || self.resolve_upvalue(self.scopes.len() - 1, &var_name).is_none() {
                        self.global_index(&var_name).map(|pos| Opcode::CastBangGlobal(pos as u16, base.to_string()))
                    } else { None };
                    // A captured variable takes the generic bang path below.
                    if let Some(op) = cast_op {
                        self.emit(op);
                        // D13: the call itself gives nil; the result went into the variable.
                        self.emit(Opcode::Pop);
                        self.emit(Opcode::LoadNil);
                        return Ok(());
                    }
                }
                // Check if it's a known builtin call pattern.
                if let Some(_) = self.try_compile_builtin_call(name, args)? {
                    // Mutation-bang free call: name!(collection, ...) stores result back.
                    // e.g. update_at!(meta, "id", val) → CallBuiltin + Dup + StoreLocal(meta)
                    // I/O builtins use ! for side-effect signaling only — they return Nil and
                    // must NOT write back to the first argument variable.
                    const IO_BANG_NO_WRITEBACK: &[&str] = &[
                        "write_text!", "write_json!", "append_file!",
                        "create_dir!", "copy_file!", "delete_path!", "zip_dir!",
                    ];
                    let bare = name.trim_start_matches(':');
                    if bare.ends_with('!') && !IO_BANG_NO_WRITEBACK.contains(&bare) {
                        if let Some(target) = args.first() {
                            if is_lvalue(target) {
                                // The result goes back into the variable the
                                // first argument was loaded from, so the call
                                // may change that variable's collection in place.
                                if matches!(target, Expr::Ident(..)) {
                                    if let Some(Opcode::CallBuiltin(id, n)) = self.scope().bytecode.last().cloned() {
                                        if n as usize == args.len() {
                                            *self.scope_mut().bytecode.last_mut().unwrap() = Opcode::CallBuiltinMut(id, n);
                                        }
                                    }
                                }
                                // D13: the call itself gives nil, as in the
                                // interpreter; the result went into the target.
                                self.compile_store_from_stack(target)?;
                                self.emit(Opcode::LoadNil);
                            }
                        }
                    }
                } else {
                    let load_op = self.resolve_load(name).map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                }
            }

            Expr::Call(recv, method, args, _) => {
                // Special case: recv.vt / recv.valtype (0 args, Ident recv) → type lock query
                if (method == "vt" || method == "valtype") && args.is_empty() {
                    if let Expr::Ident(var_name, _) = recv.as_ref() {
                        if let Some(slot) = self.scope().find_local(var_name) {
                            self.emit(Opcode::GetTypeLockLocal(slot));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(var_name) {
                            self.emit(Opcode::GetTypeLockGlobal(pos as u16));
                            return Ok(());
                        }
                    }
                }
                // Special case: recv.cast_type (0 args, Ident recv) → cast member
                const CALL_CAST_TYPES: &[&str] = &[
                    "str", "string", "bool", "int", "uint", "float", "big", "pct",
                    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
                ];
                if CALL_CAST_TYPES.contains(&method.as_str()) && args.is_empty() {
                    if let Expr::Ident(var_name, _) = recv.as_ref() {
                        if let Some(slot) = self.scope().find_local(var_name) {
                            self.emit(Opcode::CastMemberLocal(slot, method.clone()));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(var_name) {
                            self.emit(Opcode::CastMemberGlobal(pos as u16, method.clone()));
                            return Ok(());
                        }
                    }
                }
                // recv.method(args): only treat as a free-function call if the method
                // name resolves to a local/global variable. Builtins must NOT shadow
                // user-defined class methods; instead, let CallMethod fall back to
                // builtins at runtime when the receiver is a non-object.
                let is_var = self.scope().find_local(method).is_some()
                    || (self.scopes.len() > 1 && {
                        let idx = self.scopes.len() - 1;
                        self.resolve_upvalue(idx, method).is_some()
                    })
                    || self.global_index(method).is_some();
                if is_var {
                    let load_op = self.resolve_load(method).map_err(|e| self.locate_err(e))?;
                    self.emit(load_op);
                    self.compile_expr(recv)?;
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call((args.len() + 1) as u8));
                } else {
                    // Always emit CallMethod; vm.rs will fall back to builtins
                    // for non-object receivers or missing class methods.
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
                    // `Shape::Circle({"r": 3})` builds a variant when `Shape` is a
                    // declared enum (known only at run time: enums can come from
                    // imports); otherwise `Ns::f(args)` is a module call.
                    let enum_branch = if ns_is_enum {
                        let en_idx = self.add_constant(Value::Str(ns.clone()));
                        self.emit(Opcode::LoadConst(en_idx));
                        self.emit(Opcode::CallBuiltin(BuiltinId::EnumDeclared, 1));
                        let to_call = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                        if args.len() == 1 {
                            let vn_idx = self.add_constant(Value::Str(name.clone()));
                            self.emit(Opcode::LoadConst(en_idx));
                            self.emit(Opcode::LoadConst(vn_idx));
                            self.compile_expr(&args[0])?;
                            self.emit(Opcode::CallBuiltin(BuiltinId::EnumVariantCall, 3));
                        } else {
                            self.emit_runtime_error(&format!(
                                "R0301: wrong-arity: enum variant '{ns}::{name}' takes 0 arguments or 1 map of fields, got {}",
                                args.len()))?;
                            self.emit(Opcode::LoadNil);
                        }
                        let to_end = self.scope_mut().emit_jump(Opcode::Jump);
                        self.scope_mut().patch_jump(to_call);
                        Some(to_end)
                    } else { None };
                    // Namespace call: try full_name as a compile-time global/local.
                    // Do NOT fall back to bare name — a bare local named `foo` must not
                    // shadow a `ns::foo` GLAM action when the local is a pre-hoisted nil.
                    let full_name = format!("{}::{}", ns, name);
                    match self.resolve_load(&full_name) {
                        Ok(load_op) => { self.emit(load_op); }
                        Err(_) => {
                            // Not known at compile time — emit a runtime named lookup.
                            // UseGlam registers actions under the qualified name ("ns::action").
                            let name_idx = self.add_constant(Value::Str(full_name.clone()));
                            self.emit(Opcode::LoadNamed(name_idx));
                        }
                    }
                    for arg in args { self.compile_expr(arg)?; }
                    self.emit(Opcode::Call(args.len() as u8));
                    if let Some(to_end) = enum_branch { self.scope_mut().patch_jump(to_end); }
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
                // Detect cast-bang: x.cast_type! — reads x, casts in-place, updates type_lock.
                if op == "!" {
                    const CAST_BANG_TYPES: &[&str] = &[
                        "str", "bool",
                        "i8", "i16", "i32", "i64",
                        "u8", "u16", "u32", "u64",
                        "f32", "f64", "float",
                        "big", "int", "uint", "pct",
                    ];
                    // x.cast_type! — recv.method() with 0 args where method is a cast type
                    let cast_bang: Option<(String, String)> = match inner.as_ref() {
                        Expr::Call(base, method_name, args, _) if args.is_empty() && CAST_BANG_TYPES.contains(&method_name.as_str()) => {
                            match base.as_ref() {
                                Expr::Ident(var_name, _) => Some((var_name.clone(), method_name.clone())),
                                _ => None,
                            }
                        }
                        _ => None,
                    };
                    if let Some((var_name, type_name)) = cast_bang {
                        self.emit_imm_guard(&var_name)?;
                        if let Some(slot) = self.scope().find_local(&var_name) {
                            self.emit(Opcode::CastBangLocal(slot, type_name));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(&var_name) {
                            self.emit(Opcode::CastBangGlobal(pos as u16, type_name));
                            return Ok(());
                        }
                        return Err(self.locate_err(GoblinError::UndefinedVariable { name: var_name }));
                    }

                    // Mutation-bang dot-call: var.method! → compute var.method, write back to var.
                    let mutation_bang: Option<(String, String)> = match inner.as_ref() {
                        Expr::Call(base, method_name, _, _) if !CAST_BANG_TYPES.contains(&method_name.as_str()) => {
                            match base.as_ref() {
                                Expr::Ident(var_name, _) => Some((var_name.clone(), method_name.clone())),
                                _ => None,
                            }
                        }
                        _ => None,
                    };
                    if let Some((var_name, _method_name)) = mutation_bang {
                        self.emit_imm_guard(&var_name)?;
                        self.compile_expr(inner)?; // pushes method result
                        self.emit(Opcode::Dup);    // duplicate: one to store, one to leave as value
                        if let Some(slot) = self.scope().find_local(&var_name) {
                            self.emit(Opcode::StoreLocal(slot));
                            return Ok(());
                        }
                        if let Some(pos) = self.global_index(&var_name) {
                            self.emit(Opcode::StoreGlobal(pos as u16));
                            return Ok(());
                        }
                        return Err(self.locate_err(GoblinError::UndefinedVariable { name: var_name }));
                    }
                }
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
                        // x++ / x-- store x ± 1 back into x and evaluate to the new value.
                        let one = self.scope_mut().add_constant(Value::Int(1));
                        self.emit(Opcode::LoadConst(one));
                        if op == "++" { self.emit(Opcode::Add); } else { self.emit(Opcode::Sub); }
                        if is_lvalue(inner) {
                            if let Expr::Ident(n, _) = inner.as_ref() { self.emit_imm_guard(n)?; }
                            self.emit(Opcode::Dup);
                            self.compile_store_from_stack(inner)?;
                        }
                    }
                    "^" => { self.emit(Opcode::CallBuiltin(BuiltinId::PostfixCeil, 1)); }
                    "_" => { self.emit(Opcode::CallBuiltin(BuiltinId::PostfixFloor, 1)); }
                    "!" => { self.emit(Opcode::CallBuiltin(BuiltinId::PostfixFactorial, 1)); }
                    "?" => {
                        self.emit(Opcode::LoadNil);
                        self.emit(Opcode::Ne);
                    }
                    "*>>" | "*>>:show_ids" => {
                        self.emit(if op == "*>>" { Opcode::LoadFalse } else { Opcode::LoadTrue });
                        self.emit(Opcode::CallBuiltin(BuiltinId::PostfixFieldsMap, 2));
                    }
                    other => {
                        return Err(self.locate_err(GoblinError::Runtime(format!("unknown postfix operator '{other}'"))));
                    }
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
                // A block is a scope for `|` bindings (D3), as in the interpreter.
                self.scope_mut().begin_block();
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
                self.scope_mut().end_block();
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
            self.emit_obj_rebind_guard(&var_name)?;
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
            // D8: `and` / `or` give a bool, whatever the operands.
            self.emit(Opcode::Not);
            self.emit(Opcode::Not);
            return Ok(());
        }
        if op == "||" || op == "or" {
            self.compile_expr(lhs)?;
            self.emit(Opcode::Dup);
            let skip = self.scope_mut().emit_jump(Opcode::JumpIfTrue);
            self.emit(Opcode::Pop);
            self.compile_expr(rhs)?;
            self.scope_mut().patch_jump(skip);
            // D8: `and` / `or` give a bool, whatever the operands.
            self.emit(Opcode::Not);
            self.emit(Opcode::Not);
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
            "=="         => Opcode::Eq,
            "!=" | "/="  => Opcode::Ne,
            // Strict (in)equality: no int/float coercion.
            "===" => { self.emit(Opcode::CallBuiltin(BuiltinId::StrictEq, 2)); return Ok(()); }
            "!==" => {
                self.emit(Opcode::CallBuiltin(BuiltinId::StrictEq, 2));
                self.emit(Opcode::Not);
                return Ok(());
            }
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

    /// Expression-form `judge`: arms are conditions and the first true one
    /// gives the value; `else` applies only when none matched, wherever it
    /// sits; with no match and no `else` the value is nil. `judge return <h>`
    /// makes `h` the value of arms written without one. The parser has
    /// already expanded `using` into each condition. (Matches the
    /// interpreter's `Expr::Judge`.)
    fn compile_judge_expr(
        &mut self,
        _using: Option<&Expr>,
        header: Option<&Expr>,
        arms: &[JudgeArm],
        all: bool,
    ) -> Result<(), GoblinError> {
        if all {
            return self.compile_judge_all_expr(header, arms);
        }
        let mut end_jumps: Vec<usize> = Vec::new();
        for arm in arms.iter() {
            let Some(cond) = &arm.condition else { continue };
            self.compile_expr(cond)?;
            let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
            self.compile_judge_arm_value(arm, header)?;
            end_jumps.push(self.scope_mut().emit_jump(Opcode::Jump));
            self.scope_mut().patch_jump(skip);
        }
        match arms.iter().find(|a| a.condition.is_none()) {
            Some(else_arm) => self.compile_judge_arm_value(else_arm, header)?,
            None => { self.emit(Opcode::LoadNil); }
        }
        for j in end_jumps { self.scope_mut().patch_jump(j); }
        Ok(())
    }

    /// An arm's value: its own expression, else the `judge return` header, else nil.
    fn compile_judge_arm_value(&mut self, arm: &JudgeArm, header: Option<&Expr>) -> Result<(), GoblinError> {
        match (&arm.value, header) {
            (Some(v), _) => self.compile_expr(v),
            (None, Some(h)) => self.compile_expr(h),
            (None, None) => { self.emit(Opcode::LoadNil); Ok(()) }
        }
    }

    fn compile_judge_all_expr(
        &mut self,
        header: Option<&Expr>,
        arms: &[JudgeArm],
    ) -> Result<(), GoblinError> {
        // Accumulator array in a hidden local.
        self.emit(Opcode::MakeArray(0));
        let acc_slot = self.scope_mut().declare_local("__judge_all_acc__");
        self.emit(Opcode::StoreLocal(acc_slot));

        // Separate else arm from non-else arms.
        let (cond_arms, else_arms): (Vec<_>, Vec<_>) = arms.iter().partition(|a| a.condition.is_some());

        for arm in &cond_arms {
            let cond = arm.condition.as_ref().unwrap();
            self.compile_expr(cond)?;
            let skip = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
            // acc = acc.put_last(value)
            self.emit(Opcode::LoadLocal(acc_slot));
            self.compile_judge_arm_value(arm, header)?;
            self.emit(Opcode::CallBuiltin(BuiltinId::PutLast, 2));
            self.emit(Opcode::StoreLocal(acc_slot));
            self.scope_mut().patch_jump(skip);
        }

        // If accumulator is empty, evaluate else arm; otherwise push accumulator.
        self.emit(Opcode::LoadLocal(acc_slot));
        self.emit(Opcode::CallBuiltin(BuiltinId::IsEmpty, 1));
        let not_empty_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
        // Empty: the else arm's value in a one-element list, else an empty list.
        if let Some(else_arm) = else_arms.first() {
            self.emit(Opcode::LoadLocal(acc_slot));
            self.compile_judge_arm_value(else_arm, header)?;
            self.emit(Opcode::CallBuiltin(BuiltinId::PutLast, 2));
        } else {
            self.emit(Opcode::LoadLocal(acc_slot));
        }
        let end_jump = self.scope_mut().emit_jump(Opcode::Jump);
        self.scope_mut().patch_jump(not_empty_jump);
        // Not empty: push accumulator
        self.emit(Opcode::LoadLocal(acc_slot));
        self.scope_mut().patch_jump(end_jump);
        Ok(())
    }

    // ── Nested action / closure compilation ───────────────────────────────────

    /// Stores the value on top of the stack into an lvalue: a variable, or a
    /// path of indexes and `>>` fields below one (rebuilt with UpdatePath,
    /// like `update!`). Consumes the value.
    /// Emits CheckPath for the lvalue path `target` (root and keys already on
    /// the stack), so a write through the wrong bracket errors.
    fn emit_check_path(&mut self, target: &Expr) -> Result<(), GoblinError> {
        let mut info: Vec<Value> = Vec::new();
        let mut cur = target;
        loop {
            match cur {
                Expr::Index(base, _, _) => { info.push(Value::Str(format!("[{}", source_text(base)))); cur = base.as_ref(); }
                Expr::IndexMap(base, _, _) => { info.push(Value::Str(format!("{{{}", source_text(base)))); cur = base.as_ref(); }
                Expr::Member(base, _, _) => { info.push(Value::Str(".".into())); cur = base.as_ref(); }
                _ => break,
            }
        }
        if info.is_empty() { return Ok(()); }
        info.reverse();
        let n = info.len() as u8;
        let idx = self.add_constant(Value::Array(info));
        self.emit(Opcode::CheckPath(n, idx));
        Ok(())
    }

    fn compile_store_from_stack(&mut self, target: &Expr) -> Result<(), GoblinError> {
        if let Expr::Ident(var_name, _) = target {
            let op = self.resolve_store(var_name)
                .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: var_name.clone() }))?;
            self.emit(op);
            return Ok(());
        }
        let mut segs: Vec<(&Expr, bool)> = Vec::new();
        let mut cur = target;
        let root_name = loop {
            match cur {
                Expr::Index(base, key, _) | Expr::IndexMap(base, key, _) => { segs.push((key.as_ref(), false)); cur = base.as_ref(); }
                Expr::Member(base, _, _) => { segs.push((cur, true)); cur = base.as_ref(); }
                Expr::Ident(name, _) => break name.clone(),
                _ => unreachable!("is_lvalue checked the target"),
            }
        };
        segs.reverse();
        let tmp = self.scope_mut().declare_local("__store_tmp__");
        self.emit(Opcode::StoreLocal(tmp));
        let load_op = self.resolve_load(&root_name).map_err(|e| self.locate_err(e))?;
        self.emit(load_op);
        let mut mask: u16 = 0;
        for (i, (seg, is_field)) in segs.iter().enumerate() {
            if *is_field {
                mask |= 1 << i;
                let Expr::Member(_, field, _) = seg else { unreachable!() };
                let kidx = self.add_constant(Value::Str(field.clone()));
                self.emit(Opcode::LoadConst(kidx));
            } else {
                self.compile_expr(seg)?;
            }
        }
        self.emit_check_path(target)?;
        self.emit(Opcode::LoadLocal(tmp));
        self.emit(Opcode::UpdatePathMut(segs.len() as u8, mask));
        let store_op = self.resolve_store(&root_name)
            .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: root_name.clone() }))?;
        self.emit(store_op);
        Ok(())
    }

    /// `name [= v` with the value on the stack: a new local in this block,
    /// R0111 when this block already bound the name.
    fn compile_shadow_store(&mut self, name: &String, is_imm: bool, lock_type: &Option<String>) -> Result<(), GoblinError> {
        // x[= expr — shadow: always declare a new slot, unless this
        // block already bound the name (R0111, as the interpreter).
        let depth = self.scope().block_marks.len();
        let dup = match self.scope().find_local_entry(name) {
            Some((i, _)) => {
                let meta = self.scope().local_meta[i];
                meta.bound && meta.depth == depth
            }
            None => self.scopes.len() == 1 && depth == 0
                && self.bound_globals.contains(name.as_str()),
        };
        if dup {
            return self.emit_runtime_error(&format!(
                "duplicate-local: '{name}' is already declared in this block"));
        }
        let slot = self.scope_mut().declare_local(name);
        if is_imm {
            let (i, _) = self.scope().find_local_entry(name).unwrap();
            self.scope_mut().local_meta[i].imm = true;
        }
        if let Some(ref lock) = lock_type {
            self.emit(Opcode::StoreLockLocal(slot, lock.clone()));
        } else {
            self.emit(Opcode::StoreLocal(slot));
        }
        Ok(())
    }

    /// `name | v` (not in the REPL's top scope) with the value on the stack:
    /// binds a module global or a local, raising D3/D9 errors for names
    /// already bound. Shared by plain and tuple binds.
    fn compile_tether_store(&mut self, name: &String, is_imm: bool, lock_type: &Option<String>) -> Result<(), GoblinError> {
        // Top-level binding of a module global (see compile_module).
        let depth = self.scope().block_marks.len();
        let local = self.scope().find_local_entry(name)
            .map(|(i, slot)| (slot, self.scope().local_meta[i]));
        if self.scopes.len() == 1 && local.is_none() {
            if let Some(pos) = self.global_index(name) {
                if self.bound_globals.contains(name.as_str()) {
                    // D9 at the top level; D3 inside a top-level block.
                    return self.emit_bind_error(name, depth > 0);
                }
                if depth == 0 {
                    self.bound_globals.insert(name.clone());
                    if is_imm { self.imm_globals.insert(name.clone()); }
                    let name_idx = self.add_constant(Value::Str(name.clone()));
                    self.emit(Opcode::RegisterAction(name_idx));
                    if let Some(ref lock) = lock_type {
                        self.emit(Opcode::StoreLockGlobal(pos as u16, lock.clone()));
                    } else {
                        self.emit(Opcode::StoreGlobal(pos as u16));
                    }
                    return Ok(());
                }
                // Inside a top-level block before the global is
                // bound: a block-local, as in the interpreter.
            }
        }
        // x | expr — initial binding. A hoisted slot at this depth
        // is reused; a binding already made in this block is a
        // redeclaration (D9), and one made in an enclosing block
        // of this action must be updated with |= or shadowed
        // with [= (D3).
        let slot = match local {
            Some((_, meta)) if meta.bound && meta.depth == depth => {
                return self.emit_bind_error(name, false);
            }
            Some((_, meta)) if meta.bound => {
                return self.emit_bind_error(name, true);
            }
            Some((slot, meta)) if meta.depth == depth => {
                let (i, _) = self.scope().find_local_entry(name).unwrap();
                self.scope_mut().local_meta[i].bound = true;
                slot
            }
            _ => self.scopes.last_mut().unwrap().declare_local(name),
        };
        if is_imm {
            let (i, _) = self.scope().find_local_entry(name).unwrap();
            self.scope_mut().local_meta[i].imm = true;
        }
        // At top-level module scope, register in named_values so
        // other modules can access this value via LoadNamed after import.
        if self.scopes.len() == 1 && depth == 0 {
            let name_idx = self.add_constant(Value::Str(name.clone()));
            self.emit(Opcode::RegisterAction(name_idx));
        }
        if let Some(ref lock) = lock_type {
            self.emit(Opcode::StoreLockLocal(slot, lock.clone()));
        } else {
            self.emit(Opcode::StoreLocal(slot));
        }
        Ok(())
    }

    /// Before `name |= v` (v on the stack): an object variable cannot be
    /// rebound to a non-object value (R1300, as the interpreter).
    fn emit_obj_rebind_guard(&mut self, name: &str) -> Result<(), GoblinError> {
        let load_op = self.resolve_load(name).map_err(|e| self.locate_err(e))?;
        self.emit(load_op);
        let idx = self.add_constant(Value::Str(name.to_string()));
        self.emit(Opcode::GuardObjRebind(idx));
        Ok(())
    }

    /// Compiles to an error raised when execution reaches this point, so
    /// output before it still happens, as with the interpreter's runtime
    /// errors.
    fn emit_runtime_error(&mut self, msg: &str) -> Result<(), GoblinError> {
        let idx = self.add_constant(Value::Str(msg.to_string()));
        self.emit(Opcode::LoadConst(idx));
        self.emit(Opcode::CallBuiltin(BuiltinId::Panic, 1));
        self.emit(Opcode::Pop);
        Ok(())
    }

    /// `x | v` on a name already bound: in the same block it is a
    /// redeclaration (D9, R0111); from an enclosing block it is an error
    /// that points at `|=` and `[=` (D3).
    fn emit_bind_error(&mut self, name: &str, outer: bool) -> Result<(), GoblinError> {
        let msg = if outer {
            format!("'{name}' is already bound in an enclosing block; use '{name} |= …' to update it or '{name} [= …' to shadow it")
        } else {
            format!("duplicate-local: '{name}' is already bound in this scope; use '{name} |= …' to update it")
        };
        self.emit_runtime_error(&msg)
    }

    /// Parameter defaults (`b | 10`): a caller may omit trailing defaulted
    /// arguments, and the default is evaluated in the callee when it does,
    /// as the interpreter binds them.
    fn compile_param_defaults(&mut self, params: &[goblin_ast::Param]) -> Result<(), GoblinError> {
        let required = params.iter().rposition(|p| p.default.is_none()).map_or(0, |i| i + 1);
        self.scope_mut().required_params = required;
        for (i, p) in params.iter().enumerate() {
            let Some(default) = &p.default else { continue };
            let skip = self.emit(Opcode::JumpIfLocalSet(i as u8, 0));
            self.compile_expr(default)?;
            self.emit(Opcode::StoreLocal(i as u8));
            self.scope_mut().patch_jump(skip);
        }
        Ok(())
    }

    fn compile_action_decl(&mut self, action: &ActionDecl) -> Result<(), GoblinError> {
        let param_names: Vec<String> = action.params.iter().map(|p| p.name.clone()).collect();
        let n_params = param_names.len();

        self.push_scope(&action.name, n_params);
        {
            let scope = self.scopes.last_mut().unwrap();
            scope.declare_params(&param_names);
        }
        self.compile_param_defaults(&action.params)?;

        match &action.body {
            ActionBody::Block(stmts) => {
                // Two-pass: pre-register nested action names as globals before compiling
                // statements. This lets sibling nested actions reference each other
                // (forward refs) the same way the interpreter can via sess.actions.
                let mut nested_action_names: Vec<String> = Vec::new();
                collect_action_names(stmts, &mut nested_action_names);
                for name in &nested_action_names {
                    self.declare_global(name);
                }

                let mut hoisted: Vec<String> = Vec::new();
                collect_bind_names(stmts, &mut hoisted);
                for name in hoisted.iter().filter(|n| !param_names.contains(n)) {
                    let slot = self.scope_mut().declare_hoisted(name);
                    self.emit(Opcode::LoadNil);
                    self.emit(Opcode::StoreLocal(slot));
                }
                // Last statement's value is the implicit return (matches interpreter).
                if !stmts.is_empty() {
                    let (body, last) = stmts.split_at(stmts.len() - 1);
                    for s in body { self.compile_stmt(s)?; }
                    match &last[0] {
                        Stmt::Expr(e) => { self.compile_expr(e)?; }
                        Stmt::Bind(b) => {
                            self.compile_stmt(&last[0])?;
                            self.compile_expr(&b.expr)?;
                        }
                        Stmt::TupleBind(b) => {
                            self.compile_stmt(&last[0])?;
                            self.compile_expr(&b.expr)?;
                        }
                        other => {
                            self.compile_stmt(other)?;
                            self.emit(Opcode::LoadUnit);
                        }
                    }
                } else {
                    self.emit(Opcode::LoadUnit);
                }
                let scope = self.scopes.last_mut().unwrap();
                scope.emit(Opcode::Return);
            }
            ActionBody::Expr(e) => {
                self.compile_expr(e)?;
                self.scope_mut().emit(Opcode::Return);
            }
        }

        let mut func_obj = self.pop_scope();
        if self.scopes.len() == 1 {
            func_obj.owner_glam = self.glam_namespace.clone();
        }
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
            // is_bound_name("x") with a literal name: whether `x` is bound where
            // the call is written (local, captured or module global).
            "is_bound_name" if args.len() == 1 && matches!(&args[0], Expr::Str(..)) && !self.user_actions.contains(bare) => {
                let Expr::Str(var, _) = &args[0] else { unreachable!() };
                let bound = match self.scope().find_local_entry(var) {
                    Some((i, _)) => self.scope().local_meta[i].bound,
                    None => {
                        (self.scopes.len() > 1 && self.resolve_upvalue(self.scopes.len() - 1, var).is_some())
                            || (self.global_index(var).is_some()
                                && (self.scopes.len() > 1 || self.bound_globals.contains(var.as_str())))
                    }
                };
                let idx = self.add_constant(Value::Bool(bound));
                self.emit(Opcode::LoadConst(idx));
                Ok(true)
            }
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
                if args.len() != 2 {
                    // `:while(...)` called by name with the wrong arguments (interp R0301).
                    self.emit_runtime_error(&format!("wrong number of arguments to while (expected 2, got {})", args.len()))?;
                    self.emit(Opcode::LoadNil); // keep the stack shape; never reached
                    return Ok(true);
                }
                self.compile_expr(&args[0])?;
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // Loop context so `skip`/`stop` in the body target this loop.
                self.loop_stack.push(LoopCtx::default());
                self.compile_expr(&args[1])?;
                self.emit(Opcode::Pop); // discard body result
                // Jump back to loop start
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                self.scope_mut().patch_jump(exit_jump);
                let exit_ip = self.scope_mut().bytecode.len();
                let ctx = self.loop_stack.pop().unwrap();
                for idx in ctx.break_patches { self.scope_mut().patch_jump_to(idx, exit_ip); }
                for idx in ctx.continue_patches { self.scope_mut().patch_jump_to(idx, loop_start); }
                self.emit(Opcode::LoadNil);
                Ok(true)
            }

            // repeat(n_or_array, body [, as_name])
            // Supports: count loop (Int), array iteration (Array), infinite loop (Nil), cond loop (Bool)
            "repeat" => {
                if !(2..=4).contains(&args.len()) {
                    // `:repeat(...)` called by name with the wrong arguments (interp R0301).
                    self.emit_runtime_error(&format!("wrong number of arguments to repeat (expected 2-4, got {})", args.len()))?;
                    self.emit(Opcode::LoadNil); // keep the stack shape; never reached
                    return Ok(true);
                }
                // Compile limit/collection, store in hidden local.
                // Special case: bare Ident may be a class name — use QueryByIdent to handle both.
                if let Expr::Ident(name, _) = &args[0] {
                    // Check if name resolves as a local/global variable
                    let has_local = self.scope().find_local(name).is_some()
                        || self.global_index(name.as_str()).is_some();
                    if !has_local {
                        // Emit as class/overlay query by name string
                        let name_str = Value::Str(name.clone());
                        let cidx = self.add_constant(name_str);
                        self.emit(Opcode::LoadConst(cidx));
                        self.emit(Opcode::CallBuiltin(BuiltinId::QueryByIdent, 1));
                    } else {
                        self.compile_expr(&args[0])?;
                    }
                } else {
                    self.compile_expr(&args[0])?;
                }
                // RepeatPrep(n) → [mode, iterable, limit]; mode 0 = count (int),
                // 1 = items (array/collection), 2 = map (iterable of [k, v]),
                // 3 = forever (nil), 4 = condition (bool; re-evaluated each pass),
                // matching the interpreter's `repeat` dispatch.
                // Loop variables (it, key, val, idx, `as` names) are scoped to the loop.
                self.scope_mut().begin_block();
                self.emit(Opcode::CallBuiltin(BuiltinId::RepeatPrep, 1));
                let prep_slot = self.scope_mut().declare_local("__repeat_prep__");
                self.emit(Opcode::StoreLocal(prep_slot));
                let get = |c: &mut Self, slot: u8, i: i64| {
                    let k = c.scope_mut().add_constant(Value::Int(i));
                    c.emit(Opcode::LoadLocal(slot));
                    c.emit(Opcode::LoadConst(k));
                    c.emit(Opcode::GetIndex);
                };
                let mode_slot = self.scope_mut().declare_local("__repeat_mode__");
                get(self, prep_slot, 0);
                self.emit(Opcode::StoreLocal(mode_slot));
                let iter_slot = self.scope_mut().declare_local("__repeat_iter__");
                get(self, prep_slot, 1);
                self.emit(Opcode::StoreLocal(iter_slot));
                let limit_slot = self.scope_mut().declare_local("__repeat_limit__");
                get(self, prep_slot, 2);
                self.emit(Opcode::StoreLocal(limit_slot));

                let zero = self.scope_mut().add_constant(Value::Int(0));
                let one = self.scope_mut().add_constant(Value::Int(1));
                let two = self.scope_mut().add_constant(Value::Int(2));
                let four = self.scope_mut().add_constant(Value::Int(4));
                self.emit(Opcode::LoadConst(zero));
                let counter_slot = self.scope_mut().declare_local("__repeat_i__");
                self.emit(Opcode::StoreLocal(counter_slot));

                let name_arg = |e: Option<&Expr>| match e {
                    Some(Expr::Str(s, _)) | Some(Expr::Ident(s, _)) if !s.is_empty() => Some(s.clone()),
                    _ => None,
                };
                let as_name = name_arg(args.get(2));
                let val_name = name_arg(args.get(3));
                let item_slot = self.scope_mut().declare_local(as_name.as_deref().unwrap_or("it"));
                // With an `as` name, items mode and map-key mode bind the same name,
                // so they share one slot.
                let key_slot = if as_name.is_some() {
                    item_slot
                } else {
                    self.scope_mut().declare_local("key")
                };
                let val_slot = self.scope_mut().declare_local(val_name.as_deref().unwrap_or("val"));
                let idx_slot = self.scope_mut().declare_local("idx");
                self.emit(Opcode::LoadConst(zero));
                self.emit(Opcode::StoreLocal(idx_slot));

                // LOOP START: counter < limit
                let loop_start = self.scope_mut().bytecode.len();
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::LoadLocal(limit_slot));
                self.emit(Opcode::Lt);
                let exit_jump = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                // condition mode: re-evaluate the header expression
                self.emit(Opcode::LoadLocal(mode_slot));
                self.emit(Opcode::LoadConst(four));
                self.emit(Opcode::Eq);
                let not_cond = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                self.compile_expr(&args[0])?;
                let cond_exit = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                self.scope_mut().patch_jump(not_cond);
                // items mode: bind the element
                self.emit(Opcode::LoadLocal(mode_slot));
                self.emit(Opcode::LoadConst(one));
                self.emit(Opcode::Eq);
                let not_items = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                self.emit(Opcode::LoadLocal(iter_slot));
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::GetIndex);
                self.emit(Opcode::StoreLocal(item_slot));
                self.scope_mut().patch_jump(not_items);
                // map mode: bind key and value
                self.emit(Opcode::LoadLocal(mode_slot));
                self.emit(Opcode::LoadConst(two));
                self.emit(Opcode::Eq);
                let not_map = self.scope_mut().emit_jump(Opcode::JumpIfFalse);
                for (slot, k) in [(key_slot, zero), (val_slot, one)] {
                    self.emit(Opcode::LoadLocal(iter_slot));
                    self.emit(Opcode::LoadLocal(counter_slot));
                    self.emit(Opcode::GetIndex);
                    self.emit(Opcode::LoadConst(k));
                    self.emit(Opcode::GetIndex);
                    self.emit(Opcode::StoreLocal(slot));
                }
                self.scope_mut().patch_jump(not_map);
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::StoreLocal(idx_slot));

                self.loop_stack.push(LoopCtx::default());
                self.compile_expr(&args[1])?;
                self.emit(Opcode::Pop);
                let increment_ip = self.scope_mut().bytecode.len();
                self.emit(Opcode::LoadLocal(counter_slot));
                self.emit(Opcode::LoadConst(one));
                self.emit(Opcode::Add);
                self.emit(Opcode::StoreLocal(counter_slot));
                let cur = self.scope_mut().bytecode.len();
                let offset = -(((cur - loop_start) as i16) + 1);
                self.emit(Opcode::Jump(offset));
                let exit_ip = self.scope_mut().bytecode.len();
                self.scope_mut().patch_jump(exit_jump);
                self.scope_mut().patch_jump(cond_exit);
                let ctx = self.loop_stack.pop().unwrap();
                for idx in ctx.break_patches { self.scope_mut().patch_jump_to(idx, exit_ip); }
                for idx in ctx.continue_patches { self.scope_mut().patch_jump_to(idx, increment_ip); }
                self.scope_mut().end_block();
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
                // The loop variable is scoped to the loop (D3).
                self.scope_mut().begin_block();
                // Compile iterable; coerce maps/strings/nil to sequential array
                self.compile_expr(&args[1])?;
                self.emit(Opcode::CallBuiltin(BuiltinId::ToForIter, 1));
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
                self.scope_mut().end_block();
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

            // skip / stop — loop control
            "skip" | "stop" => {
                // Outside a loop of the current action this is a runtime error
                // (R0405), raised when the statement is reached.
                if self.loop_stack.len() <= self.loop_floors.last().copied().unwrap_or(0) {
                    let msg = format!("R0405: '{name}' used outside of a loop");
                    let idx = self.add_constant(Value::Str(msg));
                    self.emit(Opcode::LoadConst(idx));
                    self.emit(Opcode::CallBuiltin(BuiltinId::Panic, 1));
                    return Ok(true);
                }
                let patch_idx = self.scope_mut().bytecode.len();
                self.emit(Opcode::Jump(0)); // patched when the loop compiles continue/exit
                let ctx = self.loop_stack.last_mut().unwrap();
                if name == "skip" { ctx.continue_patches.push(patch_idx); } else { ctx.break_patches.push(patch_idx); }
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
                if args.len() != 2 {
                    // `:collect(...)` called by name with the wrong arguments (interp R0301).
                    self.emit_runtime_error(&format!("wrong number of arguments to collect (expected 2, got {})", args.len()))?;
                    self.emit(Opcode::LoadNil); // keep the stack shape; never reached
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
                // evaluate body, then append to accumulator in-place
                self.compile_expr(&args[1])?;
                self.emit(Opcode::ArrayPushToLocal(arr_slot));
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
                    // `:attempt()` called by name with no arguments (interp R0301).
                    self.emit_runtime_error("attempt requires at least 1 argument")?;
                    self.emit(Opcode::LoadNil); // keep the stack shape; never reached
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
                    // No rescue: run the ensure block, then raise the error again.
                    let err_slot = self.scope_mut().declare_local("__attempt_error__");
                    self.emit(Opcode::StoreLocal(err_slot));
                    if args.len() > 2 {
                        let ensure_body = args[2].clone();
                        self.compile_expr(&ensure_body)?;
                        self.emit(Opcode::Pop);
                    }
                    self.emit(Opcode::LoadLocal(err_slot));
                    self.emit(Opcode::CallBuiltin(BuiltinId::Panic, 1));
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

            // update!(target, new_val) — lvalue whole-replacement
            // Handles: update!(var, v), update!(var[idx], v), update!(var{key}, v)
            "update!" => {
                if args.len() != 2 {
                    return Err(GoblinError::CompileError {
                        message: "'update!' takes exactly 2 arguments".into(),
                        span_debug: String::new(),
                    });
                }
                match &args[0] {
                    Expr::Ident(var_name, _) => {
                        // update!(var, new_val) → compile new_val, store to var
                        self.compile_expr(&args[1])?;
                        let op = self.resolve_store(var_name)
                            .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: var_name.clone() }))?;
                        self.emit(op);
                        self.emit(Opcode::LoadNil);
                    }
                    Expr::Index(..) | Expr::IndexMap(..) | Expr::Member(..) => {
                        // update!(root[k1] >> f [k3]…, new_val): walk the lvalue path
                        // down to its root variable, push root, each key (in source
                        // order), then the new value, and let UpdatePath rebuild the
                        // chain. The result is stored back into the root variable.
                        let mut segs: Vec<(&Expr, bool)> = Vec::new();
                        let mut cur = &args[0];
                        let root_name = loop {
                            match cur {
                                Expr::Index(base, key, _) | Expr::IndexMap(base, key, _) => {
                                    segs.push((key.as_ref(), false));
                                    cur = base.as_ref();
                                }
                                Expr::Member(base, _, _) => {
                                    segs.push((cur, true));
                                    cur = base.as_ref();
                                }
                                Expr::Ident(name, _) => break name.clone(),
                                _ => {
                                    return Err(GoblinError::CompileError {
                                        message: "'update!' target must start with a variable".into(),
                                        span_debug: String::new(),
                                    });
                                }
                            }
                        };
                        segs.reverse();
                        if segs.len() > 16 {
                            return Err(GoblinError::CompileError {
                                message: "'update!' target path is too deep (max 16 segments)".into(),
                                span_debug: String::new(),
                            });
                        }
                        let load_op = self.resolve_load(&root_name)
                            .map_err(|e| self.locate_err(e))?;
                        self.emit(load_op);
                        let mut mask: u16 = 0;
                        for (i, (seg, is_field)) in segs.iter().enumerate() {
                            if *is_field {
                                mask |= 1 << i;
                                let Expr::Member(_, field, _) = seg else { unreachable!() };
                                let kidx = self.add_constant(Value::Str(field.clone()));
                                self.emit(Opcode::LoadConst(kidx));
                            } else {
                                self.compile_expr(seg)?;
                            }
                        }
                        self.emit_check_path(&args[0])?;
                        self.compile_expr(&args[1])?;
                        self.emit(Opcode::UpdatePathMut(segs.len() as u8, mask));
                        let store_op = self.resolve_store(&root_name)
                            .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: root_name.clone() }))?;
                        self.emit(store_op);
                        self.emit(Opcode::LoadNil);
                    }
                    _ => {
                        return Err(GoblinError::CompileError {
                            message: "'update!' target must be a variable or index expression".into(),
                            span_debug: String::new(),
                        });
                    }
                }
                Ok(true)
            }

            // reap!(arr [, count]) — atomically remove+return random items.
            // Interpreter bang form: (arr, count?) NOT the {src,count} config-map.
            // Compiles to: ReapBang → [remaining, reaped]; store remaining back, return reaped.
            "reap!" => {
                if args.is_empty() || args.len() > 2 {
                    return Err(GoblinError::CompileError {
                        message: "'reap!' takes 1 or 2 arguments: (arr [, count])".into(),
                        span_debug: String::new(),
                    });
                }
                let arr_name = match &args[0] {
                    Expr::Ident(n, _) => n.clone(),
                    _ => return Err(GoblinError::CompileError {
                        message: "'reap!' first argument must be a plain variable".into(),
                        span_debug: String::new(),
                    }),
                };
                // compile ReapBang(arr [, count]) → [remaining, reaped]
                let load_arr = self.resolve_load(&arr_name).map_err(|e| self.locate_err(e))?;
                self.emit(load_arr.clone());
                if args.len() == 2 { self.compile_expr(&args[1])?; }
                self.emit(Opcode::CallBuiltin(BuiltinId::ReapBang, args.len() as u8));
                // Store result temporarily, then extract remaining and reaped
                let tmp_slot = self.scope_mut().declare_local("__reap_tmp__");
                self.emit(Opcode::StoreLocal(tmp_slot));
                // remaining = result[0] → store back to arr
                self.emit(Opcode::LoadLocal(tmp_slot));
                let zero_idx = self.add_constant(Value::Int(0));
                self.emit(Opcode::LoadConst(zero_idx));
                self.emit(Opcode::CallBuiltin(BuiltinId::GetAt, 2));
                let store_arr = self.resolve_store(&arr_name)
                    .ok_or_else(|| self.locate_err(GoblinError::UndefinedVariable { name: arr_name.clone() }))?;
                self.emit(store_arr);
                // reaped = result[1] → leave on stack as expression value
                self.emit(Opcode::LoadLocal(tmp_slot));
                let one_idx = self.add_constant(Value::Int(1));
                self.emit(Opcode::LoadConst(one_idx));
                self.emit(Opcode::CallBuiltin(BuiltinId::GetAt, 2));
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
        let bare = name.trim_start_matches(':');
        // `name!(x, …)` is `name(x, …)` written back into x (the caller does
        // the write-back), as in the interpreter, which strips the `!`.
        let bid = match builtin_by_name(bare).or_else(|| builtin_by_name(name))
            .or_else(|| bare.strip_suffix('!').and_then(builtin_by_name)) {
            Some(b) => b,
            None    => return Ok(None),
        };
        if (bare == "objects" || bare == "overlays") && args.len() == 1 {
            self.compile_predicate_lambda(&args[0])?;
            self.emit(Opcode::CallBuiltin(bid, 1));
            return Ok(Some(()));
        }
        for arg in args { self.compile_expr(arg)?; }
        self.emit(Opcode::CallBuiltin(bid, args.len() as u8));
        Ok(Some(()))
    }

    /// Compile `expr` as an implicit `fn(it) { expr }` closure on the stack.
    fn compile_predicate_lambda(&mut self, expr: &Expr) -> Result<(), GoblinError> {
        self.push_scope("__pred__", 1);
        self.scope_mut().declare_params(&["it".to_string()]);
        self.scope_mut().pred_fields = true;
        self.compile_expr(expr)?;
        self.scope_mut().emit(Opcode::Return);
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
        ":gc_mode"     | "gc_mode"      => BuiltinId::GcMode,
        ":stash_count" | "stash_count"  => BuiltinId::StashCount,
        ":tether_count"| "tether_count" => BuiltinId::TetherCount,
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
        "str" | "string"  => BuiltinId::ToStr,
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
        "starts_with"                   => BuiltinId::StartsWith,
        "ends_with"                     => BuiltinId::EndsWith,
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
        "reap_where"                    => BuiltinId::ReapWhere,
        // New Position×Operation matrix
        "get_first"    | ":get_first"    => BuiltinId::GetFirst,
        "get_last"     | ":get_last"     => BuiltinId::GetLast,
        "get_at"       | ":get_at"       => BuiltinId::GetAt,
        "get_where"    | ":get_where"    => BuiltinId::GetWhere,
        "get_all"      | ":get_all"      => BuiltinId::GetAll,
        "get_matching" | ":get_matching" => BuiltinId::GetMatching,
        "get_between"  | ":get_between"  => BuiltinId::GetBetween,
        "get_random"   | ":get_random"   => BuiltinId::GetRandom,
        "put_matching" | ":put_matching" => BuiltinId::PutMatching,
        "put_between"  | ":put_between"  => BuiltinId::PutBetween,
        "put_random"   | ":put_random"   => BuiltinId::PutRandom,
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
        "reverse"                       => BuiltinId::Reverse,
        "reverse_chars"                 => BuiltinId::ReverseChars,
        "minimize"                      => BuiltinId::Minimize,
        "parse_bool"                    => BuiltinId::ParseBool,
        "sort"                          => BuiltinId::Sort,
        "sort_by"                       => BuiltinId::SortBy,
        "map"                           => BuiltinId::Map,
        "map_fn"                        => BuiltinId::MapFn,
        "filter"                        => BuiltinId::Filter,
        "filter_fn"                     => BuiltinId::FilterFn,
        "reduce"                        => BuiltinId::Reduce,
        "reduce_fn"                     => BuiltinId::ReduceFn,
        "for_each_fn"                   => BuiltinId::ForEachFn,
        "any"                           => BuiltinId::Any,
        "all"                           => BuiltinId::All,
        "find"                          => BuiltinId::Find,
        "unique"                        => BuiltinId::Unique,
        "slice"                         => BuiltinId::Slice,
        "is_nil"                        => BuiltinId::IsNil,
        "is_bool"                       => BuiltinId::IsBool,
        "is_int"                        => BuiltinId::IsInt,
        "is_float"                      => BuiltinId::IsFloat,
        "is_str"                        => BuiltinId::IsStr,
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
        "secure_pick"    | ":secure_pick"    => BuiltinId::SecurePick,
        "secure_random"  | ":secure_random"  => BuiltinId::SecureRandom,
        "secure_shuffle" | ":secure_shuffle" => BuiltinId::SecureShuffle,
        "pack"         | ":pack"          => BuiltinId::Pack,
        "unpack"       | ":unpack"        => BuiltinId::Unpack,
        "lines"        | ":lines"         => BuiltinId::Lines,
        "words"        | ":words"         => BuiltinId::Words,
        "chars"        | ":chars"         => BuiltinId::Chars,
        "format"       | ":format"        => BuiltinId::Format,
        "pct"          | "percent"       => BuiltinId::Pct,
        "between"                        => BuiltinId::Between,
        "is_control"                     => BuiltinId::IsControl,
        "ignore_blocks_first"            => BuiltinId::IgnoreBlocksFirst,
        "pick"                           => BuiltinId::Pick,
        "read_json"                      => BuiltinId::ReadJson,
        "write_text!"                    => BuiltinId::WriteText,
        "append_file!"                   => BuiltinId::AppendFile,
        "write_json!"                    => BuiltinId::WriteJson,
        // Non-bang I/O forms must error — these are intentionally separate.
        "write_text" | "append_file" | "write_json" => BuiltinId::RequiresBang,
        "is_type"                        => BuiltinId::IsType,
        "is_bound_name"                  => BuiltinId::IsBoundName,
        "invoke"                         => BuiltinId::Invoke,
        "summon"                         => BuiltinId::Summon,
        "provoke"                        => BuiltinId::Provoke,
        "need"                           => BuiltinId::Need,
        "yall_parse"                     => BuiltinId::YallParse,
        "yall_parse_file"                => BuiltinId::YallParseFile,
        "yall_write"                     => BuiltinId::YallWrite,
        "yall_write_file"                => BuiltinId::YallWriteFile,
        "yall_pretty"                    => BuiltinId::YallPretty,
        "yall_minify"                    => BuiltinId::YallMinify,
        "create_dir!"                    => BuiltinId::CreateDir,
        "copy_file!"                     => BuiltinId::CopyFile,
        "delete_path!"                   => BuiltinId::DeletePath,
        "zip_dir!"                       => BuiltinId::ZipDir,
        // Non-bang I/O forms must error — these are intentionally separate.
        "create_dir" | "copy_file" | "delete_path" | "zip_dir" => BuiltinId::RequiresBang,
        "md_to_html"                     => BuiltinId::MdToHtml,
        "highlight_code"                 => BuiltinId::HighlightCode,
        "big"  | "b"                     => BuiltinId::ToBig,
        "to_map" | "m"                   => BuiltinId::ToMap,
        "read_text"                      => BuiltinId::ReadText,
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
        "path_fix_separators"            => BuiltinId::PathFixSeparators,
        "path_relative_to"               => BuiltinId::PathRelativeTo,
        "walk"                           => BuiltinId::Walk,
        "list_dirs"                      => BuiltinId::ListDirs,
        "escape_html"                    => BuiltinId::EscapeHtml,
        "url_decode"                     => BuiltinId::UrlDecode,
        "url_encode"                     => BuiltinId::UrlEncode,
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

        // Date/time
        "now"                            => BuiltinId::DtNow,
        "epoch_ms"                       => BuiltinId::DtEpochMs,
        "epoch_s"                        => BuiltinId::DtEpochS,
        "utc_now"                        => BuiltinId::DtUtcNow,
        "local_now"                      => BuiltinId::DtLocalNow,
        "today"                          => BuiltinId::DtToday,
        "tomorrow"                       => BuiltinId::DtTomorrow,
        "yesterday"                      => BuiltinId::DtYesterday,
        "date"                           => BuiltinId::CastDate,
        "time"                           => BuiltinId::CastTime,
        "datetime"                       => BuiltinId::CastDatetime,
        "duration"                       => BuiltinId::CastDuration,
        "to_iso"                         => BuiltinId::DtToIso,
        "from_iso"                       => BuiltinId::DtFromIso,
        "to_epoch_ms"                    => BuiltinId::DtToEpochMs,
        "from_epoch_ms"                  => BuiltinId::DtFromEpochMs,
        "format_datetime"                => BuiltinId::DtFormatDatetime,
        "format_date"                    => BuiltinId::DtFormatDate,
        "format_time"                    => BuiltinId::DtFormatTime,
        "year"                           => BuiltinId::DtYear,
        "month"                          => BuiltinId::DtMonth,
        "day"                            => BuiltinId::DtDay,
        "hour"                           => BuiltinId::DtHour,
        "minute"                         => BuiltinId::DtMinute,
        "second"                         => BuiltinId::DtSecond,
        "weekday"                        => BuiltinId::DtWeekday,
        "add_duration"                   => BuiltinId::DtAddDuration,
        "since"                          => BuiltinId::DtSince,
        "until"                          => BuiltinId::DtUntil,
        "timezone"                       => BuiltinId::DtTimezone,
        "to_timezone"                    => BuiltinId::DtToTimezone,

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

        // Object/overlay query
        "objects"  | ":objects"          => BuiltinId::Objects,
        "overlays" | ":overlays"         => BuiltinId::Overlays,

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

        "tokenize"                       => BuiltinId::Tokenize,
        "get"                            => BuiltinId::Get,

        // ── Bang variants (in-place mutating forms) ───────────────────────────
        // In the VM these are functionally identical to the non-bang versions:
        // they return the updated collection; the caller handles the write-back.
        "put!"                           => BuiltinId::Put,
        "put_first!"                     => BuiltinId::PutFirst,
        "put_last!"                      => BuiltinId::PutLast,
        "put_at!"                        => BuiltinId::PutAt,
        "put_matching!"                  => BuiltinId::PutMatching,
        "put_between!"                   => BuiltinId::PutBetween,
        "put_random!"                    => BuiltinId::PutRandom,
        "update_first!"                  => BuiltinId::UpdateFirst,
        "update_last!"                   => BuiltinId::UpdateLast,
        "update_at!"                     => BuiltinId::UpdateAt,
        "update_all!"                    => BuiltinId::UpdateAll,
        "update_where!"                  => BuiltinId::UpdateWhere,
        "update_matching!"               => BuiltinId::UpdateMatching,
        "update_between!"                => BuiltinId::UpdateBetween,
        "update_random!"                 => BuiltinId::UpdateRandom,
        "delete!"                        => BuiltinId::Delete,
        "delete_first!"                  => BuiltinId::DeleteFirst,
        "delete_last!"                   => BuiltinId::DeleteLast,
        "delete_at!"                     => BuiltinId::DeleteAt,
        "delete_where!"                  => BuiltinId::DeleteWhere,
        "delete_all!"                    => BuiltinId::DeleteAll,
        "delete_matching!"               => BuiltinId::DeleteMatching,
        "delete_between!"               => BuiltinId::DeleteBetween,
        "delete_random!"                 => BuiltinId::DeleteRandom,
        "reap_first!"                    => BuiltinId::ReapFirst,
        "reap_last!"                     => BuiltinId::ReapLast,
        "reap_at!"                       => BuiltinId::ReapAt,
        "reap_where!"                    => BuiltinId::ReapWhere,
        "reap_matching!"                 => BuiltinId::ReapMatching,
        "reap_between!"                  => BuiltinId::ReapBetween,

        // Outbound HTTP
        "http_get"     => BuiltinId::HttpGet,
        "http_post"    => BuiltinId::HttpPost,
        "http_put"     => BuiltinId::HttpPut,
        "http_delete"  => BuiltinId::HttpDelete,
        "http_request" => BuiltinId::HttpRequest,

        // Postgres
        "db_query"     => BuiltinId::DbQuery,
        "db_query_one" => BuiltinId::DbQueryOne,
        "db_exec"      => BuiltinId::DbExec,

        // Render mode
        "render_template" => BuiltinId::RenderTemplate,

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

/// Compile a REPL snippet with knowledge of already-declared globals.
/// `known_globals` is the list of variable names already in the session.
/// New names get appended. Returns the compiled function and the updated globals list.
pub fn compile_repl_snippet(
    module: &Module,
    known_globals: &[String],
) -> Result<CompiledModule, GoblinError> {
    compile_repl_snippet_for_file(module, known_globals, "")
}

/// Like `compile_repl_snippet` but stamps `source_file` onto compiled functions.
pub fn compile_repl_snippet_for_file(
    module: &Module,
    known_globals: &[String],
    source_file: &str,
) -> Result<CompiledModule, GoblinError> {
    let mut c = Compiler::new().for_file(source_file);
    c.globals = known_globals.to_vec();
    c.repl_known_globals_count = known_globals.len();
    c.compile_repl_module(module)
}

impl Compiler {
    /// Compile a single REPL statement into a FunctionObject.
    /// Top-level binds use StoreGlobal/LoadGlobal so state persists across REPL entries.
    /// If the statement is a bare Expr, its value is left on the stack (returned to caller).
    /// All other statements execute and return Nil.
    pub fn compile_repl_module(mut self, module: &Module) -> Result<CompiledModule, GoblinError> {
        self.repl_mode = true;
        self.push_scope("__main__", 0);

        // Hoist bind names and action names as globals so they're addressable.
        let mut hoisted: Vec<String> = Vec::new();
        collect_bind_names(&module.items, &mut hoisted);
        for name in &hoisted {
            if !self.globals.contains(name) {
                self.globals.push(name.clone());
            }
        }
        for stmt in &module.items {
            if let goblin_ast::Stmt::Action(a) = stmt {
                if !self.globals.contains(&a.name) {
                    self.globals.push(a.name.clone());
                }
            }
        }

        if module.items.is_empty() {
            let scope = self.scopes.last_mut().unwrap();
            scope.emit(Opcode::LoadNil);
            scope.emit(Opcode::Return);
        } else {
            // All stmts before the last are side-effect only.
            let (body, last) = module.items.split_at(module.items.len() - 1);
            for stmt in body {
                self.compile_stmt(stmt)?;
            }
            // Last stmt: if bare expr, leave value on stack; otherwise push Nil.
            match &last[0] {
                goblin_ast::Stmt::Expr(e) => { self.compile_expr(e)?; }
                other => {
                    self.compile_stmt(other)?;
                    let scope = self.scopes.last_mut().unwrap();
                    scope.emit(Opcode::LoadNil);
                }
            }
            let scope = self.scopes.last_mut().unwrap();
            scope.emit(Opcode::Return);
        }

        let entry = self.pop_scope();
        self.seal_globals();
        Ok(CompiledModule {
            entry,
            classes: self.collected_classes,
            enums: self.collected_enums,
            global_names: self.globals.clone(),
        })
    }
}

/// A variable, or indexes / `>>` fields below one (at most 16 segments).
/// For `name!(x[k1]…[kN], args…)` with a builtin `name` that writes back
/// into its first argument: the builtin, the root variable and the keys.
/// Paths with `>>` fields keep the general lowering.
fn bang_path_call<'a>(name: &str, args: &'a [Expr]) -> Option<(BuiltinId, String, Vec<&'a Expr>)> {
    const IO_BANG_NO_WRITEBACK: &[&str] = &[
        "write_text!", "write_json!", "append_file!",
        "create_dir!", "copy_file!", "delete_path!", "zip_dir!",
    ];
    let bare = name.trim_start_matches(':');
    if !bare.ends_with('!') || IO_BANG_NO_WRITEBACK.contains(&bare) || args.len() > 255 { return None; }
    let bid = builtin_by_name(bare).or_else(|| builtin_by_name(name))
        .or_else(|| bare.strip_suffix('!').and_then(builtin_by_name))?;
    let mut keys = Vec::new();
    let mut cur = args.first()?;
    let root = loop {
        match cur {
            Expr::Index(base, key, _) | Expr::IndexMap(base, key, _) => { keys.push(key.as_ref()); cur = base.as_ref(); }
            Expr::Ident(name, _) => break name.clone(),
            _ => return None,
        }
    };
    if keys.is_empty() || keys.len() > 16 { return None; }
    keys.reverse();
    Some((bid, root, keys))
}

fn is_lvalue(e: &Expr) -> bool {
    let mut cur = e;
    let mut depth = 0;
    loop {
        match cur {
            Expr::Ident(..) => return true,
            Expr::Index(base, _, _) | Expr::IndexMap(base, _, _) | Expr::Member(base, _, _) => {
                depth += 1;
                if depth > 16 { return false; }
                cur = base.as_ref();
            }
            _ => return false,
        }
    }
}

/// The position a `reap_*!` call removes from.
/// Cast names whose bang form recasts a variable in place (`:int!(k)`).
const CAST_BANG_FREE: &[&str] = &[
    "str", "bool", "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64", "f32", "f64",
    "float", "big", "int", "uint", "pct",
];

fn reap_bang_position(name: &str) -> Option<&'static str> {
    Some(match name {
        "reap_first!" => "first",
        "reap_last!" => "last",
        "reap_at!" => "at",
        "reap_where!" => "where",
        "reap_matching!" => "matching",
        "reap_between!" => "between",
        _ => return None,
    })
}

/// The `{name}` placeholders of an interpolated string, in order, without
/// duplicates (escaped `\{`, `{{{…}}}` tokens and `{#…}` box keys skipped).
fn interp_placeholder_names(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut out: Vec<String> = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '\\' { i += 2; continue; }
        if chars[i] == '{' {
            if i + 2 < chars.len() && chars[i + 1] == '{' && chars[i + 2] == '{' {
                let mut j = i + 3;
                while j + 2 < chars.len() && !(chars[j] == '}' && chars[j + 1] == '}' && chars[j + 2] == '}') { j += 1; }
                i = j + 3;
                continue;
            }
            let mut j = i + 1;
            while j < chars.len() && chars[j] != '}' { j += 1; }
            if j >= chars.len() { break; }
            let inner: String = chars[i + 1..j].iter().collect();
            let inner = inner.trim();
            if !inner.is_empty() && inner.chars().all(|c| c.is_alphanumeric() || c == '_')
                && !out.iter().any(|n| n == inner)
            {
                out.push(inner.to_string());
            }
            i = j + 1;
            continue;
        }
        i += 1;
    }
    out
}

/// How an indexed expression reads in the source, for bracket errors.
fn source_text(e: &Expr) -> String {
    match e {
        Expr::Ident(n, _) => n.clone(),
        Expr::Index(b, k, _) => format!("{}[{}]", source_text(b), key_text(k)),
        Expr::IndexMap(b, k, _) => format!("{}{{{}}}", source_text(b), key_text(k)),
        Expr::Member(b, f, _) => format!("{}.{}", source_text(b), f),
        _ => "this value".into(),
    }
}

fn key_text(e: &Expr) -> String {
    match e {
        Expr::Str(s, _) => format!("\"{s}\""),
        Expr::Ident(n, _) => n.clone(),
        Expr::Number(n, _) => n.clone(),
        _ => "…".into(),
    }
}
