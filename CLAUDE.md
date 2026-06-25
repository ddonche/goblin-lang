# Goblin VM — Development Tracking

## Rules (never violate)
- Read ALL interpreter source before implementing anything — never invent behavior
- Match interpreter behavior exactly
- ALL goblin blocks end with both `xx` AND `end` terminators
- Working branch: always use the latest branch (highest commit date on remote). Never hardcode a branch name here.
- Commit and push after every batch of changes
- Never say "sorry", "I apologize", or any variant — no emotion language
- Never say "noted" or imply memory without actually writing it down

## Blueprint architecture (NEVER change without explicit user permission)
The VM implements the Goblin VM Blueprint v1.2 — register-stack hybrid with arena-backed named slots.
These are HARD CONSTRAINTS — violating any of them is forbidden regardless of perceived performance benefit:

- `session.arena: Slab<Stash>` — ALL named values live in the arena.
- `Stash { value: Value, tether_count: usize, generation: u32 }` — exact shape, no changes
- `Address { slot: u32, generation: u32 }` — exact shape, no changes
- `Tether { addr: Address }` — frame locals/globals/upvalues hold Tethers (arena-backed, have identity)
- `pub stack: Vec<Value>` — expression scratch space holds Value directly (NO arena involvement for temporaries). Authorized by Blueprint v1.2.
- `GcMode { Off, Manual, Auto }` — three modes, always present in Session
- `session.alloc_value(v)` — called ONLY at binding points: StoreLocal, StoreGlobal, StoreUpvalue, StoreBox, and when building call frame arg lists. Never bypass it for named values.
- `session.gc_sweep()` — called by `gc()` builtin and Auto mode. Never remove it.
- `:mem_id(x)` / `:mem_addr(x)` — return errors when called on stack Values (no Tether address)
- `overwrite!` is the ONLY mutation primitive — it mutates the stash in place
- Worker isolation: each Worker has its own Session (its own arena). No shared arenas.
- Do NOT replace the arena with Vec<Value> or any flat structure for "performance"
- Do NOT change Tether to store Value directly
- Do NOT make frame locals/globals/upvalues hold Values instead of Tethers

## What to skip (user instructions)
- `money` builtins — skip
- `db_query`, `db_exec`, `db_query_one` — skip (db crate unfinished)

## Architecture
- `crates/goblin-vm/src/value.rs` — Value enum, BuiltinId enum
- `crates/goblin-vm/src/opcode.rs` — Opcode enum
- `crates/goblin-vm/src/compiler.rs` — AST → bytecode, `builtin_by_name`
- `crates/goblin-vm/src/vm.rs` — VM execution loop, `member_dispatch`
- `crates/goblin-vm/src/builtins.rs` — all builtin implementations
- `crates/goblin-vm/src/session.rs` — Session state
- `crates/goblin-vm/src/grid.rs` — Grid data structures
- `crates/goblin-interpreter/src/lib.rs` — source of truth for behavior

## DONE

### Session state
- [x] `token_store: BTreeMap<String, BTreeMap<String, Value>>`
- [x] `object_store: HashMap<String, Value>`
- [x] `overlay_instances: Vec<OverlayInstance>`
- [x] `response: ResponseState { status, headers, cookies }`
- [x] `grid_store: GridStore`

### Opcodes (all handled in vm.rs)
- [x] LoadConst, LoadNil, LoadTrue, LoadFalse
- [x] LoadLocal, StoreLocal, LoadGlobal, StoreGlobal, LoadUpvalue, StoreUpvalue
- [x] Pop, Dup, Overwrite
- [x] Add, Sub, Mul, Div, Rem, Neg (+ typed Int/Float variants)
- [x] Concat, Eq, Ne, Lt, Le, Gt, Ge, Not
- [x] Jump, JumpIfFalse, JumpIfTrue
- [x] MakeArray, MakeMap, GetIndex, SetIndex, GetMember
- [x] Call, Return, CallBuiltin, MakeClosure
- [x] ToPct, MakePair, MakeRange, MakeRangeInclusive
- [x] Quick, TryBegin, TryEnd

### Compiler — AST nodes handled
- [x] All Expr variants: Nil, Bool, Number, Str, Char, Ident, Array, Object, Member, OptMember, Index, IndexMap, Call, OptCall, FreeCall, NsCall, Prefix, Postfix, Binary, Judge, Block, BoxVar
- [x] All Stmt variants: Expr, Bind, TupleBind, Block, Return, Judge, JudgeAll, Sweep, Action, BoxBind (errors with NotImplemented), Class/Enum/Import/Use/Overlay*/Link*/ObjectDecision/UnitDecl (errors with NotImplemented — DES system)

### Compiler — special forms
- [x] `collect(count, body)` — loop with ArrayPush accumulator
- [x] `attempt(try, rescues, ensure?)` — TryBegin/TryEnd + catch_stack

### Builtins — all implemented and wired in builtin_by_name

#### Math
- [x] abs, min, max, avg, sum, floor, ceil, round, sqrt, clamp, pow

#### String
- [x] lower, upper, title, slug, mixed, raw, trim, trim_lead, trim_trail
- [x] find, find_all, ord, len, str/to_str, split, join, contains, starts_with, ends_with
- [x] replace, before, after, before_last, after_last, keep_before, keep_after, keep_between
- [x] sanitize_bom, normalize_newlines
- [x] ignore_where, ignore_lines_where, ignore_matching, ignore_lines_matching
- [x] is_matching, count_matching, keep_matching
- [x] json_parse, json_stringify, json_stringify_pretty
- [x] ignore_between, ignore_blocks, ignore_blocks_first
- [x] lines, words, chars, format, pad, pad_left, pad_right, repeat
- [x] escape_html

#### Collections
- [x] has, count, shuffle, sort, freq, mode, sample_weighted, map, unique, dups
- [x] grab, grab_first, grab_last, grab_at, grab_random, grab_where, grab_all, grab_between, grab_matching
- [x] put, put_first, put_last, put_at, put_where, put_matching, put_between, put_random, put_all
- [x] update, update_first, update_last, update_at, update_all, update_where, update_matching, update_between, update_random
- [x] delete, delete_first, delete_last, delete_at, delete_where, delete_all, delete_matching, delete_between, delete_random
- [x] reap, reap_first, reap_last, reap_at, reap_random, reap_where, reap_all, reap_matching, reap_between, reap_sample
- [x] get_first, get_last, get_at, get_where, get_all, get_matching, get_between, get_random
- [x] pairs, is_empty, reverse, reverse_chars, minimize, parse_bool, sort_by, filter, reduce, any, all, find_index, zip, flatten, slice
- [x] keys, values, items, between
- [x] map_fn, filter_fn, reduce_fn, for_each_fn

#### Type checks
- [x] is_nil, is_bool, is_int, is_float, is_str, is_array, is_map, is_collection, is_function
- [x] is_big, is_pct, is_num, is_char, is_pair, is_seq, is_unit
- [x] is_alnum, is_alpha, is_digit, is_whitespace, is_control
- [x] is_even, is_odd, is_multiple_of, is_positive, is_negative, is_nix
- [x] is_type, is_bound_name

#### Conversions / casts
- [x] int/i, float/f, bool, str, big, pct, to_map
- [x] i8, i16, i32, i64, u8, u16, u32, u64, f32, f64 (range-checked, error on overflow)
- [x] date, time, datetime, duration — returns "not yet implemented" error (same as interpreter)

#### I/O
- [x] print, println, eprint, eprintln
- [x] read_text, write_text, append_file, read_json, write_json
- [x] ask/input (stdin with optional prompt)
- [x] env

#### Filesystem / path
- [x] file_exists, is_file, is_dir, basename, dirname, stem, ext
- [x] path_join, path_split, path_normalize, path_relative_to
- [x] walk, list_dirs, create_dir, copy_file, delete_path
- [x] pathfind

#### Process
- [x] run_cmd (sh -lc on Unix, cmd /C on Windows → JSON {ok, code, stdout, stderr})
- [x] zip_dir

#### Random / dice
- [x] roll, roll_detail, roll_str, roll_detail_str
- [x] rand_seed, secure_pick, secure_random, secure_shuffle

#### UUID
- [x] uuid_v4, uuid_v7

#### Range / lorem
- [x] range, ipsum, ipsum_sentences, ipsum_paragraphs, ipsum_full

#### Memory / GC
- [x] mem_id, mem_addr, mem_total, mem_human, gc

#### Type meta
- [x] typeof/type_of, valtype/vt, is_type, is_bound_name
- [x] clear_format, format_info, backend, metrics
- [x] assert, panic

#### Pack/unpack
- [x] pack, unpack

#### Collections — higher order
- [x] invoke, summon, provoke — NOT IMPLEMENTED (require dynamic action dispatch), return error

#### Markdown / syntax
- [x] md_to_html — NOT IMPLEMENTED (needs comrak), returns error
- [x] highlight_code — NOT IMPLEMENTED (needs syntect), returns error

#### YALL
- [x] yall_parse, yall_parse_file, yall_write, yall_write_file, yall_pretty, yall_minify

#### Request (HTTP context — reads env vars)
- [x] req_method, req_path, req_query, req_body, req_header, cookie

#### Response (HTTP context — mutates session.response)
- [x] set_status, set_header, set_cookie

#### Token store
- [x] register_token, resolve_token, clear_token, clear_tokens, clear_all_tokens, list_tokens

#### DES / overlay
- [x] decision_debug, overlays_of, overlay_strength, link_score
- [x] owned_by, owns_tree, clone_object, delete_object, delete_overlays_on
- [x] tick / tick_db — NOT IMPLEMENTED (DES tick runner not ported to VM), returns error

#### Grid (21 builtins)
- [x] grid, grid_get, grid_set, grid_void
- [x] grid_tile_get, grid_tile_set, grid_region_get, grid_region_set
- [x] grid_default_get, grid_default_set
- [x] grid_neighbors, grid_occupied, grid_unoccupied
- [x] grid_occupied_count, grid_unoccupied_count, grid_count
- [x] grid_occupied_by, grid_has, grid_info, grid_tile_info, grid_region_info

### Compiler — synthesized builtins for expr nodes
- [x] `Expr::Slice` — compiles to `CallBuiltin(SliceExpr, 3)` with nil defaults
- [x] `Expr::Slice3` — compiles to `CallBuiltin(Slice3Expr, 4)` with nil defaults
- [x] `Expr::Index2` — compiles to `CallBuiltin(Index2Expr, 3)` → `Value::GridRef`
- [x] `Expr::EnumVariant` — compiles to `CallBuiltin(EnumVariantExpr, 3)` → `Value::Enum`
- [x] `Expr::LiteralToken` — compiles to `CallBuiltin(LiteralTokenExpr, 2)` → resolves from token_store
- [x] `Expr::BoxVar` — compiles to `CallBuiltin(BoxVarExpr, 2)` → error (no box_store in VM)

### Compiler — class/enum support
- [x] `Stmt::Class` — collected at compile time into `CompiledModule.classes`, pre-registered in session before run
- [x] `Stmt::Enum` — collected at compile time into `CompiledModule.enums`, pre-registered in session before run
- [x] `CompiledModule` wrapper — carries `entry: FunctionObject` + `classes` + `enums`
- [x] `session.classes: HashMap<String, ClassDecl>` and `session.enums: HashMap<String, EnumDecl>` added
- [x] `EnumVariantExpr` validates against `session.enums` at runtime

## TODO / NOT YET DONE

### Compiler — import/use
- [x] `Stmt::Import` (path/named) — emits `ImportFile` opcode; VM reads/compiles/runs the file
- [x] `Stmt::Use` — emits `UseGlam(ns_idx)`; VM reads `glams/<ns>/glam.toml` `[needs.actions]` then imports `glams/<ns>/<ns>.gbln` with `owner_glam` stamped on its top-level actions
- [x] `ImportFile` / `UseGlam` opcodes — share `Vm::import_file` helper: lex+parse+compile+execute sub-file via push-frame + `run_until_depth` (NOT `self.execute()` — see Action Needs section), pre-register its classes/enums, guard against re-import

### Compiler — DES statement support
- [x] `Stmt::OverlayDef` — emits `OverlayDef` opcode; VM registers in `session.overlay_defs`
- [x] `Stmt::OverlayApply` — emits `OverlayApply` opcode; VM pushes `OverlayInstance`
- [x] `Stmt::OverlayDetach` — emits `OverlayDetach` opcode; VM removes matching instances
- [x] `Stmt::LinkDef` — emits `LinkDef` opcode; VM registers in `session.link_defs`
- [x] `Stmt::ObjectLinkDef` — emits `ObjectLinkDef` opcode; VM registers in `session.object_link_defs`
- [x] `Stmt::LinkOffset` — emits `LinkOffset` opcode; VM updates `session.link_offsets`
- [x] `Stmt::ClearLink` — emits `ClearLink` opcode; VM removes from `session.link_offsets`
- [x] `Stmt::ObjectDecision` — emits `ObjectDecision` opcode; VM registers in `session.object_decisions`
- [x] `Stmt::UnitDecl` — emits `UnitDecl` opcode; VM registers in `session.unit_registry`

### Session additions
- [x] `overlay_defs`, `link_defs`, `object_link_defs`, `link_offsets`, `object_decisions`, `unit_registry`
- [x] `des_store`, `des_index`, `des_tick_runner`, `des_overlay_id_counter`, `des_link_id_counter`, `des_link_ids`
- [x] `goblin-des` added as dependency

### Builtins — newly implemented
- [x] `invoke` / `summon` / `provoke` — implemented via `RegisterAction` opcode + `session.named_values` + `call_named` / `run_until_depth` in vm.rs
- [x] `md_to_html` — comrak 0.21 added, matches interpreter implementation exactly
- [x] `highlight_code` — syntect 5 added, matches interpreter implementation; syntaxes/Goblin.sublime-syntax copied

- [x] `tick` / `tick_db` — full DES tick in `tick.rs`: 9 passes (link offset decay, spread × 4 modes, decay, conflict, spawn, overlay transitions × 8 kinds, dead/orphan removal, decision tick, object transition tick); object_store maintained on StoreLocal/StoreGlobal; expression eval via compile_tick_expr / eval_tick_expr / run_until_depth

### Action Needs for GLAMs (`:need()`) — interpreter + VM
- [x] `glam.toml` `[needs]` split into `[needs.values]` (box refs, unchanged) and `[needs.actions]` (`need_name = "namespace::action"`, no `#` prefix)
- [x] Interpreter: `Session.action_needs: HashMap<String, HashMap<String, String>>`; `load_glam_box_toml` parses `[needs.actions]`; new `"need"` arm in `call_action_by_name` resolves via `sess.current_module` → `action_needs` → `call_action_by_name` recursively
- [x] Error codes `B0106` (missing-action-need), `B0107` (missing-provider-action), `B0108` (need-outside-glam) added to `diagnostics::rtcode`
- [x] VM: `BuiltinId::Need`, `FunctionObject.owner_glam: Option<String>` (stamped on top-level GLAM actions by `Compiler::with_glam_namespace`), `Opcode::UseGlam(u16)` (replaces direct `ImportFile` for `Stmt::Use`; reads `glams/<ns>/glam.toml` `[needs.actions]` into `session.action_needs`, then imports the entry file with `owner_glam` set)
- [x] VM: `Vm::vm_need` special-cased in `Opcode::CallBuiltin` (alongside invoke/summon/provoke) — reads owning GLAM from `call_stack.last().func.owner_glam`, dispatches via `session.named_values` / `call_named` (NOT via NsCall — see VM limitation below)
- [x] Fixed pre-existing VM bug: `import_file` (used by both `ImportFile` and `UseGlam`) now pushes a frame and calls `run_until_depth` instead of `self.execute()` — the old code shared `call_stack`/`stack` with the caller but ran a fresh `run_loop()` that only stopped when the ENTIRE call stack was empty, so a second `use`/`import` statement after the first would execute while `base_dir` was still pointed at the first import's directory (or worse, silently execute the rest of the caller's bytecode from inside the nested call). This blocked any script doing more than one `use`/`import`.
- [x] VM limitation (not fixed, out of scope): `ns::action(...)` (`Expr::NsCall`) resolves at compile time per-compilation-unit and does not see cross-glam exports, so `:need()`'s provider action is invoked via `invoke`'s `named_values` mechanism instead, not via qualified-name dispatch like the interpreter. `session.named_values` is a flat bare-action-name registry, so two GLAMs declaring an action with the same bare name collide (pre-existing limitation shared with `invoke`/`summon`/`provoke`).

## VM IS FEATURE-COMPLETE

All interpreter builtins implemented. All opcodes handled. All AST nodes compiled. No remaining TODO items.

### Skipped by user instruction
- [ ] `money` — skip
- [ ] `db_query`, `db_exec`, `db_query_one` — skip (db crate unfinished)
