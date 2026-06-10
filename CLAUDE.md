# Goblin VM — Development Tracking

## Rules (never violate)
- Read ALL interpreter source before implementing anything — never invent behavior
- Match interpreter behavior exactly
- ALL goblin blocks end with both `xx` AND `end` terminators
- Working branch: `claude/quirky-mendel-4df8rm`
- Commit and push after every batch of changes

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

## TODO / NOT YET DONE

### Compiler — unimplemented AST nodes (return NotImplemented error)
- [ ] `Expr::Slice` — slice expressions (e.g. `arr[1:3]`)
- [ ] `Expr::Slice3` — 3-arg slice (e.g. `arr[1:3:2]`)
- [ ] `Expr::Index2` — 2D index for grid (`grid[x, y]`)
- [ ] `Expr::EnumVariant` — enum variant construction expressions
- [ ] `Expr::LiteralToken` — literal token expressions
- [ ] `Stmt::Class` / `Stmt::Enum` / `Stmt::Import` / `Stmt::Use` — class/enum/import system
- [ ] `Stmt::OverlayDef` / `Stmt::OverlayApply` / `Stmt::OverlayDetach` — overlay system
- [ ] `Stmt::LinkDef` / `Stmt::ObjectLinkDef` / `Stmt::LinkOffset` / `Stmt::ClearLink` — link system
- [ ] `Stmt::ObjectDecision` / `Stmt::UnitDecl` — DES decisions, unit declarations

### Builtins — not implemented
- [ ] `invoke` / `summon` / `provoke` — dynamic action dispatch (needs VM action call infrastructure)
- [ ] `md_to_html` — needs comrak crate
- [ ] `highlight_code` — needs syntect crate
- [ ] `tick` / `tick_db` — needs full DES tick runner ported to VM

### Skipped by user instruction
- [ ] `money` — skip
- [ ] `db_query`, `db_exec`, `db_query_one` — skip (db crate unfinished)
