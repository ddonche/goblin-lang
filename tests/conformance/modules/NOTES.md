# modules: notes

There are 96 cases. They cover imports and loading, calls to module actions, module globals and constants, user actions and variables whose names collide with builtins, and the basics of enums and classes, including ones declared in modules. Helper modules live in `tests/conformance/_support/modules/` and are imported as `_support/modules/<name>`. Imports resolve from cwd (`tests/conformance`).

Binaries:
- **baseline**: `goblin-base`.
- **wip1**: `goblin-wip1`. It fixes three things:
  - `run --vm` resolves imports from cwd;
  - nested `update!` works on the VM;
  - user actions shadow builtins on both engines.

Every `known-gap` line describes what **wip1** still gets wrong. The baseline behaviour is recorded below.

## Syntax actually implemented (vs. docs)

The docs (§24 of `docs/language-spec.md`, and the cheat sheet's "Modules & Imports") describe `import "./helpers" as H` anchored at a fixed `modules/` directory, with `op` declarations. Both engines actually do this:
- **Import form:** `import a/b/c as x`, unquoted or quoted. `"./a/b"` also works.
- **Path resolution:** relative to cwd/project root. This holds inside nested modules as well. It does not resolve relative to the importing file (case `import_module_relative_path_errors`).
- **Calls:** `x::act(...)`.
- **Declarations:** `act`/`xx`, `enum Name ... xx` with `Name::Variant`, and `<>Class | fields + act ... xx` with `obj <> Class | f: v` and `obj >> f`.

Sheriff does the same, e.g. `sheriff-core/modules/scout_nav/scout.gbln:5` has `import modules/trailboss_wiki/routes_json as rjson`. That is root-relative from a nested module, and `trailboss.gbln:10` imports the same module again (a diamond).

## (a) Disagreements and gaps

Columns are baseline interp / baseline VM / wip1 interp / wip1 VM. "path" means the baseline VM failed only because `run --vm` resolved imports from the script directory (`modules/`). The fix is in `crates/goblin-cli/src/main.rs`, around line 1172. During exploration I used a temporary symlink so I could see the baseline VM's semantics behind that path bug; those observations are given in brackets.

### Import / loading

**import_two_aliases_runs_once** (known-gap interp)
- Results: interp `loud loaded` ×2 / path / ×2 / once (OK).
- Spec §24.4: "A module's top-level code runs once at its first import; subsequent imports use the cached module".
- Cause: the interp caches by alias, not by file. See `modules.rs:124-131`: the namespace is the alias, and the code does `if self.loaded.contains_key(&namespace)`.

**import_two_aliases_share_state** (known-gap both)
- Results: interp error R0111 duplicate-local (it re-runs the top level in the shared frame) / path / same / VM prints `0`, from the global-write bug below.
- Spec §24.4 again: there is one cached instance.

**import_alias_conflict_errors** (known-gap both)
- Results: interp keeps the first module (`alias_a`) and the VM keeps the last (`alias_b`). Neither errors.
- Spec §24.5: "Duplicate import … as Alias within one file → ModuleNameConflictError."

**import_cycle_errors** (known-gap both)
- Results: both engines accept A→B→A silently. Mutual calls across the cycle even work: I checked `c(3)` → `cdcd` on both.
- Spec §24.6: "Import cycles are disallowed in this version … ModuleCycleError". The spec is explicit, so I followed the brief's rule. **The owner may prefer to bless cycles, since they work.**

**import_without_alias** (undecided **D-import-without-alias**; see (b))
- Results: interp prints `5`, using the last path segment as namespace. The VM gives `undefined variable 'mathy::add'`.

**import_unqualified_call_errors** (both error, agree)
- The VM fails at compile time, so the case prints nothing before `<error>`.

**import_resolves_from_cwd** and the other import cases: baseline VM path error; wip1 OK.

### Module actions

**module_action_too_many_args_errors** (known-gap interp)
- Results: interp prints `3`, silently dropping the extra argument on a qualified call / path / `3` / error.
- The interp itself raises R0301 for the same mistake on a top-level action, so this is an inconsistency inside the interp.
- Code: the arity check in the qualified-call path, `lib.rs` ~18845 (eval) vs `call_action_by_name` ~11310.

**importer_action_same_name_defined_before_import** (known-gap vm)
- Results: wip1 VM prints `left+hi` for the importer's own `name()`. Importing overwrites a bare action that was defined earlier.
- `vm.rs` ~1610 `ImportFileAs` registers module acts under bare and qualified names ("file merge"). It works if the importer defines its action *after* the import (case `..._after_import`).

**module_internal_call_ignores_importer_action** (known-gap vm)
- Results: the wip1 VM's `m::twice_add` calls the importer's `add`, which leads to `type error in mul`. The interp correctly calls the module's own `add`.

**module_action_calls_own_{all,join,find,update,url_encode,url_decode,pct,len,count,keys}** and **module_action_colon_call_prefers_own_action**
- Baseline: path. [Symlink run: the baseline VM called the builtin. `one`→`all` gave `arity mismatch calling 'all'` (the Campfire `lib/db.gbln` failure); `join`→`a`, `url_encode`→`a%20b`, `len`→`3`, `keys`→`[a]`.]
- wip1: OK on both.

**module_action_calls_own_{round,sum}**
- Results: baseline interp `3` (the builtin won even inside a module) / path / OK / OK.

**module_builtin_named_actions_qualified**: baseline VM path [symlink: OK]; wip1 OK.

### Module globals

These four cases are known-gap vm:
- **module_global_reassign_persists**
- **module_global_string_reassign_persists**
- **module_global_put_last_persists**
- **module_global_update_persists**

Details:
- Results: interp OK / path / OK / VM loses the write. Examples: `get_counter()` → `0` after two `bump()`s; `push` returns `0`; `get_tally_a()` → `0`.
- A write is visible to *the same* action on later calls (`bump()` returns 1, then 2) but not to other actions or to the top level.
- This is **not module-specific**. The same happens in a plain script; see the `script_global_*` cases below. The audit had found this "not reproducible"; it reproduces whenever a *different* action reads the global.
- Likely cause: top-level binds compile to locals of the entry function, and actions reach them as upvalues, so each closure gets its own copy. See `compiler.rs:475` `resolve_load` / `:532` `resolve_store` (local → upvalue → global) and the `Stmt::Bind` Tether arm at ~553, which only uses globals in REPL mode.
- Spec §5 "Scope": `op add_point()  score = score + 1  /// updates global score`.

The four script-level cases are also known-gap vm. They print `1 2 0`, `0`, `0 0 0` and `1 2 0` on both VM binaries:
- **script_global_reassign_from_action**
- **script_global_reassign_seen_by_other_action**
- **script_global_put_last_from_action**
- **script_global_update_from_action**

**module_global_not_visible_unqualified** (known-gap both)
- Results: both print the module's `counter` unqualified (`0`).
- Spec §24: "no implicit re-exports, and no 'magical' namespace merges … refer to their contents via Alias::symbol only".

**module_global_vs_importer_global_before_import** and **module_global_vs_importer_global_after_import** (known-gap interp)
- Results: the interp fails with R0111 duplicate-local when the importer and the module both bind `x`. The VM keeps them separate (`100`, `1`).
- Cause: the module's top-level binds land in the importer's frame. `lib.rs:1132-1170`: `get_var`/`set_var` consult the module env, but the Bind path's `define_local` checks `sess.env[cur]`.

**module_constant_qualified_read** (known-gap interp)
- Results: interp R0117 "unknown enum 'm'". `modules.rs:166-178` exports only Action/Class/Enum.
- VM: `10`/`11`.
- Spec §24.3: "In expose files: everything is importable except symbols marked vault."

**module_variable_qualified_read_is_live** (known-gap both)
- Results: the interp has no `m::var`. The VM prints `0 0` because of the lost-write bug.
- Intended: `0 1`, the live cached instance (§24.4).

### Builtin-named user actions at top level (spec §4: "Built-ins (shadowable operations & types)")

- **user_action_named_{all,find,join,update}**: baseline VM arity error.
- **user_action_named_{count,len,keys,values,sort,pct,url_encode,url_decode}** and **user_action_named_len_colon_call**: the baseline VM returned the builtin's result.
- **user_action_named_{round,sum,min,max,upper}**: *both* baseline engines used the builtin. The interp special-cases these before the user-action lookup.
- **user_action_named_first**: OK everywhere, because `first` is a soft keyword and not a builtin.
- wip1: all of these pass on both engines.

### Variables named like builtins

These cases are known-gap both:
- **builtin_name_variable_index_len**: both print `1`.
- **builtin_name_variable_index_keys**: the interp gives T0205; the VM gives `[0]`.
- **builtin_name_variable_arith_count**: the interp gives T0205; the VM gives "unknown prefix operator".

Details:
- Cause: the shared parser reads `len[1]` / `count + 1` as a prefix call (`len [1]`).
- `sum[1]` happens to work (`builtin_name_variable_index_sum`).
- Plain reads and parameters named `keys`/`len`/`join` work on both engines (`builtin_name_variable_read`, `builtin_name_action_params`).
- Evidence: spec §4 lists them as shadowable; the audit (§A.13) says "Builtin names hijack `name[0]` indexing".
- **The owner may call this "prefix-call syntax wins" instead.** If so, convert these three cases to `undecided`.

### Enums and classes

**enum_display** (undecided **D-enum-display**)
- Results: interp `Status::Idle`; VM `Status.Idle`, for both `say` and `:str`.

**enum_variant_fields** (known-gap vm)
- Results: `Shape::Circle { r: 2 }` gives "type error in enum fields: expected map or nil, got collection". The VM map literal is a Collection.

**class_method_mutates_self** (known-gap interp)
- Results: interp `6 6 5`; VM `6 7 7`.
- Evidence: `tests/action_test.gbln:5-8,32-33` expects `Rome.fortify()` (`self >> stability |= …`) to change `Rome`.

**module_class_method_call** and **module_class_constructed_in_importer** (known-gap vm)
- Results: `b.area()` on an object of a class declared in an imported module gives "method call 'area' on non-object (object)". Field reads work.
- The same class declared at top level works on the VM (`class_method_call`).

**module_enum_used_by_importer**: both engines agree that enums and classes declared in a module are usable by their bare name in the importer. The spec says module symbols are reached as `Alias::symbol`, but `sh::Color::Red` and `<> sh::Box` do not parse on either engine (see Spec-only). I recorded the working bare-name behaviour.

## (b) Undecided slugs

### D-import-without-alias (`import_without_alias`)

- **Interp:** `import a/b/mathy` binds namespace `mathy`. This is deliberate: `modules.rs:124-128` uses `import_path.split('/').last()`.
- **VM:** it treats an unaliased import as a "file merge". `vm.rs` ~1600 says "`import` is a file merge, not a namespace load", so `mathy::add` is undefined. Whether bare `add` is callable after it was not clear in my probe.
- **Spec §24.2:** "Every import requires an alias", which suggests an error at the import. Sheriff only uses unaliased imports for `.imports` manifests.

Options:
- (1) Require an alias: a parse/compile error in `goblin-parser` (Stmt::Import), or in both loaders.
- (2) Adopt the interp's last-segment namespace in the VM: `compiler.rs` ~757-766 would emit `ImportFileAs` with the derived alias.
- (3) Adopt merge semantics in the interp: `lib.rs` 4434 `Stmt::Import`.

### D-enum-display (`enum_display`)

- **Interp:** `Status::Idle`. This matches the access syntax that both engines accept (`Status::Idle`); `Status.Idle` is an unknown identifier on both.
- **VM:** `Status.Idle`. This matches the cheat sheet's enum examples (`Status.Pending`), but that dotted access form is not implemented.
- To change the VM: `goblin-vm/src/builtins.rs:5226` and `debug.rs:209` (`format!("{}.{}")`).
- To change the interp: its enum Display arm.

## (c) Spec-only (in neither engine; no cases)

- `modules/` root anchoring; ModulePathError for `..` and absolute paths.
- `expose` / `vault` visibility and `set @policy` module modes.
- ModuleVisibilityError / PolicyVisibilityError.
- Qualified access to module enums and classes: `sh::Color::Red` and `x <> sh::Box | …` are parse errors on both.
- `goblin module <alias> <op>` CLI; `[modules]` manifest.
- Enum `.name` / `.value` / `.ordinal`, `Status.values`, `from_name` / `from_value`; dotted `Status.Pending` access. `.name` gives "variant has no fields" on the interp and an index type error on the VM.
- Cheat-sheet class syntax: `class Pet = name: … :: age: 0`, `Pet: "Fido" :: 3`, and auto-generated `set_x` setters.

## (d) Inventory builtin names exercised

all, count, find, join, keys, len, max, min, pct, put_last, round, sort, str, sum, update, upper, url_decode, url_encode, values. Most appear as user-action names that shadow the builtin; `put_last`, `update`, `len` and `str` are also called as builtins (`:put_last!`, `:update!`, `:len`, `:str`).

## Also observed (no case written)

- `f | m::add; f(1, 2)`: interp R0117; VM "arity mismatch calling 'ToFloat'". Module actions as values are undocumented.
- A module action can call an action defined only in the importer (both engines). The spec neither allows nor forbids this.
- `say` of a map on the VM prints only its values (`[0]`). That belongs to the collections area; I avoided printing maps.
- `obj <> Class | nme: …` (unknown field) is silently ignored on both engines.
