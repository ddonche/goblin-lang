# Conformance decisions: questions for the owner

**Status (2026-10-06 16:15 UTC):** the owner accepted the recommendations below for D2-D29 (D1 was answered separately: insert). The cases now expect the recommended behaviour; an engine that does not implement it yet carries a `known-gap` marker naming the decision.

The interpreter (`goblin run`) and the VM (`goblin run --vm`) disagree on the language questions below. Each answer will turn its `/// undecided: D-<slug>` cases into ordinary cases with a `.out` file. While this file was being written, vm-parity commits changed the VM on three questions: D1 is recorded as decided, and the current VM now matches the interpreter on D17 and D25 (these still need your confirmation).
For each question this file gives what each engine does, what Goblin's own docs, tests, Sheriff and the Campfire port show, and what each answer would require.
Every behaviour shown was re-run on two builds. **base** is `goblin-base`, the baseline the cases were written against. **cur** is `/home/claude/goblin-build/target/release/goblin` as rebuilt at 2026-10-06 16:08 UTC, with the vm-parity VM fixes (map printing, slicing, nested and inserting `update!`, builtin shadowing, structural equality, repeat modes). Where the two builds differ, the section says so.
A general tiebreaker exists. AGENTS.md:5 and CLAUDE.md:5 say "Match interpreter behavior exactly", and AGENTS.md:22 and CLAUDE.md:43 call `crates/goblin-interpreter/src/lib.rs` the "source of truth for behavior". The recommendations below cite that rule only where nothing more specific applies.
Sheriff runs on the interpreter (`sheriff-core/desk/api/build.gbln:44` runs `goblin run main.gbln`). So does Campfire (`campfire-goblin/GAPS.md:190`).

## Quick answers

Reply in the form "D1 A, D2 B, …". Decisions are ordered by how much they matter to real programs.

| ID | Question | Options | Recommended |
|---|---|---|---|
| D1 (D-update-missing-key) | `:update!(m["k"], v)` when `k` is absent | A insert · B error | **decided: A** (commit a1791ca); nothing to answer |
| D2 (D-missing-key-read) | Reading `m["k"]` / `m.k` when `k` is absent | A error · B `nil` · C error, but `??` and `?.` treat it as nil | **B** |
| D3 (D-block-scope) | Are `if`/loop bodies their own scope for `\|` bindings? | A block scope · B action scope · C block scope, but `x \| v` on an outer `x` is an error | **C** |
| D4 (D-cast-rebinds) | Does `:str(x)` / `:int(x)` rebind `x`? | A pure (no) · B rebinds | **A** |
| D5 (D-int-division) | `10 / 4` | A `2` (truncate) · B `2.5`, exact stays int · C always float | **B** |
| D6 (D-map-key-order) | Order of `keys`, iteration and printing for a map | A sorted · B insertion order | **B** |
| D7 (D-int-float-equality) | `1 == 1.0` | A `true` · B `false` | **A** |
| D8 (D-truthiness) | Non-bool values in `if` / `and` / `or` / `not` | A must be bool · B truthiness, `and`/`or` give bool · C truthiness, `and`/`or` give an operand | **B** (evidence balanced) |
| D9 (D-redeclare) | `x \| 1` then `x \| 2` in the same scope | A error R0111 · B rebind | **A** |
| D10 (D-get-matching-empty) | `:get_matching` with no match | A error R0701 · B `[]` | **B** |
| D11 (D-negative-int-division) | `-7 // 2` and `-7 % 3` | A floor (`-4`, `2`) · B truncate (`-3`, `-1`) | **A** |
| D12 (D-string-plus-nonstring) | `"a" + 1` (the engines agree on `a1`; the docs say TypeError) | A keep coercion, fix the docs · B TypeError | **B** (owner reversed it 2026-10-07: "You should not mix types like that. You should error. Tell people to convert explicitly." VM only; the interpreter keeps coercing, known-gap) |
| D13 (D-fallthrough-return-value + D-bang-io-return) | Value of an action that falls off the end, and of `write_text!` etc. | A `unit` · B `nil` | **B** |
| D14 (D-whole-float-type) | `:valtype(3.0)` | A `int` · B `float` | **B** |
| D15 (D-import-without-alias) | `import a/b/mathy` with no `as` | A namespace `mathy` · B file merge · C error (except `.imports` manifests) | **C** |
| D16 (D-imm) | `imm x \| 1` then `x \|= 2` | A error R0113 · B allowed, drop `imm` | **A** (evidence balanced) |
| D17 (D-join-non-strings) | `:join([1, 2], ",")` (the cur VM now errors too) | A TypeError · B `1,2` | **A** (confirm) |
| D18 (D-int-cast-nonnumeric) | `:int(true)`, `:int(nil)` | A error · B `1` / `nil` | **A** |
| D19 (D-float-div-zero + D-sqrt-negative) | `5.0 / 0`, `:sqrt(-1)` | A error · B `inf` / `NaN` | **A** |
| D20 (D-unclosed-interp-brace) | `"a{bad"` | A error R0500 · B literal text | **A** |
| D21 (D-enum-display) | How `say(Status::Idle)` prints | A `Status::Idle` · B `Status.Idle` | **A** |
| D22 (D-find-on-array) | `:find([5, 6, 7], 6)` | A error (strings only) · B index `1` | **A** |
| D23 (D-keys-on-array) | `:keys([1, 2])` | A error · B `[0, 1]` | **A** (evidence balanced) |
| D24 (D-slice-builtin-end) | End of `:slice(a, s, e)` (a VM-only builtin) | A exclusive for arrays and strings · B inclusive for arrays (VM today) | **A** |
| D25 (D-repeat-negative) | `repeat -1` (the cur VM now errors too) | A error R0207 · B zero iterations | **A** (confirm) |
| D26 (D-toplevel-return) | `return` at the top level of a script | A ignored · B ends the script · C error | **B** |
| D27 (D-format-info-no-thousands) | `th` in `:format_info` when there is no separator | A `"none"` · B `nil` | evidence balanced; **A** |
| D28 (D-multi-return) | `return 1, 2` (cur builds now agree) | A `{_1: 1, _2: 2}` · B `[1, 2]` | **A** (confirm) |
| D29 (D-to-map) | `:to_map({"a": 1})` (cur builds now agree) | A `{{a: 1}}` as today · B the map itself | **B** (confirm) |

---

## D1 · D-update-missing-key: `:update!(m["k"], v)` when the key is absent (DECIDED)

**Status.** This is decided. The case header reads "Decided 2026-10-06 (D-update-missing-key): update! on a missing map key inserts it" (`collections/update_bang_missing_map_key.gbln:1`). Commit a1791ca made the VM insert, and on cur both engines now print `2`. The section below is kept as the record.

**Question.** Does `update!` on a map index insert a missing key, or raise an error?

```
m | {"a": 1}
:update!(m["z"], 2)
say(m["z"])
```
| | base | cur |
|---|---|---|
| interp | `2` | `2` |
| vm | `VM: runtime-error: key not found` | `2` (since a1791ca) |

A nested target (`:update!(r["h"]["b"], 2)` on `{"h": {"a": 1}}`) gives `{h: {a: 1, b: 2}}` on the interpreter. On the VM, base fails to compile (`'update!' index target must be a plain variable`); cur matches the interpreter. Before a1791ca, cur gave `key not found`. In both engines the functional form `:update_at!(n, "z", 3)` errors: interp R0403 "no key 'z'", help "Insert the key with a value before updating it"; VM `key not found`.

**Evidence**
- language-spec.md:2391: `stats["intelligence"] = 8 /// add`, i.e. assigning to a new key adds it. Also language-spec.md:2418: "Update: `m["key"] = value`". Today `:update!(m[k], v)` is the only way to write that.
- The interpreter does this on purpose. lib.rs:17282 has the comment "Any scalar key for maps — auto-inserts Nil if key missing (enables seen[n] |! v)", and lib.rs:17285 calls `map.entry(key).or_insert(Value::Nil)`.
- Campfire depends on insertion:
  - `lib/http.gbln:167` and `:170` build query params by inserting into `params | {}`.
  - `lib/http.gbln:515` sets `res["negotiated"]` and `:538` sets `res["headers"]["Location"]`. Neither key exists in `response()` (`http.gbln:494`).
  - Campfire has 175 `:update!(x[…])` sites in total.
- Sheriff depends on it too:
  - `modules/frontier_fm/frontier.gbln:43-44`: `if :has(fm_map, "categories") == false` / `:update!(fm_map["categories"], [])`.
  - Sheriff has 44 `:update!(x[…])` sites.

**Options**
- **A. Insert (interpreter).** Done on the VM in a1791ca: `update!` now always goes through `Opcode::UpdatePath` (compiler.rs:2343-2410, vm.rs:903). `:update_at!` still errors on a missing key in both engines.
- **B. Error (VM).** In the interpreter, `get_lvalue_mut` (lib.rs:17282-17290) must stop calling `or_insert`. Campfire (≥4 sites) and Sheriff (≥1 site) would then need `:put_at!` or a new insert form. Size: small, plus the app rewrites.

**Recommendation: A.** The spec's own update example adds a key (spec:2391). Both Sheriff and Campfire rely on it, and this is the main blocker to running either on the VM.

---

## D2 · D-missing-key-read: reading a key that is not in the map

**Question.** Does `m["z"]` (or `m.z`) raise an error or give `nil` when `z` is absent?

```
m | {"a": 1}
say "before"
say(m["z"])
say "after"
```
| | base | cur |
|---|---|---|
| interp | `before`, then `R0403 no-such-field: missing key 'z'` | same |
| vm | `before` / `nil` / `after` | same |

`m.zz` behaves the same way: interp `R0403 missing key ‘zz’`, VM `nil`. Maps from `:json_parse` and `:yall_parse_file` behave the same on both engines.

Coalescing is where this matters. `x | m["z"] ?? 5` raises R0403 on the interpreter and gives `5` on the VM.

**Evidence**
- No doc says what a missing key does. Related lines:
  - language-spec.md:1155: "x ?? y yields x when x is non‑nil, else y".
  - language-spec.md:5228: "|> does not swallow nil or errors; combine with ?./?? explicitly".
  - language-spec.md:2369: maps are accessed "via bracket notation only". This conflicts with cheat-sheet.md:1982, `m.key /// dot access (identifier keys)`.
  - cheat-sheet.md:2262 lists `KeyError` among the core error types but never says when it is raised.
- **Sheriff relies on nil.**
  - It writes `m["k"] ?? fallback` **139 times**. Examples:
    - `main.gbln:82`: `post_schedule | cfg["post"] ?? => return post_ctx`
    - `scout.gbln:375`: `href | meta["href"] ?? => return scout_missing_link_info()`
  - On the interpreter the fallback runs only when the key is present and holds nil. A missing key raises.
  - Running the shape of main.gbln:82 (`run_post({"pre": []})`): interp `R0403 missing key 'post'`, VM `no post`.
- Sheriff also wraps 24 reads in `attempt` and comments on the error, for example `prospector.gbln:91` "(but don't die if key missing)" and `:102` "missing key or not a map – ignore". These blocks still work if the read returns nil, because each one tests `.nix?` afterwards or then fails inside the same `attempt`.
- Campfire guards every optional read (`lib/controllers/blobs.gbln:162` `if :has(m, k) == false or m[k] == nil => return nil`). That pattern works under either answer.

**Options**
- **A. Error (interpreter).** Make the VM's `collections::get_index` return `KeyNotFound`:
  - collections.rs:1098 and 1105 (Map, MapOrd);
  - collections.rs:1111-1115 (Collection map layouts, `unwrap_or(Value::Nil)`);
  - the member fallback at vm.rs:3584.

  Sheriff's 139 `??` sites only work while the key is present. Size: small.
- **B. `nil` (VM).** In the interpreter, return `Value::Nil` instead of R0403 in:
  - `Expr::Index`, lib.rs:19295-19311;
  - `Expr::IndexMap`, lib.rs:19356-19372;
  - member read, lib.rs:19724.

  Leave the lvalue paths alone (D1). Size: small.
- **C. Error, except under `??` / `?.`.** The interpreter's `??` (lib.rs:22475) would evaluate an index or member operand in a "missing → nil" mode, and the VM would need a checked and an unchecked `GetIndex`. Size: medium.

**Recommendation: B.** The `??` operator is documented as the fallback for nil (spec:1155), and Sheriff's 139 `m[k] ?? …` sites only make sense if a missing key reads as nil. Choose C if strict reads matter more than that.

---

## D3 · D-block-scope: do `if` and loop bodies scope `|` bindings?

**Question.** Is a binding made inside an `if`, `for` or `while` body local to that block? Does `x | v` inside a block shadow an outer `x`, or overwrite it?

```
x | 1
if true
    x | 2
    say(x)
xx
say(x)
```
| | base | cur |
|---|---|---|
| interp | `2`, `1` | same |
| vm | `2`, `2` | same |

The other cases behave the same way on both builds:
- `if true` / `z | 2` / `xx` / `say(z)`: interp `R0101 unknown identifier z`, VM `2`.
- A declaration in a loop body is visible after the loop: interp R0101, VM `20`.
- `x | 1` / `for x in [7, 8]` / `say(x)` after the loop: interp `1`, VM `8`.
- `x [= 2` inside an `if` (explicit shadow): interp `2,1`, VM `2,2`.

**Evidence**
- The interpreter does this on purpose:
  - lib.rs:5916-5935 (`BindMode::Tether`) says "DECLARE-ONLY in the CURRENT frame … (shadows outer if it exists there)";
  - lib.rs:1115 `push_block` pushes a frame for each block;
  - the parser has an explicit shadow bind, `[=` (goblin-parser lib.rs:2781-2820, `BindMode::Shadow`).
- The spec (language-spec.md:228-241, "Scope") covers only global and operation-local variables and says nothing about blocks.
- **Sheriff is written as if scope were per action.** On the interpreter these lines silently do nothing:
  - `main.gbln:66` and `:94`: `pre_ctx | ret` / `post_ctx | ret` in an `else`, next to the comment "/// keep old pre_ctx" (:64).
  - `main.gbln:141-142`: `best_len | cur_len`, `best_path | md_path`.
  - `scout.gbln:173,201`: `idx | 0` … `idx | idx + 1`, a loop counter.
  - `prospector.gbln:82` and `glams/prospector/prospector.gbln:80`: stripping `.md`.
  - `brindle.gbln:452`: a fallback path.
- I ran the scout shape (`idx | 0` / `for s in [a,b,c]` / `is_last | (idx == :len(segs) - 1)` / `idx | idx + 1`):
  - interp: `a last=false`, `b last=false`, `c last=false`;
  - VM: `… c last=true`.

  The main.gbln:66 shape prints `old` on the interpreter and `new` on the VM.
- Campfire uses `|=` to update outer variables. Its five nested redeclarations (`router.gbln:129`, `pwa.gbln:152/160`, `qrcode.gbln:436`, `ojson.gbln:20`) are local temporaries and work either way.

**Options**
- **A. Block scope (interpreter).** The VM compiler must open a local scope for each block and resolve against it:
  - `declare_local`, compiler.rs:79;
  - `resolve_load` / `resolve_store`, compiler.rs:531 / 578;
  - the loop variable must also be scoped;
  - `[=` must be honoured.

  The Sheriff sites above stay silently broken and should be rewritten with `|=`. Size: medium to large.
- **B. Action scope (VM).** The interpreter stops pushing frames for `if`/loop bodies (`push_block`, lib.rs:1115), and `[=` loses its meaning. Size: medium. It also changes D9 (block-level R0111).
- **C. Block scope, but `x | v` is an error when `x` is already visible from an enclosing block of the same action.** The error would be R0111-style ("use `|=` to update or `[=` to shadow"). In the interpreter this is the Tether arm, lib.rs:5916, which must check outer frames up to the action frame. The VM needs A plus the same check at compile time. Size: medium to large. Sheriff's 7 sites become loud errors instead of silent no-ops.

**Recommendation: C.** Goblin already has an explicit shadow operator, `[=` (parser lib.rs:2781), and Sheriff shows that an implicit shadow made with `|` causes silent bugs (main.gbln:66, scout.gbln:201).

---

## D4 · D-cast-rebinds: does a cast mutate its argument variable?

**Question.** After `s | :str(i)`, is `i` still an int?

```
i | 5
s | :str(i)
say(s)
say(i + 1)
say(:valtype(i))
```
| | base | cur |
|---|---|---|
| interp | `5`, `51`, `str` | same |
| vm | `5`, `6`, `int` | same |

The same split applies to the other casts:
- `:big` / `:i` / `:string` / `:percent` change the variable's type on the interpreter: `big`, `int`, `str`, `pct`, against VM `int`, `str`, `int`, `int`.
- With `f | 2.5; n | :int(f)`, `f` becomes `2` on the interpreter and stays `2.5` on the VM.
- Inside an action, `t | :str(v)` then `v + 1` gives interp `11`, VM `2`.

There is also a crash. Casting a loop variable or counter makes a later `|=` fail with `R0000 internal: const table missing entry` on the interpreter (cases `cast_while_counter_probe`, `cast_in_loop_then_reassign`). Campfire's repro `docs/goblin-repros/while_local.gbln` shows it: interp `R0000 … const table missing entry for 'i'`, VM (cur) `[p0, p1, p2]`.

**Evidence**
- cheat-sheet.md:159: "Casting **never mutates** the original value; it returns a new value or throws."
- The interpreter does it on purpose: lib.rs:20165, "Mutating casts for function form when arg is a plain identifier", followed by `sess.set_var(var_name, out)` at lib.rs:20172.
- Campfire ran into it twice and worked around it both times:
  - GAPS.md:179 (`while_local.gbln`, workaround: "`for` over a list of indexes");
  - GAPS.md:180 (`for_variable_reassign.gbln`, "assign to a new name").
- Campfire has 80 `:cast(identifier)` sites (e.g. `rooms.gbln:50` `[…, :int(id), at]`). In the ~25 I read, none uses the variable afterwards expecting the new type.
- Sheriff has no such sites.

**Options**
- **A. Pure (VM).** Delete the block at interp lib.rs:20164-20175. That also removes the `const table missing entry` crashes listed above. Size: tiny.
- **B. Rebinding (interpreter).** The VM compiler must emit a store back to the identifier after `str/int/float/big/pct/f/i/b/string/percent` calls on a bare identifier, and the interpreter's `set_var` frame bug still has to be fixed. Size: small to medium.

**Recommendation: A.** cheat-sheet.md:159 says casts never mutate, and the rebinding is the cause of two Campfire workarounds.

---

## D5 · D-int-division: `int / int` when the result is not whole

**Question.** What do `10 / 4` and `x /= 4` give?

```
say(10 / 4)
say(1 / 3)
```
| | base | cur |
|---|---|---|
| interp | `2.5`, `0.3333333333333333` | same |
| vm | `2`, `0` | same |

`x | 10; x /= 4` gives interp `2.5`, VM `2`. When the division is exact, both engines give an int: `:valtype(10 / 5)` is `int` (see "Docs vs both engines").

**Evidence**
- The docs say float:
  - language-spec.md:738: `15 / 3 /// 5.0 (always float)`;
  - language-spec.md:801: "/ always float; // integer quotient";
  - language-spec.md:1051: `10 / 3 /// 3.333... (always float)`;
  - cheat-sheet.md:101: `10 / 3 → 3.333... /// float division`.
- Interpreter: lib.rs:21845-21849 gives `Int` when exact and `Float` otherwise.
- VM:
  - `Div` with Int,Int truncates (vm.rs:647-650);
  - the type specialisation rewrites `Div` to `DivInt` (vm.rs:2117, with `DivInt` at vm.rs:668).
- **Campfire relies on float.** `lib/content.gbln:645-646` computes `half | storage::ruby_float(dims[0] / 2)` and `ratio | …(dims[0] / dims[1])` for inline media sizes. With `dims = [301, 200]` the interpreter gives `150.5` and `1.505`; the VM gives `150` and `1`.
- Campfire's helpers `idiv`/`imod` (`http.gbln:19-25`, `qrcode.gbln:74`, `webpush.gbln:31`, `opengraph.gbln:17`) use `:int(:floor(a / b))` and work either way.
- The comment at `http.gbln:17` ("Goblin's % and // always produce floats") is out of date: `7 // 2` and `7 % 2` give `int` on all four runs.
- Sheriff has no `/` arithmetic in loaded code.

**Options**
- **A. Truncate (VM).** The interpreter changes lib.rs:21845-21849 to `Int(a / b)`. Campfire's media dimensions change. Size: tiny.
- **B. Float when inexact, int when exact (interpreter).** In the VM:
  - `Div` with Int,Int returns `Float` when `x % y != 0` (vm.rs:647-650);
  - `DivInt` (vm.rs:668-672) gets the same rule, or the specialisation at vm.rs:2117 is removed;
  - `/=` follows.

  Size: small.
- **C. Always float (docs).** Do B, then also return `Float` when the division is exact, in both engines. This flips the `div_exact_result_type` case. Size: small. Code that indexes with `n / 2` would break, because arrays take only int indexes (Campfire's `http.gbln:17` comment).

**Recommendation: B now.** The docs and Campfire agree on `10 / 4 = 2.5`. The exact case (C) is a separate question, under "Docs vs both engines".

---

## D6 · D-map-key-order: key order of maps

**Question.** Do `keys`, `values`, iteration and printing follow insertion order or sorted order?

```
say(:keys({"b": 1, "a": 2, "c": 3}))
say({"b": 1, "a": 2})
```
| | base | cur |
|---|---|---|
| interp | `[a, b, c]`, `{a: 2, b: 1}` | same |
| vm | `[b, a, c]`, and the map prints as `[1, 2]` (display bug) | `[b, a, c]`, `{b: 1, a: 2}` |

`for p in {"b": 1, "a": 2}` iterates `a, b` on the interpreter and `b, a` on the VM.

The VM is not consistent with itself. Only literal maps keep insertion order. After `:put_at!(m, "a", 2)` on `{"z": 1, "b": 0}`, both engines give `[a, b, z]`. `:json_parse("{\"b\":1,\"a\":2}")` gives keys `[a, b]` on both, and `:json_stringify({"b": 1, "a": 2})` gives `{"a":2,"b":1}` on both.

**Evidence**
- The docs say insertion order:
  - language-spec.md:1420: "/// Map iteration (insertion order)";
  - language-spec.md:1430: "for key, value in map iterates key/value pairs in insertion order";
  - language-spec.md:1694: the same rule;
  - language-spec.md:2382: `prices.keys /// ["sword","shield","potion"]` (not sorted);
  - language-spec.md:2146: `freq [...] /// {sword: 1, potion: 2}`.
- Interpreter: map literals build a `BTreeMap` (lib.rs:19248-19254, `Value::Map`, lib.rs:291). An ordered `Value::MapOrd(IndexMap)` exists (lib.rs:292) and is used for YAML results (lib.rs:1504).
- VM: `MakeMap` keeps pairs in order (vm.rs:785-795). Any operation that goes through `Value::Map` sorts.
- **Campfire wanted insertion order.** GAPS.md:187 says "Maps keep their keys sorted, so JSON loses Rails' key order", and `lib/ojson.gbln:1-4` exists only to work around that.
- Campfire's byte-for-byte page match was validated with sorted order: `ojson.gbln:26` iterates `:keys(v)` for plain maps. A change to insertion order means re-running Campfire's comparisons.
- Sheriff sorts explicitly wherever order matters (`trailboss.gbln:1869-1870`, `routes_json.gbln:345`).

**Options**
- **A. Sorted everywhere (interpreter today).** The VM `MakeMap` (vm.rs:795) builds a sorted map (or `CollectionValue::from_map` sorts). Size: small.
- **B. Insertion order everywhere (docs).**
  - Interpreter: literals build `MapOrd` (lib.rs:19249), and `put_at`/`json_parse`/`json_stringify`/`keys`/`values`/`for` preserve it. That is a broad `BTreeMap` → `IndexMap` move.
  - VM: the same for `Value::Map` paths (collections.rs map arms, `json_*`).
  - Size: large.

**Recommendation: B.** All five doc examples show insertion order, and Campfire had to build `ojson` because order was lost. Re-check Campfire's byte-for-byte pages afterwards.

---

## D7 · D-int-float-equality: `1 == 1.0`

**Question.** Is `==` numeric across int and float?

```
say(1 == 1.0)
say(2 != 2.0)
```
| | base | cur |
|---|---|---|
| interp | `true`, `false` | same |
| vm | `false`, `true` | same |

This shows up in everyday code because some VM builtins return floats. `t | :sum([1, 2]); say(t == 3)` gives interp `true` and VM `false`; the VM's `:valtype(t)` is `float`. `1.5 + 1.5 == 3` gives interp `true` and VM `false`.

**Evidence**
- cheat-sheet.md:109: "numeric 3 == 3.0 is true".
- language-spec.md:908: `3 == 3.0 /// true (numeric equality ignores type)`.
- language-spec.md:911-913: `===` is the strict, type-sensitive form.
- Interpreter: lib.rs:22282-22296 compares numerically.
- VM: `Opcode::Eq` / `Ne` (vm.rs:725-732) uses `Value::PartialEq` (value.rs:200-232), which has no cross-type numeric arm.

**Options**
- **A. `true` (interpreter, docs).** Give the VM's `Eq`/`Ne` opcodes (vm.rs:725-732) a numeric comparison for Int/Float/Big/Pct. Do not change `Value::PartialEq`, because it is also used for map-key hashing (value.rs:199). Size: small.
- **B. `false`.** The interpreter's `==` drops its numeric branch (lib.rs:22288-22299), and the docs change. Size: small.

**Recommendation: A.** Both the cheat-sheet (line 109) and the spec (line 908) say so, and `===` already exists as the strict form.

---

## D8 · D-truthiness: non-bool values in conditions and logic

**Question.** Can `if`, `while`, `and`, `or` and `not` take non-bool values? If so, what do `and` and `or` return?

```
if 1 => say("one")
if "" => say("empty")
if nil => say("nil")
say("end")
```
| | base | cur |
|---|---|---|
| interp | `R0201 if condition requires a boolean value` | same |
| vm | `one`, `end` | same |

`say(nil or 5)`, `say(0 or 5)`, `say(2 and 3)`, `say(not nil)` give interp `T0203 boolean expected` and VM `5`, `5`, `3`, `true`. So the VM's `and`/`or` return an operand, not a bool.

**Evidence (the docs disagree with each other)**
- For booleans only:
  - language-spec.md:1265: "Conditions are boolean expressions; non-boolean values must be compared explicitly";
  - interp lib.rs:2574 ("requires a boolean value") and lib.rs:22410.
- For truthiness:
  - language-spec.md:871: "Goblin also defines truthiness: which non-boolean values behave as true or false in conditionals";
  - language-spec.md:960-1003, the Truthiness Rules, "Falsy: false, 0, 0.0, "", [], {}, nil";
  - language-spec.md:397: "Empty string is falsy in conditionals";
  - VM `JumpIfFalse` (vm.rs:763) and `Value::is_truthy` (value.rs:165).
- No doc says what `and`/`or` return.
- Sheriff and Campfire always write explicit tests, because they were written for the interpreter: `.nix?`, `== false`, `:len(x) > 0`. They work under every option.

**Options**
- **A. Bool required (interpreter).** The VM type-checks `JumpIfFalse`/`JumpIfTrue` and the logic opcodes. Size: small. The strings cases `empty_string_falsy_in_if` and `bool_of_string` become wrong.
- **B. Truthiness in conditions, and `and`/`or`/`not` return a bool.** The interpreter uses the falsy list at lib.rs:2574 and in the logic operators (lib.rs ~22400-22430). The VM's `and`/`or` return a bool. Size: small to medium.
- **C. Truthiness, and `and`/`or` return an operand (VM today).** The interpreter adopts both. Size: small to medium.

**Recommendation: B.** The evidence is balanced: spec:1265 against spec:871, 960-1003 and 397. The spec gives truthiness a whole section with an explicit falsy list, and `and`/`or` returning operands is documented nowhere.

---

## D9 · D-redeclare: `x | 1` then `x | 2` in the same scope

```
x | 1
x | 2
say(x)
```
| | base | cur |
|---|---|---|
| interp | `R0111 duplicate-local: 'x' is already declared in this block`, help "Use '\|=' to reassign an existing variable." | same |
| vm | `2` | same |

**Evidence**
- The interpreter's error is deliberate, with its own code and help text (lib.rs:5921-5932).
- The docs use `=` for both declaring and reassigning (language-spec.md:229-236), so they don't address this.
- Loaded Sheriff and Campfire code never redeclares (they run on the interpreter).
- Sheriff's `sawbones.gbln:167-169` (`t | text` / `t | sb_remove_bom(t)`) does redeclare, but it is not imported by `manifest.imports`.

**Options**
- **A. Error (interpreter).** The VM compiler rejects a second `|` of a name already declared in the same scope: `declare_local`, compiler.rs:79. Size: small.
- **B. Rebind (VM).** Delete the R0111 check in the interpreter's Tether arm (lib.rs:5921-5933). Size: tiny.

**Recommendation: A.** `|` and `|=` are separate operators, and the interpreter's help text names `|=` as the way to reassign. This also fits D3 option C.

---

## D10 · D-get-matching-empty: `:get_matching` with no match

```
say "before"
say(:get_matching(["a", "b"], "[0-9]"))
```
| | base | cur |
|---|---|---|
| interp | `before`, then `R0701 empty-collection: pattern did not match any elements` | same |
| vm | `before`, `[]` | same |

**Evidence**
- No docs.
- **Campfire wants `[]`:**
  - `lib/html.gbln:137-141` has a wrapper commented "/// :get_matching raises when nothing matches; this returns [] instead";
  - `lib/platform.gbln:53-55` guards with `:is_matching` and then checks `:len(found) == 0`;
  - `lib/storage.gbln:168-169` checks `found == nil or :len(found) == 0` without a guard, so it would raise on the interpreter when nothing matches.

**Options**
- **A. Error (interpreter).** The VM raises at collections.rs:170 and collections.rs:441 (`Matching` + `Get`) when the result is empty. Size: tiny.
- **B. `[]` (VM).** The interpreter returns an empty array instead of R0701, in the array arm (lib.rs ~11063) and the map arm. Size: tiny.

**Recommendation: B.** Campfire wrote a wrapper only to get `[]`, and `storage.gbln:168` already assumes it.

---

## D11 · D-negative-int-division: `//` and `%` with a negative operand

```
say(-7 // 2)
say(-7 % 3)
```
| | base | cur |
|---|---|---|
| interp | `-4`, `2` | same |
| vm | `-3`, `-1` | same |

**Evidence**
- The docs only show positive operands (language-spec.md:739-740, 1052-1053; cheat-sheet.md:102).
- The interpreter calls this "floor division" in its diagnostics (lib.rs:22044-22052) and computes `r = a - floor(a/b) * b` for `%` (lib.rs:21993-22000).
- Campfire's own integer helpers implement floor semantics: `imod(a, b) = a - :floor(a / b) * b` (`http.gbln:23-25`), and `idiv` floors in `http.gbln:19-21`, `qrcode.gbln:73-75`, `webpush.gbln:31` and `opengraph.gbln:17`.
- `int 3.9 → 3 (truncate toward 0)` (cheat-sheet.md:166) is about casts, not division.

**Options**
- **A. Floor (interpreter).** In the VM, `DivInt` (vm.rs:668), `Div` with Int,Int (vm.rs:647), `Rem` (vm.rs:677) and `RemInt` (vm.rs:690) use `div_euclid`-style floor arithmetic, and the Float arms do the same. Size: small.
- **B. Truncate (VM).** Change the interpreter's `//` and `%` arms (lib.rs:21993, 22044). Size: small.

**Recommendation: A.** The interpreter names the operator floor division, and Campfire's hand-written helpers floor.

---

## D12 · D-string-plus-nonstring: `"a" + 1` (the engines agree, the docs disagree)

```
say("a" + 1)
say("n=" + 2.5)
say("b" + true)
```
Both engines on both builds print `a1`, `n=2.5`, `btrue`.

**Evidence**
- Against coercion:
  - language-spec.md:22: "No silent string coercion";
  - language-spec.md:31: `/// say "Score: " || score /// TypeError`;
  - language-spec.md:574: "joins require strings (convert explicitly or interpolate)".
- For coercion: both implementations coerce on purpose (interp lib.rs ~21670-21677 `(Value::Str(a), other)`, VM vm.rs:592-593).
- Campfire mostly converts explicitly (`room_admin.gbln:231` `rid | :str(...)`), but `boosts.gbln:59-61` does `:int(id)` and then `"boost_" + id`. On the interpreter `id` is an int by then (D4), so that line relies on coercion today.

**Options**
- **A. Keep coercion and correct the docs.** No engine change.
- **B. TypeError.** Remove the mixed `Str` arms in interp lib.rs ~21670-21677 and VM vm.rs:592-593. Size: tiny, but programs must be audited.

**Recommendation: A.** Both engines implement the coercion deliberately and existing code depends on it. The question is really whether the docs or the implementation are canonical.

---

## D13 · D-fallthrough-return-value + D-bang-io-return: `unit` or `nil`

These two slugs ask the same thing: what value a statement with no result produces.

```
act fx(x)
    if x > 0
        return "pos"
    xx
xx
say(:valtype(fx(-1)))
```
| | base | cur |
|---|---|---|
| interp | `unit` | same |
| vm | `nil` | same |

Two more programs give the same split:
- `r | :write_text!(path, "x")`, then `:is_nil(r)` and `:vt(r)`: interp `false`, `unit`; VM `true`, `nil` (io case).
- An action that falls through, then `"[{r}]"`: interp `[]`, VM `[nil]`.

**Evidence**
- The docs have no `unit` value. The primitives are int, float, bool, string and nil (cheat-sheet.md:134-141).
- language-spec.md:1769 ends an op with `nil /// explicit final return`.
- The response builtins `set_status`/`set_header`/`set_cookie` return nil on both engines (io NOTES).
- Interpreter: action bodies start from `let mut last = Value::Unit` (lib.rs:11373, 11503, 11577), and the bang file builtins `return Ok(Value::Unit)` (e.g. lib.rs:18083 for `write_text!`).
- VM: the implicit return is `LoadNil` (compiler.rs:392-407), and builtins return `Ok(Value::Nil)` (e.g. builtins.rs:1082).

**Options**
- **A. `unit` (interpreter).** The VM returns `Value::Unit` from implicit returns (compiler.rs:392-407) and from the bang builtins (builtins.rs WriteText/AppendFile/CreateDir/CopyFile/DeletePath/ZipDir/WriteJson). The display and `is_nil` rules for Unit must also be documented. Size: small to medium.
- **B. `nil` (VM, docs).** The interpreter converts `Unit` to `Nil` where it escapes to user code: action results (lib.rs:11373/11503/11577) and the bang builtins in lib.rs ~17733-18170. Size: medium; there are 51 `Value::Unit` sites to review.

**Recommendation: B.** `unit` appears nowhere in the docs, and the spec's own example returns `nil` explicitly.

---

## D14 · D-whole-float-type: is `3.0` an int or a float?

```
say(:valtype(:float(3)))
say(:valtype(1.5 + 1.5))
say(:is_float(2.5 * 2))
say(:valtype(3.0))
```
| | base | cur |
|---|---|---|
| interp | `int`, `int`, `false`, `int` | same |
| vm | `float`, `float`, `true`, `float` | same |

**Evidence**
- cheat-sheet.md:168: `float 3 /// 3.0`.
- language-spec.md:760: `5.float /// 5.0`.
- The interpreter is deliberate: lib.rs:12368, `Value::Float(n) if n.is_finite() && n.fract() == 0.0 => "int"`, with the same rule in `is_int`/`is_float`.
- Printing is not part of this question. Both engines print `1.0` as `1` (`fmt_num_trim`).
- Neither Sheriff nor Campfire calls `:is_float`/`:is_int` or compares `valtype` with `"int"`/`"float"`.

**Options**
- **A. `int` (interpreter).** The VM's `valtype`/`is_int`/`is_float` check `fract() == 0`. Size: tiny.
- **B. `float` (VM).** Remove the whole-float arm at interp lib.rs:12368 and its twins in `is_int`/`is_float`. Size: tiny.

**Recommendation: B.** The docs show `float 3` giving `3.0`, a float. This is independent of D7, which makes `3.0 == 3` true either way.

---

## D15 · D-import-without-alias

```
import _support/modules/mathy
say(mathy::add(2, 3))
```
| | base | cur |
|---|---|---|
| interp | `5` | same |
| vm | import path error (the cwd bug, fixed in cur) | `undefined variable: 'mathy::add'` |

**Evidence**
- language-spec.md:4885: "Every import requires an alias; access via Alias::name".
- Interpreter: the last path segment becomes the namespace (modules.rs:124-128).
- VM: vm.rs:1601 says "`import` is a file merge, not a namespace load".
- Campfire writes all 198 imports with `as`.
- Sheriff's only unaliased import is the manifest, `main.gbln:3` `import "../sheriff-core/manifest.imports"`, which must keep working.

**Options**
- **A. Namespace from the last segment (interpreter).** The VM compiler (compiler.rs:805 `Stmt::Import`) emits `ImportFileAs` with the derived alias. Size: small.
- **B. File merge (VM).** The interpreter's `Stmt::Import` loads into the importer's namespace. Size: medium.
- **C. Error unless the target is a `.imports` manifest.** Add a parser or loader check in both engines. Size: small.

**Recommendation: C.** The spec requires an alias (line 4885), and no `.gbln` import in either app omits one.

---

## D16 · D-imm

```
imm x | 1
say(x)
x |= 2
say(x)
```
| | base | cur |
|---|---|---|
| interp | `1`, then `R0113 cannot reassign immutable 'x'` | same |
| vm | `1`, `2` | same |

**Evidence**
- Against `imm`:
  - cheat-sheet.md:62: "no immutable variables or constants";
  - language-spec.md:244: "Goblin does not enforce immutability".

  Both lines are in sections written in the older `=` syntax.
- For `imm`:
  - the parser accepts `imm` (goblin-parser lib.rs:2663);
  - the interpreter enforces it with its own error code R0113 (lib.rs:3784, 3919).
- The VM never reads `is_imm`.
- Neither app uses `imm`.

**Options**
- **A. Enforce.** The VM compiler records `is_imm` per local or global and rejects `|=` on it: `resolve_store`, compiler.rs:588. Size: small.
- **B. Drop `imm`.** Remove it from the parser (lib.rs:2663, 5771) and the interpreter's R0113 paths. Size: small.

**Recommendation: A.** The evidence is balanced. The keyword and its error code are implemented features, while the contrary doc lines sit in sections already stale on syntax.

---

## D17 · D-join-non-strings

This slug overlaps `strings/join_non_string_errors`, which is already marked `known-gap: vm` on the strength of the doc line below. On cur both engines now agree on the error, after commit 6bb23a1 ("collection-aware builtins").

```
say(:join([1, 2], ","))
```
| | base | cur |
|---|---|---|
| interp | `T0205 ‘join’ expects an array/seq of strings or chars` | same |
| vm | `1,2` | `type error in join: expected str or char element, got int` |

**Evidence.** language-spec.md:556: "join accepts an array of strings and a string separator; any non-string element ⇒ TypeError".

**Options**
- **A. TypeError.** Already the case on cur (VM `Join`, builtins.rs:430). Nothing to change.
- **B. Stringify.** Both engines' `join` stringify elements: interp lib.rs ~15800, VM builtins.rs:430. The strings case flips. Size: tiny.

**Recommendation: A.** That is what spec:556 says. Please confirm, so both cases can get a `.out` file.

---

## D18 · D-int-cast-nonnumeric: `:int(true)`, `:int(nil)`

| | base | cur |
|---|---|---|
| interp | `R0316 int(): cannot cast value to int`, help "Valid inputs: Int, Float, Pct, Big, or numeric String." (both calls) | same |
| vm | `1`; `nil` | same |

**Evidence**
- cheat-sheet.md:216: "**TypeError** for unsupported conversions".
- cheat-sheet.md:217: "Casting is explicit".
- bool and nil are not mentioned.

**Options**
- **A. Error.** The VM's `ToInt`/`Int` (builtins.rs:2234) rejects Bool and Nil. Size: tiny.
- **B. `1`/`0` and `nil`.** The interpreter adds the arms. Size: tiny.

**Recommendation: A.** The docs make unsupported conversions an error, and the interpreter's help text lists the valid inputs.

---

## D19 · D-float-div-zero + D-sqrt-negative: errors or IEEE values

```
say(5.0 / 0)
say(:sqrt(-1))
```
| | base | cur |
|---|---|---|
| interp | `R0206 division by zero`; `R0207 sqrt(): cannot take square root of a negative value` | same |
| vm | `inf`; `NaN` | same |

**Evidence**
- cheat-sheet.md:2262 lists `ZeroDivisionError` as a core error type.
- Integer `/ 0` errors on both engines (vm.rs:648).
- Neither app divides floats by zero or calls `:sqrt`/`:pow`.

**Options**
- **A. Error.** The VM's `Div` with Float operands (vm.rs:651-653), `DivFloat` (vm.rs:673) and `Sqrt` (builtins.rs:191) raise on zero or negative input. Size: tiny.
- **B. IEEE values.** The interpreter returns `inf`/`NaN`, and the docs gain rules for printing and comparing them. Size: small.

**Recommendation: A.** `ZeroDivisionError` is a documented core error, and the VM already raises it for integers.

---

## D20 · D-unclosed-interp-brace

```
say("a{bad")
```
| | base | cur |
|---|---|---|
| interp | `R0500 unclosed '{' in interpolated string`, help `Use "\{" to render a literal '{'` | same |
| vm | `a{bad` | same |

**Evidence**
- language-spec.md:461 documents `{{ }}` for literal braces, which neither engine implements (see "Docs vs both engines").
- Campfire follows the interpreter's convention and writes `\{` 28 times (e.g. `http.gbln:447`).
- Sheriff writes `\{` 3 times.

**Options**
- **A. Error.** The VM's `render_string_interp` (vm.rs:1916) raises when no `}` follows. Size: tiny.
- **B. Literal text.** Remove the R0500 branch from the interpreter's `render_interpolated` (lib.rs:3321-3335). Size: tiny.

**Recommendation: A.** Existing code already escapes braces with `\{`, and an error catches typos such as `"{name"`.

---

## D21 · D-enum-display

```
enum Status
    Idle
    Busy
xx
say(Status::Idle)
say(:str(Status::Busy))
```
| | base | cur |
|---|---|---|
| interp | `Status::Idle`, `Status::Busy` | same |
| vm | `Status.Idle`, `Status.Busy` | same |

**Evidence**
- The cheat-sheet writes `Status.Pending` (cheat-sheet.md:1485), but dotted access is a parse or unknown-identifier error on both engines (modules NOTES).
- `Status::Idle` is the access syntax both engines accept.

**Options**
- **A. `::`.** Change the VM's display at builtins.rs:5250 and debug.rs:209. Size: tiny.
- **B. `.`.** Change the interpreter at lib.rs:2774 (and the JSON form at lib.rs:1738). Size: tiny.

**Recommendation: A.** A printed value then matches the syntax that reads it back.

---

## D22 · D-find-on-array

```
say(:find([5, 6, 7], 6))
```
| | base | cur |
|---|---|---|
| interp | `T0205 find expects a string.` | same |
| vm | `1` | same |

**Evidence**
- `find` is documented only for strings: cheat-sheet.md:448 ("first start index (0-based) or nil"), language-spec.md:504.
- The VM also has a separate `find_index` builtin (VM-only, see below).

**Options**
- **A. Strings only.** The VM's `Find` (builtins.rs:311) rejects arrays. Size: tiny.
- **B. Arrays too.** Add an array arm to the interpreter's `find`. Size: tiny.

**Recommendation: A.** The docs define `find` only for strings. Arrays can use `find_index` once D-VM-only builtins are settled.

---

## D23 · D-keys-on-array

```
say(:keys([1, 2]))
```
| | base | cur |
|---|---|---|
| interp | `T0205 ‘keys’ expects a Map.` | same |
| vm | `[0, 1]` | same |

**Evidence.** There are no docs. cheat-sheet.md:1984 shows `keys` only on a map.

**Options**
- **A. Error.** The VM's `Keys` (builtins.rs:1108) rejects arrays. Size: tiny.
- **B. Indices.** The interpreter's `actions::maps::keys` returns `0..len`. Size: tiny.

**Recommendation: A.** The evidence is balanced, so this follows the interpreter as source of truth (AGENTS.md:22) and the docs' map-only usage.

---

## D24 · D-slice-builtin-end: `:slice(a, s, e)`

```
say(:slice([1, 2, 3, 4], 1, 3))
say(:slice("abcd", 1, 3))
say([1, 2, 3, 4][1:3])
```
| | base | cur |
|---|---|---|
| interp | `A0401 unknown action ‘slice’` | same |
| vm | `[2, 3, 4]`, `bc`, then `slice expects array or string, got collection` | `[2, 3, 4]`, `bc`, `[2, 3]` |

The VM's `:slice` includes the end for arrays but excludes it for strings, while `[s:e]` excludes it for both.

**Evidence**
- `slice` is undocumented.
- language-spec.md:2399: "Slicing: `a[s:e]` (end exclusive)".
- VM: builtins.rs:2056, which goes to `grab_between` (inclusive) for arrays.

**Options**
- **A. Exclusive for both, then add `slice` to the interpreter.** Change the VM's array path in `BuiltinId::Slice` (builtins.rs:2056) and add an interpreter builtin. Size: small.
- **B. Keep the VM's split.** Port it to the interpreter. Size: small.

**Recommendation: A.** It matches the documented `[s:e]` slicing and the builtin's own string behaviour.

---

## D25 · D-repeat-negative

```
repeat -1
    say("no")
xx
say("done")
```
| | base | cur |
|---|---|---|
| interp | `R0207 'repeat' count must be >= 0 (got -1).` | same |
| vm | `done` | `runtime error: 'repeat' count must be >= 0 (got -1)` (since fcd81fe) |

**Evidence.** There are no docs (the cheat-sheet's Repeat section, lines 1249-1275, gives only positive counts). The interpreter's check is explicit (lib.rs:20557).

**Options**
- **A. Error.** Already the case on cur (fcd81fe, "VM: repeat matches interpreter modes"). Nothing to change.
- **B. Zero iterations.** Remove the check in both engines. Size: tiny.

**Recommendation: A** (please confirm). The evidence is balanced, and both engines now raise the error.

---

## D26 · D-toplevel-return

```
say("x")
return 5
say("y")
```
| | base | cur |
|---|---|---|
| interp | `x`, `y` (the `return` is ignored) | same |
| vm | `x` | same |

**Evidence**
- There are no docs.
- Neither app has a top-level `return`.

**Options**
- **A. Ignore (interpreter).** The VM compiler drops a top-level `Return`. Size: tiny.
- **B. End the script (VM).** The interpreter treats a top-level `CtrlReturn` as end of program. Size: small.
- **C. Parse or compile error.** Size: small, in both engines.

**Recommendation: B.** Option A silently ignores a statement, which is the one outcome nothing argues for.

---

## D27 · D-format-info-no-thousands

```
say(:format_info(:format(2.5, 2)))
```
| | base | cur |
|---|---|---|
| interp | `{dec: 2, decmark: ., th: none}` | same |
| vm | `{dec: 2, decmark: ., th: nil}` | same |

**Evidence**
- There are no docs.
- The parser accepts the bare word `none` as the "no separator" spelling in `format(...)` (strings NOTES).
- `none` exists as an interpreter-only name in the builtin inventory.

**Options**
- **A. `"none"`.** Change the VM at builtins.rs:3510. Size: tiny.
- **B. `nil`.** Change the interpreter at lib.rs:13157. Size: tiny.

**Recommendation: A.** The evidence is balanced. `"none"` matches the spelling `format` accepts.

---

## D28 · D-multi-return (the current builds agree; please confirm)

```
act fx()
    return 1, 2
xx
say(fx())
```
| | base | cur |
|---|---|---|
| interp | `{_1: 1, _2: 2}` | same |
| vm | `[1, 2]` | `{_1: 1, _2: 2}` |

**Evidence.** The docs only show destructuring a divmod (`q, r = 10 >> 3`, language-spec.md:1054).

**Options**
- **A. Map `{_1, _2}` (both engines on cur).** No change needed.
- **B. Array.** Change the interpreter's multi-value `return` and the VM back. Size: small.

**Recommendation: A.** It needs no change. Please confirm so the case can get a `.out` file.

---

## D29 · D-to-map (the current builds agree; please confirm)

```
say(:to_map({"a": 1}))
```
| | base | cur |
|---|---|---|
| interp | `{{a: 1}}` | same |
| vm | `{}` | `{{a: 1}}` |

**Evidence**
- `to_map` is undocumented.
- The interpreter's `cast_to_map` (lib.rs:2093) wraps the map, and the VM's `ToMap` is at builtins.rs:3073.

**Options**
- **A. Keep `{{a: 1}}`.** No change.
- **B. Return a map argument unchanged.** Make `cast_to_map` the identity for Map/MapOrd, and do the same in the VM. Size: tiny.

**Recommendation: B.** Casting a value to its own type should return it unchanged, as `int 5 → 5` does (language-spec.md:759).

---

## D30 · D-interp-backslash (DECIDED 2026-10-06: A, escape once)

```
x | 1
say("a\\\\b {x}")        /// interp: a\b 1    VM: a\\b 1
say("a\\\\b")            /// both:   a\\b
say("""p="\\S" {x}""")  /// interp: p="\S" 1  VM: p="\\S" 1
```

The interpreter runs the escape `\\` a second time when a string has a `{placeholder}`, so the same backslashes print differently depending on whether a placeholder is present. The VM processes escapes once, in the lexer. Triple-quoted strings take no escapes in the lexer (`goblin-lexer/src/lib.rs:989`, "No transforms, no escape handling"; language-spec.md:404 "preserves newlines & indentation literally"), but the interpreter unescapes `\\` in them when they contain a placeholder.

- **A. Once (VM).** A backslash prints the same with or without a placeholder. Campfire's boost form (`lib/views/message_edit.gbln:89`, `pattern="\\S+.*"` inside a `"""` template) then has to write `\S`, the way Rails does. Interpreter change: the placeholder renderer stops turning `\\` into `\` (it keeps `\{` and `\}`).
- **B. Twice (interpreter).** Campfire stays as it is. The VM's renderer has to unescape `\\` too.

**Decided: A** (owner, 2026-10-06): the VM's behaviour is canonical. The interpreter's placeholder renderer keeps `\{`/`\}` as literal braces and leaves every other backslash as written. Campfire's boost form now writes `\S`. Case: `strings/interp_backslash_same_with_placeholder`.

## Docs vs both engines (`known-gap: both`)

**Owner ruling, 2026-10-06:** the docs predate Goblin and are not canonical. Where both engines agree, their current behaviour is what Goblin does, and the cases now expect it. These were re-encoded: mode, between, `"  7 ".int`, `nil.str`, descending ranges, `-2 ** 2`, `lines` with CRLF, `pct`, `raw` escapes, `split` with "", `trim_lead`, `{{ }}`, `4.inc`, `parse_bool` yes/no, string `reverse`, import cycles, and unqualified module globals. The items below that are still open are ones where the engines disagree, or where both give an error or a wrong value.

In these cases both engines agree and the docs say something else. For each one: is the doc or the implementation canonical? They are lower priority than D1-D29. "Rec" is my recommendation. Outputs are the same on base and cur unless noted.

**Ranges and `between`.** Cases: `for_range_inclusive`, `for_range_exclusive`, `for_range_variable_bounds`, `for_range_descending`, `range_literal_as_array`, `between_range_check`.
- Docs: `..` is inclusive and `...` exclusive (cheat-sheet.md:608-609, 1142; language-spec.md:1439-1447, 1464 "Numbers: .. inclusive end, ... exclusive end; descending supported"). `between` is inclusive (cheat-sheet.md:120, language-spec.md:1381 "a <= x <= b").
- Both engines do the opposite: `for i in 1..3` gives `1, 2`; `1...3` gives `1, 2, 3`; `5..3` gives nothing; `5 between 1..5` is `false`.
- The parser marks the flip as deliberate: goblin-parser lib.rs:1574/1577 and 3624/3627, `// CHANGED: ".." => exclusive`.
- Neither app uses range literals.
- **Rec:** the implementation is canonical (the `CHANGED` comments record an intentional decision). Update the docs and flip the six cases. Descending ranges and an inclusive `between` still need their own answer.

**`/` result type when the division is exact.** Case: `div_exact_result_type`.
- Docs: `/` always yields a float (language-spec.md:738, 801, 1051; cheat-sheet.md:101).
- Both engines: `10 / 5` gives `2`, and its `valtype` is `int`. The interpreter does this deliberately (lib.rs:21845).
- **Rec:** decide together with D5. If D5 is B, the implementation is canonical and the docs change.

**Raw strings.** Case: `raw_single_no_escapes`.
- Docs: `raw "…"` means no escapes and no interpolation (language-spec.md:413).
- Both engines: `raw "a\tb"` contains a TAB and has length 3.
- **Rec:** the doc is canonical; without that, `raw` does nothing.

**Interpolation forms.** Cases: `interp_double_brace_literal`, `interp_zero_arg_method`.
- Docs: `{{`/`}}` give literal braces (language-spec.md:461); `"{name.slug}"` is allowed (language-spec.md:459).
- Both engines print `{{…}}` unchanged and do not interpolate inside it; `{name.slug}` is left as literal text.
- **Rec:** the doc is canonical for `{{ }}` (D20 then decides what a lone `{` does). The implementation is canonical for `{name.slug}` until expression interpolation is designed.

**`trim_lead` on a literal.** Case: `trim_lead_literal_dedent`.
- Docs: removes the common leading indent (language-spec.md:418, cheat-sheet.md:514).
- Both engines strip all leading whitespace from each line.
- **Rec:** the doc is canonical.

**`minimize`.** Case: `minimize_removes_all_whitespace`.
- Docs: "remove ALL whitespace", giving `"HelloWorld"` (language-spec.md:486, cheat-sheet.md:412).
- Both engines collapse runs to one space (`Hello World`). The interpreter does this deliberately (lib.rs ~15983-15995).
- **Rec:** the implementation is canonical. Fix the docs.

**`split` with an empty delimiter.** Case: `split_empty_delimiter_errors`.
- Docs: ValueError (cheat-sheet.md:538).
- Both engines split into characters (`[a, ,, b]`).
- **Rec:** the doc is canonical (`chars` already does the character split).

**`lines` with `\r\n`.** Case: `lines_crlf`.
- Docs: "universal newlines" (cheat-sheet.md:439).
- Both engines leave `y\r` as a 2-character line.
- **Rec:** the doc is canonical.

**`reverse` on a string.** Case: `reverse_string_method`.
- Docs: `"daniel".reverse` gives `"leinad"` (cheat-sheet.md:407).
- Both engines raise a type error; `reverse_chars` works.
- **Rec:** the doc is canonical.

**`chars` elements.** Case: `chars_element_as_needle`.
- Docs: `chars "abc"` gives `["a","b","c"]` (language-spec.md:544).
- Both engines return Char values, which `find` rejects. The VM's message is "expected str or array, got str".
- **Rec:** the doc is canonical. Either Char is accepted wherever a 1-character string is, or `chars` returns strings.

**`pct` constructor.** Case: `pct_constructor_points`.
- Docs: `pct 25` is `25%` (cheat-sheet.md:1731).
- Both engines: `:pct(25) == (25%)` is `false`.
- **Rec:** the doc is canonical.

**Large whole floats.** Case: `float_large_not_saturated`.
- Docs: nothing specific.
- Both engines print `:str(1e20)` and `:str(1.5e300)` as `9223372036854775807`.
- **Rec:** this is a bug in both (`fmt_num_trim` casts to i64), not a semantic question.

**Cast details.** Cases: `cast_int_untrimmed_string`, `cast_str_nil`, `parse_bool_yes_no`.
- Docs:
  - `"  7 ".int` is a ValueError (cheat-sheet.md:171);
  - `nil.str` is `""` (cheat-sheet.md:179);
  - `parse_bool` also accepts yes/no/1/0 (cheat-sheet.md:195).
- Both engines:
  - give `7`;
  - give `[nil]` for `"[" + :str(nil) + "]"`;
  - reject `"yes"`.
- **Rec:** the doc is canonical for all three. They are small, explicit rules.

**`x++`.** Case: `postfix_increment`.
- Docs: `x++` returns the old value, then increments (cheat-sheet.md:97, language-spec.md:753).
- Engines: the interpreter gives `4,4`, the VM `4,3`. They also disagree with each other.
- **Rec:** the doc is canonical.

**`**` and unary minus.** Case: `unary_minus_pow_precedence`.
- Docs: pow binds tighter than unary minus (language-spec.md:1209; cheat-sheet.md:74-75).
- Both engines give `-2 ** 2 = 4`.
- **Rec:** the doc is canonical.

**`2 ** 64` overflow.** Case: `pow_overflow`.
- Docs: none specific.
- Engines: the interpreter gives `9223372036854775807`, the VM `0`. Both promote `+` and `*` to big.
- **Rec:** promote to big. Both engines already do that for `+` and `*`.

**`attempt`/`ensure` without `rescue`.** Case: `attempt_ensure_without_rescue_propagates`.
- Docs: "If no rescue matches, the error propagates after running ensure" (language-spec.md:5326).
- Both engines print `a e after`, so the error is swallowed.
- **Rec:** the doc is canonical.

**`judge using`.** Case: `judge_using_value`.
- Docs: the form is used in `tests/judge-repeat.gbln`.
- Engines: the interpreter raises R0201; the VM runs the first arm (`L`).
- **Rec:** the doc and test are canonical (it should print `G`).

**Zero-arg user method without parens.** Case: `method_call_user_action_no_parens`.
- Docs: `10.double` (language-spec.md:1791; cheat-sheet.md:325).
- Both engines reject `4.inc`.
- **Rec:** the doc is canonical.

**Variables named after builtins.** Cases: `builtin_named_var_indexed_*`, `builtin_name_variable_*`.
- Docs: builtins are "shadowable" (language-spec.md:184; cheat-sheet.md:2346).
- Both engines parse `count[0]`, `words[1]` and `count + 1` as builtin calls. Results: `1`, a type error, and "unknown prefix operator".
- **Rec:** the doc is canonical. A bound variable must win over a builtin name.

**Module rules.** Cases: `import_alias_conflict_errors`, `import_cycle_errors`, `module_global_not_visible_unqualified`, `module_variable_qualified_read_is_live`.
- Docs:
  - duplicate alias → ModuleNameConflictError (language-spec.md:4926);
  - cycles → ModuleCycleError (language-spec.md:4935-4936);
  - access via `Alias::symbol` only (language-spec.md:4817).
- Engines (cur):
  - a duplicate alias is accepted (the interpreter keeps the first, the VM the last);
  - a cycle runs (`a`);
  - a module global is visible unqualified (`0`);
  - `st::counter` is R0117 on the interpreter and reads `0 0` on the VM.
- **Rec:** the doc is canonical for the alias conflict and for qualified-only access. For cycles, the evidence is balanced: they work today, so you may prefer to allow them.

**`mode`.** Case: `mode_single`.
- Docs: `mode [1, 2, 2, 3]` gives `[2]` (language-spec.md:2147).
- Engines: the interpreter gives `{2: 2}`. The VM gave `[2]` on base and gives `{2: 2}` on cur, so cur changed the VM to match the interpreter and away from the doc.
- **Rec:** the doc is canonical ("most common value(s)").

## Owner rulings, 2026-10-06 20:35

Remaining VM gaps after the docs were set aside. Implemented on the VM (and in the shared lexer/parser where noted); where the interpreter differs the case is a `known-gap: interp`, since the interpreter is going away.

| case | ruling | change |
|---|---|---|
| `strings/raw_single_no_escapes`, `strings/raw_no_interpolation` | "Raw literally means keep what's in there." | Lexer: the literal after `raw` takes no escapes. VM: `raw "…"` loads the literal as written (no interpolation); `:raw(x)` returns x unchanged (it used to double braces). |
| `collections/reap_sentence_from`, `collections/reap_sentence_count` | "Fix reap from." | Parser: `reap [n] from var` lowers to the destructive `reap!`, so the taken items leave the source. |
| `core/attempt_ensure_without_rescue_propagates` | "Give an error." | VM: with no rescue, the ensure block runs and the error is raised again. |
| `core/judge_using_value` | "Judge is supposed to pick the first true arm. Judge all picks all true arms." | Parser: under `judge using x`, a literal arm `"go":` means `x == "go"`; judge takes the first true arm. |
| `strings/chars_element_as_needle` | "Chars. Do whatever you want." | VM: string-only builtins accept a char as a one-character string. |
| `strings/float_large_not_saturated` | "Fix big." | VM: whole floats past i64 print all their digits instead of 9223372036854775807. |
| `core/postfix_increment` | "Fix increment." | VM: `x++` / `x--` store the new value and evaluate to it (as the interpreter did). |
| `core/pow_overflow` | "Do what's best for exponent." | VM: an integer power past i64 is promoted to big, like `+` and `*`. |
| `modules/import_alias_conflict_keeps_first` | "Import the first and provide a warning." | VM: a second module under an alias already used in the file is skipped with a warning on stderr. |
| `modules/module_variable_qualified_read_is_live` | (approved with the batch) | VM: `alias::var` reads the module's global as it is now. |
| `collections/slice_builtin` | D24 A | VM: `:slice(a, s, e)` excludes the end for arrays as for strings. |

Variables named like builtins (owner, 21:03: "Good. Allow the : on all builtin functions."): a bound variable wins (`count[0]`, `count + 1` use the variable; parser `bound_names`), and `:name(...)` always reaches the builtin. Cases: `builtin_named_var_*`, `builtin_name_variable_*`, `builtin_named_var_colon_calls_builtin`.

## Brackets for arrays, braces for maps (owner, 2026-10-06 21:08)

"We need to do brackets for arrays and braces for maps … Give an easy error if you use the wrong one."
- Arrays and strings are indexed with `[]`, maps with `{}`, for reads and for writes through a path (`update!`, `name!(x{k}…)`, `|=` targets).
- The other bracket is a runtime error naming the value: `` `m` is a map. Use {} for maps: m{"key"} ``, `` `a` is an array. Use [] for arrays: a[0] ``.
- VM opcodes `IndexGet` / `KeyGet` (reads) and `CheckPath` (writes). The interpreter still accepts `[]` on maps (known-gap: interp).
- Cases: `collections/bracket_on_map_*`, `collections/brace_on_*`. 74 existing case files (including _support modules) were rewritten from `m["k"]` to `m{"k"}`.
- Sheriff (about 384 `m["key"]` sites) is to be refactored later; Campfire uses `m["key"]` throughout.

## VM-only builtins

**Decided by the owner, 2026-10-06.** The interpreter will be removed once the VM works, so nothing is added to it.
- Removed from the VM (cases in `removed/` check that neither engine has them):
  - to_int, to_float, to_str, to_string, to_bool, to_upper, to_lower (use int, float, str, bool, upper, lower);
  - the grab* family (use get*);
  - find_index, flatten, zip, is_empty, pairs, put_all, put_where, reap_all, reap_random, is_collection, array_push, contains;
  - replace, pad, pad_left, pad_right, repeat_str;
  - print, println, eprint, eprintln, panic, assert;
  - ipsum*;
  - type_of (use valtype).
- Kept on the VM: filter, filter_fn, map_fn, reduce, reduce_fn, for_each_fn, sort_by, any, all, range, slice, url_encode, url_decode, http_*, render_template, is_function, and the VM-internals group.

The analysis below is what the ruling was made from.


All 69 names below give `A0401 unknown action` on the interpreter and are recognised by the VM. I checked each one with `say(:name([1]))` on both builds. The question for each group: should it be added to the interpreter, or declared VM-only / deprecated?

| Group | Names | Documented? | Use in apps | Rec |
|---|---|---|---|---|
| Casts | `to_int`, `to_float`, `to_str`, `to_string`, `to_bool` | Deprecated: language-spec.md:856-857 "Earlier drafts used .to_int, .to_float, and .to_string … These forms are now deprecated" | none | **Deprecate.** Remove them from the VM or keep them as aliases with a warning; do not add them to the interpreter. |
| Case | `to_upper`, `to_lower` | no (the documented forms are `upper`/`lower`) | none | Deprecate, as for the casts. |
| Higher-order | `filter`, `filter_fn`, `map_fn`, `reduce`, `reduce_fn`, `for_each_fn`, `sort_by`, `any`, `all` | no (the docs show `map upper, names`, cheat-sheet.md:645, and `for … where`) | none (Campfire's `db::all` is a user act) | **Add to the interpreter** once actions can be passed as values there (cases `action_as_value` and `closure_captures_param` are known interp gaps). Fix `sort_by`'s string-keyed comparison first (`vm_sort_by_inner`, vm.rs:2923). |
| `grab*` family | `grab`, `grab_first`, `grab_last`, `grab_at`, `grab_all`, `grab_random`, `grab_where`, `grab_matching`, `grab_between` | no | none | Evidence balanced. Either add them (they complete the get/put/delete/update/reap matrix) or declare them VM-only. Rec: add. |
| Collection extras | `put_all`, `put_where`, `reap_all`, `reap_random`, `find_index`, `pairs`, `flatten`, `zip`, `range`, `slice`, `array_push`, `is_empty`, `is_collection`, `contains` | no; `slice` is undocumented (see D24) | Campfire writes its own `upto`/`span` because `:range` is missing (GAPS.md:185; `http.gbln:352`, `qrcode.gbln:27,37`, `opengraph.gbln:28`) | **Add** `range`, `slice` (after D24), `find_index`, `flatten`, `zip`, `is_empty`, `pairs`, `put_all`, `put_where`, `reap_all`, `reap_random`. `array_push` and `contains` duplicate `put_last!` and `has`: declare them VM-only aliases or drop them. |
| Strings | `replace`, `pad`, `pad_left`, `pad_right`, `repeat_str`, `url_encode`, `url_decode` | `replace`: cheat-sheet.md:245, 463 and language-spec.md:521 (replaces all). `pad`: cheat-sheet.md:246. Others no | **Sheriff calls `:url_encode`** (`badge_labels/badge.gbln:168-169`), which fails on the interpreter Sheriff runs on. Campfire writes its own `url_decode`/`url_encode` (`http.gbln:52`, `:106`) | **Add all** to the interpreter; `replace` and `pad` are documented. Fix the VM's `pad` ignoring its fill argument first (builtins.rs:2836). |
| Output / errors | `print`, `println`, `eprint`, `eprintln`, `panic`, `assert` | `assert`: cheat-sheet.md:1291. Others no | none | **Add.** `assert` is documented, and `panic`/`eprint` are needed for scripts. |
| Introspection | `type_of`, `is_function` | no (`valtype` is the documented-in-use form) | none | `type_of`: declare VM-only or drop it in favour of `valtype`. `is_function`: add it together with actions-as-values. |
| HTTP | `http_get`, `http_post`, `http_put`, `http_delete`, `http_request` | `http_post("/pay", payload)` returning a body (language-spec.md:4482), a different shape | Campfire calls `curl` through `bin/net` because the interpreter has no HTTP (GAPS.md:88, section 5) | **Add to the interpreter** with the VM's `{status, body, ok}` shape, and update the spec. |
| Templates / text | `render_template`, `ipsum`, `ipsum_full`, `ipsum_paragraphs`, `ipsum_sentences` | no | `tests/render_test.gbln`, `tests/gen1000.gbln` | `render_template`: add. `ipsum*`: the VM's are stubs returning a constant (builtins.rs:2283), so declare them VM-only or drop them. |
| VM runtime | `gc`, `gc_mode`, `mem_id`, `objects`, `overlays`, `stash_count`, `tether_count`, `delete_object`, `delete_overlays_on` | no | none | **Declare VM-only.** They expose VM internals. |

The interpreter also has three builtins the VM lacks:
- `db_exec`, `db_query` and `db_query_one` are interpreter-only at base. The io NOTES say cur adds pooled `db_*` to the VM.
- `none` is interpreter-only too (see D27).

## Owner rulings, 2026-10-06 23:04

- A grid ref prints as `GridRef(grid, x, y)`; `valtype` of a grid ref is `grid_ref`.
- `resolve_token` on a missing token or namespace gives `nil`.
- `backend` of a value that is not a collection is a type error.
- `array + array` is a new array holding both operands in order.
