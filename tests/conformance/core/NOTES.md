# core conformance cases: notes

Area: values and types, bindings and scoping, casts, numeric operators and comparisons, math
builtins, conditionals, loops, actions, runtime errors, and keywords/identifiers.
There are 197 cases. 109 have no directive, 19 are `known-gap: interp`, 21 are `known-gap: vm`,
20 are `known-gap: both`, and 28 are `undecided`.

Every case was run on the baseline binary
(`scratchpad/bin/goblin-base`, through the same cwd/flags as `conf-run`). For each case, every
engine that has no directive produces exactly the `.out` file, and every engine with a known-gap
directive does not. The undecided cases are exactly those where the two engines differ.

## How the docs were used

`docs/cheat-sheet.md` and `docs/language-spec.md` describe a surface that differs from what both
engines implement:
- The docs use `=` for assignment. The engines use `|` and `|=` (bare `=` is lexer error L0113).
- The docs use `op` and `end`. The engines use `act` and `xx`.
- The docs use method or prefix calls. The engines use `:builtin(...)` free calls.

The docs were therefore used only for semantics that are independent of syntax, such as what a
cast returns, what `/` yields, and range inclusivity. They were not used for syntax. Sheriff
(`/home/claude/ddonche/sheriff`) and `tests/*.gbln` were used for idioms. These include
`:valtype(x) == "array"`/`"map"` (96 uses), `.nix?`, `rescue err`, `if c => stmt`, `skip`,
`judge` blocks, and `send`.

`case_name`: interp result / VM result → classification. The evidence follows each entry.

## (a) Disagreements and classifications

### Values / types
- `valtype_array_map`: `array`,`map` / `collection`,`collection` → **known-gap vm**. Sheriff
  compares `:valtype(x) == "array"` 18× and `== "map"` 26×. `collection` is the VM's internal
  representation (`Value::Collection`, vm.rs MakeArray/MakeMap).
- `type_of_builtin`, `is_collection_builtin`, `to_bool_builtin`, `to_int_to_float_builtins`,
  `to_str_to_string_builtins`: interp `A0401 unknown action` / VM works → **known-gap interp**,
  because these builtins exist only on the VM (inventory). Note: language-spec.md §7 ("Deprecation
  Note") calls `.to_int/.to_float/.to_string` deprecated. That supports dropping them from both
  engines rather than adding them to the interpreter. The owner should decide.
- `is_map_literal`: true / false → **known-gap vm**. `is_nix_empty_collections`: true / false
  → **known-gap vm** (`builtins.rs:2217` IsNix has no `Collection` arm). The interp comment at
  lib.rs:12780 says empty array and empty map are nix.
- `is_big_value`: true / false → **known-gap vm**. The VM's own `valtype` reports `big` for the
  same value.
- `is_bound_name_global`: true,false / false,false → **known-gap vm**.
- `is_function_action_ref`: interp cannot reference an action as a value (R0101) and lacks
  `is_function` / VM true → **known-gap interp**.
- `whole_float_type`: interp says `int` for every whole-valued float (`:float(3)`, `1.5+1.5`,
  `3.0`, `5.float`). The VM says `float`. → **undecided D-whole-float-type** (see b).

### Bindings / scoping
- `redeclare_same_scope` (`x | 1` then `x | 2`): interp R0111 duplicate-local / VM rebinds →
  **undecided D-redeclare**.
- `block_scope_if_decl`, `block_scope_redeclare_in_if`, `block_scope_loop_body_decl`,
  `loop_var_after_loop`, `shadow_bind_in_block` → **undecided D-block-scope**. The interpreter
  scopes `|` and `[=` to the enclosing if or loop block, and the loop variable does not outlive
  the loop. The VM has action-level scoping, so variables leak out of blocks, `x | 2` inside an
  `if` overwrites the outer `x`, and the `for` variable overwrites an outer variable of the same
  name.
- `global_rebind_in_action`: `5,5` / `5,1` → **known-gap vm**. language-spec.md §5 "Scope"
  (~line 240): `score = score + 1 /// updates global score` inside an op. On the VM, `|=` on a
  global name inside an act silently creates a local.
- `imm_binding_reassign`: interp R0113 / VM allows reassignment → **undecided D-imm**.
- `compound_div_assign` (`x /= 4`): 2.5 / 2 → **undecided D-int-division**.
- `postfix_increment`: interp prints `4,4` (it returns the new value), VM prints `4,3` (it
  returns the new value and does not store it). Both differ from the documented `3,4`
  (cheat-sheet.md:96 "x++ → old x, then x = x + 1"; spec §9) → **known-gap both**.

### Casts
- `cast_rebinds_str`, `cast_rebinds_int_from_float`, `cast_rebinds_other_casts`,
  `cast_rebinds_param_in_action`, `cast_while_counter_probe` → **undecided D-cast-rebinds**.
  The interpreter rebinds the variable, so `i | 5; s | :str(i); i + 1` gives `51`. The VM is pure
  and gives `6`.
- `cast_in_loop_then_reassign`, `cast_for_var_then_reassign`, `cast_in_action_while_loop`: interp
  fails with `R0000 internal: const table missing entry` / VM is correct → **known-gap interp**.
  These cases reassign with a constant (`i |= 5`), so the intended output is the same whichever
  way D-cast-rebinds is decided. The crash is an internal error under either choice. Mechanism:
  lib.rs:20165–20174 calls `set_var`, which inserts into the *current* (loop body) frame without a
  consts entry. A later `|=` then fails the mirror check at lib.rs ~5972.
- `cast_int_invalid_string`: R0316 / `nil` → **known-gap vm**. cheat-sheet.md:211–215: `int "12a"`
  → ValueError.
- `cast_int_untrimmed_string`: both give 7 → **known-gap both**. cheat-sheet.md:170: `"  7 ".int`
  → ValueError (no auto-trim).
- `cast_int_bool` (`:int(true)`), `cast_int_nil`: interp error / VM `1`, `nil` →
  **undecided D-int-cast-nonnumeric**.
- `cast_str_nil`: both give `"nil"` → **known-gap both**. cheat-sheet.md:178 `nil.str /// ""`.
- `cast_bool_truthiness`: interp R0215 "bool lock only accepts boolean values" (and `:bool([])`
  returns `[]`) / VM truthiness → **known-gap interp**. cheat-sheet.md:183–196 and spec §8:
  `bool 0 → false`, `bool " " → true`.
- `big_plus_float`: 2.5 / type error → **known-gap vm** (spec §5: "any with big → big").
- `parse_bool_yes_no`: both reject → **known-gap both**. cheat-sheet.md:198 and spec §8 say
  "yes/no/1/0" are accepted.

### Numeric operators and comparisons
- `div_inexact` (`10/4`, `1/3`): 2.5, 0.333… / 2, 0 → **undecided D-int-division**.
- `div_exact_result_type`: both say `valtype(10/5) == int` → **known-gap both**. Spec §7 says
  "15 / 3 /// 5.0 (always float)" and cheat-sheet.md:101 says "float division". Caveat: the
  interpreter's "exact → int, else float" rule is deliberate (lib.rs ~21815). The owner may
  prefer to update the docs and flip this case.
- `floor_div_negative` (`-7 // 2`): -4 / -3 and `modulo_negative` (`-7 % 3`): 2 / -1 →
  **undecided D-negative-int-division**. The interpreter floors and the VM truncates.
- `modulo_float`: 1.5, 1 / type error → **known-gap vm**. vm.rs:677 `Rem` handles only
  Int,Int and Float,Float.
- `float_div_by_zero` (`5.0 / 0`): R0206 / `inf` → **undecided D-float-div-zero**.
- `int_overflow_mul_exact`: 18446744073709551614 / …616 → **known-gap vm** (precision loss).
- `int_overflow_sub_exact`: −9223372036854775817 / −9223372036854775808 → **known-gap vm**
  (saturates). Both engines promote `+` overflow to big exactly, so exact results are clearly
  intended.
- `pow_overflow`: `2**64` gives 9223372036854775807 / 0 → **known-gap both**. The intended result
  is the exact 18446744073709551616, which matches how both engines promote `+` and `*`.
- `unary_minus_pow_precedence`: both give `-2 ** 2 = 4` → **known-gap both**. The precedence
  table in cheat-sheet.md:70–85 and spec §9 places `**` above unary minus.
- `eq_int_float` (`1 == 1.0`): true / false → **undecided D-int-float-equality**.
- `eq_arrays_maps_structural`: true / false → **known-gap vm** (vm.rs:725 uses Rust `==` on
  `Collection`). cheat-sheet.md:110: "containers compare structurally".
- `logical_non_bool_operands` and `if_non_bool_condition` → **undecided D-truthiness**. The
  interpreter requires a bool (T0203 and R0201) and the VM uses truthiness.
- `between_range_check`: `5 between 1..5` is false on both → **known-gap both** (cheat-sheet.md:120
  "inclusive").

### Math builtins
- `math_pow_negative_exponent` (`:pow(2,-1)`): 0.5 / "expected number, got mixed" →
  **known-gap vm**.
- `math_sqrt_negative`: R0207 / NaN → **undecided D-sqrt-negative**.
- `math_method_style_array_sum` (`[1,2,3].sum`): A0401 unknown postfix action / 6 →
  **known-gap interp**. Spec §9 and cheat-sheet.md:305: `numbers.sum`.

### Conditionals
- `judge_no_match_nil`: nil / "RegisterAction: empty stack" → **known-gap vm**. Spec §10: "else
  is optional; if omitted and no guard matches, the result is nil."
- `judge_return_header`: `fb`,`mid` / `ok`,`ok` → **known-gap vm**. The expected values come from
  the `/// EXPECT` comments in tests/judge-return.gbln.
- `judge_using_value`: interp R0201 / VM runs the *first* arm (`L`) → **known-gap both**. The
  form appears in tests/judge-repeat.gbln, and the intended output is the matching arm (`G`).

### Loops
- `for_range_inclusive`, `for_range_exclusive`, `for_range_variable_bounds`,
  `for_range_descending` → **known-gap both**. Both engines treat `..` as exclusive and `...` as
  inclusive, and neither iterates a descending range. The docs say the opposite:
  cheat-sheet.md:607–609 and 1142, and spec §11 lines ~1440–1470 (`1..5` gives 1–5, `1...5`
  gives 1–4, `5..1` gives 5,4,3,2,1). The parser marks the flip as deliberate:
  `crates/goblin-parser/src/lib.rs:1578` and `:3626` say `// CHANGED: ".." => exclusive`. If the
  owner meant that change, these four cases and `between_range_check` should flip, and the docs
  are stale. The collections agent wrote `range_literal_as_array` the same way.
- `while_skip_stop`, `while_stop_if_colon_form`: VM `stack underflow` → **known-gap vm**.
- `repeat_stop`: interp ignores `stop` inside `repeat N` (5 instead of 3) → **known-gap interp**
  (spec §11 "Use stop to exit early").
- `return_inside_repeat`: interp ignores `return` inside `repeat` → **known-gap interp**.
- `repeat_map_as_key_value`: VM leaves `k` as nil and does not bind `v` → **known-gap vm**.
- `repeat_bare_with_stop`: VM fails with "expected comparable, got nil" → **known-gap vm**.
- `repeat_negative`: interp R0207 / VM runs zero times → **undecided D-repeat-negative**.
- `toplevel_return`: interp keeps executing after a top-level `return` / VM stops →
  **undecided D-toplevel-return**.

### Actions
- `action_default_param` (`act fx(a, b | 10)`): 11,3 / arity mismatch → **known-gap vm**. The
  parser supports defaults (parser lib.rs:2419) and spec §12 documents them.
- `action_fallthrough_value`: interp `unit` / VM `nil` → **undecided D-fallthrough-return-value**.
- `multi_value_return` (`return 1, 2`): `{_1: 1, _2: 2}` / `[1, 2]` → **undecided D-multi-return**.
- `closure_captures_param`, `action_as_value`: interp cannot use actions as values / VM works →
  **known-gap interp** (cheat-sheet.md:282: "you may still call a callable value held in a
  variable with ()").
- `user_action_shadows_builtin` (`all`, `str`, `f`): user act wins / builtin wins → **known-gap
  vm** (spec §4 lists builtins as "shadowable").
- `user_action_shadows_math_builtin` (`round`, `abs`): builtin wins on **both** → **known-gap
  both**. The interpreter's `eval_builtin` builtins (round, abs, sum, clamp, …) are checked before
  user acts. Only the `call_action_by_name` builtins lose to user acts.
- `method_call_user_action_no_parens` (`4.inc`): both reject → **known-gap both** (spec §12:
  `10.double`).

### Errors
- `attempt_rescue_binds_error`: interp R0101 unknown identifier `err` / VM binds a string →
  **known-gap interp**. The parser supports `rescue err` (parser lib.rs ~5459) and Sheriff
  writes it 8×.
- `panic_caught_by_attempt`, `assert_true_passes`: interp lacks `panic` and `assert` →
  **known-gap interp**. On the interpreter, `assert_false_errors` passes only because the
  unknown-action error also fails.
- `attempt_ensure_without_rescue_propagates`: both swallow the error → **known-gap both** (spec §26:
  "If no rescue matches, the error propagates after running ensure").

### Keywords / identifiers
- `builtin_named_var_indexed_words`, `_count_len`, `_keys_values`, `_in_action` → **known-gap
  both**. `words[1]` parses as `words([1])` on both engines, and `count[0]`/`len[0]` silently give
  1. Spec §4 lists these names as "Built-ins (shadowable operations & types)".
- `stop_as_identifier`, `action_keyword_as_param`, `blob_keyword_as_variable`: both engines fail,
  and the `.out` is `<error>` (spec §4 lists `stop` as a hard keyword). VM error quality is poor:
  `stack underflow on CallBuiltin` for `stop`.

### Error timing (not a semantic difference)
- `rebind_undeclared_error`, `unknown_action_error`, `unknown_builtin_colon_error`: the VM
  reports undefined names at compile time, so no earlier output is printed. The interpreter
  reports them at runtime. These cases avoid output before the error. `unknown_identifier_error`
  (`nope + 1`) fails at runtime on both.

## (b) Undecided slugs: evidence and what a change would touch

- **D-int-division**: Interp gives exact→Int, else Float (lib.rs ~21815–21840 binary `/`). VM
  `Div` (vm.rs:639) and `DivInt` (vm.rs:668), chosen at compiler.rs:2117, truncate. The docs
  (cheat-sheet.md:101, spec §7, §9 "always float") favour float. Choosing float means removing
  the `DivInt` specialisation and making `Div` Int,Int give a float. It also requires changing
  the interpreter's exact case (see `div_exact_result_type`).
- **D-negative-int-division**: Interp floors (`//` lib.rs:21341/22044, `%` 21336/21993). VM
  truncates (Rust `/` and `%` in vm.rs `DivInt`/`RemInt`/`Rem`). The docs only show positive
  operands.
- **D-float-div-zero**: Interp R0206. VM IEEE `inf` (vm.rs:651 `Float/Float`). No doc evidence.
- **D-int-float-equality**: The docs favour true (cheat-sheet.md:108 "numeric 3 == 3.0 is true",
  spec §8). VM `Eq` is derived `==` (vm.rs:725) and would need numeric coercion.
- **D-whole-float-type**: The interpreter deliberately reports a whole float as int in `valtype`,
  `is_int` and `is_float` (lib.rs:12368, 12415, 12429). The VM reports by representation. The
  docs show `float 3 → 3.0` (cheat-sheet.md:166) and `5.float → 5.0` (spec §7), which favours the
  VM.
- **D-truthiness**: The docs contradict each other. Spec §10 says "Conditions are boolean
  expressions; non-boolean values must be compared explicitly", which supports the interpreter
  (lib.rs:2574 "requires a boolean value"). Spec §6 ("Empty string is falsy in conditionals") and
  spec §8 (a falsy list) support the VM (`JumpIfFalse` truthiness, vm.rs:763).
- **D-cast-rebinds**: cheat-sheet.md:155 says "Casting **never mutates** the original value",
  which favours the VM. To make casts pure, delete the block at interp lib.rs:20165–20174. That
  also fixes the three `const table missing entry` known-gap cases.
- **D-int-cast-nonnumeric**: Interp R0316 for bool and nil. VM gives 1/0 and nil
  (builtins.rs:2230 ToInt). cheat-sheet.md:213 says "TypeError for unsupported conversions" but
  does not cover bool or nil.
- **D-block-scope**: The interpreter pushes an env/consts frame per block (lib.rs:1121
  push_scope). The VM resolves locals per action. Spec §5 talks only about operation-local
  scope. Choosing block scope means the VM compiler must scope locals per block
  (compiler.rs `push_scope` / local resolution).
- **D-redeclare**: Interp R0111 (lib.rs:5892–5935, help text: "Use '|=' to reassign"). The VM
  `StoreLocal` overwrites. The interpreter's error is deliberate. There is no doc evidence.
- **D-imm**: Interp enforces `imm` (R0113, lib.rs ~5998). The VM compiler never reads `is_imm`.
  cheat-sheet.md:62 says "no immutable variables or constants", which contradicts the parser
  supporting `imm`.
- **D-fallthrough-return-value**: Interp `Value::Unit`, VM `nil`. Spec §12 shows an explicit
  `nil` at the end of an op, which slightly favours nil.
- **D-multi-return**: Interp returns a `{_1, _2}` map and the VM returns an array. The docs use
  `q, r = 10 >> 3` destructuring only.
- **D-repeat-negative**: Interp R0207 (lib.rs:20557). The VM runs zero times.
- **D-toplevel-return**: Interp ignores a top-level `return`. The VM ends the program. No doc
  evidence.
- **D-sqrt-negative**: Interp R0207. VM NaN (builtins.rs:191). No doc evidence.

## (c) Spec-only (in the docs, implemented by neither engine; no cases)

- Chained comparisons `1 < x < 10` (spec §8). Both give a type error.
- `^^` show-work power (spec §9). Both: operator not implemented.
- `>>` divmod (cheat-sheet.md:103). Both fail to parse.
- Big literals `123b`/`5.0b` and `5i`/`3.14f` suffixes (spec §5, §7). Both fail to parse.
- `.pct!`/`.pct?` string percent casts. Prefix casts `int x`/`str x` (cheat-sheet.md:153).
- `for … where …` filtering and `for i, v in …` / `for k, v in map` destructuring (spec §11).
  Both fail.
- `jump` / `until` loop strides.
- `|>` pipeline (`x |> .abs`). Both fail to parse.
- Named arguments `f(b: 1, a: 5)`, variadic `...nums`, one-liner `act f(x) = expr`.
- `rescue Type as e`, `raise`, `error "…"` (spec §26). The engines support only `rescue [name]`.
- Inline judge `judge: c: v :: else: v`, and judge used as the implicit last-expression return
  value (both return an empty value).
- `:round(x, digits)`.
- Docs-only syntax that both engines reject by design: `=` assignment, `op`, and the `end`-only
  closers on `act`. These are not treated as gaps.

## (d) Inventory builtins exercised

abs, avg, b, big, bool, ceil, clamp, f, f32, f64, float, floor, i, i8, i16, i32, i64, int,
is_alnum, is_alpha, is_array, is_big, is_bool, is_bound_name, is_char, is_collection,
is_control, is_digit, is_even, is_float, is_function, is_int, is_map, is_multiple_of,
is_negative, is_nil, is_nix, is_num, is_odd, is_pair, is_pct, is_positive, is_seq, is_str,
is_type, is_unit, is_whitespace, max, min, panic, assert, parse_bool, pct, percent, pow,
round, sqrt, str, string, sum, to_bool, to_float, to_int, to_str, to_string, true, false,
type_of, u8, u16, u32, u64, valtype, vt, len, all, find (as user action names), count, keys,
values, words, lines (as variable names).

Not covered here: `none` (interp-only name), `mode` and `between` as a function
(collections/strings areas).
