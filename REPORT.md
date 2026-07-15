# Goblin Language Surface Audit

Read-only audit. Every entry below is cited `file:line` from the actual source read by a
research pass; nothing is inferred from documentation, naming conventions, or memory.
Where a pass could not fully resolve something, it is listed under that section's
**UNRESOLVED** heading rather than guessed.

Ground truth for behavior is `crates/goblin-interpreter/src/lib.rs` (the tree-walking
interpreter). The VM (`crates/goblin-vm`) is the thing being audited for parity gaps.

---

## 0. Structural note on the interpreter's builtin dispatch

The interpreter does **not** have a single builtin dispatch table. There are (at least)
two separate literal-string match dispatchers, plus one non-name-based dispatcher:

1. **`fn eval_builtin`** — `crates/goblin-interpreter/src/lib.rs:6322`–`7490`. A flat
   `match name { "literal" => ... }` over the builtin name, ending `_ => return Ok(None)`
   at line 7479 (falls through to the next dispatcher).
2. **`fn call_action_by_name`** — `crates/goblin-interpreter/src/lib.rs:11295`–`17098`
   (~5800 lines). The dispatcher actually reached for most string/collection/type-check
   builtins and method-style calls. Unmatched names fall through to `eval_builtin` at
   line 17059 (i.e. the two dispatchers call into each other — see UNRESOLVED below).
3. **`fn collection_operation`** — `crates/goblin-interpreter/src/lib.rs:9379`–`11293`.
   **Not name-dispatched at all.** Signature is
   `collection_operation(coll: &Value, pos: Position, op: Operation, sp: &Span, sess: &mut Session)`.
   It implements the `grab*/put*/update*/delete*/reap*/get*` family by matching a
   pre-parsed `Position` enum (`Random|First|Last|At|Where|Matching|Between|All`) crossed
   with an `Operation` enum (`Get|Reap|Put|Update|Delete`). The `"grab_at"`-style name
   strings are resolved to `Position`/`Operation` values at their call sites inside
   `call_action_by_name` (e.g. `get_at` at line 14780, `put_first` at 14819, `delete_where`
   at 14921 — all one-line delegations into `collection_operation`).
4. A separate function, **`fn mutate_via_call_name`** (`lib.rs:17535` onward), handles the
   bang-form (`!`) mutation path for some builtins (e.g. `delete_object!`,
   `delete_overlays_on!`) and is reached via a different call path than either dispatcher
   above.

**UNRESOLVED**: `round`, `floor`, `ceil`, `abs`, `pow`, `sqrt` all have match arms in
*both* `eval_builtin` (lib.rs:6537–6753) *and* `call_action_by_name` (lib.rs:13208–13383).
Likewise `escape_html`, `read_json`/`write_json`, all `yall_*`, `pack`/`unpack`,
`mem_addr`/`mem_total`/`mem_human`, `secure_pick`/`secure_random`/`secure_shuffle`,
`rand_seed`, `roll`/`roll_detail`, `format`/`clear_format`/`format_info`, `input`/`ask`,
`md_to_html`/`highlight_code`, `invoke`/`summon`/`provoke`, `valtype`/`vt`, `owned_by`/
`owns_tree`/`clone_object`, and `env` all have arms in both functions. Given
`call_action_by_name` falls through to `eval_builtin` for unmatched names (lib.rs:17059),
and no evidence was found of the reverse (eval_builtin falling through to
call_action_by_name), the two dispatchers are likely reached from **different call
sites** in the AST evaluator (e.g. one for `Expr::FreeCall`, one for method/action-call
syntax), each maintaining its own — partially overlapping — copy of the builtin table.
This split-table architecture was not fully traced to its root cause within this audit's
scope; flagging as a genuine finding rather than resolving it.

Additionally: `"after"` has **two** match arms inside `call_action_by_name` itself —
line 15535 (substring-after-separator semantics) and line 17045 (prefix-strip semantics).
Since Rust match arms are checked in order, the second is dead/unreachable code. Not
independently confirmed against a compiler warning.

---

## 1. BUILTINS (interpreter — ground truth)

### 1.1 Math (`eval_builtin`, `crates/goblin-interpreter/src/lib.rs`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| round | 6537 | 1 | Int/Float/Pct/Big (else NUMERIC_EXPECTED) | same variant, rounded (Big→`round_dp(0)`) |
| floor | 6557 | 1 | Int/Float/Pct/Big | same variant, floored |
| ceil | 6577 | 1 | Int/Float/Pct/Big | same variant, ceiled |
| abs | 6597 | 1 | Int/Float/Pct/Big | same variant (Pct abs→Float) |
| pow | 6617 | 2 (base, exp) | numeric; Big-aware (`decimal_powi`/f64 fallback) | Big or Float |
| sqrt | 6702 | 1 | Big or numeric via `to_f64_for_math`; negative→MATH_DOMAIN err | Float |
| sum | 6753 | 1 | Array of numbers (else ARRAY_EXPECTED) | Big or Float |
| avg | 6782 | 1 | Array of numbers; empty→Float(0.0) | Big or Float |
| min | 6815 | ≥1 (1-arg=Array, else varargs) | numeric; empty array→EMPTY_ARRAY err | Big or Float |
| max | 6897 | ≥1 (same shape as min) | numeric | Big or Float |
| clamp | 6979 | 3 exactly (value, lo, hi) | Int/Float only; lo>hi→MATH_DOMAIN err | Int if all-Int else Float |
| int, i | 6523 / 13170 (dup, see §0) | 1 | any | delegates `cast_to_int_like` |
| float, f | 6527 / 13174 | 1 | any | delegates `cast_to_float` |
| big, b | 6531 / 13178 | 1 | any | delegates `cast_to_big` |

### 1.2 Math / conversions (`call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| str, string, pct, percent, to_map, m, bool | 13170–13196 | 1 | any | `cast_value_to_lock` variants; `to_map`/`m`→`cast_to_map` |
| i8, i16, i32, i64, u8, u16, u32, u64, f32, f64 | 13198–13207 | 1 | any | `cast_value_to_lock(.., "i8".."f64", ..)`, range-checked |

### 1.3 String (`eval_builtin`, delegates to `crate::actions::strings`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| upper | 7059 (executes; a duplicate dead arm exists per §0-style shadowing) | 1 | Str/Char/Array-or-Seq-of-Str-or-Char | mirrors input shape |
| lower | 7059 | 1 | same | same |
| title | 7060 | 1 | same | title-cased words, same shape |
| slug | 7061 | 1 | same | ascii-alnum lowercased, `-`-joined |
| raw | 7062 (executes); **dead duplicate at 7071** | 1 | same | passthrough |
| mixed | 7063 (executes); **dead duplicate at 7078** | 1 | same | per-char random case, seeded from `sess.next_u128()` |
| trim | 7066 | 1 | same | trims whitespace + NBSP/BOM/ZWSP/ZWNJ/ZWJ/word-joiner/MVS |
| trim_lead | 7067 | 1 | same | same set, start only |
| trim_trail | 7068 | 1 | same | same set, end only |

### 1.4 String (`call_action_by_name`, `crate::actions::strings::*` unless noted)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| find | 14539 | 2 | (Str, Str sub) | Int index or Nil |
| find_all | 14540 | 2 | (Str, Str sub) | Array\<Int\> (non-overlapping) |
| ord | 14541 | 1 | Char or single-char Str | Int codepoint |
| before | 15508 | 2 | (Str, Str sep) | Str (before first sep, or whole string) |
| after | 15535 (dead dup at 17045) | 2 | (Str, Str sep) | Str (after first sep, or "") |
| before_last | 15562 | 2 | (Str, Str sep) | Str |
| after_last | 15589 | 2 | (Str, Str sep) | Str |
| between (string builtin, distinct from `between` operator) | 15616 | 3 | (Str, left, right) | Str (first left…right span) |
| lines | 15652 | 1 | Str | Array\<Str\> |
| words | 15674 | 1 | Str | Array\<Str\> |
| chars | 15692 | 1 | Str | Array\<Char\> |
| split | 15711 | 2 | (Str, Str sep, `r/…` regex supported) | Array\<Str\> |
| join | 15756 | 2 | (Str/Array/Seq, Str sep) | Str |
| count_matching | 15870 | 2 | (Str, Str pattern) | Int (regex match count) |
| is_matching | 15835 | 2 | (Str, Str pattern) | Bool (regex) |
| tokenize | 15904 | 2–3 | (Str, Str delims, Bool keep_delims?) | Array\<Str\> |
| reverse_chars | 15977 | 1 | Str or array of Str | same, chars reversed |
| minimize | 15983 | 1 | Str or array of Str | same, whitespace collapsed |
| parse_bool | 15998 | 1 | Str ("true"/"false", case-insens) | Bool (errors otherwise) |
| sanitize_bom | 16145 | 1 | Str | Str, BOM stripped |
| normalize_newlines | 16165 | 1 | Str | Str, CRLF/CR→LF |
| ignore_where | 16179 | 2 | (Str, Str needle) | Str, literal remove |
| ignore_lines_where | 16187 | 2 | (Str, Str prefix) | Str, drop matching lines |
| ignore_matching | 16204 | 2–3 | (Str, Str pattern, Map\|Nil flags{i,m,s}) | Str, regex remove |
| ignore_lines_matching | 16271 | 2–3 | same | Str, drop matching lines |
| ignore_between | 16342 | 3–5 (runtime-checked) | (Str, open, close, opts) | Str |
| ignore_blocks | 16459 | variable | (Str + block markers/opts) | Str |
| ignore_blocks_first | 16595 | variable | same family | Str |
| keep_matching | 16745 | 2–3 | (Str, Str pattern, Map\|Nil flags) | Str, concatenated matches |
| keep_before | 16812 | 2 | (Str, delimiter) | Str |
| keep_after | 16841 | 2 | (Str, delimiter) | Str |
| keep_between | 16876 | 3–4 | (Str, open, close, Map\|Nil opts) | Str |
| starts_with | 17006 | 2 | (Str, Str or Array\<Str\> needles) | Bool |
| ends_with | 17025 | 2 | (Str, Str or Array\<Str\> needles) | Bool |
| escape_html | 14760 (→`actions::files::escape_html`) | 1 | Str | Str |
| json_parse | 15158 | 1 | Str | parsed Value |
| json_stringify | 15192 | 1 | any Value | Str |
| json_stringify_pretty | 15223 | 1 | any Value | Str, pretty |
| format | 12915 | 2 or 4 | (numeric, Int dec[, sep_th, sep_dec]) | `Value::Formatted` |
| clear_format | 13114 | 1 | any | unwraps `Formatted` |
| format_info | 13134 | 1 | Formatted value | Map{dec,th,decmark} or Nil |

### 1.5 Collections (`call_action_by_name`, mostly `crate::actions::collections::*`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| has | 14549 | 2 | (container, needle) | Bool |
| count | 14550 | 1–2 | (x) or (Str, sub) | Int |
| shuffle | 14551 | 1 | Str/Int/Array-like | same type, shuffled |
| sort | 14552 | 1 | Str/Int/Array-like | same type, sorted |
| freq | 14553 | 1 | Str/Array-like | Map(element→count) |
| mode | 14554 | 1 | Array/seq | Map{value:count} (errors on empty) |
| sample_weighted | 14555 | 1 | Map cfg {src, weights, count?} | Value |
| map | 14556 | 2 | (Str/Array/Seq, Str action name) | Str or Array |
| unique | 14557 | 1 | Str or Array-like | same, de-duped, order preserved |
| dups | 14558 | 1 | Str or Array-like | same, only duplicated elements |
| keys | 14544 (`actions::maps::keys`) | 1 | Map/MapOrd | Array\<Str\> |
| values | 14545 (`actions::maps::values`) | 1 | Map/MapOrd | Array\<Value\> |
| items | 14546 (`actions::maps::items`) | 1 | Map/MapOrd | Array\<Pair\<Str,Value\>\> |
| reverse | 15956 | 1 | Array/Seq | Array, reversed |
| len | 16034 | 1 | Str/Array/Seq/Map | Int |
| backend | 16070 | 1 | Seq/Array | Str (backend name) |
| metrics | 16104 | 1 | Seq/Array | Map (perf/shape metrics) |
| pack | 12851 | 1 | Array (all-digit Int→Int; all Str/Char→Str; mixed→Str) | Int/Str/Nil |
| unpack | 12813 | 1 | Int or Str | Array\<Int\> digits or Array\<Char\>; Nil on invalid |
| reap | 15314 | 1 | Map cfg {src, count, ...} | removes+returns elements |
| get, get_first, get_last, get_at, get_where, get_all, get_matching, get_between, get_random | 14765,14770,14775,14780,14785,14791,14796,14802,14809 | 1–3 | — | delegates `collection_operation(.., Position::*, Operation::Get)` |
| put, put_first, put_last, put_at, put_matching, put_between, put_random | 14814,14819,14824,14829,14834,14840,14847 | 2–4 | — | `collection_operation(.., Operation::Put(v))` |
| update, update_first, update_last, update_at, update_where, update_all, update_matching, update_between, update_random | 14852,14857,14862,14867,14872,14878,14883,14889,14896 | 2–4 | — | `collection_operation(.., Operation::Update(v))` |
| delete, delete_first, delete_last, delete_at, delete_where, delete_all, delete_matching, delete_between, delete_random | 14901,14906,14911,14916,14921,14927,14932,14938,14945 | 1–3 | — | `collection_operation(.., Operation::Delete)` |
| reap_first, reap_last, reap_at, reap_where, reap_matching, reap_between | 14950,14955,14960,14965,14971,14977 | 1–3 | — | `collection_operation(.., Operation::Reap)` |

**Not confirmed as present anywhere**: `is_empty`, `pairs`, `sort_by`, `filter`, `reduce`,
`any`, `all`, `find_index`, `zip`, `flatten`, `slice`, `map_fn`/`filter_fn`/`reduce_fn`/
`for_each_fn` — none of these appeared in any of the three interpreter passes' findings.
**UNRESOLVED**: this may mean they live in a part of `call_action_by_name` or
`eval_builtin` that wasn't captured verbatim in agent output (both functions are very
large and were read in chunks), or they may not exist in the interpreter at all. Given
the VM does implement all of them (§2), this is flagged as the single highest-priority
follow-up: **confirm by direct grep** whether `is_empty`, `pairs`, `sort_by`, `filter`,
`reduce`, `any`, `all`, `find_index`, `zip`, `flatten`, `slice`, and the `*_fn` family
exist in `goblin-interpreter` before concluding they're VM-only additions.

### 1.6 Type-check predicates (`call_action_by_name`, arity 1 unless noted, return Bool)

| name | file:line | notes |
|---|---|---|
| is_nil | 12390 | |
| is_bool | 12399 | |
| is_int | 12408 | incl. int-like Float |
| is_float | 12421 | proper non-int-like Float |
| is_big | 12435 | |
| is_pct | 12444 | |
| is_num | 12453 | |
| is_str | 12466 | |
| is_char | 12475 | |
| is_array | 12484 | |
| is_map | 12493 | |
| is_pair | 12502 | |
| is_seq | 12511 | |
| is_unit | 12520 | |
| is_control | 12529 | CtrlSkip/CtrlStop/CtrlReturn |
| is_digit | 12542 | Char or Str |
| is_alpha | 12557 | Char or Str |
| is_even | 12587 | Int/Big/int-like Float |
| is_odd | 12620 | Int/Big/int-like Float |
| is_multiple_of | 12651 | arity 2 |
| is_positive | 12714 | Int/Float/Big/Pct |
| is_negative | 12732 | Int/Float/Big/Pct |
| is_alnum | 12750 | Char or Str |
| is_whitespace | 12763 | Char or Str |
| is_nix | 12779 | Nil/empty-or-ws Str/empty Array/empty Map |
| is_type | 11753 | arity 2 (value, type-name Str) |
| is_bound_name | 11715 | Str var name |
| is_file | →`actions::files::is_file`, dispatch 14753 | |
| is_dir | →`actions::files::is_dir`, dispatch 14754 | |

### 1.7 Date/Time (`eval_builtin`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| now, utc_now | 7206 | 0 | — | DateTime (`Utc::now()`) |
| epoch_ms | 7210 | 0 | — | Int (millis) |
| epoch_s | 7214 | 0 | — | Int (secs) |
| local_now | 7218 | 0 | — | DateTime, computed tz offset |
| today | 7229 | 0 | — | DateTime, kind=Date |
| tomorrow | 7238 | 0 | — | DateTime, kind=Date, +1d |
| yesterday | 7247 | 0 | — | DateTime, kind=Date, -1d |
| date | 7256 | 1 (Str) or 3 (y,mo,d:Int) | — | DateTime, kind=Date |
| time | 7279 | 1 (Str) or 3 (h,m,s:Int) | — | DateTime, kind=Time |
| datetime | 7302 | 1 (Str RFC3339) or 6 (y,mo,d,h,m,s) | — | DateTime, kind=DateTime |
| duration | 7326 | any | — | **always errors** ("not yet implemented") |
| to_iso | 7329 | 1 | DateTime | Str |
| from_iso | 7336 | 1 | Str (RFC3339) | DateTime |
| to_epoch_ms | 7344 | 1 | DateTime | Int |
| from_epoch_ms | 7351 | 1 | Int | DateTime |
| format_datetime, format_date, format_time | 7358 | 2 | (DateTime, Str pattern) | Str (chrono `.format`) |
| year/month/day/hour/minute/second | 7370–7400 | 1 each | DateTime | Int |
| weekday | 7406 | 1 | DateTime | Str |
| add_duration | 7421 | 2 | (DateTime, Map{years,months,weeks,days,hours,minutes,seconds}) | DateTime |
| since | 7451 | 1 | DateTime | Int (seconds since now) |
| until | 7457 | 1 | DateTime | Int (seconds until) |
| timezone | 7463 | 1 | DateTime | Str |
| to_timezone | 7468 | 2 | (DateTime, Str tz) | DateTime |

### 1.8 I/O, Filesystem, Process (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| print, println, eprint, eprintln | **NOT FOUND** anywhere in `goblin-interpreter` | — | — | (exist only in VM as `BuiltinId::Print/Println/Eprint/Eprintln`) |
| read_text | `actions/files.rs:118`, dispatch lib.rs:14744 | 1 | Str(path) | Str or Diag |
| write_text | `actions/files.rs:68`, dispatch lib.rs:14743 | — | — | non-bang always errors; requires `write_text!` |
| append_file | `actions/files.rs:82`, dispatch lib.rs:14741 | — | — | non-bang always errors |
| read_json | lib.rs:15268 | 1 | Str(path) | parsed Value |
| write_json | lib.rs:15254 | — | — | non-bang always errors |
| ask, input | lib.rs:11816 | 0–1 | Str(prompt)? | Str(stdin line); errors if `GOBLIN_NONINTERACTIVE=1` |
| env | lib.rs:14491 | 1 | Str(name) | Str(value) or Str("") |
| file_exists | `actions/files.rs:18`, dispatch 14738 | 1 | Str | Bool |
| is_file | `actions/files.rs:381`, dispatch 14753 | 1 | Str | Bool |
| is_dir | `actions/files.rs:400`, dispatch 14754 | 1 | Str | Bool |
| basename | `actions/files.rs:274`, dispatch 14750 | 1 | Str | Str |
| dirname | `actions/files.rs:223`, dispatch 14748 | 1 | Str | Str (slash-normalized) |
| stem | `actions/files.rs:171`, dispatch 14746 | 1 | Str | Str |
| ext | `actions/files.rs:195`, dispatch 14747 | 1 | Str | Str (leading `.` incl.) |
| path_join | `actions/files.rs:247`, dispatch 14749 | 2 | Str, Str | Str |
| path_split | `actions/files.rs:423`, dispatch 14755 | 1 | Str | Array\<Str\> |
| path_normalize | `actions/files.rs:358`, dispatch 14751 | 1 | Str | Str |
| path_relative_to | `actions/files.rs:450`, dispatch 14756 | 2 | Str, Str | Str |
| path_fix_separators | `actions/files.rs:299`, dispatch 14752 | 1 | Str | Str |
| walk | `actions/files.rs:478`, dispatch 14758 | 1–2 | Str(dir), Str(pattern)? | Array\<Str\> |
| list_dirs | `actions/files.rs:529`, dispatch 14759 | 1 | Str | Array\<Str\> (empty if not a dir) |
| create_dir | `actions/files.rs:40`, dispatch 14739 | — | — | non-bang always errors |
| copy_file | `actions/files.rs:153`, dispatch 14745 | — | — | non-bang always errors |
| delete_path | `actions/files.rs:100`, dispatch 14742 | — | — | non-bang always errors |
| pathfind | `actions/files.rs:660`, dispatch 14757 | 2–3 | Str, Str, Str(mode)? | Str |
| uuid_v4 | `actions/files.rs:620`, dispatch 14761 | 0 | — | Str |
| uuid_v7 | `actions/files.rs:637`, dispatch 14762 | 0 | — | Str |
| run_cmd | `actions/process.rs:8`, dispatch lib.rs:14561 | 1–3 | Str(cmd), Str(cwd)?, Map(env)? | Str (**JSON-encoded string**, not a Map) |
| zip_dir | `actions/files.rs:54`, dispatch 14740 | — | — | non-bang always errors |

### 1.9 Random / dice (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| roll | lib.rs:13983 | 1 | Map{count,sides,modifier/mod,keep_high,drop_low,reroll_eq,explode,adv,dis,clamp_min,clamp_max} | Int |
| roll_detail | lib.rs:14188 | 1 | same shape | Map(values/kept/dropped/sum/total + echoed cfg) |
| roll_str | lib.rs:14437 (shared arm w/ roll_detail_str) | 1 | Str dice notation | delegates to `roll()` → Int |
| roll_detail_str | lib.rs:14437 | 1 | Str | delegates to `roll_detail()` → Map |
| rand_seed | lib.rs:13973 | 1 | numeric | Unit (reseeds session RNG) |
| pick | lib.rs:13436 | 1 | Map cfg (very long, ~500 lines, not exhaustively traced) | Int/Str/Array |
| secure_pick | `actions/csprng.rs:344`, dispatch lib.rs:14571 | 1 | Map cfg{count/count_expr,digits,unique,src,range_start/end,range_inclusive,allow_dups} | Value(item/Array/Str) |
| secure_random | `actions/csprng.rs:683`, dispatch 14572 | 2 | Int(min), Int(max) | Int |
| secure_shuffle | `actions/csprng.rs:732`, dispatch 14573 | 1 | Array or Str | same variant |

### 1.10 Memory/GC, Type meta, Pack (`eval_builtin` + `call_action_by_name`)

| name | file:line | notes |
|---|---|---|
| mem_id | **NOT FOUND** | absent from interpreter (VM has it, always errors there) |
| mem_addr | `actions/mem.rs:55`, dispatch 14566 | 1 arg, any Value → Str (hex pointer) |
| mem_total | `actions/mem.rs:75`, dispatch 14567 | 0 args → Int (RSS bytes) |
| mem_human | `actions/mem.rs:162`, dispatch 14568 | 0 args → Str |
| gc | **NOT FOUND** | absent from interpreter (interpreter has no arena/GC to sweep) |
| typeof, type_of | **NOT FOUND** | absent (interpreter uses `valtype`/`vt` instead — see below) |
| valtype, vt | lib.rs:12359 | 1 arg → Str (kind name) |
| clear_format | lib.rs:13114 | see §1.4 |
| format_info | lib.rs:13134 | see §1.4 |
| backend | lib.rs:16070 | see §1.5 |
| metrics | lib.rs:16104 | see §1.5 |
| assert | **NOT FOUND** | absent from interpreter |
| panic | **NOT FOUND** | absent from interpreter |
| pack | lib.rs:12851 | see §1.5 |
| unpack | lib.rs:12813 | see §1.5 |

### 1.11 Higher-order / GLAM (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| invoke | lib.rs:11899 | ≥2 | Str(action name, optional `ns::action`), forwarded args | Value |
| need | lib.rs:12135 | ≥1 | Str(need_name), ... | resolves GLAM `[needs.actions]` config, recurses into `call_action_by_name` |
| summon | lib.rs:12242 | 2 | any Value(seed), Array/Seq of Str(event names) | Value (threaded accumulator) |
| provoke | lib.rs:12308 | 1–2 | Bool(condition), optional message | Bool(true) or Err |

### 1.12 Markdown / syntax, YALL (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| md_to_html | lib.rs:11882 | 1 | Str | Str (via `crate::modules::markdown::md_to_html`) |
| highlight_code | lib.rs:11890 | 4 | Str(code), Str(lang), Str(dark theme), Str(light theme) | Str |
| yall_parse | lib.rs:14985 | 2 | Str(text), Str(label) | parsed Value |
| yall_parse_file | lib.rs:15024 | 1 | Str(path) | parsed Value |
| yall_write | lib.rs:15062 | 1 | any Value | Str |
| yall_write_file | lib.rs:15082 | 2 | Str(path), Value | Nil |
| yall_pretty | lib.rs:15119 | 1 | any Value | Str |
| yall_minify | lib.rs:15138 | 1 | any Value | Str |

### 1.13 HTTP request/response, Token store (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| req_method | `actions/request.rs:21`, dispatch 14725 | 0 | — | Str (env `GOBLIN_METHOD`) |
| req_path | `actions/request.rs:28`, dispatch 14726 | 0 | — | Str (`GOBLIN_PATH`) |
| req_query | `actions/request.rs:35`, dispatch 14727 | 0 | — | Str (`GOBLIN_QUERY_STRING`) |
| req_body | `actions/request.rs:42`, dispatch 14728 | 0 | — | Str (`GOBLIN_BODY`) |
| req_header | `actions/request.rs:49`, dispatch 14729 | 1 | Str(name, case-insens.) | Str or Nil |
| cookie | `actions/request.rs:95`, dispatch 14730 | 1 | Str(name) | Str or Nil |
| set_status | `actions/response.rs:16`, dispatch 14733 | 1 | Int(code) | Nil (mutates `sess.response.status`) |
| set_header | `actions/response.rs:41`, dispatch 14734 | 2 | Str,Str | Nil |
| set_cookie | `actions/response.rs:82`, dispatch 14735 | 2–3 | Str,Str, Map(options)? | Nil |
| register_token | lib.rs:6365 | 3 | Str(module), Str(ident), Value | Unit |
| resolve_token | lib.rs:6374 | 1 (render-all-in-string) or 2 (module,ident) | — | Str or Value; errors if not found |
| clear_token | lib.rs:6430 | 2 | Str,Str | Unit |
| clear_tokens | lib.rs:6442 | 1 | Str | Unit |
| clear_all_tokens | lib.rs:6466 | 0 | — | Unit |
| list_tokens | lib.rs:6475 | 0–1 | Str(module)? | Map |

### 1.14 DES / overlay, Grid (`eval_builtin` + `call_action_by_name`)

| name | file:line | arity | arg types | return type |
|---|---|---|---|---|
| decision_debug | lib.rs:7108 | 0 | — | Unit (prints to stdout) |
| overlays_of | lib.rs:7129 | 1 | Str(host var) | Array\<Str\> |
| overlay_strength | lib.rs:7140 | 2 | Str,Str | Float or Nil |
| link_score | lib.rs:7151 | 3–4 | Str(class),[Str(channel)],Str,Str | Float (clamped 0..1) or Nil |
| owned_by | lib.rs:14602 | 1 | Object or Str(uuid) | Array\<Value\> |
| owns_tree | lib.rs:14630 | 1 | Object or Str(uuid) | Str (indented tree text) |
| clone_object | lib.rs:14688 | 1 | Object/Str var name | Object |
| delete_object | lib.rs:17558, inside `mutate_via_call_name` (bang-only) | 1 | plain Ident | Unit |
| delete_overlays_on | lib.rs:17632, inside `mutate_via_call_name` (bang-only) | 1 | — | — |
| tick, tick_db | lib.rs:7098 (shared arm) | 0 | — | Unit (9-pass DES tick) |
| grid | `actions/grid.rs:168`, dispatch 14576 | 3,4, or 7 | Str(name),Int,Int,[mode],[tile_w,tile_h,regions] | Str(world name) |
| grid_get | `actions/grid.rs:242`, dispatch 14579 | 4 | world,Int,Int,Str(layer) | Value or Nil |
| grid_set | `actions/grid.rs:270`, dispatch 14580 | 5 | world,Int,Int,Str,Value | Unit |
| grid_void | `actions/grid.rs:327`, dispatch 14581 | 3 | world,Int,Int | Unit |
| grid_tile_get | `actions/grid.rs:345`, dispatch 14590 | 4 | — | Value or Nil |
| grid_tile_set | `actions/grid.rs:364`, dispatch 14591 | 5 | — | Unit |
| grid_region_get | `actions/grid.rs:384`, dispatch 14592 | 4 | — | Value or Nil |
| grid_region_set | `actions/grid.rs:403`, dispatch 14593 | 5 | — | Unit |
| grid_default_get | `actions/grid.rs:437`, dispatch 14594 | 2 | — | Value or Nil |
| grid_default_set | `actions/grid.rs:423`, dispatch 14595 | 3 | — | Unit |
| grid_neighbors | `actions/grid.rs:454`, dispatch 14582 | 3 | — | Array\<GridRef\> |
| grid_occupied | `actions/grid.rs:475`, dispatch 14583 | 1 | — | Array\<GridRef\> |
| grid_unoccupied | `actions/grid.rs:491`, dispatch 14584 | 1 | — | Array\<GridRef\> |
| grid_occupied_count | `actions/grid.rs:507`, dispatch 14585 | 1 | — | Int |
| grid_unoccupied_count | `actions/grid.rs:520`, dispatch 14586 | 1 | — | Int |
| grid_count | `actions/grid.rs:533`, dispatch 14587 | 2 | — | Int |
| grid_occupied_by | `actions/grid.rs:548`, dispatch 14588 | 2 | — | Array\<GridRef\> |
| grid_has | `actions/grid.rs:566`, dispatch 14589 | 2 | — | Bool |
| grid_info | `actions/grid.rs:602`, dispatch 14596 | 1 | — | Map(name,width,height,mode,layers,...) |
| grid_tile_info | `actions/grid.rs:644`, dispatch 14597 | 3 | — | Map |
| grid_region_info | `actions/grid.rs:670`, dispatch 14598 | 3 | — | Map |

### 1.15 Range/lorem — NOT FOUND

`range`, `ipsum`, `ipsum_sentences`, `ipsum_paragraphs`, `ipsum_full` — confirmed absent
from both `eval_builtin` and `call_action_by_name`. `crates/goblin-interpreter/src/actions/ipsum.rs`
exists as a source file and `api/ipsum.gob` exists as a fixture, but no dispatch arm in
either function calls into it — the module appears **unwired/dead** in the interpreter.

### 1.16 Known-skipped, existence-checked only (per project instructions)

| name | file:line | note |
|---|---|---|
| money | lib.rs:3582, inside `fn cast_value_to_lock` | type-lock keyword only; always `Err("not yet implemented")` |
| db_query | dispatch lib.rs:14720 → `actions/db.rs:103` | present, not analyzed further (skip per instructions) |
| db_query_one | dispatch lib.rs:14721 → `actions/db.rs:172` | present, not analyzed further |
| db_exec | dispatch lib.rs:14722 → `actions/db.rs:239` | present, not analyzed further |

---

## 2. VM PARITY

VM sources: `crates/goblin-vm/src/value.rs` (`BuiltinId` enum, 423 variants),
`crates/goblin-vm/src/compiler.rs` (`builtin_by_name`, name→`BuiltinId` mapping),
`crates/goblin-vm/src/builtins.rs` (the ~357-arm dispatch), `crates/goblin-vm/src/vm.rs`
(`Opcode::CallBuiltin` handler — some builtins are intercepted here **before**
`builtins.rs` is ever reached, making their `builtins.rs` arm dead code).

### 2.1 VM-side architectural findings (independent of interpreter parity)

- **VM special-cases these in `vm.rs:1107–1436`, bypassing `builtins.rs` entirely**:
  `Gc`, `StashCount`, `TetherCount`, `Invoke`, `Summon`, `Provoke`, `Need`, `Tick`,
  `QueryByIdent` (synthesized for `repeat ClassName`), `Objects`, `Overlays`, `Map`
  (2-arg action-name form), `GetWhere`, `DeleteWhere`, `UpdateWhere`, `ReapWhere`,
  `ReapWhere2`, `PutWhere`, `GrabWhere`, `Filter`, `Reduce`, `Any`, `All`, `FindIndex`,
  `SortBy`, `MapFn`, `FilterFn`, `ReduceFn`, `ForEachFn`, `ResolveToken` (1-arg form only).
  Their `builtins.rs` arms exist only as unreachable defensive fallbacks (several literally
  `return NotImplemented`, e.g. `builtins.rs:2018`).
- **Orphaned `BuiltinId` variants** — have a live `builtins.rs` implementation arm but
  **no** `compiler.rs` name mapping, so are unreachable from any Goblin source text:
  `Grab, GrabFirst, GrabLast, GrabAt, GrabRandom, GrabWhere, GrabAll, GrabBetween,
  GrabMatching` (`value.rs:460–468`; arms `builtins.rs:1499–1540` — all `"grab_*"` source
  names instead map to the newer `Get*` family), `Reap` (`value.rs:491`; arm
  `builtins.rs:1640` — `"reap"` maps to `ReapSample` instead), `ReapFirst2, ReapLast2,
  ReapAt2, ReapWhere2, ReapRandom2` (`value.rs:509`; arms `builtins.rs:1922–1957`),
  `ToString` (`value.rs:410`; dead alias of `ToStr`), and **`Count`** (`value.rs:449`) —
  no name maps to it, and no independent `builtins.rs` arm was found for it either
  (`"count"` maps to `Len`, `builtins.rs:375`); `Count` appears to have **no
  implementation at all**.
- **Naming inconsistency**: `compiler.rs:2504` maps source name `"grab_where"` to
  `BuiltinId::GetWhere` (not `GrabWhere`). `vm.rs:1294` special-cases `BuiltinId::GrabWhere`
  — dead code, since nothing ever emits that ID.
- **`BuiltinId::Map` ambiguity**: `vm.rs:1192` intercepts `Map` unconditionally (requiring
  arg 2 to be a `Value::Str` action name), so the general-purpose `Map` arm at
  `builtins.rs:1403` is likely unreachable — not fully confirmed (would require reading
  `vm_map_inner`'s body for a possible internal fallback).

### 2.2 Parity table

`Y` = present and reachable; `Y (dead)` = a `BuiltinId`/arm exists but is unreachable per
§2.1; `N` = confirmed absent; `?` = interpreter presence itself is unresolved (§1.5).

| Builtin | In interpreter? | In VM? | Note |
|---|---|---|---|
| int, i | Y (6523, 13170 dup) | Y (`ToInt`, compiler.rs:2673, builtins.rs:2230) | |
| float, f | Y (6527, 13174 dup) | Y (`ToFloat`, compiler.rs:2674) | |
| bool | Y (13194) | Y (`ToBool`, compiler.rs:2675) | |
| str, string, to_str | Y (13182) | Y (`ToStr`, compiler.rs:2424/2609) | |
| big, b | Y (6531, 13178 dup) | Y (`ToBig`, compiler.rs:2658) | VM's `Big` is only a cast target — no arbitrary-precision Big type in VM per CLAUDE.md arch notes |
| pct, percent | Y (13186) | Y (`Pct`, compiler.rs:2627) | |
| to_map, m | Y (13190) | Y (`ToMap`, compiler.rs:2659) | |
| i8/i16/i32/i64/u8/u16/u32/u64/f32/f64 | Y (13198–13207) | Y (`CastI8`…`CastF64`, compiler.rs:2662–2671) | |
| date, time, datetime | Y (7256,7279,7302) | Y (`CastDate/CastTime/CastDatetime`, compiler.rs:2730–2732) | interpreter has full impl; **VM's returns "not yet implemented" per CLAUDE.md — re-verify against builtins.rs:3567-3637** |
| duration | Y (7326 — always errors, "not yet implemented") | Y (`CastDuration`, compiler.rs:2733) | both sides error; genuinely unimplemented in both |
| round, floor, ceil, abs, pow, sqrt | Y (dup arms, §0) | Y (compiler.rs:2417–2422) | |
| min, max, avg, sum, clamp | Y (6815,6897,6782,6753,6979) | Y (compiler.rs:2413–2416,2421) | |
| upper, lower, title, slug, raw, mixed | Y (7059–7078) | Y (compiler.rs:2430–2435) | |
| trim, trim_lead, trim_trail | Y (7066–7068) | Y (compiler.rs:2427–2429) | |
| find, find_all, ord | Y (14539–14541) | Y (compiler.rs:2573,2436,2437) | |
| len, count | Y (16034, 14550) | Y (`Len`, compiler.rs:2423 — both names map to same ID; `Count` orphaned, §2.1) | interpreter treats `count` as distinct arity-2 form (Str,sub); VM's `count`→`Len` loses that overload — **behavioral gap** |
| to_upper, to_lower | N (not found under these names; interpreter uses `upper`/`lower`) | Y (`ToUpperCase`/`ToLowerCase`, compiler.rs:2425–2426) | **VM-only aliases**, no interpreter equivalent name |
| split, join, contains, starts_with, ends_with, replace | Y (15711,15756,§1.4/1.6,17006,17025, — `replace` not directly cited by any pass, flag below) | Y (compiler.rs:2438–2443) | **`replace` UNRESOLVED on interpreter side — see §2.3** |
| before/after/before_last/after_last/keep_before/keep_after/keep_between | Y (15508–16876) | Y (compiler.rs:2444–2450) | |
| sanitize_bom, normalize_newlines | Y (16145,16165) | Y (compiler.rs:2451–2452) | |
| ignore_where/ignore_lines_where/ignore_matching/ignore_lines_matching/ignore_between/ignore_blocks/ignore_blocks_first/keep_matching | Y (16179–16745, `ignore_blocks_first` at 16595) | Y (compiler.rs:2453–2459,2630) | |
| is_matching, count_matching | Y (15835,15870) | Y (compiler.rs:2457–2458) | |
| json_parse, json_stringify, json_stringify_pretty | Y (15158–15223) | Y (compiler.rs:2460–2462) | |
| env | Y (14491) | Y (compiler.rs:2465) | |
| between (string builtin) | Y (15616) | Y (compiler.rs:2628, arity 3 x,lo,hi — **VM's `Between` looks numeric-range shaped, not string-span shaped — behavioral divergence, see §2.3**) | |
| is_control | Y (12529) | Y (compiler.rs:2629) | |
| pick | Y (13436) | Y (`Pick`, compiler.rs:2631) | |
| read_text, read_json | Y (14744,15268) | Y (compiler.rs:2660,2632) | |
| write_text!, append_file!, write_json! | Y (bang forms; non-bang errors both sides) | Y (`RequiresBang` pattern, compiler.rs:2633–2637) | |
| is_type, is_bound_name | Y (11753,11715) | Y (compiler.rs:2638–2639) | |
| yall_parse/parse_file/write/write_file/pretty/minify | Y (14985–15138) | Y (compiler.rs:2644–2649) | |
| create_dir!, copy_file!, delete_path!, zip_dir! | Y (bang-only; non-bang errors) | Y (compiler.rs:2650–2655) | |
| md_to_html, highlight_code | Y (11882,11890) | Y (compiler.rs:2656–2657) | |
| array_push | N (not found — interpreter has no bare `array_push` name in either dispatcher found) | Y (`ArrayPush`, compiler.rs:2661) | **VM-only, per current audit** |
| keys, values, items, has | Y (14544–14549) | Y (compiler.rs:2554,2555,2466,2553) | |
| shuffle, sort, freq, mode, sample_weighted | Y (14551–14555) | Y (compiler.rs:2467–2470) | |
| map | Y (14556) | Y (compiler.rs:2564, special-cased vm.rs:1192) | |
| unique, dups | Y (14557–14558) | Y (compiler.rs:2577,2471) | |
| is_empty | ? (§1.5, not confirmed) | Y (`IsEmpty`, compiler.rs:2557) | flag for interpreter re-check |
| pairs | ? (§1.5) | Y (`Pairs`, compiler.rs:2556) | flag for interpreter re-check |
| reverse, reverse_chars | Y (15956,15977) | Y (`Reverse`/`ReverseChars`, compiler.rs:2558–2559) | |
| minimize, parse_bool | Y (15983,15998) | Y (compiler.rs:2560–2561) | |
| sort_by, filter, reduce, any, all, find_index | ? (§1.5) | Y (compiler.rs:2563,2566,2568,2571,2572,2574; special-cased vm.rs) | flag for interpreter re-check |
| zip, flatten, slice | ? (§1.5) | Y (compiler.rs:2575,2576,2578) | flag for interpreter re-check |
| map_fn/filter_fn/reduce_fn/for_each_fn | ? (§1.5) | Y (compiler.rs:2564–2570; special-cased vm.rs) | flag for interpreter re-check |
| grab*/put*/update*/delete*/reap*/get* family (18 position×operation combos) | Y (all confirmed, §1.5) | Y (extensive family, compiler.rs:2499–2834 — with several orphaned dead variants, §2.1) | VM has a much larger surface here (legacy + new "Position×Operation" families both mapped); interpreter has one canonical family via `collection_operation` |
| print, println, eprint, eprintln | **N** (§1.8) | Y (compiler.rs:2579–2582) | **VM-only** — interpreter has no such builtins (uses a different `say`/print mechanism not audited here — see UNRESOLVED) |
| is_nil…is_nix (23 type-check predicates) | Y (§1.6) | Y (compiler.rs:2583–2606) | full parity confirmed |
| is_big | Y (12435) | Y (compiler.rs:2590) | **VM's `is_big` always returns `false`** ("no Big in VM" per code comment, builtins.rs:2131) — behavioral divergence, not a presence gap |
| assert, panic | **N** (§1.10) | Y (compiler.rs:2612–2613) | **VM-only** |
| type_of | **N** (§1.10, interpreter uses `valtype`/`vt`) | Y (`TypeOf`, compiler.rs:2611) | interpreter's `valtype`/`vt` (12359) has VM equivalent `ValType` (compiler.rs:2704) too — both languages have both names but under different builtin IDs; not a gap, just dual-named |
| range | **N** (§1.15) | Y (`Range`, compiler.rs:2472) | **VM-only**; interpreter's range literal (`a..b`) is a language construct, not a builtin — may be a false-positive gap, see UNRESOLVED |
| ipsum, ipsum_sentences, ipsum_paragraphs, ipsum_full | **N** (§1.15, unwired dead module) | Y (compiler.rs:2477–2480) | **VM implements what the interpreter left dead** |
| run_cmd | Y (14561) | Y (`RunCmd`, compiler.rs:2481) | interpreter returns JSON-encoded **Str**; VM returns a **Map/Value** directly (per VM pass) — **return-shape divergence** |
| rand_seed, roll, roll_detail, roll_str, roll_detail_str | Y (§1.9) | Y (compiler.rs:2498,2496,2497,2700,2701) | |
| req_method/path/query/body/header, cookie | Y (§1.13) | Y (compiler.rs:2484–2489) | |
| set_status/header/cookie | Y (§1.13) | Y (compiler.rs:2492–2494) | |
| secure_pick/secure_random/secure_shuffle | Y (§1.9) | Y (compiler.rs:2614–2616) | |
| pack, unpack | Y (§1.5) | Y (compiler.rs:2617–2618) | |
| lines, words, chars | Y (§1.4) | Y (compiler.rs:2619–2621) | |
| format | Y (12915) | Y (compiler.rs:2622) | |
| pad, pad_left, pad_right, repeat | ? — **not confirmed present in interpreter by any pass** (not in requested-category lists that were found) | Y (`Pad`/`PadLeft`/`PadRight`/`Repeat`, compiler.rs:2623–2626) | **flag: likely VM-only, needs interpreter grep confirmation** |
| mem_id | **N** (§1.10) | Y (`MemId`, compiler.rs:2406 — but **always errors** in VM too, "arena introspection not supported", builtins.rs:37) | both sides effectively non-functional |
| mem_addr, mem_total, mem_human | Y (§1.10) | Y (compiler.rs:2407,2475–2476 — VM's `mem_addr` also always errors, builtins.rs:44) | interpreter's `mem_addr` works (returns hex ptr); VM's does not — **behavioral gap** |
| gc | **N** (§1.10) | Y (`Gc`, compiler.rs:2408, special-cased vm.rs:1164 — returns Nil, no-op beyond triggering sweep) | **VM-only**, matches Blueprint's arena/GC design; interpreter has no arena |
| gc_mode, stash_count, tether_count | **N** (interpreter has no arena) | Y (compiler.rs:2409–2411) | **VM-only**, architecturally expected (Blueprint arena feature) |
| file_exists, is_file, is_dir, basename, dirname, stem, ext, path_join, path_split, path_normalize, path_relative_to, path_fix_separators, walk, list_dirs | Y (§1.8) | Y (compiler.rs:2677–2690) | |
| escape_html | Y (14760) | Y (compiler.rs:2691) | |
| url_encode, url_decode | **N** — not found in interpreter by any pass | Y (compiler.rs:2692–2693) | **flag: likely VM-only** |
| uuid_v4, uuid_v7 | Y (§1.8) | Y (compiler.rs:2694–2695) | |
| pathfind | Y (§1.8) | Y (compiler.rs:2696) | |
| ask, input | Y (11816) | Y (`AskInput`, compiler.rs:2698) | |
| valtype, vt | Y (12359) | Y (compiler.rs:2704) | |
| clear_format, format_info | Y (13114,13134) | Y (compiler.rs:2705–2706) | |
| backend, metrics | Y (16070,16104) | Y (compiler.rs:2707–2708) | |
| zip_dir | Y (bang-only) | Y (compiler.rs:2653,2711) | |
| year/month/day/hour/minute/second/weekday, add_duration, since, until, timezone, to_timezone, format_datetime/date/time, now/utc_now, epoch_ms/s, local_now, today/tomorrow/yesterday, to_iso, from_iso, to_epoch_ms, from_epoch_ms | Y (§1.7, full set) | Y (compiler.rs:2722–2752, full mirrored set) | full date/time parity in naming; behavioral equivalence not independently re-verified line-by-line |
| tick, tick_db | Y (7098) | Y (`Tick`, compiler.rs:2755, special-cased vm.rs:1137) | |
| decision_debug, overlays_of, overlay_strength, link_score | Y (§1.14) | Y (compiler.rs:2758–2761) | |
| owned_by, owns_tree, clone_object, delete_object, delete_overlays_on | Y (§1.14) | Y (compiler.rs:2762–2766) | |
| objects, overlays (query builtins) | **N** — not found as bare names in interpreter passes (interpreter may expose equivalent via different syntax, e.g. `repeat ClassName`) | Y (`Objects`/`Overlays`, compiler.rs:2769–2770, special-cased vm.rs:1152,1158) | **flag: needs interpreter-side re-check** |
| register_token, resolve_token, clear_token, clear_tokens, clear_all_tokens, list_tokens | Y (§1.13) | Y (compiler.rs:2714–2719) | |
| grid family (21 builtins) | Y (§1.14, full set) | Y (compiler.rs:2773–2793, full set) | full name parity; **0% test coverage on either side** (§6) |
| tokenize | Y (15904) | Y (`Tokenize`, compiler.rs:2795) | |
| get (bare) | Y (14765) | Y (`Get`, compiler.rs:2796/2499, builtins.rs:4509) | |
| http_get, http_post, http_put, http_delete, http_request | **N** — not found in interpreter by any pass (no `actions/http.rs` mentioned) | Y (compiler.rs:2837–2841) | **VM-only** — outbound HTTP client not in interpreter's audited surface |
| render_template | **N** — not found in interpreter by any pass | Y (`RenderTemplate`, compiler.rs:2844) | **VM-only** |
| invoke, summon, provoke, need | Y (§1.11) | Y (compiler.rs:2640–2643, special-cased vm.rs:1117–1136) | |
| money, db_query, db_query_one, db_exec | Y (skip, §1.16) | not enumerated by VM pass (out of scope, project instructs skip) | per CLAUDE.md, skip both sides |

### 2.3 UNRESOLVED (parity)

1. **`replace`**: listed as a VM builtin (`compiler.rs:2443`) and expected on the
   interpreter side per the project's own `CLAUDE.md` builtin list, but no interpreter
   pass in this audit produced a `file:line` citation for it. Needs a direct grep
   (`grep -n '"replace"' crates/goblin-interpreter/src/lib.rs`) before concluding
   presence/absence.
- **`is_empty`, `pairs`, `sort_by`, `filter`, `reduce`, `any`, `all`, `find_index`, `zip`,
  `flatten`, `slice`, `map_fn`/`filter_fn`/`reduce_fn`/`for_each_fn`, `pad`/`pad_left`/
  `pad_right`/`repeat`, `url_encode`/`url_decode`, `array_push`, `objects`/`overlays`
  (query builtins), `http_get`/`http_post`/`http_put`/`http_delete`/`http_request`,
  `render_template`**: none of these were confirmed present in the interpreter by any of
  the three interpreter-focused passes. Given the size of `call_action_by_name`
  (~5800 lines, read in ~700-line windows) and `eval_builtin` (~1170 lines, read in full),
  false-negative risk is non-trivial for names that might sit in an unread window. Treat
  every "N" and "?" row in §2.2 as **provisional** pending a targeted grep pass over
  `crates/goblin-interpreter/src/{lib.rs,actions/*.rs}` for each specific name before
  this is used to justify VM feature removal or interpreter feature addition.
2. **`between` behavioral shape**: interpreter's `between` (lib.rs:15616) is a 3-arg
   string-span extractor `(text, left, right) -> Str`. VM's `Between`
   (`compiler.rs:2628`) takes `(x, lo, hi) -> Bool` — this reads as a **different
   builtin that happens to share a name** with the parser-level `between`/`!between`
   comparison operator (see §3), not a true parity pair. Needs disambiguation against
   actual Goblin source using each form.
3. **`run_cmd` return shape**: interpreter returns a JSON-**string**; VM pass described
   the result as "Map/Value (subprocess result)" without confirming whether it's a raw
   Map or also a JSON string. If the VM returns a structured Map while the interpreter
   returns a string, GLAM code parsing `run_cmd`'s result would break identically on both
   backends only if it always calls `json_parse` first — worth an explicit runtime check.
4. **`is_big`**: VM's implementation is a hardcoded `false` (no Big type exists in the
   VM's `Value` enum at all per the Blueprint's architecture notes), while the
   interpreter's `is_big` can be true. This is an intentional design divergence per
   CLAUDE.md (VM doesn't have arbitrary-precision Big — `big`/`b` cast likely maps to a
   different internal representation), not a bug, but worth flagging since it's a
   behavioral gap a test suite would catch.

---

## 3. OPERATORS

### 3.1 Lexer token table

The lexer has no per-operator enum variants — all operators funnel into
`TokenKind::Op(String)` (`crates/goblin-lexer/src/lib.rs:33`, enum defined 14–42).

| spelling | meaning | file:line |
|---|---|---|
| `xx` | word-form block/section closer | lexer:1210 |
| `_` | standalone placeholder/"floor" operator | lexer:1220 |
| `$` | bare (non-money-literal) | lexer:1517 |
| `//=` | floor-div-assign | lexer:1736 |
| `//` | floor division | lexer:1742 |
| `/=` | div-assign | lexer:1748 |
| `/` | division | lexer:1754 |
| `...` | inclusive range | lexer:1770 |
| `..` | exclusive range | lexer:1776 |
| `.` | dot-call/member | lexer:1782 |
| `::` | namespace/enum-variant separator | lexer:1790 |
| `:` | slice/key separator | lexer:1796 |
| `( ) [ ] { } ,` | grouping/punctuation | lexer:1877–1891 |
| `===` | strict equality | lexer:1899 |
| `==` | equality | lexer:1905 |
| `=>` | arrow | lexer:1911 |
| bare `=` | **rejected** — error `L0113` | lexer:1913–1929 |
| `!==` | strict inequality | lexer:1936 |
| `!=` | inequality | lexer:1942 |
| `!` | logical-not (prefix) / postfix cast-bang | lexer:1948 |
| `<<` | lexed, **no parser consumer found** | lexer:1969 |
| `<=` | less-or-equal | lexer:1974 |
| `<>` | class sigil `<>Name`, else OR-alias operator | lexer:2009 (ClassIdent) / 2014 (Op) |
| `<` | less-than | lexer:2020 |
| `>>` | member access | lexer:2029 |
| `>=` | greater-or-equal | lexer:2035 |
| `><` | divmod | lexer:2041 |
| `>` | greater-than | lexer:2047 |
| `&&` | logical-and alias | lexer:2055 |
| `&=` | **rejected** — error `L0112` | lexer:2057–2064 |
| `&` | unary definedness / bitwise-and-ish prefix | lexer:2069 |
| `**=` | power-assign | lexer:2077 |
| `**` | power | lexer:2083 |
| `*>>` | dump/inspect postfix | lexer:2090 |
| `*=` | mul-assign | lexer:2096 |
| `*` | multiplication | lexer:2102 |
| `++` | concat / postfix-increment-ish | lexer:2110 |
| `+=` | add-assign | lexer:2116 |
| `+` | unary/binary plus | lexer:2122 |
| `--` | postfix decrement | lexer:2130 |
| `-=` | sub-assign | lexer:2136 |
| `-` | unary/binary minus | lexer:2142 |
| `%=` | mod-assign | lexer:2150 |
| `%s` | percent-of-self (digit-preceded) | lexer:2157 |
| `%o` | percent-o variant (digit-preceded) | lexer:2160 |
| `%` | modulo / percent literal | lexer:2163, 2170 |
| `^^` | alt-power | lexer:2178 |
| `^=` | lexed, **no parser consumer found** | lexer:2184 |
| `^` | postfix | lexer:2190 |
| `~=` | lexed, **no parser consumer found** | lexer:2198 |
| `~` | raw-field sigil | lexer:2204 |
| `\|!` | update-in-place statement sugar | lexer:2212 |
| `\|=` | declare-reassign | lexer:2218 |
| `\|\|` | **rejected** — error `L0114` | lexer:2220–2234 |
| `\|` | declare / bitwise-or-ish | lexer:2240 |
| `??` | nullish coalesce | lexer:2248 |
| `?>>` | optional member/call | lexer:2254 |
| `?` | postfix `.prop?` sugar | lexer:2260 |
| `;;` | (double semicolon) | lexer:2268 |
| `;` | statement separator | lexer:2274 |
| `@` | **rejected** — error `L0402` | lexer:2278–2288 |

Word-form logical operators (`and`, `or`, `not`, `is`, `is not`, `between`, `!between`)
are **not** lexer tokens — they arrive as plain `TokenKind::Ident` and are recognized by
the parser matching identifier text.

### 3.2 Parser precedence / associativity

No numeric precedence table exists — precedence is implicit via a recursive-descent call
chain, each level a `loop`-based left-associative binder unless noted. Entry:
`parse_coalesce`. Chain, loosest → tightest, all in `crates/goblin-parser/src/lib.rs`:

| level | operators | function | file:line | associativity |
|---|---|---|---|---|
| 1 | `??` | `parse_coalesce`/`parse_coalesce_impl` | 685 / 9020 | left |
| 2 | `or`, `<>` | `parse_or` | 9034 | left |
| 3 | `and`, `&&` | `parse_and` | 9054 | left |
| 4 | `is`, `is not`, `between`/`!between` (desugars to and/or of `<`,`<=`,`>`), `===`,`!==`,`==`,`!=`,`<=`,`>=`,`<`,`>` | `parse_compare` | 9076 | left (chains by rebinding `lhs`, not classic non-chaining — see UNRESOLVED) |
| 5 | `..`, `...` | `parse_range` | 9161 | left |
| 6 | `++`, `+`, `-` | `parse_additive` | 9176 | left |
| 7 | `><` (divmod), `//`, `*`, `/`, `%` | `parse_multiplicative` | 9222 | left |
| 8 | `**`, `^^` | `parse_power` | 9250 | **right** |
| 9 | unary `!`/`not`, `+`, `-`, `&` (definedness) | `parse_unary` | 10316 (`&` at 10320; `!`/`not` at 10978–10989; unary `+`/`-` at 10992–10999) | right (prefix) |
| 10 | postfix `(...)`,`[...]`/slice,`**`/`//` (postfix, disambiguated from binary by lookahead), `++`/`--`, `?`, `!`, `^`, `_`, `*>>`, `%o`,`%`,`%s` | `parse_postfix` | 11005 (body 11040–11235) | left-to-right chain |
| 11 (tightest) | `[...]`, `{...}`, `>>`, `?>>`, `.` | `parse_member` | 11240 (body 11247–11480+) | left-to-right chain |
| base | literals/identifiers/groups | `parse_primary`/`parse_primary_impl` | 575 / 8137 | — |

Declaration/assignment operators are **statement-level**, outside this expression
ladder: `|` (declare), `|=` (reassign) at `parser:5786–5788, 6287–6289`; compound-assign
sugar `+=,-=,*=,/=,%=,**=,//=,??=` (lowered to `name |= (name <op> rhs)`) at
`parser:6423–6499`; `|!` (lowered to `update!`/`update_at!`) at `parser:6371, 6465–6484`.

### 3.3 UNRESOLVED (operators)

- `<<`, `^=`, `~=` are lexed but no parser consumption site was found — likely dead
  lexer productions.
- `??=` is referenced in a parser match arm (`parser:6433`) but the lexer path to
  actually produce it is unclear (lexing `??` then `=` hits the bare-`=`-rejected error
  at `lexer:1913–1929`) — this parser arm may be dead/unreachable code.
- `parse_compare`'s chaining (`a < b < c` parses as `(a<b) < c`) is left-associative in
  implementation but doesn't enforce classic mathematical non-chaining semantics —
  flagged, not resolved, since it wasn't the audit's job to judge correctness.
- `parse_expr` (`parser:8092–8135`) looks like a separate, minimal, likely-unused
  expression entry point distinct from the real `PExpr` ladder — not confirmed dead or
  live.

---

## 4. SYNTAX FORMS

AST source: `crates/goblin-ast/src/lib.rs`. All construction sites verified against
`crates/goblin-parser/src/lib.rs`.

### 4.1 Statements (`Stmt` enum, `goblin-ast/src/lib.rs:42–77`)

| Form | syntax | parse fn | AST construction |
|---|---|---|---|
| Expr | expression-statement fallback | `parse_stmt` 5750 → 6526 | parser:6527 |
| Bind (tether) | `name \| expr`, typed `name.TYPE \| expr` | `parse_bind_stmt` 2659 | parser:3033 |
| Bind (retether) | `name \|= expr` | `parse_bind_stmt` 2659 (2813–2816) | parser:3033 |
| Bind (shadow) | `name [= expr` | `parse_bind_stmt` 2659 (2817–2820) | parser:3033 |
| Bind (`local`) | `local name \| expr` | `parse_local_bind` 501 | parser:547 |
| Bind (compound-assign sugar) | `name += / -= / *= / /= / %= / **= / ??= / //= expr` (ident-only LHS) | `parse_stmt` 6423–6511 | parser:6507 |
| TupleBind | `a, b \| expr` (also `\|=`, `[=`) | `parse_bind_stmt` 2830–2857 | parser:2850 |
| BoxBind | `#namespace::name \| expr` / `\|=` | `parse_stmt` 5776–5804 | parser:5801 |
| Class | `<>Name \| field: val, ... end/xx` | `parse_class_decl` 3843 | parser:6800 (also 6601 via `lower_class_stmt_from_pexpr` 6743) |
| Action (free) | `act name(params) => expr` or block form; `action` keyword variant | `parse_free_action` 2463 / `parse_action_after_keyword` 2395 | parser:2525,2539,2553,2632 |
| Enum | `enum Name \n Variant ... end/xx` | `parse_enum_decl` 4357, `lower_enum_stmt` 6530 | parser:6566 |
| Import | `import path`, `import "str"`, `import { a, b as c } from src` | `parse_import` 7903 | parser:7985,8010,8020,8053 |
| Use | `use namespace [as alias]` | `parse_use` 8060 | parser:8085 |
| Judge (stmt) | `judge [subj] [using X]: cond: body ... else: body end/xx` | `parse_judge_stmt` 9460 | parser:9608 |
| JudgeAll (stmt) | `judge_all ...` | `parse_judge_all_stmt` 9611 | parser:9758 |
| Sweep | `sweep target: pat: body, ..., all: body end/xx` | `parse_sweep_stmt` 9987 | parser:10056,10171 |
| Sweep (all mode) | `sweep_all ...` | same fn, mode flag | parser:10056,10171 |
| Return | `return expr[,expr...]`, bare `return`; `send` alias | `parse_return_stmt` 1990 | parser:2024,2058 |
| OverlayDef | `overlay Name \| ... end/xx` | `parse_overlay_def` 6853 | parser:7210 |
| OverlayApply | `overlay Name on target [at strength] [for N ticks]` | `parse_overlay_apply` 7227 | parser:7262 |
| OverlayDetach | `detach Name from target` | `parse_overlay_detach` 7272 | parser:7301 |
| LinkDef | `link ClassName [channel] by [ formula ]` | `parse_link_def` 7327 | parser:7383 |
| ObjectLinkDef | `ObjectVar link [channel] by [ formula ]` | `parse_object_link_def` 7388 | parser:7499 |
| LinkOffset | `VarA link to VarB on channel offset value [for N ticks]` | inside `parse_object_link_def` (7404–7456) | parser:7448 |
| ClearLink | `clear link VarA to VarB on channel` | `parse_clear_link` 7560 | parser:7592 |
| ObjectDecision | `VarName score \| decision against ClassName by [ formula ]` | `parse_object_decision` 7504 | parser:7557 |
| UnitDecl | `unit name \| types: a, b; N a = M b end/xx` | `parse_unit_decl` 7596 | parser:7676 |
| Block | anonymous nested block (internal grouping) | various (`parse_class_decl`, `lower_matrix_stmt`) | parser:3074, 6670 |

**Desugared to `Stmt::Expr(FreeCall(...))` — no dedicated `Stmt` variant:**

| Form | syntax | parse fn | construction |
|---|---|---|---|
| If | `if cond => body [elif...] [else...] end/xx` | `parse_if_stmt` 4646 | parser:5042 |
| Unless | `unless cond => body ...` (desugars to inverted `if`) | `parse_unless_stmt` 4934 | parser:4931 |
| For | `for name in iterable ... end/xx` | `parse_for_stmt` 5045 | parser:5133 |
| While | `while cond ... end/xx` | `parse_while_stmt` 5136 | parser:5215 |
| Repeat | `repeat N ... end/xx` | `parse_repeat_stmt` 5218 | parser:5352 |
| Collect | `collect count ... end/xx` | `parse_collect_stmt` 5359 | parser:5388 |
| Attempt | `attempt ... [rescue name => ...] [ensure ...] end/xx` | `parse_attempt_stmt` 5423 | parser:5593 |
| Provoke (stmt) | `provoke => cond` or block form | `parse_provoke_stmt` 9309 | parser:9337, 9457 |
| stop / stop if: cond | `stop`, `stop if: cond` | inline, `parse_stmt` 5837–5882 | parser:5868,5877 |
| skip / skip if: cond | `skip`, `skip if: cond` | inline, `parse_stmt` 5884–5929 | parser:5915,5924 |

### 4.2 Expressions (`Expr` enum, `goblin-ast/src/lib.rs:501–558`)

All produced via parser-internal `PExpr` and lowered by `lower_expr_preview`
(`parser:1659–1934`) / `lower_expr` (1936).

| Form | syntax | PExpr producer | lowering arm |
|---|---|---|---|
| Nil | `nil` | `parse_primary_impl` 8696 | parser:1670 |
| Bool | `true`/`false` | `parse_primary_impl` 8693–8695 | parser:1667 |
| Number | int/float, `_` separators, radix, unit/type suffixes | `parse_primary_impl` ~8768+ | parser:1663–1666 |
| Str | `"..."`, sugar `..`→`""` | `parse_primary_impl` (8172–8175) | parser:1668 |
| Char | `,` `.` `_` `'` shortcuts in context | `parse_literal_token` 1067–1122 | parser:1669 |
| Ident | bare identifier / `:name` builtin | `parse_primary_impl` 8697, 8222 | parser:1662 |
| Slice | `expr[start..end]` | postfix/index parsing | parser:1908–1915 |
| Slice3 | `expr[start..end..step]` | same | parser:1917–1926 |
| Array | `[e1, e2, ...]` | `parse_primary_impl` 8503–8537 | parser:1699–1705 |
| Object | `{ key: val, ... }` | `parse_primary_impl` 8705–8764 | parser:1706–1712 (+ TemplateApply sugar 1895–1906) |
| Member | `obj.name` | `parse_member` 11240 | parser:1849–1852 |
| OptMember | `obj?.name` | `parse_member` 11240 | parser:1853–1856 |
| Index | `obj[idx]` | `parse_postfix` 11005 | parser:1857–1861 |
| IndexMap | `obj{key}` | `parse_postfix` 11005 | parser:1862–1866 |
| Index2 | `grid[x, y]` | `parse_postfix` 11005 | parser:1867–1872 |
| Call | `recv.name(args)` | `parse_member`/postfix, `parse_args_paren` 11664 / `parse_args_colon` 11687 | parser:1817–1824 |
| OptCall | `recv?.name(args)` | same | parser:1839–1846 |
| FreeCall | `name(args)`, whitelisted bare-no-paren (8629–8688), sugar (`skip`,`stop`,`reap!`,`:builtin(...)`) | various sites (8204,8220,8558,8563,8626,8687) | parser:1825–1831 |
| NsCall | `Ns::name(args)` | member/postfix call path | parser:1832–1838 |
| Prefix | unary `-x`,`!x` | `parse_unary` 10316 | parser:1875–1878 |
| Postfix | `%`,`%s`,`*>>`(Dump),etc. | `parse_postfix` 11005 | parser:1884–1887 (+1879–1883 Dump) |
| Binary | arithmetic, `++`, comparisons, `and`/`or`, ranges, `of` (percent-of) | `parse_additive`/`parse_multiplicative`/`parse_power`/`parse_compare`/`parse_range`/`parse_and`/`parse_or` | parser:1888–1892 |
| EnumVariant | `EnumName::Variant` or `EnumName::Variant(field: val,...)` | member/postfix `::` chain | parser:1713–1724 |
| Judge (expr) | `judge [subj] [using X] [return expr]: cond: val ... else: val end/xx` | `parse_primary_impl` 8228–8329 | parser:1726–1763 |
| Judge (all, expr) | `judge_all ...` | `parse_primary_impl` 8332+ | parser:1765–1802 |
| Block | `PExpr::Block`; also direct `ast::Expr::Block{}` for desugared if/for/while/repeat/attempt/collect bodies | various (4691,4821,5034,5125,5212,5336,5569,9452) | parser:1804–1814 (+ direct literals elsewhere) |
| LiteralToken | `{{{ module::ident }}}` | `parse_literal_token` 327 | parser:1694–1696 |
| BoxVar | `#namespace::name` | `parse_primary_impl` 8154–8168 | parser:1697 |

**Dead `PExpr` variants** (declared, never constructed): `Assign`, `MutateAssign`,
`TupleAssign`, `BlobStr`, `BlobNum`, `Date`, `DateTime`, `Money`, `Time` — only appear in
exhaustiveness-guard match arms (e.g. `parser:889`). True `=` assignment is explicitly
disabled: `parse_assign` (line 607) comment states *"Assignment expressions are removed
from the AST"*; forwards to `parse_coalesce()`.

### 4.3 Block terminator convention — CORRECTS CLAUDE.md

`CLAUDE.md` states *"ALL goblin blocks end with both `xx` AND `end` terminators."* This
audit's parser-focused pass found this to be **inaccurate**: `end` and `xx` are
**interchangeable alternative single closers** — a block consumes exactly *one* of them,
never both.

- `eat_block_close` (`parser:3268–3278`): tries `end`, else `xx`, returns on first match.
- `expect_block_close` (`parser:3281–3300`): errors unless current token's value is
  `"end"` OR `"xx"` — a single token, either spelling.
- Every block-closing call site consumes exactly one token: `parse_if_stmt` (4807–4815),
  `parse_unless_stmt` (4998–5006), `parse_for_stmt` (5096–5104), `parse_while_stmt`
  (5177–5185), `parse_repeat_stmt` (5316–5330), `parse_attempt_stmt` (5527),
  `parse_class_decl`/`parse_class_decl_keyword` (4071–4108), `parse_enum_decl`
  (4390–4410), `parse_overlay_def` (6992/7029/7084/7161), `parse_unit_decl`
  (7774–7854), judge/judge_all statement and expression forms, `parse_sweep_stmt`,
  `parse_provoke_stmt` (9419–9436).

**This should be treated as a documentation correction, verified against source, not an
ambiguity.**

### 4.4 Assignment / bind forms

- `name | expr` → `BindMode::Tether` (`parser:2802–2805`)
- `name |= expr` → Retether (`parser:2813–2816`)
- `name [= expr` → Shadow (lexer emits dedicated `TokenKind::Shadow` for `[=`) (`parser:2817–2820`)
- `name <> ClassName | field: val, ...` → object-construction sugar via `is_object_construct` (`parser:2806–2894`), reuses `Stmt::Bind`, not a distinct variant
- `a, b | expr` (+ `|=`, `[=`) → `Stmt::TupleBind` (`parser:2830–2857`)
- True `=` assignment does not exist; `parse_assign` (607–612) is a compatibility shim only
- Compound assign (`+= -= *= /= %= **= ??= //=`) and `|!` update-sugar are statement-level
  lowering only, no dedicated AST node (`parser:6423–6511`, `6359–6421`)
- `#ns::name | expr` / `|=` → `Stmt::BoxBind` (`parser:5776–5804`)

### 4.5 Literals

- Numbers: `PExpr::Int/Float/IntWithUnit/FloatWithUnit` all collapse to a single
  `ast::Expr::Number(String, Span)` (raw text preserved).
- Strings: standard `"..."` plus sugar `..`→empty string; `nc`,`,`,`.`,`_`,`'` map to
  Char/Str("none") shortcuts in token-literal contexts (`parser:1064–1122`).
- Ranges: no dedicated AST node — `a..b`/`a...b` parsed by `parse_range` (9161–9174),
  lowered to `Expr::Binary(lhs, ".."/"...", rhs)`.
- Arrays: `[e1, e2, ...]`, trailing comma/newline tolerant (`parser:8503–8537`).
- Maps/objects: `{ key: val, ... }` (`parser:8705–8764`); keyed access via `obj{key}`.
- Pairs: no distinct AST "Pair" expression type — key-value pairs only appear inside
  Object/EnumVariant/TemplateApply/judge-arm lists.

### 4.6 Class / Enum / Overlay / Link / Unit — concrete syntax

- Class: `<>Name [!] | field: val, ... [act name(...) => expr | ... end] [score | decision
  against T by [formula]] [judge ... end] [transition ...] [capacity: N] end/xx`
  (`parse_class_decl` 3843)
- Enum: `enum Name \n Variant1 \n Variant2(field: default, ...) ... end/xx`
  (`parse_enum_decl` 4357)
- Overlay def: `overlay Name | [host types] [spreads] [decay] [modifiers] [conflicts]
  [transitions] end/xx` (`parse_overlay_def` 6853)
- Overlay apply: `overlay Name on target [at strength] [for N ticks]`
  (`parse_overlay_apply` 7227)
- Overlay detach: `detach Name from target` (`parse_overlay_detach` 7272)
- Link (class-level): `link ClassName [channel] by [ formula ]` (`parse_link_def` 7327)
- Link (object-level): `ObjectVar link [channel] by [ formula ]`
  (`parse_object_link_def` 7388)
- Link offset: `VarA link to VarB on channel offset value [for N ticks]` (same fn, 7404–7456)
- Clear link: `clear link VarA to VarB on channel` (`parse_clear_link` 7560)
- Decision: `VarName score | decision against ClassName by [ formula ]`
  (`parse_object_decision` 7504)
- Unit: `unit name | types: a, b; N a = M b end/xx` (`parse_unit_decl` 7596)
- Import: `import path/parts`, `import "string/path"` (dynamic if contains `{`),
  `import { a, b as c } from source`, optional `as alias` (`parse_import` 7903)
- Use: `use namespace [as alias]` (`parse_use` 8060)

### 4.7 UNRESOLVED (syntax forms)

- `PExpr::Assign/MutateAssign/TupleAssign/BlobStr/BlobNum/Date/DateTime/Money/Time`:
  declared but no construction site found in `goblin-parser/src/lib.rs`. A literal-form
  fast path for `Date`/`Time`/`DateTime`/`Money`/`Duration` token kinds (which do exist in
  the lexer) may live past line 8768 of `parse_primary_impl`, not exhaustively read.
- `Expr::Judge`'s `all` flag only distinguishes Judge/JudgeAll in *expression* position;
  statement forms are separate types (`JudgeStmt`/`JudgeAllStmt`, confirmed distinct —
  `goblin-ast/src/lib.rs:433–444`).
- `parse_matrix_decl`/`lower_matrix_stmt` (class matrix declarations, `<>Name matrix ...`)
  dispatch confirmed (`parser:6206–6210`, `lower_matrix_stmt` 6574) but the matrix-cell
  grammar (`nc`, `::`) was only spot-checked, not exhaustively verified.

---

## 5. RESERVED WORDS

The lexer recognizes only a small fixed set of words as dedicated token kinds
(`lex_identifier`, `crates/goblin-lexer/src/lib.rs`):

| keyword | file:line |
|---|---|
| `blob` → `TokenKind::Blob` | 1254–1256 |
| `date` → `TokenKind::Date` | 1257–1259 |
| `time` → `TokenKind::Time` | 1260–1262 |
| `datetime` → `TokenKind::DateTime` | 1263–1265 |
| `act` → `TokenKind::Act` | 1266–1268 |
| `action` → `TokenKind::Action` | 1269–1271 |
| `import` → `TokenKind::Import` | 1272–1274 |
| `use` → `TokenKind::Use` | 1275–1277 |
| `export` → `TokenKind::Export` | 1278–1280 |
| `vault` → `TokenKind::Vault` | 1281–1283 |
| (everything else) → `TokenKind::Ident` | 1284–1286 |

**Everything else that behaves like a keyword is a soft/contextual keyword** — lexed as
plain `Ident`, recognized only by the parser matching the identifier's string value.
Confirmed soft keywords found via parser scan (non-exhaustive): `act, action, and, as,
at, attempt, between, capacity, clamp, clear, collect, detach, dups, elif, else, end,
enum, for, from, if, imm, into, is, judge, judge_all, link, local, matrix, nc, none, not,
of, on, or, overlay, per, pick, provoke, reap, score, secure_pick, show_ids, skip, stop,
sweep, sweep_all, tick, ticks, to, transition, tz, unique, unit, unless, using, while,
with, without, wo, xx`.

**UNRESOLVED**: this soft-keyword list was compiled from a scan of literal-string
matches on `peek_ident()`/`value.as_deref()` in the parser and is explicitly
non-exhaustive per the originating pass — it should not be treated as a closed set
without a second, exhaustive grep pass over every `== "word"` comparison against an
identifier token in `goblin-parser/src/lib.rs`.

---

## 6. EXISTING TESTS

### 6.1 Rust unit tests (`#[cfg(test)]`)

| Location | Count | Covers | Example |
|---|---|---|---|
| `crates/goblin-parser/src/lib.rs:11752–11791` | 5 | Only `validate_interpolation_braces` (string-interpolation brace balancing) — nothing else in the parser | `ok_plain_interpolation` (11764) |
| `crates/goblin-vm/src/exec.rs:296–444` | 11 (note: `mod tests {}` block structurally closes at line 316; tests from 318 onward sit **outside** `#[cfg(test)]`, though `#[test]` attributes still register them under `cargo test`) | Runs real Goblin source through `execute_source()` (full parse→compile→VM). Only exercises: `if`/`say`, `secure_pick`, `:pack`, `raw`. Several tests (`password_gbln`, `password_step_by_step`, `password_step4`, `password_step5`, `char_range_pick`) have no `assert!` — they only fail on panic | `hello_world` (304–307) |
| `crates/goblin-vm/src/vm.rs:3528–3690` | 6 | Low-level opcode execution via hand-built `FunctionObject{bytecode: vec![Opcode::...]}` — **bypasses parser/compiler entirely**. Covers `LoadConst`, `AddInt`, `StoreLocal`/`LoadLocal`, `JumpIfFalse`/`Jump`, `Call`, `MakeArray` | `add_ints` (3562–3570) |
| `crates/goblin-vm/src/session.rs:620–652` | 3 | Arena/GC primitives: alloc/read, stale-tether detection after sweep, overwrite | `stale_address_detected` (633–641) |
| `crates/goblin-source/src/lib.rs:5–13` | 1 | Unmodified `cargo new` placeholder (`add(2,2)==4`) — not a real test | 9–12 |

**Crates with zero tests of any kind**: `goblin-ast`, `goblin-lexer`, `goblin-interpreter`,
`goblin-cli`, `goblin-des`, `goblin-devserver`, `goblin-diagnostics`, `goblin-gql`,
`goblin-host`, `goblin-http`, `goblin-wasm`. Notably **`goblin-interpreter`** — the
~24–30k-line tree-walking interpreter that `goblin-cli` actually calls
(`crates/goblin-cli/src/main.rs:279–287`) — has **no test infrastructure whatsoever**
(confirmed via `grep -rn '#\[test\]\|#\[cfg(test)\]\|mod tests' crates/goblin-interpreter/` → zero matches).

### 6.2 `crates/goblin-yall/tests/*.rs` (cargo integration tests)

6 files, 45 `#[test]` fns total — all testing the YALL config-file format, **unrelated to
the Goblin language**: `errors.rs` (7), `multiline.rs` (6), `nil.rs` (6), `parser.rs` (9,
e.g. `parses_top_level_map` at `crates/goblin-yall/tests/parser.rs:14–21`), `writer.rs`
(10), `comments.rs` (7).

### 6.3 `tests/lex/{ok,err}` — a real, wired harness

Harness: `run_lex_check()` in `crates/goblin-cli/src/main.rs:458` (invoked as
`goblin-cli lex --check`, wired at 232–238). Walks `tests/lex/ok` and `tests/lex/err`
(`collect_tests`, 697), lexes each `.gbln` fixture, diffs the token stream against a
paired `.expect.txt` oracle (`read_expect_summary` 804, `compare_expect` 896).

- `tests/lex/ok/`: 38 `.gbln` fixtures — lexer/tokenization coverage only (numeric/string/
  dice/money/duration literals, comments, indentation, operators). No parsing/execution.
- `tests/lex/err/`: 8 fixtures for expected lex errors.

This is the **only** automated, wired test harness in the repository, and it is scoped
strictly to the lexer stage.

### 6.4 Everything else under top-level `tests/` — unwired manual smoke files

Confirmed via `grep -rln` across all `*.rs`: **no Rust code reads any of these** except
`tests/if.gbln` (read by `exec.rs:311`, with a fallback string so not even required to
exist) and `api/password.gbln` (read by `exec.rs:331`).

- `tests/*.gbln`/`*.gob`/`*.goblin` at the tests root (26 files, e.g. `test.gbln` —2907
  lines, `europe_sim.gbln` —675 lines, `grid_test.gob` —69 lines, `sweep_test.gbln` —89
  lines, `syntax.gbln` —57 lines) — presumably hand-run via `goblin run <file>`.
- `tests/scratch/` — 63 `.gbln` files (parser/lexer edge-case smoke scripts). Not
  referenced by any Rust source.
- `tests/tmp/sweep/` — leftover fixture/output files, not test inputs.
- `tests/glams/local/` — a manual glam-module fixture, not invoked by any test runner.

### 6.5 CI

**No `.github/workflows` directory and no CI config exists in this repo at all**
(`find -iname '*.yml' -o -iname '*.yaml'` under `.github` returns nothing). No automated
`cargo test` or `lex --check` runs anywhere except by a human running it manually.

### 6.6 Coverage-fraction assessment (grep-cited)

- VM's `builtins.rs` dispatches **380** distinct `BuiltinId` variants (`grep -oP
  'BuiltinId::\w+' builtins.rs | sort -u | wc -l`); `compiler.rs` maps **402** source-level
  names onto those IDs (`grep -c '=> BuiltinId::' compiler.rs`).
- Of those ~380 builtins, only **3 actual builtin names** (`secure_pick`, `:pack`, `raw`
  — plus `if`/`say` which are control-flow, not builtins) are ever invoked through real
  Goblin source in an automated test — **under 1%**.
- `crates/goblin-interpreter/src/actions/grid.rs` defines 135 `grid_`-prefixed
  identifiers; only `tests/grid_test.gob` references grid builtins, and no test runner
  executes that file — **grid actions: 0% automated coverage**.
- The VM opcode tests cover 6 hand-built opcodes out of a much larger `Opcode` enum, and
  never go through the parser/compiler — they don't validate that Goblin syntax actually
  compiles to correct bytecode for anything beyond that toy set.
- `goblin-interpreter` (the actual interpreter used by `goblin-cli`) has **zero**
  automated tests — 0% coverage by construction.
- Parser coverage is limited to one helper function
  (`validate_interpolation_braces`); the ~11,000-line statement/expression grammar has no
  direct Rust-level test — its only indirect exercise is via `exec.rs`'s handful of
  `execute_source` calls and the unwired `tests/scratch/*.gbln` files.
- Lexer coverage (38 ok + 8 err fixtures) is comparatively solid for token-level syntax
  (numbers, strings, dice, money, durations, comments, indentation, operators) but says
  nothing about parsing, semantics, or builtins.

**Overall**: of the full language surface (lexer syntax, parser grammar, ~380 VM
builtins + ~200+ interpreter builtins, tree-walking interpreter semantics, VM opcodes),
only lexer tokenization is meaningfully covered by an automated, repeatable harness.
Parsing, execution semantics, and the entire builtin/action library are almost entirely
unexercised by anything that runs automatically. There is no CI wiring to run even the
tests that exist.

---

## 7. TOP-LEVEL UNRESOLVED (cross-cutting, prioritized)

1. **Split builtin dispatch architecture** (§0): the interpreter has two large,
   independently-maintained name→behavior tables (`eval_builtin`,
   `call_action_by_name`) with confirmed duplicate/overlapping arms for at least a
   dozen builtin names, plus one dead-code duplicate (`"after"` at two sites) and two
   dead-code shadowed arms (`raw`, `mixed` each defined twice). This should be resolved
   or at minimum documented as intentional before using either function as sole source
   of truth for future VM parity work.
2. **Provisional "N"/"?" rows in the VM parity table** (§2.2, §2.3): `is_empty`, `pairs`,
   `sort_by`, `filter`, `reduce`, `any`, `all`, `find_index`, `zip`, `flatten`, `slice`,
   the `*_fn` family, `pad*`/`repeat`, `url_encode`/`url_decode`, `array_push`,
   `objects`/`overlays` query builtins, `http_*`, `render_template`, and `replace` all
   need a direct, targeted grep against `crates/goblin-interpreter/src/{lib.rs,actions/*.rs}`
   before concluding they're genuinely VM-only additions rather than audit blind spots
   in the ~5800-line `call_action_by_name` function.
3. **`ipsum*` and `range` modules are dead in the interpreter** (§1.15) despite having
   source files (`actions/ipsum.rs`) and fixtures (`api/ipsum.gob`) — confirm whether
   this is intentional (feature retired) or an oversight, since the VM does implement
   both.
4. **Zero test coverage on `goblin-interpreter`, and near-zero on the VM's actual
   builtin/opcode/parser surface, with no CI** (§6) — this is the single largest risk
   in the codebase per this audit and is almost certainly the reason "VM missing
   builtins vs. interpreter" was suspected in the first place: neither side is
   regression-tested, so drift between them is silent by construction.
