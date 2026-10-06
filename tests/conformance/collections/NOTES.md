# Collections conformance area: notes

There are 219 cases: 207 have a `.out` file and 12 are `undecided`. 63 cases carry `known-gap: vm`, 37 carry `known-gap: interp` and 1 carries `known-gap: both`. Some cases carry two `known-gap` lines, one per engine, when each engine fails in a different way. Every case was checked against the baseline binary. Every engine not listed as a gap matches the `.out` exactly, every listed engine does not, and every `undecided` case does disagree.

A case can carry **two `known-gap` lines** (`interp` and `vm`) when each engine fails for its own reason. Examples are `put_all_bang`, `reap_random_bang`, `reap_all_bang`, `grab_whole`, `sort_by_numeric_key` and `array_push_returns_new`. The harness must collect every `known-gap` line, not only the first one.

Sources of truth used:
- `docs/cheat-sheet.md` (CS)
- `docs/language-spec.md` (LS)
- `AGENTS.md` (for builtins that no user doc covers, it states "crates/goblin-interpreter/src/lib.rs — source of truth for behavior" and "Match interpreter behavior exactly")
- the Sheriff sources, for idioms (`:put_last!`, `:update!(ctx["body"], v)`, nested `:update!(fm_map["categories"], [])`)

## (a) Disagreements and classification

Abbreviations: I is the interpreter, V is the VM, gap is known-gap, und is undecided.

### Value::Collection display and type problems (VM)
| case | I | V | class | evidence |
|---|---|---|---|---|
| map_print_simple / map_print_nested / map_print_empty / array_of_maps_print / freq_print | `{a: 1, b: 2}` etc. | values only (`[1, 2]`, `[]`) | gap vm | Spec §14 Maps / `freq ... /// {sword: 1, potion: 2}` (LS:2146). Printing a map as its values is an internal display bug. |
| slice_basic, slice_open_end, slice_open_start, slice_full_is_copy, slice_of_built_list, slice_literal_expression | works | `slice expects array or string, got collection` | gap vm | LS §14 Slicing (`nums[0:3]`, `nums[2:]`, `nums[:]`) |
| equality_arrays / nested / empty / maps / map_with_list_value, inequality_arrays | structural | always false (`!=` always true) | gap vm | CS:109 "`==` value equality (… containers compare structurally)" |
| has_array_values_not_indices, has_array_strings, has_nested_array_element, pick_one_is_member | membership | `has(arr, x)` is true iff `x` is a valid **index** (`:has([4,5,6],5)` is false, `:has([1,2,3],2)` is true) | gap vm | CS:447 `has "needle" in s` → bool. The VM's `collections::has` checks keys, not elements. |
| is_array_is_map | `{}` → is_map true | is_array true, is_map false for a map literal | gap vm | type predicate bug (builtins.rs:2127-2128 do not inspect the Collection kind) |
| items_map | `[(a, 1), (b, 2)]` | `expected map, got collection` | gap vm | Interp is the reference. `items` exists in both engines. |
| get_whole_collection, grab_whole | `[1, 2]` | `expected collection, got collection` | gap vm | internal type error |
| put_bang_replaces_whole, update_all_bang, put_all_bang | replaces | `expected array, got collection` (collections.rs:231) | gap vm | internal type error |
| array_push_returns_new | (no builtin) | `expected array, got collection` | gap interp + gap vm | Missing in interp. The VM arm (builtins.rs:3073) only matches `Value::Array`. |
| secure_shuffle_preserves_elements, secure_pick_single_source, sample_weighted_single_source, reap_config_map | works | rejects a literal (`got collection`) | gap vm | internal type error |
| sort_mixed_int_float | `[1, 2.5, 3]` | `[2.5, 1, 3]` (unsorted) | gap vm | LS §14 "sort scores → sorted copy" |
| delete_where_bang_map | `[a]` (filters entries by value) | map turned into an array of values | gap vm | interp reference (lib.rs `Position::Where` map arm, ~9560) |
| delete_first_bang_empty_error | error (empty-array) | silently succeeds | gap vm | interp reference |
| put_at_bang_at_length_appends | `[1, 2, 3, 9]` | index out of bounds | gap vm | Interp reference: Put allows `idx == len` (lib.rs array `Position::At` Put). This matches the spec's `insert … at i` semantics. |
| count_string_needle | 3 | arity error (`count` is an alias of `len`, compiler.rs:2423) | gap vm | CS:450 `count "needle" in s → occurrence count` |

### Mutation and write-back (VM)
| case | I | V | class | evidence |
|---|---|---|---|---|
| update_bang_nested_* (map in map, map in list, list in map, three levels, variable key, local in action, global from action) | works | compile error `'update!' index target must be a plain variable` (compiler.rs:2255-2271) | gap vm | Sheriff uses nested targets, e.g. `frontier.gbln:44 :update!(fm_map["categories"], [])`, and has 46 nested sites per the audit. Interp: `parse_lvalue` and `get_lvalue_mut` (lib.rs:17700-17730, 17185). |
| put_last_bang_nested_target, delete_at_bang_nested_target | mutates `m["k"]` | silently no write-back (compiler.rs:1082-1090 only writes back when arg 0 is an `Ident`) | gap vm | consistent with the interp lvalue path |
| reap_first/last/at/where/matching/between_bang, reap_at_bang_map | returns the item, source shrinks | **stores the reaped item into the variable** (`l` becomes `1`) | gap vm | Generic bang write-back (compiler.rs:1082) stores the builtin result, which is the reaped value, not the remainder. Interp: lib.rs ~18560 "reap_*! : return picked; write back updated via delete_*". |
| reap_random_bang | — | same write-back bug | gap interp (missing) + gap vm | |
| reap_all_bang | — | source unchanged | gap interp (missing) + gap vm | |
| update_bang_global_key_from_action, update_bang_global_whole_from_action, put_last_bang_global_from_action | global mutated | **global unchanged after the action returns** | gap vm | LS:229-240 `op add_point() score = score + 1 /// updates global score`. Even a scalar `n |= n + 1` inside an `act` does not persist on the VM (probe only, not a case here; that belongs to the core area). |
| reap_sentence_from, reap_sentence_count | **does not remove** from the source (`len` stays 3) | `reap: expected map, got collection` | gap interp + gap vm | LS §14 "reap mirrors pick but removes selected elements from the source collection", CS:675 |

### Missing builtins (interp lacks, VM has)
Each of these is `known-gap: interp` "unknown action": contains, pairs, put_where, put_all, reap_random, reap_all, grab, grab_first, grab_last, grab_at, grab_all, grab_random, grab_where, grab_matching, grab_between, find_index, filter, filter_fn, map_fn, reduce, reduce_fn, for_each_fn, any, all, sort_by, flatten, zip, range, is_empty, is_collection, array_push. The expected output is the VM's behaviour, which is plausible and not contradicted by docs. One exception is `sort_by_numeric_key`, see below.

| case | I | V | class |
|---|---|---|---|
| sort_by_numeric_key | missing | `[10, 100, 9]`, because VM `vm_sort_by_inner` (vm.rs:2918) compares keys by their **string** form | gap interp + gap vm (intended `[9, 10, 100]`) |
| index_negative, slice_negative_start | `index must be a non-negative integer` | works / slice fails for the Collection reason | gap interp (LS §14 "negative indices count from the end", CS `arr[-1]`) |
| reductions_dot_form (`l.sum` `.min` `.max` `.avg`) | unknown postfix action | 9 1 5 3 | gap interp (CS "nums.sum nums.avg nums.min nums.max nums.len", LS §14 `scores.min`) |
| avg_returns_float | `is_float(avg([800,…]))` false | true | gap interp (LS §14 `avg_v = scores.avg /// 1000.0`) |
| mode_single | `{2: 2}` (a map) | `[2]` | gap interp (LS:2147 `mode [1, 2, 2, 3] /// [2]`) |

### Both engines wrong
| case | both print | class | evidence |
|---|---|---|---|
| range_literal_as_array | `1..3` → `[1, 2]` (and `for i in 1...4` yields 1..4) | gap both, intended `[1, 2, 3]` | LS:1440-1444, 1464 "Numbers: .. inclusive end, ... exclusive end"; CS:607-608. Reported to conf-core. |

### Undecided
| case | slug | I | V |
|---|---|---|---|
| map_missing_key_read, map_missing_key_dot_read | D-missing-key-read | error R0403 | `nil` |
| update_bang_missing_map_key | D-update-missing-key | inserts (prints 2) | error "key not found" |
| keys_insertion_order, map_print_key_order, map_iteration_order | D-map-key-order | sorted (BTreeMap) | insertion order |
| get_matching_no_match | D-get-matching-empty | error R0701 | `[]` |
| keys_on_array | D-keys-on-array | error "keys expects a Map" | `[0, 1]` |
| find_in_array | D-find-on-array | error "find expects a string" | `1` (element index, `nil` if absent) |
| join_numbers | D-join-non-strings | error (strings/chars only) | `1,2` |
| slice_builtin | D-slice-builtin-end | unknown action | `:slice([1,2,3,4],1,3)` → `[2, 3, 4]` (inclusive end for arrays, but exclusive end for strings in the same builtin) |
| to_map_from_map | D-to-map | `{{a: 1}}` | `{}` |

## (b) Undecided slugs: evidence and what each choice would touch

- **D-missing-key-read.** Docs say nothing explicit about reading a missing key. Spec Errors lists `PickIndexError` for pick only. The interp diagnostic help says "Check the key exists". Choosing "error" means VM `collections::get_index` (collections.rs:1081, `unwrap_or(Value::Nil)` at :1097) must return `KeyNotFound`. Choosing "nil" means the interp index read paths (lib.rs:17432/17447/19297, "missing key") must return `Value::Nil`, along with dot access at lib.rs:17243/17382.
- **D-update-missing-key.** Sheriff relies on insert (audit §2 blocker 8, `res["negotiated"]`). The interp `update!` on a path (lib.rs:17700-17730) uses `get_lvalue_mut` (lib.rs:17185), which creates the slot. The functional `update_at` errors on a missing key in **both** engines (case `update_at_bang_missing_key_error`), so the interp's `update!` sugar is the odd one out. Choosing "insert" means VM compiler.rs:2255-2265 must lower `update!(m[k], v)` to `put_at` semantics (or the `UpdateAt` arm at collections.rs:469 must stop returning `KeyNotFound` for that path). Choosing "error" means interp `get_lvalue_mut` must refuse to create map keys.
- **D-map-key-order.** The docs favour insertion order. LS §14 `prices = {sword, shield, potion}; prices.keys /// ["sword","shield","potion"]` is not sorted. `freq ["sword","potion","potion"] /// {sword: 1, potion: 2}` (LS:2146) is also insertion order. The interp has a `MapOrd` (IndexMap) variant, but literals build `Value::Map` (BTreeMap). Choosing "insertion" means interp map literals become `MapOrd`, and `keys`/`values`/iteration (`actions::maps::keys`, lib.rs:14544; the `for` arm, lib.rs ~20290) follow. Choosing "sorted" means VM `MakeMap` (vm.rs:785) and `collections::keys` must sort.
- **D-get-matching-empty.** No docs. The audit lists it as a semantic difference. Interp: lib.rs:11063 (array) and the map arm (~9700) raise R0701. VM: collections.rs:170/440 return an empty array.
- **D-keys-on-array.** No docs. Interp: `actions::maps::keys`. VM: builtins.rs:1108 returns indices.
- **D-find-on-array.** CS documents `find` for strings only ("first start index (0-based) or nil"). The VM extends it to arrays (builtins.rs:311). The interp would need an array arm in its `find` implementation.
- **D-join-non-strings.** CS:544-548 only shows string elements. The interp rejects non-strings (lib.rs:15802). The VM stringifies them (builtins.rs:428).
- **D-slice-builtin-end.** `slice` is undocumented (spec slicing is the `[s:e]` syntax with an exclusive end). The VM's `slice_collection` → `grab_between` (collections.rs:1522) is end-inclusive for arrays but end-exclusive for strings (builtins.rs:2062-2078). Adding the builtin to the interp would need a decision first.
- **D-to-map.** The `:m`/`to_map` cast is undocumented for maps. Interp: `cast_to_map` (lib.rs:2093) wraps the map. VM: builtins.rs:3052 returns `{}`.

## (c) Spec-only (implemented in neither engine; no cases written)
- Index or key assignment: `stats["intelligence"] = 8`, `l[0] |= 9`, `l[0] | 9`. Both engines give a parse error. Bare `=` is L0113, and `|`/`|=` after an index target is P1006. Use `:update!(m[k], v)` instead.
- `add X to arr` and `insert X at i into arr`: both engines report an unknown identifier.
- `usurp at i in a with v` and `usurp from a with v`, `replace at i in a with v`, `drop`/`cut … from arr`.
- Deterministic `pick first|last|at i from a` and `reap first|last|at i`.
- Prefix forms `sort names`, `shuffle names`, `unique [..]`, `map upper, names` (the VM parses `s | sort l` as `s | sort` and prints `<builtin Sort>`).
- `for k, v in map` two-variable iteration (P0331). Iterating a map yields `[k, v]` pairs.
- Integer map keys in literals (`{1: "a"}`, P1201).
- `.first`/`.last` on arrays, and the `:first` builtin.
- `mode` with several modes: LS says "most common value(s)". The interp returns `{1: 2}` and the VM returns `[2]` for `[1,1,2,2,3]`. No case was written because neither engine gives a multi-mode result and the ordering is unspecified.
- `flatten` deep vs one level: only the VM has `flatten`, and it is one-level. Only the one-level form is tested.

Parser quirk, not filed as a case: `{"a": {"b": {"c": 1}}}` fails to parse on both engines (`}}}` gives P1203), and `} } }` works. This probably belongs to the core or lexer area.

## (d) Inventory builtins exercised
all, any, array_push, avg, contains, count, count_matching, delete, delete_all, delete_at, delete_between, delete_first, delete_last, delete_matching, delete_random, delete_where, dups, filter, filter_fn, find, find_index, flatten, for_each_fn, freq, get, get_all, get_at, get_between, get_first, get_last, get_matching, get_random, get_where, grab, grab_all, grab_at, grab_between, grab_first, grab_last, grab_matching, grab_random, grab_where, has, is_array, is_collection, is_empty, is_float, is_map, is_pair, is_seq, items, join, keys, len, map, map_fn, max, min, mode, pairs, pick, put, put_all, put_at, put_between, put_first, put_last, put_matching, put_random, put_where, range, reap, reap_all, reap_at, reap_between, reap_first, reap_last, reap_matching, reap_random, reap_where, reduce, reduce_fn, reverse, sample_weighted, secure_pick, secure_shuffle, shuffle, slice, sort, sort_by, sum, to_map, unique, update, update_all, update_at, update_between, update_first, update_last, update_matching, update_random, update_where, upper, values, zip.
