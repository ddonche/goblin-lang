# strings conformance area: notes

141 cases. Support file: `tests/conformance/_support/strings/greet.html` (a `<{ render }>` template
for `render_template`; the path is relative to the harness cwd `tests/conformance`).

The user docs (`docs/language-spec.md` §6–7, `docs/cheat-sheet.md` "Strings" / "Split & Join") predate the
current surface syntax: `=` binding, `|`/`||` string joins, single-quoted strings, and English prefix
forms like `split S on D` all fail on both engines. In this area that staleness is treated as follows.
Removed syntax with an explicit lexer/parser error (`||` gives L0114, multi-char `'..'` gives L0213, `=`
gives L0113) is listed under spec-only and gets no cases. Where an implemented builtin's *documented
behaviour* differs from both engines, the case is marked `known-gap: both`, as the brief requires.

## (a) Disagreements and gaps

Columns: case, interpreter result, VM result, classification, evidence.

### Interpreter-only gaps (VM matches intended)
| case | interp | vm | class | evidence |
|---|---|---|---|---|
| contains_substring | A0401 unknown action `contains` | true/false | known-gap interp | builtin present on VM only (inventory); `has` works on both |
| case_to_upper_to_lower | A0401 `to_upper` | JOHN DOE / john doe | known-gap interp | VM-only builtin |
| to_str_to_string | A0401 `to_str` | 42 / 1.5 | known-gap interp | VM-only builtins (the docs deprecate `.to_string`, but the inventory needs them covered) |
| replace_all, replace_method | A0401 `replace` | dogs and dogs | known-gap interp | spec §6 "s.replace "cats" with "dogs" /// "dogs and dogs" (ALL)"; cheat-sheet "s.replace("old","new") /// all" |
| pad_left_width, pad_right_width | A0401 `pad`/`pad_right` | padded | known-gap interp | VM-only (`crates/goblin-vm/src/builtins.rs:2846-2857`). VM ignores a fill-char argument (`pad_left("7",3,"0")` gives `"  7"`), so the cases use no fill arg |
| repeat_str_basic | A0401 | ababab | known-gap interp | VM-only |
| url_encode_basic, url_decode_basic, url_roundtrip | A0401 | encoded/decoded | known-gap interp | VM-only (prior audit §4.x) |
| render_template_json_data | A0401 | rendered page | known-gap interp | VM-only (`crates/goblin-vm/src/render.rs`) |
| ipsum_types | A0401 `ipsum` | strings | known-gap interp | VM-only. VM is a stub returning a constant (`builtins.rs:2293`), so only types are checked |
| slice_builtin_on_string | A0401 `slice` | él | known-gap interp | VM-only builtin; `s[a:b]` works on both |
| index_string_single | T0205 "indexing requires an array or map" | é / o | known-gap interp | interp slices strings (`s[1:4]`) but cannot index them. Spec §6 "Indices are Unicode code-point positions" |
| char_plus_int, char_from_code_point, chars_element_plus_int | R0200 numeric-expected | b / ’ | known-gap interp | VM `vm.rs:587-590`; audit §4.8. Only way to build a char from a computed code point |
| length_alias | A0401 `length` | 6 / 5 | known-gap interp | cheat-sheet "length (alias: len)", `"daniel".length /// 6` |
| empty_string_falsy_in_if | R0201 "if condition requires a boolean" | falsy / truthy | known-gap interp | spec §6 "Empty string is falsy in conditionals"; spec §8 "Falsy: false, 0, 0.0, "", [], {}, nil" |
| bool_of_string | R0215 type-lock-cast | false / true | known-gap interp | spec §8 `"".bool /// false`, `" ".bool /// true` |
| yall_write_parsed_value | `"{a: 1, b: nil, c: q s}"` (display string, quoted) | proper Y'all text | known-gap interp | `yall_write` on a `yall_parse` result emits the map's display form; on a map literal it works (yall_write_map_literal) |

### VM-only gaps (interpreter matches intended)
| case | interp | vm | class | evidence |
|---|---|---|---|---|
| raw_no_interpolation | `c {x}` | `c 5` | known-gap vm | spec §6 "raw: no escapes, no interpolation". Interp has a raw marker (`\u{001E}`) bypass. The VM compiler checks the marker (`compiler.rs:983`) but still interpolates |
| interp_map_value | `m={k: 1}` | `m=[1]` | known-gap vm | VM map literal is `Value::Collection`; string interpolation renders it as `[1]` |
| count_substring | 3 / 2 | arity mismatch calling 'Len' | known-gap vm | spec §6 `text.count "the" /// 1`; VM routes 2-arg `count` to `Len` |
| join_non_string_errors | error | `1,2` | known-gap vm | spec §6 "any non-string element ⇒ TypeError"; cheat-sheet same |
| split_result_equals_literal | true | false | known-gap vm | `split` returns Array, the literal is a Collection, and `==` is false |
| ignore_matching_flags_map | `[ ]` | `[ABC ]` | known-gap vm | VM silently drops the `{"i": true}` flags map (Collection, not Map) |
| ignore_between_opts_map | `a()c` | "expected map or nil, got collection" | known-gap vm | same Collection issue, internal-style error |
| render_template_map_literal | (lacks builtin) | "data must be a map, got collection" | known-gap interp + known-gap vm | `render.rs:120-128` `map_to_pairs` accepts only Map/MapOrd. Two `known-gap` header lines |
| big_from_string_is_big | true | false (valtype still says `big`) | known-gap vm | internal inconsistency between `is_big` and `valtype` |

### Both engines agree but contradict explicit docs (`known-gap: both`)
| case | both print | documented | evidence |
|---|---|---|---|
| raw_single_no_escapes | `a<TAB>b`, len 3 | `a\tb`, len 4 | spec §6 "raw: no escapes, no interpolation" (triple-quoted raw does skip escapes) |
| trim_lead_literal_dedent | `Hello\n      World` | `Hello\n  World` | spec §6 "trim_lead \"\"\"...\"\"\" /// removes common leading indent"; cheat-sheet "trims common indent" |
| interp_zero_arg_method | `/users/{name.slug}` | `/users/hello-world` | spec §6 `url = "/users/{name.slug}" /// zero-arg methods allowed` |
| interp_double_brace_literal | `body {{font-size:{score}px;}}` (also not interpolating `{score}`) | `body {font-size:15px;}` | spec §6 "Use {{ and }} for literal braces" + css example. Only `\{`/`\}` work today (interp_backslash_brace) |
| minimize_removes_all_whitespace | `Hello World` | `HelloWorld` | spec §6 `s.minimize /// "HelloWorld" (remove ALL whitespace)`; cheat-sheet `"Danielisawesome"`. Impl deliberately collapses (`goblin-interpreter/src/lib.rs:15983-15995`) |
| split_empty_delimiter_errors | `[a, ,, b]` (splits to chars) | ValueError | spec §6 and cheat-sheet: "Empty delimiter → ValueError" |
| lines_crlf | 3 parts, middle is `y\r` | `y` | cheat-sheet `lines s /// split on universal newlines`. Interp `lib.rs:15652` comment "split on '\n'"; VM `builtins.rs:2781` |
| reverse_string_method | type error | `leinad` | spec/cheat-sheet `"daniel".reverse /// "leinad"` (`reverse_chars` works) |
| chars_element_as_needle | interp "find expects a string"; VM "expected str or array, got str" | 15 | spec §6 `chars "abc" /// ["a","b","c"]`. Docs say chars are strings; engines return Char, which `find` rejects. Audit probe `chars_digits.gbln`. VM message is misleading |
| float_large_not_saturated | `:str(1e20)` == "9223372036854775807" | not saturated | `fmt_num_trim` casts whole floats with `n as i64` (interp `lib.rs:2592-2599`, VM `builtins.rs:5147-5154`). 1e20, 1.5e300 and JSON 12345678901234567890 all print as i64::MAX |
| pct_constructor_points | `:pct(25) == (25%)` false; `:percent(25)` same | true | cheat-sheet "pct 25 /// 25% (constructor from percentage points)". Also interp `100 * :pct(25)` = 2500, and VM errors "expected number, got pct" (not encoded) |

### Undecided
| case | interp | vm | slug |
|---|---|---|---|
| interp_unclosed_brace | R0500 "unclosed '{' in interpolated string" | prints `a{bad` | D-unclosed-interp-brace |
| concat_plus_non_string | `a1`, `n=2.5`, `btrue` | same | D-string-plus-nonstring (engines agree; docs vague/contradictory) |
| format_info_no_thousands | `{dec: 2, decmark: ., th: none}` | `{dec: 2, decmark: ., th: nil}` | D-format-info-no-thousands |

### Notable agreements the owner may not expect (cases encode current behaviour)
- `say(1.0)` prints `1`; `say(2.50)` prints `2.5`; `"{w}"` with w=3.0 prints `3`. Deliberate:
  `fmt_num_trim` (interp `lib.rs:2592`, VM `builtins.rs:5147`). The docs show `5.0` only as value comments, so
  case `float_whole_prints_as_int` encodes `1`. `json_stringify(1.0)` gives `1.0` (json_numbers).
- `"{x + 1}"` and `"{a{missing}b}"` are left literally on both. Only bare identifiers (with optional
  spaces) interpolate. Spec §15 says missing names raise InterpolationError, but that is the templates section, so there is no case for it.
- `"a" == 'a'` is false on both (Char vs Str). `:chars` returns Chars.
- `:before("abc","z")` returns the whole string, while `:after("abc","z")` returns "" (asymmetric, both agree).
- `:is_float(1.0)` is false on the interpreter and true on the VM (number typing; left to the core area).
- D-cast-rebinds confirmed in this area: `x | (20%); say(:str(x)); say(:is_pct(x))` prints false on the interpreter because `:str(x)` rebinds `x`. Cases avoid casting variables they reuse.
- `:is_control` is not a character predicate. Interp tests for control-flow values; VM returns `v == nil`
  (`builtins.rs:894-898`). Not encoded here.
- `:format(x, 2, ",")` (3 args): interp arity error, VM silently ignores. Not encoded.
- `:repeat("ab", 3)` prints "" on interp and errors on VM; `repeat` is a loop keyword. Not encoded.

## (b) Undecided slugs

### D-unclosed-interp-brace
A string literal with a `{` that never closes (`"a{bad"`).
- Interp: runtime error R0500 (`crates/goblin-interpreter/src/lib.rs:3321-3335`, in `render_interpolated` at :3141).
  This happens even for data strings such as `:json_parse("{bad")`, where the user gets an interpolation error instead of the JSON error.
- VM: leaves the text literal (`crates/goblin-vm/src/vm.rs:1916` `render_string_interp`).
- Evidence: docs only say `{{ }}` gives literal braces, and that is unimplemented on both. Nothing addresses a lone `{`.
- Change for "error": make the VM's `render_string_interp` raise when no `}` follows. Change for "literal": drop the
  R0500 branch in interp `render_interpolated`.

### D-string-plus-nonstring
`"a" + 1` gives `"a1"` on both engines.
- Interp: explicit coercion arms (`lib.rs:21670-21677`, `(Value::Str(a), other) => format!(... fmt_value_raw(other))`).
- VM: `vm.rs:592-593` same.
- Against: spec "Safe by Default … No silent string coercion" (language-spec.md line 24) and the `TypeError`
  example for `"Score: " || score`; spec §7 "joins require strings". For: Sheriff relies on `"…" + value` widely
  (e.g. `"toc-item toc-level-" + string`), and both implementations coerce on purpose.
- Change for "error": remove the mixed Str arms in both places listed above.

### D-format-info-no-thousands
`format_info` on a value formatted without a thousands separator.
- Interp gives `th: "none"` (string), `lib.rs:13134` + ~13152 (`None => "none".to_string()`).
- VM gives `th: nil`, `crates/goblin-vm/src/builtins.rs:3481-3490` (`None => Value::Nil`).
- The parser accepts the identifier `none` as the "no separator" spelling (`goblin-parser/src/lib.rs` ~1065), which favours
  `"none"`. `nil` is the more conventional "absent" value. Each side needs a one-line change at the cited match arm.

## (c) Spec-only (documented, implemented in neither engine; no cases)
- `|` / `||` string join operators (`||` gives L0114 "There is no `||` operator"; `|` is the binding operator).
- Single-quoted strings `'abc'` (both lex `'..'` as a Char literal; multi-char gives L0213).
- `trim`/`trim_lead`/`trim_trail` with a substring argument (arity error on both).
- `replace_first`, `remove`, `drop`, `escape`, `unescape`, `strip_lead`/`strip_trail`.
- English prefix forms: `split S on D`, `join A with S`, `before ":" in s`, `between … and … in s`,
  `has "x" in s`, `find_all "x" in s`, `replace "a" with "b" in s`.
- Regex `like` forms (`find_all like P in s`, `match like`, `replace like`).
- Format pattern strings: `n.format(",.2f")`, `".2f"`, `"%"`, `format n with ...`, `"{x:,.2f}"`. Parser P05F2
  requires `format(<int>, <sep>, <dec>)`.
- Interpolation of arbitrary expressions (not documented as supported in any case).

## (d) Inventory builtins exercised
after, after_last, before, before_last, between, big, bool, chars, clear_format, contains, count,
count_matching, ends_with, env, escape_html, find, find_all, format, format_info, has, highlight_code,
ignore_between, ignore_blocks, ignore_blocks_first, ignore_lines_matching, ignore_lines_where,
ignore_matching, ignore_where, ipsum, ipsum_full, ipsum_paragraphs, ipsum_sentences, is_alnum, is_alpha,
is_big, is_char, is_digit, is_matching, is_nil, is_pct, is_str, is_whitespace, join, json_parse,
json_stringify, json_stringify_pretty, keep_after, keep_before, keep_between, keep_matching, len, lines,
lower, md_to_html, minimize, mixed, normalize_newlines, ord, pad, pad_left, pad_right, parse_bool, pct,
percent, raw, read_text, render_template, repeat_str, replace, reverse, reverse_chars, sanitize_bom, slice,
slug, split, starts_with, str, string, title, to_lower, to_str, to_string, to_upper, tokenize, trim,
trim_lead, trim_trail, upper, url_decode, url_encode, valtype, words, yall_minify, yall_parse,
yall_parse_file, yall_pretty, yall_write, yall_write_file.

Not covered here, left to other areas: the `get_/put_/delete_/update_/reap_/grab_ *_between/*_matching`
collection family, `read_json`/`write_json` (io), `is_control` (control-flow predicate).
