# io conformance area: notes

Scope: filesystem and path builtins, process/env, output, stdin, time/date, random/ids, outbound HTTP
(`/// requires: http`), Postgres (`/// requires: db`), and the `goblin start` request/response builtins
as they behave under plain `goblin run`.

Every case was run on the baseline binary (Goblin v0.47.40, built from `7f86981`), with stdin closed as
the harness does. File:line references below are to that commit. `vm-parity` gained commits on top of it
while this area was being written (`227da6b`, `94fe112`: pooled `db_*` on the VM, Collection fixes). Several
`known-gap: vm` markers here should flip to "passes now" on a binary built from those commits. That is
the intended signal, so remove the markers then.

140 cases: fs 46, path 8, env/args 4, run_cmd 7, output 3, input 1, request/response 5, time 18,
uuid 3, random 12, http 16, db 17.

Conventions used in the cases:
- Each fs case works only under `env("GOBLIN_CONF_TMP")`. `walk` and `list_dirs` results are sorted
  before printing, because their order is unspecified and the engines use different traversals
  (WalkDir vs read_dir).
- `run_cmd` runs `sh -lc`, a login shell, so stdout and stderr can carry profile noise. On this machine
  an `nvm` line appears. Cases parse the JSON and check only the tail with `ends_with`, never the raw
  string.
- The time, uuid and random cases assert properties (types, lengths, ranges, ordering, round-trips),
  never wall-clock values. `local_now`'s offset is machine-specific and is never printed.
- Each db case creates a table named `io_<case>_<first uuid group>`. Both engines run the same case in
  parallel, so the suffix stops them colliding once the VM has `db_*`. The table is dropped at the end.
  Cases that need no table use plain `SELECT` expressions.
- Map key order (D-map-key-order) is avoided by using map literals whose keys are already sorted.

## (a) Disagreements and gaps

| case | interp | vm | classification | evidence |
|---|---|---|---|---|
| `fs_zip_dir_creates_parent` | creates `out/nested/`, prints `true` | error `zip_dir: cannot create ...: No such file or directory` | known-gap vm | Interp creates the parent on purpose: `lib.rs:17863-17873` ("failed to create zip parent directory"). The VM opens the file directly: `builtins.rs:4697-4698`. `copy_file!` creates parents on both engines. AGENTS.md:5 says "Match interpreter behavior exactly". |
| `fs_bang_io_return_value` | `false` / `unit` | `true` / `nil` | **undecided D-bang-io-return** | see (b) |
| `print_no_newline`, `println_values`, `eprint_goes_to_stderr` | `unknown action 'print'` / `'eprint'` | works | known-gap interp | The builtin exists only on the VM: `compiler.rs:2579-2582`, `builtins.rs:2100`. It has no arm in interp `eval_builtin`. No docs mention it. |
| `http_*` (14 cases) | `unknown action 'http_get'` | works | known-gap interp | VM-only: `compiler.rs:2837-2841`, goblin-http crate. The brief and the audit (§4.5) say so. |
| `http_headers_map_literal`, `http_post_headers_literal` | unknown action | `headers must be a map, got collection` | known-gap interp + known-gap vm | `http_extract_headers` (`builtins.rs:5842`) accepts Map/MapOrd/Nil only, and a VM map literal is `Value::Collection`. The same call with a `json_parse`-built map works (`http_headers_parsed_map`). |
| `run_cmd_env_map` | `true` | `run_cmd: env argument must be a map` | known-gap vm | A map literal is a Collection: `builtins.rs:2312-2320`. |
| `run_cmd_env_non_string_value` | `[5]` in env | `Int(5)` (prints `false`) | known-gap vm | VM formats with `format!("{:?}")`: `builtins.rs:2315,2318`. Interp uses `to_string()`: `actions/process.rs:33-44`. |
| `set_cookie_options_map` | ok | `set_cookie: options must be a map` | known-gap vm | Collection again: `builtins.rs:2607-2632` |
| `time_add_duration`, `time_add_duration_month_clamp` | correct datetimes | `add_duration: expected map, got collection` | known-gap vm | Collection: `builtins.rs:3786`. `time_add_duration_parsed_map` passes on both. |
| `random_roll_map_config`, `random_roll_detail_map_config` | works | `expected map config, got collection` | known-gap vm | `builtins.rs:2356`, `2408` |
| `random_secure_pick` | works | `secure_pick: expected map, got collection` | known-gap vm | `builtins.rs:2664`. (`pick` *does* accept a Collection config: `builtins.rs:965`.) |
| `random_secure_shuffle` | works | `expected array or string, got collection` | known-gap vm | `builtins.rs:2759` |
| `random_seed_repeats_sequence` | reseeding repeats the sequence (`true true`) | `false false` | known-gap vm | The VM `RandSeed` is a no-op, `builtins.rs:2351-2355` ("in VM we ignore since session handles it"). Interp reseeds the session RNG: `lib.rs:13973-13980`. |
| `random_seed_int_argument` | `rand_seed expects a numeric value` for `42` | ok (but ignores it) | known-gap interp | The `want_num` closure accepts only `Value::Float` (`lib.rs:11645-11650`), although the `rand_seed` comment (`lib.rs:13976`) says inputs of any numeric kind are meant to be deterministic. Every other numeric builtin takes ints. |
| `time_valtype_datetime` | `unknown` | `datetime` | known-gap interp | Interp `valtype` has no `Value::DateTime` arm and falls into `_ => "unknown"` (`lib.rs:12359-12384`). `unknown` is a fallback, not a type name. |
| `db_*` (17 cases) | works | `undefined variable ':db_query_one'` | known-gap vm | Absent from the VM table at the baseline. AGENTS.md:12 says "skip (db crate unfinished)". |
| `db_row_is_map` | `false` | (no db) | known-gap interp + vm | DB rows are `Value::MapOrd` (`actions/db.rs`), but interp `is_map` only matches `Value::Map` (`lib.rs:12493-12500`). A row that cannot pass `is_map` is clearly a bug. |
| `db_smallint_real_columns` | `nil`, `nil` | (no db) | known-gap interp + vm | `sql_cell_to_value` (`actions/db.rs:55-72`) tries only String/i64/i32/f64/bool and otherwise returns `Value::Nil`. smallint (i16) and real (f32) are plain numbers. |
| `db_numeric_column`, `db_timestamp_column`, `db_json_column` | non-NULL values read as `nil` | (no db) | known-gap interp + vm | Same fallthrough. See the decision note below. |

Decision on timestamp/json/numeric columns, which the brief asked about: **gap, not intended**. No user
doc covers `db_*` at all, so the call rests on these points:
1. Returning `nil` for a non-NULL value makes it indistinguishable from SQL NULL, which reads as `nil`
   (`db_null_column`). Data is silently lost.
2. The spec gives Goblin first-class datetime values and JSON readers, so a natural decoding exists.
3. The campfire port had to cast those columns to `::text` in SQL to work around it (audit §2, "Separate
   interpreter defect"). `db_timestamp_as_text_workaround` pins down that workaround.
4. The catch-all `Value::Nil` has no comment claiming it is deliberate at the baseline.

The cases assert only "is not nil", so they do not prescribe a representation (datetime vs ISO string,
parsed JSON vs text, float vs str for numeric). Counter-evidence: the new shared `crates/goblin-db/src/lib.rs:122`
(added after the baseline) says "Column types the runtimes understand; anything else reads as Null". That
describes the current behaviour, but nothing in it argues the behaviour is right. If the owner decides
nil is intended, change these three to `undecided` or drop them.

Cases where both engines agree, the behaviour looks doubtful, and no case was written:
- **`walk` glob patterns.** Only the literal patterns `"**/*.md"` and `"**/*.gbln"` filter anything.
  Every other pattern (`"**/*.txt"`, `"**/*.js"`) returns *all* files: interp `actions/files.rs:504-512`,
  VM `builtins.rs:3275-3282`. Sheriff calls `:walk(js_src, "**/*.js")` (`sheriff/dist/api/portal.gbln:429`)
  and `:walk(patterns_dir, "**/*.txt")` (`sheriff/dist/api/list_patterns.gbln:46`) and then uses the
  results without filtering, which suggests a real glob was expected. There is no doc and the engines
  agree, so this is not undecided by the rules. It should be decided before anyone writes a case for it.
- **`lines("a\nb\n")` length.** Both engines give 4 for `a\nb\nc\n` (a trailing empty line). That
  belongs to the strings area, so it was left out of `fs_append_file_existing`.

## (b) Undecided slugs

### D-bang-io-return: what a filesystem bang call evaluates to
Case: `fs_bang_io_return_value`. Interp gives `unit` (`is_nil` false). The VM gives `nil`.
- Interp side: every bang file builtin ends with `return Ok(Value::Unit)` (e.g. `write_text!`,
  `lib.rs:18051-18083`; `create_dir!` and `delete_path!` in the same block, 17787-18050). `say` of
  unit prints an empty line.
- VM side: `Ok(Value::Nil)` (`builtins.rs:1077-1083` for WriteText; same pattern for AppendFile,
  CreateDir, CopyFile, DeletePath, ZipDir, WriteJson).
- Docs: nothing. The spec describes `"out.txt".write_text(...)` only as a statement. The other
  response builtins (`set_status`, `set_header`, `set_cookie`) return `Value::Nil` on *both* engines
  (`actions/response.rs`), which is weak evidence for nil.
- To pick unit: the VM needs a Unit value or a convention (the VM's `is_unit` exists, `compiler.rs`
  `"is_unit"`). Change the `Ok(Value::Nil)` returns in the bang file arms of `builtins.rs`.
- To pick nil: change the `return Ok(Value::Unit)` lines in the bang dispatch block of interp `lib.rs`
  (17733-18170).
- This probably goes with whatever core decides for other unit-returning statements. A shared slug
  would fit there if core has one.

## (c) Spec-only (documented, implemented by neither engine; no cases)
- Date/time literals `date "2025-08-23"`, `time "14:30:05"`, `datetime "..." tz:"UTC"`
  (language-spec.md:282-284, 360-366, 838; cheat-sheet §15). Both engines parse `date "..."` into
  something they then reject ("unknown identifier Date(...)" / "undefined variable"). The builtin names
  `date`, `time` and `datetime` exist in both dispatch tables (interp `lib.rs:7256-7328`), but
  `:date(...)`/`date(...)` do not parse ("Expected a string after 'date'", P1006/P1301), so the names
  cannot be reached from source.
- `duration(n_seconds)` (cheat-sheet.md:880). Both engines error with "duration: not yet implemented"
  (interp `lib.rs:7326`).
- `utcnow()`, `local_tz()`, `to_tz()`, `.iso()`, `.format("YYYY-MM-DD")`, `trusted_now()`,
  `trusted_today()`, `add_days/add_months/add_years`, `floor_dt/ceil_dt`, `wrap_time/shift_time`, and
  datetime arithmetic with `+ 1d` (cheat-sheet §15.6-15.10, language-spec.md:4040-4115). The
  implemented equivalents are `utc_now`, `to_timezone`, `to_iso`, `format_datetime` (strftime patterns)
  and `add_duration`.
- `uuid()` (cheat-sheet.md:2055, 2183). The implemented names are `uuid_v4`/`uuid_v7`.
- Method-style file I/O: `"path".read_text`, `.write_text(s)`, `.append_text`, `.read_json(opts)`,
  `.write_json(v, opts)`, `.read_bytes`, `.read_yaml/csv` (language-spec.md:4381-4391, 4606-4614;
  cheat-sheet.md:2002-2037, 2146). JSON options (`money`, `datetime`, `blob`, `enum`) are spec-only too.
  The implemented form is `:read_text(p)` / `:write_text!(p, s)` / `:write_json!(p, v[, pretty])`.
- `http_post("/pay", payload)` returning a body string (language-spec.md:4482). The implemented
  `http_*` returns a `{status, body, ok}` map. There is no way to read *response* headers on either
  engine, so the fixture's `/headers` route (`X-Reply: yes`) cannot be observed and has no case.
- `nums.pick(3)` (cheat-sheet.md:247). The implemented `pick` takes a config map.

## (d) Inventory names exercised
append_file, ask, basename, copy_file, cookie, create_dir, day, db_exec, db_query, db_query_one,
delete_path, dirname, env, epoch_ms, epoch_s, eprint, eprintln, ext, file_exists, format_date,
format_datetime, format_time, from_epoch_ms, from_iso, hour, http_delete, http_get, http_post, http_put,
http_request, input, is_dir, is_file, list_dirs, local_now, minute, month, now, path_fix_separators,
path_join, path_normalize, path_relative_to, path_split, pathfind, pick, print, println, rand_seed,
read_json, read_text, req_body, req_header, req_method, req_path, req_query, roll, roll_detail,
roll_detail_str, roll_str, run_cmd, second, secure_pick, secure_random, secure_shuffle, set_cookie,
set_header, set_status, shuffle, since, stem, timezone, to_epoch_ms, to_iso, to_timezone, today,
tomorrow, until, utc_now, uuid_v4, uuid_v7, valtype, vt, walk, weekday, write_json, write_text, year,
yall_parse_file, yall_write_file, yesterday, zip_dir, add_duration.
Mentioned only in comments, because they cannot be reached or are spec-only: date, time, datetime, duration.
`args` is the CLI-args global (`args_empty_without_cli_args`), not a builtin.

## Behaviour under plain `goblin run` (request/response builtins)
- `req_method/req_path/req_query/req_body` read `GOBLIN_METHOD/GOBLIN_PATH/GOBLIN_QUERY_STRING/GOBLIN_BODY`
  and return `""` when these are unset. `req_header`/`cookie` parse `GOBLIN_HEADERS_JSON` and return `nil`
  (interp `actions/request.rs`; VM `builtins.rs:2508-2568`). Both engines agree.
- `set_status/set_header/set_cookie` only record state in the session and print nothing under `goblin run`.
  Both engines agree. With `GOBLIN_NONINTERACTIVE=1`, which the harness cannot set per case, the
  interpreter CLI echoes non-unit top-level expression values as a response (`goblin-cli/src/main.rs:1988`).
  No case covers that.
- `input`/`ask` with closed stdin print the prompt (no newline) and return `""` on both engines.

## Found in other areas while working here (not covered by io cases)
- VM: `:has([10, 20], 10)` is `false` for an array literal, and `:is_map({...})` is `false` for a map
  literal (Collection). `:sum([1, 2])` returns a float, so `sum == int_total` is false (D-int-float-equality).
- Strings: `"{{nope"` is a parse error ("unclosed '{'") on the interpreter, but the VM prints `{{nope`
  literally. `"{:fn()}"` is not interpolated on either engine (it prints literally).
- Interp: indexing a string (`s[14]`) is an error ("indexing requires an array or map"). The VM returns
  the char.
