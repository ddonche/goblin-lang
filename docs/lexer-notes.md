# Lexer Notes — v1.18 (Part 1: Global Rules, Comments, Operators)

## Global rules

* **Longest-match, multi-char first** for operators/punct.
* **Statement breaks**: emit `NEWLINE` at line ends except when:

  * inside `()[]{}` (implicit continuation),
* **Indentation**: 4 spaces per level; emit `INDENT`/`DEDENT`. Tabs → `IndentationError`. Mixed widths → `IndentationError`.
* **EOF**: emit final `NEWLINE` and pending `DEDENT`s.
* **Errors**: unknown byte, bad escapes/`\uXXXX`, unterminated string/block comment → `LexError`.

  * Note: other domain/runtime errors (`FileNotFoundError`, `PermissionError`, `SerializationError`, `MoneyPrecisionError`, `TimezoneError`, etc.) are **not** lexer errors and, when referenced, simply lex as `IDENT`.

## Comments

* Line: `///` → `LINE_COMMENT` (to EOL; inert).
* Block: `//// ... ////` → `BLOCK_COMMENT_START` / `BLOCK_COMMENT_END` (non-nesting; inert even if contents “look like” code). `////` inside strings is text.

## Tokens: Operators & Punct (multi-char first)

"..." RANGE\_EXCL (also doubles as variadic marker in parameter lists)
".."  RANGE\_INCL
"::"  NAMESPACE / BINDING\_SEP / TEMPLATE\_SKIP
"||"  JOIN\_SPACE
"|"   JOIN\_NOSPACE
"??"  NULLISH
"?>>"  OPT\_CHAIN (optional chaining, left-associative; nil short-circuit is semantic)
"**"  POW                 // exponentiation; also POSTFIX SQUARE
">>"  MEMBER ASSIGNMENT FIELD CHECK // user >> is_active
"><"  DIVMOD              // quotient+remainder pair
"++"  POST\_INC\_MONEY      // postfix only
"--"  POST\_DEC\_MONEY      // postfix only
"//"  INT\_DIV             // integer div (also money flavor); also POSTFIX SQRT
"%"   MODULUS              // infix
"%"   PERCENT of 100       // postfix
"%s"  PERCENT\_SELF        // percent-of-self sugar
"%o"  PERCENT\_OF\_OTHER    // percent-of-other sugar
"=="  EQ   "!==" NE  "<=" LE  ">=" GE
"===" STRICT\_EQ   "!===" STRICT\_NE
"<"   LT   ">"  GT  "="  ASSIGN "!=" Not assigned
"+=" "-=" "\*=" "/=" "%="
"**="                       // compound exponent assign
"+" "-" "\*" "/" "%" ":" "," "." ";" "@" "=>" "(" ")" "\[" "]" "{" "}"
"&&"  AND\_ALIAS             // alias for and
"<>"  OR\_ALIAS              // alias for or
"!"   NOT\_ALIAS             // alias for not
"!"   POST FACTORIAL
"^"   POST CEIL 
"_"   POST FLOOR
"@"   class name identifier
"#"   private variable identifier
"=>"  lambda
"&"   definedness operator
";;"  fang operator          // alias for burn

> Note: `::` is **overloaded** — namespace separator, binding separator, and skip marker in templates. Lexer always emits `NAMESPACE`; parser disambiguates.

Goblin splits the `=` family into three clean families:

- `=` / `!=` - Assignment state
- `==` / `!==` - Value equality  
- `===` / `!===` - Strict identity

\#########################################################################################################################################################

# Lexer Notes — v1.18 (Part 2: Keywords, Built-ins, Identifiers)

## Keywords

### Hard Keywords (cannot be shadowed)

```
if elif else for in while repeat unless
attempt rescue ensure raise say warn error
return skip jump until stop assert
class act action enum use import export via test
true false nil contract prefer
judge morph vault banish unbanish expose
pick roll roll_detail reap usurp replace override add insert map
nc self set burn 
of
end
blob
```

* **`morph`**: hard keyword, not shadowable. It is a language form, not a user-defined function. Always lexes as `KEYWORD(morph)`. replace and override used in morph; replace is also for collections

* **`set`**: hard keyword. Introduces configuration statements, including `set @policy …`.

* **`in`**: hard keyword, reserved for loop constructs only. Not valid as a boolean operator.

* as → already listed as a soft keyword, but note: in import … as it’s a hard keyword usage. Lexer emits KEYWORD(as); parser decides.

* File/string methods: .read\_text, .write\_text, .append\_text, .read\_bytes, .write\_bytes, .is\_binary, .read\_json, .write\_json, .read\_yaml, .write\_yaml, .read\_csv, .write\_csv.

### Soft Keywords (context-dependent)

```
from at first last to into with dups seq
as where raw trim_lead on like and between unique
```

* Soft keywords may lex as `KEYWORD` but parser decides semantics.
* Special rule: `raw` / `trim_lead` prefixing string literal → mark flags on STRING token.
* `between` sugar: parser expands to chained comparisons.
* `dups` and `unique` are modifiers for pick shorthand.
* **Note:** `as` is used in enum declarations (e.g., `enum X as int`). Lexer still emits `KEYWORD(as)`; parser resolves meaning.
* Also used in rescue … as e bindings. Lexer still emits KEYWORD(as); parser scopes the bound identifier to the rescue arm.
* like, before, after, before\_last, after\_last, between, words, chars, lines, split, join, match
* ./ path anchors (lexer treats "./" at the start of an import string as part of STRING, not keyword; note only in parser).
* modules is not a keyword; it’s a directory literal. Nothing to add here.

## Built-ins

* (int, float, bool, money, pct, etc.) are **shadowable** → always lex as `IDENT`.
* Built-ins like `freq`, `mode`, `sample_weighted` remain shadowable.
* Input - prompting the user for input from the CLI
* **`blob` is a hard keyword** (constructor for blob literals) and is **not shadowable**.
* save\_remainders, load\_remainders, divide\_evenly, divide\_evenly\_escrow, allocate\_round\_robin, allocate
* Tokenization: pct( → IDENT pct + (; no special token.
  Add the following identifiers (all lex as `IDENT`):
* Constructors / helpers: `now`, `today`, `utcnow`, `parse_date`, `parse_time`, `parse_datetime`
* Zone & conversion: `to_tz`, `local_tz`
* Calendar adders: `add_months`, `add_years`
* Time arithmetic helpers: `wrap_time`, `shift_time`
* Trusted time: `trusted_now`, `trusted_today`, `time_status`, `ensure_time_verified`
* Naive helpers: `datetime_to_naive_utc`, `datetime_to_naive_local`, `naive_to_datetime`
* str, to\_hex, from\_hex, to\_base64, from\_base64, read\_bytes, write\_bytes, concat
* cwd, chdir, mkdirp, listdir, glob, join
* json\_parse, json\_stringify
* has, find, find\_all, count, replace, replace\_first, remove
* Gmark API (method-style on strings): gmark, gmark\_info, gmark\_set\_ord
* Gmarks service (project-scope): gmarks, all, filter, next\_ord
* Note: these are names only for the lexer; method calls still tokenize as IDENT after . and service calls as IDENT / IDENT::op per normal rules.
* Add: state.allow\_writes (policy map key; lexes as IDENT inside policy objects).
* (You already note @policy lexes as AT\_IDENT; no new token type.)
* goblin (CLI namespace exposed for banish/unbanish/list; parsed as IDENT)
* banish, unbanish (appear as IDENT when invoked through CLI context, separate from keywords inside code)
* settle (money: snap/settlement helper referenced by policy timing)
* Contracts & introspection: contract, glam\_contracts, glams, glam\_symbols, glam\_permissions

### Text utilities

* has, find, find\_all, count
* replace, replace\_first, remove
* before, after, before\_last, after\_last, between
* lines, words, chars
* split, join
* escape, unescape
* reverse, mixed, minimize
* trim, trim\_lead, trim\_trail
* upper, lower, title, slug
* format
* parse\_bool
* match (regex)

### Collection utilities

* shuffle, sort, settle
* pick, reap, usurp, replace
* add, insert, map
* sum, avg, min, max, mode, freq, sample\_weighted
* unique, dups

### Numeric operations

* round
* floor
* ceil
* abs
* pow
* sqrt

### Types

* int, float, bool, big, money, pct
* date, time, datetime, duration
* i, f, b postfix 
* no money postfix, as it has $ and USD, etc. 
* no % postfix, as it is its own postfix
* datetime: s, m, h, d, w, mo, y

### I/O, filesystem, printing, serialization helpers

* File/text: read\_text, write\_text, append\_text, read\_bytes, write\_bytes, is\_binary, .exists
* JSON/YAML/CSV: read\_json, write\_json, json\_parse, json\_stringify, read\_yaml, write\_yaml, read\_csv, write\_csv
* FS/path: cwd, chdir, mkdirp, listdir, glob, join
* Printing/errors: say, warn, error

---

## Identifiers

* `IDENT`: `[A-Za-z_][A-Za-z0-9_]*[!?]?`
* `AT_IDENT`: `@` + same pattern (dataset/policy names, templates). Example: `@policy` lexes as `AT_IDENT('@policy')`.

  * `VERSION`: contextual token, only after `@` in a `use` statement.
* `HASH_IDENT`: `#` + same ident pattern (used for private fields in classes).
* Context-sensitive:

  * `raw` / `trim_lead` are `KEYWORD` only when directly prefixing a string literal (possibly both, any order). Otherwise lex as `IDENT`.
* **Enum variants:** inside an `enum` declaration, variant names (e.g., `Pending`, `North`, `Low`) are lexed as `IDENT`. Parser attaches them to the enum scope.
* **Methods with `!`/`?`:** IDENT rule supports trailing `!` or `?` (e.g., `pct!`, `pct?`) for strict/optional conversions.
* **Error names as identifiers:** Spec error types like

  * `FileNotFoundError`, `PermissionError`, `SerializationError`, `MoneyPrecisionError`, `TimezoneError`,
  * `MorphTypeError`, `MorphFieldError`, `MorphCurrencyError`, `MorphActionError`,
  * `GmarkConflictError`, `GmarkNotFoundError`, `GmarkInvalidError`, `GmarkPersistenceError`,
  * `DeterminismError`, `BanishError`,
  * `PolicyNotFoundError`, `PolicyValueError`, `PolicyScopeError`, `PolicyVisibilityError`,
  * `NameError`, `TypeError`.
  * EmptyPickError, PickCountError, PickIndexError, PickTypeError, DiceBoundsError, WeightError.
  * ReapEmptyError, ReapCountError, ReapIndexError, ReapTypeError
  * UsurpIndexError, UsurpCountError, UsurpArityError, UsurpTypeError
  * ReplaceIndexError, ReplaceTypeError
  * InterpolationError, FieldMismatchError, TemplateError, ParseOnlyError, IdentifierError
  * CurrencyError, PermissionError
  * UnknownNameError
  * `TimeArithmeticError`
  * `TimeSourceError`
  * `TimeSourceWarning`
  * `ValueError`
  * FileNotFoundError
  * BanishError
  * GlamError, ContractError, AmbiguityError, LockfileError, GlamNotFoundError, OpNotExposedError, IdentityMismatchError, CapabilityDeniedError, ReadOnlyPathError
  * (`TimezoneError` already exists)
  * ModulePathError, ModuleNotFoundError, ModuleNameConflictError, ModuleCycleError, ModuleVisibilityError, PolicyVisibilityError, OpNotExposedError, CapabilityDeniedError, ReadOnlyPathError
  * ArityMismatchError
  * NetworkError, ValidationError, DiskFullError, TransientError, FatalError, DatabaseError, NetworkTimeout
* Constants by convention: identifiers in ALL CAPS (e.g., MAX\_HEALTH) are allowed. Lexer treats them as IDENT, but reassignment may trigger warnings at runtime.
* `allow_state_writes` is a valid @policy key (lexes as AT\_IDENT).
* "8.5%".pct! → STRING . IDENT('pct!')
* "8.5%".pct? → STRING . IDENT('pct?')
* Regex patterns are just STRING (often raw STRING).
* like is a soft keyword that signals regex context.
* Lexer does not tokenize regex operators (\[], \d, etc.); those are parser/runtime concerns.
* cwd, chdir, mkdirp, listdir, glob, join (already listed under FS, but confirm).
* Reserved verbs (in CLI, still IDENTs): spawn, build, export, push, lint, test, info, help, rummage
* AT\_IDENT: @policy is already covered; in set @policy NAME, NAME may be IDENT or STRING (lexer emits either).
* Policy map keys (e.g., display\_precision, max\_precision, rounding, policy, rounding\_timing, currency, defer\_mode, thousands, decimal, compat, mode, track\_theft, shame\_level, tz, prefer\_trusted, time\_arith, leap\_mode, tzdata\_version, source, ttl, cache\_ttl, skew\_tolerance, cache\_path, cache\_signing\_key, debug, strings, modules, money) all lex as standard IDENT inside maps; no special tokens.
* Durations inside policy values (e.g., ttl: 60s, cache\_ttl: 24h) use the existing duration literal rules (NUMBER + DURATION\_UNIT), no new tokens.
* use glam\@ver as alias —
* use → KEYWORD(use)
* IDENT (glam name)
* @ + version string → lexed as AT\_VERSION (new token type; distinct from AT\_IDENT)
* as → KEYWORD(as)
* IDENT (alias)
* alias::Symbol → lexes as IDENT 'alias', ::, IDENT 'Symbol'.
* via is already a soft keyword (confirmed earlier). In glam contexts, it remains KEYWORD(via); no new token.
* Manifest/contract schema keys (fs.read, fs.write, env, network, etc.) → standard IDENT inside maps; lexer does not special-case.

> Note: `HASH_IDENT` is how private fields (e.g., `#balance`) are tokenized inside classes. Access outside class raises errors at runtime, not in the lexer.

\#########################################################################################################################################################

# Lexer Notes — v1.18 (Part 3: Literals)

Patch:
Definedness Operator (&)

Form: &LValue
Precedence: Unary (same tier as !, not, unary +/-)
Associativity: Right-to-left

Meaning:
The & operator tests whether an lvalue path is bound at runtime.
It answers the question: “does this variable/member/index exist?”

Works with identifiers, member access (obj.field), indexing (arr[i]), and optional chaining (?>>).

Returns true if the binding exists, even if the value is nil.

Returns false if the binding does not exist (e.g. undefined variable, missing field, absent index).

Does not evaluate the value; only checks binding existence.

Applying !&expr yields the logical opposite: “is this path not bound?”

Errors:

& requires an lvalue. &(x + 1) → SyntaxError: & requires a variable/field/index.

Examples:

if &config.database_url
    connect_to_db(config.database_url)
end

if !&session.user_id
    redirect_to_login()
end

for key in dynamic.keys()
    if &dynamic[key]
        process_field(key, dynamic[key])
    end
end


📛 Name:
I suggest calling it the Definedness Operator in the spec.
Short-hand description: &expr → “is expr defined?”

That way you’ve got clear semantics (definedness) but the shorthand is just &.

## Numbers

* Integers: `0` | `[1-9][0-9]*` with optional explicit suffix `i`
* Floats:

  * `<int> "." <digits> ( [eE] [+-]? <digits> )?`
  * `<int> [eE] [+-]? <digits>`
  * Optional explicit suffix `f`
* Big numbers: append `b` (works with int or float forms).
* No underscores in v1 **except** digit-shorthand form below.
* Big: default precision = 64 significant digits.
* Big: “Any with big → big.” (promotion rule).
* Money: backed by big engine with project-defined policy (precision, rounding, ledgering).
* Percent: `%o` = sugar for `% of expr` (parser expands); `%s` = sugar for `% of self`
* User can set separator of choice for numbers (_ , . etc)

### Add/confirm literal forms

* Big numbers: explicit suffix `b` for integers and floats (e.g., `123b`, `5.0b`).
* Money: code-trailing form `<ISO3> <number>` (e.g., `EUR 950.50`) is valid, in addition to symbol-leading and country+symbol forms.
* Percent system:
  * % postfix = % of 100  // % infix is modulus
  * Shorthand `%s` = percent-of-self.
  * Shorthand `%o` = percent-of-other.
  * Clarify `% of expr` form: parser handles expansion, but lexer must recognize `%o` token followed by expression.
* Casting suffixes for pct:

  * `.pct!` strict cast (raises CastError on failure).
  * `.pct?` safe cast (returns nil on failure).

### Add/confirm

* Escapes (non-raw only): `\n`, `\t`, `\"`, `\'`, `\\`, `\uXXXX` (exactly 4 hex).
* Interpolation:

  * `{expr}` → INTERP\_START/END
  * `{{` → LITERAL\_BRACE\_OPEN, `}}` → LITERAL\_BRACE\_CLOSE
  * Zero-arg methods allowed inside interpolation (e.g., `{name.slug}`).

### Confirm/extend string literal forms

* Basic forms: `"..."`, `'...'`, `"""..."""`, `'''...'''`
* Empty string is falsy in conditionals.
* Parse-time modifiers: `raw`, `trim_lead` (may stack, order irrelevant).
* Literal sugar: `"literal".raw` and `"literal".trim_lead` normalized to prefix-on-literal.
* `x.raw` where x is a variable is invalid.

## Randomness Literals

* **Pick digit shorthand:** `NUMBER '_' NUMBER` → `PICK_DIGIT_SHORTHAND`

  * e.g. `5_4` means *five 4-digit numbers*.
  * May be followed by `unique` or `!dups` modifiers.
* **Dice notation:** `NUMBER 'd' NUMBER ([+/-] NUMBER)?` → `DICE_LITERAL`

  * e.g. `3d6`, `4d6+2`, `1d200-100`.
  * `roll` / `roll_detail` keywords precede it.

## Percent vs modulo (CIPO)

* `8.5%` (no space) → `NUMBER` then postfix `PERCENT_LITERAL` (percentage points).
* `25%s` (no space) → `PERCENT_SELF` (percent-of-self sugar).
* `25%o` (no space) → `PERCENT_OF_OTHER` token (shorthand for `of`).
* `% of E` form: percent applied to explicit base expression `E` (parser handles).
* Bare `%` → modulo operator.
* **Clarification:** Percent is lexical (e.g., `12.5%`); modulo `%` is arithmetic. `10% of 100` → `(0.10 * 100)`. `10 % 5` → modulo.
* **Precedence:** Percent literals/applications bind tighter (group 6) than normal modulo `%` (group 7).
* Ambiguity rule: If % immediately follows a NUMBER (no whitespace), it participates in the percent literal family (`%`, `%s`, `%o`). Otherwise % is parsed as the modulo operator.

## Durations (postfix)

* Units: `s|m|h|d|w|mo|y` (greedy: `mo` beats `m`).

* `m` = **minutes only**; months are `mo`. Unknown units → `SyntaxError`.

* Tokenization: `NUMBER` followed by `DURATION_UNIT(unit)`.

* **Datetime literals:** `datetime "YYYY-MM-DDTHH:MM[:SS]" tz:"Area/City"|"+HH:MM"`

* `tz` lexes as `IDENT('tz')` `:` `STRING`.

* Zone names/offsets are not validated by lexer.

* **Duration units:** `s`, `m`, `h`, `d`, `w`, `mo`, `y`.

* `m` = minutes only; `mo` = months

* Policy fields may use duration literals (s|m|h|d|w|mo|y); the existing greedy unit rules apply (e.g., mo ≠ m).

## Money literals

Emit single `MONEY` token for:

* Symbol-leading: `[$€£¥]<number>`
* Code-trailing: `<number> <ISO3>` (space required; `ISO3 = [A-Z]{3}`)
* Country+symbol: `<CC>$<number>` (`CC = [A-Z]{2}`, e.g., `US$30.00`)
* Negatives allowed either as leading `-` or `$-12.34`. (Capture sign in token.)
* **Collision rule:** `m` suffix is reserved for minutes (duration). Never valid for money. Use `$50.00` or `50 USD` instead.
* Unknown suffixes (e.g., `7z`) → `SyntaxError`.

> Parser enforces currency semantics; lexer only recognizes shapes.

## Dates & Times

**Canonical literal forms**

* `date "YYYY-MM-DD"`
* `time "HH:MM[:SS[.fff]]"`
* `datetime "YYYY-MM-DDTHH:MM[:SS]" tz:"Area/City"|"+HH:MM"`

**Notes**

* Canonical is **keyword + string** (e.g., `date "...")`. The call-style `date("...")` is legacy; prefer canonical.
* `tz:"..."` is tokenized as `IDENT('tz') COLON STRING`. Zone can be IANA (e.g., `"America/Denver"`) or fixed offset (e.g., \`"+00:00").
* Lexer does not validate zone names; parser/runtime may.

Lex as `DATE_LITERAL`, `TIME_LITERAL`, `DATETIME_LITERAL` tokens with attached string payloads and optional tz payload.

## Blob literals

* **Keyword form:** `blob` is a hard keyword.
* `blob` → empty blob literal, tokenized as `BLOB_LITERAL` (empty).
* `blob "..."` → UTF-8 encode string → `BLOB_LITERAL` with attached bytes.
* No implicit conversion between strings and blobs; must use explicit constructors or `.str` methods.

## Classes 

* declare a class with @Class_name `@` symbol 
* class name should start with Capital letter 
* multiline class should delimit with closing `end` or `}`

## Strings & interpolation

**Literal forms**

* `"..."`, `'...'`, `"""..."""`, `'''...'''`

**Modifiers (prefix keywords)**

* `raw <string>`
* `trim_lead <string>`
* `raw trim_lead <string>` (any order)
* Lexer sets flags on the resulting `STRING` token when modifiers precede it.

**Literal sugar**

* `"literal".raw` and `"literal".trim_lead` are normalized to prefix-on-literal at lex stage.
* `x.raw` where x is a variable → **invalid**.

**Escapes (non-raw only)**

* `\n \t \" \' \\ \uXXXX` (exactly 4 hex)

**Interpolation (non-raw strings; all quote styles)**
Inside string mode:

* `{{` → `LITERAL_BRACE_OPEN`
* `}}` → `LITERAL_BRACE_CLOSE`
* `{`  → `INTERP_START` (switch to normal lexing until matching `}`)
* `}`  → `INTERP_END`
* everything else → `STR_TEXT`
  Raw strings: backslashes inert; brace escapes `{{`/`}}` still apply.

**Join operators**

* `|` → `JOIN_NOSPACE`
* `||` → `JOIN_SPACE`

---

> String operations (`.upper`, `.find`, `.replace`, etc.) are all built-ins → lex as `IDENT`.
> Regex literals use existing string rules with `like raw` — no special lexer tokens beyond keywords + STRING.

# Lexer Notes — v1.18 (Part 4: Operators, Precedence, Misc)

## Math Operators (numeric)

* `/` → always float division
* `//` → integer division
* `%` → modulo (unless postfix literal % attached)
* `**` → power operator
* `9**`  → `POSTFIX_SQUARE`
* `16//` → `POSTFIX_SQRT`
* `><` → divmod operator, returns quotient+remainder pair
* `++` / `--` → postfix only; defined for money as well
* `%` -> spaces on either side is modulo
* `!` -> POSTFIX FACTORIAL
* `^` -> POSTFIX CEIL
* `_` -> POSTFIX FLOOR

## Operator Precedence (for parser, recorded here)

1. ()                    // grouping
2. ., (), \[], ?>>         // member, call, index, opt chain (all left-assoc)
3. \*\*, //, ++, --        // postfix square/sqrt/inc/dec
4. % of, %o              // percent-of-other application (binds tight, parenthetical-like)
5. \*\* (right)        // exponentiation
6. unary + - not !       // unary operators
7. * / % // >>           // multiplication/division/modulo/divmod
8. * * ```
                     // addition, subtraction
       ```
9. \| ||                  // string joining
11. ??                   // null-coalescing
12. comparisons: = !=, == !==, === !===, < <= > >=, is, is not
13. and or (with alias && <>)              // logical ops
14. &                     // definedness operator

// Notes:
// - `?>>` is left-associative; short-circuits only on nil (semantics). Short-circuit argument rule: if E?>>m(args…) short-circuits because E is nil, argument expressions are not evaluated.
// - Percent application binds tightly (group 6). Use parentheses for clarity in complex bases.
// - `%` as modulo lives in group 7. `10%` (literal) ≠ `10 % 5` (modulo).
// - `%` as percent literal is lexical; modulo is an operator token.
// - `**` and `//` and `!` have both postfix and infix forms; spacing disambiguates.
// - Exponentiation is right-associative.
// - Bitwise ops exist only as method calls, no lexer tokens.

Goblin splits the `=` family into three clean families:

= / != → assignment state

== / !== → value equality

=== / !=== → strict identity

* Right-hand callable requirement: the right side of |> must resolve to something callable that accepts the piped value as its first parameter, otherwise an ArityMismatchError (or a type error) is raised.
* Keep the existing precedence (joins |/|| bind tighter than |>), as already defined in Part 4. §25 examples should respect that ordering.
* raise form: raise is a hard keyword. The bare form (raise with no argument) is valid inside a rescue arm and rethrows the current error; there is no special token beyond KEYWORD(raise) (semantics handled later).
* In glam contexts, alias::symbol uses the same :: token. No change in lexing.
* @ver is new: introduce AT\_VERSION token class, emitted when @ prefixes a semver/range string in a use statement.
* **Loop stride:** Only `jump` is a hard keyword.
* `step` is *not* a keyword; lexes as `IDENT` if encountered. Parser rejects in loop headers.
* **Method calls:** `.iso()`, `.format(...)`, `.epoch()` → `.` `IDENT` with call punctuation.

## Whitespace & pipelines

* Newline normally emits `NEWLINE` (statement terminator).
* **Implicit continuation**:

  * within any open `(` `[` `{`

## Module paths (token shapes only)

* `import` (KW) plus a quoted string or `./` relative path. **All module paths resolve under the fixed `<project>/modules/` root** (created by the CLI). Lexer emits `STRING` or `PATH`; resolver/policy enforce:

  * `.gbln` suffix optional.
  * **no `..` segments**, **no absolute paths**.
  * **alias required**: `import "./helpers" as H`.
* Access always via **`Alias::symbol`**. Dot `.` is for methods/properties on values, **not** for module access.

\#########################################################################################################################################################

# Lexer Notes — v1.18 (Part 4)

## Glams & Contracts (token shapes only)

### `use` (glam loading & version pins)

* **Form:** `use IDENT [@ VERSION] [as IDENT]`
* **Tokens:** `KEYWORD(use)` `IDENT` \[`@` `VERSION`] \[`KEYWORD(as)` `IDENT`]
* **`VERSION` (contextual token):** only recognized **immediately after `@` in a `use` statement**. Lex as a single `VERSION` token (not an operator sequence). Accept characters: `[A-Za-z0-9._*^+~-]+` (covers `1.2.3`, `^1.6`, `~2`, prerelease/build tags). Whitespace/comma/`as` terminates the token.
* **Notes:** `^` here is **not** the `EXPLAIN_POW` operator; it is part of the `VERSION` token when inside `use ...@VERSION`.

### Provider binding (`via`) and defaults (`prefer`)

* **Call‑site bind:** `expr via alias`  → tokens: `EXPR` `KEYWORD(via)` `IDENT`.
* **Namespaced symbol bind:** `expr via alias::Symbol`  → uses existing `NAMESPACE (::)` token: `EXPR` `KEYWORD(via)` `IDENT` `NAMESPACE` `IDENT`.
* **Project/default bind:** `prefer capability.name via alias`  → tokens: `KEYWORD(prefer)` `IDENT` `.` `IDENT` `KEYWORD(via)` `IDENT` \[`NAMESPACE` `IDENT`].
* **Precedence/assoc:** no changes; `via` and `prefer` are **declarative**; parser handles binding.

### Contracts (capability interfaces)

* **Form:** `contract Name(params...) -> RetType ... end`
* **Tokens:** `KEYWORD(contract)` `IDENT` `(` *param tokens* `)` `ARROW(=>)` *ret‑type tokens* ... `KEYWORD(end)`.
* **Semantics:** Parser validates signature/declared errors; lexer just emits tokens.

### Events (provided by events GLAM, not core)

* `emit` and `on` **are not core keywords**. They lex as `IDENT` unless a policy/module elevates them. Parser/runtime routes them only when the events GLAM is present.

### CLI verbs (out of language scope)

* Reserved glam CLI op names: `spawn`, `build`, `export`, `push`, `lint`, `test`, `info`, `help`.
* These are **identifiers in source files**; reservation applies only in the external CLI.

### No operator/precedence changes

* §31 introduces no new operators; it reuses existing `::` (NAMESPACE) and `=>` (ARROW).

---

## Lambdas

* `=>` → `ARROW` (idents around it are normal).

## Actions
* Functions are called "free actions" and methods are called "member actions" 
* Defined with hard keyword `act` or `action`. Two declaration forms:

  * `act name(params) ... end or }` (block form)
  * `act name(params) = expr` (one-liner)
  
* Return rules: last expression returned unless `return` used explicitly.
* **Calling forms:**

  1. Method-style: `value.act(...)` (universal for unary; multi-param requires receiver as first param).
  2. Prefix-style: `act value` (restricted to built-in text actions; lexer emits IDENT + STRING).
  3. Function-style: act(value) 
* **Zero-arg actions:** may be called as `value.act` or `value.act()`.
* **Variadic params:** use `...` marker (same token as RANGE\_EXCL, disambiguated in parser).
* **Namespaces/Glams:** external calls use `Module::act(args)` or `Glam::act(args)`. Lexer reuses `::` token.
* **Pipelines:** segments must be `.act` or `Module::act(...)`. Each stage receives previous result as first argument.

---

## Token Inventory (flat)

**Keywords (hard):**
if, elif, else, for, in, while, repeat, unless, attempt, rescue, ensure, return, skip, jump, until, stop, assert, class, fn, op, enum, use, import, export, via, test, true, false, nil, judge, morph, override, vault, banish, unbanish, expose, pick, roll, roll\_detail, reap, usurp, replace, add, insert, map, nc, self, of, end, blob

**Keywords (soft / context):**
from, at, first, last, to, into, with, dups, seq, as, where, raw, trim\_lead, on, like, and, between, unique, like, before, after, before\_last, after\_last, between, words, chars, lines, split, join, match

* `between` / `!between` are parser sugar for chained comparisons.

  * Lexer emits `KEYWORD(between)`; `!between` is tokenized as `!` plus `KEYWORD(between)`.
* `where` is parser sugar in loop headers. Lexer always emits `KEYWORD(where)`, parser applies it as a filter.

## Token Inventory

* Uses existing forms:
* `KEYWORD(date|time|datetime)` + `STRING`
* `IDENT('tz')` `:` `STRING`
* `NUMBER` + `DURATION_UNIT`
* Built-ins as `IDENT`

**Identifiers:**
IDENT, AT\_IDENT (@ident), HASH\_IDENT (#ident)

* Enum variants lex as IDENT inside `enum` declarations.
* IDENT allows trailing `!` or `?` (e.g., `pct!`, `pct?`).

**Literals:**
STRING (flags: raw, trim\_lead); MONEY; NUMBER (int/float with optional suffix i/f, big suffix b); DATE\_LITERAL; TIME\_LITERAL; DATETIME\_LITERAL; DURATION\_UNIT (`s`,`m`,`h`,`d`,`w`,`mo`,`y`); PERCENT\_LITERAL; PERCENT\_SELF; PERCENT\_OF\_OTHER; BLOB\_LITERAL; PATH; PICK\_DIGIT\_SHORTHAND; DICE\_LITERAL

**Operators & punct:**
`...`, `..`, `::`, `||`, `|`, `??`, `?>>`, `**`, `>>`, `><`, `++`, `--`, `//`, `%s`, `%o`, `==`, `!=`, `<=`, `>=`, `===`, `!==`, `!===`, `<`, `>`, `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `**=`, `+`, `-`, `*`, 
`/`, `%`, `:`, `,`, `.`, `;`, `@`, `#`, `=>`, `(`, `)`, `[`, `]`, `{`, `}`, `&&`, `!`, `is`, `is not`

**Postfix specials:**
POST\_INC\_MONEY (`++`), POST\_DEC\_MONEY (`--`), POSTFIX\_SQUARE (`**` after number), POSTFIX\_SQRT (`//` after number)

**Comments:**
LINE\_COMMENT (`///`), BLOCK\_COMMENT (`//// ... ////`)

**Layout:**
NEWLINE, INDENT, DEDENT, EOF

**String mode (inside STRING):**
STR\_TEXT, INTERP\_START (`{`), INTERP\_END (`}`), LITERAL\_BRACE\_OPEN (`{{`), LITERAL\_BRACE\_CLOSE (`}}`)

**Other:**
ARROW (`->`), LAMBDA (`=>`), NAMESPACE (`::`)

---

## Misc

* **File extension:** source files use `.gbln` as canonical extension.
* **Action definition:**

  * Declared with `act` or `action`.
  * Block form: `act name(params) ... end or }` (last expression returned unless `return` is used).
  * One-liner form: `act name(params) = expr`.
  * `return` exits immediately, overriding the implicit return.
* **Action calls:** three call forms are valid:

  * **Method-style:** `value.act(...)` (receiver as first argument).
  * **Prefix-style:** `act value` (English-like sugar for text actions only).
  * **Function-style** `act(value)` function-call form is permitted.
* **Parameters:**

  * Defaults bind in the parameter list.
  * Named args may be mixed with positionals.
  * Variadic params use `...` (same token as RANGE\_EXCL; parser disambiguates).
  * Zero-arg actions may be called with or without `()` (e.g. `items.len` or `items.len()`).
* **Unary vs multi-parameter actions:**

  * Unary acts auto-methodize (e.g. `9.square`).
  * Multi-parameter acts: first param acts as receiver in method-style.
  * Otherwise, invoke via `Module::act(...)` or `Glam::act(...)`.
* **Wrapping external acts:** Users may define wrapper acts if they want external `::` actions to be chainable in method-style.
* **Output sugar:** A line beginning with a string literal is equivalent to prefixing with `say`.
  Example: `"Hello"` → `say "Hello"`
* **Variable scope:**

  * Variables are created on first assignment.
  * Global variables exist at top level.
  * Operation-local variables shadow globals.
  * Constants (ALL\_CAPS) are a naming convention only. They lex as IDENT and can technically be reassigned, but reassignment may raise runtime warnings.
* **Naming rules:**

  * Must start with a letter or underscore.
  * Can contain letters, numbers, underscores.
  * Cannot start with a number, contain a dot, or be a reserved word.
* **Strings:** immutable; all operations return new strings.
* **Join operators:** `|` joins without space, `||` joins with a single space.
* **Interpolation:** auto-converts values to text.
* **find\_all:** non-overlapping; indices are Unicode code-point based.
* **trim/trim\_lead/trim\_trail:** remove whitespace by default; with an argument, remove that substring instead.
* **strip\_lead / strip\_trail:** deprecated. Use the trim family instead.
* **Percent rules:**

  * Percent literal `NUMBER%` is lexical (part of the literal), not an operator.
  * Percent-of-other (`% of E` / `%o E`) binds tightly (see group 4).
  * Percent-of-self (`%s`) is a parse-time desugaring, not a precedence operator:
    E\_lhs … N%s  ⇒  E\_lhs … (N% of E\_lhs …)
    Example: `total - 15%s`  ⇒  `total - (15% of total)`
* **Loop keywords:** `jump` and `until` are hard keywords. Lexer always emits them as KEYWORD tokens; parser enforces that they only appear inside loop headers. Outside of loop constructs, they will cause a syntax error.
* **Time anchoring:** `.wrap` (used for anchoring times across midnight) is not a keyword. It lexes as IDENT following DOT (method-style).
* nc is only valid immediately after :: as a template skip placeholder; otherwise SyntaxError.
* Quoted keys ("key":) are invalid in bindings/templates (IDENT only).
* Enum variants are always IDENT in source; the lexer does not distinguish symbolic vs backed. Parser handles .value resolution.
* Money postfix `++`/`--` increment/decrement by **one major unit** (currency base step).
* “% self is parser sugar for %s; lexer emits % then KEYWORD(self).”
* Optional chaining: guards only against nil; any error from a non-nil evaluation propagates.
* Gmark persistence paths like .goblin/gmarks.lock and .goblin/gmarks.audit.log are string/path literals only; the lexer does not treat them specially. All Gmark APIs are method/service identifiers; no new operators or keywords are introduced.
* Banish rules are not new tokens. The feature IDs (core.morph, op.pipe\_join, etc.) are always lexed as plain IDENT + DOT + IDENT.
* Config files (.goblin.banish.toml) are outside lexer scope; the lexer never special-cases them.
* Diagnostics (BanishError) are surfaced later (compile/lint), not at lex time.
* set @policy … end introduces no new tokens beyond existing: KEYWORD(set), AT\_IDENT('@policy'), IDENT|STRING (policy name), map punctuation, and KEYWORD(end).
* Policy application & precedence (inline > file header > project default) are parser/semantic concerns; not lexer-level.
