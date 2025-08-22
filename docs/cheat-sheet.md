# Goblin Language AI Cheat Sheet v1.5.6
*Complete syntax reference for AI assistants - Reorganized for Documentation Flow*

Cheat sheet = AI sync file

Short, canonical rules
High-signal, no nuance
Source of truth for any AI working on Goblin
If there's ever a conflict, cheat sheet wins

Full docs = Human expansion

Can be detailed, narrative, educational
Can cover gotchas, edge cases, examples
Can be "poetic" and explanatory
But never contradicts the cheat sheet rules

Workflow:

Cheat sheet first - establishes the canonical rules
Docs expand - add detail, context, examples, warnings
Any conflicts - cheat sheet always wins

## GETTING STARTED

### Basic Syntax Overview
- One statement per line
- Indentation defines blocks (spaces only, no tabs)
- No semicolons

### Identifiers & the dot
**Identifiers:** letters, digits, underscore; can't start with a digit; no dots.

**VALID:** `user`, `user_name`, `TeamRoster`, `x2`  
**INVALID:** `user.name`, `user-name`, `2cool`

**Dot usage (only these):**
- Method-style op: `value.op` / `value.op()` / `value.op(args...)`
- Type/namespace (unbound op value): `str.upper`
- Decimals: `1.25`

Anything else with `.` is a syntax error.

**No shadowing:** you cannot have a field and a zero-arg op with the same name (compile-time error).

### Comments
```goblin
/// Single-line comment

//// 
Block comment content
////
```

### Variables
```goblin
name = "Frank"
age = 45
price = $1.50
```
- no dots in variable names (conflicts with operations)
- no immutable variables or constants
- use convention of all caps for intended constants (which can still change!)
- goblin should check for multiple uses of same uppercase variable names and warn

## LANGUAGE GUIDE

### Core Concepts

#### Operators & Precedence (high → low)
```
()                                  /// grouping
.  ()  []  ?.                       /// member/call/index/optional chaining
**  //  ++  --                      /// postfix (square, sqrt, inc/dec)
**  ^^                              /// power (right-assoc)
unary + - not !                     /// unary
%  %s  % of E                       /// percent literals & self-reference
* / % // >>                         /// multiplicative + quotient/divmod
|>                                  /// pipeline (left-assoc; tighter than + -, looser than member/call/?.)
+ -                                 /// additive
| then ||                           /// string joins
??                                  /// null-coalescing (right-assoc)
== != < <= > >= === !== is is not   /// chainable comparisons
and or                              /// logical (aliases: && for and, ! for not)
```

**Notes:**
- Postfix ops bind to the nearest primary
- `//` is postfix sqrt vs infix quotient based on position
- No inline `?:` operator in Goblin

#### Key Operators
```goblin
/// Postfix Math
9**        → 81                    /// square
16//       → 4                     /// square root (postfix)
x++        → old x, then x = x + 1 /// increment (returns old value)
x--        → old x, then x = x - 1 /// decrement

/// Division & Divmod  
10 / 3     → 3.333...              /// float division
10 // 3    → 3                     /// integer quotient
10 >> 3    → 3 r 1                 /// REPL pretty-print of DivRem{q=3, r=1}
(10 >> 3).tuple → (3, 1)           /// canonical tuple view
(10 >> 3).q → 3                    /// quotient
(10 >> 3).r → 1                    /// remainder

/// Comparisons
==   /// value equality (numeric 3 == 3.0 is true; containers compare structurally)
!=   /// negation of ==
===  /// strict identity: type AND value must match; for objects/enums: same identity
!==  /// negation of ===
is / is not  /// same-variant checks for enums and type predicates

/// String Joins
"a" | "b"  → "ab"                 /// no-space join
"a" || "b" → "a b"                /// space join

///Between
- x between a..b    → inclusive range check
- x !between a..b   → negated range check

/// Pipeline
text |> .replace("a","b") |> .upper   /// equals text.replace("a","b").upper

/// Optional Chaining & Null-Coalescing
obj?.prop  → prop if obj exists, nil if obj is nil/undefined
obj?.method(args) → call if obj exists, nil otherwise
user.profile?.email ?? "unknown@example.com"  /// null-coalescing example
```

### Data Types

#### Primitives
```goblin
42                  /// int
3.14               /// float
true, false        /// bool
"hello"            /// string
nil                /// null/undefined
```

#### String Features
```goblin
"Hello {name}"    /// interpolation
.upper() .lower() .title() .slug()  /// operations

// zero-arg → parens optional
"dan".upper   "dan".lower   "dan".title   "dan".slug
```
## Casting

Goblin supports two ways to cast values:

* **Prefix-style:** `int x`, `float x`, `str x`, `bool x`
* **Method-style:** `x.int`, `x.float`, `x.str`, `x.bool`
  *(parens optional for zero-arg ops)*

Casting **never mutates** the original value; it returns a new value or throws.

### Quick rules

* **Numbers**

  ```goblin
  int 3.9        /// 3   (truncate toward 0)
  (-3.9).int     /// -3
  float 3        /// 3.0
  "42".int       /// 42
  "3.14".float   /// 3.14
  "  7 ".int     /// ⚠ ValueError (no auto-trim) → use .trim first
  ```
* **Strings**

  ```goblin
  str 42         /// "42"
  3.5.str        /// "3.5"
  true.str       /// "true"
  nil.str        /// ""        /// interpolation uses same rule
  [1,2].str      /// "[1,2]"   /// stable literal form
  {k:"v"}.str    /// "{k:\"v\"}"
  ```
* **Booleans**

  * `bool x` checks **truthiness** (presence/emptiness), not the word "true/false".
  * Use `parse_bool` to **interpret** specific strings.

  ```goblin
  bool ""        /// false     (empty string)
  bool " "       /// true      (non-empty)
  bool 0         /// false
  bool 5         /// true
  "".bool        /// false

  parse_bool "true"   /// true    (also "false","yes","no","1","0"; case-insensitive)
  parse_bool "maybe"  /// ⚠ ValueError
  ```
* **Dates & Times (constructors, not generic casts)**

  ```goblin
  date "2025-08-12"
  time "14:30:00"
  datetime "2025-08-12 14:30" tz:"UTC"
  duration 3600               /// seconds
  ```
* **Money & Percent**

  * Prefer **literals**: `$1.50`, `EUR 2.00`, `8.5%`, `10%s`.
  * Casting from arbitrary strings to money/percent is **not** in core (use literals or a glam parser).

### Errors

* **ValueError** for invalid textual forms
  `int "12a"` → ValueError
* **TypeError** for unsupported conversions
  `int [1,2,3]` → TypeError
* **Design note:** Casting is explicit; no auto-trim, no locale guessing, no silent rounding beyond `int` truncation.

### Cheat-ready examples

```goblin
id = "42".int
price_text = 3.5.str
is_blank = (text.trim.len == 0).bool      /// true if empty after trim
flag = parse_bool user_input.trim         /// strict boolean from input
when = datetime "2025-01-01 09:00" tz:"UTC"
```

### Calling Operations

**Two styles only:** method-style (`val.op` / `val.op(args)`) and prefix-style (`op val`, special English forms). *No* global `op(val)` calls.

Goblin has exactly two styles for calling operations:

#### Method-style (dot) — chaining powerhouse

**Zero-arg: parens optional**
```goblin
s.upper        /// == s.upper()
s.trim_lead    /// == s.trim_lead()
```

**With args: parens required**
```goblin
s.replace("a","b")
s.pad(10, " ")
nums.pick(3)
```

**Left-to-right chaining**
```goblin
result = text.trim_lead.upper.slug
result = text.trim.replace("old","new").upper
```

**Dot lookup rules**
1. If `op` is a field on value → return field
2. Else if zero-arg op exists → auto-invoke (with/without ())
3. Else if op needs args → return bound callable (call later with ())
4. Else → UnknownNameError

Type access never auto-invokes: `str.upper` gives the unbound op value.

#### Prefix-style (English-y) — for zero-arg & special forms

**Zero-arg (and special English forms) only**
```goblin
upper s
trim_lead s
replace "a" with "b" in s
split s on ","
join parts with ", "
before ":" in s
```

**Left-to-right chaining (zero-arg only)**
```goblin
result = trim reverse upper text
/// text → upper → reverse → trim
```

*Don't mix multi-arg English forms into a prefix chain; use them standalone or via method-style.*

**Note:** You may still call a callable value held in a variable with `()`. The "no function-style" rule only bans the global `op(value)` spelling; it does not ban calling a variable that holds an op.

#### Prefix-Style Calls (What They're For)

**Scope**
- Prefix-style calls are reserved for a small set of built-in TEXT operations and other English-surface helpers (e.g., trim, upper, slug).
- They are NOT used for math (sum, min, round, floor, ceil, abs) and NOT available to user-defined ops.

**Why**
- Keeps code readable without encouraging ambiguous chains like `sum max nums`.
- Math and user ops stay method-style (or pipeline) for clarity and auditability.

**Allowed (built-in text ops only)**
```goblin
text = "  Hello World  "
clean = trim upper slug text               /// "hello-world"
/// identical with pipeline:
clean = text |> .trim |> .upper |> .slug
```

**Common prefixable text ops:**
`trim`, `trim_lead`, `trim_trail`, `upper`, `lower`, `title`, `slug`, `escape`, `unescape`, `reverse`

**Chaining rule**
- Prefix chaining is only for zero-arg built-in text ops. If an op needs arguments, use method-style.

**Disallowed for prefix**
```goblin
/// Math ops → method-style only
total   = numbers.sum                      /// ✅
small   = numbers.min                      /// ✅
rounded = 3.7.round                        /// ✅
/// total = sum numbers                    /// ❌
/// r     = round 3.7                      /// ❌

/// Bitwise ops → method-style only
x = 5.bit_and(3)                           /// ✅
/// x = bit_and 5 3                        /// ❌

/// User-defined ops (op) → no prefix form
op double(x) return x * 2
val1 = 10.double                           /// ✅ method-style
/// val2 = double 10                       /// ❌ no prefix for user ops
```

**Summary rules**
- Prefix-style = built-in text ops only; zero-arg prefix chains OK.
- All math, bitwise, and user-defined ops → method-style (or pipeline).
- Prefer pipeline for readable multi-step flows:
```goblin
value = "  mix CASE  "
result = value |> .trim |> .lower |> .slug
```

§9.y Why Goblin Doesn’t Use `op(value)`

Goblin intentionally avoids the traditional function-call surface `op(value)` to keep the language visually consistent and audit-friendly.

### Design goals

* **One consistent look**: either `value.op` (method-style) or, for a small set of built-ins, `op value` (prefix for TEXT).
* **Fewer parentheses** and fewer mixed calling conventions to scan during code reviews and audits.
* **Clear left-to-right data flow**, especially with the pipeline operator.

### What to do instead

**Method-style (canonical)**

```goblin
total    = numbers.sum
rounded  = 3.7.round
masked   = "Hello".lower
```

**Pipeline (for step-by-step flows)**

```goblin
result = "  Hello World  "
       |> .trim
       |> .upper
       |> .slug
```

**Prefix (TEXT built-ins only)**

```goblin
clean = trim upper slug "  Hello World  "   /// "hello-world"
```

### Common pitfalls (and fixes)

```goblin
/// sum([1,2,3])      /// ❌ not Goblin
total = [1,2,3].sum  /// ✅ method-style

/// round(3.7)        /// ❌ not Goblin
r = 3.7.round        /// ✅ method-style

/// double(10)        /// ❌ for user ops
op double(x) return x * 2
r = 10.double        /// ✅ method-style
```


## Strings

### String/Text Operations (Core)

#### Zero-arg (paren-optional via dot)

```goblin
upper, lower, mixed, title, slug
trim, trim_lead, trim_trail
escape, unescape,
minimize
length (alias: len), reverse
```

**Examples**

```goblin
"daniel".upper        /// "DANIEL"
"  hi".trim_lead      /// "hi"
"daniel".reverse      /// "leinad"
"daniel".length       /// 6
"daniel".mixed        /// "dAniEL" (random)

s = "Daniel is awesome"
s.minimize            /// "Danielisawesome"

s = "C:\path\file.txt"
s.escape              /// "C:\\path\\file.txt"
unescape "line\\n"    /// "line\n"
```

#### Trimming & stripping (lead/trail family)

```goblin
trim s                        /// whitespace both ends
trim_lead s                   /// whitespace at start
trim_trail s                  /// whitespace at end
strip_lead  "pre"   from s    /// drop leading substring if present
strip_trail ".txt"  from s    /// drop trailing substring if present
minimize s                    /// remove ALL whitespace characters
```

#### Everyday text sugar

```goblin
before ":"         in s
after  ":"         in s
before_last "/"    in s
after_last  "/"    in s
between "<b>" and "</b>" in s

lines s            /// split on universal newlines
words s            /// split on whitespace
chars s            /// characters as an array (code points)
```

#### Literal find & replace (non-regex)

```goblin
has "needle" in s            /// bool
find "needle" in s           /// first start index (0-based) or nil
find_all "needle" in s       /// [start indices], non-overlapping
count "needle" in s          /// occurrence count

replace "old" with "new" in s        /// replaces ALL occurrences
replace_first "old" with "new" in s  /// only the first occurrence
```

**Method-style mirrors**

```goblin
s.has("needle")
s.find("needle")
s.find_all("needle")
s.count("needle")
s.replace("old","new")           /// all
s.replace_first("old","new")     /// first only
```

**Semantics**

* Searches are **literal** (no regex) and **case-sensitive**.
* Indices are **Unicode code-point** positions.
* `find_all` is **non-overlapping** (e.g., "banana" + "ana" → [1]).

#### Replace / remove / drop (strings are immutable)

```goblin
replace "old" with "new" in s          /// returns new string; replaces ALL
replace_first "old" with "new" in s    /// first only
remove  "," from s                      /// == replace "," with "" in s
drop    "," from s                      /// alias for remove (copy)
```

### String Literals

"abc"                /// double-quoted string
'abc'                /// single-quoted string
"""abc"""            /// multi-line string
'''abc'''            /// multi-line string (alt)

"line1\nline2"       /// escapes: \n, \t, ", ', \\, \uXXXX
"Hello {name}"       /// interpolation
"{{literal}}"        /// literal braces

#### Literals: raw & trim_lead at parse time

```goblin
p = raw "C:\path\file.txt"

block = trim_lead """
    Hello
      World
"""
```

**Literal sugar:** `"literal".trim_lead` and `"literal".raw` are parsed as prefix-on-literal.

**Important:** `x.raw` where `x` is a variable is not allowed (raw is parse-time). Use `escape`/`unescape` at runtime.

```
raw "abc"                 /// raw string (no escapes/interp)
raw """abc"""             /// raw multi-line string
trim_lead """
    line1
    line2
"""                       /// trims common indent
trim_lead raw """
    {not interpolated}
"""                       /// modifiers can stack
```

Modifiers like `raw`, `trim_lead`, etc. can be written in any order — they are declarative, not sequential.

## Strings — Split & Join

Human-readable forms for the most common text operations.

### Split

```goblin
words = split message on " "       /// ["hello", "world"]
parts = split csv_line on ","       /// CSV fields (no regex)
lines = split text on "\n"         /// manual line split
```

**Semantics**

* Returns an **array of strings**.
* Delimiter must be a **string**; otherwise **TypeError**.
* Empty delimiter → **ValueError**.
* Consecutive delimiters keep empty fields (no magic trimming).

### Join

```goblin
line  = join fields with ","              /// e.g., ["a","b"] → "a,b"
path  = join ["usr","local","bin"] with "/"

/// Ensure all elements are strings (explicit conversion when needed)
line2 = join ["{id}", name, "{price}"] with ","
```

**Semantics**

* Joins an **array of strings** with a **string** separator.
* Any non-string element → **TypeError** (be explicit with `str()`).
* Empty array → "" ; single element → that element.

---

### Quick Reference additions

```
split S on D                /// → [parts]
join A with S               /// → "joined"

has "x" in S                /// → bool
find "x" in S               /// → index | nil
find_all "x" in S           /// → [indices]
count "x" in S              /// → int
replace "a" with "b" in S   /// → replace ALL
replace_first "a" with "b" in S
```

---

### Regex (lean core; full power via glam)

**Core 20/80**

```goblin
find_all like PATTERN in s
match     like PATTERN in s
replace   like PATTERN with NEW in s
```

**Pattern subset:** `. [] () | ^ $ + * ? {m,n} \d \w \s`

Use raw for backslashes when helpful:

```goblin
emails = find_all like raw "[\\w.]+@[\\w.]+" in text
clean  = replace   like raw "\\s+" with " " in text
```

**Full PCRE → regex glam (opt-in, not core)**

```goblin
use glam regex as re
re.findall("(?<=\\$)\\d+(?:\\.\\d{2})?", text)
```

#### Collections

```goblin
[1, 2, 3]         /// array
{key: "val", price: 1.5}  /// map

/// Ranges
1..5              /// inclusive (1,2,3,4,5)
1...5             /// exclusive (1,2,3,4)
/// Stride with `jump`: for i in 1..10 jump 2  → 1,3,5,7,9
```

#### Collection Operations

**Non-destructive (copy)**

```goblin
replace X with Y in arr
drop    X from arr
pick names              /// random element
pick 3 from names       /// 3 random elements
shuffle names           /// shuffled copy
sort names              /// sorted copy
```

**Destructive (mutate in place)**

```goblin
usurp X with Y in arr          /// replace in place
cut   X from arr               /// remove in place
cut   at i from arr            /// remove by index
reap from names                /// remove & return random
reap 2 from names              /// remove & return 2 random
add "Item" to names            /// append
insert "Item" at 2 into names  /// insert at position
```
```goblin
scores = [85, 92, 78, 96, 88]
say sum(scores)   /// 439
say avg(scores)   /// 87.8
say min(scores)   /// 78
say max(scores)   /// 96

names = ["goblin","orc","troll"]
say map upper, names   /// ["GOBLIN","ORC","TROLL"]
```
## 🎲 Pick / 🌾 Reap / ✨ Unique — Random Selection, Destructiveness & Duplication

This section defines random selection with `pick` (non-destructive) and `reap` (destructive), and clarifies duplication at two scopes:

- **Result-level duplication** (are there repeats among the returned items?)  
  Controlled by **`with dups`** (allow repeats across results) or **`!dups`** (no repeats across results).
- **Item-internal duplication** (does a single generated item contain repeats?)  
  Controlled by **`unique`** (e.g., digits inside an `x_y` number are all distinct).

---

### Syntax

**Pick (non-destructive)**
- `pick from collection` → 1 random element
- `pick n from collection` → n random elements (**default: !dups** for finite collections)
- `pick n from collection with dups` → n elements, repeats allowed across results
- `pick n from collection !dups` → n elements, no repeats across results (explicit)
- `pick from start..end` → 1 random number in range
- `pick n from start..end [with dups|!dups]` → n numbers in range
- `pick x_y [with dups|!dups]` → pick **x** random integers, each **y** digits long
  - `pick 1_6` → one 6-digit integer (100000–999999)
  - `pick 5_4` → five 4-digit integers
- `pick x_y from start..end [with dups|!dups]` → same, constrained by range intersection
  - `pick 2_3 from 400..499 !dups`
- Post-processing: `… join with "sep"` converts the result array to a string

**Reap (destructive)**
- Mirrors `pick`, but **removes** selected values from the source (when source is a collection/range):
  - `reap from deck`, `reap 2 from deck !dups`, `reap 3 from deck with dups`
  - `reap 1_6`, `reap 2_3 from 400..499 !dups`, `reap 5_4 join with ", "`

**Unique (item-internal)**
- On digit shorthand (`x_y`), `unique` means **the digits inside each generated number are all distinct**.
  - `pick 1_5 unique` → one 5-digit number with no repeating digits
  - `pick 3_4 unique` → three 4-digit numbers, each internally digit-unique
- To also remove duplicates **across** the generated numbers, combine:
  - `pick 3_4 unique !dups`  
  - or `pick 3_4 unique |> unique` (pipeline `unique` dedupes the result set)

**Standalone / pipeline `unique` (non-destructive op)**
- `unique <collection>` → returns a deduped copy of the collection (result-level)
- `<collection> |> unique` → pipeline form

---

### Semantics & Defaults

**Result-level duplication**
- `with dups` → sampling **with replacement**: the same element/number may appear multiple times in the **result set**.
- `!dups` → sampling **without replacement**: all items in the result set are distinct.
- **Defaults**
  - Finite collections: `pick n from collection` defaults to **`!dups`**.
  - Numeric spaces (ranges, `x_y`): **default is `with dups`** (duplicates across results may occur), since the space is large and replacement is common. Use `!dups` when you require distinct outputs.

**Item-internal duplication**
- `unique` on `x_y` affects **digits inside each number** (does *not* affect the result set).  
  Example: `pick 5_3 unique` can yield `[123, 340, 507, 198, 246]`; digits don’t repeat *within* each number, but two numbers in the list might still be equal unless you also say `!dups`.

**Reap**
- Destructive: the source shrinks by the number of **unique items actually removed**.  
  With `with dups`, the same source element selected multiple times only reduces the source **once**; the returned list may still include repeats unless `!dups` is specified.
- `reap … !dups` returns a set of distinct items and removes exactly those from the source.

**Maps as sources**
- Be explicit about what you’re picking:
  - `pick from m.keys`, `pick from m.values`
- Weighted selection remains a separate op (outside this spec).

**Digit ranges**
- For `x_y`:  
  - `y = 1` ⇒ digits from `0..9`  
  - `y > 1` ⇒ implicit range `10^(y-1) .. (10^y - 1)` (no leading zeros)
- With `from A..B`, candidates = intersection of that implicit digit range with `[A..B]`.

---

### Errors

- `PickCountError` — requesting `!dups` when the candidate set is too small
- `PickRangeError` — `x_y from A..B` leaves no candidates after intersection (or `unique` internal constraint makes per-item generation impossible)
- `PickTypeError` — invalid source/type (e.g., applying digit rules to non-numeric)
- Existing `Reap*`/`Drop*` errors apply unchanged

---

### Examples

```goblin
/// Collections (default !dups)
pick 5 from names                 /// 5 distinct names
pick 5 from names with dups       /// repeats allowed across results
pick 5 from names !dups           /// explicit no-repeats

/// Ranges (default with dups)
pick 5 from 1..100                /// 5 numbers, repeats allowed by default
pick 5 from 1..100 !dups          /// 5 distinct numbers in range

/// Digit shorthand
pick 1_6                          /// e.g., 123456 (digits may repeat within other forms)
pick 5_4                          /// five 4-digit numbers (results may repeat)
pick 5_4 !dups                    /// five distinct 4-digit numbers
pick 1_5 unique                   /// one 5-digit number, no repeated digits (e.g., 13452)
pick 3_4 unique !dups             /// three 4-digit numbers, digits unique inside each, and all results distinct
pick 2_3 from 400..499 !dups      /// two distinct 3-digit numbers between 400 and 499

/// Joining / post-processing
pick 5_4 !dups join with ", "     /// "4829, 7710, 5500, 1234, 9083"

/// Reap mirrors pick (destructive)
reap 2 from deck !dups            /// remove & return 2 distinct cards
reap 3 from users with dups       /// may return repeats; source shrinks once per unique
reap 1_6 unique                   /// 6-digit number w/ unique digits (numeric generation; no collection to shrink)

/// Standalone unique (result-level dedupe op)
unique ["a","b","a","c"]          /// ["a","b","c"]
users |> map lower |> unique      /// dedupe after mapping
```

**Common reductions (method-style mirrors; paren-optional)**
```goblin
nums.sum   nums.avg   nums.min   nums.max   nums.len

/// examples
total = nums.sum        /// 10
count = nums.len
```

# 15. Date & Time (Core)

Goblin’s temporal system is explicit, safe, and policy-driven.

* **Types:** `date`, `time`, `datetime`, `duration`. No implicit coercions.
* **No naive timestamps.** A `datetime` always has a zone (from literal, `tz:` arg, or policy default).
* **Policy snapshot.** Policy is captured immutably at load or on `set @policy`; ops read O(1) fields.

---

## 15.1 Types

* `date` — calendar day (no time, no zone)
* `time` — clock time (no date, no zone)
* `datetime` — date + time + zone (zone **required**; defaults from policy)
* `duration` — elapsed seconds (no zone)

**Rule:** No implicit coercions between these types.

---

## 15.2 Policy & Defaults (Project‑wide)

**Default scaffold:**

```goblin
@policy = "project_default"
    datetime: {
        tz: local_tz(),          /// machine zone at startup
        prefer_trusted: false,   /// do not require server time
        policy: "allow",        /// local fallback is OK
        leap_mode: "clamp",     /// leap second handling (see §15.13)
        time_arith: "error",    /// "error" | "wrap" | "shift" (see §15.8)
        tzdata_version: "",     /// empty = engine default; else pin e.g. "2025a"
        debug: false             /// policy breadcrumbs & why_dt() detail
    }
```

**TZ accepts** IANA names (e.g., `"America/Denver"`), `"UTC"`, or fixed offsets (e.g., `"+00:00"`, `"-07:00"`).

**Performance:** Engines expose each policy domain as an immutable snapshot captured at script load or on `set @policy`. Datetime ops read O(1) fields (no map lookups/allocations). Common flags may be lowered to IR constants.

---

## 15.3 Trusted Time (Server → Cache → Local) — Opt‑in

Goblin core never calls the network; trusted time requires a glam with network permission.

### 15.3.1 Minimal preset

```goblin
set @policy trusted_minimal

@policy = "trusted_minimal"
    datetime: {
        source: "https://time.example/now",  /// HTTPS Date or {"epoch": float}
        policy: "warn",                      /// warn on fallback
        prefer_trusted: true,
        ttl: "60s", cache_ttl: "24h", skew_tolerance: "5s",
        cache_path: "dist/time.cache",
        cache_signing_key: "secret"
    }
```

### 15.3.2 API

* `trusted_now()        → datetime`
* `trusted_today()      → date`
* `last_trusted_sync()  → datetime | nil`
* `time_status()        → { source: "server"|"cache"|"local", verified: bool, age_s: int, offset_s: float, drift_s: float, tzdata: { local_version: string, server_version: string | nil, mismatch: bool } }`
* `ensure_time_verified(reason="")`  /// raise/warn per policy
* `clear_time_cache()   → nil`

**Behavior:** trusted\_\* uses server → signed monotonic cache → local per policy. In **strict**, missing/invalid trust ⇒ `TimeSourceError`. In **warn**, emit `TimeSourceWarning`. If server reports a tzdata version and it differs from local, `time_status().tzdata.mismatch = true`; `ensure_time_verified` fails in strict.

---

## 15.4 Construction & Parsing

```goblin
d  = date("2025-08-12")                          /// YYYY-MM-DD
t  = time("14:30:05")                            /// HH:MM[:SS[.SSS]]
dt = datetime("2025-08-12 14:30", tz: "UTC")     /// localized to tz
dt2= datetime("2025-08-12T14:30:00-06:00")       /// ISO-8601 w/ offset
```

**Strict parse helpers**

```goblin
parse_date(s, fmt="YYYY-MM-DD")
parse_time(s, fmt="HH:mm[:ss[.SSS]]")
parse_datetime(s, fmt="YYYY-MM-DD HH:mm[:ss[.SSS]]", tz=nil)
```

Tokens: `YYYY MM DD HH mm ss SSS ZZ` (`ZZ` = ±HH\:MM).

**Override rule:** If the string has `Z`/offset, it **overrides** `tz:` and policy defaults. Malformed → `ValueError`.

---

## 15.5 Duration Literals

```
1s 90s 5m 1h 2d 3w 6mo 1y
```

Units: `s` (seconds), `m` (minutes), `h` (hours), `d` (days), `w` (weeks), `mo` (months = 30d), `y` (years = 365d). Also `duration(n_seconds)`.

**Disambiguation:** `m` always minutes; `mo` months. Passing minutes to a calendar adder (e.g., `add_months(d, 3m)`) → `ValueError("minutes given where months expected; use 'mo' for months")`.

---

## 15.6 Now/Today & Zone Helpers

* `now()     → datetime`   /// local or trusted per policy (prefer\_trusted)
* `today()   → date`
* `utcnow()  → datetime`   /// always UTC; uses trusted chain if configured
* `local_tz() → string`
* `to_tz(dt, "UTC")`      /// change zone (wall-clock adjusts)

---

## 15.7 Formatting & Accessors

* `.iso()` (ISO‑8601), `.format("YYYY-MM-DD")`, `str(x)` = ISO
* **Never** emit named zone abbreviations—only numeric `ZZ`.

**Accessors (read‑only):** `year month day hour minute second millisecond weekday(1..7 Mon=1) yearday(1..366) date() time() tz():string epoch():float`

---

## 15.8 Arithmetic & Comparisons

**Date/DateTime arithmetic**

```goblin
datetime + duration  → datetime
datetime - duration  → datetime
date     + duration  → date       /// duration must be whole days
date     - duration  → date

/// Differences
datetime - datetime  → duration
date     - date      → duration

/// Comparisons
< <= > >= == != on like-types only  /// cross-type → TypeError
```

**Time arithmetic requires explicit intent**

* `time ± duration` → `TimeArithmeticError` (default).
* Policy desugaring (project toggle):

  * `time_arith: "wrap"`  — allow `time ± duration`, desugar to `wrap_time` (mod 24h)
  * `time_arith: "shift"` — desugar to `shift_time` (carry days)

**Helpers:** `wrap_time(t, dur) → time`, `shift_time(t, dur) → { days: int, time: time }`

---

## 15.9 Truncation / Rounding

`floor_dt(dt, unit)` / `ceil_dt(dt, unit)` where unit ∈ `year|month|week|day|hour|minute|second`.

* Weeks floor to **Monday 00:00** in the instance's TZ.

---

## 15.10 Calendar‑Safe Adders (field changes, clamped)

`add_days(d|dt, n)`, `add_months(d|dt, n)`, `add_years(d|dt, n)`

* Jan 31 + 1 → Feb 28/29
* Feb 29 + 1 → Feb 28

**Rule of thumb:** Use `duration` (`+ 1mo`, `+ 1y`) for elapsed time (30/365 days). Use adders for calendar changes. Goblin never guesses.

---

## 15.11 Ranges

**Dates (inclusive by default)**

```goblin
for d in date("2025-08-01")..date("2025-08-05")           /// default step=1d
for d in date("2025-08-01")..date("2025-08-05") step 2d
```

**Datetimes (exclusive by default)**

```goblin
for ts in dt_start...dt_end                                /// default step=1h
for ts in dt_start...dt_end step 30m
```

`..` = inclusive; `...` = exclusive. `step` must be a `duration`. Optional `inclusive:`/`exclusive:` flags may override defaults.

---

## 15.12 Interop & Serialization (JSON/YAML/CSV)

**Default read/write:** strings (no auto‑decode).

**JSON options:**

```goblin
write_json(path, value, { datetime: "string" | "object" = "string" })
read_json(path,  { datetime: "off" | "string" | "object" | "auto" = "auto" })
```

**Typed “object” form:**

```json
{"_type":"datetime","value":"2025-08-12T14:30:00-06:00"}
{"_type":"date","value":"2025-08-12"}
{"_type":"time","value":"14:30:00"}
{"_type":"duration","seconds":3600}
```

* `auto` tries object → strict ISO → passthrough.
* YAML/CSV mirror string mode; use parse helpers to opt in.
* When deserializing into a typed target (e.g., class field), strict ISO strings are accepted as that type even if `datetime:"off"`.

**Naive boundary adapters**

* `naive_to_datetime(epoch_s: float, tz: string) → datetime`    /// attach zone
* `datetime_to_naive_utc(dt: datetime)           → float`       /// epoch seconds, UTC
* `datetime_to_naive_local(dt: datetime)         → float`       /// epoch seconds, local

Malformed → `ValueError`.

---

## 15.13 Leap Seconds

Goblin uses POSIX/Unix epoch (no leap seconds). Parsing `HH:MM:60` is allowed **in UTC only** for historical data.

```goblin
@policy = "leap"
    datetime: { leap_mode: "clamp" | "smear" = "clamp" }
```

* **clamp:** parse as `HH:MM:59` + 1s; formatting never emits `:60`.
* **smear:** spread the leap second across the last second of the day. Formatting always normalizes per `leap_mode`.

---

## 15.14 Introspection & Debugging

* `dt_policy() → { tz, prefer_trusted, policy, leap_mode, time_arith, tzdata_version, source: string|nil, cache_path: string|nil, ttl: duration, cache_ttl: duration, skew_tolerance: duration, debug: bool }`
* `why_dt(op: string) → string`  /// Human‑readable explanation of how a datetime operation was interpreted (policy fields, parsing/adder/range decisions).

When `datetime.debug:true`, `@policy` changes append JSON lines with scope and diffs to `dist/policy.log`.

---

## 15.15 Errors & Warnings

* `ValueError` — malformed input or invalid operation
* `TypeError` — cross‑type comparisons/ops
* `OverflowError` — out of range
* `TimezoneError` — unknown tz
* `TimeSourceError` — strict trusted time failure
* `TimeSourceWarning` — trusted fallback in warn mode
* `TimeArithmeticError` — `time ± duration` attempted without helper
* `DatetimeCompatWarning` — behavior differs under `datetime_compat` profile

---

## 15.16 Compatibility & Migration (Non‑normative)

A prebuilt compat profile eases migration from naive ecosystems:

```goblin
@policy = "datetime_compat"
    datetime: { time_arith: "wrap", policy: "warn", leap_mode: "clamp" }
```

Emits `DatetimeCompatWarning` where behavior differs from canon (implicit wrapping, naive parse). Use lints/codemods to replace `time ± duration` with `wrap_time`/`shift_time`, add explicit `tz`, and swap elapsed/calendar math as needed. Tighten to `time_arith:"error"` and `policy:"strict"` once warnings are zero.

---

### Design Charter (recap)

No naive timestamps; explicit policy; trusted time is opt‑in; strict parsing/formatting; calendar‑safe adders; explicit human‑time math; clear ranges; clean serialization; deterministic core; surfaced tzdata; O(1) policy snapshots; first‑class introspection; presets & adapters; smooth migration.


#### Binary Data
```goblin
blob                 /// empty blob
blob "text"          /// from UTF-8 string
"SGVsbG8=".from_base64      /// ValueError on invalid base64
"DEADBEEF".from_hex         /// ValueError on invalid hex
```

#### Dice (only valid inside roll/roll_detail)
```goblin
roll 2d6+1        /// dice literal form
roll 1d20         /// operation form
r = roll_detail 4d8-2   /// r.dice, r.sum, r.total
```

### Control Flow

#### Conditionals
```goblin
/// Block form
if cond
    ...
elif cond
    ...
else
    ...
end

/// Unless (sugar for if not)
unless cond
    ...
else
    ...
end

### Between / !Between

Humanâ€'friendly range comparisons. Equivalent to x >= a and x <= b (or the inverse).

Form
if expr between range
    ...
end


if expr !between range
    ...
end
Examples
if age between 13..19
    say "Teenager"
end


if score !between 0..100
    error "Invalid score"
end

Works with inclusive (..) and exclusive (...) ranges.

Use in the Conditionals section alongside if / elif / else.

No new keywords required (negation via !).

/// Judge (expression form)
result = judge
    cond1: expr1
    cond2: expr2
    else: exprN
end

/// Judge inline
result = judge: cond1: expr1 :: cond2: expr2 :: else: exprN
```

#### Loops
#### Loops
```goblin
/// For loops
for i in 1..5                 /// ranges (inclusive)
for v in array                /// arrays (values)
for i, v in array             /// arrays (with index)
for k, v in map               /// maps (key/value)

/// Striding with `jump` (unified)
for i in 0..10 jump 2
    say i                     /// 0, 2, 4, 6, 8, 10
end

names = ["Alice","Bob","Charlie","Diana","Ethan","Fiona","George"]
for i, name in names jump 3
    say name                  /// Alice, Diana, George
end

for name in names jump 2
    say name                  /// Alice, Charlie, Ethan
end

## 5.2.x For … Where …

for … where … adds an inline filter to a for loop (no nested if).

Form
for var in collection where condition
    ...
end
Example
for score in scores where score >= 90
    say "Excellent: {score}"
end

Filters elements before running the body.

Sugar for for … in … + if condition guard.

/// While
while cond
    ...
end

/// Control
skip    /// continue
stop    /// break
```
Notes:

jump N strides the iteration by N.
Works on ranges (numbers, dates, datetimes) and indexed collections (arrays, maps via index order).

### Jump (staged strides)

`jump N until P` lets a loop change stride mid-flight, using **piecewise step schedules**.

#### Form

```goblin
for i in RANGE
    jump N until CONDITION
    jump M until OTHER
    jump K              /// optional final fixed stride
    ...
end
```

* Each `jump` defines the stride for a segment.
* The `until` condition says when to stop using that stride and switch to the next.
* If you omit a final `jump`, the loop falls back to stride `1`.

#### Semantics

1. Begin at the range's lower bound.
2. For a segment `jump N until P`:

   * Emit values at step `N` while `P(i)` is **false**.
   * The first value that would make `P(i)` true is **not emitted**; instead, the loop switches to the next segment.
3. When switching segments, alignment snaps forward so that the new stride starts on the correct multiple (based on the range's start).
4. After the last `until â€¦`, stride = `1` by default, unless overridden with a final `jump`.

#### Examples

Count by twos until you pass 10, then by threes until you pass 20, then finish by ones:

```goblin
for i in 1..25
    jump 2 until i > 10
    jump 3 until i > 20
    ...
end
/// 1,3,5,7,9
/// 12,15,18
/// 21,22,23,24,25
```

Ramp stride upward:

```goblin
for i in 1..30
    jump 1 until i >= 5
    jump 2 until i >= 15
    jump 5
end
/// 1,2,3,4
/// 6,8,10,12,14
/// 15,20,25,30
```

### 5.2.x Repeat

`repeat N` runs a block a fixed number of times with **no counter variable**.

#### Form
```goblin
repeat N
    ...
end
Examples
goblin
Copy
Edit
repeat 3
    say "Loading..."
end
/// Loading...
/// Loading...
/// Loading...

repeat 10
    create_test_user()
end
Expresses pure intent ("repeat N times").

No loop counter noise.

#### Error Handling
```goblin
/// Structured form
attempt
    risky_operation()
    "success"
rescue ErrorType as e
    handle_error(e)
    "recovered"
ensure
    cleanup()
end

/// Manual
error "message"
assert cond, "msg"
warn "message"
```

Grumble mode (goblin-flavored error text): OFF by default. Enable via config:
goblin.config.yaml → errors: { grumble: true }   /// otherwise errors are plain/neutral

### Operations

#### Definition
```goblin
/// Basic
op greet(name="Traveler")
    "Hello, {name}"    /// implicit return
end

/// Single-line
op add(a, b) = a + b

/// With explicit return
op complex(x)
    if x > 0
        return x * 2
    end
    return 0
end
```

**Unary ops auto-methodize**: `op f(x, …)` may be called as `x.f(…)`. Multi-arg → method-style only.

#### Calls
```goblin
/// unary prefix (preferred for simple ops)
greet "Alice"
greet name:"Bob"

/// OR: unary auto-methodization on 1st param
"Alice".greet
"Bob".greet
```

### Object-Oriented Programming

#### Classes

### Definition
```goblin
/// Template style definition (preferred)
class Pet = name: "{name}" :: age: 0
    op speak()
        "Hi, I'm {name}, age {age}"
    end
end
```
- No `new`, no `init` — just declare the class and its fields/ops.
- Template bindings (`::`) set defaults and order.

### Instantiation (Template-Only)
```goblin
fido = Pet: "Fido" :: 3
rex  = Pet: name: "Rex" :: age: 5   /// named bindings
```
- **Only template form is allowed.**
- `Pet("Fido",3)` ❌ not Goblin.
- `Pet.new("Fido",3)` ❌ not Goblin.

### Usage
```goblin
say fido.speak()        /// "Hi, I'm Fido, age 3"
fido.set_age(4)         /// auto-generated setter
say fido.speak()        /// "Hi, I'm Fido, age 4"
```

### Rule
- **Declare → Bind → Operate**
- Classes are defined with fields + ops.
- Objects are instantiated with **template binding only**.
- You then operate on the object variable itself via `.ops`. 

This keeps Goblin consistent: **no function-style calls anywhere.**


#### Access & Privacy
```goblin
/// Public fields (auto-generate accessors)
pet.name            /// getter (paren-optional)
pet.set_name("Rex") /// setter
pet.is_active()     /// boolean getter

/// Private fields (# prefix)
class Book = title: "{title}" :: price: $0
    #inventory = 0   /// private field
    
    op stock() = #inventory  /// expose via operation
end

/// Privacy: `#field` accessible only on `self` inside defining class operations; not on `other`.
```

### Templates

/// `::` is the binding separator used in templates, template-style classes, and inline judge.

Placeholders
- ::          → implicit skip (use default for this slot)
- :: nc       → explicit "no change" (use default, more readable)

Best practice:
- For 1–2 skips: use :: 
- For 3+ consecutive skips: prefer :: nc

@card = name:"{title}" :: price:.99 :: qty:1 :: condition:"new"

"King of Cups"                  /// all defaults
"Queen of Cups" :: 2            /// qty only
"Joker" :: nc :: nc :: "used"   /// explicit “no change”

Soft Keywords (context-dependent)
Add: nc (reserved only in template placeholder context).

Errors

nc outside a placeholder → UnknownNameError (regular identifier).

:: nc anywhere but in template bindings → ParseOnlyError.


#### Basic Templates
```goblin
@card = title: "{title}" :: price: .99 :: qty: 1

// Usage with data
card1 = @card: title: "Ace of Cups"
card2 = @card: title: "Two Cups" :: price: 1.25
card3 = @card: title: "Three Cups" :: :: 2  // skip price, set qty
```

#### Template Blocks
```goblin
@cards =
    card: "{name}" :: price: .99 :: qty: 1
    "Ace of Cups"
    "Two of Cups" :: 1.25            /// override price
    "Three of Cups" :: :: 2          /// skip price, set qty
    "Four of Cups" :: 1.25 :: 2      /// set both
```

#### With Loops
```goblin
suits = ["Cups", "Wands", "Swords", "Pentacles"]

@tarot = card: "{suit} - {rank}" :: price: .99 :: qty: 1

suit: "Cups"
    for rank in ["Ace", "Two", "Three"]
        "{rank}"
    end
    "Ace" :: qty: 2              // override specific cards
end
```

### Enums

#### Definition
```goblin
/// Basic enum
enum Status
    Pending
    Paid
    Shipped
end

/// Int-backed
enum Http as int
    Ok = 200
    NotFound = 404
end

/// Sequential int
enum Priority as int seq
    Low = 1
    Medium      /// auto: 2
    High        /// auto: 3
end

/// String-backed
enum Suit as string
    Clubs = "C"
    Diamonds = "D"
end
```

#### Usage
```goblin
status = Status.Pending
if status is Status.Paid
    ship_order()
end

/// Operations
status.name         /// "Pending"
status.value        /// backing value or name
status.ordinal      /// 0-based index

/// Type operations
Status.values                 /// all variants
Status.from_name("Paid")      /// Status.Paid
Http.from_value(404)          /// Http.NotFound

/// Lookups throw EnumError on failure: Status.from_name(...), Http.from_value(...)
```

## FINANCIAL PROGRAMMING

# Money & Currency (Cheat Sheet replacement)

> Money is a **first-class type**. Exact decimal math, explicit currencies, policy-driven precision/rounding, and a global per-currency remainder ledger.

---

## Literals & Constructors

```goblin
$25.99, €19.50, £22.00, ¥2500      /// symbol literals
25.99 USD, 35.00 CAD, US$30.00     /// codes / prefixed
-$100.00, $-50.00                  /// negatives
money(15.75), money(15.75, EUR)    /// constructor (uses default currency unless specified)
```

---

## Formatting (numbers • money • percent)

Short version: use method style **or** the English form. (Not `format x, "spec"`.)

```goblin
n.format(",.2f")
format n with ",.2f"
"Treasure: {treasure_hoard:,.2f}"
```

**Mini-spec (80/20):**

* `,` → use thousands grouping (actual separator comes from policy)
* `.Nf` → fixed N decimals
* `+` → always show sign
* `%` → percent view (×100 and add `%`)
* `¤` → include currency symbol (money only)
* `CUR` → include ISO code (money only)

Examples

```goblin
12345.6.format(",.2f")         /// "12,345.60"
$1234567.89.format(",.2f")     /// "1,234,567.89"
$1234567.89.format("¤,.2f")    /// "$1,234,567.89"
$1234567.89.format("CUR,.2f")  /// "USD 1,234,567.89"
12.5%.format(".1%")            /// "12.5%"
42.format("+,.0f")             /// "+42"
```

Errors: bad/unknown spec → `ValueError`; using `¤/CUR` on non-money → `TypeError`.

---

## Policy (defaults & knobs)

Set once; applies everywhere (can be overridden by scope).

```goblin
set @policy finance_defaults
  money: {
    currency: "USD",          /// default for money()
    precision: 2,             /// decimals for canonical display/export
    rounding: "bankers",      /// half-even (also: half-up, half-down, floor, ceil)
    policy: "strict",         /// precision-loss handling: truncate | warn | strict | defer
    rounding_timing: "per_tx",      /// when to round: per_tx | per_period | at_settlement
    rounding_period: "month",       /// if per_period: day | week | month | quarter | year
    period_anchor: "calendar",      /// calendar | rolling
    settlement_events: [],          /// if at_settlement: ["export","post","payout", ...]
    max_carry_duration: "P1Y",      /// ISO8601 safety valve for defer/periodic
    thousands: ",", decimal: "."    /// display separators only (does not affect math)
  }
end
```

**Rounding timing (what “when” means):**

* **per\_tx**: round after each money result (POS, invoices, taxes).
* **per\_period**: carry exact inside the period, round once at close (interest, metered billing).
* **at\_settlement**: carry exact until event (export/post/payout), then round (trading/batch).

**Mode interaction:**

* **truncate / warn**: dust goes to **global per-currency ledger** at the chosen timing.
* **strict**: if rounding would be required before timing, throw `MoneyPrecisionError` (use `>>` or change timing/mode).
* **defer**: keep full precision until timing; on round apply sub-mode: **bake-in** | **escrow** | **carry-forward**.

---

## Allowed / Forbidden ops

```goblin
$25.00 + $15.50      /// $40.50 (same currency)
$100.00 - $25.00     /// $75.00
$20.00 * 2           /// $40.00
$100.00 // 3         /// $33.33         (quotient only, policy/timing governs rounding)
$100.00 % 3          /// $0.01          (remainder only)
$100.00 >> 3         /// ($33.33, $0.01) (quotient, remainder)
m += $5.00; m *= 1.1 /// compound ok
price++; price--     /// add/subtract 1 major unit (e.g., $1 for USD)
```

Forbidden (be explicit):

```goblin
$100.00 / 3          /// MoneyDivisionError (use // or >>)
$100.00 + €85.00     /// CurrencyError (convert explicitly)
m /= 2; m %= 3       /// MoneyDivisionError
```

---

## Splitting helpers (fair & conservative)

```goblin
divide_evenly($10.75, 3)        /// [$3.59, $3.58, $3.58]  (deterministic extra cents)
divide_evenly_escrow($100, 7)   /// {shares: [...], escrow: $0.04}
allocate_round_robin($0.07, 5)  /// [$0.02, $0.02, $0.01, $0.01, $0.01]
allocate_money($1000, [50,30,20])  /// [$500, $300, $200]
```

---

## Remainder ledger (global, per currency)

```goblin
clear_remainders()
q, r = $10.75 >> 3             /// (3.58, 0.01)
remainders_total()             /// {USD: $0.01}
remainders_report()            /// breakdown by source
drip_remainders(threshold: $1.00, commit: true)
```

---

## Currency conversion (explicit only)

```goblin
convert($100.00, to: EUR, rate: 0.91)    /// EUR 91.00
convert(€45.00, to: USD, rate: 1.10)     /// USD 49.50
/// Any sub-precision from conversion is handled per policy/timing in the TARGET currency.
```

---

## Percent × Money

```goblin
price = $80.00
price + (10% of price)   /// $88.00
price - (15% of price)   /// $68.00
price.with_tax(8.5%)     /// $86.80
price.tax(8.5%)          /// $6.80
```

---

## Rounding timing: quick contrasts

**Per-transaction (POS):**

```goblin
set @policy pos
  money: { precision: 2, rounding: "bankers", policy: "warn", rounding_timing: "per_tx" }
end

total = $12.49 + $0.99    /// $13.48  (rounded now; any dust logged)
```

**Per-period (daily interest posting):**

```goblin
set @policy daily_compound
  money: { precision: 2, rounding: "bankers",
           policy: "defer", rounding_timing: "per_period",
           rounding_period: "day", period_anchor: "calendar" }
end

minute_rate = apr / 365 / 1440
accrued = $0.00
repeat 1440
  accrued += balance * minute_rate   /// carry exact during the day
end
balance += settle(accrued)           /// round once/day (apply defer sub-mode)
```

**At settlement (export/payout):**

```goblin
set @policy batch
  money: { precision: 2, rounding: "bankers",
           policy: "defer", rounding_timing: "at_settlement",
           settlement_events: ["export"], max_carry_duration: "P90D" }
end

payout = sum(earnings)               /// carry exact
"payout.csv".write_csv({ amount: settle(payout, submode: "escrow") })
```

---

## Common hiccups

* `price++` adds **\$1.00** (major unit), not one cent → use `price += $0.01`.
* `remainders_total()` is **global per currency**, not per operation.
* In **strict**, prefer `>>` (divmod) then decide where the remainder goes.

**Errors**

* `MoneyDivisionError` (used `/` or `/=` on money)
* `CurrencyError` (mixed currencies without explicit `convert`)
* `MoneyPrecisionError` / `MoneyPrecisionWarning` (per policy)
* `ValueError` (bad format spec), `TypeError` (`¤/CUR` on non-money)

---

## Tiny “opt-out” note

If this seems overkill for your use case, you can stick to plain decimals/floats + manual rounding. You’ll lose conservation guarantees and currency safety, but it’s simpler. Goblin’s money type exists for when **auditability, fairness, and cross-system parity** matter.


### Percent System (CIPO)

#### Percent Types
```goblin
/// Percent
25%                /// 0.25 (percent of 1)
25%s               /// percent of self (left operand)
25% of price       /// percent of explicit base
pct 25             /// 25% (constructor from percentage points)
```

#### Three Forms
```goblin
/// % = percent of 1 (programmer style)
8 + 25%     → 8.25              /// 8 + 0.25

/// %s = percent of self (calculator style)  
8 + 25%s    → 10                /// 8 + (0.25 * 8)

/// % of E = explicit base
8 + (25% of 50)  → 20.5         /// 8 + (0.25 * 50)
```

#### Money Integration
```goblin
price = $80
price + 10%s    → $88.00        /// $80 + (10% of $80)
10% of price    → $8.00
price.tax(8.5%) → $6.80         /// helper operation
price.with_tax(8.5%) → $86.80   /// price + tax
```

### 11.x Percent Variables

Percent values can be bound to variables for reuse. This removes magic numbers and ensures consistency.

#### Form

```goblin
rate = 8.5%
final = rate of base
```

* Define once, use everywhere.
* Reads like English (`sales_tax_rate of price`).
* Impossible to confuse `0.08` vs `0.085`.

#### Examples

```goblin
sales_tax_rate = 8.5%

op calculate_tax(price)
    sales_tax_rate of price
end

op monthly_report()
    total_tax = sales_tax_rate of total_sales
end

/// Update rate once:
sales_tax_rate = 9.0%
```

With judge:

```goblin
tax_rate = judge
    user.state == "CA": 10.25%
    user.state == "TX": 6.25%
    user.state == "OR": 0%
    else: 8.5%
end

final_price = base_price + (tax_rate of base_price)
```

Also works with `%s` for scalable rules:

```goblin
discount = judge
    order.total >= $1000: 20%s
    order.total >= $500:  15%s
    else: 0%s
end

final_price = order.total - (discount of order.total)
```

## ADVANCED FEATURES

### Modules & Imports

#### Import/Export
```goblin
/// Import
import "./helpers" as H
import "./utils" as U

/// Usage
slug = H::slugify("Product Name")
result = U::process(data)
```

#### Visibility
```goblin
/// Expose mode (default): everything visible except 'vault'
expose op public_helper() = "available"
vault op private_helper() = "hidden"

/// Vault mode: nothing visible except 'expose'  
expose op only_this_visible() = "available"
op this_is_hidden() = "not available"
```

#### Policy Control
```goblin
/// Set module visibility policy
set @policy open_modules     /// modules: { mode: "expose" }
set @policy locked_modules   /// modules: { mode: "vault" }
```

### Glam Extensions

/// Glam = sandboxed extension package (semver). use/prefer/via control invocation.

#### Usage
```goblin
/// Load glam
use shopify@^1.6 as shp
use tarot_deck@1.2

/// Set preferences
prefer product.export via shp

/// Call capabilities
file = product.export(@cards) via shp
result = shp::csv_export(data)

/// Fanout to all loaded providers that implement the contract:
files = product.export(items) via all
/// returns { provider_key → result|error }  (provider_key = alias if present else glam name)

/// Destinations & naming (no placeholders in code):
/// - Project default directory: goblin.config.yaml → glam.artifacts.dir
/// - Optional file default:  prefer artifacts.dir "/reports"
/// - Optional call override: dir: "/adhoc"
/// - Filenames are always provider-prefixed + date-stamped; no overwrites by default.
/// - Concurrency/timeout/retry live in config (glam.fanout.*); you usually just write `via all`.
```

#### Contracts
```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end
```

### Special Forms

#### Morphing (Temporary Type Adaptation)
```goblin
/// Transform object temporarily using another class's operation
result = morph(book, Card, apply_discount(10%))
/// book keeps its type, but discount operation from Card is applied

/// Rules (quick):
/// - Call shape: result = morph(obj, TargetType, one_operation_call)
/// - One-operation rule: exactly one public instance operation on TargetType
/// - Accessors: only public getters/setters are synced:
///     x()/set_x(v), is_x()/set_x(v) for booleans
///   A field syncs only if BOTH types expose matching accessors
/// - Type safety: no implicit coercions; money requires same currency
/// - Determinism: morph itself is pure aside from field writes; target operation obeys sandbox
/// - Errors: MorphTypeError, MorphFieldError, MorphCurrencyError, MorphActionError
```

#### Gmarks (Stable References)
```goblin
/// Create stable project-local identifiers
post_ref = gmark "post/how-to-play"      /// auto-increment ord
pin_ref = gmark "post/welcome" ord: 1    /// manual position

/// Query
gmarks                    /// all gmarks, sorted by ord
gmark_info "post/welcome" /// details for specific gmark

/// More helpers
next_ord                   /// peek next auto ord (no allocation)
gmarks_filter "post/"      /// only gmarks with that prefix, ord-sorted
gmark_set_ord "post/x", 200 /// move a mark to an unused ord (error if taken)

/// Deterministic builds
/// - Writes to gmark registry are blocked under --deterministic
/// - Allow explicitly in config:
///   goblin.config.yaml:
///     glam:
///       allow_state_writes: ["gmark"]

/// CLI maintenance
# compact ords to 1..N (stable order preserved)
goblin gmark rebalance

See also: CLI `goblin gmark rebalance` to compact ords to 1..N after migrations.
```

#### Policies (Project Configuration)
```goblin
/// Define in policies.gbln
@policy = "site_default"
    money: { currency: "USD", precision: 2, policy: "truncate" }
    datetime: { tz: "America/Denver", prefer_trusted: false, policy: "allow" }
    modules: { mode: "expose" }

/// Apply
set @policy site_default
```

#### Banish (Feature Blocking)
```goblin
/// Project-local feature blocker via .goblin.banish.toml
/// Usage of a banished feature → BanishError at compile time

# Feature IDs (namespaces)
# core.<keyword>, op.<operator>, type.<type>, builtin.<function>, glam.<ns>.<symbol>

# Config (.goblin.banish.toml)
# [[banish]]
# feature = "core.morph"
# reason  = "Temporary safety"

# CLI
# goblin banish <feature_id> --reason "<text>"
# goblin unbanish <feature_id>
# goblin banish --list

# Banner (when any bans exist)
# ⚠ This project has N banished features (run `goblin banish --list`).

# Non-banishable invariants (self-protection): sandbox/determinism, money safety, lockfile integrity
```

### Collections & Utilities

#### Arrays
```goblin
arr = [1, 2, 3]
arr[0]                      /// access
arr[-1]                     /// negative indexing
arr[1:3]                    /// slicing
arr.len                     /// length
add 4 to arr                /// append
insert "X" at 1 into arr    /// insert
reap from arr               /// destructive random remove
```

#### Maps
```goblin
m = {key: "val", price: 1.5}
m.key           /// dot access (identifier keys)
m["any-key"]    /// bracket access (any key)
m.keys          /// ["key", "price"]
m.values        /// ["val", 1.5]
```

#### Math Helpers
```goblin
1 + 2 + 3          /// arithmetic
[1, 2, 3].sum      /// same as above
[1, 5, 3].min      /// minimum
[1, 5, 3].max      /// maximum
roll 2d6+1         /// dice roll
```

## I/O & DATA

### File Operations
```goblin
/// Text
"file.txt".read_text
"file.txt".write_text(content)

/// Structured data
"data.json".read_json
"export.json".write_json(obj)
"config.yaml".read_yaml
"export.csv".write_csv(rows)

/// Binary
"image.png".read_bytes
"file.bin".write_bytes(blob_data)
```

### JSON Handling

#### JSON Options
```goblin
/// Write with options
"data.json".write_json(obj, {
    money: "object",     /// "object" | "string" | "units"
    enum: "name",        /// "name" | "value" | "object"  
    datetime: "string"   /// "string" | "object" (no auto on write)
})

/// Read with options  
"data.json".read_json({
    money: "object",     /// "off" | "object" | "string" | "auto"
    enum: "name",        /// "off" | "name" | "value" | "auto"
    datetime: "auto"     /// "off" | "string" | "object" | "auto"
})

/// Defaults: write_json → money:"string", enum:"name", datetime:"string";
///           read_json  → money:"auto",   enum:"auto", datetime:"off"

/// Note: `auto` in read_json infers format from input; not supported in write_json for explicitness.
```

## ERROR REFERENCE

### Standard Error Catalog

| Error | When it happens | Example message |
|-------|----------------|-----------------|
| **SyntaxError** | Bad tokens/structure | `dots are not allowed in identifiers (did you mean user_name?)` |
| **UnknownNameError** | Name/op not found (after resolution rules) | `unknown op 'flarb' for type string` |
| **ArityError** | Wrong number of args for an op | `replace expected 2 args, got 1` |
| **TypeError** | Wrong type for operand/arg | `split expects (string, string delim); got (string, int)` |
| **ValueError** | Disallowed/invalid value (e.g., empty delimiter) | `split delimiter cannot be empty` |
| **IndexError** | Out-of-range index (e.g., cut at i) | `index 7 out of range (len=5)` |
| **ShadowConflictError** | Field name collides with zero-arg op name on a type | `field 'length' conflicts with zero-arg op on type 'string'` |
| **ParseOnlyError** | Parse-time op used at runtime | `raw can only be applied to string literals` |
| **RegexError** | Invalid regex pattern/flags | `regex error at pos 5: unterminated character class` |
| **MultiArgPrefixChainError** | Multi-arg English op used inside a prefix chain | `prefix chains allow zero-arg ops only` |

#### Split/Join specific guarantees

- `split s on ""` → **ValueError**: delimiter cannot be empty
- `join parts with d` requires `parts` is an array of strings → **TypeError**

#### Strings vs Collections

- `cut` on strings → **TypeError**: strings are immutable; use drop/remove/replace
- `usurp` on strings → **TypeError**: strings are immutable; use replace

#### Dot rules

- `user.name = "Bob"` when user has no field name but does have zero-arg op name → **UnknownNameError** (and a hint to rename)
- Dangling dot: `s.` → **SyntaxError**: expected operation after '.'

### Core Error Types
```
NameError, TypeError, ValueError, IndexError, KeyError, ZeroDivisionError,
SyntaxError, AssertionError, OverflowError
```

### Goblin-Specific Errors
```
CurrencyError, MoneyDivisionError, MoneyPrecisionError,
TimeSourceError, TimeSourceWarning, TimezoneError, TimeArithmeticError,
EnumError, GlamError, ContractError, PermissionError, AmbiguityError, 
LockfileError, DeterminismError, MorphTypeError, MorphFieldError, 
ModuleNotFoundError, PolicyNotFoundError, BanishError, DatetimeCompatWarning,
GLAM_NO_PROVIDER, GLAM_FANOUT_UNSUPPORTED, GLAM_PARTIAL_FAILURE,
GLAM_DEST_INVALID, GLAM_TIMEOUT, GLAM_OVERWRITE_DENIED, GLAM_AMBIGUOUS_PROVIDER,
GmarkConflictError, GmarkNotFoundError, GmarkInvalidError, GmarkPersistenceError,
MorphCurrencyError, MorphActionError,
DeleteThresholdExceeded
```

### List Operation Errors
```
EmptyPickError, PickCountError, PickIndexError, PickTypeError,
ReapEmptyError, ReapCountError, ReapIndexError, ReapTypeError,
UsurpIndexError, UsurpCountError, UsurpArityError, UsurpTypeError,
ReplaceIndexError, ReplaceTypeError,
InsertIndexError, InsertTypeError,
ShuffleTypeError, SortTypeError, AddTypeError
```

### Errors I don't know where they go
```
PickCountError — `!dups` but requested count > available candidates  
PickRangeError — empty candidate set after range/digit/unique constraints  
PickTypeError — invalid source/type (e.g., digit shorthand on non-numeric)
```

## EXAMPLES

### Tiny end-to-end examples

```goblin
// Clean a name
name = "  Greg Jr.  "
name = name.trim_lead.trim_trail.replace("Jr.","Sr.").upper
// "GREG SR."

// Paths
path = "/usr/local/bin"
dir  = before_last "/" in path     // "/usr/local"
file = after_last  "/" in path     // "bin"

// CSV-ish line
raw  = "alpha, beta,  ,gamma"
cells = split raw on ","
cells = cells.map(trim)            // ["alpha","beta","","gamma"]
line  = join cells with "|"

// Collections
team2 = drop "Greg" from team
cut "Greg" from team               // mutate

nums = [1,2,3,4]
total = nums.sum                   // 10

// Prefix chain (zero-arg)
result = trim reverse upper " hello "   // "OLLEH"

// Regex core
emails = find_all like raw "[\\w.]+@[\\w.]+" in text
clean  = replace   like raw "\\s+" with " " in text
```

## LANGUAGE REFERENCE

### Reserved Words

#### Hard Keywords (cannot be shadowed)
if, elif, else, for, in, while, repeat, unless, attempt, rescue, ensure, return, skip, jump, until, stop, assert,
class, op, enum, use, import, export, via, test,
true, false, nil,
judge, morph, vault, banish, unbanish, expose

#### Soft Keywords (context-dependent)
from, at, first, last, to, into, with, dups, seq, as, where, raw, trim_lead, on, like, and

#### Built-ins (shadowable operations/types)
int, float, bool, money, pct, date, time, datetime, duration,
len, shuffle, sort, add, insert, replace, roll, roll_detail, freq, mode, sample_weighted,
set, settle, pick, reap, usurp, unique,
split, join, map, format,
upper, lower, title, slug, trim, trim_lead, trim_trail, escape, unescape, reverse, mixed, minimize, remove,
before, after, before_last, after_last, between, lines, words, chars,
find_all, match, find, has, count, parse_bool,
sum, avg, min, max,
drop, cut, strip_lead, strip_trail

---

*This cheat sheet covers all major syntax and language constructs in Goblin v1.5. For implementation details, examples, and philosophy, see the full specification.*
