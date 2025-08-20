# Goblin Language AI Cheat Sheet v1.5.4
*Complete syntax reference for AI assistants - Reorganized for Documentation Flow*

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
* `find_all` is **non-overlapping** (e.g., "banana" + "ana" → \[1]).

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

#### Literals: raw & trim\_lead at parse time

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


**Common reductions (method-style mirrors; paren-optional)**
```goblin
nums.sum   nums.avg   nums.min   nums.max   nums.len

/// examples
total = nums.sum        /// 10
count = nums.len
```

#### Date/Time (No naive timestamps)
```goblin
date "2025-08-12"                           /// calendar day (no time, no zone)
time "14:30:05"                             /// clock time (no date, no zone)  
datetime "2025-08-12 14:30" tz:"UTC"        /// date + time + zone (zone required)
duration 3600                               /// elapsed seconds constructor

/// Duration literals: s, m (minutes), h, d, w, mo (months), y
1h + 30m          /// 90 minutes
6mo               /// 6 months (NOT 6 minutes - use 'mo' for months)

/// No implicit coercions between date/time/datetime/duration types
/// time ± duration requires explicit wrap_time() or shift_time()

/// Calendar-safe helpers (field changes, clamped)
d.add_months(n), d.add_years(n)             /// Jan 31 + 1mo → Feb 28/29
dt.floor("day"), dt.ceil("hour")            /// truncation/rounding

/// Trusted time (opt-in via policy/glam)
time.trusted_now()                          /// server > cache > local per policy
time.ensure_verified("receipt timestamp")   /// verify time source or error/warn

/// Iterating calendar ranges with stride
for d in date "2025-01-01"..date "2025-01-31" jump 7d
    say "Week starting {d}"
end

for ts in datetime "2025-08-12 08:00" tz:"UTC"...datetime "2025-08-12 12:00" tz:"UTC" jump 30m
    process(ts)
end
```

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

Human‑friendly range comparisons. Equivalent to x >= a and x <= b (or the inverse).

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
4. After the last `until …`, stride = `1` by default, unless overridden with a final `jump`.

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

#### Class Definition (Two Styles)

**Template Style:**
```goblin
class Pet = name: "{name}" :: age: 0 :: species: "dog"
    op speak()
        "Hi, I'm {name}, a {age}-year-old {species}."
    end
end
```

**Function Style:**
```goblin
class Pet(name: string, age: int = 0, species: string = "dog")
    op speak()
        "Hi, I'm {name}, a {age}-year-old {species}."
    end
end
```

#### Instantiation
```goblin
/// Template style
pet = Pet: "Fido" :: 3 :: "cat"

/// Function style  
pet = Pet("Fido", 3, "cat")
pet = Pet("Rex", species: "wolf")    /// named args
pet = Pet("Max", :: "cat")           /// skip middle arg
```

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

### Money System

#### Money Types & Literals
```goblin
/// Money
$1.50, USD 1.50, €2.00, £3.00, ¥100    /// literals
```

#### Precision & Policy
```goblin
/// Money follows active policy for precision/rounding
set @policy strict_money    /// precision: 2, policy: "strict"

price = $10.75
q, r = price >> 3           /// (USD 3.00, USD 1.75)
shares = divide_evenly(price, 3)  /// [USD 3.59, USD 3.58, USD 3.58]

/// Remainder tracking
remainders_total()          /// current remainder ledger
drip_remainders(threshold: $0.05, commit: true)  /// payout accumulated
```

#### Currency Rules
```goblin
/// Same currency operations allowed
$10 + $5    → $15
$10 * 2     → $20
$10 // 3    → $3 (quotient only)
$10 >> 3    → ($3, $1)  /// quotient + remainder

/// Forbidden
$10 / 3     → MoneyDivisionError  /// use // or >> instead
$10 + €5    → CurrencyError       /// no auto-conversion
```

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
len, shuffle, sort, add, insert, replace, roll, freq, mode, sample_weighted,
set, settle, pick, reap, usurp,
split, join, map,
upper, lower, title, slug, trim, trim_lead, trim_trail, escape, unescape, reverse,
mixed, minimize, remove,
before, after, before_last, after_last, between, lines, words, chars,
find_all, match, find, has, count,
sum, avg, min, max,
drop, cut,
strip_lead, strip_trail


---

*This cheat sheet covers all major syntax and language constructs in Goblin v1.5. For implementation details, examples, and philosophy, see the full specification.*
