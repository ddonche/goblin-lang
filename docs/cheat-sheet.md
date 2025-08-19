# Goblin Language AI Cheat Sheet v1.5.3
*Complete syntax reference for AI assistants - Reorganized for Documentation Flow*

## GETTING STARTED

### Basic Syntax Overview
- One statement per line
- Indentation defines blocks (spaces only, no tabs)
- No semicolons

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

/// Pipeline
x |> f(y)  → f(x, y)              /// left-to-right chaining

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
.upper() .lower() .title() .slug()  /// methods
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

#### Date/Time (No naive timestamps)
```goblin
date("2025-08-12")                          /// calendar day (no time, no zone)
time("14:30:05")                            /// clock time (no date, no zone)  
datetime("2025-08-12 14:30", tz: "UTC")     /// date + time + zone (zone required)
duration(3600)                              /// elapsed seconds constructor

/// Duration literals: s, m (minutes), h, d, w, mo (months), y
1h + 30m          /// 90 minutes
6mo               /// 6 months (NOT 6 minutes - use 'mo' for months)

/// No implicit coercions between date/time/datetime/duration types
/// time ± duration requires explicit wrap_time() or shift_time()

/// Calendar-safe helpers (field changes, clamped)
add_months(d, n), add_years(d, n)           /// Jan 31 + 1mo → Feb 28/29
floor_dt(dt, "day"), ceil_dt(dt, "hour")    /// truncation/rounding

/// Trusted time (opt-in via policy/glam)
trusted_now()                               /// server > cache > local per policy
ensure_time_verified("receipt timestamp")   /// verify time source or error/warn

/// Iterating calendar ranges with stride
for d in date("2025-01-01")..date("2025-01-31") jump 7d
    say "Week starting {d}"
end

for ts in datetime("2025-08-12 08:00", tz:"UTC")...datetime("2025-08-12 12:00", tz:"UTC") jump 30m
    process(ts)
end
```

#### Binary Data
```goblin
blob()            /// empty blob
blob("text")      /// from UTF-8 string
from_base64(s), from_hex(s)  /// from encoded strings
```

#### Dice (only valid inside roll/roll_detail)
```goblin
roll 2d6+1        /// dice literal form
roll(1d20)        /// function form
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

### Functions

#### Definition
```goblin
/// Basic
fn greet(name="Traveler")
    "Hello, {name}"    /// implicit return
end

/// Single-line
fn add(a, b) = a + b

/// With explicit return
fn complex(x)
    if x > 0
        return x * 2
    end
    return 0
end
```

#### Calls
```goblin
greet("Alice")
greet(name: "Bob")     /// named parameter
```

### Object-Oriented Programming

#### Class Definition (Two Styles)

**Template Style:**
```goblin
class Pet = name: "{name}" :: age: 0 :: species: "dog"
    fn speak()
        "Hi, I'm {name}, a {age}-year-old {species}."
    end
end
```

**Function Style:**
```goblin
class Pet(name: string, age: int = 0, species: string = "dog")
    fn speak()
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
pet.name()          /// getter
pet.set_name("Rex") /// setter
pet.is_active()     /// boolean getter

/// Private fields (# prefix)
class Book = title: "{title}" :: price: $0
    #inventory = 0   /// private field
    
    fn stock() = #inventory  /// expose via method
end

/// Privacy: `#field` accessible only on `self` inside defining class methods; not on `other`.
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

/// Methods
status.name()       /// "Pending"
status.value()      /// backing value or name
status.ordinal()    /// 0-based index

/// Type methods
Status.values()                 /// all variants
Status.from_name("Paid")       /// Status.Paid
Http.from_value(404)           /// Http.NotFound

/// Lookups throw EnumError on failure: Status.from_name(...), Http.from_value(...)
```

## FINANCIAL PROGRAMMING

### Money System

#### Money Types & Literals
```goblin
/// Money
$1.50, USD 1.50, €2.00, £3.00, ¥100    /// literals
money(1.50, USD)                         /// constructor
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
pct(25)           /// 25% (constructor from percentage points)
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
tax(price, 8.5%) → $6.80        /// helper function
with_tax(price, 8.5%) → $86.80  /// price + tax
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
expose fn public_helper() = "available"
vault fn private_helper() = "hidden"

/// Vault mode: nothing visible except 'expose'  
expose fn only_this_visible() = "available"
fn this_is_hidden() = "not available"
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
/// Transform object temporarily using another class's method
result = morph(book, Card, apply_discount(10%))
/// book keeps its type, but discount method from Card is applied

/// Rules (quick):
/// - Call shape: result = morph(obj, TargetType, one_method_call)
/// - One-method rule: exactly one public instance method on TargetType
/// - Accessors: only public getters/setters are synced:
///     x()/set_x(v), is_x()/set_x(v) for booleans
///   A field syncs only if BOTH types expose matching accessors
/// - Type safety: no implicit coercions; money requires same currency
/// - Determinism: morph itself is pure aside from field writes; target method obeys sandbox
/// - Errors: MorphTypeError, MorphFieldError, MorphCurrencyError, MorphActionError
```

#### Gmarks (Stable References)
```goblin
/// Create stable project-local identifiers
post_ref = gmark("post/how-to-play")      /// auto-increment ord
pin_ref = gmark("post/welcome", ord: 1)   /// manual position

/// Query
gmarks()                    /// all gmarks, sorted by ord
gmark_info("post/welcome")  /// details for specific gmark

/// More helpers
next_ord()                   /// peek next auto ord (no allocation)
gmarks_filter("post/")       /// only gmarks with that prefix, ord-sorted
gmark_set_ord("post/x", 200) /// move a mark to an unused ord (error if taken)

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
len(arr)                    /// length
add 4 to arr                /// append
insert "X" at 1 into arr    /// insert
reap from arr               /// destructive random remove
```

#### Maps
```goblin
m = {key: "val", price: 1.5}
m.key           /// dot access (identifier keys)
m["any-key"]    /// bracket access (any key)
m.keys()        /// ["key", "price"]
m.values()      /// ["val", 1.5]
```

#### List Operations
```goblin
/// Non-destructive
pick names              /// random element
pick 3 from names       /// 3 random elements
shuffle names           /// shuffled copy
sort names              /// sorted copy

/// Destructive (mutate original)
reap from names         /// remove & return random
reap 2 from names       /// remove & return 2 random
usurp at 1 in names with "New"  /// replace & return (old, new)
add "Item" to names     /// append
insert "Item" at 2 into names   /// insert at position
```

#### Math Helpers
```goblin
add(1, 2, 3)           /// sum
sum([1, 2, 3])         /// same as add
min(1, 5, 3)           /// minimum
max([1, 5, 3])         /// maximum
roll 2d6+1             /// dice roll
```

## I/O & DATA

### File Operations
```goblin
/// Text
read_text("file.txt")
write_text("file.txt", content)

/// Structured data
read_json("data.json")
write_json("data.json", obj)
read_yaml("config.yaml")
write_csv("export.csv", rows)

/// Binary
read_bytes("image.png")
write_bytes("file.bin", blob_data)
```

### JSON Handling

#### JSON Options
```goblin
/// Write with options
write_json("data.json", obj, {
    money: "object",     /// "object" | "string" | "units"
    enum: "name",        /// "name" | "value" | "object"  
    datetime: "string"   /// "string" | "object" (no auto on write)
})

/// Read with options  
read_json("data.json", {
    money: "object",     /// "off" | "object" | "string" | "auto"
    enum: "name",        /// "off" | "name" | "value" | "auto"
    datetime: "auto"     /// "off" | "string" | "object" | "auto"
})

/// Defaults: write_json → money:"string", enum:"name", datetime:"string";
///           read_json  → money:"auto",   enum:"auto", datetime:"off"

/// Note: `auto` in read_json infers format from input; not supported in write_json for explicitness.
```

## ERROR REFERENCE

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

## LANGUAGE REFERENCE

### Reserved Words

#### Hard Keywords (cannot be shadowed)
```
if, elif, else, for, in, while, unless, attempt, rescue, ensure, return, skip, jump, stop, assert,
class, fn, enum, use, import, export, via, test, true, false, nil, int, float, bool, money, pct,
date, time, datetime, duration, morph, vault, judge, banish, unbanish, expose, set, settle, 
pick, reap, usurp, len, shuffle, sort, add, insert, replace, roll, freq, mode, sample_weighted
```

#### Soft Keywords (context-dependent)
```
from, at, first, last, to, into, with, dups, seq, as
```

/// `in` is hard for loops/list-DSL; no general boolean `in` operator.

---

*This cheat sheet covers all major syntax and language constructs in Goblin v1.5. For implementation details, examples, and philosophy, see the full specification.*
