```goblin
add(1, 2, 3)           /// sum
sum([1, 2, 3])         /// same as add
min(1, 5, 3)           /// minimum
max([1, 5, 3])         /// maximum
roll 2d6+1             /// dice roll# Goblin Language AI Cheat Sheet v1.5
*Complete syntax reference for AI assistants*

## 1. Basic Syntax

### Comments
```goblin
/// Single-line comment

//// 
Block comment content
////
```

### Statements
- One statement per line
- Indentation defines blocks (spaces only, no tabs)
- No semicolons

### Variables
```goblin
name = "Frank"
age = 45
price = $1.50
```

## 2. Operators & Precedence (high → low)

```
()                                  /// grouping
.  ()  []  ?.                      /// member/call/index/optional chaining
**  //  ++  --                     /// postfix (square, sqrt, increment/decrement)
**  ^^                             /// power (right-associative)
unary + - not !
%  %s  % of E                      /// percent literals and self-reference
* / % // >>                        /// multiplicative + quotient/divmod
+ -                                /// additive
|>                                 /// pipeline (left-associative)
| then ||                          /// string joins
== != < <= > >= is is not  /// comparisons (chainable)
and or                             /// logical (aliases: && for and, ! for not)
```

### Key Operators
```goblin
/// Postfix Math
9**        → 81                    /// square
16//       → 4                     /// square root (postfix)
x++        → old x, then x = x + 1 /// increment (returns old value)
x--        → old x, then x = x - 1 /// decrement

/// Division & Divmod  
10 / 3     → 3.333...              /// float division
10 // 3    → 3                     /// quotient only (infix)
10 >> 3    → (3, 1)               /// divmod pair (quotient, remainder)

/// String Joins
"a" | "b"  → "ab"                 /// no-space join
"a" || "b" → "a b"                /// space join

/// Pipeline
x |> f(y)  → f(x, y)              /// left-to-right chaining

/// Optional Chaining
obj?.prop  → prop if obj exists, nil if obj is nil/undefined
obj?.method(args) → call if obj exists, nil otherwise
```

## 3. Types & Literals

### Core Types
```goblin
/// Primitives
42                  /// int
3.14               /// float
true, false        /// bool
"hello"            /// string
nil                /// null/undefined

/// Money
$1.50, USD 1.50, €2.00, £3.00, ¥100    /// literals
money(1.50, USD)                         /// constructor

/// Percent
25%                /// 0.25 (percent of 1)
25%s               /// percent of self (left operand)
25% of price       /// percent of explicit base
pct(25)           /// 25% (constructor from percentage points)

/// Collections
[1, 2, 3]         /// array
{key: "val", price: 1.5}  /// map

/// Ranges
1..5              /// inclusive (1,2,3,4,5)
1...5             /// exclusive (1,2,3,4)

/// Date/Time
date("2025-08-12")
time("14:30:05")
datetime("2025-08-12 14:30", tz: "UTC")
1h + 30m          /// duration literals: s, m, h, d, w, mo, y

/// Dice (only valid inside roll/roll_detail)
roll 2d6+1        /// dice literal form
roll(1d20)        /// function form

/// Binary Data
blob()            /// empty blob
blob("text")      /// from UTF-8 string
from_base64(s), from_hex(s)  /// from encoded strings
```

### String Features
```goblin
"Hello {name}"    // interpolation
.upper() .lower() .title() .slug()  // methods
```

## 4. Control Flow

### Conditionals
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

### Loops
```goblin
/// For loops
for i in 1..5        /// ranges
for v in array       /// arrays
for i, v in array    /// with index
for k, v in map      /// maps

/// While
while cond
    ...
end

/// Control
skip    /// continue
stop    /// break
```

### Error Handling
```goblin
/// New structured form
attempt
    risky_operation()
    "success"
rescue ErrorType as e
    handle_error(e)
    "recovered"
ensure
    cleanup()
end

/// Legacy (still supported)
try
    risky_operation()
catch e
    handle_error(e)
finally
    cleanup()
end

/// Manual
error "message"
assert cond, "msg"
warn "message"
```

## 5. Functions

### Definition
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

### Calls
```goblin
greet("Alice")
greet(name: "Bob")     /// named parameter
```

## 6. Classes

### Definition (Two Styles)

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

### Instantiation
```goblin
/// Template style
pet = Pet: "Fido" :: 3 :: "cat"

/// Function style  
pet = Pet("Fido", 3, "cat")
pet = Pet("Rex", species: "wolf")    /// named args
pet = Pet("Max", :: "cat")           /// skip middle arg
```

### Access & Privacy
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
```

## 7. Templates

*Templates use `::` to separate fields in key-value records.*

### Basic Templates
```goblin
@card = title: "{title}" :: price: .99 :: qty: 1

// Usage with data
card1 = @card: title: "Ace of Cups"
card2 = @card: title: "Two Cups" :: price: 1.25
card3 = @card: title: "Three Cups" :: :: 2  // skip price, set qty
```

### Template Blocks
```goblin
@cards =
    card: "{name}" :: price: .99 :: qty: 1
    "Ace of Cups"
    "Two of Cups" :: 1.25            /// override price
    "Three of Cups" :: :: 2          /// skip price, set qty
    "Four of Cups" :: 1.25 :: 2      /// set both
```

### With Loops
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

## 8. Enums

### Definition
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

### Usage
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
```

## 9. Modules

### Import/Export
```goblin
/// Import
import "./helpers" as H
import "./utils" as U

/// Usage
slug = H::slugify("Product Name")
result = U::process(data)
```

### Visibility
```goblin
/// Expose mode (default): everything visible except 'vault'
expose fn public_helper() = "available"
vault fn private_helper() = "hidden"

/// Vault mode: nothing visible except 'expose'  
expose fn only_this_visible() = "available"
fn this_is_hidden() = "not available"
```

### Policy Control
```goblin
/// Set module visibility policy
set @policy open_modules     /// modules: { mode: "expose" }
set @policy locked_modules   /// modules: { mode: "vault" }
```

## 10. Glam (Extensions)

*Glam are Goblin's first-class extension system - modular capabilities that feel native to the language.*

### Usage
```goblin
/// Load glam
use shopify@^1.6 as shp
use tarot_deck@1.2

/// Set preferences
prefer product.export via shp

/// Call capabilities
file = product.export(@cards) via shp
result = shp::csv_export(data)
```

### Contracts
```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end
```

## 11. Money System

### Precision & Policy
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

### Currency Rules
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

## 12. Percent System (CIPO)

### Three Forms
```goblin
/// % = percent of 1 (programmer style)
8 + 25%     → 8.25              /// 8 + 0.25

/// %s = percent of self (calculator style)  
8 + 25%s    → 10                /// 8 + (0.25 * 8)

/// % of E = explicit base
8 + (25% of 50)  → 20.5         /// 8 + (0.25 * 50)
```

### Money Integration
```goblin
price = $80
price + 10%s    → $88.00        /// $80 + (10% of $80)
10% of price    → $8.00
tax(price, 8.5%) → $6.80        /// helper function
with_tax(price, 8.5%) → $86.80  /// price + tax
```

## 13. Collections & Utilities

### Arrays
```goblin
arr = [1, 2, 3]
arr[0]          /// access
arr[-1]         /// negative indexing
arr[1:3]        /// slicing
len(arr)        /// length
arr.push(4)     /// mutate
```

### Maps
```goblin
m = {key: "val", price: 1.5}
m.key           /// dot access (identifier keys)
m["any-key"]    /// bracket access (any key)
m.keys()        /// ["key", "price"]
m.values()      /// ["val", 1.5]
```

### List Operations
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

### Math Helpers
```goblin
add(1, 2, 3)           /// sum
sum([1, 2, 3])         /// same as add
min(1, 5, 3)           /// minimum
max([1, 5, 3])         /// maximum
roll 2d6+1             /// dice roll
```

## 14. I/O & Serialization

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

### JSON Options
```goblin
/// Write with options
write_json("data.json", obj, {
    money: "object",     /// "object" | "string" | "units"
    enum: "name",        /// "name" | "value" | "object"  
    datetime: "string"   /// "string" | "object"
})

/// Read with options  
read_json("data.json", {
    money: "object",     /// "off" | "object" | "string" | "auto"
    enum: "name"         /// "off" | "name" | "value" | "auto"
})
```

## 15. Error Types

### Core Errors
```
NameError, TypeError, ValueError, IndexError, KeyError, ZeroDivisionError,
SyntaxError, AssertionError, OverflowError
```

### Goblin-Specific Errors
```
CurrencyError, MoneyDivisionError, MoneyPrecisionError,
TimeSourceError, TimezoneError, EnumError, GlamError, ContractError,
PermissionError, AmbiguityError, LockfileError, DeterminismError,
MorphTypeError, MorphFieldError, ModuleNotFoundError, PolicyNotFoundError,
BanishError
```

### List Operation Errors
```
EmptyPickError, PickCountError, ReapEmptyError, UsurpIndexError,
ShuffleTypeError, SortTypeError, AddTypeError
```

## 16. Reserved Words

**Hard Keywords (cannot be shadowed):**
```
if, elif, else, for, in, while, unless, attempt, rescue, ensure, return, skip, stop, assert,
class, fn, enum, use, import, export, via, test, true, false, nil, int, float, bool, money, pct,
morph, vault, judge, banish, unbanish, expose, set, settle, pick, reap, usurp, len, shuffle, sort, 
add, insert, replace, roll, freq, mode, sample_weighted
```

**Soft Keywords (context-dependent):**
```
from, at, first, last, to, into, with, dups, seq, as
```

## 17. Special Forms

### Morphing (Temporary Type Adaptation)
```goblin
/// Transform object temporarily using another class's method
result = morph(book, Card, apply_discount(10%))
/// book keeps its type, but discount method from Card is applied
```

### Gmarks (Stable References)
```goblin
/// Create stable project-local identifiers
post_ref = gmark("post/how-to-play")      /// auto-increment ord
pin_ref = gmark("post/welcome", ord: 1)   /// manual position

/// Query
gmarks()                    /// all gmarks, sorted by ord
gmark_info("post/welcome")  /// details for specific gmark
```

### Policies (Project Configuration)
```goblin
/// Define in policies.gbln
@policy = "site_default"
    money: { currency: "USD", precision: 2, policy: "truncate" }
    modules: { mode: "expose" }

/// Apply
set @policy site_default
```

### Banish (Feature Blocking)
```goblin
/// Block language features project-wide via .goblin.banish.toml
/// Usage of banished features → BanishError at compile time
```

---

*This cheat sheet covers all major syntax and language constructs in Goblin v1.5. For implementation details, examples, and philosophy, see the full specification.*
