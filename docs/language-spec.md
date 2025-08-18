# Goblin — Language Specification v1.5 "Treasure Hoarder"

## Philosophy

**Human-readable**: One statement per line; indentation defines blocks.

**Safe by default**: Undefined variables are errors (with "Did you mean…?" suggestions). No silent string coercion.

**Practical batteries**: CSV/YAML I/O, FS helpers, money with currency, friendly math helpers.

**File extension**: `.gbln`

# 0. Keywords and Prelude

## Hard Keywords (cannot be shadowed)

    if, elif, else, for, in, while, unless, attempt, rescue, ensure, return, skip, stop, assert,
    class, fn, enum, use, import, export, via, test, true, false, nil, int, float, bool, money, pct,
    morph, vault, judge, banish, unbanish, expose, set, settle, pick, reap, usurp, len, shuffle, sort,
    add, insert, replace, roll, freq, mode, sample_weighted

## Soft Keywords (context-dependent)

    from, at, first, last, to, into, with, dups, seq, as

## Notes

- `in` is hard for loops and list-DSL; no general boolean `in` operator.
- File extension: `.gbln`
- One statement per line; indentation defines blocks.

## Prelude (global, shadowable)

    pick, reap, usurp, shuffle, sort, add, insert, replace, len, roll, roll_detail, freq, mode,
    sample_weighted, min, max, abs, floor, ceil, round, pow, sum, error, warn

- Prelude helpers are injected globally and shadowable. Originals are reachable via `std.*`.

## Dice Literals

- Valid inside `roll` and `roll_detail`.
- Forms: `roll 2d6+1`, `roll(1d20)`, `roll_detail 4d8-2`.
- Example:

    roll 2d6+1        /// dice roll
    roll(1d20)        /// function form
    r = roll_detail 4d8-2   /// r.dice, r.sum, r.total
# 1. Files, Printing, Variables

## 1.1 Files & Layout

- Source files are UTF-8, with LF newlines.
- Indentation uses **spaces only** (no tabs).
- One statement per line; blank lines are allowed.

**Comments**

    /// Single-line comment

    //// 
    Block comment content
    ////

- Block comment markers must appear alone on their line (no trailing text).

## 1.2 Printing

    say expr

**Leading-quote shorthand:** Any line beginning with a quoted string is equivalent to `say`:

    "Hello"    ≡    say "Hello"

## 1.3 Variables

    name = "Frank"
    age = 45
    price = $1.50   /// money literal

- Using a variable before assignment → `NameError` (with "Did you mean …?" suggestion).
- Empty assignment (`x =`) → `SyntaxError`.
- Types are inferred unless cast.

**Core Types**

- **Primitives:** `int`, `float`, `bool`, `string`, `nil`
- **Extended:** `money`, `percent`, `date`, `time`, `datetime`, `duration`, `blob`, `enum`, `array`, `map`

**Notes**

- Binary data uses the `blob` type for raw bytes, encoded strings, or files.
- Enums are user-defined closed sets (see §Enums).

## 1.4 Casting & Formatting

    /// Casts
    str(x), int(x), float(x), bool(x), money(x, CUR?)

    /// Percent construction
    pct(25)          /// 25% as a type-safe percent

    /// Binary data
    blob("text")     /// UTF-8 conversion (strict)
    from_base64(s), from_hex(s)  /// decode into blob

    /// Parse helpers
    int("$1,234")    → 1234
    float("$1,234.50") → 1234.5

    /// Numeric formatting
    fmt(x, ".2f"), fmt(x, ","), fmt(x, ",.2f")

## 1.5 Strings

    /// Interpolation
    "Hello {name}"

    /// Methods
    .upper() .lower() .title() .slug()

    /// Concatenation
    "a" | "b"   → "ab"    /// no-space join
    "a" || "b"  → "a b"   /// space join

- Mixed types must be cast (e.g., `str(5) || str(10)`).
- `.` is for **member access, method calls, indexing, and optional chaining** (`?.`).
- Methods may be called directly on literals, e.g., `"hi".upper()`.

# 2. Operators & Math

## 2.1 Precedence (high → low)

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

**Notes**
- Postfix ops bind tighter than any binary operator, only to the nearest primary.
- `//` is postfix sqrt when trailing, but infix quotient when between operands.
- `>>` (divmod) is grouped with multiplicative ops.
- `?.` (optional chaining) is at the same tier as member/call/index.
- `|>` (pipeline) sits between arithmetic and string joins.
- No inline `?:` operator in Goblin (use `judge`).
- Time arithmetic is explicit: `time ± duration` → `TimeArithmeticError`. Use `wrap_time()` or `shift_time()`, or set a project policy to desugar.

**Examples**

```goblin
16// ** 2    → (sqrt(16)) ** 2 → 16
a + b** * 3  → a + (b**) * 3
9 // 2**     → SyntaxError   /// postfix must end expression
```

## 2.2 Division & Divmod

    10 / 3    → 3.333...              /// float division
    10 // 3   → 3                     /// integer quotient (floor)
    10 >> 3   → 3 r 1                 /// REPL pretty-print
    (10 >> 3).tuple → (3, 1)          /// canonical tuple view
    (10 >> 3).q     → 3               /// quotient
    (10 >> 3).r     → 1               /// remainder

**Money**

    price = $10.75
    q, r = price >> 3   /// (USD 3.00, USD 1.75)
    q = price // 3      /// USD 3.00
    price / 3           /// MoneyDivisionError

**Remainder**

- `%` is numeric remainder (modulus).
- For money: `amount % n` = same as the second element of `amount >> n`.

## 2.3 Exponentiation

    5 ** 3   → 125        /// normal power
    5 ^^ 3   → 125        /// explain-power (same precedence/assoc)
    say 5 ^^ 3 → 125 (5 × 5 × 5)

- `^` is reserved (future XOR).

## 2.4 Postfix Math Shorthands

    9**      → 81        /// square
    16//     → 4         /// square root
    x++      → old x, then x = x + 1
    x--      → old x, then x = x - 1

**Rules**

- `n**`, `n//`: numeric only (int/float). Money not allowed → `TypeError`.
- `x++`, `x--`: valid on int/float/money; money changes by one major unit.
- Lvalues only (e.g. `arr[i]++` OK). Not allowed on literals/temps.
- No chaining: `(x++)++` → `SyntaxError`.
- No prefix forms (`++x`, `--x`) in v1.5.

## 2.5 Bitwise

Bitwise operators are removed to avoid conflict with string joins. Use functions:

    bit.and(a,b), bit.or(a,b), bit.xor(a,b), bit.not(x)
    bit.shl(x,n), bit.shr(x,n)

## 2.6 Built-in Math Functions

All accept both variadic args and arrays:

    add(1,2,3)    → 6
    add([1,2,3])  → 6
    sum([1,2,3])  → 6      /// synonym for add
    min(1,5,3)    → 1
    max([1,5,3])  → 5

Random/dice support:

    roll 2d6+1
    rand(), randint(1,10)

## 2.7 Increments & Compound Assignment

    x++  /// postfix increment (yield old value, then mutate)
    x--  /// postfix decrement

    /// Compound assignment
    +=  -=  *=  **=

**Money restrictions**

- `+=`, `-=`, `*=` allowed, subject to currency rules and precision policy.
- `/=` and `%=` on money → `MoneyDivisionError`.

# 3. Comparisons, Booleans, Truthiness

```goblin
/// Equality
==    /// value equality (numeric 3 == 3.0 is true; containers compare structurally)
!=    /// negation of ==

/// Strict identity
===   /// type AND value must match (including enums, datetime, money, etc.)
!==   /// negation of ===

/// Enum/type checks
is / is not   /// same-variant checks for enums and type predicates

/// Chaining
if 1 < x < 10    /// allowed
```

**Datetime & Money Equality Notes**
- `==` compares **value in canonical form**: `datetime("2025-01-01T00:00Z") == datetime("2024-12-31T19:00-05:00")` → true Money in the same currency with equal amount is also true.
- `===` requires same type, zone, and representation: The above datetimes are **not** strictly identical; `$1.00 USD === 1.00` is false.

## 3.1 Boolean Logic

```goblin
and, or        /// logical ops
&&             /// alias for and
!              /// alias for not
not            /// keyword form
```

## 3.2 Truthiness

**Falsey values**
- `false`
- `0`, `0.0`
- `""` (empty string)
- `[]` (empty array)
- `{}` (empty map)
- `nil`

**Notes**
- Accessing an undefined variable → `NameError`, not a falsey.
- Comparisons are chainable (`a < b < c`).
- Datetime and money values are always truthy (even zero durations).

# 4. Conditionals

Goblin supports four conditional keywords:
- `if` — run the block if the condition is **true**
- `unless` — run the block if the condition is **false** (sugar for `if not`)
- `elif` — test a new condition if all previous branches failed
- `else` — run the block if no previous branch executed

Datetime, money, and duration comparisons may be used in conditions directly (`if dt1 < dt2`, `if price >= $100`).

## 4.1 Block Form — Classic and Readable

```goblin
if cond
    ...
elif cond
    ...
else
    ...
end

unless cond
    ...
else
    ...
end
```

- No colons; indentation defines blocks.
- `elif` and `else` are optional.
- `unless cond` compiles as `if not cond`.
- You may mix `if`/`unless` with `elif`/`else`.
- Block form is a **statement** (does not yield a value); use `say` or assignment inside.

**Examples**

```goblin
if score >= 90
    say "A"
elif score >= 80
    say "B"
elif score >= 70
    say "C"
else
    say "F"
end

unless user.is_admin
    deny_access()
else
    grant_admin_access()
end
```

## 4.2 Judge — Declarative, Expression-Oriented Branching

`judge` is an **expression form** that always yields a value. Two styles exist: multiline and inline.

### 4.2.1 Multiline

```goblin
result = judge
    cond1: expr1
    cond2: expr2
    else: exprN
end
```

- Conditions are tested top-to-bottom; first match wins.
- `else:` is optional (default = `nil`).
- Each `expr` can be any expression (string, call, money, datetime, etc.).

**Example**

```goblin
grade = judge
    score >= 90: "A"
    score >= 80: "B"
    score >= 70: "C"
    else: "F"
end
```

### 4.2.2 Inline

```goblin
judge: cond1: expr1 :: cond2: expr2 :: else: exprN
```

**Example**

```goblin
say judge: score >= 90: "A" :: score >= 80: "B" :: else: "F"
```

- Inline `judge` must be assigned or wrapped in `say`; otherwise it is a no-op.

### 4.2.3 Unless Inside Judge

`unless` works inside judge for negative logic:

```goblin
status = judge
    unless user.is_verified: "verify_email"
    unless user.has_payment: "add_payment"
    user.is_premium: "premium_flow"
    else: "standard_flow"
end
```

## 4.3 Interop Notes

- `judge` can appear anywhere an expression can.
- Expressions inside branches follow all normal type rules.
- Short-circuit: evaluation stops at the first match.

**Example**

```goblin
label = "Tier " || judge: price >= $100: "Gold" :: price >= $50: "Silver" :: else: "Bronze"
```

## 4.4 Quick Cheat-Sheet

- **Block:** `if cond ... elif cond ... else ...`
- **Unless:** `unless cond ... else ...`
- **Judge multiline:**

```goblin
x = judge
    c1: v1
    c2: v2
    else: v3
end
```

- **Judge inline:** `say judge: c1: v1 :: c2: v2 :: else: v3`
  
# 5. Loops

## 5.1 Ranges

```goblin
/// Numbers
1..5    /// inclusive → 1,2,3,4,5
1...5   /// exclusive → 1,2,3,4
5..1    /// reversed (step = -1)
```

**Dates**

```goblin
for d in date("2025-08-01")..date("2025-08-03")
    say d
end
/// → 2025-08-01, 2025-08-02, 2025-08-03
```

- Always **inclusive** (`..`)
- Default step = `1d`
- Step must be whole-day durations (`1d`, `2d`, …)

**Datetimes**

```goblin
for ts in dt_start...dt_end
    process(ts)
end

for ts in dt_start...dt_end step 30m
    tick(ts)
end
```

- Always **exclusive** (`...`)
- Default step = `1h`
- Step may be any duration (`m/h/d/...`)

**Notes**
- `time ± duration` is not allowed directly in range heads — wrap/shift first:

```goblin
for t in wrap_time(time("23:30"), 1h)...wrap_time(time("02:30"), 1h)
    ...
end
```

## 5.2 Loop Forms

```goblin
for i in 1..5          /// numeric range
for v in array         /// iterate array values
for i, v in array      /// array with index
for k, v in map        /// map keys and values

while cond
    ...
end
```

## 5.3 Loop Control

```goblin
skip    /// continue to next iteration
stop    /// break out of loop
```

- Using `skip` or `stop` outside a loop → `SyntaxError`.
- Map iteration order is insertion-stable.

# 6. Collections

## 6.1 Arrays

    /// Literals
    [1, 2, 3]

    /// Indexing (supports negative indexes)
    arr[0]      /// first element
    arr[-1]     /// last element

    /// Slicing
    arr[1:3]    /// subarray

    /// Length
    len(arr)    /// array length

**Mutators**

    add 4 to arr                 /// append
    insert "X" at 1 into arr     /// insert at index
    reap from arr                /// destructive random remove
    reap 2 from arr              /// remove & return 2 random elements
    usurp at 1 in arr with "New" /// replace element, return (old,new)

**Helpers**

    pick arr             /// random element (non-destructive)
    pick 3 from arr      /// 3 random elements
    shuffle arr          /// shuffled copy
    sort arr             /// sorted copy
    sum(arr)             /// numeric sum

## 6.2 Maps

    /// Literals
    {key: "val", price: 1.5}

    /// Access
    m.key         /// dot-access (identifier keys only)
    m["any-key"]  /// bracket-access (any key)

    /// Methods
    m.keys()      /// list of keys
    m.values()    /// list of values

# 7. Functions

## 7.1 Definition

    /// Basic
    fn greet(name="Traveler")
        "Hello, {name}"    /// implicit return (last expression)
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

- `return` is allowed; otherwise the last expression is returned.
- Defaults are supported. Arity is strict, but defaults satisfy arity.

## 7.2 Calls

    greet("Alice")
    greet(name: "Bob")     /// named parameter

## 7.3 Module Function Calls

    import "./helpers" as H
    slug = H::slugify("Product Name")

- `::` is the namespace separator.
- Works with modules loaded by `import`.

## 7.4 Glam Function Calls

    /// Load glams
    use shopify@^1.6 as shp
    use translator@1.0 as trans

    /// Direct glam calls
    csv = shp::csv_export(products)
    spanish = trans::translate_spanish("Hello world")

    /// Fanout across multiple glams
    files = product.export(items) via all

**Notes**

- Explicit aliases (`as shp`, `as trans`) prevent collisions.
- `prefer` / `via` can route calls when multiple glams implement the same contract.
- Fanout returns `{ provider_key → result|error }`.

# 8. Key-Value Binding (`::`) and Templates

## 8.1 Inline KV with `::`

    card: "Two of Cups" :: price: .99 :: qty: 4

- `::` is the binding separator.
- Left-hand side must be identifiers.
- Right-hand side may be any expression (strings, numbers, maps, arrays, etc.).

## 8.2 Templates

Templates define reusable records with defaults. `::` applies values in order, with optional key overrides.

### 8.2.1 Positional with Defaults

    /// Define template
    @card = title: "{title}" :: price: .99 :: qty: 1

    /// Usage
    card1 = @card: title: "Ace of Cups"
    card2 = @card: title: "Two of Cups" :: price: 1.25
    card3 = @card: title: "Three of Cups" :: :: 2   /// skip price, set qty

**Rules**

- Values fill in positional order.
- Trailing fields may be omitted → defaults apply.
- To skip a field, leave its slot empty with `::`.
- Keyed overrides are always allowed (`price: 1.25`).
- Fields can contain interpolated strings (`"{title}"`).

### 8.2.2 Arrays, Loops, and Overrides

    suits = ["Cups", "Wands", "Swords", "Pentacles"]

    @tarot = card: "{suit} – {rank}" :: price: .99 :: qty: 1

    suit: "Cups"
        for rank in ["Ace", "Two", "Three"]
            "{rank}"
        "Ace"   :: qty: 2             /// keyed override
        "Two"   :: price: 1.25        /// keyed override
        "Three" :: 1.25 :: 2          /// positional override
    end

**Rules**

- Defaults apply unless overridden.
- Overrides match the generated entry by loop vars.
- Both positional and keyed overrides are supported.

### 8.2.3 Nested Data Structures

Templates can embed maps and arrays as fields. Interpolation works inside string literals.

    @product =
        title: "{title}" ::
        price: .99 ::
        meta: { tags: ["new","featured"], inventory: { count: 0, warehouse: "A" } } ::
        variants: [
            { size: "S", color: "red", sku: "{title}-S-R" },
            { size: "M", color: "blue", sku: "{title}-M-B" }
        ]

    item = @product: title: "Magic Sword"

**Result**

    {
      "title": "Magic Sword",
      "price": 0.99,
      "meta": { "tags": ["new","featured"], "inventory": { "count": 0, "warehouse": "A" } },
      "variants": [
        { "size": "S", "color": "red", "sku": "Magic Sword-S-R" },
        { "size": "M", "color": "blue", "sku": "Magic Sword-M-B" }
      ]
    }

**Notes**

- Use double quotes around interpolated strings.
- Keys remain bare identifiers.
- Nested maps/arrays are literal unless you insert interpolated strings inside them.

# 9. Objects & Classes

## 9.1 Definition & Constructor

A **class** defines a reusable object type with fields and methods. Calling the class name creates a new instance.

**Two styles:**

**Template style**

    class Pet = name: "{name}" :: age: 0 :: species: "dog"
        fn speak()
            "Hi, I'm {name}, a {age}-year-old {species}."
        end
    end

**Function style**

    class Pet(name: string, age: int = 0, species: string = "dog")
        fn speak()
            "Hi, I'm {name}, a {age}-year-old {species}."
        end
    end

- Template style uses `::` separators.
- Function style uses commas.
- Field order is the constructor order; changing it is a breaking change.

## 9.2 Instantiation

Both styles support both instantiation forms:

    /// Template style
    pet1 = Pet: "Fido" :: 3 :: "cat"

    /// Function style
    pet2 = Pet("Fido", 3, "cat")
    pet3 = Pet("Rex", species: "wolf")    /// named override
    pet4 = Pet("Max", :: "cat")           /// skip middle arg

- Defaults apply if omitted.
- Missing required field → `TypeError`.
- Invalid type → `TypeError`.

## 9.3 Field Semantics

- **Interpolation:** `name: "{name}"` → expects a string.
- **Capture:** `price: {price}` → uses type inference.
- **Computed defaults:** expressions like `sku: slug("{title}")` are allowed.
- **Per-instance defaults:** evaluated at instantiation (e.g. `uuid()`).

## 9.4 Access Rules

- **Public fields**: externally visible, with auto-generated accessors:
  - `x()` getter
  - `set_x(v)` setter
  - `is_x()` for booleans
  - User-defined methods override auto-generated ones.
- **Private fields**: `#field` prefix. Accessible only inside the class.
  - External access → `PermissionError`.
  - No auto-accessors generated.
- **Readonly fields**:

      readonly id: uuid()

  Creates getter only; writes → `TypeError`.

## 9.5 Lifecycle

- `on_create()` runs after field binding, before returning the instance.
- Throws → aborts construction.
- Must be deterministic; nondeterministic work raises `DeterminismError` unless explicitly permitted.

## 9.6 Style Choice

Both forms are equivalent.

- **Template style** → declarative, readable, matches `@template`.
- **Function style** → compact, traditional.

## 9.7 Errors

- Missing field → `TypeError`
- Unknown field → `TypeError`
- Type mismatch → `TypeError`
- Currency mismatch → `CurrencyError`
- Private field access → `PermissionError`

## 9.8 Serialization & Helpers

- `write_json` / `write_yaml`: serializes public fields by default; private fields require `include_private: true`.
- `to_map()` → returns map of public fields.
- `copy(overrides…)` → shallow copy with overrides.
- Structural equality (`==`) compares public fields.
- `is` tests identity.

## 9.9 Morph Compatibility

`morph` (§Special Forms) works on **public fields via accessors**. Private fields remain sealed unless exposed.

## 9.10 No Inheritance

Goblin does not support class inheritance. Prefer **composition** and `morph`.

## 9.11 Percent & Money Behavior

Inside class fields and methods, percent follows CIPO rules (§Percent):

    class Book = title: "{title}" :: price: $0
        fn discount(rate: percent) = #price - (rate of #price)
        fn add_tax(rate: percent) = #price + (rate of #price)
        fn quick_calc() = #price + 10%s
    end

**Examples**

    b = Book: "Guide" :: $8
    say b.discount(25%)   /// $6.00
    say b.add_tax(10%)    /// $8.80
    say b.quick_calc()    /// $8.80

- `%` → percent of 1 (adds $0.25)
- `%s` → percent of self (adds 25% of price)
- `% of E` → explicit base
- Money division with `/` → `MoneyDivisionError` (use `//` or `>>`).

# 10. Money & Currency

Money is a core type with strict semantics. All behavior (currency, precision, division, cross-currency rules) is governed by the active **policy** (§27).

## 10.1 Precision Policy

    @policy = "strict_money"
        money: { currency: "USD", precision: 2, policy: "strict" }

**Parameters**

- **precision**: number of decimal places (≥ 0).
- **policy**: `"truncate"` (default) | `"warn"` | `"strict"` | `"defer"`.
  - *truncate*: truncate to precision, ledger remainder.
  - *warn*: same as truncate + `MoneyPrecisionWarning`.
  - *strict*: raise `MoneyPrecisionError`.
  - *defer*: carry full precision until explicit settlement.

**Aliases (keywords only)**: `"cleave"`, `"grumble"`, `"grim"`, `"hoard"`.

**Scope**: Per currency; policy domains (money, datetime, modules, strings) are independent.

## 10.2 Defer ("Hoard") Policy

Keeps sub-precision crumbs until you explicitly settle or cross an export boundary.

**Helpers**

    settle(x: money) -> money    /// canonicalize to current precision
    excess(x: money) -> float    /// inspect carried precision
    with_money_policy("truncate")
        ...
    end

**Boundaries that canonicalize automatically**

- Explicit `settle` call.
- Export (`write_csv`, `write_json`, glam surface).
- Scoped override via `with_money_policy`.

Display always shows canonical precision; internal value may be higher in `"defer"`.

## 10.3 Construction & Literals

    money(1.50)        /// uses active policy currency
    money(1.50, USD)   /// explicit currency

    $1.50, €1.50, 1.50 USD, US$1.50
    -$5.00, $-5.00

- High-precision input truncates and records excess in ledger.
- `"strict"` forbids excess; `"warn"` logs a warning.
- `str(money)` → `CUR 1.23`.

## 10.4 Typing Defaults

    123    → int
    1.23   → float
    $1.50  → money

If a policy sets default currency:

    set @policy usd_money
    5   → USD 5.00

Explicit casts (`int(x)`, `float(x)`, `money(x, CUR)`) always win.

## 10.5 Arithmetic & Comparisons

    + - * // >> %    /// allowed, same currency
    /                /// forbidden → MoneyDivisionError

- Multiply/divide by scalar: allowed, ledger remainder tracked.
- `//` → floor quotient only.
- `>>` → `(quotient, remainder)`.
- `%` → remainder only.
- Cross-currency arithmetic → `CurrencyError`.

**Compound assignment**

- `+=`, `-=`, `*=` allowed.
- `/=`, `%=` forbidden (`MoneyDivisionError`).

## 10.6 Currency Config

    currency:
      default: USD
      allow_suffix: true
      symbol_map:
        "$": USD
        "US$": USD
        "C$": CAD
        "€": EUR
        "£": GBP
        "¥": JPY
        "₹": INR

Build-time override: `goblin build --currency USD`.

## 10.7 Increments

    price++   /// add 1 major unit
    price--   /// subtract 1 major unit

- Only works on `money`.
- Postfix `**` and `//` not allowed (TypeError).

## 10.8 Division, Splits & Remainders

### 10.8.1 Divmod

    q = total // parts
    q, r = total >> parts

- `/` is always invalid.
- `//` returns floor share.
- `>>` returns pair `(quotient, remainder)`.

### 10.8.2 Even Splits

    divide_evenly($100, 3)        /// [$33.34, $33.33, $33.33]
    divide_evenly_escrow($100, 7) /// { shares: [...], escrow: $0.04 }

- Perfect conservation guaranteed.
- Escrow form holds leftover centrally.

### 10.8.3 Remainder Ledger

Any discarded crumbs are tracked automatically.

    remainders_total()   /// { USD: $0.02 }
    remainders_report()
    clear_remainders()

### 10.8.4 Round-Robin Distribution

    allocate_round_robin($0.07, 5)
    /// [$0.02, $0.02, $0.01, $0.01, $0.01]

- Distributes one quantum at a time in rotation.
- Fairness > compactness.

### 10.8.5 Weighted Allocation

    allocate_money($100, [50,30,20]) 
    /// [$50, $30, $20]

- Weights may be numbers or money (same currency).
- Negative/zero weights error.
- Policy applies at quantization step.

## 10.9 Currency Conversion

    convert($10, to: EUR, rate: 0.91) → EUR 9.10

- Logs any sub-precision in target ledger.
- `"strict"` forbids excess.

## 10.10 Dripping Remainders

    drip_remainders(threshold=1, commit=false)

- **threshold**: minimum chunk to release (default = 1 quantum).
- **commit=false**: audit only.
- **commit=true**: actually pay out, reduce ledger.

## 10.11 Conservation Examples

    q, r = $100 >> 3
    /// $100 = $33.33 × 3 + $0.01 ✓

    divide_evenly($100, 3)
    /// $33.34 + $33.33 + $33.33 = $100 ✓

# 11. Percent Type — "Percent of what?"

## 11.0 Reserved Tokens / Keywords

**Reserved:**

- `%` (postfix percent)
- `%s` (postfix "percent of self")
- `of` (base binder)
- `pct` (constructor)

**Modulus** continues to use infix `%` with spaces: `a % b`.

## 11.1 Core Principle (CIPO)

Goblin follows the **Context-Independent Percent Operator** rule:

- `%` is always a **first-class percent value**. It is never "naked."
- **Default base:** `1` → so `25%` = `0.25` as a factor.
- `%s` = percent of self (calculator-style).
- `% of E` = percent of explicit base.
- **No operator changes meaning**: all forms must use `%`, `%s`, or `% of E`.

## 11.2 Literals & Construction

    25%      → factor 0.25  
    pct(25)  → 25% (0.25)  
    pct(0.5) → 0.5% (0.005)  
    pct(-10) → -10%  
    str(25%) → "25%"

`pct(n)` always interprets `n` as percentage points.

## 11.3 Operator Forms

A) `%` — percent of 1 (default)

    8 * 25%   = 2
    100 / 25% = 400
    8 + 25%   = 8.25
    8 - 25%   = 7.75

B) `%s` — percent of self (left operand)

    8 + 25%s = 10
    8 - 25%s = 6
    8 * 25%s = 16
    8 / 25%s = 4

C) `% of E` — explicit base

    8 + (25% of 50) = 20.5

D) `%` with spaces — modulus

    8 % 3 = 2

## 11.4 Binding & Precedence

- `p%`, `p%s`, and `p% of E` are **atomic primaries**, binding tighter than `* //`.
- `of` binds to the percent literal: `25% of X` is one unit.
- `a % b` with spaces = modulus, grouped with `* / //`.

**Example:**

    x + 25%s * 2  ==  x + ((25% of x) * 2)

## 11.5 Desugaring

    A ∘ (p%s)       ⇒ A ∘ ((p/100) * A)   /// ∘ ∈ {+ - * /}
    p%              ⇒ (p / 100)
    (p% of B)       ⇒ (p / 100) * B
    A % B [spaced]  ⇒ Mod(A, B)
    pct(X)          ⇒ (X / 100) as a percent value

## 11.6 Functions & Composition

Percent is dimensionless and valid in numeric functions:

    sqrt(25%)  = 0.5
    (25%)^2    = 0.0625
    sin(50%)   = sin(0.5)

With `of`, the result inherits the base's unit:

    10% of $80  = $8

## 11.7 Money Interop

- `percent × money` follows §10 fixed-point precision.
- `/` with money is forbidden — use `//` or `>>`.

**Examples:**

    $80 * 10%       = $8.00
    10% of $80      = $8.00
    $80 + 15%s      = $92.00

## 11.8 Tax Helpers

    tax(subtotal, rate_or_rates, compound=false) → tax amount
    with_tax(subtotal, rate_or_rates, compound=false) → subtotal + tax(...)

- `rate_or_rates`: accepts `0.10`, `10%`, or array `[8.25%, 1%]`.
- `compound=true` → sequential; else additive.
- Precision: truncation, with remainders ledgered.
- `strict` → `MoneyPrecisionError`.

## 11.9 Worked Examples

    price = $80
    fee   = $5

    price + 10%          /// $80.10 (10% = 0.10 of 1 → $0.10)
    price + 10%s         /// $88.00 (10% of self)
    10% of price         /// $8.00
    (10% of fee) + price /// $80.50

    /// Constructor
    rate    = pct(8.5)   /// 8.5%
    tiny    = pct(0.15)  /// 0.15% = 0.0015
    big     = pct(15)    /// 15%

## 11.10 Context-Bound `%s`

`%s` requires a **self** base.

Illegal:

    rate = 8.5%s    /// ERROR: no self context

Correct:

    rate = 8.5%                 /// store as percent
    total = price + (rate of price)

    discount = 8.5% of price    /// bound to explicit base

This rule ensures **CIPO compliance**: every percent has a determinate base at creation.

# 12. Helper Sugar (Functions + Brackets)

## 12.1 Call Styles

    /// Variadic
    helper(a, b, c)

    /// Array
    helper([a, b, c])

    /// Bracket sugar
    helper[array_expr] ≡ helper(array_expr)

## 12.2 Bracket-Enabled Helpers (Math Functions)

All built-in math functions support bracket sugar for array operations:

    add sum mult sub div min max avg root abs floor ceil round pow divmod

- `add` / `sum` — sum (returns element type; money stays money with precision rules)
- `mult` — product (tracks remainders for money operations)
- `sub` — left-fold subtraction
- `div` — left-fold division (returns float unless exact)
- `min` / `max` — extrema
- `avg` — arithmetic mean (money if same-currency with truncation; else float)
- `root` — sequential roots (`root(27,3)` → 3)
- `abs` — scalar abs; for array form returns elementwise array
- `floor` / `ceil` / `round` — elementwise for arrays
- `pow` — sequential powers
- `divmod` — sequential divmod

**Type rules:**

- Mixed numeric → float
- Money arrays must share currency (else `TypeError`)

## 12.3 Strings & Non-Numeric Arrays

Strings are treated as lists of characters in Goblin (§6). Helper sugar applies consistently:

- `add`
  - On strings/char arrays → concatenates.
  - Example: `add["hi","there"]` → `"hithere"`.
- `mult`
  - On string × int → repeat.
  - Example: `mult["ha", 3]` → `"hahaha"`.
  - On pure string arrays → `TypeError` (no notion of "product" of multiple strings).
- **Other math helpers** (`sub`, `div`, `avg`, etc.)
  - On non-numeric arrays or strings → `TypeError`.
  - Example: `avg["hi","ho"]` → `TypeError("avg requires numeric array")`.

**Principle:** helper sugar does **not** silently coerce non-numeric arrays (except for the explicit string cases above).

# 13. Regex & Text Utilities

## 13.1 Regex Functions (PCRE-style)

    re_test(s, pat, flags="")         → bool
    re_match(s, pat, flags="")        → first match object or nil
    re_findall(s, pat, flags="")      → array of matches
    re_replace(s, pat, repl, flags="") → string

**Flags:**

- `i` — case-insensitive
- `m` — multi-line (`^` and `$` match per line)
- `s` — dot-all (`.` matches newline)
- `x` — verbose (ignore whitespace and allow comments)

**String method shorthands:**

    "abc123".matches(pat, flags="")   → bool
    "abc123".replace(pat, repl, flags="") → string
    "abc123".findall(pat, flags="")   → array

## 13.2 Text Helpers

    lines(s)                   /// split by \n (no terminators)
    strip(s), lstrip(s), rstrip(s)
    split(s, sep=nil, max=-1)  /// default: split on whitespace
    join(arr, sep)             /// concat array elements with sep
    slug(s), title(s), lower(s), upper(s)

- `lines(s)` always returns an array of strings.
- `strip` variants remove whitespace (or explicit chars, if passed).
- `split` with `sep=nil` collapses consecutive whitespace.
- `slug(s)` → lowercased, hyphen-separated, ASCII-safe identifier.
- `title(s)` → capitalizes words; `lower/upper` are case transforms.

# 14. File I/O (Sandboxed)

## 14.1 Core Functions

    /// Text
    read_text(path), write_text(path, s)

    /// YAML
    read_yaml(path), write_yaml(path, obj)

    /// CSV
    read_csv(path) → array of maps (values as strings)
    write_csv(path, rows)  /// rows = array of maps

    /// JSON
    read_json(path, opts={}) → value
    write_json(path, value, opts={}) → nil

    /// In-memory JSON (no filesystem)
    json_stringify(value, opts={}) → string
    json_parse(s, opts={}) → value

    /// File System
    exists(path), mkdirp(path), listdir(path), glob(pattern)
    cwd(), chdir(path), join(a,b,...)

    /// Binary
    read_bytes(path), write_bytes(path, blob)
    is_binary(path)  /// content sniffing heuristic

    /// Utils
    now(), uuid()

*All paths are relative to current working directory unless absolute.*

## 14.2 JSON

### 14.2.1 Type Mapping

- `null` → `nil`
- `true/false` → `true/false`
- numbers → `float` (no auto-cast to `int`/`money`)
- strings → `string` (no auto-cast)
- arrays → `array`
- objects → `map` (string keys only)

**No silent money parsing.** Strings like `"USD 1.50"` stay strings unless you opt in via options. Money from JSON floats requires explicit `money(float, CUR)` or decode options.

### 14.2.2 Options (All JSON Functions)

`opts` is a map. Unknown keys ignored.

**Writing:**

- `indent: int` (default `2`) — pretty print spaces; `0`/`nil` for minified
- `sort_keys: bool` (default `false`)
- `money: "object" | "string" | "units"` (default `"object"`)
  - `"object"` (canonical):

        { "_type": "money", "currency": "USD", "amount": "123.45" }

  - `"string"`: `"USD 123.45"`
  - `"units"`:

        { "_type": "money", "currency": "USD", "units": 12345, "precision": 2 }

- `datetime: "string" | "object"` (default `"string"`)
- `enum: "name" | "value" | "object"` (default `"name"`)
- `blob: "base64" | "hex" | "error"` (default `"error"`)

**Reading:**

- `money: "off" | "object" | "string" | "auto"` (default `"off"`)
- `datetime: "off" | "string" | "object" | "auto"` (default `"off"`)
- `enum: "off" | "name" | "value" | "object" | "auto"` (default `"off"`)
- `enum_schema: map<string,string>` (for `enum:"value"`)
- `strict_numbers: bool` (default `false`) — error on NaN/Infinity
- `blob: "off" | "base64" | "hex" | "auto"` (default `"off"`)

**Glam:** may register their own JSON/YAML handlers for custom types. If none registered, they fall back to these rules.

### 14.2.3 Errors

- Malformed JSON → `ValueError`
- Bad money object (missing/invalid fields) → `ValueError`
- Precision policy applies on decode:
  - `strict` → `MoneyPrecisionError` on sub-precision
  - `warn`/`truncate` → canonicalize + ledger remainder

### 14.2.4 Examples

    /// Write canonical money object
    write_json("cart.json", { total: $123.45 }, { money: "object", indent: 2 })

    /// Read without decoding
    raw = read_json("cart.json")  /// money stays map

    /// Read with decode
    cart = read_json("cart.json", { money: "object" })
    say cart.total  /// USD 123.45

    /// String mode
    write_json("price.json", $19.99, { money: "string" })
    read_json("price.json", { money: "string" })  /// USD 19.99

    /// In-memory round-trip
    s = json_stringify({ m: $0.05 }, { money: "units" })
    obj = json_parse(s, { money: "units" })

## 14.3 YAML

- **Functions:** `read_yaml(path)`, `write_yaml(path, obj)`
- **Options:** YAML honors the same `money`, `datetime`, `enum`, and `blob` options as JSON.
- **Errors:** invalid YAML → `ValueError`.
- Type mapping identical to JSON (null→nil, etc.).

## 14.4 CSV

- **Functions:**
  - `read_csv(path) → array<map<string,string>>`
  - `write_csv(path, rows: array<map>)`
- **Notes:**
  - Values are strings on read; caller must cast (`int`, `float`, `money`, etc.).
  - `write_csv` supports `money` options (`"string"`, `"object"`, `"units"`) for round-trip fidelity.
  - No auto-decode of money/datetime; explicit opts required.
  - Malformed CSV → `ValueError`.

# 15. Date & Time (Core)

## 15.1 Types

```goblin
date      /// calendar day (no time, no zone)
time      /// clock time (no date, no zone)
datetime  /// date + time + zone (zone required; defaults from policy)
duration  /// elapsed seconds (no zone)
```

No implicit coercions between these types.

## 15.2 Policy & Defaults (Project-wide)

Datetime semantics follow the active policy (§27). Default scaffold (no network required):

```goblin
@policy = "project_default"
    datetime: {
        tz: local_tz(),            /// machine zone at startup
        prefer_trusted: false,     /// do not require server time
        policy: "allow",           /// local fallback is OK
        leap_mode: "clamp",        /// leap second handling (see §15.13)
        time_arith: "error",       /// "error" | "wrap" | "shift" (see §15.8)
        tzdata_version: ""         /// empty = engine default; else pin, e.g. "2025a"
        debug: false               /// policy breadcrumbs & why_dt() detail
    }
```

TZ accepts IANA names ("America/Denver"), "UTC", or fixed offsets ("+00:00", "-07:00").

Affects constructors without explicit `tz:` and `now()`/`today()`.

**Performance requirement**: Engines expose each policy domain as an immutable snapshot captured at script load or `set @policy`. Datetime ops read O(1) fields (no map lookups/allocations). Common flags may be lowered to IR constants.

## 15.3 Trusted Time (Server → Cache → Local) — Opt-in

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

```goblin
trusted_now()        → datetime
trusted_today()      → date
last_trusted_sync()  → datetime | nil
time_status()        → {
  source: "server"|"cache"|"local",
  verified: bool, age_s: int,
  offset_s: float, drift_s: float,
  tzdata: { local_version: string, server_version: string | nil, mismatch: bool }
}
ensure_time_verified(reason="")  /// raise/warn per policy
clear_time_cache()               → nil
```

**Behavior**

`trusted_*` uses server → signed monotonic cache → local per policy.

In `strict`, missing/invalid trust → `TimeSourceError`. In `warn`, emit `TimeSourceWarning`.

If server reports a tzdata version and it differs from local, `time_status().tzdata.mismatch = true`; `ensure_time_verified` fails in `strict`.

## 15.4 Construction & Parsing

```goblin
d  = date("2025-08-12")                         /// YYYY-MM-DD
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

Tokens: `YYYY MM DD HH mm ss SSS ZZ` (ZZ = ±HH:MM).
If the string has Z/offset, it overrides defaults.
Malformed → `ValueError`.

## 15.5 Duration Literals

```goblin
1s  90s  5m  1h  2d  3w  6mo  1y
```

Units: `s` (seconds), `m` (minutes), `h` (hours), `d` (days), `w` (weeks), `mo` (months = 30d), `y` (years = 365d).
`duration(n_seconds)` also available.

**Disambiguation**

`m` always minutes; `mo` months.

Passing minutes to a calendar adder (e.g., `add_months(d, 3m)`) →
`ValueError("minutes given where months expected; use 'mo' for months")`.

## 15.6 Now/Today & Zone Helpers

```goblin
now()          → datetime   /// local or trusted per policy (prefer_trusted)
today()        → date
utcnow()       → datetime   /// always UTC; uses trusted chain if configured
local_tz()     → string
to_tz(dt, "UTC")            /// change zone (wall-clock adjusts)
```

## 15.7 Formatting & Accessors

```goblin
.iso()                    /// ISO-8601
.format("YYYY-MM-DD")     /// token format
str(x)                    /// alias for .iso()

/// Accessors (read-only)
dt.year dt.month dt.day
dt.hour dt.minute dt.second dt.millisecond
dt.weekday   /// 1..7 (Mon=1)
dt.yearday   /// 1..366
dt.date()    → date
dt.time()    → time
dt.tz()      → string
dt.epoch()   → float   /// seconds since Unix epoch (UTC)
```

Formatting never emits named zone abbreviations—only numeric ZZ.

## 15.8 Arithmetic & Comparisons

```goblin
/// Datetime & date arithmetic
datetime + duration  → datetime
datetime - duration  → datetime
date     + duration  → date       /// duration must be whole days
date     - duration  → date

/// Differences
datetime - datetime  → duration
date     - date      → duration

/// Comparisons
< <= > >= == !=  on like-types only  /// cross-type → TypeError
```

**Time arithmetic requires explicit intent**

```goblin
time ± duration → TimeArithmeticError
"time arithmetic requires explicit helper: use wrap_time(...) or shift_time(...)"
```

**Helpers:**

```goblin
wrap_time(t: time, dur: duration)  → time
/// Wrap modulo 24h: wrap_time(time("23:30"), 1h) → 00:30

shift_time(t: time, dur: duration) → { days: int, time: time }
/// Carry overflow/underflow: shift_time(time("23:30"), 1h) → { days: 1, time: 00:30 }
```

**Optional policy desugaring (project toggle):**

`time_arith: "wrap"` — allow `time ± duration`, desugar to `wrap_time`.

`time_arith: "shift"` — desugar to `shift_time`.

`time_arith: "error"` (default) — keep forbidden.

## 15.9 Truncation / Rounding

```goblin
floor_dt(dt, "year"|"month"|"week"|"day"|"hour"|"minute"|"second")
ceil_dt(dt,  "year"|"month"|"week"|"day"|"hour"|"minute"|"second")
```

Weeks floor to Monday 00:00 in the instance's TZ.

## 15.10 Calendar-Safe Adders (field changes, clamped)

```goblin
add_days(d|dt, n)    → same type
add_months(d|dt, n)  → same type    /// Jan 31 + 1 → Feb 28/29
add_years(d|dt, n)   → same type    /// Feb 29 + 1 → Feb 28
```

**Rule of thumb:**

Use duration (`+ 1mo`, `+ 1y`) for elapsed time (30/365 days).

Use adders for calendar changes (clamped). Goblin never guesses.

## 15.11 Ranges

```goblin
/// Dates (inclusive by default)
for d in date("2025-08-01")..date("2025-08-05")           /// default step=1d
for d in date("2025-08-01")..date("2025-08-05") step 2d

/// Datetimes (exclusive by default)
for ts in dt_start...dt_end                                /// default step=1h
for ts in dt_start...dt_end step 30m
```

`..` = inclusive; `...` = exclusive.

`step` must be a duration. Optional `inclusive:`/`exclusive:` flags may override defaults.

## 15.12 Interop & Serialization (JSON/YAML/CSV)

Default writing & reading: strings (no auto-decode).

**JSON options:**

```goblin
write_json(path, value, { datetime: "string" | "object" = "string" })
read_json(path,  { datetime: "off" | "string" | "object" | "auto" = "auto" })
```

**"object" (typed) form:**

```json
{"_type":"datetime","value":"2025-08-12T14:30:00-06:00"}
{"_type":"date","value":"2025-08-12"}
{"_type":"time","value":"14:30:00"}
{"_type":"duration","seconds":3600}
```

"auto" tries object → strict ISO → passthrough.

YAML/CSV mirror string mode; use parse helpers to opt in.

When deserializing into a typed target (e.g., class field), strict ISO strings are accepted as that type even if `datetime:"off"`.

**Naive boundary adapters**

```goblin
naive_to_datetime(epoch_s: float, tz: string) → datetime    /// attach zone
datetime_to_naive_utc(dt: datetime)           → float       /// epoch seconds, UTC
datetime_to_naive_local(dt: datetime)         → float       /// epoch seconds, local
```

Errors: malformed → `ValueError`.

## 15.13 Leap Seconds

Goblin uses POSIX/Unix epoch (no leap seconds).
Parsing "HH:MM:60" is allowed in UTC only for historical data.

```goblin
@policy = "leap"
    datetime: { leap_mode: "clamp" | "smear" = "clamp" }
```

`clamp`: parse as "HH:MM:59" + 1s; formatting never emits :60.

`smear`: spread the leap second across the last second of the day.
Formatting always normalizes per leap_mode.

## 15.14 Introspection & Debugging

```goblin
dt_policy() → {
  tz: string, prefer_trusted: bool, policy: string, leap_mode: string,
  time_arith: string, tzdata_version: string, source: string|nil,
  cache_path: string|nil, ttl: duration, cache_ttl: duration, skew_tolerance: duration,
  debug: bool
}

why_dt(op: string) → string
/// Human-readable explanation of how a datetime operation was interpreted,
/// including policy fields and parsing/adder/range decisions.
```

When `datetime.debug:true`, `@policy` changes append JSON lines to `dist/policy.log` with scope and diffs.

## 15.15 Errors & Warnings

- **ValueError** — malformed input or invalid operation
- **TypeError** — cross-type comparisons/ops
- **OverflowError** — out of range
- **TimezoneError** — unknown tz
- **TimeSourceError** — strict trusted time failure
- **TimeSourceWarning** — trusted fallback in warn mode
- **TimeArithmeticError** — time ± duration attempted without helper
- **DatetimeCompatWarning** — behavior differs under datetime_compat profile (see below)

## 15.16 Compatibility & Migration (Non-normative)

A prebuilt compat profile eases migration from naive ecosystems:

```goblin
@policy = "datetime_compat"
    datetime: { time_arith: "wrap", policy: "warn", leap_mode: "clamp" }
```

Emits `DatetimeCompatWarning` where behavior differs from canon (implicit wrapping, naive parse).

Use lints/codemods to replace `time ± duration` with `wrap_time`/`shift_time`, add explicit tz, and swap elapsed/calendar math as needed. Tighten to `time_arith:"error"` and `policy:"strict"` once warnings are zero.

---

## Design Charter (recap)

No naive timestamps; explicit policy; trusted time is opt-in; strict parsing/formatting; calendar-safe adders; explicit human time math; clear ranges; clean serialization; deterministic core; surfaced tzdata; O(1) policy snapshots; first-class introspection; presets & adapters; smooth migration.

# 16. Errors & Warnings

## 16.1 Error Handling Syntax

```goblin
/// Structured error handling
attempt
    risky_operation()
    "success"
rescue ErrorType as e
    handle_error(e)
    "recovered"
ensure
    cleanup()
end

/// Manual throws
error "message"

/// Assertions
assert cond, "msg"

/// Warnings
warn "msg"
```

## 16.2 Error Categories

**Core Error Types**

```
NameError, TypeError, ValueError, IndexError, KeyError,
ZeroDivisionError, SyntaxError, AssertionError, OverflowError
```

**Goblin-Specific Error Types**

```
CurrencyError, MoneyDivisionError, MoneyPrecisionError,
TimeSourceError, TimeSourceWarning, TimezoneError, TimeArithmeticError,
EnumError, GlamError, ContractError, PermissionError, AmbiguityError,
LockfileError, DeterminismError, MorphTypeError, MorphFieldError,
ModuleNotFoundError, PolicyNotFoundError, BanishError, DatetimeCompatWarning,
GLAM_NO_PROVIDER, GLAM_FANOUT_UNSUPPORTED, GLAM_PARTIAL_FAILURE,
GLAM_DEST_INVALID, GLAM_TIMEOUT, GLAM_OVERWRITE_DENIED, GLAM_AMBIGUOUS_PROVIDER
```

**List Operation Errors**

```
EmptyPickError, PickCountError, PickIndexError, PickTypeError,
ReapEmptyError, ReapCountError, ReapIndexError, ReapTypeError,
UsurpIndexError, UsurpCountError, UsurpArityError, UsurpTypeError,
ReplaceIndexError, ReplaceTypeError,
InsertIndexError, InsertTypeError,
ShuffleTypeError, SortTypeError, AddTypeError
```

## 16.3 Warnings

Built-in warning types:

```
MoneyPrecisionWarning, TimeSourceWarning, UnusedJudgeValueWarning
```

## 16.4 Notes on Goblin-Themed Error Messages

- The specification supports **standardized error and warning messages** as listed above.
- A future feature called **"grumble mode"** will allow enabling *goblin-flavored error messages* for personality and developer feedback.
- By default, **grumble mode is off**. Config option will enable it.
- The **cheat sheet should document** this option when grumble mode is implemented.

# 17. Reserved Words

## 17.1 Hard Keywords

These cannot be shadowed or redefined:

```
if, elif, else, for, in, while, unless, attempt, rescue, ensure, return, skip, stop, assert,
class, fn, enum, use, import, export, via, test, true, false, nil, int, float, bool, money, pct,
date, time, datetime, duration, morph, vault, judge, banish, unbanish, expose, set, settle,
pick, reap, usurp, len, shuffle, sort, add, insert, replace, roll, freq, mode, sample_weighted
```

## 17.2 Soft Keywords

These are context-dependent (reserved only in specific constructs):

```
from, at, first, last, to, into, with, dups, seq, as
```

Note: `in` is always a hard keyword (loop syntax); there is no general boolean `in` operator.

## 17.3 Notes

- `delete`: not a reserved word. In v1.5 it is only valid as an operation within `export/publish` blocks. If promoted to a statement in a future version, it will be added here.
- Some identifiers (e.g. built-in functions like `read_text`, `write_json`, `divide_evenly`, etc.) are *standard library functions*, not reserved words. They may be shadowed unless explicitly banished by policy.

## 17.4 Version Codenames

- v1.0: *Evil Redcap* (MVP)
- v1.5: *Treasure Hoarder* (current)
- v1.6: *Gold Sniffer*
- v1.7: *Wealth Watcher*
- v1.8: *Loot Guardian*
- v2.0: *Hoard Lord*
- v2.5: *Sneaky Pilferer*
- v3.0: *Trickster King*
- v3.5: *Mischief Maker*

# 18. Core vs. Glam Architecture

## 18.1 Design Philosophy

Goblin maintains a **lean, auditable core** with **extensible glam**.

- **Core** guarantees determinism, safety, and language coherence.
- **Glam** provides domain-specific functionality (commerce, APIs, custom records).

Core cannot be bypassed or compromised by glam. Glams must operate within the rules of the runtime.

## 18.2 Must Be Core

The following remain in **core** to ensure safety, determinism, and predictability:

**Syntax & Parser**
- Keywords: `use`, `via`, `prefer`, `contract`, `test`, `import`, `set`
- Operators: percent literals (`%`, `%s`, `% of`), postfix `++`/`--`, infix `//`, `>>`
- Operator precedence and `%` semantics (`%` vs `%s` vs `% of`)
- Template syntax: `::`, `@name`

**Type System Primitives**
- Built-in types: `int`, `float`, `bool`, `string`, `money`, `percent`, `date`, `time`, `datetime`, `duration`, `enum`
- Money conservation, precision, and remainder tracking
- Trusted time semantics and verification chains
- Enum identity and singleton guarantees

**Contract System**
- Global contract registry (`contract name → signature/errors`)
- Compliance enforcement at implementation and call sites
- Call-site resolution order: `via` > `prefer` > default
- Error classes: `AmbiguityError`, `ContractError`

**Sandbox & Determinism**
- Permission enforcement (FS/NET allowlists, env gating)
- Deterministic mode (`--deterministic`): wall-clock blocking, RNG seeding, network restrictions
- Dry-run infrastructure

**Security Infrastructure**
- Trusted time chains and HMAC verification
- Lockfile management and version pinning
- Build reproducibility guarantees

**Runtime Safety**
- Standard error classes and glam error wrapping
- Structured JSONL logging around capability calls
- Reserved word collision prevention

**Core I/O & Serialization**
- JSON, YAML, CSV built-ins
- Serialization hooks for glam-defined types
- Type-safe deserialization policies

## 18.3 Should Be Glam

These belong to **glam**, not core:

- Domain capabilities (e.g. Shopify, blockchain, invoicing)
- Domain types (`Product`, `Order`, etc.)
- Platform integrations and APIs
- Specialized exporters/importers
- Domain-specific validation rules
- External telemetry (beyond core logging)
- Workflow CLIs and developer tooling

## 18.4 Gray Areas

Some features may evolve between core and glam:

- **Event bus / scheduling**: Core ensures minimal scheduling; glam may extend with workers or queues.
- **Randomness**: Core provides seeded `rand`; crypto glams may add secure RNG with explicit permissions.
- **Formatting & personality**: Core supports plain errors; optional goblin "grumble mode" may be enabled (config).

## 18.5 Architectural Benefits

This split provides:

- **Lean, auditable core** — deterministic and safe.
- **Innovative glams** — flexible domain extensions.
- **No vendor lock-in** — multiple glam providers can compete.
- **Auditability** — core execution remains verifiable and predictable.

# 19. Glam — Philosophy & Architecture

## 19.1 Purpose

Glams are first-class, modular extensions that feel native to Goblin. The core stays lean; anything domain-specific lives in a glam.

Key properties:

- **Language-native**: use, namespacing, via, prefer
- **Type-aware**: glams can register types & capabilities
- **Contract-checked**: implementations must match declared contracts
- **Deterministic**: pinned versions, lockfiles, sandboxed side effects

## 19.2 Loading & Versioning

```goblin
use shopify@^1.6 as shp
use tarot_deck@1.2
use invoice            /// latest allowed by policy if not pinned (discouraged)
```

- Pin by default. Projects should reference a version/range.
- `as` sets a local alias (`shp::csv`).
- Lockfile (`glam.lock`) records `{glam, version, checksum, source}`; builds resolve only from lock unless `--update`.

## 19.3 Capability Resolution

Glams declare capabilities (functions, templates, exporters). Calls resolve deterministically:

**Call-site via:**

```goblin
export @cards via shp::csv
```

**Global preference:**

```goblin
prefer product.export via shp
```

**Project defaults:** `goblin.config.yaml` maps contracts → glams.

If multiple providers remain, throw `AmbiguityError`.

**Namespacing**

- Public symbols: `glam::Symbol` (e.g., `shopify::csv`)
- Use `via glam::symbol` or `via alias::symbol` to bind a call

## 19.4 Contracts (First-Class)

Contracts define capability shape & allowed errors. Goblin checks them at use time.

```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end
```

**Rules:**

- Signature (names, arity, types) must match exactly.
- Only declared errors may be thrown; others wrapped as `GlamError(glam, capability, cause)`.
- Contracts are global IDs (`product.export`).

**Introspection**

```goblin
glam_contracts("shopify")
```

## 19.5 Glam Manifest & Permissions

Each glam ships a `glam.yaml`:

```yaml
name: shopify
version: 1.6.2
provides: [product.export, inventory.sync]
contracts: [product.export]
requires:
  fs: ["dist/"]
  net: ["api.shopify.com"]
  env: ["SHOPIFY_TOKEN"]
permissions:
  mode: "fs+net"
checksum: "sha256-…"
```

- Glam names may not be reserved words (§17).
- Core validates manifest/permissions against project policy.
- In deterministic builds, network is blocked unless allowlisted.

```goblin
glam_permissions("shopify")
```

## 19.6 Event Bus

Lightweight, in-process bus with sync/async semantics.

**Emit:**

```goblin
emit "catalog.ready", payload            /// sync (errors bubble)
emit_async "order.ready", payload        /// async queue (job id)
```

**Subscribe:**

```goblin
on "order.ready" mode: "async" concurrency: 4 error: "collect"
    ...
end
```

**Options:**

- `mode`: "sync" | "async" (default sync)
- `concurrency`: async workers (default 1)
- `error`: "stop" | "skip" | "collect"

## 19.7 Sandbox & Determinism

**Sandbox** = glam manifest + project policy

**Modes:** "none" | "fs" | "fs+net"

**Deterministic builds:**

- Block wall-clock (`trusted_now()` required)
- Block network unless allowlisted
- Reject unseeded RNG

**Dry-runs** allowed for exporter contracts:

```goblin
file = product.export(@items) via shp dry_run:true
```

## 19.8 Logging & Telemetry

Core emits JSONL logs (`dist/glam.log`) around glam calls:

```json
{"ts":"2025-08-12T15:30:00Z","glam":"shopify","cap":"product.export","ms":128,"ok":true}
{"ts":"2025-08-12T15:30:01Z","glam":"shopify","cap":"product.export","ok":false,"err":"ValidationError: missing title"}
```

No phoning home unless glam explicitly requests and permissions allow.

## 19.9 Testing Hooks

Inline tests run in sandbox:

```goblin
test "shopify csv emits header"
    rows = shopify::csv_preview(@cards)
    assert rows[0].starts_with("Handle,Title")
end
```

**CLI:**

```bash
glam test shopify
glam test --all
```

## 19.10 Introspection APIs

```goblin
glams()                         /// ["shopify","tarot_deck"]
glam_symbols("shopify")         /// exported symbols
glam_contracts("shopify")       /// implemented contracts
glam_permissions("shopify")     /// sandbox info
```

## 19.11 Usage Patterns

### 19.11.1 Single Export

```goblin
use tarot_deck@1.2, shopify@^1.6 as shp
prefer product.export via shp

@cards = tarot_deck::card_template(price: .99, qty: 1)
    "Ace of Cups"
    "Two of Cups"

file = product.export(@cards) via shp
```

### 19.11.2 Multi-Platform Chain

```goblin
use board_game, shopify@^1.6 as shp, etsy@^2

@games = board_game::template()
    "Catan" :: qty: 4
    "Pandemic" :: qty: 2

export @games via shp::csv to "dist/shopify.csv"
export @games via etsy::csv to "dist/etsy.csv"
```

### 19.11.3 Event-Driven Pipeline

```goblin
emit "catalog.ready", @games

on "catalog.ready" mode: "async" concurrency: 2 error: "collect"
    product.export($event.payload) via shp
end
```

## 19.12 Errors

Glams may throw:

```
GlamError(glam, capability, cause)
ContractError
PermissionError
AmbiguityError
LockfileError
DeterminismError
```

## 19.13 Project Config (excerpt)

```yaml
glam:
  defaults:
    product.export: shopify
  permissions:
    mode: "fs+net"
    fs_allow: ["dist/"]
    net_allow: ["api.shopify.com"]
  enforce_lock: true
  deterministic_build: true
```

## 19.14 Rust-Backed Glams

Glams may wrap Rust crates:

- In-process crate (fastest)
- Dynamic plugin (`.so`/`.dll`)
- Out-of-process/WASM with IPC

**VM Enforces:**

- Contract fidelity (unexpected errors → `GlamError`)
- Permissions (respect fs/net/env allowlists)
- Determinism (no wall-clock, seeded RNG, `trusted_now` only)
- Introspection APIs

## 19.15 Namespace Collisions

If two glams export the same symbol:

```goblin
use trans_lib1@1.0 as lib1
use trans_lib2@1.0 as lib2

say lib1::translate_spanish("Hello")
say lib2::translate_spanish("Good morning")
```

- Alias required to disambiguate
- Compile-time error if duplicate names with no alias
- `prefer`/`via` can set defaults, but alias always overrides

## 19.16 Output File Namespace Isolation

Goblin automatically namespaces glam outputs to avoid clobbering:

- `<alias>_v<resolved-version>_<filename>` if alias present
- `<glam-name>_v<resolved-version>_<filename>` if not

**Example:**

```
shp_v1.6_out.csv
etsy_v1.12_out.csv
```

**Rationale:** No silent overwrites across glams.

## 19.17 CRUD Operations & Safety Gates

Goblin enforces explicit CRUD with strong defaults:

- **Create** = no-clobber by default
- **Read** = cwd allowed; external paths require `--allow`
- **Update** = opt-in (append, patch, replace) gated by CLI
- **Delete** = soft by default; hard delete requires double-gates (`hard: true` + `--fs-delete=hard`)

**For DB/services:**

- Dry-run by default (plan)
- `--commit` required for mutations
- Deletes threshold-gated (`DeleteThresholdExceeded` if over N rows)

## 19.18 Default Safety Policy

- **Files:** Create-only, no overwrite. Updates gated. Deletes double-gated.
- **DB/Services:** Read-only by default. C/U/D require explicit commit and gating.
- **Determinism:** hash manifests required for updates/deletes.

# 20. Enums (Core)

Enums are a core primitive type (see §1.3). They define closed sets of named constants with optional backing values. Enums are strict, first-class, and never implicitly coerce to other types.

## 20.1 Purpose

- Closed, finite sets of named constants.
- Optional backing values (int or string).
- Strict equality, no implicit coercion.
- Safe to use as keys in maps/sets (singleton guarantee).

## 20.2 Declaration

```goblin
enum Status
    Pending
    Paid
    Shipped
end

enum Http as int
    Ok = 200
    NotFound = 404
end

enum Suit as string
    Clubs = "C"
    Diamonds = "D"
    Hearts = "H"
    Spades = "S"
end
```

- `as int` / `as string` are optional.
- No backing → symbolic enum with stable ordinal (0..n-1).
- Int/string-backed values must be explicit (unless using `seq`).
- Variants must be unique; enums are closed (no late additions).

### 20.2.1 Sequential Int Enums

Optional `seq` auto-increments int-backed enums:

```goblin
enum Priority as int seq
    Low = 1
    Medium       /// 2
    High         /// 3
end

enum Http as int seq
    Ok = 200
    Created      /// 201
    BadRequest = 400
    NotFound     /// 401
    ServerError = 500
    BadGateway   /// 501
end
```

**Rules:**

- `seq` only valid with `as int`.
- First entry must be explicit.
- Auto-values increment from last explicit.
- Mixed explicit + auto is valid; duplicates error.
- Not allowed for string-backed enums.

## 20.3 Construction & Access

```goblin
s = Status.Paid
h = Http.Ok
c = Suit.Clubs
```

- Type is the enum (`Status`), not string/int.
- Access via `EnumName.Variant`.
- Singleton guarantee: all references to a variant point to the same instance.

## 20.4 Introspection & Methods

```goblin
s.name()      → "Paid"
s.value()     → backing or name()
s.ordinal()   → 0-based index
str(s)        → "Status.Paid"

Status.values()  → [Status.Pending, Status.Paid, Status.Shipped]
Status.names()   → ["Pending","Paid","Shipped"]
Status.from_name("Paid")      → Status.Paid
Status.try_from_name("X")     → nil
Http.from_value(404)          → Http.NotFound
Http.try_from_value(201)      → nil
```

## 20.5 Operators & Type Rules

- **Comparisons:** `==`, `!=`, `is`, `is not` (same enum type only).
- **Ordering** (`<`, `>`) allowed only for int-backed enums.
- Cross-type comparisons → `TypeError`.
- No arithmetic, increment/decrement, or postfix math.
- Valid as map/set keys.

## 20.6 Control Flow

```goblin
if s is Status.Pending
    "hold"
elif s is Status.Paid
    "ship"
else
    "investigate"
```

No special switch/syntax; use conditionals or map dispatch.

## 20.7 Interop (JSON/YAML/CSV)

Default: string name (`"Enum.Variant"`) for readability. Options:

```goblin
write_json(path, v, { enum: "name" | "value" | "object" = "name" })
read_json(path, { enum: "off" | "name" | "value" | "object" | "auto" = "off" })
```

- **"name":** `"Status.Paid"` (qualified).
- **"value":** backing value only (requires `enum_schema` for decode).
- **"object":** canonical object:

```json
{"_type":"enum","enum":"Status","name":"Paid","value":"Paid","ordinal":1}
```

- **"auto":** best-effort decode.

YAML/CSV behave like "name" mode by default.

## 20.8 Casting & Formatting

```goblin
str(Status.Paid)       → "Status.Paid"
Status.Paid.name()     → "Paid"
Status.Paid.value()    → "Paid"
fmt(Status.Paid, "")   → "Status.Paid"
```

## 20.9 Iteration

```goblin
for v in Status
    say v.name()
```

Enums are iterable in declaration order.

## 20.10 Errors

- **EnumError:** unknown name/value, duplicates in int/string enums.
- **TypeError:** invalid comparisons or operations.
- **Dev tools:** warn if `seq` gaps exceed 100 (potential mistake).

## 20.11 Examples

```goblin
enum Status
    Pending
    Paid
    Shipped
end

order = { id: 17, status: Status.Pending }

if order.status is Status.Pending
    "hold"
else
    "continue"

enum Priority as int seq
    Low = 1
    Medium       /// 2
    High         /// 3
    Critical = 10
    Urgent       /// 11
end

assert Priority.Low < Priority.High
assert Priority.Critical.value() == 10

enum Level as int seq
    Beginner = 1
    Advanced = 10
    Expert       /// 11
end

enum Suit as string
    Clubs = "C"
    Diamonds = "D"
    Hearts = "H"
    Spades = "S"
end

write_json("order.json", order, { enum: "name" })
back = read_json("order.json", { enum: "name" })
say back.status  /// Status.Pending
```

# 21. Gmark — Project-Local Stable References

## 21.1 What is a gmark?

A **gmark** is a stable, human-readable handle that uniquely identifies a piece of content within a project (e.g., blog post, page, product). Gmarks provide **internal linking and stable ordering** that survive file moves/renames, so glams can reorganize content without breaking references.

- **Name** — string key (`"post/how-to-play"`, `"how-to-play"`)
- **Ord** — project-wide integer for stable ordering (newest at the end by default)
- **ID (opaque)** — unique token for registry internals; not user-facing

## 21.2 Persistence

Registry lives at `.goblin/gmarks.lock` (JSON):

```json
{
  "last_ord": 137,
  "marks": {
    "post/how-to-play": { "ord": 121, "id": "gm_8C3...", "created":"...", "updated":"..." },
    "post/faq":         { "ord": 122, "id": "gm_F91...", "created":"...", "updated":"..." }
  }
}
```

- Written atomically on mutation
- Read-only during `--deterministic` builds (unless explicitly allowed by policy)

## 21.3 Creating/Ensuring a gmark

```goblin
gmark("post/how-to-play")            /// auto-increment ord
gmark("post/how-to-play", ord: 42)   /// manual ord
```

**Rules:**
- Omit `ord:` → uses `last_ord + 1`
- Provide `ord:` → must be free, else `GmarkConflictError`
- Re-calling with same name is idempotent (returns existing record unless ord differs)

## 21.4 Naming Rules

- Allowed: letters, digits, `_`, `-`, `/`, `.`
- No leading/trailing slashes; no empty segments
- Max length: 256
- Case-sensitive
- Must not be a reserved word (§19)
- Invalid names → `GmarkInvalidError`

## 21.5 Updating ord

```goblin
gmark_set_ord("post/how-to-play", 200)
```

- Target ord must be unused → else `GmarkConflictError`
- Does not renumber other marks
- Bulk rebalancing is CLI-only (see §21.12)

## 21.6 Introspection & Lookup

```goblin
gmark_info("post/how-to-play")  /// -> { name, ord, id, created, updated } | nil
gmarks()                        /// -> [{ name, ord, id }] sorted by ord
next_ord()                      /// -> last_ord + 1 (peek only, does not allocate)
gmarks_filter("post/")          /// -> only "post/*", ord-sorted
```

## 21.7 Linking from Content

Glams resolve gmarks into URLs/paths; core only provides stable keys:

```goblin
blog::href(gmark: "post/how-to-play")   /// "/posts/how-to-play"
blog::link(text: "How to Play", gmark: "post/how-to-play")
```

## 21.8 Sorting & Querying

- `gmarks()` → ord-sorted chronological list
- Glams may add their own indices (tags/dates/etc.)
- `ord` remains the canonical project-wide sequence

## 21.9 Determinism & Policy

- In `--deterministic` builds, gmark writes are blocked unless explicitly allowed
- Allocation attempt without permission → `DeterminismError`
- Reads are always allowed

Example config to allow gmark writes:

```yaml
glam:
  allow_state_writes:
    - "gmark"
```

When enabled, the runtime:
- Appends JSONL audit entries to `.goblin/gmarks.audit.log`
- Each entry includes `{ before, after, ts, actor, op, lock_checksum }`
- Preserves integrity via mutex + fsync

## 21.10 Errors

- `GmarkConflictError` — duplicate name or ord
- `GmarkNotFoundError` — name not in registry
- `GmarkInvalidError` — illegal format or reserved collision
- `GmarkPersistenceError` — registry file unreadable/unwritable
- `DeterminismError` — allocation blocked in deterministic mode

## 21.11 Examples

**Auto vs manual:**

```goblin
post = gmark("post/hello-world")         /// auto → ord 138
pin  = gmark("post/welcome", ord: 1)     /// manual pin
```

**Stable list:**

```goblin
for m in gmarks()
    say m.name || "@" || str(m.ord)
```

**Move later:**

```goblin
target = next_ord() + 10
gmark_set_ord("post/hello-world", target)
```

## 21.12 Rebalance & Prefix Demo

```goblin
# CLI: glam gmark rebalance

for m in gmarks_filter("post/")
    say blog::link(
        text: m.name.replace("post/","").title(),
        gmark: m.name
    )
```

# 22. Morph — Temporary Type Adaptation (Core)

## 22.1 Purpose

`morph` allows a value of one class to temporarily be treated as another class for the duration of a **single method call**, syncing overlapping fields before and after.

- **Reuse**: call an existing method without adapters/duplication.
- **Transactional**: all updates happen atomically; rollback on error.
- **Privacy-safe**: only public accessors are used; no private state leaks.
- **Deterministic**: no nondeterministic behavior beyond the target method.

## 22.2 Signature

```goblin
result = morph(obj, TargetType, method_call)
```

- **obj** — source instance.
- **TargetType** — class to borrow method from.
- **method_call** — exactly one method call (must exist on TargetType).
- Returns the result of the call; `obj` remains its original type.

## 22.3 Accessor Conventions

Morph uses only public getter/setter pairs with a shared field name:

- `x()` / `set_x(v)`
- `is_x()` / `set_x(v)` (booleans only)

Only fields with *both* a getter and setter on *both* classes participate.

## 22.4 Process

1. **Validate** TargetType is a class and method is public.
2. **Find shared fields** by matching compatible accessor names.
3. **Instantiate** a temporary TargetType:
   - Call `init()` with no args if available.
   - Or call `from_map(map)` if provided.
   - Otherwise require a zero-arg constructor.
4. **Copy in** source fields → temp target.
5. **Invoke** the method on the temp target.
6. **Copy out** updated fields → source object.
   - Type re-validated.
   - If mismatch, rollback entirely.
7. **Return** the method result.

## 22.5 Type Compatibility

- **Primitives**: must match exactly (`int↔int`, etc.).
- **Money**: allowed only if currencies match; precision reconciled per money policy.
- **Datetime types**: same kind (`date↔date`, `datetime↔datetime`, etc.).
- **Arrays/Maps**: allowed if declared element types match.
- **Enums/Classes**: must be the same type.
- **nil**: only if setter accepts it.

## 22.6 Scope & Visibility

- Only **public methods** may be called.
- Only **public accessors** are copied.
- Morph never inspects or mutates private (`#`) state.

## 22.7 Performance

- Shared field maps may be cached per `(SourceType, TargetType)`.
- Complexity is proportional to number of shared fields.

## 22.8 Errors

- **MorphTypeError** — invalid target, method not found.
- **MorphFieldError(field, expected, actual)** — incompatible field type.
- **MorphCurrencyError(field, from_cur, to_cur)** — mismatched money.
- **MorphActionError(cause)** — method threw an error.

All morph errors are transactional: source object remains unchanged.

## 22.9 Examples

**Geometry (rotate):**

```goblin
class Dot = x:0 :: y:0
    fn x() = #x; fn set_x(v)=#x=v
    fn y() = #y; fn set_y(v)=#y=v
end

class Shape = x:0 :: y:0
    fn x() = #x; fn set_x(v)=#x=v
    fn y() = #y; fn set_y(v)=#y=v
    fn rotate(deg) = (#x,#y) = (#x-deg, #y+deg); self
end

p = Dot: 1 :: 0
morph(p, Shape, rotate(90))
say p.x(), p.y()
```

**Discount reuse:**

```goblin
class Book = title:"" :: price:$0
    fn set_price(v)=#price=v
end

class Card = name:"" :: price:$0
    fn set_price(v)=#price=v
    fn apply_discount(rate) = #price=#price*(1-rate); #price
end

b = Book: "Guide" :: $29.99
morph(b, Card, apply_discount(10%))
say b.price()    /// $26.99
```

**Boolean accessors:**

```goblin
class A = active:true
    fn active()=#active
    fn set_active(v)=#active=v
end

class B = enabled:false
    fn is_active()=#enabled
    fn set_active(v)=#enabled=v
    fn toggle()=#enabled=not #enabled
end

a = A: true
morph(a, B, toggle())
say a.active()   /// false
```

## 22.10 Testing Hooks

```goblin
test "morph discount works"
    b = Book: "Gloomhaven" :: $100
    r = morph(b, Card, apply_discount(25%))
    assert b.price() == $75
    assert r == $75
end
```

## 22.11 Determinism

Morph itself is deterministic. Any nondeterminism comes only from the target method and is controlled by sandbox/permissions.

## 25. Banish — Project-Local Feature Blocking

### 25.1 Purpose & Scope
**Goal**: Give projects a simple, portable safety brake to block specific language features.

**Scope**: Project-local only. Banish rules live in the repo and travel with it.

**Effect**: Any usage of a banished feature raises `BanishError` at compile time (hard error).

### 25.2 Feature IDs (Namespaces)
Feature IDs are stable strings used by tools and the compiler. They are namespaced for clarity:

- `core.<keyword>` — core language constructs (e.g., `core.morph`, `core.judge.inline`)
- `op.<operator>` — operators and forms (e.g., `op.pipe_join`, `op.divmod`, `op.postfix.inc`)
- `type.<type>` — types/literals (e.g., `type.money`, `type.percent`, `type.datetime`)
- `builtin.<function>` — built-ins and std helpers (e.g., `builtin.tax`, `builtin.write_json`)
- `glam.<ns>.<symbol>` — glam capabilities by namespace (e.g., `glam.html.raw`, `glam.shopify.product.export`)

**Notes:**
- Feature IDs reference **usage**, not availability. Banish never rewrites code; it only blocks.
- Core safety invariants are not features and cannot be banished (see §25.7).

### 25.3 Config File
**Location**: project root `.goblin.banish.toml`. Subdirectories are ignored.

```toml
# .goblin.banish.toml

[[banish]]
feature = "core.morph"
reason  = "Temporary safety: known issue in v1.5.x"

[[banish]]
feature = "op.pipe_join"
reason  = "Creative constraint: prefer explicit concatenation"
```

- **feature** — required string (one of the IDs above)
- **reason** — required string (human explanation that shows in errors)

**Audit log**: Banish and unbanish actions append JSON Lines to `dist/banish.log`:

```json
{"ts":"2025-08-13T15:21:08Z","op":"banish","feature":"core.morph","reason":"Temporary safety"}
{"ts":"2025-08-13T15:22:44Z","op":"unbanish","feature":"core.morph"}
```

### 25.4 CLI
```bash
goblin banish <feature_id> --reason "<text>"   # add/replace entry
goblin unbanish <feature_id>                   # remove entry
goblin banish --list                           # show active bans
```

**Notes:**
- `--reason` is required for `banish` to promote transparency.
- All commands operate on `.goblin.banish.toml` in the repo root.
- Each command appends an entry to `dist/banish.log` (see §25.3).

### 25.5 Compiler & Tooling Behavior
On any compile/run/lint step, Goblin loads `.goblin.banish.toml`.

If the AST / resolved symbols contain a banished feature, emit `BanishError` with source location.

LSP/IDE surfaces the same diagnostics in-editor.

**Banner Notice (project status)**: When any banishments exist, Goblin prints a one-liner on CLI startup:
```
⚠ This project has N banished features (run `goblin banish --list`).
```

### 25.6 Errors
Add to §18 (Errors & Warnings):

**BanishError**: Thrown when code uses a banished feature.

Message style (Goblin voice allowed):
```
🚫 Feature '{feature}' has been banished from this project (line {line}, col {col}).
    Reason: {reason}
    Unbanish with: goblin unbanish {feature}
```

**Examples:**
```goblin
morph(book, Card, apply_discount(10%))
/// → BanishError: Feature 'core.morph' has been banished...
```

### 25.7 Self-Protection & Non-banishable Items
Certain core guarantees cannot be banished:

- The banish mechanism itself (banish, config loading, diagnostics).
- Core sandbox/permissions and determinism gates.
- Money safety invariants (e.g., "/ on money is illegal").
- Lockfile integrity checks.

Attempting to banish these emits a config-time `ValueError`:
```
"The goblins won't let you banish '{feature}'"
```

### 25.8 MVP Constraints (v1)
To keep the first release simple and reliable:

- Project-local only (no user-global overlays).
- Hard errors only (no warn-only/severity levels).
- Flat list of banishes (feature, reason).
- No dependency scanning (rules apply to app code).
- No expiry/version gates (manual unbanish when you're ready).

**Future (non-MVP) ideas**: severity levels, version-gated unbanish (e.g., "until ≥1.6"), dependency scope (deps|all), presets, and advisory imports (GQA).

### 25.9 Examples

**Banish morph for safety:**
```bash
goblin banish core.morph --reason "Edge-case bug; waiting for fix"
```

**Creative constraint:**
```bash
goblin banish op.pipe_join --reason "Practice explicit string building"
```

**List:**
```bash
goblin banish --list
# core.morph  — Edge-case bug; waiting for fix
# op.pipe_join — Practice explicit string building
```

**Unbanish:**
```bash
goblin unbanish core.morph
```

---

**Design note**: Goblins banish things. This feature is the emergency brake: rarely used, always ready. It favors safety, clarity, and auditability without changing program semantics or requiring language patches.
# 26. HTML Glam (v1.0)

The HTML Glam renders HTML/CSS from Goblin data using the same template (`::`) ergonomics you already use for cards, products, etc. It keeps business logic in Goblin and outsources HTML‑specific concerns (escaping, head metadata, assets, layouts, partials) to a deterministic, sandboxed glam.

You typically load it like:

```goblin
use html@^1.0 as web
```
(Examples below use `html::...` in signatures; feel free to alias to `web::...` in your scripts.)

## 26.1 Core Concepts

**Goblin templates in, HTML out.** You pass data maps or template records built with `::`. The glam returns strings (HTML) or writes files to `dist/` per permissions.

**Auto‑escape by default.** Text nodes and attributes are HTML‑escaped unless explicitly marked raw.

**Composable.** Use `render_many` for arrays, `layout` for full pages, and `partials` to DRY up shared fragments.

**Deterministic.** FS‑only by default; no network. Works in `--deterministic` builds.

## 26.2 Contracts (Public API)

```goblin
/// Render a single node or component to HTML (string)
html::render(tpl: map, data: map|nil = nil, opts: map|nil = nil) -> string

/// Render many: map each element through tpl and join
html::render_many(tpl: map, rows: array<map>, opts: map|nil = nil) -> string

/// Return a named partial's rendered HTML (string)
html::partial(name: string, data: map|nil = nil) -> string

/// Wrap content with a layout (doctype, head/meta, body)
html::layout(wrapper: string|map, opts: map = {}) -> string

/// Declare / write CSS with optional fingerprinting; returns the href
html::write_css(path: string, theme: string|map = "default", opts: map|nil = nil) -> string

/// Register / fingerprint an asset (css/js/img). Returns href string.
html::asset(path: string, opts: map|nil = nil) -> string

/// Write a rendered HTML string to a file under dist/
html::write_static(path: string, html: string, opts: map|nil = nil) -> nil

/// Escape helpers
html::raw(s: string) -> string         /// mark as safe (opt‑in)
html::esc(s: string) -> string         /// force escape
```

**Common `opts` keys:**
- `join: string` (`render_many` joiner; default `""`)
- `minify: bool` (`layout` / `render`; default `false`)
- `indent: int` (pretty print; default `2` when not minifying)
- `attrs: map<string,string|bool>` (extra attributes for a node)
- `lang: string` (`layout` `<html lang="">"; default `"en"`)
- `meta: bool|map` (control metadata injection; see §26.8)
- `title: string` (`layout` `<title>`)
- `css: array<string>` (`layout` CSS links; paths go through `asset()`)

## 26.3 Data Shapes (What `tpl` Looks Like)

You can pass either a canonical html node map or your own domain template that your renderer understands.

**Canonical node map (lowest level):**
```goblin
node = {
  tag: "div",
  class: "card",          /// optional
  id: "product-123",      /// optional
  attrs: { "data-stock": "7", disabled: false },  /// bool true prints key only
  content: "<p>safe html or nested render output</p>"  /// string (already rendered)
}
html::render(node)
```

**Domain template (preferred):**
```goblin
@card = id: "{id}" :: title: "{title}" :: price: "{price}" :: stock: 0
html::render(@card, product)     /// renderer for @card composes canonical nodes
```

Glam implementations SHOULD ship built‑ins for common nodes (`div`, `h1..h6`, `p`, `img`, `a`, `button`) to keep templates concise.

## 26.4 Escaping & Safety

**Auto‑escape:** text and attribute values are escaped (`& < > " '`) by default.

`html::raw(s)` opts out — use sparingly and only for trusted content.

`html::esc(s)` forces escaping when you're unsure.

**Errors:**
- `ValueError("untrusted raw content")` when glam is configured to forbid `raw()` in deterministic mode.

## 26.5 Partials

Partials are small HTML fragments resolved by name.

**Resolution order (first match wins):**
1. **Project overrides:** `partials/` in your project root
   - Example: `partials/meta/og_tags.html`, `partials/head.html`, `partials/footer.html`
2. **Glam defaults:** `glams/html/partials/...`

**Load & render:**
```goblin
head = html::partial("head", { title: "Shop" })
badge = html::partial("badge", { type: "low", text: "Low stock" })
```

Partials can interpolate `{vars}` from data and may nest other partials.

## 26.6 Assets & CSS

`html::asset("styles.css")` → returns fingerprinted href like `/assets/styles.4f9a7.css`

`html::write_css("dist/styles.css", "shop-theme")` writes CSS and returns href

Both register links so `layout()` can auto‑inject `<link rel="stylesheet" ...>` tags

**`opts` for `write_css` / `asset`:**
- `fingerprint: bool` (default `true`)
- `subdir: string` (default `"assets"`)
- `copy: bool` (for asset to copy inputs under `dist/`; default `true`)

## 26.7 Permissions (Manifest)

The html glam ships with FS‑only permissions:

```yaml
# glams/html/glam.yaml (excerpt)
requires:
  fs: ["dist/","assets/","partials/"]
permissions:
  mode: "fs"
```

Writes outside `dist/` → `PermissionError`. In `--deterministic` builds, behavior is unchanged; all paths must be inside allowlists.

## 26.8 Metadata Injection (Layouts + Meta Partials)

`html::layout(wrapper, opts)` wraps your content and injects sane defaults:

```html
<!DOCTYPE html>
<html lang="{lang}">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{opts.title or "Untitled"}</title>
  {css_links}
  {meta_tags}
</head>
<body>
  {content}
</body>
</html>
```

**CSS links:** collected from `opts.css` and previously registered assets.

**Meta tags:** concatenation of meta partials loaded from:
- **Project:** `partials/meta/*.html`
- **Glam defaults:** `glams/html/partials/meta/*.html`

Disable with `opts.meta: false`, or pass a map to control which meta partials to include:

```goblin
html::layout(page, { title: "Shop", meta: { include: ["og_tags","twitter_card"], exclude:["favicon"] } })
```

## 26.9 Full Example — "Adventurer's Emporium"

**Goal:** Render a product grid page, with auto metadata, CSS links, badges, and buy button behavior. Clean business logic in `.gbln`; HTML fuss handled by the glam.

```goblin
# shop.gbln
set @policy site_default
use html@^1.0 as web

/// --- Data ---------------------------------------------------------------
products = [
  { id: "p1", title: "Magic Sword",    price: $29.99, stock: 8 },
  { id: "p2", title: "Healing Potion", price: $5.50,  stock: 2 },
  { id: "p3", title: "Dragon Shield",  price: $89.00, stock: 0 }
]

/// --- Templates (domain-level) -------------------------------------------
@page = title: "{title}" :: css: ["styles.css"] :: content: ""

@card = id: "{id}" :: title: "{title}" :: price: "{price}" :: stock: 0

/// --- Renderers ----------------------------------------------------------
/// Return HTML for a product card. Inside we build canonical nodes and let glam render.
fn render_card(p)
    /// title
    title_node = { tag: "h2", content: web::esc(p.title) }

    /// price (format money as string; keep business rules here)
    price_str = "$" | fmt(float(p.price), ",.2f")
    price_node = { tag: "p", class: "price", content: web::esc(price_str) }

    /// badges
    badge_html = judge
        p.stock == 0: web::partial("badge", { type: "soldout", text: "Sold out" })
        p.stock < 5:  web::partial("badge", { type: "low",     text: "Low stock" })
        else: ""

    /// button
    btn_attrs = judge
        p.stock == 0: { type: "submit", disabled: true }
        else:         { type: "submit" }
    btn_node = { tag: "button", attrs: btn_attrs, content: "Buy Now" }

    /// card container
    card_node = {
      tag: "div",
      class: "card",
      id: p.id,
      content: 
        web::render(title_node) |
        web::render(price_node) |
        badge_html |
        web::render(btn_node)
    }

    web::render(card_node)
end

/// --- Page assembly ------------------------------------------------------
cards = []
for p in products
    cards.push(render_card(p))
end

grid_node = { tag: "div", class: "grid", content: join(cards, "") }
grid_html = web::render(grid_node)

page = page: title: "Adventurer's Emporium" :: css: ["styles.css"] :: content: grid_html

html_out = web::layout(
  web::partial("base"),              /// wrapper (can be a partial or a map)
  { title: page.title, css: page.css, content: page.content }
)

/// --- Assets & output ----------------------------------------------------
href_css = web::write_css("dist/styles.css", "shop-theme")   /// returns href; also auto-linked by layout

mkdirp("dist")
web::write_static("dist/index.html", html_out)
say "Wrote dist/index.html"
```

**`partials/` (project overrides are optional):**

```
partials/
  base.html                 # optional: outer shell (if not provided, glam uses its default)
  badge.html                # optional; otherwise use glam default
  meta/
    og_tags.html            # optional meta fragment
    twitter_card.html       # optional meta fragment
    favicon.html            # optional meta fragment
```

**`badge.html` example (project override):**

```html
<span class="badge {type}">{text}</span>
```

**Glam default CSS (theme) example (implementation-defined):**

`web::write_css("dist/styles.css", "shop-theme")` writes a minimal, dark UI:

```css
:root { font-family: ui-sans-serif, system-ui, -apple-system, "Segoe UI"; }
body { margin: 0; padding: 2rem; background: #0b0f14; color: #e6edf3; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(260px,1fr)); gap: 1rem; }
.card { background: #10161d; border: 1px solid #1f2937; border-radius: 16px; padding: 1rem; }
.card h2 { margin: 0 0 .5rem 0; font-size: 1.125rem; }
.price { margin: .25rem 0 1rem 0; font-weight: 600; }
.badge { display: inline-block; margin-right: .5rem; padding: .125rem .5rem; border-radius: 999px; font-size: .75rem; }
.badge.low { background: #332700; color: #ffd166; border: 1px solid #ffcc4d; }
.badge.soldout { background: #2a0e12; color: #ff6b6b; border: 1px solid #ff6b6b; }
button { padding: .5rem .75rem; border-radius: 10px; border: 1px solid #334155; background: #0f172a; color: #e6edf3; }
button[disabled] { opacity: .5; cursor: not-allowed; }
```

## 26.10 Errors & Warnings

- **`ValueError`** — bad node shape, invalid partial name, malformed attrs
- **`TypeError`** — wrong argument types for API
- **`PermissionError`** — attempts to write outside allowlisted paths
- **`GlamError("html", cap, cause)`** — any unexpected glam failure
- **`UnusedHtmlWarning`** — glam MAY warn if you compute HTML and never use/write it (lint‑style)

## 26.11 Notes & Best Practices

- Keep money math in Goblin (§10–§11). Pass strings to HTML only at render time.
- Prefer partials for boilerplate head/meta and badges/footers.
- Use `render_many` for clean list rendering without explicit loops when you like, but loops are fine when conditional glue is needed.
- Only use `html::raw` for trusted fragments (e.g., sanitized CMS).

With this, you can keep `.gbln` scripts clean, predictable, and auditable — while shipping production‑ready HTML with zero repetition.

---

## 27. Policies — Project "Loadouts"

### 27.1 Purpose
Policies are named "loadouts" that centralize behavior for key features. Define them once, then apply them:
- Project‑wide (via policies.gbln site default)
- Per file (header)
- Inline (within any block)

Policies replace scattered directives. Environment/config knobs (e.g., currency map, time server) stay in goblin.config.yaml; policies can override them selectively.

### 27.2 Where Policies Live
Create a special, auto‑loaded file at the project root:
```
policies.gbln
```

It's always available (no import). Define named templates there using standard template syntax; nested maps/arrays are allowed on the RHS of fields.

```goblin
/// policies.gbln
@policy = name: "{name}" :: money: {} :: modules: {} :: strings: {} :: datetime: {}

@policy = "site_default"
    money:   { currency: "USD", precision: 2, policy: "truncate" }
    modules: { mode: "expose" }      /// expose | vault
    strings: { trim: true, strip_html: false, escape: false }
    datetime:{ prefer_trusted: false, policy: "warn" }  /// warn | strict | allow

@policy = "strict_money"
    money:   { currency: "USD", precision: 2, policy: "strict" }

@policy = "sealed_mods"
    modules: { mode: "vault" }
```

### 27.3 Applying Policies
Policies are applied with a single statement:
```goblin
set @policy site_default
```

**Scopes:**
- **File header**: first non‑blank/comment statement → applies file‑wide.
- **Inline**: anywhere in code; applies to the current block and inner scopes.

**Precedence**: inline > file > project default (either "site_default" or the first policy declared with default: true, if you add that convention later).

**Partial fills**: Any key not set in a policy inherits from project config (goblin.config.yaml). Nothing is silently guessed.

### 27.4 Categories & Fields

#### 27.4.1 Money
```goblin
money: {
  currency: "USD",            /// default currency (string ISO code)
  precision: 2,               /// ≥ 0
  policy: "truncate",         /// truncate | warn | strict | defer (same semantics as §10)
  compat: {                   /// compatibility mode for external/broken systems
    mode: "none",              /// none | round_per_op | round_final | bankers_round | truncate_display
    track_theft: true,         /// maintain parallel "theft ledger" for discrepancies
    shame_level: "silent"      /// silent | educational | passive_aggressive | brutal
                               /// (lost-money variants allowed for passive_aggressive/brutal)
  }
}
```

**Behavior:**
- Applies to money construction, promotion, *, tax/with_tax, convert, glam return values that are money.
- Policy overrides default money behavior from goblin.config.yaml; unset fields inherit from config.
- Compat mode affects ONLY how amounts are emitted to external systems (exports, invoices, APIs). Internal math always uses perfect Goblin math.
- When compat.mode != "none", Goblin tracks both:
    - Correct Ledger (pure Goblin math)
    - Broken Ledger (external system's result per compat.mode)
  Differences are recorded in theft ledger (even if money is lost instead of gained).

**Shame Level Semantics:**
- `silent` — emit no commentary; still logs theft ledger internally if track_theft = true.
- `educational` — provide step-by-step breakdown of calculations for debugging/teaching.
- `passive_aggressive` — provide summary of discrepancy with light snark.
- `brutal` — provide aggressively snarky discrepancy report.
- lost-money variants — passive_aggressive/brutal text adapted for when compat mode causes the *user* to lose money instead of gain.

**Shame Level Examples:**

`shame_level: "passive_aggressive"`
```
# ═══════════════════════════════════════════════════════════════
# GOBLIN PRECISION AUDIT
# ═══════════════════════════════════════════════════════════════
# Correct total: USD 1,247.66 (mathematically accurate)
# Your total: USD 1,247.83 (rounded to match your expectations)
# Discrepancy: USD 0.17 (tracked in goblin ledger)
#
# Note: Your system's rounding caused this difference.
# The goblins maintain perfect records for your reference.
# 🧌 Where Every Cent Counts
# ═══════════════════════════════════════════════════════════════
```

`shame_level: "brutal"`
```
# ═══════════════════════════════════════════════════════════════
# 🧌 GOBLIN FINANCIAL CRIMES UNIT REPORT 🧌
# ═══════════════════════════════════════════════════════════════
# CORRECT AMOUNT: USD 1,247.66 (what honest math produces)
# YOUR AMOUNT: USD 1,247.83 (what your broken system thinks)
# THEFT AMOUNT: USD 0.17 (you ripped someone off)
#
# 🚨 FINANCIAL MALPRACTICE DETECTED 🚨
# Your rounding errors have resulted in USD 0.17 being stolen
# from someone. The goblins are disappointed in your life choices.
# 
# Maybe consider upgrading from "calculator math" to "actual math"?
# We'll be here with perfect arithmetic when you're ready to evolve.
#
# Sincerely,
# The Goblin Financial Accuracy Department
# "We count coins so you don't have to steal them"
# ═══════════════════════════════════════════════════════════════
```

`shame_level: "passive_aggressive"` (Lost Money)
```
# ═══════════════════════════════════════════════════════════════
# GOBLIN PRECISION AUDIT
# ═══════════════════════════════════════════════════════════════
# Correct total: USD 1,247.83 (mathematically accurate)
# Your total: USD 1,247.66 (rounded to match your expectations)
# Money lost: USD 0.17 (vanished into the rounding void)
#
# Note: Your system's rounding caused you to lose money.
# The goblins tried to warn you, but here we are.
# 🧌 Where Every Cent Counts (Even The Ones You Lose)
# ═══════════════════════════════════════════════════════════════
```

`shame_level: "brutal"` (Lost Money)
```
# ═══════════════════════════════════════════════════════════════
# 🧌 GOBLIN FINANCIAL INCOMPETENCE REPORT 🧌
# ═══════════════════════════════════════════════════════════════
# CORRECT AMOUNT: USD 1,247.83 (what competent math produces)
# YOUR AMOUNT: USD 1,247.66 (what your broken system lost)
# MONEY EVAPORATED: USD 0.17 (poof! gone forever!)
#
# 💸 FINANCIAL SELF-HARM DETECTED 💸
# Congratulations! Your rounding errors just made USD 0.17 
# disappear into the mathematical void. The goblins would cry,
# but we're too busy counting our precisely-tracked coins.
#
# Pro tip: Money doesn't usually vanish by itself.
# That takes special incompetence that only humans can achieve.
#
# Tragically yours,
# The Goblin "We Told You So" Department  
# "Watching humans lose money since 2025"
# ═══════════════════════════════════════════════════════════════
```

**Logging Note:**
When `compat.track_theft = true`, Goblin always records the theft ledger internally, even if `shame_level = "silent"`.  
By default, this is emitted as a structured log entry at `warn` level on export, containing:
  - correct_amount
  - broken_amount
  - discrepancy
  - mode
  - timestamp

Logging level is configurable via goblin.config.yaml. This ensures project maintainers are aware of all discrepancies, even when suppressed in output sent to external systems.

#### 27.4.2 Modules
```goblin
modules: {
  mode: "expose"       /// expose | vault
}
```

Mirrors module visibility (§28). If a file sets modules.mode, it behaves as if it had a pragma header (see §28).

**Namespacing & calls**: importing uses `import "./path" as X` and call sites use `X::symbol` (module) and `glam::symbol` (glam); the namespaces don't collide.

**Errors (visibility)**:
- Attempt to import a vault file or call a vault symbol → `ModuleVisibilityError`.
- If a policy sets a file to vault and you still try to import → `PolicyVisibilityError`.

#### 27.4.3 Strings
```goblin
strings: {
  trim: true,          /// l/r trim for inputs to selected helpers
  strip_html: false,   /// tag removal for designated surfaces
  escape: false        /// conservative escaping for non-HTML sinks
}
```

Hints honored by core helpers and glams where sensible. HTML glam still auto‑escapes; escape:true is additive for non‑HTML sinks.

#### 27.4.4 Datetime
```goblin
datetime: {
  prefer_trusted: false,  /// when true, now()/today() prefer trusted chain
  policy: "warn",         /// strict | warn | allow (as in §15.3)
  tz: nil                 /// override default time zone (string); nil → config
}
```

**Config fallback**: trusted‑time URL, TTLs, HMAC key, etc., stay in goblin.config.yaml.

### 27.5 Syntax Notes
- **Nested structures in templates**: Allowed on RHS: `{ … }` and `[ … ]` (same literals as §6).
- `set @policy …` is a statement (no value). Re‑applying later replaces the current effective policy for subsequent statements in that scope.
- Linters should warn if a file has both a `# @module …` pragma and a conflicting `modules.mode` via policy.

### 27.6 Errors
- `PolicyNotFoundError("name")` — unknown policy name in set @policy.
- `PolicyValueError(path, reason)` — invalid field (e.g., negative precision).
- `PolicyScopeError` — header set @policy is not first statement when a strict build/lint requires it.
- `PolicyVisibilityError` — import/use blocked by an active policy (e.g., modules.mode: "vault").

### 27.7 Examples

**Quick before/after:**

Before:
```goblin
default money USD precision: 2 policy: truncate
price = $80
say price + 10%
```

After:
```goblin
set @policy site_default
price = $80
say price + 10%
```

**Inline tighten:**
```goblin
set @policy strict_money
tax_amount = tax($405.95, 8.25%)   /// raises in strict if sub-precision appears
```

**Modules via policy:**
```goblin
set @policy sealed_mods    /// this file acts like # @module vault
```

---

## 28. Modules — Project‑Local Imports

### 28.1 Purpose
Modules let you split code across files and reuse it without glam. They're project‑local, namespaced, and governed by the Modules policy (see §27). You can run in "open by default" mode or lock things down and explicitly expose only what you need.

### 28.2 Import basics
- Any .gbln file can be imported (no special folders).
- Paths resolve relative to the importing file.
- The .gbln suffix is optional.
- Each import requires an alias; access via Alias::name.

```goblin
import "./helpers" as H
say H::slug("Deviant Moon")
```

Namespace access: `Alias::symbol` (keep `.` for method/property access only).

### 28.3 Visibility via policy (open vs. locked)
Visibility is driven by the active Modules policy:
- **expose** (easy mode): top‑level declarations are importable unless marked `vault`.
- **vault** (locked mode): nothing is importable unless marked `expose`.

You set policy the same way as elsewhere:
- **Project‑wide default** in policies.gbln (e.g., `modules: { mode: "expose" }` or `modules: { mode: "vault" }`).
- **File‑level**: first non‑comment line
  ```goblin
  set @policy my_modules_policy
  ```
- **Scoped/inline**:
  ```goblin
  fn build()
      set @policy locked_modules   /// modules: { mode: "vault" }
      ...
  end
  ```

**Precedence**: inner scope > file header > project default.

**Errors**:
If a file is effectively vault because of policy and you try to import a symbol not marked expose, that import fails. Use error taxonomy in §28.8.

### 28.4 Symbol‑level modifiers
Independently of file policy, mark specific declarations:
```goblin
expose fn new_id() = uuid()            /// importable
vault  fn seed()   = 12345              /// never importable

expose class Product = title: "{t}" :: price: $0
vault  enum Mode
    A
    B
end

expose @row = title: "{t}" :: price: "{p}"
```

**Rules:**
- In **expose** files: everything is importable except symbols marked `vault`.
- In **vault** files: nothing is importable except symbols marked `expose`.
- Conflicting redeclarations (same name with different visibility) → `ModuleVisibilityError` at definition time.

### 28.5 Execution & caching
- A module's top‑level code executes once on its first import; subsequent imports use the cached exports.
- Side‑effects follow sandbox rules; in `--deterministic` builds, disallowed FS/NET fail as usual.

### 28.6 Aliases & collisions
- Duplicate `import … as Alias` in one file → `ModuleNameConflictError`.
- Duplicate symbol definitions inside a module → normal duplicate‑definition error at load time.

### 28.7 Cycles (MVP)
Import cycles are disallowed in v1.5. Detecting A → B → A raises:
```
ModuleCycleError("A <-> B")
```

(A future release may relax this with deferred bindings.)

### 28.8 Interop (modules vs. glam)
- **Modules**: `import "./x" as X` → `X::symbol`.
- **Glam**: `use glam@ver as g` and `via g::cap`.

Namespaces won't collide; both use `Alias::symbol`, but aliases originate from different mechanisms.

### 28.9 Errors
- `ModuleNotFoundError` — bad path or unreadable file.
- `ModuleNameConflictError` — duplicate alias in the importer.
- `ModuleCycleError` — cyclic import detected.
- `ModuleVisibilityError` — symbol hidden by its declaration (vault) or not exposed under a vault file.
- `PolicyVisibilityError` — import denied because the active policy makes the target effectively vault and the symbol isn't expose.

(Goblin‑flair messages allowed per §18.)

### 28.10 Examples

#### A. Open by default (easy reuse)
**policies.gbln**
```goblin
@policy = "easy_modules"
    modules: { mode: "expose" }
```

**strings.gbln**
```goblin
fn slug(s) = s.lower().replace(" ", "-")     /// importable (expose file)
vault fn only_here() = 7                     /// force hidden
```

**post.gbln**
```goblin
set @policy easy_modules
import "./strings" as S

say S::slug("Deviant Moon")    /// "deviant-moon"
S::only_here()                 /// ModuleVisibilityError
```

#### B. Locked file with explicit exposes
**policies.gbln**
```goblin
@policy = "locked_modules"
    modules: { mode: "vault" }
```

**ids.gbln**
```goblin
set @policy locked_modules

expose fn new_id() = uuid()    /// allowed to import
fn seed() = 12345              /// hidden (implicit vault under file vault)
```

**main.gbln**
```goblin
import "./ids" as IDs
say IDs::new_id()
IDs::seed()                    /// ModuleVisibilityError (or PolicyVisibilityError if policy blocks file)
```

#### C. Mixed sections
**tools.gbln**
```goblin
set @policy easy_modules       /// expose section
fn slug(s) = s.lower().replace(" ", "-")

set @policy locked_modules     /// vault section
expose fn checksum(b) = hash(b, "sha256")
fn secret() = "g0b1in"
```

Importer can use `slug` and `checksum`, not `secret`.

### 28.11 Reserved words (delta)
Add to §19:
```
import, expose, vault
```

(Keep as contextual inside import statements. Names in strings/fields are unaffected.)

---
29. Blob Type & Binary Data
29.1 Purpose
The blob type handles raw binary data for images, files, cryptographic operations, and binary protocols. Unlike strings (UTF-8), blobs store arbitrary bytes.
29.2 Construction
goblin/// From file
data = read_bytes("image.png")

/// Empty blob
empty = blob()

/// From encodings
pdf_data = from_base64("JVBERi0xLjQK...")
signature = from_hex("deadbeef1234567890abcdef")

/// From UTF-8 string
utf8_bytes = blob("Hello, world!")

29.3 Operations
goblin/// Length and slicing
len(data), data.len()
data[0:100]     /// first 100 bytes
data[10:]       /// from byte 10 to end
data[:-4]       /// all but last 4 bytes

 /// Concatenation
  blob_concat(a, b)   /// explicit helper (canonical)
  a + b               /// operator overload (exactly equivalent to blob_concat)

29.4 Encoding Conversions
goblin/// Base64 (RFC 4648)
to_base64(data) → string
from_base64(s)  → blob (ValueError if invalid)

/// Hexadecimal (lowercase output, case-insensitive input)
to_hex(data)    → string  
from_hex(s)     → blob (ValueError if invalid)

/// UTF-8 (strict)
str(blob_data)  → string (ValueError if invalid UTF-8)
blob(text)      → blob
29.5 JSON Serialization
Default: blobs are not auto-serialized.
Options:
goblinwrite_json(path, value, { blob: "base64" | "hex" | "error" = "error" })
read_json(path, { blob: "off" | "base64" | "hex" | "auto" = "off" })
29.6 Examples
goblin/// Read image, encode for web
img = read_bytes("logo.png")
data_url = "data:image/png;base64," | to_base64(img)

/// Binary protocol parsing
packet = from_hex("deadbeef00010203")
header = packet[0:4]        /// first 4 bytes
payload = packet[4:]        /// remaining bytes

/// File type detection
if is_binary("document.pdf")
    content = read_bytes("document.pdf")
else
    content = blob(read_text("document.txt"))
end

PATCH 6: NEW SECTION 30 - Insert after Section 29 (Blob Type)
30. Cryptographic Hashing & HMAC
30.1 Core Functions
goblinhash(data: string|blob, algorithm: string = "sha256") → string
hmac(data: string|blob, key: string|blob, algorithm: string = "sha256") → string
Supported algorithms:

"sha256" (default, recommended)
"sha512"
"sha1" (legacy, emits warning)
"md5" (legacy, emits strong warning)

Output is lowercase hex string.
30.2 Use Cases
goblin/// File integrity verification
content = read_bytes("document.pdf")
checksum = hash(content)
say "SHA256: {checksum}"

/// API request signing
payload = json_stringify(order_data)
api_key = "your-secret-key"
signature = hmac(payload, api_key)
headers = { "X-Signature": "sha256=" | signature }

/// File deduplication
files_seen = {}
for path in glob("**/*.jpg")
    h = hash(read_bytes(path))
    if files_seen[h]
        say "Duplicate: {path} = {files_seen[h]}"
    else
        files_seen[h] = path
    end
end
30.3 Security Notes

Not for passwords: Use proper password hashing (bcrypt, scrypt, Argon2) via security glam
Timing attacks: Hash comparison is not constant-time
Key management: Keep HMAC keys secure; rotate regularly

30.4 Algorithm Warnings
goblin/// Emits: "SHA1 is cryptographically weak; consider SHA256"
legacy_hash = hash(data, "sha1")

/// Emits: "MD5 is broken for security; use only for non-cryptographic checksums"
checksum = hash(data, "md5")

30.5 Streaming API (incremental hashing/HMAC)

goblin
# Hash (incremental)
h = hash.start("sha256")
h.update(part1)                /// part1: blob|string
h.update(part2)
digest = h.finish()            /// digest: lowercase hex string

# HMAC (incremental)
s = hmac.start(key: "secret", algorithm: "sha256")
s.update(part1)
s.update(part2)
sig = s.finish()               /// signature: lowercase hex string

Rules:
- .start(algo) returns a stateful context; .update(x) accepts blob or string; .finish() returns lowercase hex.
- Streaming output must match one-shot hash(...) / hmac(...) for the same data.
- Provide golden-vector tests for streaming and one-shot forms (identical results).

  31. Runes (v1.2, KIC by default)
31.0 Lore

The goblins hide their steps inside the message.
To the untrained eye it’s just scratches; to a goblin it’s a map.

31.1 Purpose

runes is a built-in, symmetric, lore-flavored cipher for quick secrecy and puzzles.
Default (v1.2): the movement key is hidden inside the ciphertext via Key-in-Ciphertext (KIC).

Runes is obfuscation-first. For strong cryptography, wrap with HMAC or AEAD (see §31.10).

31.2 Grid

Fixed 6×6 “banded” grid, wraparound moves:

Row0: A  B  D  E  F  G
Row1: 0  1  2  3  4  Z
Row2: H  I  J  K  L  M
Row3: 5  6  7  8  9  .
Row4: N  O  P  Q  R  S
Row5: T  U  V  W  X  Y

31.3 Key

Key = list of (direction, distance) pairs (length 1–6).

Directions: 0..8 (0=stay, 1=N, 2=NE, …, 8=NW)

Distance: 0..6 (wraparound)

If a single pair is given (e.g., "1,3"), it repeats for all characters.

Default key if omitted: 1,1;2,1;3,1;4,1;5,1;6,1;7,1;8,1.

(§31.9 allows optional per-character variation via Pack/Seed.)

31.4 Normalization

Plaintext uppercased.

Spaces → . (dot).

Allowed plaintext: A–Z (no C), digits 0–9, dot ..

Default mapping: C → K.
Configurable: e.g., C → S.
Unsupported glyphs are dropped unless a mapping is specified.

31.5 API & CLI
Language

KIC is on by default.

ct = runes.encrypt("meet at dawn", key: "1,3")    /// headerless, key hidden in ct
pt = runes.decrypt(ct)                            /// auto-recovers key


Sealed KIC (passphrase):

ct = runes.encrypt("meet at dawn", key: "1,3", passphrase: "riddleme")
pt = runes.decrypt(ct, passphrase: "riddleme")


Seeded grids (§31.9):

ct = runes.encrypt("meet at dawn", key: "1,3", seed: 87)
pt = runes.decrypt(ct, seed: 87)


Sealed with seed:

ct = runes.encrypt("meet at dawn", key: "1,3", seed: 87, passphrase: "riddleme")
pt = runes.decrypt(ct, seed: 87, passphrase: "riddleme")

CLI

Canonical positional form (short):

# Encrypt: goblin runes.encrypt "<message>" [:: <key>] [:: <seed>]
# Decrypt: goblin runes.decrypt "<ciphertext>" [:: <key>] [:: <seed>]

goblin runes.encrypt "treasure at oak"                   # default key + fixed grid
goblin runes.encrypt "treasure at oak" :: 1,3            # custom key
goblin runes.encrypt "treasure at oak" :: 1,3 :: 87      # custom key + seeded grid
goblin runes.decrypt "T8X@1X1G#3KN4X7"                   # auto-recover key


Long-form flags (--key, --seed, --passphrase, --no-kic) remain valid.

--no-kic reverts to legacy header mode.

31.6 Key-in-Ciphertext (KIC v1)
31.6.1 Marker set

Reserved cycle: @, #, %, _, -, $ (then repeat).

31.6.2 Key serialization

Pairs (d,s) encoded as hex-like digits (0–9, A–F).
Multiple pairs joined with F.

Examples:

"1,3"                → 13
"1,1;2,2;3,1"        → 11F22F31


(Triples from Pack mode (§31.9): d s g → d s <gid in base16>, joined with F.)

31.6.3 Insertion schedule

Walk ciphertext left→right.

After every 3 carrier symbols, insert marker + next key-char.

If carrier ends with key-chars remaining, append them (marker+char) at the tail.

Decoding:

Scan for markers. Each marker+next char = embedded key.

Remove them, reconstruct key, then decrypt cleaned carrier.

Sealed KIC:

Serialize key, AEAD-encrypt with passphrase, embed result via same schedule.

On decode: extract, AEAD-decrypt, recover movement key.

31.7 Golden Test Vectors
31.7.1 Simple pair
Plaintext: HELLO.W0RLD
Key: 1,3 → 13
Base ct: T8XX1GKN4X7
KIC ct:  T8X@1X1G#3KN4X7

31.7.2 Multi-pair
Plaintext: SECRET.PRAETOR  (C mapped → K)
Key: 1,1;2,2;3,1 → 11F22F31
Base ct: .SLXSUMFSTSU6M
KIC ct:  .SL@1XSU#1MFS%FTSU_26M-2$F@3#1

31.8 Pack & Seed Interop

Pack mode (triples) and Seed mode (pairs) serialize normally and embed via KIC.

Seed camouflage: SEED=<seed>|KEY=<key> embedded.

Sealed: key+seed AEAD-encrypted together; decrypt requires passphrase only.

31.9 Seeded Grids

Deterministic infinite grids without packs.

Seed: integer 1..1000 (expandable later).

Base symbols:
A B D E F G 0 1 2 3 4 Z H I J K L M 5 6 7 8 9 . N O P Q R S T U V W X Y

Shuffle: Fisher–Yates with PRNG seeded by:
HMAC-SHA256("RUNES-SEED-v1", seed_int_be)

Reshape into 6×6 grid.

Rules:

Omit seed → fixed grid (§31.2).

Provide seed → both sides must match.

KIC embeds seed if present (camouflage or sealed).

31.10 Safety Banner

Runes is for lore, puzzles, and light secrecy.
For tamper-evident or strong confidentiality, wrap with HMAC or AEAD (e.g., ChaCha20-Poly1305).

31.11 Self-Test

runes.selftest() (and CLI: goblin runes.selftest) validates an implementation:

Fixed grid + KIC

Input: HELLO.W0RLD

Key: 1,3 → 13

Output: T8X@1X1G#3KN4X7

Fixed grid + KIC + multi-pair

Input: SECRET.PRAETOR

Key: 1,1;2,2;3,1 → 11F22F31

Output: .SL@1XSU#1MFS%FTSU_26M-2$F@3#1

Seeded grid (smoke)

Seed: 87, Key: 1,3

Input: MEET.AT.DAWN

Verify: decrypt(encrypt(x, key, seed), seed) == x

Self-test passes if all three succeed.

31.12 Implementation Constants

Marker cycle: @ # % _ - $

KIC cadence: after every 3 symbols, insert marker+char; append remainder.

Key serialization: pairs ds; joined with F.

PRF (seed): HMAC-SHA256("RUNES-SEED-v1" || seed_int_be)

PRF (sealed KIC): PBKDF2-HMAC-SHA256(passphrase, salt="RUNES-KIC-AEAD", iters=100k) → 32-byte key

AEAD: ChaCha20-Poly1305 (reference choice).

Quick CLI Note
goblin runes.encrypt "treasure at oak"


→ quick, reproducible cipher (default key + fixed grid).
Add :: 1,3 for custom key; add :: 87 for seeded grid.

§32 — List Utilities
32.0 Overview

Goblin lists are ordered, 0-based, and homogenous by convention (mixed types allowed but some ops forbid mixing). Random forms respect the global RNG seed (deterministic when provided).

Indexing sugar:

at <I> → element at index I (0-based).

first → index 0.

last → final element.

Mutability conventions:

Non-destructive: pick, shuffle, sort.

Destructive: reap, usurp, replace, insert, add.

Reserved words in this section:

Hard: pick, reap, usurp, len, shuffle, sort, add, insert, replace.

Soft (only recognized inside list verbs): from, at, first, last, to, into, in, with, dups.

32.1 pick — non-destructive selection

Return element(s) from a list without modifying it.

Forms

pick <list>
pick <N> from <list>
pick <N> dups from <list>
pick first from <list>
pick last from <list>
pick at <I> from <list>


Returns

Element for single-slot forms.

List of length N for pick <N> ….

Errors
EmptyPickError | PickCountError | PickIndexError | PickTypeError

Examples

names = ["Alice","Bob","Charlie","Dana"]
say pick names                  /// "Charlie"
say pick 2 from names           /// ["Dana","Alice"]
say pick 5 dups from [1,2,3]    /// [2,2,3,1,2]
say pick first from names       /// "Alice"
say pick at 2 from names        /// "Charlie"

32.2 reap — destructive selection

Remove element(s) and return them. Mutates the list.

Forms

reap from <list>
reap <N> from <list>
reap first|last from <list>
reap at <I> from <list>


Errors
ReapEmptyError | ReapCountError | ReapIndexError | ReapTypeError

32.3 usurp — destructive replace

Replace slot(s) with new values, return (old,new) pairs.

Forms

usurp at <I> in <list> with <V>
usurp from <list> with <V>
usurp <N> from <list> with <Vs>


Errors
UsurpIndexError | UsurpEmptyError | UsurpCountError | UsurpArityError | UsurpTypeError

32.4 replace — overwrite at index

Mutates without return.

Form

replace at <I> in <list> with <V>


Errors
ReplaceIndexError | ReplaceTypeError

32.5 add — append / concat

Forms

add <value> to <list>
add <list2> to <list1>


Errors
AddTypeError

32.6 insert — insert at index

Form

insert <value> at <I> into <list>


Errors
InsertIndexError | InsertTypeError

32.7 len — list length
len <list> → int

32.8 shuffle — random permutation
shuffle <list> → new_list


Errors
ShuffleTypeError

32.9 sort — sorted copy
sort <list> → new_list


Errors
SortTypeError

Rules

Ascending, stable.

Numeric → numeric compare.

String → lexicographic.

Mixed → error.

32.10 Determinism

Random verbs (pick, reap from … (random), usurp from … (random), shuffle) must use the runtime RNG and honor the global seed.

32.11 Range Integration

Ranges (A..B) are list literals. All list verbs apply directly to ranges.

Examples

say pick 1..100
say pick 5 from 1..100
say pick 5 dups from 1..100
say reap from 10..20
say len 50..75
say shuffle 1..6


Notes

Ranges immutable; destructive verbs materialize mutable copy.

len returns B − A + 1.

32.12 Functional Aliases

Readable sugar ≡ call form.

Examples

pick names                ≡ pick(names)
pick 2 from names         ≡ pick_n(names, 2)
pick 5 dups from xs       ≡ pick_n_dups(xs, 5)
pick at 3 from xs         ≡ pick_at(xs, 3)
pick 1..100               ≡ pick_range(1,100)
pick 5 from 1..100        ≡ pick_range_n(1,100,5)

reap from xs              ≡ reap_one(xs)
reap at 2 from xs         ≡ reap_at(xs,2)
usurp at 2 in xs with v   ≡ usurp_at(xs,2,v)
add 3 to xs               ≡ push(xs,3)
insert "B" at 1 into xs   ≡ insert_at(xs,1,"B")

shuffle xs                ≡ shuffle_copy(xs)
sort xs                   ≡ sort_copy(xs)
len xs                    ≡ length(xs)
len 1..100                ≡ length_range(1,100)

32.13 Mini Game Recipes
1. Randomly kill 3 enemies (distinct, removed from play)
enemies = ["goblin","orc","slime","bat","skeleton","bandit"]

killed = reap 3 from enemies
say "💀 Killed: " || killed
say "👣 Remaining: " || enemies

2. Draw cards vs burn random cards
deck = ["A♠","K♠","Q♠","J♠","10♠"]

hand = [reap first from deck, reap first from deck]
say "✋ Hand: " || hand
say "📦 Deck: " || deck

burned = reap 2 from deck
say "🔥 Burned: " || burned
say "📦 Deck now: " || deck

3. Randomly damage N items in inventory
inventory = ["Sword","Shield","Bow","Helmet","Boots"]
replacements = ["Sword (Damaged)","Shield (Cracked)"]

pairs = usurp 2 from inventory with replacements
say pairs
say "🎒 Inventory: " || inventory

4. Random loot without repeats
loot_table = ["gold","gem","potion","map","ring"]
drop = reap from loot_table
say "🎁 Drop: " || drop
say "📜 Loot left: " || loot_table

5. Gacha pulls (allow duplicates)
pool = ["common","common","common","rare","rare","epic"]

pulls = pick 10 dups from pool
say "🧪 Pulls: " || pulls
say "Pool unchanged: " || pool

6. Random party order (initiative)
party = ["Lamar","Tanisha","Marcus","Ingrid"]
initiative = shuffle party
say "🎲 Initiative: " || initiative

7. Trap: remove 1 random item
bag = ["Key","Rope","Torch","Pickaxe"]
lost = reap from bag
say "🕳️ Lost to trap: " || lost
say "🎒 Bag now: " || bag

8. Weapon break (swap-in broken variant)
weapons = ["Dagger","Axe","Spear"]
(old, new) = usurp at 1 in weapons with "Axe (Broken)"
say "💥 " || old || " → " || new
say weapons

9. Event preview vs trigger
events = ["Ambush","Treasure","Merchant","Rest"]

say "Peek: " || pick events
say "Trigger: " || reap from events
say "Queue left: " || events

10. “Cull the horde” with cap
horde = ["A","B","C"]
n = 5
if n > len horde
  n = len horde
end
culled = reap n from horde
say culled, horde


Notes

reap = destructive grab, great for consumable effects.

pick = non-destructive peek/preview.

usurp = destructive swap-in.

Random ops always honor the global seed (--seed 1337 reproducible).

§33 — Divmod
33.0 Overview

Goblin provides a first-class division-with-remainder:

Operator: >> — returns quotient and remainder together.

Function (alias): div_rem(a, b) — identical semantics (optional alias).

Both forms produce a structured pair (q, r) and support destructuring, as used elsewhere in the spec (e.g., q, r = price >> 3). 

33.1 Typing

Defined for integers and fixed-scale numeric types (e.g., money/decimal with integral storage).

Conceptual signature

div_rem : (IntLike a) => (a, a) -> (a, a)   -- optional alias
(>>)     : (IntLike a) => (a, a) -> (a, a)


IntLike includes built-in integers and fixed-scale numerics (money/decimal) where the scale matches (see Errors).

Result is a pair (q, r) of the same numeric kind.

33.2 Semantics

Goblin uses floor division semantics (as shown in §2.2 examples). 

For a, b with b ≠ 0:

q = floor(a / b)

r = a − q*b

Remainder r has the sign of the divisor b.

Invariants

a = q*b + r

0 ≤ |r| < |b|

33.3 Grammar & Precedence

>> is left-associative and sits in the multiplicative tier with * / % (already specified in §2.1/§2.2). 

(No new grammar beyond what §2 provides; this section consolidates semantics and typing.)

33.4 Destructuring & Access

Tuple destructuring is supported:

q, r = 17 >> 5      /// q=3, r=2
q2, r2 = div_rem(17, 5)  /// same result (if alias is enabled)


You can pass the pair through APIs or extract components via pattern matching, as shown in money examples. 

33.5 Money / Fixed-Scale Notes

Divmod works for money when the scale is compatible with the divisor (your money rules show price // 3 and price >> 3; / on money is an error). 

For incompatible scales, emit a typed error (see Errors).

33.6 Errors

DivisionByZero — divisor is zero.

ScaleMismatch — fixed-scale types with incompatible scale/base.

MoneyDivisionError — / used with money (already defined in §10.7). 

(Overflow follows your global integer policy.)

33.7 Examples (canonical)

Numbers (consistent with §2.2):

10.75 / 3  → 3.5833333333
10.75 // 3 → 3
10.75 >> 3 → (3, 1.75)
-7 // 3    → -3
-7 >> 3    → (-3, 2)


Money (consistent with §10.7, §17 Examples):

price = $10.75
q = price // 3      /// USD 3.00
q, r = price >> 3   /// (USD 3.00, USD 1.75)
price / 3           /// MoneyDivisionError


33.8 Conformance (sketch)

Identity: a == (a >> b).0 * b + (a >> b).1 and abs(r) < abs(b)

Sign law: sign(r) == sign(b) or r == 0

Equivalence: div_rem(a,b) (if present) equals a >> b

Negatives: confirm the four (-/+) examples above

Money: valid when scale compatible; / → MoneyDivisionError

§34 — Pipelines, Optional Chaining, and Error Blocks
34.0 Overview

This section introduces three new language features:

Pipeline operator (|>) — left-to-right function chaining.

Optional chaining (?.) — safe navigation for null/undefined.

Structured error handling (attempt / rescue / ensure) — block form exception handling.

34.1 Pipeline Operator (|>)

Meaning:
a |> f(x) desugars to f(a, x).

Associativity:
Left-to-right.

a |> f() |> g(y)      # g(f(a), y)


Precedence:

Binds looser than arithmetic (+, *)

Binds tighter than string joins (| then ||), comparisons, and logical operators

Binds looser than function call, member access, indexing, and optional chaining (?.)

Arity rule:
The RHS must accept the piped value as its first argument. If not → ArityMismatch.

Lambda escape hatch:

val |> (s -> between(lower: 0, upper: s, value: val2))


Null handling:
Pipelines do not short-circuit; they pass null downstream. Combine with ?? or ?. if needed.

34.2 Optional Chaining (?.)

Meaning:
Safely access a property or call a method; if the left-hand side is null or undefined, evaluation short-circuits to null.

a?.b          # null if a is null/undefined
a?.b()?.c     # null if a is null, or if b() is null


Scope of short-circuit:

obj?.prop → skip property read if obj is null/undefined.

obj?.method(args) → skip the entire call if obj is null/undefined (arguments not evaluated in that context).

Chains short-circuit at the first null.

Errors:

Does not swallow exceptions: if the access/call is performed and throws, the error propagates normally.

Applies equally to both null and undefined.

34.3 attempt / rescue / ensure

Form:

result =
  attempt
      risky()
      "ok"
  rescue Timeout as e
      backoff(); "retrying"
  ensure
      close_handles()
  end


Semantics:

Run the attempt block.

If it throws, match the first rescue arm by error type.

ensure always runs, regardless of success/failure.

If no rescue matches, the error propagates after ensure.

Return value = last expression of whichever branch executes.

Features:

Multiple rescue clauses allowed; first match wins.

Bare raise rethrows the current error with its original stack.

No implicit propagation sugar (no Rust-style ?). Explicitly omit rescue or use raise.

34.4 Interactions & Gotchas

Pipelines + Optional Chaining

user
  |> fetch_profile()
  |> (_.address?.city)
  |> (c -> c ?? "Unknown")


Pipelines + Error Blocks
Pipelines don’t catch. Use inside attempt as needed.

attempt
    data |> parse() |> validate()
rescue ParseError
    log("bad input"); raise
ensure
    metrics.increment("parse_attempt")
end


Optional Chaining in rescue/ensure
Works normally:

rescue e
    say e?.message ?? "unknown error"


Method chaining vs pipelines

obj.method1().method2()       # traditional
obj |> method1() |> method2() # pipeline style


Pipelines are preferred when mixing functions and methods, or when long chains improve readability.

34.5 Errors

ArityMismatch — RHS of pipeline cannot accept piped argument.

All other thrown exceptions propagate normally (unless rescued).

?. never raises on null/undefined, always collapses to null.

§35 — Play & Randomization Helpers

35.0 Overview
Goblin bakes in tabletop-style dice, weighted sampling, and frequency helpers. Random operations honor the global RNG seed (deterministic if seeded).

Grammar note:
A new literal form dice_expr is recognized inside roll and roll_detail.
Form:

dice_expr ::= INT "d" INT [("+"|"-") INT]?


Examples: 1d10, 2d6+1, 4d8-2

Outside roll and roll_detail, dice_expr is not valid.

35.1 roll — dice evaluation

Rolls a dice expression and returns the total.

Forms:

roll dice_expr
roll(dice_expr)


Examples:

say roll 2d6+1          /// e.g., 9
r = roll_detail 4d8-2
say r.dice, r.sum       /// [3,8,1,4], 16
say r.total             /// 14


Results:

roll → total only (Int)

roll_detail → record: { dice: [..], sum: int, total: int }

dice: list of raw rolls

sum: pre-modifier sum

total: sum after modifier

Errors:

DICE_PARSE — invalid form (e.g., 2d7q+1)

DICE_BOUNDS — nonpositive dice count or sides

35.2 freq — frequency map

Count elements in a list and return a {value:count} map.

Form:

freq list → map


Examples:

say freq ["a","b","a","c","a","b"]  
/// {"a":3, "b":2, "c":1}

say mode ["orc","goblin","orc","slime","orc","slime"]  
/// ["orc"]

35.3 mode — statistical mode(s)

Return list of most frequent element(s).

Form:

mode list → list


Examples:

say mode ["orc","goblin","orc","slime","orc","slime"]
/// ["orc"]

say mode ["a","b","b","a"]  
/// ["a","b"]     # multimodal

35.4 sample_weighted — weighted random choice

Randomly select items with bias. Accepts either (value, weight) pairs or a weight map.

Forms:

sample_weighted list        # list of (value, weight) pairs
sample_weighted map         # {"value": weight, ...}


Weights:

must be ≥ 0

relative only (no normalization needed)

Examples:

loot = [("potion", 10), ("ring", 3), ("sword", 1)]
say sample_weighted loot

loot_map = {"potion":10, "ring":3, "sword":1}
say sample_weighted loot_map


Errors:

WEIGHT_TYPE — weight not numeric

WEIGHT_EMPTY — no positive weights

35.5 Recipes

Percentile dice (d100):

d1 = roll 1d10    /// 1–10
d2 = roll 1d10    /// 1–10
percent = (d1-1)*10 + (d2-1)   # 0–99
if percent == 0
  percent = 100
end
say "🎲 Percentile: " || percent


Skill check (d10-themed):

check = roll 1d10
if check >= 8
  say "✅ Success!"
else
  say "❌ Fail!"
end


Yahtzee-style:

rolls = roll_detail 5d6
say "🎲 Dice: " || rolls.dice
say "Total: " || rolls.total


Card draws (with reap/shuffle):

deck = ["A♠","K♠","Q♠","J♠","10♠"]
shuffle deck              # permute deck
hand = [reap first from deck, reap first from deck]
say "✋ Hand: " || hand
say "📦 Deck left: " || deck


35.6 Error Codes (summary)

DICE_PARSE

DICE_BOUNDS

WEIGHT_TYPE

WEIGHT_EMPTY

✨ This makes dice rolls native Goblin literals, not strings — so players see results they trust, and engineers get clean syntax.

36. Release Checklist

## ✅ Must-Have for v1.5

### Goblin Core

**gmarks basics** (see §23.9)
- gmarks_filter(prefix) implementation & tests
- Deterministic-build write policy gate + .goblin/gmarks.audit.log

**Blob/JSON interop glue** (see §29.5)
- Ensure JSON/YAML leave blobs alone by default (no auto-encode)
- Example helpers to base64 when needed

**Hashing & HMAC** (see §30)
- hash(data: string|blob, algorithm="sha256") -> string
- hmac(data: string|blob, key: string|blob, algorithm="sha256") -> string
- Supported algos: sha256, sha1, md5 (warn), sha512
- Streaming over blobs; tests with known vectors

### Baseline Glam

- regex glam: regex::test, regex::findall, regex::replace (+ flags)
- fs.glob glam: fs::glob(pattern) -> array<string>, fs::walk(dir, pattern="**/*")
- retry/backoff glam: retry::with_backoff(fn, attempts=3, base_ms=250, jitter=true)
- http glam polish: http::request(method, url, headers={}, body=""), deterministic-mode safeguards

---

## ➕ Optional / v1.5+ (Can Push to First Point Release)

### Core Horde-Readiness

- Pure function runner mode (--stdin / --stdout)
- Warm VM / preload mode (goblin serve --preload)
- Determinism hardening knobs (--seed, block wall clock, etc.)
- Resource limits (--cpu-ms, --mem-mb, --max-steps)
- Correlation & idempotency (request_id, --idempotency-key)
- Structured exit codes
- Audit log enrichment with IDs/keys
- Clock/testing hook (freeze_time)

### Glam / Host-Side Horde-Readiness

- scheduler glam: scheduler::cron(spec, task) (executes scripts/caps on a schedule)
- s3::upload / netlify::deploy glam
- metrics::emit glam
- horde examples (Rust worker pool + Kubernetes YAML)

### Other Optional Core Enhancements

- glam gmark rebalance (CLI) implementation
- Extra blob helpers (beyond base64/hex) if needed for niche formats

---

If we ship just the must-have list, Goblin v1.5 Core will be fully usable, have a clean feature set, and Glam will cover the basics. Then we drop horde-readiness + deploy glam in v1.5.1 or v1.6 without delaying launch.

*[End of Goblin Language Specification v1.5 "Treasure Hoarder" - Refactored]*
