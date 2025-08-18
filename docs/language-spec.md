# Goblin — Language Specification v1.5.3 "Treasure Hoarder"

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

# 23. Banish — Project-Local Feature Blocking

## 23.1 Purpose & Scope

Banish provides projects with a simple, portable safety brake to block specific language features.

* **Scope**: project-local only. Rules live in the repo and travel with it.
* **Effect**: any usage of a banished feature raises `BanishError` at compile time (hard error).
* **Guarantee**: banish never rewrites or alters code — it only blocks.

---

## 23.2 Feature IDs (Namespaces)

Feature IDs are stable strings used by tools and the compiler. They always reference **usage**, not availability.

* `core.<keyword>` — core language constructs (e.g., `core.morph`, `core.judge.inline`)
* `op.<operator>` — operators/forms (e.g., `op.pipe_join`, `op.divmod`, `op.postfix.inc`)
* `type.<type>` — types/literals (e.g., `type.money`, `type.percent`, `type.datetime`)
* `builtin.<function>` — built-ins and std helpers (e.g., `builtin.tax`, `builtin.write_json`)
* `glam.<ns>.<symbol>` — glam capabilities (e.g., `glam.html.raw`, `glam.shopify.product.export`)

---

## 23.3 Config File

Config lives at `.goblin.banish.toml` in the project root. Subdirectories are ignored.

```toml
[[banish]]
feature = "core.morph"
reason  = "Temporary safety: known issue in v1.5.x"

[[banish]]
feature = "op.pipe_join"
reason  = "Creative constraint: prefer explicit concatenation"
```

* **feature** — required string (from §23.2).
* **reason** — required human explanation (shown in errors).

**Audit log**: every banish/unbanish appends JSONL to `dist/banish.log`:

```json
{"ts":"2025-08-13T15:21:08Z","op":"banish","feature":"core.morph","reason":"Temporary safety"}
{"ts":"2025-08-13T15:22:44Z","op":"unbanish","feature":"core.morph"}
```

---

## 23.4 CLI

```goblin
goblin banish <feature_id> --reason "<text>"   /// add or replace an entry
goblin unbanish <feature_id>                   /// remove an entry
goblin banish --list                           /// show active bans
```

**Notes:**

* `--reason` is mandatory for `banish` (promotes transparency).
* All commands operate on `.goblin.banish.toml`.
* Each command appends to `dist/banish.log`.

---

## 23.5 Compiler & Tooling Behavior

On compile/run/lint, Goblin loads `.goblin.banish.toml`.

* If AST/resolved symbols include a banished feature → raise `BanishError` with source location.
* LSP/IDE shows identical diagnostics in-editor.

**Banner notice (status):** When any banishments exist, Goblin prints:

```
⚠ This project has N banished features (run `goblin banish --list`).
```

---

## 23.6 Errors

* **BanishError** — raised when code uses a banished feature.

Message style (Goblin voice allowed):

```text
🚫 Feature '{feature}' has been banished from this project (line {line}, col {col}).
    Reason: {reason}
    Unbanish with: goblin unbanish {feature}
```

**Example:**

```goblin
morph(book, Card, apply_discount(10%))
/// → BanishError: Feature 'core.morph' has been banished…
```

---

## 23.7 Self-Protection — Non-Banishable Features (Exhaustive, v1.5)

The following feature IDs are permanently unbanishable.
Attempting to include one in `.goblin.banish.toml` raises a config-time `ValueError`:

```
"The goblins won't let you banish '{feature}'"
```

### Banish infrastructure & config

```goblin
core.banish                 /// the banish mechanism & diagnostics
core.config                 /// config parsing & enforcement
core.errors                 /// core error classes & raising
core.contracts              /// global contract registry & resolution
core.capability_resolution  /// via/prefer/default dispatch semantics
```

### Sandbox & determinism guarantees

```goblin
core.sandbox                /// FS/NET allowlists, ENV gating
core.determinism            /// --deterministic enforcement
core.rng.seed               /// deterministic RNG seeding
core.trusted_time           /// trusted time chain, verification, cache control
```

### Money safety invariants

```goblin
core.money.invariants       /// conservation/precision/remainder tracking
core.money.policy           /// enforcement of active money policy
core.money.ops.guard        /// illegal ops guard (e.g. "/" on money)
```

### Lockfile & build integrity

```goblin
core.lockfile               /// lockfile parsing & enforcement
core.lockfile.hmac          /// HMAC/signature verification
core.build.reproducibility  /// reproducible build guarantees
```

### Language integrity (syntax, types, reserved words)

```goblin
core.parser                 /// grammar, tokenization, precedence
core.reserved_words         /// canonical reserved words set (§19)
core.type_system            /// base types: int, float, bool, string, money, percent, date, time, datetime, duration, enum
```

### Runtime auditability & security rails

```goblin
core.jsonl_logging          /// standardized JSONL logs around capability calls
core.logging.safety         /// logging of restricted/state-changing actions
core.permissions            /// permission model (FS/NET/ENV)
core.policy.visibility      /// policy-based visibility restrictions (imports, etc.)
```

---

## 23.8 MVP Constraints (v1)

To keep the first release simple and reliable:

* Project-local only (no user-global overlays).
* Hard errors only (no warn-only/severity levels).
* Flat list of banishes (feature + reason).
* No dependency scanning (rules apply to app code only).
* No expiry/version gates (manual unbanish when ready).

**Future (non-MVP) ideas:** severity levels, version-gated unbanish (e.g. “until ≥1.6”), dependency scope (deps|all), presets, advisory imports (GQA).

---

## 23.9 Examples

```goblin
goblin banish core.morph --reason "Edge-case bug; waiting for fix"   /// banish morph for safety
goblin banish op.pipe_join --reason "Practice explicit string building"  /// creative constraint
goblin banish --list   /// show active bans
goblin unbanish core.morph   /// unbanish morph
```
**Design note**: Goblins banish things. This feature is the emergency brake: rarely used, always ready. It favors safety, clarity, and auditability without changing program semantics or requiring language patches.

## 24. Policies — Project "Loadouts"

### 24.1 Purpose

Policies are named "loadouts" that centralize behavior for key features. Define them once, then apply them:

* Project‑wide (via `policies.gbln` site default)
* Per file (header)
* Inline (within any block)

Policies replace scattered directives. Environment/config knobs (e.g., currency map, time server) stay in `goblin.config.yaml`; policies can override them selectively.

### 24.2 Where Policies Live

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

### 24.3 Applying Policies

Policies are applied with a single statement:

```goblin
set @policy site_default
```

**Scopes:**

* **File header**: first non‑blank/comment statement → applies file‑wide.
* **Inline**: anywhere in code; applies to the current block and inner scopes.

**Precedence**: inline > file > project default (either `site_default` or the first policy declared with `default: true`, if that convention is added later).

**Partial fills**: Any key not set in a policy inherits from project config (`goblin.config.yaml`). Nothing is silently guessed.

### 24.4 Categories & Fields

#### 24.4.1 Money

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

* Applies to money construction, promotion, `*`, `tax/with_tax`, `convert`, glam return values that are money.
* Overrides default money behavior from `goblin.config.yaml`; unset fields inherit from config.
* Compat mode affects **only** how amounts are emitted to external systems (exports, invoices, APIs). Internal math always uses perfect Goblin math.
* When compat.mode ≠ `none`, Goblin tracks both:

  * Correct Ledger (pure Goblin math)
  * Broken Ledger (external system's result per compat.mode)
    Differences are recorded in theft ledger (even if money is lost instead of gained).

**Shame Levels:**

* `silent` — no commentary; still logs internally if `track_theft = true`.
* `educational` — step-by-step breakdowns.
* `passive_aggressive` — summary with light snark.
* `brutal` — aggressively snarky discrepancy report.
* Lost-money variants adapt text when compat causes *user* to lose money.

**Logging:** Theft ledger is always recorded internally when `track_theft = true`, even if shame\_level = `silent`. Emitted as structured log entries at `warn` level by default.

#### 24.4.2 Modules

```goblin
modules: {
  mode: "expose"       /// expose | vault
}
```

* Mirrors module visibility (§25). If a file sets `modules.mode`, it behaves as if it had a pragma header.
* Importing uses `import "./path" as X`; call sites use `X::symbol` (module) and `glam::symbol` (glam). Namespaces never collide.

**Errors:**

* Import vault file or call vault symbol → `ModuleVisibilityError`.
* File policy vault + attempted import → `PolicyVisibilityError`.

#### 24.4.3 Strings

```goblin
strings: {
  trim: true,          /// l/r trim for inputs to selected helpers
  strip_html: false,   /// tag removal for designated surfaces
  escape: false        /// conservative escaping for non-HTML sinks
}
```

Hints honored by core helpers and glams where sensible. HTML glam still auto‑escapes; escape\:true is additive for non‑HTML sinks.

#### 24.4.4 Datetime

```goblin
datetime: {
  prefer_trusted: false,  /// when true, now()/today() prefer trusted chain
  policy: "warn",         /// strict | warn | allow (as in §15.3)
  tz: nil                 /// override default time zone (string); nil → config
}
```

Config fallback: trusted‑time URL, TTLs, HMAC key, etc., remain in `goblin.config.yaml`.

### 24.5 Syntax Notes

* Nested structures allowed on RHS (`{ … }`, `[ … ]`).
* `set @policy …` is a statement (no value). Re‑applying replaces current effective policy for subsequent scope.
* Linters should warn if file has both a `# @module …` pragma and a conflicting `modules.mode` via policy.

### 24.6 Errors

* `PolicyNotFoundError("name")` — unknown policy name.
* `PolicyValueError(path, reason)` — invalid field (e.g., negative precision).
* `PolicyScopeError` — header set @policy not first statement when strict build/lint requires.
* `PolicyVisibilityError` — import/use blocked by active policy.

### 24.7 Examples

**Before:**

```goblin
default money USD precision: 2 policy: truncate
price = $80
say price + 10%
```

**After:**

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

# 25. Modules — Project-Local Imports

## 25.1 Purpose

Modules let you split code across files and reuse it without glam. They’re project‑local, namespaced, and governed by the **Modules policy** (§24). You can run in "open by default" mode or lock things down and explicitly expose only what you need.

---

## 25.2 Import Basics

* Any `.gbln` file can be imported (no special folders).
* Paths resolve **relative to the importing file**.
* The `.gbln` suffix is optional.
* Each import **requires an alias**; access via `Alias::name`.

```goblin
import "./helpers" as H
say H::slug("Deviant Moon")
```

Namespace access: `Alias::symbol` (keep `.` for method/property access only).

---

## 25.3 Visibility via Policy (open vs. locked)

Visibility is driven by the active **Modules policy** (§24):

* **expose** (easy mode): top‑level declarations are importable **unless** marked `vault`.
* **vault** (locked mode): nothing is importable **unless** marked `expose`.

Apply policy the same ways as elsewhere:

* **Project‑wide default** in `policies.gbln` (e.g., `modules: { mode: "expose" }` or `modules: { mode: "vault" }`).
* **File‑level**: first non‑comment statement.

```goblin
set @policy my_modules_policy
```

* **Scoped/inline**:

```goblin
fn build()
    set @policy locked_modules   /// modules: { mode: "vault" }
    ...
end
```

**Precedence**: inner scope > file header > project default.

**Errors**: If a file is effectively `vault` because of policy and you try to import a symbol not marked `expose`, that import fails (see §25.9 for error taxonomy).

---

## 25.4 Symbol‑Level Modifiers

Mark specific declarations regardless of file policy:

```goblin
expose fn new_id() = uuid()            /// importable
vault  fn seed()   = 12345             /// never importable

expose class Product = title: "{t}" :: price: $0
vault  enum Mode
    A
    B
end

expose @row = title: "{t}" :: price: "{p}"
```

**Rules:**

* In **expose** files: everything is importable **except** symbols marked `vault`.
* In **vault** files: nothing is importable **except** symbols marked `expose`.
* Conflicting redeclarations (same name with different visibility) → `ModuleVisibilityError` at definition time.

---

## 25.5 Execution & Caching

* A module’s top‑level code **executes once** on its first import; subsequent imports use the cached exports.
* Side‑effects follow sandbox rules; in `--deterministic` builds, disallowed FS/NET fail as usual.

---

## 25.6 Aliases & Collisions

* Duplicate `import … as Alias` in one file → `ModuleNameConflictError`.
* Duplicate symbol definitions inside a module → normal duplicate‑definition error at load time.

---

## 25.7 Cycles (MVP)

Import cycles are disallowed in v1.5. Detecting `A → B → A` raises:

```text
ModuleCycleError("A <-> B")
```

(A future release may relax this with deferred bindings.)

---

## 25.8 Interop (Modules vs. Glam)

* **Modules**: `import "./x" as X` → `X::symbol`.
* **Glam**: `use glam@ver as g` and `via g::cap`.

Namespaces don’t collide; both use `Alias::symbol`, but aliases originate from different mechanisms.

---

## 25.9 Errors

* `ModuleNotFoundError` — bad path or unreadable file.
* `ModuleNameConflictError` — duplicate alias in the importer.
* `ModuleCycleError` — cyclic import detected.
* `ModuleVisibilityError` — symbol hidden by its declaration (`vault`) or not exposed under a `vault` file.
* `PolicyVisibilityError` — import denied because the active policy makes the target effectively `vault` and the symbol isn’t `expose`.

(Goblin‑flair messages allowed per §18.)

---

## 25.10 Examples

### A. Open by default (easy reuse)

**policies.gbln**

```goblin
@policy = "easy_modules"
    modules: { mode: "expose" }
```

**strings.gbln**

```goblin
fn slug(s) = s.lower().replace(" ", "-")     /// importable (expose file)
vault fn only_here() = 7                       /// force hidden
```

**post.gbln**

```goblin
set @policy easy_modules
import "./strings" as S

say S::slug("Deviant Moon")    /// "deviant-moon"
S::only_here()                 /// ModuleVisibilityError
```

### B. Locked file with explicit exposes

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

### C. Mixed sections

**tools.gbln**

```goblin
set @policy easy_modules       /// expose section
fn slug(s) = s.lower().replace(" ", "-")

set @policy locked_modules     /// vault section
expose fn checksum(b) = hash(b, "sha256")
fn secret() = "g0b1in"
```

Importer can use `slug` and `checksum`, not `secret`.

## 26. Blob Type & Binary Data

### 26.1 Purpose

The `blob` type handles raw binary data for images, files, cryptographic operations, and binary protocols. Unlike strings (UTF-8), blobs store arbitrary bytes.

### 26.2 Construction

```goblin
/// From file
data = read_bytes("image.png")

/// Empty blob
empty = blob()

/// From encodings
pdf_data   = from_base64("JVBERi0xLjQK...")
signature  = from_hex("deadbeef1234567890abcdef")

/// From UTF-8 string
utf8_bytes = blob("Hello, world!")
```

### 26.3 Operations

```goblin
/// Length and slicing
len(data), data.len()
data[0:100]     /// first 100 bytes
data[10:]       /// from byte 10 to end
data[:-4]       /// all but last 4 bytes

/// Concatenation
blob_concat(a, b)   /// explicit helper (canonical)
a + b               /// operator overload (equivalent to blob_concat)
```

### 26.4 Encoding Conversions

```goblin
/// Base64 (RFC 4648)
to_base64(data) → string
from_base64(s)  → blob   /// ValueError if invalid

/// Hexadecimal (lowercase output, case-insensitive input)
to_hex(data)    → string
from_hex(s)     → blob   /// ValueError if invalid

/// UTF-8 (strict)
str(blob_data)  → string /// ValueError if invalid UTF-8
blob(text)      → blob
```

### 26.5 JSON Serialization

Default: blobs are not auto-serialized.

Options:

```goblin
write_json(path, value, { blob: "base64" | "hex" | "error" = "error" })
read_json(path, { blob: "off" | "base64" | "hex" | "auto" = "off" })
```

### 26.6 Examples

```goblin
/// Read image, encode for web
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
```
# 27. Cryptographic Hashing & HMAC

## 27.1 Core Functions

```goblin
hash(data: string|blob, algorithm: string = "sha256") → string
hmac(data: string|blob, key: string|blob, algorithm: string = "sha256") → string
```

* **Algorithms supported**:

  * `"sha256"` (default, recommended)
  * `"sha512"`
  * `"sha1"` (legacy; emits warning)
  * `"md5"` (legacy; emits strong warning)

* **Output**: lowercase hex string.

---

## 27.2 Use Cases

```goblin
/// File integrity verification
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
```

---

## 27.3 Security Notes

* **Not for passwords**: Use password hashing (bcrypt, scrypt, Argon2) via security glam.
* **Timing attacks**: Hash comparison is not constant-time.
* **Key management**: Keep HMAC keys secure; rotate regularly.

---

## 27.4 Algorithm Warnings

```goblin
/// Emits: "SHA1 is cryptographically weak; consider SHA256"
legacy_hash = hash(data, "sha1")

/// Emits: "MD5 is broken for security; use only for non-cryptographic checksums"
checksum = hash(data, "md5")
```

---

## 27.5 Streaming API (incremental hashing/HMAC)

```goblin
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
```

**Rules:**

* `.start(algo)` returns a stateful context.
* `.update(x)` accepts blob or string.
* `.finish()` returns lowercase hex.
* Streaming output must match one-shot `hash(...)` / `hmac(...)` for identical data.
* Golden-vector tests required for equivalence of streaming vs. one-shot forms.

  ## 28. Runes (v1.2, KIC by default)

### 28.0 Lore

The goblins hide their steps inside the message. To the untrained eye it’s just scratches; to a goblin it’s a map.

### 28.1 Purpose

`runes` is a built-in, symmetric, lore-flavored cipher for quick secrecy and puzzles. Default (v1.2): the movement key is hidden inside the ciphertext via **Key-in-Ciphertext (KIC)**.

Runes is obfuscation-first. For strong cryptography, wrap with HMAC or AEAD (see §28.10).

### 28.2 Grid

Fixed 6×6 “banded” grid, wraparound moves:

```
Row0: A  B  D  E  F  G
Row1: 0  1  2  3  4  Z
Row2: H  I  J  K  L  M
Row3: 5  6  7  8  9  .
Row4: N  O  P  Q  R  S
Row5: T  U  V  W  X  Y
```

### 28.3 Key

Key = list of (direction, distance) pairs (length 1–6).

* **Directions:** 0..8 (0=stay, 1=N, 2=NE, …, 8=NW)
* **Distance:** 0..6 (wraparound)

If a single pair is given (e.g., `"1,3"`), it repeats for all characters.

Default key if omitted: `1,1;2,1;3,1;4,1;5,1;6,1;7,1;8,1`.

### 28.4 Normalization

* Plaintext uppercased.
* Spaces → `.` (dot).
* Allowed plaintext: A–Z (no C), digits 0–9, `.`
* Default mapping: `C → K` (configurable; unsupported glyphs dropped unless mapped).

### 28.5 API & CLI

**Language API (KIC on by default):**

```goblin
ct = runes.encrypt("meet at dawn", key: "1,3")    /// headerless, key hidden in ct
pt = runes.decrypt(ct)                            /// auto-recovers key

ct = runes.encrypt("meet at dawn", key: "1,3", passphrase: "riddleme")
pt = runes.decrypt(ct, passphrase: "riddleme")

ct = runes.encrypt("meet at dawn", key: "1,3", seed: 87)
pt = runes.decrypt(ct, seed: 87)

ct = runes.encrypt("meet at dawn", key: "1,3", seed: 87, passphrase: "riddleme")
pt = runes.decrypt(ct, seed: 87, passphrase: "riddleme")
```

**CLI:**

```
# Encrypt: goblin runes.encrypt "<message>" [:: <key>] [:: <seed>]
# Decrypt: goblin runes.decrypt "<ciphertext>" [:: <key>] [:: <seed>]

goblin runes.encrypt "treasure at oak"                   # default key + fixed grid
goblin runes.encrypt "treasure at oak" :: 1,3            # custom key
goblin runes.encrypt "treasure at oak" :: 1,3 :: 87      # custom key + seeded grid
goblin runes.decrypt "T8X@1X1G#3KN4X7"                   # auto-recover key
```

Long-form flags (`--key`, `--seed`, `--passphrase`, `--no-kic`) remain valid. `--no-kic` reverts to legacy header mode.

### 28.6 Key-in-Ciphertext (KIC v1)

**Marker set:** `@, #, %, _, -, $` (then repeat).

**Serialization:**

* Pairs (d,s) → hex-like digits (0–9,A–F).
* Multiple pairs joined with `F`.
* Example: `"1,3" → 13`; `"1,1;2,2;3,1" → 11F22F31`

**Insertion schedule:**

* After every 3 symbols, insert marker+key-char.
* If carrier ends with key-chars left, append them at tail.
* On decode: extract marker pairs, reconstruct key, then decrypt.

**Sealed KIC:** key serialized, AEAD-encrypted with passphrase, then embedded.

### 28.7 Golden Test Vectors

* **Simple pair:**

  * PT: `HELLO.W0RLD`
  * Key: `1,3 → 13`
  * Base ct: `T8XX1GKN4X7`
  * KIC ct:  `T8X@1X1G#3KN4X7`

* **Multi-pair:**

  * PT: `SECRET.PRAETOR` (`C→K`)
  * Key: `1,1;2,2;3,1 → 11F22F31`
  * Base ct: `.SLXSUMFSTSU6M`
  * KIC ct:  `.SL@1XSU#1MFS%FTSU_26M-2$F@3#1`

### 28.8 Pack & Seed Interop

* Pack mode (triples) and Seed mode (pairs) serialize and embed via KIC.
* Seed camouflage: `SEED=<seed>|KEY=<key>` embedded.
* Sealed: key+seed AEAD-encrypted together.

### 28.9 Seeded Grids

* Seed: integer 1..1000.
* Base symbols: `A B D E F G 0 1 2 3 4 Z H I J K L M 5 6 7 8 9 . N O P Q R S T U V W X Y`
* Shuffle: Fisher–Yates with PRNG seeded by `HMAC-SHA256("RUNES-SEED-v1", seed)`.
* Reshape into 6×6 grid.
* Omit seed → fixed grid. Provide seed → both sides must match.

### 28.10 Safety Banner

Runes is for lore, puzzles, and light secrecy. For tamper-evident or strong confidentiality, wrap with HMAC or AEAD (e.g., ChaCha20-Poly1305).

### 28.11 Self-Test

`runes.selftest()` (and CLI) validates:

* Fixed grid + KIC, simple and multi-pair
* Seeded grid smoke test
* Passes if encrypt→decrypt roundtrips correctly

### 28.12 Implementation Constants

* **Marker cycle:** `@ # % _ - $`
* **Cadence:** every 3 symbols insert marker+char; append remainder.
* **Key serialization:** pairs `ds`, joined with `F`.
* **PRF (seed):** `HMAC-SHA256("RUNES-SEED-v1" || seed)`
* **PRF (sealed KIC):** `PBKDF2-HMAC-SHA256(passphrase, salt="RUNES-KIC-AEAD", iters=100k)` → 32-byte key
* **AEAD:** ChaCha20-Poly1305

### 28.13 Quick CLI Note

```
goblin runes.encrypt "treasure at oak"
```

→ quick, reproducible cipher (default key + fixed grid). Add `:: 1,3` for custom key; add `:: 87` for seeded grid.

## 29. List Utilities

### 29.0 Overview

Goblin lists are ordered, 0-based sequences. By convention, they are homogenous, though mixed-type lists are permitted (some operations may reject mixed types).

Randomized verbs use the runtime RNG and respect the global seed for deterministic reproducibility.

**Indexing sugar:**

* `at <I>` → element at index I (0-based)
* `first` → alias for index 0
* `last` → alias for the final element

**Mutability conventions:**

* **Non-destructive verbs:** `pick`, `shuffle`, `sort`
* **Destructive verbs:** `reap`, `usurp`, `replace`, `insert`, `add`

**Reserved words in this section:**

* **Hard:** `pick`, `reap`, `usurp`, `len`, `shuffle`, `sort`, `add`, `insert`, `replace`
* **Soft (contextual only inside list verbs):** `from`, `at`, `first`, `last`, `to`, `into`, `in`, `with`, `dups`

---

### 29.1 pick — non-destructive selection

`pick` is your go-to for sampling from lists without consuming them. Think of it as "looking" at random items in a bag without actually taking them out. Perfect for previewing enemies to fight, checking what loot might drop, or getting random elements while keeping the original list intact.

The key insight: `pick` never changes your original list. It's read-only randomness.

**Forms:**

```
pick <list>                     /// Random single element
pick <N> from <list>            /// N random elements, no duplicates
pick <N> dups from <list>       /// N random elements, duplicates allowed
pick first from <list>          /// First element (deterministic)
pick last from <list>           /// Last element (deterministic)
pick at <I> from <list>         /// Element at specific index
```

**Returns:**

* Single element for scalar forms (`pick <list>`, `pick first`, etc.)
* List of length N for `pick <N> ...` forms

**When to use `pick`:**
- Previewing random content without consuming it
- Selecting enemies for an encounter while keeping the spawn pool intact
- Rolling random events that can happen multiple times
- Getting samples for display purposes

**Errors:** `EmptyPickError | PickCountError | PickIndexError | PickTypeError`

**Examples:**

```goblin
enemies = ["goblin","orc","troll","dragon"]
say pick enemies                /// "troll" (random, enemies unchanged)
say pick 2 from enemies         /// ["dragon","goblin"] (2 random, no repeats)
say pick 3 dups from enemies    /// ["orc","orc","goblin"] (allows repeats)
say enemies                     /// Still ["goblin","orc","troll","dragon"]

/// Perfect for event systems that don't consume events
random_events = ["ambush","treasure","merchant","trap"]
next_event = pick random_events /// Preview without removing
```

---

### 29.2 reap — destructive selection

`reap` is the opposite of `pick` — it permanently removes and returns elements from your list. Think of it as "harvesting" or "consuming" items. When you reap something, it's gone forever (unless you put it back manually).

This is essential for consumable game mechanics: drawing cards, using up inventory items, removing defeated enemies, or any time you need something to be permanently taken out of play.

**Forms:**

```
reap from <list>                /// Remove and return random element
reap <N> from <list>            /// Remove and return N random elements
reap first|last from <list>     /// Remove and return first/last element
reap at <I> from <list>         /// Remove and return element at index
```

**Returns:**
* Single element for scalar forms
* List of N elements for `reap <N> from <list>`

**When to use `reap`:**
- Drawing cards from a deck (cards leave the deck)
- Using consumable items from inventory
- Removing defeated enemies from the battlefield
- Processing a work queue where items get consumed
- Any time something should be permanently removed

**Errors:** `ReapEmptyError | ReapCountError | ReapIndexError | ReapTypeError`

**Examples:**

```goblin
deck = ["ace","king","queen","jack","ten"]
hand = reap 2 from deck
say hand                        /// ["queen","ace"] (drawn cards)
say deck                        /// ["king","jack","ten"] (cards gone from deck)

/// Perfect for inventory consumption
potions = ["health","mana","stamina","poison"]
used_potion = reap from potions /// Randomly use a potion
say "Used: " || used_potion     /// "mana"
say "Remaining: " || potions    /// ["health","stamina","poison"]

/// Queue processing
tasks = ["upload","process","notify"]
current_task = reap first from tasks /// Take next task
```

---

### 29.3 usurp — destructive replace

`usurp` replaces elements in your list and tells you what got replaced. It's perfect for state changes where you need to track what happened — like equipment breaking, items degrading, or status effects changing.

Unlike simple assignment, `usurp` returns a tuple showing you both the old and new values, making it excellent for logging, undo systems, or any time you need to know what changed.

**Forms:**

```
usurp at <I> in <list> with <V>     /// Replace element at specific index
usurp from <list> with <V>          /// Replace random element
usurp <N> from <list> with <Vs>     /// Replace N random elements
```

**Returns:**
* `(old_value, new_value)` tuple for single replacements
* List of `(old, new)` tuples for multiple replacements

**When to use `usurp`:**
- Equipment durability systems (sword becomes broken_sword)
- Status effect changes (healthy becomes poisoned)
- Item upgrades where you need to track what changed
- Any replacement where you need an audit trail

**Errors:** `UsurpIndexError | UsurpEmptyError | UsurpCountError | UsurpArityError | UsurpTypeError`

**Examples:**

```goblin
weapons = ["iron_sword","steel_axe","magic_bow"]
/// Weapon breaks during combat
(old, new) = usurp at 0 in weapons with "broken_iron_sword"
say "Your " || old || " became " || new
/// "Your iron_sword became broken_iron_sword"
say weapons /// ["broken_iron_sword","steel_axe","magic_bow"]

/// Multiple random degradation
armor_pieces = ["helm","chest","legs","boots"]
damaged = ["cracked_helm","torn_chest"]
changes = usurp 2 from armor_pieces with damaged
say changes /// [("helm","cracked_helm"), ("legs","torn_chest")]
```

---

### 29.4 replace — overwrite at index

`replace` is the simple, no-frills way to overwrite a list element. Unlike `usurp`, it doesn't return the old value — it just makes the change and moves on. Use this when you know exactly what index you want to change and you don't care about tracking the old value.

**Form:**

```
replace at <I> in <list> with <V>
```

**When to use `replace`:**
- Simple assignments where you don't need the old value
- Performance-critical code (slightly faster than `usurp`)
- Updating known positions in arrays

**Errors:** `ReplaceIndexError | ReplaceTypeError`

**Examples:**

```goblin
player_stats = [100, 50, 25]     /// [health, mana, stamina]
replace at 0 in player_stats with 85
say player_stats                 /// [85, 50, 25]

/// Updating game state
map_tiles = ["grass","water","mountain","forest"]
replace at 1 in map_tiles with "ice"
say map_tiles                    /// ["grass","ice","mountain","forest"]
```

---

### 29.5 add — append / concat

`add` puts new elements at the end of your list. You can add single values or merge entire lists together. This is your bread-and-butter operation for building lists incrementally — collecting loot, adding players to a game, or accumulating results.

**Forms:**

```
add <value> to <list>           /// Append single value
add <list2> to <list1>          /// Concatenate list2 onto list1
```

**When to use `add`:**
- Collecting items into inventory
- Building result lists
- Adding players to a lobby
- Accumulating any kind of data over time

**Errors:** `AddTypeError`

**Examples:**

```goblin
loot = ["gold_coin"]
add "magic_ring" to loot
say loot                        /// ["gold_coin","magic_ring"]

/// Merging lists
treasure_chest = ["ruby","emerald"]
add treasure_chest to loot
say loot                        /// ["gold_coin","magic_ring","ruby","emerald"]

/// Building results
high_scores = []
add 1250 to high_scores
add 980 to high_scores
say high_scores                 /// [1250, 980]
```

---

### 29.6 insert — insert at index

`insert` pushes a new element into your list at a specific position, shifting everything else to the right. This is perfect for maintaining sorted lists, inserting items at priority positions, or any time you need precise control over element placement.

**Form:**

```
insert <value> at <I> into <list>
```

**When to use `insert`:**
- Maintaining sorted or priority-ordered lists
- Inserting items at the beginning or middle of queues
- Building lists where position matters

**Errors:** `InsertIndexError | InsertTypeError`

**Examples:**

```goblin
priority_tasks = ["urgent","normal"]
insert "critical" at 0 into priority_tasks
say priority_tasks              /// ["critical","urgent","normal"]

/// Building a sorted list manually
scores = [100, 80, 60]
insert 90 at 1 into scores
say scores                      /// [100, 90, 80, 60]
```

---

### 29.7 len — list length

Returns the number of elements in a list. Simple but essential for bounds checking, loop conditions, and game logic that depends on collection sizes.

```
len <list> → int
```

**Examples:**

```goblin
party = ["warrior","mage","rogue"]
say len party                   /// 3

if len party < 4
  say "Party needs more members"
end
```

---

### 29.8 shuffle — random permutation

`shuffle` creates a new list with the same elements in random order. The original list stays unchanged. Essential for randomizing turn order, shuffling decks, or creating random arrangements while preserving the original data.

```
shuffle <list> → new_list
```

**When to use `shuffle`:**
- Randomizing turn order in games
- Shuffling card decks
- Creating random arrangements for procedural generation
- Any time you need a random permutation

**Errors:** `ShuffleTypeError`

**Examples:**

```goblin
original_deck = ["A","K","Q","J","10"]
shuffled_deck = shuffle original_deck
say shuffled_deck               /// ["Q","10","A","K","J"] (random order)
say original_deck               /// ["A","K","Q","J","10"] (unchanged)

/// Initiative order for combat
party = ["tank","healer","dps1","dps2"]
turn_order = shuffle party
say "Combat order: " || turn_order
```

---

### 29.9 sort — sorted copy

`sort` creates a new list with elements arranged in ascending order. Like `shuffle`, it leaves the original unchanged. Handles numbers, strings, and mixed types with clear rules about what gets sorted how.

```
sort <list> → new_list
```

**Rules:**
* Ascending order, stable sort (equal elements maintain relative order)
* Numbers: mathematical comparison (1, 2, 10, 20)
* Strings: lexicographic/alphabetical (apple, banana, cherry)
* Mixed types: throws error (can't compare apples to oranges)

**When to use `sort`:**
- Creating leaderboards
- Organizing inventory alphabetically
- Preparing data for display
- Any time you need ordered results

**Errors:** `SortTypeError`

**Examples:**

```goblin
scores = [85, 92, 78, 96, 88]
leaderboard = sort scores
say leaderboard                 /// [78, 85, 88, 92, 96]
say scores                      /// [85, 92, 78, 96, 88] (unchanged)

names = ["zoe","alice","bob","charlie"]
alphabetical = sort names
say alphabetical                /// ["alice","bob","charlie","zoe"]

/// Mixed types fail safely
mixed = [5, "hello", 3]
/// sort mixed would throw SortTypeError
```

---

### 29.10 Determinism

All random verbs (`pick`, `reap` random, `usurp` random, `shuffle`) use the runtime RNG and honor the global seed.

This means if you run your program with `--seed 42`, all random list operations will produce identical results every time. Perfect for debugging, testing, and creating reproducible gameplay experiences.

**Examples:**

```goblin
/// With --seed 1337, these always produce the same results
enemies = ["goblin","orc","troll"]
say pick enemies                /// Always the same enemy with same seed
say shuffle enemies             /// Always the same shuffle pattern
```

---

### 29.11 Range Integration

Ranges (A..B) are treated as list literals. All list verbs apply directly to ranges, making them incredibly useful for working with number sequences.

**Examples:**

```goblin
say pick 1..100                 /// Random number from 1 to 100
say pick 5 from 1..20           /// 5 random numbers, no duplicates
say len 10..50                  /// 41 (50-10+1)
say shuffle 1..6                /// Shuffled dice roll outcomes

/// Ranges are immutable, so destructive verbs copy first
numbers = 1..10
subset = reap 3 from numbers    /// Creates mutable copy, then reaps
```

**Notes:**
* Ranges are immutable; destructive verbs automatically copy to a mutable list
* `len` returns B − A + 1 for ranges

---

### 29.12 Functional Aliases

Every readable sugar form has an equivalent functional syntax. Use whichever style fits your codebase better.

**Examples:**

```
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
```

---

### 29.13 Mini Game Recipes

Real-world patterns for common game programming scenarios. These examples show the verbs in action solving actual game development problems.

**1. Combat: Remove defeated enemies**

```goblin
enemies = ["goblin","orc","slime","bat","skeleton","bandit"]

/// Kill 3 enemies randomly (they're removed from battle)
killed = reap 3 from enemies
say "Defeated: " || killed      /// ["bat","goblin","slime"]
say "Still fighting: " || enemies /// ["orc","skeleton","bandit"]
```

**2. Card game: Drawing vs burning cards**

```goblin
deck = ["AS","KS","QS","JS","10S"]

/// Deal 2 cards to hand (cards leave deck)
hand = [reap first from deck, reap first from deck]
say "Hand: " || hand            /// ["AS","KS"]
say "Deck: " || deck            /// ["QS","JS","10S"]

/// Burn 2 random cards (lost forever)
burned = reap 2 from deck
say "Burned: " || burned        /// ["10S","QS"]
say "Deck now: " || deck        /// ["JS"]
```

**3. Equipment durability: Items break during use**

```goblin
inventory = ["Sword","Shield","Bow","Helmet","Boots"]
damaged_versions = ["Cracked_Sword","Dented_Shield"]

/// 2 random items take damage
damage_results = usurp 2 from inventory with damaged_versions
say damage_results              /// [("Sword","Cracked_Sword"), ("Shield","Dented_Shield")]
say "Inventory: " || inventory  /// ["Cracked_Sword","Dented_Shield","Bow","Helmet","Boots"]
```

**4. Loot system: Items are consumed when taken**

```goblin
loot_table = ["gold","gem","potion","map","ring"]

/// Take random loot (removed from table so it can't drop again)
drop = reap from loot_table
say "Found: " || drop           /// "gem"
say "Remaining loot: " || loot_table /// ["gold","potion","map","ring"]
```

**5. Gacha/lottery: Pulls with replacement**

```goblin
gacha_pool = ["common","common","common","rare","rare","epic"]

/// 10 pulls with duplicates allowed (pool unchanged)
pulls = pick 10 dups from gacha_pool
say "Pulled: " || pulls         /// ["common","rare","common","common","epic","rare","common","common","rare","common"]
say "Pool unchanged: " || gacha_pool /// ["common","common","common","rare","rare","epic"]
```

**6. Turn-based games: Random initiative order**

```goblin
party = ["Warrior","Mage","Rogue","Cleric"]
initiative_order = shuffle party
say "Turn order: " || initiative_order /// ["Rogue","Cleric","Warrior","Mage"]
```

**7. Traps: Random item loss**

```goblin
backpack = ["Rope","Torch","Pickaxe","Rations"]

/// Trap destroys random item
lost_item = reap from backpack
say "Lost to trap: " || lost_item /// "Torch"
say "Remaining gear: " || backpack /// ["Rope","Pickaxe","Rations"]
```

**8. Weapon degradation: Track what broke**

```goblin
equipped_weapons = ["Iron_Dagger","Steel_Axe","Magic_Bow"]

/// One weapon breaks in combat
(broken_weapon, replacement) = usurp at 1 in equipped_weapons with "Broken_Steel_Axe"
say broken_weapon || " broke and became " || replacement
/// "Steel_Axe broke and became Broken_Steel_Axe"
say equipped_weapons /// ["Iron_Dagger","Broken_Steel_Axe","Magic_Bow"]
```

**9. Event systems: Preview vs consume**

```goblin
event_queue = ["Ambush","Treasure","Merchant","Rest"]

/// Preview next event without consuming it
preview = pick event_queue
say "Next event might be: " || preview /// "Merchant"

/// Actually trigger an event (removes it from queue)
triggered = reap from event_queue
say "Event triggered: " || triggered /// "Treasure"
say "Remaining events: " || event_queue /// ["Ambush","Merchant","Rest"]
```

**10. Enemy spawning: Capped removal**

```goblin
enemy_horde = ["zombie","zombie","zombie"]
max_spawn = 5

/// Safely spawn up to max_spawn enemies
spawn_count = max_spawn
if spawn_count > len enemy_horde
  spawn_count = len enemy_horde
end
spawned = reap spawn_count from enemy_horde
say "Spawned: " || spawned      /// ["zombie","zombie","zombie"]
say "Horde remaining: " || enemy_horde /// []
```

**Design patterns:**
* `reap` for permanent consumption (cards drawn, items used, enemies defeated)
* `pick` for sampling without consumption (previews, random selection pools)
* `usurp` for state changes with audit trails (equipment breaking, status effects)
* Random operations respect `--seed` for reproducible gameplay and debugging

# 30. Divmod

## 30.0 Overview

Divmod gives you both the quotient and remainder from division in a single operation. Instead of doing division and then modulo separately, you get both results at once. What makes Goblin's divmod special is that it has its own readable output format that makes division results crystal clear.

This is incredibly useful for splitting quantities evenly with leftovers — like dividing loot among party members, calculating how many full stacks of items you have plus extras, or figuring out time conversions.

**Two ways to use it:**
* **Operator:** `>>` — returns a pair but **prints in Goblin form** (`quotient r remainder`)
* **Function alias:** `div_rem(a, b)` — returns a regular tuple `(quotient, remainder)`

The key difference: when you print or display the `>>` result, Goblin shows it in human-readable "quotient r remainder" format, making it perfect for user-facing output.

```goblin
say 47 >> 4         /// Prints: "11 r 3" (11 with remainder 3)
q, r = 47 >> 4      /// q=11, r=3 (still destructures normally)

/// Compare with function form
result = div_rem(47, 4)  /// Returns tuple (11, 3)
say result              /// Prints: "(11, 3)" (regular tuple format)
```

This makes divmod results incredibly readable in game output:

```goblin
treasure = 157
party_size = 4
say "Treasure split: " || treasure >> party_size
/// Prints: "Treasure split: 39 r 1" 
/// (39 gold each, 1 leftover)
```

## 30.1 What Types Work

Divmod works with integers and fixed-scale numbers like money or decimals that have integral storage. Both the operator and function have identical type requirements — the only difference is how they display results.

**Conceptual signatures:**

```
div_rem : (IntLike a) => (a, a) -> (a, a)   /// Returns tuple (q, r)
(>>)     : (IntLike a) => (a, a) -> (a, a)   /// Returns pair, prints as "q r r"
```

**IntLike includes:**
- Regular integers (1, 42, -17)
- Fixed-scale numbers like money ($10.50) where the scale matches
- Decimal types with integral storage

Both forms return the same mathematical result, but `>>` has special display formatting while `div_rem` returns a standard tuple.

```goblin
/// Integer divmod
items = 23
stack_size = 8
say items >> stack_size        /// Prints: "2 r 7" (2 full stacks, 7 extras)
stacks, extras = items >> stack_size  /// stacks=2, extras=7

/// Money divmod with readable output
bill = $47.50
people = 3
say bill >> people            /// Prints: "$15.83 r $0.01"
per_person, leftover = bill >> people /// ($15.83, $0.01)
```

## 30.2 How It Works (Floor Division Semantics)

Goblin uses **floor division** for divmod, which means the quotient is always rounded down to the nearest integer. This keeps behavior consistent and predictable, especially with negative numbers.

For any `a >> b` where `b ≠ 0`:

```
quotient = floor(a / b)
remainder = a − quotient * b
```

**Key properties:**
* **Remainder sign:** The remainder always has the same sign as the divisor (or is zero)
* **Reconstruction:** You can always rebuild the original: `a = quotient * b + remainder`
* **Bounded remainder:** The remainder is always smaller than the divisor: `|remainder| < |divisor|`

The "quotient r remainder" format makes these relationships crystal clear in output:

```goblin
say -17 >> 5   /// Prints: "-4 r 3" 
/// Meaning: -17 = (-4) * 5 + 3 = -20 + 3 = -17 ✓

say 17 >> -5   /// Prints: "-4 r -3"
/// Meaning: 17 = (-4) * (-5) + (-3) = 20 + (-3) = 17 ✓
```

## 30.3 Syntax and Precedence

The `>>` operator fits right into Goblin's arithmetic alongside multiplication and division. It's left-associative and has the same precedence as `*`, `/`, and `%`.

```goblin
/// These expressions group the same way
a * b >> c    /// Same as (a * b) >> c
a >> b >> c   /// Same as (a >> b) >> c

/// Precedence with addition/subtraction  
a + b >> c    /// Same as a + (b >> c)
```

## 30.4 Using the Results

Divmod results can be used in multiple ways, from full destructuring to extracting just the piece you need:

**Full result display:**
```goblin
say 17 >> 5                    /// Prints: "3 r 2" (readable format)
say div_rem(17, 5)            /// Prints: "(3, 2)" (tuple format)
```

**Extracting specific parts with `q` and `r` operators:**
```goblin
say q 17 >> 5                 /// 3 (just the quotient)  
say r 17 >> 5                 /// 2 (just the remainder)

/// Perfect for when you only need one part
stacks = q items >> stack_size        /// How many full stacks?
extras = r items >> stack_size        /// How many items left over?
each_pays = q bill >> people          /// How much does each person pay?

/// Note: if you only need the quotient, you can also use floor division
stacks = items // stack_size          /// Same as q items >> stack_size
each_pays = bill // people            /// Same as q bill >> people
```

**Destructuring (when you need both values):**
```goblin
quotient, remainder = 17 >> 5          /// q=3, r=2
q2, r2 = div_rem(17, 5)               /// q=3, r=2 (same values)

gold_each, leftover = total_gold >> players
hours, minutes = total_minutes >> 60
```

**Grammar and precedence:**
The `q` and `r` operators are soft unary operators that work specifically with divmod expressions:
- `q <expr>` extracts the first element (quotient) if `<expr>` is a divmod pair
- `r <expr>` extracts the second element (remainder) if `<expr>` is a divmod pair  
- Same precedence as unary `-` or `not`
- Outside divmod context, `q` and `r` are just normal identifiers

**When to use which approach:**
- Use `q expr` or `r expr` when you only need one part (most common)
- Use full `expr >> expr` for user-facing display
- Use destructuring when you need both values for calculations
- Use `div_rem()` for APIs that expect standard tuples

## 30.5 Money and Fixed-Scale Numbers

Divmod works great with money, and the `>>` operator's readable format is especially nice for financial calculations. Regular division (`/`) is forbidden with money to prevent precision loss, but divmod (`>>`) and floor division (`//`) are both allowed.

```goblin
restaurant_bill = $47.85
people = 3

/// Floor division gives you just the quotient
per_person = restaurant_bill // people     /// $15.95

/// Divmod gives you both parts with readable output
say restaurant_bill >> people              /// Prints: "$15.95 r $0.00"
per_person, leftover = restaurant_bill >> people  /// ($15.95, $0.00)

/// Regular division is FORBIDDEN
/// per_person = restaurant_bill / people  /// MoneyDivisionError!
```

The readable format makes financial splits crystal clear:

```goblin
group_bill = $127.50
diners = 4
say "Bill split: " || group_bill >> diners
/// Prints: "Bill split: $31.87 r $0.02"
/// Immediately clear: $31.87 each, 2 cents leftover
```

**Scale compatibility rules:**
* Both numbers must have compatible scales (same currency, same decimal places)
* You can't mix different money types or mix money with plain integers
* The result maintains the same scale as the inputs
* The readable format preserves currency symbols and formatting

```goblin
/// Valid - same currency
usd_bill = $50.00
say usd_bill >> 3             /// Prints: "$16.66 r $0.02"

/// Invalid - mixed currencies would throw ScaleMismatch
/// eur_bill = €40.00
/// mixed_result = usd_bill >> eur_bill  /// Error!
```

## 30.6 Error Conditions

Divmod can fail in several specific ways. The `q` and `r` operators add one additional error case:

**DivisionByZero** — You tried to divide by zero:
```goblin
say 10 >> 0                    /// DivisionByZero error
say q 10 >> 0                  /// DivisionByZero error (same underlying operation)
```

**TypeError** — Using `q` or `r` on non-divmod expressions:
```goblin
say q 42                       /// TypeError: expected divmod pair
say r "hello"                  /// TypeError: expected divmod pair
say q (10 + 5)                /// TypeError: expected divmod pair (15 is not a divmod result)
```

**ScaleMismatch** — Fixed-scale types don't match:
```goblin
usd_amount = $50.00
eur_amount = €30.00
/// result = usd_amount >> eur_amount     /// ScaleMismatch error
/// quotient = q usd_amount >> eur_amount /// ScaleMismatch error
```

**MoneyDivisionError** — Using regular division instead of divmod:
```goblin
price = $100.00
/// result = price / 3                    /// MoneyDivisionError
/// Use price >> 3, q price >> 3, or price // 3 instead
```

**Overflow** — Result too large for the number type:
```goblin
/// Follows global integer policy for handling overflow
```

## 30.7 Real-World Examples

The readable output format makes divmod perfect for user-facing game messages and financial displays:

**Splitting loot among party members:**
```goblin
treasure = 1247
party_size = 4

say "Loot distribution: " || treasure >> party_size
/// Prints: "Loot distribution: 311 r 3"
/// Clear message: 311 gold each, 3 goes to guild bank

gold_each, bank_leftover = treasure >> party_size
```

**Converting time units:**
```goblin
total_seconds = 7265

say "Duration breakdown:"
minutes_result = total_seconds >> 60
say "Minutes: " || minutes_result          /// Prints: "Minutes: 121 r 5"

hours_result = 121 >> 60  
say "Hours: " || hours_result              /// Prints: "Hours: 2 r 1"

/// Final: 2 hours, 1 minute, 5 seconds
```

**Inventory management with clear stacking info:**
```goblin
loose_arrows = 87
arrows_per_quiver = 30

say "Arrow organization: " || loose_arrows >> arrows_per_quiver
/// Prints: "Arrow organization: 2 r 27"
/// Clear: 2 full quivers, 27 loose arrows

full_quivers, loose_count = loose_arrows >> arrows_per_quiver
```

**Restaurant bill splitting with precise financial display:**
```goblin
bill = $67.84
people = 4

say "Bill split: " || bill >> people
/// Prints: "Bill split: $16.96 r $0.00"
/// Perfect: exactly $16.96 each, no leftover

/// Compare with messier division
say "Each pays: " || bill // people       /// Just: "$16.96"
/// Missing info: is there leftover? How much?
```

**Pagination with clear page info:**
```goblin
total_items = 147
items_per_page = 10

say "Pagination: " || total_items >> items_per_page
/// Prints: "Pagination: 14 r 7"
/// Clear: 14 full pages, 7 items on final page

pages, items_on_last = total_items >> items_per_page
total_pages = if items_on_last > 0 then pages + 1 else pages end
```

**Resource allocation in games:**
```goblin
wood_collected = 156
wood_per_building = 25

say "Building capacity: " || wood_collected >> wood_per_building
/// Prints: "Building capacity: 6 r 6"
/// Clear: can build 6 buildings, 6 wood leftover

buildings_possible, wood_remaining = wood_collected >> wood_per_building
```

**Damage distribution:**
```goblin
total_damage = 73
armor_reduction = 8

say "Damage calculation: " || total_damage >> armor_reduction
/// Prints: "Damage calculation: 9 r 1"
/// 9 full points of damage get through, 1 point absorbed

effective_damage, absorbed = total_damage >> armor_reduction
```

## 30.8 Mathematical Properties and Output Format

Divmod follows strict mathematical rules, and the readable output format helps verify these properties:

**Reconstruction property:**
```goblin
/// You can always rebuild the original number
a = 47
b = 5
say a >> b                     /// Prints: "9 r 2"
/// Verify: 9 * 5 + 2 = 45 + 2 = 47 ✓
```

**Remainder bounds (visible in output):**
```goblin
/// Remainder is always smaller than the divisor
say 47 >> 5                   /// "9 r 2" - remainder 2 < divisor 5 ✓
say 23 >> 7                   /// "3 r 2" - remainder 2 < divisor 7 ✓
```

**Sign consistency (clear in readable format):**
```goblin
say 17 >> 5                   /// "3 r 2"   - positive remainder, positive divisor
say 17 >> -5                  /// "-4 r -3" - negative remainder, negative divisor  
say -17 >> 5                  /// "-4 r 3"  - positive remainder, positive divisor
say -17 >> -5                 /// "3 r -2"  - negative remainder, negative divisor
```

**Function equivalence:**
```goblin
/// Both forms give same mathematical result
a, b = 23, 7
op_result = a >> b            /// Prints as "3 r 2"
func_result = div_rem(a, b)   /// Returns (3, 2)
/// Same values, different display formats
```

The readable "quotient r remainder" format makes it easy to verify these mathematical properties and creates clear, unambiguous output for users. This is especially valuable in games where players need to understand resource distribution, damage calculations, or any situation where showing both the main result and the leftover matters.

§34 — Pipelines, Optional Chaining, and Error Blocks
34.0 Overview

This section introduces three new language features:

Pipeline operator (|>) — left-to-right function chaining.

Optional chaining (?.) — safe navigation for null/undefined.

Structured error handling (attempt / rescue / ensure) — block form exception handling.

# 31. Pipelines, Optional Chaining, and Error Handling

## 31.1 Pipeline Operator (`|>`)

The pipeline operator `|>` lets you chain function calls in a way that reads like a data transformation recipe. Instead of nested function calls that read inside-out, pipelines read left-to-right like you're following data through a processing pipeline.

**The core magic:**

```goblin
player_data |> parse_json() |> validate_stats() |> save_to_db()
/// Much clearer than: save_to_db(validate_stats(parse_json(player_data)))
```

**How it works:**
```goblin
a |> f(x)   /// Becomes: f(a, x) - 'a' gets passed as first argument
```

This means the value on the left becomes the first parameter to the function on the right. Perfect for data processing chains where each step transforms the previous result.

**Chaining multiple operations:**
```goblin
player_score 
  |> apply_bonus(multiplier: 2.0)     /// apply_bonus(player_score, multiplier: 2.0)
  |> clamp_to_max(10000)              /// clamp_to_max(result_from_above, 10000)
  |> format_display()                 /// format_display(final_clamped_score)
```

**Associativity:** Left-to-right, so:
```goblin
a |> f() |> g(y)   /// Same as: g(f(a), y)
```

### Precedence Rules

Pipeline binds with specific precedence to avoid surprises:

```goblin
/// Arithmetic happens first, then pipeline
score + bonus |> format()        /// Same as: format(score + bonus)

/// Pipeline happens before string operations  
result |> process() || " points"  /// Same as: process(result) || " points"

/// Function calls and property access happen before pipeline
obj.method() |> transform()       /// Same as: transform(obj.method())
data?.value |> validate()         /// Same as: validate(data?.value)
```

### Function Signature Requirements

The function on the right side must accept the piped value as its **first argument**. If it can't, you get an `ArityMismatch` error:

```goblin
/// This works - process() can take player_data as first arg
player_data |> process(options: fast_mode)

/// This fails - Math.sqrt() expects a number as first arg, not player_data
player_data |> Math.sqrt()   /// ArityMismatch error
```

### Lambda Escape Hatch

When you need to pipe into a function that doesn't take your value as the first parameter, use a lambda to rearrange things:

```goblin
damage_amount |> (dmg -> apply_damage(target: enemy, amount: dmg, type: fire))
/// The lambda lets you put 'dmg' wherever it needs to go

temperature |> (temp -> clamp(min: 0, max: 100, value: temp))
/// Rearrange parameters to fit the function signature
```

### Null Handling

**Important:** Pipelines don't automatically handle nulls - they pass them right through:

```goblin
null |> process() |> format()   /// process(null) gets called, might crash!

/// If you need null safety, combine with other operators:
player?.inventory |> sort_items() |> display()   /// Safe: stops at null
result |> process() ?? "default"                  /// Safe: fallback for null results
```

### Real-World Pipeline Examples

**Game data processing:**
```goblin
raw_save_data
  |> decompress()
  |> parse_save_format()  
  |> validate_player_data()
  |> migrate_to_current_version()
  |> load_into_game()
```

**Damage calculation pipeline:**
```goblin
base_damage
  |> apply_weapon_mods()
  |> apply_character_stats() 
  |> apply_enemy_resistances()
  |> apply_critical_hit()
  |> display_damage_number()
```

**User input processing:**
```goblin
user_input
  |> trim_whitespace()
  |> validate_command()
  |> parse_arguments() 
  |> execute_command()
  |> format_response()
```

---

## 31.2 Optional Chaining (`?.`)

Optional chaining is your safety net for navigating potentially null or undefined data. Instead of writing defensive null checks everywhere, `?.` lets you safely drill into object properties and call methods, automatically stopping if anything along the way is null.

**The basic idea:**
```goblin
player?.inventory?.weapons?.primary?.damage
/// Instead of: if player && player.inventory && player.inventory.weapons && ...
```

**How it works:**
- If the left side is `null` or `undefined`, the whole expression becomes `null`
- If the left side has a value, access proceeds normally
- Chains short-circuit at the first null, so no further evaluation happens

**Safe property access:**
```goblin
user?.profile?.avatar?.url     /// null if user, profile, or avatar is null
config?.graphics?.resolution   /// null if config or graphics is null
```

**Safe method calls:**
```goblin
player?.save()                /// Only calls save() if player exists
enemy?.take_damage(50)        /// Only calls method if enemy exists
connection?.close()?.then()   /// Chains safely through multiple calls
```

**Short-circuit behavior:**
```goblin
/// If 'obj' is null, expensive_calculation() never runs
result = obj?.method(expensive_calculation())

/// Compare to unsafe version:
/// result = obj.method(expensive_calculation())  /// Crashes if obj is null!
```

**Error handling:**
Optional chaining only handles `null` and `undefined` - if the property access or method call actually runs and throws an error, that error still propagates:

```goblin
player?.inventory?.break_item()   /// If break_item() throws ItemNotFound, you still get that error
```

### Combining with Pipelines

Optional chaining works great with pipelines for safe data processing:

```goblin
user
  |> fetch_profile()       /// Returns profile or null
  |> (profile -> profile?.address?.city)   /// Safely extract city
  |> (city -> city ?? "Unknown")           /// Provide fallback
```

### Real-World Optional Chaining Examples

**Safe game state access:**
```goblin
/// Safely check if player has a specific item
has_key = player?.inventory?.items?.find(item -> item.type == "key")

/// Safe UI updates
player?.ui?.health_bar?.update(new_health)

/// Safe network operations  
response?.data?.player_stats?.level
```

**Configuration management:**
```goblin
/// Safely read nested config with fallbacks
graphics_quality = config?.graphics?.quality ?? "medium"
sound_volume = settings?.audio?.volume ?? 0.5
debug_mode = flags?.development?.debug ?? false
```

---

## 31.3 `attempt` / `rescue` / `ensure`

Goblin's error handling is built around explicit, readable error management. No hidden exceptions or surprise crashes - you clearly state what might go wrong and how to handle it.

**The basic structure:**
```goblin
result = 
  attempt
    risky_operation()
    "success!"
  rescue NetworkError as e
    log("Network failed: " || e.message)
    "offline_mode"
  rescue ValidationError as e
    log("Bad data: " || e.message) 
    "default_data"
  ensure
    cleanup_resources()
  end
```

### How It Works

**attempt block:** Contains code that might throw errors. If it completes successfully, its last expression becomes the result.

**rescue blocks:** Handle specific error types. First matching rescue wins. The variable after `as` gives you access to the error object.

**ensure block:** Always runs, whether the attempt succeeded or failed. Perfect for cleanup like closing files, releasing locks, or updating metrics.

**Return values:** The result is the last expression from whichever branch actually executes (attempt or rescue).

### Multiple Rescue Blocks

You can handle different error types differently:

```goblin
save_result =
  attempt
    player_data |> validate() |> compress() |> write_to_disk()
    "saved_successfully"
  rescue ValidationError as e
    log("Invalid player data: " || e.message)
    "validation_failed" 
  rescue DiskFullError as e
    log("Disk full, trying cloud save")
    attempt_cloud_save(player_data)
  rescue NetworkError as e  
    log("Cloud save failed, using local cache")
    write_to_cache(player_data)
  ensure
    update_save_metrics()
  end
```

### Error Propagation

If no rescue block matches the error type, the error propagates up after the ensure block runs:

```goblin
attempt
  dangerous_operation()   /// Throws UnhandledErrorType
rescue KnownError as e
  handle_known_error(e)   /// This won't match
ensure  
  cleanup()               /// This still runs
end
/// UnhandledErrorType propagates to caller after cleanup
```

### Rethrowing Errors

Use bare `raise` to rethrow the current error while preserving the original stack trace:

```goblin
attempt
  critical_operation()
rescue TransientError as e
  log("Retrying after transient error")
  backoff_delay()
  raise   /// Rethrow original error with original stack
rescue FatalError as e
  log("Fatal error, giving up") 
  raise   /// Rethrow fatal error
end
```

### Real-World Error Handling Examples

**File operations with cleanup:**
```goblin
config_data =
  attempt
    file_handle = open_config_file()
    file_handle |> read_all() |> parse_json()
  rescue FileNotFound
    log("Config missing, using defaults")
    default_config()
  rescue ParseError as e
    log("Corrupt config: " || e.message)
    backup_config()
  ensure
    file_handle?.close()   /// Always close if it was opened
  end
```

**Network operations with retries:**
```goblin
player_stats =
  attempt
    fetch_player_data(player_id) |> validate_stats()
  rescue NetworkTimeout as e
    log("Request timed out, retrying...")
    retry_with_backoff()
    raise   /// Let caller handle persistent timeouts
  rescue ValidationError as e
    log("Server returned bad data")
    cached_player_stats(player_id)   /// Use cache as fallback
  ensure
    close_network_connection()
  end
```

**Game state management:**
```goblin
save_result =
  attempt
    game_state |> serialize() |> compress() |> write_save_file()
    update_ui("Game saved successfully")
    true
  rescue SerializationError as e
    show_error("Failed to prepare save data") 
    false
  rescue DiskError as e
    show_error("Could not write save file")
    false
  ensure
    release_save_lock()   /// Always release the save file lock
    update_save_timestamp()
  end
```

---

## 31.4 Powerful Combinations

These features work together to create robust, readable data processing pipelines:

### Pipelines + Optional Chaining

Safe data transformation chains:

```goblin
user_profile =
  raw_user_data
  |> parse_json()
  |> (data -> data?.profile)     /// Safely extract profile
  |> validate_profile()
  |> (profile -> profile?.settings?.theme ?? "default")
```

### Pipelines + Error Handling

Robust data processing with clear error management:

```goblin
processed_data =
  attempt
    input_data 
      |> validate_format()
      |> transform_data()
      |> apply_business_rules()
      |> save_to_database()
  rescue ValidationError as e
    log("Invalid input: " || e.message)
    fallback_data()
  rescue DatabaseError as e 
    log("Database error: " || e.message)
    cache_for_retry(input_data)
    temp_storage_data()
  ensure
    metrics.record("data_processing_attempt")
  end
```

### Optional Chaining in Error Blocks

Safe error introspection:

```goblin
attempt
  risky_network_call()
rescue NetworkError as e
  /// Safely access error details that might not exist
  error_code = e?.details?.status_code ?? "unknown"
  retry_after = e?.headers?.retry_after ?? 60
  log("Network error " || error_code || ", retrying in " || retry_after || "s")
ensure
  connection?.close()   /// Safe cleanup
end
```

### Method Chaining vs Pipelines

Choose the right tool for the job:

```goblin
/// Method chaining: good for fluent object APIs
query_result = database
  .table("players") 
  .where("level > 10")
  .order_by("score")
  .limit(100)

/// Pipelines: good for data transformation chains
final_scores = raw_scores
  |> filter_valid_scores()
  |> apply_bonuses()
  |> normalize_to_scale()
  |> round_to_integers()
```

---

## 31.5 Error Types and Edge Cases

**ArityMismatch** — The most common pipeline error:
```goblin
/// This fails because Math.sqrt expects (number), not (string, number)
"hello" |> Math.sqrt(4)   /// ArityMismatch: cannot pipe string to sqrt
```

**Null propagation behavior:**
```goblin
/// Optional chaining stops at null, returns null
null?.anything?.else   /// null (safe)

/// Pipelines pass null through, might cause errors
null |> process()      /// Calls process(null), might crash

/// Combine for safety:
value?.property |> transform() ?? "default"
```

**Exception handling specifics:**
```goblin
/// Optional chaining doesn't catch exceptions from successful calls
obj?.method_that_throws()   /// Still throws if obj exists and method fails

/// Use attempt/rescue for exception handling:
attempt
  obj?.method_that_throws()
rescue SomeError as e
  handle_error(e)
end
```

This combination of features makes Goblin excellent for robust data processing, safe navigation of complex objects, and clear error handling - all essential for game development where you're constantly dealing with user input, network data, and complex game state.

# 32. Play & Randomization Helpers

## 32.1 Overview

Goblin brings the tabletop experience directly into your code with proper dice rolling, weighted loot tables, and statistical helpers. Whether you're building a roguelike, a card game, or just need some controlled randomness, these tools give you the authentic feel of rolling real dice and drawing from carefully balanced probability tables.

All random operations respect the global RNG seed, so you can have deterministic randomness for testing, speedruns, or any time you need reproducible results. Run with `--seed 1337` and your dice will always roll the same sequence.

**New dice syntax:** Inside `roll` and `roll_detail` functions, you can use natural tabletop notation:

```
dice_expr ::= INT "d" INT [ ("+" | "-") INT ]?
```

This means expressions like `1d10`, `2d6+1`, `4d8-2` work exactly like you'd write them on paper. Outside of dice functions, this is just regular syntax (so `2d6` would be multiplication), but inside dice context it becomes magical.

```goblin
/// Natural dice notation that feels like tabletop
damage = roll 2d6+3        /// Roll 2 six-sided dice, add 3
initiative = roll 1d20     /// Classic d20 roll
fireball = roll 8d6        /// Massive damage roll
```

---

## 32.2 `roll` and `roll_detail` — Universal Random Number Generation

**Important:** `roll` is Goblin's primary random number generator, not just for dice! The "dice" notation is used because it's a familiar, readable way to express random ranges that everyone understands. You can use ANY numbers - `7d38+9`, `1d1000`, `25d4-12` - whatever ranges you need.

Think of `roll` as the replacement for `rand()`, `randint()`, `random.choice()` and similar functions in other languages, but with a much more intuitive syntax.

**Simple rolling with `roll`:**
```goblin
roll dice_expr      /// Natural syntax: roll 2d6+1
roll(dice_expr)     /// Function syntax: roll(2d6+1)
```

**Detailed rolling with `roll_detail`:**
Gets you the full breakdown - individual random values, the sum before modifiers, and the final total after modifiers.

### Multiple Ways to Generate Random Numbers in Goblin

Goblin gives you several approaches to random number generation, each with different strengths:

**`roll` - Best for most cases:**
```goblin
roll 1d100              /// 1-100, clear and intuitive
roll 2d6+3              /// Bell curve distribution, 5-15
roll 1d20-10            /// -9 to +10, with offset
```

**`pick` with ranges - Alternative for simple cases:**
```goblin
pick 1..100             /// 1-100, exactly like roll 1d100
pick 0..255             /// 0-255, good for RGB values
pick -50..50            /// -50 to +50, symmetric range
```

**When to use which:**

- **Use `roll`** when you want bell curves (multiple dice), need modifiers, or want the self-documenting dice notation
- **Use `pick`** when you have a simple flat range and want the most concise syntax

```goblin
/// These are equivalent for flat distributions:
player_id = roll 1d999999        /// Dice notation - self-documenting
player_id = pick 1..999999       /// Range notation - more concise

/// But roll gives you more power:
ability_score = roll 3d6         /// Bell curve, most results 10-11
damage_burst = roll 5d4+2        /// Multiple dice with modifier

/// And pick integrates with other list operations:
random_element = pick ["fire", "ice", "lightning"]
random_from_list = pick enemy_spawn_points
```

**Pro tip:** You can combine them in the same program based on what reads clearest in context!

The notation `XdY+Z` is just a clear way to express "generate X random numbers from 1 to Y, sum them, then add Z". You're not limited to traditional gaming dice:

- `1d10` → one random number from 1-10 (replaces `randint(1, 10)`)
- `1d100` → one random number from 1-100 (replaces `randint(1, 100)`)
- `3d4` → three random numbers from 1-4, summed (bell curve distribution)
- `1d1000-500` → random number from -499 to 500 
- `10d2` → sum of 10 coin flips (1 or 2), giving you 10-20
- `1d256` → random byte value equivalent

```goblin
/// Generate random game values using intuitive notation
player_id = roll 1d999999           /// Random player ID: 1-999999
spawn_delay = roll 1d5000+1000      /// Random delay: 1001-6000 milliseconds  
damage_variance = roll 1d20-10      /// Random modifier: -9 to +10
rgb_red = roll 1d256-1              /// Red color component: 0-255
temperature = roll 1d200-100        /// Temperature: -99 to +100 degrees
```

### Why This Beats Traditional Random Functions

Traditional random functions are often confusing and inconsistent:

```python
# Other languages - confusing and inconsistent
random.randint(1, 6)        # 1-6 inclusive? exclusive?
random.uniform(0, 1)        # 0-1 exclusive? inclusive?
random.choice(range(1, 7))  # Verbose for simple ranges
random.gauss(50, 10)        # Hard to understand the distribution
```

Goblin's dice notation is self-documenting and consistent:

```goblin
/// Goblin - clear and intuitive
roll 1d6              /// Obviously 1-6, always inclusive
roll 1d100            /// Obviously 1-100, no confusion
roll 3d6+5            /// Obviously bell curve around 15.5, easy to understand
roll 2d10-2           /// Obviously 0-18 with slight bell curve
```

### Random Number Generation Patterns

**Simple random integers (replacing randint):**
```goblin
/// Instead of randint(1, 100)
random_percent = roll 1d100

/// Instead of randint(0, 255)  
color_component = roll 1d256-1

/// Instead of randint(-50, 50)
position_offset = roll 1d101-51
```

**Bell curve distributions (multiple dice):**
```goblin
/// Bell curve centered around 10.5 (replaces complex gaussian)
character_stat = roll 3d6           /// 3-18, most results near 10-11

/// Bell curve for damage with high variance
explosion_damage = roll 10d10       /// 10-100, clustered around 55

/// Subtle randomness with tight bell curve
minor_variation = roll 5d2          /// 5-10, heavily weighted toward 7-8
```

**Bounded random with offsets:**
```goblin
/// Random price with minimum base cost
item_price = roll 1d500+100         /// 101-600 gold

/// Random spawn time with delay
next_wave = roll 1d30+10            /// 11-40 seconds

/// Random level generation
dungeon_rooms = roll 2d8+5          /// 7-21 rooms, slightly favors middle
```

### Real-World Non-Gaming Examples

**Network and timing:**
```goblin
/// Connection timeout with jitter
timeout_ms = roll 1d2000+3000       /// 3001-5000ms

/// Retry backoff with randomization  
backoff_delay = roll 1d1000+500     /// 501-1500ms

/// Load balancing random selection
server_choice = roll 1d5            /// Pick server 1-5
```

**Procedural generation:**
```goblin
/// Terrain height variation
terrain_height = roll 3d20+20       /// 23-80, bell curve around 50

/// Building floor count
building_floors = roll 1d40+5       /// 6-45 floors

/// Cloud density
cloud_coverage = roll 2d50          /// 2-100, weighted toward middle
```

**AI and behavior:**
```goblin
/// NPC reaction variation
mood_modifier = roll 1d20-10        /// -9 to +10 mood change

/// AI decision making with weighted randomness
aggression_level = roll 3d4         /// 3-12, most NPCs around 7-8

/// Patrol route variation
route_deviation = roll 1d6-3        /// -2 to +3 waypoint offset
```

### Advanced Random Patterns

**Multiple independent values:**
```goblin
/// Generate RGB color
rgb_color = {
  red: roll 1d256-1,        /// 0-255
  green: roll 1d256-1,      /// 0-255  
  blue: roll 1d256-1        /// 0-255
}

/// Random 2D position
spawn_position = {
  x: roll 1d800+100,        /// 101-900
  y: roll 1d600+50          /// 51-650
}
```

**Detailed breakdown for analysis:**
```goblin
/// When you need to see the individual random components
price_breakdown = roll_detail 3d100+50
say "Base prices: " || price_breakdown.dice    /// [67, 23, 89] 
say "Subtotal: " || price_breakdown.sum        /// 179
say "Final price: " || price_breakdown.total   /// 229 (179 + 50)

/// Useful for debugging random generation
terrain_detail = roll_detail 5d10+20
say "Height samples: " || terrain_detail.dice  /// See individual height points
say "Terrain roughness: " || standard_deviation(terrain_detail.dice)
```

### Error Handling

The same error rules apply regardless of what numbers you use:

**DICE_BOUNDS** — Any invalid parameters:
```goblin
/// roll 0d1000     /// DICE_BOUNDS: need at least 1 roll
/// roll 1d0        /// DICE_BOUNDS: need at least 1-sided "die"  
/// roll -5d100     /// DICE_BOUNDS: negative count
```

The beauty of Goblin's `roll` is that it makes random number generation intuitive and self-documenting, whether you're making games, simulations, or any application that needs controlled randomness.

---

## 32.3 `freq` — Frequency Analysis

When you're balancing drop rates, analyzing player behavior, or just curious about distributions, `freq` counts how often each element appears in a list and gives you a clean frequency map.

```goblin
freq list → map
```

Perfect for analyzing loot drops, player choices, or any data where you need to see patterns.

**Basic frequency counting:**
```goblin
loot_drops = ["sword","potion","potion","gold","sword","potion","ring"]
drop_rates = freq loot_drops
say drop_rates    /// {"sword":2, "potion":3, "gold":1, "ring":1}

/// Analyze the results
most_common = drop_rates["potion"]    /// 3
say "Potions dropped " || most_common || " times"
```

**Analyzing dice rolls:**
```goblin
/// Roll a bunch of d6s and see the distribution
results = []
for i in 1..100
  add roll 1d6 to results
end

distribution = freq results
say distribution    /// {"1":16, "2":18, "3":15, "4":17, "5":19, "6":15}

/// Check if dice are fair
for value, count in distribution
  percentage = (count * 100.0) / 100
  say value || ": " || percentage || "%"
end
```

**Player behavior analysis:**
```goblin
player_actions = ["attack","defend","attack","spell","attack","defend","run"]
action_freq = freq player_actions
say action_freq    /// {"attack":3, "defend":2, "spell":1, "run":1}

/// AI can adapt based on player patterns
if action_freq["attack"] > action_freq["defend"]
  say "Player is aggressive, use defensive strategy"
end
```

---

## 32.4 `mode` — Finding the Most Common

`mode` tells you which elements appear most frequently in your data. Unlike `freq` which gives you all the counts, `mode` directly answers "what's the most common thing?"

```goblin
mode list → list
```

Returns a list because there might be ties (multiple elements with the same highest frequency).

**Finding dominant strategies:**
```goblin
enemy_types = ["orc","goblin","orc","slime","orc","slime","dragon","orc"]
most_common = mode enemy_types
say most_common    /// ["orc"] - orcs appear 4 times, more than anything else

/// Adapt spawning based on what's been seen
if most_common[0] == "orc"
  say "Too many orcs lately, spawn something different"
end
```

**Handling ties:**
```goblin
balanced_data = ["warrior","mage","warrior","mage","rogue"]
common_classes = mode balanced_data
say common_classes    /// ["warrior","mage"] - both appear twice

if len common_classes > 1
  say "Tied between: " || common_classes
else
  say "Clear winner: " || common_classes[0]
end
```

**Player preference analysis:**
```goblin
weapon_choices = ["sword","bow","sword","staff","bow","sword","axe","bow"]
preferred = mode weapon_choices
say "Player prefers: " || preferred[0]    /// "sword" (appears 3 times)

/// Game can suggest similar weapons
recommend_weapon_type(preferred[0])
```

---

## 32.5 `sample_weighted` — Weighted Random Selection

Sometimes you need randomness, but not all outcomes should be equally likely. `sample_weighted` lets you create biased probability tables where some items are more likely to appear than others. Perfect for loot tables, random encounters, or any system where rarity matters.

**Two input formats:**

```goblin
sample_weighted list_of_pairs    /// [("item", weight), ...]
sample_weighted weight_map       /// {"item": weight, ...}
```

Weights are relative - a weight of 10 is twice as likely as a weight of 5. You don't need to normalize to 100 or anything, just make the ratios match what you want.

**Classic loot table:**
```goblin
/// Traditional (value, weight) pairs
loot_table = [
  ("common_potion", 50),    /// 50/64 = ~78% chance
  ("rare_gem", 10),         /// 10/64 = ~16% chance  
  ("epic_sword", 3),        /// 3/64 = ~5% chance
  ("legendary_ring", 1)     /// 1/64 = ~1.5% chance
]

dropped_item = sample_weighted loot_table
say "Found: " || dropped_item
```

**Map-based weights (cleaner for complex tables):**
```goblin
encounter_table = {
  "weak_goblin": 40,
  "orc_warrior": 25,  
  "elite_troll": 8,
  "dragon": 2,
  "treasure_chest": 15,
  "empty_room": 30
}

encounter = sample_weighted encounter_table
say "You encounter: " || encounter
```

**Dynamic weight adjustment:**
```goblin
/// Adjust weights based on player level
base_monster_weights = {"slime": 10, "orc": 5, "dragon": 1}

/// Higher level players face tougher enemies
if player_level > 10
  base_monster_weights["dragon"] = 5    /// Dragons become more common
  base_monster_weights["slime"] = 2     /// Slimes become rare
end

next_enemy = sample_weighted base_monster_weights
```

**Rarity system with clear progression:**
```goblin
/// Item rarity follows a clear progression
item_rarities = {
  "common": 1000,      /// Very common
  "uncommon": 300,     /// 3x less likely than common  
  "rare": 80,          /// 4x less likely than uncommon
  "epic": 15,          /// 5x less likely than rare
  "legendary": 2       /// 7x less likely than epic
}

item_rarity = sample_weighted item_rarities
item = generate_item(rarity: item_rarity)
```

### Weight Requirements

**All weights must be ≥ 0:**
```goblin
/// This works
valid_weights = [("option_a", 0), ("option_b", 5)]   /// option_a never chosen

/// This fails  
/// bad_weights = [("option_a", -1), ("option_b", 5)]   /// WEIGHT_TYPE error
```

**Must have at least one positive weight:**
```goblin
/// This fails
/// all_zero = [("option_a", 0), ("option_b", 0)]   /// WEIGHT_EMPTY error
```

---

## 32.6 Game Design Recipes

Real patterns you'll use constantly in game development:

**Percentile dice (classic d100 as two d10s):**
```goblin
/// Old-school percentile dice: roll two d10s
d10_tens = roll 1d10        /// 1-10 for tens place
d10_ones = roll 1d10        /// 1-10 for ones place

/// Convert to 0-99, then adjust so 00 = 100
percentile = (d10_tens - 1) * 10 + (d10_ones - 1)
if percentile == 0
  percentile = 100
end

say "Rolled " || percentile || "%"

/// Use for percentage-based checks
if percentile <= player.luck_stat
  say "Lucky! Something good happens"
end
```

**Skill checks with degrees of success:**
```goblin
skill_check = roll 1d20 + skill_modifier
difficulty = 15

if skill_check >= difficulty + 10
  say "Critical success!"
elif skill_check >= difficulty + 5  
  say "Great success!"
elif skill_check >= difficulty
  say "Success"
elif skill_check >= difficulty - 5
  say "Partial success"
else
  say "Failure"
end
```

**Yahtzee-style scoring:**
```goblin
/// Roll 5 dice and analyze the results
yahtzee_roll = roll_detail 5d6
dice_values = yahtzee_roll.dice
say "Rolled: " || dice_values

/// Count frequencies for scoring
counts = freq dice_values
max_count = max(counts.values())

scoring = if max_count == 5 then "YAHTZEE!"
         elif max_count == 4 then "Four of a kind"  
         elif max_count == 3 then "Three of a kind"
         else "No multiples"
         end
say scoring
```

**Weighted random encounters based on region:**
```goblin
/// Different encounter tables for different areas
forest_encounters = {
  "peaceful_deer": 30,
  "wolf_pack": 20,
  "bandit_ambush": 15,
  "ancient_tree": 10,
  "treasure_cache": 5
}

dungeon_encounters = {
  "skeleton_warrior": 35,
  "treasure_chest": 25,
  "trap": 20,
  "boss_monster": 5,
  "secret_passage": 15
}

current_area = "forest"
encounter_table = if current_area == "forest" then forest_encounters
                 else dungeon_encounters
                 end

encounter = sample_weighted encounter_table
handle_encounter(encounter)
```

**Critical hit system with confirmation:**
```goblin
attack_roll = roll 1d20
target_ac = enemy.armor_class

if attack_roll >= target_ac
  /// Normal hit, check for critical
  if attack_roll == 20 or attack_roll >= (target_ac + 10)
    /// Potential critical, confirm with second roll
    confirm_roll = roll 1d20
    if confirm_roll >= target_ac
      /// Confirmed critical hit
      damage = roll 2 * weapon_damage_dice  /// Double damage dice
      say "CRITICAL HIT! " || damage || " damage!"
    else
      /// Failed to confirm, just normal hit
      damage = roll weapon_damage_dice
      say "Hit for " || damage || " damage"
    end
  else
    /// Regular hit
    damage = roll weapon_damage_dice  
    say "Hit for " || damage || " damage"
  end
else
  say "Miss!"
end
```

**Procedural loot generation:**
```goblin
/// Generate loot based on enemy type and player luck
function generate_loot(enemy_type, luck_modifier)
  base_loot = {
    "nothing": 40,
    "coins": 30,
    "potion": 20,
    "equipment": 8,
    "rare_item": 2
  }
  
  /// Luck improves chances
  base_loot["rare_item"] += luck_modifier
  base_loot["equipment"] += luck_modifier / 2
  base_loot["nothing"] -= luck_modifier
  
  /// Powerful enemies drop better loot
  if enemy_type == "boss"
    base_loot["rare_item"] *= 3
    base_loot["equipment"] *= 2
    base_loot["nothing"] = 0
  end
  
  loot_type = sample_weighted base_loot
  return create_item(type: loot_type, quality: roll 1d100)
end

player_loot = generate_loot("orc_chieftain", player.luck)
```

---

## 32.7 Error Handling

Goblin gives you clear error messages when random operations go wrong:

**DICE_PARSE** — Your dice notation isn't valid:
```goblin
/// roll 2d6x+1      /// DICE_PARSE: 'x' isn't valid in dice expressions
/// roll abc         /// DICE_PARSE: not a valid dice expression
/// roll 2d6++1      /// DICE_PARSE: double '+' operator
```

**DICE_BOUNDS** — Your dice parameters don't make sense:
```goblin
/// roll 0d6         /// DICE_BOUNDS: need at least 1 die
/// roll 2d0         /// DICE_BOUNDS: dice need at least 1 side  
/// roll -3d6        /// DICE_BOUNDS: negative dice count
/// roll 2d-4        /// DICE_BOUNDS: negative sides
```

**WEIGHT_TYPE** — Weights must be numbers:
```goblin
bad_loot = [("sword", "heavy"), ("potion", 5)]
/// sample_weighted bad_loot    /// WEIGHT_TYPE: "heavy" isn't numeric
```

**WEIGHT_EMPTY** — Need at least one positive weight:
```goblin
empty_table = [("nothing", 0), ("void", 0)]  
/// sample_weighted empty_table    /// WEIGHT_EMPTY: no positive weights
```

These tools give you everything you need to add authentic randomness and probability to your games. From simple dice rolls to complex weighted systems, Goblin makes randomness feel natural and controllable, just like rolling dice at a real tabletop.

# Appendix — Release Checklist (v1.5)

## ✅ Must-Have for v1.5

### Goblin Core

**gmarks basics** (see §23.9)  
- `gmarks_filter(prefix)` implementation & tests  
- Deterministic-build write policy gate + `.goblin/gmarks.audit.log`  

**Blob/JSON interop glue** (see §29.5)  
- Ensure JSON/YAML leave blobs alone by default (no auto-encode)  
- Example helpers to base64 when needed  

**Hashing & HMAC** (see §30)  
- `hash(data: string|blob, algorithm="sha256") -> string`  
- `hmac(data: string|blob, key: string|blob, algorithm="sha256") -> string`  
- Supported algorithms: `sha256`, `sha512`, `sha1` (warn), `md5` (strong warn)  
- Streaming over blobs; tests with known vectors  

**Divmod operator** (see §33)  
- `>>` operator returns Goblin-flair `q r r` format  
- Function alias `div_rem(a, b)` → tuple form  
- Golden-vector tests for integers, negatives, money/fixed-scale types  
- Errors: `DivisionByZero`, `ScaleMismatch`, `MoneyDivisionError`  

---

## ➕ Optional / v1.5+ (Can Push to First Point Release)

### Core Horde-Readiness

- Pure function runner mode (`--stdin / --stdout`)  
- Warm VM / preload mode (`goblin serve --preload`)  
- Determinism hardening knobs (`--seed`, block wall clock, etc.)  
- **Random ops reproducibility**: `roll`, `pick`, `shuffle`, `sample_weighted` deterministic under `--seed`  
- Resource limits (`--cpu-ms`, `--mem-mb`, `--max-steps`)  
- Correlation & idempotency (`request_id`, `--idempotency-key`)  
- Structured exit codes  
- Audit log enrichment with IDs/keys  
- Clock/testing hook (`freeze_time`)  

### Glam / Host-Side Horde-Readiness

- `scheduler::cron(spec, task)` (executes scripts/caps on schedule)  
- `s3::upload` / `netlify::deploy` glam  
- `metrics::emit` glam  
- Horde examples (Rust worker pool + Kubernetes YAML)  

### Other Optional Core Enhancements

- glam gmark rebalance (CLI) implementation  
- Extra blob helpers (beyond base64/hex) if needed for niche formats  
- **Divmod sugar helpers**: `q 10 >> 3`, `r 10 >> 3` (ergonomic accessors)  
- **Play/Random helpers** (see §32):  
  - `roll`, `roll_detail`  
  - `freq`, `mode`  
  - `sample_weighted`  
  - Game-design recipes validated against golden seeds  

---

If we ship just the must-have list, Goblin v1.5 Core will be fully usable, have a clean feature set, and Glam will cover the basics. Then we drop horde-readiness + deploy glam in v1.5.1 or v1.6 without delaying launch.

*[End of Goblin Language Specification v1.5 "Treasure Hoarder" - Refactored]*
