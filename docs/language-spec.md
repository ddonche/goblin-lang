# Goblin — Language Specification v1.5 "Treasure Hoarder"

## Philosophy

**Human-readable**: One statement per line; indentation defines blocks.

**Safe by default**: Undefined variables are errors (with "Did you mean…?" suggestions). No silent string coercion.

**Practical batteries**: CSV/YAML I/O, FS helpers, money with currency, friendly math helpers.

**File extension**: `.gbln`

---

## 1. Files, Printing, Variables

### 1.1 Files & Layout
- UTF-8, LF newlines
- Indent with spaces only (no tabs)
- One statement per line; blank lines allowed

**Comments:**
```goblin
/// Single-line comment

//// 
Block comment content
////
```
Block comment markers must appear alone on their line (no trailing text).

**Statements:**
```goblin
set @policy <name>    /// changes the active policy for the current scope
```

### 1.2 Printing
```goblin
say expr

/// Leading-quote shorthand: a line beginning with a quoted string is printed
"Hello" ≡ say "Hello"
```

### 1.3 Variables
```goblin
name = "Frank"
age = 45
price = 1.50
```
- Using a variable before assignment → `NameError` (with "Did you mean …?")
- Empty assignment (`x =`) → `SyntaxError`
- Numeric types (int/float/money) are inferred unless cast

### 1.4 Casting & Formatting
```goblin
/// Casts
str(x), int(x), float(x), bool(x), money(x, CUR?)

/// Parse helpers
int("$1,234") → 1234
float("$1,234.50") → 1234.5

/// Numeric formatting
fmt(x, ".2f"), fmt(x, ","), fmt(x, ",.2f")
```

### 1.5 Strings
```goblin
/// Interpolation
"Hello {name}"

/// Methods
.upper() .lower() .title() .slug()

/// Concatenation operators
| → no-space join for strings
|| → space join for strings
```
- Mixed types must be cast (e.g., `str(5) || str(10)`)
- `.` is for method/property access only
- Methods may be called on literals, e.g., `"hi".upper()`

---

## 2. Operators & Math

### 2.1 Precedence (high → low)
```
()
postfix operators: ** (square), // (square root), ++, --
**, ^^ (right-to-left)      [binary power and explain-power]
unary + - not !
* / % // >>                 [multiplicative family]
+ -
| then ||                   [string joins only]
comparisons: == != < <= > >= === !== is is not   (chaining allowed)
logical: and / or          (aliases: ! for not, && for and)
inline conditional ? ?? :   (right-associative)
```

**Notes:**
- Postfix forms (`**`, `//`, `++`, `--`) bind tighter than any binary operator (like parentheses)
- Bind only to the **nearest primary** (literal, variable, or parenthesized expression)
- `>>` sits at the same precedence level as `* / % //` (multiplicative group)
- **Lexer rule:** Postfix operators are recognized when immediately followed by line end, `)`, `]`, `}`, `:`, `,`, or another operator that cannot start an expression. Everything else is infix.

**Examples:**
```goblin
16// ** 2 → (sqrt(16)) ** 2 → 16
a + b** * 3 → a + (b**) * 3
9 // 2** → SyntaxError  /// postfix needs expression end
```

### 2.2 Division & Divmod
```goblin
/// Division types
/ → float division (numbers only)
// → quotient-only division (floor division, Python-style)  
>> → divmod returning pair (q, r) for numbers and money

/// Examples (numbers)
10.75 / 3 → 3.5833333333
10.75 // 3 → 3
10.75 >> 3 → (3, 1.75)
-7 / 3 → -2.3333333333
-7 // 3 → -3      /// floor semantics
-7 >> 3 → (-3, 2) /// because -7 = (-3)*3 + 2

/// Money (using `/` with money → MoneyDivisionError)
price = $10.75
q = price // 3      /// USD 3.00 (quotient only)
q, r = price >> 3   /// (USD 3.00, USD 1.75)
price / 3           /// MoneyDivisionError

/// Remainder operator
% → remainder (modulus)
/// For money: amount % n returns money remainder (same as second element of amount >> n)
```

### 2.3 Exponentiation
```goblin
** → normal exponent (5 ** 3 → 125)
^^ → explain-power
say 5 ^^ 3 → 125 (5 × 5 × 5)
```
- Same precedence/associativity/value as `**`
- `^` reserved for future XOR (not an operator in v1.5)

### 2.4 Postfix Math Shorthands
```goblin
n** → square (9** → 81)
n// → square root (9// → 3)
x++ → yields old x, then x = x + 1
x-- → yields old x, then x = x - 1

/// Precedence examples
16// ** 2 → (sqrt(16)) ** 2 → 16
a + b** * 3 → a + (b**) * 3
arr[i]++ * 2 → (old arr[i]) * 2, then mutate
```

**Types:**
- `n**` and `n//`: int/float only (money not allowed; TypeError)
- `x++`/`x--`: int/float/money; money changes by 1 whole unit
- Lvalues only (e.g., `arr[i]++` ok). Not on literals/temporaries: `(x+1)++` → SyntaxError
- No chaining: `x++++` and `(x++)++` → SyntaxError
- No prefix forms (`++x`, `--x`) in v1.5

### 2.5 Bitwise
Operators removed to avoid conflict with string pipes. Use functions instead:
```goblin
bit.and(a,b), bit.or(a,b), bit.xor(a,b), bit.not(x), bit.shl(x,n), bit.shr(x,n)
```

### 2.6 Built-in Math Functions
All support variadic and array forms:
```goblin
add sub mult div mod divmod pow root floor ceil round abs min max rand randint
add(1,2,3) == add([1,2,3])
sum(arr)  /// semantic twin of add for arrays
```

### 2.7 Increments & Compound Assignment
```goblin
/// Postfix ++ / -- are expressions (yield old value, then mutate lvalue)
/// Valid on int, float, money

/// Compound assignment
+= -= *= **=
```
Money ops must follow currency rules (same currency, see §10). For money, +=, -=, and *= follow the same promotion, precision, and ledger rules described in §10 (including precision:/policy:).

**Money restrictions:** `/=` and `%=` with money → `MoneyDivisionError` (same as `/` and `%` restrictions in §10.4).

---

## 3. Comparisons, Booleans, Truthiness

```goblin
/// Equality
== (value); strict: === / !== (type + value)

/// Aliases
is ≡ ==, is not ≡ !=

/// Chaining allowed
if 1 < x < 10

/// Falsey values
false, 0, 0.0, "", [], {}, nil
```
**Note:** Undefined var access → error, not falsey

---

## 4. Conditionals

Goblin supports four conditional keywords:
* `if` — run the block if the condition is **true**.
* `unless` — run the block if the condition is **false** (syntactic sugar for `if not`).
* `elif` — test a new condition if all previous `if`/`unless` branches failed.
* `else` — run the block if no previous branch executed.

### 4.1 Block Form — Classic and Readable

```goblin
if cond
    ...
elif cond
    ...
else
    ...

unless cond
    ...
else
    ...
```

* No colons; indentation defines blocks.
* `elif` and `else` are optional.
* `unless <condition>` is compiled exactly as `if not (<condition>)`.
* You may freely mix `if` and `unless` with `elif` and `else`.
* Last expression in a branch is the value returned by that branch **only if** you explicitly use it (assign/say/etc.). The block form itself does not yield a value.

**Examples:**
```goblin
if score >= 90
    say "A"
elif score >= 80
    say "B"
elif score >= 70
    say "C"
else
    say "F"

unless user.is_admin
    deny_access()
else
    grant_admin_access()

if user.is_verified
    process_order()
elif user.needs_verification
    send_verification_email()
else
    require_signup()
```

### 4.2 Judge — Declarative, Expression‑Oriented Branching
`judge` is a compact, template‑style conditional that **returns a value**. It comes in multiline and inline forms.

#### 4.2.1 Multiline Form (Most Readable)
```goblin
result = judge
    cond1: expr1
    cond2: expr2
    ...
    else: exprN
```
* Evaluated **top‑to‑bottom**; first true `cond:` wins.
* `else:` is optional, but recommended. If omitted and nothing matches → `nil`.
* Each `expr` is any expression (string literal, function call, money, pct, etc.).

**Examples:**
```goblin
/// REPL: prints "A" because REPL auto-prints expression results
judge
    score >= 90: "A"
    score >= 80: "B"
    score >= 70: "C"
    else: "F"

/// Script: silent, because you didn't use the value
grade = judge
    score >= 90: "A"
    score >= 80: "B"
    score >= 70: "C"
    else: "F"
say grade  /// prints later
```

You can also put **actions** inside an expression, but remember `judge`'s value is whatever the winning expression evaluates to:

```goblin
judge
    total > $100: say "High roller!"
    else: say "Thanks!"
```
(Here the value is the result of `say ...` which is typically `nil`—that's fine if you only care about the side effect.)

#### 4.2.2 Inline Form (Template‑Style, One Line)
```goblin
judge: cond1: expr1 :: cond2: expr2 :: ... :: else: exprN
```

**Examples:**
```goblin
/// Print immediately
say judge: score >= 90: "A" :: score >= 80: "B" :: score >= 70: "C" :: else: "F"

/// Store and use later
grade = judge: score >= 90: "A" :: score >= 80: "B" :: score >= 70: "C" :: else: "F"
say grade
```

**Important:** Outside the REPL, a bare inline `judge` with no `say`/assignment is a no‑op:
```goblin
judge: score >= 90: "A" :: else: "F"  /// does nothing in a script
```
Use `say` or assign it to a name.

#### 4.2.3 Discarded‑Value Warning (Scripts)
If a `judge` value is evaluated and **not used** in non‑interactive mode, Goblin emits:
```
UnusedJudgeValueWarning: result of 'judge' is ignored (line N). Assign it or wrap with 'say'.
```
*(No warning in the REPL, where auto‑print is expected.)*

### 4.3 Interop Notes
* `judge` is an **expression**; it can appear anywhere an expression can:
```goblin
label = "Tier " || judge: price >= $100: "Gold" :: price >= $50: "Silver" :: else: "Bronze"
```
* Branch expressions follow all usual type rules (money, pct, etc.).
* Short‑circuit semantics: evaluation stops at the first matching condition.

### 4.4 Removed: Inline `? ?? :`
The old inline conditional (`? ?? :`) is **removed** in v1.5 to favor readability and consistency with templates. `judge` and the classic block form replace it.

**Before (old):**
```goblin
say score >= 90 ? "A" ?? score >= 80 ? "B" ?? score >= 70 ? "C" : "F"
```

**Now (pick one):**
```goblin
say judge: score >= 90: "A" :: score >= 80: "B" :: score >= 70: "C" :: else: "F"

/// or the classic block:
if score >= 90
    say "A"
elif score >= 80
    say "B"
elif score >= 70
    say "C"
else
    say "F"
```

### 4.5 Quick Cheat‑Sheet
* **Block:** `if cond ... elif cond ... else ...`
* **Unless:** `unless cond ... else ...` (same as `if not cond`)
* **Judge (multiline):** `x = judge c1: v1 c2: v2 else: v3`
* **Judge (inline):** `say judge: c1: v1 :: c2: v2 :: else: v3`
* **Warning (scripts):** Ignored `judge` → `UnusedJudgeValueWarning` with line number and hint.

---

## 5. Loops

```goblin
/// Ranges
1..5   /// inclusive
1...5  /// exclusive
5..1   /// reversed (step -1)

/// Loop forms
for i in 1..5
for v in array
for i, v in array
for k, v in map
while cond

/// Control
skip  /// continue
stop  /// break
```
Using `skip`/`stop` outside loops → SyntaxError

---

## 6. Collections

### 6.1 Arrays
```goblin
/// Literals
[1, 2, 3]

/// Indexing (negative indexing supported)
arr[0], arr[-1]

/// Slicing
a[1:4]

/// Length
len(a) or a.len()

/// Mutators
push pop unshift shift insert remove clear, reverse(), sorted()

/// Helpers
sum(a), join(a, sep)
```

### 6.2 Maps
```goblin
/// Literals
{key: "val", price: 1.5}

/// Access
m.key         /// identifier keys
m["any-key"]  /// any key

/// Methods
keys() values() items() delete()
```

---

## 7. Functions

```goblin
fn greet(name="Traveler")
    "Hello, {name}"   /// last expression is implicit return
end
```
- `return` allowed; else last expression returns
- Defaults supported; arity is strict (defaults satisfy arity)
- Single-line functions: `fn add(a, b) = a + b`

---

## 8. Key-Value "Pipes" (::) and Templates

### 8.1 Inline KV with ::
```goblin
/// Anywhere a short record is helpful
card: "Two of Cups" :: price: .99 :: qty: 4
```
- `::` separates fields
- LHS must be identifiers
- RHS any expression (including nested maps/arrays)

### 8.2 Templates

#### 8.2.1 Positional with Defaults
```goblin
/// Define template
@cards = card: "{name}" :: price: .99 :: qty: 1

/// Use template
@cards =
    card: "{name}" :: price: .99 :: qty: 1
    "Ace of Cups"
    "Two of Cups" :: 1.25
    "Three of Cups" :: :: 2
    "Four of Cups" :: 1.25 :: 2
    "Five of Cups" :: qty: 3         /// keyed override also allowed
```

**Rules (positional):**
- Values map in the same order as template fields
- Trailing fields may be omitted → defaults are used
- To skip a middle field but provide a later one, leave the slot empty with `::`
- Keys are optional—`price: 1.25` is allowed for clarity, but not required
- The first field can itself be templated, e.g., `card: "{name}"`

#### 8.2.2 Arrays, Loops, and Selective Overrides
```goblin
minor_arcana = [
    "Ace","Two","Three","Four","Five",
    "Six","Seven","Eight","Nine","Ten",
    "Page","Knight","Queen","King"
]

@tarot = card: "{suit} — {card}" :: price: .99 :: qty: 1

suit: "Cups"
    for card in minor_arcana
        "{card}"
    "Ace" :: qty: 2                 /// override qty only
    "Two" :: price: 1.25            /// override price only
    "Three" :: 1.25 :: 2            /// positional override of price, qty

suit: "Wands"
    for card in minor_arcana
        "{card}"
```

**Rules (arrays/loops):**
- Template parameters fill from defaults unless overridden on a line
- Overrides match the generated entry by the current loop variables (`{suit}`, `{card}`, etc.)
- You may use positional overrides (`::`) or keyed overrides (`price: 1.25`)
- `_` is not required; just use `::` to skip a position when you need to reach a later field

---

## 9. Objects & Classes

### 9.1 Definition & Constructor
A **class** defines a reusable object type with **public** and **private** fields and methods. Calling the class name **creates a new instance**.

**Two equivalent styles:**

**Template Class (Goblin-flavored):**
```goblin
class Pet = name: "{name}" :: age: 0 :: species: "dog"
    fn speak()
        "Hi, I'm {name}, a {age}-year-old {species}."
    end
end
```

**Regular Class (function-style):**
```goblin
class Pet(name: string, age: int = 0, species: string = "dog")
    fn speak()
        "Hi, I'm {name}, a {age}-year-old {species}."
    end
end
```

**Constructor signature:**
* The **constructor line** defines the **field order**, **types**, and **defaults**.
* Reordering fields is a **breaking change**.
* Template style uses `::` separators; function style uses comma separators.

### 9.2 Instantiation
Both class styles support both instantiation methods:

* **Template style:** `Pet: "Fido" :: 3 :: "cat"`
* **Function style:** `Pet("Fido", 3, "cat")` /// maps **strictly** to declared field order

**Options:**
* **Named overrides:** `Pet("Rex", species: "wolf")`
* **Skip positional:** `Pet("Max", :: "cat")` /// skip `age`, set `species`
* **Defaults:** Omitted fields use their declared default; if a required field has no default → `TypeError("missing field 'name' for Pet")`.
* **Type safety:** Invalid overrides (e.g., `species: 42`) → `TypeError`.

### 9.3 Field Semantics
* **Interpolation:** `name: "{name}"` → expects string parameter.
* **Capture:** `price: {price}` → expects typed parameter (inferred from usage).
* **Computed defaults:** `sku: slug("{title}")` allowed.
* **Per‑instance defaults:** Evaluate **at instantiation** (not class load). E.g., `id: uuid()` is unique per instance.

### 9.4 Access Rules
* **Public fields** (no `#`): readable/writable externally. Auto‑generated accessors:
   * `x()` getter
   * `set_x(v)` setter  
   * `is_x()` for booleans
   
   **Precedence:** If a class defines a method with the same name as an auto-generated accessor, the user-defined method takes precedence (must match expected signature).

* **Private fields** (`#x`): accessible **only** inside class methods. External access → `PermissionError("cannot access private field '#energy'")`.
* **Readonly:** `readonly id: uuid()` → getter only; writing → `TypeError("field 'id' is readonly")`.

### 9.5 Lifecycle
* `on_create()` (optional) runs **after** field binding but **before** instance is returned; throw to abort construction.
* **Determinism:** `on_create()` **must be deterministic**. Side effects (FS/NET/env) require glam with declared permissions; non‑deterministic work raises `DeterminismError` in deterministic mode.

### 9.6 Style Choice
*Any Goblin class can be defined in template form or function form. The two are interchangeable; choose the style that's most readable for your project.*

* **Template style** → declarative, readable, matches `@template` variables
* **Function style** → compact, familiar, traditional

### 9.7 Errors
* Missing field → `TypeError("missing field 'x' for Pet")`
* Unknown field → `TypeError("unknown field 'foo' for Pet")`
* Type mismatch → `TypeError("field 'price' expects money, got int")`
* Currency mismatch → `CurrencyError` (see §10)
* Private access → `PermissionError("cannot access private field '#x'")`

### 9.8 Serialization & Helpers
* `write_json` / `write_yaml`: serialize **public fields** by default; `include_private: true` opt‑in.
* `to_map()` → map of public fields.
* `copy(overrides…)` → shallow copy with overrides.
* Structural equality (`==`) compares public fields; `is` is identity.

### 9.9 Morph Compatibility
`morph` (§24) operates on **public fields via auto‑accessors**; `#private` remains sealed unless you expose public getters/setters.

### 9.10 No Inheritance
Goblin has **no class inheritance**. Prefer **composition** and `morph` (§24).

### 9.11 Percent & Money Behavior
Inside methods and field expressions:
* **Percent is "percent of the left operand"** (§11):
   * `$8 + 25%` → `$10` (`$8 + ($8 * 0.25)`)
   * `$8 * 25%` → `$16` (`$8 * ($8 * 0.25)`)
   * `$8 / 25%` → `$4` (`$8 / ($8 * 0.25)`)
* **Money is fixed‑point** with precision policy and ledger (§10).
* `/` on money is forbidden → `MoneyDivisionError`. Use `//` for quotient or `>>` for divmod.

**Example:**
```goblin
class Book = title: "{title}" :: price: $0
    fn discount(p: percent) = price - (price * p)    /// §11 percent rule
end

b = Book: "Guide" :: $8
say b.discount(25%)     /// $6
say b.price() + 25%     /// $10  
say b.price() * 25%     /// $16
say b.price() / 25%     /// $4
```

---

## 10. Money & Currency

Money behavior (currency, precision, rounding/truncation) is governed by the active policy (§27).  
When integrating with external systems that use flawed rounding, see §27.4.1 compat for mode/shame_level controls.

### 10.0 Money Precision Policy

Money behavior is governed by the active policy (§27).

```goblin
/// Example policy configuration
@policy = "strict_money"
    money: { currency: "USD", precision: 2, policy: "strict" }
```

**Parameters:**
- **precision:** integer ≥ 0 (decimal places in major units)
- **policy:** `"truncate"` (default) | `"warn"` | `"strict"` | `"defer"`
  - `"truncate"`: canonicalize via truncate-and-ledger (no rounding)
  - `"warn"`: same as truncate, also warn `MoneyPrecisionWarning(...)`
  - `"strict"`: throw `MoneyPrecisionError` if any op would produce sub-precision
  - `"defer"`: carry full precision until explicit settlement or export boundary

**Goblin Aliases:** `policy: "cleave"` (= truncate), `policy: "grumble"` (= warn), `policy: "grim"` (= strict), `policy: "hoard"` (= defer)

**Scope:** Per currency. Unset currencies use global default unless overridden.

### 10.0.1 Defer ("Hoard") Policy — Keep crumbs until told otherwise

**Purpose:** For multi-step financial math (compound interest, accruals, amortization), users must be able to carry sub-precision forward across steps and only canonicalize when they say so. Policy `"defer"` does exactly that.

**Policy keywords:**
- `policy: "defer"` — canonical name
- `policy: "hoard"` — goblin alias (exact synonym)

**Example:**
```goblin
@policy = "compound_interest"
    money: { currency: "USD", precision: 2, policy: "defer" }
```

**Semantics (precise but simple):**

Under `"defer"`, money values internally keep full intermediate precision (engine precision), instead of truncating at each operation.

Display (`str(x)`, `say x`) still shows the canonical 2-dp surface so files and logs stay human-readable; the extra fraction is carried but not lost.

Canonicalization to the configured precision occurs only when:
- you call an explicit settle helper (below), or
- you cross a boundary that requires canonical amounts (e.g., CSV/JSON export in "string/object" modes, FS writes by a glam that declares money surfaces), or
- you temporarily opt into truncate/warn/strict via a scoped override.

**Important:** When canonicalization happens, the sub-precision excess is moved into the standard remainder ledger (§10.7/§10.9). `"defer"` changes when we commit crumbs, not where they go.

**New helpers (small, focused):**

```goblin
settle(x: money) -> money
```
Canonicalize x to current precision; adds the sub-precision remainder to the ledger; returns the canonical money. (Idempotent if already clean.)

```goblin
excess(x: money) -> float
```
Inspect the sub-precision being carried in defer mode (major-unit float; sign can be ±). For dashboards and tests. Returns 0.0 in non-defer modes.

```goblin
with_money_policy(policy, block)
```
Temporarily override precision policy inside block. Useful to do a one-off truncation step inside an otherwise deferred workflow.

```goblin
with_money_policy("truncate")
    // this block behaves like truncate
end
```

**Export/serialization boundaries:**

Core writers (`write_csv`, `write_json` in "string/object" modes, glam exports that surface money as text/decimal) canonicalize amounts they serialize. In defer mode, this implies an automatic ledger entry for any carried remainder on those values at the moment of write.

If a glam needs raw exacts (rare), it must opt in via its contract/options and emit in units mode (§14.2.2: "units"), which carries integers + precision without touching the ledger.

**Examples:**

**A. Compound interest (works as users expect)**
```goblin
@policy = "compound_interest"
    money: { currency: "USD", precision: 2, policy: "defer" }

set @policy compound_interest

bal = $1000.00
for _ in 1..3
    bal = bal * 1.05
end

say bal          /// USD 1157.625 (shows actual carried value)
say excess(bal)  /// 0.0 (no hidden precision - it's all visible)

/// End of quarter: settle to books (moves crumb to ledger)
bal = settle(bal)
say bal          /// USD 1157.63
say remainders_total()  /// { USD: $0.00... } + ledger includes 0.005 from settle()
```

**B. Mixed workflow: one step must be "clean"**
```goblin
@policy = "mixed_precision"
    money: { currency: "USD", precision: 2, policy: "defer" }

set @policy mixed_precision

x = $10.00 * 2.5     /// carries extra precision if any
/// One particular step must match a legacy system's truncate behavior:
with_money_policy("truncate")
    x = x * 1.07     /// canonicalized here; any crumb logged immediately
end
/// Continue deferring after the block
x = x * 1.03
```

**C. Export boundary auto-settles for that surface**
```goblin
@policy = "export_defer"
    money: { currency: "USD", precision: 2, policy: "defer" }

set @policy export_defer

invoice = $1157.625    /// via prior math
write_csv("dist/invoice.csv", [ { total: invoice } ])
/// -> Writes "USD 1157.63"
/// -> Moves 0.005 to remainder ledger (export is a canonicalizing boundary)
```

**Error/warning behavior (unchanged where it should be):**

`"strict"` still forbids sub-precision creation; `"warn"` behaves like truncate + warning.

`"defer"` allows sub-precision and never warns until a canonicalization point (explicit settle or an export).

All cross-currency rules, `/` prohibitions, and divmod semantics remain as in §10.4—§10.7.

**Notes (goblin flavor):**

"Hoard mode" = the coin-counting goblins keep every shaving in your pouch until you say "book it."

Once booked (by settle or export), the shavings go into the hoard ledger and can be handled exactly like today with `drip_remainders` / `goblin_payout` or `goblin_stash` patterns.

### 10.1 Type & Precision Handling
- Values are stored as integer quanta of size `10^(-precision)` in major units; any sub-quantum remainder is tracked per §10.7.
- Policy applies at canonicalization sites: `money(v,C)`, promotion (`money` ± int/float), `*`, `tax`, `with_tax`, `convert`, and glam functions returning money.
- **No rounding** - excess precision becomes explicit remainder
- Perfect conservation: `input_value = money_part + remainder`

### 10.2 Construction & Literals
```goblin
money(1.50)        /// uses active policy currency
money(1.50, USD)   /// explicit currency

/// Accepted literals (all yield money(amount, CUR)):
/// ISO formats
USD 1.50, USD$1.50, 1.50 USD

/// Symbols via map
$1.50, €1.50, £1.50, ¥150, ₹99, ₽200

/// Suffix (if enabled)
1.50€, 99₹

/// Disambiguated $
US$1.50, C$1.50, A$…, NZ$…, MX$…, HK$…, S$…

/// Sign
-$5.00, $-5.00, -USD 5.00

/// High-precision construction (truncates, tracks remainder)
money(10.555, USD) → $10.55 + remainder(0.005)

Any numeric (int or float) passed to money(v, CUR) is canonicalized: the quantum value is truncated toward zero and any sub-quantum remainder is recorded in the ledger. This applies equally to literals, variables, or computed expressions—no special cases for floats.

/// Display
say money(3.2, USD) → USD 3.20
str(money) → CUR 1.23
fmt(float(m), ",.2f")  /// numeric only
```

If active policy is `strict` and `v` has more than `precision` decimals, raise `MoneyPrecisionError` (no ledger update).

### 10.3 Number Typing & Defaults
```goblin
/// If no default yet:
123      → int
1.23     → float
$1.50    → money

/// Policy affects subsequent untyped numerics:
set @policy usd_money   /// if this policy sets money.currency: USD
x = 5    → USD 5.00
y = 2.5  → USD 2.50

/// Explicit casts always win:
age = int(45), tax = float(.0725), price = money(.99, EUR)
```

### 10.4 Arithmetic & Comparisons
```goblin
/// Allowed (same currency): + - * // >> %
/// Not allowed: / on money (→ MoneyDivisionError)

/// Scalar multiply/divide by numbers:
money * int|float → money (same currency) + remainder tracking
money // int → quotient money (no remainder returned)
money >> int → pair (quotient: money, remainder: money)
money % int → remainder money (alias of second element of money >> int)
```

**Division behavior:**
- `money / anything` → `MoneyDivisionError` 
- `money // int` → quotient only (same as `(money >> int).first`)
- `money >> int` → complete divmod pair `(quotient, remainder)`
- `money % int` → remainder only (same as `(money >> int).second`)

**Compound assignment restrictions:** `/=` and `%=` with money → `MoneyDivisionError`.

**Scalar multiply** converts to major units, multiplies exactly, then canonicalizes (minor-units formulation is equivalent).

**Promotion rule:** If one operand is `money(CUR)` and the other is `int|float`, promote numeric to `money(CUR)` and operate (except `/`, which is disallowed). Promotion also applies to compound assignments (+=, -=, *= etc.), which are syntactic sugar for the corresponding binary operations.

**Negative money** follows the same rules. For money // int, integer division truncates toward zero; the remainder has the same sign as the dividend or is zero.

**Cross-currency arithmetic/comparison** → `CurrencyError` (no coercion).

### 10.5 Currency Config
```yaml
currency:
  default: USD
  allow_suffix: true
  symbol_map:
    "$": USD
    "US$": USD
    "C$": CAD
    "A$": AUD
    "NZ$": NZD
    "MX$": MXN
    "HK$": HKD
    "S$": SGD
    "€": EUR
    "£": GBP
    "¥": JPY
    "CN¥": CNY
    "₹": INR
    "₽": RUB
```
Build-time override: `goblin build … --currency USD`

Policies do not redefine symbol maps; they only set currency, precision, and policy.

### 10.6 Increments with Money
```goblin
money++ / money--  /// add/subtract exactly 1 whole unit
```
- Postfix `**` and `//` are not allowed on money (TypeError)
- To change at quantum level: do it explicitly (`price = price + .05`)

### 10.7 Even Splits & Remainder Ledger

#### Hard Rule on Division
Using `/` with money always errors: `MoneyDivisionError: Use // or >> for quotient, or divide_evenly(total, parts).`

#### Divmod for Money
```goblin
q = total // parts     /// quotient only
q, r = total >> parts  /// quotient and remainder
```
`q` and `r` are money at current precision; no ledger change.

#### Even Split Helper
```goblin
divide_evenly(total: money, parts: int) -> array<money>

/// Goblin alias
goblin_stash(total: money, parts: int) -> { shares: array<money>, escrow: money }  // = divide_evenly_escrow()
```
- Distributes integer quanta: `r` shares of `(q+1 quantum)`, else `q`.
- Sum equals total exactly; ledger unchanged.
- **Sugar:** `divide_evenly(A // n)` ≡ `divide_evenly(A, n)`.

**Special Output for escrow:**
- When using `divide_evenly_escrow()`: *"Goblins stashed ${amount} for safekeeping"*

#### Escrow Even Split
```goblin
divide_evenly_escrow(total: money, parts: int)
  -> { shares: array<money>, escrow: money }
```
- Returns `parts` shares all at floor share `q`; all leftover goes to `escrow` (0 ≤ escrow < 1 quantum).
- **Conservation:** `add[shares] + escrow == total`; ledger unchanged.

#### Remainder Ledger (Audit)
Goblin tracks any money remainder you don't capture:

```goblin
/// If you ignore remainder, it's tracked:
q, _ = total >> n

/// High-precision construction tracking:
money(10.555, USD)  /// Logs 0.005 remainder automatically

/// End-of-script helpers:
remainders_total()   → map { CUR: money }
remainders_report()  → human-readable summary lines
clear_remainders()   → reset ledger
```

`remainders_total()`, `remainders_report()`, `clear_remainders()` operate on the sub-precision ledger (not the escrow helper above).

### 10.8 Currency Conversion
```goblin
convert(amount: money(C1), to: C2, rate: float) → money(C2)
```
Multiplies amount in major units by rate (exact rational).

Canonicalizes to C2 quanta + remainder.

Logs any sub-quantum in the target currency's ledger.

Cross-currency arithmetic without explicit convert remains a CurrencyError.

Policy applies in the **target** currency; in `strict`, sub-precision results error.

### 10.9 Dripping Remainders

```goblin
drip_remainders(threshold=1, commit=false, label=nil) -> map<CUR, money>

/// Goblin alias
goblin_payout(threshold=1, commit=false, label=nil) -> map<CUR, money>  // = drip_remainders()
```

- **threshold:** chunk size per currency (default = 1 quantum at current precision). Accepts:
  - integer (quanta), or a money, or a `{CUR: money}` map.
- **commit=false** (default): audit-only. Compute potential emission, append to log, return `{}`, do not modify the ledger.
- **commit=true**: actually reduce the ledger by emitted amount(s) and return `{ CUR: money }`.

Always appends a JSON line to `dist/remainders.log` with before/after, potential/emitted, currency, precision, label, and timestamp.

**Special Output:**
- When `commit=true`: *"Goblins paid out ${amount} from their hoard"*
- When `commit=false`: *"Goblins would pay out ${amount} (audit only)"*

### 10.10 Allocation Patterns

To settle dripped amounts across recipients, use:
- `divide_evenly(drops.CUR, n)` to consume into shares, or
- `divide_evenly_escrow(drops.CUR, n)` to hold back remainder centrally.

**Perfect Conservation Examples:**
```goblin
set @policy site_default

/// Construction with remainder
precise_amount = money(100.567, USD)  /// $100.56 + remainder(0.007)

/// Division with remainder
q, r = $100.00 >> 3   /// q = $33.33, r = $0.01
/// Conservation: $100.00 = $33.33 × 3 + $0.01 ✓

/// Even split with perfect distribution
shares = divide_evenly($100.00, 3)
/// shares → [$33.34, $33.33, $33.33]
/// Conservation: $33.34 + $33.33 + $33.33 = $100.00 ✓

/// Escrow split
result = divide_evenly_escrow($100.00, 7)
/// result.shares → 7 × $14.28, result.escrow → $0.04
/// Conservation: 7 × $14.28 + $0.04 = $100.00 ✓

/// Remainder tracking for ignored values
_, _ = $100.00 >> 7    /// remainder logged automatically
say remainders_total()  /// => { USD: $0.02 }
clear_remainders()
```

### 10.11 Money Allocation Helper

```goblin
allocate_money(total: money, weights: array<number>) -> array<money>

/// Goblin alias
goblin_divvy(total: money, weights: array<number>) -> array<money>
```

Allocates a monetary total across weighted portions with perfect conservation.

**Rules:**
- No rounding loss; `sum(outputs) == total` at declared precision
- Honors money policy (truncate/strict/warn/defer) and currency precision
- Weights are proportional; they don't need to sum to any particular value
- Extra precision from allocation math goes to remainder ledger per policy

**Algorithm:**
1. Calculate total weight: `W = sum(weights)`
2. Calculate ideal shares: `share[i] = total * (weights[i] / W)`
3. Truncate each share to quantum precision
4. Distribute any leftover quanta to maximize accuracy (largest remainder method)

**Examples:**
```goblin
set @policy site_default

/// Equal three-way split
payouts = allocate_money($100.00, [1, 1, 1])
say payouts  /// [$33.34, $33.33, $33.33]

/// Weighted allocation
shares = allocate_money($100.00, [1, 2, 7])  /// 10%, 20%, 70%
say shares   /// [$10.00, $20.00, $70.00]

/// Complex weights
result = allocate_money($100.00, [3.5, 1.2, 5.3])
/// Allocates proportionally: 35%, 12%, 53%
say result   /// [$35.00, $12.00, $53.00]

/// Perfect conservation guaranteed
weights = [17, 23, 41, 19]
allocated = allocate_money($1000.00, weights)
say sum(allocated) == $1000.00  /// true
```

**Error Handling:**
- Empty weights array → `ValueError("allocate_money requires non-empty weights")`
- Zero or negative total → `ValueError("allocate_money requires positive total")`
- All weights zero → `ValueError("allocate_money requires non-zero weights")`
- Non-money total → `TypeError("allocate_money expects money total")`

---

## 11. Percent Type — "Percent of what?"

### 11.0 Reserved tokens / keywords

**Reserved**: `%` (postfix percent), `%s` (postfix "percent of self"), `of` (base binder), `pct` (constructor).

**Modulus** uses infix `%` with spaces: `a % b`.

### 11.1 Core principle (CIPO)

Goblin uses the Context‑Independent Percent Operator rule:

**`%` is a first‑class percent value; it is never "naked." It always denotes percent of a base.**

- **Default base** for `%` is 1 (programmer‑style).
- This makes `p%` equal to `p/100` as a numeric factor.
- **`%s` (self)** means percent of the left operand (calculator‑style), for all four operators.
- **`% of E`** names an explicit base E.
- **No operator changes the meaning of `%`**. There is no context magic.

### 11.2 Literals and construction

**Literal**: `25%` → numeric factor 0.25 in numeric contexts.

**Constructor (percentage points)**:
```goblin
pct(25)     → 25%         /// 0.25 in numeric contexts
pct(0.5)    → 0.5%        /// 0.005 in numeric contexts
pct(-10)    → -10%
str(25%)    → "25%"
```

`pct(n)` always interprets n as percentage points.

### 11.3 Operator forms (explicit and uniform)

#### A) `%` — percent of 1 (default)
```goblin
8 * 25%   = 2           /// 8 * 0.25
100 / 25% = 400         /// 100 / 0.25
8 + 25%   = 8.25        /// 8 + 0.25
8 - 25%   = 7.75        /// 8 - 0.25
```

#### B) `%s` — percent of self (left operand)
```goblin
8 + 25%s  = 10          /// 8 + (0.25 * 8)
8 - 25%s  = 6           /// 8 - (0.25 * 8)
8 * 25%s  = 16          /// 8 * (0.25 * 8)
8 / 25%s  = 4           /// 8 / (0.25 * 8)
```

#### C) `% of E` — explicit base
```goblin
8 + (25% of 50) = 20.5  /// 8 + (0.25 * 50)
```

#### D) `%` with spaces — modulus (unchanged)
```goblin
8 % 3 = 2
```

### 11.4 Binding & precedence

- `p%`, `p%s`, and `p% of E` are **atomic numeric factors** (postfix/primary), binding tighter than `*//`.
- `of` binds to the percent literal: `p% of E` is one unit.
- **Spaced modulus** (`a % b`) is a multiplicative infix operator alongside `*` and `/`.

**Example**:
```goblin
x + 25%s * 2  ==  x + ((25% of x) * 2)
```

### 11.5 Desugaring (spec‑level operational definition)

```
A ∘ (p%s)        ⇒  A ∘ ((p/100) * A)         /// ∘ ∈ { +, -, *, / }
p%               ⇒  (p / 100)
(p% of B)        ⇒  (p / 100) * B
A % B  [spaced]  ⇒  Mod(A, B)
pct(X)           ⇒  (X / 100)  as a percent value
```

### 11.6 Functions and composition

Percent values are **dimensionless numbers** and work anywhere numbers work:

```goblin
sqrt(25%)  = sqrt(0.25) = 0.5
(25%)^2    = (0.25)^2   = 0.0625
sin(50%)   = sin(0.5)
```

With `of`, the result inherits the unit of the base (e.g., money, length).

### 11.7 Money interop (with §10)

- `percent × money` uses §10 fixed‑point rules and precision.
- **Division on money** follows §10: if the result is money, divide by a scalar, not money. Use quotient/remainder for money‑to‑money division.

**Examples**:
```goblin
$80 * 10%        = $8.00
10% of $80       = $8.00
($80 + 15%s)     = $92.00
```

### 11.8 Tax helpers (unchanged API; now true percent type)

```goblin
tax(subtotal, rate_or_rates, compound=false) → tax amount
with_tax(subtotal, rate_or_rates, compound=false) → subtotal + tax(...)
```

- **`rate_or_rates`**: accept `0.10` (decimal) or `10%` (percent type), or array `[8.25%, 1%]`.
- **`compound=true`** applies sequentially; else additive.
- **Precision**: truncation, with sub‑quantum remainders ledgered per policy.
- **`strict` policy** raises `MoneyPrecisionError`; `warn/truncate` record and/or warn.

### 11.9 Worked examples (spec)

```goblin
price = $80
fee   = $5

price + 10%            /// $88.00    (10% of 1 → 0.10; $80 + $0.10? NO → applies to numbers. Use %s for self.)
                       /// CORRECT: with money, + 10% means + 0.10 units. Prefer explicit base forms below.

price + 10%s           /// $88.00    ($80 + 0.10*$80)
10% of price           /// $8.00
(10% of price) / 2     /// $4.00
price + (10% of fee)   /// $80 + $0.50 = $80.50

8 * 25%                /// 2
8 * 25%s               /// 16
8 + (25% of 50)        /// 20.5

rate        = pct(8.5)     /// 8.5%
discount    = pct(0.15)    /// 0.15% (i.e., 0.0015)
bigDiscount = pct(15)      /// 15%
```

**Note on money + bare %**: Because `%` defaults to "of 1," adding a bare percent to a money amount adds a scalar amount (e.g., `$80 + 10%` adds `$0.10`). For clarity, prefer `10%s` (self) or `10% of price` (explicit base) when operating on money.

### 11.10 Examples
```goblin
price = $100

discount = 15%                /// 0.15 (scalar form, percent of 1)
price - 15%s                  /// $85.00 (15% of price — percent of self)
15% of price                  /// $15.00 (explicit base)
with_tax($100, 8.5%)          /// $108.50 (tax helper)

/// Using constructor
rate = pct(8.5)               /// 8.5%  → 8.5 / 100 = 0.085
discount_rate = pct(15)       /// 15%   → 15 / 100 = 0.15
// if you wrote:
oops = pct(0.15)              /// 0.15% → 0.15 / 100 = 0.0015

/// Concrete check with pct(0.15):
price = $100
r = pct(0.15)                 /// 0.15% → 0.0015
r of price                    /// 0.0015 * $100 = $0.15
price - (r of price)          /// $99.85
```

### 11.11 Context-Bound Percent Literals (%s)

`%s` means "percent of self," where self is the left-hand operand in the containing expression.

**`%s` is illegal in any context where self is undefined** (e.g., direct assignment to a variable).

Attempting to assign `8.5%s` without a base results in a compile-time error:

```goblin
tax_rate = 8.5%s  // ERROR: %s has no base context
```

To store a reusable rate, either:

```goblin
tax_rate = 8.5% of price      // Bind to explicit base
```

or

```goblin
tax_rate = 8.5%               // Store as pure percent
total = price + (tax_rate of price)
```

This restriction enforces CIPO's guarantee that every percentage has a determinate base at creation time, preventing dangling references and context-dependent interpretation.

---

## 12. Helper Sugar (Functions + Brackets)

### 12.1 Call Styles
```goblin
/// Variadic
helper(a, b, c)

/// Array
helper([a, b, c])

/// Bracket sugar
helper[array_expr] ≡ helper(array_expr)
```

### 12.2 Bracket-Enabled Helpers (All Math Functions)
All built-in math functions support bracket sugar for array operations:
```goblin
add sum mult sub div min max avg root abs floor ceil round pow divmod
```
- `add` / `sum` — sum (returns element type; money stays money with perfect precision)
- `mult` — product (tracks remainders for money operations)
- `sub` — left fold subtraction
- `div` — left fold division (returns float unless exact)
- `min` / `max` — extrema
- `avg` — arithmetic mean (money if same-currency with truncation; else float)
- `root` — sequential roots (`root(27,3)` → 3)
- `abs` — scalar abs; for array form returns elementwise array
- `floor` / `ceil` / `round` — elementwise for arrays
- `pow` — sequential powers; `divmod` — sequential divmod

**Type rules:** mixed numeric → float; money arrays must share currency (else TypeError)

---

## 13. Regex & Text Utilities

### 13.1 Regex Functions (PCRE-style)
```goblin
re_test(s, pat, flags="") → bool
re_match(s, pat, flags="") → first match object or nil
re_findall(s, pat, flags="") → array of matches
re_replace(s, pat, repl, flags="") → string

/// Flags: i (case-insensitive), m (multi-line), s (dot-all), x (verbose)

/// String shorthands:
"abc123".matches(pat, flags="") → bool
"abc123".replace(pat, repl, flags="") → string
"abc123".findall(pat, flags="") → array
```

### 13.2 Text Helpers
```goblin
lines(s)     /// split by \n (keeps no terminators)
strip(s), lstrip(s), rstrip(s)
split(s, sep=nil, max=-1), join(arr, sep)
slug(s), title(s), lower(s), upper(s)
```

---

## 14. File I/O (Sandboxed)

### 14.1 Core Functions
```goblin
/// Text
read_text(path), write_text(path, s)

/// Binary
read_bytes(path), write_bytes(path, blob)

/// YAML
read_yaml(path), write_yaml(path, obj)

/// CSV
read_csv(path) → array of maps (values as strings)
write_csv(path, rows) /// rows = array of maps

/// JSON
read_json(path, opts={}) → value
write_json(path, value, opts={}) → nil

/// In-memory JSON (no filesystem)
json_stringify(value, opts={}) → string
json_parse(s, opts={}) → value

/// File System
exists(path), mkdirp(path), listdir(path), glob(pattern)
cwd(), chdir(path), join(a,b,...)

/// Utils
now(), uuid()
```
Paths relative to CWD unless absolute.

### 14.2 JSON

#### 14.2.1 Type Mapping
- JSON `null` → Goblin `nil`
- JSON `true/false` → Goblin `true/false`
- JSON number → Goblin `float` (no auto-cast to `int`/`money`)
- JSON string → Goblin `string` (no auto-cast)
- JSON array → Goblin `array`
- JSON object → Goblin `map` (string keys)

**No silent money parsing.** Strings like `"USD 1.50"` remain strings unless you opt in (see below).

**Money from JSON floats:** When reading JSON numbers into Goblin, money must be explicitly constructed via `money(float_value, CUR)` or by using `read_json(..., { money: ... })` options. External systems expect money as floats/decimals, so Goblin exports money at the precision they can handle and imports by explicit conversion.

When importing, Goblin promotes values to money only if the source schema or header explicitly marks them as monetary (e.g., "money", "price", "tax", "amount"), or the field type in a mapped class is declared as money. In all other cases, decimal numbers are imported as numeric types. All internal money arithmetic is done in integer sub-units according to currency precision, and exports follow the declared precision policy.

#### 14.2.2 Options (All Functions)
`opts` is a map. Unknown keys are ignored.

**Writing:**
- `indent: int` (default `2`) — pretty print spaces; `0` or `nil` for minified
- `sort_keys: bool` (default `false`) — stable key order for objects
- `money: "object" | "string" | "units"` (default `"object"`)
  - `"object"` (canonical): Money is written as:
    ```json
    { "_type": "money", "currency": "USD", "amount": "123.45" }
    ```
    `amount` is a **string in major units** with the active precision (no float drift).
  - `"string"`: `"USD 123.45"`
  - `"units"`:
    ```json
    { "_type": "money", "currency": "USD", "units": 12345, "precision": 2 }
    ```
    where `units` are integer **quanta** at `precision` (see §10.0).
- `blob: "off" | "base64" | "hex"` (default `"off"`)
  - `"off"`: blobs remain as-is (not serialized)
  - `"base64"`: encode blobs as base64 strings
  - `"hex"`: encode blobs as hex strings
- `datetime: "string" | "object"` (default `"string"`)
  - `"string"`: canonical ISO-8601 strings
  - `"object"`: structured form with `_type` field
- `enum: "name" | "value" | "object"` (default `"name"`)

**Reading:**
- `money: "off" | "object" | "string" | "auto"` (default `"off"`)
  - `"off"`: never decode money; you always get plain maps/strings.
  - `"object"`: only decode the **canonical object** form (above) to Goblin `money`.
  - `"string"`: decode strings strictly matching `"{CUR} {major}"` (e.g., `"USD 1.23"`) to `money`. Uses `money(major, CUR)` and follows precision policy (§10.0).
  - `"auto"`: try `"object"` then `"string"`; if both fail, leave as-is.
- `blob: "off" | "base64" | "hex" | "auto"` (default `"off"`)
  - `"off"`: never decode blobs; you always get plain strings.
  - `"base64"`: decode base64 strings to `blob`
  - `"hex"`: decode hex strings to `blob`
  - `"auto"`: try base64 then hex; if both fail, leave as string
- `datetime: "off" | "string" | "object" | "auto"` (default `"off"`)
- `enum: "off" | "name" | "value" | "object" | "auto"` (default `"off"`)
- `enum_schema: map<string,string>` — key → EnumName, only used with `enum:"value"`
- `strict_numbers: bool` (default `false`): when `true`, error on non-finite numbers (NaN/Infinity) if present.

**Glam and Serialization:** Glam may register their own JSON/YAML serialization handlers for custom types they define. If no handler is registered, the glam's types inherit the core JSON serialization rules. Glam-specific serialization follows the same safety policies (deterministic build restrictions, permission checks) as all other glam operations.

#### 14.2.3 Errors
- Malformed JSON → `ValueError`
- `money:"object"` with missing/invalid `currency`/`amount|units` → `ValueError`
- `blob:"base64"` with invalid base64 → `ValueError`
- `blob:"hex"` with invalid hex → `ValueError`
- When decoding to `money`, **precision policy applies**:
  - `strict` → `MoneyPrecisionError` if sub-precision appears
  - `warn`/`truncate` → canonicalize + ledger remainder (and warn in `warn`)

#### 14.2.4 Examples

**Write canonical money objects:**
```goblin
set @policy site_default
cart = { total: $123.45, items: 3 }
write_json("dist/cart.json", cart, { money: "object", indent: 2 })
```

**Read with safe defaults (no money decoding):**
```goblin
raw = read_json("dist/cart.json")  /// money stays as maps with _type
```

**Opt-in decode to money:**
```goblin
cart = read_json("dist/cart.json", { money: "object" })
say cart.total  /// USD 123.45
```

**String mode round-trip:**
```goblin
write_json("price.json", $19.99, { money: "string" })
p = read_json("price.json", { money: "string" })  /// -> USD 19.99
```

**In-memory stringify/parse:**
```goblin
s = json_stringify({ x: 1, m: $0.05 }, { money: "units" })
obj = json_parse(s, { money: "units" })
```

**Blob handling:**
```goblin
data = blob.from_hex("deadbeef")
write_json("data.json", { payload: data }, { blob: "base64" })
back = read_json("data.json", { blob: "base64" })
say back.payload.to_hex()  /// "deadbeef"
```

---

## 15. Blob Type & Binary Data

### 15.1 Purpose
The `blob` type represents immutable binary data with deterministic operations. Unlike strings, blobs handle raw bytes without encoding assumptions.

### 15.2 Construction & Literals
```goblin
/// Construction
b = blob()                    /// empty blob
b = blob([65, 66, 67])        /// from byte array

/// From encodings
b = blob.from_hex("deadbeef") 
b = blob.from_base64("SGVsbG8=")

/// From file
data = read_bytes("image.png")
```

### 15.3 Core Operations
```goblin
/// Length
len(b)           /// byte count
b.len()          /// method form

/// Slicing
b.slice(start, end)     /// returns new blob
b[start:end]            /// slice notation

/// Concatenation  
b.concat(other)         /// returns new blob
b1 | b2                 /// operator form

/// Comparison
b1 == b2                /// byte-wise equality
b1 != b2
```

### 15.4 Encoding/Decoding
```goblin
/// To strings
b.to_hex()              /// "deadbeef"
b.to_base64()           /// "3q2+7w=="

/// From strings  
blob.from_hex("deadbeef")
blob.from_base64("SGVsbG8=")

/// String conversion (UTF-8)
b.to_string()           /// decode as UTF-8 (may error)
blob.from_string("text") /// encode as UTF-8
```

### 15.5 Type Rules
- **Immutable**: All operations return new blobs
- **Binary equality**: Compares raw bytes, not encoded representations
- **No coercion**: Binary I/O must not silently coerce to/from string
- **Deterministic**: All operations must pass golden-vector tests

### 15.6 File I/O Integration
```goblin
/// Binary file operations
data = read_bytes("file.bin")        /// returns blob
write_bytes("output.bin", data)      /// accepts blob

/// Error cases
write_text("file.txt", blob_data)    /// TypeError
write_bytes("file.bin", "string")    /// TypeError
```

### 15.7 JSON Interop
Blobs require explicit encoding for JSON serialization:

```goblin
/// Manual encoding
data = blob.from_hex("deadbeef")
json_str = json_stringify({ 
    hex: data.to_hex(),
    b64: data.to_base64()
})

/// Via options (§14.2.2)
write_json("data.json", { payload: data }, { blob: "base64" })
back = read_json("data.json", { blob: "base64" })
```

### 15.8 Examples
```goblin
/// Create blob from hex
signature = blob.from_hex("deadbeef")
say len(signature)                   /// 4

/// Concatenate blobs
header = blob.from_hex("89504e47")
chunk = blob.from_hex("0d0a1a0a")
png_start = header.concat(chunk)
say png_start.to_hex()              /// "89504e470d0a1a0a"

/// Slice operations
data = blob.from_base64("SGVsbG8gV29ybGQ=")
first_5 = data.slice(0, 5)
say first_5.to_string()             /// "Hello"

/// File round-trip
write_bytes("data.bin", signature)
loaded = read_bytes("data.bin")
say loaded == signature             /// true
```

### 15.9 Errors
- **ValueError**: Invalid hex/base64 encoding
- **TypeError**: Type mismatches in operations
- **IndexError**: Slice bounds out of range
- **UnicodeError**: Invalid UTF-8 in `to_string()`

---

## 16. Cryptographic Functions

### 16.1 Purpose
Goblin provides cryptographic hashing and HMAC functions with deterministic, auditable implementations. All algorithms use well-tested implementations and support both streaming and direct interfaces.

### 16.2 Hash Functions
```goblin
/// Direct hashing
hash(data: string|blob, algorithm="sha256") -> string

/// Examples
digest = hash("hello world")                    /// SHA-256 by default
sha1_hash = hash("data", "sha1")                /// with warning
md5_hash = hash(blob_data, "md5")               /// with warning
```

**Supported algorithms:**
- **sha256** (default) — SHA-256, recommended for most uses
- **sha512** — SHA-512, for high-security applications  
- **sha1** — SHA-1, deprecated (emits warning)
- **md5** — MD5, deprecated (emits warning)

### 16.3 HMAC (Keyed Hashing)
```goblin
/// HMAC with key
hmac(data: string|blob, key: string|blob, algorithm="sha256") -> string

/// Examples
signature = hmac("message", "secret_key")
auth_tag = hmac(file_data, key_blob, "sha512")
```

### 16.4 Streaming Interface
For large data or incremental hashing:

```goblin
/// Start streaming hash
h = hash.start("sha256")
h.update("chunk1")
h.update(blob_chunk)
h.update("final_chunk")
digest = h.finish()         /// returns string

/// Streaming HMAC
mac = hmac.start("sha256", key)
mac.update(data_chunk1)
mac.update(data_chunk2) 
tag = mac.finish()
```

### 16.5 Type Handling
- **Input**: Accepts both `string` and `blob` data
- **Keys**: HMAC keys can be strings or blobs
- **Output**: Always returns hex-encoded string (lowercase)
- **Encoding**: Strings are encoded as UTF-8 bytes

### 16.6 Determinism & Testing
All cryptographic functions are deterministic and must pass golden-vector tests:

```goblin
/// Known test vectors
assert hash("abc") == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
assert hmac("data", "key") == "5031fe3d989c6d1537a013fa6e739da23463fdaec3b70137d828e36ace221bd0"
```

### 16.7 Security Considerations
- **Deprecated algorithms**: SHA-1 and MD5 emit warnings about known vulnerabilities
- **Key management**: HMAC keys should be properly generated and stored
- **Timing attacks**: All implementations use constant-time comparisons where applicable
- **Randomness**: Use secure random sources for key generation (not covered in core)

### 16.8 Examples
```goblin
/// Document integrity checking
content = read_bytes("document.pdf")
checksum = hash(content, "sha256")
say "SHA-256: {checksum}"

/// API request signing
payload = json_stringify({ user: "alice", action: "transfer" })
secret = "api_secret_key"
signature = hmac(payload, secret)
headers = { "X-Signature": signature }

/// Incremental hashing of large file
h = hash.start("sha256")
file_stream = open("large_file.bin")
while chunk = file_stream.read(8192)
    h.update(chunk)
end
final_hash = h.finish()
say "File hash: {final_hash}"

/// Password verification (simplified)
stored_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8"
user_input = "password"
if hash(user_input) == stored_hash
    say "Login successful"
else
    say "Invalid password"
end
```

### 16.9 Performance Notes
- **Streaming preferred**: Use streaming interface for data > 1MB
- **Algorithm choice**: SHA-256 provides good security/performance balance
- **Blob efficiency**: Operations on blobs avoid UTF-8 encoding overhead

### 16.10 Errors
- **ValueError**: Unknown algorithm specified
- **TypeError**: Invalid data type for hashing
- **HashError**: Streaming hash used after `finish()` called

---

## 17. Date & Time (Core)

### 17.1 Types
```goblin
date — calendar day (no time, no zone)
time — clock time (no date, no zone)
datetime — date + time + zone (zone required; defaults to active default)
duration — elapsed time in seconds (no zone)
```
No implicit coercion between these types.

### 17.2 Defaults & Time Zone
Datetime behavior (trusted time, policy, default tz) follows the active policy (§27).

```goblin
/// Example policy setting
@policy = "site_default"
    datetime: { tz: "America/Denver", prefer_trusted: false, policy: "warn" }
```

Accepts IANA names ("America/Denver"), "UTC", or fixed offsets ("+00:00", "-07:00").

Affects constructors, parse helpers without explicit `tz:`, and `today()`/`now()` when using local clock.

### 17.3 Trusted Time (Server → Cache → Local)
Goblin can source time from a trusted server, then fall back to a signed monotonic cache, then to the local clock.

#### 17.3.1 Config
```goblin
@policy = "strict_time"
    datetime: { 
        source: "https://time.goblin-lang.org/api/now",
        policy: "strict",
        prefer_trusted: true,
        ttl: "60s",
        cache_ttl: "24h",
        skew_tolerance: "5s",
        cache_path: "dist/time.cache",
        cache_signing_key: "my-secret-123"
    }
```

`cache_signing_key` is used for HMAC verification of the trusted‑time cache; if nil, cache is unsigned (less secure in hostile environments).

**Policies:**
- `strict`: If neither server nor valid cache are available → `TimeSourceError`.
- `warn`: Fall back to cache or local and emit `TimeSourceWarning`.
- `allow`: Fall back silently.

#### 17.3.2 API
```goblin
trusted_now()        → datetime        /// server > cache > local per policy
trusted_today()      → date
last_trusted_sync()  → datetime | nil  /// last server-verified instant
time_status()        → { source: "server"|"cache"|"local",
                         verified: bool, age_s: int,
                         offset_s: float, drift_s: float }
ensure_time_verified(reason="")        /// raise/warn per policy if not verified
clear_time_cache()   → nil
```

**Special Output:**
- When `ensure_time_verified()` succeeds: *"Time goblins confirm: timestamp verified! ⏰"*
- When `ensure_time_verified()` fails: *"Time goblins are suspicious of this timestamp"*

**Behavior (summary):**
- Server-first (HTTPS Date header or JSON epoch). On success, cache:
  `server_epoch_utc`, `local_monotonic_at_sync`, `local_wall_at_sync`, `tz_at_sync`, `sig`.
- Cache fallback reconstructs current epoch via monotonic delta. If signature bad, cache too old (> `cache_ttl`), or drift exceeds `skew_tolerance`, react per policy.
- Local last‑resort uses wall clock (unverified).
- Every attempt appends JSONL to `dist/time.log`.
- `now()`/`today()` use the local clock unless `prefer_trusted:true`. Use `trusted_*` where verification matters (e.g., money).

### 17.4 Construction & Parsing
```goblin
d  = date("2025-08-12")                           /// YYYY-MM-DD
t  = time("14:30:05")                              /// HH:MM[:SS[.SSS]]
dt = datetime("2025-08-12 14:30", tz: "UTC")       /// localizes to tz
dt2 = datetime("2025-08-12T14:30:00-06:00")        /// ISO-8601 w/ offset
```

**Strict parse helpers (format-guided):**
```goblin
parse_date(s, fmt="YYYY-MM-DD")
parse_time(s, fmt="HH:mm[:ss[.SSS]]")
parse_datetime(s, fmt="YYYY-MM-DD HH:mm[:ss[.SSS]]", tz=nil)
```
Format tokens: `YYYY MM DD HH mm ss SSS ZZ` (e.g., `"YYYY-MM-DD'T'HH:mm:SSS ZZ"`).

If string has Z/offset, it overrides `tz:` and defaults.

Malformed input → `ValueError`.

### 17.5 Duration Literals
```goblin
1s  90s  5m  1h  2d  3w  6mo  1y
```
Units: `s`, `m`, `h`, `d`, `w`, `mo`(=30d), `y`(=365d).

Combine via `+` (e.g., `1h + 30m`). Negative allowed.

`duration(n_seconds)` also available.

### 17.6 Now/Today & Zone Helpers
```goblin
now()          → datetime   /// local or trusted per prefer_trusted
today()        → date
utcnow()       → datetime   /// Always uses trusted chain when configured; else local clock
local_tz()     → string
to_tz(dt, "UTC")            /// convert datetime's zone (wall time adjusts)
```

### 17.7 Formatting & Accessors
```goblin
/// Stringification
.iso()                    /// ISO-8601 canonical
.format("YYYY-MM-DD")     /// token format
str(x)                    /// uses .iso()

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
Tokens: `YYYY`, `MM`, `DD`, `HH`, `mm`, `ss`, `SSS`, `ZZ`(+/-HH:MM), `ZZZ`(UTC/offset name).

### 17.8 Arithmetic & Comparisons
```goblin
/// Add/sub durations
datetime + duration  → datetime
datetime - duration  → datetime
date     + duration  → date      /// duration must be whole days
date     - duration  → date
time     + duration  → time      /// must remain within 00:00..24:00 (else ValueError)

/// Differences
datetime - datetime  → duration  /// exact seconds
date     - date      → duration  /// whole-day seconds

/// Comparisons (like-types only)
< <= > >= == !=  on date/date, time/time, datetime/datetime
```
Cross-type comparison → `TypeError`.

### 17.9 Truncation / Rounding
```goblin
floor_dt(dt, unit)   /// "year","month","week","day","hour","minute","second"
ceil_dt(dt, unit)
```
Weeks floor to Monday 00:00 in the instance's tz.

### 17.10 Calendar-Safe Adders
Month/Year math respects month length & leap years (clamps when needed).

```goblin
add_days(d|dt, n)       → same type
add_months(d|dt, n)     → same type    /// e.g., Jan 31 + 1mo → Feb 28/29
add_years(d|dt, n)      → same type    /// Feb 29 → Feb 28 on non-leap
```

### 17.11 Ranges
```goblin
for d  in date("2025-08-01")..date("2025-08-05")      /// step=1d by default
for ts in dt_start...dt_end step 30m                   /// exclusive end, custom step
```
Date range default step: `1d`. Datetime range default step: `1h`.

`step` must be a duration.

### 17.12 JSON / YAML / CSV Interop
Default surface is string (no auto-decode) per §14 principles.

**Canonical strings:**
- `date` → `"YYYY-MM-DD"`
- `time` → `"HH:MM:SS[.SSS]"`
- `datetime` → `"2025-08-12T14:30:00-06:00"`
- `duration` → `"PT3600S"` (ISO‑8601)

**JSON options (write/read):**
```goblin
write_json(path, value, { datetime: "string" | "object" = "string" })
read_json(path,  { datetime: "off" | "string" | "object" | "auto" = "off" })
```

**"object" forms:**
```json
{"_type":"datetime","value":"2025-08-12T14:30:00-06:00"}
{"_type":"date","value":"2025-08-12"}
{"_type":"time","value":"14:30:00"}
{"_type":"duration","seconds":3600}
```

`"auto"`: try "object", then strict ISO string; else leave raw.

Bad values → `ValueError`.

(YAML/CSV mirror "string" mode; use parse helpers to opt in.)

### 17.13 Errors & Warnings
`ValueError` (malformed), `TypeError` (illegal cross-type op), `OverflowError` (out of range), `TimezoneError` (unknown tz), `TimeSourceError`, `TimeSourceWarning`.

### 17.14 Examples
```goblin
set @policy site_default
start = datetime("2025-08-12 09:00")          /// MDT
say start.iso()                                /// 2025-08-12T09:00:00-06:00

/// Verified timestamp for a receipt
ensure_time_verified("checkout capture")
stamp = trusted_now()
say stamp.format("YYYY-MM-DD HH:mm ZZ")

/// Blockchain-ready verification
ensure_time_verified("blockchain-tx")         /// Verifies cache with HMAC
transaction_stamp = trusted_now()

/// Date iteration
for d in date("2025-08-01")..date("2025-08-03")
    say d                                     /// 01, 02, 03

/// Floor/Ceil
ts = datetime("2025-08-12 14:37:55")
say floor_dt(ts, "hour")                      /// 2025-08-12T14:00:00-06:00

/// Calendar adders
bill = date("2025-01-31")
say add_months(bill, 1)                       /// 2025-02-28

/// Differences
lap = datetime("2025-08-12 10:00") - datetime("2025-08-12 09:42")
say lap                                       /// PT1080S (18m)
```

**Server Endpoint (reference):**
The default source expects either:
- HTTPS Date header (RFC 7231)
- or JSON: `{"epoch": 1723473000.0}` (UTC seconds, float).

Clients must tolerate minor network/processing latency; monotonic adjustment is applied at cache write.

---

## 18. Shopify Profile (Glam: "Game Goblin")

### 18.1 Repo Layout
```
goblin.config.yaml
policies.gbln
types-registry.yaml
collections-map.yaml
registry/
  titles.yaml
  skus.jsonl
  collisions.log
titles/
  board_game/…yaml
  tarot_deck/…yaml
  book/…yaml
dist/
  <slug>/
    products.csv
    append.csv
    report.txt
    warnings.log
    remainders.log
```

### 18.2 CLI (Glam)
```
glam init                          # Initialize project
glam spawn tarot_deck "Mystic"     # Generate template  
glam build mystic.yaml --chain tarot_deck,shopify,etsy,ebay  # Chain exports
glam build --deterministic         # Enforce deterministic build with locked glam
glam lint                          # Check for reserved word conflicts and other issues
glam list                          # Show available glam
glam install community_glam        # Install from repository

# Goblin easter eggs
goblin hoard status                 # = remainders status  
goblin --about-goblins              # Fun goblin lore
goblin --version                    # Shows ASCII art + version name

# Codes
glam code set "Deviant Moon Borderless Tarot" DMBT
glam codes list
glam codes grep DMBT

# Remainders & Warnings
goblin remainders status            # show ledger and last N entries
goblin remainders clear             # clear in-memory ledger (keeps logs)
goblin remainders rotate            # rotate remainders.log
goblin warnings clear|rotate

glam gmark rebalance  # Compact ords by current sort; reassign 1..N (atomic)
```

### 18.3 Configuration Files
- **collections-map.yaml**: Authoritative map from Goblin category to Shopify collections (first = primary)
- **types-registry.yaml**: Per type: required fields, allowed categories, defaults (options, split strategy, SEO templates)
- **goblin.config.yaml**: SKU policy, paths, currency settings

### 18.4 SKU Generation
```yaml
sku_policy:
  default_pattern: "{CODE}-{YEAR?}-{LIST}-{PARTCODE}-{ATTRCODE?}"
  tarot_deck_pattern: "{CODE}-{LIST}-{SUITCODE}-{CARDNUM:02}"
  require_year_for_types: ["rpg_book", "vg_guide", "vintage"]
  autoinc: false  # set true to allow A/B suffixing on collisions
```

### 18.5 CSV Import/Export (Shopify)
- `--initial`: full rows + qty; Draft by default unless `meta.publish=true`
- `--append`: minimal rows; only new variants; no qty changes
- CSV exports numeric Variant Price in store currency with perfect precision
- If item currency ≠ store currency: warn and log to report.txt

### 18.6 Glam Settlement Options

```goblin
shopify::configure(
  settlement: "line_item" | "adjust_price" | "none" = "none",
  settlement_sku: "ROUND-ADJ",
  settlement_title: "Rounding Adjustment"
)
```

- **none**: export 2-dp values; call `drip_remainders(label: order_id)` (audit-only).
- **line_item**: `drip_remainders(commit:true, label: order_id)`; add single ± line item of the emitted amount.
- **adjust_price**: `drip_remainders(commit:true, label: order_id)`; distribute via `divide_evenly` across lines; recompute tax.

### 18.7 Logs (Standardized)

- **dist/warnings.log** — JSON Lines; precision warnings, with op, currency, remainder, location.
- **dist/remainders.log** — JSON Lines; drip operations (log-only or commit), before/after ledger, thresholds, emitted.

---

## 19. Examples

### Concatenation & Casting
```goblin
"Deviant" | "Moon" → DeviantMoon
"Deviant" || "Moon" → Deviant Moon
str(5) || str(10) → 5 10
```

### Division & Divmod Examples
```goblin
/// Numbers
10.75 / 3 → 3.5833333333
10.75 // 3 → 3
10.75 >> 3 → (3, 1.75)
-7 / 3 → -2.3333333333
-7 // 3 → -3         /// floor semantics
-7 >> 3 → (-3, 2)    /// because -7 = (-3)*3 + 2

/// Money
set @policy site_default
price = $10.75
q = price // 3       /// USD 3.00 (quotient only)
q, r = price >> 3    /// (USD 3.00, USD 1.75)
price / 3            /// MoneyDivisionError

/// Quick sanity check
a = 10.75
say a / 3            /// 3.5833333333
say a // 3           /// 3
say a >> 3           /// (3, 1.75)
```

### Postfix Math
```goblin
9**         → 81
16//        → 4
16// ** 2   → 16
```

### Explain-Power
```goblin
say 5 ** 3  → 125
say 5 ^^ 3  → 125 (5 × 5 × 5)
```

### Money Examples with Perfect Conservation
```goblin
set @policy site_default

a = 1.50      /// USD 1.50
b = $2.25     /// USD 2.25  
c = €3.00     /// EUR 3.00

say a + b     /// USD 3.75
a + c         /// CurrencyError

/// High-precision handling
precise = money(4.567, USD)  /// $4.56 + remainder(0.007)
price = $4.50
price++
price = price + .05  /// USD 5.55

/// Perfect conservation in operations
total = $100.00
shares = divide_evenly(total, 3)  /// [$33.34, $33.33, $33.33]
say add[shares]  /// $100.00 exactly

/// Money allocation
weights = [1, 2, 7]  /// 10%, 20%, 70%
allocations = allocate_money($1000.00, weights)
say allocations      /// [$100.00, $200.00, $700.00]
say sum(allocations) == $1000.00  /// true
```

### Precision Policy Examples
```goblin
set @policy strict_money

price = 19.99                        /// ok
price = money(19.991, USD)          /// MoneyPrecisionError
tax_amount = tax($405.95, 8.25%)    /// error (would be 33.990375)

/// Fix by even-splitting a rounded rate or adjusting:
tax_amount = divide_evenly($405.95 * 8.25%, 1)[0]

/// Or switch policy temporarily
set @policy site_default
tax_amount = tax($405.95, 8.25%)    /// ok, logs remainder

/// Bank-style precision:
set @policy high_precision   /// precision: 5, policy: truncate
fee = $0.00037                      /// ok, logs remainder if any operation shrinks below 5 dp
```

### Templates
```goblin
@cards =
    card: "{name}" :: price: .99 :: qty: 1
    "Ace of Cups"
    "Two of Cups" :: 1.25
    "Three of Cups" :: :: 2
    "Four of Cups" :: 1.25 :: 2
```

### Remainder Management
```goblin
/// Check current remainder ledger
say remainders_total()  /// {USD: $0.0347}

/// Audit potential drips (no commit)
potential = drip_remainders(threshold: $0.05, label: "order_123")
/// Logs to dist/remainders.log but doesn't modify ledger

/// Actually drip remainders
actual = drip_remainders(threshold: $0.05, commit: true, label: "order_123")
say actual  /// {USD: $0.03}
/// Ledger now has {USD: $0.0047}

/// Allocate dripped amounts
shares = divide_evenly(actual.USD, 3)  /// [$0.01, $0.01, $0.01]
```

### Date & Time Examples
```goblin
set @policy site_default

/// Basic construction
start_date = date("2025-08-12")
meeting_time = datetime("2025-08-12 14:30")
duration = 1h + 30m

/// Arithmetic
end_time = meeting_time + duration
days_until = date("2025-12-25") - today()

/// Ranges and iteration
for d in date("2025-08-01")..date("2025-08-07")
    say "Day: {d.format('YYYY-MM-DD')}"

/// Trusted time for financial records
ensure_time_verified("transaction_log")
transaction_stamp = trusted_now()
```

### Blob Examples
```goblin
/// Create and manipulate binary data
signature = blob.from_hex("deadbeef")
header = blob.from_hex("89504e47")
combined = signature.concat(header)

say combined.to_base64()    /// "3q2+74lQTkc="
say len(combined)           /// 8

/// File operations
image_data = read_bytes("logo.png")
thumbnail = image_data.slice(0, 1024)  /// first 1KB
write_bytes("thumb.png", thumbnail)

/// Hash verification
content = read_bytes("document.pdf")
expected = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
actual = hash(content, "sha256")
if actual == expected
    say "Document verified"
else
    say "Document corrupted!"
end
```

### Cryptographic Examples
```goblin
/// API request signing
payload = json_stringify({ user: "alice", amount: $100.00 })
secret_key = "your_api_secret"
signature = hmac(payload, secret_key, "sha256")

headers = {
    "Content-Type": "application/json",
    "X-Signature": signature
}

/// File integrity
data = read_bytes("important_file.dat")
checksum = hash(data, "sha512")
write_text("important_file.sha512", checksum)

/// Streaming hash for large files
h = hash.start("sha256")
for chunk in read_file_chunks("huge_file.bin", 8192)
    h.update(chunk)
end
final_hash = h.finish()
say "Hash: {final_hash}"
```

### Enum Examples
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

/// Int-backed with sequential auto-increment
enum Priority as int seq
    Low = 1
    Medium       /// 2
    High         /// 3
    Critical = 10
    Urgent       /// 11
end

assert Priority.Low < Priority.High
assert Priority.Critical.value() == 10

/// JSON roundtrip
write_json("order.json", order, { enum: "name" })
back = read_json("order.json", { enum: "name" })
say back.status                    /// Status.Pending
```

### Glam Examples
```goblin
use tarot_deck@1.2 as tarot
use shopify@^1.6 as shp

prefer product.export via shp

@cards = tarot::card_template(price: .99, qty: 1)
    "Ace of Cups" :: 3
    "Two of Cups"
    "Three of Cups" :: price: 1.25 :: 2

ensure_time_verified("shopify export")
file = product.export(@cards) via shp
say "Wrote {file}"
```

---

## 20. Errors & Warnings

```goblin
/// Throw errors
error "message"

/// Assertions
assert cond, "msg"

/// Try/catch/finally
try
    ...
catch e
    "Oops: {e.message}"
finally
    "done"

/// Warnings
warn "msg"
```

**Built-in error types:**
`NameError`, `TypeError`, `ValueError`, `IndexError`, `KeyError`, `ZeroDivisionError`, `SyntaxError`, `AssertionError`, `CurrencyError`, `MoneyDivisionError`, `MoneyPrecisionError`, `TimezoneError`, `TimeSourceError`, `OverflowError`, `EnumError`, `GlamError`, `ContractError`, `PermissionError`, `AmbiguityError`, `LockfileError`, `DeterminismError`, `GmarkConflictError`, `GmarkNotFoundError`, `GmarkInvalidError`, `GmarkPersistenceError`, `MorphTypeError`, `MorphFieldError`, `MorphCurrencyError`, `MorphActionError`, `ModuleNotFoundError`, `ModuleNameConflictError`, `ModuleCycleError`, `ModuleVisibilityError`, `PolicyNotFoundError`, `PolicyValueError`, `PolicyScopeError`, `PolicyVisibilityError`, `HashError`, `UnicodeError`

**Goblin Error Messages:**
Error messages may occasionally include goblin-themed variations for personality:

- `NameError`: 
  - *"The goblins can't find variable 'pricess' (did you mean 'prices'?)"*
  - *"Variable 'cofee' has vanished! (did you mean 'coffee'?)"*
  - *"The treasure map is missing 'totla' (did you mean 'total'?)"*
  - *"Goblins searched everywhere but 'nmae' doesn't exist (did you mean 'name'?)"*

- `MoneyPrecisionError`: 
  - *"The goblins refuse to lose precision! Use policy: truncate or fix your decimals"*
  - *"Treasure counting error: goblins demand exact amounts"*
  - *"The coin-counting goblins reject imprecise values"*
  - *"Money goblins detected sub-precision chaos - fix your math!"*

- `CurrencyError`: 
  - *"The goblins won't mix USD and EUR without explicit conversion!"*
  - *"Currency mixing detected - goblins demand clarity"*
  - *"The exchange goblins refuse to blend currencies"*
  - *"Different coins in the same pouch? The goblins disapprove!"*

- `TimeSourceError`: 
  - *"The time goblins can't verify this timestamp"*
  - *"Chronometer goblins report: time source unavailable"*
  - *"The temporal guardians have lost track of time"*
  - *"Time verification failed - the goblins are confused"*

- `TimeSourceWarning`: 
  - *"Time goblins warning: using unverified timestamp"*
  - *"The chronometer goblins are concerned about timing"*
  - *"Temporal warning: goblins suggest verification"*
  - *"Time precision questionable - goblins recommend caution"*

- `EnumError`: 
  - *"The classification goblins don't recognize 'Payed' (did you mean Status.Paid?)"*
  - *"Enum goblins report: 'BadReques' is not a valid Http value"*
  - *"The sorting goblins found an unknown variant"*
  - *"Category error: goblins can't find that enum value"*

- `GlamError`: 
  - *"The glam goblins encountered an unexpected error in '{glam}'"*
  - *"Mechanical failure: {glam} goblins report malfunction"*
  - *"The engineering goblins detected chaos in '{capability}'"*
  - *"Glam malfunction: goblins recommend checking '{glam}' configuration"*

- `ContractError`: 
  - *"The contract goblins found signature mismatch in '{capability}'"*
  - *"Legal eagles (goblin division) report contract violation"*
  - *"The agreement goblins are confused by this signature"*
  - *"Contract breach: goblins expected different parameters"*

- `SyntaxError`: 
  - *"The goblins are confused by line {line} - missing 'end'?"*
  - *"Syntax goblins stumbled at line {line}: unexpected '{token}'"*
  - *"The grammar goblins don't understand this structure"*
  - *"Code parsing failed - goblins need clearer instructions"*

- `TypeError`: 
  - *"The type goblins won't let you {operation} a {type1} and {type2}"*
  - *"Type mismatch: goblins refuse to mix incompatible types"*
  - *"The classification goblins detected type chaos"*
  - *"Type error: goblins demand compatible data types"*

- `ValueError`: 
  - *"The value goblins reject this input: {value}"*
  - *"Invalid data detected by the validation goblins"*
  - *"The quality control goblins found a bad value"*
  - *"Value error: goblins can't process this input"*

- `IndexError`: 
  - *"The index goblins can't find position {index} in this array"*
  - *"Array bounds error: goblins only found {length} items"*
  - *"The list goblins report: index {index} is out of range"*
  - *"Position error: goblins can't access item {index}"*

- `KeyError`: 
  - *"The key goblins can't find '{key}' in this map"*
  - *"Dictionary search failed: goblins don't see key '{key}'"*
  - *"The lookup goblins report: '{key}' doesn't exist"*
  - *"Map error: goblins found no entry for '{key}'"*

- `ZeroDivisionError`: 
  - *"The math goblins refuse to divide by zero!"*
  - *"Division error: goblins can't split by nothing"*
  - *"The calculation goblins detected impossible math"*
  - *"Zero division chaos - goblins demand valid denominators"*

- `AssertionError`: 
  - *"The verification goblins found a failed assertion: {message}"*
  - *"Assertion failed: goblins expected this to be true"*
  - *"The testing goblins report: assertion violated"*
  - *"Logic error: goblins can't verify this condition"*

- `MoneyDivisionError`: 
  - *"The treasury goblins forbid dividing money directly - use // or >> for division, or divide_evenly()"*
  - *"Money division blocked: goblins demand explicit remainder handling"*
  - *"The accounting goblins refuse money division without precision control"*
  - *"Currency math error: goblins require // or >> for money division"*

- `TimezoneError`: 
  - *"The geography goblins don't recognize timezone '{timezone}'"*
  - *"Time zone error: goblins are lost in '{timezone}'"*
  - *"The mapping goblins can't find that time zone"*
  - *"Timezone chaos: goblins need a valid zone identifier"*

- `OverflowError`: 
  - *"The capacity goblins report: number too large to handle"*
  - *"Overflow detected: goblins can't count that high"*
  - *"The size goblins found an impossibly large value"*
  - *"Number overflow: goblins reached their counting limit"*

- `PermissionError`: 
  - *"The security goblins deny access to '{resource}'"*
  - *"Permission denied: goblins guard this resource"*
  - *"The guardian goblins block unauthorized access"*
  - *"Access error: goblins require proper permissions"*

- `AmbiguityError`: 
  - *"The decision goblins found multiple options for '{capability}'"*
  - *"Ambiguity detected: goblins need clearer instructions"*
  - *"The choice goblins are confused by multiple providers"*
  - *"Resolution error: goblins can't pick between options"*

- `LockfileError`: 
  - *"The archive goblins found lockfile problems"*
  - *"Lockfile error: goblins can't verify glam versions"*
  - *"The versioning goblins report lockfile chaos"*
  - *"Lock verification failed: goblins demand consistent versions"*

- `DeterminismError`: 
  - *"The consistency goblins detected non-deterministic behavior"*
  - *"Determinism error: goblins demand predictable results"*
  - *"The reliability goblins found chaotic operations"*
  - *"Non-deterministic chaos: goblins require consistent execution"*
 
- `GmarkConflictError`:
  - *"The bookmark goblins found duplicate gmark '{name}' - they demand unique names!"*
  - *"Gmark collision detected: archive goblins refuse duplicate ord {ord}"*
  - *"The registry goblins report: '{name}' already exists with ord {ord}"*
  - *"Conflict chaos: goblins can't assign ord {ord} - already taken!"*

- `GmarkNotFoundError`:
  - *"The search goblins can't find gmark '{name}' in the registry"*
  - *"Missing bookmark: archive goblins don't see '{name}'"*
  - *"The catalog goblins report: gmark '{name}' has vanished"*
  - *"Registry error: goblins found no record of '{name}'"*

- `GmarkInvalidError`:
  - *"The naming goblins reject invalid gmark '{name}' - check the format!"*
  - *"Invalid bookmark: goblins don't allow '{name}' as a gmark"*
  - *"The format goblins found illegal characters in gmark '{name}'"*
  - *"Name validation failed: goblins demand proper gmark syntax"*

- `GmarkPersistenceError`:
  - *"The storage goblins can't save gmark registry to disk"*
  - *"Archive error: goblins failed to write .goblin/gmarks.lock"*
  - *"The file goblins report: gmark persistence chaos"*
  - *"Registry save failed: goblins can't update the bookmark database"*

- `MorphTypeError`:
  - *"The shape-shifting goblins can't morph '{obj}' using '{target}' - invalid types!"*
  - *"Morph error: transformation goblins don't recognize method '{method}'"*
  - *"The metamorphosis goblins found incompatible target type"*
  - *"Type transformation failed: goblins can't perform this morph"*

- `MorphFieldError`:
  - *"The field goblins found incompatible '{field}': expected {expected}, got {actual}"*
  - *"Morph field error: transformation goblins can't sync '{field}'"*
  - *"The accessor goblins report: field '{field}' types don't match"*
  - *"Field sync chaos: goblins refuse to mix incompatible field types"*

- `MorphCurrencyError`:
  - *"The currency goblins won't morph {from_cur} into {to_cur} for field '{field}'"*
  - *"Money morph error: exchange goblins demand matching currencies"*
  - *"The treasury goblins found currency mismatch in '{field}'"*
  - *"Currency transformation failed: goblins can't mix {from_cur} and {to_cur}"*

- `MorphActionError`:
  - *"The action goblins failed during morph: {cause}"*
  - *"Morph execution error: method goblins encountered chaos"*
  - *"The transformation goblins report: target method exploded"*
  - *"Action failure: goblins couldn't complete the morph operation"*

- `ModuleNotFoundError`:
  - *"The import goblins can't find module at '{path}'"*
  - *"Module search failed: goblins lost the trail to '{path}'"*
  - *"The file goblins report: no module at '{path}'"*
  - *"Import chaos: goblins can't locate the requested module"*

- `ModuleNameConflictError`:
  - *"The naming goblins found duplicate alias '{alias}' in imports"*
  - *"Import conflict: goblins refuse overlapping module names"*
  - *"The organization goblins detected alias collision for '{alias}'"*
  - *"Namespace chaos: goblins demand unique import aliases"*

- `ModuleCycleError`:
  - *"The dependency goblins found a circular import: {cycle}"*
  - *"Module cycle detected: goblins can't resolve circular dependencies"*
  - *"The logic goblins report: infinite import loop detected"*
  - *"Import chaos: goblins refuse circular module dependencies"*

- `ModuleVisibilityError`:
  - *"The access goblins deny import of vault symbol '{symbol}'"*
  - *"Module visibility error: goblins guard private symbols"*
  - *"The security goblins block access to hidden '{symbol}'"*
  - *"Import permission denied: goblins protect vault contents"*

- `PolicyNotFoundError`:
  - *"The policy goblins can't find configuration '{name}'"*
  - *"Unknown policy: goblins don't recognize '{name}'"*
  - *"The rulebook goblins report: policy '{name}' doesn't exist"*
  - *"Configuration error: goblins need a valid policy name"*

- `PolicyValueError`:
  - *"The validation goblins found invalid policy value at '{path}': {reason}"*
  - *"Policy configuration error: goblins reject malformed settings"*
  - *"The rules goblins detected bad policy data"*
  - *"Configuration chaos: goblins demand valid policy values"*

- `PolicyScopeError`:
  - *"The scope goblins found policy declaration in wrong location"*
  - *"Policy placement error: goblins expect header policies first"*
  - *"The organization goblins demand proper policy positioning"*
  - *"Scope violation: goblins require policies at file start"*

- `PolicyVisibilityError`:
  - *"The policy goblins block this import due to active restrictions"*
  - *"Import denied: policy goblins enforce visibility rules"*
  - *"The access goblins report: policy prevents this operation"*
  - *"Policy violation: goblins restrict this action"*

- `HashError`:
  - *"The crypto goblins found an invalid hash operation"*
  - *"Hashing error: goblins can't process this request"*
  - *"The encryption goblins detected algorithm chaos"*
  - *"Hash failure: goblins need proper data or algorithm"*

- `UnicodeError`:
  - *"The text goblins found invalid UTF-8 encoding"*
  - *"Unicode error: goblins can't decode this text"*
  - *"The character goblins detected encoding chaos"*
  - *"Text encoding failure: goblins need valid UTF-8"*

**Built-in warning types:**
`MoneyPrecisionWarning`, `TimeSourceWarning`, `UnusedJudgeValueWarning`

- `JudgeError`:
  - *"The judgment goblins found malformed judge expression"*
  - *"Decision error: goblins need proper condition: value syntax"*
  - *"The choice goblins are confused by this judge structure"*
  - *"Judge syntax chaos: goblins demand clear conditions"*

---

## 21. Reserved Words

```
if, elif, else, unless, for, in, while, class, fn, return, skip, stop, try, catch, finally, 
assert, error, warn, say, true, false, nil, int, float, money, bool, pct, blob,
read_text, write_text, read_bytes, write_bytes, read_yaml, write_yaml, read_csv, write_csv, read_json, write_json, 
json_stringify, json_parse, exists, mkdirp, listdir, glob, cwd, chdir, join, now, uuid, 
add, sum, mult, sub, div, mod, divmod, pow, root, floor, ceil, round, abs, min, max, 
rand, randint, tax, with_tax, bit, glam, use, export, via, validate, 
remainders_total, remainders_report, clear_remainders, divide_evenly, divide_evenly_escrow, 
drip_remainders, allocate_money, goblin_divvy, date, time, datetime, duration, parse_date, parse_time, parse_datetime,
today, utcnow, local_tz, to_tz, floor_dt, ceil_dt, add_days, add_months, add_years,
trusted_now, trusted_today, last_trusted_sync, time_status, clear_time_cache, ensure_time_verified,
prefer, contract, emit, emit_async, on, test, enum, seq, goblin_hoard, goblin_treasure, 
goblin_empty_pockets, goblin_stash, goblin_payout, gmark, gmarks, gmark_info, gmark_set_ord,
next_ord, ord, morph, judge, import, expose, vault, set, settle, excess, with_money_policy,
compat, mode, track_theft, shame_level, hash, hmac
```

**Version Codenames:**
- v1.0: "Evil Redcap" *(MVP)*
- v1.5: "Treasure Hoarder" *(current)*
- v1.6: "Gold Sniffer"
- v1.7: "Wealth Watcher" 
- v1.8: "Loot Guardian"
- v2.0: "Hoard Lord"
- v2.5: "Sneaky Pilferer"
- v3.0: "Trickster King"
- v3.5: "Mischief Maker"

## 22. Core vs. Glam Architecture

### 22.1 Design Philosophy
Goblin maintains a **lean core** with **extensible glam** to balance safety with flexibility. Core provides guarantees that glam cannot compromise; glam handle domain-specific functionality.

### 22.2 Must Be Core
These components **must** remain in core to ensure safety, determinism, and language coherence:

**Syntax & Parser:**
- Keywords (`use`, `via`, `prefer`, `contract`, `emit`, `on`, `test`, `import`, `set`)
- Operators (percent literals, postfix `++`/`--`, infix `//`, `>>`)
- Precedence rules and "no-space vs space" `%` behavior
- Template syntax (`::`, `@name`)

**Type System Primitives:**
- Core types (`int`, `float`, `bool`, `string`, `money`, `percent`, `date`, `time`, `datetime`, `duration`, `enum`, `blob`)
- Money conservation/precision rules and remainder tracking
- Trusted time semantics and verification chains
- Enum identity and singleton guarantees
- Blob immutability and binary safety

**Security & Cryptography:**
- Hash functions (`hash`, `hmac`) with deterministic implementations
- Cryptographic primitives with golden-vector testing
- Binary data handling without encoding assumptions

**Contract System:**
- Global contract registry (`contract name -> signature/errors`)
- Implementation compliance checking
- Call-site resolution (`via` > `prefer` > defaults)
- Error handling (`AmbiguityError`, `ContractError`)

**Sandbox & Determinism:**
- Permission enforcement (FS/NET allowlists, env gating)
- `--deterministic` behavior (wall-clock blocking, network restrictions, RNG seeding)
- Dry-run infrastructure

**Security Infrastructure:**
- Trusted time chain and HMAC verification
- Lockfile management and version resolution
- Build reproducibility guarantees

**Runtime Safety:**
- Standard error classes and glam error wrapping
- JSONL logging around capability calls
- Reserved word collision prevention

**Core I/O & Serialization:**
- JSON/YAML/CSV base functionality
- Serialization hooks for glam types
- Type-safe deserialization policies

### 22.3 Should Be Glam
These components are **appropriately** handled by glam:

- Domain capabilities (Shopify, blockchain, invoices)
- Domain types (`Product`, `Order`, custom records)
- Platform integrations and APIs
- Specialized exporters/importers
- Domain-specific validation rules
- External telemetry (beyond core logging)
- Workflow CLIs and tooling

### 22.4 Gray Areas
These may evolve between core and glam:

- **Event bus workers**: Core provides basic scheduling; advanced glam may enhance
- **RNG**: Core provides seeded `rand`; crypto glam may add secure variants (with permission checks)
- **Formatting**: Goblin personality messages stay in core but may be configurable

### 22.5 Architectural Benefits
This split ensures:
- **Core stays lean** while providing enterprise guarantees
- **Glam can innovate** without breaking safety/determinism
- **No vendor lock-in** - multiple glam implementations can compete
- **Auditability** - core behavior is predictable and verifiable

---

*[Sections 23-30 continue with the existing content from the original spec - Glam, Enums, Gmark, Morph, HTML Glam, Policies, and Modules sections remain unchanged]*

---

## 31. Release Notes

Goblin v1.5 "Treasure Hoarder" includes all planned core features for production readiness. The missing items from the original roadmap have been integrated:

### ✅ Completed in v1.5

**Blob primitives** (§15): Native `blob` type with encoding/decoding, file operations, and JSON interop.

**Hashing & HMAC** (§16): Cryptographic functions with streaming interface and deterministic testing.

**Money allocation helper** (§10.11): Perfect conservation allocation across weighted portions.

**Enhanced JSON/File I/O** (§14): Binary file operations and blob serialization options.

**Updated reserved words** (§21): Added `blob`, `hash`, `hmac`, `allocate_money`, `goblin_divvy`, `read_bytes`, `write_bytes`.

### Baseline Glam
- regex glam: regex::test, regex::findall, regex::replace (+ flags)
- fs.glob glam: fs::glob(pattern) -> array<string>, fs::walk(dir, pattern="**/*")  
- retry/backoff glam: retry::with_backoff(fn, attempts=3, base_ms=250, jitter=true)
- http glam polish: http::request(method, url, headers={}, body=""), deterministic-mode safeguards

Goblin v1.5 provides a complete, production-ready language with robust financial precision, cryptographic capabilities, binary data handling, and a comprehensive glam ecosystem.

---

*[End of Goblin Language Specification v1.5 "Treasure Hoarder"]*
