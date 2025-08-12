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
* / %
//  (infix divmod — quotient+remainder, spaces required)
+ -
| then ||                   [string joins only]
comparisons: == != < <= > >= === !== is is not   (chaining allowed)
logical: and / or          (aliases: ! for not, && for and)
inline conditional ? ?? :   (right-associative)
```

**Notes:**
- Postfix forms (`**`, `//`, `++`, `--`) bind tighter than binary math
- If an operator token is followed by an operand → binary; if it ends the expression (no RHS) → postfix
- `//=` is not supported (divmod returns a pair)

### 2.2 Arithmetic
```goblin
/// Division
/ → float division (10/2 → 5.0)
a // b → quotient + remainder pair
say 5 // 2 → 2 r 1
q, r = 5 // 2

/// Modulus
% → remainder (modulus)
```

### 2.3 Exponentiation
```goblin
** → normal exponent (5 ** 3 → 125)
^^ → explain-power
say 5 ^^ 3 → 125 (5 × 5 × 5)
```
- Same precedence/associativity/value as `**`
- `^` reserved for future XOR (not an operator in v1.4)

### 2.4 Postfix Math Shorthands
```goblin
n** → square (9** → 81)
n// → square root (9// → 3)
x++ → yields old x, then x = x + 1
x-- → yields old x, then x = x - 1
```

**Types:**
- `n**` and `n//`: int/float only (money not allowed; TypeError)
- `x++`/`x--`: int/float/money; money changes by 1 whole unit
- Lvalues only (e.g., `arr[i]++` ok). Not on literals/temporaries: `(x+1)++` → SyntaxError
- No chaining: `x++++` and `(x++)++` → SyntaxError
- No prefix forms (`++x`, `--x`) in v1.4

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
+= -= *= /= %= **=
```
Money ops must follow currency rules (same currency, see §10). For money, +=, -=, and *= follow the same promotion, precision, and ledger rules described in §10 (including precision:/policy:).

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

### 4.1 Block Form
```goblin
if cond
    ...
elif cond
    ...
el
    ...
```
No colons. Indentation defines blocks.

### 4.2 Inline Form
```goblin
/// One ? (if), any number of ?? (elif), one : (else)
say score >= 90 ? "A" ?? score >= 80 ? "B" ?? score >= 70 ? "C" : "F"
```
Right-associative; binds lower than comparisons/logical ops, higher than assignment.

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
init greet(name="Traveler")
    "Hello, {name}"   /// last expression is implicit return
end
```
- `return` allowed; else last expression returns
- Defaults supported; arity is strict (defaults satisfy arity)
- **Note:** `init` is also reserved for class constructors (see §9.3)

---

## 8. Key-Value "Pipes" (::) and Templates

### 8.1 Inline KV with ::
```goblin
/// Anywhere a short record is helpful
card: "Two of Cups" :: price: .99 :: qty: 4
```
- `::` separates fields
- LHS must be identifiers
- RHS any expression

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

@tarot = card: "{suit} – {card}" :: price: .99 :: qty: 1

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

### 9.1 Class Declaration, Constructor, Methods
```goblin
class Pet
    init(name, age=0)
        #name = name
        #age  = age
    end

    speak()
        "Hi, I'm {#name} and I'm {#age}"
    end

    birthday()
        #age++
        #age
    end
end

/// Create instance
p = Pet("Fido", 3)

/// Call methods
say p.speak()
```
- Instance variables: `#name`, `#age` (only valid inside class methods)
- No inline `#var =` outside init/methods; prefer setting in init
- `init` is the constructor name (locked in)
- `spawn` is reserved for Gears scaffolding (CLI), not for classes

### 9.2 Visibility
- Instance variables (`#x`) are private to the instance (accessible only in class body)
- Methods are public by default (future versions may add visibility keywords)

---

## 10. Money & Currency

### 10.0 Money Precision Policy

You can fix the allowed decimal places and choose how to handle sub-precision results.

```goblin
/// Syntax (anywhere before use)
default money USD precision: 2 policy: truncate
```

**Parameters:**
- **precision:** integer ≥ 0 (decimal places in major units)
- **policy:** `truncate` (default) | `warn` | `strict`
  - `truncate`: canonicalize via truncate-and-ledger (no rounding)
  - `warn`: same as truncate, also warn `MoneyPrecisionWarning(...)`
  - `strict`: throw `MoneyPrecisionError` if any op would produce sub-precision

**Goblin Aliases:** `policy: goblin` (= truncate), `policy: hoard` (= warn), `policy: greedy` (= strict)

**Scope:** from the directive forward; per currency. Unset currencies use global default (`precision: 2, policy: truncate`) unless overridden.

### 10.1 Type & Precision Handling
- Values are stored as integer quanta of size `10^(-precision)` in major units; any sub-quantum remainder is tracked per §10.7.
- `policy` applies at canonicalization sites: `money(v,C)`, promotion (`money` ± int/float), `*`, `tax`, `with_tax`, `convert`, and gear functions returning money.
- **No rounding** - excess precision becomes explicit remainder
- Perfect conservation: `input_value = money_part + remainder`

### 10.2 Construction & Literals
```goblin
money(1.50)        /// uses script/build default currency
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

If `policy: strict` and `v` has more than `precision` decimals, raise `MoneyPrecisionError` (no ledger update).

### 10.3 Number Typing & Defaults
```goblin
/// If no default yet:
123      → int
1.23     → float
$1.50    → money

/// Declaring a default mid-file affects subsequent untyped numerics:
default int / default float / default money [CUR]

/// With default money USD:
x = 5    → USD 5.00
y = 2.5  → USD 2.50

/// Explicit casts always win:
age = int(45), tax = float(.0725), price = money(.99, EUR)
```

### 10.4 Arithmetic & Comparisons
```goblin
/// Allowed (same currency): + - * // %
/// Not allowed: / on money

/// Attempting money / anything → MoneyDivisionError
/// Use // to capture remainder or divide_evenly(...)

/// Scalar multiply/divide by numbers:
money * int|float → money (same currency) + remainder tracking
money // int → pair (quotient: money, remainder: money)
money % int → remainder money (alias of second element of //)
```

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

### 10.6 Increments with Money
```goblin
money++ / money--  /// add/subtract exactly 1 whole unit
```
- Postfix `**` and `//` are not allowed on money (TypeError)
- To change at quantum level: do it explicitly (`price = price + .05`)

### 10.7 Even Splits & Remainder Ledger

#### Hard Rule on Division
Using `/` with money always errors: `MoneyDivisionError: Use // to capture remainder or divide_evenly(total, parts).`

#### Divmod for Money
```goblin
q, r = total // parts
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
q, _ = total // n

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
default money USD

/// Construction with remainder
precise_amount = money(100.567, USD)  /// $100.56 + remainder(0.007)

/// Division with remainder
q, r = $100.00 // 3   /// q = $33.33, r = $0.01
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
_, _ = $100.00 // 7    /// remainder logged automatically
say remainders_total()  /// => { USD: $0.02 }
clear_remainders()
```

---

## 11. Percentages & Tax

### 11.1 Percent Literal
```goblin
10%     → 0.10
8.25%   → 0.0825

/// % with spaces = modulo
10 % 3

/// Invalid: 10%off (insert separator)
10% * price  /// correct

/// Precedence: % binds tighter than arithmetic
say 100 + 20% * 2      /// 100 + (20% * 2) = 100 + (0.2 * 2) = 100.4
say 8 * 25%            /// 8 * 0.25 = 2.0
say 100 + 20% * 2 / 5  /// 100 + (0.2 * 2) / 5 = 100 + 0.4 / 5 = 100.08
```

### 11.2 Tax Helpers
```goblin
tax(subtotal, rate_or_rates, compound=false) → tax amount
with_tax(subtotal, rate_or_rates, compound=false) → subtotal + tax(...)

/// rate_or_rates: single rate (0.10 or 10%) or array ([8.25%, 1%])
/// compound=true applies sequentially; else additive
```
- **Precision handling:** Tax calculations use truncation, not rounding
- Any sub-quantum amounts from percentage calculations are tracked in remainder ledger
- `compound=false`: compute each component with truncation, track remainders separately
- `compound=true`: apply each rate sequentially, tracking cumulative remainders

Tax obeys the active precision policy. In `policy: strict`, if tax introduces sub-precision, raise `MoneyPrecisionError`. Under `warn`/`truncate`, record the sub-precision in the ledger and (for `warn`) emit a warning.

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

### 12.2 Bracket-Enabled Helpers (Whitelist)
```goblin
add sum mult sub div min max avg root abs
```
- `add` / `sum` — sum (returns element type; money stays money with perfect precision)
- `mult` — product (tracks remainders for money operations)
- `sub` — left fold subtraction
- `div` — left fold division (returns float unless exact)
- `min` / `max` — extrema
- `avg` — arithmetic mean (money if same-currency with truncation; else float)
- `root` — sequential roots (`root(27,3)` → 3)
- `abs` — scalar abs; for array form returns elementwise array

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
- `datetime: "off" | "string" | "object" | "auto"` (default `"off"`)
- `enum: "off" | "name" | "value" | "object" | "auto"` (default `"off"`)
- `enum_schema: map<string,string>` — key → EnumName, only used with `enum:"value"`
- `strict_numbers: bool` (default `false`): when `true`, error on non-finite numbers (NaN/Infinity) if present.

**Gears and Serialization:** Gears may register their own JSON/YAML serialization handlers for custom types they define. If no handler is registered, the gear's types inherit the core JSON serialization rules. Gear-specific serialization follows the same safety policies (deterministic build restrictions, permission checks) as all other gear operations.

#### 14.2.3 Errors
- Malformed JSON → `ValueError`
- `money:"object"` with missing/invalid `currency`/`amount|units` → `ValueError`
- When decoding to `money`, **precision policy applies**:
  - `strict` → `MoneyPrecisionError` if sub-precision appears
  - `warn`/`truncate` → canonicalize + ledger remainder (and warn in `warn`)

#### 14.2.4 Examples

**Write canonical money objects:**
```goblin
default money USD precision: 2
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

---

## 15. Date & Time (Core)

### 15.1 Types
```goblin
date — calendar day (no time, no zone)
time — clock time (no date, no zone)
datetime — date + time + zone (zone required; defaults to active default)
duration — elapsed time in seconds (no zone)
```
No implicit coercion between these types.

### 15.2 Defaults & Time Zone
```goblin
default datetime tz: "America/Denver"
```
Accepts IANA names ("America/Denver"), "UTC", or fixed offsets ("+00:00", "-07:00").

Affects constructors, parse helpers without explicit `tz:`, and `today()`/`now()` when using local clock.

### 15.3 Trusted Time (Server → Cache → Local)
Goblin can source time from a trusted server, then fall back to a signed monotonic cache, then to the local clock.

#### 15.3.1 Config
```goblin
default datetime source: "https://time.goblin-lang.org/api/now"
default datetime policy: strict | warn | allow = warn
default datetime prefer_trusted: false      /// if true: now()/today() use trusted chain
default datetime ttl: 60s                   /// refresh window while online
default datetime cache_ttl: 24h             /// max offline age for cache
default datetime skew_tolerance: 5s         /// wall-clock vs monotonic drift
default datetime cache_path: "dist/time.cache"
default datetime cache_signing_key: nil     /// optional HMAC key
```

`cache_signing_key` is used for HMAC verification of the trusted‑time cache; if nil, cache is unsigned (less secure in hostile environments).

**Policies:**
- `strict`: If neither server nor valid cache are available → `TimeSourceError`.
- `warn`: Fall back to cache or local and emit `TimeSourceWarning`.
- `allow`: Fall back silently.

#### 15.3.2 API
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

### 15.4 Construction & Parsing
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

### 15.5 Duration Literals
```goblin
1s  90s  5m  1h  2d  3w  6mo  1y
```
Units: `s`, `m`, `h`, `d`, `w`, `mo`(=30d), `y`(=365d).

Combine via `+` (e.g., `1h + 30m`). Negative allowed.

`duration(n_seconds)` also available.

### 15.6 Now/Today & Zone Helpers
```goblin
now()          → datetime   /// local or trusted per prefer_trusted
today()        → date
utcnow()       → datetime   /// UTC (ignores prefer_trusted, uses trusted chain if enabled)
local_tz()     → string
to_tz(dt, "UTC")            /// convert datetime's zone (wall time adjusts)
```

### 15.7 Formatting & Accessors
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

### 15.8 Arithmetic & Comparisons
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

### 15.9 Truncation / Rounding
```goblin
floor_dt(dt, unit)   /// "year","month","week","day","hour","minute","second"
ceil_dt(dt, unit)
```
Weeks floor to Monday 00:00 in the instance's tz.

### 15.10 Calendar-Safe Adders
Month/Year math respects month length & leap years (clamps when needed).

```goblin
add_days(d|dt, n)       → same type
add_months(d|dt, n)     → same type    /// e.g., Jan 31 + 1mo → Feb 28/29
add_years(d|dt, n)      → same type    /// Feb 29 → Feb 28 on non-leap
```

### 15.11 Ranges
```goblin
for d  in date("2025-08-01")..date("2025-08-05")      /// step=1d by default
for ts in dt_start...dt_end step 30m                   /// exclusive end, custom step
```
Date range default step: `1d`. Datetime range default step: `1h`.

`step` must be a duration.

### 15.12 JSON / YAML / CSV Interop
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

### 15.13 Errors & Warnings
`ValueError` (malformed), `TypeError` (illegal cross-type op), `OverflowError` (out of range), `TimezoneError` (unknown tz), `TimeSourceError`, `TimeSourceWarning`.

### 15.14 Examples
```goblin
default datetime tz: "America/Denver"
default datetime source: "https://time.goblin-lang.org/api/now"
default datetime policy: warn
default datetime prefer_trusted: true
default datetime cache_signing_key: "my-secret-123"

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

## 16. Shopify Profile (Gears: "Game Goblin")

### 16.1 Repo Layout
```
goblin.config.yaml
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

### 16.2 CLI (Gears)
```
gears init                          # Initialize project
gears spawn tarot_deck "Mystic"     # Generate template  
gears build mystic.yaml --chain tarot_deck,shopify,etsy,ebay  # Chain exports
gears build --deterministic         # Enforce deterministic build with locked gears
gears lint                          # Check for reserved word conflicts and other issues
gears list                          # Show available gears
gears install community_gear        # Install from repository

# Goblin easter eggs
goblin hoard status                 # = remainders status  
goblin --about-goblins              # Fun goblin lore
goblin --version                    # Shows ASCII art + version name

# Codes
gears code set "Deviant Moon Borderless Tarot" DMBT
gears codes list
gears codes grep DMBT

# Remainders & Warnings
goblin remainders status            # show ledger and last N entries
goblin remainders clear             # clear in-memory ledger (keeps logs)
goblin remainders rotate            # rotate remainders.log
goblin warnings clear|rotate
```

### 16.3 Configuration Files
- **collections-map.yaml**: Authoritative map from Goblin category to Shopify collections (first = primary)
- **types-registry.yaml**: Per type: required fields, allowed categories, defaults (options, split strategy, SEO templates)
- **goblin.config.yaml**: SKU policy, paths, currency settings

### 16.4 SKU Generation
```yaml
sku_policy:
  default_pattern: "{CODE}-{YEAR?}-{LIST}-{PARTCODE}-{ATTRCODE?}"
  tarot_deck_pattern: "{CODE}-{LIST}-{SUITCODE}-{CARDNUM:02}"
  require_year_for_types: ["rpg_book", "vg_guide", "vintage"]
  autoinc: false  # set true to allow A/B suffixing on collisions
```

### 16.5 CSV Import/Export (Shopify)
- `--initial`: full rows + qty; Draft by default unless `meta.publish=true`
- `--append`: minimal rows; only new variants; no qty changes
- CSV exports numeric Variant Price in store currency with perfect precision
- If item currency ≠ store currency: warn and log to report.txt

### 16.6 Gear Settlement Options

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

### 16.7 Logs (Standardized)

- **dist/warnings.log** — JSON Lines; precision warnings, with op, currency, remainder, location.
- **dist/remainders.log** — JSON Lines; drip operations (log-only or commit), before/after ledger, thresholds, emitted.

---

## 17. Examples

### Concatenation & Casting
```goblin
"Deviant" | "Moon" → DeviantMoon
"Deviant" || "Moon" → Deviant Moon
str(5) || str(10) → 5 10
```

### Infix Divmod & Destructure
```goblin
q, r = 17 // 5  /// q=3, r=2
say 5 // 2      /// 2 r 1
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
default money USD

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
```

### Precision Policy Examples
```goblin
default money USD precision: 2 policy: strict

price = 19.99                        /// ok
price = money(19.991, USD)          /// MoneyPrecisionError
tax_amount = tax($405.95, 8.25%)    /// error (would be 33.990375)

/// Fix by even-splitting a rounded rate or adjusting:
tax_amount = divide_evenly($405.95 * 8.25%, 1)[0]

/// Or switch policy temporarily
default money USD policy: truncate
tax_amount = tax($405.95, 8.25%)    /// ok, logs remainder

/// Bank-style precision:
default money USD precision: 5 policy: truncate
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
default datetime tz: "America/Denver"

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
el
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

### Gear Examples
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

## 18. Errors & Warnings

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
`NameError`, `TypeError`, `ValueError`, `IndexError`, `KeyError`, `ZeroDivisionError`, `SyntaxError`, `AssertionError`, `CurrencyError`, `MoneyDivisionError`, `MoneyPrecisionError`, `TimezoneError`, `TimeSourceError`, `OverflowError`, `EnumError`, `GearError`, `ContractError`, `PermissionError`, `AmbiguityError`, `LockfileError`, `DeterminismError`

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

- `GearError`: 
  - *"The gear goblins encountered an unexpected error in '{gear}'"*
  - *"Mechanical failure: {gear} goblins report malfunction"*
  - *"The engineering goblins detected chaos in '{capability}'"*
  - *"Gear malfunction: goblins recommend checking '{gear}' configuration"*

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
  - *"The treasury goblins forbid dividing money directly - use // or divide_evenly()"*
  - *"Money division blocked: goblins demand explicit remainder handling"*
  - *"The accounting goblins refuse money division without precision control"*
  - *"Currency math error: goblins require // for money division"*

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
  - *"Lockfile error: goblins can't verify gear versions"*
  - *"The versioning goblins report lockfile chaos"*
  - *"Lock verification failed: goblins demand consistent versions"*

- `DeterminismError`: 
  - *"The consistency goblins detected non-deterministic behavior"*
  - *"Determinism error: goblins demand predictable results"*
  - *"The reliability goblins found chaotic operations"*
  - *"Non-deterministic chaos: goblins require consistent execution"*
 
- `GmarkConflictError`:
- `GmarkNotFoundError`:
- `GmarkInvalidError`:
- `GmarkPeristenceError`:


**Built-in warning types:**
`MoneyPrecisionWarning`, `TimeSourceWarning`

---

## 19. Reserved Words

```
if, elif, el, for, in, while, class, init, return, skip, stop, try, catch, finally, 
assert, error, warn, say, true, false, nil, default, int, float, money, bool, 
read_text, write_text, read_yaml, write_yaml, read_csv, write_csv, read_json, write_json, 
json_stringify, json_parse, exists, mkdirp, listdir, glob, cwd, chdir, join, now, uuid, 
add, sum, mult, sub, div, mod, divmod, pow, root, floor, ceil, round, abs, min, max, 
rand, randint, tax, with_tax, bit, gear, use, export, import, via, validate, 
remainders_total, remainders_report, clear_remainders, divide_evenly, divide_evenly_escrow, 
drip_remainders, date, time, datetime, duration, parse_date, parse_time, parse_datetime,
today, utcnow, local_tz, to_tz, floor_dt, ceil_dt, add_days, add_months, add_years,
trusted_now, trusted_today, last_trusted_sync, time_status, clear_time_cache, ensure_time_verified,
prefer, contract, emit, emit_async, on, test, enum, seq, goblin_hoard, goblin_treasure, 
goblin_empty_pockets, goblin_stash, goblin_payout, gmark, ord
```

**Version Codenames:**
- v1.5: "Treasure Hoarder" *(current)*
- v1.6: "Gold Sniffer"
- v1.7: "Wealth Watcher" 
- v1.8: "Loot Guardian"
- v2.0: "Hoard Lord"
- v2.5: "Sneaky Pilferer"
- v3.0: "Trickster King"
- v3.5: "Mischief Maker"

## 20. Core vs. Gears Architecture

### 20.1 Design Philosophy
Goblin maintains a **lean core** with **extensible gears** to balance safety with flexibility. Core provides guarantees that gears cannot compromise; gears handle domain-specific functionality.

### 20.2 Must Be Core
These components **must** remain in core to ensure safety, determinism, and language coherence:

**Syntax & Parser:**
- Keywords (`use`, `via`, `prefer`, `contract`, `emit`, `on`, `test`)
- Operators (percent literals, postfix `++`/`--`, infix `//`)
- Precedence rules and "no-space vs space" `%` behavior
- Template syntax (`::`, `@name`)

**Type System Primitives:**
- Core types (`int`, `float`, `bool`, `string`, `money`, `percent`, `date`, `time`, `datetime`, `duration`, `enum`)
- Money conservation/precision rules and remainder tracking
- Trusted time semantics and verification chains
- Enum identity and singleton guarantees

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
- Standard error classes and gear error wrapping
- JSONL logging around capability calls
- Reserved word collision prevention

**Core I/O & Serialization:**
- JSON/YAML/CSV base functionality
- Serialization hooks for gear types
- Type-safe deserialization policies

### 20.3 Should Be Gears
These components are **appropriately** handled by gears:

- Domain capabilities (Shopify, blockchain, invoices)
- Domain types (`Product`, `Order`, custom records)
- Platform integrations and APIs
- Specialized exporters/importers
- Domain-specific validation rules
- External telemetry (beyond core logging)
- Workflow CLIs and tooling

### 20.4 Gray Areas
These may evolve between core and gears:

- **Event bus workers**: Core provides basic scheduling; advanced gears may enhance
- **RNG**: Core provides seeded `rand`; crypto gears may add secure variants (with permission checks)
- **Formatting**: Goblin personality messages stay in core but may be configurable

### 20.5 Architectural Benefits
This split ensures:
- **Core stays lean** while providing enterprise guarantees
- **Gears can innovate** without breaking safety/determinism
- **No vendor lock-in** - multiple gear implementations can compete
- **Auditability** - core behavior is predictable and verifiable

## 21. Gears — Philosophy & Architecture

### 21.1 Purpose
Gears are first‑class, modular extensions that feel native to Goblin. The core stays lean; anything domain‑specific lives in a gear.

**Key properties:**
- **Language‑native**: `use`, namespacing, `via`, `prefer`
- **Type‑aware**: gears can register types & capabilities
- **Contract‑checked**: implementations must match declared contracts
- **Deterministic**: pin versions, lock builds, sandbox side effects

### 20.2 Loading & Versioning
```goblin
use shopify@^1.6 as shp
use tarot_deck@1.2
use invoice            /// latest allowed by policy if not pinned (discouraged)
```
Pin by default. Projects should reference a version/range.

Alias: `as` sets a local alias (`shp::csv`).

Lockfile: `gears.lock` records `{gear, version, checksum, source}`; builds resolve only from lock unless `--update`.

### 20.3 Capability Resolution
Gears declare capabilities (named functions/templates/exporters). Calls resolve deterministically:

Call‑site `via`:
```goblin
export @cards via shp::csv
```

Global preference:
```goblin
prefer product.export via shp
```

Project config default map (`goblin.config.yaml`)

If multiple providers remain → `AmbiguityError`.

**Namespacing:**
Public symbols: `gear::Symbol` (e.g., `shopify::csv`)

Use `via gear::symbol` (or `via alias::symbol`) to bind a call

### 20.4 Contracts (First‑Class)
Contracts define the shape & errors of a capability; Goblin checks them at use time.

```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end

contract ledger_json(data: map) -> string
    errors: [ValueError, TypeError]
end
```

**Rules:**
Signature (names, arity, types) must match exactly

Only declared errors may be thrown; others are wrapped as `GearError(gear, capability, cause)`

Contracts are global IDs (e.g., `product.export`)

**Introspection:**
```goblin
gear_contracts("shopify")   /// ["product.export", "inventory.sync", ...]
```

### 20.5 Gear Manifest & Permissions
Each gear ships a `gear.yaml`:

```yaml
name: shopify
version: 1.6.2
provides: [product.export, inventory.sync]
contracts: [product.export]
requires:
  fs: ["dist/"]               # allowlisted paths
  net: ["api.shopify.com"]    # allowlisted hosts
  env: ["SHOPIFY_TOKEN"]
permissions:
  mode: "fs+net"              # none | fs | fs+net
checksum: "sha256-…"
```

**Gear naming:** Gear names may not be reserved words (see §19) to avoid namespace conflicts.

On use, core validates manifest/permissions against project policy. In deterministic builds, network is blocked unless allowlisted.

```goblin
gear_permissions("shopify")
```

### 20.6 Event Bus
Lightweight, in‑process bus with clear sync/async semantics.

**Emit:**
```goblin
emit "catalog.ready", payload            /// synchronous (errors bubble)
emit_async "order.ready", payload        /// enqueued (returns job id)
```

**Subscribe:**
```goblin
on "order.ready" mode: "async" concurrency: 4 error: "collect"
    ...
end
```

**Options:**
- `mode`: "sync" | "async" (default "sync")
- `concurrency`: workers for async handlers (default 1)
- `error`: "stop" (default for sync), "skip", "collect"

### 20.7 Sandbox & Determinism
Sandbox is enforced by the gear manifest + project policy.

Sandbox modes: "none" | "fs" | "fs+net"

Deterministic builds:

`goblin build --deterministic` blocks wall‑clock (use `trusted_now()` if permitted), blocks network unless allowlisted, and rejects nondeterministic randomness (seed it).

Dry‑run support for exporter contracts:
```goblin
file = product.export(@items) via shp dry_run:true
```

### 20.8 Logging & Telemetry
Standard JSONL at `dist/gear.log`, emitted by core around capability calls:

```json
{"ts":"2025-08-12T15:30:00Z","gear":"shopify","cap":"product.export","ms":128,"ok":true}
{"ts":"2025-08-12T15:30:01Z","gear":"shopify","cap":"product.export","ok":false,"err":"ValidationError: missing title"}
```

No phoning home unless a gear explicitly does so and permissions allow.

### 20.9 Testing Hooks
Inline tests run in a sandbox:

```goblin
test "shopify csv emits header"
    rows = shopify::csv_preview(@cards)
    assert rows[0].starts_with("Handle,Title")
end
```

**CLI:**
- `gears test shopify`
- `gears test --all`

### 20.10 Introspection APIs
```goblin
gears()                         /// ["shopify","tarot_deck"]
gear_symbols("shopify")         /// ["csv","api","configure", ...]
gear_contracts("shopify")       /// implemented contracts
gear_permissions("shopify")     /// sandbox/allowlists
```

**Development Tools:** Lint tools should warn if gear names match reserved words (§19) during `gears test` to catch conflicts early.

### 20.11 Usage Patterns

#### 20.11.1 Single Export
```goblin
use tarot_deck@1.2, shopify@^1.6 as shp
prefer product.export via shp

@cards = tarot_deck::card_template(price: .99, qty: 1)
    "Ace of Cups"
    "Two of Cups"

ensure_time_verified("shopify export")
file = product.export(@cards) via shp
say "Wrote {file}"
```

#### 20.11.2 Multi‑Platform Chain
```goblin
use board_game, shopify@^1.6 as shp, etsy@^2

@games = board_game::template()
    "Catan" :: qty: 4
    "Pandemic" :: qty: 2

export @games via shp::csv to "dist/shopify.csv"
export @games via etsy::csv to "dist/etsy.csv"
```

#### 20.11.3 Event‑Driven Pipeline
```goblin
emit "catalog.ready", @games

on "catalog.ready" mode: "async" concurrency: 2 error: "collect"
    ensure_time_verified("bulk export")
    product.export($event.payload) via shp
end

/// Blockchain ledger logging
emit "tx.logged", ledger_json({ 
    total: $100.00, 
    status: Status.Paid, 
    ts: trusted_now() 
})
```

### 20.12 Errors
`GearError(gear, capability, cause)`, `ContractError`, `PermissionError`, `AmbiguityError`, `LockfileError`, `DeterminismError`.

### 20.13 Project Config (excerpt)
```yaml
# goblin.config.yaml
gears:
  defaults:
    product.export: shopify
  permissions:
    mode: "fs+net"
    fs_allow: ["dist/"]
    net_allow: ["api.shopify.com"]
  enforce_lock: true
  deterministic_build: true
```

### 20.14 Example Contract & Call (sketch)
```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end

# Provided by shopify gear
file = product.export(@cards) via shopify::csv
```

## 22. Enums (Core)

### 21.1 Purpose
Closed sets of named constants with an optional backing int or string. Enums are first‑class types with strict equality and simple utilities. No implicit coercion.

### 21.2 Declaration
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

`as int` / `as string` are optional.

No backing specified → symbolic enum with stable ordinal (0..n‑1).

With `as int`/`as string`, values must be explicit to avoid surprises.

Variant names must be unique within the enum.

Enums are closed (no late additions).

#### 21.2.1 Sequential Int Enums (Optional)
For int‑backed enums, add `seq` to enable auto‑increment:

```goblin
enum Priority as int seq
    Low = 1      /// seed required
    Medium       /// auto: 2  
    High         /// auto: 3
end

enum Http as int seq
    Ok = 200
    Created      /// 201
    BadRequest = 400
    NotFound     /// 401 (continues from last explicit)
    ServerError = 500
    BadGateway   /// 501
end
```

**Rules:**
- `seq` only allowed with `as int`
- First variant in a `seq` enum **must** have an explicit int
- After any explicit value, subsequent unassigned variants continue `+1`
- Mixed explicit+auto is fine; duplicates still raise `EnumError`
- No `seq` for string‑backed enums (too magical/conflict‑prone)
- Omit `seq` if you want every int explicit (original behavior still supported)

### 21.3 Construction & Access
```goblin
s = Status.Paid
h = Http.Ok
c = Suit.Clubs
```
Type of `s` is `Status`, not string/int.

Access is namespaced: `EnumName.Variant`.

Singleton guarantee: Each enum variant is a unique, immutable singleton object. Any reference to `Status.Paid` in the program points to the same instance in memory, so equality checks are O(1) identity comparisons (`Status.Paid is Status.Paid` → true).

This also means you can safely use enum members as map/set keys without worrying about duplicate construction.

### 21.4 Introspection & Methods
```goblin
/// Instance methods
s.name()         → "Paid"
s.value()        → backing value or s.name() for symbolic enums
s.ordinal()      → 0‑based declaration index
str(s)           → "Status.Paid"

/// Type methods
Status.values()  → [Status.Pending, Status.Paid, Status.Shipped]
Status.names()   → ["Pending","Paid","Shipped"]
Status.from_name("Paid")          → Status.Paid
Status.try_from_name("X")         → nil
Http.from_value(404)              → Http.NotFound
Http.try_from_value(201)          → nil
```
`from_*` throws `EnumError` on failure; `try_from_*` returns `nil`.

### 21.5 Operators & Type Rules
Allowed comparisons: `==`, `!=`, `is`, `is not` between the same enum type.

Ordering (`<` etc.): allowed **only** for int‑backed enums (including `seq`) and **only** within the same enum type.

Cross‑type comparisons (e.g., `Status.Paid == "Paid"`): `TypeError`. Cast explicitly if needed.

Arithmetic / `++` / `--` / postfix math: not allowed on enums.

Maps/sets: enum members are valid keys (use bracket notation for keys):

```goblin
prices = {}
prices[Suit.Clubs] = 1.25
```

### 21.6 Patterning (Simple)
No special switch construct. Use standard conditionals or map dispatch:

```goblin
if s is Status.Pending
    "hold"
elif s is Status.Paid
    "ship"
el
    "investigate"
```

### 21.7 Interop (JSON/YAML/CSV)
Default surface is string name to keep files human‑readable, with opt‑ins mirroring money/datetime.

**Write options (additive to §14.2.2):**
```goblin
write_json(path, v, { enum: "name" | "value" | "object" = "name" })
```

**Read options:**
```goblin
read_json(path, { enum: "off" | "name" | "value" | "object" | "auto" = "off" })
```

**Modes:**
- **"name"**: write/read `"Status.Paid"` (qualified) to disambiguate across enums. On read with `enum:"name"`, strings of the form `"Enum.Variant"` decode if the enum exists; bare `"Variant"` is not decoded.

- **"value"**: write the backing value only (int/string); decode requires target enum context:
  ```goblin
  orders = read_json("orders.json", { enum: "value", enum_schema: { "status": "Status" } })
  ```
  Unknown keys in `enum_schema` are ignored.

- **"object"** (canonical object):
  ```json
  {"_type":"enum","enum":"Status","name":"Paid","value":"Paid","ordinal":1}
  ```

- **"auto"**: try "object" → qualified "Enum.Variant" → leave raw.

YAML/CSV behave like JSON "name" mode by default. No auto‑decode unless options specify.

**Stability note:** If stability across versions matters, prefer `"object"` or `"name"` modes in JSON. `"value"` mode ties you to the numeric plan (seq or explicit).

### 21.8 Casting & Formatting
```goblin
str(Status.Paid)     → "Status.Paid"
Status.Paid.name()   → "Paid"
Status.Paid.value()  → "Paid"   /// for symbolic enums equals name()
fmt(Status.Paid, "") → "Status.Paid"   /// fmt defers to str()
```

### 21.9 Ranges & Loops
```goblin
for v in Status
    say v.name()
```
Enum types are iterable in declaration order.

### 21.10 Errors
`EnumError` (unknown name/value, duplicate value in int/string enums), plus `TypeError` where noted.

**Development Tools:** Lint tools should warn if `seq` gaps exceed 100 to catch potential mistakes (e.g., `Low = 1, High = 1000`) that could break ordering logic.

### 21.11 Examples
```goblin
enum Status
    Pending
    Paid
    Shipped
end

order = { id: 17, status: Status.Pending }

if order.status is Status.Pending
    "hold"
el
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

/// Non-sequential example showing seq flexibility
enum Level as int seq
    Beginner = 1
    Advanced = 10
    Expert       /// 11
end

say Level.Expert.value()  /// 11

/// String-backed
enum Suit as string
    Clubs = "C"
    Diamonds = "D"
    Hearts = "H"
    Spades = "S"
end

/// JSON roundtrip
write_json("order.json", order, { enum: "name" })
back = read_json("order.json", { enum: "name" })
say back.status                    /// Status.Pending

/// From strings/values
Status.from_name("Paid")          /// Status.Paid
Http.from_value(404)              /// Http.NotFound

/// Predicate helpers for value ranges
is_client_error(code: Http) = code.value() in 400..499
is_server_error(code: Http) = code.value() in 500..599
```

## 22. gmark (Goblin Mark)

**gmark** is Goblin's built-in **global reference system** — a unique, persistent identifier for records within a project. It's designed to make cross-file, cross-gear linking dead-simple without relying on databases, manual key management, or brittle file paths.

### 22.1 Purpose
`gmark` solves three common problems in modular, file-driven projects:

1. **Stable Linking** — A gmark stays the same even if file paths, names, or structures change.
2. **Conflict-Free** — Each gmark is unique **within a project scope**, avoiding collisions between unrelated records.
3. **Gear-Aware** — Gears can read/write gmarks without knowing file layouts, making interlinking trivial.

### 22.2 Structure
A gmark consists of:

- **Type Prefix** *(optional)* — Short string indicating the kind of thing (`page`, `post`, `order`, etc.).
- **Name String** *(required)* — Human-readable slug (`about-us`, `invoice-1928`).
- **ord** *(auto-assigned)* — Internal integer ID used for sorting/search; invisible to end-users.

**Example:**
```
gmark: post:about-us ord: 42
```

### 22.3 Creation
- **Automatic**: When a new record is created, Goblin assigns the next available `ord` and generates a gmark from the provided name.
- **Manual**: Developers may specify a gmark explicitly to preserve legacy links or match external systems.
- **Guaranteed Unique**: Goblin enforces uniqueness at project scope. A conflict results in a `GmarkConflictError`.

### 22.4 Storage
- All gmarks and their ord values are tracked in a `.gmarks` file at the project root.
- This file is updated automatically on create, delete, or rename operations.
- It is **core Goblin**, not gear-specific, so every gear benefits from the same ID system.

**Example `.gmarks` file:**
```
post:about-us: 42
post:contact: 43
order:2025-08-12-004: 44
```

### 22.5 Usage
Gmarks are used to:

- Link one record to another (`related: gmark:post:about-us`)
- Cross-reference between gears (Shopify gear links `order` to `customer`)
- Resolve records even if file paths change

**Example linking in YAML:**
```yaml
title: Contact
gmark: post:contact
related: 
  - post:about-us
```

### 22.6 Search & Sort
- Gmarks can be **looked up** directly (`find_gmark("post:about-us")`).
- `ord` can be used for **ordering** (chronological inserts) without parsing names.
- The `.gmarks` registry is indexed in memory for instant lookups.

### 22.7 Reserved Behaviors
- **Project Scope**: Gmarks are unique per project, but different projects may reuse the same names without conflict.
- **Immutable by Default**: Changing a gmark after creation is discouraged; Goblin will emit a `GmarkChangeWarning`.
- **Gear-Agnostic**: Any gear can read/write gmarks without special integration.

### 22.8 Error Types
- `GmarkConflictError` — Attempted creation of a duplicate gmark.
- `GmarkMissingError` — Reference to a gmark that does not exist.
- `GmarkChangeWarning` — Modification of an existing gmark.

This system makes Goblin feel **tightly integrated** between core and gears, while keeping the flexibility of a purely file-driven, modular environment.
