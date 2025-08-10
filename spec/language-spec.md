# Goblin — Language Specification v1.4

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
Money ops must follow currency rules (same currency, see §10).

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

### 10.1 Type & Rounding
- `money` = currency-aware numeric with exactly two decimals
- Internal math uses minor units (cents) and converts back for display
- Rounding policy: half-away-from-zero after operations

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

/// Display
say money(3.2, USD) → USD 3.20
str(money) → CUR 1.23
fmt(float(m), ",.2f")  /// numeric only
```

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
money * int|float → money (same currency)
money // int → pair (quotient: money, remainder: money)
money % int → remainder money (alias of second element of //)
```

**Promotion rule:** If one operand is `money(CUR)` and the other is `int|float`, promote numeric to `money(CUR)` and operate (except `/`, which is disallowed).

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
- To change cents: do it explicitly (`price = price + .05`)

### 10.7 Even Splits & Remainder Ledger

#### Hard Rule on Division
Using `/` with money always errors: `MoneyDivisionError: Use // to capture remainder or divide_evenly(total, parts).`

#### Divmod for Money
```goblin
q, r = total // parts
/// q = base share (money)
/// r = leftover remainder (money, < 1 unit in minor currency)
```

#### Even Split Helper
```goblin
divide_evenly(total: money, parts: int) -> array<money>
```
- Splits using minor units and distributes leftover cents to the first k shares deterministically (largest remainders method)
- Sum of shares equals total exactly; no remainder returned

#### Remainder Ledger (Audit)
Goblin tracks any money remainder you don't capture:

```goblin
/// If you ignore remainder, it's tracked:
q, _ = total // n

/// End-of-script helpers:
remainders_total()   → map { CUR: money }
remainders_report()  → human-readable summary lines
clear_remainders()   → reset ledger
```

**Examples:**
```goblin
default money USD
total = 100

q, r = total // 3   /// q = USD 33.33, r = USD 0.01

shares = divide_evenly(total, 3)
/// shares → [USD 33.34, USD 33.33, USD 33.33]

/// If you discard remainder, it's tracked:
_, _ = total // 7
say remainders_total()     /// => { USD: USD 0.?? }
say remainders_report()
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
```

### 11.2 Tax Helpers
```goblin
tax(subtotal, rate_or_rates, compound=false) → tax amount
with_tax(subtotal, rate_or_rates, compound=false) → subtotal + tax(...)

/// rate_or_rates: single rate (0.10 or 10%) or array ([8.25%, 1%])
/// compound=true applies sequentially; else additive
```
- Rounding: half-away-from-zero to 2dp
- `compound=false`: compute each component, round each to 2dp, then sum
- `compound=true`: apply each rate sequentially, rounding each step's tax to 2dp

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
- `add` / `sum` — sum (returns element type; money stays money)
- `mult` — product
- `sub` — left fold subtraction
- `div` — left fold division (returns float unless exact)
- `min` / `max` — extrema
- `avg` — arithmetic mean (money if same-currency; else float; rounds to 2 dp)
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

/// File System
exists(path), mkdirp(path), listdir(path), glob(pattern)
cwd(), chdir(path), join(a,b,...)

/// Utils
now(), uuid()
```
Paths relative to CWD unless absolute.

---

## 15. Shopify Profile (Gears: "Game Goblin")

### 15.1 Repo Layout
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
```

### 15.2 CLI (Gears)
gears init                          # Initialize project
gears spawn tarot_deck "Mystic"     # Generate template  
gears build mystic.yaml --chain tarot_deck,shopify,etsy,ebay  # Chain exports
gears list                          # Show available gears
gears install community_gear        # Install from repository

# Codes
gears code set "Deviant Moon Borderless Tarot" DMBT
gears codes list
gears codes grep DMBT

### 15.3 Configuration Files
- **collections-map.yaml**: Authoritative map from Goblin category to Shopify collections (first = primary)
- **types-registry.yaml**: Per type: required fields, allowed categories, defaults (options, split strategy, SEO templates)
- **goblin.config.yaml**: SKU policy, paths, currency settings

### 15.4 SKU Generation
```yaml
sku_policy:
  default_pattern: "{CODE}-{YEAR?}-{LIST}-{PARTCODE}-{ATTRCODE?}"
  tarot_deck_pattern: "{CODE}-{LIST}-{SUITCODE}-{CARDNUM:02}"
  require_year_for_types: ["rpg_book", "vg_guide", "vintage"]
  autoinc: false  # set true to allow A/B suffixing on collisions
```

### 15.5 CSV Import/Export (Shopify)
- `--initial`: full rows + qty; Draft by default unless `meta.publish=true`
- `--append`: minimal rows; only new variants; no qty changes
- CSV exports numeric Variant Price in store currency
- If item currency ≠ store currency: warn and log to report.txt

---

## 16. Examples

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

### Money Examples
```goblin
default money USD

a = 1.50      /// USD 1.50
b = $2.25     /// USD 2.25  
c = €3.00     /// EUR 3.00

say a + b     /// USD 3.75
a + c         /// CurrencyError

price = 4.50
price++
price = price + .05  /// USD 5.55
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

---

## 17. Errors & Warnings

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
`NameError`, `TypeError`, `ValueError`, `IndexError`, `KeyError`, `ZeroDivisionError`, `SyntaxError`, `AssertionError`, `CurrencyError`, `MoneyDivisionError`

---

## 18. Reserved Words

```
if, elif, el, for, in, while, class, init, return, skip, stop, try, catch, finally, 
assert, error, warn, say, true, false, nil, default, int, float, money, bool, 
read_text, write_text, read_yaml, write_yaml, read_csv, write_csv, exists, mkdirp, 
listdir, glob, cwd, chdir, join, now, uuid, add, sum, mult, sub, div, mod, divmod, 
pow, root, floor, ceil, round, abs, min, max, rand, randint, tax, with_tax, bit, gear
```
## 19. Gears: Modular Add-ons

### 19.1 Overview
Gears are modular add-ons for Goblin that extend language capabilities for specific domains. Each gear is self-contained with no external dependencies, avoiding conflicts and dependency hell.

**Key Points:**
- **Independent** — Each gear operates in isolation, avoiding conflicts
- **Community-driven** — Users can create and share gears for their industries
- **Composable** — Install only the gears you need for your workflow
- **First-class syntax integration** — Gears feel native in Goblin code

### 19.2 Loading Gears
```goblin
use shopify
use tarot_deck as tarot
use invoice@^1.2
```
- `use gear_name` loads a gear into the program
- Optional alias with `as`
- Optional version specifier with `@`

### 19.3 Namespaced Symbols
Gears expose templates, functions, and exporters under their namespace:
```goblin
tarot::card_template
shopify::csv
invoice::calc_fee
```

### 19.4 Gear Templates
Gears can publish template structures that integrate with Goblin's template syntax:
```goblin
use tarot_deck as tarot

@cards = tarot::card_template(price: .99, qty: 1)
    "Ace of Cups" :: 3
    "Two of Cups"                  /// uses defaults
    "Three of Cups" :: price: 1.25 :: 2
```
- Gear defines template structure and optional defaults
- Local binding with `@name` sets project-specific defaults
- Uses standard positional override syntax (`::`)

### 19.5 Export/Import via Gears
```goblin
/// Export data through gear formatters
export @cards via shopify::csv to "dist/products.csv" mode: "append"

/// Import data through gear parsers
orders = import "orders.csv" via shopify::csv
```
- `via gear` specifies which gear handles the operation
- Target format: `shopify::csv`, `shopify::api`, etc.
- Extra options (`to`, `mode`, etc.) are passed to the gear

### 19.6 Gear Functions
```goblin
fee = invoice::calc_fee(subtotal: 125.00, rate: 2%)
```
Gear functions integrate with Goblin's type system (money, percentages, etc.)

### 19.7 Configuration & Validation
```goblin
/// Configure gear behavior
shopify::configure(
    store: "game-goblin",
    currency: USD,
    strict_currency: false
)

/// Validate data against gear rules
validate @cards via tarot::rules
```
If validation fails, Goblin raises standard errors or warnings.

### 19.8 Introspection
```goblin
say gears()                  /// ["shopify", "tarot_deck"]
say gear_symbols("shopify")  /// ["csv", "api", "configure", ...]
```

### 19.9 Full Example
```goblin
use tarot_deck as tarot
use shopify

shopify::configure(store: "game-goblin", currency: USD)

@cards = tarot::card_template(price: .99, qty: 1)
    "Ace of Cups" :: 3
    "Two of Cups"
    "Three of Cups" :: price: 1.25 :: 2

validate @cards via tarot::rules
export @cards via shopify::csv to "dist/products.csv" mode: "append"
```

### 19.10 Example Gear Ideas
- **tarot_deck** — Knows about suits, arcana, card pricing
- **shopify** — Handles Shopify CSV format, product variants, inventory
- **restaurant_menu** — Manages dishes, ingredients, dietary restrictions
- **invoice** — Generates business invoices with tax calculations
