1. Philosophy
Goblin's guiding principle: code should be as clear to read as it is to write.
 All design choices follow: intuitive, simple, consistent.

Human-Readable Code
One statement per line (no semicolons).
Indentation defines blocks (no braces, no end in simple cases).
Natural constructs mirror human phrasing.
name = "Alice"
age = 25
price = $19.99

if score > 90
    say "Excellent work!"
    award_bonus 100

pick 3 from enemies
add gold to inventory
reap first from deck


Safe by Default
Undefined variables → error.
No silent string coercion.
Type safety without ceremony.
player_name = "Alice"
say playername   /// NameError: undefined variable 'playername'

score = 100
say "Score: " || "{score}"
/// say "Score: " || score     /// TypeError

price = $29.99
count = 5
total = price * count
/// total = price + count      /// TypeError


Practical Batteries
Built-ins for finance, data interchange, and randomness.
subtotal = $127.50
tax = 8.25% of subtotal
total = subtotal + tax

data = "config.json".read_json
users = "users.csv".read_csv
"output.yaml".write_yaml(processed_data)

damage = roll 2d6+3
enemy = pick enemies
loot = sample_weighted drops


Goblin Culture
Language includes humor, easter eggs, and goblin-themed concepts.
Glams (glamours) are extensions.
Built-in money/percent ensure precise counting.
Emphasis on controlled chaos (randomness/probability).
treasure_hoard = $1234567.89
formatted_display = treasure_hoard.format(",.2f")
tax_rate = 12.5%
after_tax = treasure_hoard - (tax_rate of treasure_hoard)


The Goblin Way
Goblin code is:
Readable (understand months later)
Safe (confidence in refactor)
Practical (real apps out of the box)
Fun (playful by design)
File extension: .gbln

2. Installation and Setup
Coming soon...
3. Basic Syntax
Your First Goblin File
Goblin files end with .gbln.
One statement per line.
Indentation defines blocks (4 spaces). Blocks close with end.
health = 100        /// Good: one action per line
say "Player ready"  /// Good: clear and simple
health = 100; say "Player ready"  /// Bad: don't do this

if player.health > 0
    continue_game     /// Indented inside block
    update_display
end                   /// Block closes


Comments
Single-line: ///
Block: //// … ////
# = private variables in classes
// = integer division
/// Single-line comment
health = 100  /// Inline comment

////
Block comment
////

class Player
    #health = 100    /// Private
    name = "Alice"   /// Public
end

result = 15 // 4     /// Integer division = 3


Identifiers & Variable Names
Valid: letters, numbers, underscores; cannot start with number.
Invalid: no . or -.
Case-sensitive.
player_name = "Alice"    /// Valid
user123 = "Bob"          /// Valid
TeamRoster = []          /// Valid
x2 = 50                  /// Valid

user.name = "Alice"      /// Invalid: no dots
user-name = "Bob"        /// Invalid: no dashes
2cool = "Dave"           /// Invalid: cannot start with number

Dot is reserved for:
Method calls (text.upper)
Decimals (price = 1.25)
Type ops (str.upper)

Calling Operations
Exactly two forms: method-style and prefix-style.
Method-style (dot)
text.upper        /// Zero-arg: parens optional
text.trim_lead
text.replace("old","new")
numbers.pick(3)

result = dirty_text.trim_lead.upper.replace("_"," ").slug

Prefix-style (English-like)
upper text
trim_lead text
replace "old" with "new" in text
split message on ","
join parts with ", "

Unary Auto-methodization
greet "Alice"        /// Prefix-style
"Alice".greet        /// Method-style

text.replace("old","new")    /// Multi-arg requires method-style


Output & Printing
Use say to print.
Lines starting with quotes auto-print.
say "Hello, world!"
say player.name
say total_score

"Hello, world!"     /// Same as: say "Hello, world!"
"Score: {score}"    /// Same as: say "Score: {score}"


Important Notes
No semicolons (line breaks separate statements).
in is loop-only, not boolean operator.
Case-sensitive identifiers.
Blocks end with end.
Only method-style and prefix-style calls (no op(val)).
4. Keywords & Reserved Words
Hard Keywords (cannot be shadowed)
if, elif, else, for, in, while, repeat, unless,
attempt, rescue, ensure,
return, skip, jump, until, stop, assert,
class, op, enum, use, import, export, via, test,
true, false, nil,
judge, morph, vault, banish, unbanish, expose


Soft Keywords (context-dependent)
from, at, first, last, to, into, with, dups, seq,
as, where, raw, trim_lead, on, like, and


Built-ins (shadowable operations & types)
int, float, bool, big, money, pct, date, time, datetime, duration,
len, shuffle, sort, add, insert, replace,
roll, roll_detail, freq, mode, sample_weighted,
set, settle, pick, reap, usurp, unique,
split, join, map, format,
upper, lower, title, slug,
trim, trim_lead, trim_trail,
escape, unescape, reverse, mixed, minimize, remove,
before, after, before_last, after_last, between,
lines, words, chars,
find_all, match, find, has, count, parse_bool,
sum, avg, min, max,
drop, cut

5. Variables
Variables are named storage locations for values. They allow you to assign, change, and reference data during program execution. In Goblin, variables are flexible: they can hold any type and be reassigned freely.
Creating and Assigning Variables
Use = to assign values to variables:
health = 100             /// health starts at 100
health = health - 25     /// health becomes 75
name = "Alice"           /// name holds text
price = $19.99           /// price holds money

The assignment operator = binds the value on the right to the name on the left. Variables begin to exist when first assigned.
Naming Rules
Must start with a letter or underscore: score, _hidden


May contain letters, numbers, underscores: max_health, level_2


Cannot start with a number: 2players is invalid


Dots are not allowed in identifiers (reserved for method calls). Use underscores instead: user_name, not user.name.


Cannot use reserved words (e.g., if, class, op).


player_health = 100      /// ✅ valid
user.name = "Alice"      /// ❌ invalid: dot not allowed

Scope
Variables exist only within the scope where they are declared:
score = 0                /// global variable

op add_point()
    score = score + 1    /// updates global score
    local_bonus = 10     /// local to operation
end

say score                /// 1
say local_bonus          /// ❌ NameError

Global variables are accessible everywhere unless shadowed. Operation-local variables are isolated.
Constants by Convention
Goblin does not enforce immutability. Instead, variables in ALL CAPS are treated as constants by convention:
MAX_HEALTH = 100
player_health = MAX_HEALTH

Reassigning ALL CAPS variables triggers a warning:
MAX_HEALTH = 200   /// ⚠️ Warning: reassigning constant-style variable

Types and Literals
Variables can hold any supported type. Goblin provides literal forms and suffixes for clarity.
Strings
name = "Alice"
quote = 'single quotes ok'
multi = """multi-line"""

Numbers
count = 5          /// int (64-bit)
count2 = 5i        /// int with explicit suffix
ratio = 3.14       /// float (~16 sig digits)
ratio2 = 3.14f     /// float with explicit suffix
bigint = 123b      /// big (high precision)
bigdec = 5.0b      /// big decimal

Money
price = $12.00
rent = EUR 950.50

⚠️ Money uses $ or ISO code literals, not suffixes.
Percent System (CIPO)
tip = 20%          /// 0.20 (percent of 1)
discount = 25%s    /// 25% of self: 80 - 25%s → 60
portion = 25%o 200 /// 25% of other → 50

Shorthands: % self = %s, % of expr = %o expr
Durations
wait = 90s   /// 90 seconds
delay = 6mo  /// 6 months

Valid suffixes: s, m, h, d, w, mo, y
 m always means minutes.
Dates & Times
today = date "2025-08-23"
clock = time "14:30:05.250"
ts = datetime "2025-08-23T14:30:00" tz:"UTC"

Boolean & Nil
flag = true
finished = false
missing = nil

Casting and Conversions
Goblin provides operations to convert between types:
"123".int       /// 123 (integer)
"3.14".float    /// 3.14 (float)
"8.5%".pct!     /// 0.085 or CastError
"8.5%".pct?     /// 0.085 or nil

123.big          /// 123b (big)
3.14.big         /// 3.14b (big)

pct! → strict cast, raises CastError on failure


pct? → safe cast, returns nil on failure


Numeric casts promote automatically: int + float → float, any with big → big


Quick Reference
Assignment:
name = value


Variables created on first assignment


Naming rules:
Letters, numbers, underscores; cannot start with number


No dots in identifiers


Reserved words forbidden


Scope:
Global = top-level, visible everywhere


Local = inside operations/modules


ALL CAPS = constant-style (warning on reassignment)


Literal forms:
Strings: "abc", """multi"""


Money: $12.00, EUR 9.50


Int: 5, 5i


Float: 3.14, 3.14f


Big: 123b, 5.0b


Percent: 20%, 25%s, 25%o other


Duration: 90s, 6mo, 1y


Date: date "2025-08-23"


Time: time "14:30:05"


Datetime: datetime "2025-08-23T14:30:00" tz:"UTC"


Boolean: true, false


Nil: nil


Casting:
.int, .float, .big


.pct! strict, .pct? safe


What's next: Now that you can store values in variables, we’ll look at working with text using Goblin's string features.

6. Strings
Strings represent text values in Goblin. This section defines literal forms, parse-time modifiers, joining operators, interpolation, escapes, and the core operations for transforming, searching, and parsing text.
String Literals
Strings are constructed with quotes only. Unlike numeric types, there are no suffixes; the quote form itself defines the type.
Basic
s1 = "Alice"           /// double quotes
s2 = 'Hello there'      /// single quotes
s3 = ""                /// empty string (falsy)
s4 = ''                 /// empty string (falsy)

Single or double quotes are equivalent.


Empty string is falsy in conditionals.


Multi-line
m1 = """
Line one
Line two
"""                       /// preserves newlines & indentation literally


m2 = '''
Alt form
Multiple lines
'''                        /// alternative triple-quoted form

Parse-time Modifiers (literals only)
p  = raw "C:\\path\\file.txt"   /// raw: no escapes, no interpolation
r1 = raw """abc"""              /// raw + multi-line
b  = trim_lead """
    Hello
      World
"""                           /// removes common leading indent
b2 = trim_lead raw """
    {not interpolated}
"""                           /// modifiers can stack

raw and trim_lead apply at parse time to literals; order does not matter (they are declarative).


Literal sugar: "literal".raw and "literal".trim_lead are parsed as prefix-on-literal.


x.raw where x is a variable is not allowed (use escape/unescape at runtime).


Combining Strings
Join Operators
first = "Hello"
second = "World"


no_space = first | second         /// "HelloWorld"
spaced   = first || second        /// "Hello World"


file = "save" | ".dat"            /// no-space join for tokens
msg  = "Welcome" || name           /// space join for sentences

| joins directly with no spaces.


|| inserts a single space.


Interpolation (recommended)
name  = "Alice"
score = 1500
money = $19.99


msg = "Player {name} scored {score} points (bank: {money})"  /// auto-converts
css = "body {{font-size:{score}px;}}"                          /// literal braces via {{ }}
url = "/users/{name.slug}"                                    /// zero-arg methods allowed

Interpolation auto-converts values to text. Use {{ and }} for literal braces.


Escape Sequences (non-raw)
n  = "line1\nline2"         /// newline
tb = "a\tb"                 /// tab
qt = "He said \"Hi\""      /// quote
bs = "C:\\path"            /// backslash
u  = "Heart: \u2764"       /// Unicode code point

String Operations
Case & Formatting
"john DOE".upper         /// "JOHN DOE"
"john DOE".lower         /// "john doe"
"john DOE".title         /// "John Doe"
"goblin".mixed          /// e.g., "gObLiN" (randomized)
"Hello, World!".slug     /// "hello-world"

Whitespace & Trimming
s = "  Hello   World  "


s.trim              /// "Hello   World" (both ends, whitespace)
s.trim_lead         /// "Hello   World  " (start only, whitespace)
s.trim_trail        /// "  Hello   World" (end only, whitespace)
s.minimize          /// "HelloWorld" (remove ALL whitespace)


"backup_data".trim_lead("backup_")   /// "data"
"file.bak".trim_trail(".bak")        /// "file"

trim, trim_lead, and trim_trail remove whitespace by default.


When given an argument, they remove that specific substring instead.


Strings are immutable; all operations return new strings.


Search & Analysis (literal, case-sensitive)
text = "The quick brown fox jumps"
text.has "fox"           /// true
text.find "fox"          /// start index (0-based) or nil
text.find_all "o"        /// [indices], non-overlapping
text.count "the"         /// 1 (case-sensitive)

Searches are literal (no regex) and case-sensitive.


Indices are Unicode code-point positions.


find_all is non-overlapping.


Replace / Remove
s = "cats and cats"


s.replace "cats" with "dogs"        /// "dogs and dogs" (ALL)
s.replace_first "cats" with "dogs"  /// "dogs and cats"
s.remove " and "                    /// "cats cats" (alias: .drop)

Extraction
email    = "alice@example.com"
before "@" in email          /// "alice"
after  "@" in email          /// "example.com"


path     = "/a/b/file.txt"
before_last "/" in path      /// "/a/b"
after_last  "/" in path      /// "file.txt"


between "<b>" and "</b>" in "<b>Bold</b>!"   /// "Bold"

Breakdown
text = "Hello\nWorld"


lines text              /// ["Hello","World"]
words "a b  c"         /// ["a","b","c"] (whitespace split)
chars "abc"            /// ["a","b","c"] (code points)


"Hello\nWorld".lines    /// method forms also exist

Split & Join
parts = split "a,b,,c" on ","   /// ["a", "", "b", "", "c"] (keeps empties)
line  = join ["a","b"] with "," /// "a,b"

split returns an array of strings; delimiter must be a string; empty delimiter ⇒ ValueError.


join accepts an array of strings and a string separator; any non-string element ⇒ TypeError; empty array ⇒ "".


Regular Expressions (lean core)
find_all like raw "[\\w.]+@[\\w.]+" in text      /// email-ish
match     like raw "\\d+"              in value     /// digits
replace   like raw "\\s+" with " "     in messy     /// collapse spaces

Pattern subset: . [] () | ^ $ + * ? {m,n} \d \w \s.


Prefer raw for readability. Full PCRE is available via the optional regex glam.


Rules & Constraints
Strings are immutable; operations return new strings.


Interpolation auto-converts; joins require strings (convert explicitly or interpolate).


Searches are literal/case-sensitive; indices are Unicode code-point based; find_all is non-overlapping.


raw and trim_lead are parse-time on literals only; modifier order is not significant; variable .raw is invalid.


| joins without a space; || inserts one space.


trim, trim_lead, and trim_trail remove whitespace by default; with an argument they remove that substring.


Quick Reference
Literals
Strings: quotes are the constructor → "text", 'text', """multi""", '''multi'''


Modifiers: raw "...", trim_lead """ ... """, may stack (order agnostic)


Combining
No-space: A | B


Space: A || B


Interpolation: "Hello {x}"; literal braces: {{ }}


Escapes (non-raw)
\n, \t, \", \', \\, \uXXXX


Operations
Case/format: .upper .lower .title .mixed .slug


Whitespace/trim: .trim .trim_lead .trim_trail .minimize


Search: .has .find .find_all .count


Replace: .replace .replace_first .remove/.drop


Extract: before/after/before_last/after_last/between


Breakdown: .lines .words .chars


Split & Join: split S on D, join A with S


Regex (lean core)
find_all like P in S, match like P in S, replace like P with N in S


Deprecation Note:
 strip_lead and strip_trail are deprecated. Use the unified trim family instead:
s.trim_lead → whitespace removal (default)


s.trim_lead("prefix") → remove substring prefix


s.trim_trail("suffix") → remove substring suffix
7. Numbers
Numbers represent integers, decimals, big precision values, percentages, and money. This section defines literal forms, suffix rules, numeric operations, conversions, and formatting.
Numeric Literals & Suffixes
Integer & Float
x = 5        /// int (64-bit)
y = 5i       /// int with explicit suffix
r = 3.14     /// float (~16 sig digits)
s = 3.14f    /// float with explicit suffix

Whole numbers default to int.


Decimals default to float.


Big
b1 = 123b        /// big (arbitrary precision)
b2 = 5.0b        /// big with decimal
"123.450".big   /// cast from string → 123.450b

Single type only: big (no split int/decimal).


Default precision: 64 sig digits.


Division rounds per context (mode half_even default).


Any with big → big.


Money
price = $12.00
rent  = EUR 950.50

Uses $ or ISO prefix literals.


Backed by big engine with project-defined policy (precision, rounding, ledgering).


Not a suffix.


Percent System (CIPO)
tip      = 20%           /// 0.20 (percent of 1)
discount = 25%s          /// 25% of self: 80 - 25%s → 60
portion  = 25%o 200      /// 25% of other: 50

% → percent of 1.


%s → percent of self.


%o → percent of other (sugar for % of).


Sugar: % self = %s, % of x = %o x.


Durations & Temporal
wait  = 90s    /// seconds
delay = 6mo    /// months
date  "2025-08-23"
time  "14:30:05.250"
datetime "2025-08-23T14:30:00" tz:"UTC"

Suffixes: s, m, h, d, w, mo, y.


m = minutes only (collision rule).


Boolean & Nil
flag = true
endp = false
none = nil

Rules
Unknown suffix ⇒ SyntaxError.


Money uses prefix $/ISO, not suffix.


Math Operations
/// Arithmetic
10 + 5     /// 15
10 - 3     /// 7
4 * 6      /// 24
15 / 3     /// 5.0 (always float)
15 // 4    /// 3 (quotient only)
15 % 4     /// 3 (modulo)
2 ** 8     /// 256 (power)
9**        /// 81 (postfix square)
16//       /// 4 (postfix sqrt)

Divmod Operator
r = 10 >> 3     /// quotient+remainder pair
say r.q         /// 3
say r.r         /// 1
q, rem = 10 >> 3

Increment/Decrement
score = 10
old = score++   /// old=10, score=11
old = score--   /// old=11, score=10

Conversions
Between Numerics
3.14159.int    /// 3 (truncates)
5.int          /// 5
5.float        /// 5.0
123.big        /// 123b

From Strings
"123".int        /// 123
"45.67".float    /// 45.67
"8.5%".pct!      /// 0.085 or CastError
"8.5%".pct?      /// 0.085 or nil

To Strings
score = 1500
msg = "Score: " || "{score}"   /// explicit join
msg = "Score: {score}"         /// interpolation auto-converts

Formatting
n = 1234567.89
n.format(",")       /// "1,234,567.89"
n.format(".2f")     /// "1234567.89"
n.format(",.2f")    /// "1,234,567.89"
3.14159.format(".2f")   /// "3.14"

.format accepts standard patterns (commas, decimals, precision).


Math Functions
3.7.round      /// 4
3.7.floor      /// 3
3.2.ceil       /// 4
(-5).abs       /// 5
2.pow(8)       /// 256
16.sqrt        /// 4.0

[5,2,8,1].min  /// 1
[5,2,8,1].max  /// 8
[5,2,8,1].sum  /// 16
[5,2,8,1].avg  /// 4.0

Method-style and prefix-style both supported.


Rules & Constraints
/ always float; // integer quotient; >> returns quotient+remainder.


% = modulus unless literal % attached.


Literals: int (i), float (f), big (b).


Any with big ⇒ big.


Money is big+policy (precision, rounding, ledgering per project policy).


Percent uses %, %s, %o. Distinct from modulus.


Quick Reference
Int: 5, 5i


Float: 3.14, 3.14f


Big: 123b, 5.0b


Money: $12.00, EUR 9.50


Percent: 25%, 25%s, 25%o x


Duration: 90s, 6mo, 1y


Date/time: date "2025-08-23", time "14:30:05", datetime "2025-08-23T14:30:00" tz:"UTC"


Convert: .int, .float, .big, .pct!, .pct?


Join/strings: interpolation auto-converts; joins require strings.


Format: .format(pattern)


Math ops: .round, .floor, .ceil, .abs, .pow, .sqrt


Collections: .min, .max, .sum, .avg


Deprecation Note:
 Earlier drafts used .to_int, .to_float, and .to_string for conversions. These forms are now deprecated.
Use the canonical casts instead:
.int for integers


.float for floats


.big for big precision


.pct! / .pct? for percent parsing

8. Booleans, Comparisons & Truthiness
Booleans represent logical values and determine how conditions evaluate. Goblin also defines truthiness: which non-boolean values behave as true or false in conditionals.
Boolean Literals
is_alive = true
is_dead  = false

Only true and false are boolean literals.


Truthiness Check vs Boolean Parsing
Truthiness
"".bool       /// false (empty string)
" ".bool      /// true (non-empty string)
0.bool        /// false
5.bool        /// true
[].bool       /// false
[1].bool      /// true
nil.bool      /// false

value.bool (or bool value) tests if a value is considered present/meaningful.


Parse Boolean
"true".parse_bool    /// true
"false".parse_bool   /// false
"YES".parse_bool     /// true (case-insensitive)
"0".parse_bool       /// false
"maybe".parse_bool   /// CastError

parse_bool converts specific strings to actual boolean values.


Strict: invalid strings throw CastError.


Rule of thumb: use .bool for presence checks, .parse_bool for parsing user input.
Comparison Operators
Equality
3 == 3.0        /// true (numeric equality ignores type)
score != 100    /// true if not equal

Strict Identity
3 === 3         /// true
3 === 3.0       /// false (different types)
$1.00 === 1.00  /// false (money vs float)

Relational
age > 18
score >= 100
level < 10
health <= 50

Chaining
if 1 < level < 10
if 0 <= score <= 100

Between
if age between 13..19
if score !between 0..100
if level between 1...10   /// exclusive upper bound

Equivalent to chained comparisons.


Type/Variant Checks
status is Status.Active
player is not Enemy

Special Equality Rules
Money
$10.00 == $10.00        /// true
$10.00 == EUR 10.00     /// false (different currencies)

Datetime
dt1 = datetime "2025-01-01T00:00Z"
dt2 = datetime "2024-12-31T19:00-05:00"
dt1 == dt2    /// true (same moment)
dt1 === dt2   /// false (different repr)

Logical Operators
is_alive and has_key
health > 0 && mana > 0   /// && alias for and
is_admin or is_mod
not is_dead
!game_over               /// ! alias for not

and, or, not are primary forms; &&, ! are aliases.


Truthiness Rules
Falsy
false, 0, 0.0, "", [], {}, nil

Always Truthy
$0.00
EUR 0.00
datetime "2025-01-01"
duration 0

Money and datetime/duration are always truthy.


Undefined variable access throws NameError (never falsy).


Quick Reference
Booleans: true, false


Check truthiness: x.bool or bool x


Parse bool strings: x.parse_bool


Comparisons: ==, !=, <, <=, >, >=


Strict identity: ===, !==


Range: chained comparisons or between/!between


Type checks: is, is not


Logic: and/&&, or, not/!


Falsy: false, 0, 0.0, "", [], {}, nil


Always truthy: money, datetime, duration


What's next: Collections — arrays, maps, and group data handling.
9. Operators & Math
Operators define how expressions are evaluated: arithmetic, comparison, access, chaining, and short-cuts. This section specifies precedence/associativity, arithmetic variants (including money), postfix forms, compound assignment, method-style math ops, bitwise (method-only), optional chaining, null-coalescing, and the pipeline. Randomness is defined separately in #13 Randomness.
PATCHES::
Percent vs modulo (tiny precedence note to add under #18 and the Percent section)
Percent literals (12.5%) just produce a percent value;
 of/%o are operators that evaluate exactly like parentheses around a normal multiplication:

 10 + 10% of 100   /// == 10 + (0.10 * 100) == 20


Modulo % is the arithmetic operator with normal precedence (same tier as *//). There’s no overlap: “percent” is lexical (12.5% → value), “modulo” is the % operator.

Precedence & Associativity
Higher rows bind first; within a row, operators are left‑to‑right unless noted.
/// Precedence sketch (high → low). Use () to override.
1) ()                   /// grouping
2) ., (), [], ?.        /// member, call, index, optional chaining
3) **, //, ++, --       /// postfix square, postfix sqrt, post-inc/dec
4) **, ^^   (right)     /// exponentiation (right-assoc), show-work pow
5) +, -, not, !         /// unary plus/minus, logical not (alias !)
6) % (percent forms)    /// percent literals/ops (see Numbers & Money)
7) *, /, %, //, >>      /// mul, division, modulo, quotient, divmod
8) |>                   /// pipeline (left-to-right data flow)
9) +, -                 /// addition, subtraction
10) |, ||               /// string joining (see Strings)
11) ??                  /// null-coalescing
12) ==, !=, <, <=, >, >=, ===, !==, is, is not   /// comparisons
13) and, or             /// logical (aliases: && for and)

Notes
Exponentiation is right-associative: 2 ** 3 ** 2 is 2 ** (3 ** 2).


Postfix binds tighter than infix pow: (1+3)** squares 4 before other ops.


Optional chaining ?. short-circuits on nil and returns nil.


String joining |, || and percent forms are defined in their sections.


Arithmetic & Division
/// Division family
10 / 3      /// 3.333...  (always float)
10 // 3     /// 3          (quotient only, integer-like)
10 % 3      /// 1          (remainder)
q, r = 10 >> 3   /// divmod: q=3, r=1

/// Divmod as a value
res = 10 >> 3
res.q        /// 3
res.r        /// 1

Money rules
price = $10.75
price // 3      /// $3.58 (currency-aware quotient)
q, r = price >> 3   /// ($3.58, $0.01)
/// price / 3      /// ❌ MoneyDivisionError (use // or >>)
$10.75 % 3         /// $0.01 (remainder)

/ is forbidden for money; use // or >>. See Money & Currency for rounding model.


Exponentiation
5 ** 3   /// 125 (power)
5 ^^ 3   /// 125 (emits 5 × 5 × 5 when printed; same value)
2 ** 3 ** 2   /// 512 (right-assoc)

Use ** for computation. ^^ is a pedagogical variant that evaluates identically.


Postfix Math Shortcuts
Postfix applies to a primary (identifier, literal, or parenthesized expr) with no space.
x = 9
x**        /// 81   (square)
16//       /// 4    (sqrt)
(1+3)**    /// 16   (square of 4)

score = 10
old = score++   /// old=10, score=11
old = score--   /// old=11, score=10

balance = $50.00
before = balance++   /// before=$50.00, balance=$51.00 (major unit)

Rules
** and // postfix: any primary.


++/--: variables only; return the original value, then mutate.


No chaining of postfix increments: (x++)++ is invalid.


No spaces: value// (postfix) vs value // y (quotient).


Compound Assignment
score = 100
score += 50    /// 150
score -= 25    /// 125
score *= 2     /// 250
score **= 2    /// 62500

name = "Hello"
name += " World"   /// "Hello World"

bal = $100.00
bal += $25.00    /// $125.00
bal -= $10.00    /// $115.00
bal *= 2         /// $230.00
/// bal /= 2     /// ❌ MoneyDivisionError

Built-in Math Operations (method-style)
Math operations are methods on numbers/collections; there is no global sum(x).
nums = [1,2,3,4]
nums.sum      /// 10
nums.min      /// 1
nums.max      /// 4

n = 3.7
n.round       /// 4
n.floor       /// 3
( -5 ).abs    /// 5   /// negative literals require parens

Pipelines keep multi-step math readable:
value = -3.7
result = value |> .abs |> .floor   /// 3

Bitwise (method-style only)
Bitwise symbols are not used; call methods instead.
5.bit_and(3)   /// 1
5.bit_or(3)    /// 7
5.bit_xor(3)   /// 6
5.bit_not      /// ... two's complement NOT
5.bit_shl(2)   /// 20
8.bit_shr(1)   /// 4

Optional Chaining & Null-Coalescing
user?.profile?.name       /// nil-safe member access (returns nil if any step is nil)
name = user?.profile?.name ?? "(unknown)"   /// use fallback when left is nil

Rules
a?.b evaluates a.b only when a is non‑nil; otherwise yields nil.


x ?? y yields x when x is non‑nil, else y.


Pipeline Operator
Left-to-right data flow; each stage is method-style or namespaced call.
result = 16
       |> .//   /// 4
       |> .**   /// 16
       |> .round

slug = "  Hello World  " |> .trim |> .upper |> .slug   /// "hello-world"

Rules
x |> .op(args) ≡ x.op(args).


Mix with namespaces as needed: data |> M::normalize |> .sum.


Design Rationale (differences to note)
Postfix vs infix tokens: // and ** exist in both forms; spacing disambiguates (postfix has no space). This enables concise square/sqrt without adding new keywords.


Money safety: forbidding / on money prevents silent rounding drift; // and >> make rounding explicit and auditable.


Bitwise as methods: keeps |/|| available for string joining and improves readability/auditability of low-level ops.


Right-assoc pow: matches mathematical convention and many languages; explicit in spec to avoid parser ambiguity.


Quick Reference
Arithmetic: + - * / // % >> ** ^^


Postfix: x** (square), x// (sqrt), v++, v-- (return old, then mutate)


Compound assignment: += -= *= **= (money: no /=)


Math methods: arr.sum, arr.min, arr.max, n.round, n.floor, n.ceil, n.abs


Bitwise (methods): n.bit_and(x), bit_or, bit_xor, bit_not, bit_shl(k), bit_shr(k)


Null handling: a?.b (nil-safe), x ?? y (fallback)


Pipeline: val |> .op |> Module::op(args)


Precedence highlights: postfix > pow (right-assoc) > unary > mul/div/mod/quot/divmod > pipeline > add/sub > join > null‑coal > compare > logic


Cross-refs: Strings (|, ||), Numbers & Money (percent forms, rounding), Randomness (dice, pick).




10. Conditionals
Conditionals choose behavior based on boolean expressions. Goblin provides statement-style branching (if/elif/else, plus unless for readable negative logic) and an expression-style selector (judge). Design goals: readable intent, no fallthrough, and alignment with range syntax (.., ...) via between.
Keywords and Forms
if — run a block when the condition is true.


unless — run a block when the condition is false (preferred to if not … when it reads better).


elif — additional condition tested only if prior branches did not run.


else — fallback when no prior branch ran.


judge — expression that returns a value chosen by the first true guard.


between / !between — readable, typed range checks aligned with ../....


Block Conditionals (statement form)
/// Basic multi-branch
if score >= 90
    grade = "A"
    say "Excellent"
elif score >= 80
    grade = "B"
elif score >= 70
    grade = "C"
else
    grade = "F"
end

/// Negative logic reads clearer with unless
unless user.is_admin
    deny_access()
else
    grant_admin_access()
end

Rules
Indentation defines blocks; close with end.


Any number of elif branches; at most one else.


Conditions are boolean expressions; non-boolean values must be compared explicitly.


Use unless when the false case is the primary path (unless ready …).


judge — Expression-Style Selection
judge selects a value by testing guards top-to-bottom and returning the first matching arm. Chosen for clarity (mapping from condition → value) and to avoid fallthrough.
/// Multiline judge: first-match-wins, returns a value
label = judge
    score >= 90: "gold"
    score >= 75: "silver"
    score >= 60: "bronze"
    else: "none"
end

message = "Your tier: " || label

/// Inline judge for simple cases
status = judge: health > 0: "alive" :: else: "dead"
price  = base_cost * judge: is_premium: 0.8 :: else: 1.0

Rules
Evaluate guards in order; return the value of the first true guard.


else: is optional; if omitted and no guard matches, the result is nil.


Right-hand expressions are evaluated only for the chosen arm.


Usable anywhere a value is expected (assignment, arguments, string interpolation, etc.).


No fallthrough; exactly one arm produces the value.


between / !between — Range Checks
between is sugar for inclusive/exclusive range comparisons that mirror .. and ....
/// Inclusive range (a <= x <= b)
if temperature between 20..30
    say "Comfortable"
end

/// Exclusive end (a <= x < b)
if progress between 0...100
    say "In progress"
end

/// Negation
if score !between 0..100
    error "Invalid score"
end

Type Rules
Endpoints and tested value must be of the same, comparable type.


Supported: numbers, dates, datetimes, times, and money (same currency for both ends).


Semantics follow the range operator: .. includes the end; ... excludes it.


With judge
shipping = judge
    weight between 0..1: "overnight"
    weight between 1..5: "two_day"
    weight between 5..20: "standard"
    else: "freight"
end

Boundary Guidance
Make ranges non-overlapping when stacking multiple between arms.


Alternatively, use ordered comparisons (>=, <) when boundaries are easier to express.


Choosing Forms
Use block conditionals for actions and multi-statement flows. Use judge to select a value concisely. Prefer unless when the negative reads more clearly. Prefer between for range readability and correctness.
/// Block: perform steps
if user.is_new
    setup_defaults()
    send_welcome_email()
else
    load_preferences()
end

/// Judge: choose a value
welcome = judge
    user.is_new: "Welcome!"
    user.last_login > 30.days: "Welcome back — it's been a while."
    else: "Welcome back!"
end

Quick Reference
Block form
if cond ... elif cond ... else ... end


unless cond ... else ... end


judge (expression)
Multiline: judge \n cond: value \n ... \n else: value \n end


Inline: judge: cond: value :: cond: value :: else: value


First-match-wins; else optional (otherwise nil).


Ranges
x between a..b ⇔ a <= x <= b


x between a...b ⇔ a <= x < b


x !between range is the negation


Works with numbers, dates, datetimes, times, money (same currency)


Guidance
Blocks for side effects; judge for values.


Prefer unless for clear negative logic.


Make between ranges non-overlapping inside judge.




11. Loops
Loops execute a block repeatedly over collections, ranges, or while a condition holds. Goblin supplies a small, consistent set: for, repeat, while, inline filtering with where, unified striding with jump, and staged striding with until. Design intent: one loop syntax that works across numbers, collections, dates, and datetimes, with clear control keywords (skip, stop).
For
Iterate values (and optionally positions) from an iterable.
/// Array values
for weapon in ["sword", "bow", "staff"]
    say weapon
end

/// Array with index (0-based)
players = ["Alice", "Bob", "Charlie"]
for i, player in players
    say "#{i}: {player}"     /// 0: Alice, 1: Bob, 2: Charlie
end

/// Map iteration (insertion order)
stats = {health: 100, mana: 50, str: 25}
for key, val in stats
    say "{key}: {val}"
end

Rules
for value in collection iterates values; for i, value in array includes index (0-based).


for key, value in map iterates key/value pairs in insertion order.


Variable names are user-chosen; pick descriptive identifiers.


Ranges
Ranges are first-class iterables.
/// Numeric ranges
for n in 1..5
    say n                 /// 1,2,3,4,5 (inclusive)
end

for n in 1...5
    say n                 /// 1,2,3,4 (exclusive end)
end

for n in 5..1
    say n                 /// 5,4,3,2,1 (descending)
end

/// Date ranges: inclusive, day stride by default
for d in date("2025-08-01")..date("2025-08-03")
    say d                 /// 2025-08-01, 02, 03
end

/// Datetime ranges: exclusive, hour stride by default
start = datetime("2025-08-18 09:00", tz: "UTC")
finish = datetime("2025-08-18 12:00", tz: "UTC")
for t in start...finish
    say t                 /// 09:00, 10:00, 11:00 (UTC)
end

Rules
Numbers: .. inclusive end, ... exclusive end; descending supported.


Dates: must use .. (inclusive). Default jump = 1d.


Datetimes: must use ... (exclusive). Default jump = 1h.


jump — unified stride
jump specifies the stride and works uniformly for ranges and collections.
/// Numeric stride
for n in 0..10 jump 2
    say n                 /// 0,2,4,6,8,10
end

/// Descending with stride
for n in 10..0 jump 3
    say n                 /// 10,7,4,1
end

/// Weekly dates
for d in date("2025-01-01")..date("2025-01-31") jump 7d
    say d                 /// Every 7th day in range
end

/// Every 30 minutes
for t in start...finish jump 30m
    say t
end

/// Collections: take every Nth element (starting at index 0)
xs = ["A","B","C","D","E","F","G"]
for x in xs jump 3
    say x                 /// A, D, G
end

/// With index
for i, x in xs jump 2
    say "#{i}: {x}"        /// 0:A, 2:C, 4:E, 6:G
end

/// To offset the start, slice first
for x in xs[1...] jump 2
    say x                 /// B, D, F
end

Rules
Direction comes from range endpoints; jump is a positive magnitude.


Start value is always produced; inclusive-end is yielded only if a stride lands on it.


Arrays: take elements 0, N, 2N, … (post-slice if you need a different start).


Maps: stride applies after establishing insertion order.


Allowed jump types: integer (numbers/collections), Xd (dates), duration like 30s/5m/2h/1d (datetimes).


Staged stride with until
until switches the stride when a condition becomes true; if no final stage is given, stride defaults to jump 1.
/// Two stages: jump 3, then (implicit) jump 1
for i in 0..12
    jump 3 until i > 6
    say i                 /// 0,3,6,7,8,9,10,11,12
end

/// Multiple stages
for i in 3..20
    jump 3 until i > 9    /// 3,6,9
    jump 2 until i > 15   /// 11,13,15
    jump 1                /// 16,17,18,19,20
    say i
end

/// Conditional stride policy with judge
for task in task_queue
    jump judge
        system.load < 30%: 10
        system.load < 70%: 5
        else: 2
    end until task.deadline < 5.minutes
    jump 1
    task.execute()
end

Rules
until <cond> changes stride selection; it does not terminate the loop.


Conditions are evaluated at each iteration boundary; when tripped, the loop continues with the next stage.


Final stage defaults to jump 1 if omitted.


for … where — inline filtering
Filter items directly in the loop header. In combined forms, filtering occurs before striding.
scores = [45, 92, 78, 96, 83, 91]
for s in scores where s >= 90
    say s                 /// 92, 96, 91
end

/// With index (index refers to original array position)
for i, s in scores where s >= 90
    say "#{i}: {s}"        /// 1:92, 3:96, 5:91
end

/// Filter then stride: "every 2nd qualifying item"
for s in scores where s >= 70 jump 2
    say s                 /// 45 filtered out; take 78, 91 ... among the kept set
end

/// Maps
grades = {alice: 95, bob: 78, charlie: 92, diana: 65}
for name, g in grades where g > 80
    say "{name}: {g}"      /// alice:95, charlie:92
end

Rules
where <predicate> is applied before any jump stride.


With for i, v in array where …, i is the index in the original array.


repeat — fixed repetitions
Run a block a fixed number of times; no loop variable is created.
repeat 3
    say "Loading..."
end

enemy_count = 5
repeat enemy_count
    battlefield.spawn_enemy()
end

/// Early exit
repeat 10
    attempt
        job.run()
        stop                 /// exit this repeat
    rescue TransientError
        wait(1.second)
    end
end

Rules
Count is an expression evaluated once when the loop starts.


Use stop to exit early; skip continues to the next iteration.


while — condition-controlled loops
Execute while a condition is true.
health = 100
while health > 0
    health = health - 10
    say health
end

/// Game loop
running = true
while running
    cmd = player.get_input()
    if cmd == "quit"
        running = false
    else
        game.process_turn(cmd)
    end
end

Rules
Condition is checked before each iteration.


Ensure loop variables change so the condition eventually becomes false.


skip and stop are valid inside while.


Loop control: skip / stop
Clear, explicit control inside any loop.
for n in 1..10
    if n % 2 == 0
        skip                 /// skip even
    end
    if n > 7
        stop                 /// exit loop
    end
    say n                    /// 1,3,5,7
end

Rules
skip → skip the current iteration.


stop → terminate the innermost loop.


Time windows crossing midnight
Anchor times to dates to express overnight windows.
/// 23:30 today → 02:30 tomorrow, every 30 minutes
start = time("23:30").wrap(0h)      /// anchor to today
finish = time("02:30").wrap(24h)    /// anchor to next day (+24h)
for t in start...finish jump 30m
    process_window(t)
end

Rules
Plain time("23:30")...time("02:30") is invalid; anchor with .wrap(offset) to cross midnight.


.wrap(0h) anchors to "today", .wrap(24h) anchors to "tomorrow"; result is a datetime usable in a ... range.


Quick Reference
for
for v in collection … end — values


for i, v in array … end — index + value (0-based)


for k, v in map … end — insertion order


ranges
Numbers: a..b inclusive, a...b exclusive; descending allowed


Dates: start..end (inclusive), default jump = 1d


Datetimes: start...end (exclusive), default jump = 1h


jump
Works on ranges and collections


Integer for numbers/collections; Xd for dates; durations for datetimes


Start is always produced; inclusive end only if the stride lands on it


until
jump N until cond switches stride when cond becomes true


Omitted final stage defaults to jump 1


Does not terminate the loop


where
for … where predicate filters before striding


Array index (if present) refers to original positions


repeat
repeat N … end — fixed count, no loop variable; stop/skip allowed


while
while cond … end — evaluate cond each iteration


control
skip — next iteration


stop — exit loop


time windows
Use time(...).wrap(offset) to anchor overnight endpoints, then start...finish with a duration jump



12. Operations
Operations are reusable units of logic that transform data or perform actions. In Goblin, operations are defined with op and called in one of two forms only. This section defines the rules, shows canonical examples, and lists constraints required to implement and use operations correctly.
Defining Operations
Operations are declared with op. Use block form for multiple statements, or expression form for one-liners. The last expression returns implicitly unless return is used.
/// Block form
op double(x)
    x * 2            /// implicit return
end

op first_positive(nums)
    for n in nums
        if n > 0
            return n  /// explicit early return
        end
    end
    nil               /// explicit final return
end

/// Expression form (one-liner)
op add(a, b) = a + b

Rules
op name(params) ... end or op name(params) = expr.


Last expression is the return value unless return is used.


return exits the operation immediately.


Calling Operations (Only Two Forms)
Goblin forbids op(value) style calls. Only the following are valid.
1) Method-Style (universal)
Every operation can be called as a method on its first argument.
[1,2,3].sum         /// built-in on arrays
3.7.round           /// built-in on numbers
10.double           /// user-defined op above
"  text  ".trim    /// built-in text op

2) Prefix-Style (built-in text ops only)
A small set of core text operations may be called in prefix form for readability.
trim "  spaced  "      /// → "spaced"
upper "hello"          /// → "HELLO"
slug  "Hello World"    /// → "hello-world"

/// Equivalent chaining
clean = trim upper slug "  Hello World  "
/// Same as: "  Hello World  ".trim.upper.slug

Rules
Valid: value.operation (any op), and operation value (built-in text ops only).


Invalid: operation(value) general function-call style.


Parameters
Required, defaulted, named, and variadic parameters are supported.
/// Defaults
op greet(name, salutation="Hello")
    "{salutation}, {name}!"
end

/// Named + positional
user1 = greet("Alice")                 /// uses default
user2 = greet("Bob", "Hi")            /// overrides default
user3 = greet(name="Cara", salutation="Yo")

/// Variadic
op sum_all(...nums)
    total = 0
    for n in nums
        total = total + n
    end
    total
end

sum_all(1,2,3)       /// 6
sum_all(5,10,15,20)  /// 50

Rules
Defaults bind in the parameter list.


Named arguments may be mixed with positionals.


Variadic collects remaining args into an array-like parameter.


Unary vs Multi-Parameter (Auto-Methodization)
Auto-methodization (calling with value.operation(...)) is only guaranteed for unary operations (one explicit parameter). For multi-parameter ops, design the first parameter as the receiver or use namespacing.
/// Unary → auto-methodizes
op square(x) = x * x
result = 9.square     /// works

/// Multi-parameter with explicit receiver first
op add(self, other) = self + other
5.add(3)              /// 8

/// Alternative: invoke via module namespace (see below)

Rules
Unary ops: method-style guaranteed.


Multi-parameter ops: first param is the receiver when using method-style.


If that is not desirable, expose via a module/glam and call with ::.


Zero-Arg Operations (Property-Like)
Zero-arg operations can be called with or without ().
op len(items) = items.count

n1 = items.len       /// preferred, reads as a property
n2 = items.len()     /// also valid

Rule
Zero-arg calls may omit parentheses.


Pipelines
Pipelines apply operations left-to-right.
result = "  Hello World  "
       |> .trim            /// "Hello World"
       |> .upper           /// "HELLO WORLD"
       |> .slug            /// "hello-world"

processed = raw
          |> .parse_csv
          |> .validate_rows
          |> .filter_active
          |> .calculate_totals

Rules
Pipeline segment must be .operation (method-style) or Module::op(...).


Each stage receives the previous stage's value as first argument.


Modules and Glams
External operations are called via namespace separator ::. . is reserved for methods on values.
import "./helpers" as H
use text_utils@1.2 as T

slugged = H::slugify("Product Name")

title   = T::title_case(" some title ")
report  = records |> T::normalize |> T::summarize

Rules
Module::op(args) and Glam::op(args) for external calls.


. never calls into modules/glams; it targets the value on the left only.


Module/glam ops do not auto-methodize. Wrap if chaining is desired.


Wrapping External Ops for Chaining
use text_utils@1.2 as T

op title_case(txt) = T::title_case(txt)

result = "  hello  ".trim.title_case

Quick Reference
Define


op name(params) ... end (block)


op name(params) = expr (one-liner)


Implicit return = last expression; return exits early


Call (only two forms)


Method-style (universal): value.operation(args)


Prefix-style (text-only): operation value


Parameters


Defaults: op f(a=1)


Named args: f(a=2)


Variadic: op f(...xs)


Auto-methodization


Guaranteed for unary ops


Multi-parameter: receiver is the first param, or call via Module::op


Pipelines


x |> .op |> Module::op(args)


Namespaces


Modules/Glams: Name::op(args)


. is never used for namespaces






# 13. Randomness

Randomness is central to Goblin for games, simulations, and any situation where unpredictability matters. The language provides two main primitives — `pick` and `roll` — designed to be both concise and expressive. These cover flat distributions, bell curves, digit codes, weighted selection, and analysis utilities.

## The Concept

Goblin random operations fall into two families:
- **pick** — flat distributions, digit shorthand, and collection/range selection
- **roll** — dice notation with natural bell curves and modifiers

Together, they cover both system testing use-cases (IDs, codes, data sampling) and gaming patterns (damage, stats, loot).

## Pick Operations — “Pick What From What”

The `pick` operator follows a simple pattern: *pick (what) from (what)*. If the first part is omitted, it defaults to 1.

### Syntax

```goblin
pick from collection              /// single random element
pick n from collection            /// n elements, default !dups for collections
pick n from collection with dups  /// repeats allowed across results
pick n from collection !dups      /// no repeats across results (explicit)

pick from start..end              /// one number from range
pick n from start..end            /// n numbers from range, default with dups

pick x_y                          /// x random numbers, each y digits
pick 1_6                          /// one 6-digit number
pick 5_4                          /// five 4-digit numbers
pick 5_4 !dups                    /// distinct 4-digit numbers only

pick 1_5 unique                   /// one 5-digit number, digits distinct inside
pick 3_4 unique                   /// three 4-digit numbers, digits distinct inside each

pick 2_3 from 400..499 !dups      /// constrained by range, distinct results

pick 5_4 !dups join with ", "     /// join results into a string
```

### Duplication Rules

- **Across results**:
  - `with dups` → sampling with replacement
  - `!dups` → sampling without replacement

- **Within a result**:
  - `unique` → enforces digit-level uniqueness for shorthand `x_y`

```goblin
/// Across results
pick 5 from names          /// 5 distinct names (default !dups)
pick 5 from names with dups /// duplicates allowed

/// Within result (digit uniqueness)
pick 1_4 unique   /// e.g. 4832, never 1122
```

**Defaults:**
- Collections → `!dups`
- Numeric spaces (ranges, digit shorthand) → `with dups`

### Practical Examples

```goblin
/// Codes and IDs
verification = pick 1_6           /// e.g. 732901
pin = pick 1_4 unique             /// e.g. 4832
user_ids = pick 100_8             /// 100 eight-digit IDs

/// Ranges
coords = pick 3 from -100..100    /// 3 random coordinates
test_scores = pick 10 from 60..100

/// Collections
weapons = ["sword", "bow", "staff"]
random_weapon = pick from weapons
starting_loadout = pick 2 from weapons
```

### Why Underscore Syntax?

The shorthand `x_y` mirrors dice notation for readability:
- `2d6` = roll 2 six-sided dice
- `2_6` = pick 2 six-digit numbers

Self-documenting, concise, and clear compared to verbose ranges.

## Roll Operations — Dice Notation and Bell Curves

Dice notation `XdY+Z` is a universal shorthand familiar to most players. Goblin adopts it directly for natural probability distributions.

### Syntax

```goblin
roll 1d6              /// 1–6, flat distribution
roll 2d6              /// 2–12, bell curve centered at 7
roll 3d6              /// 3–18, bell curve centered ~10–11
roll 4d6+2            /// 6–26, with modifier

roll 1d100            /// percentile
roll 1d999999         /// arbitrary die size
roll 1d200-100        /// shifted to -99..100
```

### Why Multiple Dice?

One die = flat distribution. Multiple dice = bell curve.

```goblin
single = roll 1d6     /// each result equal chance
double = roll 2d6     /// 7 is most likely
triple = roll 3d6     /// tight cluster around 10–11
```

This matches real-world expectations: most values cluster near the middle.

### Roll Detail

`roll_detail` exposes dice internals for debugging or gameplay logs:

```goblin
result = roll_detail 4d8+5
say result.dice   /// [3, 7, 2, 8]
say result.sum    /// 20
say result.total  /// 25
```

## Weighted Selection

Not all outcomes should be equally likely. Goblin supports weighted draws with `sample_weighted`.

```goblin
loot = [
  ("common", 50),
  ("rare", 10),
  ("epic", 3),
  ("legendary", 1)
]

pick_loot = sample_weighted loot
```

Or with maps:

```goblin
encounters = {goblin: 40, orc: 25, troll: 8, dragon: 2}
next = sample_weighted encounters
```

Rules:
- Weights ≥ 0, at least one > 0
- Ratios matter, not absolute values

## Analysis Utilities

Goblin provides simple frequency analysis tools:

```goblin
freq ["sword", "potion", "potion"]   /// {sword: 1, potion: 2}
mode [1, 2, 2, 3]                    /// [2]
```

- `freq` counts occurrences
- `mode` finds most common value(s)

## Deterministic Randomness

All random operations respect the global RNG seed for reproducibility.

```bash
goblin run game.gob --seed 1337
```

Use cases: testing, debugging, speedruns, demos.

## Errors

- **EmptyPickError** — picking from empty collection
- **PickCountError** — asking for more than available (with !dups)
- **PickIndexError** — invalid index
- **PickTypeError** — wrong type for pick
- **DiceBoundsError** — invalid dice sizes/counts
- **WeightError** — invalid or empty weights

## Quick Reference

**pick**
- `pick from collection` → one element
- `pick n from collection` → n elements, default !dups
- `pick from range` → one number
- `pick x_y` → digit shorthand (x numbers, y digits)
- Modifiers: `with dups`, `!dups`, `unique`
- Joins: `join with "sep"`

**roll**
- `roll XdY+Z` → dice notation
- `roll_detail XdY+Z` → detailed breakdown

**analysis**
- `freq` → frequency counts
- `mode` → most common element(s)

**weighted**
- `sample_weighted [(item, weight), ...]`
- `sample_weighted {item: weight, ...}`

**defaults**
- Collections → `!dups`
- Ranges/digits → `with dups`

**errors**
- EmptyPickError, PickCountError, PickIndexError, PickTypeError
- DiceBoundsError, WeightError



# 14. Collections

Collections group values for storage, access, and transformation. Goblin provides two primary kinds:

- **Arrays** — ordered, index-based sequences (0-based; negative indices from the end).
- **Maps** — key/value dictionaries with string/identifier keys.

Operations are either **non-destructive** (return new collections) or **destructive** (mutate the original). Randomized selection (`pick`, `reap`) and replacement (`usurp`, `replace`) have fixed semantics defined below.

## Arrays

Arrays preserve order and allow mixed element types.

```goblin
inventory = ["sword", "potion", "gold"]    /// Simple
mixed     = [1, true, "note"]                /// Mixed types
empty     = []                                /// Empty
```

### Indexing and Length

Zero-based indexing; negative indices count from the end.

```goblin
weapons = ["sword", "bow", "staff", "dagger"]
first   = weapons[0]     /// "sword"
last    = weapons[-1]    /// "dagger"
count   = weapons.len    /// 4
```

### Slicing

`[start:end]` with exclusive `end`. Omitted bounds default to start/end of array.

```goblin
nums = [10,20,30,40,50,60]
first_three = nums[0:3]  /// [10,20,30]
from_second = nums[2:]   /// [30,40,50,60]
all_copy    = nums[:]    /// shallow copy
```

### Adding and Inserting (Destructive)

```goblin
arr = ["sword", "potion"]
add "gold" to arr                   /// ["sword","potion","gold"] (append)
insert "bow" at 1 into arr          /// ["sword","bow","potion","gold"]
```

### Non-Destructive Derivations

```goblin
scores = [800, 1200, 600, 1500, 900]
sorted   = sort scores              /// [600,800,900,1200,1500]
shuffled = shuffle scores           /// random permutation
min_v    = scores.min               /// 600
max_v    = scores.max               /// 1500
avg_v    = scores.avg               /// 1000.0
sum_v    = scores.sum               /// 5000
```

### Mapping (Non-Destructive)

`map <op>, <array>` applies a unary operation to each element.

```goblin
names   = ["alice","bob","charlie"]
uppered = map upper, names          /// ["ALICE","BOB","CHARLIE"]
```

## Pick — Random Selection (Non-Destructive)

Sentence form follows **“pick what from what”**. Source collections are never modified.

**Syntax**

- `pick from collection` → 1 random element.
- `pick n from collection` → `n` elements (default **!dups** for finite collections).
- `pick n from collection with dups` → sampling **with** replacement.
- `pick n from collection !dups` → sampling **without** replacement.
- `pick from a..b` / `pick n from a..b` → numbers in inclusive range (numeric spaces default **with dups**).
- `pick x_y` → `x` random integers, each `y` digits long (**default with dups** across results).
- `pick x_y from a..b` → constrain by range intersection.
- `pick … join with "sep"` → join results as string.

**Deterministic forms**

```goblin
weapons = ["sword","bow","staff","dagger"]
first  = pick first from weapons   /// "sword"
last   = pick last from weapons    /// "dagger"
third  = pick at 2 from weapons    /// "staff"
```

### Digit Shorthand and `unique`

**Concept.** `unique` enforces **digit-level uniqueness within each generated number** for the digit shorthand `x_y`.

- Without `unique`: digits may repeat inside a number (e.g., `7721`).
- With `unique`: all digits inside the number are distinct (e.g., `4832`).
- It does **not** deduplicate across results; combine with `!dups` for that.

```goblin
pick 1_4            /// e.g., 7721    (digits may repeat)
pick 1_4 unique     /// e.g., 4832    (digits all distinct)
pick 3_4 unique     /// e.g., [1234, 5678, 9012]  /// per-number uniqueness
pick 3_4 unique !dups  /// three distinct 4-digit numbers; each has unique digits
```

**Defaults**

- Finite collections: default `!dups` (no repeats in the returned set).
- Numeric spaces (ranges, `x_y`): default `with dups` (repeats allowed across results).

**Errors**: `EmptyPickError | PickCountError | PickIndexError | PickTypeError`.

## Reap — Random Selection (Destructive)

`reap` mirrors `pick` but **removes** selected elements from the source collection.

**Syntax**

- `reap from collection` → remove & return 1 element.
- `reap n from collection` → remove & return `n` elements (default **!dups** for finite collections).
- `reap n from collection with dups` → may return repeats; source shrinks once per unique removal.
- `reap n from collection !dups` → remove & return `n` distinct elements.
- Deterministic: `reap first`, `reap last`, `reap at i`.
- `reap x_y` and `reap x_y unique` generate numbers (no source to shrink).

```goblin
deck = ["ace","king","queen","jack","ten"]
drawn = reap from deck           /// e.g., "queen" (deck now lacks it)
hand  = reap 2 from deck !dups   /// removes two distinct cards
```

**Errors**: `ReapEmptyError | ReapCountError | ReapIndexError | ReapTypeError`.

## Replacement

### `usurp` (Destructive, with history)

Replaces an element and returns a tuple `(old, new)`.

```goblin
weapons = ["iron_sword","steel_axe","bronze_spear"]
(old_w, new_w) = usurp at 0 in weapons with "legendary_sword"
/// weapons → ["legendary_sword","steel_axe","bronze_spear"]
```

- Also supports `usurp from <array> with <value>` to replace a random element.
- Errors: `UsurpIndexError | UsurpCountError | UsurpArityError | UsurpTypeError`.

### `replace` (Destructive, simple overwrite)

Overwrites an element; does **not** return the old value.

```goblin
inv = ["sword","potion","gold"]
replace at 1 in inv with "super_potion"  /// inv → ["sword","super_potion","gold"]
```

- Errors: `ReplaceIndexError | ReplaceTypeError`.

## Maps

Maps organize values by key; access is via bracket notation only.

```goblin
player = {name: "Alice", level: 12, class: "warrior", "special-ability": "fireball"}
name   = player["name"]              /// "Alice"
level  = player["level"]             /// 12
special = player["special-ability"]  /// works with hyphens
```

### Keys and Values

```goblin
prices = {sword: $50, shield: $30, potion: $10}
ks = prices.keys                 /// ["sword","shield","potion"]
vs = prices.values               /// [$50,$30,$10]
cheapest = prices.values.min     /// $10
```

### Updates

```goblin
stats = {strength: 10, dexterity: 12}
stats["intelligence"] = 8       /// add
stats["strength"] = 15          /// update
```

## Quick Reference

**Arrays**
- Indexing: `a[i]`, negative indices from end; length: `a.len`.
- Slicing: `a[s:e]` (end exclusive), `a[:e]`, `a[s:]`, `a[:]`.
- Destructive add/insert: `add v to a`, `insert v at i into a`.
- Non-destructive: `sort a`, `shuffle a`, `a.min|max|avg|sum`, `map op, a`.

**Pick (non-destructive)**
- `pick (n) from collection` (collections default `!dups`).
- `pick (n) from a..b` (numeric spaces default `with dups`).
- `pick x_y` (digit shorthand); `unique` ⇒ digits inside each number are all distinct.
- Deterministic: `pick first|last|at i from a`.

**Reap (destructive)**
- Same forms as `pick` but removes from source; deterministic `reap first|last|at i`.

**Replacement**
- `usurp at i in a with v` → returns `(old,new)`; random form `usurp from a with v`.
- `replace at i in a with v` → overwrite only.

**Maps**
- Access: `m["key"]`; enumerate: `m.keys`, `m.values`.
- Update: `m["key"] = value`.

**Errors**
- Pick: `EmptyPickError | PickCountError | PickIndexError | PickTypeError`.
- Reap: `ReapEmptyError | ReapCountError | ReapIndexError | ReapTypeError`.
- Usurp: `UsurpIndexError | UsurpCountError | UsurpArityError | UsurpTypeError`.
- Replace: `ReplaceIndexError | ReplaceTypeError`.

15. Key‑Value Binding and Templates
Templates and key–value binding let you construct structured maps with defaults, positional filling, and named overrides. Binding uses the :: operator and always produces a map at application time.
Concepts
Map literal: {key: value, ...} — a concrete container, evaluated once.


Binding: key: expr :: key2: expr2 — a construction pipeline; by itself it also yields a map.


Template: @name = field1: default1 :: field2: default2 ... — a reusable binding that, when applied, returns a map with defaults overridden by supplied values.


Application: @name: arg1 :: arg2 (positional) or @name: field2: value (named) — fills template fields left→right unless keyed.


Skip placeholders: :: (implicit skip) and :: nc (explicit “no change” for readability in long skips).


Rule of thumb: Use maps for fixed data; use templates (with ::) to generate consistent maps with defaults and variations.
Maps vs Binding
Map literals (data as-is)
player = {name: "Alice", level: 12, class: "warrior"}  /// Exact data; no defaults, no filling
say player["level"]  /// 12

Binding (construct then yield a map)
card = title: "Two of Cups" :: price: $0.99 :: qty: 4
/// Equivalent resulting map to a literal, but binding syntax composes with templates

Templates
Define
@card = title: "{title}" :: price: $0.99 :: qty: 1  /// fields + defaults

Apply (positional, named, mixed)
c1 = @card: title: "Ace of Cups"              /// {title:"Ace of Cups", price:$0.99, qty:1}
c2 = @card: title: "Two" :: price: $1.25     /// {title:"Two", price:$1.25, qty:1}
c3 = @card: title: "Three" :: :: 2           /// skip price, set qty -> {title:"Three", price:$0.99, qty:2}

@item = name: "{name}" :: price: $5 :: category: "general"
weapon = @item: name: "Sword" :: category: "weapon"  /// named override; price stays $5

Skips: :: vs :: nc
@four = a: 1 :: b: 2 :: c: 3 :: d: 4
x = @four: 10 :: :: 30            /// set a,c; b stays 2
y = @four: 10 :: nc :: 30         /// same result; more readable for longer runs
z = @four: 10 :: nc :: nc :: 40   /// set a,d; b,c unchanged

Guideline: For ≤2 consecutive skips, :: is fine. For 3+, prefer :: nc for clarity.
Interpolation
String placeholders inside defaults or supplied values resolve at application time from the current scope.
player_name = "Alice"; player_level = 15
@badge = label: "Lv {player_level} {player_name}" :: code: "{player_level*2}"
made = @badge
/// {label: "Lv 15 Alice", code: "30"}

Rule: Interpolation occurs only inside strings. Use variables directly for non-string fields.
Application Semantics
Field order is the template declaration order.


Positional fill assigns left→right.


Skips (:: / :: nc) advance position without change.


Named overrides set specific fields irrespective of position; they win over positional.


Result is always a map. Templates have no runtime identity beyond their output.


Examples
@weapon = name: "{name}" :: damage: 10 :: speed: "medium" :: price: $50

magic = @weapon: name: "Flamebrand" :: damage: 25 :: price: $200
fast  = @weapon: name: "Dagger" :: :: speed: "fast"        /// price default kept

/// Mixed: positionals then named override
mix   = @weapon: "Shortsword" :: :: speed: "fast"

Blocks (Contexted Generation)
Use a block to set contextual variables and generate many items with loops; optional keyed overrides adjust specific generated members.
@card = name: "{suit} {rank}" :: price: $0.99 :: rarity: "common"

suit: "Hearts"
    for rank in ["Ace", "King", "Queen", "Jack"]
        "{rank}"
    end

    "Ace"  :: rarity: "rare"         /// keyed override by loop value
    "King" :: :: rarity: "legendary"  /// skip price; set rarity
end

Nested Structures
Templates may contain maps/arrays; interpolation works inside nested strings.
@product =
    title: "{title}" ::
    price: $0.99 ::
    meta: {tags: ["new","featured"], inventory: {count: 0, warehouse: "A"}} ::
    variants: [
        {size: "S", sku: "{title}-S"},
        {size: "M", sku: "{title}-M"}
    ]

p = @product: title: "Magic Sword"

Collections × Templates
Templates compose with collection ops for generation and transformation.
@weapon = name: "{mat} {kind}" :: damage: 10 :: price: $50
mats = ["Iron","Steel","Mithril"]; kinds = ["Sword","Axe"]
all = []
for m in mats
    for k in kinds
        add (@weapon: mat: m :: kind: k) to all
    end
end

base = [
    {name: "Health Potion", value: 25},
    {name: "Mana Potion", value: 15}
]
@shop = name: "{n}" :: price: "{v*2}" :: stock: 10
op mk(item)
    @shop: n: item["name"] :: v: item["value"]
end
inv = map mk, base

Rules & Constraints
Identifiers on the left of : in bindings/templates. Quoted keys are invalid in binding; use map literals if you need arbitrary keys.


Interpolation scope: current lexical scope; missing names raise InterpolationError.


nc: only valid immediately after :: as a template skip placeholder.


Precedence: named overrides > positional; later bindings override earlier ones.


Outcome type: application returns an immutable map value (subsequent edits require normal map updates).


Errors
SyntaxError — malformed binding/template syntax.


IdentifierError — non-identifier key in binding side.


InterpolationError — unresolved {name} during application.


FieldMismatchError — named key not present in template.


TemplateError — application arity/ordering conflicts.


ParseOnlyError — use of :: nc outside template context.


ScopeError | NameError — unknown variables at application time.


Quick Reference
Define template: @T = f1: d1 :: f2: d2 :: ...


Apply (positional): @T: v1 :: v2


Apply (named): @T: f2: v2


Skip slots: :: (short), :: nc (readable for long skips)


Interpolation: only inside strings, from current scope


Blocks: var: value ... end + for ... end; overrides by generated key


Result: always a map; templates are blueprints, not runtime objects


16. Objects & Classes
Objects package data + behavior. Classes declare the fields an object has and the operations that act on those fields. Goblin uses template-style syntax for both definition and instantiation to stay consistent with binding elsewhere; there is no function‑style constructor.
Definition (Template-Style Only)
/// Class definition with template-style field list
class Player = name: "{name}" :: health: 100 :: level: 1
    op take_damage(amount)
        health = health - amount
        if health <= 0
            say "{name} has fallen!"
        end
    end

    op level_up()
        level = level + 1
        health = 100
        say "{name} reached level {level}!"
    end
end

Reasoning: One uniform, declarative form avoids constructor overloading and keeps Goblin’s call rules consistent (no Name(args) anywhere in the language).
Rules
class Name = field: default :: ... defines a class and its defaulted/required fields.


Fields are in scope inside ops; refer to them directly (no receiver needed).


Use op name(params) ... end or op name(params) = expr inside the class.


Instantiation (Template Binding Only)
class Pet = name: "{name}" :: age: 0 :: species: "dog"
    op speak() = "I'm {name}, a {species}."
end

fluffy = Pet: name: "Fluffy" :: age: 3 :: species: "cat"
buddy  = Pet: name: "Buddy" :: age: 2          /// species uses default
max    = Pet: name: "Max" :: :: species: "cat" /// skip age (keeps default)

Positional fill follows field order; :: can skip positions; named overrides may appear anywhere after the colon.
Rules
Instantiate with Name: values... (positional and/or named via field: value).


Unknown/missing required fields → TypeError.


No Name(args) or Name.new(...).


Field Kinds & Types
Interpolated string fields
class Article = title: "{title}" :: author: "{author}" :: body: "{body}"
    op preview() = "'{title}' by {author}: {body[0:80]}..."
end

Interpolation indicates the field expects a string at instantiation.
Direct value fields
class Settings = max_players: 4 :: team_mode: false :: fee: $5 :: difficulty: "normal"

Types are inferred from defaults; mismatches at creation raise TypeError (money mismatches raise CurrencyError).
Access Control
Public (default)
Public fields are part of the object’s interface. Goblin auto-generates accessors.
class User = name: "{name}" :: active: true
end

u = User: name: "Ada"
say u.name()      /// getter (parens optional)
u.set_active(false)  /// setter

Auto accessors: field() and set_field(value).
Private (#field)
Private fields are visible only inside the class; no accessors are generated.
class Bank = owner: "{owner}" :: #balance: $0 :: #pin: "{pin}"
    op deposit(amount) = #balance = #balance + amount
    op withdraw(amount, entered)
        if entered != #pin
            error "Invalid PIN"
        end
        if #balance >= amount
            #balance = #balance - amount
        else
            error "Insufficient funds"
        end
    end
    op balance(entered)
        if entered == #pin; #balance; else; error "Invalid PIN"; end
    end
end

Rules
Private fields: #name. External access → PermissionError.


Public fields participate in equality; private fields do not.


Operations in Classes
Define behavior with op. Use expression form for one‑liners; block form for multiple statements.
class Rectangle = width: 0 :: height: 0
    op area() = width * height
    op perimeter() = 2 * (width + height)
    op is_square() = width == height
end

r = Rectangle: width: 10 :: height: 5
say r.area()        /// 50
say r.is_square     /// zero-arg ops: parens optional

Custom vs auto accessors: a user-defined op with the same name as an auto accessor overrides it (for validation/formatting).
class Temperature = fahrenheit: 32
    op fahrenheit()
        "{fahrenheit}°F ({celsius()}°C)"
    end
    op set_fahrenheit(t)
        if t < -459; error "Below absolute zero"; end
        fahrenheit = t
    end
    op celsius() = (fahrenheit - 32) * 5 // 9
end

self: Inside class ops, self refers to the current object (useful when returning the object for chaining). Fields remain directly addressable without self.
class Text = value: ""
    op set(txt)
        value = txt
        self            /// enable chaining
    end
    op upper()
        value = value.upper
        self
    end
    op get() = value
end

result = (Text: value: "hello").upper().get()  /// "HELLO"

Money & Percent Inside Classes
class Product = name: "{name}" :: base_price: $0 :: tax: 8.5% :: discount: 0%
    op net()  = base_price - (discount of base_price)
    op total() = net() + (tax of net())
end

p = Product: name: "Widget" :: base_price: $25
say p.total()    /// $27.13

Objects in Collections
class Item = name: "{name}" :: price: $0 :: category: "{category}"
    op discount(rate) = price = price - (rate of price)
end

inv = [
    Item: name: "Sword"  :: price: $50 :: category: "weapon",
    Item: name: "Potion" :: price: $10 :: category: "consumable",
    Item: name: "Shield" :: price: $30 :: category: "armor"
]

for it in inv where it.category == "weapon"
    it.discount(10%)
end

Equality (Structural)
class Point = x: 0 :: y: 0
end

p1 = Point: x: 3 :: y: 4
p2 = Point: x: 3 :: y: 4
p3 = Point: x: 5 :: y: 6

p1 == p2   /// true (public fields equal)
p1 != p3   /// true

Rules
==/!= compare public field values.


Private fields are ignored.


Composition (No Inheritance)
Goblin omits class inheritance. Prefer composition to share behavior/data.
class Engine = horsepower: 150
    op start() = "Engine started"
end

class Car = make: "{make}" :: model: "{model}" :: engine: Engine :: running: false
    op start()
        if not running
            engine.start()
            running = true
            "Car started"
        else
            "Already running"
        end
    end
end

Reasoning: Composition keeps dependencies explicit and avoids inheritance ambiguities (constructor order, method resolution). It also aligns with Goblin’s preference for small, composable units.
Interop Notes
Objects can be used in pipelines via method-style ops: obj |> .op.


Module/glam ops (X::op(...)) can consume/return objects; they do not auto‑methodize.


Morphing (see Special Forms) uses public accessors only; private fields remain sealed.


Quick Reference
Define
class Name = field: default :: ... (template-style only)


Inside: op f(...) ... end or op f(...) = expr


Instantiate
Name: v1 :: v2 (positional), Name: field: val :: ... (named), :: to skip


Errors: TypeError (missing/invalid), CurrencyError (money), SyntaxError (function-style), PermissionError (private access)


Access
Auto accessors: obj.field(), obj.set_field(val)


Custom ops override accessors of the same name


Zero‑arg calls: parens optional (obj.op)


Privacy
Public by default; private via #field (no external accessors; excluded from equality)


Equality
Structural over public fields: ==, !=


Design constraints
No function-style constructors or inheritance


Use composition; return self from mutating ops to chain if desired


Method-style is the only call style for object ops; :: for namespaces


17. Enums
Enums define a closed set of named options. They are first‑class types with strict equality and no implicit conversion to numbers or strings. Goblin supports symbolic enums (no backing value) and backed enums (int or string) for external interoperability.
Defining Enums
/// Symbolic enums (no backing value)
enum Status
    Pending
    Paid
    Shipped
end

enum Direction
    North
    South
    East
    West
end

/// Backed enums (for DB/API integration)
/// Int‑backed
enum Priority as int
    Low = 1
    Medium = 2
    High = 3
end

/// String‑backed
enum Suit as string
    Clubs = "C"
    Diamonds = "D"
    Hearts = "H"
    Spades = "S"
end

Rules
enum Name ... end; variants are declared one per line in declaration order.


Backing kinds supported: int, string only.


Variant identifiers are case‑sensitive; duplicate names are invalid.


In backed enums, each variant must have a unique backing value.


Enums are closed: variants cannot be added at runtime.


Creating & Using Enum Values
order = Status.Pending
heading = Direction.North
rank = Priority.High
suit = Suit.Hearts

say order            /// prints: Status.Pending
say "Go {heading.name}"   /// "Go North"
say rank.value       /// 3 (int backing)
say suit.value       /// "H" (string backing)

Rules
Construct with EnumName.Variant.


.name returns the variant name as a string.


.value returns the backing value; for symbolic enums it returns the name string.


String interpolation prints EnumName.Variant.


Iteration & Introspection
/// Iterate directly
for s in Status
    say s.name
end

/// Or fetch all variants
all = Status.values
say all.len      /// count of variants

Rules
for v in EnumName iterates variants in declaration order.


EnumName.values yields an array of variants in declaration order.


Comparisons & Ordering
state = Status.Paid
if state == Status.Paid
    say "Thanks!"
end

/// Ordering uses backing values explicitly
if Priority.High.value > Priority.Medium.value
    say "Escalate"
end

Rules
Equality/inequality: ==, != are defined within the same enum type.


Cross‑type comparison (e.g., Status.Pending == Priority.High) is a TypeError.


Ordering (<, >, …) is not defined on enum values; compare .value explicitly when needed.


Control Flow with judge
msg = judge order
    Status.Pending:  "Order received"
    Status.Paid:     "Preparing shipment"
    Status.Shipped:  "On the way"
end

arrow = judge heading
    Direction.North: "↑"
    Direction.East:  "→"
    Direction.South: "↓"
    Direction.West:  "←"
end

Rules
judge may match directly on enum variants.


As with all judge, first matching arm wins; include else: for a total mapping when needed.


Enums in Data Structures
/// Map keys
enum State
    Idle
    Loading
    Success
    Error
end

ui = {
    State.Idle:    {color: "gray",  icon: "circle"},
    State.Loading: {color: "blue",  icon: "spinner"},
    State.Success: {color: "green", icon: "check"},
    State.Error:   {color: "red",   icon: "x"}
}

now = State.Loading
say ui[now].icon    /// "spinner"

/// Arrays + filtering
tasks = [
    {name: "Docs",   prio: Priority.Low},
    {name: "Bugfix", prio: Priority.High},
    {name: "Review", prio: Priority.Medium}
]

for t in tasks where t.prio == Priority.High
    say "HOT: {t.name}"
end

Rules
Enum values are stable as map keys (hash/equality are structural on the enum type+variant).


where conditions work with enums via equality.


Objects & Enums
enum UserRole
    Guest
    Member
    Admin
    Owner
end

class User = name: "{name}" :: role: UserRole.Member
    op can_delete_users() = role == UserRole.Owner
    op can_edit_posts()  = role == UserRole.Admin or role == UserRole.Owner
end

u = User: name: "Alice"
if u.can_edit_posts()
    say "Alice can edit"
end

Rule
Use enums directly in class fields (role: UserRole.Member). Access via method‑style as usual.


External Integration (Backed Enums)
/// API sends string codes; map to enum by value
enum OrderStatus as string
    Pending    = "PENDING"
    Processing = "PROCESSING"
    Shipped    = "SHIPPED"
    Delivered  = "DELIVERED"
end

api = {status: "SHIPPED"}
status = nil
for s in OrderStatus.values
    if s.value == api.status
        status = s
        stop
    end
end

if status != nil
    say "Status: {status.name}"
else
    error "Unknown status: {api.status}"
end

/// Outbound: use .value
payload = {status: OrderStatus.Processing.value}
"update.json".write_json(payload)

Rules
Convert inbound codes by comparing to .value of each variant (or maintain a lookup map).


Emit outbound codes with .value.


Constraints & Errors
/// ❌ Cross‑type compare
/// Status.Pending == Priority.Low     /// TypeError

/// ❌ Duplicate backing values
/// enum X as int
///     A = 1
///     B = 1     /// TypeError
/// end

/// ❌ Case mismatch / unknown variant
/// s = Status.paid                    /// UnknownNameError

Rules
Cross‑enum comparisons are a TypeError.


Duplicate backing values in a single enum are a TypeError.


Misspelled variant identifiers raise UnknownNameError.


No arithmetic on enums; for backed enums, operate on .value when needed.


Quick Reference
Define


Symbolic: enum Name ... end


Backed: enum Name as int|string ... end with Variant = <backing>


Construct


Enum.Variant


Inspect


.name → variant name (string)


.value → backing value (or name for symbolic)


Iterate: for v in Enum, or Enum.values


Compare


==, != within same enum type only


No ordering; compare .value explicitly if required


Use in Data


Safe as map keys and in arrays; declaration order preserved for iteration


Interop


Inbound: match codes to Enum.values by .value


Outbound: emit .value


Errors


TypeError (cross‑type compare, duplicate backing, arithmetic on enum)


UnknownNameError (bad variant name)


18. Money & Currency
Money is a first‑class type in Goblin. It preserves exact decimal values, tracks currency, forbids ambiguous math, and routes any sub‑cent loss according to a configurable policy. The goal is conservation (totals always reconcile), auditability, and explicitness. Under the hood, money is powered by the Big decimal engine with a currency tag and policy‑controlled precision.
Design intent: floats are not acceptable for money. Goblin encodes currency and precision rules directly into the type system and operator semantics. If Goblin does something differently than other languages, it is to avoid silent precision loss and cross‑currency bugs.

PATCHES::
#18 Money — “sensible default”


Ship a site_default that most folks never touch:

 set @policy site_default
    money: {
        currency: "USD",
        display_precision: 2,
        max_precision: 8,
        rounding: "bankers",
        policy: "truncate",          /// quiet, “just works”
        rounding_timing: "per_tx"    /// simplest mental model
    }
end


Rationale: no warning spam in normal dev, exact enough for 99% of cases. Teams that need stricter behavior flip to warn/strict per file or scope.


#18 Money — remainder ledger persistence (add as a patch)


Persist the process-global ledger per currency to .goblin/money.remainders.lock (atomic writes).


Add APIs:


Money::save_remainders(path = ".goblin/money.remainders.lock")


Money::load_remainders(path = ".goblin/money.remainders.lock")


(existing) Money::remainders_total(), Money::remainders_report(), Money::clear_remainders(), Money::drip_remainders(...)


Deterministic builds: writes blocked unless policy allows (money: { allow_state_writes: true }).


Optional CLI:


goblin money ledger --status|--save|--load|--clear|--drip <threshold>

Literals & Construction
Goblin supports symbol and code forms. Use whichever is unambiguous for your context.
/// Symbol literals
price   = $25.99         /// USD
fee     = €19.50         /// EUR
pounds  = £22.00         /// GBP
yen     = ¥2500          /// JPY (0‑decimal currency)

/// Code forms (useful when symbols collide or for clarity)
usd1 = 25.99 USD
cad1 = 35.00 CAD
pref = US$30.00          /// explicit country+symbol

/// Negatives
refund = -$12.34
chargeback = $-12.34     /// both are valid

/// From expressions (namespace constructor)
net   = Money::from(15.75)          /// uses policy default currency
net_e = Money::from(15.75, EUR)     /// explicit currency

Rules
A money value always carries a currency code.


Literals are exact (no float rounding).


Prefer literals or Money::from(...) over ad‑hoc number math.


Money uses $ or ISO code forms only (no suffixes like m).


Collision rule: m is minutes (durations). Use $50.00 or 50 USD, not 50m.


Unknown suffixes raise SyntaxError (e.g., 7z).


Arithmetic Operators (Constrained)
Money participates in arithmetic where the result is well‑defined and safe.
subtotal = $12.50 + $2.50        /// $15.00 (same currency)
rem      = $100.00 - $75.00      /// $25.00
scaled   = $20.00 * 2            /// $40.00 (number × money)

q  = $100.00 // 3                /// $33.33 (quotient, per policy)
r  = $100.00 %  3                /// $0.01 (remainder)
q2,r2 = $100.00 >> 3             /// ($33.33, $0.01)

bal = $100.00
bal += $25.50                    /// $125.50
bal *= 1.1                       /// $138.05 (policy rounding)

p = $19.99
p++                              /// adds one major unit → $20.99 in USD
p--                              /// subtracts one major unit → back to $19.99

Forbidden
/// cross‑currency arithmetic
/// $100.00 + €85.00          /// ❌ CurrencyError

/// float division on money
/// $100.00 / 3               /// ❌ MoneyDivisionError (use // or >>)

/// compound division/modulo
/// bal /= 2                  /// ❌ MoneyDivisionError
/// bal %= 3                  /// ❌ MoneyDivisionError

Rationale: Any operation that could hide precision loss or mix currencies implicitly is illegal. Use explicit conversion and divmod forms.
Precision & Policy (Conservation)
Precision loss happens when the currency precision can’t exactly represent a result (e.g., $10 ÷ 3). Goblin routes that loss per money policy.
set @policy financial_app
    money: {
        currency: "USD",            /// default currency for Money::from
        display_precision: 2,        /// what users see/export (e.g., cents)
        max_precision: 8,            /// internal carry precision before ledgering
        rounding: "bankers",        /// tie‑break mode (half_even)
        policy: "strict",           /// how to handle sub‑unit precision
        rounding_timing: "per_tx"   /// when to apply rounding: per_tx | per_period | at_settlement
    }
end

Policy Modes
truncate — cut extra digits, track the rest
set @policy demo_trunc
    money: { precision: 2, policy: "truncate" }
end

share = $10.00 // 3        /// $3.33
rem   = $10.00 %  3        /// $0.01 (ledger‑tracked)

Result is truncated to currency precision.


Sub‑precision is accumulated in the remainder ledger.


warn — truncate + emit diagnostic
set @policy demo_warn
    money: { precision: 2, policy: "warn" }
end

part = $20.00 // 7         /// $2.85 + MoneyPrecisionWarning

strict — refuse to lose precision
set @policy demo_strict
    money: { precision: 2, policy: "strict" }
end

/// $10.00 // 3            /// ❌ MoneyPrecisionError
q,r = $10.00 >> 3          /// ✅ ($3.33, $0.01) — you must handle r

defer — keep full internal precision; settle later
set @policy demo_defer
    money: { precision: 2, policy: "defer" }
end

x = $10.00 // 3            /// displays $3.33, retains $3.333...
x.excess                   /// 0.00333… (introspection)
final = x.settle()         /// apply precision now (see sub‑modes)

Defer sub‑modes
set @policy defer_bake
    money: { policy: "defer", defer_mode: "bake-in" }
end

set @policy defer_escrow
    money: { policy: "defer", defer_mode: "escrow" }
end

set @policy defer_carry
    money: { policy: "defer", defer_mode: "carry-forward" }
end

bake‑in: round into the value on settle().


escrow: divert sub‑precision to the ledger on settle().


carry‑forward: retain sub‑precision across operations until a final settle() (e.g., multi‑step calcs).


Rounding Timing
Configure when rounding/ledgering occurs:
per_tx: on each money operation (simplest mental model; most conservative).


per_period: accumulate within a declared period, then round once (e.g., end of day).


at_settlement: carry full precision until explicit settle()/export.


Implementation Note: Money = Big + Policy
Money arithmetic uses the Big engine; non‑terminating results carry up to max_precision.


Digits beyond max_precision are routed to the per‑currency remainder ledger.


Rounding modes mirror Big: bankers (default), half-up, half-down, floor, ceil.


Remainder Ledger (Audit Trail)
A global, per‑currency ledger records sub‑precision that cannot fit within current precision.
Money::clear_remainders()

_ = $10.00 // 3    /// adds ≈ $0.01 to USD ledger
_ = $20.00 // 7    /// adds ≈ $0.05 to USD ledger

Money::remainders_total()   /// {USD: $0.06}
Money::remainders_report()  /// detailed sources

Money::drip_remainders(threshold: $1.00, commit: true)
Money::clear_remainders()

Rules
Ledger is process‑global and per currency.


Conservation: distributed + ledger == mathematically exact total.


Conversion (Explicit Only)
Currencies never auto‑convert. You must supply a rate and a target.
usd = $100.00

/// method‑style conversion (preferred)
eur = usd.convert(to: EUR, rate: 0.91)     /// EUR 91.00

/// higher precision rate
prec = usd.convert(to: EUR, rate: 0.9127)  /// policy decides display; ledger keeps excess

Rules
Conversion may generate target‑currency remainder per policy.


Cross‑currency arithmetic remains illegal even after conversion unless both operands share the same currency.


Splitting & Allocation (Fairness)
Common partition strategies are provided under the Money namespace.
/// Even split — deterministic extra‑cent assignment
shares = Money::divide_evenly($100.00, 3)
/// [$33.34, $33.33, $33.33]; sum == $100.00

/// Even split + escrow remainder
res = Money::divide_evenly_escrow($100.00, 7)
res.shares   /// [$14.29, $14.29, $14.28, $14.28, $14.28, $14.28, $14.28]
res.escrow   /// $0.04

/// Round‑robin micro‑allocation
micro = Money::allocate_round_robin($0.07, 5)
/// [$0.02, $0.02, $0.01, $0.01, $0.01]

/// Weighted allocation (weights are numbers or money)
weights = [50, 30, 20]
alloc   = Money::allocate($1000.00, weights)
/// [$500.00, $300.00, $200.00]

Rules
All strategies conserve totals (prove by summing outputs).


Escrow variants expose unsplittable remainders explicitly.


Display vs Internal Representation
Formatting affects display/export only; internal math always uses exact decimals.
set @policy us_fmt
    money: { currency: "USD", precision: 2, thousands: ",", decimal: "." }
end
say $1234.56            /// $1,234.56

set @policy eu_fmt
    money: { currency: "EUR", precision: 2, thousands: ".", decimal: "," }
end
say €1234.56            /// €1.234,56

v = $1234.56
x = v * 2               /// exact internal $2469.12
say x                   /// formatting per active policy

"report.json".write_json({ total: x })
"api.json".write_json({ total: x }, { money_format: "canonical" })
/// canonical: "2469.12 USD"

Rules
Supported thousands: ",", ".", " ", "_", "".


Supported decimal: ".", ",".


Rounding strategies: bankers, half-up, half-down, floor, ceil (policy‑controlled).


Percent Integration
Percent operators compose with money without special casing.
price = $80.00

with_tax   = price + (8.5% of price)   /// $86.80
only_tax   = price.tax(8.5%)            /// helper: $6.80
sale_price = price - (15% of price)     /// $68.00

/// chaining with %s (self‑percent)
result = price + 10%s - 5%s             /// $80 + $8 − $4 = $84.00

/// %o — percent‑of‑other (shorthand for `of`)
bonus = 25%o base                        /// same as 25% of base

/// Casting strings to percent
rate = "8.5%".pct!                      /// 0.085 or CastError
maybe_rate = "8.5%".pct?               /// 0.085 or nil

Rules
(p% of money) produces money; %s uses the left value as the base.


%o X is sugar for % of X; % self is sugar for %s.


Collections & Multi‑Currency Totals
Keep per‑currency buckets; convert explicitly if a single total is required.
orders = [ $50.00, €45.00, £40.00 ]

usd = $0.00; eur = €0.00; gbp = £0.00
for amt in orders
    judge amt.currency
        "USD": usd += amt
        "EUR": eur += amt
        "GBP": gbp += amt
    end
end

rates = { "EUR_to_USD": 1.10, "GBP_to_USD": 1.25 }
all_usd = usd
        + eur.convert(to: USD, rate: rates["EUR_to_USD"])
        + gbp.convert(to: USD, rate: rates["GBP_to_USD"])

Common Errors & Guarantees
/// ❌ Using / with money
/// $100.00 / 3                  /// MoneyDivisionError

/// ❌ Mixing currencies
/// $50.00 + €45.00              /// CurrencyError

/// ❌ Expecting strict mode to round for you
set @policy strict_only
    money: { display_precision: 2, policy: "strict" }
end
/// $10.00 // 3                  /// MoneyPrecisionError — use >> and handle r

/// ⚠️ Ledger is global (per process)
Money::clear_remainders()
_ = $10.00 // 3
_ = $20.00 // 3
Money::remainders_total()       /// {USD: $0.03}

/// ⚠️ ++/-- change major units, not minor
p = $19.99; p++                 /// $20.99 (USD)

/// ⚠️ Suffix collisions
/// 50m == 50 minutes (duration), not money. Use $50.00 or 50 USD.
/// Unknown suffixes like 7z → SyntaxError.

Guarantees
Conservation: sums of parts plus ledger equal the mathematical total.


No silent conversion: currency mixing requires explicit convert.


Policy‑driven rounding: every loss path is configured and audited.


Quick Reference
Literals & Construction
$12.34, €19.50, £22.00, ¥2500


25.99 USD, US$30.00


Money::from(amt), Money::from(amt, EUR)


Operators
Allowed: +, -, *, //, %, >>, +=, -=, *=


Forbidden: /, /=, %= on money; cross‑currency arithmetic


Postfix: money++, money-- → ±1 major unit


Policy
display_precision, max_precision, rounding, rounding_timing


policy: truncate | warn | strict | defer


defer_mode: bake-in | escrow | carry-forward


Ledger (Money namespace)**
Money::remainders_total()


Money::remainders_report()


Money::drip_remainders(threshold:, commit:)


Money::clear_remainders()


Conversion
amount.convert(to: CURRENCY, rate: R)


Allocation (Money namespace)
Money::divide_evenly(amount, parts)


Money::divide_evenly_escrow(amount, parts)


Money::allocate_round_robin(amount, parts)


Money::allocate(amount, weights)


Formatting
Policy controls thousands ","|"."|" "|"_"|"" and decimal "."|"," separators


Human‑facing export follows policy; canonical export via money_format: "canonical"


Percent shortcuts
% → percent‑of‑1; %s → percent‑of‑self; %o <expr> → percent‑of‑other


String → percent: "12.5%".pct! / .pct?


Design Differences (Why)
No float division: prevents hidden bias and drift.


No implicit currency mixing: prevents book‑keeping errors.


Global ledger: makes remainder flow auditable.


19. Percent Operator (CIPO)
Percent is a first‑class type. Goblin’s Context‑Independent Percent Operator (CIPO) requires that every percent has an explicit base at write time. This removes context‑dependent ambiguities common in other languages and keeps money math policy‑safe.
PATCHES::
Percent vs modulo (tiny precedence note to add under #18 and the Percent section)
Percent literals (12.5%) just produce a percent value;
 of/%o are operators that evaluate exactly like parentheses around a normal multiplication:

 10 + 10% of 100   /// == 10 + (0.10 * 100) == 20


Modulo % is the arithmetic operator with normal precedence (same tier as *//). There’s no overlap: “percent” is lexical (12.5% → value), “modulo” is the % operator.

Forms & Semantics
% — percent of 1 (scalar)
Represents a unitless multiplier. With numbers it scales; with money it can serve as a small absolute addend when combined with +/-.
p = 75%                      /// 0.75 percent value
x = base * 150%              /// scale by 1.5
fee = $100 + 5%              /// adds 5% of one major unit → $100.05 (USD); policy governs rounding

Numbers: x * 25% ≡ x * 0.25.


Money: % with +/- adds/subtracts that percent of a single major unit of the money’s currency (e.g., 1% with USD = $0.01). Precision/rounding/ledgering follow the active money policy.


%s — percent of self (calculator)
Base is the left operand (or the explicit of target). Use for “X% of this value”.
subtotal = $50
with_tax = subtotal + 8.5%s        /// $54.25 (8.5% of $50)
price    = $80
sale     = price - 25%s             /// $60

Lint rule: Storing %s without a bound base is discouraged and flags LINT-PCT-001. Store a base‑free percent and bind the base at use time.
rate = 8.5%                          /// base‑free
final = subtotal + (rate of subtotal)/// bind explicitly

% of E — explicit base (portable)
Spells the base; recommended for clarity and reuse.
tax        = 8.5% of subtotal       /// unambiguous base
commission = 2%  of sale_price

%o E — shorthand for of
Prefix form equivalent to of.
25%o 200            /// 50
25% of 200          /// 50 (equivalent)

Sugar: % self
% self is identical to %s.
total = price + 10% self            /// same as price + 10%s

Reasoning (Design)
Other languages overload % contextually, causing silent base changes and double‑application bugs. CIPO fixes the base at author time, separates rate from application, and composes cleanly with the Money policy (rounding, max precision, ledgers, timing). The result is auditable math and predictable code generation.
Construction, Casting, Display
r1 = 8.5%                    /// literal percent (percentage points)
r2 = pct(0.15)               /// 0.15% (constructor takes percentage points)
str = "8.5%".pct!           /// 0.085 or CastError
opt = "bad".pct?            /// nil (no throw)

% literals are in percentage points.


String casts: .pct! (strict), .pct? (optional).


Percent values print as e.g. "8.5%".


Money Integration (Policy‑Safe)
Applying a percent to money yields money and follows the Money section’s policy (display_precision, max_precision, rounding, policy, rounding_timing).
price = $80.00

amount_tax = 8.5% of price         /// $6.80
with_tax   = price + 8.5%s          /// $86.80
small_fee  = price + 1%             /// $80.01 (percent of one major unit)

/// Helpers
only_tax = price.tax(8.5%)
final    = price.with_tax(8.5%)
compound = price.tax([8%, 2%], compound: true)

Composition & Policy Engines
Prefer base‑free rules and bind bases at use sites; works with judge.
discount_rate = judge
  order.total >= $1000: 20%
  order.total >= $500:  15%
  order.total >= $100:  10%
  else: 0%
end

final_total = order.total - (discount_rate of order.total)

Rules & Constraints
Base selection: % = of 1; %s = of left operand; % of E/%o E = of expression E.


Storage: Store base‑free percents (r = 8.5%). Storing %s triggers LINT-PCT-001.


Application: rate of x is the canonical application; %s is self‑bound sugar.


Types: Applying a percent to a number yields number; to money yields money.


Precedence: Percent application binds tightly (see Operators). Use parentheses to make bases explicit in complex expressions.


Parsing: Modulus is a % b (with spaces). 10% is a percent literal; 10 % 5 is modulo.


Interop: Numeric promotion rules apply before/after percent math (int/float/big; any with big → big).


Quick Reference
Forms: % (of 1), %s (of self), % of E / %o E (explicit base).


Best practice: Define rate = 8.5%; apply as rate of subtotal.


Money: Exact arithmetic + policy (rounding, max precision, ledgers). % with +/- on money adds a small absolute amount (percent of one major unit).


Constructor & casts: pct(n) (n in percentage points), "8.5%".pct!/.pct?.


Helpers: money.tax(rate), money.with_tax(rate), tax([rates], compound: bool).


Lint: LINT-PCT-001 for stored %s.


Difference vs others: No context‑dependent %; bases are explicit, portable, and auditable.


20. Date & Time
Goblin’s temporal system forbids “naive” time. Values are typed (date, time, datetime, duration), carry all needed context (time zone where applicable), and only permit explicit, calendar-safe operations. This eliminates silent bugs around DST, offsets, and ambiguous units.

Model (Types & Semantics)
date — calendar day (no time, no zone). Use for birthdays, deadlines, holidays.


time — clock time (no date, no zone). Use for business hours, daily schedules.


datetime — full instant (date + time + time zone). Use for events, logging, storage.


duration — span (no calendar or zone). Use for intervals, timeouts, elapsed.


Type safety
No implicit coercions among date/time/datetime/duration.


Arithmetic and comparisons are defined only for like/compatible types.


Cross-type math/compare raises TypeError.


/// Like-type comparisons only
start = date "2025-08-12"
stop  = date "2025-08-31"
say start < stop           /// true

/// Cross-type compare → error
nowdt = datetime "2025-08-12T14:30:00" tz:"UTC"
/// say start > nowdt     /// ❌ TypeError


Construction (Literals & Constructors)
From the canonical literals patch:
/// Strings — quotes construct strings
"abc", 'abc', """multi"""

/// Dates/Times/Datetimes — constructor keywords
birthday = date "1990-05-15"
lunch    = time "12:30:05.250"
meet     = datetime "2025-08-23T14:30:00" tz:"UTC"

/// Durations — unit suffixes
short  = 90s
pause  = 30m
slot   = 2h
sprint = 2w
phase  = 6mo
term   = 1y

/// Booleans
true, false

Units & collisions
Duration units: s, m, h, d, w, mo, y.
 m = minutes only; months are mo (patch rule). Unknown unit → SyntaxError.


Money uses $/ISO literals, not duration suffixes.


⚠ Cheatsheet–canonical note
 Older examples may show date("YYYY-MM-DD")/datetime("...", tz:"..."). The canonical form is keyword + string (date "...", datetime "..." tz:"..."). If this conflicts with any earlier sample, prefer this section and flag the sample for update.

Arithmetic
Duration arithmetic
/// datetime ± duration
start = datetime "2025-08-12T14:00:00" tz:"UTC"
end   = start + 90m

/// date ± whole-day duration
 today   = date "2025-08-12"
 tomorrow = today + 1d
 span     = (date "2025-09-01") - (date "2025-08-12")   /// duration

/// Differences
dt_dur = end - start    /// 1h 30m
d_dur  = (date "2025-08-20") - (date "2025-08-12")  /// 8d

Rules
datetime ± duration → datetime.


date ± duration requires whole days; fractional-day durations raise TypeError.


datetime - datetime → duration; date - date → duration.


Calendar-safe arithmetic
Use field-change adders to express calendar intent (end-of-month, leap years):
inv = date "2025-01-31"
next = add_months(inv, 1)   /// Feb 28/29 (clamped), never Mar 02 via 30d

bday = date "2000-02-29"
next_bday = add_years(bday, 1)   /// 2001-02-28 (clamped)

meet = datetime "2025-01-31T14:00" tz:"UTC"
next_meet = add_months(meet, 1)  /// 2025-02-28T14:00Z

Time arithmetic (clock times without date)
Clock times have no date to absorb overflow. Intent must be explicit:
open  = time "09:00"
close = time "17:00"

/// Explicit behaviors
wrap  = wrap_time(open, 10h)      /// 19:00, same-day wrap (policy: wrap)
shift = shift_time(close, 10h)    /// {days: 0|1|… , time: HH:MM} overflow info

/// Implicit time ± duration is policy-controlled; default is error.


Time Zones
Zones use IANA identifiers ("America/Denver", "Europe/London") or fixed offsets ("+00:00", "-07:00").


Conversion preserves the instant; only wall-clock components change.


mtg_local = datetime "2025-08-12T14:30:00" tz:"America/Denver"
mtg_utc   = to_tz(mtg_local, "UTC")         /// same epoch
mtg_tokyo = to_tz(mtg_local, "Asia/Tokyo")  /// next-day wall clock likely

say mtg_local.epoch() == mtg_utc.epoch()      /// true


Policy (Project-wide Temporal Configuration)
set @policy temporal_defaults
  datetime: {
    tz: local_tz(),          /// default zone for constructors w/out tz
    time_arith: "error",     /// time ± duration: error|wrap|shift
    prefer_trusted: false,   /// see Trusted Time
    policy: "allow",         /// server/cache/local fallback policy
    leap_mode: "clamp",      /// leap-second handling: clamp|smear
    tzdata_version: "",      /// pin or engine default
    debug: false
  }
end

time_arith — governs time ± duration if used; best practice is wrap_time/shift_time instead of relying on policy.


leap_mode — clamp parses :60 as :59+1s; smear spreads into last second.


Trusted Time (optional)
For verifiable timestamps: server → signed cache → local fallback by policy.
set @policy trusted_strict
  datetime: {
    source: "https://time.nist.gov/api/now",
    prefer_trusted: true,
    policy: "strict",
    ttl: 60s, cache_ttl: 24h,
    skew_tolerance: 5s,
    cache_path: "dist/time.cache",
    cache_signing_key: "secret"
  }
end

verified = trusted_now()        /// server or signed cache
info     = time_status()        /// {source, verified, age_s, offset_s, tzdata...}
ensure_time_verified("audit")  /// error if not trusted under strict


Ranges & Iteration
Date ranges default inclusive (..).


Datetime ranges default exclusive (...).


step must be a duration.


/// Dates (inclusive)
for d in date "2025-08-01" .. date "2025-08-05"
  say d
end
/// 01..05 included

/// Datetimes (exclusive), hourly by default if engine provides a default; otherwise specify
start = datetime "2025-08-12T08:00" tz:"UTC"
end   = datetime "2025-08-12T17:00" tz:"UTC"
for t in start ... end step 1h
  schedule(t)
end
/// 08:00..16:00

If your cheatsheet states a different default step for datetime ranges, prefer the cheatsheet and flag this section.

Formatting & Parsing
Standard: .iso() and str() emit ISO-8601 with numeric offsets (never zone abbreviations).


Custom: .format("YYYY-MM-DD HH:mm:ss") style tokens.


Parsing: parse_date, parse_time, parse_datetime with fmt:; explicit Z/offset in input overrides tz:/policy defaults.


mtg = datetime "2025-08-12T14:30:00" tz:"UTC"
say mtg.iso()                 /// 2025-08-12T14:30:00Z
say mtg.format("YYYY-MM-DD")  /// 2025-08-12

u = parse_datetime("2025-08-12 14:30", fmt:"YYYY-MM-DD HH:mm", tz:"UTC")

Tokens: YYYY, MM, DD, HH, mm, ss, SSS, ZZ.

Serialization & Exchange
mtg = datetime "2025-08-12T14:30:00" tz:"America/Denver"

/// JSON: string form (portable)
write_json("meeting.json", { meeting: mtg })
/// {"meeting":"2025-08-12T14:30:00-06:00"}

/// JSON: typed object
write_json("meeting.json", { meeting: mtg }, { datetime: "object" })
/// {"meeting":{"_type":"datetime","value":"2025-08-12T14:30:00-06:00"}}

/// Reading
read_json("meeting.json", { datetime: "auto" })
read_json("meeting.json", { datetime: "string" })   /// strict strings
read_json("meeting.json", { datetime: "off" })      /// no parsing

DB helpers
epoch = mtg.epoch()                      /// float seconds since Unix epoch
utc_naive   = datetime_to_naive_utc(mtg)
local_naive = datetime_to_naive_local(mtg)
restored    = naive_to_datetime(epoch, "America/Denver")


Leap Seconds
set @policy leap
  datetime: { leap_mode: "clamp" }   /// or: "smear"
end

clamp: parse HH:MM:60 (UTC only) as HH:MM:59 + 1s; never format :60.


smear: distribute the leap second across the last second of the day for continuity.



Introspection
dt_policy()  /// → {tz, prefer_trusted, policy, leap_mode, time_arith, tzdata_version, ...}
why_dt("add_months")  /// human-readable decision trace (parsing/adder/range/policy)

If debug:true, policy changes append JSON lines to dist/policy.log with scope and diffs.

Rules & Constraints
No naive timestamps: all datetime values carry an explicit zone (input Z/offset wins over tz:/policy).


No implicit date/time mixing: combine explicitly if you need a datetime.


Calendar intent vs fixed duration: use add_months/add_years for calendar schedules; use durations for exact intervals.


Time arithmetic requires intent: use wrap_time/shift_time or set policy; default rejects implicit time ± duration.


Duration months/years are approximations: mo ≈ 30d, y ≈ 365d. Use calendar adders for real calendars.



Quick Reference
Types
date "YYYY-MM-DD"


time "HH:MM[:SS[.SSS]]"


datetime "YYYY-MM-DDTHH:MM[:SS]" tz:"Area/City"


duration: 30s, 5m, 2h, 7d, 1w, 6mo, 1y (patch units; m=minutes)


Build
Constructors above; current: now(), today(), utcnow().


Parse: parse_date, parse_time, parse_datetime (fmt:, tz:).


Arithmetic
datetime ± duration → datetime; date ± whole-day duration → date.


Differences: datetime - datetime → duration; date - date → duration.


Calendar adders: add_months, add_years (clamped EOM/leap).


Time zones
IANA names or fixed offsets; convert with to_tz(dt, "Zone").


Ranges
Dates: d1..d2 inclusive; ... exclusive.


Datetimes: typically dt1...dt2; step requires a duration.


Format
.iso(), str(), .format("YYYY-MM-DD HH:mm:ss"); tokens include YYYY MM DD HH mm ss SSS ZZ.


Serialization
JSON: string (default) or { datetime:"object" }; reading: { datetime:"auto"|"string"|"off" }.


Epoch/naive helpers: .epoch(), datetime_to_naive_utc/local, naive_to_datetime.


Policy
datetime: { tz, time_arith:"error|wrap|shift", prefer_trusted, policy, leap_mode:"clamp|smear" }.


Trusted time
trusted_now(), trusted_today(), time_status(), ensure_time_verified(reason).


Errors
TypeError, TimezoneError, ValueError, TimeArithmeticError, TimeSourceError, TimeSourceWarning.


21. Blob & Binary Data
Goblin’s blob is the canonical type for raw bytes. It is not a string: strings are always UTF‑8 text; blobs are byte sequences. Conversions between the two are explicit to prevent data corruption and security mistakes.
Design
Text ≠ Bytes. No implicit encoding/decoding. You must choose when text becomes bytes and vice‑versa.


Fail fast. Invalid encodings raise errors instead of producing garbage.


Safe slices. Indexing/slicing never overruns memory; semantics are defined and bounds‑checked.


Method‑style. All functionality is via methods/operations; no global helpers.


Type & Construction
Literals & Constructors
/// Empty blob for incremental building
b = blob

/// Text → bytes (explicit UTF‑8 encoding)
hello = blob "Hello, world!"
non_ascii = blob "こんにちは"
emoji = blob "🎉✨"

/// Back to text (strict UTF‑8 decoding)
text = hello.str            /// "Hello, world!"

Reasoning. Goblin requires blob "…" and .str so the encoding choice (UTF‑8) is visible at the call site (no hidden defaults).
From Encodings (Strings → Blob)
/// Base64 (common for web/APIs, email attachments, data URLs)
png = "iVBORw0K...".from_base64

/// Hex (common for crypto, protocols, debugging)
sig = "deadbeef1234567890abcdef".from_hex

/// Errors are explicit
/// bad = "xyz".from_hex       /// ❌ ValueError
/// bad = "not-base64".from_base64  /// ❌ ValueError

From Files (Binary I/O)
img = "logo.png".read_bytes
pdf = "report.pdf".read_bytes

"copy.png".write_bytes(img)
"backup.pdf".write_bytes(pdf)

Operations
Inspection
size = img.len                 /// byte count (int)
empty = (blob).len == 0

/// Bytewise equality
a = "deadbeef".from_hex
b = "deadbeef".from_hex
same = (a == b)                /// true

Indexing & Slicing
data = "deadbeef1234567890abcdef".from_hex

/// Single‑byte indexing → int 0..255 (bounds‑checked)
first = data[0]                /// 222 (0xDE)
last  = data[-1]               /// 52  (0x34)
/// oob = data[999]            /// ❌ ValueError (index out of bounds)

/// Slices → blob (copies); bounds are clamped
head4 = data[0:4]
mid   = data[2:6]
tail4 = data[-4:]
rest  = data[10:1000]          /// returns available bytes; never overruns

Rules. Index may be negative (from end). Slice endpoints outside range are clamped. Slice produces a new blob (no aliasing views). Step is not supported in blob slices.
Encoding/Decoding
bin = blob "Hello, 世界! 🌍"

/// To text (strict UTF‑8)
text = bin.str

/// To encodings
h = bin.to_hex                 /// lowercase hex string
b64 = bin.to_base64            /// base64 string

/// Round‑trip check
ok = (b64.from_base64 == bin)

Rules. .str raises ValueError if bytes are not valid UTF‑8. Hex input is case‑insensitive; hex output is lowercase.
Concatenation & Building
hdr = "89504e47".from_hex       /// PNG signature
payload = "image.dat".read_bytes

packet1 = hdr + payload          /// operator form
packet2 = hdr.concat(payload)    /// method form (equivalent)

Rules. + and .concat() produce a new blob. Strings cannot be concatenated with blobs; convert explicitly (blob "…" or .str).
JSON Serialization
Blobs are not serializable as JSON without an explicit representation.
b = "photo.jpg".read_bytes

/// Explicit encoding choice (recommended)
"data.json".write_json({image: b}, {blob: "base64"})
/// or
"data.json".write_json({image: b}, {blob: "hex"})

/// Reading
obj1 = "data.json".read_json({blob: "auto"})     /// try base64 → hex → keep string
obj2 = "data.json".read_json({blob: "base64"})  /// expect base64 only
obj3 = "data.json".read_json({blob: "off"})     /// keep strings (no decode)

/// Without an encoding, serialization fails
/// "out.json".write_json({image: b})            /// ❌ SerializationError

Reasoning. JSON is text; a blob needs a text encoding (base64 for portability, hex for readability).
Patterns
File Type Detection (Magic Bytes)
op mime_for(data)
    if data.len < 4
        return "application/octet-stream"
    end
    hdr4 = data[0:4]
    if hdr4 == "89504e47".from_hex        /// PNG
        "image/png"
    elif hdr4 == "25504446".from_hex      /// PDF
        "application/pdf"
    elif hdr4 == "ffd8ffe0".from_hex or hdr4 == "ffd8ffe1".from_hex  /// JPEG variants
        "image/jpeg"
    elif hdr4[0:2] == "504b".from_hex     /// ZIP/OOXML
        "application/zip"
    else
        "application/octet-stream"
    end
end

kind = mime_for("document.bin".read_bytes)

Simple Binary Protocol Parse (u16 BE)
op parse_uint16_be(bytes)
    if bytes.len != 2
        error "Need exactly 2 bytes"
    end
    (bytes[0] << 8) + bytes[1]
end

op parse_packet(data)
    if data.len < 8
        error "Packet too short"
    end
    magic = data[0:4]
    if magic != "deadbeef".from_hex
        error "Bad magic: {magic.to_hex}"
    end
    len  = parse_uint16_be(data[4:6])
    typ  = parse_uint16_be(data[6:8])
    body = data[8:]
    if body.len != len
        error "Length mismatch: {len} vs {body.len}"
    end
    {type: typ, data: body}
end

Data URL Construction
op data_url(filename)
    bytes = filename.read_bytes
    mime = mime_for(bytes)
    "data:{mime};base64,{bytes.to_base64}"
end

url = data_url("logo.png")

Rules & Constraints
Blobs are byte sequences; strings are UTF‑8 text. No implicit conversion in either direction.


blob "…" encodes text as UTF‑8. .str decodes UTF‑8 and raises ValueError on invalid data.


.from_base64 / .to_base64, .from_hex / .to_hex convert between string encodings and blobs. Invalid input → ValueError.


Indexing returns an int (0..255). Out‑of‑bounds single index → ValueError.


Slicing returns a new blob; endpoints are clamped; negative indices count from end.


+ / .concat() join blobs. Mixing string + blob is a TypeError.


JSON: require {blob: "base64"|"hex"|"off"} for (de)serialization. Missing policy → SerializationError.


Quick Reference
Constructors
blob — empty


blob "text" — UTF‑8 encode text → blob


"file.bin".read_bytes — file → blob


"…".from_base64 / "…".from_hex — string → blob


Conversions
b.str — blob → UTF‑8 string


b.to_base64 — blob → base64 string


b.to_hex — blob → hex string (lowercase)


Ops
b.len — byte length


b[i] — byte (int)


b[a:b] — slice (blob)


b1 + b2 / b1.concat(b2) — concatenate


JSON
"out.json".write_json(obj, {blob: "base64"|"hex"})


read_json({blob: "auto"|"base64"|"hex"|"off"})


Errors
ValueError — invalid UTF‑8/base64/hex; oob index


TypeError — mixing blob with string ops


SerializationError — blob → JSON without encoding


22. Files, Printing & I/O
Goblin’s I/O is designed to be correct-by-default: sandboxed paths, explicit encodings, and strict separation of text, binary, and structured data. APIs are minimal, predictable, and avoid silent coercions that cause data loss (especially for money and time values).
Principles
Sandboxed paths. File ops resolve relative to the current working directory (CWD); absolute paths must be explicit. This prevents accidental access outside the project root.


Explicit encoding. Text files are UTF‑8 only. Binary data uses blob. No implicit text↔binary conversions.


Typed serialization. JSON/YAML reading never "guesses" domain types (e.g., money, datetime, enum). Writers allow explicit formatting modes; readers require explicit decode options.



Text Files (UTF‑8)
Text is always UTF‑8; invalid data raises ValueError.
/// Read UTF‑8 text
readme   = "README.md".read_text
config   = "app.config".read_text

/// Write/append UTF‑8 text
"out.txt".write_text("Hello, world! 🌍")
"log.txt".append_text("started\n")

/// Simple line processing
lines = "data.txt".read_text.split("\n")
clean = lines.filter(line => line.trim.len > 0)
"cleaned.txt".write_text(clean.join("\n"))

Rules
Text APIs operate on string only; binary content must use .read_bytes / .write_bytes.


UTF‑8 decoding is strict (no lossy replacements). ValueError on invalid input.



Binary Files (blob)
Binary data is raw bytes; use the blob type and byte APIs.
/// Read/write blobs
photo   = "avatar.jpg".read_bytes
"backup.jpg".write_bytes(photo)

/// Heuristic check (nulls, distribution, etc.)
if "mystery".is_binary
    bytes = "mystery".read_bytes
    say "Binary: {bytes.len} bytes"
else
    text = "mystery".read_text
    say "Text: {text.len} chars"
end

/// Magic bytes (PNG)
if photo[0:4] == "89504e47".from_hex
    say "PNG image"
end

Rules
No implicit text↔binary coercion. Combining string with blob is a TypeError.


Blob slicing is bounds‑safe; single‑index out‑of‑range is ValueError.



JSON (Structured Data)
Reading
JSON values map to generic Goblin types unless decoding is requested.
JSON
Goblin default
null
nil
true/false
bool
number
float
string
string
array
array
object
map (string keys)

raw = "{""price"": ""19.99"", ""when"": ""2025-01-15T12:00:00Z""}".json_parse()
price_str = raw.price         /// "19.99" (string)
when_str  = raw.when          /// ISO string

/// Explicit domain conversions
price = price_str.float.money("USD")
when  = when_str.datetime

Writing
Writers offer formatting modes for special domains. Defaults:
money → object (round‑trip precision)


datetime → string (ISO‑8601)


enum → name (variant name)


blob → error (must choose encoding)


order = { total: $1299.99, created_at: now(), status: OrderStatus.Pending }
"order.json".write_json(order)  /// money:object, datetime:string, enum:name

/// Choose explicit encodings
"sales.json".write_json({revenue: $50000.00}, {money: "string"})   /// "USD 50000.00"
"acct.json" .write_json({cash: $1_234.56},   {money: "units"})    /// integer units

/// Blob requires an encoding
img = "logo.png".read_bytes
"img.json".write_json({data: img}, {blob: "base64"})

In‑memory
payload = json_stringify({amount: $99.99, ts: now()}, {money: "string", datetime: "string"})
resp    = http_post("/pay", payload)
parsed  = json_parse(resp, {money: "string", datetime: "string"})

Rules
Readers do not auto‑promote strings/numbers to domain types; use options ({money: "object"|"string"|"units"|"auto"}, {datetime: "string"|"auto"|"off"}, {enum: "name"|"off"}).


Money never loses precision under any JSON mode; string/units forms are exact by construction.



YAML (Human‑edited Config)
Same options as JSON; YAML is text‑first and supports comments.
cfg = {
  pricing: { base: $5.00, per_item: $1.50, tax: 8.5% },
  features: ["auth", "metrics"]
}
"config.yaml".write_yaml(cfg, {money: "string", datetime: "string"})

loaded = "config.yaml".read_yaml({money: "string", datetime: "string"})
base   = loaded.pricing.base     /// money

Rules
YAML read returns native Goblin types only where decode options are supplied; otherwise strings.



CSV (Tabular)
CSV carries no types; read returns arrays of string maps.
rows = "sales.csv".read_csv     /// [{"date":"2025-01-15","amount":"1250.00"}, ...]

sales = rows.map(r => {
  date: r.date.datetime,
  amount: r.amount.float.money("USD"),
  customer: r.customer
})

sum = sales.sum(s => s.amount)

Writing converts values to strings (with options for money/datetime) or pre‑normalize columns:
custs = [
  {name: "Alice", spent: $1250.00, joined: today()},
  {name: "Bob",   spent: $890.50,  joined: today()}
]
"customers.csv".write_csv(custs, {money: "string", datetime: "string"})

/// Spreadsheet‑friendly split
flat = custs.map(c => {
  name: c.name,
  spent_amount: c.spent.amount,
  spent_currency: c.spent.currency,
  joined: c.joined.to_string
})
"customers_flat.csv".write_csv(flat)

Rules
Read: always strings. Convert explicitly via .int, .float, .bool, .datetime, .float.money("USD"), "USD 1.23".money_parse().



Paths & File System
say cwd()                     /// current directory
chdir("projects/myapp")

if "config.yaml".exists
    cfg = "config.yaml".read_yaml({money: "string"})
end

mkdirp("logs/2025/jan")      /// creates parents if needed
files = listdir(".")
imgs  = glob("**/*.{png,jpg}")

/// Cross‑platform paths
log_path   = join("logs", "app.log")
report_csv = join("reports", "2025", "jan", "sales.csv")

Rules
Relative paths resolve under CWD; absolute paths bypass sandbox by being explicit.


glob supports ** recursion and brace sets {a,b}.



Printing & Streams
Console output is explicit and side‑effect‑free (no hidden encodings).
say "Hello"                      /// stdout with newline
say {a: 1, b: 2}                 /// uses str() for formatting
warn "caution"                  /// stderr, non‑fatal

/// Errors are control flow, not printing
error "bad config"              /// raises, halts unless rescued

/// Formatting via interpolation
name = "Ada"
say "Hi, {name}!"               /// → Hi, Ada!

Rules
say writes UTF‑8 to stdout; warn writes UTF‑8 to stderr; both append \n.


Interpolation calls str(x) for each placeholder; domain types (money, datetime, enum, blob) use their canonical str forms.



Errors
File system: FileNotFoundError, PermissionError.


Encoding/Parsing: ValueError (UTF‑8, JSON, YAML, CSV structural issues).


Type: TypeError (mixing text and blob, invalid ops).


Serialization: SerializationError when writing blobs to JSON/YAML without an explicit {blob: ...} mode.


Money precision: MoneyPrecisionError only if a strict money policy in effect during decode (see Money section).



Quick Reference
Text
"path".read_text / "path".write_text(str) / "path".append_text(str)


Binary
"path".read_bytes / "path".write_bytes(blob) / "path".is_binary


JSON
"path".read_json(opts) / "path".write_json(val, opts)


json_parse(str, opts) / json_stringify(val, opts)


Options: {money: "object"|"string"|"units"|"auto"|"off", datetime: "string"|"auto"|"off", enum: "name"|"off", blob: "base64"|"hex"|"error"}


YAML
"path".read_yaml(opts) / "path".write_yaml(val, opts) (same options as JSON)


CSV
"path".read_csv → [{col: string, ...}]


"path".write_csv(rows, opts) (e.g., {money: "string", datetime: "string"})


FS
cwd() / chdir(path) / "path".exists


mkdirp(path) / listdir(path) / glob(pattern)


join(parts...) (portable path building)


Conversions
"123".int, "1.23".float, "true".bool


"2025-01-15".datetime


"19.99".float.money("USD"), "USD 19.99".money_parse()


Printing
say str|value (stdout), warn str|value (stderr), error str (raise)


23. Regex & Text Utilities
Goblin separates literal text operations from regular-expression (regex) pattern matching to keep semantics explicit and Unicode-safe. Literal search/replace uses exact substrings; regex uses a lean, deterministic subset (with optional full PCRE via a glam). Utilities for splitting, joining, extracting, and tokenizing text live here as well.
Design rationale. Text bugs often come from silent coercions, locale surprises, or unclear return types. Goblin fixes this by: (1) explicit literal vs regex forms, (2) Unicode code-point indexing, (3) immutable strings, and (4) deliberate return shapes.

Literal Search & Replace
Literal operations are case-sensitive and do not interpret metacharacters.
text = "The quick brown fox jumps"

text.has "fox"          /// true
text.find "fox"         /// 16 (0-based start) or nil if absent
text.find_all "o"       /// [12, 17, 26] (non-overlapping indices)
text.count "the"        /// 1 (case-sensitive)

s = "cats and cats"
s.replace "cats" with "dogs"        /// "dogs and dogs" (replace ALL)
s.replace_first "cats" with "dogs"  /// "dogs and cats"
s.remove " and "                      /// "cats cats" (alias: .drop)

Rules. Indices are Unicode code-point positions. find_all returns non-overlapping indices for literals. All results are new strings (immutability).

Extraction Helpers (Literal)
Convenience forms for common delimiter-based extraction.
email = "alice@example.com"
before "@" in email          /// "alice"
after  "@" in email          /// "example.com"

path = "/a/b/file.txt"
before_last "/" in path      /// "/a/b"
after_last  "/" in path      /// "file.txt"

between "<b>" and "</b>" in "<b>Bold</b>!"   /// "Bold"

Rules. If the delimiter(s) are not found, these return nil. For between, if either bound is missing or reversed, returns nil.

Breakdown & Tokenization
"Hello\nWorld".lines   /// ["Hello","World"]
words "a b  c"        /// ["a","b","c"] (splits on whitespace)
chars "abc"           /// ["a","b","c"] (Unicode code points)

Method forms exist: s.lines, s.words, s.chars.

Split & Join
Declarative split/join for delimited text.
parts = split "a,b,,c" on ","        /// ["a", "", "b", "", "c"] (keeps empties)
line  = join ["a","b"] with ","    /// "a,b"

Rules. split requires a string delimiter; empty delimiter ⇒ ValueError. join requires an array of strings and a string separator; any non-string element ⇒ TypeError. Joining an empty array yields "".

Regex (Lean Core)
Use regex when literal searching is insufficient. Prefer raw literals for patterns to avoid double escaping.
text   = "Email me at a.b@example.com or root@host"
find_all like raw "[\\w.]+@[\\w.]+" in text    /// ["a.b@example.com", "root@host"]

value  = "Order #12345"
match     like raw "\\d+" in value                /// true (any match?)

messy  = "a\t b\n  c"
replace   like raw "\\s+" with " " in messy     /// "a b c"
replace_first like raw "\\d+" with "X" in value  /// "Order #X"

Pattern subset (core). . [] () | ^ $ + * ? {m,n} and shorthands \d \w \s. All matches are case-sensitive; no flags in the core. Use the regex glam for full PCRE features (flags, lookarounds, named groups, etc.).
Return shapes (regex vs literal).
Literal: find_all → indices.


Regex: find_all like ... → matched substrings in order, non-overlapping.


match like ... in s → bool (exists?).


replace like ... / replace_first like ... → new string.


This split mirrors common use: indices for literal token work; substrings for pattern extraction.

Case & Normalization
All matching (literal and core-regex) is case-sensitive and operates on Unicode code points. For case-insensitive matching, normalize explicitly:
needle = "straße"
hay    = "STRASSE"
hay.lower.has needle.lower   /// true (simple fold)

Locale-sensitive folds are not performed implicitly. If you need locale-aware behavior, use dedicated libraries/glams.

Performance Notes
Literal search/replace are generally faster than regex. Prefer literals when possible; use regex where structure demands it. Both operate on immutable strings and return new values.

Rules & Constraints
Strings are immutable; all utilities return new strings.


Literal search/replace are case-sensitive and non-regex.


Indexing and counts use Unicode code-point positions.


find_all (literal) → indices; find_all like (regex) → substrings.


Core regex has no flags; use raw patterns (raw "...") to avoid double-escaping.


split/join require string types (enforced); empty delimiter ⇒ ValueError.


Extraction helpers return nil when delimiters are missing.



Quick Reference
Literal
s.has "t" · s.find "t" → idx|nil · s.find_all "t" → [idx...] · s.count "t"


s.replace A with B · s.replace_first A with B · s.remove A (.drop alias)


before X in s · after X in s · before_last X in s · after_last X in s · between A and B in s


Tokenization
s.lines / lines s · s.words / words s · s.chars / chars s


split S on D → array (keeps empties); join A with S → string


Regex (core subset)
find_all like P in s → [match, ...]


match like P in s → bool


replace like P with N in s → string


replace_first like P with N in s → string


Subset: . [] () | ^ $ + * ? {m,n} \d \w \s (case-sensitive). Prefer raw patterns.


Full PCRE: enable the regex glam (flags, lookarounds, named groups).


Types & Errors
ValueError: empty delimiter in split; out-of-range args (where applicable).


TypeError: non-string in split/join inputs.


All functions are pure (no mutation).


24. Modules & Imports
Modules split code across project files and provide namespaced reuse without glams. Importing is explicit, always aliased, and governed by the Modules policy. Visibility is policy‑driven ("open" vs "locked") and can also be set per symbol with expose/vault.
Design differences: Goblin has no global/package search path for modules, no implicit re‑exports, and no "magical" namespace merges. You import files by path and refer to their contents via Alias::symbol only (keep . for methods/properties). All project modules live under the fixed modules/ directory (created by goblin new); the directory name cannot be changed and imports cannot escape it.
PATCHES:
#24 Modules — path anchoring (answering your “Yes?”)
Project modules live under the fixed modules/ root (created on new projects; name is not configurable).


Import paths are anchored to that root, not to the importing file’s directory. You cannot hop outside with .. or absolute paths.


Example:

 import "./helpers" as H        /// resolves to modules/helpers.gbln
import "./utils/slug" as U     /// resolves to modules/utils/slug.gbln


If you truly need code outside modules/, make it a GLAM or move it under modules/. This keeps packaging, sandboxing, and CLI symmetry reliable.


24.1 Policy Model (Visibility)
Two visibility modes, selected by the active Modules policy:
expose mode (open): top‑level declarations are importable unless marked vault.


vault mode (locked): nothing is importable unless marked expose.


Policy can be set at three levels; inner overrides outer:
Project default (e.g., in policies.gbln):


set @policy project_default
    modules: { mode: "expose" }
end

File‑level: first non‑comment statement in the file.


set @policy locked_modules   /// modules: { mode: "vault" }

Scoped/inline: inside an op/class/etc.


op build()
    set @policy locked_modules   /// applies within this scope
    ...
end

Import effect: If a file is effectively vault under the resolved policy and a symbol isn’t expose, attempts to import it fail with a policy/visibility error.

24.2 Modules Directory & Import Basics
Rooted at modules/: All imports resolve under <project>/modules/.


import "./helpers" as H → <project>/modules/helpers.gbln


import "./util/math" as M → <project>/modules/util/math.gbln


The literal directory name modules is fixed; Goblin creates it for new projects. You cannot rename or remap it.


The .gbln suffix is optional.


No escapes/absolutes: .. segments and absolute paths are forbidden.


Every import requires an alias; access via Alias::name.


import "./helpers" as H
say H::slug("Deviant Moon")      /// OK → "deviant-moon"
/// H.slug(...)                    /// ❌ Wrong: `.` is for methods on values

/// Forbidden paths
import "../other" as O            /// ❌ ModulePathError (cannot escape modules/)
import "/etc/passwd" as P         /// ❌ ModulePathError (absolute path)

Namespace access uses :: only for module symbols. Only values support . for methods/properties.
24.3 Symbol‑Level Modifiers
Mark individual declarations regardless of file mode:
expose op new_id() = uuid()             /// importable
vault  op seed()   = 12345              /// never importable

expose class Product = title: "{t}" :: price: $0

vault enum Mode
    A
    B
end

expose @row = title: "{t}" :: price: "{p}"

Rules:
In expose files: everything is importable except symbols marked vault.


In vault files: nothing is importable except symbols marked expose.


Conflicting redeclarations (same name, different visibility) raise ModuleVisibilityError at definition time.



24.4 Execution & Caching
A module’s top‑level code runs once at its first import; subsequent imports use the cached module instance/exports. Side‑effects follow sandbox policy (FS/NET). In --deterministic builds, disallowed effects fail as usual.

24.5 Aliases & Collisions
Duplicate import … as Alias within one file → ModuleNameConflictError.


Duplicate symbol definitions inside a module → the usual duplicate‑definition error at module load.


Aliases are local to the importing file and do not leak globally.

24.6 Cycles (v1.5)
Import cycles are disallowed in this version. Detection such as A → B → A raises:
ModuleCycleError("A <-> B")

(A future release may relax this with deferred bindings.)

24.7 Interop: Modules vs Glams
Modules: project‑local files → import "./x" as X then X::symbol.


Glams: external capabilities → use glam@ver as g then g::cap.


Both use Alias::symbol, but they originate from different mechanisms and namespaces don’t collide.

24.8 Errors
ModulePathError — path escapes the modules/ root, uses an absolute path, or otherwise violates import path rules.


ModuleNotFoundError — target file not found or unreadable under modules/.


ModuleNameConflictError — duplicate alias in the importer.


ModuleCycleError — cyclic import detected.


ModuleVisibilityError — symbol hidden by vault or conflicting visibility.


PolicyVisibilityError — import denied because the active policy makes the target effectively vault and the symbol isn’t expose.


Goblin‑style messages are allowed (concise, actionable).

24.9 Examples
A) Open by default (easy reuse)
/// policies.gbln
set @policy easy_modules
    modules: { mode: "expose" }
end

/// strings.gbln  (file uses project default → expose)
op slug(s) = s.lower().replace(" ", "-")   /// importable
vault op only_here() = 7                        /// force hidden

/// post.gbln
set @policy easy_modules
import "./strings" as S

say S::slug("Deviant Moon")   /// "deviant-moon"
S::only_here()                 /// ❌ ModuleVisibilityError

B) Locked file with explicit exposes
/// policies.gbln
set @policy locked_modules
    modules: { mode: "vault" }
end

/// ids.gbln
set @policy locked_modules

expose op new_id() = uuid()   /// allowed to import
op seed() = 12345              /// hidden (implicit vault under file vault)

/// main.gbln
import "./ids" as IDs
say IDs::new_id()
IDs::seed()                   /// ❌ ModuleVisibilityError (or PolicyVisibilityError if policy blocks file)

C) Mixed sections in one file
/// tools.gbln
set @policy easy_modules       /// expose section
op slug(s) = s.lower().replace(" ", "-")

set @policy locked_modules     /// vault section
expose op checksum(b) = hash(b, "sha256")
op secret() = "g0b1in"

/// Importer may use tools::slug and tools::checksum, not tools::secret


24.10 Rules & Constraints
Imports are rooted at <project>/modules/; .gbln is optional.


./ denotes the modules/ root; subpaths allowed; no .. and no absolute paths.


The directory name modules is fixed and created by the CLI; it cannot be renamed.


Alias is required; access symbols via Alias::name only.


. is for methods/properties on values; not for module access.


Visibility = policy mode (expose/vault) plus symbol modifiers.


Policy precedence: inner scope > file header > project default.


Module code executes once; subsequent imports are cached.


Cycles are rejected in v1.5.



Module CLI (project‑local)
Modules can expose command‑line operations. The selector is module (GLAMs use glam; see §—).
/// Shape
$ goblin module <alias> <op> [args]

/// Help
$ goblin help                  /// top‑level manual
$ goblin help module           /// module CLI manual
$ goblin module <alias> help   /// ops exposed by one module

Aliases (CLI) come from your project manifest. The alias points to a path under the fixed modules/ directory.
# goblin.toml (project)
[modules]
scaff = "modules/scaffolder"   /// alias → module path (under modules/)
media = "modules/media_tools"

# usage
$ goblin module scaff build  /// calls the exposed op `build` in modules/scaffolder

Visibility & contracts:
Only **expose**d ops are callable from the CLI and they must be declared in the manifest with JSON Schemas.


The CLI validates args against the input schema, renders --help from the schemas, and pretty‑prints outputs.


# goblin.toml (snippets)
[modules.scaff.ops.build]
input_schema  = "modules/scaffolder/schemas/build.json"   /// JSON Schema (inputs)
output_schema = "modules/scaffolder/schemas/build.out.json"

[modules.scaff.capabilities]
fs.read  = ["./templates", "./schemas"]    /// RO mounts
fs.write = ["./work", "./out"]             /// RW mounts
# env   = ["API_KEY"]                      /// explicit env whitelist
# network = { domains=["api.example.com"], methods=["POST"] }

Capability sandbox (modules): same model as GLAMs. No ambient FS/ENV/NET. Reads and writes are constrained by the module’s declared capabilities; writing to a read‑only mount fails.
$ goblin module scaff build --name "Blog"
/// CapabilityDeniedError (outside declared FS/NET/ENV)
/// ReadOnlyPathError      (attempt to write under fs.read)

Reserved verbs: spawn, build, export, push, lint, test, info, help. Custom verbs are allowed; declare them under [modules.<alias>.ops.<verb>].
CLI‑facing errors (modules):
ModuleNotFoundError — alias not found in [modules] or target path missing.


ModulePathError — path escapes modules/ (e.g., .. or absolute path).


OpNotExposedError — symbol not exposed in code or not listed under [modules.<alias>.ops].


ModuleVisibilityError / PolicyVisibilityError — hidden by vault or policy.


CapabilityDeniedError — access outside declared capabilities.


ReadOnlyPathError — write attempted to a read‑only mount.


Quick Reference
Root & Paths
Root: <project>/modules/ (fixed; created by goblin new).


import "./helpers" as H → modules/helpers.gbln.


.gbln optional; no ..; no absolute paths.


Import
import "./path" as A — rooted at modules/.


Use A::name to access symbols (keep . for methods on values).


Visibility
Policy modes: modules: { mode: "expose" | "vault" }.


Modifiers: expose / vault on op, class, enum, rows, etc.


Precedence: inner scope > file header > project default.


Execution
Top‑level runs once on first import; later imports are cached.


Interop
Modules: import … as X → X::symbol.


Glams: use glam@ver as g → g::cap (separate mechanism).


Errors
ModulePathError, ModuleNotFoundError, ModuleNameConflictError, ModuleCycleError, ModuleVisibilityError, PolicyVisibilityError.


25. Pipelines & Optional Chaining
Pipelines linearize data flow; optional chaining makes nil-safe navigation explicit. Goblin prefers readability and explicitness over implicit magic: |> rewrites call sites left→right, and ?. short‑circuits on nil without catching other errors. This section defines both operators, their desugarings, precedence, and constraints.
Pipeline Operator |>
Definition & Desugaring
/// A |> f(x, y: k)  ≡  f(A, x, y: k)
score |> clamp(0, 100)          /// clamp(score, 0, 100)
user  |> save(debug: true)       /// save(user, debug: true)

Reasoning. Nested calls read inside‑out; pipelines read like a recipe. The operator is purely syntactic sugar; it does not change arity or evaluation rules beyond argument reordering.
Associativity & Precedence
A |> f() |> g(x)                /// g(f(A), x)   (left‑associative)

score + bonus |> fmt()          /// fmt(score + bonus)  (arithmetic before |>)
result |> render() || " pts"   /// (render(result)) || " pts"  (|> before string joins)
obj.method() |> t()             /// t(obj.method())     (call/access before |>)
config?.value |> check()        /// check(config?.value)

Associativity: left‑to‑right.


Precedence (from high → low among these): property/method access & calls > arithmetic > |> > string joins (| and ||).


Callable on the Right
The right‑hand side must be an operation (or callable) whose first parameter can accept the piped value. Otherwise an ArityMismatch (or type) error is raised at compile time when resolvable, or at runtime when dynamic.
player |> process(mode: "fast")    /// ok if op process(v, mode:)
player |> Math.sqrt()               /// error unless sqrt(v) is defined

Lambda Escape Hatch
Use a lambda to place the piped value anywhere in the argument list or to adapt shapes.
base |> (v -> apply(target: enemy, amount: v, type: fire))

temp |> (t -> clamp(min: 0, max: 100, value: t))

Nil Propagation is not Automatic
|> does not treat nil specially; it passes whatever is on the left.
nil |> process()                 /// calls process(nil)

/// Combine with optional chaining or a fallback when needed
player?.inventory |> sort_items()          /// stops before calling if player is nil
value |> normalize() ?? "(missing)"       /// coalesce after the call

Examples
raw
  |> decompress()
  |> parse_save()
  |> validate_player()
  |> migrate_to_current()
  |> load_into_game()

base_damage
  |> apply_weapon_mods()
  |> apply_stats()
  |> apply_resistances()
  |> maybe_crit()
  |> display_damage()

input
  |> trim()
  |> validate_command()
  |> parse_args()
  |> exec()
  |> format_response()

Rules & Constraints (Pipeline)
A |> f(args) rewrites to f(A, args); no other semantics change.


Right side must be callable with the piped value as first parameter (else ArityMismatch / type error).


Evaluation order: left operand, then resolve/capture the callable, then evaluate the argument list, then call.


Precedence: calls/access > arithmetic > |> > string joins.


|> does not swallow nil or errors; combine with ?./?? explicitly.


Optional Chaining ?.
Purpose & Semantics
Optional chaining performs nil‑guarded member access and calls. If the left side is nil, the whole expression yields nil and short‑circuits (arguments to a skipped call are not evaluated). If the left side is non‑nil, normal lookup/invocation occurs and any error from that evaluation propagates.
player?.inventory?.weapons?.primary?.damage   /// nil if any link is nil

user?.profile()?.avatar_url                   /// calls profile() only if user != nil
conn?.close()?.then()                         /// chains through safely

Short‑Circuiting of Arguments
result = obj?.do(expensive())    /// if obj is nil, expensive() is not evaluated

Not an Error Handler
?. guards only against nil. If a property/method is present but its evaluation throws, that error still surfaces.
player?.inventory?.break_item()  /// if break_item throws, the error propagates

With Pipelines & Fallbacks
Combine ?. with |> and ?? to express “try, then default.”
user
  |> fetch_profile()                         /// may return nil
  |> (p -> p?.address?.city)                 /// safely extract nested field
  |> (city -> city ?? "Unknown")            /// fallback value

Rules & Constraints (Optional Chaining)
Forms: E?.prop, E?.method(args...), repeated as needed.


If E is nil, the result is nil and argument expressions are not evaluated.


If E is non‑nil: normal access/call semantics apply; any resulting error propagates.


?. does not catch non‑nil errors and does not test for existence beyond nil.


Quick Reference
Pipeline |>
Desugar: A |> f(x, y: k) → f(A, x, y: k)


Assoc/Prec: left‑assoc; calls/access > arithmetic > |> > string joins


RHS must accept the piped value as first parameter (else ArityMismatch / type error)


Nil is passed through; use ?./?? for safety


Lambda escape: A |> (v -> g(a: v, b: 3))


Optional Chaining ?.
Forms: E?.p, E?.m(args...), chains allowed


If left is nil, result is nil (no argument evaluation)


If left is non‑nil, normal access/call; errors propagate


Compose with pipelines: x |> (v -> v?.m()) |> (r -> r ?? def)


26. Error Handling
Terminology note. If you are used to try/catch/finally, Goblin uses the same control structure with clearer names: attempt (do the risky work), rescue (handle specific error types), and ensure (always‑run cleanup). The semantics match try/catch/finally; the identifiers are chosen to make intent explicit in code.
Goblin uses explicit, expression‑oriented error control. There are no implicit try/catch blocks or hidden conversions: you declare what can fail and how failures are handled. The construct is attempt / rescue / ensure, designed to be readable, composable, and safe.
Why this design? Silent failures and implicit exception flows make programs brittle. Goblin’s block is an expression with a clear result, typed by the branch that executed. Cleanup is explicit and guaranteed. Errors that you don’t handle propagate unchanged (after cleanup), preserving debuggability.

attempt / rescue / ensure
attempt encloses code that may raise. rescue arms handle specific error types (first match wins). ensure always runs—success or failure—and is ideal for releasing resources.
result =
  attempt
    user_input
      .trim()                 /// any code that might raise
      .validate()             /// last expression becomes result on success
    "ok"                      /// explicit last value
  rescue NetworkError as e
    say "Network failed: {e.message}"
    "offline"                /// this branch’s last value
  rescue ValidationError as e
    say "Bad data: {e.message}"
    "default"
  ensure
    metrics.record("attempted_validate")
  end

Semantics
The block is an expression. Its value is the last expression from the arm that ran: the attempt body if no error, else the first matching rescue body. ensure has no value; it always runs.


Match is by error type (including derived types). The first matching rescue wins.


If no rescue matches, the error propagates after running ensure.


Multiple rescues
save_state =
  attempt
    state.serialize().compress().fs.write_bytes("save.bin")
    true
  rescue DiskFullError as e
    say "Disk full → staging to cloud"
    cloud.stash(state)        /// may itself raise
  rescue NetworkError as e
    cache.write(state)
    false
  ensure
    locks.release("save")
  end

Propagation & rethrow
Use bare raise inside a rescue to rethrow the original error (stack preserved).
attempt
  critical.step()
rescue TransientError as e
  backoff.sleep()
  raise                 /// rethrow the same error (keeps stack)
rescue FatalError as e
  audit.log("fatal: {e.message}")
  raise
end

Cleanup that always runs
Variables that might not be bound are safely handled with optional chaining.
file = nil
config =
  attempt
    file = fs.open("config.json")
    file.read_text().json.parse({})
  rescue FileNotFoundError
    defaults.config()
  rescue ValueError as e         /// malformed JSON
    backups.restore("config.json")
  ensure
    file?.close()                /// runs whether success or error, and only if opened
  end


Interplay with Pipelines & Optional Chaining
The block composes with pipelines (§25) and ?. navigation (§25):
profile_theme =
  attempt
    raw
      .decompress()
      .json.parse()
      .get("profile")
      .get("settings")
      ?.get("theme")           /// short‑circuit on null
      ?? "default"
  rescue ValueError as e        /// parse/decode failures
    "default"
  ensure
    metrics.record("profile_theme_lookup")
  end

Guidance
Optional chaining avoids null crashes; it does not catch exceptions. Use attempt for exceptions.


Keep pipelines pure inside attempt; do side‑effects in ensure or after success.



Patterns
Retry with backoff (rethrow to caller)
attempt
  api.fetch(id).validate()
rescue NetworkTimeout as e
  backoff.jittered(tries: 3)      /// sleep & mutate internal counter
  raise                           /// let caller decide after retries
end

Fallback data on validation errors
processed =
  attempt
    data.clean().validate().transform()
  rescue ValidationError as e
    say "invalid input: {e.message}"
    caches.load("last_good")
  end

Guard long‑running resources
conn = nil
result =
  attempt
    conn = db.connect(url)
    conn.txn().run(query)
  rescue DatabaseError as e
    audit.db_error(e)
    nil
  ensure
    conn?.close()
  end


Rules & Constraints
attempt / rescue / ensure / end is a single expression.


ensure always executes and has no value; it cannot alter which arm’s value is returned.


Matching in rescue is by type, first match wins; unhandled errors propagate after ensure.


as name binds the error object for that arm; its scope is the rescue body only.


raise (bare) inside a rescue rethrows the current error (stack preserved). Passing a new error value is reserved for a later revision.


Do not use ensure to mask failures; it must be side‑effect/cleanup only.


Method‑style only: use receiver.method(...) in examples and code. Free‑function forms are non‑canonical.



Quick Reference
Construct
value =
  attempt
    work()
  rescue SomeError as e
    recover()
  ensure
    cleanup()
  end

Semantics
value = last expression of the executed arm (attempt or first matching rescue).


ensure always runs; unhandled error rethrown after ensure.


Idioms
Rethrow: raise inside rescue.


Optional close: handle?.close().


Fallback: ... ?? default after ?. chains.


Composes with
Pipelines: x |> .validate() |> .transform() inside attempt.


Optional chaining: obj?.field?.call() in any arm.


Common Errors
TypeError — non‑callable in pipeline/chain.


ArityMismatch — wrong method parameters.


Any domain‑specific error (e.g., ValueError, NetworkError, DiskFullError) you choose to raise.


Design Notes
No implicit catches; no swallowed exceptions.


Deterministic cleanup and unchanged stack traces on rethrow.


27. Morph — Temporary Type Adaptation
Morph lets an instance of one class be temporarily treated as another class for exactly one method call, syncing overlapping public fields before and after the call. It is a controlled, transactional adaptation—no private state leaks, no permanent type change.
Why Morph? It enables reuse of existing methods (pricing, validation, transforms) without writing adapters or duplicating logic. Behavior is deterministic; the only nondeterminism you can observe is whatever the invoked method itself performs (and that remains subject to the sandbox/capabilities policy).
Patches::
#27 Morph — advanced/optional
Keep as-is but banish by default in new app templates:

 [[banish]]
feature = "core.morph"
reason  = "Advanced feature; enable intentionally."


Leave it fully specified for power users; everyone else won’t see it accidentally.


Signature & Semantics
result = morph(obj, TargetType, method_call)
/// obj:        source instance (its class stays unchanged)
/// TargetType: class whose method will run
/// method_call: exactly one public method *of TargetType*
/// returns:    the method's result; obj's overlapping public fields may be updated

morph(...) is a language form (not a user-definable function). It copies overlapping public fields into a temporary TargetType instance, invokes the method, then copies overlapping fields back to obj atomically.


If any step fails (type mismatch, visibility, thrown error), everything rolls back and obj is unchanged.



Public Accessor Contract
Only fields with public getter+setter pairs on both classes are synced. Names must match under these conventions:
x() / set_x(v)


is_x() / set_x(v) (booleans only)


Fields lacking either accessor on either side are ignored (not synced).

Execution Model (Atomic)
Validate TargetType is a class and method_call names a public method on it.


Discover overlapping fields by matching accessor names.


Create a temporary TargetType instance using the first available initializer:


init() (no-arg), or


from_map(map) (preferred for complex types), or


zero-arg constructor.


Copy-in: move overlapping public field values from obj → temp target.


Invoke the one allowed method on the temp target.


Copy-out: move overlapping public field values from temp target → obj.


Re-validate field types; on any incompatibility, rollback to the pre-morph state and raise an error.


All updates behave as a single transaction.

Type Compatibility Rules
Primitives: exact kind match (int↔int, float↔float, etc.).


Money: allowed only if currencies match; precision is reconciled per active money policy (§18). Otherwise → MorphCurrencyError.


Date/Time: must be the same temporal kind (date↔date, datetime↔datetime, time↔time, duration↔duration).


Arrays / Maps: element/value types must match exactly.


Enums / Classes: must be of the same declared type.


nil: permitted only if the destination setter accepts nil.



Scope & Visibility
Only public methods may be invoked.


Only public accessors participate in sync.


Morph never touches #private state and does not bypass access control.



Performance
Overlapping-field discovery may be cached per (SourceType, TargetType) pair.


Complexity is proportional to the number of shared fields. Typical use is fast; prefer from_map for large objects.



Errors (Transactional)
All morph errors leave obj unchanged.
MorphTypeError — invalid target, or method not found / not public.


MorphFieldError(field, expected, actual) — incompatible field types.


MorphCurrencyError(field, from_cur, to_cur) — currency mismatch in money field.


MorphActionError(cause) — target method threw an error.



Examples
Geometry reuse (rotate)
class Dot = x:0 :: y:0
    fn x() = #x; fn set_x(v) = #x = v
    fn y() = #y; fn set_y(v) = #y = v
end

class Shape = x:0 :: y:0
    fn x() = #x; fn set_x(v) = #x = v
    fn y() = #y; fn set_y(v) = #y = v
    fn rotate(deg)
        (#x, #y) = (#x - deg, #y + deg)
        self
    end
end

p = Dot: 1 :: 0
morph(p, Shape, rotate(90))
say p.x(), p.y()   ///  -89, 90

Reuse a pricing method across types
class Book = title:"" :: price:$0
    fn price() = #price
    fn set_price(v) = #price = v
end

class Card = name:"" :: price:$0
    fn price() = #price
    fn set_price(v) = #price = v
    fn apply_discount(rate) = #price = #price * (1 - rate); #price
end

b = Book: "Guide" :: $29.99
morph(b, Card, apply_discount(10%))
say b.price()      /// $26.99

Boolean accessor bridging
class A = active:true
    fn active() = #active
    fn set_active(v) = #active = v
end

class B = enabled:false
    fn is_active() = #enabled
    fn set_active(v) = #enabled = v
    fn toggle() = #enabled = not #enabled
end

a = A: true
morph(a, B, toggle())
say a.active()     /// false


Testing Hooks
test "morph discount works"
    b = Book: "Gloomhaven" :: $100
    r = morph(b, Card, apply_discount(25%))
    assert b.price() == $75
    assert r == $75
end


Determinism
Morph is deterministic and side-effect–free apart from the one method you explicitly invoke. Any I/O or nondeterminism inside that method is governed by the runtime sandbox/capabilities policy.

Quick Reference
Form: result = morph(obj, TargetType, method_call)


Sync scope: Overlapping public accessors only (x()/set_x, is_x()/set_x for booleans).


Atomicity: Copy-in → call → copy-out, or rollback on any failure.


Type rules: primitives exact; money same currency (precision via policy); date/time same kind; arrays/maps element types match; enums/classes same type; nil only if setter accepts.


Visibility: method must be public; private state never read/written.


Errors: MorphTypeError, MorphFieldError, MorphCurrencyError, MorphActionError.


Performance: cache shared-field maps per (SourceType, TargetType).


27. Morph — Temporary Type Adaptation (Core)
Purpose. morph lets an instance be temporarily treated as another class for exactly one public method call. Public fields shared by the two classes are copied into a temporary target instance, the method runs, and then compatible field changes are copied back atomically. No private state ever leaks.
Design. This is a scoped, transactional adapter:
Reuse existing APIs without inheritance, casts, or permanent conversion.


Transactional copy‑in/call/copy‑out with rollback on any error.


Public‑only: works strictly via public accessors; private # state is invisible.


Deterministic: behavior is fully defined; side‑effects are those of the called method and subject to sandbox/policy.



27.1 Signature & Semantics
/// Temporary adaptation for one call; returns the method's result
result = morph(obj, TargetType, method_call)

obj — source instance (remains its original class).


TargetType — class from which you want to call a public method.


method_call — exactly one public method invocation that exists on TargetType.


Return — whatever the method returns (note: if it returns self, that is the temporary TargetType instance, not obj).


Single‑call rule. method_call must be a single call expression with its arguments. Chaining or additional statements are not allowed inside morph.
Copy semantics. Copy‑in/out is shallow and uses normal assignment semantics:
For reference fields (arrays, maps, objects), references are transferred; nested objects are not deep‑cloned.


Money/date types remain their exact types; no stringification.


Atomicity. Copy‑back uses a two‑phase apply; if any setter fails or a type check trips, all changes are rolled back and an error is raised.

27.2 Accessor Conventions (Public‑Only)
Fields participate only if both classes expose compatible public accessors for the same logical field name.
Accepted getter/setter shapes (booleans include the is_ form):
x()        / set_x(v)
is_x()     / set_x(v)   /// booleans only

Normalization. The logical field name is the identifier after any optional is_ prefix. Example: is_active()/set_active(v) ↔ active()/set_active(v) map to field active.
Participation rule. A field is copied only if both classes provide both a compatible getter and setter for that logical name. Read‑only or write‑only fields are ignored.
No custom mappings. Name remapping (e.g., enabled ↔ active) is not supported; write explicit adapter code if you need it.

27.3 Process (Normative)
Validate TargetType is a class and method_call names a public method on it.


Discover shared fields by accessor matching and type compatibility (see §27.4).


Construct temporary target instance using the first available path:


call zero‑arg init() if present; else


call from_map(map) if present; else


call a zero‑arg constructor; else raise MorphTypeError.


from_map(map) receives a map of shared fields only (logical name → value) from the source.


Copy‑in values for shared fields from source → temp target via public setters.


Invoke the specified public method on the temp target.


Copy‑out updated shared fields from temp target → source via public setters.


Commit or rollback: if any setter/type check fails at any stage, rollback source to its pre‑morph state and raise an error.


Return the method’s return value.



27.4 Type Compatibility
Primitives: exact kind match (int, float, big, bool, string).
Money: allowed only if currencies match; precision is reconciled by the active money policy at the call site (see Money §18). Currency mismatch → MorphCurrencyError.
Date/Time: must be the same kind (date↔date, datetime↔datetime, time↔time, duration↔duration).
Arrays/Maps: element/value declared types must match (structural equality). Copy is shallow (references shared).
Enums/Classes: must be the same type. Subtype/supertype is not implicitly accepted.
nil: allowed only if the setter’s parameter type admits nil.

27.5 Scope & Visibility
Only public methods may be called by morph.


Only public getters/setters participate in field transfer.


Private members (#field, private methods) are never inspected or mutated.



27.6 Errors (All Atomic)
MorphTypeError — invalid TargetType, method not found, or no valid constructor path.


MorphFieldError(field, expected, actual) — incompatible field type during copy‑in/out.


MorphCurrencyError(field, from_cur, to_cur) — money currency mismatch.


MorphActionError(cause) — target method threw; includes original error as cause.


All morph errors guarantee the source object remains unchanged.

27.7 Performance
Shared‑field discovery may be cached per (SourceType, TargetType).


Time complexity is proportional to the number of shared fields.


Copy‑in/out uses normal public setters; there is no reflective access to private state.



27.8 Examples
Rotate via Compatible Geometry Fields
class Dot = x:0 :: y:0
    fn x() = #x; fn set_x(v) = #x = v
    fn y() = #y; fn set_y(v) = #y = v
end

class Shape = x:0 :: y:0
    fn x() = #x; fn set_x(v) = #x = v
    fn y() = #y; fn set_y(v) = #y = v
    fn rotate(deg) = (#x, #y) = (#x - deg, #y + deg); self
end

p = Dot: 1 :: 0
morph(p, Shape, rotate(90))
say p.x(), p.y()

Reusing a Discount Method Across Types (Money‑aware)
class Book = title:"" :: price:$0
    fn price() = #price
    fn set_price(v) = #price = v
end

class Card = name:"" :: price:$0
    fn price() = #price
    fn set_price(v) = #price = v
    fn apply_discount(rate) = #price = #price * (1 - rate); #price
end

b = Book: "Guide" :: $29.99
morph(b, Card, apply_discount(10%))
say b.price()            /// $26.99 (currency must match; money policy applies)

Boolean Accessor Normalization (is_ ↔ bare)
class A = active:true
    fn active() = #active
    fn set_active(v) = #active = v
end

class B = enabled:false
    fn is_active() = #enabled
    fn set_active(v) = #enabled = v
    fn toggle() = #enabled = not #enabled
end

a = A: true
morph(a, B, toggle())
say a.active()           /// false


27.9 Determinism & Sandbox
morph itself is deterministic and side‑effect free aside from:
the effect of the invoked public method on the temporary target, and


the public copy‑back into the source.


All file/network/env effects come only from that method and remain governed by the runtime capability sandbox and policies active at the call site.

27.10 Rules & Constraints
Single public method call only; no chaining inside morph.


Copy‑in/out is shallow; nested references are shared.


Only fields with both getter and setter on both classes participate.


No name remapping; logical names must match (with is_ normalization for booleans).


Money precision/currency follow the active policy; mismatched currencies are errors.


Any error during construction, invocation, or copy‑back → rollback and raise.



Quick Reference
Call: result = morph(obj, TargetType, method(args...))


Fields: match by public accessors x()/set_x(v) (and is_x() for booleans)


Copy: shallow; public setters only; two‑phase commit; rollback on error


Types: exact primitive; same enum/class; arrays/maps element types match; same‑kind datetime; money requires same currency


Errors: MorphTypeError, MorphFieldError, MorphCurrencyError, MorphActionError


Policy: money precision per active policy at call site


Privacy: private # state never touched; only public methods/fields


28. Gmark — Project‑Local Stable References
Goblin projects often need stable, human‑readable identifiers for content (posts, pages, products, entries) that survive file moves/renames and provide a global ordering. A gmark supplies that: a persistent record { name, ord, id, created, updated } stored under the project’s .goblin state.
Why gmarks? Paths change, links shouldn’t. Gmarks decouple identity (name) and sequence (ord) from filesystem layout, so modules/glams can reorganize content without breaking internal references. The registry is atomic, audited, and policy‑controlled for deterministic builds.

Model & Persistence
Record fields
name — string key (e.g., "post/how-to-play", "how-to-play").


ord — project‑wide integer for stable ordering (monotonic append default).


id — opaque unique token for registry internals (not user‑facing).


created / updated — timestamps (engine‑managed).


Storage
Registry path: .goblin/gmarks.lock (JSON; single‑writer; fsync on commit).


Atomic writes; mutex guarded.


Deterministic builds: read‑only unless policy explicitly allows allocation/mutation.


/// Shape of the lock file (illustrative)
{
  "last_ord": 137,
  "marks": {
    "post/how-to-play": { "ord": 121, "id": "gm_8C3...", "created": "...", "updated": "..." },
    "post/faq":         { "ord": 122, "id": "gm_F91...", "created": "...", "updated": "..." }
  }
}


Construction & Updates (Method‑Style)
Gmark APIs are method‑style for consistency:
/// Ensure/create (auto ord): returns { name, ord, id, created, updated }
"post/how-to-play".gmark()

/// Ensure/create with explicit ord (must be free)
"post/how-to-play".gmark(ord: 42)

/// Idempotent: same name → same record unless ord conflicts
r1 = "post/how-to-play".gmark()
r2 = "post/how-to-play".gmark()   /// r2.id == r1.id

/// Inspect
info = "post/how-to-play".gmark_info()  /// map | nil

/// Update ord (target ord must be unused)
"post/how-to-play".gmark_set_ord(200)

Listing & Querying
Use the project gmarks service for aggregates:
all = gmarks.all()                 /// [{ name, ord, id }] ord‑sorted
next = gmarks.next_ord()           /// peek last_ord + 1 (no allocation)
post_only = gmarks.filter("post/") /// ord‑sorted subset by prefix


Naming Rules
Allowed characters: letters, digits, _, -, /, .
No leading/trailing slash; no empty segments ("post//x" invalid).


Max length: 256 characters.


Case‑sensitive.


Must not collide with reserved words (§19).


Violations → GmarkInvalidError at call site.

Semantics & Ordering
Omit ord on ensure → registry assigns last_ord + 1.


Explicit ord → must be free; else GmarkConflictError.


Re‑ensuring the same name is idempotent (returns existing record) unless the call requests a conflicting ord.


Reordering: name.gmark_set_ord(n) moves that one entry; other ords are unchanged. Bulk rebalancing is CLI‑only (see Tools).



Determinism, Policy & Auditing
In --deterministic builds, writes are blocked unless policy grants explicit permission.
set @policy ci_lockdown
  state: { deterministic: true }
end

set @policy allow_gmark_writes
  state: {
    deterministic: true,
    allow_writes: ["gmark"]        /// narrowly whitelists gmark allocations
  }
end

When writes are allowed, the engine appends JSONL audit entries to .goblin/gmarks.audit.log with { before, after, ts, actor, op, lock_checksum }.
/// Safe allocate with audit in allowed mode
post = "post/hello-world".gmark()    /// auto → next ord
pin  = "post/welcome".gmark(ord: 1)  /// manual pin

Reads are always permitted.

Integration: Linking & Display
Core only guarantees stable keys + sequence. Modules/Glams map marks to URLs/paths for presentation.
/// Example: a blog glam/module resolving marks → URLs
href = blog::href(gmark: "post/how-to-play")      /// "/posts/how-to-play"
link = blog::link(text: "How to Play", gmark: "post/how-to-play")

A typical listing view:
for m in gmarks.filter("post/")
  say blog::link(
    text: m.name.replace("post/", "").title(),
    gmark: m.name
  )
end


Errors
GmarkConflictError — duplicate name on create or requested ord already taken.


GmarkNotFoundError — name absent for operations that require existence.


GmarkInvalidError — name violates format/reserved rules.


GmarkPersistenceError — registry unreadable/unwritable.


DeterminismError — attempted allocation/mutation when writes are blocked by policy.


All mutations are atomic; on any failure the registry remains unchanged.

Tools (CLI reference)
Some maintenance is CLI‑only to keep the runtime minimal and deterministic:
/// Rebalance ords within a prefix (spacing, gaps) — provided by tooling
# goblin glam gmark rebalance --prefix post/

This does not change names; it compacts ords in prefix scope while preserving relative order. Use during migrations or large reorganizations.

Examples
Auto vs. Manual
post = "post/hello-world".gmark()       /// auto → ord N
pin  = "post/welcome".gmark(ord: 1)     /// manual pin at start

Stable List
for m in gmarks.all()
  say m.name || "@" || str(m.ord)
end

Move Later
target = gmarks.next_ord() + 10
"post/hello-world".gmark_set_ord(target)


Rules & Constraints
Method‑style API: "name".gmark(...), gmarks.all().


Names must satisfy the Naming Rules above; validate at the call site.


Allocation is idempotent by name; explicit ord must be free.


Registry lives at .goblin/gmarks.lock; writes are atomic.


Deterministic builds are read‑only unless policy whitelists gmark writes.


Bulk renumbering is tooling/CLI, not runtime.



Quick Reference
Ensure / Inspect / Reorder
"name".gmark() → ensure; auto ord.


"name".gmark(ord: N) → ensure; fixed ord (free only).


"name".gmark_info() → { name, ord, id, created, updated } | nil.


"name".gmark_set_ord(N) → move to N (must be free).


List & Query
gmarks.all() → all marks, ord‑sorted.


gmarks.filter(prefix) → subset, ord‑sorted.


gmarks.next_ord() → peek last_ord + 1.


Naming Rules
Allowed: letters, digits, _, -, /, .; no leading/trailing /; no empty segments; max 256; case‑sensitive; not reserved.


Determinism/Policy
Default: reads OK; writes blocked in --deterministic.


Allow writes via policy whitelist: state.allow_writes: ["gmark"].


Audit log: .goblin/gmarks.audit.log (JSONL).


Errors
GmarkConflictError, GmarkNotFoundError, GmarkInvalidError, GmarkPersistenceError, DeterminismError.


Linking (by consumer modules/glams)
blog::href(gmark: name) → URL string.


blog::link(text: T, gmark: name) → rendered link.


29. Banish — Project‑Local Feature Blocking
Goblin provides a project‑local safety brake to prohibit specific language features. Banish rules live in your repo, compile with your code, and surface as clear, actionable diagnostics. Banish never rewrites code; it only blocks usage.
Why: Large codebases and regulated domains sometimes need to forbid sharp tools (e.g., experimental ops, unsafe I/O forms) until policy or version gates are satisfied. A compile‑time, auditable switch keeps teams aligned without forks or patching the language.

Design & Model
Scope: Project‑local only; rules reside in the repository and travel with it.


Effect: Any use of a banished feature is a hard compile error (BanishError).


Determinism: Enforcement is pure and reproducible; it never alters program semantics.


Diagnostics: IDE/LSP mirrors compiler errors at the call site with the stated reason.



Feature IDs (What you can banish)
Feature IDs are stable strings that denote usage sites, not installation state.
core.<keyword> — core language constructs (e.g., core.morph, core.judge.inline).


op.<operator> — operators/forms (e.g., op.pipe_join, op.divmod, op.postfix.inc).


type.<type> — types/literals (e.g., type.money, type.percent, type.datetime).


builtin.<function> — built‑ins and std helpers (e.g., builtin.tax, builtin.write_json).


glam.<ns>.<symbol> — glam capabilities (e.g., glam.html.raw).


⚠️ The ban applies to use at call/parse sites. It does not uninstall or mutate dependencies.

Configuration & Persistence
Config path: .goblin.banish.toml (project root; subfolders ignored).


Audit log: dist/banish.log (JSONL; appended on every change). The log is part of your build artifacts and supports traceability.


/// .goblin.banish.toml
[[banish]]
feature = "core.morph"
reason  = "Temporary safety: known issue in v1.5.x"

[[banish]]
feature = "op.pipe_join"
reason  = "Creative constraint: prefer explicit concatenation"

/// dist/banish.log (JSONL, appended atomically)
{"ts":"2025-08-13T15:21:08Z","op":"banish","feature":"core.morph","reason":"Temporary safety"}
{"ts":"2025-08-13T15:22:44Z","op":"unbanish","feature":"core.morph"}

Validation: Missing feature or reason entries → ValueError at config load.

Compiler & Tooling Behavior
On compile/run/lint, Goblin loads .goblin.banish.toml and scans the resolved AST.


If a banished feature is used, fail fast with BanishError including source location.


IDE/LSP surfaces the same diagnostic inline.


When any bans exist, a banner is printed:


/// status banner
⚠ This project has N banished features (run `goblin banish --list`).

Example (compile‑time block):
/// .goblin.banish.toml bans core.morph
morph(book, Card, apply_discount(10%))
/// → BanishError: Feature 'core.morph' has been banished (line X, col Y).
///   Reason: Temporary safety: known issue in v1.5.x
///   Unbanish with: goblin unbanish core.morph


CLI (Project‑local)
Banish is managed via the Goblin CLI; commands edit .goblin.banish.toml and append to dist/banish.log.
goblin banish <feature_id> --reason "<text>"   /// add or replace an entry (reason required)
goblin unbanish <feature_id>                    /// remove an entry
goblin banish --list                            /// print active bans

Transparency: --reason is mandatory for banish.


Audit: Every command appends a JSONL record; writes are atomic.



Non‑Banishable Features (Self‑Protection, v1.5)
The following feature IDs are permanently unbanishable. Listing one in .goblin.banish.toml raises a config‑time ValueError with the message “The goblins won't let you banish '{feature}'”.
Banish infrastructure & config
 core.banish, core.config, core.errors, core.contracts, core.capability_resolution
Sandbox & determinism
 core.sandbox, core.determinism, core.rng.seed, core.trusted_time
Money safety invariants
 core.money.invariants, core.money.policy, core.money.ops.guard
Lockfile & build integrity
 core.lockfile, core.lockfile.hmac, core.build.reproducibility
Language integrity (syntax/types/reserved words)
 core.parser, core.reserved_words, core.type_system
Runtime auditability & security rails
 core.jsonl_logging, core.logging.safety, core.permissions, core.policy.visibility

MVP Constraints (v1)
Project‑local only (no global overlays).


Hard errors only (no warn/severity levels).


Flat list of { feature, reason } entries.


No dependency scanning (rules apply to your app code only).


No expiry/version gates (manual unbanish when ready).


Future ideas (non‑MVP): severity levels, version‑gated bans (e.g., “until ≥1.6”), dependency scope (deps|all), presets.

Examples
/// Safety brake for an experimental construct
goblin banish core.morph --reason "Edge-case bug; waiting for fix"

/// Team convention: disallow pipe-join in this repo
goblin banish op.pipe_join --reason "Prefer explicit string building"

goblin banish --list        /// show active bans
goblin unbanish core.morph  /// remove a ban when safe

/// Compile-time enforcement in code (illustrative)
name = "Deviant Moon"
slug = name.lower().replace(" ", "-")
/// name | "-" | id        /// would hit op.pipe_join if that operator were used here


Rules & Constraints
Banish is pure enforcement: it never rewrites source or alters evaluation.


Enforcement triggers on use sites; symbols may still be defined/installed.


Config must include both feature and reason per entry.


Attempting to ban a non‑banishable feature → ValueError at config load.



Quick Reference
Config
Path: .goblin.banish.toml (root only).


Entry: [[banish]] feature = "…" reason = "…" (both required).


Audit: dist/banish.log (JSONL, append‑only).


CLI
goblin banish <feature_id> --reason "…"


goblin unbanish <feature_id>


goblin banish --list


IDs
core.*, op.*, type.*, builtin.*, glam.*


Errors
BanishError — use of a banished feature.


ValueError — invalid config (missing fields or non‑banishable IDs).


Behavior
Compile/lint fail on use; IDE mirrors diagnostics.


Banner when any bans are active.


Project‑local, deterministic, audit‑logged.


30. Policies — Project “Loadouts”
Policies are named loadouts that centralize behavioral switches for the engine (money, datetime, modules, strings). They remove scattered toggles and make intent explicit and auditable.
Semantics. Engines read an immutable snapshot of the active policy at script load and whenever set @policy … executes; reads are O(1). Unset fields inherit from project config (goblin.config.yaml).

Where Policies Live
Create policies.gbln at the project root. It is auto‑loaded; no imports required. Define named policies with maps for each domain.
/// policies.gbln (skeleton)
@policy = name: "{name}" :: money: {} :: modules: {} :: strings: {} :: datetime: {}

@policy = "site_default"
    money:   { display_precision: 2, max_precision: 8, rounding: "truncate", policy: "warn", rounding_timing: "per_tx" }
    modules: { mode: "expose" }              /// expose | vault (see §24)
    strings: { trim: true, strip_html: false, escape: false }
    datetime:{ tz: nil, prefer_trusted: false, policy: "warn", time_arith: "error", leap_mode: "clamp" }
end


Applying Policies (Scope & Precedence)
Apply with a single statement.
set @policy site_default

Scopes
File header: first non‑blank/comment statement → applies file‑wide.


Inline: anywhere; applies to current block and inner scopes.


Precedence: inline > file header > project default (the project default is the first declared policy or an explicitly configured default in tooling).
Reapplying a policy replaces the active snapshot for subsequent code.

Categories & Fields
30.1 Money (Powered by Big + Policy)
Controls precision, rounding, and conformance when emitting values.
money: {
  display_precision: 2,          /// what users see/export; snap on export/settlement
  max_precision: 8,              /// carried internally; digits beyond → remainder ledger
  rounding: "truncate",          /// truncate | half_even | half_up | ceil | floor
  policy: "warn",               /// warn | strict | defer | truncate (enforcement semantics)
  rounding_timing: "per_tx",    /// per_tx | per_period | at_settlement
  currency: "USD",              /// default ISO code (optional; for constructors)
  compat: {                      /// (optional) external emission quirks; internal math stays exact
    mode: "none",               /// none | round_per_op | round_final | bankers_round | truncate_display
    track_theft: true,           /// record delta between exact vs emitted amounts
    shame_level: "silent"       /// silent | educational | passive_aggressive | brutal
  }
}

Behavior
Internal math uses big engine semantics; any operation that would exceed max_precision contributes dust to a per‑currency remainder ledger.


Exports and settle(x) snap to display_precision using rounding and rounding_timing.


policy governs enforcement: e.g., strict raises on sub‑precision; warn logs; defer postpones until settlement; truncate forcibly truncates.


compat only affects emission toward external/broken systems; the “theft ledger” records discrepancies.


⚠️ Rename/Refactor Note: legacy money.precision is replaced by display_precision (what users see) and max_precision (what we carry). Update configs accordingly.
Examples
set @policy site_default
price = $80.123456
invoice = settle(price)     /// snaps to display_precision (2) at export time

set @policy "finance_strict"
    money: { display_precision: 2, max_precision: 8, rounding: "half_even", policy: "strict", rounding_timing: "per_tx" }
end

subtotal = $123.45
rate = 8.25%
tax = rate of subtotal       /// exact; dust (if any) → ledger per USD


30.2 Datetime (Trusted Time, TZ, Arithmetic)
datetime: {
  tz: nil,                   /// default zone (IANA); nil → config/machine default
  prefer_trusted: false,     /// prefer server‑verified time sources
  policy: "warn",           /// strict | warn | allow
  time_arith: "error",      /// error | wrap | shift (see §20)
  leap_mode: "clamp",       /// clamp | smear
  tzdata_version: "",       /// pin tzdb; empty = engine default
  /// Optional trusted‑time overrides (see §20 Trusted Time)
  source: nil, ttl: 60s, cache_ttl: 24h, skew_tolerance: 5s,
  cache_path: "dist/time.cache", cache_signing_key: nil,
  debug: false
}

Behavior
now()/today() read the active snapshot; if prefer_trusted is true, the trusted chain (server → signed cache → local) is used per policy.


time_arith controls time ± duration on time(...) values: forbid, wrap, or return overflow (shift).


Formatting/parsing follow §20; named zones preferred over fixed offsets.


Examples
set @policy "trusted_minimal"
    datetime: { prefer_trusted: true, policy: "warn", source: "https://time.nist.gov/api/now" }
end

verified = trusted_now()


30.3 Modules (Visibility)
modules: {
  mode: "expose"   /// expose | vault
}

Behavior
Mirrors §24: modules live in the modules/ directory. Visibility is policy‑driven: expose (open) vs vault (locked). Symbol‑level expose/vault still apply.


Errors: importing a hidden symbol → ModuleVisibilityError; file made effectively vault by policy → PolicyVisibilityError.


Example
set @policy "sealed_mods"
    modules: { mode: "vault" }
end

import "./helpers" as H      /// points to modules/helpers.gbln; may be blocked under vault


30.4 Strings (Input Hygiene Hints)
strings: {
  trim: true,         /// l/r trim for designated helpers
  strip_html: false,  /// remove tags on marked surfaces
  escape: false       /// conservative escaping for non‑HTML sinks
}

Hints are honored by core helpers and glams where sensible. HTML glam still auto‑escapes; escape:true is additive elsewhere.

Determinism & Audit
In --deterministic builds, policies govern whether stateful features (e.g., trusted time cache, money settlement) are permitted. Changes to the active snapshot may emit JSONL breadcrumbs when datetime.debug: true or money compat tracking is enabled.

Errors
PolicyNotFoundError("name") — unknown policy.


PolicyValueError(path, reason) — invalid field value (e.g., negative max_precision).


PolicyScopeError — header set @policy not first statement when strict lint/build requires.


PolicyVisibilityError — import/use blocked by active policy (§24 integration).



Examples
File‑wide default
set @policy site_default

price = $80
say price + 10%         /// percent‑of‑1 → money addition per policy

Tighten for a block
set @policy site_default

op checkout(cart)
    set @policy "finance_strict"
        money: { rounding: "half_even", policy: "strict", rounding_timing: "per_tx" }
    end

    total = cart.total()
    tax   = 8.25% of total
    settle(total + tax)
end

Datetime behavior switch
set @policy "strict_time"
    datetime: { time_arith: "error" }
end

/// time("23:30") + 1h  /// TimeArithmeticError under this policy

Modules as vault
set @policy "sealed_mods"
    modules: { mode: "vault" }
end

import "./ids" as IDs
IDs::new_id()           /// may require symbol‑level `expose` in the module


Quick Reference
File: policies.gbln (auto‑loaded). Define named loadouts per domain.


Apply: set @policy NAME (header or inline). Precedence: inline > file > project.


Money: display_precision, max_precision, rounding, policy, rounding_timing, optional currency, compat{…}; dust → remainder ledger.


Datetime: tz, prefer_trusted, policy, time_arith, leap_mode, tzdata_version, optional trusted‑time fields.


Modules: mode: expose|vault; integrates with §24 (modules live in modules/).


Strings: trim, strip_html, escape (hints for helpers/glams).


Errors: PolicyNotFoundError, PolicyValueError, PolicyScopeError, PolicyVisibilityError.


Design Rationale. Policies make behavior explicit, auditable, and deterministic. Money math remains exact internally (Big engine); precision/rounding are policy‑controlled at the edges. Datetime avoids naïve timestamps; policy expresses trust and arithmetic intent. Module visibility is centralized. Strings hints keep input hygiene predictable.
30.4.1 Money — updated fields (v1.5)
Policies now control both what users see and how internal precision is carried. Money is powered by Big with a currency tag; policy fields decide display, rounding, and when/where sub‑precision is ledgered.
set @policy finance_defaults
    money: {
        currency: "USD",            /// default for Money::from
        display_precision: 2,         /// user/export digits (e.g., cents)
        max_precision: 8,             /// internal carry digits before ledgering
        rounding: "bankers",         /// half_even | half_up | half_down | floor | ceil
        policy: "truncate",          /// truncate | warn | strict | defer
        rounding_timing: "per_tx",   /// per_tx | per_period | at_settlement
        defer_mode: "escrow",        /// if policy==defer: bake-in | escrow | carry-forward
        thousands: ",",               /// "," | "." | " " | "_" | ""
        decimal: "."                  /// "." | ","
        /// compat (optional; external/broken systems shim)
        compat: {
            mode: "none",            /// none | round_per_op | round_final | bankers_round | truncate_display
            track_theft: true,
            shame_level: "silent"     /// silent | educational | passive_aggressive | brutal
        }
    }
end

Semantics
display_precision controls rendering/exports; internal math always uses exact decimals from Big.


max_precision bounds internal carry; digits beyond are routed to the per‑currency remainder ledger. Totals conserve: distributed + ledger == exact.


rounding selects tie‑break behavior; applies wherever a rounding step is required by the active policy.


policy governs how sub‑unit precision is handled:


truncate — cut extra digits; ledger accumulates the remainder.


warn — as truncate, plus a MoneyPrecisionWarning diagnostic.


strict — refuse loss; use >> to receive (q, r) and handle r explicitly.


defer — keep full precision until you call settle(); choose defer_mode:


bake-in — round into the value at settle.


escrow — divert remainder to ledger at settle.


carry-forward — retain across ops until a final settle/export.


rounding_timing chooses when rounding/ledgering happens: per_tx, per_period, or at_settlement.


thousands/decimal affect display only (including CSV/printed strings). Canonical JSON export remains available via writer options.


Examples
set @policy strict_math
    money: { display_precision: 2, max_precision: 8, rounding: "bankers", policy: "strict" }
end

/// Strict: you must handle the remainder explicitly
q, r = $10.00 >> 3    /// q=$3.33, r=$0.01

set @policy daily_rollup
    money: { display_precision: 2, max_precision: 8, policy: "defer", rounding_timing: "per_period", defer_mode: "carry-forward" }
end

x = $10.00 // 3       /// displays $3.33; carries full precision internally
final = x.settle()     /// apply rounding per policy/defer_mode

Integration notes
Percent operators compose with money; policy controls rounding of results:


price = $80.00
with_tax = price + (8.5% of price)   /// $86.80 (policy rounding/ledger apply if needed)

Conversions remain explicit and may generate target‑currency remainder per policy:


$100.00.convert(to: EUR, rate: 0.9127)

⚠ Conflict note (cheat‑sheet canonical): Older drafts used money.precision. This is superseded by display_precision (what users see) and max_precision (internal carry). Engines treat precision as an alias for display_precision for one cycle and emit a deprecation lint. User guidance will reconcile final naming if discrepancies remain.
Quick Reference — Money policy
Fields: currency, display_precision, max_precision, rounding, policy, rounding_timing, defer_mode, thousands, decimal, optional compat{}.


Policies: truncate | warn | strict | defer (with defer_mode).


Timing: per_tx | per_period | at_settlement.


Ledger: overflow of max_precision → per‑currency remainder ledger (audit‑friendly; totals conserve).


Display: separators affect rendering only; canonical exports available in writers.


31. Glams — Philosophy & Architecture
Glams are first‑class, versioned extensions that add domain capabilities without bloating core. They feel native (same syntax contracts, same safety rails) and run under the same sandbox/determinism rules as your code.
Why Glams? Core stays small and stable; anything domain‑specific (exporters, storefronts, formatters, generators) ships as a glam with explicit contracts and permissions. Resolution is deterministic and auditable.

Loading & Versioning
use shopify@^1.6 as shp        /// pin or range; alias is required
use tarot_deck@1.2             /// alias optional if never ambiguous
use invoice                    /// discouraged (unbounded) unless policy allows

Lockfile: glam.lock pins { glam, resolved_version, checksum, source }. Builds read from lock unless --update.


Aliases: as sets a local name used at call sites (shp::csv).


Storage (installed): ~/.goblin/glams/<DisplayName>__<PublisherID>_<GlamID>@<Version>/ (canonical identity is enforced by the package glam.toml).


Design: Version pinning + checksums → reproducible builds. Aliases make call sites explicit and collision‑free.

Capability Resolution (via / prefer)
Glams implement contracts (named capabilities). Calls bind explicitly using via and, optionally, a namespaced symbol.
/// Bind by provider alias (contract chosen by call site)
file = product.export(@items) via shp

/// Bind a concrete symbol from a glam
csv  = export.preview(@items) via shp::csv

/// Project default (fallback) — declared once; callers may still override
prefer product.export via shp

Resolution rules:
A call‑site via wins.


Otherwise a prefer statement or project default applies.


If multiple providers still match → AmbiguityError.


Namespacing: Call public symbols as alias::Symbol (glam) just like Alias::symbol (module). Namespaces never collide because glams must be introduced with use.

Contracts (Type‑Checked Interfaces)
Contracts define the shape and allowed errors for a capability. Implementations must match exactly.
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end

/// Introspection
contracts = glam_contracts("shopify")

Rules:
Signature (names, arity, types) must match; extra/omitted params → ContractError.


Only declared errors may be thrown; others are wrapped as GlamError(glam, capability, cause).


Contracts are globally identified by name (e.g., product.export).


Reasoning: Contracts make glams swappable and verifiable at compile/bind time.

Manifest & Permissions (Sandbox)
Each glam ships a manifest declaring identity, provided capabilities, and an explicit sandbox.
# glam.toml (inside the package)
publisher_id = "central:u_7K3V2"
glam_id      = "shopify"
version      = "1.6.2"
display_name = "Shopify"
tagline      = "product export & sync"

[ops.export]
input_schema  = "schemas/export.in.json"
output_schema = "schemas/export.out.json"

[capabilities]
fs.read  = ["./templates", "./schemas"]     # read‑only mounts
fs.write = ["./work", "./out"]              # writeable mounts
env      = ["SHOPIFY_TOKEN"]
# network = { domains=["api.shopify.com"], methods=["POST"] }

Sandbox rules:
No ambient FS/NET/ENV. Only declared paths/domains/vars are reachable.


Read‑only mounts are enforced; writes outside fs.write → ReadOnlyPathError.


Project policy can further restrict or deny capabilities.


Deterministic builds additionally:
Block wall‑clock (now()); require trusted_now().


Block network unless allow‑listed.


Reject unseeded RNG.



Project Manifest (Aliases & Defaults)
Projects map aliases to canonical glams and keep track of installed-but‑unused packages.
# goblin.toml (project)
[glams]                      # active aliases (you edit)
shp         = "central:u_7K3V2/shopify@1.6.2"

[unused_glams]               # auto‑maintained; promote by copy/paste
"central:u_9H7F4/tarot@0.4.2" = "tarot scaffold system"

Notes:
Aliases are project‑local; renaming an alias does not move the install.


Lockfile pins canonical IDs + hashes; display names and taglines are cosmetic.



CLI (Selector → alias → op)
goblin glam <alias> <op> [args]     # run an exposed op from a glam

Reserved op names: spawn, build, export, push, lint, test, info, help.


Custom ops may be declared in the manifest under [ops.<name>] with JSON Schemas for inputs/outputs.


Contracts & Schemas: The CLI validates arguments using the bound op’s JSON Schema. goblin glam <alias> help shows auto‑generated help.


Help & Introspection
goblin help                # base manual
goblin glam <alias> help   # ops for one glam
goblin glam test <alias>   # run glam tests

Safety defaults (CLI):
Files: create‑only by default; updates and deletes require explicit flags.


Services/DB: dry‑run by default; --commit required to mutate.


Output names are auto‑namespaced to avoid clobbers: <alias>_v<version>_<filename>.



Logging, Telemetry, and Event Bus

Patches::
Event Bus
Since you don’t need it right now, move it out of core and note: “provided by the events GLAM.” Core stays quiet; we can bring it back later with tighter guarantees.

Structured logs surround every glam call (dist/glam.log):
{"ts":"2025-08-12T15:30:00Z","glam":"shopify","cap":"product.export","ms":128,"ok":true}
{"ts":"2025-08-12T15:30:01Z","glam":"shopify","cap":"product.export","ok":false,"err":"ValidationError: missing title"}

Event bus (in‑process) supports sync/async processing:
emit "catalog.ready", @games                 /// sync (errors bubble)

on "catalog.ready" mode: "async" concurrency: 2 error: "collect"
    product.export($event.payload) via shp
end


Usage Patterns
Single export
use tarot_deck@1.2, shopify@^1.6 as shp
prefer product.export via shp

@cards = tarot_deck::card_template(price: .99, qty: 1)
    "Ace of Cups"
    "Two of Cups"

file = product.export(@cards) via shp

Multi‑platform chain
use board_game, shopify@^1.6 as shp, etsy@^2

@games = board_game::template()
    "Catan"   :: qty: 4
    "Pandemic":: qty: 2

export @games via shp::csv  to "dist/shopify.csv"
export @games via etsy::csv to "dist/etsy.csv"

Dry‑run under determinism
set @policy build_lock
    datetime: { prefer_trusted: true, policy: "strict" }
end

plan = product.export(@games) via shp dry_run:true
say plan.summary


Errors
GlamError(glam, capability, cause) — provider threw an unexpected error; core wraps the cause.


ContractError — signature/declared errors mismatch.


PermissionError / CapabilityDeniedError — attempted access outside declared sandbox.


AmbiguityError — multiple providers match; add via or prefer.


LockfileError / IdentityMismatchError — lock/install mismatch or tampered package.


ReadOnlyPathError — write attempted to an fs.read mount.



Rules & Constraints
Glams are introduced with use; call sites bind providers with via (and optionally alias::Symbol).


Contracts are mandatory for cross‑project capabilities; implementations must match exactly.


All side‑effects run under explicit sandbox mounts and respect deterministic‑build policy.


Output files get versioned‑alias prefixes to avoid clobbering.


Project manifest tracks aliases; lockfile pins versions and checksums.



CLI, Manifests & Identity (Additions)
One Mental Model
Modules are project‑local private glams; packaged GLAMs are versioned and shareable. Both obey the same contracts, capability sandbox, and CLI shape. See §24 for module‑specific rules; this section focuses on GLAMs.
CLI: Selector → Alias → Op
/// Project module (local/private)
/// Use when calling an op exposed by a module in ./modules/
$ goblin module <alias> <op> [args]

/// Installed GLAM (external/packaged)
$ goblin glam   <alias> <op> [args]

/// Verbs are the op names you expose.
/// Core reserved verbs: spawn, build, export, push, lint, test, info, help.
/// Custom verbs allowed (e.g., x:draw) when declared in the manifest.
/// Selector disambiguates scope (no name conflicts between modules & glams).

Visibility & CLI Contracts
/// vault → private; never callable from CLI
/// expose → public; CLI‑callable only if also declared in the manifest
/// Each exposed op ships JSON Schema pointers (input/output) used by the CLI for:
///  - argument validation
///  - autogenerated --help
///  - autocompletion

Installed Location & Identity (GLAMs)
/// Human‑friendly + canonical on disk (per install):
~/.goblin/glams/<DisplayName>__<PublisherID>_<GlamID>@<Version>/

/// Examples
~/.goblin/glams/Auth__central:u_7K3V2_auth@1.0.3/
~/.goblin/glams/Tarot Tools__central:u_9H7F4_tarot@0.4.2/

/// DisplayName is cosmetic; canonical identity lives in glam.toml
/// (publisher_id, glam_id, version). The engine verifies identity.

Project Manifest & Aliases
/// goblin.toml (project) – map project‑local aliases to canonical IDs
[glams]
shp = "central:u_7K3V2/auth@1.0.3"
tarot_tools = "central:u_9H7F4/tarot@0.4.2"

/// Then:
$ goblin glam shp <op> [args]
$ goblin glam tarot_tools <op> [args]

/// Aliases are project‑local; renaming them does not touch installed GLAMs.

/// New installs are appended to an auto‑maintained parking area:
[unused_glams]
"central:u_7K3V2/auth@1.0.3"  = "authentication and sessions"
"central:u_9H7F4/tarot@0.4.2" = "tarot scaffold system"
/// Promote by copy/paste into [glams] and giving an alias.

Help Commands
$ goblin help                 /// base manual
$ goblin glam <alias> help    /// ops for one glam
$ goblin help glam            /// extended manual

Manifest (Authoring, Minimal TOML)
# glam.toml (canonical as of v1.5)
# identity
publisher_id = "central:u_7K3V2"
glam_id      = "auth"
version      = "1.0.3"

# presentation
display_name = "Auth"
tagline      = "authentication and sessions"

# ops & contracts (ops must be exposed in code)
[ops.spawn]
input_schema  = "schemas/spawn.json"
output_schema = "schemas/spawn.out.json"

[ops.export]
input_schema  = "schemas/export.json"

# capabilities (sandbox)
[capabilities]
fs.read  = ["./templates", "./schemas"]   /// read‑only; writes error
fs.write = ["./work", "./out"]            /// explicit writeable roots
env      = ["SHOPIFY_API_KEY"]             /// explicit env allowlist
# network = { domains=["api.shopify.com"], methods=["POST"] }

⚠️ Manifest format update. Earlier drafts referenced glam.yaml. The canonical manifest is glam.toml (v1.5). Engines accept glam.yaml in legacy mode with a deprecation warning; prefer TOML going forward.
Additional CLI‑Facing Errors
GlamNotFoundError      /// alias doesn’t resolve to an installed canonical ID
OpNotExposedError      /// op not exposed or not listed in manifest [ops]
CapabilityDeniedError  /// attempted access outside declared capabilities
IdentityMismatchError  /// directory name vs glam.toml identity mismatch
ReadOnlyPathError      /// write attempted to an fs.read mount

Dev → Share Path (Author Workflow)
1) Write a module (plain code under ./modules/) → library‑only by default.
2) Add a project manifest entry → module becomes CLI‑callable:
   $ goblin module <alias> <op> [args]
3) Package as a GLAM → others can install & call it:
   $ goblin glam <alias> <op> [args]
/// Same ops, same contracts, same capabilities — no new model to learn.

Quick Reference
Load & alias
use glam@ver as a


Lockfile: glam.lock


Bind provider
result = capability(args) via a


symbol = do_it(args) via a::Symbol


Defaults: prefer capability via a


Contracts
contract name(args...) -> ret; errors: [...] end


glam_contracts(alias)


Sandbox
Manifest declares fs.read, fs.write, env, network.


Deterministic builds block wall‑clock, unseeded RNG, undeclared network.


Project manifest
goblin.toml → [glams] active aliases; [unused_glams] auto‑maintained.


CLI
goblin glam <alias> <op> [args]


Help: goblin glam <alias> help, goblin help glam


Tests: goblin glam test <alias>


Introspection
glams(), glam_symbols(alias), glam_permissions(alias)


Errors
GlamError, ContractError, PermissionError, AmbiguityError, LockfileError, ReadOnlyPathError



