# Goblin Money System – Formal Semantics (v0.1)

## 1. Overview

The Goblin language treats money as a first-class type with built-in conservation guarantees.

**Key Invariant:**
```
For all well-typed operations:
sum(inputs) = sum(outputs) + sum(remainders)
```
This invariant holds within each currency and is guaranteed by construction of the operational semantics.

## 2. Syntax

### 2.1 Types
```
τ ::= int | float | money(C) | <money(C), money(C)>
```

Where:
- `C` is a currency code (e.g., USD, EUR)
- Pairs `<money(C), money(C)>` represent (quotient, remainder) from division

### 2.2 Expressions
```
e ::= n | m | e op e | divmod(e, e) | divide_evenly(e, e)
```

Where:
- `n ∈ Z ∪ R` (numeric literals)
- `m` is a money literal, e.g., `$10.50`
- `op ∈ {+, -, *, //}`

## 3. Typing Rules

**Notation:** `Γ |- e : τ` means "under environment Γ, expression e has type τ"

### 3.1 Promotion
```
Γ |- e1 : money(C)    Γ |- e2 : num
─────────────────────────────────────
Γ |- e1 op e2 : money(C)
```
Where `num ∈ {int, float}`, and `e2` is promoted to `money(C)` before evaluation.

### 3.2 Addition & Subtraction
```
Γ |- e1 : money(C)    Γ |- e2 : money(C)
──────────────────────────────────────
Γ |- e1 + e2 : money(C)
```
(Similarly for subtraction)

### 3.3 Multiplication
```
Γ |- e1 : money(C)    Γ |- e2 : num
────────────────────────────────────
Γ |- e1 * e2 : money(C)
```

### 3.4 Division (Integer Quotient & Remainder)
```
Γ |- e1 : money(C)    Γ |- e2 : int    e2 ≠ 0
─────────────────────────────────────────────
Γ |- e1 // e2 : <money(C), money(C)>
```
- First component: quotient
- Second component: remainder

### 3.5 Even Split
```
Γ |- e1 : money(C)    Γ |- e2 : int    e2 > 0
─────────────────────────────────────────────
Γ |- divide_evenly(e1, e2) : list(money(C))
```

## 4. Operational Semantics

**Notation:** `e => v` means "expression e evaluates to value v"

### 4.1 Addition
```
e1 => m1    e2 => m2
────────────────────
e1 + e2 => m1 +C m2
```
Where `+C` is decimal addition in currency C, rounded half-away-from-zero to 2 decimal places.

### 4.2 Subtraction
```
e1 => m1    e2 => m2
────────────────────
e1 - e2 => m1 -C m2
```

### 4.3 Multiplication
```
e1 => m    e2 => k
──────────────────
e1 * e2 => m ×C k
```

### 4.4 Division with Remainder
```
e1 => m    e2 => n
──────────────────
e1 // e2 => <q, r>
```

Where:
- `q = floor(m / n)`
- `r = m - q × n`

**Exact equality holds:** `m = q × n + r`

### 4.5 Even Split
```
e1 => m    e2 => n
─────────────────────────────
divide_evenly(e1, e2) => [s1, ..., sn]
```

Such that:
- `sum(s1, ..., sn) = m`
- `|si - sj| ≤ 0.01` for all i, j (largest remainder method)

## 5. Money Conservation Theorem

**Theorem (Money Conservation):**
If `e` is a well-typed Goblin expression and evaluates without error:

### For operations returning a money value:
```
sum(inputs) = output
```

### For operations returning (quotient, remainder) pairs:
```
sum(inputs) = quotient × divisor + remainder
```

### For divide_evenly:
```
sum(parts) = input
```

**Proof Sketch:** By structural induction on evaluation derivations:

1. **Base cases:** Literals conserve trivially
2. **Inductive step:** Show each operation's evaluation rule satisfies the conservation equation given its premises
3. **Composition:** If subexpressions conserve, the composition also conserves

## 6. Currency Safety

Goblin enforces currency matching at compile-time:

- Cross-currency arithmetic is a type error unless explicitly converted via a currency conversion function
- Guarantees the theorem applies per currency
- No silent currency coercion

## 7. Implementation Notes

### Rounding
All intermediate operations round half-away-from-zero to avoid bias.

### Remainder Tracking
The interpreter can optionally log remainders from `//` for auditing.

### Promotion
Numeric literals in mixed operations are promoted to `money(C)` with smallest unit precision before evaluation.

### Error Conditions
- Division by zero: `MoneyDivisionError`
- Currency mismatch: `CurrencyError`
- Regular division on money: `MoneyDivisionError: Use // to capture remainder`

## 8. Examples

### Basic Conservation
```goblin
// Input: $10.00
price = $10.00
q, r = price // 3
// Output: q = $3.33, r = $0.01
// Conservation: $10.00 = $3.33 × 3 + $0.01 ✓
```

### Even Split Conservation
```goblin
// Input: $100.00
total = $100.00
shares = divide_evenly(total, 3)
// Output: [$33.34, $33.33, $33.33]
// Conservation: $33.34 + $33.33 + $33.33 = $100.00 ✓
```

### Currency Safety
```goblin
usd_price = $10.00
eur_price = €8.50
total = usd_price + eur_price  // CurrencyError at compile time
```

## 9. Formal Verification Goals

1. **Mechanize this semantics** in Coq/Lean/Isabelle
2. **Prove money conservation** theorem mechanically
3. **Verify interpreter implementation** against formal semantics
4. **Extend to gear system** with formal module safety
5. **Add currency conversion** with exchange rate tracking
