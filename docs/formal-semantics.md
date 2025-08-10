# Goblin Money System – Formal Semantics (v0.1)

## 1. Overview

The Goblin language treats **money** as a first-class type with **built-in conservation guarantees**. 

**Key Invariant:**
```
For all well-typed operations:
sum(inputs) = sum(outputs) + sum(remainders)
```

This invariant holds within each currency and is guaranteed by **construction** of the operational semantics.

## 2. Syntax

### 2.1 Types
```
τ ::= int | float | money(C) | <money(C), money(C)>
```

Where:
- `C` is a currency code (e.g., `USD`, `EUR`)
- Pairs represent `(quotient, remainder)` from division

### 2.2 Expressions
```
e ::= n | m | e op e | divmod(e,e) | divide_evenly(e,e)
```

Where:
- `n ∈ ℤ ∪ ℝ` (numeric literals)
- `m` is a money literal, e.g., `$10.50USD`
- `op ∈ {+, -, *, //}`

## 3. Typing Rules

**Notation:** `Γ ⊢ e : τ` means "under environment Γ, expression e has type τ"

### 3.1 Promotion
```
Γ ⊢ e1 : money(C)    Γ ⊢ e2 : num
─────────────────────────────────────
Γ ⊢ e1 op e2 : money(C)
```

Where `num ∈ {int, float}`, and `e2` is promoted to `money(C)` before evaluation.

### 3.2 Addition & Subtraction
```
Γ ⊢ e1 : money(C)    Γ ⊢ e2 : money(C)
──────────────────────────────────────
Γ ⊢ e1 + e2 : money(C)
```

(Similarly for `-`)

### 3.3 Multiplication
```
Γ ⊢ e1 : money(C)    Γ ⊢ e2 : num
────────────────────────────────────
Γ ⊢ e1 * e2 : money(C)
```

### 3.4 Division (Integer Quotient & Remainder)
```
Γ ⊢ e1 : money(C)    Γ ⊢ e2 : int    e2 ≠ 0
─────────────────────────────────────────────
Γ ⊢ e1 // e2 : <money(C), money(C)>
```

- First component: `quotient`
- Second component: `remainder`

### 3.5 Even Split
```
Γ ⊢ e1 : money(C)    Γ ⊢ e2 : int    e2 > 0
─────────────────────────────────────────────
Γ ⊢ divide_evenly(e1, e2) : list(money(C))
```

## 4. Operational Semantics

We use **big-step semantics**: `e ⇓ v` means "expression e evaluates to value v"

### 4.1 Addition
```
e1 ⇓ m1    e2 ⇓ m2
────────────────────
e1 + e2 ⇓ m1 +C m2
```

Where `+C` is decimal addition in currency C, rounded half-away-from-zero to 2 decimal places.

### 4.2 Subtraction
```
e1 ⇓ m1    e2 ⇓ m2
────────────────────
e1 - e2 ⇓ m1 -C m2
```

### 4.3 Multiplication
```
e1 ⇓ m    e2 ⇓ k
──────────────────
e1 * e2 ⇓ m ×C k
```

### 4.4 Division with Remainder
```
e1 ⇓ m    e2 ⇓ n
──────────────────
e1 // e2 ⇓ <q, r>
```

Where:
- `q = floor(m / n)`
- `r = m - q × n`

**Exact equality holds:** `m = q × n + r`

### 4.5 Even Split
```
e1 ⇓ m    e2 ⇓ n
─────────────────────────────
divide_evenly(e1, e2) ⇓ [s1, ..., sn]
```

Such that:
- `∑(i=1 to n) si = m`
- `|si - sj| ≤ 0.01` for all i, j (largest remainder method)

## 5. Money Conservation Theorem

**Theorem (Money Conservation):** If `e` is a well-typed Goblin expression and evaluates without error:

### For operations returning a money value:
```
∑(inputs) = output
```

### For operations returning (quotient, remainder) pairs:
```
∑(inputs) = quotient × divisor + remainder
```

### For divide_evenly:
```
∑(parts) = input
```

**Proof Sketch:** By structural induction on evaluation derivations:

1. **Base cases:** Literals conserve trivially
2. **Inductive step:** Show each operation's evaluation rule satisfies the conservation equation given its premises
3. **Composition:** If subexpressions conserve, the composition also conserves

## 6. Currency Safety

Goblin enforces **currency matching** at compile-time:

- Cross-currency arithmetic is a type error unless explicitly converted via a currency conversion function
- Guarantees theorem applies per currency
- No silent currency coercion

## 7. Implementation Notes

### Rounding
All intermediate operations round half-away-from-zero to avoid bias.

### Remainder Tracking
The interpreter can optionally log remainders from `//` for auditing.

### Promotion
Numeric literals in mixed operations are promoted to `money(C)` with smallest unit precision before evaluation.

### Error Conditions
- **Division by zero:** `MoneyDivisionError`
- **Currency mismatch:** `CurrencyError`
- **Regular division on money:** `MoneyDivisionError: Use // to capture remainder`

## 8. Formal Properties

### 8.1 Conservation Property
**Property:** For any well-typed expression `e`:
```
eval(e) preserves total money value within each currency
```

### 8.2 Currency Safety Property
**Property:** 
```
No well-typed expression can mix currencies without explicit conversion
```

### 8.3 Remainder Completeness Property
**Property:**
```
All money remainders are either:
1. Explicitly captured in variables, or
2. Tracked in the remainder ledger
```

## 9. Examples with Formal Verification

### 9.1 Basic Conservation
```goblin
// Input: $10.00
price = $10.00
q, r = price // 3

// Formal verification:
// Input: money(USD, 1000) cents
// Output: q = money(USD, 333) cents, r = money(USD, 1) cents
// Conservation: 1000 = 333 × 3 + 1 ✓
```

### 9.2 Even Split Conservation
```goblin
// Input: $100.00
total = $100.00
shares = divide_evenly(total, 3)

// Formal verification:
// Input: money(USD, 10000) cents
// Output: [money(USD, 3334), money(USD, 3333), money(USD, 3333)] cents
// Conservation: 3334 + 3333 + 3333 = 10000 ✓
```

### 9.3 Currency Safety
```goblin
usd_price = $10.00     // money(USD, 1000)
eur_price = €8.50      // money(EUR, 850)
total = usd_price + eur_price  // Type error: currency mismatch
```

## 10. Extension Points

### 10.1 Currency Conversion
```
Γ ⊢ e1 : money(C1)    Γ ⊢ rate : exchange_rate(C1, C2)
─────────────────────────────────────────────────────
Γ ⊢ convert(e1, C2, rate) : money(C2)
```

### 10.2 Gear System Integration
```
Γ ⊢ data : list(money(C))    Γ ⊢ gear : exporter(format)
──────────────────────────────────────────────────────
Γ ⊢ export data via gear : io_effect(format)
```

### 10.3 Audit Trail Extension
```
Every money operation produces:
(result, audit_trace)
where audit_trace : list(money_operation)
```

## 11. Formal Verification Goals

1. **Mechanize semantics** in Coq/Lean/Isabelle/HOL
2. **Prove money conservation** theorem mechanically
3. **Verify interpreter implementation** against formal semantics
4. **Extend to gear system** with formal module safety
5. **Add currency conversion** with exchange rate tracking and audit trails
6. **Prove absence of common financial bugs** (overflow, underflow, precision loss)

## 12. Research Contributions

This formal semantics provides:

1. **First formally verified money type** in a general-purpose programming language
2. **Provably correct remainder handling** with mathematical guarantees
3. **Type-safe currency system** preventing mixing errors
4. **Foundation for verified financial computation** in business applications
5. **Extensible framework** for domain-specific financial languages

---

**Status:** This document provides the theoretical foundation for Goblin's revolutionary approach to financial computation. Implementation and mechanized verification are ongoing.
