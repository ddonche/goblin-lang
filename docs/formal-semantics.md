# Goblin Money System — Formal Semantics (v0.2, No Rounding)

## 1. Overview

Goblin treats money as **cents (minor units) + an explicit remainder** for any sub-cent amount. **No rounding ever occurs.**

**Global Invariant (per currency C):**
```
sum(inputs in C) = sum(outputs in C) + sum(remainders in C)
```

Remainders are (optionally) recorded in a **remainder ledger**; they can be inspected or later allocated (e.g., with `divide_evenly`).

## 2. Syntax (Excerpt)

### 2.1 Types
```
τ ::= int | float | money(C) | <money(C), money(C)> | list(money(C))
```

### 2.2 Expressions
```
e ::= money(v, C) | $vC | e + e | e - e | e * k | e // n | divide_evenly(e, n)
```

Where:
- `v ∈ ℚ` (arbitrary-precision numeric literal)
- `k ∈ ℚ` (scalar)
- `n ∈ ℤ, n > 0` (split/divisor)

## 3. Canonicalization (Truncate-and-Remainder)

Fix a currency `C` with cent size `uC = 0.01`.

**Define:**
- `to_centsC(x) = trunc(x / uC)` (truncate toward zero to integer cents)
- `canonC(x) = (cents = to_centsC(x), rem = x - cents*uC)`

Where `rem` satisfies `0 ≤ |rem| < uC` and has the same sign as `x`.

**Intuition:** Split any amount into whole cents plus a sub-cent remainder; never round.

## 4. Typing (Selected Rules)

### Same-Currency Operations
```
Γ ⊢ e1 : money(C)   Γ ⊢ e2 : money(C)
──────────────────────────────────────
Γ ⊢ e1 ± e2 : money(C)
```

### Scalar Multiplication
```
Γ ⊢ e : money(C)   Γ ⊢ k : ℚ
──────────────────────────────
Γ ⊢ e * k : money(C)
```

### Division with Remainder
```
Γ ⊢ e : money(C)   Γ ⊢ n : int   n > 0
────────────────────────────────────────
Γ ⊢ e // n : <money(C), money(C)>
```

### Even Split
```
Γ ⊢ e : money(C)   Γ ⊢ n : int   n > 0
────────────────────────────────────────
Γ ⊢ divide_evenly(e, n) : list(money(C))
```

**Cross-currency arithmetic is ill-typed** (enforced by `C`).

## 5. Evaluation with a Remainder Ledger

We use **big-step semantics** with an explicit **remainder ledger** `ℛ`, a map `Currency → ℚ`, accumulating sub-cent amounts.

**Notation:**
```
e ⇓ v ; ℛ'
```
Meaning: evaluating `e` yields value `v` and updates the ledger from current `ℛ` to `ℛ'`.

### 5.1 Money Construction (Literal or Constructor)

Literal `$vC` or `money(v, C)`:

```
canonC(v) = (cents, rem)
---------------------------------------
money(v, C) ⇓ money(C, cents*uC) ; ℛ[C] += rem
```

**No rounding:** the money value is the truncated cents; `rem` is logged.

### 5.2 Addition/Subtraction (Same Currency)

Exact on cents; no new remainder:

```
e1 ⇓ m1 ; ℛ1      e2 ⇓ m2 ; ℛ2      (cur m1 = cur m2 = C)
----------------------------------------------------------
e1 + e2 ⇓ m1 ⊕C m2 ; merge(ℛ1, ℛ2)    // cents add exactly
```

(Same for subtraction)

### 5.3 Scalar Multiplication

Multiply cents exactly in `ℚ`, then canonicalize with truncate-and-remainder:

```
e ⇓ m ; ℛ
let x = (cents(m) * uC) * k      // exact rational
canonC(x) = (cents', rem')
--------------------------------------------------
e * k ⇓ money(C, cents'*uC) ; ℛ[C] += rem'
```

### 5.4 Divmod // (Money by Positive Int)

Quotient in cents: integer division; remainder in cents:

```
e ⇓ m ; ℛ      n > 0
let q_c = cents(m) / n          // integer division
let r_c = cents(m) - q_c * n    // 0 ≤ |r_c| < n
------------------------------------------------
e // n ⇓ <money(C, q_c*uC), money(C, r_c*uC)> ; ℛ
```

**No new sub-cent remainder** is created here; both results are money (cent-precision).

### 5.5 Even Split

Distribute cents exactly; no sub-cent remainder produced:

```
e ⇓ m ; ℛ      n > 0
let q_c = cents(m) / n
let r_c = cents(m) mod n
produce list: r_c shares of (q_c+1) cents, else q_c cents
----------------------------------------------------------
divide_evenly(e, n) ⇓ [m1..mn] ; ℛ
```

**Sum of shares equals `m` exactly.**

**Ledger Behavior:** Only construction and scalar multiplication (and any future op that yields sub-cent results) log to `ℛ`. Pure cents-only ops don't touch `ℛ`.

## 6. Conservation Theorem (No Rounding)

**For any well-typed expression `e` in currency `C`:**

```
Let eval(e) produce value v and ledger change Δℛ.
Then   total_input_C = value_C + Δℛ[C]
```

**Formally, by structural induction over the rules above:**

1. **Construction:** `v = trunc(v0)`, `Δℛ[C] = v0 - trunc(v0)`
2. **+/-:** cents add exactly; ledger is just the merge of sub-ledgers
3. **\* k:** `x = value * k`, `v = trunc(x)`, `Δℛ[C] = x - trunc(x)`
4. **// n:** `m = q*n + r` in cents; ledger unchanged
5. **divide_evenly:** shares sum to input; ledger unchanged

**No step introduces rounding; all fractional loss is recorded as remainder.**

## 7. Practical Guidance

### Perfect Preservation
You **never lose a fraction**: it's either represented as cents or logged in `ℛ`.

### Auditability
`remainders_total()` and `remainders_report()` just read `ℛ`.

### Allocation
Use `divide_evenly(total, n)` to assign cents without rounding.

### Remainder Management
To allocate ledger remainders later, expose a helper (e.g., `drip(ℛ[C])`) that periodically converts accumulated remainder to a cent and emits it deterministically.

## 8. Error Conditions

- **Division by zero:** `MoneyDivisionError`
- **Currency mismatch:** `CurrencyError`
- **Regular money /:** `MoneyDivisionError ("use // or * by rational with truncation+ledger")`

## 9. Notes Toward Mechanization

### Coq/Lean Representation
- Represent money as `(C, ℤ)` cents
- Represent the ledger as `Currency → ℚ`

### Key Lemmas
- Define `canonC` and prove `x = cents*uC + rem` with `|rem| < uC`
- Replace any former rounding lemmas with truncation + remainder lemmas

### Conservation Proofs
Conservation proofs become simpler: each rule explicitly preserves `value + remainder`.

## 10. Implementation Requirements

### Money Type
```
Money = (currency: Currency, cents: Integer)
RemainderLedger = Map<Currency, Rational>
```

### Core Operations
All operations must:
1. Preserve exact conservation
2. Update remainder ledger when sub-cent amounts arise
3. Never perform rounding

### Verification Goals
1. **Mechanize in formal verification system** (Coq/Lean/Isabelle)
2. **Prove conservation theorem** mechanically
3. **Verify interpreter implementation** against these semantics
4. **Extend to full language** with gears and other features

---

**Status:** This specification provides the mathematical foundation for Goblin's revolutionary approach to financial computation with perfect conservation and no precision loss.
