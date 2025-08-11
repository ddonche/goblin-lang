# Goblin Money System — Formal Semantics (v0.3.2, Precision, Policy, & Allocation)

## 1. Overview

Goblin models money as **major units + explicit remainder** for all fractions smaller than the declared precision. **No implicit rounding ever occurs** unless the policy is explicitly round (strongly discouraged).

For each currency C:
- **Precision pC** → number of decimal places stored (default: 2)
- **Quantum uC = 10^(-pC)** (in major units)
- **Policy** for sub-quantum remainders:
  - `truncate` → store quantum part, append remainder to ledger
  - `warn` → same as truncate, but also append a warning to the warning log
  - `strict` → reject any non-zero remainder with MoneyPrecisionError

**Global Invariant (per currency C):**
```
Σ(inputs in C) = Σ(outputs in C) + Σ(ledgered remainders in C)
```

## 2. Syntax (Excerpt)

### 2.1 Types
```
τ ::= int | float | money(C)
    | <money(C), money(C)>
    | list(money(C))
    | {shares: list(money(C)), escrow: money(C)}
    | map(Currency, money(C))
```

### 2.2 Expressions
```
e ::= money(v, C) | $vC
     | e + e | e - e
     | e * k | e // n
     | divide_evenly(e, n)
     | divide_evenly_escrow(e, n)
     | convert(e, C2, rate)
     | drip_remainders(th?, commit?, label?)
```

## 3. Canonicalization (Precision + Policy)

For currency C:
- `uC = 10^(-pC)`
- `to_unitsC(x) = trunc(x / uC)` // toward zero

**canonC(x):**
1. `units = to_unitsC(x)`
2. `rem = x - units*uC`
3. Apply policy:
   - `truncate` → accept, ledger rem
   - `warn` → accept, ledger rem, emit warning if rem ≠ 0
   - `strict` → if rem ≠ 0 then raise MoneyPrecisionError

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

### Even Split (Allocation)
```
Γ ⊢ e : money(C)   Γ ⊢ n : int   n > 0
────────────────────────────────────────
Γ ⊢ divide_evenly(e, n) : list(money(C))
```

### Escrow Split
```
Γ ⊢ e : money(C)   Γ ⊢ n : int   n > 0
────────────────────────────────────────
Γ ⊢ divide_evenly_escrow(e, n) : {shares: list(money(C)), escrow: money(C)}
```

### Drip Remainders (Audit + Commit)
```
Γ ⊢ th : ℚ | money(_) | map(Currency, money(_))  [optional]
Γ ⊢ commit : bool   [optional]
Γ ⊢ label : string  [optional]
────────────────────────────────────────────────────────────
Γ ⊢ drip_remainders(th?, commit?, label?) : map(Currency, money(_))
```

## 5. Evaluation with a Remainder Ledger

- **Remainder Ledger ℛ : Currency → ℚ** — sub-quantum amounts not yet allocated.
- **Warning Log 𝒲** — append-only list of warning records.
- **Policy Table Π : Currency → (precision, policy)** — per-currency settings.

**Notation:**
```
e ⇓ v ; ℛ' ; 𝒲'
```
Means: evaluating `e` yields value `v`, ledger `ℛ'`, and warnings `𝒲'`.

### 5.1 Money Construction
```
canonC(v) = (units, rem)  // per pC, policyC
------------------------------------------------
money(v, C) ⇓ money(C, units*uC) ; ℛ[C] += rem ; warn_if_needed(rem)
```

### 5.2 Addition/Subtraction
```
e1 ⇓ m1 ; ℛ1 ; 𝒲1    e2 ⇓ m2 ; ℛ2 ; 𝒲2   cur(m1) = cur(m2) = C
---------------------------------------------------------------
e1 + e2 ⇓ m1 ⊕C m2 ; merge(ℛ1, ℛ2) ; merge(𝒲1, 𝒲2)
```

### 5.3 Scalar Multiplication
```
e ⇓ m ; ℛ ; 𝒲
let x = value(m) * k
canonC(x) = (units', rem')
-------------------------------------------------
e * k ⇓ money(C, units'*uC) ; ℛ[C] += rem' ; warn_if_needed(rem')
```

### 5.4 Integer Division (//)
```
e ⇓ m ; ℛ ; 𝒲     n > 0
q_u = units(m) // n
r_u = units(m) - q_u*n
------------------------------------------------
e // n ⇓ <money(C, q_u*uC), money(C, r_u*uC)> ; ℛ ; 𝒲
```

### 5.5 Even Split
```
e ⇓ m ; ℛ ; 𝒲     n > 0
q_u = units(m) // n
r_u = units(m) mod n
Produce list: r_u shares of (q_u+1)uC else q_u*uC
-------------------------------------------------
divide_evenly(e, n) ⇓ [m1..mn] ; ℛ ; 𝒲
```

### 5.6 Escrow Even Split
```
e ⇓ total ; ℛ ; 𝒲     n > 0
a = units(total)    u = uΠ(C)
q = trunc(a / n)    r = a - q*n
shares = [money(C, q*u)]^n
escrow = money(C, r*u)
------------------------------------------------
divide_evenly_escrow(e, n) ⇓ {shares, escrow} ; ℛ ; 𝒲
```
Ledger unaffected — no sub-quantum generated.

### 5.7 Currency Conversion
```
e ⇓ m ; ℛ ; 𝒲
let x = value(m) * rate
canonC2(x) = (units', rem')
---------------------------------------------------
convert(e, C2, rate) ⇓ money(C2, units'*uC2) ; ℛ[C2] += rem' ; warn_if_needed(rem')
```

### 5.8 Drip Remainders

**Log-only (commit=false or default):**
```
e ⇓ • ; ℛ ; 𝒲
kC = ⌊ |ℛ[C]| / thC ⌋
potential[C] = sign(ℛ[C]) * kC * thC
---------------------------------------------------
drip_remainders(th, false, label) ⇓ {} ; ℛ ; 𝒲
/// Side effect: append to audit log: {ℛ, potential, th, label, timestamp}
```

**Commit:**
```
e ⇓ • ; ℛ ; 𝒲
kC = ⌊ |ℛ[C]| / thC ⌋
emit[C] = sign(ℛ[C]) * kC * thC
ℛ'[C] = ℛ[C] - emit[C]
---------------------------------------------------
drip_remainders(th, true, label) ⇓ {C ↦ money(C, units(emit[C])*uΠ(C))} ; ℛ' ; 𝒲
/// Side effect: append to audit log: {before: ℛ, after: ℛ', emitted: emit, label, timestamp}
```

- No rounding — only exact multiples leave the ledger.
- Conservation holds: `value_out + ℛ' = value_out + ℛ`.

## 6. Conservation Theorem

For any well-typed `e` in currency C:
```
input_total_C = output_total_C + ℛ[C]
```
Holds under all operations above.

## 7. Error & Policy Behavior

- **truncate** — store quantum part, ledger remainder.
- **warn** — as truncate, plus append to warning log if remainder ≠ 0.
- **strict** — throw MoneyPrecisionError if remainder ≠ 0.
- **Cross-currency ±** → CurrencyError.
- **Division by zero** → MoneyDivisionError.

## 8. Desugaring & Warnings

### Syntactic Sugar
```
divide_evenly(A // n) ≡ divide_evenly(A, n)
```

### Warning Behavior
- Warnings (`warn` policy) occur only at canonicalization, ensuring no silent loss.
- Warning log 𝒲 is append-only until explicitly cleared or deleted.

## 9. Implementation Notes

This version makes audit logs and warning logs explicit, keeps your escrow and split semantics, and bakes in `drip_remainders` as a first-class, trackable operation.

From here, an interpreter just needs to:
1. Maintain three state structures: ℛ (remainders), 𝒲 (warnings), Π (policies).
2. Append to logs as described.
3. Apply policy in `canonC()` consistently.
