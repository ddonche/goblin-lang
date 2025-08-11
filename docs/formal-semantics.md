# Goblin Money System — Formal Semantics (v0.3, Precision & Policy)

## 1. Overview

Goblin treats money as **major units + explicit remainder** for any fraction smaller than the declared precision. **No rounding ever occurs** unless the policy is explicitly round (not recommended).

Every currency C has:
- **Precision pC**: number of decimal places stored in the value (default: 2)
- **Unit quantum uC = 10^(-pC)** in major units
- **Policy**: how to handle sub-uC remainders:
  - `truncate` → keep floor(|x| / uC) units, ledger the remainder
  - `strict` → throw MoneyPrecisionError if |rem| ≥ uC
  - `warn` → same as truncate but emit a warning

**Global Invariant (per currency C):**
```
sum(inputs in C) = sum(outputs in C) + sum(remainders in C)
```

## 2. Syntax (Excerpt)

### 2.1 Types
```
τ ::= int | float | money(C) | <money(C), money(C)> | list(money(C))
```

### 2.2 Expressions
```
e ::= money(v, C) | $vC | e + e | e - e | e * k | e // n
     | divide_evenly(e, n) | convert(e, C2, rate)
```

## 3. Canonicalization (Precision + Policy)

For currency C:
- `uC = 10^(-pC)`
- `to_unitsC(x) = trunc(x / uC)` (truncate toward zero)

**canonC(x):**
1. `cents = to_unitsC(x)`
2. `rem = x - cents*uC`
3. Apply policy:
   - `truncate`: accept, ledger rem
   - `warn`: accept, ledger rem, emit warning if rem ≠ 0
   - `strict`: if rem ≠ 0 → MoneyPrecisionError

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

### Conversion
```
Γ ⊢ e : money(C1)   Γ ⊢ r : ℚ
──────────────────────────────────────
Γ ⊢ convert(e, C2, r) : money(C2)
```

## 5. Evaluation with a Remainder Ledger

Ledger **ℛ : Currency → ℚ** holds all remainders smaller than uC.

**Notation:**
```
e ⇓ v ; ℛ'
```
Means: evaluating `e` yields value `v` and ledger `ℛ'`.

### 5.1 Money Construction
```
canonC(v) = (units, rem)  // using pC, policyC
---------------------------------------------
money(v, C) ⇓ money(C, units*uC) ; ℛ[C] += rem
```

### 5.2 Addition/Subtraction
```
e1 ⇓ m1 ; ℛ1      e2 ⇓ m2 ; ℛ2    cur(m1) = cur(m2) = C
--------------------------------------------------------
e1 + e2 ⇓ m1 ⊕C m2 ; merge(ℛ1, ℛ2)
```

### 5.3 Scalar Multiplication
```
e ⇓ m ; ℛ
let x = value(m) * k
canonC(x) = (units', rem')  // using pC, policyC
-------------------------------------------------
e * k ⇓ money(C, units'*uC) ; ℛ[C] += rem'
```

### 5.4 Division (//)
```
e ⇓ m ; ℛ     n > 0
q_u = units(m) // n
r_u = units(m) - q_u*n
-----------------------------------------------
e // n ⇓ <money(C, q_u*uC), money(C, r_u*uC)> ; ℛ
```

### 5.5 Even Split
```
e ⇓ m ; ℛ     n > 0
q_u = units(m) // n
r_u = units(m) mod n
Produce list: r_u shares of (q_u+1)uC else q_u*uC
-------------------------------------------------
divide_evenly(e, n) ⇓ [m1..mn] ; ℛ
```

### 5.6 Currency Conversion
```
e ⇓ m ; ℛ
let x = value(m) * rate
canonC2(x) = (units', rem')  // using pC2, policyC2
---------------------------------------------------
convert(e, C2, rate) ⇓ money(C2, units'*uC2) ; ℛ[C2] += rem'
```

## 6. Conservation Theorem

For any well-typed expression `e` in currency C:
```
input_total_C = value_C + ℛ[C]
```
Holds for all ops given the above rules.

## 7. Error & Policy Behavior

- **truncate**: Always stores quantum part, ledgers remainder
- **warn**: Same as truncate + emit warning if remainder ≠ 0
- **strict**: If remainder ≠ 0, raise MoneyPrecisionError
- **cross-currency add/sub**: CurrencyError
- **division by zero**: MoneyDivisionError

## 8. Practical Defaults

### Retail scenario:
```goblin
default money USD precision: 2 policy: truncate
```

### Bank scenario:
```goblin
default money USD precision: 5 policy: truncate
```

### Strict accounting:
```goblin
default money USD precision: 2 policy: strict
```
