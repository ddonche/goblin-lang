# Goblin Money System — Formal Semantics (v0.3.2, Precision, Policy & Ledger Extensions)

## 1. Overview

Goblin treats money as **major units + explicit remainder** for any fraction smaller than the declared precision. **No rounding ever occurs** unless the policy explicitly requests it (not recommended).

Every currency C has:
- **Precision pC**: number of decimal places (default: 2)
- **Unit quantum uC = 10^(-pC)** in major units
- **Policy**: handling of sub-uC remainders:
  - `truncate` → keep floor(|x| / uC) units, ledger remainder
  - `warn` → same as truncate but emit a warning
  - `strict` → throw MoneyPrecisionError if remainder ≠ 0
  - `allocate_evenly` → redistribute remainder among recipients (for splits)
  - `escrow` → set aside remainder in escrow record (bookkeeping)

**Global Invariant (per currency C):**
```
sum(inputs_C) = sum(outputs_C) + sum(remainders_C)
```
No quantum is ever created or destroyed; all sub-quantum is explicit.

## 2. Syntax

### 2.1 Types
```
τ ::= int 
    | float 
    | money(C) 
    | <money(C), money(C)> 
    | list(money(C))
    | escrow(C) 
    | json
```

### 2.2 Expressions
```
e ::= money(v, C) | $vC
     | e + e | e - e | e * k | e // n
     | divide_evenly(e, n) 
     | divide_evenly_escrow(e, n)
     | convert(e, C2, rate)
     | drip_remainders(th?, commit?, label?)
     | ledger_json()
```

## 3. Canonicalization

For currency C:
- `uC = 10^(-pC)`
- `to_unitsC(x) = trunc(x / uC)` (toward zero)

**canonC(x):**
1. `units = to_unitsC(x)`
2. `rem = x - units*uC`
3. Apply policy:
   - `truncate`: accept; ledger rem
   - `warn`: same as truncate + log warning
   - `strict`: if rem ≠ 0, error
   - `allocate_evenly`: store rem for redistribution step
   - `escrow`: move rem to escrow ledger

## 4. Typing Rules

### Same-Currency Arithmetic
```
Γ ⊢ e1 : money(C)   Γ ⊢ e2 : money(C)
──────────────────────────────────────
Γ ⊢ e1 ± e2 : money(C)
```

### Scalar Multiply
```
Γ ⊢ e : money(C)   Γ ⊢ k : ℚ
──────────────────────────────
Γ ⊢ e * k : money(C)
```

### Integer Division with Remainder
```
Γ ⊢ e : money(C)   Γ ⊢ n : int, n > 0
────────────────────────────────────────
Γ ⊢ e // n : <money(C), money(C)>
```

### Even Split
```
Γ ⊢ e : money(C)   Γ ⊢ n : int, n > 0
────────────────────────────────────────
Γ ⊢ divide_evenly(e, n) : list(money(C))
```

### Escrow Split
```
Γ ⊢ e : money(C)   Γ ⊢ n : int, n > 0
──────────────────────────────────────────────
Γ ⊢ divide_evenly_escrow(e, n) : {shares: list(money(C)), escrow: money(C)}
```

### Drip Remainders
```
Γ ⊢ th : ℚ | money(_) | map(Currency, money(_))   [optional]
Γ ⊢ commit : bool   [optional]
Γ ⊢ label : string  [optional]
────────────────────────────────────────────────────────────
Γ ⊢ drip_remainders(th?, commit?, label?) : map(Currency, money(_))
```

### Ledger Export
```
Γ ⊢ ledger_json() : json
```

## 5. Evaluation (Ledger + Escrow)

### 5.1 Money Construction
```
canonC(v) = (units, rem)
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
canonC(x) = (units', rem')
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
If `policy(C) = allocate_evenly`, remainders are distributed round-robin until exhausted.
Otherwise use normal divide_evenly:

```
e ⇓ m ; ℛ     n > 0
q_u = units(m) // n
r_u = units(m) mod n
Produce list: r_u shares of (q_u+1)uC else q_u*uC
-------------------------------------------------
divide_evenly(e, n) ⇓ [m1..mn] ; ℛ
```

### 5.6 Escrow Split
```
e ⇓ total ; ℛ     n > 0
q = trunc(units(total) / n)
r = units(total) - q*n
shares = [ money(C, q*uC) ]^n
escrow = money(C, r*uC)
-------------------------------------------------
divide_evenly_escrow(e, n) ⇓ {shares, escrow} ; ℛ
```

### 5.7 Currency Conversion
```
e ⇓ m ; ℛ
let x = value(m) * rate
canonC2(x) = (units', rem')
---------------------------------------------------
convert(e, C2, rate) ⇓ money(C2, units'*uC2) ; ℛ[C2] += rem'
```

### 5.8 Drip Remainders

**Log-only (commit=false):**
```
ℛ → potential payouts based on threshold th
Append audit log entry {ℛ, potential, th, label, timestamp}
Return {}
```

**Commit (commit=true):**
```
Emit only multiples of th per currency
Update ℛ to subtract emitted amounts
Append audit log entry {before, after, emitted}
Return { C ↦ money(C, emitted) }
```

## 6. JSON Serialization

### ledger_json() →
```json
{
  "timestamp": "...",
  "ledger": { "USD": 0.005, "EUR": 0.002 },
  "escrow": { "USD": 0.01 },
  "policy": { "USD": "truncate", "EUR": "allocate_evenly" }
}
```

### Money serializes as:
```json
{ "currency": "USD", "value": 123.45 }
```

## 7. Conservation Law

For any well-typed expression `e` in currency C:
```
value_C_out + ℛ[C] + escrow_C = value_C_in
```
This holds under all operations and policy modes.

## 8. Error & Policy Behavior

- **truncate**: ledger sub-quantum remainder
- **warn**: same as truncate + warning log entry
- **strict**: error if remainder ≠ 0
- **allocate_evenly**: store remainder for redistribution in current op
- **escrow**: move remainder to escrow ledger

**Errors:**
- Cross-currency arithmetic → CurrencyError
- Divide by zero → MoneyDivisionError
- Precision violation in strict mode → MoneyPrecisionError

## 9. Audit & External System Mode

- Audit log is append-only
- Can run in `external=true` mode where Goblin tracks but does not apply money changes — for monitoring IRS, Fed, bank transactions, etc.
- Remainders can be aggregated and reported without touching source balances

## 10. Desugaring

```
divide_evenly(A // n) ≡ divide_evenly(A, n)
```
