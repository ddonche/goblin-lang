**`clamp`** keeps a number within a minimum and maximum value.

::: shard
See [[Numbers]] for Goblin's numeric operations. This page covers **clamp** specifically.
:::

---

## Overview

Use `clamp` when a value must stay inside an allowed range:

```goblin
volume | 125.clamp(0, 100)

:say(volume)  /// 100
```

Values below the minimum become the minimum. Values above the maximum become the maximum.

---

## Method Style

Call `clamp` on the value, followed by the lower and upper bounds:

```goblin
temperature | (-12).clamp(0, 30)

:say(temperature)  /// 0
```

The equivalent free-call form is:

```goblin
temperature | clamp(-12, 0, 30)
```

Both forms behave the same way.

---

## Below the Minimum

If the value is less than the lower bound, `clamp` returns the lower bound:

```goblin
(-5).clamp(0, 10)  /// 0
```

---

## Above the Maximum

If the value is greater than the upper bound, `clamp` returns the upper bound:

```goblin
18.clamp(0, 10)  /// 10
```

---

## Inside the Range

If the value is already between the bounds, it is returned unchanged:

```goblin
7.clamp(0, 10)  /// 7
```

The minimum and maximum themselves are included in the allowed range:

```goblin
0.clamp(0, 10)   /// 0
10.clamp(0, 10)  /// 10
```

---

## Equal Bounds

The lower and upper bounds may be equal. In that case, every value becomes that bound:

```goblin
42.clamp(5, 5)  /// 5
```

---

## Integers and Floats

`clamp` currently accepts integers and floats.

If the value and both bounds are integers, the result is an integer:

```goblin
15.clamp(0, 10)  /// 10
```

If any of the three values is a float, the result is a float:

```goblin
7.clamp(0.0, 10)  /// 7.0
```

Percentages and big numbers are not currently accepted by the interpreter's `clamp` operation.

---

## Bound Order

The lower bound must be less than or equal to the upper bound:

```goblin
5.clamp(10, 0)
/// error: R0207 math-domain
```

Swap the bounds when they are in the wrong order.

---

## Signature

```goblin
value.clamp(minimum, maximum)
clamp(value, minimum, maximum)
```

| Argument | Type | Description |
| -------- | ---- | ----------- |
| `value` | integer or float | The number to limit. |
| `minimum` | integer or float | The lowest allowed result. |
| `maximum` | integer or float | The highest allowed result. |

`clamp` returns the value, the minimum, or the maximum according to where the value falls.

---

## Errors

`clamp` requires a value and two bounds:

```goblin
clamp(5, 0)
/// error: R0301 wrong-arity
```

All three arguments must be integers or floats:

```goblin
"5".clamp(0, 10)
/// error: T0205 type-mismatch
```

The minimum cannot be greater than the maximum:

```goblin
clamp(5, 10, 0)
/// error: R0207 math-domain
```

---

::: nav
next [[Clear All Tokens]]
previous [[Chars]]
:::

::: nav
related [[Numbers]]
related [[Min]]
related [[Max]]
related [[Ranges]]
related [[Traits]]
:::
