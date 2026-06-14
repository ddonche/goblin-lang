**`clear_all_tokens`** removes every registered token from every token namespace.

::: shard
See [[Tokens]] for Goblin's token registry. This page covers **clear_all_tokens** specifically.
:::

---

## Overview

Use `clear_all_tokens` when the entire session token registry should be emptied:

```goblin
register_token("THEME", "ACCENT", "purple")
register_token("SITE", "TITLE", "Cave Journal")

clear_all_tokens()
```

After the call, neither namespace contains registered tokens.

---

## Clears Every Namespace

`clear_all_tokens` is global to the current Goblin session. It does not take a namespace argument:

```goblin
register_token("THEME", "ACCENT", "purple")
register_token("THEME", "BACKGROUND", "black")
register_token("SITE", "TITLE", "Cave Journal")

clear_all_tokens()

list_tokens()  /// {}
```

Use [[Clear Tokens]] when only one namespace should be removed.

---

## What It Does Not Clear

`clear_all_tokens` clears the token registry only. It does not remove ordinary variables:

```goblin
title | "Cave Journal"
register_token("SITE", "TITLE", title)

clear_all_tokens()

:say(title)  /// Cave Journal
```

Values that were already copied into variables remain available. Future token lookups can no longer retrieve the cleared registrations.

---

## Already Empty

Calling `clear_all_tokens` when no tokens are registered succeeds without an error:

```goblin
clear_all_tokens()
clear_all_tokens()
```

---

## Return Value

`clear_all_tokens` returns `unit`:

```goblin
result | clear_all_tokens()

:say(result.vt)  /// unit
```

Its purpose is the change it makes to the token registry.

---

## Related Clearing Operations

Goblin provides three levels of token removal:

| Operation | Removes |
| --------- | ------- |
| `clear_token(namespace, token)` | One token from one namespace. |
| `clear_tokens(namespace)` | Every token in one namespace. |
| `clear_all_tokens()` | Every token in every namespace. |

---

## Signature

```goblin
clear_all_tokens()
```

`clear_all_tokens` takes no arguments and returns `unit`.

---

## Errors

Passing any argument raises a wrong-arity error:

```goblin
clear_all_tokens("THEME")
/// error: R0301 wrong-arity
```

Use `clear_tokens("THEME")` to clear one namespace.

---

::: nav
next [[Clear Format]]
previous [[Clamp]]
:::

::: nav
related [[Tokens]]
related [[Register Token]]
related [[Resolve Token]]
related [[List Tokens]]
related [[Clear Token]]
related [[Clear Tokens]]
:::
