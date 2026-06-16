**`clear_token`** removes one registered token from a token namespace.

::: shard
See [[Tokens]] for Goblin's token registry. This page covers **clear_token** specifically.
:::

---

## Overview

Use `clear_token` when one token should be removed without disturbing the other tokens in its namespace:

```goblin
register_token("THEME", "ACCENT", "purple")
register_token("THEME", "BACKGROUND", "black")

clear_token("THEME", "ACCENT")

:say(list_tokens("THEME"))  /// {BACKGROUND: black}
```

Only `ACCENT` is removed. `BACKGROUND` remains registered.

---

## Call Styles

`clear_token` can be called as a free function:

```goblin
clear_token("THEME", "ACCENT")
```

It also supports method style, with the namespace as the receiver:

```goblin
"THEME".clear_token("ACCENT")
```

Both forms remove the same token.

---

## Namespace and Identifier

The first argument identifies the namespace. The second identifies the token inside it:

```goblin
clear_token("SITE", "TITLE")
```

This removes `TITLE` from `SITE`. A token with the same identifier in another namespace is unaffected:

```goblin
register_token("SITE", "TITLE", "Goblin Journal")
register_token("ARTICLE", "TITLE", "A Night in the Library")

clear_token("SITE", "TITLE")

:say(resolve_token("ARTICLE", "TITLE"))  /// A Night in the Library
```

---

## Namespace Capitalization

Token namespace names are case-insensitive. Goblin normalizes them to uppercase:

```goblin
register_token("theme", "ACCENT", "purple")
clear_token("THEME", "ACCENT")

:say(list_tokens("theme"))  /// {}
```

Token identifiers are case-sensitive. `ACCENT` and `accent` are different identifiers:

```goblin
register_token("THEME", "ACCENT", "purple")
register_token("THEME", "accent", "green")

clear_token("THEME", "ACCENT")

:say(list_tokens("THEME"))  /// {accent: green}
```

---

## Missing Tokens

Clearing a token that is not registered succeeds without an error:

```goblin
clear_token("THEME", "MISSING")
```

The same is true when the namespace does not exist:

```goblin
clear_token("UNKNOWN", "MISSING")
```

In both cases, the token registry remains unchanged.

---

## Return Value

`clear_token` returns `unit`:

```goblin
result | clear_token("THEME", "ACCENT")

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
clear_token(namespace, token)
namespace.clear_token(token)
```

| Argument | Type | Description |
| -------- | ---- | ----------- |
| `namespace` | string | The namespace containing the token. |
| `token` | string | The identifier of the token to remove. |

Returns `unit`.

---

## Errors

`clear_token` requires exactly two arguments in free-call style, or one argument after the receiver in method style:

```goblin
clear_token("THEME")
/// error: R0301 wrong-arity
```

Both the namespace and token identifier must be strings:

```goblin
clear_token(42, "ACCENT")
/// error: T0205 type-mismatch

clear_token("THEME", 42)
/// error: T0205 type-mismatch
```

A missing token is not an error.

---

::: nav
next [[Clear Tokens]]
previous [[Clear Format]]
:::

::: nav
related [[Tokens]]
related [[Register Token]]
related [[Resolve Token]]
related [[List Tokens]]
related [[Clear Tokens]]
related [[Clear All Tokens]]
:::
