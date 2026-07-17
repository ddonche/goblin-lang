**`count`** returns how many things are in a string or collection.

::: shard
See [[Collections]] for collection basics and [[Strings]] for text helpers. This page covers **count** specifically.
:::

---

## Overview

Use `count` when you want the size of a value:

```goblin
names | ["daniel", "dave", "amanda", "frank"]

:count(names)  /// 4
```

For size, `count` is the same thing as `len`:

```goblin
x | [1, 2, 3, 4, 5]

:count(x)  /// 5
:len(x)    /// 5
```

Dot style is also available:

```goblin
x.count   /// 5
x.len     /// 5
x.length  /// 5
```

---

## Call Styles

The primary form is the builtin call:

```goblin
:count(names)
```

Dot style is a shorthand:

```goblin
names.count
names.count()
```

For length-only checks, these are equivalent:

```goblin
:count(names)
:len(names)
:length(names)

names.count
names.len
names.length
```

---

## Arrays

For arrays, `count` returns the number of elements:

```goblin
scores | [10, 20, 30]

:count(scores)  /// 3
```

Nested arrays count as one element each:

```goblin
rows | [[1, 2], [3, 4]]

:count(rows)  /// 2
```

---

## Strings

For strings, `count` returns the number of characters:

```goblin
name | "Goblin"

:count(name)  /// 6
```

This counts characters, not bytes:

```goblin
word | "café"

:count(word)  /// 4
```

---

## Maps

For maps, `count` returns the number of entries:

```goblin
person | {name: "Mina", role: "Archivist"}

:count(person)  /// 2
```

It counts keys, not nested values.

---

## Nil and Unit

`:count(nil)` returns `0`:

```goblin
:count(nil)  /// 0
```

Dot style also treats `nil.count`, `nil.len`, and `nil.length` as `0`:

```goblin
missing | nil

:count(missing)  /// 0

missing.count    /// 0
missing.len      /// 0
```

---

## Counting Substrings

With two string arguments, `count` counts non-overlapping occurrences of a substring:

```goblin
spell | "abracadabra"

:count(spell, "abra")  /// 2
```

Dot style works too:

```goblin
spell.count("a")  /// 5
```

An empty substring returns `0`:

```goblin
:count("Goblin", "")  /// 0
```

Use [[Count Matching]] when you want to count regex matches.

---

## Count vs Len

The builtin form is the main form:

```goblin
if :count(cart) > 0
    checkout_enabled | true
end
```

```goblin
if :len(password) < 12
    password_ok | false
end
```

Dot style is available when it reads better inside a chain or a quick check:

```goblin
if cart.count > 0
    checkout_enabled | true
end
```

`count` feels natural for collections. `len` feels natural for strings. In Goblin, they answer the same length question.

---

## Signature

```goblin
:count(value)
value.count
value.count()

:len(value)
value.len
value.length

:count(text, substring)
text.count(substring)
```

| Argument | Type | Description |
| -------- | ---- | ----------- |
| `value` | string, array, seq, map, nil, or unit | The value to measure. |
| `text` | string | The string to search. |
| `substring` | string | The substring to count. |

Returns an integer.

---

## Errors

`count` accepts one argument for size, or two string arguments for substring counting:

```goblin
:count()
/// error: R0301 wrong-arity

:count("Goblin", "o", "extra")
/// error: R0301 wrong-arity
```

Unsupported values raise a type error:

```goblin
:count(42)
/// error: T0205 type-mismatch
```

Substring counting requires both arguments to be strings:

```goblin
:count("Goblin", 1)
/// error: T0205 type-mismatch
```

---

::: nav
next [[Count Matching]]
previous [[Copy File]]
:::

::: nav
related [[Len]]
related [[Length]]
related [[Collections]]
related [[Strings]]
related [[Count Matching]]
related [[Is Empty]]
:::
