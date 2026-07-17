**`needs`** declares what a glam expects the outside project to provide.

::: shard
See [[Glams]] for the big picture. This page covers the **needs** section in `glam.toml`.
:::

---

## Overview

A glam can be written as a reusable package, but it should not have to hard-code every project-specific value or provider action. The `needs` section lets the glam say, "I need this value" or "I need this action, but the project decides where it comes from."

`needs` lives in `glam.toml`:

```toml
[needs.values]
content_dir = "#site::content_dir"

[needs.actions]
insert = "goblin_supabase::insert"
```

There are two kinds of needs:

| Section | Purpose |
| ------- | ------- |
| `[needs.values]` | Pulls Box values into the glam as local names. |
| `[needs.actions]` | Maps a local need name to a provider action. |

---

## Value Needs

Use `[needs.values]` when the glam needs a value supplied by the project:

```toml
[needs.values]
content_dir = "#site::content_dir"
site_title = "#site::title"
```

Each entry maps a local name to a Box reference. The Box reference must start with `#` and use `namespace::name`:

```toml
local_name = "#namespace::name"
```

When the glam loads, Goblin looks up the Box value and defines the local name for the glam:

```goblin
content_dir
site_title
```

The glam code can use `content_dir` without knowing which project namespace supplied it.

---

## Action Needs

Use `[needs.actions]` when the glam wants to call a provider action without hard-coding that provider in the glam code:

```toml
[needs.actions]
insert = "goblin_supabase::insert"
```

Inside the glam, call the need by its local name:

```goblin
action save_post(post)
    :need("insert", "posts", post)
end
```

Goblin resolves `insert` through the glam's `[needs.actions]` table and calls the configured provider action:

```goblin
goblin_supabase::insert("posts", post)
```

The arguments after the need name are forwarded to the provider action.

---

## Why Use Needs

`needs` keeps a glam reusable. The glam says what it needs, while the project decides what satisfies that need.

For example, a blog glam might need an `insert` action. One project can wire that to Supabase:

```toml
[needs.actions]
insert = "goblin_supabase::insert"
```

Another project can wire the same glam to a local JSON provider:

```toml
[needs.actions]
insert = "local_store::insert"
```

The glam code does not change:

```goblin
:need("insert", "posts", post)
```

---

## Needs Values vs Values

`[needs.values]` and `[values]` are related, but they are not the same thing.

`[needs.values]` pulls values from outside the glam:

```toml
[needs.values]
content_dir = "#site::content_dir"
```

`[values]` defines default values owned by the glam itself:

```toml
[values]
theme = "midnight"
```

Use `[needs.values]` when the project must supply the value. Use `[values]` when the glam owns the default.

---

## Needs Actions vs Direct Calls

A direct provider call ties the glam to one provider:

```goblin
goblin_supabase::insert("posts", post)
```

A need call lets the project choose the provider:

```goblin
:need("insert", "posts", post)
```

That makes the glam easier to reuse, test, and swap between environments.

---

## Shape

The full `needs` shape is:

```toml
[needs.values]
local_value_name = "#box_namespace::box_name"

[needs.actions]
local_action_name = "provider_namespace::provider_action"
```

Value needs use Box references with a leading `#`.

Action needs use action paths without a leading `#`.

---

## Errors

A value need must be a non-empty string Box reference:

```toml
[needs.values]
content_dir = ""
```

```text
error: B0104 unresolved-glam-need
```

This is also invalid because action paths belong in `[needs.actions]`, not `[needs.values]`:

```toml
[needs.values]
content_dir = "site::content_dir"
```

Value needs must use the `#namespace::name` form.

---

An action need must be a non-empty string action path:

```toml
[needs.actions]
insert = ""
```

```text
error: B0106 missing-action-need
```

This is invalid because Box references belong in `[needs.values]`, not `[needs.actions]`:

```toml
[needs.actions]
insert = "#goblin_supabase::insert"
```

Action needs must use the `namespace::action` form.

---

Calling `need` outside a glam action is an error:

```goblin
:need("insert", "posts", post)
```

```text
error: B0108 need-outside-glam
```

`need` only has a glam context when it runs inside an action defined by a loaded glam.

---

If a need points to an action that does not exist or has not been loaded, Goblin raises a missing provider action error:

```text
error: B0107 missing-provider-action
```

Make sure the provider glam is loaded and exports the action named in `[needs.actions]`.

---

## Signatures

In `glam.toml`:

```toml
[needs.values]
name = "#namespace::value"

[needs.actions]
name = "namespace::action"
```

In Goblin code:

```goblin
:need(name, ...args)
```

| Argument | Type | Description |
| -------- | ---- | ----------- |
| `name` | string | The local action need name from `[needs.actions]`. |
| `...args` | any | Arguments forwarded to the provider action. |

`need` returns whatever the provider action returns.

---

::: nav
next [[Need]]
previous [[Glams]]
:::

::: nav
related [[Glams]]
related [[Glam Toml]]
related [[Need]]
related [[Box]]
related [[Provides]]
:::
