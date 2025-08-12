# Goblin Gears — Philosophy & Architecture

## Overview

Gears are first-class, modular extensions for the Goblin programming language.
Unlike traditional plugin systems, Gears are built into Goblin's DNA — the language syntax, type system, and runtime are all designed with Gears in mind.

**The goal:**
- Keep the core of Goblin lean and focused
- Push domain-specific or specialized functionality into Gears  
- Make Gears feel native, not bolted on

When someone uses Goblin, they should assume they will be using Gears — it's not an "advanced" feature.
**Goblin is the gear language.**

## Why Gears Are Different

Other systems either:
- **Have fat cores with plugins** (Rails, WordPress)
- **Have lean cores but treat plugins as second-class** (UNIX scripts, DSLs)  
- **Have contract systems but require heavyweight service plugins** (Terraform providers, cloud SDKs)

**Goblin's Gears are unique because:**

### 1. Language-native integration
Gears plug directly into Goblin grammar:

```goblin
use shopify, tarot_deck

@cards = tarot_deck.template()
    "Ace of Cups"
    "Two of Cups"

export @cards via shopify
```

### 2. Type-aware
Gears register types and capabilities directly with Goblin's type system.

### 3. Contract enforcement
Gears must match well-defined contracts that Goblin validates at load time.

### 4. No service dependency
Gears run in-process by default but can scale out-of-process if needed.

## Core Philosophy

### 1. Lean Core

The Goblin core:
- Provides only the universal language essentials
- Includes built-in types (money, percent, date, etc.)
- Handles parsing, execution, type enforcement, and gear loading

**Anything domain-specific or platform-specific goes into a Gear.**

### 2. Gears as First-Class Citizens

Gears are not "extensions" in the traditional sense — they are the primary way to expand Goblin.

To make them first-class:
- **Native syntax** (`use gear_name`)
- **Native type registration** (gears add types the same way core does)
- **Native event system** (gears emit/subscribe without extra libraries)
- **Native contract validation** (if a gear says it implements `product.export`, Goblin checks it)

### 3. Capability Model

Every gear declares capabilities — functions, types, events it provides.

Example:
```goblin
gear shopify:
    provides:
        product.export
        inventory.sync
```

When your Goblin code calls:
```goblin
product.export(my_items)
```

The runtime:
- Looks up which gear(s) provide `product.export`
- Resolves conflicts by explicit user preference, default order, or config

### 4. Event Bus

All Gears share a lightweight in-process event bus:
- Gears emit named events with payloads
- Other gears can subscribe to these events
- Events are synchronous by default, async if specified
- No "offline" gears — all loaded gears are always active

Example:
```goblin
emit "user.logged_in", user_id

on "user.logged_in":
    send_welcome_email(user_id)
```

### 5. Contracts

Contracts define what a capability looks like:
- Function signatures
- Type expectations  
- Error behavior

Stored centrally so any gear implementing a capability is guaranteed to be compatible.

Example contract:
```goblin
contract product.export(items: list[product]) -> file
```

### 6. Data Sharing

- All loaded gears share a common runtime context
- State can be passed explicitly via events or stored in shared memory
- No global variable sprawl — shared state must be registered

## Example Use Cases

### Shopify Listing Generator

1. **tarot_deck gear:** generates card product data
2. **shopify gear:** formats & exports to Shopify CSV
3. **You:**

```goblin
use tarot_deck, shopify

@cards = tarot_deck.template()
    "Ace of Cups"
    "Two of Cups"

export @cards via shopify
```

### Multi-Platform Export

You can chain multiple gears for multi-platform listings:

```goblin
use board_game, shopify, etsy

@games = board_game.template()
    "Catan" :: qty: 4
    "Pandemic" :: qty: 2

export @games via shopify, etsy
```

## Why This Matters

**Developers don't ask:** *"Can Goblin do X?"*

**They ask:** *"Is there a gear for X?"*

And if there isn't, they write one — without hacking the core.

- **Goblin stays lean**
- **Gears stay native**  
- **The ecosystem grows without turning into dependency hell**

## In One Line

> **Goblin is a lean, domain-agnostic core language, designed from day one to be extended by native-feeling, first-class gears — making specialized programming as simple as writing plain Goblin.**
