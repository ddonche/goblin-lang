## 19. Gears — Philosophy & Architecture

### 19.1 Purpose
Gears are first‑class, modular extensions that feel native to Goblin. The core stays lean; anything domain‑specific lives in a gear.

**Key properties:**
- **Language‑native**: `use`, namespacing, and `via` are part of Goblin
- **Type‑aware**: gears can register types and capabilities
- **Contract‑checked**: gears must implement declared contracts
- **Deterministic**: pin versions, lock builds, sandbox side effects

### 19.2 Loading & Versioning
```goblin
use shopify@^1.6 as shp
use tarot_deck@1.2
use invoice          /// latest allowed by policy if not pinned (discouraged)
```

**Pin by default**: Projects should reference a version/range.

**Aliases**: `as` sets a local alias (e.g., `shp::csv`).

**Lockfile**: `gears.lock` records `{gear, version, checksum, source}`.
Builds resolve only from the lock unless `--update` is used.

**CLI behavior (informative):**
- `gears install` writes/updates `gears.lock`
- `goblin build` fails if a required gear isn't locked (deterministic by default)

### 19.3 Capability Resolution
Gears declare capabilities (named functions/templates/exporters). Calls resolve deterministically:

**Order of precedence:**
1. Call‑site `via`:
   ```goblin
   export @cards via shp::csv
   ```

2. Global preference:
   ```goblin
   prefer product.export via shp
   ```

3. Project config default map (`goblin.config.yaml`)

Ambiguity error if multiple providers remain.

**Namespacing:**
- Public symbols: `gear::Symbol` (e.g., `shopify::csv`)
- Use `via gear::symbol` to bind a call to a provider

### 19.4 Contracts (First‑Class)
Contracts define the shape and errors of a capability and are checked at use time.

```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end
```

**Rules:**
- Signature (names, arity, types) must match exactly in any implementing gear
- Only declared errors may be thrown. Others are wrapped as `GearError(gear, capability, cause)`
- Contracts are global identifiers (`product.export`); gears register implementations

**Introspection:**
```goblin
gear_contracts("shopify")   /// ["product.export", "inventory.sync", ...]
```

### 19.5 Gear Manifest & Permissions
Each gear ships a `gear.yaml`:

```yaml
name: shopify
version: 1.6.2
provides:
  - product.export
  - inventory.sync
contracts:
  - product.export
requires:
  fs:
    - "dist/"               # path allowlist
  net:
    - "api.shopify.com"     # host allowlist
  env:
    - "SHOPIFY_TOKEN"
permissions:
  mode: "fs+net"            # none | fs | fs+net
checksum: "sha256-…"
```

**On use:**
- Core validates manifest and permissions against project policy
- In `--deterministic` mode, network is blocked unless allowlisted

**Introspection:**
```goblin
gear_permissions("shopify")
```

### 19.6 Event Bus
Lightweight, in‑process bus with clear sync/async semantics.

**Emit:**
```goblin
emit "catalog.ready", payload            /// synchronous (errors bubble)
emit_async "order.ready", payload        /// enqueued (returns job id)
```

**Subscribe:**
```goblin
on "order.ready" mode: "async" concurrency: 4 error: "collect"
    ...
end
```

**Options:**
- `mode`: `"sync"|"async"` (default "sync" in code; "async" recommended for long tasks)
- `concurrency`: workers for async handlers (default 1)
- `error` policy:
  - `"stop"` (default for sync): first error aborts, bubbles
  - `"skip"`: ignore handler errors, continue others
  - `"collect"`: aggregate errors; expose via handler result

### 19.7 Sandbox & Side‑Effects
**Sandbox modes:**
- `"none"`: no FS/NET (pure)
- `"fs"`: file I/O allowed only on allowlisted paths
- `"fs+net"`: file I/O plus outbound HTTP to allowlisted hosts

**Enforced by:**
- Gear manifest `permissions.mode` + `requires.fs/net`
- Project overrides via `goblin.config.yaml`
- `--deterministic` build flag downgrades to "fs" or "none" unless explicitly allowed

### 19.8 Determinism, Lockfile, Dry‑Run
Lockfile required for reproducible builds.

`goblin build --deterministic`:
- Blocks wall‑clock (use `trusted_now()` only if permitted)
- Blocks network unless allowlisted
- Fails on nondeterministic randomness (use seeded rand)

**Dry‑run support on exporter contracts:**
```goblin
file = product.export(@items) via shp dry_run:true
```

### 19.9 Logging & Telemetry
Standard JSONL: `dist/gear.log`

```json
{"ts":"2025-08-12T15:30:00Z","gear":"shopify","cap":"product.export","ms":128,"ok":true}
{"ts":"2025-08-12T15:30:01Z","gear":"shopify","cap":"product.export","ok":false,"err":"ValidationError: missing title"}
```

Emitted by core around capability calls.

No phoning home. External telemetry only if a gear explicitly implements it and permissions allow.

### 19.10 Testing Hooks
Inline gear tests run in a sandbox:

```goblin
test "shopify csv emits header"
    rows = shopify::csv_preview(@cards)
    assert rows[0].starts_with("Handle,Title")
end
```

**CLI:**
- `gears test shopify` runs all test blocks in that gear
- `gears test --all` runs across loaded gears

### 19.11 Introspection APIs
```goblin
gears()                         /// ["shopify","tarot_deck"]
gear_symbols("shopify")         /// ["csv","api","configure", ...]
gear_contracts("shopify")       /// implemented contracts
gear_permissions("shopify")     /// sandbox/allowlists
```

### 19.12 Usage Patterns

#### 19.12.1 Single Export
```goblin
use tarot_deck@1.2, shopify@^1.6 as shp
prefer product.export via shp

@cards = tarot_deck::card_template(price: .99, qty: 1)
    "Ace of Cups"
    "Two of Cups"

ensure_time_verified("shopify export")
file = product.export(@cards) via shp
say "Wrote {file}"
```

#### 19.12.2 Multi‑Platform Chain
```goblin
use board_game, shopify@^1.6 as shp, etsy@^2

@games = board_game::template()
    "Catan" :: qty: 4
    "Pandemic" :: qty: 2

export @games via shp::csv
export @games via etsy::csv
```

#### 19.12.3 Event‑Driven Pipeline
```goblin
emit "catalog.ready", @games

on "catalog.ready" mode: "async" concurrency: 2 error: "collect"
    ensure_time_verified("bulk export")
    product.export($event.payload) via shp
end
```

### 19.13 Errors
- `GearError(gear, capability, cause)` — unknown/undeclared error bubbled from a gear
- `ContractError` — signature mismatch at load/resolution
- `PermissionError` — sandbox/requirement violation
- `AmbiguityError` — multiple providers with no resolution (via/prefer/config)
- `LockfileError` — missing/incompatible lock entries
- `DeterminismError` — disallowed side effect in deterministic mode

### 19.14 Project Config (excerpt)
```yaml
# goblin.config.yaml
gears:
  defaults:
    product.export: shopify
  permissions:
    mode: "fs+net"
    fs_allow:
      - "dist/"
    net_allow:
      - "api.shopify.com"
  enforce_lock: true
  deterministic_build: true
```

### 19.15 Example Contract & Gear Implementation (sketch)
```goblin
contract product.export(items: array<Product>) -> file
    errors: [ValidationError, AuthError]
end

# Provided by shopify gear
file = product.export(@cards) via shopify::csv
```

**Gear side (manifest idea):**
```yaml
name: shopify
version: 1.6.2
provides: [product.export]
contracts: [product.export]
requires:
  fs: ["dist/"]
  net: ["api.shopify.com"]
permissions:
  mode: "fs+net"
```
