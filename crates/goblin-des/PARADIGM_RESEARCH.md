# DES Paradigm Research Baseline

**Purpose:** retain the investigation already done so that future work starts with the **actual Goblin implementation** and does not repeatedly discover that object-orientation, ECS, ABM, class mutation, overlays/layers, or graph rewrites already exist. This is a research ledger, **not a novelty announcement**.

**Canonical runtime description:** [DESIGN.md](DESIGN.md).  
**Source snapshot inspected:** `jareth` at `e05830db316987656483e578481cba89a5c3ed8f` (2026-11-12).

## Established DES facts: do not propose these again as missing features

- Entity identity is an ordinary opaque ID; `mutate` can replace an entity's class **without** restricting the target to a related kind. Unrelated transitions such as Wolf → Spoon and bacterium → aircraft are valid subject to declared classes and executable triggers. The VM projects matching destination fields and supplies other defaults.
- All eight object transitions—spawn, erase, mutate, split, fracture, merge, absorb, subjugate—are implemented in the VM, each with specific identity, successor, ownership, overlays and link effects. Overlay instances have their own lifecycle and transformations; overlay subjugate is disallowed.
- Decisions, judges, periodic rules, overlays, spread, dynamic kinds, ownership, links, grids, events, ordered tick phases, and region-based execution are already present. Do not present any of them as a suggested invention.
- The `goblin-des` Rust crate alone does **not** implement the whole language model. Much of DES lives in `goblin-vm`. Read `transition.rs`, `tick.rs`, `world.rs` and conformance cases before making capability claims.
- `owner` is containment, not physical location or execution region; explicit `:move!` changes the execution region.
- `:event` is narrative logging and does not itself influence simulated state.
- Class composition uses aspects and interfaces without requiring inheritance.
- Determinism has been tested for **specified seeded simulations and worker counts**. Do not silently generalize to all programs.

### Identity matrix (regression baseline)

| Operation | Existing ID persists? | New IDs? |
| --- | --- | --- |
| Mutate | Yes | No |
| Spawn | Parent yes | Child one |
| Erase | No | No |
| Split | No | Two successors |
| Fracture | Parent yes | Fragment one |
| Merge | Neither original | One successor |
| Absorb | Absorber yes, target no | No |
| Subjugate | Both yes | No |

Source: `tests/conformance/concurrency/world_tick_transition_identities.gbln` and `crates/goblin-vm/src/transition.rs`.

## Already-known neighboring work (not discoveries)

These references have **already been raised and discussed**. Record them here to prevent repeating the exercise without a new question, not to claim they are identical to DES.

| Family | Already-known overlap | Question that would actually matter |
| --- | --- | --- |
| OOP; Common Lisp/CLOS; dynamic reclassification languages | Mutable objects, change-class and identity-preserving type change are known | Are arbitrary category change **and** runtime-managed behavioral/relationship transitions semantically equivalent under a demonstrated translation? |
| ECS / entity architectures | Identified entities and mutable membership/composition are known | Can a given ECS implementation express DES class reclassification and transition guarantees natively, or only as user-authored systems? Either answer needs an exact implementation and comparison. |
| Agent-oriented programming; ABM; NetLogo; GAMA/GAML | Autonomous decisions, agent breeds/species, schedules, actions and simulations exist; dynamic classification has precedents | Compare target-class constraints, identity retention, field projection, runtime ownership/link/overlay continuity and scheduling in a *specific version*, not wolf/dog analogies. |
| Context-/role-/aspect-oriented approaches | Dynamic behavior modification is established | Compare hosted overlay instances, propagation, independent life cycle, overlay transitions and generated kinds, not just "behavior changes." |
| Actor/distributed/concurrent models | Actors, isolated regions, messages, coordinated steps and deterministic variants exist | Compare tick-boundary visibility, cross-region claims, entity movement and reproducibility conditions. |
| Rewriting logic, graph transformations, dynamic graph models | Creation/deletion/rewriting of nodes and relationships are established | Compare expressiveness and operation semantics, especially successor IDs and automatic relation handling, not the presence of split/merge names. |
| "Entity-oriented programming" | The phrase predates Goblin and has multiple unrelated uses | Do not present this label as new or as an uncontested name for DES. |

**Prior discussion is not a substitute for a citation-based equivalence proof.** Entries above are intentionally qualitative; they do not conclude equivalence or originality. If a specific external system is investigated again, add a *dated, versioned primary source*, one executable comparison, and the result below.

## Minimum standard for a genuinely new paradigm claim

A claim is actionable only with all of the following:

1. **Precise semantics:** say which program states exist, which operations modify them, and what observers can read before/after each operation.
2. **Minimal executable witness:** a short, running Goblin example showing the proposed distinguishing behavior (preferably a conformance test).
3. **Closest competing witness:** a program in the nearest existing system that attempts the same behavior, using the closest actually supported primitives.
4. **Explicit point of difference:** identify a missing primitive, semantic restriction, required programmer-managed invariant, observable behavior difference, or computational guarantee. "It is integrated" alone is not a proof.
5. **Counterexample test:** attempt to falsify the claim; document whether translation reproduces the effect. A translation that works must be acknowledged.
6. **Independent terminology audit:** search the proposed name and its synonymous terms **before** classifying it as a new paradigm.
7. **Decision:** `supported`, `refuted`, or `unresolved`, with source versions, references and tests. Do not call `unresolved` "novel".

Until those criteria are met, the accurate description is **Goblin's Dynamic Entity System runtime/programming model**. This is a design specification and classification, not a uniqueness verdict.

## Research work items (not requests to reinvent DES)

### R1 — Unrelated-class mutation

**Goblin witness:** `tests/conformance/core/des_unrelated_class_mutation.gbln` (added with this document), backed by `copy_fields` and `recast` in `transition.rs`.

**Question:** for a specific competitor, does its own runtime let one live identity become an arbitrary unrelated kind, losing nonshared old fields, gaining new defaults and behavior, with the same references observing the changed entity? Is the transformation a runtime primitive or a manually coded reconstruction?

**State:** Goblin semantics established; cross-language equivalence **unresolved**. Don't assume that a breed, role or component change is identical or nonidentical without checking each semantic step.

### R2 — Entity-structural transitions

**Goblin witness:** the transition-identity and split/merge/absorb/subjugate conformance cases.

**Question:** which relationship invariants does another runtime preserve/relocate on structural changes automatically? Compare ownership, overlays, links, reference behavior, hook ordering, and remote claims, including negative tests.

**State:** Goblin semantics established; novel computational capability **unresolved**.

### R3 — Evolving overlay kinds

**Goblin witness:** `tests/conformance/core/des_transition_overlay_kinds.gbln`, `des_overlay_lineage.gbln`, and `sims/capstone/overlays.gbln`.

**Question:** what competitors support runtime generation of new behavior/influence kinds with ancestry, independently transformable hosted instances, spreading and reversible film-like modifiers?

**State:** Goblin implementation established; comparative uniqueness **unresolved**.

### R4 — Synchronization and distributed effects

**Goblin witness:** `world_tick_transition_identities.gbln`, cross-region transition conformance cases, and the seeded simulation check scripts.

**Question:** are region-ordered, barrier-synchronized decisions and claims semantically/observationally distinct from available synchronous actor and graph-rewriting systems? Document corner cases and errors.

**State:** behavior documented; novelty **unresolved**.

## Decision register

| Date | Decision | Basis |
| --- | --- | --- |
| 2026-11-12 | Keep **Dynamic Entity System (DES)** for the implemented model while research continues. | Descriptive, established project name; renaming now would obscure the implementation without showing semantic necessity. |
| 2026-11-12 | **Do not** adopt "Entity-Oriented Programming" as a newly coined paradigm. | The name is already used elsewhere; an independent naming audit is required for any replacement. |
| 2026-11-12 | Do not equate DES with ECS, OOP, NetLogo or GAMA merely because individual primitives overlap. | Similar names/capabilities do not establish semantic equivalence. |
| 2026-11-12 | Do not assert DES is academically unprecedented without a falsifiable comparison. | The objective is to **establish** a new paradigm if the evidence supports one, not presume or deny it. |
| 2026-11-12 | Future proposals must first name the precise unimplemented behavior and cite the VM file/test proving it is absent. | Prevents proposing existing DES features as new ideas. |

## Procedure for the next discussion or proposal

1. Read [DESIGN.md](DESIGN.md) and the relevant **current** implementation/test files. Check whether a later commit has superseded this baseline.
2. Update a changed semantic rule and its regression case in the spec before debating its implications.
3. Consult the known-comparison table **without** re-presenting it as fresh research.
4. Add only a **new, testable** hypothesis or a versioned, verified external finding, and record the outcome here.
5. If a new paradigm name is warranted, maintain a separate terminology check, include its previous uses and compare alternatives; do not retroactively redefine "object" or "entity" to make a novelty argument.

This file deliberately stores **what we know, what was already investigated, and what is still open**. Repetition without new evidence is not progress.
