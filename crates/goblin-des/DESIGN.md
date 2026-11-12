# Dynamic Entity System (DES): Runtime Design and Semantics

**Status:** implementation-grounded design specification (VM, not a proposal for a new subsystem)  
**Baseline:** `jareth` at `e05830db316987656483e578481cba89a5c3ed8f` (2026-11-12)  
**Primary authority:** VM implementation and conformance tests. Update this document when behavior changes.

## 1. Purpose and boundaries

DES is Goblin's runtime programming model for **dynamic entities**: individually identifiable things with state and optionally actions, decisions, overlays, relations, scheduled behavior, and transitions. Entities can change *what kind of thing they are*, not just their values. DES handles life-cycle and relationship effects of those changes as part of execution.

An entity is **not** an ECS-style empty ID with components processed exclusively by external systems. It is also **not** required to have a human-like mind or to represent a living organism: a polity, bacterium, wolf, spoon, disaster, or aircraft can all be modeled. Algorithmic decision evaluation is available when the class defines it; it is not mandatory.

**Key distinction:** the source class and destination class of a DES `mutate` need **no inheritance, shared parent, subtype, or domain relationship**. A Wolf can become a Spoon or a bacterium an aircraft if those classes are declared and a transition is reached. The VM checks that the destination class exists; it does not ask whether the conversion is biologically plausible or whether the classes share a taxonomy. This is a language/runtime semantic rule, not an analogy to changing breeds within a predefined hierarchy.

This document distinguishes **implemented VM semantics** from **research claims**. It does not assert that a mechanism is unprecedented simply because its syntax or integration is different.

## 2. Essential vocabulary

| Term | DES meaning |
| --- | --- |
| Dynamic entity | A class object participating in DES; its classification and state can change under runtime transitions. |
| Class | The current declaration governing an entity's fields, actions, decisions, periodic rules, hooks, and transitions. |
| ID | An opaque identifier, not an essence or metaphysical continuity model. Object fields `id` and `uuid` expose the same read-only identity; the VM stores an object UUID key. `goblin-des` additionally uses a slot/generation `EntityHandle` for its indexes. |
| Decision | A rule for filtering and scoring candidate targets; a judge determines what action to perform using the results. |
| Overlay | A hosted, independently instantiated influence with intensity, duration, fields, hooks, and potentially propagation and transitions. |
| Link | A named relation/score between entities, with declarations, formulas, and optional offsets. |
| Owner | A containment/ownership relationship among entities. **Ownership is not location.** |
| Region | An actor-owned execution partition. Regions exchange effects through tick coordination; a region is not an entity class. |
| Transition | A runtime operation that changes entity state, classification, existence, or relationships, with specified successor/retirement semantics. |
| Event | A narrative/log entry. `:event` records information without itself changing modeled state. |

The full field state is stored in the VM's `object_store`, not duplicated into each `goblin_des::entity::Entity`. The latter holds indexing/handle, classification, traits and sparse pending-write metadata.

## 3. Core invariants

1. **Identity and class are separate.** The same entity ID can designate different classes at successive instants; `mutate` preserves that ID.
2. **The class may be unrelated.** `mutate into: TargetClass` has no requirement for class ancestry, interfaces, aspects, or shared fields. A target class must exist.
3. **A class change changes the class's behavior.** The VM replaces the stored `ObjectVal` with one carrying the destination class, its projected/defaulted fields, and its associated class-defined rules. It does not merely toggle a label or add a component.
4. **Field projection is defined.** For a mutation to another class, fields shared by name retain stored values; other destination fields take declared defaults; `carries:` overrides are then applied. Fields not in the destination class's resulting layout are not automatically retained. A spawn instead starts with destination defaults plus `child:` overrides.
5. **Identity is not always inherited.** Splits and merges create new IDs; mutation keeps the original; fracture and spawn keep the origin and create new IDs; absorb preserves the absorber; subjugate preserves both. Erased and retired IDs cease to resolve as live entities.
6. **Relationships are runtime state.** Ownership, overlays, link offsets/formulas, and per-object decisions have explicit handling during transitions; their continuation is not inferred from shared class names.
7. **A decision is algorithmic.** It filters/scores candidates and its judge executes a chosen action. An entity can exist without a decision; a spoon need not think.
8. **A model has a clock.** Ticks run overlay work, periodic rules, decisions and transitions in defined order; named phases can subdivide this work.
9. **Regions and ownership are distinct.** Changing `owner` does not move an entity into another actor region. `:move!` changes its execution region explicitly.
10. **No inheritance is required for composition.** Aspects contribute fields; interfaces group/share behavior. A class `uses` them without establishing a subtype relation.

These are assertions about this implementation, not claims about what other programming systems can never do.

## 4. Class mutation is unrestricted by category

A minimal illustration of the semantics (the accompanying regression test is the executable authority):

```goblin
<>Wolf |
    name: ..
    fur: 1
    transition mutate where @fur > 0
        into: Spoon
    xx
xx

<>Spoon |
    name: ..
    material: "silver"
xx

w <> Wolf | name: "Lupin"
before | w >> id
:tick()
:say(:is_type(w, "Spoon"))
:say((w >> id) == before)
:say(w >> material)
```

The expected result is a live Spoon with the **same ID**, retaining the shared `name` and receiving `material` from the destination class. This is not a Wolf subclass, Wolf role, or Wolf component configuration. The old class's nonshared `fur` field is not part of the Spoon's projected declared layout.

The implementation in `transition.rs::fire` invokes `copy_fields` and `recast`; `copy_fields` matches destination fields by name and computes defaults; `recast` builds the new `ObjectVal` using the existing ID. See `tests/conformance/core/des_transition_mutate_spawn.gbln` and `des_transition_successor_identity.gbln`.

## 5. Object transitions

These eight operations are implemented by the VM (`crates/goblin-vm/src/transition.rs`).

| Transition | Object IDs after the operation | Main effects |
| --- | --- | --- |
| `spawn` | Source survives; child gets a new ID | Child starts from target class defaults and `child:` values. |
| `erase` | Source retires | `on erase` runs; contents and references are handled by store/world rules. |
| `mutate` | Same ID | Reclassifies in place; shared fields/defaults/overrides; overlays stay on the entity. |
| `split` | Original retires; two new IDs | Two successors; the first takes owned contents; overlays/links follow explicit continuity rules. |
| `fracture` | Original survives; fragment gets new ID | Fragment derives fields; parent may receive `carries:`; overlay/link rules apply. |
| `merge` | Both sources retire; successor gets a new ID | Successor obtains contents and selected overlays/links; fields start from initiating parent plus overrides. |
| `absorb` | Absorber keeps ID; target retires | Target contents move to absorber; other state follows specified policies. |
| `subjugate` | Both IDs survive | Target's `owner` becomes dominant entity; ownership cycles are prohibited. |

A successor's creation, field overrides, overlay/link handling, retirement and `on <transition>` hooks are ordered in the VM. A removed object can still be passed **read-only** to its hook. On each normal transition pass an object fires at most one transition; unary cases run ahead of binary cases. Immediate erase checks can also happen after actions that change an object.

**Continuity policies:** `overlays:` can select relevant split/transfer/drop behavior; `links:` specifies inheritance/reset where the transition supports it. Do not claim that every relationship automatically persists under every operation. `owner` is tracked independently and retiring entities rehome contents according to the relevant operation.

**Across regions:** a remote `merge`/`absorb` becomes a claim resolved at a world tick boundary in stable region/claim order; the remote target is retired and the effect applied on the following tick. Remote `subjugate` writes the ownership relationship across regions. Region semantics are not equivalent to instantaneous unrestricted shared memory.

**Overlays are also transitional**, but their permitted operations and results differ. Overlay instances can mutate, split, fracture, merge, absorb, spawn and erase; overlay **subjugate is invalid**. Some overlay transformations create new overlay *kinds* and record parent kind(s), which is different from object-class mutation.

## 6. Decisions and active behavior

A class can define multiple named `decision` blocks. Each identifies candidate types (`targets:`), optional filters (`where`), and scoring logic (`score`). A `judge` or `judge_all` maps results to actions; `one of` chooses alternatives. `order` or order-by rules determine eligible evaluation order, and `every N ticks` runs scheduled class behavior.

These are not cognition or sentience requirements. They are runtime-evaluated algorithms for entities which declare them. Ordinary class methods and actions coexist with DES decisions.

Target filtering can be narrowed by class, ownership, indexed fields, or links rather than scanning everything in the common indexed cases. Decision actions are applied to live object references; VM errors inside actions propagate rather than silently skipping a decision. Changed objects can be examined for immediate erase rules after an action.

Evidence: `crates/goblin-vm/src/tick.rs`; `tests/conformance/core/des_named_decisions.gbln`, `des_decision_where.gbln`, `des_decision_runs_class_action.gbln`, `des_erase_after_action.gbln`; `sims/rpg_combat/combat.gbln`.

## 7. Overlays: influences with their own lifecycle

An overlay has a declaration or overlay-class membership, individual copy/instance identity, a host, optional fields, intensity, duration, aging and lifecycle hooks. Overlays may:

- Modify reads through a reversible film of contributions on top of stored host values;
- Perform periodic or per-tick lasting writes, distinct from reversible modifications;
- Spread according to host eligibility and probability using the model's random stream;
- Replace, stack, coexist, or conflict according to duplicate/conflict rules;
- Transform themselves and potentially generate new overlay kinds with recorded parentage;
- Run `on start`, `on end`, and transition hooks.

Overlay instances are **not** simply additional fields or boolean flags. The VM tracks their own instances, lifecycle and transition rules. They need not themselves make decisions: `every`, `spread` and transitions can express influence without personifying the overlay.

Evidence: `crates/goblin-vm/src/tick.rs` (`overlay_tick`, `spread_pass`, film maintenance), `transition.rs` (overlay transitions), `session.rs` (definitions/instances); `tests/conformance/core/des_overlay_class.gbln`, `des_overlay_end_takes_back_own_change.gbln`, `des_transition_overlay_kinds.gbln`; `sims/epidemic/plague.gbln`, `sims/capstone/overlays.gbln`.

## 8. Relations, indexes and space

- **Class membership:** indexed and updated as entities are made, reclassified or retired.
- **Ownership:** a tree-like containment relationship checked for loops; erasure/retirement passes contents upward or to a specified successor. A holder is an entity, not a region.
- **Links:** declared and/or per-object relationships between entities, with computed scores and temporary offsets. Relevant data can be copied to successors when requested.
- **Grids:** spatial storage with layers, void/occupied cells, neighborhood rules and tick snapshots. Grid position is not an entity's class.
- **Indexes:** `goblin-des/src/index.rs` has class, ownership, overlays, links, grid and transition-candidate views into the canonical store; the VM also maintains its own object-store indexes. Avoid assuming the small Rust crate contains the whole language-level DES behavior.

A dynamically reclassified entity is indexed under the *new* class after `recast`; `:objects("Class")` and `:is_type` interrogate current classification. Relations are distinct from class membership.

## 9. Time, phases, actors and determinism

`:tick()` coordinates a default tick. Within the VM, overlay work includes link-offset decay, spreading, intensity/duration advancement, overlay periodic blocks and transitions, expiration/hooks, then class periodic blocks, decisions and object transitions. Grid snapshots/pending writes are committed at defined boundaries.

Named `phase: NAME` blocks split a tick into ordered phases while advancing the tick clock **once**; phase work can run region-wide with a synchronization boundary between phases. Regions are actor-owned partitions: they keep local entities, receive remote views/messages, and pass state and effects through a world coordinator. `:move!` migrates an entity **with** its overlays, links and decisions to the target region, distinct from changing its `owner`.

With a fixed seed, the checked simulations compare repeated runs and worker-count configurations, including `sims/town_des/check.sh`, `sims/predator_prey/check.sh` and `sims/rpg_combat/check.sh`. Claim **tested deterministic runs in these configurations**, not an unconditional guarantee for all programs with external IO or nondeterministic host behavior.

A narrative `:event` writes history without directly changing entities or decisions. This keeps narration separate from simulation state.

Evidence: `crates/goblin-vm/src/tick.rs`, `world.rs`, `swarm.rs`, `events.rs`, `grid.rs`; `tests/conformance/core/des_tick_phases.gbln`, `tests/conformance/concurrency/world_tick_transition_identities.gbln`; simulation `check.sh` files.

## 10. Architectural map: read these files before altering DES

| Responsibility | Source of truth |
| --- | --- |
| Surface declarations and transition AST | `crates/goblin-ast/src/lib.rs`, parser |
| Entity field state, live references and class indexes | `crates/goblin-vm/src/value.rs`, `store.rs`, `session.rs` |
| Transition execution and successor policies | `crates/goblin-vm/src/transition.rs` |
| Decision/overlay evaluation, tick order, phase behavior | `crates/goblin-vm/src/tick.rs` |
| Ownership across regions, migration and cross-region claims | `crates/goblin-vm/src/world.rs` |
| Worker/actor program state and execution isolation | `crates/goblin-vm/src/actor.rs`, `swarm.rs` |
| Index and pending metadata (support crate) | `crates/goblin-des/src/{entity,store,index,tick}.rs` |
| Executable examples and evidence | `tests/conformance/core/des_*.gbln`, `tests/conformance/concurrency/world_*.gbln`, `sims/*` |

**Important implementation boundary:** the Rust `goblin-des` crate is not the entire DES runtime. For example, its `Entity` record contains only lightweight metadata and a sparse pending buffer; the VM's `ObjectVal` stores committed class/field data and the VM supplies the behavior. Reading only the `goblin-des` crate gives an incorrect design picture.

## 11. Scope and verification status

**Verified by source reading:** unrelated-class `mutate`, identity rules for all eight object transitions, overlay-kind transitions, fields/defaults projection, decision execution, actor-region coordination, distinct ownership and location, and implementation boundaries listed above.

**Evidence in repository:** conformance sources and checked expected outputs exist. This documentation PR does **not** imply that the author ran the full suite locally; use CI and the repository's test harness to confirm it against the proposed branch.

**Not asserted:** that all transitions are globally atomic under every error condition; that every external program is deterministic; that Goblin's mechanisms are without prior art; or that all these mechanisms establish a formally recognized new programming paradigm.

## 12. Relationship to the paradigm question

Call the implemented subsystem **Dynamic Entity System (DES)** until a comparison establishes a more precise term. A candidate paradigm must be specified by **program semantics and examples**, not the English meaning of *object*, *entity*, or a new label.

The falsifiable kernel to investigate is: *programmer-defined entities with identity independent of unrestricted class reclassification, executable decisions, first-class evolving hosted overlays, and structural transitions whose lifecycle and relationship effects are defined by the runtime*. This is what should be compared with competing models **as implemented**, one semantic claim at a time.

See [PARADIGM_RESEARCH.md](PARADIGM_RESEARCH.md) for the reusable research baseline, known comparisons, and acceptance criteria for any future claim of distinction.
