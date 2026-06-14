**Vampire** is Goblin's bytecode virtual machine: the runtime that compiles Goblin programs into instructions and executes them.

Most Goblin programs do not need to know how Vampire works internally. This page is both a practical guide to the runtime and an architectural reference for readers who want to understand what happens beneath Goblin code.

---

## Overview

Vampire turns Goblin source code into bytecode before running it. This separates understanding the program from executing it and gives Goblin a dedicated runtime for memory management, functions, objects, DES, grids, workers, and future optimization.

The source code calls this engine `goblin-vm`. **Vampire** is its Goblin-facing name.

```text
Goblin source
    -> lexer
    -> parser
    -> abstract syntax tree
    -> Vampire compiler
    -> bytecode
    -> Vampire execution
```

The command-line interface can currently select Vampire explicitly:

```powershell
goblin --vm
goblin run --vm program.gbln
```

The VM REPL identifies itself when it starts:

```text
[VM mode - engine: goblin-vm]
```

---

## Why Vampire Exists

Goblin began with a tree-walking interpreter. That interpreter established the language's behavior, but it evaluates the syntax tree directly.

Vampire gives Goblin a purpose-built execution engine with:

- compiled bytecode;
- indexed local and global slots;
- an explicit operand stack and call stack;
- closures and captured values;
- specialized instructions for common operations;
- an arena-backed memory model;
- garbage collection and memory inspection;
- isolated worker sessions;
- native runtime state for DES and grids.

The current parity effort is translating the interpreter's established behavior into Vampire. Once parity is complete and the interpreter is retired, Vampire is simply Goblin's runtime rather than an alternative language mode.

---

## From Source to Execution

Running Goblin through Vampire has four major stages.

### 1. Lexing

The lexer reads source text and produces tokens: names, numbers, strings, operators, delimiters, keywords, and other meaningful pieces of Goblin syntax.

### 2. Parsing

The parser turns those tokens into an abstract syntax tree, or AST. The AST describes the program in terms such as bindings, calls, expressions, actions, classes, overlays, and loops.

### 3. Compilation

The Vampire compiler lowers the AST into a `FunctionObject` containing:

- bytecode instructions;
- constants;
- local-slot and parameter counts;
- captured-value descriptions;
- source line numbers;
- local names used by interpolation and diagnostics.

A compiled module also carries class declarations, enum declarations, and global names that must be registered before execution.

### 4. Execution

Vampire executes the bytecode against a `Session`. The VM maintains an operand stack of tethers and a call stack of frames. Each frame tracks its function, instruction position, local slots, captured values, and stack base.

---

## Bytecode

Bytecode is Vampire's compact instruction language. Each instruction tells the runtime to perform one operation.

Major instruction families include:

| Family | Examples |
| ------ | -------- |
| Values | `LoadConst`, `LoadNil`, `LoadTrue`, `LoadFalse` |
| Variables | `LoadLocal`, `StoreLocal`, `LoadGlobal`, `StoreGlobal` |
| Captures | `LoadUpvalue`, `StoreUpvalue`, `MakeClosure` |
| Arithmetic | `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg` |
| Comparison | `Eq`, `Ne`, `Lt`, `Le`, `Gt`, `Ge` |
| Control flow | `Jump`, `JumpIfFalse`, `JumpIfTrue` |
| Collections | `MakeArray`, `MakeMap`, `GetIndex`, `SetIndex` |
| Objects | `GetMember`, `SetField`, `ClassInstantiate`, `CallMethod` |
| Calls | `Call`, `CallBuiltin`, `Return` |
| Runtime systems | imports, DES declarations, grid expressions, error handling |

Locals live in indexed slots, while expressions use an operand stack. Vampire is therefore a slot-and-stack hybrid rather than a pure stack machine.

---

## Quickening

Vampire can replace generic arithmetic bytecode with instructions specialized for known operand types.

For example, a generic `Add` may become:

- `AddInt` for two integers;
- `AddFloat` for two floats;
- `Concat` for two strings.

The current quickening pass specializes operations when their input types can be seen directly in the function's constants. Each worker owns its bytecode copy, so that copy can be specialized without changing another worker's execution.

---

## Tethers and Stashes

Vampire's memory model uses Goblin-native vocabulary.

A **stash** is an arena slot containing a value and its memory metadata.

A **tether** is a runtime reference that points to a stash.

```text
name -> local or global slot -> tether -> stash -> value
```

A stash contains:

- the value;
- a tether count;
- a generation number.

Variables do not contain their values directly. Their runtime slots contain tethers to the stashes holding those values.

---

## Retethering

When a variable is retethered, its slot begins pointing to a new stash:

```goblin
name | "Vampire"
name |= "Dracula"
```

The old value may then have no live tether pointing to it. Vampire calls that stash **abandoned**. It is unreachable garbage waiting to be reclaimed.

Abandoned memory is not automatically a memory leak. It becomes a leak only if unreachable stashes are never reclaimed. Vampire's garbage collector exists to reclaim them.

---

## Addresses and Generations

Every tether carries a logical arena address:

```text
{ slot, generation }
```

The slot identifies a position in the arena. The generation identifies the allocation that currently belongs to that slot.

When an arena slot is reused, the new stash receives a new generation. A tether carrying an old generation is stale and cannot silently read the new value. Vampire reports a stale-address error instead.

This protects the runtime from accidentally treating a reused memory slot as the value that previously occupied it.

---

## Garbage Collection

Vampire provides three garbage-collection modes:

| Mode | Behavior |
| ---- | -------- |
| `"auto"` | Runs collection automatically after an allocation watermark. This is the default. |
| `"manual"` | Runs collection only when `gc()` is called. |
| `"off"` | Disables collection for the session. |

The current automatic watermark is 10,000 allocations.

```goblin
gc_mode("manual")
gc()
gc_mode("auto")
```

Calling `gc()` performs a mark-and-sweep collection. Vampire begins with live roots on the operand stack, in call-frame locals, and in captured-value cells. Arena stashes not reachable from those roots are removed.

Switching modes takes effect immediately. Changing from `"off"` to `"manual"` does not itself perform a collection.

See [[Garbage Collection]] for the complete user guide.

---

## Inspecting Memory

Vampire exposes memory tools because memory behavior is part of Goblin's observable runtime model.

| Call | Returns | Meaning |
| ---- | ------- | ------- |
| `stash_count()` | map | Arena totals: `{ total, live, abandoned }`. |
| `tether_count(value)` | integer | Runtime tether information for a value. |
| `mem_id(value)` | map | The value's `{ slot, generation }` arena address. |
| `mem_addr(value)` | string | The current process pointer for the value's stash. |
| `mem_total()` | integer | Resident memory of the whole Goblin process in bytes. |
| `mem_human()` | string | The same process memory formatted for people, such as `"6.4 MB"`. |

Example:

```goblin
letters | "Goblin".chars

:say(stash_count())
:say(mem_id(letters))
:say(mem_addr(letters))
:say(mem_human())
```

`mem_total` and `mem_human` report process memory, not arena slot counts. `stash_count` reports aggregate arena state.

`mem_addr` is diagnostic only. Its result may change after retethering and between program runs. Do not persist it or use it as application identity.

---

## Calls and Frames

Vampire uses its own call stack rather than Rust recursion for ordinary Goblin function calls.

Each call frame contains:

- the compiled function;
- an instruction pointer;
- local tether slots;
- captured upvalues;
- the operand-stack position where the call began;
- optional object-method context.

`Call` creates a function call. `Return` removes the frame and leaves the returned tether for the caller.

The REPL keeps one session alive across snippets, so global bindings and runtime state survive from one prompt to the next.

---

## Closures and Captured Values

Compiled functions describe which local or outer captured slots a closure needs. `MakeClosure` creates the closure and fills its captured-value cells.

The current implementation uses snapshot semantics: a closure captures the tether present when the closure is created. Fully shared mutable upvalues, where later outer and inner mutations always observe one shared slot, are identified in the source as a future improvement.

---

## Builtins and Method Calls

User-facing builtin names are resolved by the compiler to a `BuiltinId`:

```text
source name -> builtin_by_name -> BuiltinId -> CallBuiltin -> call_builtin
```

This is why free-call and method forms can reach the same behavior:

```goblin
chars("Goblin")
"Goblin".chars
```

The compiler lowers both user-facing forms into the runtime operation appropriate for that builtin. Object methods use their own compiled method dispatch, while ordinary member-style builtin calls pass the receiver as the first argument.

---

## Session State

A Vampire `Session` owns more than the value arena. It also holds the long-lived state required by a running Goblin program, including:

- globals and their names;
- random-number state;
- registered tokens;
- classes, enums, and compiled methods;
- imported-file tracking;
- named actions;
- Box values;
- response status, headers, and cookies;
- object and overlay state;
- links, decisions, units, and DES indexes;
- grid worlds.

Each worker receives its own session and therefore its own heap and runtime state.

---

## DES Runtime

Vampire includes Goblin's Dynamic Entity System state directly in the session. Classes, objects, overlays, links, decisions, ownership, units, transitions, and tick state can therefore participate in one runtime lifecycle.

The tick runner coordinates grid snapshots with DES processing. The current tick implementation includes passes for:

- temporary link-offset decay;
- overlay spreading;
- strength, age, and duration decay;
- conflict suppression and boosts;
- spawning;
- overlay transitions;
- dead, expired, and orphaned overlay removal;
- decisions;
- object transitions.

See [[DES]] and [[Ticks]] for the language-facing model.

---

## Grid Runtime

The session also owns a `GridStore` containing named grid worlds.

Grid worlds support:

- cells and layers;
- occupied, unoccupied, and void states;
- configurable neighbor modes;
- world defaults;
- optional tile and region hierarchies;
- tick snapshots and commits;
- queries for positions and stored values.

Grid values and DES objects can participate in the same running session without requiring a separate external engine.

See [[Grid]] for the user-facing grid system.

---

## Workers and Swarms

Vampire's worker model isolates runtime state. Each worker owns:

- one VM;
- one session;
- one value arena;
- its own bytecode copy;
- message channels for communication.

Workers do not share mutable Goblin memory. Values sent between workers are converted into owned transfer values and reconstructed in the receiving session. This gives cross-worker messaging deep-copy behavior.

The current worker implementation runs synchronously on the calling operating-system thread. True parallel workers are planned after internal shared values move from `Rc` to a thread-safe representation.

Functions, closures, objects, references, grid references, classes, and other complex values cannot currently be transferred between workers.

See [[Workers]] and [[Swarms]] for the public execution model.

---

## Errors and Safety

Vampire has dedicated errors for runtime failures such as:

- stale or empty stash addresses;
- cross-worker access or mutation;
- type mismatches;
- division by zero;
- collection bounds and missing keys;
- wrong arity;
- calling a non-callable value;
- stack overflow;
- undefined variables;
- compilation failures;
- unavailable features.

Compiled functions retain source line numbers for bytecode instructions. The CLI converts VM failures into Goblin diagnostics and attaches a source location when one is available.

---

## Debugging Tools

The VM crate includes tools for:

- disassembling a compiled function;
- displaying constants and bytecode;
- recursively inspecting nested functions;
- dumping live stashes;
- showing generation numbers and tether counts;
- inspecting globals and tethers;
- dumping operand and call stacks.

These tools write to standard error and do not change program execution.

---

## Vampire and the Original Interpreter

The interpreter remains the source of truth while parity work is in progress. That does not make its architecture part of the permanent Goblin language.

The intended end state is:

```text
Goblin language behavior
        =
Vampire behavior
```

Parity must be demonstrated through tests, not inferred merely because both engines contain similarly named code. Until the migration is finished, a difference between the engines is an implementation issue to investigate rather than a language feature to document as two valid behaviors.

---

## Current Status

**Status date: June 12, 2026.**

Vampire is actively being brought to behavioral parity with Goblin's original interpreter. It already contains the primary compiler, bytecode engine, builtin dispatch, memory arena, garbage collector, classes and enums, DES tick runtime, grids, imports, actions, and worker architecture.

Some architectural pieces are intentionally still evolving. In particular:

- parity coverage is incomplete because many language behaviors do not yet have interpreter-versus-VM tests;
- closure captures currently use snapshot semantics;
- workers are isolated but execute synchronously rather than in parallel;
- some complex values cannot cross worker boundaries;
- `--vm` still explicitly selects the VM during the migration period.

This section should be updated as the parity suite grows and Vampire becomes the sole Goblin runtime.

---

## Design Philosophy

Vampire follows several ideas that shape the rest of Goblin:

**Language behavior comes first.** The runtime exists to execute Goblin's semantics, not to redefine them for implementation convenience.

**Memory should be safe and inspectable.** Tethers, generations, garbage collection, and memory builtins make runtime behavior visible without allowing stale addresses to silently access reused storage.

**Isolation beats accidental sharing.** Workers own separate sessions and communicate through explicit value transfer.

**Advanced machinery should not burden ordinary code.** Most programs can use automatic garbage collection and ignore bytecode, stashes, and frames entirely. The deeper tools remain available when simulations, servers, debugging, or performance work need them.

**Goblin should describe itself in Goblin terms.** Tethers and stashes are not decorative names pasted over conventional concepts. They form the vocabulary used consistently by the language, runtime, diagnostics, and documentation.

---

## Source References

- Source-to-execution pipeline: `crates/goblin-vm/src/exec.rs:45`
- Bytecode compiler and builtin resolution: `crates/goblin-vm/src/compiler.rs:1`, `crates/goblin-vm/src/compiler.rs:2017`
- Opcode model and quickening design: `crates/goblin-vm/src/opcode.rs:3`
- VM stacks and execution entry points: `crates/goblin-vm/src/vm.rs:70`, `crates/goblin-vm/src/vm.rs:88`
- Runtime quickening: `crates/goblin-vm/src/vm.rs:1368`
- Tethers, addresses, stashes, and functions: `crates/goblin-vm/src/value.rs:5`, `crates/goblin-vm/src/value.rs:247`, `crates/goblin-vm/src/value.rs:268`
- Session state and GC configuration: `crates/goblin-vm/src/session.rs:75`, `crates/goblin-vm/src/session.rs:86`, `crates/goblin-vm/src/session.rs:170`
- Allocation, generations, and tether counts: `crates/goblin-vm/src/session.rs:228`, `crates/goblin-vm/src/session.rs:258`, `crates/goblin-vm/src/session.rs:312`
- Mark-and-sweep roots: `crates/goblin-vm/src/vm.rs:1433`
- Memory builtins: `crates/goblin-vm/src/builtins.rs:38`, `crates/goblin-vm/src/vm.rs:882`
- Worker isolation and transfer values: `crates/goblin-vm/src/worker.rs:1`, `crates/goblin-vm/src/worker.rs:19`, `crates/goblin-vm/src/worker.rs:154`
- Grid runtime: `crates/goblin-vm/src/grid.rs:264`, `crates/goblin-vm/src/grid.rs:542`
- DES tick runtime: `crates/goblin-vm/src/tick.rs:1`, `crates/goblin-vm/src/tick.rs:79`
- VM errors: `crates/goblin-vm/src/error.rs:3`
- Debug and disassembly tools: `crates/goblin-vm/src/debug.rs:1`
- CLI VM selection and REPL marker: `crates/goblin-cli/src/main.rs:209`, `crates/goblin-cli/src/main.rs:1131`

---

::: nav
related [[Goblin]]
related [[Garbage Collection]]
related [[Memory]]
related [[Tethers]]
related [[Stashes]]
related [[Bytecode]]
related [[Opcodes]]
related [[Workers]]
related [[Swarms]]
related [[DES]]
related [[Grid]]
:::
