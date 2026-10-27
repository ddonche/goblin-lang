# Goblin vs Mesa

The same three simulations, written in Goblin (`sims/`) and in Python with
Mesa 3.5.1 (this folder). Each Mesa model follows the rules of its Goblin
sim, so the two can be compared on code size, speed and how much has to
be built by hand. The random numbers differ, so the exact outputs differ.

| Sim | Goblin | Mesa |
|---|---|---|
| epidemic: 4 towns, a `Sick` overlay that spreads between people of a town | `sims/epidemic/plague.gbln` | `epidemic.py` |
| predator-prey: 4 meadows, rabbits and foxes with hunger, breeding, hunting | `sims/predator_prey/meadows.gbln` | `predator_prey.py` |
| feudal: 3 kingdoms, nobles holding estates and vassals, inheritance, sales, seizures | `sims/feudal/kingdom.gbln` | `feudal.py` |

Run `bench/vs_mesa/run.sh [goblin] [python-with-mesa]`.

## Code size

These are lines of code, not counting blank lines or comments (Goblin `///`,
Python docstrings and `#`).

| Sim | Goblin | Goblin without `xx`/`end` lines | Mesa |
|---|---|---|---|
| epidemic | 71 | 53 | 58 |
| predator-prey | 115 | 86 | 84 |
| feudal | 312 | 234 | 241 |

The two come out about even.

- Goblin spends 15–20 lines per sim on regions: one actor per town, meadow or kingdom, so `tick(regions)` runs them on all cores. Mesa has no such step.
- Mesa spends lines on what DES gives Goblin:
  - Ownership: `Held` keeps each holder's list in feudal.py by hand, and a dead noble's holdings move up by hand.
  - Timed overlays: a sickness that lasts N days and then ends.
  - Decisions with `where`/`by`/margin: a hand-written loop over candidates.
  - Erasing an agent in the middle of a step.

## Speed

Times are wall-clock on one machine with 4 cores; repeated runs vary by about 10%. Mesa's time does not include importing Mesa, which takes about 1.2 s.

| Sim | Goblin 1 thread | Goblin 4 threads | Mesa (1 thread) |
|---|---|---|---|
| epidemic 4×100 people, 120 days | 1.19 s | 0.74 s | 0.11 s |
| epidemic 4×1000 people, 60 days | 20.7 s | 8.8 s | 8.0 s |
| predator-prey, 360 days | 21.2 s | 7.3 s | 4.8 s |
| feudal, 200 years | 4.4 s | 3.0 s | 0.41 s |

On one core, Mesa is 2.5–11× faster. Goblin on 4 threads draws level only on the large epidemic. Before #121–#124:
- epidemic 4×1000 took 141 s on one thread;
- predator-prey took 31 s;
- feudal took 6.6 s.

Where Goblin's time still goes, from profiling:

- **Field reads.** An object's fields are a map keyed by name, and objects are found by string ids. Reading a field in a formula costs 350–700 ns; Python reads an attribute in about 70 ns.
- **Conditions that call actions.** A filter like `strength(self)` or `kingdom(target)` runs on the full VM, at 12–34k instructions per call.
- **Copies.** `:objects` returns copies of the objects it finds, but that is only about 1.5% of the feudal run.
