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
| epidemic | 62 | 46 | 58 |
| predator-prey | 102 | 75 | 84 |
| feudal | 301 | 226 | 241 |

Without `xx`/`end` lines Goblin is 6–21% shorter than Mesa; counting them, it is 7–25% longer.

- Regions cost Goblin a few lines per sim: one `:actor(...)` per town, meadow or kingdom, so `tick(regions)` runs them on all cores. Moving something to another region is setting its `owner`; there is no handler and no list of regions to pass around (before #129/#130 this took 15–20 lines). Mesa has no such step.
- Mesa spends lines on what DES gives Goblin:
  - Ownership: `Held` keeps each holder's list in feudal.py by hand, and a dead noble's holdings move up by hand.
  - Timed overlays: a sickness that lasts N days and then ends.
  - Decisions with `where`/`by`/margin: a hand-written loop over candidates.
  - Erasing an agent in the middle of a step.

## Speed

Times are wall-clock on one machine with 4 cores; repeated runs vary by about 10%. Mesa's time does not include importing Mesa, which takes about 1.2 s.

| Sim | Goblin 1 thread | Goblin 4 threads | Mesa (1 thread) |
|---|---|---|---|
| epidemic 4×100 people, 120 days | 0.80 s | 0.39 s | 0.09 s |
| epidemic 4×1000 people, 60 days | 15.5 s | 6.2 s | 5.8 s |
| predator-prey, 360 days | 14.8 s | 5.1 s | 3.7 s |
| feudal, 200 years | 3.7 s | 2.2 s | 0.36 s |

On one core, Mesa is 2.7–10× faster. Goblin on 4 threads draws level only on the large epidemic. This table was measured after the sims were rewritten on `owner` (#130 and the PR after it); this run's machine was faster than the earlier one (Mesa took 4.8 s on predator-prey there, 3.7 s here). Before #121–#124:
- epidemic 4×1000 took 141 s on one thread;
- predator-prey took 31 s;
- feudal took 6.6 s.

Where Goblin's time still goes, from profiling:

- **Field reads.** An object's fields are a map keyed by name, and objects are found by string ids. Reading a field in a formula costs 350–700 ns; Python reads an attribute in about 70 ns.
- **Conditions that call actions.** A filter like `strength(self)` or `kingdom(target)` runs on the full VM, at 12–34k instructions per call.
- **Copies.** `:objects` returns copies of the objects it finds, but that is only about 1.5% of the feudal run.
