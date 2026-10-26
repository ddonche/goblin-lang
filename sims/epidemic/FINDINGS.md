# Epidemic on DES: what got in the way

`plague.gbln` has four towns of 100 people. Each town is a region (an
actor), and `tick(towns)` runs one day in all of them at once. One person
in town 0 starts sick. Being sick is an overlay, `Sick`: it costs 1 health
a day for 8 days (`each tick:`), and when it runs out the host is immune
(`on end:`). People die at 0 health (`transition erase`). People now and
then travel to the next town with `:move!` and take the sickness with them.

The sickness reaches every town, peaks around day 35 with about 120 sick,
and is gone by day 65. Of 400 people, 122 die and 216 end up immune.
120 days take 0.7 s on 4 threads.

`check.sh`: 60 days match `expected.txt`, two runs match, and 1 thread
matches 4.

## What worked well

- **`each tick:` (#91) and `on end:` (#82) made the sickness itself four
  lines:** `lasts 8 ticks`, `each tick: health - 1`, `on end: recover`.
- **Erase right after the action (#93):** a person who reaches 0 health is
  gone at once and can't infect anyone after dying.
- **`:overlay_strength("Sick", other)` taking an object (#94)** is how a
  person checks whether someone is already sick.
- `:move!` carries the overlay along, so travel spreads the sickness with
  no extra code.

## 1. `spreads` has no chance in it, so it can't model an infection

The overlay already has a way to spread:

```goblin
overlay Sick |
    spreads where (other >> town == self >> town) at .2 per tick
```

But it isn't random. Each tick it works out `strength * rate`, and if that
is above 0.15 (a number written into the runtime, not in the docs) every
matching object gets the overlay. Below 0.15 nobody does. So:

| rate | result |
|---|---|
| `.1` | nobody else is ever infected |
| `.16` or `.2` | whole towns are infected at once: 200 sick by day 5, 148 dead in the end |

There is no setting in between, so I wrote the spreading by hand in an
`every` block:

```goblin
every 1 tick
    if :overlay_strength("Sick", self) != nil
        repeat :objects(it >> kind == "person" and it >> town == self >> town and it >> immune == 0) as other
            if (pick 1 from 1...1000) <= 4 and :overlay_strength("Sick", other) == nil
                overlay Sick on other at 1
            xx
        xx
    xx
```

**Change (a name for you to pick):** let `spreads` take a chance per
contact, for example
`spreads where (other >> town == self >> town) chance 4 in 1000 per tick`.
Each matching object would then catch it with that chance each tick, drawn
from the seeded random numbers like `pick`. The 0.15 cutoff should also be
either documented or turned into a setting, because today a rate below it
silently does nothing.

## 2. Spreading costs every sick person a scan of everyone

The `every` block above runs a `:objects` scan for each sick person, so a
day costs (sick people × all people). With 120 sick in 400 people this is
still fast (0.7 s for 120 days), but it grows as the square of the town.
`spreads where` has the same cost inside the runtime. Proposal 3 from the
predator-prey sim (a decision or query limited to its own region) would
cut it to one town; a chance-based `spreads` (1) would at least move it
into native code.

## 3. Again: no way to ask an object its class

Counting people needs a `~kind: "person"` field, as with rabbits and
foxes (predator-prey, finding 1). Same proposal: make
`:is_type(p, "Person")` true for an object of class `Person`.

## 4. Again: a region has to be told the list of regions

`:move!` needs the next town's handle, so the main program posts the list
of towns to every town, and `town_day` exists only to store it
(predator-prey, finding 4).

## 5. Again: `{...}` with `it` inside stays as written

`say("immune {:len(:objects(it >> immune == 1))}")` prints the braces,
because `it` isn't a variable there (predator-prey, finding 5). I read the
count into a variable first.

## 6. Objects made before the first tick aren't visible yet

Patient zero first got the overlay in the main program, after the towns
were made:

```goblin
first | :objects(it >> kind == "person")[0]
```

That list is empty: objects made inside a region only reach the main
program's view after the first `tick(towns)`. So patient zero is now made
sick inside `make_town`. This is how regions work today (the main view is
refreshed each tick), but nothing says so when it happens; the program
just finds no one.

**Change (a choice for you):** either refresh the main view once when the
regions are started, or leave it and document it.
