# Epidemic on DES: what got in the way

`plague.gbln` has four towns of 100 people. Each town is a region (an
actor), and `tick(towns)` runs one day in all of them at once. One person
in town 0 starts sick. Being sick is an overlay, `Sick`: it costs 1 health
a day for 8 days (`each tick:`), and when it runs out the host is immune
(`on end:`). People die at 0 health (`transition erase`). People now and
then travel to the next town with `:move!` and take the sickness with them.

The sickness reaches every town, peaks around day 30 with about 100 sick,
and is gone by day 70. Of 400 people, 124 die and 227 end up immune.
120 days take 0.5 s on 4 threads.

`check.sh`: 60 days match `expected.txt`, two runs match, and 1 thread
matches 4.

## What worked well

- **`each tick:` (#91) and `on end:` (#82) made the sickness itself four
  lines:** `lasts 8 ticks`, `each tick: health - 1`, `on end: recover`.
- **Erase right after the action (#93):** a person who reaches 0 health is
  gone at once and can't infect anyone after dying.
- `:move!` carries the overlay along, so travel spreads the sickness with
  no extra code.

## 1. `spreads` had no chance in it (fixed in the next PR)

`spreads ... at rate` used a hidden cutoff: each tick it worked out
`strength * rate`, and above 0.15 every matching object caught the overlay
at once; below it, nobody did. `.1` infected no one, `.2` made whole towns
sick in a day, and the sim spread the sickness by hand in an `every`
block.

Now the rate is the chance: each tick, each object the rule reaches
catches it with chance `rate` times the spreader's strength, drawn from
the seeded random numbers like `pick`. The sim says it in one line:

```goblin
overlay Sick |
    spreads where target >> town == self >> town and target >> immune == 0 at .004 per tick
```

It gives the same kind of curve as the hand-written block (about 100 sick
at the peak, 124 dead) and runs 120 days in 0.5 s instead of 0.7 s.

## 2. Spreading costs every sick person a scan of everyone

`spreads where` checks its condition for each sick person against every
object, so a day costs (sick people × all people), including the other
towns' copies. With 100 sick in 400 people this is fast (0.5 s for 120
days), but it grows as the square of the town. Proposal 3 from the
predator-prey sim (limited to its own region) would cut it to one town.

## 3. Again: no way to ask an object its class

Counting people needs a `kind: "person"` field, as with rabbits and
foxes (predator-prey, finding 1). Same proposal: make
`:is_type(p, "Person")` true for an object of class `Person`.

**Fixed in the runtime:** `:objects("Person")` and `:is_type(p, "Person")`
(predator-prey, finding 1). The sim uses them and has no `kind` field.

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
