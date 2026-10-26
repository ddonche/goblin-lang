# A small civilization on DES: what got in the way

`villages.gbln` has four villages, each a region, run a month at a time
with `tick(villages)`. People eat from their village's store, and each
month do one job (`one of build, farm, chop`): build a hut when the
village is short of room, farm when it has less than a year of food, else
cut wood. Children are born when there is food and room. People die after
three hungry months or of old age, and the village counts its graves
(`on erase:`). Winter (an overlay, three months a year) cuts the harvest
and is taken back when it ends. A crowded village sends its homeless to
the next one (`:move!`), and a village with food to spare sends some to a
hungry neighbour.

Over 50 years the villages grow, starve, and recover; one dies out in
year 45. 50 years take 1.7 s on 4 threads (3.4 s on 1), about 3 ms a
month for 150 to 270 people.

`check.sh`: 120 months match `expected.txt`, two runs match, and 1 thread
matches 4.

## What worked well

- `one of build, farm, chop` with `where` reads like a job list: the
  first job whose condition holds is the one done this month.
- `modifies:` with `lasts 3 ticks` made winter four lines, with nothing
  to undo by hand.
- A region's message handler can change its own objects, so food sent as
  a message lands in the village's store.

## 1. A field that holds an object holds a copy, and writes through it vanish

The natural way to give a person a home is a field:

```goblin
<>Person |
    ~home: nil
    every 1 tick
        self >> home >> food |= self >> home >> food - 1
    end
xx
p <> Person | home: v
```

`p >> home` is a copy of `v` made when `p` was. The line runs without an
error and `v >> food` never changes. So every job in the sim is a
decision against `Village`, with `where [ target >> village == self >>
village ]`, only to reach the person's own village. Homes, owners,
employers, spouses and parents all hit this.

**Change:** a field set to a DES object keeps a reference to it, the way
a decision's `target` does: reads and writes go to the object itself, and
the field reads nil once the object is erased. No new name. At the least,
a write through such a copy should be an error.

**Fixed in the runtime:** a field given an object now holds the object
itself (its id): writes through `p >> home` reach the village, and the
field reads nil once the village is erased. The sim still finds its
village by number; it moves to `home` with the object index change.

## 2. An action in a `one of` group can't say it didn't act

The first version had `leave` first in the group: a homeless person
leaves with a chance of 1 in 6. `move_on` returned `false` when the chance
said no, but the group ends as soon as a judge arm runs an action, so the
other 5 in 6 neither left nor worked that month. The leaving moved to the
village's `every` block.

**Change (a choice for you):** an action in a `one of` group that returns
`false` lets the next decision in the group run. Today the return value
is ignored.

## 3. `by [ 1 ]` when the score doesn't matter

Every decision here has one possible target, so the score means nothing,
but `by [ ... ]` is required. Five decisions end in `by [ 1 ]`.

**Change:** let `by [ ... ]` be left out; every target then scores the
same (1), and `margin`/`order by` work as now.

## 4. Sending something to another region takes a message and a search

The next village's object is a read-only copy in this region (as of last
month). Giving it food takes a `:post` to its region, a handler that
recognizes the message, and a `:objects(...)` search in the handler to
find the village again:

```goblin
:post(all_villages[to], {"food": give})
...
if :has(msg, "food")
    repeat :objects(it >> kind == "village" and it >> village == st{"n"}) as v
        v >> food |= v >> food + msg{"food"}
    xx
xx
```

**Change (a choice for you):** let a write to another region's object
from inside an action go to that region and be applied at the start of
the next tick, the way `:move!` hands an object over. Writes would stay
in order, and a write to an erased object would be dropped. This is a
bigger change than the others; the message works today.

## 5–6. Repeats

- Nothing can ask an object its class, so both classes carry `~kind` for
  counting (predator-prey 1).
- A region must be told the list of regions before `:move!` and `:post`
  work (predator-prey 4).

## Not a language problem: tuning

The first run starved every village in the first winter: farming stopped
at six months of food, and winter left the farmers with nothing. Farming
to a year of food and a milder winter fixed it.
