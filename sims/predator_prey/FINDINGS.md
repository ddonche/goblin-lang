# Predator and prey on DES: what got in the way

`meadows.gbln` has rabbits that eat grass and foxes that eat rabbits, over
four meadows. Each meadow is a region (an actor), and `tick(meadows)` runs
one day in all of them at once. Animals now and then wander to the next
meadow with `:move!`. Rabbits graze with a decision against `Grass`. Foxes
hunt with a decision against `Rabbit`, only when hungry (`where`), and the
more rabbits there are, the likelier a hunt succeeds. Both have young when
well fed and starve at 0 energy.

The two populations rise and fall in turn, about every 90 days, for as long
as I ran it (1,000 days). A year takes 7.7 s on 4 threads, about 21 ms a
day for 500 to 800 animals.

`check.sh`: 60 days match `expected.txt`, two runs match, and 1 thread
matches 4.

## What worked well

- **Erase right after the action (#93) made a kill one line.** `catch`
  sets the rabbit's energy to 0. The rabbit is gone as soon as the hunt
  ends, so it can't graze or be caught again that day.
- **`where` (#89) put hunger in the decision:** `where [ self >> energy < 30
  and target >> meadow == self >> meadow ]`.
- Young are made inside `every` blocks in the regions, and they show up
  correctly.

## 1. You can't ask an object what class it is

To count rabbits and foxes I gave both classes a `kind` field that only
exists for counting:

```goblin
<>Rabbit |
    ~kind: "rabbit"
...
rabbits | :len(:objects(it >> kind == "rabbit"))
```

`:is_type(r, "Rabbit")` is false, `:vt(r)` says `object`, and `:objects`
has no way to name a class. Decisions can already say `against Rabbit`.

**Change:** make `:is_type(r, "Rabbit")` true for an object of class
`Rabbit`. Then `:objects(:is_type(it, "Rabbit"))` works with no new
builtin. The query would also need to run fast for this form.

**Fixed in the runtime:** the object store keeps each class's objects in
one place, updated as objects are made and erased. `:objects("Rabbit")`
reads that list directly (`:objects("Rabbit", pred)` filters it), and
`:is_type(r, "Rabbit")` is true for a Rabbit. The sim now uses them and
has no `kind` field; it runs in 5.6 s instead of 7.3 s.

## 2. Counting what's nearby costs a scan per action

How well a hunt goes depends on how many rabbits are in the meadow, so
`catch` counts them:

```goblin
act catch(f, r)
    here | :len(:objects(it >> kind == "rabbit" and it >> meadow == f >> meadow))
```

That is one scan of every object per hunt. Before #99 it was 97% of the
run time (12.6 s for 200 days). After #99 it is still about 40% (4.0 s).
The `hunt` decision has just looked at every rabbit and kept those in the
fox's meadow, so the number is already known.

**Change (a name for you to pick):** let a judge arm hand the number of
targets that passed `where` to the action, the way it hands `score` and
`target`. For example `>= 0: catch(target, candidates)`.

## 3. Decisions see every region's objects

A fox in meadow 0 scores the rabbits in all four meadows, because each
region also holds read-only copies of the other regions' objects. To hunt
in its own meadow the sim writes `target >> meadow == self >> meadow`,
keeps a `meadow` field on every animal in step with `:move!`, and still
pays to look at the other meadows' rabbits. The town needs the opposite:
residents shop in any district.

**Change (a name for you to pick):** a way to say a decision looks only
at the objects in its own region, for example `decision against Rabbit
here`. With it, the `meadow` field and that half of the `where` go away,
and so does the cost of the other meadows.

## 4. A region has to be told the list of regions

`:move!(a, region)` needs the other region's handle. A region's code can't
see the main program's `meadows` list, so the main program posts the list
to every meadow, and each meadow keeps it in a global (`meadow_day` exists
only for that). The town does the same.

**Change:** `tick(meadows)` could give each region the list it was run
with, so `:move!(a, meadows[to])` works inside a region. No new name.

## 5. An expression in braces that can't be filled in stays as written, silently

```goblin
say("rabbits {:len(:objects(it >> kind == \"rabbit\"))}")
```

prints the braces as written, because `it` isn't a variable where the
string is (it's the predicate's name). The rule from #95 is what makes
CSS and regex braces safe, but here the reader can't tell why nothing
happened.

**Change (a choice for you):** either keep it, or make a placeholder that
reads as an expression but names something unknown an error that says
which name, and to write `\{` for a literal brace. The second could break
strings that hold code, such as JavaScript.

## Not a language problem: tuning

The first versions died out within 60 days: the rabbits ate the grass
bare, or the foxes ate every rabbit and then starved. Finding numbers that
cycle took a sweep of 16 runs over four constants. The sweep edited copies
of the file with `sed`, because the constants are in the code.
