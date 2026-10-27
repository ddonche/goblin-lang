# Feudal kingdoms on DES: what got in the way

`kingdom.gbln` is three kingdoms, each a region, run a year at a time with
`tick(realms)`. Everything is held by someone, through the `owner` field:
a realm holds its king, a king his lords and estates, a lord his knights,
estates and children. Each year estates pay their holder, vassals pay a
tenth to their liege, and every noble does at most one deed (`one of`):

- **defect**: a disloyal vassal half as strong as his liege swears to
  another kingdom's king and moves there (`:move!`) with everything he
  holds;
- **sell**: a noble in debt sells his poorest estate to the richest of his
  liege's other vassals;
- **seize**: a noble twice as strong as a fellow vassal takes one of his
  estates;
- **grant**: a lord with three estates gives one to a grown child or vassal
  without land.

At death (`on erase: inherit`) the eldest child takes the title and
everything the dead noble held. With no child the runtime moves what he
held up to his liege. When a king dies without a child, his lords and lands
move up to the realm, and the realm crowns the strongest of them; the
others swear to him.

Over 200 years, 20 lords change kingdoms with their lands and households
(the realms trade the lead: Aldmark holds 2 estates at year 100 and 66 at
year 200), Caddis crowns 5 kings, about 150 nobles inherit and 34 die
without heirs, and all 72 estates stay held by some realm. 200 years take
3.7 s on 4 threads (5.7 s on 1), 60 to 140 nobles.

`check.sh`: 100 years match `expected.txt`, two runs match, and 1 thread
matches 4.

## What worked well

- Ownership reads like the domain. "What a lord holds" is
  `:objects("Estate", it >> owner == lord)`, a transfer is
  `estate >> owner |= buyer`, and escheat (lands going back to the liege)
  is no code at all.
- Inheritance is one `on erase:` action that re-points `owner` fields.
- A defecting lord takes his estates, knights and children with one
  `:move!`, because they are his contents.
- Conditions written in terms of ownership are cheap: `target >> owner ==
  self` looks only at the noble's own contents list.

## Fixed in the runtime while building it

| # | Finding | Fix |
|---|---------|-----|
| 1 | a container handed to a region, or moved, left its contents behind; a lord who owned anything couldn't be handed to a region at all (C0113) | contents go with their container (#113) |
| 2 | a decision's `where` couldn't call an action (`kingdom(target)` was "undefined variable") | formulas compile against the program's names (#114) |
| 3 | every decision checked every object of the target class, even with `target >> owner == self` | owner conditions use the contents list; target-free conditions run once (#114): 300 lords × 3,000 knights, 10 ticks, 11.5 s → 0.09 s |
| 4 | a lord who moved carried copies of his new king and realm; walking up `owner` stored those copies over the real ones, so the new realm lost its counts | objects cross regions by id (#114) |
| 5 | an action called in a condition (`strength(self)`) ran once per target | an action that only reads runs once per set of arguments while a decision is scored: 200 years 5.2 s → 3.7 s on 4 threads |
| 6 | walking up `owner` (`x \|= x >> owner`) copied each object and wrote it back into the store | a variable given a stored object holds its id; field chains and `==` don't copy (3% of the run; most of what is left is copying object ids, which are text) |

The findings below are still open.

## 1. "Is this object in my region?"

During a tick a region sees the other regions' objects too, so a
condition that means "an estate in my kingdom" has to walk up the owners
(`kingdom(target) == kingdom(self)`), which used to be slow (fixed: see the table). The sim
avoids the question by asking about ownership instead ("an estate held by
another vassal of my liege": `target >> owner >> owner == self >> owner`),
which is also the more feudal rule. No change proposed; noting it because
the first version of the sim reached for the slow form.

## 2. A field that names another region's object reads nil from outside

A region's snapshot keeps a field that names one of its own objects, but a
field that names another region's object (a vassal whose liege lives
elsewhere) becomes nil there, so the program running the world reads nil
through it. Here a defector always moves to his new liege's region, so the
sim never sees it.

**Change:** keep the id; the object can be found wherever it lives.

## 3. Text with quotes can't go inside `{}` in a string

`say("estates {:len(:objects("Estate"))}")` is a parse error: the inner
quote ends the string. Same as finding 9 of the RPG sim; the sim reads the
value into a variable first.
