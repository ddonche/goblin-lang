# Town on DES: what is built and what still stops it

`town.gbln` is the town written with DES. Residents, businesses and district
funds are classes. Each district is a region, and `tick(districts)` runs a
day. It is 377 lines; the plain-map town in `sims/town` is 841.

What it does now:

- **Residents** age every day, shop every day (decision `shop`), take a job
  at a business that is hiring (decision `work`), pay rent to their
  district's fund every 30 days, and die (transitions).
- **Businesses** update whether they are hiring every day, and pay their
  staff every 14 days. Staff can live in any district. When a business is
  short of cash it lays off its newest staff, and it shares spare cash as a
  bonus. A business with no staff and no cash closes.
- **Funds** pay a weekly benefit to adults without work, and spend what
  they hold above 20,000 at their district's open businesses.

`check.sh` runs 60 days and checks four things: the output is as expected,
two runs match, 1 thread matches 4 threads, and with one district (so no
money is in flight between districts) the money total never changes.

## Gaps

| # | Gap | Status |
|---|-----|--------|
| 1 | A class can't run a rule every tick | **Built**: `every N ticks ... end` (PR #68) |
| 2 | A class can have only one decision | **Built**: several named decisions, `judge using <name>` (PR #69) |
| 3 | Scores on real-value (`~`) fields collapse to 0 | Open. The town uses a 0..1 trait, `cheapness`, set when a business is made |
| 4 | Nothing happens when an overlay ends | Open. Stops births (pregnancy, then a baby) |
| 5 | An erased object takes its fields with it | Open. A death takes the person's money out of the town |
| 6 | Objects can't move between regions | Open, later. Stops households moving district |

## Found while building this

- **Fixed: class methods couldn't read program variables** (PR #70). The VM
  read `adult` inside a method as a field of `self` and got nil. The
  interpreter reads the program's variable.
- **Fixed: looping over `:overlays()` deleted the overlays' hosts** (PR #67).
- **`margin` is one number for all of a class's decisions.** The town wants
  a wide spread for shopping (`.8`) and none for work. Proposed: `margin`
  per decision, e.g. `shop | decision against Business margin .8 by [...]`.
  Needs Daniel's OK.
- **A decision's action fails silently.** An error inside `buy` or `apply`
  is ignored, so a typo there shows up only as "nothing happens".
  `every` blocks stop the tick on an error instead. Proposed: decision
  actions should do the same. Needs Daniel's OK, because it changes how
  existing programs behave.
- **`and` inside call parentheses doesn't parse**, so
  `:objects(it >> district == d and it >> age > 18)` has to be a loop
  with an `if`.

## The 365-day run

Nearly every adult has a job, all 50 businesses stay open, and the
poorest fifth averages 137. The money total moves only by what is in flight between
districts. It drops most on paydays (day 210 is one), because wages to
staff in other districts arrive the next day. One person died, and their
money left with them (gap 5).

```
town on DES: 1000 residents, 50 businesses, 4 districts, 365 days
money at the start: 850500
day 30: population 1000, adults 737, working 737, open businesses 50, money 830370
day 60: population 1000, adults 739, working 739, open businesses 50, money 839967
day 90: population 1000, adults 742, working 742, open businesses 50, money 840666
day 120: population 1000, adults 744, working 744, open businesses 50, money 840527
day 150: population 999, adults 745, working 745, open businesses 50, money 846308
day 180: population 999, adults 745, working 745, open businesses 50, money 850265
day 210: population 999, adults 746, working 746, open businesses 50, money 801764
day 240: population 999, adults 748, working 739, open businesses 50, money 834136
day 270: population 999, adults 749, working 742, open businesses 50, money 840378
day 300: population 999, adults 751, working 751, open businesses 50, money 840753
day 330: population 999, adults 752, working 752, open businesses 50, money 840811
day 360: population 999, adults 752, working 752, open businesses 50, money 846623
day 365: population 999, adults 753, working 745, open businesses 50, money 830958

=== End of run: day 365 ===
Where the money is (total 830958; 19542 left with the dead or is in flight):
  people            372156   45%
  businesses        243537   29%
  district funds    215265   26%

Wealth of the 753 adults, poorest fifth to richest:
  fifth   holds   average   lowest   highest
  1         9%       137        0       315
  2        21%       326      316       336
  3        22%       344      337       352
  4        23%       361      352       369
  5        25%       384      369       424

Businesses by price:
  price  businesses  open  staff  avg cash  avg sales
     15           5     5     58       662     123885
     16           9     9    105       696     131238
     17           5     5     63       763     136231
     18           5     5     63       872     144649
     19           4     4     58      2307     151288
     20           3     3     40      1713     158196
     21           5     5     76     10068     164710
     22           5     5     76     12268     171060
     23           9     9    141     11079     179408

Districts:
  district  people  adults  working  avg adult money    fund
         0     250     203      99%              302    56901
         1     249     184     100%              317    53270
         2     250     184      98%              316    52538
         3     250     182      99%              306    52556
```

Speed: a year takes about 15 seconds on 4 threads (about 41 ms a day) with
PRs #71 and #72; it took about 2 minutes 12 seconds before them, with the
same output. #71 stops `pick 1 from 1...200000` from building the whole
range on every call. #72 speeds up decision scoring and `:objects(...)`
queries. The plain-map town takes about 8 ms a day. What is left: copying
every region's objects to the other regions each day, and the actions that
decisions call.
