# Town on DES: what stops the rebuild

`town.gbln` here is the town rebuilt on DES: residents and businesses are
classes, each district is a region, and `tick(districts)` runs a day.
Today it covers residents aging, shopping (a decision) and dying (a
transition). Each gap below stopped the next piece. None of them is worked
around in the code.

| # | Gap | Stops | Proposed change (needs Daniel's OK) |
|---|-----|-------|-------------------------------------|
| 1 | A class can't run a rule every tick | aging, paydays every 14 days, rent every 30 days | An `every N ticks` block in a class, run during the DES tick as `self` |
| 2 | A class can have only one decision | shopping *and* looking for work | Several named decisions; `judge using <name>` already names which one |
| 3 | Scores of real-value (`~`) fields collapse to 0 | choosing the cheaper shop | Scale real-value formulas over the candidates actually scored |
| 4 | Nothing happens when an overlay ends | birth after pregnancy, a job answer after a wait | An overlay can run an action when it ends |
| 5 | An erased object takes its fields with it | an estate going to the district fund on death | A transition can run an action as it fires |
| 6 | Objects can't move between regions | households moving district | Later (the spec lists move as later) |

## 1. No rule that runs every tick

The district handler has to loop over its objects to age them:

```
act district_day(st, msg)
    if :has(msg, "tick")
        repeat :objects(it >> age > -1)
            it >> age |= it >> age + 1
        xx
    xx
    return st
xx
```

This is plain code outside DES, and it only reaches the region's own
objects. Paying wages is worse. Staff can live in another district, so a
handler can't pay them; only a DES rule can reach another region's objects.

Proposed: a block in the class that DES runs on every object every N ticks,
during the tick, so it sees the whole world like decisions do:

```
<>Resident |
    ~age: 0
    every 1 tick
        self >> age |= self >> age + 1
    end
xx

<>Business |
    ~cash: 6000
    ~wage: 400
    every 14 ticks
        repeat :objects(it >> job == self >> id)
            it >> money |= it >> money + self >> wage
            self >> cash |= self >> cash - self >> wage
        xx
    end
xx
```

## 2. One decision per class

A resident shops and also looks for work, and both are choices among
businesses. A class has one `decision` and one `judge`. The syntax already
names the score (`score | decision ...`, `judge using score`), so the
natural extension is several named decisions:

```
shop | decision against Business by [ ... ]
judge using shop
    >= .2: buy(target)
end

work | decision against Business by [ ... ]
judge using work
    >= .5: apply_for_job(target)
end
```

## 3. Real-value scores collapse

```
score | decision against Business by [
    1 - ((target >> price) - 15) / 10
]
```

DES scales a formula's result to 0..1 using the range it works out from the
formula, and it assumes every `>>` field is a 0..1 trait. Prices are 15 to
24, so most scores land at 0 and the ranking is wrong. In the run below
only the 15- and 16-price shops got any sales; with `margin: .1` the
collapsed scores happen to leave those two inside the margin. The same
formula scaled over real ranges would rank all ten prices properly.

Proposed: when a formula reads a real-value (`~`) field, scale over the
scores of the candidates actually scored that tick (best = 1, worst = 0).
Formulas on 0..1 traits keep today's behaviour.

## 4. Nothing happens when an overlay ends

`overlay Pregnant on r at 1 for 270 ticks` lasts the right time, but when
it ends nothing runs, so no baby is born. The old town uses its event
queue for this.

Proposed: an overlay can name an action to run on its host when it ends.

## 5. Erasing loses the object's fields

`transition erase when ...` removes a resident with their money, so the
town's money total drops on every death. The old town hands it to the
district fund.

Proposed: a transition can run an action on `self` as it fires, before the
erase.

## 6. Moving between regions

A household moving district has to leave one region and join another.
There is no way to move an object between regions yet.

## Other findings from this run

- **Everyone shops at the same few places.** A decision picks the best
  target (within `margin`), so 1,000 residents all go to the 9 cheapest
  shops. The old town picks a random local shop. `margin: 1` gives a
  random pick; a choice weighted by score doesn't exist.
- **Money in flight isn't in any report.** A resident pays at once, and a
  shop in another district gets the money at the start of the next tick.
  The main program's total is short by a day of cross-district spending.
  It is conserved (one district alone never changes), but a report has to
  know that.
- **Everyone is broke by day 40.** With no wages (gap 1) residents spend
  their 400 to 700 and stop. The money total stays flat at 851,889 from
  day 60 on, which also shows nothing leaks.
- **Speed.** 365 days took 114 s on 4 threads, about 310 ms per day, against
  about 8 ms per day for the plain-map town. Most of it is copying every
  region's objects to the others each tick, and scoring 50 businesses for
  each of 1,000 residents. Too slow to iterate on; it needs its own speed
  pass once the gaps are settled.

## The 365-day run

```
town on DES: 1000 residents, 50 businesses, 4 districts, 365 days
day 30: population 1000, money 842903
day 60: population 1000, money 851889
...  (unchanged every 30 days)
day 365: population 1000, money 851889
--- businesses: sales by price ---
price 15: 4 businesses, 59100 sales each
price 16: 5 businesses, 61747 sales each
price 17 to 23: 41 businesses, 0 sales each
```

Day 30 is 8,986 short because that day's cross-district spending is still
in flight. 114 s on 4 threads.
