# RPG combat on DES: what got in the way

`combat.gbln` is a party (knight, archer, cleric) against five monsters
(three goblins, an ogre, a shaman). One tick is one round. Every fighter
picks a target with a decision: `attack` for an enemy, `heal` for an ally.
A fighter at 0 hp falls (`transition erase`, with `on erase: fall`). The
shaman's bite poisons, and the knight raises a shield (overlay `Guarded`,
`on end: lower_shield`). It runs 1,000 battles with `:swarm` (0.6 s on 4
workers; the party wins 524), then shows battle 7 round by round.

`check.sh`: the output is as expected, two runs match, and 1 worker matches 4.

Each finding below has the code I had to write, what I would change, and
why. Nothing here is changed yet.

## 1. A timed overlay undoes everything else when it ends (bug)

When an overlay with `lasts N ticks` ends, every field it modifies goes back
to the value it had *before the overlay*, so any other change to that field
in the meantime is lost. The interpreter does the same.

```goblin
overlay Poison |
    lasts 3 ticks
    modifies:
        hp - 2
    end
end
a <> F | hp: 20
overlay Poison on a at 1      -- hp 18
a >> hp |= a >> hp - 1        -- each round something else hits for 1
tick()                        -- hp 17, 16, then 20 when Poison ends
```

The 3 points of damage taken meanwhile vanish. A shield (`def + 3`) has
the same problem: a defence change during the shield is lost when it drops.

**Change:** when an overlay ends, take back only what it added (hp + 2
here), not the old value.

## 2. An overlay can't do something every round

`modifies:` applies once, when the overlay goes on. Poison (2 damage a round
for 3 rounds) can't be an overlay, so the sim keeps a `poison` counter and
an `every 1 tick` block:

```goblin
every 1 tick
    if self >> poison > 0
        self >> hp |= self >> hp - 2
        self >> poison |= self >> poison - 1
    xx
end
```

**Change:** a per-tick block on overlays, beside `modifies:`:

```goblin
overlay Poisoned |
    lasts 3 ticks
    each tick:
        hp - 2
    end
end
```

`each tick:` would mean: while the overlay is on, apply these every tick
(scaled by strength like `modifies:`), and never take them back.

## 3. A fighter gets every decision every round

A class's decisions all run, one after another. The cleric would heal and
then attack in the same round, so every action checks and sets an `acted`
field, which an `every` block clears each round.

**Change:** a class can say its decisions are alternatives:

```goblin
heal | decision against Fighter where [...] by [...]
attack | decision against Fighter where [...] by [...]
one of heal, attack
```

`one of` would mean: try them in that order, and stop at the first whose
judge runs an action.

## 4. A decision can't leave targets out

Every object of the target class is a candidate. To aim only at enemies I
had to write arithmetic: `abs(self >> side - target >> side)` (1 for an
enemy, 0 for an ally). A comparison (`target >> side != self >> side`)
gives a bool, the formula fails, and nobody acts: no error, nothing happens.

The cleric should heal only an ally under half health. The judge sees
scores relative to the other candidates (the best one scores 1), so it
can't say "under half"; the action checks it instead.

**Change:** a `where` filter on a decision:

```goblin
heal | decision against Fighter where [
    target >> side == self >> side and target >> hp * 2 < target >> max_hp
] by [
    1 - target >> hp / target >> max_hp
]
```

Only the objects that pass are scored; with none, the decision does
nothing. Also: a formula that fails should be an error, not silence.

## 5. The fallen keep fighting until the round ends

`transition erase when hp <= 0` runs at the end of the tick, after every
decision. A fighter killed early in a round stays a target (the archer
wasted its turn on a dead shaman), and could still act. The sim drops them
from the score with `min(1, max(0, target >> hp))` and checks `hp > 0` in
every action. With `where` (4) the score part goes away, but a dead
fighter would still act.

**Change (a choice for you):** either check an object's erase
transitions right after each action that changes it, or leave the end of
tick as the one place things die and rely on `where` plus a check in the
action. I lean to the first: "dies when hp reaches 0" is what a reader
expects.

## 6. You can't ask whether an object has an overlay

`:overlay_strength(name, host)` and `:overlays_of(host)` take the *name of
the variable* the overlay was put on, not the object. Fighters made inside
an action have no such name, so the sim keeps a `guarded` field set when
the shield goes up and cleared by `on end: lower_shield`.

**Change:** let both take the object (`:overlay_strength("Guarded", t)`).
No new builtin.

## 7. Everyone acts in the order they were made

Decisions run in creation order, so the party always acts before the
monsters. Speed or initiative can't be expressed.

**Change:** a class can set its turn order:

```goblin
<>Fighter |
    ~speed: 5
    acts in order of speed
```

highest first, ties in creation order.

## 8. Smaller things

- `say("{n - wins}")` and `say("{lost[0]}")` print the braces as written;
  only plain names are filled in. Either allow expressions there or
  report an error.
- `:round(x, 1)` is an error; there is no way to round to digits.
- There is no way to clear the objects between two battles in one session.
  The sim gets around it by running each battle in a `:swarm` task, which
  starts from a clean state.
