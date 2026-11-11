# Writing Goblin

How to write Goblin the way Goblin is written. Every example here is
idiomatic and runs as shown. When two forms work, the one shown is the one
to write.

## Blocks and comments

A block closes with crossbones, `xx`.

A comment is `///`. Write one only when the code can't say it: why a rule
exists, a unit, a source. A line that reads clearly gets no comment.

```goblin
act heal(hp, amount)
    return hp + amount
xx
```

## Names and binding

`|` binds a name, `|=` gives it a new value. There is no `=`.

```goblin
gold | 10
gold |= gold + 5
first, last | ..
```

Line up the `|` of a run of bindings:

```goblin
name  | "Ann"
level | 3
hp    | 40
```

Names are `snake_case`; classes are `Capitalized`.

## Nix: nil or empty

`x.nix?` is true when `x` is nil or empty (`..`, `[]`, `{}`). It is the one
presence check: never also test `x == ..`.

`..` is the empty string. Write it instead of `""`.

```goblin
title | ..
if title.nix? => title |= "Untitled"
```

`??` gives a fallback when the left side is nix. Fallbacks chain:

```goblin
label | nil
slug  | "home"
shown | label ?? slug ?? ..
```

## Actions

`act name(args)` ... `xx`. A default is bound with `|`.

`send` returns early; `return` is the last line of the action. Use `send`
for every exit in the middle, `return` once at the end.

```goblin
act greet(name, greeting | "hi")
    if name.nix? => send ..
    return "{greeting.title} {name}"
xx
```

`?? => send` returns when a value is missing, in one line:

```goblin
act first_word(text)
    t | text ?? => send ..
    return :keep_before(t, " ") ?? t
xx
```

Builtins are called with a colon, `:len(x)`, `:say(x)`, `:tick()`; your own
actions without one, `greet("Bo")`. A module's actions are `name::action()`.
The compiler holds you to it (owner, 2026-11-11): a builtin called without
its colon is R1260, a colon on your own action is R1261. "Builtin" means
anything Goblin provides. Statement words (`if`, `repeat`, `judge`,
`collect`, `attempt`, ...) and sentence forms (`pick 1 from xs`, `roll 2d6`,
`raw "..."`, `sweep`) are syntax and take no colon. `goblin fix colons <dir>`
rewrites older code (`--md` also fixes Goblin samples in Markdown).

Some values have properties, read without parentheses: `s.len`, `s.title`,
`s.upper`, `s.trim`, `x.nix?`.

## Branching

A one-line `if` uses `=>`. Use a block only for more than one line.

```goblin
hp | 0
if hp <= 0 => :say("down")

if hp > 20
    :say("healthy")
elif hp > 0
    :say("hurt")
else
    :say("down")
xx
```

`judge` takes the first arm that holds; `judge_all` runs every arm that
holds. Prefer it to a long `elif` chain.

```goblin
is_link  | true
is_image | false
judge
    is_link: :say("link")
    is_image: :say("image")
    else: :say("text")
xx
```

`judge using x` compares each arm with `x`:

```goblin
kind | "rare"
judge using kind
    "common": :say("plain")
    "rare": :say("shiny")
    else: :say("unknown")
xx
```

## Loops: repeat

`repeat` is the only loop. Don't write `for` or `while`.

```goblin
repeat 3
    :say("again")
xx

names | ["Ann", "Bo", ..]
repeat names as n
    if n.nix? => skip
    :say("{idx}: {n}")
xx

stock | {"iron": 3, "wood": 5}
repeat stock as item, count
    :say("{item} {count}")
xx

day | 0
repeat day < 7
    day |= day + 1
xx
```

`repeat names as n` also binds `idx`. `skip` goes to the next turn and
`stop` leaves the loop. `repeat` with nothing after it runs until `stop`.

`?? => skip` skips a missing value in one line:

```goblin
rows | [{"name": "Ann"}, {}]
repeat rows as r
    name | r{"name"} ?? => skip
    :say(name)
xx
```

## Text

`{name}` puts a value in a string. Triple quotes hold several lines.

```goblin
who  | "Ann"
html | """
<p>Hello, {who}</p>
"""
```

`sweep` walks the pieces of a text between two markers; `self` is the piece:

```goblin
text | "see [[home]] and [[about]]"
out  | text
sweep text
    "[[" ... "]]":
        :update_where!(out, self, :upper(self))
xx
:say(out)
```

## Collections

`[]` is an array, `{}` is a map. Read a map with `m{"key"}`, an array with
`a[0]`.

Builtins ending in `!` change the collection itself; without `!` they give
a changed copy.

```goblin
party | ["Ann"]
:put_last!(party, "Bo")

stats | {"hp": 3}
:update!(stats{"hp"}, stats{"hp"} + 1)
```

## Pipelines

A pipeline is a list of action names. `:summon(ctx, pipeline)` runs them in
order; each one takes the context and returns it.

```goblin
act add_one(ctx)
    :update!(ctx{"n"}, ctx{"n"} + 1)
    return ctx
xx

act double(ctx)
    :update!(ctx{"n"}, ctx{"n"} * 2)
    return ctx
xx

pipeline | ["add_one", "double"]
ctx      | :summon({"n": 1}, pipeline)
:say(ctx{"n"})
```

## Classes and objects

`<>Name |` declares a class; its fields have defaults. `p <> Name | ...`
makes an object. `>>` reads a field, `.` calls a method. Inside a method,
`@field` is the object's field and `self.method()` calls another method.

```goblin
<>Hero |
    name: ..
    hp: 10
    act hurt(n)
        @hp |= @hp - n
        if @hp <= 0 => send "down"
        return "standing"
    xx
xx

h <> Hero | name: "Ann", hp: 5
:say(h.hurt(2))
h >> hp |= h >> hp + 1
:say(h >> hp)
```

## Errors

```goblin
attempt
    cfg | :read_json("settings.json")
rescue e
    :say("no settings: {e}")
xx
```

## Simulations (DES)

A simulation is classes, overlays and decisions, run with `:tick()`. A
class holds what its objects decide and when they leave. An overlay is a
temporary object on another one.

```goblin
<>Fighter |
    name: ..
    side: 0
    hp: 10
    atk: 4
    decision enemy
        targets: Fighter where (target >> side) != @side
        score => -(target >> hp)
    xx
    judge
        enemy: self.strike(enemy >> target)
    xx
    order: -@atk
    transition erase where @hp <= 0
    act strike(t)
        t >> hp |= t >> hp - @atk
    xx
xx

overlay poisoned
    hosts: Fighter
    lasts: 3
    dup: replace
    every 1 tick => @hp |= @hp - 2
xx

ann <> Fighter | name: "Ann", side: 0, hp: 12, atk: 5
orc <> Fighter | name: "Orc", side: 1, hp: 9, atk: 3
:overlay!(orc, poisoned)
repeat :len(:objects(Fighter)) > 1
    :tick()
xx
repeat :objects(Fighter) as f
    :say("{f >> name} stands with {f >> hp} hp")
xx
```

Overlays of one kind belong to an overlay class, `<^>`. Its `dup:` decides
what happens when a second member lands on the same host:

```goblin
<>Creature |
    focus: 0
    attack: 0
xx

<^>Mood |
    hosts: Creature
    dup: replace
xx

overlay calm <^> Mood
    modify
        @focus + 2
    xx
xx

overlay angry <^> Mood
    modify
        @attack + 3
    xx
xx

c <> Creature | focus: 0
:overlay!(c, calm)
:overlay!(c, angry)
:say(:overlays_of(c))
```

`tests/test.gbln` and `sims/` show larger programs.
