Types describe what kind of value something is.

When you write a string, Goblin knows it is text. When you write a number, Goblin knows it is numeric. When you create an array, Goblin knows it is a collection. Every value in Goblin has a type.

Most of the time, you do not need to think about types. Goblin determines them automatically from the values you create.

```gbln
name | "Daniel"
age | 46
active | true
```

You can inspect a value's type using `.vt` or `valtype()`.

```gbln
name.vt
```

Output:

```gbln
str
```

---

## Primitive Types

Primitive types represent individual values.

### Strings

Strings store text.

```gbln
name | "Daniel"
```

Type:

```gbln
str
```

---

### Booleans

Booleans represent true or false values.

```gbln
active | true
```

Type:

```gbln
bool
```

Values:

```gbln
true
false
```

---

### Signed Integers

Signed integers store whole numbers.

```gbln
i8
i16
i32
i64
```

Example:

```gbln
age | 46
```

Type:

```gbln
int
```

---

### Unsigned Integers

Unsigned integers store non-negative whole numbers.

```gbln
u8
u16
u32
u64
```

---

### Floating Point Numbers

Floating point numbers store decimal values.

```gbln
f32
f64
```

Example:

```gbln
score | 99.5
```

Type:

```gbln
float
```

---

### Arbitrary Precision Numbers

For extremely large numeric values, Goblin provides:

```gbln
big
```

---

## Collection Types

Collections store multiple values.

### Arrays

Arrays store ordered values.

```gbln
nums | [1, 2, 3]
```

Type:

```gbln
array
```

Arrays may contain any value type.

```gbln
mixed | [1, "two", true]
```

To restrict an array to a specific element type, declare the type during creation.

```gbln
zip_codes.int | [90210, 80924]
```

Type:

```gbln
array(int)
```

When an array is type-locked, every element must be compatible with the declared type. Goblin validates this on creation and on every future assignment.

```gbln
zip_codes |= [10001, 94102]
```

This is valid. Every element is an `int`.

```gbln
zip_codes |= ["bad"]
```

This fails. `"bad"` cannot be converted to `int`.

Retethering a collection replaces the entire value. There is no partial update. The old array is thrown away and the new one takes its place.

---

### Maps

Maps store key-value pairs.

```gbln
person | {
    name: "Daniel"
    age: 46
}
```

Type:

```gbln
map
```

Keys are unique and values may be any type.

Maps are used throughout Goblin for structured data.

To restrict map values to a specific type, declare the type during creation.

```gbln
scores.int | { alice: 95, bob: 87 }
```

Type:

```gbln
map(int)
```

When a map is type-locked, every value must be compatible with the declared type. Keys are always strings and are not affected by the type lock.

Retethering a map replaces the entire value. The same rules apply as with arrays — all values in the new map must match the declared type if one exists.

---

## Domain Types

Goblin includes several built-in types that represent common real-world concepts.

These exist because percentages, money, dates, and durations are not simply numbers with special formatting attached to them. They represent distinct concepts and are treated as first-class types.

### Percentages

```gbln
tax | 8.25%
```

Type:

```gbln
pct
```

---

### Money

```gbln
price | $19.99
```

Type:

```gbln
money
```

---

### Dates

```gbln
birthday | 1981-07-28
```

Type:

```gbln
date
```

---

### Times

```gbln
start | 14:30
```

Type:

```gbln
time
```

---

### DateTimes

```gbln
meeting | 2026-06-07 14:30
```

Type:

```gbln
datetime
```

---

### Durations

```gbln
travel_time | 3h
cooldown | 30s
```

Type:

```gbln
duration
```

---

## Enumerations

Enumerations define a fixed set of named values.

They are useful when a value should only be one of a known group of options, such as status, priority, direction, difficulty, or mode.

```gbln
enum Status
    idle
    loading
    ready
    error
end
```

Enum values are accessed through the enum name.

```gbln
status | Status::idle
```

Enum values have the type:

```gbln
enum
```

Enums work well with judge when each possible value should choose a different branch.

```gbln
judge status using Status
    idle:    :say("System is idle")
    loading: :say("Processing...")
    ready:   :say("Ready to proceed")
    error:   :say("Error occurred")
end
```

---

## Objects

Objects are instances of classes.

```gbln
<>Kingdom | name: "", gold: 0

rome <> Kingdom | name: "Rome", gold: 1000
```

Type:

```gbln
object
```

Objects combine fields and actions into a single value.

---

## Inferred Variables

Variables are inferred by default.

```gbln
score | 100
```

Goblin determines the type automatically.

Because the variable is not type-locked, it may later contain a different type.

```gbln
score |= "one hundred"
```

This is valid.

---

## Type-Locked Variables

When a variable should always remain a specific type, declare the type during creation.

```gbln
score.i32 | 100
tax_rate.pct | 8.25
price.money | 19.99
```

After a variable has been type-locked, Goblin ensures all future assignments remain compatible with the declared type.

```gbln
score.i32 | 100

score |= 101
score |= "102"
```

Both assignments are valid because Goblin can convert the new value into an `i32`.

If the conversion fails, Goblin raises an error.

---

## Immutable Variables

Immutable variables cannot be reassigned.

```gbln
imm version | "1.0.0"
```

Attempting to update the variable raises an error.

```gbln
version |= "1.0.1"
```

Type locks and immutability may be combined.

```gbln
imm score.i32 | 100
```

---

## Casting

Casting converts a value from one type into another.

Temporary casts return a converted value without changing the variable.

```gbln
str(age)
age.str

f64(age)
age.f64
```

Recasts permanently change the variable's value and type.

```gbln
str!(age)
age.str!

f64!(age)
age.f64!
```

The `!` indicates mutation.

A type-locked variable can only be recast to its declared type. Attempting to recast to a different type raises an error.

---

## Type Inspection

Goblin provides built-in type inspection through both postfix and function forms.

```gbln
tax.vt
vt(tax)

tax.valtype
valtype(tax)
```

All four forms are equivalent.

What `.vt` reports depends on how the variable was declared.

An unlocked variable reports its current runtime type.

```gbln
age | 46
age.vt
```

```
int
```

A type-locked variable reports its declared lock type.

```gbln
age.i32 | 46
age.vt
```

```
i32
```

A type-locked collection reports its declared lock type qualified by the collection kind.

```gbln
zip_codes.int | [90210, 80924]
zip_codes.vt
```

```
array(int)
```

Type inspection is useful when debugging, exploring data, or working with dynamically typed values.
