# YALL Conformance Tests

Every test case lives in its own directory.

## Valid Tests

```
tests/valid/V0001-example/
    input.yall
    expected.json
```

- `input.yall` is the source document.
- `expected.json` is the canonical parsed representation.

A parser passes if parsing `input.yall` produces a value identical to `expected.json`.

## Invalid Tests

```
tests/invalid/I0001-example/
    input.yall
    error.txt
```

- `input.yall` must fail to parse.
- `error.txt` contains the expected error message or error class.

## Roundtrip Tests

```
tests/roundtrip/R0001-example/
    input.yall
    expected.yall
```

The parser reads `input.yall`.

The writer serializes the parsed value.

The resulting output must exactly match `expected.yall`.