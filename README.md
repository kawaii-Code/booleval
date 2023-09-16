# Booleval

Parses and prints the truth table of a given formula.
The formula may contain single-letter variables,
and (&), or (|), xor (^), eq (~), implication (->)
and negation (!).

I made this because this will help me with my university
homework and also as an introduction to Zig.

## Example

```console
> a & !(b -> c)
a = 0 b = 0 c = 0 | 0
a = 1 b = 0 c = 0 | 0
a = 0 b = 1 c = 0 | 0
a = 1 b = 1 c = 0 | 1
a = 0 b = 0 c = 1 | 0
a = 1 b = 0 c = 1 | 0
a = 0 b = 1 c = 1 | 0
a = 1 b = 1 c = 1 | 0
```

```console
> (a & b), b |- c
a = 0 b = 0 c = 0 | 1
a = 1 b = 0 c = 0 | 1
a = 0 b = 1 c = 0 | 1
a = 1 b = 1 c = 0 | 0
a = 0 b = 0 c = 1 | 1
a = 1 b = 0 c = 1 | 1
a = 0 b = 1 c = 1 | 1
a = 1 b = 1 c = 1 | 1
```

## Quickstart

`zig build run`

## Todo

This program was supposed to do quine's method, but for
now it only prints the truth table. Fix that. In order to do that:
- [X] Support |- symbol
- [X] Remember variable names and print them later
- [ ] Sort variables somehow (also sort lexicographically)
- [ ] Pretty print the truth table, stop at 0
- [ ] Support for 0 and 1 constants
