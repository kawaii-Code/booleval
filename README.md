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
0 0 0 = 0
0 0 1 = 0
0 1 0 = 0
0 1 1 = 0
1 0 0 = 0
1 0 1 = 0
1 1 0 = 1
1 1 1 = 0
```

## Todo

This program was supposed to do quine's method, but for
now it only prints the truth table. Fix that. In order to do that:
- Support |- symbol
- Remember variable names and print them later
- Pretty print the truth table, stop at 0

Support for 0 and 1 constants
