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

Support for 0 and 1 constants
