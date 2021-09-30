# Janet NestedText

A janet implementation of [NestedText](https://nestedtext.org).

NestedText is an alternative to Yaml that aims to be simpler.

## Example

```
> (import nested-text)
> (nested-text/decode (slurp "example/example.nt"))
[:ok @{"president" @{"name" ...} ...}]

> (nested-text/print ["a" "b" ["c"]])
- a
- b
-
  - c
```
