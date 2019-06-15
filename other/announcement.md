The latest release introduces an easier way to construct `NamedTuples` when the field symbols
and the values to be assigned are available simultaneously as args to a function, or for direct use in the REPL.

```
using NamedTupleTools

namesof_values  = (:instrument, :madeby)
matching_values = ("violin", "Stradivarius")

nt = namedtuple(namesof_values, matching_values)
(instrument = "violin", madeby = "Stradivarius")
```

- The `names` and the `values` may be given as a `Tuple` or as a `Vector`, independently.
- The names can be given as `Symbols` or as `Strings`.  The values can be of any type or types, afaik.
