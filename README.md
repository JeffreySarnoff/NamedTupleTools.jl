# NamedTupleTools.jl
### Some NamedTuple utilities


#### Copyright © 2015-2018 by Jeffrey Sarnoff. This work is released under The MIT License.

-----


[![Build Status](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl)

-----

## Use
```julia
using NamedTupleTools

ntproto1 = tuplenames(:a, :b, :c, :d)
ntproto2 = tuplenames(:a, :b)

nt1 = ntproto1(1, 2, 3, 4)   # (a = 1, b = 2, c = 3, d = 4)
nt2 = ntproto2("one", "two") # (a = "one", b = "two")

isprototype(ntproto1) # true
isprototype(nt1) # false

ntproto2(nt1) # (a = 1, b = 2)

delete!(nt1, :a) # (b = 2, c = 3, d = 4)
delete!(nt1, :a, :c) == delete!(nt1, (:a, :c)) # (b = 2, d = 4)
delete!(ntproto1, (:b, :c)) # prototype: NamedTuple{(:a, :d),T} where T<:Tuple

merge(nt1, nt2) # (a = "one", b = "two", c = 3, d = 4)
```
