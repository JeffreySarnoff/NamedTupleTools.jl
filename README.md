# NamedTupleTools.jl
### Some NamedTuple utilities


#### Copyright © 2015-2020 by Jeffrey Sarnoff. This work is released under The MIT License.

-----

[![Build Status](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl)[![codecov](https://codecov.io/gh/JeffreySarnoff/NamedTupleTools.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JeffreySarnoff/NamedTupleTools.jl)
-----

## Overview

`NamedTuples` are built from fieldnames, given as `Symbols` and field values, as they may be given.
These utilities make some uses of `NamedTuples` more straightforward.  This package benefits greatly
from contributions by others (please see [Credits](#Credits)). 


## Operations

### Construction
- [given `names` and `values`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#construction-from-names-and-values-kristoffer-carlsson)
- [given a `Dict{Symbol, Any}`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#dict-construction-reconstruction)
- [given a `struct`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#struct-construction-conversion)
- [given a `Vector` of `Pairs`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#vector-of-pairs-peter-deffebach)
- inversive [_Reconstruction_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#reconstruction)

### Reconstruction
- [obtaining `names` and `values`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#construction-from-names-and-values-kristoffer-carlsson)
- [obtaining a `Dict{Symbol, Any}`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#dict-construction-reconstruction)
- [obtaining a `struct`](https://github.com/JeffreySarnoff/NamedTupleTools.jl#struct-construction-conversion)
- [obtaining a `Vector{Pair}`](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#convert-to-vector-of-pairs)
- inversive [_Construction_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#construction)

### Selection
- [select one or more named constituents](https://github.com/JeffreySarnoff/NamedTupleTools.jl#select-chad-scherrer)
- complements [_Deletion_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#deletion)

### Deletion
- [delete one or more named constituents](https://github.com/JeffreySarnoff/NamedTupleTools.jl#delete)
- complements [_Selection_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#selection)

### Merging
- [merge one or more NamedTuples](https://github.com/JeffreySarnoff/NamedTupleTools.jl#merge)
- undoes [_Splitting_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#splitting)

### Splitting
- [split a NamedTuple into one or more NamedTuples formed of its consituents](https://github.com/JeffreySarnoff/NamedTupleTools.jl#split-seth-axen)
- undoes [_Merging_](https://github.com/JeffreySarnoff/NamedTupleTools.jl/blob/master/README.md#merging)


-----
## Functions

### Construction from names and values
```julia
julia> using NamedTupleTools
julia> namesofvalues  = (:instrument, :madeby)
julia> matchingvalues = ("violin", "Stradivarius")

julia> nt = namedtuple(namesofvalues, matchingvalues)
(instrument = "violin", madeby = "Stradivarius")
```
- The names may be given as `Symbols` or `Strings`
- The names, values may be `Tuples` or `Vectors`

### Selecting Aspects of Elements
```julia
julia> using NamedTupleTools

julia> nt = NamedTuple{(:a, :b)}(1.0, "two")
(a = 1.0, b = "two")

julia> typeof(nt) == NamedTuple{(:a, :b),Tuple{Float64,String}}
true

julia> propertynames(nt) == (:a, :b)
true

julia> fieldnames(nt) == (:a, :b)             # synonym for the moment
true

julia> fieldtypes(nt) == (Float64, String)
true

julia> valtype(nt) == Tuple{Float64, String}
true

julia> fieldvalues(nt) == (1.0, "two")
true
```

### Use NamedTuple prototypes
```julia
using NamedTupleTools

julia> namedtuple(:a, :b, :c)(1, 2.0, "three")
(a = 1, b = 2.0, c = "three")

#=
    namedtuple(  name1, name2, ..  )
    namedtuple( (name1, name2, ..) )
       where the `names` are all `Symbols` or all `Strings`

Generate a NamedTuple prototype by specifying or obtaining the fieldnames.
The prototype is applied to fieldvalues, giving a completed NamedTuple.
=#
julia> nt = (a = 1, b = "two")
(a = 1, b = "two")

julia> nt_prototype = prototype(nt)
NamedTuple{(:a, :b),T} where T<:Tuple

julia> nt_prototype = namedtuple(:a, :b)
NamedTuple{(:a, :b),T} where T<:Tuple

julia> nt = nt_prototype(1, 2)
(a = 1, b = 2)

julia> nt = nt_prototype("A", 3)
(a = "A", b = 3)

julia> isprototype(nt_prototype)
true

julia> isprototype(nt)
false
```
### Select
```julia
using NamedTupleTools

julia> nt = (a = 1, b = 2, y = 25, z = 26)
(a = 1, b = 2, y = 25, z = 26)

julia> ay = select(nt, (:a, :y))
(a = 1, y = 25)
```

### Delete
```julia
using NamedTupleTools

julia> ntproto = namedtuple( :a, :b, :c );
NamedTuple{(:a, :b, :c),T} where T<:Tuple

julia> delete(ntproto, :b) === namedtuple(:a, :c)
true

julia> fieldnames(delete(ntproto, :b))
NamedTuple{(:a, :c),T} where T<:Tuple

julia> fieldnames(delete(ntproto, (:a, :c)), fieldnames(delete(ntproto, :a, :c)
(:b,), (:b,)

julia> nt = ntproto(1, 2, 3)
(a = 1, b = 2, c = 3)

julia> delete(nt, :a)
(b = 2, c = 3)

julia> delete(nt, :a, :c)
(b = 2,)
```

### Merge

```julia
# merge from 2..7 NamedTuples

julia> ntproto1 = namedtuple(:a, :b);
julia> ntproto2 = namedtuple(:b, :c);

julia> merge(ntproto1, ntproto2)
NamedTuple{(:a, :b, :c),T} where T<:Tuple
```

```julia
julia> nt1 = (a = 3, b = 5);
julia> nt2 = (c = 8,);

julia> merge(nt1, nt2)
(a = 3, b = 5, c = 8)

julia> nt1 = (a = 3, b = 5);
julia> nt2 = (b = 6, c = 8);

julia> merge(nt1, nt2)
(a = 3, b = 6, c = 8)
```

### Split
```julia
julia> using NamedTupleTools

julia> nt = (a = 1, b = 2, c = 3, d = 4);

julia> split(nt, :a)
((a = 1,), (b = 2, c = 3, d = 4))

julia> split(nt, (:a, :b))
((a = 1, b = 2), (c = 3, d = 4))

julia> merge(split(nt, (:a, :b))...) == nt
true
```

### Struct construction, conversion
```
using NamedTupleTools

julia> struct MyStruct
           tally::Int
           team::String
       end

julia> mystruct = MyStruct(5, "hometeam")
MyStruct(5, "hometeam")

julia> mynamedtuple = ntfromstruct(mystruct)
(tally = 5, team = "hometeam")

julia> ntstruct = structfromnt(MyStruct, mynamedtuple)
MyStruct(5, "hometeam")

julia> mystruct == ntstruct
true
```

### AbstractDict construction, reconstruction
```julia
julia> nt = (a = 1, b = 2)
(a = 1, b = 2)

julia> convert(Dict, nt)
Dict{Symbol,Int64} with 2 entries:
  :a => 1
  :b => 2
  
julia> adict = Dict(:a => 1, :b => "two")
Dict{Symbol,Any} with 2 entries:
  :a => 1
  :b => "two"

julia> nt = namedtuple(adict)
(a = 1, b = "two")

julia> convert(Dict, nt)
Dict{Symbol,Union{Int64, String}} with 2 entries:
  :a => 1
  :b => "two"

julia> nt = namedtuple(adict)
(a = 1, b = 2//11, c = "three")

julia> convert(Dict, nt)
Dict{Symbol,Union{Rational{Int64}, Int64, String}} with 3 entries:
  :a => 1
  :b => 2//11
  :c => "three"

julia> using OrderedCollections: OrderedDict, LittleDict

julia> ldict = OrderedDict(:a => 1, :b => "two")
OrderedDict{Symbol,Any} with 2 entries:
  :a => 1
  :b => "two"

julia> nt = namedtuple(ldict)
(a = 1, b = "two")

julia> convert(LittleDict, nt)
LittleDict{Symbol,Union{Int64, String},Array{Symbol,1},Array{Union{Int64, String},1}} with 2 entries:
  :a => 1
  :b => "two"
```

### Vector of Pairs
```julia
julia> vec = [:a => 1, :b => 2]
2-element Array{Pair{Symbol,Int64},1}:
 :a => 1
 :b => 2

julia> nt = namedtuple(vec)
(a = 1, b = 2)
```

### convert to Vector Of Pairs
```julia
julia> nt = (a=1, b=2);
julia> convert(Vector{Pair}, nt)
2-element Array{Pair{Symbol,Int64},1}:
 :a => 1
 :b => 2
 
nt = (a = 1, b = "two", c = 3.0);
vec = convert(Vector{Pair}, nt)
3-element Array{Pair{Symbol,B} where B,1}:
 :a => 1
 :b => "two"
 :c => 3.0
```

### Variables mixed with standard syntax 
```julia
julia> a, b, c, d, f = 1, 1.0, 1//1, "one", (g=1,)
(1, 1.0, 1//1, "one", (g = 1,))

julia> nt = @namedtuple(a, b, c, d, e = a + b, f...)
(a = 1, b = 1.0, c = 1//1, d = "one", e = 2.0, g = 1)
```

## Credits

- Construction from names and values
    - _submitted by Kristoffer Carlsson_

- Use NamedTuple prototypes
    - _improved by Chad Scherrer_

- Select
    - _submitted by Chad Scherrer_

- Split
    - _submitted by Seth Axen_

- AbstractDict construction, reconstruction
    - _improved by Kevin Squire_

- Vector of Pairs
    - _submitted by Peter Deffebach_

- Variables mixed with standard syntax 
    - _submitted by Sebastian Pfitzner, Takafumi Arakaki_
