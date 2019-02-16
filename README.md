# NamedTupleTools.jl
### Some NamedTuple utilities


#### Copyright © 2015-2019 by Jeffrey Sarnoff. This work is released under The MIT License.

-----

[![Build Status](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/NamedTupleTools.jl)
-----

## Overview

`NamedTuples` are built from fieldnames, given as `Symbols` and field values, as they may be given.
These utilities make some uses of `NamedTuples` a little more straightforward.  

## Use
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

julia> ntproto = namedtuple( :a, :b, :c )
NamedTuple{(:a, :b, :c),T} where T<:Tuple

julia> nt123 = ntproto(1, 2, 3)
(a = 1, b = 2, c = 3)

julia> ntAb3 = ntproto("A", "b", 3)
(a = "A", b = "b", c = 3)

julia> isprototype(ntproto)
true

julia> isprototype(nt123)
false


julia> delete(nt123, :a) === (b = 2, c = 3)
true
julia> delete(nt123, :a, :c) === delete(nt123, (:a, :c)) === (b = 2,)
true
julia> delete(ntproto, :b) === namedtuple(:a, :c)
true

julia> ntproto1 = namedtuple(:a, :b);
julia> ntproto2 = namedtuple(:b, :c);

# merge supports merging 2..7 NamedTuples
julia> merge(ntproto1,ntproto2)
NamedTuple{(:a, :b, :c),T} where T<:Tuple
```

## struct construction, conversion
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

## Dict construction, conversion
```julia
julia> dict = Dict("a" => 1, "b" => 2)
Dict{String,Int} with 3 entries:
  "a" => 1
  "b" => 2

julia> nt = namedtuple(dict)
(a = 1, b = "two")

julia> dict = Dict(:a => 1, :b => 2//11, :c => "three")
Dict{Symbol,Any} with 3 entries:
  :a => 1
  :b => 2//11
  :c => "three"

julia> nt = namedtuple(dict)
(a = 1, b = 2//11, c = "three")

julia> Dict(nt) == dict
true
```

## Vector of Pairs (pdeffebach)
```julia
julia> vec = [:a => 1, :b => 2]
2-element Array{Pair{Symbol,Int64},1}:
 :a => 1
 :b => 2

julia> nt = namedtuple(vec)
(a = 1, b = 2)
```

##  Defined Variables (Sebastian Pfitzner)
```julia
julia> a, b, c, d = 1, 1.0, 1//1, "one"
(1, 1.0, 1//1, "one")
julia> nt = @namedtuple(a, b, c, d)
(a = 1, b = 1.0, c = 1//1, d = "one")

```
