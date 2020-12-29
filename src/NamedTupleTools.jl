"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

see [`namedtuple`](@ref), [`isprototype`](@ref),
    [`fieldnames`](@ref), [`fieldtypes`](@ref), [`fieldvalues`](@ref)
    [`delete`](@ref), [`merge`](@ref), [`merge_recursive`](@ref)
"""
module NamedTupleTools

export @namedtuple,
       namedtuple, isprototype, prototype,
       fieldnames, fieldtypes, fieldvalues, 
       merge, merge_recursive,
       split,
       delete,
       select,
       ntfromstruct, structfromnt,
       @structfromnt

import Base: fieldnames, values, merge, split

if isdefined(Base, :fieldtypes)
     import Base: fieldtypes
end

# internal support for low level manipulation

"""
    detuple( Tuple{_} )

Retrieve the types that are internal to the `Tuple` as a (_).
"""
detuple(::Type{T}) where {T<:Tuple} = Tuple(T.parameters)

"""
    retuple( (_) )

Generate a `Tuple` with the given internal types as a `Tuple{_}`.
"""
retuple(x::Tuple) = Tuple{x...,}

# accept comma delimited values
NamedTuple{T}(xs...) where {T<:} = NamedTuple{T}(xs)

# obtain aspects of a NamedTuple

"""
   fieldnames( namedtuple )
   fieldnames( typeof(namedtuple) )

Retrieve, as symbols, the name of each field in appearance (first..last) order.
- Note: for any nested field, this obtains only the name of top-level field.

Technical note: With some applications, this function is used heavily.
Fortunately, the operation is completely determined by the argument's type.
"""
Base.fieldnames(x::T) where {N,S, T<:NamedTuple{N,S}} = N
Base.fieldnames(::Type{T}) where {N,S<:Tuple, T<:Union{NamedTuple{N},NamedTuple{N,S}}} = N

Base.fieldnames(x::T) where {T} = fieldnames(T) # for structs

"""
    field_types( namedtuple )
    field_types( typeof(namedtuple) )

Retrieve the values' types as `Tuple{<types>}`.

see: [`fieldtypes`](@ref)
"""
function field_types(x::T) where {N, S, T<:NamedTuple{N,S}} = S
function field_types(::Type{T}) where {N, S<:Tuple, T<:NamedTuple{N,S}} = S
function field_types(::Type{T}) where {N, T<:NamedTuple{N}} = NTuple{length(N),Any}
			
"""
    fieldtypes( namedtuple )
    fieldtypes( typeof(namedtuple) )

Retrieve the values' types as a tuple of types `(<types>,)`.

see: [`field_types`](@ref)
"""			
Base.fieldtypes(x::T) where {N, S, T<:NamedTuple{N,S}} = detuple(S)
Base.fieldtypes(::Type{T}) where {N, S<:Tuple, T<:NamedTuple{N,S}} = detuple(s)
function Base.fieldtypes(::Type{T}) where {N, T<:NamedTuple{N}}
     Tuple(NTuple{length(N),Any}.parameters)
end

Base.fieldtypes(x::T) where {T} = fieldtypes(T) # for structs
			
"""
    fieldvalues(x)

obtain values assigned to fields of a struct type (in field order)
- `NamedTuples` and `struct`s are struct types
"""
function fieldvalues(x::T) where {T}
     isstructtype(T) && return unsafe_fieldvalues(x)
     throw(ArgumentError("$(T) is not a struct type"))
end

unsafe_fieldvalues(x::T) where {T} = getfield.(Ref(x), fieldnames(T))

function unsafe_fieldnamesvalues(x::T) where {T}
    names = fieldnames(x)
    values = getfield.(Ref(x), names)
    return names, values				
end

namedtuple(x::DataType) = ntfromstruct(x)

function ntfromstruct(x::T) where {T}
     !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))
     names, values = unsafe_fieldnamesvalues(T)
     return NamedTuple{names}(values)
end

# an instance of type S, a Struct
function structfromnt(::Type{S}, x::NT) where {S, N, T, NT<:NamedTuple{N,T}}
     names = N
     values = unsafe_fieldvalues(x)
     if fieldnames(S) != names
          throw(ErrorException("fields in ($S) do not match ($x)"))
     end
     return S(values...,)
end

# the Struct itself
function structfromnt(structname::Union{Symbol, String}, nt::NamedTuple{N,T}) where {N,T}
    sname = Symbol(structname)
    names = N
    types = detuple(T)
    tostruct = Meta.parse(NamedTupleTools.struct_from(sname, names, types))
    eval(tostruct) # generate Struct
    return nothing
end

macro structfromnt(sname, nt)
    :( eval(structfromnt($(esc(sname)), $(esc(nt)))) )
end

# Expr part from Fredrik Ekre
struct_from(structname, names, types) =
	"Expr(:struct,
		false,
		Expr(:curly,
			 :$structname
		),
		Expr(:block,
			map((x,y) -> Expr(:(::), x, y), $names, $types)...
		)
	)"

structfrom(structname, names, types) = eval(eval(Meta.parse(struct_from(structname, names, types))))

"""
    namedtuple(namesforvalues, valuesfornames)
"""

# from kristoffer.carlsson

@inline function namedtuple(namesforvalues::NTuple{N,Symbol}, valuesfornames) where {N}
    N == length(valuesfornames) || throw(ErrorException("lengths must match"))
    return (; zip(namesforvalues, valuesfornames)...,)
end

@inline function namedtuple(namesforvalues::Vector{Symbol}, valuesfornames)
    length(namesforvalues) == length(valuesfornames) || throw(ErrorException("lengths must match"))
     return (; zip(namesforvalues, valuesfornames)...,)
end

namedtuple(namesforvalues::Vector{S}, valuesfornames) where {N,S<:AbstractString} =
    namedtuple(Symbol.(namesforvalues), valuesfornames)

namedtuple(namesforvalues::NTuple{N,S}, valuesfornames) where {N,S<:AbstractString} =
    namedtuple(Symbol.(namesforvalues), valuesfornames)


"""
    namedtuple(  name1, name2, ..  )
    namedtuple( (name1, name2, ..) )
    namedtuple(  namedtuple )

Generate a NamedTuple prototype by specifying or obtaining the fieldnames.
The prototype is applied to fieldvalues, giving a completed NamedTuple.

# Example

julia> ntprototype = namedtuple( :a, :b, :c )

NamedTuple{(:a, :b, :c),T} where T<:Tuple

julia> nt123 = ntprototype(1, 2, 3)

(a = 1, b = 2, c = 3)

julia> ntAb3 = ntprototype("A", "b", 3)

(a = "A", b = "b", c = 3)

see: [`isprototype`](@ref)
"""
namedtuple(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
namedtuple(names::Vararg{Symbol}) = NamedTuple{names}
namedtuple(names::NTuple{N,String}) where {N}  = namedtuple(Symbol.(names))
namedtuple(names::Vararg{String}) = namedtuple(Symbol.(names))
namedtuple(names::T) where {T<:AbstractVector{Symbol}} = namedtuple(names...,)
namedtuple(names::T) where {T<:AbstractVector{String}} = namedtuple(Symbol.(names))

namedtuple(nt::T) where {N,V,T<:NamedTuple{N,V}} = NamedTuple{N}

"""
    prototype(namedtuple)
    prototype(typeof(namedtuple))

provides the prototype `NamedTuple{names, T} where T<:Tuple`
    - `names` is a tuple of symbols
"""
prototype(::NamedTuple{A,B}) where {A,B} = NamedTuple{A}
prototype(::Type{NamedTuple{A,B}}) where {A,B} = NamedTuple{A}

"""
    isprototype( ntprototype )
    isprototype( namedtuple  )

Predicate that identifies NamedTuple prototypes.

see: [`namedtuple`](@ref)
"""
isprototype(::Type{T}) where {T<:NamedTuple} = eltype(T) === Any
isprototype(nt::T) where {T<:NamedTuple} = false
isprototype(::Type{UnionAll}) = false

"""
   delete(namedtuple, symbol(s)|Tuple)
   delete(ntprototype, symbol(s)|Tuple)

Generate a namedtuple [ntprototype] from the first arg omitting fields present in the second arg.

see: [`merge`](@ref)
"""
delete(a::NamedTuple, b::Symbol) = Base.structdiff(a, namedtuple(b))
delete(a::NamedTuple, b::NTuple{N,Symbol}) where {N} = Base.structdiff(a, namedtuple(b))
delete(a::NamedTuple, bs::Vararg{Symbol}) = Base.structdiff(a, namedtuple(bs))

delete(::Type{T}, b::Symbol) where {S,T<:NamedTuple{S}} = namedtuple((Base.setdiff(S,(b,))...,))
delete(::Type{T}, b::NTuple{N,Symbol}) where {S,N,T<:NamedTuple{S}} = namedtuple((Base.setdiff(S,b)...,))
delete(::Type{T}, bs::Vararg{Symbol}) where {S,N,T<:NamedTuple{S}} = namedtuple((Base.setdiff(S,bs)...,))

"""
   select(namedtuple, symbol(s)|Tuple)
   select(ntprototype, symbol(s)|Tuple)

Generate a namedtuple [ntprototype] from the first arg, including only fields present in the second arg.

see: [`merge`](@ref)
"""
select(nt::NamedTuple, k::Symbol) = nt[k]
select(nt::NamedTuple, k::NamedTuple) = select(nt, keys(k))
select(nt::NamedTuple, ks) = namedtuple(ks)(((nt[k] for k in ks)...,))


"""
    merge(namedtuple1, namedtuple2)
    merge(nt1, nt2, nt3, ..)

Generate a namedtuple with all fieldnames and values of namedtuple2
    and every fieldname of namedtuple1 that does not occur in namedtuple2
    along with their values.

see: [`delete!`](@ref)
"""
merge(::Type{T1}, ::Type{T2}) where {N1,N2,T1<:NamedTuple{N1},T2<:NamedTuple{N2}} =
    namedtuple((unique((N1..., N2...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}) where {N1,N2,N3,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3}} =
    namedtuple((unique((N1..., N2..., N3...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}) where {N1,N2,N3,N4,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4}} =
    namedtuple((unique((N1..., N2..., N3..., N4...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}) where {N1,N2,N3,N4,N5,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}, ::Type{T6}) where {N1,N2,N3,N4,N5,N6,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5},T6<:NamedTuple{N6}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5..., N6...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}, ::Type{T6}, ::Type{T7}) where {N1,N2,N3,N4,N5,N6,N7,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5},T6<:NamedTuple{N6},T7<:NamedTuple{N7}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5..., N6...,N7...))...,))

# merge(nt1::T1, nt2::T2) where {T1<:NamedTuple, T2<:NamedTuple} is already defined

merge(a::NamedTuple{an}, b::NamedTuple{bn}, c::NamedTuple{cn}) where {an, bn, cn} =
    reduce(merge,(a, b, c))
merge(a::NamedTuple{an}, b::NamedTuple{bn}, c::NamedTuple{cn}, d::NamedTuple{dn}) where {an, bn, cn, dn} =
    reduce(merge,(a, b, c, d))
merge(a::NamedTuple{an}, b::NamedTuple{bn}, c::NamedTuple{cn}, d::NamedTuple{dn}, e::NamedTuple{en}) where {an, bn, cn, dn, en} =
    reduce(merge,(a, b, c, d, e))
merge(a::NamedTuple{an}, b::NamedTuple{bn}, c::NamedTuple{cn}, d::NamedTuple{dn}, e::NamedTuple{en}, f::NamedTuple{fn}) where {an, bn, cn, dn, en, fn} =
    reduce(merge,(a, b, c, d, e, f))
merge(a::NamedTuple{an}, b::NamedTuple{bn}, c::NamedTuple{cn}, d::NamedTuple{dn}, e::NamedTuple{en}, f::NamedTuple{fn}, g::NamedTuple{gn}) where {an, bn, cn, dn, en, fn, gn} =
    reduce(merge,(a, b, c, d, e, f, g))


"""
    merge_recursive(nt1, nt2)
    merge_recursive(nt1, nt2, nt3, ..)

Recursively merge namedtuples. Where more than one of the namedtuple args share the same fieldname (same key),
    the leftmost argument's key's value will be propogated. Where each namedtuple has distinct fieldnames (keys),
    all of named fields will be gathered with their respective values. The named fields will appear in the same
    order they are encountered (leftmost arg, second leftmost arg, .., second rightmost arg, rightmost arg).

If there are no nested namedtuples, `merge(nt1, nts..., recursive=true)` is the same as `merge(nt1, nts...)`.
```
a = (food = (fruits = (orange = "mango", white = "pear"),
             liquids = (water = "still", wine = "burgandy")))

b = (food = (fruits = (yellow = "banana", orange = "papaya"),
             liquids = (water = "sparkling", wine = "champagne"),
             bread = "multigrain"))

merge(b,a)  == (fruits  = (orange = "mango", white = "pear"),
                liquids = (water = "still", wine = "burgandy"),
                bread   = "multigrain")

merge_recursive(b,a) ==
               (fruits  = (yellow = "banana", orange = "mango", white = "pear"),
                liquids = (water = "still", wine = "burgandy"),
                bread   = "multigrain")

merge(a,b)  == (fruits  = (yellow = "banana", orange = "papaya"),
                liquids = (water = "sparkling", wine = "champagne"),
                bread   = "multigrain")

merge_recursive(a,b) ==
               (fruits  = (orange = "papaya", white = "pear", yellow = "banana"),
                liquids = (water = "sparkling", wine = "champagne"),
                bread   = "multigrain")
```
see: [`merge`](@ref)
""" merge_recursive

#=
anonymous placeholder for unvalued namedtuple keys
(only used in recursion definitions)
=#
struct _Unvalued end
const _unvalued = _Unvalued()

merge_recursive(nt::NamedTuple) = nt

merge_recursive(::_Unvalued, ::_Unvalued) = _unvalued
merge_recursive(x, ::_Unvalued) = x
merge_recursive(m::_Unvalued, x) = merge_recursive(x, m)
merge_recursive(x, y) = y

function merge_recursive(nt1::NamedTuple, nt2::NamedTuple)
    all_keys = union(keys(nt1), keys(nt2))
    gen = Base.Generator(all_keys) do key
        v1 = get(nt1, key, unvalued)
        v2 = get(nt2, key, unvalued)
        key => merge_recursive(v1, v2)
    end
    return (; gen...)
end

merge_recursive(nt1::NamedTuple, nt2::NamedTuple, nts...) =
    merge_recursive(merge_recursive(nt1, nt2), nts...)

"""
    split(namedtuple, symbol(s)|Tuple)

Generate two namedtuples, the first with only the fields in the second arg, the
second with all but the fields in the second arg, such that
`merge(split(nt, ks)...) == nt` when `ks` contains the first fields of `nt`.
"""
split(nt::NamedTuple, ks::Symbol) = split(nt, (ks,))
split(nt::NamedTuple, ks) = select(nt, ks), delete(nt, ks)

#=  interconvert: NamedTuple <--> Dict =#

uniontype(nt::NamedTuple) = Union{typeof.(values(nt))...,}

"""
    gather_(x::Iterable)

Collect the elements of x into a Tuple, in their iterated order.
"""
@inline gather_(x::T) where {T} = (collect(x)...,)

namedtuple(d::T) where {T<:AbstractDict{Symbol,V}} where {V} =
    NamedTuple{gather_(keys(d)), NTuple{length(d), V}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{S,V}} where {S<:AbstractString, V} =
    NamedTuple{Symbol.(gather_(keys(d))), NTuple{length(d), V}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{Symbol,Any}} =
    NamedTuple{gather_(keys(d)), Tuple{typeof.(values(d))...}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{S,Any}} where {S<:AbstractString} =
    NamedTuple{Symbol.(gather_(keys(d))), Tuple{typeof.(values(d))...}}(gather_(values(d)))

# use: dict = convert(Dict, nt)
#=
   for Dict{Symbol,Any}:
   Base.convert(::Type{Dict}, x::NT) where {N, NT<:NamedTuple{N}} =
       Dict([sym=>val for (sym,val) in zip(fieldnames(x), fieldvalues(x))])
=#
Base.convert(::Type{D}, x::NT) where {D<:AbstractDict, N, NT<:NamedTuple{N}} =
    D{Symbol, uniontype(x)}([sym=>val for (sym,val) in zip(fieldnames(x), fieldvalues(x))])

dictionary(nt::NamedTuple) = convert(Dict, nt) # deprecated

# from PR by pdeffebach (Vector of Pairs becomes NamedTuple)
function namedtuple(v::Vector{<:Pair{<:Symbol}})
    N = length(v)
    NamedTuple{ntuple(i -> v[i][1], N)}(ntuple(i -> v[i][2], N))
end
# with names as strings
function namedtuple(v::Vector{<:Pair{String}})
    N = length(v)
    NamedTuple{ntuple(i -> Symbol(v[i][1]), N)}(ntuple(i -> v[i][2], N))
end
# NamedTuple becomes a Vector of Pairs
Base.convert(::Type{Vector{Pair}}, nt::NamedTuple) =  map(kv->Pair(first(kv), last(kv)), zip(keys(nt), values(nt)))

# from Sebastian Pfitzner (on Slack)
macro namedtuple(vars...)
   args = Any[]
   for v in vars
       if Meta.isexpr(v, :(=)) || Meta.isexpr(v, :...)
           push!(args, esc(v))
       else
           push!(args, Expr(:(=), esc(v), esc(v)))
       end
   end
   expr = Expr(:tuple, Expr(:parameters, args...))
   return expr
end

end # module NamedTupleTools
