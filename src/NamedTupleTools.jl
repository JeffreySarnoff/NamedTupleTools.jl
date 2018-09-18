"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

@ref(tuplenames), @ref(fieldnames), @ref(keys), @ref(values), @ref(remove)
"""
module NamedTupleTools

export tuplenames, remove, isprototype

import Base: fieldnames, keys, values

# accept comma delimited values
Base.NamedTuple{T}(xs...) where {T} = NamedTuple{T}(xs)

"""
    tuplenames(  name1, name2, ..  )
    tuplenames( (name1, name2, ..) )

Generate a NamedTuple prototype by specifying the field names.
The prototype is applied to field values, giving a completed NamedTuple.
"""
tuplenames(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
tuplenames(names::Vararg{Symbol}) = NamedTuple{names}

"""
    fieldnames( ntprototype )
    fieldnames( namedtuple  )

Retrieve the names as a tuple of symbols.
"""
fieldnames(::Type{T}) where {T<:NamedTuple} = Base._nt_names(T)
fieldnames(nt::T) where {T<:NamedTuple} = Base._nt_names(T)

"""
    keys( ntprototype )
    keys( namedtuple  )

Retrieve the names as a tuple of symbols.
"""
keys(::Type{T}) where {T<:NamedTuple} = Base._nt_names(T)
# keys(namedtuple) already defined

"""
    values( namedtuple )

Retrieve the values as a tuple.
"""
values(::Type{T}) where {T<:NamedTuple} = ()
# values(nt::NamedTuple) is already defined

"""
    isprototype( ntprototype )
    isprototype( namedtuple  )

Predicate that identifies NamedTuple prototypes.
"""
isprototype(::Type{T}) where {T<:NamedTuple} = true
isprototype(nt::T) where {T<:NamedTuple} = false

"""
   remove(namedtuple, namedtuple2)
   remove(namedtuple, symbol)
   remove(namedtuple, (symbols))
   remove(namedtuple, symbols...)
   remove(ntprototype, ntprototype2)

Generate a namedtuple [ntprototype] from the first arg omitting fields present in the second arg.
"""
remove(a::NamedTuple, b::NamedTuple) = Base.structdiff(a,b)
remove(a::NamedTuple, b::Symbol) = Base.structdiff(a, tuplenames(b))
remove(a::NamedTuple, b::NTuple{N,Symbol}) where {N} = Base.structdiff(a, tuplenames(b))
remove(a::NamedTuple, bs::Vararg{Symbol}) = Base.structdiff(a, tuplenames(bs))

remove(::Type{T1}, ::Type{T2}) where {N1,N2,T1<:NamedTuple{N1},T2<:NamedTuple{N2}} =
    tuplenames((Base.symdiff(N1,N2)...,))

#merge(a::NamedTuple,b::NamedTuple)

end # module NamedTupleTools
