"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

@ref(tuplenames), @ref(fieldnames), @ref(keys), @ref(values)
"""
module NamedTupleTools

export tuplenames, isprototype

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

end # module NamedTupleTools
