"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

@ref(tuplenames), @ref(fieldnames), @ref(values)
"""
module NamedTupleTools

export tuplenames

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
Base.fieldnames(::Type{T}) where {T<:NamedTuple} = Base._nt_names(T)
Base.fieldnames(nt::T) where {T<:NamedTuple} = Base._nt_names(T)

"""
    values( namedtuple )

Retrieve the values as a tuple.
"""
Base.values(::Type{T}) where {T<:NamedTuple} = ()
Base.values(nt::T) where {T<:NamedTuple} = Base.values(nt)

end # module NamedTupleTools
