"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

@ref(ntproto), @ref(ntnames), @ref(ntvalues)
"""
module NamedTupleTools

export ntproto, ntnames, ntvalues

"""
    ntproto(  name1, name2, ..  )
    ntproto( (name1, name2, ..) )

Generate a NamedTuple prototype by specifying the field names.
The prototype is applied to field values, giving a completed NamedTuple.
"""
ntproto(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
ntproto(names::Vararg{Symbol}) = NamedTuple{names}

"""
    ntnames( ntprototype )
    ntnames( namedtuple  )

Retrieve the names as a tuple of symbols.
"""
ntnames(::Type{T}) where {T<:NamedTuple} = Base._nt_names(T)
ntnames(nt::T) where {T<:NamedTuple} = Base._nt_names(T)

"""
    ntvalues( namedtuple )

Retrieve the values as a tuple.
"""
ntvalues(::Type{T}) where {T<:NamedTuple} = ()
ntvalues(nt::T) where {T<:NamedTuple} = Base.values(nt)

end # module NamedTupleTools
