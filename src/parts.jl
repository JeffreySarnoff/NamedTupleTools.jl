#=

    NamedTuples as the Parts thereof

=#


"""
    nt_names(namedtuple)

Obtains the names with which `namedtuple` labels values.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
nt_names(nt) == (:first, :second)

"""
nt_names(nt::T) where {T<:NamedTuple} = Base._nt_names(nt)
nt_names(::Type{T}) where {T<:NamedTuple} = Base._nt_names(T)

"""
    nt_names(namedtuple, ordinal)
    nt_names(namedtuple, unitrange)
    nt_names(namedtuple, ordinals)

Obtains one or more symbols from the full sequence of names
"""
nt_names(namedtuple, idx::I) where I<:SIUI = nt_names(namedtuple)[idx]
nt_names(namedtuple, idxs::R) where R<:UnitRange = nt_names(namedtuple)[idxs]
nt_names(namedtuple, idxs::T) where T<:Tuple = nt_names(namedtuple)[idxs...,]


"""
    nt_values(namedtuple)

Obtains the values that `namedtuple` labels with the names.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
nt_values(nt) == ("initial", "final")

"""
nt_values(nt::T) where {T<:NamedTuple} = Base.values(nt)
nt_values(::Type{T}) where {T<:NamedTuple} = ()

"""
    nt_values(namedtuple, ordinal)
    nt_values(namedtuple, unitrange)
    nt_values(namedtuple, ordinals)

Obtains one or more realizations from the full sequence of values
"""
nt_values(namedtuple, idx::I) where I<:SIUI = nt_values(namedtuple)[idx]
nt_values(namedtuple, idxs::R) where R<:UnitRange = nt_values(namedtuple)[idxs]
nt_values(namedtuple, idxs::T) where T<:Tuple = nt_values(namedtuple)[idxs...,]

"""
    NT_namesvalues(namedtuple)

Obtains the names and the values with which `namedtuple` is realized.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
nt_namesvalues(nt) == ((:first, :second), ("initial", "final"))

"""
nt_namesvalues(namedtuple::T) where T<:NamedTuple = (names(namedtuple), values(namedtuple))

"""
    nt_namesvalues(namedtuple, ordinal)
    nt_namesvalues(namedtuple, unitrange)
    nt_namesvalues(namedtuple, ordinals)

Obtains one or more of the names and values with which `namedtuple` is realized.
"""
nt_namesvalues(namedtuple, idx::I) where I<:SIUI = (nt_names(namedtuple)[idx], nt_values(namedtuple)[idx])
nt_namesvalues(namedtuple, idxs::R) where R<:UnitRange = (nt_names(namedtuple)[idxs], nt_values(namedtuple)[idxs])
nt_namesvalues(namedtuple, idxs::T) where T<:Tuple = (nt_names(namedtuple)[idxs...,], nt_values(namedtuple)[idxs...,])

