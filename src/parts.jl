#=

    NamedTuples as the Parts thereof

=#


"""
    NT_names(namedtuple)

Obtains the names with which `namedtuple` labels values.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
NT_names(nt) == (:first, :second)

"""
NT_names(namedtuple::T) where T<:NamedTuple = Base._nt_names(namedtuple)

"""
    NT_names(namedtuple, ordinal)
    NT_names(namedtuple, unitrange)
    NT_names(namedtuple, ordinals)

Obtains one or more symbols from the full sequence of names
"""
NT_names(namedtuple, idx::I) where I<:SIUI = NT_names(namedtuple)[idx]
NT_names(namedtuple, idxs::R) where R<:UnitRange = NT_names(namedtuple)[idxs]
NT_names(namedtuple, idxs::T) where T<:Tuple = NT_names(namedtuple)[idxs...,]


"""
    NT_values(namedtuple)

Obtains the values that `namedtuple` labels with the names.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
NT_values(nt) == ("initial", "final")

"""
NT_values(namedtuple::T) where T<:NamedTuple = Base.values(namedtuple)

"""
    NT_values(namedtuple, ordinal)
    NT_values(namedtuple, unitrange)
    NT_values(namedtuple, ordinals)

Obtains one or more realizations from the full sequence of values
"""
NT_values(namedtuple, idx::I) where I<:SIUI = NT_values(namedtuple)[idx]
NT_values(namedtuple, idxs::R) where R<:UnitRange = NT_values(namedtuple)[idxs]
NT_values(namedtuple, idxs::T) where T<:Tuple = NT_values(namedtuple)[idxs...,]

"""
    NT_namesvalues(namedtuple)

Obtains the names and the values with which `namedtuple` is realized.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
NT_namesvalues(nt) == ((:first, :second), ("initial", "final"))

"""
NT_namesvalues(namedtuple::T) where T<:NamedTuple = (names(namedtuple), values(namedtuple))

"""
    NT_namesvalues(namedtuple, ordinal)
    NT_namesvalues(namedtuple, unitrange)
    NT_namesvalues(namedtuple, ordinals)

Obtains one or more of the names and values with which `namedtuple` is realized.
"""
NT_namesvalues(namedtuple, idx::I) where I<:SIUI = (NT_names(namedtuple)[idx], NT_values(namedtuple)[idx])
NT_namesvalues(namedtuple, idxs::R) where R<:UnitRange = (NT_names(namedtuple)[idxs], NT_values(namedtuple)[idxs])
NT_namesvalues(namedtuple, idxs::T) where T<:Tuple = (NT_names(namedtuple)[idxs...,], NT_values(namedtuple)[idxs...,])

