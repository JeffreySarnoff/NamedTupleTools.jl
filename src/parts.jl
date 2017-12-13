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
    NT_values(namedtuple)

Obtains the values that `namedtuple` labels with the names.

#Example

nt = NamedTuple{(:first, :second)}(("initial", "final"))
NT_values(nt) == ("initial", "final")

"""
NT_values(namedtuple::T) where T<:NamedTuple = Base.values(namedtuple)
