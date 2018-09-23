"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

see [`tuplenames`](@ref), [`isprototype`](@ref), [`fieldname`](@ref), [`values`](@ref)
    [`valuetype`](@ref), [`valtypes`](@ref), [`delete!`](@ref), [`merge`](@ref)
"""
module NamedTupleTools

export tuplenames, valtypes, isprototype

import Base: length, fieldnames, keys, values, delete!, merge, valtype

# accept comma delimited values
Base.NamedTuple{T}(xs...) where {T} = NamedTuple{T}(xs)

length(::Type{T}) where {T<:Tuple} = length(T.parameters)
length(::Type{T}) where {T<:NamedTuple} = length(T.parameters[1])
length(::Type{T}) where {N,T<:NamedTuple{N}} = length(N)

"""
    tuplenames(  name1, name2, ..  )
    tuplenames( (name1, name2, ..) )
    tuplenames(  namedtuple )

Generate a NamedTuple prototype by specifying or obtaining the fieldnames.
The prototype is applied to fieldvalues, giving a completed NamedTuple.

see: [`isprototype`](@ref)
"""
tuplenames(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
tuplenames(names::Vararg{Symbol}) = NamedTuple{names}
tuplenames(names::NTuple{N,String}) where {N}  = tuplenames(Symbol.(names))
tuplenames(names::Vararg{String}) = tuplenames(Symbol.(names))
tuplenames(nt::T) where {T<:NamedTuple} = tuplenames(fieldnames(nt))

"""
    fieldname( ntprototype, index )
    fieldname( typeof(namedtuple), index)

Retrieve the name of the indexed field (a symbol).
"""
fieldname(::Type{T}, i::Integer) where {T<:NamedTuple} = fieldnames(T)[i]

"""
    values( namedtuple )

Retrieve the values as a tuple.

see: [`fieldnames`](@ref) or [`keys`](@ref)
"""
values(::Type{T}) where {T<:NamedTuple} = ()
# values(nt::NamedTuple) is already defined

"""
    valtype( namedtuple )

Retrieve the values' types as a typeof(tuple).
"""
valtype(x::T) where {N,S, T<:NamedTuple{N,S}} = T.parameters[2]
valtype(::Type{T}) where {N, S<:Tuple, T<:Union{NamedTuple{N},NamedTuple{N,S}}} =
    typeof(T) === UnionAll ? NTuple{length(N),Any} : T.parameters[2]

"""
    valtypes( namedtuple )
    valtypes( typeof(namedtuple) )

Retrieve the values' types as a tuple.
"""
valtypes(x::T) where {N,S, T<:NamedTuple{N,S}} = Tuple(T.parameters[2].parameters)
valtypes(::Type{T}) where {N, S<:Tuple, T<:Union{NamedTuple{N},NamedTuple{N,S}}} =
       typeof(T) === UnionAll ? Tuple((NTuple{length(N),Any}).parameters) :
                                Tuple(T.parameters[2].parameters)

"""
    isprototype( ntprototype )
    isprototype( namedtuple  )

Predicate that identifies NamedTuple prototypes.

see: [`tuplenames`](@ref)
"""
isprototype(::Type{T}) where {T<:NamedTuple} = eltype(T) === Any
isprototype(nt::T) where {T<:NamedTuple} = false
isprototype(::Type{UnionAll}) = false

"""
   delete!(namedtuple, symbol(s)|Tuple)
   delete!(ntprototype, symbol(s)|Tuple)
   
Generate a namedtuple [ntprototype] from the first arg omitting fields present in the second arg.

see: [`merge`](@ref)
"""
delete!(a::NamedTuple, b::Symbol) = Base.structdiff(a, tuplenames(b))
delete!(a::NamedTuple, b::NTuple{N,Symbol}) where {N} = Base.structdiff(a, tuplenames(b))
delete!(a::NamedTuple, bs::Vararg{Symbol}) = Base.structdiff(a, tuplenames(bs))

delete!(::Type{T}, b::Symbol) where {S,T<:NamedTuple{S}} = tuplenames((Base.setdiff(S,(b,))...,))
delete!(::Type{T}, b::NTuple{N,Symbol}) where {S,N,T<:NamedTuple{S}} = tuplenames((Base.setdiff(S,b)...,))
delete!(::Type{T}, bs::Vararg{Symbol}) where {S,N,T<:NamedTuple{S}} = tuplenames((Base.setdiff(S,bs)...,))

"""
    merge(namedtuple1, namedtuple2)

Generate a namedtuple with all fieldnames and values of namedtuple2
    and every fieldname of namedtuple1 that does not occur in namedtuple2
    along with their values.

see: [`delete!`](@ref)
"""
merge(::Type{T1}, ::Type{T2}) where {N1,N2,T1<:NamedTuple{N1},T2<:NamedTuple{N2}} =
    tuplenames((unique((N1..., N2...,))...,))
# merge(nt1::T1, nt2::T2) where {T1<:NamedTuple, T2<:NamedTuple} is already defined

end # module NamedTupleTools
