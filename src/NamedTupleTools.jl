"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

see [`tuplenames`](@ref), [`isprototype`](@ref), [`fieldname`](@ref), [`values`](@ref)
    [`valuetype`](@ref), [`valtypes`](@ref), [`delete!`](@ref), [`merge`](@ref)
"""
module NamedTupleTools

export namedtuple, valtypes, isprototype

import Base: length, fieldnames, keys, values, delete!, merge, valtype

# accept comma delimited values
Base.NamedTuple{T}(xs...) where {T} = NamedTuple{T}(xs)

length(::Type{T}) where {T<:Tuple} = length(T.parameters)
length(::Type{T}) where {T<:NamedTuple} = length(T.parameters[1])
length(::Type{T}) where {N,T<:NamedTuple{N}} = length(N)

"""
    namedtuple(  name1, name2, ..  )
    namedtuple( (name1, name2, ..) )
    namedtuple(  namedtuple )

Generate a NamedTuple prototype by specifying or obtaining the fieldnames.
The prototype is applied to fieldvalues, giving a completed NamedTuple.

see: [`isprototype`](@ref)
"""
namedtuple(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
namedtuple(names::Vararg{Symbol}) = NamedTuple{names}
namedtuple(names::NTuple{N,String}) where {N}  = namedtuple(Symbol.(names))
namedtuple(names::Vararg{String}) = namedtuple(Symbol.(names))
namedtuple(nt::T) where {N,V,T<:NamedTuple{N,V}} = NamedTuple{N}
# for speed
namedtuple(nm1::T) where T<:Symbol = NamedTuple{(nm1,)}
namedtuple(nm1::T, nm2::T) where T<:Symbol = NamedTuple{(nm1,nm2)}
namedtuple(nm1::T, nm2::T, nm3::T) where T<:Symbol = NamedTuple{(nm1,nm2,nm3)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T) where T<:Symbol = NamedTuple{(nm1,nm2,nm3,nm4)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T, nm6::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5,nm6)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T, nm6::T, nm7::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5,nm6,nm7)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T, nm6::T, nm7::T, nm8::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5,nm6,nm7,nm8)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T, nm6::T, nm7::T, nm8::T, nm9::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9)}
namedtuple(nm1::T, nm2::T, nm3::T, nm4::T, nm5::T, nm6::T, nm7::T, nm8::T, nm9::T, nm10::T) where T<:Symbol =
    NamedTuple{(nm1,nm2,nm3,nm4,nm5,nm6,nm7,nm8,nm9,nm10)}



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

see: [`valtypes`](@ref)
"""
valtype(x::T) where {N,S, T<:NamedTuple{N,S}} = T.parameters[2]
valtype(::Type{T}) where {N, S<:Tuple, T<:Union{NamedTuple{N},NamedTuple{N,S}}} =
    typeof(T) === UnionAll ? NTuple{length(N),Any} : T.parameters[2]

"""
    valtypes( namedtuple )
    valtypes( typeof(namedtuple) )

Retrieve the values' types as a tuple.

see: [`valtype`](@ref)
"""
valtypes(x::T) where {N,S, T<:NamedTuple{N,S}} = Tuple(T.parameters[2].parameters)
valtypes(::Type{T}) where {N, S<:Tuple, T<:Union{NamedTuple{N},NamedTuple{N,S}}} =
       typeof(T) === UnionAll ? Tuple((NTuple{length(N),Any}).parameters) :
                                Tuple(T.parameters[2].parameters)

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
