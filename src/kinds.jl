#=
    On The Kinds of NamedTuple
=#

isako(x, y) = Base.is(x, y)

const KindsOfNamedTuples = Union{NamedTuple, NamedTuple{N}} where N

isako(x, NamedTuple) = x <: KindsOfNamedTuples
isako(x, ::Type{NamedTuple{T}}) where T = isa(x, NamedTuple) & isa(x, Type)
isako(x::NamedTuple{T}, ::TypeaVBNamedTuple}) where T = isa(x, Type)
isako(x::NamedTuple{T}, ::Type{RealizedNamedTuple}) where T = isa(x, Type)

isako(::Type{NamedTuple{T}}, ::Type{AbstractNamedTuple}) where T = true
isako(::Type{NamedTuple{T}}, ::Type{RealizedNamedTuple}) where T = false
isako(x::NamedTuple{T}, ::Type{RealizedNamedTuple}) where T = true
isako(x::NamedTuple{T}, ::Type{AbstractNamedTuple}) where T = false
