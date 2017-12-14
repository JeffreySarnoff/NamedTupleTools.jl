#=
    On The Kinds of NamedTuple
=#

isako(x, y) = Base.is(x, y)

const KindsOfNamedTuples = Union{NamedTuple, NamedTuple{N}} where N
const AbstractNamedTuple = Val{:AbstractNamedTuple}
const RealizedNamedTuple = Val{:RealizedNamedTuple}

isako(x, NamedTuple) = x <: KindsOfNamedTuples

isako(::Type{NamedTuple{T}}, ::Type{AbstractNamedTuple}) = true
isako(::Type{NamedTuple{T}}, ::Type{RealizedNamedTuple}) = false
isako(x::NamedTuple{T}, ::Type{RealizedNamedTuple}) where T = true
isako(x::NamedTuple{T}, ::Type{AbstractNamedTuple}) where T = false
