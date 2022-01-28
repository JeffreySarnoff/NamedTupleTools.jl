# Mason Protter  2 days ago
# Here's my version with updated semantics to match Base.structdiff more closely, and vararg support:
Keys(::Type{<:NamedTuple{names}}) where {names} = names
Keys(::Type{Type{T}}) where {T} = Keys(T)

@generated function structintersect(a::NamedTuple{an}, rest::Union{NamedTuple, Type{<:NamedTuple}}...) where {an}
    names = Tuple(intersect(an, Keys.(rest)...))
    data  = Expr(:tuple, (:(getproperty(a, $(QuoteNode(name)))) for name in names)...)
    :(NamedTuple{$names}($data))
end

# julia> structintersect((;a=1, b=2, c=3, d=4), (;b=1, c=3, d=4), NamedTuple{(:b, :d)})
# (b = 2, d = 4)
