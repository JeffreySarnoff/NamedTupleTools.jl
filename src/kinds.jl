#=

    On The Kinds of NamedTuple

=#


thesenames = (:a, :b, :c)
thesevalues = ('a', 'b', 'c')
prototype_nt = NamedTuple{thesenames}
realized_nt = prototype_nt(thesevalues)

whatami(prototype_nt) = :prototype_nt
whatami(realized_nt) = :realized_nt

@inline whatami(ako_nt) = isa(ako_nt, Type)

isa(prototype_nt,Type) == true
isa(realized_nt,Type) == false

akoNamedTuple(x) = isa(Base._nt_names(realized_nt), Tuple)

isa( realized_nt, NamedTuple ) == true
isa( prototype_nt, NamedTuple ) == false

akoNamedTuple(x) = false
akoNamedTuple(::Type{NamedTuple{T}}) where T = true
akoNamedTuple(x::NamedTuple{T}) where T = true
