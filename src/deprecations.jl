 @eval Base begin
     @deprecate valtype(nt::$NamedTuple) $eltype(nt::$NamedTuple)
     @deprecate namedtuple(nt::$NamedTuple) $prototype(nt::$NamedTuple)
 end
