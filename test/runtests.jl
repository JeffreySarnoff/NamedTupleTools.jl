using NamedTupleTools
using Test

ntproto1 = namedtuple(:a, :b, :c, :d)
ntproto2 = namedtuple(:a, :b)

@test ntproto1 == NamedTuple{(:a, :b, :c, :d),T} where T<:Tuple

nt1 = ntproto1(1, 2, 3, 4)
nt2 = ntproto2("one", "two")

@test isprototype(ntproto1) === true
@test isprototype(nt1) === false

@test nt2 === (a = "one", b = "two")
@test ntproto2(nt1) === (a = 1, b = 2)

@test delete!(ntproto1, :a) === NamedTuple{(:b, :c, :d),T} where T<:Tuple
@test delete!(ntproto1, :a, :c) === NamedTuple{(:b, :d),T} where T<:Tuple
@test delete!(ntproto1, (:b, :c)) === NamedTuple{(:a, :d),T} where T<:Tuple

@test merge(nt1, nt2) === (a = "one", b  = "two", c = 3, d = 4)
