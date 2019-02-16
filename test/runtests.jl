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

@test delete(ntproto1, :a) === NamedTuple{(:b, :c, :d),T} where T<:Tuple
@test delete(ntproto1, :a, :c) === NamedTuple{(:b, :d),T} where T<:Tuple
@test delete(ntproto1, (:b, :c)) === NamedTuple{(:a, :d),T} where T<:Tuple

@test merge(nt1, nt2) === (a = "one", b  = "two", c = 3, d = 4)

struct MyStruct
    tally::Int
    team::String
end

mystruct = MyStruct(5, "hometeam")
mynamedtuple = ntfromstruct(mystruct)

@test mynamedtuple == (tally = 5, team = "hometeam")

nt = (tally=5, team="hometeam")
ntstruct = structfromnt(MyStruct, nt)

@test ntstruct == mystruct

nt = NamedTuple{(:a, :b)}(1, 2)

v = [:a => 1, :b => 2]
@test namedtuple(v) == NamedTuple{(:a, :b)}(1, 2)

v = ["a" => 1, "b" => 2]
@test namedtuple(v) == NamedTuple{("a", "b")}(1, 2)


dict = Dict(:a=>1, :b=>2//11, :c=>"three")
nt = NamedTuple(dict)

@test Dict(nt) == dict
@test namedtuple(dict) == nt

vec_pair = [:a => 1, :b => 2]
nt = (a = 1, b = 2)
@test namedtuple(vec_pair) == nt

a = 1; b = 2;
@test @namedtuple(a, b) == nt
