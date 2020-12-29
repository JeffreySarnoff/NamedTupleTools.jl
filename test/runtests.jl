using NamedTupleTools
using Test
using OrderedCollections: OrderedDict, LittleDict

namesofvalues  = (:instrument, :madeby)
matchingvalues = ("violin", "Stradivarius")
nt = namedtuple(namesofvalues, matchingvalues)
@test nt == (instrument = "violin", madeby = "Stradivarius")

namesofvalues  = [:instrument, :madeby]
matchingvalues = ["violin", "Stradivarius"]
nt = namedtuple(namesofvalues, matchingvalues)
@test nt == (instrument = "violin", madeby = "Stradivarius")

namesofvalues  = (:instrument, :madeby)
matchingvalues = ["violin", "Stradivarius"]
nt = namedtuple(namesofvalues, matchingvalues)
@test nt == (instrument = "violin", madeby = "Stradivarius")

namesofvalues  = [:instrument, :madeby]
matchingvalues = ("violin", "Stradivarius")
nt = namedtuple(namesofvalues, matchingvalues)
@test nt == (instrument = "violin", madeby = "Stradivarius")


nt = NamedTuple{(:a, :b)}(1.0, "two")

@test fieldnames(nt) == (:a, :b)
@test fieldtypes(nt) == (Float64, String)
@test NamedTupleTools.field_types(nt) == Tuple{Float64, String}

namedtuple(:a, :b) == NamedTuple{(:a, :b),T} where T<:Tuple

ntproto1 = namedtuple(:a, :b, :c, :d)
ntproto2 = namedtuple(:a, :b)

@test ntproto1 == NamedTuple{(:a, :b, :c, :d),T} where T<:Tuple

nt1 = ntproto1(1, 2, 3, 4)
nt2 = ntproto2("one", "two")

ntnt1 = (x = (a = 1, b = 2, c = 3, d = 4), y = (a = "one", b = "two", c = "three"))
ntnt2 = (; y = (a = 1.0, b = "two"))
ntnt3 = (; x = (; a = :one))

proto1 = prototype(nt1)
proto2 = prototype(nt2)

@test isprototype(ntproto1) === true
@test isprototype(nt1) === false
@test isprototype(UnionAll) === false
@test isprototype(proto1) === true
@test isprototype(proto2) === true

@test nt2 === (a = "one", b = "two")
@test ntproto2(nt1) === (a = 1, b = 2)

@test delete(ntproto1, :a) === NamedTuple{(:b, :c, :d),T} where T<:Tuple
@test delete(ntproto1, :a, :c) === NamedTuple{(:b, :d),T} where T<:Tuple
@test delete(ntproto1, (:b, :c)) === NamedTuple{(:a, :d),T} where T<:Tuple

@test delete(nt1, :a) == (b = 2, c = 3, d = 4)
@test delete(nt1, :a, :c) == (b = 2, d = 4)
@test delete(nt1, (:a, :b, :c)) === (d = 4,)

@test merge(nt1, nt2) === (a = "one", b  = "two", c = 3, d = 4)
@test merge(ntnt1, ntnt2) == (
    x = (a = 1, b = 2, c = 3, d = 4),
    y = (a = 1.0, b = "two")
)
@test merge(ntnt1, ntnt2, ntnt3) == (
    x = (a = :one,),
    y = (a = 1.0, b = "two")
)

@test merge_recursive(nt1) == nt1
@test merge_recursive(nt1, nt2) === merge(nt1, nt2)
@test merge_recursive(nt1, nt2, nt1) == merge(nt1, nt2, nt1)
@test merge_recursive(ntnt1) == ntnt1
@test merge_recursive(ntnt1, ntnt2) == (
    x = (a = 1, b = 2, c = 3, d = 4),
    y = (a = 1.0, b = "two", c = "three")
)
@test merge_recursive(ntnt1, ntnt2, ntnt3) == (
    x = (a = :one, b = 2, c = 3, d = 4),
    y = (a = 1.0, b = "two", c = "three")
)

@test select(nt1, :a) == nt1[:a]
@test select(nt1, nt2) == (a=1,b=2)
@test select(nt1, nt2) == select(nt1, keys(nt2))
@test select((a = 1, b = [1, 2]), (:b,)) == (b = [1, 2],)

@test split(nt1, :a)[1] == (a = 1,)
@test split(nt1, :a)[2] == (b = 2, c = 3, d = 4)
@test split(nt1, (:b, :c))[1] == (b = 2, c = 3)
@test split(nt1, (:b, :c))[2] == (a = 1, d = 4)
@test merge(split(nt1, (:a, :b))...) == nt1

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


v = [:a => 1, :b => 2]
@test namedtuple(v) == NamedTuple{(:a, :b)}(1, 2)
v = ["a" => 1, "b" => 2]
@test namedtuple(v) == NamedTuple{(:a, :b)}(1, 2)
v = [:b => 2, :a => 1]
@test namedtuple(v) == NamedTuple{(:b, :a)}(2, 1)
v = ["b" => 2, "a" => 1]
@test namedtuple(v) == NamedTuple{(:b, :a)}(2, 1)
v = [:a => [1, 2]]
@test namedtuple(v) == NamedTuple{(:a,)}(Ref([1, 2]),)
v = ["a" => [1, 2]]
@test namedtuple(v) == NamedTuple{(:a,)}(Ref([1, 2]),)

for DictType in [Dict, OrderedDict, LittleDict]
    let DT=DictType
        dict1 = DT(:a=>1)
        nt1 = (a = 1,)
        dict2 = DT(:a=>1, :b=>2//11, :c=>"three")
        nt2 = (a = 1, b = 2//11, c = "three")

        nt1_to_dict = convert(DT, nt1)
        @test nt1_to_dict == dict1
        @test nt1_to_dict isa DT
        @test namedtuple(dict1) == nt1

        nt2_to_dict = convert(DT, nt2)
        @test nt2_to_dict == dict2
        @test nt2_to_dict isa DT
        @test namedtuple(dict2) == nt2
    end
end

# Note: checking the types below requires an ordered dictionary
dict1 = LittleDict(:a=>1, :b=>2//11, :c=>"three")
nt1 = namedtuple(dict1)
@test nt1 isa NamedTuple{(:a, :b, :c),Tuple{Int64,Rational{Int64},String}}

dict2 = LittleDict("a"=>1, "b"=>2//11, "c"=>"three")
nt2 = namedtuple(dict2)
@test nt2 isa NamedTuple{(:a, :b, :c),Tuple{Int64,Rational{Int64},String}}


nt = (a = 1, b = 2)
a = 1; b = 2;
nt_ab = @namedtuple(a, b)
@test nt_ab == nt

nt = (a = 1, b = 2, c = 3)
@test @namedtuple(a, b, c = 3) == nt

nt1 = (a = 1, b = 2)
c = 3
nt = (a = 1, b = 2, c = 3)
@test @namedtuple(nt1..., c) == nt
