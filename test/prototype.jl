#=
    prototype(NT | NTT; types=false)
    prototype(NT | NTT | NTP [, NTuple{nfields, Type} ])

Construct prototypic NamedTuple `schema`.

NamedTuple prototypes are valid constructors.

Prototypes that specify a type for each field expect
a type-matched tuple of values. Prototypes that omit
speicifying types work with any length-matched tuple of values.

|      | kind                 | construct                 |
|:-----|:---------------------|:--------------------------|
| NT   | NamedTuple           | NamedTuple{N,T}(<values>) |
| NTT  | NamedTuple Type      | NamedTuple{N,T}           |
| NTP  | NamedTuple Prototype | NamedTuple{N}             |

=#

@testset "prototype(_)" begin
  @test prototype(Test_NTP) == Test_NTP
  @test prototype(Test_NTT) == Test_NTP
  @test prototype(Test_NT)  == Test_NTP
  @test prototype(test_nt)  == Test_NTP
end

@testset "prototype(_; types=false)" begin
  @test prototype(Test_NTT; types=false) == Test_NTP
  @test prototype(Test_NT; types=false)  == Test_NTP
  @test prototype(test_nt; types=false)  == Test_NTP
end

@testset "prototype(_; types=true)" begin
  @test prototype(Test_NTT; types=true) == Test_NTT
  @test prototype(Test_NT; types=true)  == Test_NTT
  @test prototype(test_nt; types=true)  == Test_NTT
end


  