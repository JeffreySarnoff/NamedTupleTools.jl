using Documenter, NamedTupleTools

makedocs(
    modules = [NamedTupleTools],
    sitename = "NamedTupleTools",
    format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
    pages    = Any[
        "Overview"   => "index.md",
        "Concepts"   => "concepts.md",
        "Operations" => "operations.md",
        "Functions" => "tutorial.md",
         "Index"  => "functionindex.md"
        ]
    )

deploydocs(
    repo = "github.com/JeffreySarnoff/NamedTupleTools.jl.git",
    target = "build"
)
