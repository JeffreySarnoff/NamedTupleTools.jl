using Documenter # , NamedTupleTools

makedocs(
    # modules = [NamedTupleTools],
    sitename = "NamedTupleTools",
    format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
    pages    = Any[
        "Overview"   => "index.md",
        "Concepts"   => "concepts.md",
        "NamedTuples" => "namedtuples.md",
        "Operations" => "operations.md",
        "Functions" =>  "functions.md",
        "Index"  => "generalindex.md"
        ]
    )

deploydocs(
    repo = "github.com/JeffreySarnoff/NamedTupleTools.jl.git",
    target = "build"
)
