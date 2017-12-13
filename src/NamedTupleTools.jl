"""
     NamedTupleTools

This module provides some useful aspecting of NamedTuples.

@ref(names), @ref(values)
"""
module NamedTupleTools

export nt_names, nt_values, nt_namesvalues,
       NT_names, NT_values, NT_namesvalues,

const SINT = Union{ Int8,  Int16,  Int32,  Int64}
const UINT = Union{UInt8, UInt16, UInt32, UInt64}
const SIUI = Union{SINT, UINT}

include("kinds.jl")
include("parts.jl")
include("wholes.jl")

end # module NamedTupleTools
