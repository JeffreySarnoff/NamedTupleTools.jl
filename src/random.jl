using Random

const LCASE = collect('a':'z')

function gensymbol(nchars=1, offset=0)
    nchars = max(1, min(26, nchars))
    ofs  = max(0, min(offset, 26-nchars))
    return Symbol(join(LCASE[1+ofs:nchars+ofs]))
end

function gensymbols(n=1, nchars=1; randwidth::Bool=false)
    fnwidth() = randwidth ? rand(1:nchars) : nchars
    [gensymbol(fnwidth(), i) for i=0:n-1]
end

function rand1(::Type{NamedTuple}, nfields::Int=1, nchars::Int=1; 
               permutesyms::Bool=false, permutevals::Bool=false, randwidth::Bool=false)
    syms = Tuple(gensymbols(nfields, nchars; randwidth)[permutesyms ? randperm(nfields) : 1:nfields])
    vals = (1:nfields)[permutevals ? randperm(nfields) : 1:nfields]
    return NamedTuple{syms}(vals)
end

function Random.rand(::Type{NamedTuple},n::Int=1, nfields::Int=1, nchars::Int=1; 
                      permutesyms::Bool=false, permutevals::Bool=false, randwidth::Bool=false)
    [rand1(NamedTuple,nfields, nchars;permutesyms,permutevals,randwidth) for i=1:n]
end

        
