#=

     NamedTuple predicates and primitives
     
=#

#=

    NamedTuples support these functions
    
length(t::NamedTuple) = nfields(t)
start(t::NamedTuple) = 1
done(t::NamedTuple, iter) = iter > nfields(t)
next(t::NamedTuple, iter) = (getfield(t, iter), iter + 1)
endof(t::NamedTuple) = nfields(t)
getindex(t::NamedTuple, i::Int) = getfield(t, i)
getindex(t::NamedTuple, i::Symbol) = getfield(t, i)
indexed_next(t::NamedTuple, i::Int, state) = (getfield(t, i), i+1)
isempty(::NamedTuple{()}) = true
isempty(::NamedTuple) = false

=#
