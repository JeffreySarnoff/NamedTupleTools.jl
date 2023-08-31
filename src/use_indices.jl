#=
Tokazama (2023-08-29)

I know it's a pain, but there should probably be a serious conversation
concerning when tuples of indices are used and how they're interpreted.
We have tuples of indices for a single index on multidim arrays,
but how should we refer to multiple indices on tuples and named tuples?
There's a loss of information if we require it always be another subtype
of AbstractArray

from https://github.com/JuliaLang/julia/pull/51098#discussion_r1309188308
=#

"""
    idxkey(nt::NamedTuple, idx::Integer)
    idxkey(nt::NamedTuple, idx::Tuple{Vararg{Integer}})

Map the indices given with `idx` to keys of `nt`.
""" idxkey

function idxkey(nt::NamedTuple, idx::Integer)
    getindex(keys(nt), idx)
end

function idxkey(nt::NamedTuple, @nospecialize(idxs::NTuple{N,Int} where {N}))
    getindex.(Ref(keys(nt)), idxs)
end

"""
    keep(nt::NamedTuple, sym::Symbol)
    keep(nt::NamedTuple, idx::Integer)

    keep(nt::NamedTuple, sym::Tuple{Vararg{Symbol}})
    keep(nt::NamedTuple, idx::Tuple{Vararg{Integer}})

- Construct a subset of `nt` using only the key [keys in] `sym`.
- Construct a subset of `nt` using only the index [indicies in] `idx`.
""" keep

function keep(nt::NamedTuple, sym::Symbol)
    NamedTuple{(sym,)}(nt)
end

function keep(nt::NamedTuple, idx::Integer)
    keep(nt, idxkey(nt, idx))
end

function keep(nt::NamedTuple, @nospecialize(sym::NTuple{N,Symbol} where {N}))
     NamedTuple{sym}(nt)
end

function keep(nt::NamedTuple, @nospecialize(idx::NTuple{N,<:Integer} where {N}))
     keep(nt, idxkey(nt, idx))
end

"""
    omit(nt::NamedTuple, sym::Symbol)
    omit(nt::NamedTuple, idx::Integer)

    omit(nt::NamedTuple, sym::Tuple{Vararg{Symbol}})
    omit(nt::NamedTuple, idx::Tuple{Vararg{Integer}})

- Construct a subset of `nt` omitting the key [keys in] `sym`.
- Construct a subset of `nt` omitting the index [indicies in] `idx`.
""" omit

function omit(nt::NamedTuple, sym::Symbol)
    keep(nt, Tuple(setdiff(keys(nt), (sym,))))
end

function omit(nt::NamedTuple, idx::Integer)
    omit(nt, idxkey(nt, idx))
end

function omit(nt::NamedTuple, @nospecialize(sym::NTuple{N,Symbol} where {N}))
     keep(nt, Tuple(setdiff(keys(nt), sym)))
end

function omit(nt::NamedTuple, @nospecialize(idx::NTuple{N,<:Integer} where {N}))
     omit(nt, idxkey(nt, idx))
end

