#=

    Whole NamedTuples

=#


"""
    NT_Names(names)
    
Generate a NamedTuple Prototype that labels precipient values using `names`.
"""
macro NT_Names(names)
   :(NamedTuple{names})
end

"""
    NT_NamesValues(names, values)
    
Generate a NamedTuple that evinces the assignation of the `values` to the `names`.
"""
macro NT_NamesValues(names, values)
   :(NamedTuple{$names}($values))
end

"""
    NT_NamedTupValues(namedtuple, values)_nt_names
    
Generate a NamedTuple that realizes [assigns values to the names of] `namedtuple`.
"""
macro NT_NamedTupValues(namedtuple, values)
    :($namedtuple($values))
end

"""
    NT_NamedTupNames(namedtuple, names)
    
Generate a NamedTuple constitued with the subset of `names` in `namedtuple`.

#Example
    julia> nt_abc123 = @NT_NamesValues((:a, :b, :c),(1, 2, 3))
    (a = 1, b = 2, c = 3)
    julia> typeof(nt_abc123)
    NamedTuple{(:a, :b, :c),Tuple{Int64,Int64,Int64}}
    julia> names_to_use = (:a, :c)
    (:a, :c)
    julia> nt_ac = @NT_NamedTupNames(nt_abc123, names_to_use)
    (a = 1, c = 3)
    
    julia> typeof(nt_ac)
    NamedTuple{(:a, :c),Tuple{Int64,Int64}}
"""
macro NT_NamedTupNamesValues(namedtuple, names)
    :(NamedTuple{($names)}($namedtuple))_nt_names
end
