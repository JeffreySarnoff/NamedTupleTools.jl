"""
     NamedTupleTools

This module provides some useful NamedTuple tooling.

see [`namedtuple`](@ref), [`isprototype`](@ref),
    [`fieldnames`](@ref), [`fieldtypes`](@ref), [`fieldvalues`](@ref)
    [`delete`](@ref), [`merge`](@ref)
"""
module NamedTupleTools

export @namedtuple,
       namedtuple,
       prototype, isprototype,
       fieldvalues,
       delete,
       select,
       setproperty, resetproperty,
       ntfromstruct, structfromnt,
       @structfromnt

import Base: propertynames, fieldnames, fieldtypes, valtype, merge, split

# accept comma delimited values
NamedTuple{T}(xs...) where {T} = NamedTuple{T}(xs)

propertynames(nt::NamedTuple{N,T}) where {N,T} = N
fieldnames(nt::NamedTuple{N,T}) where {N,T} = N

"""
    fieldvalues

obtain values assigned to fields of a struct type
(in field order)
"""
fieldvalues(x::NamedTuple{N,S}) where {N,S} = Tuple(x)

function fieldvalues(x::T) where {T}
     !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))
     
     return ((getfield(x, name) for name in fieldnames(T))...,)
end

"""
    fieldtypes( namedtuple )
    fieldtypes( typeof(namedtuple) )

Retrieve the values' types as a tuple.

see: [`valtype`](@ref)
"""
fieldtypes(x::NamedTuple{N,S}) where {N,S} = (S.parameters...,)
fieldtypes(::Type{NamedTuple{N,S}}) where {N,S} = (S.parameters...,)

"""
    valtype( namedtuple )
    valtype( typeof(namedtuple) )

- valtype(::Type{NamedTuple{N,S}}) where {N,S} = S
Retrieve the values' types as a typeof(tuple).

see: [`fieldtypes`](@ref)
"""
valtype(x::NamedTuple{N,S}) where {N,S} = S
valtype(::Type{NamedTuple{N,S}}) where {N,S} = S

"""
    prototype( namedtuple; keeptypes::Bool=false )
    prototype( name1, name2, ..  )
    prototype( (name1, name2, ..) )

Generate a NamedTuple prototype by specifying or obtaining the fieldnames.
The prototype is applied to a tuple fieldvalues, giving a completed NamedTuple.

Obtain a NamedTuple prototype with field types as given in the source NamedTuple.
The prototype is applied to a tuple of fieldvalues, giving a completed NamedTuple.
This elaborated form constrains the completed NamedTuple to have the same types.

# Example

julia> proto_nt = prototype( :a, :b, :c )
NamedTuple{(:a, :b, :c),T} where T<:Tuple

julia> proto_nt(1, 2, 3)
(a = 1, b = 2, c = 3)

julia> vals = (1, 2, 3); proto_nt((vals)  # faster
(a = 1, b = 2, c = 3)

julia> vals = (1, 2.0, 3//1); proto_nt(vals)
(a = 1, b = 2.0, c = 3//1)

see: [`isprototype`](@ref)
""" prototype

"""
    namedtuple(names, values)
    namedtuple(::AbstractDict{Symbol,Any})
    namedtuple(struct_instance)

Create a NamedTuple.
""" namedtuple

namedtuple(x::NamedTuple{N,S}) where {N,S} = x
namedtuple(x::Type{NamedTuple{N,S}}) where {N,S} = x
namedtuple(x::Type{T}) where {T<:NamedTuple} = x

function namedtuple(x::T; remember::Union{Nothing,Bool}=nothing) where T
    if isstructtype(T)
	result = isnothing(remember) ? ntfromstruct(x) : ntfromstruct(x, remember)
    elseif isa(x, AbstractDict)
	result = ntfromdict(x)
    else
        throw(ErrorException("$T not supported"))
    end
    return result
end

function ntfromstruct(x::T) where {T}
     !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))
     names = fieldnames(T)
     values = fieldvalues(x)
     return NamedTuple{names}(values)
end

function ntfromstruct(x::T; remember::Bool=false) where {T}
    !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))
    names = fieldnames(T)
    values = fieldvalues(x)
    if remember
        typ = typeof(x).name.wrapper
	names = (:Struct, names...,)
	values = (typ, values...,)
    end
    return NamedTuple{names}(values)
end

# an instance of type S, a Struct
function structfromnt(x::NamedTuple{N,T}) where {N,T}
    names = N
    if !(:Struct in names)
	throw(ErrorException("use `structfromnt(StructName, NamedTuple)` or\n
        create NamedTuple using `ntfromstruct(struct, keepstruct=true)`"))
    end
    values = fieldvalues(x)
    ntindicies = [.!(:Struct .== names)...,]
    names = names[ntindicies]
    values = values[ntindicies]	
    return x.Struct(values...)
end

# the Struct itself
function structfromnt(structname::Union{Symbol, String}, nt::NamedTuple{N,T}) where {N,T}
    sname = Symbol(structname)
    names = N
    types = type_untuple(T)
    tostruct = Meta.parse(NamedTupleTools.struct_from(sname, names, types))
    eval(tostruct) # generate Struct
    return nothing
end

macro structfromnt(sname, nt)
    :( eval(structfromnt($(esc(sname)), $(esc(nt)))) )
end

# Expr part from Fredrik Ekre   
struct_from(structname, names, types) = 
	"Expr(:struct,
		false,
		Expr(:curly,
			 :$structname
		),
		Expr(:block,
			map((x,y) -> Expr(:(::), x, y), $names, $types)...
		)
	)"
	
structfrom(structname, names, types) = eval(eval(Meta.parse(struct_from(structname, names, types))))

"""
    namedtuple(namesforvalues, valuesfornames)
"""

# from kristoffer.carlsson

@inline function namedtuple(namesforvalues::NTuple{N,Symbol}, valuesfornames) where {N}
    N == length(valuesfornames) || throw(ErrorException("lengths must match"))
    return (; zip(namesforvalues, valuesfornames)...,)
end

@inline function namedtuple(namesforvalues::Vector{Symbol}, valuesfornames)
    length(namesforvalues) == length(valuesfornames) || throw(ErrorException("lengths must match"))
     return (; zip(namesforvalues, valuesfornames)...,)
end

namedtuple(namesforvalues::Vector{S}, valuesfornames) where {N,S<:AbstractString} =
    namedtuple(Symbol.(namesforvalues), valuesfornames)

namedtuple(namesforvalues::NTuple{N,S}, valuesfornames) where {N,S<:AbstractString} =
    namedtuple(Symbol.(namesforvalues), valuesfornames)


"""
    prototype(namedtuple)
    prototype(typeof(namedtuple))

provides the prototype `NamedTuple{names, T} where T<:Tuple`
    - `names` is a tuple of symbols
"""

prototype(x::NamedTuple{N,S}; keeptypes::Bool=false) where {N,S} =
    keeptypes ? NamedTuple{N,S} : NamedTuple{N}
#=
prototype(::NamedTuple{A,B}) where {A,B} = NamedTuple{A}
prototype(::Type{NamedTuple{A,B}}) where {A,B} = NamedTuple{A}
=#

prototype(names::NTuple{N,Symbol}) where {N} = NamedTuple{names}
prototype(names::Vararg{Symbol}) = NamedTuple{names}
prototype(names::NTuple{N,String}) where {N}  = prototype(Symbol.(names))
prototype(names::Vararg{String}) = prototype(Symbol.(names))
prototype(names::T) where {T<:AbstractVector{Symbol}} = prototype(names...,)
prototype(names::T) where {T<:AbstractVector{String}} = prototype(Symbol.(names))

useprototype(names) = throw(error("Use `prototype($names)`"))
namedtuple(names::NTuple{N,Symbol}) where {N} = useprototype(names)
namedtuple(names::Vararg{Symbol}) = useprototype(names)
namedtuple(names::NTuple{N,String}) where {N}  = useprototype(names)
namedtuple(names::Vararg{String}) = useprototype(names)
namedtuple(names::T) where {T<:AbstractVector{Symbol}} = useprototype(names)
namedtuple(names::T) where {T<:AbstractVector{String}} = useprototype(names)

"""
    isprototype( ntprototype )
    isprototype( namedtuple  )

Predicate that identifies NamedTuple prototypes.

see: [`namedtuple`](@ref)
"""
isprototype(::Type{T}) where {T<:NamedTuple} = eltype(T) === Any
isprototype(nt::T) where {T<:NamedTuple} = false
isprototype(::Type{UnionAll}) = false

"""
   delete(namedtuple, symbol(s)|Tuple)
   delete(ntprototype, symbol(s)|Tuple)
   
Generate a namedtuple [ntprototype] from the first arg omitting fields present in the second arg.

see: [`merge`](@ref)
"""
delete(a::NamedTuple, b::Symbol) = Base.structdiff(a, prototype(b))
delete(a::NamedTuple, b::NTuple{N,Symbol}) where {N} = Base.structdiff(a, prototype(b))
delete(a::NamedTuple, bs::Vararg{Symbol}) = Base.structdiff(a, prototype(bs))

delete(::Type{T}, b::Symbol) where {S,T<:NamedTuple{S}} = prototype((Base.setdiff(S,(b,))...,))
delete(::Type{T}, b::NTuple{N,Symbol}) where {S,N,T<:NamedTuple{S}} = prototype((Base.setdiff(S,b)...,))
delete(::Type{T}, bs::Vararg{Symbol}) where {S,N,T<:NamedTuple{S}} = prototype((Base.setdiff(S,bs)...,))

"""
   select(namedtuple, symbol(s)|Tuple)
   select(ntprototype, symbol(s)|Tuple)
   
Generate a namedtuple [ntprototype] from the first arg, including only fields present in the second arg.

see: [`merge`](@ref)
"""
select(nt::NamedTuple, k::Symbol) = nt[k]
select(nt::NamedTuple, k::NamedTuple) = select(nt, keys(k))
select(nt::NamedTuple, ks) = prototype(ks)(((nt[k] for k in ks)...,))


"""
    merge(namedtuple1, namedtuple2)
    merge(nt1, nt2, nt3, ..)

Generate a namedtuple with all fieldnames and values of namedtuple2
    and every fieldname of namedtuple1 that does not occur in namedtuple2
    along with their values.

see: [`delete!`](@ref)
"""
# merge(nt1::T1, nt2::T2 ...) where {T1<:NamedTuple, T2<:NamedTuple, ...} is already defined

merge(::Type{T1}, ::Type{T2}) where {N1,N2,T1<:NamedTuple{N1},T2<:NamedTuple{N2}} =
    namedtuple((unique((N1..., N2...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}) where {N1,N2,N3,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3}} =
    namedtuple((unique((N1..., N2..., N3...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}) where {N1,N2,N3,N4,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4}} =
    namedtuple((unique((N1..., N2..., N3..., N4...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}) where {N1,N2,N3,N4,N5,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}, ::Type{T6}) where {N1,N2,N3,N4,N5,N6,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5},T6<:NamedTuple{N6}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5..., N6...,))...,))
merge(::Type{T1}, ::Type{T2}, ::Type{T3}, ::Type{T4}, ::Type{T5}, ::Type{T6}, ::Type{T7}) where {N1,N2,N3,N4,N5,N6,N7,T1<:NamedTuple{N1},T2<:NamedTuple{N2},T3<:NamedTuple{N3},T4<:NamedTuple{N4},T5<:NamedTuple{N5},T6<:NamedTuple{N6},T7<:NamedTuple{N7}} =
    namedtuple((unique((N1..., N2..., N3..., N4..., N5..., N6...,N7...))...,))

"""
    split(namedtuple, symbol(s)|Tuple)

Generate two namedtuples, the first with only the fields in the second arg, the
second with all but the fields in the second arg, such that
`merge(split(nt, ks)...) == nt` when `ks` contains the first fields of `nt`.
"""
split(nt::NamedTuple, ks::Symbol) = split(nt, (ks,))
split(nt::NamedTuple, ks) = select(nt, ks), delete(nt, ks)


#=  interconvert: NamedTuple <--> Dict =#

namedtuple(d::T) where {T<:AbstractDict{Symbol,V}} where {V} =
    NamedTuple{gather_(keys(d)), NTuple{length(d), V}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{S,V}} where {S<:AbstractString, V} =
    NamedTuple{Symbol.(gather_(keys(d))), NTuple{length(d), V}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{Symbol,Any}} =
    NamedTuple{gather_(keys(d)), Tuple{typeof.(values(d))...}}(gather_(values(d)))
namedtuple(d::T) where {T<:AbstractDict{S,Any}} where {S<:AbstractString} =
    NamedTuple{Symbol.(gather_(keys(d))), Tuple{typeof.(values(d))...}}(gather_(values(d)))

# use: dict = convert(Dict, nt)
#=
   for Dict{Symbol,Any}: 
   Base.convert(::Type{Dict}, x::NT) where {N, NT<:NamedTuple{N}} = 
       Dict([sym=>val for (sym,val) in zip(fieldnames(x), fieldvalues(x))])
=#
Base.convert(::Type{D}, x::NT) where {D<:AbstractDict, N, NT<:NamedTuple{N}} = 
    D{Symbol, uniontype(x)}([sym=>val for (sym,val) in zip(fieldnames(x), fieldvalues(x))])

# from PR by pdeffebach (Vector of Pairs becomes NamedTuple)
namedtuple(v::Vector{<:Pair{<:Symbol}}) = prototype([p[1] for p in v]...)([p[2] for p in v]...)
# with names as strings
namedtuple(v::Vector{<:Pair{<:String}}) = prototype([p[1] for p in v]...)([p[2] for p in v]...)

# NamedTuple becomes a Vector of Pairs 
Base.convert(::Type{Vector{Pair}}, nt::NamedTuple) =  map(kv->Pair(first(kv), last(kv)), zip(keys(nt), values(nt)))

# from Sebastian Pfitzner (on Slack)
macro namedtuple(vars...)
   args = Any[]
   for v in vars
       if Meta.isexpr(v, :(=)) || Meta.isexpr(v, :...)
           push!(args, esc(v))
       else
           push!(args, Expr(:(=), esc(v), esc(v)))
       end
   end
   expr = Expr(:tuple, Expr(:parameters, args...))
   return expr
end

"""
    setproperty(NamedTuple, Symbol, value)

Construct a copy of the NamedTuple using the new value for Symbol.
- the type of `value` must match the type assigned in the `NamedTuple`.
"""
function setproperty(nt::NamedTuple{N,T}, property::Symbol, value) where {N,T}
    values = fieldvalues(nt)
    idx = findfirst(x->x===property, N)
    isnothing(idx) && throw(ErrorException("There is no field named $property."))
    if !isa(value, fieldtypes(nt)[idx])
	throw(ErrorException("The typeof($value) must conform to $(fieldtypes(nt)[idx]). Use `resetproperty` to change the type of a value."))
    end
    values = (values[1:idx-1]..., value, values[idx+1:end]...)
    return NamedTuple{N}(values)
end

"""
    resetproperty(NamedTuple, Symbol, value)

Construct a copy of the NamedTuple using the new value for Symbol.
- the type of `value` need not match the type assigend in the `NamedTuple`
"""
function setproperty(nt::NamedTuple{N,T}, property::Symbol, value) where {N,T}
    values = fieldvalues(nt)
    idx = findfirst(x->x===property, N)
    isnothing(idx) && throw(ErrorException("There is no field named $property."))
    values = (values[1:idx-1]..., value, values[idx+1:end]...)
    return NamedTuple{N}(values)
end

if VERSION < v"1.5.0-DEV"
    """
        @NamedTuple{key1::Type1, key2::Type2, ...}
        @NamedTuple begin key1::Type1; key2::Type2; ...; end
    This macro gives a more convenient syntax for declaring `NamedTuple` types. It returns a `NamedTuple`
    type with the given keys and types, equivalent to `NamedTuple{(:key1, :key2, ...), Tuple{Type1,Type2,...}}`.
    If the `::Type` declaration is omitted, it is taken to be `Any`.   The `begin ... end` form allows the
    declarations to be split across multiple lines (similar to a `struct` declaration), but is otherwise
    equivalent.
    For example, the tuple `(a=3.1, b="hello")` has a type `NamedTuple{(:a, :b),Tuple{Float64,String}}`, which
    can also be declared via `@NamedTuple` as:
    ```jldoctest
    julia> @NamedTuple{a::Float64, b::String}
    NamedTuple{(:a, :b),Tuple{Float64,String}}
    julia> @NamedTuple begin
               a::Float64
               b::String
           end
    NamedTuple{(:a, :b),Tuple{Float64,String}}
    ```
    """
    macro NamedTuple(ex)
        Meta.isexpr(ex, :braces) || Meta.isexpr(ex, :block) ||
            throw(ArgumentError("@NamedTuple expects {...} or begin...end"))
        decls = filter(e -> !(e isa LineNumberNode), ex.args)
        all(e -> e isa Symbol || Meta.isexpr(e, :(::)), decls) ||
            throw(ArgumentError("@NamedTuple must contain a sequence of name or name::type expressions"))
        vars = [QuoteNode(e isa Symbol ? e : e.args[1]) for e in decls]
        types = [esc(e isa Symbol ? :Any : e.args[2]) for e in decls]
        return :(NamedTuple{($(vars...),), Tuple{$(types...)}})
    end

    export @NamedTuple	
end

# low level utility functions for internal use only

uniontype(nt::NamedTuple{N,S}) where {N,S} = Union{S.parameters...}	

"""
    gather_(x::Iterable)

Collect the elements of x into a Tuple, in their iterated order. 
"""
@inline gather_(x::T) where {T} = (x...,)

"""
    type_untuple( Tuple{_} )

Retrieve the types that are internal to the `Tuple` as a (_).
"""
type_untuple(::Type{T}) where {T<:Tuple} = (T.parameters...,)


"""
    type_retuple( (_) )

Generate a `Tuple` with the given internal types as a `Tuple{_}`.
"""
type_retuple(x::Tuple) = Tuple{x...}


end # module NamedTupleTools
