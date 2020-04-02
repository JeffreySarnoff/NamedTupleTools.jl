## Using Julia's NamedTuples

### Creating a NamedTuple

#### The Empty NamedTuple
```julia
julia> empty_nt = NamedTuple()
NamedTuple()
julia> isempty(empty_nt)
true
julia> nfields(empty_nt)
0
```

#### A NamedTuple with one field
```julia
julia> (a = 1,)
(a = 1,)
# however, the field name must be immediate
julia> field_name = :afield  # field names must be symbols
:afield
julia> nt = (field_name = 1,)   # the trailing comma is required for single field NamedTuples
(field_name = 1,)
# the field value may be given through a variable
julia> field_value = 1;
julia> (a = field_value,)
(a = 1,)
```
