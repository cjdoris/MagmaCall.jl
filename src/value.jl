"""
    MagmaValue <: Any
    MagmaValue(x)

Abtract type for any kind of Magma value. Most functions accept any `MagmaValue` and return a `MagmaObject`.

Subtypes include [`MagmaObject`](@ref), [`MagmaRef`](@ref), [`MagmaExpr`](@ref).

Overload `MagmaValue(x)` to provide conversions from Julia values to Magma ones.
"""
abstract type MagmaValue end
MagmaValue(x::MagmaValue) = x
MagmaValue(x::Bool) = MagmaExpr(x ? "true" : "false")
MagmaValue(x::Union{Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,BigInt}) = MagmaExpr(string(x))
MagmaValue(x::Integer) = MagmaValue(convert(BigInt, x))
MagmaValue(x::AbstractUnitRange{<:Integer}) = MagmaExpr("[$(valstr(MagmaValue(first(x))))..$(valstr(MagmaValue(last(x))))]")
MagmaValue(x::AbstractRange{<:Integer}) = MagmaExpr("[$(valstr(MagmaValue(first(x))))..$(valstr(MagmaValue(last(x)))) by $(valstr(MagmaValue(step(x))))]")
MagmaValue(x::Union{String,SubString{String}}) = all(isascii, x) ? MagmaExpr(repr(x)) : error("magma only supports ASCII strings")
MagmaValue(x::AbstractString) = MagmaValue(convert(String, x))
MagmaValue(x::AbstractVector) = magseq(x)
MagmaValue(x::AbstractSet) = magset(x)
MagmaValue(x::Tuple) = magtuple(x)
export MagmaValue

argstr(o::MagmaValue) = valstr(o)

"""
    MagmaObject <: MagmaValue
    MagmaObject(x)

A Magma object value. Most functions return a `MagmaObject`.

It is represented by a handle to a variable in the Magma interpreter.
"""
mutable struct MagmaObject <: MagmaValue
    id :: Int
    function MagmaObject()
        id = LASTID[] += 1
        o = new(id)
        finalizer(o) do o
            RUNNING[] && @async magdelete(o)
        end
        return o
    end
end
MagmaObject(x::MagmaObject) = x
MagmaObject(x::MagmaValue) = interact() do p
    o = MagmaObject()
    putcmd(p, varname(o), " := ", valstr(x))
    readtotoken(Nothing, p, missing)
    o
end
MagmaObject(x) = MagmaObject(MagmaValue(x))
export MagmaObject

id(o::MagmaObject) = getfield(o, :id)
varname(o::MagmaObject) = "$(VARPREFIX)$(id(o))"
valstr(o::MagmaObject) = varname(o)

"""
    MagmaRef <: MagmaValue
    MagmaRef(o::MagmaObject)

A reference to a Magma object, suitable for passing to mutating procedures such as `Append` or `Sort`.
"""
struct MagmaRef <: MagmaValue
    o :: MagmaObject
    MagmaRef(o::MagmaObject) = new(o)
end
MagmaRef(x::MagmaRef) = x
MagmaRef(x) = MagmaRef(MagmaObject(x))
export MagmaRef

MagmaObject(o::MagmaRef) = getfield(o, :o)
valstr(o::MagmaRef) = valstr(getfield(o, :o))
argstr(o::MagmaRef) = string("~", valstr(o))

"""
    MagmaExpr <: MagmaValue
    MagmaExpr(ex::String)

A literal Magma value, represented by an expression which evaluates to the value.

For simple values such as integers and strings, `MagmaValue(x)` will return a `MagmaExpr`.
"""
struct MagmaExpr <: MagmaValue
    ex :: String
end
export MagmaExpr
valstr(x::MagmaExpr) = getfield(x, :ex)

"""
    MagmaIntrinsic <: MagmaValue
    MagmaIntrinsic(name)

The Magma intrinsic with the given name.
"""
struct MagmaIntrinsic <: MagmaValue
    name :: String
end
MagmaIntrinsic(name::Symbol) = MagmaIntrinsic(string(name))
export MagmaIntrinsic
valstr(x::MagmaIntrinsic) = "'$(getfield(x, :name))'"

intrarg(x) = MagmaValue(x)
intrarg(x::Symbol) = MagmaIntrinsic(x)

"""
    MagmaType <: MagmaValue
    MagmaType(name, ...)

The Magma type with the given name.

Extra arguments make an extended type, e.g. `MagmaType(:RngUPol, :RngInt)` is Magma's `RngUPol[RngInt]`.
"""
struct MagmaType{Ts<:Tuple} <: MagmaValue
    name :: String
    args :: Ts
    MagmaType(name::String, args::Vararg{MagmaType}) = new{typeof(args)}(name, args)
end
MagmaType(name::String, args...) = MagmaType(name, map(MagmaType, args)...)
MagmaType(name::Symbol, args...) = MagmaType(string(name), args...)
MagmaType(t::MagmaType) = t
MagmaType(args::Tuple) = MagmaType(args...)
export MagmaType
valstr(x::MagmaType) = isempty(getfield(x, :args)) ? getfield(x, :name) : string(getfield(x, :name), "[", join(map(valstr, getfield(x, :args)), ", "), "]")

typearg(x) = MagmaValue(x)
typearg(x::Union{Symbol,Tuple{Symbol,Vararg}}) = MagmaType(x)

"""
    MagmaCallable{N} <: MagmaValue
    MagmaCallable{N}(o)

Wrap the value `o`, declaring it to be callable with `N` return values.

The resulting object can use the usual function call syntax, instead of `magmacall`.
"""
struct MagmaCallable{N,T<:MagmaValue} <: MagmaValue
    o :: T
end
MagmaCallable{N}(o::MagmaValue) where {N} = MagmaCallable{N,typeof(o)}(o)
MagmaCallable{N}(o::MagmaCallable) where {N} = MagmaCallable{N}(getfield(o, :o))
export MagmaCallable
valstr(x::MagmaCallable) = valstr(getfield(x, :o))
argstr(x::MagmaCallable) = argstr(getfield(x, :o))
MagmaObject(o::MagmaCallable) = MagmaObject(getfield(o, :o))
(f::MagmaCallable{N})(args...; opts...) where {N} = magcall(Val(N), getfield(f,:o), args...; opts...)
