"""
This file contains an implementation of some of the Magma Object Transfer Protocol.

The following types can be read:
- Numbers: integer, rational
- Containers: string, list, sequence, set
- Structures: null, integers, rationals, strings

Writing objects is not yet implemented.

We use this to convert from Magma to Julia, by setting up a persistent TCP socket between
Julia and Magma ([`magotp_start`](@ref)), using the MOTP over this socket to get a
representation of the Magma object, and converting to the desired type.
See [`magconvert`](@ref).

The MOTP is currently undocumented, so the implementation is based on experimentation.
The following is a way to see how an object `x` is encoded:
```julia
io = magf.Open("test", "w")
magp.WriteObject(io, x)
magdelete(io)
read("test")
```
"""

const MAGOTP_MAX_VERSION = UInt32(1)
const MAGOTP_VERSION = Ref(MAGOTP_MAX_VERSION)
const MAGOTP_SOCKET = Ref{Sockets.TCPSocket}()
const MAGOTP_MAG_SERVER_SOCKET = MagmaObject()
const MAGOTP_MAG_CLIENT_SOCKET = MagmaObject()
const MAGOTP_LOCK = Channel{Nothing}(1)

@enum MAGOTP_CMD::UInt32 MAGOTP_CMD_VERSION=0 MAGOTP_CMD_OBJECT=1
@enum(MAGOTP_TYPE::UInt16,
    MAGOTP_TYPE_NULLSTR   = 0x0000,
    MAGOTP_TYPE_INTEGERS  = 0x0001,
    MAGOTP_TYPE_INTEGER   = 0x0002,
    MAGOTP_TYPE_RATIONALS = 0x0003,
    MAGOTP_TYPE_RATIONAL  = 0x0004,
    MAGOTP_TYPE_REALS     = 0x0005,
    MAGOTP_TYPE_STRINGS   = 0x000b,
    MAGOTP_TYPE_STRING    = 0x000c,
    MAGOTP_TYPE_LIST      = 0x0017,
    MAGOTP_TYPE_SEQUENCE  = 0x0018,
    MAGOTP_TYPE_SET       = 0x001c,
)

abstract type MagmaOTPValue end
abstract type MagmaOTPStr <: MagmaOTPValue end
abstract type MagmaOTPRng <: MagmaOTPStr end
abstract type MagmaOTPFld <: MagmaOTPRng end
abstract type MagmaOTPElt <: MagmaOTPValue end
abstract type MagmaOTPRngElt <: MagmaOTPElt end
abstract type MagmaOTPFldElt <: MagmaOTPRngElt end
struct MagmaOTPNullStr <: MagmaOTPStr end
struct MagmaOTPIntegers <: MagmaOTPRng end
struct MagmaOTPInteger{T<:Integer} <: MagmaOTPRngElt
    value :: T
end
struct MagmaOTPRationals <: MagmaOTPFld end
struct MagmaOTPRational{T<:Rational} <: MagmaOTPFldElt
    value :: T
end
struct MagmaOTPReals <: MagmaOTPFld
    precision :: Int
end
struct MagmaOTPStrings <: MagmaOTPStr end
struct MagmaOTPString{T<:AbstractString} <: MagmaOTPElt
    value :: T
end
struct MagmaOTPList{V<:AbstractVector} <: MagmaOTPValue
    value :: V
end
struct MagmaOTPNullSequence <: MagmaOTPValue end
struct MagmaOTPRangeSequence{U<:MagmaOTPValue, V<:StepRangeLen} <: MagmaOTPValue
    universe :: U
    value :: V
end
struct MagmaOTPSequence{U<:MagmaOTPValue, V<:AbstractVector} <: MagmaOTPValue
    universe :: U
    value :: V
end
struct MagmaOTPNullSet <: MagmaOTPValue end
struct MagmaOTPRangeSet{U<:MagmaOTPValue, V<:StepRangeLen} <: MagmaOTPValue
    universe :: U
    value :: V
end
struct MagmaOTPSet{U<:MagmaOTPValue, V} <: MagmaOTPValue
    universe :: U
    value :: V
end

magotp_interact_start() = (take!(MAGOTP_LOCK); MAGOTP_SOCKET[])
magotp_interact_stop() = (put!(MAGOTP_LOCK, nothing); nothing)

function magotp_interact(f)
    io = magotp_interact_start()
    try
        return f(p)
    finally
        magotp_interact_stop()
    end
end

function magotp_start()
    interact() do p
        # magma: make the server socket and print the port number
        putcmd(p, varname(MAGOTP_MAG_SERVER_SOCKET), " := Socket(:LocalHost:=\"localhost\"); SocketInformation(", varname(MAGOTP_MAG_SERVER_SOCKET), ")[2]", err=true)
        port = readtotoken(Int, p, missing)
        checkerr(p)
        # magma: wait for a connection from julia
        putcmd(p, varname(MAGOTP_MAG_CLIENT_SOCKET), " := WaitForConnection(", varname(MAGOTP_MAG_SERVER_SOCKET), ")", err=true)
        # julia: connect to the socket
        MAGOTP_SOCKET[] = Sockets.connect(port)
        # magma: finish waiting
        echototoken(p, missing)
        checkerr(p)
        # version exchange
        # # julia: send MOTP version
        # magotp_notify_version()
        # # magma: exchange MOTP version
        # putcmd(p, "ExchangeVersions(", varname(MAGOTP_MAG_CLIENT_SOCKET), ")")
        # echototoken(p, missing)
        # checkerr(p)
        # # julia: read MOTP version
        # magotp_expect_version()
    end
    put!(MAGOTP_LOCK, nothing)
end

function magotp_stop()
    take!(MAGOTP_LOCK)
    interact() do p
        putcmd(p, "delete ", varname(MAGOTP_MAG_SERVER_SOCKET), "; delete ", varname(MAGOTP_MAG_CLIENT_SOCKET), err=true)
        skiptotoken(p, missing)
        checkerr(p)
    end
end

function magotp_restart()
    magotp_stop()
    magotp_start()
end

magotp_notify_version(ver=MAGOTP_VERSION[]) = magotp_interact() do io
    write(io, MAGOTP_CMD_VERSION)
    write(io, UInt32(ver))
    return
end

magotp_expect_version() = magotp_interact() do io
    r = read(io, UInt32)
    r == MAGOTP_HDR_VERSION || error()
    v = read(io, UInt32)
    if v < MAGOTP_VERSION[]
        MAGOTP_VERSION[] = v
    end
    return v
end

magotp_read_cmd(io::IO) = read(io, MAGOTP_CMD)

magotp_read_type(io::IO) = read(io, MAGOTP_TYPE)

"""
    magotp_read_full_object(::Type{T}, io::IO)

Read an object from `io`, including the header. Return it as a `T`.
"""
function magotp_read_full_object(::Type{T}, io::IO) where {T}
    c = magotp_read_cmd(io)
    c == MAGOTP_CMD_OBJECT || error()
    n = magotp_read_uinteger(Int, io)
    data = read(io, n)
    magotp_read_object(T, IOBuffer(data))::T
end

"""
    magotp_read_object(::Type{T}, io::IO)

Read an object from `io`. Return it as a `T`.
"""
function magotp_read_object(::Type{T}, io::IO) where {T}
    t = magotp_read_type(io)
    if t == MAGOTP_TYPE_NULLSTR
        magotp_read_nullstr(T, io)
    elseif t == MAGOTP_TYPE_INTEGERS
        magotp_read_integers(T, io)
    elseif t == MAGOTP_TYPE_INTEGER
        magotp_read_integer(T, io)
    elseif t == MAGOTP_TYPE_RATIONALS
        magotp_read_rationals(T, io)
    elseif t == MAGOTP_TYPE_RATIONAL
        magotp_read_rational(T, io)
    elseif t == MAGOTP_TYPE_STRINGS
        magotp_read_strings(T, io)
    elseif t == MAGOTP_TYPE_REALS
        magotp_read_reals(T, io)
    elseif t == MAGOTP_TYPE_STRING
        magotp_read_string(T, io)
    elseif t == MAGOTP_TYPE_LIST
        magotp_read_list(T, io)
    elseif t == MAGOTP_TYPE_SEQUENCE
        magotp_read_sequence(T, io)
    elseif t == MAGOTP_TYPE_SET
        magotp_read_set(T, io)
    else
        error("unknown type: $t")
    end::T
end

@generated _typeintersect(::Type{T1}, ::Type{T2}) where {T1,T2} = typeintersect(T1, T2)

"""
    magotp_read_object(::Type{T}, io::IO, u::MagmaOTPStr)

Read an object from `io` belonging to known structure `u`. Return it as a `T`.
"""
function magotp_read_object end

"""
    magotp_readtype(::Type{T}, u::MagmaOTPStr)

The return type of `magotp_read_object(T, ::IO, u)`.
"""
function magotp_readtype end

### NULL STRUCTURE

magotp_read_nullstr(::Type{T}, io::IO) where {T} =
    if MagmaOTPNullStr <: T
        MagmaOTPNullStr()
    else
        throw(MethodError(magotp_read_nullstr, (T, io)))
    end::T

### INTEGER RING

magotp_read_integers(::Type{T}, io::IO) where {T} =
    if MagmaOTPIntegers <: T
        MagmaOTPIntegers()
    else
        throw(MethodError(magotp_read_integers, (T, io)))
    end::T

### RATIONAL FIELD

magotp_read_rationals(::Type{T}, io::IO) where {T} =
    if MagmaOTPRationals <: T
        MagmaOTPRationals()
    else
        throw(MethodError(magotp_read_rationals, (T, io)))
    end::T

### REAL FIELD

magotp_read_reals(::Type{T}, io::IO) where {T} =
    if MagmaOTPReals <: T
        x = read(io, UInt8)
        x == 0x02 || error("not implemented")
        p = magotp_read_uinteger(Int, io)
        MagmaOTPReals(p)
    else
        throw(MethodError(magotp_read_reals, (T, io)))
    end::T

### STRING STRUCTURE

magotp_read_strings(::Type{T}, io::IO) where {T} =
    if MagmaOTPStrings <: T
        MagmaOTPStrings()
    else
        throw(MethodError(magotp_read_strings, (T, io)))
    end::T

### NULL

magotp_readtype(::Type{T}, u::MagmaOTPNullStr) where {T} =
    if Nothing <: T
        Nothing
    else
        Union{}
    end

magotp_read_null(::Type{T}, io::IO) where {T} = magotp_read_object(T, io, MagmaOTPNullStr())::T
magotp_read_object(::Type{T}, io::IO, u::MagmaOTPNullStr) where {T} = _magotp_read_null(magotp_readtype(T, u), io)::T
_magotp_read_null(::Type{Nothing}, io::IO) = nothing

### UNSIGNED INTEGER

magotp_read_uinteger(::Type{T}, io::IO) where {T} =
    if (S = _typeintersect(T, Integer)) != Union{}
        _magotp_read_uinteger(BigInt <: S ? BigInt : Int <: S ? Int : UInt <: S ? UInt : S, io)
    else
        throw(MethodError(magotp_read_uinteger, (T, io)))
    end::T

function _magotp_read_uinteger(::Type{T}, io::IO) where {T<:Integer}
    x₁ = read(io, UInt8)
    if x₁ ≤ 0xf7
        convert(T, x₁)
    elseif x₁ == 0xff
        x₂ = read(io, UInt8)
        convert(T, 0x00f8 + UInt16(x₂))
    elseif x₁ == 0xfe
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        convert(T, UInt16(x₂) | (UInt16(x₃) << 8))
    elseif x₁ == 0xfd
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        x₄ = read(io, UInt8)
        convert(T, UInt32(x₂) | (UInt32(x₃) << 8) | (UInt32(x₄) << 16))
    elseif x₁ == 0xfc
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        x₄ = read(io, UInt8)
        x₅ = read(io, UInt8)
        convert(T, UInt32(x₂) | (UInt32(x₃) << 8) | (UInt32(x₄) << 16) | (UInt32(x₅) << 24))
    else
        error("not implemented")
    end::T
end

### INTEGER

magotp_readtype(::Type{T}, u::MagmaOTPIntegers) where {T} =
    if (S = _typeintersect(T, Integer)) != Union{}
        BigInt <: S ? BigInt : Int <: S ? Int : UInt <: S ? UInt : S
    elseif (S = _typeintersect(T, Rational)) != Union{}
        Rational <: S ? Rational{BigInt} : S
    elseif (S = _typeintersect(T, Real)) != Union{}
        BigFloat <: S ? BigFloat : S
    elseif (S = _typeintersect(T, MagmaOTPInteger)) != Union{}
        MagmaOTPInteger <: S ? MagmaOTPInteger{magotp_readtype(Integer, u)} : S
    else
        throw(MethodError(magotp_readtype, (T, u)))
    end

magotp_read_integer(::Type{T}, io::IO) where {T} = magotp_read_object(T, io, MagmaOTPIntegers())::T
magotp_read_object(::Type{T}, io::IO, u::MagmaOTPIntegers) where {T} = _magotp_read_integer(magotp_readtype(T, u), io)::T

function _magotp_read_integer(::Type{T}, io::IO) where {T<:Integer}
    x₁ = read(io, UInt8)
    if x₁ ≤ 0x7f
        convert(T, x₁)
    elseif x₁ ≥ 0x85
        convert(T, reinterpret(Int8, x₁))
    elseif x₁ == 0x80
        x₂ = read(io, UInt8)
        if x₂ ≤ 0x7f
            convert(T, 0x80 + x₂)
        else
            convert(T, reinterpret(Int8, x₂) - Int(0x7b))
        end
    elseif x₁ == 0x81
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        x = UInt16(x₂) | (UInt16(x₃)<<8)
        if x₃ ≤ 0x7f
            convert(T, x)
        else
            convert(T, reinterpret(Int16, x))
        end
    elseif x₁ == 0x82
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        x₄ = read(io, UInt8)
        x = UInt32(x₂) | (UInt32(x₃)<<8) | (UInt32(x₄)<<16)
        if x₄ ≤ 0x7f
            convert(T, x)
        else
            convert(T, reinterpret(Int32, x | 0xff000000))
        end
    elseif x₁ == 0x83
        x₂ = read(io, UInt8)
        x₃ = read(io, UInt8)
        x₄ = read(io, UInt8)
        x₅ = read(io, UInt8)
        x = UInt32(x₂) | (UInt32(x₃)<<8) | (UInt32(x₄)<<16) | (UInt32(x₅)<<24)
        if x₅ ≤ 0x7f
            convert(T, x)
        else
            convert(T, reinterpret(Int32, x))
        end
    elseif x₁ == 0x84
        L = magotp_read_integer(Int, io)
        n = abs(L)
        if n ≤ sizeof(UInt64)
            x = zero(UInt64)
            for i in 1:n
                x |= UInt64(read(io, UInt8)) << (8*(i-1))
            end
            if L ≥ 0
                convert(T, x)
            elseif x ≤ typemax(Int)
                convert(T, -Int(x))
            else
                convert(T, -BigInt(x))
            end
        else
            # Too large to fit into 64 bits, read as a BigInt.
            # We do this by constructing a hex string, then parsing it.
            # This has memory overhead of 2x for the char array, plus 2x for the string.
            cs = UInt8[]
            for i in 1:n
                b = read(io, UInt8)
                c = b & 0x0f
                push!(cs, (c ≤ 0x09 ? 0x30 : 0x57) + c)
                c = b >> 4
                push!(cs, (c ≤ 0x09 ? 0x30 : 0x57) + c)
            end
            push!(cs, 0x78, 0x30)
            reverse!(cs)
            x = parse(BigInt, String(cs))
            if L ≥ 0
                convert(T, x)
            else
                convert(T, -x)
            end
        end
    else
        @assert false
    end::T
end

_magotp_read_integer(::Type{T}, io::IO) where {T<:MagmaOTPInteger} = T(magotp_read_integer(Integer, io))::T
_magotp_read_integer(::Type{MagmaOTPInteger{T}}, io::IO) where {T<:Integer} = MagmaOTPInteger{T}(magotp_read_integer(T, io))

### RATIONAL

magotp_readtype(::Type{T}, u::MagmaOTPRationals) where {T} =
    if (S = _typeintersect(T, Rational)) != Union{}
        Rational <: S ? Rational{BigInt} : S
    elseif (S = _typeintersect(T, Real)) != Union{}
        BigFloat <: S ? BigFloat : S
    elseif (S = _typeintersect(T, MagmaOTPRational)) != Union{}
        MagmaOTPRational <: S ? MagmaOTPRational{magotp_convertype(Rational, u)} : S
    else
        throw(MethodError(magotp_readtype, (T, u)))
    end

magotp_read_rational(::Type{T}, io::IO) where {T} = magotp_read_object(T, io, MagmaOTPRationals())::T
magotp_read_object(::Type{T}, io::IO, u::MagmaOTPRationals) where {T} = _magotp_read_rational(magotp_readtype(T, u), io)::T

function _magotp_read_rational(::Type{Rational{T}}, io::IO) where {T}
    x = _magotp_read_integer(T, io)
    if iseven(x)
        Rational{T}(fld(x, 2))
    else
        y = _magotp_read_integer(T, io)
        Rational{T}(fld(x, 2), y)
    end
end

### STRING

magotp_readtype(::Type{T}, u::MagmaOTPStrings) where {T} =
    if (S = _typeintersect(T, AbstractString)) != Union{}
        String <: S ? String : S
    elseif (S = _typeintersect(T, AbstractChar)) != Union{}
        Char <: S ? Char : S
    elseif (S = _typeintersect(T, AbstractVector{UInt8})) != Union{}
        Vector{UInt8} <: S ? Vector{UInt8} : S
    elseif (S = _typeintersect(T, MagmaOTPString)) != Union{}
        MagmaOTPString <: S ? MagmaOTPString{magotp_readtype(AbstractString, u)} : S
    else
        throw(MethodError(magotp_readtype, (T, u)))
    end

magotp_read_string(::Type{T}, io::IO) where {T} = magotp_read_object(T, io, MagmaOTPStrings())::T
magotp_read_object(::Type{T}, io::IO, u::MagmaOTPStrings) where {T} = _magotp_read_string(magotp_readtype(T, u), io)::T

function _magotp_read_string(::Type{Vector{UInt8}}, io::IO)
    r = UInt8[]
    n = magotp_read_uinteger(Int, io) - 1
    for i in 1:n
        push!(r, read(io, UInt8))
    end
    r
end

_magotp_read_string(::Type{T}, io::IO) where {T<:AbstractVector} = convert(T, magotp_read_string(Vector, io))
_magotp_read_string(::Type{String}, io::IO) = String(magotp_read_string(Vector, io))
_magotp_read_string(::Type{T}, io::IO) where {T<:AbstractString} = convert(T, magotp_read_string(String, io))
function _magotp_read_string(::Type{Char}, io::IO)
    n = magotp_read_uinteger(Int, io) - 1
    if n == 1
        return Char(read(io, UInt8))
    else
        error("can only convert strings of length 1 to a character, got length $n")
    end
end
_magotp_read_string(::Type{T}, io::IO) where {T<:AbstractChar} = convert(T, magotp_read_string(Char, io))
_magotp_read_string(::Type{MagmaOTPString{T}}, io::IO) where {T} = MagmaOTPString{T}(magotp_read_string(T, io))
_magotp_read_string(::Type{MagmaOTPString}, io::IO) = MagmaOTPString(magotp_read_string(String, io))

### LIST

magotp_read_list(::Type{T}, io::IO) where {T} =
    if (S = _typeintersect(T, Vector)) != Union{}
        _magotp_read_list(S, io)
    elseif (S = _typeintersect(T, AbstractVector)) != Union{}
        convert(S, magotp_read_list(Vector, io))
    elseif (S = _typeintersect(T, MagmaOTPList)) != Union{}
        _magotp_read_list(S, io)
    else
        throw(MethodError(magotp_read_list, (T, io)))
    end

function _magotp_read_list(::Type{T}, io::IO) where {T<:Vector}
    xs = T()
    n = magotp_read_uinteger(Int, io)
    for i in 1:n
        x = magotp_read_object(eltype(xs), io)
        push!(xs, x)
    end
    xs
end
_magotp_read_list(::Type{MagmaOTPList{T}}, io::IO) where {T} = MagmaOTPList{T}(magotp_read_list(T, io))
_magotp_read_list(::Type{MagmaOTPList}, io::IO) = MagmaOTPList(magotp_read_list(T, io))

### SEQUENCE

function magotp_read_sequence(::Type{T}, io::IO) where {T}
    u = magotp_read_object(MagmaOTPValue, io)
    if u === MagmaOTPNullStr()
        magotp_read_nullstr_sequence(T, io)
    else
        x = read(io, UInt8)
        if x == 0x01
            magotp_read_range_sequence(T, io, u)
        elseif x == 0x02
            magotp_read_general_sequence(T, io, u)
        else
            error("unknown sequence format: $x")
        end
    end::T
end

magotp_read_nullstr_sequence(::Type{T}, io::IO) where {T} =
    if (S = _typeintersect(T, Vector)) != Union{}
        S()
    elseif (S = _typeintersect(T, MagmaOTPNullSequence)) != Union{}
        S()
    else
        throw(MethodError(magotp_read_nullstr_sequence, (T, io)))
    end::T

magotp_read_general_sequence(::Type{T}, io::IO, u) where {T} =
    if (S = _typeintersect(T, Vector)) != Union{}
        _magotp_read_general_sequence(S, io, u)
    elseif (S = _typeintersect(T, AbstractVector)) != Union{}
        convert(S, magotp_read_general_sequence(Vector{eltype(S)}, io, u))
    elseif (S = _typeintersect(T, MagmaOTPSequence)) != Union{}
        _magotp_read_general_sequence(S, io, u)
    else
        throw(MethodError(magotp_read_general_sequence, (T, io, u)))
    end::T

function _magotp_read_general_sequence(::Type{T}, io::IO, u) where {T<:Union{Vector,Set}}
    r = T()
    n = magotp_read_uinteger(Int, io)
    for _ in 1:n
        push!(r, magotp_read_object(eltype(r), io, u))
    end
    r
end

_magotp_read_general_sequence(::Type{Vector}, io::IO, u) =
    _magotp_read_general_sequence(Vector{magotp_readtype(Any, u)}, io, u)

_magotp_read_general_sequence(::Type{Set}, io::IO, u) =
    _magotp_read_general_sequence(Set{magotp_readtype(Any, u)}, io, u)

_magotp_read_general_sequence(::Type{MagmaOTPSequence}, io::IO, u) =
    MagmaOTPSequence(u, magotp_read_general_sequence(Vector, io, u))

_magotp_read_general_sequence(::Type{MagmaOTPSequence{U}}, io::IO, u::U) where {U} =
    MagmaOTPSequence{U}(u, magotp_read_general_sequence(Vector, io, u))

_magotp_read_general_sequence(::Type{MagmaOTPSequence{U,V}}, io::IO, u::U) where {U,V} =
    MagmaOTPSequence{U,V}(u, magotp_read_general_sequence(V, io, u))

_magotp_read_general_sequence(::Type{MagmaOTPSequence{<:Any,V}}, io::IO, u::U) where {U,V} =
    MagmaOTPSequence{U,V}(u, magotp_read_general_sequence(V, io, u))

magotp_read_range_sequence(::Type{T}, io::IO, u) where {T} =
    if (S = _typeintersect(T, StepRangeLen)) != Union{}
        _magotp_read_range_sequence(S, io, u)
    elseif (S = _typeintersect(T, AbstractVector)) != Union{}
        convert(S, magotp_read_range_sequence(StepRangeLen, io, u))
    elseif (S = _typeintersect(T, MagmaOTPRangeSequence)) != Union{}
        _magotp_read_range_sequence(S, io, u)
    else
        throw(MethodError(magotp_read_range_sequence, (T, io, u)))
    end::T

function _magotp_read_range_sequence(::Type{StepRangeLen{T,R,S}}, io::IO, u) where {T,R,S}
    len = magotp_read_object(Int, io, u)
    start = magotp_read_object(R, io, u)
    step = magotp_read_object(S, io, u)
    StepRangeLen{T,R,S}(start, step, len)
end

_magotp_read_range_sequence(::Type{StepRangeLen{T}}, io::IO, u) where {T} =
    _magotp_read_range_sequence(StepRangeLen{T,T,T}, io, u)

_magotp_read_range_sequence(::Type{StepRangeLen}, io::IO, u) =
    _magotp_read_range_sequence(StepRangeLen{magotp_readtype(Integer, u)}, io, u)

_magotp_read_range_sequence(::Type{MagmaOTPRangeSequence{U,V}}, io::IO, u) where {U,V} =
    MagmaOTPRangeSequence{U,V}(u, magotp_read_range_sequence(V, io, u))

_magotp_read_range_sequence(::Type{MagmaOTPRangeSequence{_U,V} where {_U}}, io::IO, u::U) where {U,V} =
    _magotp_read_range_sequence(MagmaOTPRangeSequence{U,V}, io, u)

_magotp_read_range_sequence(::Type{MagmaOTPRangeSequence{U}}, io::IO, u) where {U} =
    MagmaOTPSequence{U}(u, magotp_read_range_sequence(StepRangeLen, io, u))

_magotp_read_range_sequence(::Type{MagmaOTPRangeSequence}, io::IO, u) =
    MagmaOTPSequence(u, magotp_read_range_sequence(StepRangeLen, io, u))

### SET

function magotp_read_set(::Type{T}, io::IO) where {T}
    u = magotp_read_object(MagmaOTPValue, io)
    if u === MagmaOTPNullStr()
        magotp_read_nullstr_set(T, io)
    else
        x = read(io, UInt8)
        if x == 0x01
            magotp_read_range_set(T, io, u)
        elseif x == 0x02
            magotp_read_general_set(T, io, u)
        else
            error("unknown set format: $x")
        end
    end::T
end

magotp_read_nullstr_set(::Type{T}, io::IO) where {T} =
    if (S = _typeintersect(T, Set)) != Union{}
        S()
    elseif (S = _typeintersect(T, Vector)) != Union{}
        S()
    elseif (S = _typeintersect(T, MagmaOTPNullSet)) != Union{}
        S()
    else
        throw(MethodError(magotp_read_nullstr_set, (T, io)))
    end::T

magotp_read_general_set(::Type{T}, io::IO, u) where {T} =
    if (S = _typeintersect(T, Set)) != Union{}
        _magotp_read_general_sequence(S, io, u)
    elseif (S = _typeintersect(T, AbstractSet)) != Union{}
        convert(S, magotp_read_general_set(Set{eltype(S)}, io, u))
    elseif (S = _typeintersect(T, Vector)) != Union{}
        _magotp_read_general_sequence(S, io, u)
    elseif (S = _typeintersect(T, AbstractVector)) != Union{}
        convert(S, magotp_read_general_set(Vector{eltype(S)}, io, u))
    elseif (S = _typeintersect(T, MagmaOTPSet)) != Union{}
        _magotp_read_general_set(S, io, u)
    else
        throw(MethodError(magotp_read_general_set, (T, io, u)))
    end::T

_magotp_read_general_set(::Type{MagmaOTPSet}, io::IO, u) =
    MagmaOTPSet(u, magotp_read_general_set(Vector, io, u))

_magotp_read_general_set(::Type{MagmaOTPSet{U}}, io::IO, u::U) where {U} =
    MagmaOTPSet{U}(u, magotp_read_general_set(Vector, io, u))

_magotp_read_general_set(::Type{MagmaOTPSet{U,V}}, io::IO, u::U) where {U,V} =
    MagmaOTPSet{U,V}(u, magotp_read_general_set(V, io, u))

_magotp_read_general_set(::Type{MagmaOTPSet{<:Any,V}}, io::IO, u::U) where {U,V} =
    MagmaOTPSet{U,V}(u, magotp_read_general_set(V, io, u))

magotp_read_range_set(::Type{T}, io::IO, u) where {T} =
    if (S = _typeintersect(T, StepRangeLen)) != Union{}
        _magotp_read_range_sequence(S, io, u)
    elseif (S = _typeintersect(T, Set)) != Union{}
        S(magotp_read_range_set(StepRangeLen, io, u))
    elseif (S = _typeintersect(T, AbstractSet)) != Union{}
        convert(S, magotp_read_range_set(Set, io, u))
    elseif (S = _typeintersect(T, AbstractVector)) != Union{}
        convert(S, magotp_read_range_set(StepRangeLen, io, u))
    elseif (S = _typeintersect(T, MagmaOTPRangeSet)) != Union{}
        _magotp_read_range_set(S, io, u)
    else
        throw(MethodError(magotp_read_range_set, (T, io, u)))
    end::T

_magotp_read_range_set(::Type{MagmaOTPRangeSet{U,V}}, io::IO, u) where {U,V} =
    MagmaOTPRangeSet{U,V}(u, magotp_read_range_set(V, io, u))

_magotp_read_range_set(::Type{MagmaOTPRangeSet{_U,V} where {_U}}, io::IO, u::U) where {U,V} =
    _magotp_read_range_set(MagmaOTPRangeSet{U,V}, io, u)

_magotp_read_range_set(::Type{MagmaOTPRangeSet{U}}, io::IO, u) where {U} =
    MagmaOTPSet{U}(u, magotp_read_range_set(StepRangeLen, io, u))

_magotp_read_range_set(::Type{MagmaOTPRangeSet}, io::IO, u) =
    MagmaOTPSet(u, magotp_read_range_set(StepRangeLen, io, u))
