struct MagmaRuntimeError <: Exception
    e :: MagmaObject
end

"""
    magassigned(o::MagmaObject)

True if `o` is assigned. This should only occur if `o = MagmaObject()` or if `magdelete(o)` has been called.

Most functions do not expect unassigned values, and undefined behaviour occurs.
"""
magassigned(o::MagmaObject) = interact() do p
    putcmd(p, "assigned ", varname(o))
    readtotoken(Bool, p, missing)
end
export magassigned

"""
    magdelete(o::MagmaObject)

Delete the object referenced by `o`.

Most functions do not expect deleted values, and undefined behaviour occurs.
"""
magdelete(o::MagmaObject) = interact() do p
    putcmd(p, "delete ", varname(o))
    readtotoken(Nothing, p, missing)
end
export magdelete

# binary relations
for op in [:eq, :ne, :cmpeq, :cmpne, :le, :lt, :ge, :gt, :in, :notin, :adj, :notadj, :subset, :notsubset]
    jop = Symbol(:mag, op)
    @eval function $jop(_x, _y)
        x = MagmaValue(_x)
        y = MagmaValue(_y)
        GC.@preserve x y interact() do p
            putcmd(p, "__jlans := ", valstr(x), $(" $op "), valstr(y), err=true)
            echototoken(p, missing)
            checkerr(p)
            putcmd(p, "__jlans")
            putcmd(p, "delete __jlans")
            readtotoken(Bool, p, missing)
        end
    end
    @eval export $jop
end

# binary operators
for (op,jop) in [(:diff, :magdiff), (:div, :magdiv), (:join, :magjoin), (:meet, :magmeet), (:mod, :magmod), (:sdiff, :magsdiff), (:+, :magadd), (:-, :magsub), (:*, :magmul), (:/, :magtruediv), (:^, :magpow)]
    @eval function $jop(_x, _y)
        x = MagmaValue(_x)
        y = MagmaValue(_y)
        r = MagmaObject()
        GC.@preserve x y interact() do p
            putcmd(p, varname(r), " := ", valstr(x), $(" $op "), valstr(y), err=true)
            echototoken(p, missing)
            checkerr(p)
        end
        r
    end
    @eval export $jop
end

# containers without universe
for (a, b, jop) in [("[*", "*]", :maglist), ("<", ">", :magtuple)]
    @eval function $jop(_xs=())
        xs = _xs isa Tuple ? map(MagmaValue, _xs) : [MagmaValue(x) for x in _xs]
        xv = join(map(valstr, xs), ", ")
        r = MagmaObject()
        GC.@preserve xs interact() do p
            putcmd(p, varname(r), " := ", $a, xv, $b, err=true)
            readtotoken(Nothing, p, missing)
            checkerr(p)
        end
        r
    end
    @eval export $jop
end

# containers with universe
for (a, b, jop, add) in [("[", "]", :magseq, "Append"), ("{", "}", :magset, "Include"), ("{*", "*}", :magmset, "Include"), ("{@", "@}", :magiset, "Include")]
    @eval function $jop(_xs=(); universe=nothing)
        r = MagmaObject()
        if _xs===()
            xs = ()
            av = ""
        else
            xs = maglist(_xs)
            av = string(";\nfor x in ", valstr(xs), " do\n", $add, "(~", varname(r), ", x);\nend for")
        end
        if universe===nothing
            u = nothing
            uv = ""
        else
            u = MagmaValue(universe)
            uv = string(valstr(u), " | ")
        end
        GC.@preserve xs u interact() do p
            putcmd(p, varname(r), " := ", $a, uv, $b, av, err=true)
            readtotoken(Nothing, p, missing)
            checkerr(p)
        end
        r
    end
    @eval export $jop
end

"""
    magprint(io::IO, x, level=:default)

Print `x` to `io` at the given `level`, which must be one of `:default`, `:magma`, `:maximal` or `:minimal`.
"""
function magprint(io::IO, _x, level::Symbol=:default)
    x = MagmaValue(_x)
    lv =
        level in (:default      , :Default) ? "Default" :
        level in (:mag, :magma  , :Magma  ) ? "Magma"   :
        level in (:max, :maximal, :Maximal) ? "Maximal" :
        level in (:min, :minimal, :Minimal) ? "Minimal" :
        error("invalid printing level: $level")
    GC.@preserve x interact() do p
        putcmd(p, valstr(x), ":", lv, err=true, stdout=io)
        nlines = 0
        for line in eachlinetotoken(p, missing)
            nlines == 0 || println(io)
            nlines += 1
            print(io, line)
        end
        checkerr(p)
    end
end
export magprint

"""
    magcall(Val(N), f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options.

If `N==0` then `f` is called as an intrinsic and nothing is returned.

Otherwise, `f` is called as a function with `N` return values. If `N==1`, the single return value is returned, otherwise a tuple is returned.
"""
function magcall(::Val{N}, _f, _args...; _opts...) where {N}
    f = intrarg(_f)
    args = map(MagmaValue, _args)
    argsstr = join((argstr(arg) for arg in args), ", ")
    if isempty(_opts)
        opts = nothing
        optsstr = ""
    else
        opts = Dict(k=>MagmaValue(v) for (k,v) in _opts)
        optsstr = string(" : ", join(("$k:=$(valstr(v))" for (k,v) in opts), ", "))
    end
    rs = ntuple(i->MagmaObject(), Val(N))
    lhs = N==0 ? "" : string(join(map(varname, rs), ", "), " := ")
    GC.@preserve f args opts interact() do p
        putcmd(p, lhs, valstr(f), "(", argsstr, optsstr, ")", err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    N==0 ? nothing : N==1 ? rs[1] : rs
end
function magcall(::Type{T}, f, args...; opts...) where {T}
    r = magcall(Val(1), f, args...; opts...)
    GC.@preserve r interact() do p
        putcmd(p, varname(r), err=true)
        v = readtotoken(T, p, missing)
        checkerr(p)
        v
    end
end
export magcall

"""
    magcallp(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options.
"""
magcallp(f, args...; opts...) = magcall(Val(0), f, args...; opts...)
export magcallp

"""
    magcallf(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first return value.
"""
magcallf(f, args...; opts...) = magcall(Val(1), f, args...; opts...)
export magcallf

"""
    magcallf2(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first two return values.
"""
magcallf2(f, args...; opts...) = magcall(Val(2), f, args...; opts...)
export magcallf2

"""
    magcallf3(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first three return values.
"""
magcallf3(f, args...; opts...) = magcall(Val(3), f, args...; opts...)
export magcallf3

"""
    magcallf4(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first four return values.
"""
magcallf4(f, args...; opts...) = magcall(Val(4), f, args...; opts...)
export magcallf4

"""
    magcallf5(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first five return values.
"""
magcallf5(f, args...; opts...) = magcall(Val(5), f, args...; opts...)
export magcallf5

"""
    magcallf6(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first six return values.
"""
magcallf6(f, args...; opts...) = magcall(Val(6), f, args...; opts...)
export magcallf6

"""
    magcallf7(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first seven return values.
"""
magcallf7(f, args...; opts...) = magcall(Val(7), f, args...; opts...)
export magcallf7

"""
    magcallf8(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first eight return values.
"""
magcallf8(f, args...; opts...) = magcall(Val(8), f, args...; opts...)
export magcallf8

"""
    magcalli(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. It must return an integer, which is returned.
"""
magcalli(f, args...; opts...) = magcall(Int, f, args...; opts...)
export magcalli

"""
    magcallb(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. It must return a boolean, which is returned.
"""
magcallb(f, args...; opts...) = magcall(Bool, f, args...; opts...)
export magcallb

"""
    magcallg(names, f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Assign the given `names` to the result. Return a tuple of the result and the generators.
"""
function magcallg(names::Tuple, f, args...; opts...)
    R = magcallf(f, args...; opts...)
    magcallp(:AssignNames, MagmaRef(R), magseq(map(x->x isa Symbol ? string(x) : x, names)))
    (R, ntuple(i->magcallf(:Name, R, i), length(names))...)
end
magcallg(name::Union{String,Symbol}, f, args...; opts...) =
    magcallg((string(name),), f, args...; opts...)
export magcallg

"""
    maggetindex(o, i, ...)

Equivalent to `o[i, ...]` in Magma.
"""
function maggetindex(_o, _idxs...)
    o = MagmaValue(_o)
    idxs = map(MagmaValue, _idxs)
    r = MagmaObject()
    iv = join(map(valstr, idxs), ", ")
    GC.@preserve o idxs interact() do p
        putcmd(p, varname(r), " := ", valstr(o), "[", iv, "]", err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export maggetindex

"""
    magsetindex!(o, x, i...)

Equivalent to `o[i, ...] := x` in Magma.
"""
function magsetindex!(o::MagmaObject, _x, _idxs...)
    x = MagmaValue(_x)
    idxs = map(MagmaValue, _idxs)
    iv = join(map(valstr, idxs), ", ")
    GC.@preserve o x idxs interact() do p
        putcmd(p, varname(o), "[", iv, "] := ", valstr(x), err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    o
end
export magsetindex!

"""
    maghelp(query)

Equivalent to `? query`.
"""
function maghelp(_q="")
    q = strip(string(_q))
    if isempty(q)
        q = "/"
    end
    interact() do p
        putcmd(p, "?", q)
        echototoken(p, missing)
    end
end
export maghelp

"""
    magtypeof(o)

The Magma type of `o`.
"""
magtypeof(o) = magcallf(:Type, o)
export magtypeof

"""
    magetypeof(o)

The Magma extended type of `o`.
"""
magetypeof(o) = magcallf(:ExtendedType, o)
export magetypeof

"""
    magissubtype(t1, t2)

True if Magma type `t1` is a subtype of `t2`.

`Symbol` arguments are interpreted as types.
"""
magissubtype(t1, t2) = magcallb(:ISA, typearg(t1), typearg(t2))
export magissubtype

"""
    magisinstance(o, t)

True if `o` is an instance of Magma type `t`.

If `t` is a `Symbol`, it is interpreted as a type.
"""
magisinstance(o, t) = magissubtype(magetypeof(o), typearg(t))
export magisinstance

"""
    mageval(ex; vars...)

Evaluate the Magma expression `ex` and return its value.

Keyword arguments can be used to name variables refereced by `ex`, e.g. `mageval("x+x", x=3)` returns `6`.

This is the worker function behind [`@mag_str`](@ref).
"""
function mageval(_ex; _vars...)
    r = MagmaObject()
    ex = MagmaValue(_ex)
    if isempty(_vars)
        vars = nothing
        vv = ""
        dv = ""
    else
        vars = Dict(k=>MagmaValue(v) for (k,v) in _vars)
        vv = string(join((string(k, " := ", valstr(v)) for (k,v) in vars), ";\n"), ";\n")
        dv = string(";\n", join((string("delete ", k) for (k,v) in vars), ";\n"))
    end
    GC.@preserve ex vars interact() do p
        putcmd(p, vv, varname(r), " := eval ", valstr(ex), dv, err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export mageval

"""
    mag"..."

Evaluates the given string as a Magma expression.

Julia values may be interpolated with `\$` as usual.
"""
macro mag_str(ex::String)
    chunks = String[]
    kws = []
    while !isempty(ex)
        i = findfirst('$', ex)
        if i === nothing
            push!(chunks, ex)
            ex = ""
        elseif !checkbounds(Bool, ex, i+1)
            push!(chunks, ex)
            ex = ""
        elseif ex[i+1] == '$'
            push!(chunks, ex[1:i])
            ex = ex[i+2:end]
        else
            push!(chunks, ex[1:i-1])
            jex, i = Meta.parse(ex, i+1, greedy=false)
            ex = ex[i:end]
            k = Symbol("__jlarg_", length(kws)+1)
            push!(chunks, string("(", k, ")"))
            push!(kws, Expr(:kw, k, esc(jex)))
        end
    end
    :(mageval($(join(chunks)); $(kws...)))
end
export @mag_str

"""
    maglength(o)

The length of `o`, equivalent to `# o`.
"""
maglength(o) = magcallf(Symbol("#"), o)
export maglength

"""
    magimage(x, f)

The image of `x` under `f`, equivalent to `x @ f`.
"""
magimage(x, f) = magcallf(Symbol("@"), x, f)
export magimage

"""
    magpreimage(x, f)

The preimage of `x` under `f`, equivalent to `x @@ f`.
"""
magpreimage(x, f) = magcallf(Symbol("@@"), x, f)
export magpreimage

"""
    magcoerce(s, x)

Coerce `x` into structure `s`, equivalent to `s ! x`.
"""
magcoerce(s, x) = magcallf(Symbol("!"), s, x)
export magcoerce

"""
    maggen(x, n)

The `n`th generator of `x`, equivalent to `x.n`.
"""
maggen(x, n) = magcallf(Symbol("."), x, n)
export maggen

struct MagmaTypes end
const magt = MagmaTypes()
export magt
Base.getproperty(::MagmaTypes, k::Symbol) = MagmaType(k)

struct MagmaIntrinsics{N} end
const magp = MagmaIntrinsics{0}()
const magf = MagmaIntrinsics{1}()
const magf2 = MagmaIntrinsics{2}()
const magf3 = MagmaIntrinsics{3}()
const magf4 = MagmaIntrinsics{4}()
const magf5 = MagmaIntrinsics{5}()
const magf6 = MagmaIntrinsics{6}()
const magf7 = MagmaIntrinsics{7}()
const magf8 = MagmaIntrinsics{8}()
const magg = MagmaIntrinsics{:g}()
export magp, magf, magf2, magf3, magf4, magf5, magf6, magf7, magf8, magg
Base.getproperty(::MagmaIntrinsics{N}, k::Symbol) where {N} = MagmaCallable{N}(MagmaIntrinsic(k))
Base.getproperty(::MagmaIntrinsics{:g}, k::Symbol) = (names, args...; opts...) -> magcallg(names, k, args...; opts...)

"""
    magrecformat(field, ...)

Create a record format with the given fields.

Each `field` may be a symbol, giving the name, or a `name=>type` pair.
"""
function magrecformat(_fields...)
    r = MagmaObject()
    fields = map(_fields) do f
        if f isa Symbol
            (f, nothing)
        elseif f isa Pair{Symbol, <:Any}
            (f.first, typearg(f.second))
        else
            error("bad format field: $(repr(f))")
        end
    end
    v = join((t===nothing ? string(n) : string(n, " : ", valstr(t)) for (n,t) in fields), ", ")
    interact() do p
        putcmd(p, varname(r), " := recformat< ", v, " >", err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export magrecformat

"""
    magrec(fmt; vals...)

A record with the given format and values.
"""
function magrec(_fmt; _vals...)
    fmt = MagmaValue(_fmt)
    vals = Dict(k=>MagmaValue(v) for (k,v) in _vals)
    r = MagmaObject()
    interact() do p
        putcmd(p, varname(r), " := rec< ", valstr(fmt), " | ", join((string(k, " := ", valstr(v)) for (k,v) in vals), ", "), " >", err=true)
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export magrec

"""
    maggetfield(x, k)

Get field `k` from `x`, equivalent to ```x`k```.
"""
function maggetfield(_x, _k)
    x = MagmaValue(_x)
    k = MagmaValue(_k isa Symbol ? string(_k) : _k)
    r = MagmaObject()
    interact() do p
        putcmd(p, varname(r), " := ", valstr(x), "``", valstr(k))
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export maggetfield

"""
    magsetfield!(x, k, v)

Set field `k` of `x` to `v`, equivalent to ```x`k := v```.
"""
function magsetfield!(x::MagmaObject, _k, _v)
    k = MagmaValue(_k isa Symbol ? string(_k) : _k)
    v = MagmaValue(_v)
    interact() do p
        putcmd(p, valstr(x), "``", valstr(k), " := ", valstr(v))
        echototoken(p, missing)
        checkerr(p)
    end
    x
end
export magsetfield!
