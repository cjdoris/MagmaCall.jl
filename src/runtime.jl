struct MagmaRuntimeError <: Exception
    e :: MagmaObject
end

function Base.showerror(io::IO, e::MagmaRuntimeError)
    print(io, "Magma: ")
    interact() do p
        putcmd(p, "__jl_showerror(", valstr(e.e), ")")
        n = 0
        for line in eachlinetotoken(p, missing)
            n += 1
            n == 1 || println(io)
            print(io, line)
        end
        checkerr(p)
    end
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
    doc = """
        $jop(x, y)

    True if `x $op y` in Magma.
    """
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
    @eval @doc $doc $jop
    @eval export $jop
end

# binary operators
for (op,jop) in [("diff", :magdiff), ("div", :magdiv), ("join", :magjoin), ("meet", :magmeet), ("mod", :magmod), ("sdiff", :magsdiff), ("cat", :magcat), ("+", :magadd), ("-", :magsub), ("*", :magmul), ("/", :magtruediv), ("^", :magpow)]
    doc = """
        $jop(x, y)

    Equivalent to `x $op y` in Magma.
    """
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
    @eval @doc $doc $jop
    @eval export $jop
end

# unary operators
for (op,jop) in [("+", :magpos), ("-", :magneg), ("&+", :magsum), ("&*", :magprod), ("&and", :magreduceand), ("&or", :magreduceor), ("&join", :magreducejoin), ("&meet", :magreducemeet), ("&cat", :magreducecat)]
    doc = """
        $jop(x)

    Equivalent to `$op x` in Magma.
    """
    @eval function $jop(_x)
        x = MagmaValue(_x)
        r = MagmaObject()
        GC.@preserve x interact() do p
            putcmd(p, varname(r), " := ", $op, "(", valstr(x), ")", err=true)
            echototoken(p, missing)
            checkerr(p)
        end
        r
    end
    @eval @doc $doc $jop
    @eval export $jop
end

# containers without universe
for (a, b, jop, what) in [("[*", "*]", :maglist, "list"), ("<", ">", :magtuple, "tuple")]
    doc = """
        $jop([xs])

    A new Magma $what, equivalent to `$a $b`.

    Optionally, values can be specified with iterable `xs`.
    """
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
    @eval @doc $doc $jop
    @eval export $jop
end

# containers with universe
for (a, b, jop, add, what, rangeok) in [("[", "]", :magseq, "Append", "sequence", true), ("{", "}", :magset, "Include", "set", true), ("{*", "*}", :magmset, "Include", "multiset", false), ("{@", "@}", :magiset, "Include", "indexed set", false)]
    doc = """
        $jop([xs]; [universe])

    A new Magma $what, equivalent to `$a universe | $b`.

    Optionally, values can be specified with iterable `xs`.
    """
    @eval function $jop(_xs=(); universe=nothing)
        r = MagmaObject()
        if _xs===()
            xs = nothing
            ev = pv = ""
        elseif $rangeok && _xs isa AbstractUnitRange{<:Integer}
            xs = nothing
            ev = "$(magliteralstr(first(_xs))) .. $(magliteralstr(last(_xs))) by $(magliteralstr(step(_xs))) "
            pv = ""
        else
            xs = maglist(_xs)
            pv = string(";\nfor x in ", valstr(xs), " do\n", $add, "(~", varname(r), ", x);\nend for")
            ev = ""
        end
        if universe===nothing
            u = nothing
            uv = ""
        else
            u = MagmaValue(universe)
            uv = string(";\nChangeUniverse(~", varname(r), ", ", valstr(u), ")")
        end
        GC.@preserve xs u interact() do p
            putcmd(p, varname(r), " := ", $a, " ", ev, " ", $b, pv, uv, err=true)
            readtotoken(Nothing, p, missing)
            checkerr(p)
        end
        r
    end
    @eval @doc $doc $jop
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
    magsprint(x, level=:default)

String representation of `x` at the given `level` (see [`magprint`](@ref)).
"""
magsprint(x, level::Symbol=:default) = sprint(magprint, x, level)
export magsprint

"""
    magcall(Val(C), f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options according to calling convention `C`.

If `f` is a `Symbol`, then it is interpreted as the name of a Magma intrinsic.

`C` may be:
* `:p`: `f` is a procedure and `nothing` is returned. Note that if `f` is actually a function, its return value will be printed.
* `:f`: `f` is a function and its first return value is returned.
* `:fN`: `f` is a function and its first `N` return values are returned as a tuple.
* `:b`: `f` is a function returning a boolean, which is returned as a `Bool`.
* `:i`: `f` is a function returning an integer, which is returned as an `Int`.
* `:g`: `f` is a function returning a structure `S`. `args[1]` is a tuple of generator names, which are applied to `S`. Returns a tuple of `S` and generator names.
* `:pN`: Same as `:p` (procedure) but the `N`th argument is passed by reference.

Equivalently, you can call `magcallC(f, args...; opts...)` or `magC.f(args...; opts...)`.
"""
function magcall(::Val{C}, _f, _args...; _opts...) where {C}
    if C in (:p, :f, :f2, :f3, :f4, :f5, :f6, :f7, :f8)
        N = C==:p ? 0 : C==:f ? 1 : C==:f2 ? 2 : C==:f3 ? 3 : C==:f4 ? 4 : C==:f5 ? 5 : C==:f6 ? 6 : C==:f7 ? 7 : C==:f8 ? 8 : error()
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
        return N==0 ? nothing : N==1 ? rs[1] : rs
    elseif C in (:i, :b)
        T = C==:i ? Int : C==:b ? Bool : error()
        r = magcall(Val(:f), _f, _args...; _opts...)
        GC.@preserve r interact() do p
            putcmd(p, varname(r), err=true)
            v = readtotoken(T, p, missing)
            checkerr(p)
            v
        end
    elseif C == :m
        ok, r = magcall(Val(:f2), _f, _args...; _opts...)
        magboolconvert(Bool, ok) ? r : nothing
    else
        error("Invalid magcall calling convention: $(repr(C))")
    end
end
export magcall
const CALLING_CONVENTIONS = (:p, :f, :f2, :f3, :f4, :f5, :f6, :f7, :f8, :b, :i, :g, :p1, :p2, :p3, :p4, :m)

"""
    magcallp(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options.

Equivalent to `magcall(Val(:p), f, args...; opts...)`.
"""
magcallp(f, args...; opts...) = magcall(Val(:p), f, args...; opts...)
export magcallp

"""
    magcallf(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first return value.

Equivalent to `magcall(Val(:f), f, args...; opts...)`.
"""
magcallf(f, args...; opts...) = magcall(Val(:f), f, args...; opts...)
export magcallf

"""
    magcallf2(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first two return values.

Equivalent to `magcall(Val(:f2), f, args...; opts...)`.
"""
magcallf2(f, args...; opts...) = magcall(Val(:f2), f, args...; opts...)
export magcallf2

"""
    magcallf3(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first three return values.

Equivalent to `magcall(Val(:f3), f, args...; opts...)`.
"""
magcallf3(f, args...; opts...) = magcall(Val(:f3), f, args...; opts...)
export magcallf3

"""
    magcallf4(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first four return values.

Equivalent to `magcall(Val(:f4), f, args...; opts...)`.
"""
magcallf4(f, args...; opts...) = magcall(Val(:f4), f, args...; opts...)
export magcallf4

"""
    magcallf5(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first five return values.

Equivalent to `magcall(Val(:f5), f, args...; opts...)`.
"""
magcallf5(f, args...; opts...) = magcall(Val(:f5), f, args...; opts...)
export magcallf5

"""
    magcallf6(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first six return values.

Equivalent to `magcall(Val(:f6), f, args...; opts...)`.
"""
magcallf6(f, args...; opts...) = magcall(Val(:f6), f, args...; opts...)
export magcallf6

"""
    magcallf7(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first seven return values.

Equivalent to `magcall(Val(:f7), f, args...; opts...)`.
"""
magcallf7(f, args...; opts...) = magcall(Val(:f7), f, args...; opts...)
export magcallf7

"""
    magcallf8(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Return the first eight return values.

Equivalent to `magcall(Val(:f8), f, args...; opts...)`.
"""
magcallf8(f, args...; opts...) = magcall(Val(:f8), f, args...; opts...)
export magcallf8

"""
    magcalli(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. It must return an integer, which is returned as an `Int`.

Equivalent to `magcall(Val(:i), f, args...; opts...)`.
"""
magcalli(f, args...; opts...) = magcall(Val(:i), f, args...; opts...)
export magcalli

"""
    magcallb(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. It must return a boolean, which is returned as a `Bool`.

Equivalent to `magcall(Val(:b), f, args...; opts...)`.
"""
magcallb(f, args...; opts...) = magcall(Val(:b), f, args...; opts...)
export magcallb

"""
    magcallg(f, names, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. Assign the given `names` to the result. Return a tuple of the result and the generators.

Equivalent to `magcall(Val(:g), f, names, args...; opts...)`.
"""
magcallg(f, args...; opts...) = magcall(Val(:g), f, args...; opts...)
function magcall(::Val{:g}, f, _names::Union{Tuple,String,Symbol}, args...; opts...)
    names = _names isa Union{String,Symbol} ? (_names,) : _names
    R = magcallf(f, args...; opts...)
    magcallp(:AssignNames, MagmaRef(R), magseq(map(x->x isa Symbol ? string(x) : x, names)))
    (R, ntuple(i->magcallf(:Name, R, i), length(names))...)
end
export magcallg

"""
    magcallp1(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options, passing the first
argument by reference.

Equivalent to `magcallp1(Val(:p1), args...; opts...)`.
"""
magcallp1(f, args...; opts...) = magcall(Val(:p1), f, args...; opts...)
magcall(::Val{:p1}, f, arg1, args...; opts...) = magcallp(f, MagmaRef(arg1), args...; opts...)
export magcallp1

"""
    magcallp2(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options, passing the second
argument by reference.

Equivalent to `magcallp2(Val(:p2), args...; opts...)`.
"""
magcallp2(f, args...; opts...) = magcall(Val(:p2), f, args...; opts...)
magcall(::Val{:p2}, f, arg1, arg2, args...; opts...) = magcallp(f, arg1, MagmaRef(arg2), args...; opts...)
export magcallp2

"""
    magcallp3(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options, passing the third
argument by reference.

Equivalent to `magcallp3(Val(:p3), args...; opts...)`.
"""
magcallp3(f, args...; opts...) = magcall(Val(:p3), f, args...; opts...)
magcall(::Val{:p3}, f, arg1, arg2, arg3, args...; opts...) = magcallp(f, arg1, arg2, MagmaRef(arg3), args...; opts...)
export magcallp3

"""
    magcallp4(f, args...; opts...)

Call the Magma procedure `f` with the given arguments and keyword options, passing the fourth
argument by reference.

Equivalent to `magcallp4(Val(:p4), args...; opts...)`.
"""
magcallp4(f, args...; opts...) = magcall(Val(:p4), f, args...; opts...)
magcall(::Val{:p4}, f, arg1, arg2, arg3, arg4, args...; opts...) = magcallp(f, arg1, arg2, arg3, MagmaRef(arg4), args...; opts...)
export magcallp4

"""
    magcallm(f, args...; opts...)

Call the Magma function `f` with the given arguments and keyword options. If the first
return value is true, return the second return value, else return `nothing`.

Equivalent to `magcall(Val(:m), args...; opts...)`.
"""
magcallm(f, args...; opts...) = magcall(Val(:m), f, args...; opts...)
export magcallm

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
    mag"..."[C]

Evaluates the given string as a Magma expression.

Julia values may be interpolated with `\$` as usual.

If `C` is given, the returned value is wrapped as a `MagmaCallable{C}`, for example:
```
f = mag"func<x, y | x^2 + 3*x*y + y^2>"f
f(2, 3)
```
"""
macro mag_str(ex::String, C::String="")
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
    r = :(mageval($(join(chunks)); $(kws...)))
    if C == ""
        return r
    elseif Symbol(C) in CALLING_CONVENTIONS
        return :($(MagmaCallable{Symbol(C)})($r))
    else
        error("invalid postfix on `mag\"...\"` macro: $(repr(C))")
    end
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
Base.getproperty(::MagmaTypes, k::Symbol) = MagmaType(k)
function Base.propertynames(::MagmaTypes)
    r = Symbol[]
    interact() do p
        putcmd(p, "ListTypes()")
        for line in eachlinetotoken(p, missing)
            for t in split(strip(line))
                push!(r, Symbol(t))
            end
        end
        checkerr(p)
    end
    r
end
const magt = MagmaTypes()
export magt

struct MagmaIntrinsics{C} end
Base.getproperty(::MagmaIntrinsics{C}, k::Symbol) where {C} = MagmaCallable{C}(MagmaIntrinsic(k))
for C in CALLING_CONVENTIONS
    j = Symbol(:mag, C)
    @eval const $j = $(MagmaIntrinsics{C}())
    @eval export $j
end
function Base.propertynames(::MagmaIntrinsics)
    r = Symbol[]
    interact() do p
        putcmd(p, "ListSignatures(Any)")
        for line in eachlinetotoken(p, missing)
            m = match(r"^\s*([a-zA-Z0-9_]+)\s*\(", line)
            if m !== nothing
                push!(r, Symbol(m.captures[1]))
            end
        end
        checkerr(p)
    end
    r
end

"""
    magrecformat(field, ...)

Create a record format with the given fields.

Each `field` may be a symbol, giving the name, or a `name=>type` pair.
"""
function magrecformat(_fields...; cache=false)
    cache && return get!(()->magrecformat(_fields...; cache=false), MAGRECFORMAT_CACHE, _fields)
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
const MAGRECFORMAT_CACHE = Dict{Any,MagmaObject}()
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
    maggetattr(x, k)

Get attribute `k` from `x`, equivalent to ```x`k```.
"""
function maggetattr(_x, _k)
    x = MagmaValue(_x)
    k = MagmaValue(_k isa Symbol ? string(_k) : _k)
    r = MagmaObject()
    GC.@preserve x k interact() do p
        putcmd(p, varname(r), " := ", valstr(x), "``", valstr(k))
        echototoken(p, missing)
        checkerr(p)
    end
    r
end
export maggetattr

"""
    magsetattr!(x, k, v)

Set attribute `k` of `x` to `v`, equivalent to ```x`k := v```.
"""
function magsetattr!(x::MagmaObject, _k, _v)
    k = MagmaValue(_k isa Symbol ? string(_k) : _k)
    v = MagmaValue(_v)
    GC.@preserve x k v interact() do p
        putcmd(p, valstr(x), "``", valstr(k), " := ", valstr(v))
        echototoken(p, missing)
        checkerr(p)
    end
    x
end
export magsetattr!

"""
    magattrnames(x)

The attribute names of x.
"""
function magattrnames(_x)
    x = MagmaValue(_x)
    r = Symbol[]
    GC.@preserve x interact() do p
        putcmd(p, "__jl_ListFields(", valstr(x), ")")
        for line in eachlinetotoken(p, missing)
            for x in split(strip(line))
                push!(r, Symbol(x))
            end
        end
        checkerr(p)
    end
    sort!(r)
    unique!(r)
end
