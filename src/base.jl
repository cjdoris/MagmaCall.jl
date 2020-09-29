function Base.show(io::IO, o::MagmaObject)
    if magassigned(o)
        try
            magprint(io, o, :magma)
        catch
            print(io, "<magma object $(id(o))>")
        end
    else
        print(io, "<unassigned>")
    end
    if get(io, :typeinfo, Any) != typeof(o)
        print(io, " :: ", typeof(o))
    end
end

function Base.show(io::IO, ::MIME"text/plain", o::MagmaObject)
    if magassigned(o)
        interact() do p
            putcmd(p, "__jl_display(", varname(o), ")", err=true)
            lines = collect(eachlinetotoken(p, missing))
            h, w = displaysize(io)
            h = max(h-4, 5)
            w = max(w, 5)
            if length(lines) > h
                hpre = cld(h-1, 2)
                hpost = h-1-hpre
                lines = [lines[1:hpre]; " ..."; lines[end-hpost+1:end]]
            end
            nlines = 0
            for line in lines
                nlines == 0 || println(io)
                if length(line) > w
                    print(io, line[1:w-1], '…')
                else
                    print(io, line)
                end
                nlines += 1
            end
            checkerr(p)
        end
    else
        nlines = 1
        print(io, "<unassigned>")
    end
    if get(io, :typeinfo, Any) != typeof(o)
        nlines > 1 ? println(io) : print(io, ' ')
        print(io, ":: ", typeof(o))
    end
end

Base.show(io::IO, o::MagmaExpr) = print(io, valstr(o), " :: MagmaExpr")
Base.show(io::IO, o::MagmaIntrinsic) = print(io, valstr(o), " :: MagmaIntrinsic")
Base.show(io::IO, o::MagmaType) = print(io, valstr(o), " :: MagmaType")
Base.show(io::IO, o::MagmaCallable{N}) where {N} = (show(io, getfield(o,:o)); print(io, " :: MagmaCallable{$(repr(N))}"))

Base.print(io::IO, o::MagmaObject) = magprint(io, o)

Base.convert(::Type{MagmaValue}, x::MagmaValue) = x
Base.convert(::Type{MagmaValue}, x) = MagmaValue(x)

Base.convert(::Type{MagmaObject}, x::MagmaObject) = x
Base.convert(::Type{MagmaObject}, x) = MagmaObject(x)

Base.getindex(o::MagmaValue, idxs...) = maggetindex(o, idxs...)
Base.setindex!(o::MagmaObject, x, idxs...) = magsetindex!(o, x, idxs...)

Base.IteratorEltype(::Type{<:MagmaValue}) = Base.HasEltype()
Base.eltype(o::MagmaValue) = MagmaObject

Base.IteratorSize(::Type{<:MagmaValue}) = Base.SizeUnknown()

function Base.iterate(x::MagmaValue, st=nothing)
    # get the state
    if st === nothing
        y = magcallf(:__jl_indexable, x)
        n = magcalli(Symbol("#"), y)
        i = 1
    else
        y, n, i = st
    end
    # find the next defined value
    while i ≤ n
        if magcallb(:__jl_IsDefined, y, i)
            return maggetindex(y, i), (y, n, i+1)
        else
            i += 1
        end
    end
    # nothing left
    return nothing
end

# unary operators
for (m,b) in [(:magpos, :+), (:magneg, :-), (:magsum, :sum), (:magprod, :prod), (:magreduceand, :all), (:magreduceor, :any)]
    @eval Base.$b(x::MagmaValue) = $m(x)
end

# reductions
for (m,b) in [(:magreducejoin, :union), (:magreducemeet, :intersect), (:magreducecat, :cat)]
    @eval Base.reduce(::typeof($b), x) = $m(x)
end

# binary operators
for (m,b) in [(:mageq, :(==)), (:magne, :(!=)), (:magle, :(<=)), (:maglt, :(<)),
    (:magge, :(>=)), (:maggt, :(>)), (:magin, :in), (:magsubset, :issubset),
    (:magjoin, :union), (:magmeet, :intersect), (:magdiff, :setdiff), (:magdiv, :div),
    (:magmod, :mod), (:magsdiff, :symdiff), (:magadd, :+), (:magsub, :-), (:magmul, :*),
    (:magtruediv, :/), (:magpow, :^), (:magcat, :cat)]
    @eval Base.$b(x::MagmaValue, y::MagmaValue) = $m(x, y)
    @eval Base.$b(x::MagmaValue, y) = $m(x, y)
    @eval Base.$b(x, y::MagmaValue) = $m(x, y)
end

Base.getproperty(o::MagmaValue, k::Symbol) = maggetattr(o, k)
Base.setproperty!(o::MagmaObject, k::Symbol, v) = magsetattr!(o, k, v)
Base.propertynames(o::MagmaValue) = magattrnames(o)

Base.length(o::MagmaValue) = interact() do p
    putcmd(p, "__jl_tmp := '#'(", valstr(o), ");\nIsFinite(__jl_tmp)", err=true)
    ok = readtotoken(Bool, p, missing)
    checkerr(p)
    ok || error("not finite")
    putcmd(p, "__jl_tmp; delete __jl_tmp", err=true)
    n = readtotoken(Int, p, missing)
    checkerr(p)
    n
end
