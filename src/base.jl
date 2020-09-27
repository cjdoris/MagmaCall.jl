function Base.show(io::IO, o::MagmaObject)
    if magassigned(o)
        magprint(io, o, :magma)
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
Base.show(io::IO, o::MagmaCallable{N}) where {N} = (show(io, getfield(o,:o)); print(io, " :: MagmaCallable{$N}"))

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

for (m,b) in [(:mageq, :(==)), (:magne, :(!=)), (:magle, :(<=)), (:maglt, :(<)),
    (:magge, :(>=)), (:maggt, :(>)), (:magin, :in), (:magsubset, :issubset),
    (:magjoin, :union), (:magmeet, :intersect), (:magdiff, :setdiff), (:magdiv, :div),
    (:magmod, :mod), (:magsdiff, :symdiff), (:magadd, :+), (:magsub, :-), (:magmul, :*),
    (:magtruediv, :/), (:magpow, :^)]
    @eval Base.$b(x::MagmaValue, y::MagmaValue) = $m(x, y)
    @eval Base.$b(x::MagmaValue, y) = $m(x, y)
    @eval Base.$b(x, y::MagmaValue) = $m(x, y)
end

Base.getproperty(o::MagmaValue, k::Symbol) = maggetfield(o, k)
Base.setproperty!(o::MagmaObject, k::Symbol, v) = magsetfield!(o, k, v)
