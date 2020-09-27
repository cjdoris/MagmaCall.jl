const EXE = "magma"
const PROC = Ref{Base.Process}()
const RUNNING = Ref(false)
const GIL = Channel{Nothing}(1)
const LASTID = Ref(0)
const VARPREFIX = "__jlvar_"
const COLUMNS = Ref(-1)
const STDOUT = Ref{IO}()

function __init__()
    PROC[] = open(`$EXE -b`, "r+")
    RUNNING[] = true
    STDOUT[] = stdout
    put!(GIL, nothing)
    skiptotoken(PROC[], missing)
    println(PROC[], "Attach($(repr(joinpath(@__DIR__, "intrinsics.mag"))));")
    echototoken(PROC[], missing)
    atexit() do
        RUNNING[] = false
        kill(PROC[])
    end
end

@inline function interact(f)
    take!(GIL)
    try
        return f(PROC[])
    finally
        put!(GIL, nothing)
    end
end

token() = randstring(12)

puttoken(io::IO, tok::String=token()) = (println(io, "\"", tok, "\";"); tok)

function putcmd(io::IO, args...; err=false, stdout=STDOUT[])
    if stdout !== nothing
        h, w = displaysize(stdout)
        if w != COLUMNS[]
            COLUMNS[] = w
            println(io, "SetColumns($w);")
        end
    end
    err && println(io, "try")
    println(io, args..., ";")
    err && println(io, "catch e; __jlerr := e; end try;")
end

function checkerr(io::IO)
    putcmd(io, "assigned __jlerr")
    if readtotoken(Bool, io, missing)
        e = MagmaObject()
        putcmd(io, varname(e), ":=__jlerr")
        putcmd(io, "delete __jlerr")
        throw(MagmaRuntimeError(e))
    end
end

struct EachLineToToken{IO}
    io :: IO
    tok :: String
end
function Base.iterate(xs::EachLineToToken, st=nothing)
    eof(xs.io) && error("unexpected end of output")
    x = readline(xs.io)
    if x == xs.tok
        return nothing
    else
        return x, nothing
    end
end
Base.IteratorSize(::Type{<:EachLineToToken}) = Base.SizeUnknown()

struct MagmaUnexpectedError <: Exception
    want
    got
end

eachlinetotoken(io::IO, tok::String) = EachLineToToken(io, tok)
eachlinetotoken(io::IO, ::Missing) =
    let tok = token()
        puttoken(io, tok)
        eachlinetotoken(io, tok)
    end
skiptotoken(io::IO, tok) =
    for x in eachlinetotoken(io, tok)
        # do nothing
    end
echototoken(io::IO, tok, stdout=STDOUT[]) =
    for x in eachlinetotoken(io, tok)
        println(stdout, x)
    end
readtotoken(::Type{Nothing}, io::IO, tok) =
    for x in eachlinetotoken(io, tok)
        if !isempty(x)
            skiptotoken(io, tok)
            throw(MagmaUnexpectedError("nothing", x))
        end
    end
readtotoken(::Type{String}, io::IO, tok) = join(eachlinetotoken(io, tok), "\n")
readtotoken(io::IO, tok) = readtotoken(String, io, tok)
readtotoken(::Type{Bool}, io::IO, tok) =
    let s = readtotoken(io, tok)
        s == "true" ? true : s == "false" ? false : throw(MagmaUnexpectedError("bool", s))
    end
readtotoken(::Type{T}, io::IO, tok) where {T<:Integer} =
    let s = readtotoken(io, tok)
        r = tryparse(T, s)
        r === nothing ? throw(MagmaUnexpectedError("integer", s)) : r
    end
