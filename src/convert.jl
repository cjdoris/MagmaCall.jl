magboolconvert(::Type{Bool}, x::MagmaValue) = interact() do p
    putcmd(p, valstr(x))
    r = false
    done = false
    tok = puttoken(p)
    for line in eachlinetotoken(p, tok)
        if done
            skiptotoken(p, tok)
            throw(MagmaUnexpectedError("bool", line))
        else
            if line == "true"
                r = true
            elseif line == "false"
                r = false
            else
                skiptotoken(p, tok)
                throw(MagmaUnexpectedError("bool", line))
            end
            done = true
        end
    end
    done || throw(MagmaUnexpectedError("bool", "nothing"))
    r
end
export magboolconvert

magintconvert(::Type{T}, x::MagmaValue) where {T<:Integer} = interact() do p
    putcmd(p, valstr(x))
    r = SubString{String}[]
    done = false
    tok = puttoken(p)
    for line in eachlinetotoken(p, tok)
        if done
            skiptotoken(p, tok)
            checkerr(p)
            throw(MagmaUnexpectedError("integer", line))
        else
            m = match(r"^\s*([0-9]+)\s*(\\?)$", line)
            if m === nothing
                skiptotoken(p, tok)
                checkerr(p)
                throw(MagmaUnexpectedError("integer", line))
            else
                push!(r, m.captures[1])
                done = isempty(m.captures[2])
            end
        end
    end
    checkerr(p)
    parse(T, join(r))
end
export magintconvert

"""
    magconvert(T, o)

Convert the Magma object `o` to a `T`.
"""
function magconvert(::Type{T}, _o) where {T}
    # A simpler implementation is:
    #     magcallp(:WriteObject, MAGOTP_MAG_CLIENT_SOCKET, o)
    #     magotp_interact(io->magotp_read_full_object(T, io))
    # but the call to WriteObject blocks until everything is written, and the buffer is
    # finite, so can deadlock on large objects. This implementation reads and writes
    # simultaneously.
    o = MagmaValue(_o)
    p = interact_start()
    try
        putcmd(p, "WriteObject(", valstr(MAGOTP_MAG_CLIENT_SOCKET), ", ", valstr(o), ")", err=true)
        io = magotp_interact_start()
        try
            return magotp_read_full_object(T, io)::T
        finally
            magotp_interact_stop()
            echototoken(p, missing)
            checkerr(p)
        end
    finally
        interact_stop()
    end
end
export magconvert
