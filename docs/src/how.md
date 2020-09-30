# How it works

We keep a Magma interpreter around for the whole Julia session, send it expressions to evaluate, and read its output.

Because every operation involves sending and receiving synchronous IO messages, which also invoke the Magma interpreter (and parser, etc.), this is all very slow, on the order of 1ms per operation. Hence the [Caveat](@ref).

## Interacting with the interpreter

Calling `MagmaCall.interact(f)` will wait for the interpreter to become available, lock it, call `f(io)` where `io` is the interpreter process, unlock it, and return whatever `f(io)` returned. In order to support asyncronous interaction (in particular, the Julia garbage collector is asyncronous, and we have finalizers that delete Magma objects), the function `f` must complete everything it is doing and leave the interpreter in a state ready for the next operation.

A typical interaction looks like this:
```julia
interact() do io
    putcmd(io, ..., err=true)
    for line in eachlinetotoken(io, missing)
        # process output...
    end
    checkerr(io)
end
```

Here, `putcmd` sends a command to the interpreter. This essentially is the same as `print`, except that a semicolon is appended automatically. Further, if `err=true` (recommended) then the whole thing is wrapped in a `try ... catch` block to support `checkerr`.

Next, we **must process all output that occurred as a result of the command**. Typically we do this by generating a random token, instructing Magma to print it, then reading everything up to that token. In this example, `eachlinetotoken(io, tok)` is an iterator over each line of output until the token `tok` is observed. If `tok` is `missing`, then a new token is randomly generated and sent to Magma first.

There are also `skiptotoken` (ignores everything), `echototoken` (prints each line to a given IO stream) and `readtotoken` (reads everything as a string).

After all output is processed, the call to `checkerr(io)` checks to see if an error occurred. If so, an appropriate error is raised.

Since we must process all output, we cannot break out of the loop early. Alternatively, we can call `skiptotoken(tok)` then `break`.

If the loop might throw an error, it must be caught, any remaining output must be processed (e.g. with `skiptotoken`), and `checkerr` called before rethrowing it:
```julia
interact() do io
    putcmd(io, ..., err=true)
    tok = puttoken()
    try
        for line in eachlinetotoken(io, tok)
            # process output...
        end
    catch err
        skiptotoken(io, tok)
        checkerr(io)
        rethrow(err)
    end
    checkerr(io)
end
```
