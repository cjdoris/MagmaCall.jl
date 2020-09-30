# MagmaCall.jl

`MagmaCall.jl` makes it easy to call the [Magma computer algebra system](http://magma.maths.usyd.edu.au/magma/) from Julia.

## Example

```julia-repl
julia> using MagmaCall

julia> Q = magf.Rationals() # Properties of `magf` are intrinsic functions (and auto-completion is supported)
Rational Field :: MagmaObject

julia> R, x = magg.PolynomialRing(:x, Q); # `magg` additionally names and returns generators

julia> R
Univariate Polynomial Ring in x over Rational Field :: MagmaObject

julia> x
x :: MagmaObject

julia> f = x^2 - 3x + 2
x^2 - 3*x + 2 :: MagmaObject

julia> facs = magf.Factorization(f)
[
    <x - 2, 1>,
    <x - 1, 1>
]
:: MagmaObject

julia> list = maglist() # Also `magseq`, `magset`, etc.
[* *] :: MagmaObject

julia> magp.Append(MagmaRef(list), "foo") # Use `magp` for procedure calls, `MagmaRef` to pass references

julia> magp1.Append(list, "bar") # Shorthand for the above, `magpN` passes the `N`th argument by reference

julia> list
[* foo, bar *] :: MagmaObject
```

## Install

```julia-repl
pkg> add https://github.com/cjdoris/MagmaCall.jl
```

You need to have Magma already installed, with the `magma` executable in your `PATH`.

## Caveat

This package is SLOW, each operation takes around 1ms. Hence this package is most suitable for interactive use and a high-level control.

If you need to write any tight loops, write the whole loop in Magma. Instead of `magseq(magf.GF(p) for p in magf.PrimesUpTo(N))` do `mag"[GF(p) : p in PrimesUpTo($N)]"` (which is 100 times faster).
