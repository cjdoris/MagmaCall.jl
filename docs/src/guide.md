# Guide

## Syntax

The following tables gives the mapping between Magma and Julia syntax.

| Magma | Julia |
| :---- | :---- |
| `r := Intrinsic(x, y: opt:=z)` | `r = magf.Intrinsic(x, y, opt=z)` |
| `r, s := Intrinsic(x)` | `r, s = magf2.Intrinsic(x)` |
| `R<x,y> := Intrinsic(p, q)` | `R, x, y = magg.Intrinsic((:x, :y), p, q)` |
| `IntrinsicProcedure(x)` | `magp.IntrinsicProcedure(x)` |
| `IntrinsicProcedure(~x, y)` | `magp1.IntrinsicProcedure(x, y)` or `magp.IntrinsicProcedure(MagmaRef(x), y)` |
| `SomeType` | `magt.SomeType` |
| `x + y` | `magadd(x, y)` or `x + y` |
| `x - y` | `magsub(x, y)` or `x - y` |
| `x * y` | `magmul(x, y)` or `x * y` |
| `x / y` | `magtruediv(x, y)` or `x / y` |
| `x ^ y` | `magpow(x, y)` or `x ^ y` |
| `x diff y` | `magdiff(x, y)` or `setdiff(x, y)` |
| `x div y` | `magdiv(x, y)` or `div(x, y)` |
| `x join y` | `magjoin(x, y)` or `union(x, y)` |
| `x meet y` | `magmeet(x, y)` or `intersect(x, y)` |
| `x mod y` | `magmod(x, y)` or `mod(x, y)` |
| `x sdiff y` | `magsdiff(x, y)` or `symdiff(x, y)` |
| `x cat y` | `magcat(x, y)` or `cat(x, y)` |
| `x eq y` | `mageq(x, y)` or `x == y` |
| `x ne y` | `magne(x, y)` or `x != y` |
| `x cmpeq y` | `magcmpeq(x, y)` |
| `x cmpne y` | `magcmpne(x, y)` |
| `x le y` | `magle(x, y)` or `x <= y` |
| `x lt y` | `maglt(x, y)` or `x < y` |
| `x ge y` | `magge(x, y)` or `x >= y` |
| `x gt y` | `maggt(x, y)` or `x > y` |
| `x in y` | `magin(x, y)` or `x in y` |
| `x notin y` | `magnotin(x, y)` |
| `x adj y` | `magadj(x, y)` |
| `x notadj y` | `magnotadj(x, y)` |
| `x subset y` | `magsubset(x, y)` or `issubset(x, y)` |
| `x notsubset y` | `magnotsubset(x, y)` |
| `&+ x` | `magsum(x)` or `sum(x)` |
| `&* x` | `magprod(x)` or `prod(x)` |
| `&and x` | `magreduceand(x)` |
| `&or x` | `magreduceor(x)` |
| `&join x` | `magreducejoin(x)` |
| `&meet x` | `magreducemeet(x)` |
| `&cat x` | `magreducecat(x)` |
| `[* a, b, c *]` | `maglist((a, b, c))` |
| `< a, b, c >` | `magtuple((a, b, c))` |
| `[ a, b, c ]` | `magseq((a, b, c))` |
| `[ U \| a, b, c ]` | `magseq((a, b, c), universe=U)` |
| `[ a..b by c]` | `magseq(a:c:b)` |
| `{ a, b, c }` | `magset((a, b, c))` |
| `{* a, b, c *}` | `magmset((a, b, c))` |
| `{@ a, b, c @}` | `magiset((a, b, c))` |
| `print x` | `magprint(io, x)` |
| `print x: Magma` | `magprint(io, x, :magma)` |
| `Sprint(x, "Magma")` | `magsprint(x, :magma)` |
| `x[i, j]` | `maggetindex(x, i, j)` or `x[i, j]` |
| `x[i, j] := y` | `magsetindex!(x, y, i, j)` or `x[i, j] = y` |
| `# x` | `maglength(x)` (or `length(x)` provided the result is an integer) |
| `x @ f` | `magimage(x, f)` |
| `x @@ f` | `magpreimage(x, f)` |
| `S ! x` | `magcoerce(S, x)` |
| `S.n` | `maggen(S, n)` |
| ```x`attr``` | `maggetattr(x, :attr)` or `x.attr` |
| ```x`attr := y``` | `magsetattr!(x, :attr, y)` or `x.attr = y` |
| `recformat<r, s:RngInt>` | `magrecformat(:r, :s=>:RngInt)` |
| `rec<fmt \| r:=x>` | `magrec(fmt, r=x)` |
| `? query` | `maghelp("query")` |
| `...any expression...` | `mag"..."` (supporting `$` interpolation) |

## Conversion to Magma

When passed as arguments to Magma functions the following conversions are made from Julia types to Magma types:

| Julia | Magma |
| :---- | :---- |
| `Bool` | `BoolElt` |
| `Integer` | `RngIntElt` |
| `Rational` | `FldRatElt` |
| `Real` | `FldReElt` |
| `Complex` | `FldComElt` |
| `AbstractString` | `MonStgElt` |
| `AbstractChar` | `MonStgElt` of length 1 |
| `AbstractVector` | `SeqEnum` |
| `AbstractSet` | `SetEnum` |
| `AbstractDict` | `Assoc` |
| `Tuple` | `Tup` |
| `NamedTuple` | `Rec` |

## Conversion to Julia

Call `magconvert(T, x)` to convert the Magma value `x` to a Julia `T`.

The following table gives the supported conversions. These are in preference order: the first type in each row that is a subtype of `T` is the return type from `magconvert`.

| Magma | Julia |
| :---- | :---- |
| `RngIntElt` (integer) | `BigInt`, `Int`, `UInt`, `Integer`, `Rational`, `Real`, `MagmaOTPInteger` |
| `FltRatElt` (rational) | `Rational`, `Real`, `MagmaOTPRational` |
| `MonStgElt` (string) | `String`, `AbstractString`, `Char`, `AbstractChar`, `Vector{UInt8}`, `AbstractVector{UInt8}`, `MagmaOTPString` |
| `List` (list) | `Vector`, `AbstractVector`, `MagmaOTPList` |
| `SeqEnum` (sequence) | `Vector`, `AbstractVector`, `MagmaOTPSequence` |
| `SetEnum` (set) | `Set`, `AbstractSet`, `Vector`, `AbstractVector`, `MagmaOTPSet` |

For sequences and sets, the result depends on whether it is a null sequence/set (in which case `MagmaOTPNullSequence` might be returned) or is a range (in which case a `StepRangeLen` is the preferred return type, and `MagmaOTPRangeSequence` is the least preferred).

The `MagmaOTP*` types are exact representations of objects from the Magma Object Transfer Protocol. They are all subtypes of `MagmaOTPValue`. They have no extra semantics, so are always the least-preferred option. Not included in the table are structures themselves (e.g. `RngInt` (ring of integers), `MonStg` (strings)) which are always converted to a corresponding `MagmaOTPStr`.

## Calling functions and procedures

In Magma, there is no syntactic difference between function calls and procedure calls, so in MagmaCall, we must make the distinction explicit.

The simplest way to call an intrinsic function is to use the syntax `magf.Intrinsic(...)`. Similarly `magp.Intrinsic(...)` is a procedure call (returning `nothing`) and `magf2.Intrinsic(...)` is a function call returning a pair of values.

For each "calling convention" `C` in the following table, there is a corresponding `magC` object which can be accessed in this way.

| Convention | Interpretation |
| p | Procedure call. Return `nothing`. |
| f | Function call. Return a `MagmaObject`. |
| fN | Function call with `N` return values. Return a tuple of `MagmaObject`. |
| i | Function call returning an integer. Return an `Int`. |
| b | Function call returning a boolean. Return a `Bool`. |
| g | Function call with generators, so that `R, x, y = magg.Intrinsic((:x, :y), ...)` is equivalent to `R<x,y> := Intrinsic(...)` in Magma. |
| m | Function call returning a bool and a value. Return `nothing` if the bool is false, else the value. |
| pN | Procedure call, passing the `N`th argument by reference. Return `nothing`. |

The way this works is that `magC.Intrinsic` is a `MagmaCallable{:C}`, which is a `MagmaValue` with function call syntax overloaded, so that `magC.Intrinsic(...)` is equivalent to `magcall(Val(:C), magC.Intrinsic, ...)`. [`magcall`](@ref) is the worker function where calling conventions are implemented.

If you have a Magma value `f` and need to call it, then `f(...)` will not work because the desired calling convention is unknown. Instead, use `magcall(Val(:C), f, ...)` or `magcallC(f, ...)`. Alternatively, you can explicitly wrap it as `MagmaCallable{:C}(f)` and call that.

Note also that the `mag"..."C` is equivalent to `MagmaCallable{:C}(mag"...")`.

The following are equivalent:
```julia
magf.PolynomialRing(Z)
magcallf(:PolynomialRing, Z)
magcall(Val(:f), :PolynomialRing, Z)
MagmaCallable{:f}(MagmaIntrinsic(:PolynomialRing))(Z)
mag"PolynomialRing"f(Z)
mag"PolynomialRing($Z)"
```
