fun   int    redplus1( [int]  a) = reduce(op +, 0, a)
fun  [int]   redplus2([[int]] a) = map   (redplus1, a)

fun  [int]   mul1( [int]  a,  [int]  b) = map(op *, zip(a, b))
fun [[int]]  mul2([[int]] a, [[int]] b) = map(mul1, zip(a, b))

fun [[int]]  replin(int N, [int] a) = replicate(N, a)

fun [[int]] matmultFun([[int]] a, [[int]] b ) =
    let N   = size(0, a)                   in
    let br  = replicate( N, transpose(b) ) in
    let ar  = map      ( replin(N),    a ) in
    let abr = map  (mul2, zip(ar, br))     in
        map(redplus2, abr)

fun [[int]] main([[int]] x, [[int]] y) =
  matmultFun(x, y)
