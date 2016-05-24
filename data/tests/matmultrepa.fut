-- Matrix multiplication written in a Repa-like style.
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
-- structure { Map 2 Map/Map/Redomap 1 }
fun int redplus1([int]  a) = reduce(+, 0, a)
fun [int,n] redplus2([[int],n] a) = map(redplus1, a)

fun  [int,m] mul1([int,m] a, [int,m] b) = zipWith(*, a, b)
fun [[int,m],n] mul2([[int,m],n] a, [[int,m],n] b) = zipWith(mul1, a, b)

fun [[int,m],N]  replin(int N, [int,m] a) = replicate(N, a)

fun [[int,n],n] matmultFun([[int,m],n] a, [[int,n],m] b ) =
    let N   = size(0, a)                   in
    let br  = replicate( N, transpose(b) ) in
    let ar  = map      ( replin(N),    a ) in
    let abr = map  (mul2, zip(ar, br))     in
        map(redplus2, abr)

fun [[int,n],n] main([[int,m],n] x, [[int,n],m] y) =
  matmultFun(x, y)
