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
fun int redplus1([]int  a) = reduce(+, 0, a)
fun [n]int redplus2([n][]int a) = map(redplus1, a)

fun  [m]int mul1([m]int a, [m]int b) = zipWith(*, a, b)
fun [n][m]int mul2([n][m]int a, [n][m]int b) = zipWith(mul1, a, b)

fun [n][m]int  replin(int n, [m]int a) = replicate(n, a)

fun [n][n]int matmultFun([n][m]int a, [m][n]int b ) =
    let br  = replicate( n, transpose(b) ) in
    let ar  = map      ( replin(n),    a ) in
    let abr = map  (mul2, zip(ar, br))     in
        map(redplus2, abr)

fun [n][n]int main([n][m]int x, [m][n]int y) =
  matmultFun(x, y)
