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
fun redplus1(a: []int): int = reduce (+) 0 a
fun redplus2(a: [n][]int): [n]int = map redplus1 a

fun mul1(a: [m]int, b: [m]int): [m]int = zipWith (*) a b
fun mul2(a: [n][m]int, b: [n][m]int): [n][m]int = map mul1 (zip a b)

fun replin(n: int) (a: [m]int): [n][m]int = replicate n a

fun matmultFun(a: [n][m]int, b: [m][n]int ): [n][n]int =
    let br  = replicate n (transpose b) in
    let ar  = map       (replin n) a in
    let abr = map   mul2 (zip ar br)     in
        map redplus2 abr

fun main(x: [n][m]int, y: [m][n]int): [n][n]int =
  matmultFun(x, y)
