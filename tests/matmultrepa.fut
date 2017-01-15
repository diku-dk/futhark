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
fun redplus1(a: []i32): i32 = reduce (+) 0 a
fun redplus2(a: [n][]i32): [n]i32 = map redplus1 a

fun mul1(a: [m]i32, b: [m]i32): [m]i32 = map (*) a b
fun mul2(a: [n][m]i32, b: [n][m]i32): [n][m]i32 = map mul1 (zip a b)

fun replin(n: i32) (a: [m]i32): [n][m]i32 = replicate n a

fun matmultFun(a: [n][m]i32, b: [m][n]i32 ): [n][n]i32 =
    let br  = replicate n (transpose b)
    let ar  = map       (replin n) a
    let abr = map   mul2 (zip ar br)     in
        map redplus2 abr

fun main(x: [n][m]i32, y: [m][n]i32): [n][n]i32 =
  matmultFun(x, y)
