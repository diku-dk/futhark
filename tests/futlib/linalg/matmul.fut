-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }

include futlib.linalg

module I32LinAlg = LinAlg(I32)

fun main(x: [][]i32, y: [][]i32): [][]i32 =
  I32LinAlg.matmul x y
