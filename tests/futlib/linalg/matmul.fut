-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }

import "futlib/linalg"

module i32linalg = linalg(i32)

fun main(x: [][]i32, y: [][]i32): [][]i32 =
  i32linalg.matmul x y
