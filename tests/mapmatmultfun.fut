-- Mapping matrix multiplication, written in a functional style.
-- ==
-- input {
--   [[ [1,2], [3,4] ],
--    [ [5,6], [7,8] ]]
--   [[ [1,2], [3,4] ],
--    [ [5,6], [7,8] ]]
-- }
-- output {
--    [[[7i32, 10i32],
--      [15i32, 22i32]],
--     [[67i32, 78i32],
--      [91i32, 106i32]]]
-- }
-- structure { Map 3 Map/Map/Map/Redomap 1 }

fun matmult(x: [n][m]int) (y: [m][p]int): [n][p]int =
  map (fn xr =>
         map (fn yc => reduce (+) 0 (map (*) xr yc))
       (transpose y))
  x


fun main(xs: [k][n][m]int, ys: [k][m][p]int): [k][n][p]int =
  map matmult xs ys
