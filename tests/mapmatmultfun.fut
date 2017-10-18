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

let matmult [n][m][p] (x: [n][m]i32) (y: [m][p]i32): [n][p]i32 =
  map (\xr ->
         map (\yc -> reduce (+) 0 (map (*) xr yc))
       (rearrange (1,0) y))
  x


let main [k][n][m][p] (xs: [k][n][m]i32, ys: [k][m][p]i32): [k][n][p]i32 =
  map matmult xs ys
