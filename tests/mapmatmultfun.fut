-- Mapping matrix multiplication, written in a functional style.
-- This is primarily a test of tiling.
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
-- compiled random input { [1][16][16]i32 [1][16][16]i32 } auto output
-- compiled random input { [2][16][32]i32 [2][32][16]i32 } auto output
-- compiled random input { [3][32][16]i32 [3][16][32]i32 } auto output
-- compiled random input { [4][128][17]i32 [4][17][128]i32 } auto output
-- structure { /Screma 1 /Screma/Screma 1 /Screma/Screma/Screma 1 /Screma/Screma/Screma/Screma 1 }

def matmult [n] [m] [p] (x: [n][m]i32) (y: [m][p]i32) : [n][p]i32 =
  map (\xr ->
         map (\yc -> reduce (+) 0 (map2 (*) xr yc))
             (transpose y))
      x

def main [k] [n] [m] [p] (xs: [k][n][m]i32) (ys: [k][m][p]i32) : [k][n][p]i32 =
  map2 matmult xs ys
