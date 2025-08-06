-- https://rosettacode.org/wiki/Matrix_multiplication
--
-- Matrix multiplication written in a functional style.
--
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
-- structure { /Screma 1 /Screma/Screma 1 /Screma/Screma/Screma 1 }

def main [n] [m] [p] (x: [n][m]i32) (y: [m][p]i32) : [n][p]i32 =
  map (\xr -> map (\yc -> reduce (+) 0 (map2 (*) xr yc)) (transpose y))
      x
