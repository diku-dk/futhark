-- ==
-- input {
--   [ [2,3,4] , [3,2,1] ]
--   [5,7]
-- }
-- output {
-- [ [5, 15, 30]
-- , [21, 7, 0 ]
-- ]
-- }
def main [m] [n] (xss: [m][n]i32) (ys: [m]i32) : [][]i32 =
  map (\(xs: [n]i32, y: i32) : [n]i32 ->
         map (\(x: i32) : i32 ->
                let tmp1 = map i32.i64 (iota (i64.i32 x))
                let tmp2 = map (* y) tmp1
                in reduce (+) 0 tmp2)
             xs)
      (zip xss ys)
