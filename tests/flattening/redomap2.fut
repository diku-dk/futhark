-- ==
-- input {
--   [1,2,3]
--   [6,7,8]
-- }
-- output {
--   27
-- }
def main [n] (xs: [n]i32) (ys: [n]i32) : i32 =
  let tmp =
    map (\(x: i32, y: i32) : i32 -> x + y)
        (zip xs ys)
  in reduce (+) 0 tmp
