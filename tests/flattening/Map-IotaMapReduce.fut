-- ==
-- input {
--   [2,3,4]
--   [8,3,2]
-- }
-- output {
--   [8,9,12]
-- }
def main [n] (xs: [n]i32) (ys: [n]i32) : []i32 =
  map (\(x: i32, y: i32) : i32 ->
         let tmp1 = 0..<x
         let tmp2 = map (* y) tmp1
         in reduce (+) 0 tmp2)
      (zip xs ys)
