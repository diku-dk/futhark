-- ==
-- input {
--   [1,2,3,4]
-- }
-- output {
--   [0, 1, 3, 6]
-- }
def main (xs: []i32) : []i32 =
  map (\(x: i32) : i32 ->
         let tmp = 0..<x
         in reduce (+) 0 tmp)
      xs
