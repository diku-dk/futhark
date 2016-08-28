-- ==
-- input {
--   [1,2,3]
--   [6,7,8]
-- }
-- output {
--   27
-- }
fun main (xs: [n]int, ys: [n]int): int =
  let tmp =
    map  (fn (x: int, y: int): int  => x+y
        ) (zip (xs,ys)) in
  reduce (+) 0 tmp
