-- ==
-- input {
--   [2,3,4]
--   [8,3,2]
-- }
-- output {
--   [8,9,12]
-- }
fun main (xs: [n]int, ys: [n]int): []int =
  map (\(x: int, y: int): int  ->
         let tmp1 = iota(x)
         let tmp2 = map (*y) tmp1 in
         reduce (+) 0 tmp2
     ) (zip xs ys )
