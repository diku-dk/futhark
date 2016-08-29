-- ==
-- input {
--   [ [1,2,3], [4,5,6]
--   , [6,7,8], [9,10,11]
--   ]
--   [1,2,3,4]
--   5
-- }
-- output {
--   [[7, 8, 9],
--    [16, 17, 18],
--    [24, 25, 26],
--    [33, 34, 35]]
-- }
fun addToRow (xs: []int, y: int): []int =
  map (fn (x: int): int  => x+y) xs

fun main (xss: [][]int, cs: []int, y: int): [][]int =
  map  (fn (xs: []int, c: int): []int  =>
         let y' = y * c + c in
         let zs = addToRow(xs,y') in
         zs
      ) (zip  xss cs)
