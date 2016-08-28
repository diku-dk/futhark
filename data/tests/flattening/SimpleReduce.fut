-- ==
-- input {
--   [ [1,2,3], [5,6,7], [8,9,10] ]
-- }
-- output {
--   [ 6, 18, 27 ]
-- }
fun main (xss: [][]int): []int =
  map  (fn (xs: []int ): int  =>
         reduce (+) 0 xs
      ) xss
