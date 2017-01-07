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
fun main (xss: [m][n]int, ys: [m]int): [][]int =
  map (fn (xs: [n]int, y: int): [n]int  =>
         map  (fn (x: int): int  =>
                let tmp1 = iota(x)
                let tmp2 = map (*y) tmp1 in
                reduce (+) 0 tmp2
             ) xs
     ) (zip xss ys )
