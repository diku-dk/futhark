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
fun [][]int main ([m][n]int xss, [m]int ys) =
  map( fn [n]int ([n]int xs, int y) =>
         map (fn int (int x) =>
                let tmp1 = iota(x) in
                let tmp2 = map(*y,tmp1) in
                reduce(+,0,tmp2)
             , xs)
     , zip(xss, ys) )
