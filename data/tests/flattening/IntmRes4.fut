-- ==
-- tags { no_opencl }
-- input {
--   [ [ [ [1,2,3], [4,5,6] ]
--     ]
--   , [ [ [6,7,8], [9,10,11] ]
--     ]
--   , [ [ [3,2,1], [4,5,6] ]
--     ]
--   , [ [ [8,7,6], [11,10,9] ]
--     ]
--   ]
--   [3,3,3,3]
--   5
-- }
-- output {
--   [[[[2, 4, 6],
--      [8, 10, 12]]],
--    [[[12, 14, 16],
--      [18, 20, 22]]],
--    [[[6, 4, 2],
--      [8, 10, 12]]],
--    [[[16, 14, 12],
--      [22, 20, 18]]]]
-- }
fun []int addRows ([]int xs, []int ys) =
  map(+, zip (xs,ys))

fun [][][][]int main ([][][][]int xssss, []int cs, int y) =
  map (fn [][][]int ([][][]int xsss, int c) =>
         unsafe
         let yss = reshape ( (2,c), xsss ) in
         map (fn [][]int ([][]int xss) =>
                map(fn []int ([]int xs, []int ys) =>
                      addRows(xs,ys)
                   , zip (xss, yss))
            , xsss)
      , zip (xssss,cs))
