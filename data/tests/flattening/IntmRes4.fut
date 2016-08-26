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
fun addRows (xs: []int, ys: []int): []int =
  map(+, zip (xs,ys))

fun main (xssss: [][][][]int, cs: []int, y: int): [][][][]int =
  map (fn (xsss: [][][]int, c: int): [][][]int  =>
         unsafe
         let yss = reshape ( (2,c), xsss ) in
         map (fn (xss: [][]int): [][]int  =>
                map(fn (xs: []int, ys: []int): []int  =>
                      addRows(xs,ys)
                   , zip (xss, yss))
            , xsss)
      , zip (xssss,cs))
