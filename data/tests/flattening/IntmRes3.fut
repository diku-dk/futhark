-- ==
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
--   [1,2,3,4]
--   5
-- }
-- output {
--   [[[[7, 8, 9],
--      [10, 11, 12]]],
--    [[[18, 19, 20],
--      [21, 22, 23]]],
--    [[[21, 20, 19],
--      [22, 23, 24]]],
--    [[[32, 31, 30],
--      [35, 34, 33]]]]
-- }
fun addToRow (xs: []int, y: int): []int =
  map(fn (x: int): int  => x+y, xs)

fun main (xssss: [][][][]int, cs: []int, y: int): [][][][]int =
  map (fn (xsss: [][][]int, c: int): [][][]int  =>
         let y' = y * c + c in
         map (fn (xss: [][]int): [][]int  =>
                map(fn (xs: []int): []int  =>
                      addToRow(xs,y')
                   , xss)
            , xsss)
      , zip (xssss,cs))
