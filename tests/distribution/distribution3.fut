-- Expected distributed structure:
--
-- map
--   map
--     scan
-- map
--   map
--     scan
--
-- ==
--
-- structure distributed { Kernel 2 }

fun main(a: [][n][m]int): [][][]int =
  map (fn (a_row: [][]int): [m][n]int  =>
        let b = map (fn (a_row_row: []int): []int  =>
                      scan (+) 0 (a_row_row)
                   ) (a_row) in
        map (fn (b_col: []int): []int  =>
              scan (+) 0 (b_col)
           ) (transpose(b))
     ) a
