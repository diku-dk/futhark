-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--     map
-- map
--   map
--     concat
-- map
--   map
--     reduce (which becomes a segmented reduction)
--
-- ==
--
-- structure distributed {
--   /If/True/Kernel 1
--   /If/False/Kernel 1
--   /If/False/If/True/Kernel 1
--   /If/False/If/False/Kernel 5
--   /If/False/If/False/ScanKernel 2
-- }

fun main(a: [n][an][]int, b: [n][bn]int): ([][]int,[][]int) =
  unzip(zipWith (fn (a_row: [][]int) (b_row: []int): ([bn]int,[an]int)  =>
                  (map (-1) (b_row),
                   map (fn (a_row_row: []int): int  =>
                         let x = map (+1) (a_row_row) in
                         reduce (+) 0 (concat x x)
                      ) a_row)) a b)
