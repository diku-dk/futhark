-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--
-- ==
-- structure distributed { Kernel 2 }

fun main(a: [n][an]int, b: [n][bn]int): ([][]int,[][]int) =
  unzip(zipWith(fn (a_row: []int, b_row: []int): ([an]int,[bn]int)  =>
                  (map((+1), a_row),
                   map((-1), b_row)),
                a, b))
