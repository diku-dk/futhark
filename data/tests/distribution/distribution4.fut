-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--
-- ==
-- structure distributed { Kernel 2 }

fun ([][]int,[][]int) main([n][an]int a, [n][bn]int b) =
  unzip(zipWith(fn ([an]int,[bn]int) ([]int a_row, []int b_row) =>
                  (map(+1, a_row),
                   map(-1, b_row)),
                a, b))
