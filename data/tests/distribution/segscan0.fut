-- Contrived segmented scan where the results are used in a
-- different order than they are produced.
--
-- ==
-- input { [[1,2],[3,4]] [[5.0f32,6.0f32],[7.0f32,8.0f32]] }
-- output {
--  [[5.0f32, 11.0f32],
--   [7.0f32, 15.0f32]]
--  [[1i32, 3i32],
--   [3i32, 7i32]]
-- }

fun main(ass: [n][m]int, bss: [n][m]f32): ([][]f32, [][]int) =
  unzip(zipWith (fn (as: []int, bs: []f32): ([m]f32, [m]int)  =>
                  let (asum, bsum) =
                    unzip(scan (fn (x_a,x_b) (y_a,y_b)  =>
                                 (x_a + y_a, x_b + y_b)) (0, 0f32) (zip(as, bs)))
                  in (bsum, asum)) ass bss)
