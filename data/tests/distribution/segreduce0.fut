-- Contrived segmented reduction where the results are used in a
-- different order than they are produced.
--
-- ==
-- input { [[1,2],[3,4]] [[5.0f32,6.0f32],[7.0f32,8.0f32]] }
-- output { [11.0f32, 15.0f32] [3i32, 7i32] }

fun ([f32], [int]) main([[int,m],n] ass, [[f32,m],n] bss) =
  unzip(zipWith(fn (f32, int) ([int] as, [f32] bs) =>
                  let (asum, bsum) =
                    reduce(fn (int, f32) ((int, f32) x, (int, f32) y) =>
                             let (x_a, x_b) = x
                             let (y_a, y_b) = y
                             in (x_a + y_a, x_b + y_b),
                           (0, 0f32), zip(as, bs))
                  in (bsum, asum),
                ass, bss))
