-- Contrived segmented reduction where the results are used in a
-- different order than they are produced.
--
-- ==
-- input { [[1,2],[3,4]] [[5.0f32,6.0f32],[7.0f32,8.0f32]] }
-- output { [11.0f32, 15.0f32] [3i32, 7i32] }

def main [n] [m] (ass: [n][m]i32) (bss: [n][m]f32) : ([]f32, []i32) =
  unzip (map2 (\(as: []i32) (bs: []f32) : (f32, i32) ->
                 let (asum, bsum) =
                   reduce (\(x: (i32, f32)) (y: (i32, f32)) : (i32, f32) ->
                             let (x_a, x_b) = x
                             let (y_a, y_b) = y
                             in (x_a + y_a, x_b + y_b))
                          (0, 0f32)
                          (zip as bs)
                 in (bsum, asum))
              ass
              bss)
