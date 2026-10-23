-- Rank-two results. Expected derivatives are componentwise suffix sums.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[[1f32, 2], [3f32, 4]], [[5f32, 6], [7f32, 8]],
--          [[9f32, 10], [11f32, 12]]]
--         [[[1f32, 2], [3f32, 4]], [[5f32, 6], [7f32, 8]],
--          [[9f32, 10], [11f32, 12]]] }
-- output { [[[15f32, 18], [21f32, 24]], [[14f32, 16], [18f32, 20]],
--           [[9f32, 10], [11f32, 12]]] }
-- input { [[[1f32, 2], [3f32, 4]]] [[[5f32, 6], [7f32, 8]]] }
-- output { [[[5f32, 6], [7f32, 8]]] }
-- input { empty([0][2][2]f32) empty([0][2][2]f32) }
-- output { empty([0][2][2]f32) }

entry main [n] (xs: [n][2][2]f32) (adj: [n][2][2]f32) =
  vjp (scan (\x y -> [[x[0, 0] + y[0, 0], x[0, 1] + y[0, 1]],
                      [x[1, 0] + y[1, 0], x[1, 1] + y[1, 1]]])
            (replicate 2 (replicate 2 0)))
      xs adj
