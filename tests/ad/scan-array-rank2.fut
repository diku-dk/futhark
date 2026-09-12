-- Rank-two results. Expected derivatives are componentwise suffix sums.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[[1f64, 2], [3f64, 4]], [[5f64, 6], [7f64, 8]],
--          [[9f64, 10], [11f64, 12]]]
--         [[[1f64, 2], [3f64, 4]], [[5f64, 6], [7f64, 8]],
--          [[9f64, 10], [11f64, 12]]] }
-- output { [[[15f64, 18], [21f64, 24]], [[14f64, 16], [18f64, 20]],
--           [[9f64, 10], [11f64, 12]]] }
-- input { [[[1f64, 2], [3f64, 4]]] [[[5f64, 6], [7f64, 8]]] }
-- output { [[[5f64, 6], [7f64, 8]]] }
-- input { empty([0][2][2]f64) empty([0][2][2]f64) }
-- output { empty([0][2][2]f64) }

entry main [n] (xs: [n][2][2]f64) (adj: [n][2][2]f64) =
  vjp (scan (\x y -> [[x[0, 0] + y[0, 0], x[0, 1] + y[0, 1]],
                      [x[1, 0] + y[1, 0], x[1, 1] + y[1, 1]]])
            (replicate 2 (replicate 2 0)))
      xs adj
