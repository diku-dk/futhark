-- Array-valued scan from #2208. Each derivative is a suffix sum of adj.
-- ==
-- tags { autodiff }
-- entry: scan_arr_add
-- input { [[1f32, 2, 3], [4f32, 5, 6]]
--         [[1f32, 2, 4], [3f32, 5, 7]] }
-- output { [[7f32, 6, 4], [15f32, 12, 7]] }
-- input { [[2f32], [3f32]] [[4f32], [5f32]] }
-- output { [[4f32], [5f32]] }
-- input { empty([2][0]f32) empty([2][0]f32) }
-- output { empty([2][0]f32) }

entry scan_arr_add [n]
                   (inp: [2][n]f32)
                   (adj: [2][n]f32) : [2][n]f32 =
  let adj =
    vjp (scan (\x y -> [x[0] + y[0], x[1] + y[1]])
              (replicate 2 0))
        (transpose inp)
        (transpose adj)
  in transpose adj
