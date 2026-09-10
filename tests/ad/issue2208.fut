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

def scan_arr_add_primal [n] (inp: [2][n]f32) : [2][n]f32 =
  scan (\x y -> [x[0] + y[0], x[1] + y[1]])
       (replicate 2 0) (transpose inp)
  |> transpose

entry scan_arr_add [n]
                   (inp: [2][n]f32)
                   (adj: [2][n]f32) : [2][n]f32 =
  vjp scan_arr_add_primal inp adj

-- ==
-- entry: scan_arr_add_vec
-- input { [[1f32, 2, 3], [4f32, 5, 6]]
--         [[[1f32, 2, 4], [3f32, 5, 7]],
--          [[2f32, -1, 3], [0f32, 4, -2]]] }
-- output { [[[7f32, 6, 4], [15f32, 12, 7]],
--           [[4f32, 2, 3], [2f32, 2, -2]]] }
entry scan_arr_add_vec [n] [k]
                       (inp: [2][n]f32)
                       (adjs: [k][2][n]f32) : [k][2][n]f32 =
  mjp scan_arr_add_primal inp adjs
