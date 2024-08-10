-- ==
-- entry: rev fwd
-- input { 
--   [[[0f32,1f32],[2f32,3f32]],
--    [[5f32,1f32],[3f32,0f32]],
--    [[0f32,1f32],[4f32,4f32]]] } 
-- output { [[[[[0f32, 0f32], [0f32, 0f32]], [[0f32, 0f32], [0f32, 0f32]], [[0f32, 0f32], [0f32, 0f32]]], [[[0f32, 1f32], [0f32, 0f32]], [[0f32, 1f32], [0f32, 0f32]], [[0f32, 1f32], [0f32, 0f32]]]], [[[[0f32, 0f32], [12f32, 0f32]], [[0f32, 0f32], [8f32, 0f32]], [[0f32, 0f32], [6f32, 0f32]]], [[[0f32, 0f32], [0f32, 0f32]], [[0f32, 0f32], [0f32, 12f32]], [[0f32, 0f32], [0f32, 0f32]]]]] }

let f [n][m][k] (xs: [n][m][k]f32) : [m][k]f32 =
  reduce (map2 (map2 (*))) (replicate m (replicate k 1)) xs

entry rev [n][m][k] (xs: [n][m][k]f32) : [m][k][n][m][k]f32 =
  tabulate_2d m k (\i j -> vjp f xs (replicate m (replicate k 0) with [i] = (replicate k 0 with [j] = 1)))

entry fwd [n][m][k] (xs: [n][m][k]f32) : [m][k][n][m][k]f32 =
  tabulate_3d n m k (\i j l -> jvp f xs (replicate n (replicate m (replicate k 0)) with [i] = (replicate m (replicate k 0) with [j] = (replicate k 0 with [l] = 1))))
  |> transpose |> map transpose