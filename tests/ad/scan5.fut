-- Scan with vectorised product.
-- Vectorised special case + generic case
-- ==
-- entry: fwd_J rev_J
-- input { [[1f32,1f32],[2f32,2f32],[3f32,3f32],[4f32,4f32],[5f32,5f32]] }
-- output {
-- [[[1f32, 1f32], [0f32, 0f32], [0f32, 0f32], [0f32, 0f32], [0f32, 0f32]],
--   [[2f32, 2f32], [1f32, 1f32], [0f32, 0f32], [0f32, 0f32], [0f32, 0f32]],
--   [[6f32, 6f32], [3f32, 3f32], [2f32, 2f32], [0f32, 0f32], [0f32, 0f32]],
--   [[24f32, 24f32], [12f32, 12f32], [8f32, 8f32], [6f32, 6f32], [0f32, 0f32]],
--   [[120f32, 120f32], [60f32, 60f32], [40f32, 40f32], [30f32, 30f32], [24f32, 24f32]]]
-- }

def primal [n] [k] (a: [n][k]f32) =
  scan (map2 (*)) (replicate k 1) a

entry fwd_J [n] [k] (a: [n][k]f32) =
  tabulate n (\i -> jvp primal a (replicate n (replicate k 0) with [i] = replicate k 1))
  |> transpose

entry rev_J [n] [k] (a: [n][k]f32) =
  tabulate n (\i -> vjp primal a (replicate n (replicate k 0) with [i] = replicate k 1))
