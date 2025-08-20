-- ==
-- entry: fwd_map fwd_vec
-- input { [1f32, 2f32, 3f32] }
-- output { [[0.5f32, 0.0, 0.0], [0.0f32, 0.35355338, 0.0], [0.0f32, 0.0, 0.28867513]] }

def primal = map f32.sqrt

entry fwd_map [n] (xs: [n]f32) =
  tabulate n (\i -> jvp primal xs (replicate n 0 with [i] = 1))

entry fwd_vec [n] (xs: [n]f32) =
  jvp_vec primal xs (tabulate n (\i -> replicate n 0 with [i] = 1))
