-- Maximum
-- ==
-- tags { autodiff }
--  entry: fwd_map fwd_vec rev_map rev_vec
--  input {
--    5i64
--    [4i64,5i64,2i64,4i64,5i64,2i64,0i64,0i64,4i64,5i64,1i64,4i64,1i64,3i64,3i64,1i64,8i64,-1i64]
--    [11f32,16f32,7f32,0f32,14f32,6f32,2f32,1f32,13f32,0f32,5f32,0f32,3f32,0f32,9f32,4f32,17f32,18f32]
--  }
-- output {
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [0f32, 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32, 0f32],
--  [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]
-- }

def primal [n] (k: i64) (is: [n]i64) (vs: [n]f32) =
  hist f32.max f32.lowest k is vs

entry fwd_map [n] (k: i64) (is: [n]i64) (vs: [n]f32) =
  tabulate n (\i -> jvp (primal k is) vs (replicate n 0 with [i] = 1))
  |> transpose

entry fwd_vec [n] (k: i64) (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jvp_vec (primal k is) vs seeds
     |> transpose

entry rev_map [n] (k: i64) (is: [n]i64) (vs: [n]f32) =
  tabulate k (\i -> vjp (primal k is) vs (replicate k 0 with [i] = 1))

entry rev_vec [n] (k: i64) (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate k (\i -> replicate k 0 with [i] = 1)
  in vjp_vec (primal k is) vs seeds
