-- Addition
-- ==
-- tags { autodiff }
--  entry: fwd_map fwd_vec rev_map rev_vec
--  input {
--    [4i64,5i64,2i64,4i64,5i64,2i64,0i64,0i64,4i64,5i64,1i64,4i64,1i64,3i64,3i64,1i64,8i64,-1i64]
--    [11f32,16f32,7f32,0f32,14f32,6f32,2f32,1f32,13f32,0f32,5f32,0f32,3f32,0f32,9f32,4f32,17f32,18f32]
--    [1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32]
--  }
--  output {
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 1f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 2f32, 0f32, 2f32, 0f32, 0f32, 2f32, 0f32, 0f32],
--   [0f32, 0f32, 3f32, 0f32, 0f32, 3f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 4f32, 4f32, 0f32, 0f32, 0f32],
--   [5f32, 0f32, 0f32, 5f32, 0f32, 0f32, 0f32, 0f32, 5f32, 0f32, 0f32, 5f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 6f32, 0f32, 0f32, 6f32, 0f32, 0f32, 0f32, 0f32, 6f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]
--  [[3f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 12f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 13f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 9f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 24f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 30f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--   [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]
-- }

def f [n] [m] (is: [n]i64) (vs: [n]f32, c: [m]f32) =
  let r = hist (+) 0 m is vs
  in map2 (*) r c

entry fwd_map [n] [m] (is: [n]i64) (vs: [n]f32) (c: [m]f32) =
  tabulate m (\i -> vjp (f is) (vs, c) (replicate m 0 with [i] = 1))
  |> unzip

entry fwd_vec [n] [m] (is: [n]i64) (vs: [n]f32) (c: [m]f32) =
  let seeds =
    tabulate (n + m) (\i ->
                        ( tabulate n ((i ==) >-> f32.bool)
                        , tabulate m (((i - n) ==) >-> f32.bool)
                        ))
  in jvp_vec (f is) (vs, c) seeds
     |> transpose
     |> map split
     |> unzip

entry rev_map [n] [m] (is: [n]i64) (vs: [n]f32) (c: [m]f32) =
  tabulate m (\i -> vjp (f is) (vs, c) (replicate m 0 with [i] = 1))
  |> unzip

entry rev_vec [n] [m] (is: [n]i64) (vs: [n]f32) (c: [m]f32) =
  let seeds = tabulate m (\i -> replicate m 0 with [i] = 1)
  in vjp_vec (f is) (vs, c) seeds
     |> unzip
