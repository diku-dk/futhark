-- ==
-- tags { autodiff }
--  entry: rev rev_vec
--  input {
--    [0i64,1i64,2i64,1i64,0i64,1i64,2i64]
--    [1f32,2f32,3f32,4f32,5f32,6f32,7f32] }
--  output {
-- [[1f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [5f32, 1f32, 0f32, 1f32, 1f32, 1f32, 0f32],
--  [0f32, 24f32, 1f32, 12f32, 0f32, 8f32, 1f32],
--  [0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 3f32]] }

entry f [n] (is: [n]i64) (vs: [n]f32) =
  let r1 = reduce_by_index (replicate 4 1) (*) 1 (map (+ 1) is) vs
  let r2 = reduce_by_index (replicate 4 0) (+) 0 is (map (+ 2) vs)
  in map2 (+) r1 r2

entry rev [n] (is: [n]i64) (vs: [n]f32) =
  tabulate 4 (\i -> vjp (f is) vs (replicate 4 0 with [i] = 1))

entry rev_vec [n] (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate 4 (\i -> replicate 4 0 with [i] = 1)
  in mjp (f is) vs seeds

-- entry fwd [n] (is: [n]i64) (vs: [n]f32) =
--   tabulate n (\i -> jvp (f is) vs (replicate n 0 with [i] = 1))
--   |> map (.1) |> transpose

-- entry ftest [n] (is: [n]i64) (vs: [n]f32) =
--   reduce_by_index (replicate 4 0) (+) 0 is (map (+2) vs)

-- entry revtest [n] (is: [n]i64) (vs: [n]f32) =
--   tabulate 4 (\i -> vjp (ftest is) vs (replicate 4 0 with [i] = 1))

-- entry fwdtest [n] (is: [n]i64) (vs: [n]f32) =
--   tabulate n (\i -> jvp (ftest is) vs (replicate n 0 with [i] = 1))
--   |> transpose

-- entry revmap [n] (vs: [n]f32) =
--   tabulate n (\i -> vjp (map (+2)) vs (replicate n 0 with [i] = 1))

-- entry fwdmap [n] (vs: [n]f32) =
--   tabulate n (\i -> jvp (map (+2)) vs (replicate n 0 with [i] = 1))
--   |> transpose

-- entry revp [n] (is: [n]i64) (vs: [n]f32) =
--   (vjp2 (f is) vs (replicate 4 1))

-- entry fwdp [n] (is: [n]i64) (vs: [n]f32) =
--   (jvp2 (f is) vs (replicate n 1))

-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [5f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [0f32, 24f32, 0f32, 12f32, 0f32, 8f32, 0f32],
--  [0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 3f32]]

-- [[1f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [5f32, 1f32, 0f32, 1f32, 1f32, 1f32, 0f32],
--  [0f32, 24f32, 1f32, 12f32, 0f32, 8f32, 1f32],
--  [0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 3f32]]

-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [5f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [0f32, 24f32, 0f32, 12f32, 0f32, 8f32, 0f32],
--  [0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 3f32]]

-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
--  [5f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [0f32, 24f32, 0f32, 12f32, 0f32, 8f32, 0f32],
--  [0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 3f32]]

-- [[1f32, 0f32, 0f32, 0f32, 1f32, 0f32, 0f32],
--  [0f32, 1f32, 0f32, 1f32, 0f32, 1f32, 0f32],
--  [0f32, 0f32, 1f32, 0f32, 0f32, 0f32, 1f32],
--  [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]
