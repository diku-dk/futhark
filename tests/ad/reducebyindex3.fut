-- ==
--  entry: rev
--  input {
--    [0i64,1i64,2i64,1i64,0i64,1i64,2i64] 
--    [1f64,2f64,3f64,4f64,5f64,6f64,7f64] }
--  output {
-- [[1f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [5f64, 1f64, 0f64, 1f64, 1f64, 1f64, 0f64], 
--  [0f64, 24f64, 1f64, 12f64, 0f64, 8f64, 1f64], 
--  [0f64, 0f64, 7f64, 0f64, 0f64, 0f64, 3f64]] }

entry f [n] (is: [n]i64) (vs: [n]f64) =
  let r1 = reduce_by_index (replicate 4 1) (*) 1 (map (+1) is) vs
  let r2 = reduce_by_index (replicate 4 0) (+) 0 is (map (+2) vs)
  in map2 (+) r1 r2

entry rev [n] (is: [n]i64) (vs: [n]f64) =
  tabulate 4 (\i -> vjp (f is) vs (replicate 4 0 with [i] = 1))

-- entry fwd [n] (is: [n]i64) (vs: [n]f64) =
--   tabulate n (\i -> jvp (f is) vs (replicate n 0 with [i] = 1))
--   |> map (.1) |> transpose

-- entry ftest [n] (is: [n]i64) (vs: [n]f64) =
--   reduce_by_index (replicate 4 0) (+) 0 is (map (+2) vs)

-- entry revtest [n] (is: [n]i64) (vs: [n]f64) =
--   tabulate 4 (\i -> vjp (ftest is) vs (replicate 4 0 with [i] = 1))

-- entry fwdtest [n] (is: [n]i64) (vs: [n]f64) =
--   tabulate n (\i -> jvp (ftest is) vs (replicate n 0 with [i] = 1))
--   |> transpose

-- entry revmap [n] (vs: [n]f64) =
--   tabulate n (\i -> vjp (map (+2)) vs (replicate n 0 with [i] = 1))

-- entry fwdmap [n] (vs: [n]f64) =
--   tabulate n (\i -> jvp (map (+2)) vs (replicate n 0 with [i] = 1))
--   |> transpose

-- entry revp [n] (is: [n]i64) (vs: [n]f64) =
--   (vjp2 (f is) vs (replicate 4 1))

-- entry fwdp [n] (is: [n]i64) (vs: [n]f64) =
--   (jvp2 (f is) vs (replicate n 1))

-- [[0f64, 0f64, 0f64, 0f64, 0f64, 0f64, 0f64], 
--  [5f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [0f64, 24f64, 0f64, 12f64, 0f64, 8f64, 0f64], 
--  [0f64, 0f64, 7f64, 0f64, 0f64, 0f64, 3f64]]

-- [[1f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [5f64, 1f64, 0f64, 1f64, 1f64, 1f64, 0f64], 
--  [0f64, 24f64, 1f64, 12f64, 0f64, 8f64, 1f64], 
--  [0f64, 0f64, 7f64, 0f64, 0f64, 0f64, 3f64]]


-- [[0f64, 0f64, 0f64, 0f64, 0f64, 0f64, 0f64], 
--  [5f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [0f64, 24f64, 0f64, 12f64, 0f64, 8f64, 0f64], 
--  [0f64, 0f64, 7f64, 0f64, 0f64, 0f64, 3f64]]

-- [[0f64, 0f64, 0f64, 0f64, 0f64, 0f64, 0f64], 
--  [5f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [0f64, 24f64, 0f64, 12f64, 0f64, 8f64, 0f64], 
--  [0f64, 0f64, 7f64, 0f64, 0f64, 0f64, 3f64]]


-- [[1f64, 0f64, 0f64, 0f64, 1f64, 0f64, 0f64], 
--  [0f64, 1f64, 0f64, 1f64, 0f64, 1f64, 0f64], 
--  [0f64, 0f64, 1f64, 0f64, 0f64, 0f64, 1f64], 
--  [0f64, 0f64, 0f64, 0f64, 0f64, 0f64, 0f64]]