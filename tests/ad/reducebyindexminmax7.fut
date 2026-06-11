-- ==
-- entry: rev fwd rev_vec fwd_vec
-- compiled input @ reducebyindexminmax7.in output @ reducebyindexminmax7.out.gz

def primal [n] [m] [k] (is': [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  in reduce_by_index (copy dst) (map2 f32.max) (replicate k f32.lowest) is vs

entry rev [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate m (\i -> vjp (primal is dst) vs (replicate m (replicate k 0) with [i] = replicate k 1))

entry fwd [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate n (\i -> jvp (primal is dst) vs (replicate n (replicate k 0) with [i] = replicate k 1))
  |> transpose

entry rev_vec [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let seeds = tabulate m (\i -> replicate m (replicate k 0) with [i] = replicate k 1)
  in vjp_vec (primal is dst) vs seeds

entry fwd_vec [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let seeds = tabulate n (\i -> replicate n (replicate k 0) with [i] = replicate k 1)
  in jvp_vec (primal is dst) vs seeds
     |> transpose
