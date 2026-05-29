-- ==
-- tags { autodiff }
-- compiled random input { [100]i64 [50][30][20]f32 [100][30][20]f32 } output { true }

def primal2 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  reduce_by_index (copy dst) (map2 (map2 f32.max)) (replicate k (replicate l f32.lowest)) is vs

def rev2 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  tabulate m (\i -> vjp (primal2 is dst) vs (replicate m (replicate k (replicate l 0)) with [i] = replicate k (replicate l 1)))

def fwd2 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  tabulate n (\i -> jvp (primal2 is dst) vs (replicate n (replicate k (replicate l 0)) with [i] = replicate k (replicate l 1)))
  |> transpose

def rev_vec2 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  let seeds = tabulate m (\i -> replicate m (replicate k (replicate l 0)) with [i] = replicate k (replicate l 1))
  in vjp_vec (primal2 is dst) vs seeds

def fwd_vec2 [n] [m] [k] [l] (is: [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  let seeds = tabulate n (\i -> replicate n (replicate k (replicate l 0)) with [i] = replicate k (replicate l 1))
  in jvp_vec (primal2 is dst) vs seeds
  |> transpose

def main [n] [m] [k] [l] (is': [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  let r = rev2 is dst vs
  let f = fwd2 is dst vs
  let rv = rev_vec2 is dst vs
  let fv = fwd_vec2 is dst vs
  let eq_rf = map2 (map2 (map2 (==))) r f |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  let eq_rrv = map2 (map2 (map2 (==))) r rv |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  let eq_ffv = map2 (map2 (map2 (==))) f fv |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
  in eq_rf && eq_rrv && eq_ffv
