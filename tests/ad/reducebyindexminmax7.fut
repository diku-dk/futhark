-- ==
-- tags { autodiff }
-- compiled random input { [500]i64 [100][30]f32 [500][30]f32 } output { true }

def primal [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  reduce_by_index (copy dst) (map2 f32.max) (replicate k f32.lowest) is vs

def rev [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate m (\i -> vjp (primal is dst) vs (replicate m (replicate k 0) with [i] = replicate k 1))

def fwd [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate n (\i -> jvp (primal is dst) vs (replicate n (replicate k 0) with [i] = replicate k 1))
  |> transpose

def rev_vec [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let seeds = tabulate m (\i -> replicate m (replicate k 0) with [i] = replicate k 1)
  in vjp_vec (primal is dst) vs seeds

def fwd_vec [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let seeds = tabulate n (\i -> replicate n (replicate k 0) with [i] = replicate k 1)
  in jvp_vec (primal is dst) vs seeds
  |> transpose

def main [n] [m] [k] (is': [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  let r = rev is dst vs
  let f = fwd is dst vs
  let rv = rev_vec is dst vs
  let fv = fwd_vec is dst vs
  let eq_rf = map2 (map2 (==)) r f |> map (reduce (&&) true) |> reduce (&&) true
  let eq_rrv = map2 (map2 (==)) r rv |> map (reduce (&&) true) |> reduce (&&) true
  let eq_ffv = map2 (map2 (==)) f fv |> map (reduce (&&) true) |> reduce (&&) true
  in eq_rf && eq_rrv && eq_ffv
