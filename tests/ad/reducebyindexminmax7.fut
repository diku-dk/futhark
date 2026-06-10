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

def approx_eql (rel_tol: f32) (a: f32) (b: f32) : bool =
  let diff = f32.abs (a - b)
  let scale = f32.max (f32.abs a) (f32.abs b)
  let abs_tol = 100.0 * f32.epsilon * scale
  in diff <= f32.max abs_tol (rel_tol * scale)

def main [n] [m] [k] (is': [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  let r = rev is dst vs
  let f = fwd is dst vs
  let rv = rev_vec is dst vs
  let fv = fwd_vec is dst vs
  let eq_rf = and (map2 (approx_eql 1e-9) (flatten_3d r) (flatten_3d f))
  let eq_rrv = and (map2 (approx_eql 1e-9) (flatten_3d r) (flatten_3d rv))
  let eq_ffv = and (map2 (approx_eql 1e-9) (flatten_3d f) (flatten_3d fv))
  in eq_rf && eq_rrv && eq_ffv
