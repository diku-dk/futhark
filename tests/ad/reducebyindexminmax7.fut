-- ==
-- compiled random input { [500]i64 [100][30]f32 [500][30]f32 } output { true }

def primal [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  reduce_by_index (copy dst) (map2 f32.max) (replicate k f32.lowest) is vs

def rev [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate m (\i -> vjp (primal is dst) vs (replicate m (replicate k 0) with [i] = replicate k 1))

def fwd [n] [m] [k] (is: [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  tabulate n (\i -> jvp (primal is dst) vs (replicate n (replicate k 0) with [i] = replicate k 1))
  |> transpose

def main [n] [m] [k] (is': [n]i64) (dst: [m][k]f32) (vs: [n][k]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  let r = rev is dst vs
  let f = fwd is dst vs
  in map2 (map2 (==)) r f |> map (reduce (&&) true) |> reduce (&&) true
