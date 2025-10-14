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

def main [n] [m] [k] [l] (is': [n]i64) (dst: [m][k][l]f32) (vs: [n][k][l]f32) =
  let is = map (\i -> (i64.abs i) %% m) is'
  let r = rev2 is dst vs
  let f = fwd2 is dst vs
  in map2 (map2 (map2 (==))) r f |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
