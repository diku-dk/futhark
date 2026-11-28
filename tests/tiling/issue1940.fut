-- The #[unsafe] is just to avoid noisy assertions.  The bug here is
-- the handling of the loop.  The loop parameter 'y' is different for
-- each thread, so the reduction over it must not be tiled.
-- ==
-- input {
--   [[3f32, 2f32, 1f32, 1f32]]
--   [[[13f32, 4f32, 4f32, 1f32], [1f32, 13f32, 3f32, 4f32], [2f32, 2f32, 12f32, 4f32], [2f32, 3f32, 3f32, 13f32]],
--    [[14f32, 3f32, 3f32, 3f32], [3f32, 13f32, 3f32, 3f32], [1f32, 2f32, 12f32, 3f32], [3f32, 2f32, 3f32, 14f32]],
--    [[12f32, 1f32, 3f32, 3f32], [2f32, 11f32, 2f32, 3f32], [3f32, 4f32, 12f32, 2f32], [2f32, 2f32, 4f32, 13f32]],
--    [[11f32, 4f32, 1f32, 3f32], [1f32, 13f32, 2f32, 2f32], [3f32, 4f32, 13f32, 4f32], [2f32, 3f32, 4f32, 12f32]]]
-- }
-- auto output

def dotprod [n] (a: [n]f32) (b: [n]f32) : f32 =
  map2 (*) a b |> reduce (+) 0

def newton_equality [n] [m] (_: [m][n]f32) hessian_val =
  #[unsafe]
  let KKT = replicate (n + m) (replicate (n + m) 0)
  let KKT[:n, :n] = hessian_val
  in loop y = replicate n 0
     for i in 0..<n do
       let sum = dotprod KKT[i, :i] y[:i]
       let y[i] = copy (1 - sum) / KKT[i, i]
       in y

entry main A hessian_vals =
  #[flattening(only_outer)]
  map (newton_equality A) hessian_vals
