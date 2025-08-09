-- The #[unsafe] is just to avoid noisy assertions.  The bug here is
-- the handling of the loop.
-- ==
-- structure gpu { /SegMap/Loop/SegMap 2 }

def dotprod [n] (a: [n]f64) (b: [n]f64) : f64 =
  map2 (*) a b |> reduce (+) 0

def newton_equality [n] [m] (_: [m][n]f64) hessian_val =
  #[unsafe]
  let KKT = replicate (n + m) (replicate (n + m) 0)
  let KKT[:n, :n] = hessian_val
  in loop y = replicate n 0
     for i in 0..<n do
       let sum = dotprod KKT[i, :i] y[:i]
       let y[i] = copy (0 - sum) / KKT[i, i]
       in y

entry main A hessian_vals =
  #[sequential_inner]
  map (newton_equality A) hessian_vals
