-- ==
-- entry: f_jvp
-- input { [0i64,1i64,2i64,3i64] [1f64,2f64,3f64,4f64] }
-- output { [[1f64,0f64,0f64,0f64],[0f64,1f64,0f64,0f64],[0f64,0f64,1f64,0f64],[0f64,0f64,0f64,1f64]] }
def f [n] (is: [n]i64) (vs: [n]f64) =
  hist (+) 0 4 is (map (+ 2) vs)

entry f_jvp [n] (is: [n]i64) (vs: [n]f64) =
  tabulate n (\i -> jvp (f is) vs (replicate n 0 with [i] = 1))
  |> transpose
