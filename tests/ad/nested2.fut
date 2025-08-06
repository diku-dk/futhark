-- ==
-- entry: f_vjp
-- input { [1,2,3] [0,1,2] }
-- output { [24,48,72] [0,0,0] }
def f [n] (xsis: ([n]i32, [n]i32)) =
  let (xs, is) = xsis
  in map (\i -> xs[i] * xs[i] * xs[i] * xs[i]) is

entry f_vjp [n] (xs: [n]i32) (is: [n]i32) =
  vjp (\(xs, is) -> vjp (\(xs, is) -> vjp f (xs, is) (replicate n 1)) (xs, is) (replicate n 1, replicate n 0)) (xs, is) (replicate n 1, replicate n 0)
