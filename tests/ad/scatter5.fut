-- Based on #2522.
-- ==
-- tags { autodiff }
-- input { [1.0,2.0,3.0,4.0] }
-- output { [0.0, 1.0, 1.0, 1.0] }

def f [n] (xs: [n]f64) =
  let is = iota n ++ replicate n 0i64
  let res = scatter (map id xs) is (xs ++ replicate n xs[0])
  in f64.sum res[1:n]

entry main xs =
  vjp f xs 1.0
