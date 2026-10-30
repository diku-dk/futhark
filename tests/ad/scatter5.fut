-- Based on #2522.
-- ==
-- tags { autodiff }
-- input { [1.0f32,2.0f32,3.0f32,4.0f32] }
-- output { [0.0f32, 1.0f32, 1.0f32, 1.0f32] }

def f [n] (xs: [n]f32) =
  let is = iota n ++ replicate n 0i64
  let res = scatter (map id xs) is (xs ++ replicate n xs[0])
  in f32.sum res[1:n]

entry main xs =
  vjp f xs 1.0
