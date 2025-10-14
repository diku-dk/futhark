-- ==
-- tags { autodiff }
-- entry: f_vjp
-- input { [1, 2, 3] }
-- output { [9, 9, 9] }

def f [n] (xs: [n]i32) =
  loop (B, A) = (xs, xs) for i < n do (map (+ 1) B, map (* 2) A)

entry f_vjp [n] (xs: [n]i32) = vjp f xs (replicate n 1, replicate n 1)
