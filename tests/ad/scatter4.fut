-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1.0], [2.0], [3.0]] }
-- output { [[1.0], [1.0], [1.0]] }

def f [n] (xs: [n][1]f64) : [n][1]f64 =
  scatter (#[scratch] replicate n (replicate 1 0.0)) ((0..<n) ++ (0..<n)) (xs ++ xs)

entry main [n] (xs: [n][1]f64) =
  vjp f xs (replicate n (replicate 1 1.0))
