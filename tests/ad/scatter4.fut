-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1.0f32], [2.0f32], [3.0f32]] }
-- output { [[1.0f32], [1.0f32], [1.0f32]] }

def f [n] (xs: [n][1]f32) : [n][1]f32 =
  scatter (#[scratch] replicate n (replicate 1 0.0)) ((0..<n) ++ (0..<n)) (xs ++ xs)

entry main [n] (xs: [n][1]f32) =
  vjp f xs (replicate n (replicate 1 1.0))
