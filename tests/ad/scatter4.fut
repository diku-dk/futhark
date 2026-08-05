-- Reverse-mode AD of a scatter over a 2D array where the index array is formed
-- by concatenation.  This used to crash the compiler with an internal type
-- error ("Variable X referenced after being consumed") after the 'ad' pass,
-- because diffUpdateAcc generated an if-expression whose true branch consumed
-- the accumulator adjoint via Update while the false branch also referenced it.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[1.0], [2.0], [3.0]] }
-- output { [[1.0], [1.0], [1.0]] }

def f [n] (xs: [n][1]f64) : [n][1]f64 =
  scatter (#[scratch] replicate n (replicate 1 0.0)) ((0..<n) ++ (0..<n)) (xs ++ xs)

entry main [n] (xs: [n][1]f64) =
  vjp f xs (replicate n (replicate 1 1.0))
