-- ==
-- entry: f_vjp
-- input { [1,2,3] }
-- output { [24,48,72] }
def f [n] (xs: [n]i32) =
  map (\x -> x * x * x * x) xs

entry f_vjp [n] (xs: [n]i32) =
  vjp (\xs -> vjp (\xs -> vjp f xs (replicate n 1)) xs (replicate n 1)) xs (replicate n 1)
