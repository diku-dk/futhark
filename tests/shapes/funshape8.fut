-- Curried (2-argument) higher-order parameter.
-- ==
-- input { 2i64 3i64 } output { [0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0] }
def f (n: i64) (m: i64) (g: i64 -> i64 -> [m ** n]f64) = g 0 1

entry main n m = f n m (\a b -> map (\i -> if i % 2 == 0 then f64.i64 a else f64.i64 b) (iota (m ** n)))
