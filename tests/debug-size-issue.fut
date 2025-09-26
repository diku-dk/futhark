-- Simple test to debug the issue
-- ==
-- input { 2i64 3i64 } output { [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0] }

def f (n: i64) (m: i64) (g: f64 -> [m ** n]f64) = g 0

entry main n m = f n m (\x -> replicate (m ** n) x)