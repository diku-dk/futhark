-- Basic test to ensure existing functionality isn't broken
-- ==
-- input { 5i64 } output { [0.0,0.0,0.0,0.0,0.0] }

def f [n] (g: f64 -> [n]f64) = g 0.0

entry main n = f (\x -> replicate n x)