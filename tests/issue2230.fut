-- ==
-- input { 1i64 2i64 } output { [0.0f64, 0.0f64] }

def f (n: i64) (m: i64) (g: f64 -> [m ** n]f64) = g 0

entry main a b = f a b (\x -> replicate (b ** a) x)
