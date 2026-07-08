-- Same size expression shared across two higher-order parameters.
-- ==
-- input { 2i64 3i64 } output { [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0] }
def f (n: i64) (m: i64) (g1: f64 -> [m ** n]f64) (g2: f64 -> [m ** n]f64) = (g1 0, g2 1)

entry main n m = f n m (\x -> replicate (m ** n) x) (\x -> replicate (m ** n) x)
