-- The problematic size occurs in the *argument* type of a higher-order
-- value, rather than its return type.
-- ==
-- input { 1i64 2i64 }
-- output { 0.0 }

def f (n: i64) (m: i64) = \(g: [m ** n]f64 -> f64) -> g (replicate (m ** n) 0.0)

entry main n m = f n m (\x -> head x)
