-- HOF field alongside an ordinary field in the same record.
-- ==
-- input { 2i64 3i64 } output { [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0] 42i64 }
def f (n: i64) (m: i64) = {g = (\(x: f64) -> replicate (m ** n) x), k = 42i64}

entry main n m = let r = f n m in (r.g 0, r.k)
