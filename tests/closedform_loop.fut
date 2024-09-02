-- ==
-- input { 100i64 }
-- output { false }

entry main n = 1 == f64.sum (replicate n (1/f64.i64 n))
