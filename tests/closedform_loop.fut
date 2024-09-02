-- ==
-- structure { Loop 1 }
-- structure gpu { Loop 1 }

entry main n = f64.sum (replicate n (1/f64.i64 n))
