-- ==
-- structure { Screma 1 }
-- structure gpu { SegRed 1 }

entry main n = f64.sum (replicate n (1/f64.i64 n))
