-- ==
-- structure { Screma 1 }
-- structure gpu { SegRed 1 }

entry main n = f32.sum (replicate n (1 / f32.i64 n))
