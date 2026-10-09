-- Reference outputs can be generated for script inputs.
-- ==
-- entry: doeswork
-- script input { mkdata 100i64 } auto output
-- script input { (100i64, map f32.i64 (iota 100)) } auto output

entry mkdata n = (n, map f32.i64 (iota n))

entry doeswork n arr = f32.sum arr + f32.i64 n
