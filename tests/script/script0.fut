-- ==
-- entry: doeswork
-- script input { mkdata 100i64 } output { 5050.0f32 }
-- script input { mkdata 10000i64 }
-- script input { mkdata 1000000i64 }

entry mkdata n = (n, map f32.i64 (iota n))

entry doeswork n arr = f32.sum arr + f32.i64 n
