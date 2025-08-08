-- ==
-- entry: doeswork_named
-- script input { mkdata_named 100i64 } output { 5050.0f32 }
-- script input { mkdata_named 10000i64 }
-- script input { mkdata_named 1000000i64 }

type~ data = (i64, []f32)

entry mkdata_named n : data = (n, map f32.i64 (iota n))
entry doeswork_named ((n, arr): data) = f32.sum arr + f32.i64 n
