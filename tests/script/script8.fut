-- Local entry points can be used in script inputs, and tested.
-- ==
-- entry: doeswork
-- script input { mkdata 100i64 } output { 5050.0f32 }

local entry mkdata n = (n, map f32.i64 (iota n))

local entry doeswork n arr = f32.sum arr + f32.i64 n
