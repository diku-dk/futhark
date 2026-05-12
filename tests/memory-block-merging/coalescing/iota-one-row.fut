-- ==
-- entry: iota_one_row
-- input { 1i64 [ [10i64, 11i64, 12i64, 13i64, 14i64],
--                [15i64, 16i64, 17i64, 18i64, 19i64] ] }
-- output { [ [10i64, 11i64, 12i64, 13i64, 14i64],
--            [0i64, 1i64, 2i64, 3i64, 4i64] ] }
-- structure gpu-mem { Alloc 0 }
-- structure seq-mem { Alloc 0 }

entry iota_one_row [n] [m] (i: i64) (xs: *[n][m]i64) : *[n][m]i64 =
  xs with [i] = iota m
