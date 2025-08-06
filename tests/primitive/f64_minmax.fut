-- ==
-- entry: testMax
-- input { [0f64, 1f64, -1f64, 1f64, f64.nan, -1f64, f64.nan, -1f64, -1f64 ]
--         [1f64, 1f64, 1f64, -1f64, -1f64, f64.nan, f64.nan, f64.inf, -f64.inf] }
-- output { [1f64, 1f64, 1f64, 1f64, -1f64, -1f64, f64.nan, f64.inf, -1f64] }

-- ==
-- entry: testMin
-- input { [0f64, 1f64, -1f64, 1f64, f64.nan, -1f64, f64.nan, -1f64, -1f64 ]
--         [1f64, 1f64, 1f64, -1f64, -1f64, f64.nan, f64.nan, f64.inf, -f64.inf] }
-- output { [0f64, 1f64, -1f64, -1f64, -1f64, -1f64, f64.nan, -1f64, -f64.inf] }

entry testMax = map2 f64.max
entry testMin = map2 f64.min
