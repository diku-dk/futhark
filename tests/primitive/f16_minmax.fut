-- ==
-- entry: testMax
-- input { [0f16, 1f16, -1f16, 1f16, f16.nan, -1f16, f16.nan, -1f16, -1f16 ]
--         [1f16, 1f16, 1f16, -1f16, -1f16, f16.nan, f16.nan, f16.inf, -f16.inf] }
-- output { [1f16, 1f16, 1f16, 1f16, -1f16, -1f16, f16.nan, f16.inf, -1f16] }

-- ==
-- entry: testMin
-- input { [0f16, 1f16, -1f16, 1f16, f16.nan, -1f16, f16.nan, -1f16, -1f16 ]
--         [1f16, 1f16, 1f16, -1f16, -1f16, f16.nan, f16.nan, f16.inf, -f16.inf] }
-- output { [0f16, 1f16, -1f16, -1f16, -1f16, -1f16, f16.nan, -1f16, -f16.inf] }

entry testMax = map2 f16.max
entry testMin = map2 f16.min
