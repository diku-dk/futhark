-- ==
-- entry: testMax
-- input { [0f32, 1f32, -1f32, 1f32, f32.nan, -1f32, f32.nan, -1f32, -1f32 ]
--         [1f32, 1f32, 1f32, -1f32, -1f32, f32.nan, f32.nan, f32.inf, -f32.inf] }
-- output { [1f32, 1f32, 1f32, 1f32, -1f32, -1f32, f32.nan, f32.inf, -1f32] }

-- ==
-- entry: testMin
-- input { [0f32, 1f32, -1f32, 1f32, f32.nan, -1f32, f32.nan, -1f32, -1f32 ]
--         [1f32, 1f32, 1f32, -1f32, -1f32, f32.nan, f32.nan, f32.inf, -f32.inf] }
-- output { [0f32, 1f32, -1f32, -1f32, -1f32, -1f32, f32.nan, -1f32, -f32.inf] }

entry testMax = map2 f32.max
entry testMin = map2 f32.min
