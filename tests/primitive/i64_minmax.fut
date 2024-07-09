-- ==
-- entry: testMax
-- input { [0i64, 1i64, -1i64, 1i64]
--         [1i64, 1i64, 1i64, -1i64]}
-- output { [1i64, 1i64, 1i64, 1i64] }

-- ==
-- entry: testMin
-- input { [0i64, 1i64, -1i64, 1i64]
--         [1i64, 1i64, 1i64, -1i64]}
-- output { [0i64, 1i64, -1i64, -1i64] }
entry testMax = map2 i64.max
entry testMin = map2 i64.min