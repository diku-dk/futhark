-- ==
-- entry: testMax
-- input { [0i32, 1i32, -1i32, 1i32]
--         [1i32, 1i32, 1i32, -1i32]}
-- output { [1i32, 1i32, 1i32, 1i32] }

-- ==
-- entry: testMin
-- input { [0i32, 1i32, -1i32, 1i32]
--         [1i32, 1i32, 1i32, -1i32]}
-- output { [0i32, 1i32, -1i32, -1i32] }
entry testMax = map2 i32.max
entry testMin = map2 i32.min
