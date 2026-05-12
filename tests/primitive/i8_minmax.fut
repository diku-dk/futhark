-- ==
-- entry: testMax
-- input { [0i8, 1i8, -1i8, 1i8]
--         [1i8, 1i8, 1i8, -1i8]}
-- output { [1i8, 1i8, 1i8, 1i8] }

-- ==
-- entry: testMin
-- input { [0i8, 1i8, -1i8, 1i8]
--         [1i8, 1i8, 1i8, -1i8]}
-- output { [0i8, 1i8, -1i8, -1i8] }
entry testMax = map2 i8.max
entry testMin = map2 i8.min
